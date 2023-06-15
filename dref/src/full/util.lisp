(in-package :dref)

;;;; Symbols and packages

(defun find-package* (name)
  ;; On AllegroCL, FIND-PACKAGE will signal an error if a relative
  ;; package name has too many leading dots. On CMUCL, (FIND-PACKAGE
  ;; "..") fails.
  #+(or allegro cmucl)
  (ignore-errors (find-package name))
  #-(or allegro cmucl)
  (find-package name))


;;;; Types

(defun valid-type-specifier-p (type)
  (handler-case
      (null (nth-value 1 (ignore-errors (typep nil type))))
    ;; Avoid "WARNING: * is not permitted as a type specifier" on
    ;; SBCL.
    #+sbcl
    (warning (c) (ignore-errors (muffle-warning c)))
    ;; Silence compiler notes on SBCL when run via ASDF:TEST-SYSTEM.
    #+sbcl
    (sb-kernel:parse-unknown-type ())
    #+cmucl
    (sys::parse-unknown-type ())))


;;;; Macros

(defun special-operator-p* (name)
  (or (special-operator-p name)
      ;; KLUDGE: CCL is mistaken about DECLARE.
      #+ccl (eq name 'declare)))


;;;; SETF

(defun setf-name-p (name)
  (and (listp name)
       (= (length name) 2)
       (eq (first name) 'setf)))

;;; See if SYMBOL has a [setf expander][clhs] or [setf
;;; function][clhs].
(defun has-setf-p (symbol)
  (or (has-setf-expander-p symbol)
      (has-setf-function-p symbol)))

(defun has-setf-expander-p (symbol)
  ;; FIXME: other implemenations
  #+ccl (gethash symbol ccl::%setf-methods%)
  #+clisp (get symbol 'system::setf-expander)
  #+sbcl (swank/sbcl::setf-expander symbol)
  #-(or ccl clisp sbcl)
  ;; KLUDGE: When there is no setf expansion, we get this:
  ;;
  ;;   (nth-value 3 (get-setf-expansion '(undefined)))
  ;;   => (FUNCALL #'(SETF UNDEFINED) #:NEW1)
  ;;
  ;; which is non-portable in theory, but luckily, portable in
  ;; practice.
  (let ((storing-form (nth-value 3 (ignore-errors
                                    (get-setf-expansion `(,symbol))))))
    ;; Sadly, using (NTH-VALUE 3 (GET-SETF-EXPANSION `(,SYMBOL))) to
    ;; tell whether SYMBOL has a setf expansion doesn't work in
    ;; general because GET-SETF-EXPANSION may fail due to either
    ;; DEFINE-SETF-EXPANDER or the macro named by the value of SYMBOL
    ;; failing (e.g. with an insufficient number of arguments). For
    ;; this reason, DEFSETFs can be detected, but
    ;; DEFINE-SETF-EXPANDERs cannot in general.
    (and storing-form
         (not (and (eq (first storing-form) 'funcall)
                   (equal (second storing-form) `#'(setf ,symbol)))))))

(defun has-setf-function-p (symbol)
  (values (ignore-errors (fdefinition* `(setf ,symbol)))))


;;;; Functions

(defun function-name (function)
  (let* ((function (unencapsulated-function function))
         (name #+clisp (system::function-name function)
               #-clisp (swank-backend:function-name function)))
    #-abcl
    (let ((kind (and (listp name)
                     (= (length name) 2)
                     (case (first name)
                       ((macro-function) 'macro)
                       ((compiler-macro) 'compiler-macro)))))
      (if kind
          (values (second name) kind)
          name))
    ;; ABCL has function names like (FOO (SYSTEM::INTERPRETED)).
    #+abcl
    (if (and (listp name) (not (eq (first name) 'setf)))
        (first name)
        name)))

;;; Like SYMBOL-FUNCTION but sees through encapsulated functions.
(defun symbol-function* (symbol)
  #+abcl
  (or (system::untraced-function symbol)
      (symbol-function symbol))
  #+clisp
  (or (system::get-traced-definition symbol)
      (symbol-function symbol))
  #-(or abcl clisp)
  (unencapsulated-function (symbol-function symbol)))

(defun fdefinition* (name)
  #+abcl
  (or (system::untraced-function name)
      (fdefinition name))
  #+clisp
  (if (listp name)
      (eval `(function ,name))
      (or (system::get-traced-definition name)
          (fdefinition name)))
  #-(or abcl clisp)
  (unencapsulated-function (fdefinition name)))

(defun unencapsulated-function (function)
  (or #+ccl (ccl::find-unencapsulated-definition function)
      #+cmucl (loop for fn = function then (fwrappers:fwrapper-next fn)
                    while (typep fn 'fwrappers:fwrapper)
                    finally (return fn))
      #+ecl (find-type-in-sexp (function-lambda-expression function) 'function)
      #+sbcl (maybe-find-encapsulated-function function)
      function))

#+ecl
(defun find-type-in-sexp (form type)
  (dolist (x form)
    (cond ((listp x)
           (let ((r (find-type-in-sexp x type)))
             (when r
               (return-from find-type-in-sexp r))))
          ((typep x type)
           (return-from find-type-in-sexp x))
          (t
           nil))))

#+sbcl
;;; Tracing typically encapsulates a function in a closure. The
;;; function we need is at the end of the encapsulation chain.
(defun maybe-find-encapsulated-function (function)
  (declare (type function function))
  (if (eq (sb-impl::%fun-name function) 'sb-impl::encapsulation)
      (maybe-find-encapsulated-function
       (sb-impl::encapsulation-info-definition
        (sb-impl::encapsulation-info function)))
      function))


;;; Return either the arglist and FOUNDP, or NIL and NIL if the
;;; arglist was not found.
(defun function-arglist (function-designator &optional (foundp t))
  (let ((function-designator
          (if (symbolp function-designator)
              #-cmucl function-designator
              #+cmucl (symbol-function* function-designator)
              (unencapsulated-function function-designator))))
    #-(or abcl allegro ccl)
    (let ((arglist (swank-backend:arglist function-designator)))
      (if (eq arglist :not-available)
          (values nil nil)
          (values arglist foundp)))
    #+abcl
    (multiple-value-bind (arglist foundp*)
        (extensions:arglist function-designator)
      (cond (foundp*
             (values arglist foundp))
            ((typep function-designator 'generic-function)
             (values (mop:generic-function-lambda-list function-designator)
                     foundp))
            ((and (symbolp function-designator)
                  (typep (symbol-function* function-designator)
                         'generic-function))
             (values (mop:generic-function-lambda-list
                      (symbol-function* function-designator))
                     foundp))))
    #+allegro
    (handler-case
        (let* ((symbol (if (symbolp function-designator)
                           function-designator
                           (function-name function-designator)))
               (lambda-expression (ignore-errors
                                   (function-lambda-expression
                                    (symbol-function symbol)))))
          (values (if lambda-expression
                      (second lambda-expression)
                      (excl:arglist function-designator))
                  foundp))
      (simple-error () nil))
    #+ccl
    (let* ((function-designator (or (and (functionp function-designator)
                                         (function-name function-designator))
                                    function-designator))
           (arglist (swank-backend:arglist function-designator)))
      ;; Function arglist don't have the default values of &KEY and
      ;; &OPTIONAL arguments. Get those from CCL:FUNCTION-SOURCE-NOTE.
      (alexandria:nth-value-or 0
        (and (listp arglist)
             (or (find '&key arglist) (find '&optional arglist))
             (function-arglist-from-source-note function-designator foundp))
        (values (if (listp arglist)
                    ;; &KEY arguments are given as keywords, which
                    ;; screws up WITH-DISLOCATED-SYMBOLS when
                    ;; generating documentation for functions.
                    (mapcar (lambda (x)
                              (if (keywordp x)
                                  (intern (string x))
                                  x))
                            arglist)
                    arglist)
                foundp)))))

#+ccl
(defun function-arglist-from-source-note (function-designator foundp)
  (multiple-value-bind (function-name function)
      (if (functionp function-designator)
          (values (function-name function-designator) function-designator)
          (values function-designator (fdefinition function-designator)))
    (when function
      (let ((source-note (ccl:function-source-note function)))
        (when source-note
          (let ((text (ccl:source-note-text source-note)))
            (when text
              (values (lambda-list-from-source-note-text text function-name)
                      foundp))))))))

;;; Extract the lambda list from TEXT, which is like "(defun foo (x
;;; &optional (o 1)) ...".
#+ccl
(defun lambda-list-from-source-note-text (text symbol)
  ;; This is a heuristic. It is impossible to determine what *PACKAGE*
  ;; was when the definition form was read.
  (let ((*package* (symbol-package symbol)))
    (with-input-from-string (s text)
      (when (eql (read-char s nil) #\()
        ;; Skip DEFUN and the name.
        (let ((*read-suppress* t))
          (read s nil)
          (read s nil))
        (ignore-errors (read s))))))

;;; Return the names of the function arguments in ARGLIST, which is an
;;; [ordinary lambda list][clhs]. Handles &KEY, &OPTIONAL, &REST,
;;; &AUX, &ALLOW-OTHER-KEYS.
(defun function-arg-names (arglist)
  (multiple-value-bind (requireds optionals rest keywords other-keys-p auxs)
      (alexandria:parse-ordinary-lambda-list arglist)
    (declare (ignore other-keys-p))
    (let ((names requireds))
      (dolist (optional optionals)
        (push (first optional) names)
        ;; SUPPLIEDP
        (when (third optional)
          (push (third optional) names)))
      (when rest
        (push rest names))
      (dolist (keyword keywords)
        (push (second (first keyword)) names)
        (when (third keyword)
          (push (third keyword) names)))
      (dolist (aux auxs)
        (push (first aux) names))
      (reverse names))))

(defun method-arglist (method)
  (let ((arglist (swank-mop:method-lambda-list method)))
    ;; Some implementations include the specializers. Remove them.
    (loop for arg in arglist
          collect (if (and (listp arg)
                           (= (length arg) 2))
                      (first arg)
                      arg))))


;;;; Methods

(defun find-method* (function-designator qualifiers specializers
                     &optional (errorp t))
  (find-method (if (functionp function-designator)
                   function-designator
                   (fdefinition* function-designator))
               qualifiers
               (specializers-to-objects specializers)
               errorp))

(defun specializers-to-objects (specializers)
  #-(or allegro ccl clisp) specializers
  #+(or allegro ccl clisp) (mapcar #'specializer-to-object specializers))

(defun objects-to-specializers (objects)
  #-(or allegro ccl clisp) objects
  #+(or allegro ccl clisp) (mapcar #'object-to-specializer objects))

#+(or allegro ccl clisp)
(defun specializer-to-object (specializer)
  (cond ((symbolp specializer)
         (find-class specializer))
        ((and (listp specializer)
              (= (length specializer) 2)
              (eq (first specializer) 'eql))
         #+allegro (aclmop:intern-eql-specializer (second specializer))
         #+ccl (ccl:intern-eql-specializer (second specializer))
         #+clisp specializer)
        (t specializer)))

#+(or allegro ccl clisp)
(defun object-to-specializer (object)
  (cond ((typep object 'class)
         (class-name object))
        #+ccl
        ((typep object 'ccl:eql-specializer)
         `(eql ,(ccl:eql-specializer-object object)))
        (t object)))


;;;; Strings

;;; Convert to full width character string. Useful for prettier
;;; printing and ensuring canonical form.
(defun character-string (string)
  (make-array (length string) :element-type 'character
              :initial-contents string))

(defun adjust-string-case (string)
  (ecase (readtable-case *readtable*)
    ((:upcase) (string-upcase string))
    ((:downcase) (string-downcase string))
    ;; We don't care about convenience with :INVERT.
    ((:preserve :invert) string)))

(defun first-lines (string &optional (n-lines 1))
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for i below n-lines do
        (let ((line (read-line in nil nil)))
          (when line
            (cond ((< i (1- n-lines))
                   (write-line line out))
                  ((= i (1- n-lines))
                   (write-string line out)))))))))

(defun first-line (string)
  (first-lines string))


;;;; I/O

(defmacro with-standard-io-syntax* (&body body)
  `(with-standard-io-syntax
     ;; With *PRINT-READABLY*, CLISP insists on printing FOO as |FOO|.
     (let (#+clisp (*print-readably* nil))
       ,@body)))


;;;; DOCUMENTATION

(defun documentation* (object doc-type)
  "A small wrapper around CL:DOCUMENTATION to smooth over differences
  between implementations."
  ;; KLUDGE: Some just can't decide where the documentation is. Traced
  ;; generic functions complicate things.
  (when (functionp object)
    #+(or ccl ecl)
    (when (and (eq doc-type 'function)
               (null (documentation object 'function)))
      (setq object (function-name object)))
    #+cmucl
    (setq object (function-name object)))
  #+cmucl
  (when (typep object 'class)
    (setq object (class-name object)
          doc-type 'type))
  (when (and (eq doc-type 'setf)
             (null (documentation object 'setf)))
    ;; CLISP runs into NO-APPLICABLE-METHOD currently.
    #-clisp
    (setq object `(setf ,object)
          doc-type 'function))
  (let* ((docstring (documentation object doc-type))
         #+sbcl
         (docstring (filter-junk-docstrings docstring)))
    docstring))

#+sbcl
(defun filter-junk-docstrings (docstring)
  (if (member docstring
              '("Return whether debug-block represents elsewhere code."
                "automatically generated accessor method"
                "automatically generated reader method"
                "automatically generated writer method")
              :test #'equal)
      nil
      docstring))


;;;; Misc

;;; Only compute the key for each element once.
(defun sort-list-with-precomputed-key (list pred &key key)
  (map 'list #'car (sort (map 'vector (lambda (x)
                                        (cons x (funcall key x)))
                              list)
                         pred :key #'cdr)))

(defun unlist1 (obj)
  (if (and (listp obj) (= (length obj) 1))
      (first obj)
      obj))
