(in-package :mgl-pax)

(defmacro with-standard-io-syntax* (&body body)
  `(with-standard-io-syntax
     ;; With *PRINT-READABLY*, CLISP insists on printing FOO as |FOO|.
     (let (#+clisp (*print-readably* nil))
       ,@body)))

(defun adjust-string-case (string)
  (ecase (readtable-case *readtable*)
    ((:upcase) (string-upcase string))
    ((:downcase) (string-downcase string))
    ;; We don't care about convenience with :INVERT.
    ((:preserve :invert) string)))

;;; Return the number of characters that would be read by
;;; READ-FROM-STRING. May signal READER-ERROR or END-OF-FILE.
(defun n-chars-would-read (string)
  (nth-value 1 (let ((*read-suppress* t))
                 (read-from-string string))))

(defun read-interned-symbol-from-string (string)
  (let ((pos (n-chars-would-read string)))
    (multiple-value-bind (symbol foundp)
        (swank::parse-symbol (string-trim *whitespace-chars*
                                          (subseq string 0 pos)))
      (when foundp
        (multiple-value-bind (symbol2 pos) (read-from-string string)
          (assert (eq symbol symbol2))
          (values symbol2 pos))))))

(defparameter *utf-8-external-format*
  #+abcl :utf-8
  #+clisp charset:utf-8
  #-(or abcl clisp) :default)

(defun find-package* (name)
  ;; On AllegroCL, FIND-PACKAGE will signal an error if a relative
  ;; package name has too many leading dots.
  #+allegro
  (ignore-errors (find-package name))
  #-allegro
  (find-package name))

(defun external-symbol-p (symbol)
  (let ((package (symbol-package symbol)))
    (and package
         (eq (nth-value 1 (find-symbol (symbol-name symbol) package))
             :external))))

(defun special-operator-p* (name)
  (or (special-operator-p name)
      ;; KLUDGE: CCL is mistaken about DECLARE.
      #+ccl (eq name 'declare)))

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

(defun symbol-global-value (symbol)
  #+allegro
  (multiple-value-bind (value bound) (sys:global-symbol-value symbol)
    (values value (eq bound :unbound)))
  #+ccl
  (let ((value (ccl::%sym-global-value symbol)))
    (values value (eq value (ccl::%unbound-marker))))
  #+sbcl
  (ignore-errors (sb-ext:symbol-global-value symbol))
  #-(or allegro ccl sbcl)
  (ignore-errors (symbol-value symbol)))

;;; Like SYMBOL-FUNCTION*, but sees through encapsulated functions.
(defun symbol-function* (symbol)
  #+abcl
  (or (system::untraced-function symbol)
      (symbol-function symbol))
  #+clisp
  (or (system::get-traced-definition symbol)
      (symbol-function symbol))
  #+cmucl
  (eval `(function ,symbol))
  #-(or abcl cmucl clisp)
  (unencapsulated-function (symbol-function symbol)))

(defun unencapsulated-function (function)
  (or #+ccl (ccl::find-unencapsulated-definition function)
      #+ecl (find-type-in-sexp (function-lambda-expression function) 'function)
      #+sbcl (maybe-find-encapsulated-function function)
      function))

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

(defun function-name (function)
  (let* ((function (unencapsulated-function function))
         (name #+clisp (system::function-name function)
               #-clisp (swank-backend:function-name function)))
    ;; ABCL has function names like (FOO (SYSTEM::INTERPRETED)).
    (if (listp name)
        (first name)
        name)))

(defun arglist (function-designator)
  (let ((function-designator
          (if (symbolp function-designator)
              function-designator
              (unencapsulated-function function-designator))))
    #+abcl
    (multiple-value-bind (arglist foundp)
        (extensions:arglist function-designator)
      (cond (foundp arglist)
            ((typep function-designator 'generic-function)
             (mop:generic-function-lambda-list function-designator))
            ((and (symbolp function-designator)
                  (typep (symbol-function* function-designator)
                         'generic-function))
             (mop:generic-function-lambda-list
              (symbol-function* function-designator)))))
    #+allegro
    (handler-case
        (let* ((symbol (if (symbolp function-designator)
                           function-designator
                           (function-name function-designator)))
               (lambda-expression (ignore-errors
                                   (function-lambda-expression
                                    (symbol-function symbol)))))
          (if lambda-expression
              (second lambda-expression)
              (excl:arglist symbol)))
      (simple-error () :not-available))
    #+ccl
    (let ((arglist (swank-backend:arglist function-designator)))
      ;; Function arglist don't have the default values of &KEY and
      ;; &OPTIONAL arguments. Get those from CCL:FUNCTION-SOURCE-NOTE.
      (or (and (or (find '&key arglist) (find '&optional arglist))
               (function-arglist-from-source-note function-designator))
          (if (listp arglist)
              ;; &KEY arguments are given as keywords, which screws up
              ;; WITH-DISLOCATED-SYMBOLS when generating documentation
              ;; for functions.
              (mapcar (lambda (x)
                        (if (keywordp x)
                            (intern (string x))
                            x))
                      arglist)
              arglist)))
    #-(or abcl allegro ccl)
    (swank-backend:arglist function-designator)))

#+ccl
(defun function-arglist-from-source-note (function-designator)
  (multiple-value-bind (function-name function)
      (if (functionp function-designator)
          (values (function-name function-designator) function-designator)
          (values function-designator (fdefinition function-designator)))
    (when function
      (let ((source-note (ccl:function-source-note function)))
        (when source-note
          (let ((text (ccl:source-note-text source-note)))
            (when text
              (lambda-list-from-source-note-text text function-name))))))))

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


(defun find-method* (function-designator qualifiers specializers
                     &optional (errorp t))
  (find-method (if (symbolp function-designator)
                   (symbol-function* function-designator)
                   function-designator)
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


(defmacro with-debugger-hook (fn &body body)
  (alexandria:with-gensyms (prev-debugger-hook condition this-hook)
    `(let* ((,prev-debugger-hook *debugger-hook*)
            (*debugger-hook* (lambda (,condition ,this-hook)
                               (declare (ignore ,this-hook))
                               (funcall ,fn ,condition)
                               (let ((*debugger-hook* ,prev-debugger-hook))
                                 (invoke-debugger ,condition)))))
       ,@body)))


;;; Convert to full width character string. Useful for prettier
;;; printing and ensuring canonical form.
(defun character-string (string)
  (make-array (length string) :element-type 'character
              :initial-contents string))

(defun subseq* (seq start)
  (subseq seq (min (length seq) start)))

(defun relativize-pathname (pathname reference-pathname)
  "Return a pathname that's equivalent to PATHNAME but relative to
  REFERENCE-PATHNAME if possible. Like ENOUGH-NAMESTRING, but inserts
  :UP components if necessary."
  (let ((pathname (merge-pathnames pathname *default-pathname-defaults*))
        (reference-pathname (merge-pathnames reference-pathname
                                             *default-pathname-defaults*)))
    (assert (equal (pathname-host pathname)
                   (pathname-host reference-pathname)))
    (assert (equal (pathname-device pathname)
                   (pathname-device reference-pathname)))
    (let* ((dir (pathname-directory pathname))
           (ref-dir (pathname-directory reference-pathname))
           (mismatch-index (or (mismatch dir ref-dir :test #'equal)
                               (length dir))))
      (normalize-pathname
       (make-pathname :directory (nconc (list :relative)
                                        (make-list (- (length ref-dir)
                                                      mismatch-index)
                                                   :initial-element :up)
                                        (subseq dir mismatch-index))
                      :defaults pathname)))))

(defun normalize-pathname (pathname)
  (if (equal '(:relative) (pathname-directory pathname))
      ;; Some implementations print (:RELATIVE) as "", some as "./",
      ;; no such troubles with the equivalent ().
      (make-pathname :directory () :defaults pathname)
      pathname))


;;;; String utilities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *whitespace-chars*
    '(#\Space #\Tab #\Return #\Newline #\Linefeed #\Page)))

(defun whitespacep (char)
  (member char *whitespace-chars*))

(defun blankp (string)
  (every #'whitespacep string))

(defun trim-whitespace (string)
  (string-trim #.(format nil "~{~A~}" *whitespace-chars*) string))

;;; Add PREFIX to every line in STRING.
(defun prefix-lines (prefix string &key exclude-first-line-p)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for i upfrom 0 do
        (multiple-value-bind (line missing-newline-p) (read-line in nil nil)
          (unless line
            (return))
          (if (and exclude-first-line-p (= i 0))
              (format out "~a" line)
              (format out "~a~a" prefix line))
          (unless missing-newline-p
            (terpri out)))))))

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

(defun shorten-string (string &key n-lines n-chars ellipsis)
  (let ((shortened string))
    (when n-lines
      (setq shortened (first-lines shortened n-lines)))
    (when (and n-chars (< n-chars (length shortened)))
      (setq shortened (subseq shortened 0 n-chars)))
    (if (and ellipsis (< (length shortened) (length string)))
        (concatenate 'string shortened ellipsis)
        shortened)))


(declaim (inline hashash))
(defun hashash (key hash-table)
  (nth-value 1 (gethash key hash-table)))
