(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @mgl-pax-locative-types (:title "Locative Types")
  "As we have already briefly seen, locatives allow us to refer to,
  document and find the source location of various definitions beyond
  what standard Common Lisp offers. See @MGL-PAX-EXTENSION-API for a
  more detailed treatment. The following are the locatives types
  supported out of the box. As all locative types, they are symbols
  and their names should make it obvious what kind of things they
  refer to. Unless otherwise noted, locatives take no arguments.

  When there is a corresponding CL type, a locative can be resolved to
  a unique object as is the case in `(LOCATE 'FOO 'CLASS)` returning
  `#<CLASS FOO>`. Even if there is no such CL type, the source
  location and the docstring of the defining form is recorded (see
  LOCATE-AND-FIND-SOURCE, LOCATE-AND-DOCUMENT in the
  @MGL-PAX-EXTENSION-API), which makes navigating the sources with
  `M-.` (see @MGL-PAX-EMACS-INTEGRATION) and
  @MGL-PAX-GENERATING-DOCUMENTATION possible."
  (variable locative)
  (constant locative)
  (macro locative)
  (compiler-macro locative)
  (function locative)
  (generic-function locative)
  (method locative)
  (accessor locative)
  (reader locative)
  (writer locative)
  (structure-accessor locative)
  (type locative)
  (class locative)
  (condition locative)
  (restart locative)
  (define-restart macro)
  (asdf:system locative)
  (package locative)
  (section locative)
  (include locative)
  (glossary-term locative)
  (define-glossary-term macro)
  (locative locative)
  (dislocated locative)
  (argument locative))



;;;; Utilities for argument handling

;;; Return the names of the function arguments in ARGLIST that's a
;;; lambda list. Handles &KEY, &OPTIONAL, &REST.
(defun function-arg-names (arglist)
  (unless (eq arglist :not-available)
    (mapcar (lambda (arg)
              (if (and (listp arg)
                       (symbolp (first arg)))
                  (first arg)
                  arg))
            arglist)))

;;; Return the names of the arguments in ARGLIST that's a macro lambda
;;; list.
(defun macro-arg-names (arglist)
  (unless (eq arglist :not-available)
    (let ((names ()))
      (labels ((foo (arglist)
                 (let ((seen-special-p nil))
                   (loop for arg in arglist
                         do (cond ((member arg '(&key &optional &rest &body))
                                   (setq seen-special-p t))
                                  ((symbolp arg)
                                   (push arg names))
                                  (seen-special-p
                                   (when (symbolp (first arg))
                                     (push (first arg) names)))
                                  (t
                                   (foo arg)))))))
        (foo arglist))
      (reverse names))))

(defmacro with-local-references ((refs) &body body)
  `(let ((*local-references* (append ,refs *local-references*)))
     ,@body))

(defmacro with-dislocated-symbols ((symbols) &body body)
  `(with-local-references ((mapcar (lambda (symbol)
                                (make-reference symbol 'dislocated))
                              ,symbols))
     ,@body))


;;;; Indentation utilities

;;; Normalize indentation of docstrings as it's described in
;;; (METHOD () (STRING T)) DOCUMENT-OBJECT.
(defun strip-docstring-indentation (docstring &key (first-line-special-p t))
  (let ((indentation
          (docstring-indentation docstring
                                 :first-line-special-p first-line-special-p)))
    (values (with-output-to-string (out)
              (with-input-from-string (s docstring)
                (loop for i upfrom 0
                      do (multiple-value-bind (line missing-newline-p)
                             (read-line s nil nil)
                           (unless line
                             (return))
                           (if (and first-line-special-p (zerop i))
                               (write-string line out)
                               (write-string (subseq* line indentation) out))
                           (unless missing-newline-p
                             (terpri out))))))
            indentation)))

(defun n-leading-spaces (line)
  (let ((n 0))
    (loop for i below (length line)
          while (char= (aref line i) #\Space)
          do (incf n))
    n))

;;; Return the minimum number of leading spaces in non-blank lines
;;; after the first.
(defun docstring-indentation (docstring &key (first-line-special-p t))
  (let ((n-min-indentation nil))
    (with-input-from-string (s docstring)
      (loop for i upfrom 0
            for line = (read-line s nil nil)
            while line
            do (when (and (or (not first-line-special-p) (plusp i))
                          (not (blankp line)))
                 (when (or (null n-min-indentation)
                           (< (n-leading-spaces line) n-min-indentation))
                   (setq n-min-indentation (n-leading-spaces line))))))
    (or n-min-indentation 0)))


;;;; High level printing utilities

;;; Print (DOCUMENTATION OBJECT DOC-TYPE) to STREAM in FORMAT. Clean
;;; up docstring indentation, then indent it by four spaces.
;;; Automarkup symbols.
(defun maybe-print-docstring (object doc-type stream)
  (let ((docstring (filter-documentation object doc-type)))
    (when docstring
      (format stream "~%~A~%" (massage-docstring docstring)))))

(defun massage-docstring (docstring &key (indentation "    "))
  (if *table-of-contents-stream*
      ;; The output is going to /dev/null and this is a costly
      ;; operation, skip it.
      ""
      (let ((docstring (strip-docstring-indentation docstring)))
        (prefix-lines indentation (codify-and-autolink docstring)))))

(defun filter-documentation (symbol doc-type)
  (let ((docstring (documentation symbol doc-type)))
    #+sbcl
    (if (member docstring
                '("Return whether debug-block represents elsewhere code."
                  "automatically generated accessor method"
                  "automatically generated reader method"
                  "automatically generated writer method")
                :test #'equal)
        ;; Discard the garbage docstring.
        nil
        docstring)
    #-sbcl
    docstring))


;;;; VARIABLE locative

(define-locative-type variable (&optional initform)
  "Refers to a global special variable. INITFORM, or if not specified,
  the global value of the variable is included in the documentation.")

(defmethod locate-object (symbol (locative-type (eql 'variable)) locative-args)
  (assert (<= (length locative-args) 1))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'variable))
                                locative-args stream)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (locate-and-print-bullet locative-type locative-args symbol stream)
    (write-char #\Space stream)
    (multiple-value-bind (value unboundp) (symbol-global-value symbol)
      (print-arglist (prin1-to-string (cond (initformp initform)
                                            (unboundp "-unbound-")
                                            (t value)))
                     stream))
    (print-end-bullet stream)
    (with-local-references ((list (make-reference symbol 'variable)))
      (maybe-print-docstring symbol locative-type stream))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'variable))
                                   locative-args)
  (declare (ignore locative-args))
  (find-one-location (swank-backend:find-definitions symbol)
                     '("variable" "defvar" "defparameter"
                       "special-declaration")))

(defvar end-of-variable-example)


;;;; CONSTANT locative

(define-locative-type constant (&optional initform)
  "Refers to a DEFCONSTANT. INITFORM, or if not specified,
  the value of the constant is included in the documentation.")

(defmethod locate-object (symbol (locative-type (eql 'constant)) locative-args)
  (assert (<= (length locative-args) 1))
  (assert (constantp symbol))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'constant))
                                locative-args stream)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (locate-and-print-bullet locative-type locative-args symbol stream)
    (write-char #\Space stream)
    (print-arglist (prin1-to-string (cond (initformp
                                           initform)
                                          ((boundp symbol)
                                           (symbol-value symbol))
                                          (t
                                           "<unbound>")))
                   stream)
    (print-end-bullet stream)
    (with-local-references ((list (make-reference symbol 'constant)))
      (maybe-print-docstring symbol 'variable stream))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'constant))
                                   locative-args)
  (declare (ignore locative-args))
  (find-one-location (swank-backend:find-definitions symbol)
                     '("defconstant" "constant" "variable")))


;;;; MACRO locative

(define-locative-type macro ())

(defmethod locate-object (symbol (locative-type (eql 'macro)) locative-args)
  (unless (macro-function symbol)
    (locate-error))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'macro))
                                locative-args stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (write-char #\Space stream)
  (let ((arglist (swank-backend:arglist symbol)))
    (print-arglist arglist stream)
    (print-end-bullet stream)
    (with-local-references ((list (make-reference symbol 'macro)))
      (with-dislocated-symbols ((macro-arg-names arglist))
        (maybe-print-docstring symbol 'function stream)))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'macro))
                                   locative-args)
  (declare (ignore locative-args))
  (find-source (macro-function symbol)))


;;;; COMPILER-MACRO locative

(define-locative-type compiler-macro ())

(defmethod locate-object (symbol (locative-type (eql 'compiler-macro))
                          locative-args)
  (unless (compiler-macro-function symbol)
    (locate-error))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'compiler-macro))
                                locative-args stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (write-char #\Space stream)
  ;; FIXME: This is the arglist of the function, not the compiler
  ;; macro.
  (let ((arglist (swank-backend:arglist symbol)))
    (print-arglist arglist stream)
    (print-end-bullet stream)
    (with-dislocated-symbols ((macro-arg-names arglist))
      (maybe-print-docstring symbol 'compiler-macro stream))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'compiler-macro))
                                   locative-args)
  (declare (ignore locative-args))
  #-allegro
  (find-source (compiler-macro-function symbol))
  #+allegro
  (find-one-location (swank-backend:find-definitions symbol)
                     '("compiler-macro")))


;;;; FUNCTION and GENERIC-FUNCTION locatives

(define-locative-type function ()
  "Note that the arglist in the generated documentation depends on
  the quality of SWANK-BACKEND:ARGLIST. It may be that default
  values of optional and keyword arguments are missing.")

(define-locative-type generic-function ())

(defmethod locate-object (symbol (locative-type (eql 'function)) locative-args)
  (declare (ignore locative-args))
  (when (macro-function symbol)
    (locate-error "~S is a macro, not a function." symbol))
  (let ((function (symbol-function symbol)))
    (when (typep function 'generic-function)
      (locate-error "~S is a generic function, not a plain function." symbol))
    function))

(defmethod locate-object (symbol (locative-type (eql 'generic-function))
                          locative-args)
  (declare (ignore locative-args))
  (let ((function (symbol-function symbol)))
    (unless (typep function 'generic-function)
      (locate-error "#'~S is not a generic function." symbol))
    function))

(defmethod canonical-reference ((function function))
  (make-reference (swank-backend:function-name function) 'function))

(defmethod canonical-reference ((function generic-function))
  (make-reference (swank-mop:generic-function-name function) 'generic-function))

(defmethod document-object ((function function) stream)
  (let ((reference (canonical-reference function)))
    (print-bullet reference stream)
    (write-char #\Space stream)
    (let ((arglist (swank-backend:arglist function)))
      (print-arglist arglist stream)
      (print-end-bullet stream)
      (with-local-references ((list reference))
        (with-dislocated-symbols ((function-arg-names arglist))
          (maybe-print-docstring (reference-object reference) 'function
                                 stream))))))


;;;; METHOD locative

(define-locative-type method (method-qualifiers method-specializers)
  "See CL:FIND-METHOD for the description of the arguments
  METHOD-QUALIFIERS and METHOD-SPECIALIZERS. For example, this
  DEFSECTION entry refers to the default method of the three argument
  generic function FOO:

      (foo (method () (t t t)))")

(defmethod locate-object (symbol (locative-type (eql 'method))
                          locative-args)
  (assert (= 2 (length locative-args)))
  (destructuring-bind (qualifiers specializers) locative-args
    (or (ignore-errors
         (find-method (symbol-function symbol) qualifiers
                      (loop for specializer in specializers
                            collect (typecase specializer
                                      ;; SPECIALIZER can be a cons
                                      ;; like (:EQL :SOME-VALUE) ...
                                      (cons specializer)
                                      ;; or a type specifier denoting
                                      ;; a class:
                                      (t (find-class specializer))))))
        (locate-error))))

(defmethod canonical-reference ((method method))
  (make-reference (swank-mop:generic-function-name
                   (swank-mop:method-generic-function method))
                  `(method ,(swank-mop:method-qualifiers method)
                           ,(method-specializers-list method))))

;;; Return the specializers in a format suitable as the second
;;; argument to FIND-METHOD.
(defun method-specializers-list (method)
  (mapcar (lambda (spec)
            (typecase spec
              (swank-mop:eql-specializer
               `(eql ,(swank-mop:eql-specializer-object spec)))
              (t (swank-mop:class-name spec))))
          (swank-mop:method-specializers method)))

(defmethod document-object ((method method) stream)
  (let ((arglist (rest (method-for-inspect-value method))))
    (print-bullet method stream)
    (write-char #\Space stream)
    (print-arglist arglist stream)
    (print-end-bullet stream)
    (with-dislocated-symbols ((function-arg-names arglist))
      (maybe-print-docstring method t stream))))

;;;; These were lifted from the fancy inspector contrib and then
;;;; tweaked.

(defun method-specializers-for-inspect (method)
  """Return a "pretty" list of the method's specializers. Normal
  specializers are replaced by the name of the class, eql specializers
  are replaced by `(eql ,object)."""
  (mapcar (lambda (name spec)
            (let ((name (if (listp name) (first name) name)))
              (if (eq spec t)
                  name
                  (list name spec))))
          (swank-mop:method-lambda-list method)
          (method-specializers-list method)))

(defun method-for-inspect-value (method)
  """Returns a "pretty" list describing METHOD. The first element of
  the list is the name of generic-function method is specialized on,
  the second element is the method qualifiers, the rest of the list is
  the method's specialiazers (as per
  METHOD-SPECIALIZERS-FOR-INSPECT)."""
  (append (list (swank-mop:generic-function-name
                 (swank-mop:method-generic-function method)))
          (swank-mop:method-qualifiers method)
          (method-specializers-for-inspect method)))


;;;; ACCESSOR, READER and WRITER locatives

(define-locative-type accessor (class-name)
  "To refer to an accessor named `FOO-SLOT` of class
  `FOO`:

      (foo-slot (accessor foo))")

(define-locative-type reader (class-name)
  "To refer to a reader named `FOO-SLOT` of class
  `FOO`:

      (foo-slot (reader foo))")

(define-locative-type writer (class-name)
  "To refer to a writer named `FOO-SLOT` of class
  `FOO`:

      (foo-slot (writer foo))")

(defmethod locate-object (symbol (locative-type (eql 'accessor))
                          locative-args)
  (assert (= 1 (length locative-args)) ()
          "The syntax of the ACCESSOR locative is (ACCESSOR <CLASS-NAME>).")
  (find-accessor-slot-definition symbol (first locative-args))
  (make-reference symbol (cons locative-type locative-args)))

(defun find-accessor-slot-definition (accessor-symbol class-symbol)
  (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
    (when (and (find accessor-symbol
                     (swank-mop:slot-definition-readers slot-def))
               (find `(setf ,accessor-symbol)
                     (swank-mop:slot-definition-writers slot-def)
                     :test #'equal))
      (return-from find-accessor-slot-definition slot-def)))
  (locate-error "Could not find accessor ~S for class ~S." accessor-symbol
                class-symbol))

(defmethod locate-object (symbol (locative-type (eql 'reader))
                          locative-args)
  (assert (= 1 (length locative-args)) ()
          "The syntax of the READER locative is (READER <CLASS-NAME>).")
  (find-reader-slot-definition symbol (first locative-args))
  (make-reference symbol (cons locative-type locative-args)))

(defun find-reader-slot-definition (accessor-symbol class-symbol)
  (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
    (when (find accessor-symbol (swank-mop:slot-definition-readers slot-def))
      (return-from find-reader-slot-definition slot-def)))
  (locate-error "Could not find reader ~S for class ~S." accessor-symbol
                class-symbol))

(defmethod locate-object (symbol (locative-type (eql 'writer))
                          locative-args)
  (assert (= 1 (length locative-args)) ()
          "The syntax of the WRITER locative is (WRITER <CLASS-NAME>).")
  (find-writer-slot-definition symbol (first locative-args))
  (make-reference symbol (cons locative-type locative-args)))

(defun find-writer-slot-definition (accessor-symbol class-symbol)
  (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
    (when (find accessor-symbol (swank-mop:slot-definition-writers slot-def))
      (return-from find-writer-slot-definition slot-def)))
  (locate-error "Could not find writer ~S for class ~S." accessor-symbol
                class-symbol))

(defmethod locate-and-document (symbol (locative-type (eql 'accessor))
                                locative-args stream)
  (generate-documentation-for-slot-definition
   symbol (find-accessor-slot-definition symbol (first locative-args))
   locative-type locative-args stream))

(defmethod locate-and-document (symbol (locative-type (eql 'reader))
                                locative-args stream)
  (generate-documentation-for-slot-definition
   symbol (find-reader-slot-definition symbol (first locative-args))
   locative-type locative-args stream))

(defmethod locate-and-document (symbol (locative-type (eql 'writer))
                                locative-args stream)
  (generate-documentation-for-slot-definition
   symbol (find-writer-slot-definition symbol (first locative-args))
   locative-type locative-args stream))

(defun generate-documentation-for-slot-definition
    (symbol slot-def locative-type locative-args stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (write-char #\Space stream)
  (print-arglist locative-args stream)
  (when (or (swank-mop:slot-definition-initargs slot-def)
            (swank-mop:slot-definition-initfunction slot-def))
    (write-char #\Space stream)
    (if (and *document-mark-up-signatures* (eq *format* :html))
        (let ((initarg-strings
                (when (swank-mop:slot-definition-initargs slot-def)
                  (mapcar #'prin1-and-escape-markdown
                          (swank-mop:slot-definition-initargs slot-def)))))
          (print-arglist
           (format nil "(~{~A~^ ~}~A)" initarg-strings
                   (if (swank-mop:slot-definition-initfunction slot-def)
                       (format nil "~A= ~A"
                               (if initarg-strings " " "")
                               (codify-and-autolink
                                (prin1-and-escape-markdown
                                 (swank-mop:slot-definition-initform
                                  slot-def))))
                       ""))
           stream))
        (print-arglist
         (prin1-to-string
          `(,@(when (swank-mop:slot-definition-initargs slot-def)
                (swank-mop:slot-definition-initargs slot-def))
            ,@(when (swank-mop:slot-definition-initfunction slot-def)
                `(=
                  ,(swank-mop:slot-definition-initform slot-def)))))
         stream)))
  (print-end-bullet stream)
  ;; No documentation for condition accessors, and some
  ;; implementations signal warnings.
  (with-local-references ((list (make-reference symbol
                                                (cons locative-type
                                                      locative-args))))
    (unless (subtypep (find-class (first locative-args)) 'condition)
      (let ((docstring (swank-mop:slot-definition-documentation slot-def)))
        (when docstring
          (format stream "~%~A~%" (massage-docstring docstring)))))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'accessor))
                                   locative-args)
  (find-source (find-method (symbol-function symbol)
                            '() (list (find-class (first locative-args))))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'reader))
                                   locative-args)
  (find-source (find-method (symbol-function symbol)
                            '() (list (find-class (first locative-args))))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'writer))
                                   locative-args)
  (find-source (find-method (symbol-function symbol)
                            '() (mapcar #'find-class
                                        (list t (first locative-args))))))


;;;; STRUCTURE-ACCESSOR locative

(define-locative-type structure-accessor ()
  "This is a synonym of FUNCTION with the difference that the often
  ugly and certainly uninformative lambda list will not be printed.")

(defmethod locate-object ((symbol symbol)
                          (locative-type (eql 'structure-accessor))
                          locative-args)
  ;; Signal an error if it doesn't exist.
  (or (ignore-errors (symbol-function symbol))
      (locate-error))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document ((symbol symbol)
                                (locative-type (eql 'structure-accessor))
                                locative-args stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (print-end-bullet stream)
  (with-local-references ((list (make-reference symbol 'structure-accessor)))
    (maybe-print-docstring symbol 'function stream)))

(defmethod locate-and-find-source (symbol
                                   (locative-type (eql 'structure-accessor))
                                   locative-args)
  (declare (ignore locative-args))
  ;; Some implementations can not find the source location of the
  ;; accessor function, so fall back on FIND-ONE-LOCATION.
  (let ((location (find-source (symbol-function symbol))))
    (if (eq :error (first location))
        (find-one-location (swank-backend:find-definitions symbol)
                           '("function" "operator"))
        location)))


;;;; TYPE locative

(define-locative-type type ()
  "TYPE can refer to classes as well, but it's better style to use the
  more specific CLASS locative type for that. Another difference to
  CLASS is that an attempt is made at printing the arguments of type
  specifiers.")

(defmethod locate-object (symbol (locative-type (eql 'type)) locative-args)
  (unless (swank-backend:type-specifier-p 'symbol)
    (locate-error))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'type)) locative-args
                                stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (let ((arglist (swank-backend:type-specifier-arglist symbol)))
    (when (and arglist (not (eq arglist :not-available)))
      (write-char #\Space stream)
      (print-arglist arglist stream)))
  (print-end-bullet stream)
  (with-local-references ((list (make-reference symbol 'type)))
    (maybe-print-docstring symbol 'type stream)))

(defmethod locate-and-find-source (symbol (locative-type (eql 'type))
                                   locative-args)
  (declare (ignore locative-args))
  (find-one-location (swank-backend:find-definitions symbol)
                     '("type" "class")))


;;;; CLASS and CONDITION locatives

(define-locative-type class ()
  "Naturally, CL:CLASS is the locative type for classes. To refer to a
  class named FOO:

      (foo class)")

(define-locative-type condition ()
  "CL:CONDITION is the locative type for condition. To refer to a
  condition named FOO:

      (foo condition)")

(defmethod locate-object (symbol (locative-type (eql 'class)) locative-args)
  (declare (ignore locative-args))
  (or (find-class symbol :errorp nil)
      (locate-error)))

(defmethod locate-object (symbol (locative-type (eql 'condition))
                          locative-args)
  (assert (= 0 (length locative-args)))
  (let ((class (find-class symbol :errorp nil)))
    (unless (subtypep class 'condition)
      (locate-error))
    class))

(defmethod canonical-reference ((class class))
  (if (subtypep class 'condition)
      (make-reference (class-name class) 'condition)
      (make-reference (class-name class) 'class)))

(defmethod document-object ((class class) stream)
  (let* ((conditionp (subtypep class 'condition))
         (symbol (class-name class))
         (superclasses
           (remove-if (lambda (name)
                        (or (eq name 'standard-object)
                            (and conditionp (eq name 'condition))))
                      (mapcar #'class-name
                              (swank-mop:class-direct-superclasses class)))))
    (print-bullet class stream)
    (when superclasses
      (write-char #\Space stream)
      (if *document-mark-up-signatures*
          (print-arglist (mark-up-superclasses superclasses) stream)
          (print-arglist superclasses stream)))
    (print-end-bullet stream)
    (with-local-references ((list (make-reference symbol 'class)))
      (maybe-print-docstring class t stream))))

(defun mark-up-superclasses (superclasses)
  (with-output-to-string (stream)
    (loop for class in superclasses
          for i upfrom 0
          do (let ((reference (make-reference class 'class)))
               (let ((name (escape-markdown (prin1-to-string class))))
                 (unless (zerop i)
                   (format stream " "))
                 (if (find-known-reference reference)
                     (format stream "[~A][~A]" name
                             (link-to-reference reference))
                     (format stream "~A" name)))))))

(defun find-known-reference (reference)
  (find reference *references* :test #'reference=))


;;;; RESTART locative

(define-symbol-locative-type restart ()
  "A locative to refer to the definition of a restart defined by
  DEFINE-RESTART.")

(define-definer-for-symbol-locative-type define-restart restart
  """A definer macro to hang the documentation of a restart on a
  symbol.

  ```
  (define-restart my-ignore-error ()
    "Available when MY-ERROR is signalled, MY-IGNORE-ERROR unsafely continues.")
  ```

  Then `(MY-IGNORE-ERROR RESTART)` refers to the above definition.
  Note that while there is a CL:RESTART class, there is no
  corresponding source location or docstring like for CONDITIONs.
  """)


;;;; ASDF:SYSTEM locative

(define-locative-type asdf:system ()
  "Refers to an asdf system. The generated documentation will include
  meta information extracted from the system definition. This also
  serves as an example of a symbol that's not accessible in the
  current package and consequently is not exported.")

(defmethod locate-object (symbol (locative-type (eql 'asdf:system))
                          locative-args)
  (assert (endp locative-args))
  ;; FIXME: This is slow as hell.
  (or (asdf:find-system symbol nil)
      (locate-error)))

(defmethod canonical-reference ((system asdf:system))
  (make-reference (slot-value system 'asdf::name) 'asdf:system))

(defmethod document-object ((system asdf:system) stream)
  (with-heading (stream system
                        (format nil "~A ASDF System Details"
                                (string-upcase
                                 (slot-value system 'asdf::name))))
    (flet ((foo (name fn &key type)
             (let ((value (funcall fn system)))
               (when value
                 (case type
                   ((:link)
                    (format stream "- ~A: [~A](~A)~%" name value value))
                   ((:mailto)
                    (format stream "- ~A: [~A](mailto:~A)~%"
                            name value value))
                   ((:source-control)
                    (format stream "- ~A: [~A](~A)"
                            name (first value) (second value)))
                   ((:docstring)
                    (format stream "- ~A: ~A~%" name
                            ;; Like MASSAGE-DOCSTRING but without
                            ;; indenting.
                            (prefix-lines "  "
                                          (codify-and-autolink
                                           (strip-docstring-indentation value))
                                          :exclude-first-line-p t)))
                   ((nil)
                    (format stream "- ~A: ~A~%" name value)))))))
      (foo "Version" 'asdf/component:component-version)
      (foo "Description" 'asdf/system:system-description :type :docstring)
      (foo "Long Description" 'asdf/system:system-long-description
           :type :docstring)
      (foo "Licence" 'asdf/system:system-licence)
      (foo "Author" 'asdf/system:system-author)
      (foo "Maintainer" 'asdf/system:system-maintainer)
      (foo "Mailto" 'asdf/system:system-mailto :type :mailto)
      (foo "Homepage" 'asdf/system:system-homepage :type :link)
      (foo "Bug tracker" 'asdf/system:system-bug-tracker :type :link)
      (foo "Source control" 'asdf/system:system-source-control
           :type :source-control)
      (terpri stream))))

(defmethod find-source ((system asdf:system))
  `(:location
    (:file ,(namestring (asdf/system:system-source-file system)))
    (:position 1)
    (:snippet "")))

(defvar end-of-asdf-example)


;;;; PACKAGE locative

(define-locative-type package ())

(defmethod locate-object (symbol (locative-type (eql 'package))
                          locative-args)
  (assert (= 0 (length locative-args)))
  (or (find-package symbol) (locate-error)))

(defmethod canonical-reference ((package package))
  (make-reference (package-name package) 'package))

(defmethod document-object ((package package) stream)
  (let ((symbol (package-name package)))
    (print-bullet package stream)
    (print-end-bullet stream)
    (with-local-references ((list (make-reference symbol 'package)))
      (maybe-print-docstring package t stream))))


;;;; SECTION locative

(define-locative-type section ()
  "Refers to a section defined by DEFSECTION.")

(defmethod locate-object (symbol (locative-type (eql 'section))
                          locative-args)
  (declare (ignore locative-args))
  (assert (typep (symbol-value symbol) 'section))
  (symbol-value symbol))

(defmethod canonical-reference ((section section))
  (make-reference (section-name section) 'section))

(defmethod collect-reachable-objects ((section section))
  (mapcan (lambda (reference)
            (cons reference (collect-reachable-objects reference)))
          (remove-if-not (lambda (entry)
                           (typep entry 'reference))
                         (section-entries section))))

(defvar *section*)

(defmethod document-object ((section section) stream)
  (let ((same-package (eq *package* (section-package section)))
        (*package* (if *document-normalize-packages*
                       (section-package section)
                       *package*))
        (*readtable* (section-readtable section))
        (*section* section))
    (with-heading (stream section (section-title-or-name section)
                          :link-title-to (section-link-title-to section))
      (when (and *document-normalize-packages* (not same-package))
        (format stream "###### \\[in package ~A~A\\]~%" (package-name *package*)
                (if (package-nicknames *package*)
                    (format nil " with nicknames ~{~A~^, ~}"
                            (package-nicknames *package*))
                    "")))
      (let ((firstp t))
        (dolist (entry (section-entries section))
          (if firstp
              (setq firstp nil)
              (terpri stream))
          (with-nested-headings ()
            (document-object entry stream)))))))

(defmethod find-source ((section section))
  (locate-and-find-source (section-name section) 'variable ()))


;;;; INCLUDE locative

(define-locative-type include (source &key line-prefix header footer
                                      header-nl footer-nl)
  """Refers to a region of a file. SOURCE can be a string or a
  pathname in which case the whole file is being pointed to or it can
  explicitly supply START, END locatives. INCLUDE is typically used to
  include non-lisp files in the documentation (say markdown or elisp
  as in the next example) or regions of lisp source files. This can
  reduce clutter and duplication.

  ```commonlisp
  (defsection example-section ()
    (pax.el (include #.(asdf:system-relative-pathname :mgl-pax "src/pax.el")
                     :header-nl "```elisp" :footer-nl "```"))
    (foo-example (include (:start (foo function)
                           :end (end-of-foo-example variable))
                          :header-nl "```commonlisp"
                          :footer-nl "```"))

  (defun foo (x)
    (1+ x))

  ;;; Since file regions are copied verbatim, comments survive.
  (defmacro bar ())

  ;;; This comment is the last thing in FOO-EXAMPLE's
  ;;; documentation since we use the dummy END-OF-FOO-EXAMPLE
  ;;; variable to mark the end location.
  (defvar end-of-foo-example)

  ;;; More irrelevant code follows.
  ```

  In the above example, pressing `M-.` on PAX.EL will open the
  `src/pax.el` file and put the cursor on its first character. `M-.`
  on `FOO-EXAMPLE` will go to the source location of the `(asdf:system
  locative)` locative.

  When documentation is generated, the entire `pax.el` file is
  included in the markdown surrounded by the strings given as
  HEADER-NL and FOOTER-NL (if any). The trailing newline character is
  assumed implicitly. If that's undesirable, then use HEADER and
  FOOTER instead. The documentation of `FOO-EXAMPLE` will be the
  region of the file from the source location of the START
  locative (inclusive) to the source location of the END
  locative (exclusive). START and END default to the beginning and end
  of the file, respectively.

  Note that the file of the source location of :START and :END must be
  the same. If SOURCE is pathname designator, then it must be absolute
  so that the locative is context independent.

  Finally, if specified LINE-PREFIX is a string that's prepended to
  each line included in the documentation. For example, a string of
  four spaces makes markdown think it's a code block.""")

(defmethod exportable-locative-type-p ((locative-type (eql 'include)))
  nil)

(defmethod locate-object (symbol (locative-type (eql 'include))
                          locative-args)
  (destructuring-bind (source &key line-prefix header footer
                       header-nl footer-nl) locative-args
    (declare (ignore source line-prefix header footer header-nl footer-nl))
    (make-reference symbol (cons locative-type locative-args))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'include))
                                   locative-args)
  (multiple-value-bind (file start) (include-region (first locative-args))
    (assert file)
    `(:location
      (:file ,(namestring file))
      (:position ,(1+ start))
      nil)))

(defmethod locate-and-document (symbol (locative-type (eql 'include))
                                locative-args stream)
  (destructuring-bind (source &key (line-prefix "") header footer
                       header-nl footer-nl) locative-args
    (when header
      (format stream "~A" header))
    (when header-nl
      (format stream "~A~%" header-nl))
    (format stream "~A"
            (prefix-lines line-prefix
                          (multiple-value-call #'file-subseq
                            (include-region source))))
    (when footer
      (format stream "~A" footer))
    (when footer-nl
      (format stream "~A~%" footer-nl))))

;;; Return the filename and start, end positions of the region to be
;;; included.
(defun include-region (source)
  (cond ((or (stringp source) (pathnamep source))
         (assert (uiop/pathname:absolute-pathname-p source) ()
                 "Pathnames given as the SOURCE argument of the ~
                 INCLUDE locative must be absolute, but ~S is not."
                 source)
         (values source 0 nil))
        ((and source (listp source))
         (destructuring-bind (&key start end) source
           (let ((start (find-source (resolve (entry-to-reference start))))
                 (end (find-source (resolve (entry-to-reference end)))))
             (when start
               (check-location start))
             (when end
               (check-location end))
             (let ((start-file (when start (location-file start)))
                   (start-position (when start (location-position start)))
                   (end-file (when end (location-file end)))
                   (end-position (when end (location-position end))))
               (when (and start end)
                 (assert (string= (namestring (truename start-file))
                                  (namestring (truename end-file)))
                         () "Include starts in file ~S and ends in ~
                         another file ~S." start-file end-file))
               (values (or start-file end-file) start-position end-position)))))
        (t
         (error "~@<Malformed include source ~S.~:@>" source))))

;;; Check that LOCATION looks like this:
;;;
;;;     (:location
;;;      (:file "filename")
;;;      (:position 1)
;;;      (:snippet ""))
(defun check-location (location)
  (assert (listp location) () "Location ~S is not a list." location)
  (assert (eq (first location) :location) ()
          "Location ~S does not start with ~S." location :location)
  (assert (and (location-file location)
               (location-position location))
          () "Location ~S should contain: ~S."
          location '(:file :position)))

(defun location-file (location)
  (second (find :file (rest location) :key #'first)))

(defun location-position (location)
  (1- (second (find :position (rest location) :key #'first))))

(defun file-subseq (pathname &optional start end)
  (with-open-file (stream pathname)
    (let ((*print-pretty* nil)
          (start (or start 0))
          (end (or end (file-length stream)))
          (buffer-size 4096))
      (file-position stream start)
      (with-output-to-string (datum)
        (let ((buffer (make-array buffer-size :element-type 'character)))
          (loop
            for bytes-read = (read-sequence
                              buffer stream
                              :end (min buffer-size
                                        (- end (file-position stream))))
            do (write-sequence buffer datum :start 0 :end bytes-read)
            while (= bytes-read buffer-size)))))))


;;;; GLOSSARY-TERM locative

(defclass glossary-term ()
  ((name
    :initarg :name :reader glossary-term-name
    :documentation "The name of the global variable whose value is
    this GLOSSARY-TERM object.")
   (title
    :initarg :title :reader glossary-term-title
    :documentation "Used in generated documentation.")
   (docstring
    :initarg :docstring :reader glossary-term-docstring)))

(defmacro define-glossary-term
    (name (&key title (discard-documentation-p *discard-documentation-p*))
     docstring)
  "Define a global variable with NAME and set it to a glossary term
  object. A glossary term is just a symbol to hang a docstring on. It
  is a bit like a SECTION in that, when linked to, its TITLE will be
  the link text instead of the name of the symbol. Unlike sections
  though, glossary terms are not rendered with headings, but in the
  more lightweight bullet + locative + name/title style.

  When DISCARD-DOCUMENTATION-P (defaults to *DISCARD-DOCUMENTATION-P*)
  is true, DOCSTRING will not be recorded to save memory."
  `(defparameter ,name
     (make-instance 'glossary-term
                    :name ',name :title ,title
                    :docstring ,(unless discard-documentation-p
                                  docstring))))

(defun glossary-term-title-or-name (glossary-term)
  (or (glossary-term-title glossary-term)
      (maybe-downcase (prin1-to-string (glossary-term-name glossary-term)))))

(defmethod print-object ((glossary-term glossary-term) stream)
  (print-unreadable-object (glossary-term stream :type t)
    (format stream "~a" (glossary-term-name glossary-term))))

(define-locative-type glossary-term ()
  "Refers to a glossary term defined by DEFINE-GLOSSARY-TERM.")

(defmethod locate-object (symbol (locative-type (eql 'glossary-term))
                          locative-args)
  (declare (ignore locative-args))
  (assert (typep (symbol-value symbol) 'glossary-term))
  (symbol-value symbol))

(defmethod document-object ((glossary-term glossary-term) stream)
  (let ((symbol (glossary-term-name glossary-term)))
    (locate-and-print-bullet 'glossary-term () symbol stream
                             :name (glossary-term-title-or-name glossary-term))
    (print-end-bullet stream)
    (with-local-references ((list (make-reference symbol 'glossary-term)))
      (let ((docstring (glossary-term-docstring glossary-term)))
        (when docstring
          (format stream "~%~A~%" (massage-docstring docstring)))))))

(defmethod canonical-reference ((glossary-term glossary-term))
  (make-reference (glossary-term-name glossary-term) 'glossary-term))

(defmethod find-source ((glossary-term glossary-term))
  (locate-and-find-source (glossary-term-name glossary-term) 'variable ()))


;;;; LOCATIVE locative

(define-locative-type locative (lambda-list)
  "This is the locative for locatives. When `M-.` is pressed on
  `SOME-NAME` in `(SOME-NAME LOCATIVE)`, this is what makes it
  possible to land at the `(DEFINE-LOCATIVE-TYPE SOME-NAME ...)` form.
  Similarly, `(LOCATIVE LOCATIVE)` leads to this very definition.")

(defmethod locate-object (symbol (locative-type (eql 'locative)) locative-args)
  (assert (endp locative-args))
  (or (ignore-errors (locative-lambda-list-method-for-symbol symbol))
      (locate-error))
  (make-reference symbol (cons locative-type locative-args)))

(defun locative-lambda-list-method-for-symbol (symbol)
  (find-method #'locative-lambda-list () `((eql ,symbol))))

(defmethod locate-and-document (symbol (locative-type (eql 'locative))
                                locative-args stream)
  (let ((method (locative-lambda-list-method-for-symbol symbol))
        (lambda-list (locative-lambda-list symbol)))
    (locate-and-print-bullet locative-type locative-args symbol stream)
    (with-dislocated-symbols ((macro-arg-names lambda-list))
      (when lambda-list
        (write-char #\Space stream)
        (print-arglist lambda-list stream))
      (print-end-bullet stream)
      (with-local-references ((list (make-reference symbol 'locative)))
        (maybe-print-docstring method t stream))))
  (format stream "~&"))

(defmethod locate-and-find-source (symbol (locative-type (eql 'locative))
                                   locative-args)
  (declare (ignore locative-args))
  (find-source (locative-lambda-list-method-for-symbol symbol)))


;;;; DISLOCATED locative

(define-locative-type dislocated ()
  "Refers to a symbol in a non-specific context. Useful for preventing
  autolinking. For example, if there is a function called `FOO` then

      `FOO`

  will be linked to (if *DOCUMENT-LINK-CODE*) its definition. However,

      [`FOO`][dislocated]

  will not be. On a dislocated locative LOCATE always fails with a
  LOCATE-ERROR condition.")

(defmethod locate-object (symbol (locative-type (eql 'dislocated))
                          locative-args)
  (declare (ignore symbol locative-args))
  (locate-error))


;;;; ARGUMENT locative

(define-locative-type argument ()
  """An alias for DISLOCATED, so the one can refer to an argument of a
  macro without accidentally linking to a class that has the same name
  as that argument. In the following example, FORMAT may link to
  CL:FORMAT (if we generated documentation for it):

  ```
  "See the FORMAT in DOCUMENT."
  ```

  Since ARGUMENT is a locative, we can prevent that linking by writing:

  ```
  "See the FORMAT argument of DOCUMENT."
  ```""")

(defmethod locate-object (symbol (locative-type (eql 'argument)) locative-args)
  (declare (ignore symbol locative-args))
  (locate-error))

