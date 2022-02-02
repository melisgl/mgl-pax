(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

;;; Make Allegro record lambda lists, from which we can extract
;;; default values of arguments.
#+allegro
(eval-when (:compile-toplevel)
  (declaim (optimize (debug 3))))

(defsection @mgl-pax-locative-types (:title "Locative Types")
  """As we have already briefly seen in DEFSECTION and
  @MGL-PAX-LOCATIVES-AND-REFERENCES, locatives allow us to refer to,
  document and find the source location of various definitions beyond
  what standard Common Lisp offers. See @MGL-PAX-EXTENSION-API for a
  more detailed treatment. The following are the locatives types
  supported out of the box. As all locative types, they are symbols,
  and their names should make it obvious what kind of things they
  refer to. Unless otherwise noted, locatives take no arguments.

  When there is a corresponding CL type, a locative can be resolved to
  a unique object as is the case in `(LOCATE 'FOO 'CLASS)` returning
  `#<CLASS FOO>`. Even if there is no such CL type, the source
  location and the docstring of the defining form is recorded (see
  LOCATE-AND-FIND-SOURCE, LOCATE-AND-DOCUMENT in the
  @MGL-PAX-EXTENSION-API), which makes navigating the sources with
  `M-.` (see @MGL-PAX-NAVIGATING-IN-EMACS) and
  @MGL-PAX-GENERATING-DOCUMENTATION possible.
  """
  (@mgl-pax-variablelike-locatives section)
  (@mgl-pax-macrolike-locatives section)
  (@mgl-pax-functionlike-locatives section)
  (@mgl-pax-typelike-locatives section)
  (@mgl-pax-condition-system-locatives section)
  (@mgl-pax-packagelike-locatives section)
  (@mgl-pax-pax-locatives section)
  (@mgl-pax-external-locatives section))

(defsection @mgl-pax-variablelike-locatives (:title "Locatives for Variables")
  (variable locative)
  (constant locative))

(defsection @mgl-pax-macrolike-locatives (:title "Locatives for Macros")
  (macro locative)
  (symbol-macro locative)
  (compiler-macro locative))

(defsection @mgl-pax-functionlike-locatives (:title "Locatives for Functions")
  (function locative)
  (generic-function locative)
  (method locative)
  (method-combination locative)
  (accessor locative)
  (reader locative)
  (writer locative)
  (structure-accessor locative))

(defsection @mgl-pax-typelike-locatives
    (:title "Locatives for Types and Declarations")
  (type locative)
  (class locative)
  (declaration locative))

(defsection @mgl-pax-condition-system-locatives
    (:title "Condition System Locatives")
  (condition locative)
  (restart locative)
  (define-restart macro))

(defsection @mgl-pax-packagelike-locatives
    (:title "Locatives for Packages and Readtables")
  (asdf:system locative)
  (package locative)
  (readtable locative))

(defsection @mgl-pax-pax-locatives (:title "Locatives for PAX Constructs")
  (section locative)
  (glossary-term locative)
  (define-glossary-term macro)
  (locative locative)
  (dislocated locative)
  (argument locative)
  (include locative))

(defsection @mgl-pax-external-locatives (:title "External Locatives")
  (clhs locative))


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


;;;; VARIABLE locative

(define-locative-type variable (&optional initform)
  """Refers to a global special variable. INITFORM, or if not specified,
  the global value of the variable is included in the documentation.

  ```
  ;;; A REFERENCE is returned because there is no such type as VARIABLE.
  (locate '*FORMAT* 'variable)
  ==> #<REFERENCE *FORMAT* VARIABLE>
  ```

  For the output of `(DOCUMENT (MAKE-REFERENCE '*FORMAT* 'VARIABLE))`,
  see *FORMAT*. Note that *FORMAT* is unbound. If the variable is
  BOUNDP, then its _current_ value is included in the documentation.
  See *DOCUMENT-LINK-CODE* for an example output. To override the
  current value, `INITFORM` may be provided. This is particulary
  useful if the value of the variable is something undesirable such as
  `#<MY-CLASS {100171ED93}>`.
  """)

(defmethod locate-object (symbol (locative-type (eql 'variable)) locative-args)
  (unless (<= (length locative-args) 1)
    (locate-error "The lambda list of the VARIABLE locative is ~
                   (&OPTIONAL INITFORM)."))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'variable))
                                locative-args stream)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (locate-and-print-bullet locative-type locative-args symbol stream)
    (write-char #\Space stream)
    (multiple-value-bind (value unboundp) (symbol-global-value symbol)
      (when (or initformp (not unboundp))
        (print-arglist (prin1-and-escape-markdown (if initformp
                                                      initform
                                                      value))
                       stream)))
    (print-end-bullet stream)
    (with-local-references ((list (make-reference symbol 'variable)))
      (maybe-print-docstring symbol locative-type stream))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'variable))
                                   locative-args)
  (declare (ignore locative-args))
  (find-definition symbol (swank-variable-dspecs symbol)))

(defvar end-of-variable-example)


;;;; CONSTANT locative

(define-locative-type constant (&optional initform)
  "Refers to a DEFCONSTANT. INITFORM, or if not specified,
  the value of the constant is included in the documentation. The
  CONSTANT locative is like the VARIABLE locative, but it also checks
  that its object is CONSTANTP.")

(defmethod locate-object (symbol (locative-type (eql 'constant)) locative-args)
  (unless (<= (length locative-args) 1)
    (locate-error "The lambda list of the CONSTANT locative is ~
                   (&OPTIONAL INITFORM)."))
  (unless (constantp symbol)
    (locate-error "~S is not CONSTANTP." symbol))
  ;; KLUDGE: CONSTANTP is non-compliant on CLISP.
  #-clisp
  (unless (constantp symbol)
    (locate-error "~S is not CONSTANTP." symbol))
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
  (find-definition symbol (swank-constant-dspecs symbol)))


;;;; MACRO locative

(define-locative-type macro ()
  "Refers to a global macro, typically defined with DEFMACRO or a
  [special operator][SPECIAL-OPERATOR-P FUNCTION]. See the FUNCTION
  locative for a note on arglists.")

(defmethod locate-object (symbol (locative-type (eql 'macro)) locative-args)
  (unless (or (macro-function symbol)
              (special-operator-p symbol))
    (locate-error "~S does not name a macro." symbol))
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
  (find-definition* (macro-function symbol) symbol
                    (swank-macro-dspecs symbol)))


;;;; SYMBOL-MACRO locative

(define-locative-type symbol-macro ()
  """Refers to a global symbol macro, defined with DEFINE-SYMBOL-MACRO.
  Note that since DEFINE-SYMBOL-MACRO does not support docstrings, PAX
  defines methods on the DOCUMENTATION generic function specialized
  for `DOC-TYPE` SYMBOL-MACRO.

  ```
  (define-symbol-macro my-mac 42)
  (setf (documentation 'my-mac 'symbol-macro)
        "This is MY-MAC.")
  (documentation 'my-mac 'symbol-macro)
  ```
  """)

(defvar *symbol-macro-docstrings* (make-hash-table))

(defmethod documentation ((symbol symbol) (doc-type (eql 'symbol-macro)))
  (gethash symbol *symbol-macro-docstrings*))

(defmethod (setf documentation) (docstring (symbol symbol)
                                 (doc-type (eql 'symbol-macro)))
  (setf (gethash symbol *symbol-macro-docstrings*) docstring))

(defmethod locate-object (symbol (locative-type (eql 'symbol-macro))
                          locative-args)
  ;; There is no portable way to test the existence of the symbol
  ;; macro.
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'symbol-macro))
                                locative-args stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (print-end-bullet stream)
  (with-local-references ((list (make-reference symbol 'symbol-macro)))
    (maybe-print-docstring symbol 'symbol-macro stream)))

(defmethod locate-and-find-source (symbol (locative-type (eql 'symbol-macro))
                                   locative-args)
  (declare (ignore locative-args))
  (find-definition symbol (swank-symbol-macro-dspecs symbol)))


;;;; COMPILER-MACRO locative

(define-locative-type compiler-macro ()
  "Refers to a compiler macro, typically defined with
  DEFINE-COMPILER-MACRO. See the FUNCTION locative for a note on
  arglists.")

(defmethod locate-object (symbol (locative-type (eql 'compiler-macro))
                          locative-args)
  (unless (compiler-macro-function symbol)
    (locate-error "~S does not name a compiler macro." symbol))
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
    (with-local-references ((list (make-reference symbol 'compiler-macro)))
      (with-dislocated-symbols ((macro-arg-names arglist))
        (maybe-print-docstring symbol 'compiler-macro stream)))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'compiler-macro))
                                   locative-args)
  (declare (ignore locative-args))
  (find-definition* (compiler-macro-function symbol)
                    symbol (swank-compiler-macro-dspecs symbol)))


;;;; FUNCTION and GENERIC-FUNCTION locatives

(define-locative-type function ()
  "Refers to a global function, typically defined with DEFUN.

  Note that the arglist in the generated documentation depends on the
  quality of SWANK-BACKEND:ARGLIST. It [may
  be][@mgl-pax-document-implementation-notes section] that default
  values of optional and keyword arguments are missing.")

(define-locative-type generic-function ()
  "Refers to a [GENERIC-FUNCTION][class], typically defined with
  DEFGENERIC.")

(defmethod locate-object (symbol (locative-type (eql 'function)) locative-args)
  (declare (ignore locative-args))
  (when (macro-function symbol)
    (locate-error "~S is a macro, not a function." symbol))
  (let ((function (ignore-errors (symbol-function* symbol))))
    (unless function
      (locate-error "~S does not name a function." symbol))
    (when (typep function 'generic-function)
      (locate-error "~S names a generic function, not a plain function."
                    symbol))
    function))

(defmethod locate-object (symbol (locative-type (eql 'generic-function))
                          locative-args)
  (declare (ignore locative-args))
  (let ((function (symbol-function* symbol)))
    (unless (typep function 'generic-function)
      (locate-error "~S does not name a generic function." symbol))
    function))

(defmethod canonical-reference ((function function))
  (make-reference (function-name function) 'function))

;;; It may be that (NOT (EQ SYMBOL (FUNCTION-NAME (FUNCTION
;;; SYMBOL)))), perhaps due to (SETF SYMBOL-FUNCTION). Thus if we have
;;; a REFERENCE, don't resolve it and construct using FUNCTION-NAME.
(defmethod locate-canonical-reference
    (object (locative-type (eql 'function)) locative-args)
  (make-reference object (cons locative-type locative-args)))

(defmethod canonical-reference ((function generic-function))
  (make-reference (swank-mop:generic-function-name function)
                  'generic-function))

(defmethod locate-canonical-reference
    (object (locative-type (eql 'generic-function)) locative-args)
  (make-reference object (cons locative-type locative-args)))

(defmethod document-object ((function function) stream)
  (let* ((function (unencapsulated-function function))
         (reference (canonical-reference function)))
    (print-bullet reference stream)
    (write-char #\Space stream)
    (let ((arglist (arglist function)))
      (print-arglist arglist stream)
      (print-end-bullet stream)
      (with-local-references ((list reference))
        (with-dislocated-symbols ((function-arg-names arglist))
          ;; KLUDGE: Some just can't decide where the documentation
          ;; is. Traced generic functions complicate things.
          #+(or ccl ecl)
          (if (documentation function 'function)
              (maybe-print-docstring function 'function stream)
              (maybe-print-docstring (function-name function) 'function
                                     stream))
          #-(or ccl ecl)
          (maybe-print-docstring (function-name function) 'function
                                 stream))))))

(defmethod locate-and-find-source
    (symbol (locative-type (eql 'function)) locative-args)
  (declare (ignore locative-args))
  (find-definition* (symbol-function symbol)
                    symbol (swank-function-dspecs symbol)))

(defmethod locate-and-find-source
    (symbol (locative-type (eql 'generic-function)) locative-args)
  (declare (ignore locative-args))
  (find-definition* (symbol-function symbol)
                    symbol (swank-generic-function-dspecs symbol)))


;;;; METHOD locative

(define-locative-type method (method-qualifiers method-specializers)
  "See CL:FIND-METHOD for the description of the arguments
  METHOD-QUALIFIERS and METHOD-SPECIALIZERS. For example,
  a `(FOO (METHOD () (T (EQL XXX))))` as a DEFSECTION entry refers to
  this method:

      (defmethod foo (x (y (eql 'xxx)))
        ...)

  METHOD is not EXPORTABLE-LOCATIVE-TYPE-P.")

(defmethod locate-object (symbol (locative-type (eql 'method)) locative-args)
  (unless (= 2 (length locative-args))
    (locate-error "The syntax of the METHOD locative is ~
                   (METHOD <METHOD-QUALIFIERS> <METHOD-SPECIALIZERS>)."))
  (destructuring-bind (qualifiers specializers) locative-args
    (or (ignore-errors (find-method* symbol qualifiers specializers))
        (locate-error "Method does not exist."))))

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
    (with-local-references ((list (canonical-reference method)))
      (with-dislocated-symbols ((function-arg-names arglist))
        (maybe-print-docstring method t stream)))))

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

(defmethod locate-and-find-source
    (symbol (locative-type (eql 'method)) locative-args)
  ;; FIND-DEFINITION* would be faster on SBCL, but then we need to
  ;; implement dealing EQL specialier objects.
  (find-definition* (find-method* symbol (first locative-args)
                                  (second locative-args))
                    symbol (swank-method-dspecs symbol (first locative-args)
                                                (second locative-args))))


;;;; METHOD-COMBINATION locative

(define-locative-type method-combination ()
  "Refers to a [METHOD-COMBINATION][class], defined with
  DEFINE-METHOD-COMBINATION.")

(defmethod locate-object (symbol (locative-type (eql 'method-combination))
                          locative-args)
  (when locative-args
    (locate-error "The METHOD-COMBINATION locative takes no arguments."))
  (unless (symbolp symbol)
    (locate-error))
  ;; There is no portable way to get the actual METHOD-COMBINATION
  ;; object.
  (make-reference symbol 'method-combination))

(defmethod locate-and-document
    (symbol (locative-type (eql 'method-combination)) locative-args stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (print-end-bullet stream)
  (with-local-references ((list (make-reference symbol 'macro)))
    (maybe-print-docstring symbol 'method-combination stream)))

(defmethod locate-and-find-source
    (symbol (locative-type (eql 'method-combination)) locative-args)
  (declare (ignore locative-args))
  (find-definition symbol (swank-method-combination-dspecs symbol)))


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
  (unless (= 1 (length locative-args))
    (locate-error "The syntax of the ACCESSOR locative is ~
                   (ACCESSOR <CLASS-NAME>)."))
  (find-accessor-slot-definition symbol (first locative-args))
  (make-reference symbol (cons locative-type locative-args)))

(defun find-accessor-slot-definition (accessor-symbol class-symbol)
  (when (class-slots-supported-p class-symbol)
    (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
      (when (and (find accessor-symbol
                       (swank-mop:slot-definition-readers slot-def))
                 (find `(setf ,accessor-symbol)
                       (swank-mop:slot-definition-writers slot-def)
                       :test #'equal))
        (return-from find-accessor-slot-definition slot-def)))
    (locate-error "Could not find accessor ~S for class ~S."
                  accessor-symbol class-symbol)))

(defun class-slots-supported-p (class)
  #-cmucl (declare (ignore class))
  #-cmucl t
  #+cmucl (not (subtypep class 'condition)))

(defmethod locate-object (symbol (locative-type (eql 'reader))
                          locative-args)
  (unless (= 1 (length locative-args))
    (locate-error "The syntax of the READER locative is ~
                   (READER <CLASS-NAME>)."))
  (find-reader-slot-definition symbol (first locative-args))
  (make-reference symbol (cons locative-type locative-args)))

(defun find-reader-slot-definition (reader-symbol class-symbol)
  (when (class-slots-supported-p class-symbol)
    (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
      (when (find reader-symbol (swank-mop:slot-definition-readers slot-def))
        (return-from find-reader-slot-definition slot-def)))
    (locate-error "Could not find reader ~S for class ~S." reader-symbol
                  class-symbol)))

(defmethod locate-object (symbol (locative-type (eql 'writer))
                          locative-args)
  (unless (= 1 (length locative-args))
    (locate-error "The syntax of the WRITER locative is ~
                   (WRITER <CLASS-NAME>)."))
  (find-writer-slot-definition symbol (first locative-args))
  (make-reference symbol (cons locative-type locative-args)))

(defun find-writer-slot-definition (accessor-symbol class-symbol)
  (when (class-slots-supported-p class-symbol)
    (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
      (when (find accessor-symbol (swank-mop:slot-definition-writers slot-def))
        (return-from find-writer-slot-definition slot-def)))
    (locate-error "Could not find writer ~S for class ~S."
                  accessor-symbol class-symbol)))

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
  (when (and slot-def
             (or (swank-mop:slot-definition-initargs slot-def)
                 (swank-mop:slot-definition-initfunction slot-def)))
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
                               (codify-and-link
                                (prin1-and-escape-markdown
                                 (swank-mop:slot-definition-initform
                                  slot-def))))
                       ""))
           stream))
        (print-arglist
         (prin1-and-escape-markdown
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
  (find-definition* (find-method* symbol () (list (first locative-args)))
                    symbol (swank-accessor-dspecs symbol (first locative-args)
                                                  nil)))

(defmethod locate-and-find-source (symbol (locative-type (eql 'reader))
                                   locative-args)
  (find-definition* (find-method* symbol () (list (first locative-args)))
                    symbol (swank-accessor-dspecs symbol (first locative-args)
                                                  nil)))

(defmethod locate-and-find-source (symbol (locative-type (eql 'writer))
                                   locative-args)
  (find-definition* (find-method* symbol () (list t (first locative-args)))
                    symbol (swank-accessor-dspecs symbol (first locative-args)
                                                  t)))


;;;; STRUCTURE-ACCESSOR locative

(define-locative-type structure-accessor ()
  "This is a synonym of [FUNCTION][locative] with the difference that
  the often ugly and certainly uninformative lambda list will not be
  printed.")

(defmethod locate-object ((symbol symbol)
                          (locative-type (eql 'structure-accessor))
                          locative-args)
  ;; Signal an error if it doesn't exist.
  (or (ignore-errors (symbol-function* symbol))
      (locate-error "~S does not name a function." symbol))
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
  (find-definition* (symbol-function symbol)
                    symbol (swank-function-dspecs symbol)))


;;;; TYPE locative

(define-locative-type type ()
  "This locative can refer to any Lisp type. For types defined with
  DEFTYPE, an attempt is made at printing the arguments of type
  specifiers. When TYPE refers to a [CL:CLASS][class], the class is
  documented as an opaque type: no mention is made of that it is a
  class or its superclasses. Use the CLASS locative if those things
  are part of the contract.")

(defmethod locate-object (symbol (locative-type (eql 'type)) locative-args)
  (unless (and (symbolp symbol)
               ;; On some lisps, SWANK-BACKEND:TYPE-SPECIFIER-P is not
               ;; reliable.
               #-(or abcl allegro clisp cmucl ecl)
               (swank-backend:type-specifier-p symbol))
    (locate-error "~S is not a valid type specifier." symbol))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'type)) locative-args
                                stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (let ((arglist (swank-backend:type-specifier-arglist symbol)))
    (when (and arglist (not (eq arglist :not-available)))
      (write-char #\Space stream)
      (print-arglist arglist stream))
    (print-end-bullet stream)
    (with-local-references ((list (make-reference symbol 'type)))
      (with-dislocated-symbols ((function-arg-names arglist))
        (maybe-print-docstring symbol 'type stream)))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'type))
                                   locative-args)
  (declare (ignore locative-args))
  (find-definition symbol (append (swank-type-dspecs symbol)
                                  (swank-class-dspecs symbol)
                                  (swank-condition-dspecs symbol))))


;;;; CLASS and CONDITION locatives

(define-locative-type class ()
  "Naturally, CLASS is the locative type for [CLASS][class]es.
  To refer to a class named FOO:

      (foo class)

  In the generated documention, only superclasses denoted by [external
  symbols][find-symbol function] are included.")

(define-locative-type condition ()
  "CONDITION is the locative type for [CONDITION][condition]s. To
  refer to a condition named FOO:

      (foo condition)

  In the generated documention, only superclasses denoted by [external
  symbols][find-symbol function] are included.")

(defmethod locate-object (symbol (locative-type (eql 'class)) locative-args)
  (or (and (symbolp symbol)
           (endp locative-args)
           (find-class symbol nil))
      (locate-error "~S does not name a class." symbol)))

(defmethod locate-object (symbol (locative-type (eql 'condition))
                          locative-args)
  (let ((class (find-class symbol nil)))
    (unless (and (symbolp symbol)
                 (endp locative-args)
                 class
                 (subtypep class 'condition))
      (locate-error "~S does not name a condition class." symbol))
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
                            (and conditionp (eq name 'condition))
                            ;; Omit non-exported superclasses.
                            (not (eq (nth-value
                                      1 (find-symbol (symbol-name name)
                                                     (symbol-package name)))
                                     :external))))
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
      #+cmucl
      (maybe-print-docstring symbol 'type stream)
      #-cmucl
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

(defmethod locate-and-find-source
    (symbol (locative-type (eql 'class)) locative-args)
  (declare (ignore locative-args))
  (find-definition* (find-class symbol)
                    symbol (swank-class-dspecs symbol)))

(defmethod locate-and-find-source
    (symbol (locative-type (eql 'condition)) locative-args)
  (declare (ignore locative-args))
  (find-definition* (find-class symbol)
                    symbol (swank-condition-dspecs symbol)))


;;;; DECLARATION locative

(define-locative-type declaration ()
  """Refers to a declaration, used in DECLARE, DECLAIM and PROCLAIM.
  For example, `[DEBUG][declaration]` refers to the standard DEBUG
  declaration and links to the hyperspec if
  *DOCUMENT-LINK-TO-HYPERSPEC* is true.

  User code may also define new declarations with CLTL2 functionality,
  but there is no way to provide a docstring.

  ```
  (cl-environments:define-declaration my-decl (&rest things)
    (values :declare (cons 'foo things)))
  ```

  Also, `M-.` (see @MGL-PAX-NAVIGATING-IN-EMACS) on declarations
  currently only works on SBCL.
  """)

(defvar *ansi-declarations*
  '(compilation-speed debug declaration dynamic-extent ftype ignorable
    ignore inline notinline optimize safety space special speed type))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-cltl2))

(defmethod locate-object (symbol (locative-type (eql 'declaration))
                          locative-args)
  (unless (and (symbolp symbol)
               (or (find symbol *ansi-declarations*)
                   #-sbcl t
                   #+sbcl (find symbol (sb-cltl2:declaration-information
                                        'declaration))))
    (locate-error "~S is not a known declaration." symbol))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'declaration))
                                locative-args stream)
  (declare (ignore locative-args))
  (let ((reference (canonical-reference (make-reference symbol 'declaration))))
    (print-bullet reference stream)
    (print-end-bullet stream)))

(defmethod locate-and-find-source (symbol (locative-type (eql 'declaration))
                                   locative-args)
  (declare (ignore #-sbcl symbol locative-args))
  #+sbcl
  (swank-backend:find-source-location (sb-int:info :declaration :known symbol))
  #-sbcl
  '(:error "Don't know how to find the source location of declarations."))

;;; Lacking DECLARATION-INFORMATION form CLTL2 on Lisps other than
;;; SBCL, LOCATE-OBJECT always succeeds, which leads to M-. thinking
;;; that there is a declaration if it's in the search list.
#+sbcl
(add-locative-to-source-search-list 'declaration)


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
  Note that while there is a CL:RESTART type, there is no
  corresponding source location or docstring like for
  [CONDITION][condition]s.
  """)


;;;; ASDF:SYSTEM locative

(define-locative-type asdf:system ()
  "Refers to an asdf system. The generated documentation will include
  meta information extracted from the system definition. This also
  serves as an example of a symbol that's not accessible in the
  current package and consequently is not exported.

  ASDF:SYSTEM is not EXPORTABLE-LOCATIVE-TYPE-P.")

(defmethod locate-object (name (locative-type (eql 'asdf:system))
                          locative-args)
  (or (and (endp locative-args)
           ;; FIXME: This is slow as hell.
           (asdf:find-system (string-downcase (string name)) nil))
      (locate-error "~S does not name an asdf system." name)))

(defmethod canonical-reference ((system asdf:system))
  (make-reference (character-string (slot-value system 'asdf::name))
                  'asdf:system))

;;; For testing
(defvar *omit-asdf-slots* nil)

(defmethod document-object ((system asdf:system) stream)
  (with-heading (stream system
                        (format nil "~A ASDF System Details"
                                (string-upcase
                                 (slot-value system 'asdf::name))))
    (flet ((foo (name fn &key type)
             (let ((value (funcall fn system)))
               (when (and value (not (equal value "")))
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
                                          (codify-and-link
                                           (strip-docstring-indentation value))
                                          :exclude-first-line-p t)))
                   ((nil)
                    (format stream "- ~A: ~A~%" name value)))))))
      (unless *omit-asdf-slots*
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
        (terpri stream)))))

(defmethod find-source ((system asdf:system))
  `(:location
    (:file ,(namestring (asdf/system:system-source-file system)))
    (:position 1)
    (:snippet "")))

(add-locative-to-source-search-list 'asdf:system)

(defvar end-of-asdf-example)

;;; In addition to the CANONICAL-REFERENCE method above, define this
;;; as well to prevent LOCATE-CANONICAL-REFERENCE `(METHOD () (T T
;;; T))` from trying to LOCATE the reference. This is all because
;;; ASDF:FIND-SYSTEM is so slow.
(defmethod locate-canonical-reference (name (locative-type (eql 'asdf:system))
                                       locative-args)
  (declare (ignore locative-args))
  (make-reference (character-string (string-downcase (string name)))
                  'asdf:system))

;; By a similar rationale, let's specialize this too.
(defmethod locate-and-collect-reachable-objects
    (name (locative-type (eql 'asdf:system)) locative-args)
  (declare (ignore name locative-args))
  ())


;;;; PACKAGE locative

(define-locative-type package ()
  "Refers to a [PACKAGE][type], defined by DEFPACKAGE. PACKAGE is not
  EXPORTABLE-LOCATIVE-TYPE-P.")

(defmethod locate-object (package-designator (locative-type (eql 'package))
                          locative-args)
  (or (and (endp locative-args)
           (or (symbolp package-designator)
               (stringp package-designator))
           (find-package* package-designator))
      (locate-error "~S does not name a package." package-designator)))

(defmethod canonical-reference ((package package))
  (make-reference (character-string (package-name package)) 'package))

(defmethod document-object ((package package) stream)
  (let ((symbol (package-name package)))
    (print-bullet package stream)
    (print-end-bullet stream)
    (with-local-references ((list (make-reference symbol 'package)))
      (maybe-print-docstring package t stream))))

(defmethod locate-and-find-source
    (name (locative-type (eql 'package)) locative-args)
  (declare (ignore locative-args))
  (find-definition* (find-package* name)
                    (if (stringp name)
                        (make-symbol name)
                        name)
                    (swank-package-dspecs name)))


;;;; READTABLE locative

(define-locative-type readtable ()
  "Refers to a named [READTABLE][] defined with
  NAMED-READTABLES:DEFREADTABLE, which associates a global name and a
  docstring with the readtable object. Unfortunately, source location
  information is not available.")

(defmethod locate-object (symbol (locative-type (eql 'readtable))
                          locative-args)
  (declare (ignore locative-args))
  (named-readtables:find-readtable symbol))

(defmethod document-object ((readtable readtable) stream)
  (let ((symbol (readtable-name readtable)))
    (locate-and-print-bullet 'readtable () symbol stream)
    (print-end-bullet stream)
    (with-local-references ((list (make-reference symbol 'readtable)))
      (maybe-print-docstring symbol 'readtable stream))))

(defmethod canonical-reference ((readtable readtable))
  (let ((name (readtable-name readtable)))
    (unless name
      (error "~S is not a NAMED-READTABLE." readtable))
    (make-reference name 'readtable)))

(defmethod find-source ((readtable readtable))
  '(:error "Don't know how find the source location of readtables."))


;;;; SECTION locative

(define-locative-type section ()
  "Refers to a section defined by DEFSECTION.")

(defun section-title-or-name (section)
  (or (section-title section)
      (funcall (if (downcasingp)
                   #'string-downcase
                   #'identity)
               (prin1-to-string (section-name section)))))

(defmethod locate-object (symbol (locative-type (eql 'section))
                          locative-args)
  (declare (ignore locative-args))
  (unless (and (symbolp symbol)
               (boundp symbol)
               (typep (symbol-value symbol) 'section))
    (locate-error))
  (symbol-value symbol))

(defmethod canonical-reference ((section section))
  (make-reference (section-name section) 'section))

(defmethod collect-reachable-objects ((section section))
  (handler-case
      (mapcan (lambda (reference)
                (cons reference (collect-reachable-objects reference)))
              (remove-if-not (lambda (entry)
                               (typep entry 'reference))
                             (section-entries section)))
    (locate-error (e)
      (error "~@<SECTION ~S has an unresolvable reference:~%~A~:@>"
             (section-name section) e))))

(defvar *section*)

(defmethod document-object ((section section) stream)
  (let ((same-package (and (boundp '*section*)
                           (eq *package* (section-package section))))
        (*package* (if *document-normalize-packages*
                       (section-package section)
                       *package*))
        (*readtable* (if *document-normalize-packages*
                         (section-readtable section)
                         *readtable*))
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

(defmethod locate-and-find-source (symbol (locative-type (eql 'section))
                                   locative-args)
  (declare (ignore locative-args))
  (find-definition symbol (swank-variable-dspecs symbol)))


;;;; GLOSSARY-TERM locative

(defun glossary-term-title-or-name (glossary-term)
  (or (glossary-term-title glossary-term)
      (prin1-to-string (glossary-term-name glossary-term))))

(defmethod print-object ((glossary-term glossary-term) stream)
  (print-unreadable-object (glossary-term stream :type t)
    (format stream "~a" (glossary-term-name glossary-term))))

(define-locative-type glossary-term ()
  "Refers to a glossary term defined by DEFINE-GLOSSARY-TERM.")

(defmethod locate-object (symbol (locative-type (eql 'glossary-term))
                          locative-args)
  (declare (ignore locative-args))
  (unless (typep (symbol-value symbol) 'glossary-term)
    (locate-error))
  (symbol-value symbol))

(defmethod document-object ((glossary-term glossary-term) stream)
  (let ((symbol (glossary-term-name glossary-term)))
    (locate-and-print-bullet 'glossary-term () symbol stream
                             :name (glossary-term-title-or-name glossary-term))
    (print-end-bullet stream)
    ;; Don't add a local reference for this because GLOSSARY-TERMs
    ;; must link any number of times.
    (let ((docstring (glossary-term-docstring glossary-term)))
      (when docstring
        (format stream "~%~A~%" (massage-docstring docstring))))))

(defmethod canonical-reference ((glossary-term glossary-term))
  (make-reference (glossary-term-name glossary-term) 'glossary-term))

(defmethod find-source ((glossary-term glossary-term))
  (locate-and-find-source (glossary-term-name glossary-term) 'variable ()))


;;;; LOCATIVE locative

(define-locative-type locative (lambda-list)
  "This is the locative for locatives. When `M-.` is pressed on
  `SOME-NAME` in `(SOME-NAME LOCATIVE)`, this is what makes it
  possible to land at the corresponding DEFINE-LOCATIVE-TYPE form.
  Similarly, `(LOCATIVE LOCATIVE)` leads to this very definition.")

(defmethod locate-object (symbol (locative-type (eql 'locative)) locative-args)
  (when locative-args
    (locate-error "The syntax of the LOCATIVE locative is ~
                   (LOCATIVE <LOCATIVE-TYPE>)."))
  (or (ignore-errors (locative-lambda-list-method-for-symbol symbol))
      (locate-error "~S is not a valid locative." symbol))
  (make-reference symbol (cons locative-type locative-args)))

(defun locative-lambda-list-method-for-symbol (symbol)
  (find-method* #'locative-lambda-list () `((eql ,symbol))))

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
  (find-definition* (locative-lambda-list-method-for-symbol symbol)
                    'locative-lambda-list
                    (swank-method-dspecs 'locative-lambda-list ()
                                         `((eql ,symbol)))))

(add-locative-to-source-search-list 'locative)


;;;; DISLOCATED locative

(define-locative-type dislocated ()
  "Refers to a symbol in a non-specific context. Useful for preventing
  [autolinking][@mgl-pax-explicit-and-autolinking section]. For example, if
  there is a function called `FOO` then

      `FOO`

  will be linked (if *DOCUMENT-LINK-CODE*) to its definition. However,

      [`FOO`][dislocated]

  will not be. With a dislocated locative, LOCATE always fails with a
  LOCATE-ERROR condition. Also see @MGL-PAX-PREVENTING-AUTOLINKING.")

(defmethod locate-object (symbol (locative-type (eql 'dislocated))
                          locative-args)
  (declare (ignore symbol locative-args))
  (locate-error "DISLOCATED can never be located."))


;;;; ARGUMENT locative

(define-locative-type argument ()
  """An alias for DISLOCATED, so the one can refer to an argument of a
  macro without accidentally linking to a class that has the same name
  as that argument. In the following example, [FORMAT][dislocated] may
  link to CL:FORMAT (if we generated documentation for it):

  ```
  "See the FORMAT in DOCUMENT."
  ```

  Since ARGUMENT is a locative, we can prevent that linking by writing:

  ```
  "See the FORMAT argument of DOCUMENT."
  ```""")

(defmethod locate-object (symbol (locative-type (eql 'argument)) locative-args)
  (declare (ignore symbol locative-args))
  (locate-error "ARGUMENT can never be located."))


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

  In the above example, pressing `M-.` on `PAX.EL` will open the
  `src/pax.el` file and put the cursor on its first character. `M-.`
  on `FOO-EXAMPLE` will go to the source location of the `(asdf:system
  locative)` locative.

  When documentation is generated, the entire `src/pax.el` file is
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
  four spaces makes markdown think it's a code block.
  
  INCLUDE is not EXPORTABLE-LOCATIVE-TYPE-P.""")

(defmethod exportable-locative-type-p ((locative-type (eql 'include)))
  nil)

(defmethod locate-object (symbol (locative-type (eql 'include))
                          locative-args)
  (handler-case
      (destructuring-bind (source &key line-prefix header footer
                           header-nl footer-nl) locative-args
        (declare (ignore source line-prefix header footer header-nl footer-nl))
        (make-reference symbol (cons locative-type locative-args)))
    (error ()
      (locate-error "The lambda list of the INCLUDE locative is ~
                     (SOURCE &KEY LINE-PREFIX HEADER FOOTER HEADER-NL ~
                     FOOTER-NL)."))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'include))
                                   locative-args)
  (declare (ignore symbol))
  (multiple-value-bind (file start) (include-region (first locative-args))
    (unless file
      (locate-error))
    `(:location
      (:file ,(namestring file))
      (:position ,(1+ start))
      nil)))

(defmethod locate-and-document (symbol (locative-type (eql 'include))
                                locative-args stream)
  (declare (ignore symbol))
  (destructuring-bind (source &key (line-prefix "") header footer
                       header-nl footer-nl) locative-args
    (let ((text (handler-case
                    (multiple-value-call #'file-subseq
                      (include-region source))
                  (error (e)
                    (warn "~A" e)
                    (princ-to-string e)))))
      (when header
        (format stream "~A" header))
      (when header-nl
        (format stream "~A~%" header-nl))
      (format stream "~A"
              (prefix-lines line-prefix text))
      (when footer
        (format stream "~A" footer))
      (when footer-nl
        (format stream "~A~%" footer-nl)))))

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
           (let ((start (find-source (entry-to-reference start nil)))
                 (end (find-source (entry-to-reference end nil))))
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
;;;      (:position 333) ; or (:offset 1 333)
;;;      (:snippet ""))
(defun check-location (location)
  (unless (listp location)
    (error "Location ~S is not a list." location))
  (unless (eq (first location) :location)
    (error "Location ~S does not start with ~S." location :location))
  (unless (and (location-file location)
               (location-position location))
    (error "Location ~S should contain :POSITION or :OFFSET."
           location)))

(defun location-file (location)
  (second (find :file (rest location) :key #'first)))

(defun location-position (location)
  (let ((position (find :position (rest location) :key #'first)))
    (if position
        (1- (second position))
        (let ((offset (find :offset (rest location) :key #'first)))
          (if offset
              (1- (+ (second offset) (third offset)))
              nil)))))

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


;;;; CLHS locative

(define-locative-type clhs ()
  """Refers to sections in the Common Lisp hyperspec. These have no
  source location so `M-.` will not work. What works is linking. The
  following markdown examples all produce a link to CLHS `3.4`, the
  section 'Lambda Lists', which is in file `03_d.htm`.

  ```
  CLHS `3.4`
  `3.4` CLHS
  [3.4][]
  [`3.4`][]
  [3.4][CLHS]
  [Lambda Lists][clhs]
  [03_d][clhs]
  ```

  The rules of matching sections are the following. If the object of
  the reference is STRING= to the section number string (without the
  trailing dot) or to the name of its file without the `.htm`
  extension, then the reference refers to that section. Else, if the
  object is a case-insensitive substring of the title of some section,
  then the reference refers to the first such section in breadth-first
  order.

  To link to issue and issue summary pages, all of the above markdown
  examples work, just make the object of the reference the name of the
  issue prefixed by `ISSUE:` or `SUMMARY:` as appropriate. For
  example, to refer to the `AREF-1D` issue use `[ISSUE:AREF-1D][clhs]`
  and get [ISSUE:AREF-1D][clhs]. Similary, `[SUMMARY:AREF-1D][clhs]`
  turns into [SUMMARY:AREF-1D][clhs]. Alternatively, matching the name
  of the file also works (`[iss009][clhs]` renders as [iss009][clhs])

  The generated links are relative to *DOCUMENT-HYPERSPEC-ROOT*.

  To detach the discussion from markdown syntax, let's see these cases
  through the programmatic interface.

  ```
  (locate "3.4" 'clhs)
  ==> #<REFERENCE "3.4" CLHS>
  (locate "03_d" 'clhs)
  ==> #<REFERENCE "03_d" CLHS>
  (locate "lambda" 'clhs)
  ==> #<REFERENCE "3.4" CLHS>
  (locate "ISSUE:AREF-1D" 'clhs)
  ==> #<REFERENCE "ISSUE:AREF-1D" CLHS>
  (locate "SUMMARY:AREF-1D" 'clhs)
  ==> #<REFERENCE "SUMMARY:AREF-1D" CLHS>
  ```
  """)

(defmethod locate-object (name (locative-type (eql 'clhs)) locative-args)
  (let ((hyperspec-id (and (endp locative-args)
                           (stringp name)
                           (find-hyperspec-id name))))
    (if hyperspec-id
        (make-reference hyperspec-id 'clhs)
        (locate-error "Cannot find ~S in the CLHS." name))))
