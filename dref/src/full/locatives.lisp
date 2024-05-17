(in-package :dref)

(in-readtable pythonic-string-syntax)

;;;; VARIABLE locative

(define-locative-type variable (&optional initform)
  """Refers to a global special variable.
  INITFORM, or if not specified, the global value of the variable is
  to be used for @PRESENTATION.

  ```cl-transcript
  (dref '*print-length* 'variable)
  ==> #<DREF *PRINT-LENGTH* VARIABLE>
  ```
  
  VARIABLE references do not RESOLVE.""")

(define-definition-class variable variable-dref)

(defmethod dref* (symbol (locative-type (eql 'variable))
                         locative-args)
  (check-locative-args variable locative-args)
  (unless (and (symbolp symbol)
               #+ccl (member (ccl::variable-information symbol)
                             '(:special :constant))
               #+sbcl (member (sb-int:info :variable :kind symbol)
                              '(:special :constant)))
    (locate-error))
  (%make-dref 'variable-dref symbol 'variable))

(defmethod map-definitions (fn name (locative-type (eql 'variable)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod docstring* ((dref variable-dref))
  (documentation* (dref-name dref) 'variable))

(defmethod source-location* ((dref variable-dref))
  (swank-source-location (dref-name dref) 'variable))


;;;; CONSTANT locative

(define-locative-type constant (&optional initform)
  "Refers to a constant variable defined with DEFCONSTANT. INITFORM,
  or if not specified, the value of the constant is included in the
  documentation. The CONSTANT locative is like the VARIABLE locative,
  but it also checks that its object is CONSTANTP.

  CONSTANT references do not RESOLVE.")

(define-definition-class constant constant-dref (variable-dref))

(defmethod dref* (symbol (locative-type (eql 'constant)) locative-args)
  (check-locative-args constant locative-args)
  (unless (and (symbolp symbol)
               (not (keywordp symbol))
               ;; CONSTANTP may detect constant symbol macros, for
               ;; example.
               (boundp symbol)
               (constantp symbol))
    (locate-error "~S is not a non-keyword symbol that's CONSTANTP." symbol))
  (%make-dref 'constant-dref symbol 'constant))

(defun actualize-variable-to-constant (dref)
  (when (eq (dref-locative-type dref) 'variable)
    (dref (dref-name dref)
                 `(constant ,@(xref-locative-args (dref-origin dref)))
                 nil)))

(add-dref-actualizer 'actualize-variable-to-constant)

(defmethod docstring* ((dref constant-dref))
  (documentation* (dref-name dref) 'variable))

(defmethod source-location* ((dref constant-dref))
  (swank-source-location (dref-name dref) 'constant))


;;;; MACRO locative

;;; FIXME: Resolve if we can?
(define-locative-type macro ()
  "Refers to a global macro, typically defined with DEFMACRO, or to a
  [special operator][SPECIAL-OPERATOR-P FUNCTION].

  MACRO references do not RESOLVE.")

(define-definition-class macro macro-dref)

(defmethod dref* (symbol (locative-type (eql 'macro)) locative-args)
  (check-locative-args macro locative-args)
  (unless (and (symbolp symbol)
               (or (macro-function symbol)
                   (special-operator-p* symbol)))
    (locate-error "~S does not name a macro." symbol))
  (%make-dref 'macro-dref symbol (cons locative-type locative-args)))

(defmethod arglist* ((dref macro-dref))
  (alexandria:when-let (fn (macro-function (dref-name dref)))
    (function-arglist fn :macro)))

(defmethod docstring* ((dref macro-dref))
  (documentation* (dref-name dref) 'function))

(defmethod source-location* ((dref macro-dref))
  (let ((symbol (dref-name dref)))
    (alexandria:when-let (fn (macro-function symbol))
      (swank-source-location* fn symbol 'macro))))


;;;; SYMBOL-MACRO locative

(define-locative-type symbol-macro ()
  """Refers to a global symbol macro, defined with DEFINE-SYMBOL-MACRO.
  Note that since DEFINE-SYMBOL-MACRO does not support docstrings, PAX
  defines methods on the DOCUMENTATION generic function specialized on
  `(DOC-TYPE (EQL 'SYMBOL-MACRO))`.

  ```
  (define-symbol-macro my-mac 42)
  (setf (documentation 'my-mac 'symbol-macro)
        "This is MY-MAC.")
  (documentation 'my-mac 'symbol-macro)
  => "This is MY-MAC."
  ```

  SYMBOL-MACRO references do not RESOLVE.""")

(defvar *symbol-macro-docstrings* (make-hash-table :test #'eq))

(define-definition-class symbol-macro symbol-macro-dref)

(defmethod dref* (symbol (locative-type (eql 'symbol-macro))
                         locative-args)
  (check-locative-args symbol-macro locative-args)
  (unless (and (symbolp symbol)
               #+ccl (gethash symbol ccl::*symbol-macros*)
               #+sbcl (sb-int:info :variable :macro-expansion symbol))
    (locate-error "~S does not name a symbol macro." symbol))
  (%make-dref 'symbol-macro-dref symbol (cons locative-type locative-args)))

;;; SWANK-BACKEND:FIND-DEFINITIONS does not support symbol macros on CCL.
#-ccl
(defmethod map-definitions (fn name (locative-type (eql 'symbol-macro)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod documentation ((symbol symbol) (doc-type (eql 'symbol-macro)))
  (gethash symbol *symbol-macro-docstrings*))

(defmethod (setf documentation) (docstring (symbol symbol)
                                 (doc-type (eql 'symbol-macro)))
  (setf (gethash symbol *symbol-macro-docstrings*) docstring))

(defmethod docstring* ((dref symbol-macro-dref))
  (documentation* (dref-name dref) 'symbol-macro))

(defmethod source-location* ((dref symbol-macro-dref))
  (swank-source-location (dref-name dref) 'symbol-macro))


;;;; COMPILER-MACRO locative

;;; FIXME: Resolve if possible?
(define-locative-type compiler-macro ()
  "Refers to a compiler macro, typically defined with
  DEFINE-COMPILER-MACRO.

  COMPILER-MACRO references do not RESOLVE.")

(define-definition-class compiler-macro compiler-macro-dref)

(defmethod dref* (symbol (locative-type (eql 'compiler-macro)) locative-args)
  (check-locative-args compiler-macro locative-args)
  (unless (and (symbolp symbol)
               (compiler-macro-function symbol))
    (locate-error "~S does not name a compiler macro." symbol))
  (%make-dref 'compiler-macro-dref symbol 'compiler-macro))

(defmethod arglist* ((dref compiler-macro-dref))
  (function-arglist (compiler-macro-function (dref-name dref)) :macro))

(defmethod docstring* ((dref compiler-macro-dref))
  (documentation* (dref-name dref) 'compiler-macro))

(defmethod source-location* ((dref compiler-macro-dref))
  (let ((symbol (dref-name dref)))
    (swank-source-location* (compiler-macro-function symbol) symbol
                            'compiler-macro)))


;;;; SETF locative

(define-locative-type setf (&optional method)
  "Refers to a [setf expander][clhs] (see DEFSETF and DEFINE-SETF-EXPANDER)
  or a [setf function][clhs] (e.g. `(DEFUN (SETF NAME) ...)` or the
  same with DEFGENERIC). The format in DEFSECTION is `(<NAME> SETF)`
  in all these cases.

  To refer to methods of a setf generic function, use a METHOD
  locative inside SETF like this:

      (dref 'documentation '(setf (method () (t symbol (eql function))))

  References to setf functions RESOLVE to the function object. Setf
  expander references do not RESOLVE.")

(define-definition-class setf setf-dref)

(defmethod dref* (symbol (locative-type (eql 'setf)) locative-args)
  (check-locative-args setf locative-args)
  (unless (and (symbolp symbol)
               (or (has-setf-p symbol)
                   ;; KLUDGE: On some implementations HAS-SETF-P may
                   ;; return NIL even though there is a
                   ;; DEFINE-SETF-EXPANDER.
                   (documentation* symbol 'setf)))
    (locate-error "~S does not have a SETF expansion." symbol))
  (when locative-args
    (let ((method-locative (first locative-args)))
      (unless (and (listp method-locative)
                   (eq (first method-locative) 'method))
        (locate-error "Only the METHOD locative is supported within SETF."))
      (unless (ignore-errors (find-method* `(setf ,symbol)
                                           (second method-locative)
                                           (third method-locative)))
        (locate-error "METHOD not found."))))
  (%make-dref 'setf-dref symbol (cons locative-type locative-args)))

;;; SWANK-BACKEND:FIND-DEFINITIONS does not support setf on CCL.
#-ccl
(defmethod map-definitions (fn name (locative-type (eql 'setf)))
  (declare (ignore fn name))
  ;; FIXME: setf function, method
  'swank-definitions)

(defmethod resolve* ((dref setf-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (or (if locative-args
            (let ((method-locative (first locative-args)))
              (assert (eq (first method-locative) 'method))
              (ignore-errors
               (find-method* `(setf ,symbol) (second method-locative)
                             (third method-locative))))
            (ignore-errors (fdefinition* `(setf ,symbol))))
        (resolve-error))))

(defmethod arglist* ((dref setf-dref))
  ;; FIXME: Handle setf expanders.
  (let ((fn-or-method (resolve dref nil)))
    (if fn-or-method
        (if (typep fn-or-method 'method)
            (values (method-arglist fn-or-method) :ordinary)
            (function-arglist fn-or-method :ordinary))
        nil)))

(defmethod docstring* ((dref setf-dref))
  (let* ((symbol (dref-name dref))
         (fn-or-method (resolve dref nil)))
    (if fn-or-method
        (documentation* fn-or-method (if (typep fn-or-method 'method)
                                         t
                                         'function))
        (documentation* symbol 'setf))))

(defmethod source-location* ((dref setf-dref))
  (let* ((symbol (dref-name dref))
         (name `(setf ,symbol))
         (fn (ignore-errors (fdefinition name))))
    (if fn
        (swank-source-location* fn  name 'function)
        (swank-source-location symbol 'setf))))


;;;; FUNCTION locative

(define-locative-type function ()
  "Refers to a global function, typically defined with DEFUN. The
  @NAME must be a SYMBOL (see the SETF locative for how to reference
  [setf functions][clhs]). It is also allowed to reference
  GENERIC-FUNCTIONs as FUNCTIONs:

  ```cl-transcript
  (dref 'docstring 'function)
  ==> #<DREF DOCSTRING FUNCTION>
  ```")

(define-definition-class function function-dref)

(defmethod locate* ((function function))
  (multiple-value-bind (name kind) (function-name function)
    (unless name
      (locate-error "~S has no name." function))
    (cond (kind
           (dref name kind))
          (t
           (cond ((and (symbolp name)
                       (eq (macro-function name) function))
                  (dref name 'macro))
                 ((and (symbolp name)
                       (eq (compiler-macro-function name) function))
                  (dref name 'compiler-macro))
                 (t
                  (unless (eq (ignore-errors (fdefinition* name))
                              (unencapsulated-function function))
                    (locate-error "The name ~S does not denote the function."
                                  name))
                  (if (setf-name-p name)
                      (dref (second name) 'setf)
                      (dref name 'function))))))))

(defmethod dref* (symbol (locative-type (eql 'function))
                         locative-args)
  (check-locative-args function locative-args)
  (when (and (symbolp symbol) (macro-function symbol))
    (locate-error "~S names a macro not a function." symbol))
  (unless (ignore-errors (symbol-function* symbol))
    (locate-error "~S is not a symbol naming a function." symbol))
  (%make-dref 'function-dref symbol 'function))

(defmethod arglist* ((dref function-dref))
  (function-arglist (dref-name dref) :ordinary))

(defmethod resolve* ((dref function-dref))
  (symbol-function* (dref-name dref)))

(defmethod docstring* ((dref function-dref))
  (documentation* (symbol-function* (dref-name dref)) 'function))

(defmethod source-location* ((dref function-dref))
  (let ((symbol (dref-name dref)))
    (swank-source-location* (symbol-function* symbol) symbol 'function)))
(defvar end-of-function-example)


;;;; GENERIC-FUNCTION locative

(define-locative-type generic-function ()
  "Refers to a [GENERIC-FUNCTION][class], typically defined with
  DEFGENERIC. The @NAME must be a SYMBOL (see the SETF locative for
  how to reference [setf functions][clhs]).")

(define-definition-class generic-function generic-function-dref (function-dref))

(defmethod locate* ((function generic-function))
  (let ((name (swank-mop:generic-function-name function)))
    (if (setf-name-p name)
        (dref (second name) 'setf)
        (%make-dref 'generic-function-dref name 'generic-function))))

(defmethod dref* (symbol (locative-type (eql 'generic-function))
                         locative-args)
  (check-locative-args generic-function locative-args)
  (let ((function (ignore-errors (symbol-function* symbol))))
    (unless (typep function 'generic-function)
      (locate-error "~S is not a symbol naming a generic function." symbol))
    (%make-dref 'generic-function-dref symbol 'generic-function)))

(defun actualize-function-to-generic-function (dref)
  (when (eq (dref-locative-type dref) 'function)
    (dref (dref-name dref) 'generic-function nil)))

(add-dref-actualizer 'actualize-function-to-generic-function)

(defmethod resolve* ((dref generic-function-dref))
  (symbol-function* (dref-name dref)))

(defmethod docstring* ((dref generic-function-dref))
  (documentation* (dref-name dref) 'function))

(defmethod source-location* ((dref generic-function-dref))
  (let ((symbol (dref-name dref)))
    (swank-source-location* (symbol-function* symbol) symbol
                            'generic-function)))


;;;; METHOD locative

(define-locative-type method (method-qualifiers method-specializers)
  "Refers to METHODs named by SYMBOLs (for SETF methods,
  see the SETF locative). METHOD-QUALIFIERS and METHOD-SPECIALIZERS
  are similar to the CL:FIND-METHOD's arguments of the same names. For
  example, the method

  ```cl-transcript
  (defgeneric foo-gf (x y z)
    (:method :around (x (y (eql 'xxx)) (z string))
      (values x y z)))
  ```

  can be referred to as

  ```cl-transcript
  (dref 'foo-gf '(method (:around) (t (eql xxx) string)))
  ==> #<DREF FOO-GF (METHOD (:AROUND) (T (EQL XXX) STRING))>
  ```

  METHOD is not EXPORTABLE-LOCATIVE-TYPE-P.")

(define-definition-class method method-dref)

(defmethod locate* ((method method))
  (let* ((name (swank-mop:generic-function-name
                (swank-mop:method-generic-function method)))
         (qualifiers (swank-mop:method-qualifiers method))
         (specializers (method-specializers-list method)))
    (%make-dref 'method-dref name `(method ,qualifiers ,specializers))))

(defmethod dref* (name (locative-type (eql 'method)) locative-args)
  (check-locative-args method locative-args)
  (destructuring-bind (qualifiers specializers) locative-args
    (or (and (symbolp name)
             (ignore-errors (find-method* name qualifiers specializers)))
        (locate-error "Method does not exist.")))
  (%make-dref 'method-dref name (cons locative-type locative-args)))

(defmethod map-definitions (fn name (locative-type (eql 'method)))
  (declare (ignore fn name))
  'swank-definitions)

(defun actualize-method-to-accessor-or-setf (dref)
  (when (eq (dref-locative-type dref) 'method)
    (let ((name (dref-name dref)))
      (destructuring-bind (qualifiers specializers) (dref-locative-args dref)
        (cond
          ;; E.g. (METHOD () (T NUMBER))
          ((and (endp qualifiers)
                (= (length specializers) 2)
                (eq (first specializers) t))
           (if (setf-name-p name)
               (dref (second name) `(accessor ,(second specializers))
                            nil)
               (dref name `(writer ,(second specializers)) nil)))
          ((not (setf-name-p name))
           ;; See if (METHOD () (NUMBER)) is an ACCESSOR or READER.
           (and (endp qualifiers) (= (length specializers) 1)
                (or (dref name `(accessor ,(first specializers)) nil)
                    (dref name `(reader ,(first specializers)) nil))))
          ;; It's a SETF but not an ACCESSOR.
          (t
           ;; Fall back on (SETF (METHOD () (T NUMBER)).
           (dref (second name) `(setf ,(dref-locative dref)))))))))

(add-dref-actualizer 'actualize-method-to-accessor-or-setf)

;;; Return the specializers in a format suitable as the second
;;; argument to FIND-METHOD.
(defun method-specializers-list (method)
  (mapcar (lambda (spec)
            (typecase spec
              (swank-mop:eql-specializer
               `(eql ,(swank-mop:eql-specializer-object spec)))
              (t (swank-mop:class-name spec))))
          (swank-mop:method-specializers method)))

;;;; These were lifted from the fancy inspector contrib and then
;;;; tweaked.

;;; Return a "pretty" list of the method's specializers. Normal
;;; specializers are replaced by the name of the class, eql
;;; specializers are replaced by `(EQL ,OBJECT).
(defun method-specializers-for-inspect (method)
  (mapcar (lambda (name spec)
            (if (eq spec t)
                name
                (list name spec)))
          (method-arglist method)
          (method-specializers-list method)))

;;; Returns a "pretty" list describing METHOD. The first element of
;;; the list is the name of generic-function method is specialized on,
;;; the second element is the method qualifiers, the rest of the list
;;; is the method's specializers (as per
;;; METHOD-SPECIALIZERS-FOR-INSPECT).
(defun method-for-inspect-value (method)
  (append (list (swank-mop:generic-function-name
                 (swank-mop:method-generic-function method)))
          (swank-mop:method-qualifiers method)
          (method-specializers-for-inspect method)))

(defmethod resolve* ((dref method-dref))
  (destructuring-bind (qualifiers specializers) (dref-locative-args dref)
    (or (ignore-errors (find-method* (dref-name dref)
                                     qualifiers specializers))
        (resolve-error "Method does not exist."))))

(defmethod arglist* ((dref method-dref))
  (values (method-arglist (resolve dref)) :ordinary))

(defmethod docstring* ((dref method-dref))
  (documentation* (resolve dref) t))

(defmethod source-location* ((dref method-dref))
  (swank-source-location* (resolve dref) (dref-name dref)
                          (dref-locative dref)))


;;;; METHOD-COMBINATION locative

(define-locative-type method-combination ()
  "Refers to a [METHOD-COMBINATION][class], defined with
  DEFINE-METHOD-COMBINATION.

  METHOD-COMBINATION references do not RESOLVE.")

(define-definition-class method-combination method-combination-dref)

(defmethod dref* (symbol (locative-type (eql 'method-combination))
                         locative-args)
  (check-locative-args method-combination locative-args)
  (unless (and (symbolp symbol)
               #+ccl (ccl::method-combination-info symbol)
               #+sbcl (gethash symbol sb-pcl::**method-combinations**))
    (locate-error))
  (%make-dref 'method-combination-dref symbol 'method-combination))

(defmethod map-definitions (fn name (locative-type (eql 'method-combination)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod docstring* ((dref method-combination-dref))
  (documentation* (dref-name dref) 'method-combination))

(defmethod source-location* ((dref method-combination-dref))
  (swank-source-location (dref-name dref) 'method-combination))


;;;; ACCESSOR, READER and WRITER locatives

(define-locative-type accessor (class-name)
  """Refers to an :ACCESSOR in a DEFCLASS:

   ```cl-transcript (:dynenv dref-std-env)
   (defclass foo ()
     ((xxx :accessor foo-xxx)))
   
   (dref 'foo-xxx '(accessor foo))
   ==> #<DREF FOO-XXX (ACCESSOR FOO)>
   ```

   An :ACCESSOR in DEFCLASS creates a reader and a writer method.
   Somewhat arbitrarily, ACCESSOR references RESOLVE to the writer
   method but can be LOCATEd with either.""")

(define-locative-type reader (class-name)
  "Like ACCESSOR, but refers to a :READER method in a DEFCLASS.")

(define-locative-type writer (class-name)
  "Like ACCESSOR, but refers to a :WRITER method in a DEFCLASS.")

(define-definition-class accessor accessor-dref)
(define-definition-class reader reader-dref)
(define-definition-class writer writer-dref)

(defmethod dref* (symbol (locative-type (eql 'accessor))
                         locative-args)
  (check-locative-args accessor locative-args)
  (unless (ignore-errors
           (find-accessor-slot-definition symbol (first locative-args)))
    (locate-error))
  (%make-dref 'accessor-dref symbol (cons locative-type locative-args)))

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

(defmethod dref* (symbol (locative-type (eql 'reader)) locative-args)
  (check-locative-args reader locative-args)
  (cond ((ignore-errors
          (find-accessor-slot-definition symbol (first locative-args)))
         (dref symbol `(accessor ,@locative-args)))
        ((ignore-errors
          (find-reader-slot-definition symbol (first locative-args)))
         (%make-dref 'reader-dref symbol (cons locative-type locative-args)))
        (t
         (locate-error))))

(defun find-reader-slot-definition (reader-symbol class-symbol)
  (when (class-slots-supported-p class-symbol)
    (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
      (when (find reader-symbol (swank-mop:slot-definition-readers slot-def))
        (return-from find-reader-slot-definition slot-def)))
    (locate-error "Could not find reader ~S for class ~S." reader-symbol
                  class-symbol)))

(defmethod dref* (symbol (locative-type (eql 'writer)) locative-args)
  (check-locative-args writer locative-args)
  (cond ((ignore-errors
          (find-accessor-slot-definition symbol (first locative-args)))
         (dref symbol `(accessor ,@locative-args)))
        ((ignore-errors
          (find-writer-slot-definition symbol (first locative-args)))
         (%make-dref 'writer-dref symbol (cons locative-type locative-args)))
        (t
         (locate-error))))

(defun find-writer-slot-definition (accessor-symbol class-symbol)
  (when (class-slots-supported-p class-symbol)
    (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
      (let ((writers (swank-mop:slot-definition-writers slot-def)))
        (when (or (find accessor-symbol writers)
                  (find `(setf ,accessor-symbol) writers :test #'equal))
          (return-from find-writer-slot-definition slot-def))))
    (locate-error "Could not find writer ~S for class ~S."
                  accessor-symbol class-symbol)))

(defmethod map-definitions (fn name (locative-type (eql 'accessor)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod map-definitions (fn name (locative-type (eql 'reader)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod map-definitions (fn name (locative-type (eql 'writer)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod resolve* ((dref accessor-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (find-method* `(setf ,symbol) () (list t (first locative-args)))))

(defmethod resolve* ((dref reader-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (find-method* symbol () (list (first locative-args)))))

(defmethod resolve* ((dref writer-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (find-method* symbol () (list t (first locative-args)))))

(defmethod docstring* ((dref accessor-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (swank-mop:slot-definition-documentation
     (find-accessor-slot-definition symbol (first locative-args)))))

(defmethod docstring* ((dref reader-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (swank-mop:slot-definition-documentation
     (find-reader-slot-definition symbol (first locative-args)))))

(defmethod docstring* ((dref writer-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (swank-mop:slot-definition-documentation
     (find-writer-slot-definition symbol (first locative-args)))))

(defmethod source-location* ((dref accessor-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (swank-source-location* (find-method* symbol ()
                                          (list (first locative-args)))
                            symbol `(accessor ,(first locative-args)))))

(defmethod source-location* ((dref reader-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (swank-source-location* (find-method* symbol ()
                                          (list (first locative-args)))
                            symbol `(reader ,(first locative-args)))))

(defmethod source-location* ((dref writer-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (swank-source-location* (find-method* symbol ()
                                          (list t (first locative-args)))
                            symbol `(writer ,(first locative-args)))))


;;;; STRUCTURE-ACCESSOR locative

(define-locative-type structure-accessor (&optional structure-class-name)
  "Refers to an accessor function generated by DEFSTRUCT.
  A LOCATE-ERROR condition is signalled if the wrong
  STRUCTURE-CLASS-NAME is provided.

  Note that there is no portable way to detect structure accessors,
  and on some platforms, `(LOCATE #'MY-ACCESSOR)`, DEFINITIONS and
  DREF-APROPOS will return FUNCTION references instead. On such
  platforms, STRUCTURE-ACCESSOR references do not RESOLVE.")

(define-definition-class structure-accessor structure-accessor-dref)

(defmethod dref* (symbol (locative-type (eql 'structure-accessor))
                         locative-args)
  (check-locative-args structure-accessor locative-args)
  (unless (and (symbolp symbol)
               (ignore-errors (symbol-function* symbol)))
    (locate-error "~S is not a symbol that names a function." symbol))
  (let ((structure-name* nil))
    #+ccl
    (let ((info (ccl::structref-info symbol)))
      (unless (ccl::accessor-structref-info-p info)
        (locate-error "~S is not a structure accessor."
                      (symbol-function* symbol)))
      (setq structure-name* (cdr info)))
    #+sbcl
    (let ((info (sb-kernel:structure-instance-accessor-p symbol)))
      (unless info
        (locate-error "~S is not a structure accessor."
                      (symbol-function* symbol)))
      (setq structure-name* (slot-value (first info) 'sb-kernel::name)))
    (let ((structure-name (first locative-args)))
      (when (and structure-name structure-name*
                 (not (eq structure-name structure-name*)))
        (locate-error "This accessor is on structure ~S not on ~S."
                      structure-name* structure-name))
      (%make-dref 'structure-accessor-dref symbol
                  (if (or structure-name structure-name*)
                      `(structure-accessor ,(or structure-name
                                                structure-name*))
                      'structure-accessor)))))

(defun actualize-function-to-structure-accessor (dref)
  (when (eq (dref-locative-type dref) 'function)
    (dref (dref-name dref) 'structure-accessor nil)))

#+(or ccl sbcl)
(add-dref-actualizer 'actualize-function-to-structure-accessor)

(defmethod map-definitions (fn name (locative-type (eql 'structure-accessor)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod resolve* ((dref structure-accessor-dref))
  #+(or ccl sbcl)
  (symbol-function* (dref-name dref))
  #-(or ccl sbcl)
  (resolve-error))

(defmethod docstring* ((dref structure-accessor-dref))
  (documentation* (dref-name dref) 'function))

(defmethod source-location* ((dref structure-accessor-dref))
  (let ((symbol (dref-name dref)))
    (swank-source-location* (symbol-function* symbol) symbol 'function)))


;;;; TYPE locative

(define-locative-type type ()
  "This locative can refer to any Lisp type and [compound type
  specifiers][clhs] such as [AND][type]. For types defined with
  DEFTYPE, their ARGLIST is available. [CLASSes][class] and
  [CONDITIONs][type] may be referred to as TYPEs:

  ```cl-transcript
  (dref 'xref 'type)
  ==> #<DREF XREF CLASS>
  ```
  ```cl-transcript
  (dref 'locate-error 'type)
  ==> #<DREF LOCATE-ERROR CONDITION>
  ```

  TYPE references do not RESOLVE.")

(define-definition-class type type-dref)

(defmethod dref* (symbol (locative-type (eql 'type)) locative-args)
  (check-locative-args type locative-args)
  (unless (and (symbolp symbol)
               ;; On most Lisps, SWANK-BACKEND:TYPE-SPECIFIER-P is not
               ;; reliable.
               #-(or abcl allegro clisp cmucl ecl sbcl)
               (swank-backend:type-specifier-p symbol)
               #+sbcl
               (or (find-class symbol nil)
                   (sb-int:info :type :expander symbol)))
    (locate-error "~S is not a valid type specifier." symbol))
  (%make-dref 'type-dref symbol (cons locative-type locative-args)))

(defmethod map-definitions (fn name (locative-type (eql 'type)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod arglist* ((dref type-dref))
  (let ((name (dref-name dref)))
    (alexandria:nth-value-or 1
      #+ccl
      (function-arglist (gethash name ccl::%deftype-expanders%) :deftype)
      (let ((arglist (swank-backend:type-specifier-arglist name)))
        (if (eq arglist :not-available)
            nil
            (values arglist :deftype))))))

(defmethod docstring* ((dref type-dref))
  (documentation* (dref-name dref) 'type))

(defmethod source-location* ((dref type-dref))
  (swank-source-location (dref-name dref) 'type 'class 'condition))


;;;; CLASS locative
;;;;
;;;; Be careful changing this because DREF-EXT::@ADDING-NEW-LOCATIVES
;;;; INCLUDEs the code in a rather fine-grained way.

(define-locative-type class ()
  "Naturally, CLASS is the locative type for [CLASS][class]es.
  [CONDITIONs][type] may be referred to as CLASSes:

  ```cl-transcript
  (dref 'locate-error 'class)
  ==> #<DREF LOCATE-ERROR CONDITION>
  ```")

(define-definition-class class class-dref)

(defmethod locate* ((class class))
  (make-instance 'class-dref :name (class-name class) :locative 'class))

(defmethod dref* (symbol (locative-type (eql 'class)) locative-args)
  (check-locative-args class locative-args)
  (unless (and (symbolp symbol)
               (find-class symbol nil))
    (locate-error "~S does not name a class." symbol))
  (make-instance 'class-dref :name symbol :locative 'class))

(defun actualize-type-to-class (dref)
  (when (eq (dref-locative-type dref) 'type)
    (dref (dref-name dref) 'class nil)))

(add-dref-actualizer 'actualize-type-to-class)

(defmethod resolve* ((dref class-dref))
  (find-class (dref-name dref)))

(defmethod docstring* ((class class))
  (documentation* class t))

(defmethod source-location* ((dref class-dref))
  (swank-source-location* (resolve dref) (dref-name dref) 'class))

(defvar %end-of-class-example)


;;;; CONDITION locative

(define-locative-type condition ()
  "CONDITION is the locative type for [CONDITION][condition]s.")

(define-definition-class condition condition-dref (class-dref))

(defmethod dref* (symbol (locative-type (eql 'condition))
                         locative-args)
  (check-locative-args condition locative-args)
  (let ((class (and (symbolp symbol) (find-class symbol nil))))
    (unless (and class
                 (subtypep class 'condition))
      (locate-error "~S does not name a condition class." symbol))
    (%make-dref 'condition-dref symbol 'condition)))

(defun actualize-class-to-condition (dref)
  (when (eq (dref-locative-type dref) 'class)
    (dref (dref-name dref) 'condition nil)))

(add-dref-actualizer 'actualize-class-to-condition)

(defmethod source-location* ((dref condition-dref))
  (swank-source-location* (resolve dref) (dref-name dref) 'condition))


;;;; DECLARATION locative

(define-locative-type declaration ()
  """Refers to a declaration, used in DECLARE, DECLAIM and PROCLAIM.

  User code may also define new declarations with CLTL2 functionality,
  but there is currently no way to provide a docstring, and their
  ARGLIST is always NIL.

  ```
  (cl-environments:define-declaration my-decl (&rest things)
    (values :declare (cons 'foo things)))
  ```

  DECLARATION references do not RESOLVE.

  Also, SOURCE-LOCATION on declarations currently only works on SBCL.""")

(define-definition-class declaration declaration-dref)

(defvar *ansi-declarations*
  '(compilation-speed debug declaration dynamic-extent ftype ignorable
    ignore inline notinline optimize safety space special speed type))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-cltl2))

(defmethod dref* (symbol (locative-type (eql 'declaration))
                         locative-args)
  (check-locative-args declaration locative-args)
  (unless (and (symbolp symbol)
               (or (find symbol *ansi-declarations*)
                   #+allegro (find symbol (system:declaration-information
                                           'declaration))
                   #+ccl (find symbol (ccl:declaration-information
                                       'declaration))
                   #+sbcl (find symbol (sb-cltl2:declaration-information
                                        'declaration))
                   #-(or allegro ccl sbcl)
                   t))
    (locate-error "~S is not a known declaration." symbol))
  (%make-dref 'declaration-dref symbol 'declaration))

(defmethod map-definitions (fn name (locative-type (eql 'declaration)))
  #+(or ccl sbcl)
  (alexandria:when-let (dref (dref name locative-type nil))
    (funcall fn dref))
  ;; Lacking DECLARATION-INFORMATION form CLTL2 on other Lisps,
  ;; DREF* always succeeds.
  #-(or ccl sbcl)
  (declare (ignore fn name))
  (values))

;;; FIXME: implement a new doc-type for DOCUMENTATION?

(defmethod source-location* ((dref declaration-dref))
  #+sbcl
  (swank-backend:find-source-location
   (sb-int:info :declaration :known (dref-name dref)))
  #-sbcl
  '(:error "Don't know how to find the source location of declarations."))


;;;; RESTART locative

;;; Provide definitions for standard CL restarts.
(define-restart use-value (value)
  "This is the name of the RESTART to which [USE-VALUE][function]
  transfers control.")
(define-restart store-value (value)
  "This is the name of the RESTART to which [STORE-VALUE][function]
  transfers control.")
(define-restart muffle-warning ()
  "This is the name of the RESTART to which [MUFFLE-WARNING][function]
  transfers control.")
(define-restart continue ()
  "This is the name of the RESTART to which [CONTINUE][function]
  transfers control.")
(define-restart abort ()
  "This is the name of the RESTART to which [ABORT][function]
  transfers control.")


;;;; ASDF:SYSTEM locative

(define-locative-type asdf:system ()
  "Refers to a registered ASDF:SYSTEM.
  The @NAME may be anything ASDF:FIND-SYSTEM supports.

  ASDF:SYSTEM is not EXPORTABLE-LOCATIVE-TYPE-P.")

(define-definition-class asdf:system asdf-system-dref)

(defmethod locate* ((system asdf:system))
  (%make-dref 'asdf-system-dref
              (character-string (slot-value system 'asdf::name))
              'asdf:system))

(defmethod dref* (name (locative-type (eql 'asdf:system))
                         locative-args)
  (check-locative-args asdf:system locative-args)
  (let ((name (ignore-errors (string-downcase (string name)))))
    (unless (progn
              #+(or allegro clisp ecl)
              (when (member name (asdf:registered-systems) :test #'string=)
                (asdf:find-system name))
              #-(or allegro clisp ecl)
              (asdf:registered-system name))
      (locate-error "~S does not name an ASDF:SYSTEM." name))
    (%make-dref 'asdf-system-dref (character-string name) 'asdf:system)))

(defmethod map-names (fn (locative-type (eql 'asdf:system)))
  (map nil fn (asdf:registered-systems)))

(defmethod resolve* ((dref asdf-system-dref))
  (handler-bind ((warning #'muffle-warning))
    (asdf:find-system (dref-name dref))))

(defmethod source-location* ((dref asdf-system-dref))
  (let ((system (resolve dref)))
    `(:location
      (:file ,(namestring (asdf/system:system-source-file system)))
      (:position 1)
      (:snippet ,(format nil "defsystem ~S" (dref-name dref))))))


;;;; PACKAGE locative

(define-locative-type package ()
  "Refers to a [PACKAGE][type], defined by DEFPACKAGE or MAKE-PACKAGE.
  The @NAME may be anything FIND-PACKAGE supports.

  PACKAGE is not EXPORTABLE-LOCATIVE-TYPE-P.")

(define-definition-class package package-dref)

(defmethod locate* ((package package))
  (%make-dref 'package-dref (character-string (package-name package))
              'package))

(defmethod dref* (package-designator (locative-type (eql 'package))
                         locative-args)
  (check-locative-args package locative-args)
  (unless (and (or (symbolp package-designator)
                   (stringp package-designator))
               (find-package* package-designator))
    (locate-error "~S does not name a package." package-designator))
  (%make-dref 'package-dref (character-string
                             (package-name (find-package* package-designator)))
              'package))

(defmethod map-names (fn (locative-type (eql 'package)))
  (map nil (lambda (package)
             (funcall fn (package-name package)))
       (list-all-packages)))

(defmethod resolve* ((dref package-dref))
  (find-package* (dref-name dref)))

(defmethod docstring* ((dref package-dref))
  (documentation* (resolve dref) t))

(defmethod source-location* ((dref package-dref))
  (let ((name (dref-name dref)))
    (swank-source-location* (find-package* name)
                            (if (stringp name)
                                (make-symbol name)
                                name)
                            'package)))


;;;; READTABLE locative

(define-locative-type readtable ()
  "Refers to a named [READTABLE][] defined with
  NAMED-READTABLES:DEFREADTABLE, which associates a global name and a
  docstring with the readtable object. The @NAME may be anything
  FIND-READTABLE supports. Unfortunately, SOURCE-LOCATION information
  is not available.

  READTABLE references RESOLVE to FIND-READTABLE on their @NAME.")

(define-definition-class readtable readtable-dref)

(defmethod locate* ((readtable readtable))
  (let ((name (readtable-name readtable)))
    (unless name
      (locate-error "~S is not a NAMED-READTABLE." readtable))
    (%make-dref 'readtable-dref name 'readtable)))

(defmethod dref* (symbol (locative-type (eql 'readtable))
                         locative-args)
  (check-locative-args readtable locative-args)
  (let ((readtable (and (symbolp symbol)
                        (named-readtables:find-readtable symbol))))
    (if readtable
        (%make-dref 'readtable-dref (named-readtables:readtable-name readtable)
                    'readtable)
        (locate-error))))

(defmethod resolve* ((dref readtable-dref))
  (named-readtables:find-readtable (dref-name dref)))

(defmethod docstring* ((dref readtable-dref))
  (documentation* (dref-name dref) 'readtable))

(defmethod source-location* ((dref readtable-dref))
  '(:error "Don't know how find the source location of readtables."))


;;;; LOCATIVE locative

(define-locative-type locative ()
  "This is the locative for @LOCATIVE-TYPEs defined with
  DEFINE-LOCATIVE-TYPE, DEFINE-PSEUDO-LOCATIVE-TYPE and
  DEFINE-LOCATIVE-ALIAS.

  ```
  (first-line (source-location-snippet
               (source-location (dref 'macro 'locative))))
  => \"(define-locative-type macro ()\"
  ```")

(define-definition-class locative locative-dref)

(defmethod dref* (symbol (locative-type (eql 'locative)) locative-args)
  (check-locative-args locative locative-args)
  ;; Faster than calling LOCATIVE-TYPE-LAMBDA-LIST-METHOD-FOR-SYMBOL.
  (when (nth-value 1 (ignore-errors
                      (values (locative-type-lambda-list symbol))))
    (locate-error "~S is not a valid locative." symbol))
  (%make-dref 'locative-dref symbol 'locative))

(defun locative-type-lambda-list-method-for-symbol (symbol)
  (find-method* #'locative-type-lambda-list () `((eql ,symbol))))

(defmethod arglist* ((dref locative-dref))
  (values (locative-type-lambda-list (dref-name dref)) :destructuring))

(defmethod docstring* ((dref locative-dref))
  (multiple-value-bind (arglist docstring package)
      (locative-type-lambda-list (dref-name dref))
    (declare (ignore arglist))
    (values docstring package)))

(defmethod source-location* ((dref locative-dref))
  (let ((symbol (dref-name dref)))
    (swank-source-location* (locative-type-lambda-list-method-for-symbol symbol)
                            'locative-type-lambda-list
                            `(method () ((eql ,symbol))))))


;;;; Finish the job of DEFINE-SYMBOL-LOCATIVE-TYPE

(defmethod arglist* ((dref symbol-locative-dref))
  (let ((symbol (dref-name dref))
        (locative-type (dref-locative-type dref)))
    (values (symbol-lambda-list symbol locative-type) :macro)))

(defmethod docstring* ((dref symbol-locative-dref))
  (let* ((symbol (dref-name dref))
         (locative-type (dref-locative-type dref))
         (method (symbol-lambda-list-method symbol locative-type)))
    (values (documentation method t)
            (nth-value 1 (symbol-lambda-list symbol locative-type)))))

(defmethod source-location* ((dref symbol-locative-dref))
  (let ((symbol (dref-name dref))
        (locative-type (dref-locative-type dref)))
    (swank-source-location*
     (symbol-lambda-list-method symbol locative-type)
     'symbol-lambda-list `(method () ((eql ,symbol) (eql ,locative-type))))))


;;;; UNKNOWN locative

(define-pseudo-locative-type unknown (dspec)
  "This [pseudo locative type][pseudo-locative-types] is to allow PAX
  to work in a limited way with locatives it doesn't know. UNKNOWN
  definitions come from DEFINITIONS, which uses
  SWANK/BACKEND:FIND-DEFINITIONS. The following examples show PAX
  stuffing the Swank dspec `(:DEFINE-ALIEN-TYPE DOUBLE-FLOAT)` into an
  UNKNOWN locative on SBCL.

  ```cl-transcript (:dynenv dref-std-env)
  (definitions 'double-float :locative-types (locative-types))
  ==> (#<DREF DOUBLE-FLOAT CLASS> #<DREF DOUBLE-FLOAT (CLHS TYPE)>
  -->  #<DREF DOUBLE-FLOAT (UNKNOWN (:DEFINE-ALIEN-TYPE DOUBLE-FLOAT))>)
  ```

  ```cl-transcript (:dynenv dref-std-env)
  (dref 'double-float '(unknown (:define-alien-type double-float)))
  ==> #<DREF DOUBLE-FLOAT (UNKNOWN (:DEFINE-ALIEN-TYPE DOUBLE-FLOAT))>
  ```

  ARGLIST and DOCSTRING return NIL for UNKNOWNs, but SOURCE-LOCATION
  works.")

(define-definition-class unknown unknown-dref)

(defmethod dref* (name (locative-type (eql 'unknown)) locative-args)
  (unless (and (symbolp name)
               (find (first locative-args) (swank-dspecs name) :test #'equal))
    (locate-error))
  (%make-dref 'unknown-dref name `(unknown ,@locative-args)))

(defmethod map-definitions (fn name (locative-type (eql 'unknown)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod source-location* ((dref unknown-dref))
  (let ((dspec-and-location-list (swank-dspecs-and-locations (dref-name dref)))
        (dspec (first (dref-locative-args dref))))
    (second (find dspec dspec-and-location-list :key #'first :test #'equal))))


;;;; LAMBDA locative

(define-pseudo-locative-type lambda
    (&key arglist arglist-type docstring docstring-package
          file file-position snippet
          &allow-other-keys)
  """A [pseudo locative type][ pseudo-locative-types] that carries its
  ARGLIST, DOCSTRING and SOURCE-LOCATION in the locative itself. See
  MAKE-SOURCE-LOCATION for the description of FILE, FILE-POSITION, and
  SNIPPET. LAMBDA references do not RESOLVE. The @NAME must be NIL.

  ```cl-transcript (:dynenv dref-std-env)
  (arglist (dref nil '(lambda :arglist ((x y) z)
                                     :arglist-type :macro)))
  => ((X Y) Z)
  => :MACRO
  ```

  ```cl-transcript (:dynenv dref-std-env)
  (docstring (dref nil '(lambda :docstring "xxx"
                                       :docstring-package :dref)))
  => "xxx"
  ==> #<PACKAGE "DREF">
  ```

  ```cl-transcript (:dynenv dref-std-env)
  (source-location-file
   (source-location (dref nil '(lambda :file "xxx.el"))))
  => "xxx.el"
  ```

  See the PAX:INCLUDE locative for an example.""")

(define-definition-class lambda lambda-dref)

(defmethod dref* (name (locative-type (eql 'lambda)) locative-args)
  (check-locative-args lambda locative-args)
  (when name
    (locate-error "The name ~S is not NIL." name))
  (%make-dref 'lambda-dref name (cons locative-type locative-args)))

(defmethod arglist* ((dref lambda-dref))
  (let ((arglist (getf (dref-locative-args dref) :arglist '%not-there))
        (arglist-type (getf (dref-locative-args dref) :arglist-type t)))
    (unless (eq arglist '%not-there)
      (values arglist arglist-type))))

(defmethod docstring* ((dref lambda-dref))
  (let ((docstring (getf (dref-locative-args dref) :docstring '%not-there))
        (package (getf (dref-locative-args dref) :docstring-package)))
    (unless (eq docstring '%not-there)
      (values docstring (find-package* package)))))

(defmethod source-location* ((dref lambda-dref))
  (let* ((args (dref-locative-args dref))
         (file (getf args :file))
         (file-position (getf args :file-position))
         (snippet (getf args :snippet)))
    (make-source-location :file file :file-position file-position
                          :snippet snippet)))
