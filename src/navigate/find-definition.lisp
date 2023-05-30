(in-package :mgl-pax)

(defsection @extending-find-source (:title "Extending FIND-SOURCE")
  "The following utilities are for writing new FIND-SOURCE and
  LOCATE-AND-FIND-SOURCE methods. Their locative arguments are
  translated to Swank `dspecs`, and it is an error if there is no
  translation. In general, Swank supports Common Lisp
  definitions (hence the VARIABLE and FUNCTION locatives, for example)
  but not \PAX- and user-defined additions (e.g. SECTION,
  ASDF:SYSTEM)."
  (find-definition function)
  (find-definition* function))

(defun/autoloaded find-definition (object &rest locatives)
  "Return a Swank source location for a definition of OBJECT. Try
  forming @REFERENCEs with OBJECT and one of LOCATIVES. Stop at the
  first locative with which a definition is found, and return its
  location. If no location was found, then return the usual
  Swank `(:ERROR ...)`. The implementation is based on the rather
  expensive SWANK-BACKEND:FIND-DEFINITIONS function."
  (let* ((dspecs (loop for locative in locatives
                       collect (reference-to-dspec* object locative)))
         (dspec-and-location-list
           (mapcar (lambda (dspec-and-location)
                     (list (normalize-dspec (first dspec-and-location))
                           (second dspec-and-location)))
                   (swank-find-definitions object)))
         (entry (loop for dspec in dspecs
                        thereis (find dspec dspec-and-location-list
                                      :key #'first :test #'match-dspec))))
    (if entry
        (second entry)
        `(:error (format nil "Could not find source location for ~S."
                         dspecs)))))

(defun/autoloaded find-definition* (object reference-object &rest locatives)
  "Like FIND-DEFINITION, but tries to get the definition of
  OBJECT (for example a FUNCTION or METHOD object) with the fast but
  not widely supported SWANK-BACKEND:FIND-SOURCE-LOCATION before
  calling the much slower but more complete
  SWANK-BACKEND:FIND-DEFINITIONS."
  (or (find-source-location object)
      (apply #'find-definition reference-object locatives)))

(defun find-source-location (object)
  (when object
    (let ((location (swank-backend:find-source-location
                     (if (functionp object)
                         (unencapsulated-function object)
                         object))))
      (when (and (listp location)
                 (eq (first location) :location))
        location))))

(defun reference-to-dspec (reference)
  (reference-to-dspec* (reference-object reference)
                       (reference-locative reference)))

(defun reference-to-dspec* (name locative)
  (let ((type (locative-type locative))
        (args (locative-args locative)))
    (ecase type
      (variable (swank-variable-dspec name))
      (constant (swank-constant-dspec name))
      (macro (swank-macro-dspec name))
      (compiler-macro (swank-compiler-macro-dspec name))
      (symbol-macro (swank-symbol-macro-dspec name))
      (function (swank-function-dspec name))
      (generic-function (swank-generic-function-dspec name))
      (method (swank-method-dspec name (first args) (second args)))
      (accessor (swank-accessor-dspec name (first args) t))
      (reader (swank-accessor-dspec name (first args) nil))
      (writer (swank-accessor-dspec name (first args) t))
      (method-combination (swank-method-combination-dspec name))
      (type (swank-type-dspec name))
      (class (swank-class-dspec name))
      (condition (swank-condition-dspec name))
      (package (swank-package-dspec name)))))

(defmacro define-dspec (name lambda-list &body body)
  (multiple-value-bind (clauses declarations) (alexandria:parse-body body)
    (let ((dspec-forms (loop for (feature-expr dspec-form) on clauses
                             by #'cddr
                             when (alexandria:featurep feature-expr)
                               collect dspec-form)))
      (assert (<= (length dspec-forms) 1))
      `(defun ,name ,lambda-list
         ,@declarations
         ,(if dspec-forms
              (first dspec-forms)
              (progn
                (format *error-output* "!!! No definition for ~S." name)
                `'(,(gensym "NOTIMPLEMENTED"))))))))

(defun match-dspec (dspec-prefix dspec)
  (and (listp dspec)
       (alexandria:starts-with-subseq dspec-prefix dspec :test #'equal)))

#-(or allegro ecl)
(defun normalize-dspec (dspec)
  dspec)

#+allegro
(defun normalize-dspec (dspec)
  ;; (DEFVAR *FOO*) without a global binding is a
  ;; :SPECIAL-DECLARATION.
  (if (and (listp dspec) (eq (first dspec) :special-declaration))
      (cons :variable (rest dspec))
      dspec))

#+ecl
(defun normalize-dspec (dspec)
  ;; (DEFMETHOD TEST-GF (X NUMBER) (Y (EQL 'F)) => (DEFMETHOD TEST-GF
  ;; NUMBER (EQL F)) so that it matches SWANK-METHOD-DSPEC.
  (flet ((remove-arg-name (specifier)
           (if (and (listp specifier)
                    (= (length specifier) 2))
               (second specifier)
               specifier))
         (remove-quote-from-eql (specifier)
           (if (and (listp specifier)
                    (= (length specifier) 2)
                    (eq (first specifier) 'eql)
                    ;; OBJ is (QUOTE F) in the above example.
                    (let ((obj (second specifier)))
                      (and (listp obj)
                           (= (length obj) 2)
                           (eq (first obj) 'quote))))
               `(eql ,(second (second specifier)))
               specifier)))
    (case (first dspec)
      ((defmethod)
       (list* 'defmethod (second dspec)
              (mapcar (lambda (specifier)
                        (remove-quote-from-eql
                         (remove-arg-name specifier)))
                      (cddr dspec))))
      ((defparameter)
       (cons 'defvar (rest dspec)))
      (t
       dspec))))

(define-dspec swank-variable-dspec (name)
  (:or :abcl :ecl :sbcl) `(defvar ,name)
  :allegro `(:variable ,name)
  :ccl `(variable ,name)
  :cmucl `(variable :special ,name))

(define-dspec swank-constant-dspec (name)
  (:or :abcl :ecl :sbcl) `(defconstant ,name)
  :allegro `(:variable ,name)
  #+ccl :ccl #+ccl `(ccl::constant ,name)
  :cmucl `(variable :constant ,name))

(define-dspec swank-macro-dspec (name)
  (:or :abcl :ecl :cmucl :sbcl) `(defmacro ,name)
  :allegro `(:operator ,name)
  :ccl `(function ,name))

(define-dspec swank-compiler-macro-dspec (name)
  (:or :abcl :ecl :cmucl :sbcl) `(define-compiler-macro ,name)
  :allegro `(:compiler-macro ,name)
  :ccl `(compiler-macro ,name))

(define-dspec swank-symbol-macro-dspec (name)
  (:or :abcl :ecl :sbcl) `(define-symbol-macro ,name)
  :allegro `(:symbol-macro ,name)
  ;; Symbol macros are not currently supported by Swank, so this is
  ;; moot.
  :ccl `(symbol-macro ,name)
  :cmucl `(variable :macro ,name))

(define-dspec swank-function-dspec (name)
  #+ccl (declare (ignore name))
  (:or :abcl :ecl :sbcl) `(defun ,name)
  :allegro `(:operator ,name)
  :cmucl `(function ,name)
  ;; (PAX::FIND-DSPECS 'FUNCTION)
  ;; => ((CCL:DEFINITION-TYPE FUNCTION)
  ;;     (FUNCTION CCL::NX1-FUNCTION))
  ;;
  ;; (MATCH-DSPEC '(FUNCTION) '(FUNCTION CCL::NX1-FUNCTION))
  ;; => T
  :ccl `(function))

(define-dspec swank-generic-function-dspec (name)
  (:or :abcl :cmucl :ecl :sbcl) `(defgeneric ,name)
  :allegro `(:operator ,name)
  :ccl `(function ,name))

;;; QUALIFIERS and SPECIALIZERS are straight from the DEFMETHOD form.
;;; That is, SPECIALIZERS contains names such as NUMBER or (EQL 7),
;;; not class objects and eql specializer objects.
(define-dspec swank-method-dspec (name qualifiers specializers)
  (:or :abcl :clisp :ecl :sbcl) `(defmethod ,name ,@qualifiers ,@specializers)
  :allegro `(:operator (method ,name ,@qualifiers ,specializers))
  :ccl `(:method ,name ,@qualifiers
          ,(mapcar 'specializer-to-object specializers))
  :cmucl `(method ,name ,@qualifiers ,specializers))

(define-dspec swank-accessor-dspec (name class-name writerp)
  :allegro `(:type (method ,name (,@(when writerp '(t)) ,class-name)))
  #+ccl :ccl #+ccl `(,(if writerp
                          'ccl::writer-method
                          'ccl::reader-method)
                     (:method ,name
                       (,@(when writerp (list (find-class t)))
                        ,(find-class class-name))))
  :cmucl `(method ,name () (,@(when writerp '(t)) ,class-name))
  :sbcl `(defmethod ,name ,@(when writerp '(t)) ,class-name))

(define-dspec swank-method-combination-dspec (name)
  :allegro `(:define-method-combination ,name)
  :ccl `(method-combination ,name)
  :sbcl `(define-method-combination ,name))

(define-dspec swank-type-dspec (name)
  (:or :abcl :cmucl :sbcl) `(deftype ,name)
  :allegro `(:type ,name)
  :ccl `(type ,name))

(define-dspec swank-class-dspec (name)
  (:or :abcl :cmucl :ecl) `(defclass ,name)
  :allegro `(:type ,name)
  :ccl `(class ,name)
  :sbcl (if (and (valid-type-specifier-p name)
                 (subtypep name 'structure-object))
            `(defstruct ,name)
            `(defclass ,name)))

(define-dspec swank-condition-dspec (name)
  :sbcl `(define-condition ,name)
  (:or :abcl :ecl) `(defclass ,name)
  :allegro `(:type ,name)
  (:or :ccl :cmucl) `(class ,name))

(define-dspec swank-package-dspec (name)
  ;; Name may be a string or a symbol. MATCH-DSPEC wouldn't work if we
  ;; had NAME in the prefix.
  (declare (ignore name))
  :ccl `(package)
  :sbcl `(defpackage))


;;; An almost inverse of REFERENCE-TO-DSPEC, which is not invertible
;;; in some cases (for example, in CCL the dspecs of GENERIC-FUNCTION,
;;; FUNCTION, and MACRO are the same). So, we may need to look at the
;;; current definitions of NAME to disambiguate.
(defun dspec-to-reference (dspec name)
  (or (loop named lazy-wasteful-parsing
            for (locative-type dspec-prefix)
              in `((variable ,(swank-variable-dspec name))
                   (constant ,(swank-constant-dspec name))
                   (function ,(swank-function-dspec name))
                   (macro ,(swank-macro-dspec name))
                   (compiler-macro ,(swank-compiler-macro-dspec name))
                   (symbol-macro ,(swank-symbol-macro-dspec name))
                   (generic-function ,(swank-generic-function-dspec name))
                   (method-combination ,(swank-method-combination-dspec name))
                   (type ,(swank-type-dspec name))
                   (class ,(swank-class-dspec name))
                   (condition ,(swank-condition-dspec name))
                   (package ,(swank-package-dspec name)))
            do (when (match-dspec dspec-prefix dspec)
                 #+allegro
                 (when (and (eq locative-type 'variable)
                            (constantp name))
                   (setq locative-type 'constant))
                 #+(or allegro ccl cmucl ecl)
                 (when (eq locative-type 'function)
                   (cond ((or (macro-function name)
                              (special-operator-p* name))
                          (setq locative-type 'macro))
                         ((typep (ignore-errors (symbol-function name))
                                 'generic-function)
                          (setq locative-type 'generic-function))))
                 #+allegro
                 (when (and (eq locative-type 'type)
                            (find-class name nil))
                   (setq locative-type 'class))
                 #+(or abcl allegro ccl cmucl ecl)
                 (when (eq locative-type 'class)
                   (when (subtypep name 'condition)
                     (setq locative-type 'condition)))
                 (return-from lazy-wasteful-parsing
                   (actualize-swank-reference
                    (make-reference name locative-type)))))
      (method-dspec-to-reference dspec)))

;;; From SWANK-BACKEND:FIND-DEFINITIONS, we get SECTIONs and
;;; GLOSSARY-TERMs as VARIABLEs. FIXME: This is not extensible.
(defun actualize-swank-reference (reference)
  (let ((object (reference-object reference))
        (locative-type (reference-locative-type reference)))
    (case locative-type
      ((variable)
       (cond ((locate object 'section :errorp nil)
              (make-reference object 'section))
             ((locate object 'glossary-term :errorp nil)
              (make-reference object 'glossary-term))
             (t
              reference)))
      ((type)
       (cond ((locate object 'condition :errorp nil)
              (make-reference object 'condition))
             ((locate object 'class :errorp nil)
              (make-reference object 'class))
             (t
              reference)))
      ((class)
       (cond ((locate object 'condition :errorp nil)
              (make-reference object 'condition))
             (t
              reference)))
      (t
       reference))))

;;; METHOD-DSPEC-TO-REFERENCE is the inverse of SWANK-METHOD-DSPEC
;;; and SWANK-ACCESSOR-DSPEC. To be able to do that, sometimes it
;;; needs to look at the current definitions in the running Lisp.
#-(or allegro ccl cmucl)
(defun method-dspec-to-reference (dspec)
  (when (eq (first dspec) 'defmethod)
    (multiple-value-bind (qualifiers specializers)
        (parse-dspec-method-qualifiers-and-specializers (nthcdr 2 dspec))
      (let ((name (second dspec)))
        (or (recognize-accessor-methods name qualifiers specializers)
            (make-reference (second dspec)
                            `(method ,qualifiers ,specializers)))))))

;;; (:AFTER (EQL 5) CLASS-NAME) => (:AFTER) ((EQL 5) CLASS-NAME)
;;; (:AFTER ((EQL 5) CLASS-NAME)) => (:AFTER) (((EQL 5) CLASS-NAME))
(defun parse-dspec-method-qualifiers-and-specializers (list)
  (let ((qualifiers ())
        (specializers ()))
    (setq qualifiers (loop for rest on list
                           while (keywordp (first rest))
                           collect (first rest)
                           finally (setq specializers rest)))
    (values qualifiers specializers)))

(defun recognize-accessor-methods (method-name qualifiers specializers)
  (when (endp qualifiers)
    (cond ((and (= (length specializers) 1)
                (locate method-name `(reader ,(first specializers))
                        :errorp nil))
           (make-reference method-name `(reader ,(first specializers))))
          ((and (= (length specializers) 2)
                (eq (first specializers) t))
           (cond ((locate method-name `(writer ,(second specializers))
                          :errorp nil)
                  (make-reference method-name
                                  `(writer ,(second specializers))))
                 ((locate method-name `(accessor ,(second specializers))
                          :errorp nil)
                  (make-reference method-name
                                  `(accessor ,(second specializers)))))))))

#+allegro
(defun method-dspec-to-reference (dspec)
  ;; This screams for a pattern matcher.
  ;; (:OPERATOR (METHOD FOO :AFTER ((EQL 5) CLASS-NAME)))
  ;; (:TYPE (METHOD FOO (T CLASS-NAME)))
  (when (and (listp dspec)
             (member (first dspec) '(:operator :type))
             (= (length dspec) 2))
    (let ((operatorp (eq (first dspec) :operator))
          (dspec (second dspec)))
      ;; (METHOD FOO :AFTER ((EQL 5) CLASS-NAME))
      ;; (METHOD FOO (T CLASS-NAME))
      (when (and (listp dspec)
                 (eq (first dspec) 'method)
                 (<= 3 (length dspec)))
        (multiple-value-bind (qualifiers specializers)
            (parse-dspec-method-qualifiers-and-specializers (nthcdr 2 dspec))
          (when (= 1 (length specializers))
            (let ((specializers (first specializers))
                  (name (second dspec)))
              (or (and (not operatorp)
                       (recognize-accessor-methods name qualifiers
                                                   specializers))
                  (make-reference name `(method ,qualifiers
                                                ,specializers))))))))))

#+ccl
(defun method-dspec-to-reference (dspec)
  (cond ((eq (first dspec) :method)
         (multiple-value-bind (qualifiers specializers)
             (parse-dspec-method-qualifiers-and-specializers (nthcdr 2 dspec))
           (when (= (length specializers) 1)
             (make-reference (second dspec)
                             `(method ,qualifiers
                                      ,(objects-to-specializers
                                        (first specializers)))))))
        ((eq (first dspec) 'ccl::reader-method)
         ;; E.g. (CCL::READER-METHOD (:METHOD FOO (#<STANDARD-CLASS CCC>)))
         (destructuring-bind (method-keyword method-name classes) (second dspec)
           (when (eq method-keyword :method)
             (make-reference method-name
                             `(reader ,@(mapcar #'class-name classes))))))
        ((eq (first dspec) 'ccl::writer-method)
         ;; E.g. (CCL::WRITER-METHOD (:METHOD FOO (#<BUILT-IN-CLASS T>
         ;; #<STANDARD-CLASS CCC>)))
         (destructuring-bind (method-keyword method-name classes) (second dspec)
           (when (and (eq method-keyword :method)
                      (= (length classes) 2))
             (let ((specializers (mapcar #'class-name classes)))
               (or (recognize-accessor-methods method-name () specializers)
                   (make-reference method-name
                                   `(writer ,(second specializers))))))))))

#+cmucl
(defun method-dspec-to-reference (dspec)
  (when (and (listp dspec)
             (eq (first dspec) 'method))
    (let ((name (second dspec)))
      (if (and (= (length dspec) 4)
               (null (third dspec)))
          (let ((specializers (fourth dspec)))
            (when (listp specializers)
              (if (eq (first specializers) t)
                  ;; (METHOD FOO-WRITER () (T CLASS-NAME))
                  (or (recognize-accessor-methods name () specializers)
                      (make-reference name `(writer ,(second specializers))))
                  ;; (METHOD FOO-READER () (CLASS-NAME))
                  (make-reference name `(reader ,(first specializers))))))
          ;; (METHOD FOO :AFTER ((EQL 5) CLASS-NAME))
          (multiple-value-bind (qualifiers specializers)
              (parse-dspec-method-qualifiers-and-specializers (nthcdr 2 dspec))
            (when (= (length specializers) 1)
              (make-reference name `(method ,qualifiers
                                            ,(first specializers)))))))))


;;; This can be awfully slow because it may READ sources to get the
;;; source locations. The rest below, on the other hand, are Lisp
;;; implementation specific Swank implementations of
;;; SWANK-BACKEND:FIND-DEFINITIONS modified not to read files or
;;; buffers.
#-sbcl
(defun find-dspecs (name)
  (mapcar #-abcl #'first
          #+abcl (lambda (dspec-and-location)
                   (if (eq (first dspec-and-location) :primitive)
                       '(function)
                       (first dspec-and-location)))
          (swank-find-definitions name)))

#+sbcl
(defun find-dspecs (name)
  (loop for type in swank/sbcl::*definition-types* by #'cddr
        for defsrcs = (sb-introspect:find-definition-sources-by-name name type)
        for filtered-defsrcs
          = (if (eq type :generic-function)
                (remove :invalid defsrcs
                        :key #'swank/sbcl::categorize-definition-source)
                defsrcs)
        append (loop for defsrc in filtered-defsrcs
                     collect (swank/sbcl::make-dspec type name defsrc))))
