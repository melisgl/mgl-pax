;;;; After this file is loaded, the rest of PAX can be written using
;;;; DEFSECTION.

(in-package :mgl-pax)

;;; Should this remove docstrings of referenced things?
(defvar *discard-documentation-p* nil
  "The default value of DEFSECTION's DISCARD-DOCUMENTATION-P argument.
  One may want to set *DISCARD-DOCUMENTATION-P* to true before
  building a binary application.")

(defmacro defsection (name (&key (package '*package*) (readtable '*readtable*)
                            (export t) title link-title-to
                            (discard-documentation-p *discard-documentation-p*))
                      &body entries)
  "Define a documentation section and maybe export referenced symbols.
  A bit behind the scenes, a global variable with NAME is defined and
  is bound to a [SECTION][class] object. By convention, section names
  start with the character `@`. See @TUTORIAL for an example.

  ##### Entries

  ENTRIES consists of docstrings and references in any order.
  Docstrings are arbitrary strings in markdown format.

  REFERENCES are given in the form `(OBJECT LOCATIVE)`. For example,
  `(FOO FUNCTION)` refers to the function `FOO`, `(@BAR SECTION)` says
  that `@BAR` is a subsection of this one. `(BAZ (METHOD () (T T T)))`
  refers to the default method of the three argument generic function
  `BAZ`. `(FOO FUNCTION)` is equivalent to `(FOO (FUNCTION))`. See
  @LOCATIVES-AND-REFERENCES for more.

  The same object may occur in multiple references, typically with
  different locatives, but this is not required.

  The references are not looked up (see RESOLVE in the
  @LOCATIVES-AND-REFERENCES-API) until documentation is generated, so
  it is allowed to refer to things yet to be defined.

  ##### Exporting

  If EXPORT is true (the default), NAME and the @OBJECTs of references
  among ENTRIES which are SYMBOLs are candidates for exporting. A
  candidate symbol is exported if

  - it is [accessible][find-symbol] in PACKAGE, and

  - there is a reference to it in the section being defined which is
    approved by EXPORTABLE-REFERENCE-P.

  See DEFINE-PACKAGE if you use the export feature. The idea with
  confounding documentation and exporting is to force documentation of
  all exported symbols.

  ##### Misc

  TITLE is a string containing markdown or NIL. If non-NIL, it
  determines the text of the heading in the generated output.
  LINK-TITLE-TO is a reference given as an `(OBJECT LOCATIVE)` pair or
  NIL, to which the heading will link when generating HTML. If not
  specified, the heading will link to its own anchor.

  When DISCARD-DOCUMENTATION-P (defaults to *DISCARD-DOCUMENTATION-P*)
  is true, ENTRIES will not be recorded to save memory."
  ;; Let's check the syntax as early as possible.
  (transform-entries entries name)
  (transform-link-title-to link-title-to)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (when ,export
         (export-some-symbols ',name ',entries ,package)))
     (defparameter ,name
       (make-instance 'section
                      :name ',name
                      :package ,package
                      :readtable ,readtable
                      :title ,title
                      :link-title-to (transform-link-title-to ',link-title-to)
                      :entries ,(if discard-documentation-p
                                    ()
                                    `(transform-entries ',entries ',name))))))


(defclass reference ()
  ((object :initarg :object :reader reference-object)
   (locative :initarg :locative :reader reference-locative))
  (:documentation "A REFERENCE represents a path (REFERENCE-LOCATIVE)
  to take from an object (REFERENCE-OBJECT)."))

;;; Canonicalize it a bit for easier comparison. E.g. (FUNCTION) =>
;;; FUNCTION.
(declaim (inline normalize-locative))
(defun normalize-locative (locative)
  (if (and (listp locative)
           (null (cdr locative)))
      (first locative)
      locative))

(defun make-reference (object locative)
  (make-instance 'reference :object object
                 :locative (normalize-locative locative)))

(defmethod print-object ((object reference) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S ~S" (reference-object object)
            (reference-locative object))))

(defun reference= (reference-1 reference-2)
  (and (reference-object= (reference-object reference-1) reference-2)
       (equal (reference-locative reference-1)
              (reference-locative reference-2))))

(declaim (inline reference-locative-type))
(defun reference-locative-type (reference)
  (locative-type (reference-locative reference)))

(declaim (inline reference-locative-args))
(defun reference-locative-args (reference)
  (locative-args (reference-locative reference)))

;;; This could eventually be turned into a generic function
;;; dispatching on LOCATIVE-TYPE.
(defun reference-object= (object reference)
  (let ((object-2 (reference-object reference))
        (locative-type (reference-locative-type reference)))
    (cond ((member locative-type '(package clhs))
           (equal (string object) (string object-2)))
          ((eq locative-type 'asdf:system)
           (equalp (string object) (string object-2)))
          (t
           (eq object object-2)))))

(defun locative-type (locative)
  "Return the first element of LOCATIVE if it's a list. If it's a symbol,
  then return that symbol itself."
  (if (listp locative)
      (first locative)
      locative))

(defun locative-args (locative)
  "The REST of LOCATIVE if it's a list. If it's a symbol then it's
  NIL."
  (if (listp locative)
      (rest locative)
      ()))


(defclass section ()
  ((name
    :initarg :name :reader section-name
    :documentation "The name of the global variable whose value is
    this SECTION object.")
   (package
    :initarg :package :reader section-package
    :documentation "*PACKAGE* will be bound to this package when
    generating documentation for this section.")
   (readtable
    :initarg :readtable :reader section-readtable
    :documentation "*READTABLE* will be bound to this when generating
    documentation for this section.")
   (title
    :initarg :title :reader section-title
    :documentation "A markdown string or NIL. Used in generated
    documentation.")
   (link-title-to
    :initform nil
    :initarg :link-title-to :reader section-link-title-to
    :documentation "A REFERENCE or NIL. Used in generated documentation.")
   (entries
    :initarg :entries :reader section-entries
    :documentation "A list of markdown docstrings and REFERENCE
    objects in the order they occurred in DEFSECTION."))
  (:documentation "DEFSECTION stores its NAME, TITLE, [PACKAGE][type],
  [READTABLE][type] and ENTRIES arguments in [SECTION][class]
  objects."))

(defmethod print-object ((section section) stream)
  (print-unreadable-object (section stream :type t)
    (format stream "~S" (section-name section))))

(defun transform-entries (entries section-name)
  (mapcar (lambda (entry)
            (if (stringp entry)
                entry
                (entry-to-reference entry section-name)))
          entries))

(defun entry-to-reference (entry section-name)
  (handler-case
      (destructuring-bind (object locative) entry
        (make-reference object locative))
    (error ()
      (error "~@<Malformed entry ~A in the definition of SECTION ~A. ~
               Entries must be of the form (SYMBOL LOCATIVE).~:@>"
             ;; Force symbols to be fully qualified so that M-. works
             ;; in the Slime debugger.
             (prin1-to-string/fully-qualified entry)
             (prin1-to-string/fully-qualified section-name)))))

(defun prin1-to-string/fully-qualified (object)
  (let ((*package* (find-package :keyword)))
    (prin1-to-string object)))

(defun transform-link-title-to (link-title-to)
  (when link-title-to
    (if (typep link-title-to 'reference)
        link-title-to
        (apply #'make-reference link-title-to))))


;;;; Exporting

(defun export-some-symbols (name entries package)
  (when (exportablep package name 'section)
    (export name package))
  (dolist (entry entries)
    (when (listp entry)
      (destructuring-bind (object locative) entry
        (when (and (symbolp object)
                   (exportablep package object locative))
          (export object package))))))

(defun exportablep (package symbol locative)
  (and (symbol-accessible-in-package-p symbol package)
       (exportable-reference-p package symbol (locative-type locative)
                               (locative-args locative))))

(defun symbol-accessible-in-package-p (symbol package)
  (eq symbol (find-symbol (symbol-name symbol) package)))

(defgeneric exportable-reference-p (package symbol locative-type locative-args)
  (:documentation "Return true iff SYMBOL is to be exported from
  PACKAGE when it occurs in a DEFSECTION as a reference with
  LOCATIVE-TYPE and LOCATIVE-ARGS. SYMBOL is [accessible][find-symbol]
  in PACKAGE.

  The default method calls EXPORTABLE-LOCATIVE-TYPE-P with
  LOCATIVE-TYPE and ignores the other arguments.

  By default, SECTIONs and GLOSSARY-TERMs are not exported although
  they are EXPORTABLE-LOCATIVE-TYPE-P. To export symbols naming
  section from MGL-PAX, the following method could be added:

  ```
  (defmethod exportable-reference-p ((package (eql (find-package 'mgl-pax)))
                                     symbol (locative-type (eql 'section))
                                     locative-args)
    t)
  ```")
  (:method (package symbol locative-type locative-args)
    (declare (ignore package symbol locative-args))
    (exportable-locative-type-p locative-type)))

(defmethod exportable-reference-p (package symbol
                                   (locative-type (eql 'section))
                                   locative-args)
  (declare (ignore symbol locative-args))
  nil)

(defmethod exportable-reference-p (package symbol
                                   (locative-type (eql 'glossary-term))
                                   locative-args)
  (declare (ignore symbol locative-args))
  nil)

(defgeneric exportable-locative-type-p (locative-type)
  (:documentation "Return true iff symbols in references with
  LOCATIVE-TYPE are to be exported by default when they occur in a
  DEFSECTION. The default method returns T, while the methods for
  SECTION, GLOSSARY-TERM, PACKAGE, ASDF:SYSTEM, METHOD and INCLUDE
  return NIL.

  This function is called by the default method of
  EXPORTABLE-REFERENCE-P to decide what symbols DEFSECTION shall
  export when its EXPORT argument is true.")
  (:method (locative-type)
    (declare (ignore locative-type))
    t))

;;;; These methods must be defined here else the DEFSECTION forms in
;;;; pax.lisp will export too much.

(defmethod exportable-locative-type-p ((locative-type (eql 'asdf:system)))
  nil)

(defmethod exportable-locative-type-p ((locative-type (eql 'package)))
  nil)

(defmethod exportable-locative-type-p ((locative-type (eql 'method)))
  nil)

(defmethod exportable-locative-type-p ((locative-type (eql 'include)))
  nil)


(defclass glossary-term ()
  ((name
    :initarg :name :reader glossary-term-name
    :documentation "The name of the global variable whose value is
    this GLOSSARY-TERM object.")
   (title
    :initarg :title :reader glossary-term-title
    :documentation "A markdown string or NIL. Used in generated
    documentation.")
   (docstring :initarg :docstring :reader glossary-term-docstring))
  (:documentation "DEFINE-GLOSSARY-TERM instantiates a GLOSSARY-TERM
  with its NAME and TITLE arguments."))

(defmacro define-glossary-term
    (name (&key title (discard-documentation-p *discard-documentation-p*))
     docstring)
  "Define a global variable with NAME, and set it to a
  [GLOSSARY-TERM][class] object. A glossary term is just a symbol to
  hang a docstring on. It is a bit like a SECTION in that, when linked
  to, its TITLE will be the link text instead of the name of the
  symbol. Also as with sections, both TITLE and DOCSTRING are markdown
  strings or NIL.

  Unlike sections though, glossary terms are not rendered with
  headings, but in the more lightweight bullet + locative + name/title
  style. See the glossary entry @NAME for an example.

  When DISCARD-DOCUMENTATION-P (defaults to *DISCARD-DOCUMENTATION-P*)
  is true, DOCSTRING will not be recorded to save memory."
  `(defparameter ,name
     (make-instance 'glossary-term
                    :name ',name :title ,title
                    :docstring ,(unless discard-documentation-p
                                  docstring))))
