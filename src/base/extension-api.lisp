(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @extension-api (:title "Writing Extensions")
  (@new-object-types section)
  (@reference-based-extensions section)
  (@extending-document section)
  (@extending-find-source section)
  (@sections section)
  (@glossary-terms section))


(defsection @new-object-types (:title "Adding New Object Types")
  """One may wish to make the DOCUMENT function and `\\M-.` navigation
  work with new object types. DOCUMENT can be extended by defining a
  DOCUMENT-OBJECT method specialized on that type. To allow these
  objects to be referenced from DEFSECTION, LOCATE-OBJECT method is to
  be defined. If there are multiple equivalent references possible for
  the same thing, then CANONICAL-REFERENCE must be specialized. For
  the DOCSTRING locative to work on the new type, a DOCSTRING method
  is needed. For `\\M-.` FIND-SOURCE can be specialized. Finally,
  EXPORTABLE-LOCATIVE-TYPE-P may be overridden if exporting does not
  makes sense. Here is how all this is done for ASDF:SYSTEM:"""
  (asdf-example (include (:start (asdf:system locative)
                          :end (end-of-asdf-example variable))
                         :header-nl "```"
                         :footer-nl "```"))
  (define-locative-type macro)
  (define-locative-alias macro)
  (locate-object generic-function)
  (locate-error function)
  (canonical-reference generic-function)
  (collect-reachable-objects generic-function)
  (document-object generic-function)
  (docstring generic-function)
  (find-source generic-function)
  (exportable-reference-p generic-function)
  (exportable-locative-type-p generic-function))

(defmacro define-locative-type (locative-type lambda-list &body docstring)
  """Declare LOCATIVE-TYPE as a [LOCATIVE][locative]. One gets two
  things in return: first, a place to document the format and
  semantics of LOCATIVE-TYPE (in LAMBDA-LIST and DOCSTRING); second,
  being able to reference `(LOCATIVE-TYPE LOCATIVE)`. For example, if
  you have:

  ```
  (define-locative-type variable (&optional initform)
    "Dummy docstring.")
  ```

  then `(VARIABLE LOCATIVE)` refers to this form."""
  (assert (or (endp docstring)
              (and (= 1 (length docstring))
                   (string (first docstring)))))
  `(defmethod locative-lambda-list ((symbol (eql ',locative-type)))
     ,@docstring
     ',lambda-list))

(defmacro define-locative-alias (alias locative-type)
  """Define ALIAS as a locative equivalent to LOCATIVE-TYPE (both
  SYMBOLs). The following example shows how to make docstrings read
  more naturally by defining an alias.

  ```
  (defclass my-string ()
    ())

  (defgeneric my-string (obj)
    (:documentation "Convert OBJ to MY-STRING."))

  ;;; This version of FOO has a harder to read docstring because
  ;;; it needs to disambiguate the MY-STRING reference.
  (defun foo (x)
    "FOO takes and argument X, a [MY-STRING][class] object.")

  ;;; Define OBJECT as an alias for the CLASS locative.
  (define-locative-alias object class)

  ;;; Note how no explicit link is needed anymore.
  (defun foo (x)
    "FOO takes an argument X, a MY-CLASS object.")
  ```"""
  `(progn
     (define-locative-type ,alias ()
       ,(format nil "An alias for the ~S locative." locative-type))
     (defmethod locate-object (symbol (locative-type (eql ',alias))
                               locative-args)
       (locate-object symbol ',locative-type locative-args))))

;;; A somewhat dummy generic function on which the docstring can be
;;; hung, and which provides a source location. It returns LAMBDA-LIST
;;; from DEFINE-LOCATIVE-TYPE.
(defgeneric locative-lambda-list (symbol))

(defvar *locate-object-object*)
(defvar *locate-object-locative*)

(defgeneric locate-object (object locative-type locative-args)
  (:documentation "Return the object to which OBJECT and the locative
  refer. Signal a LOCATE-ERROR condition by calling the LOCATE-ERROR
  function if the lookup fails. If a REFERENCE is returned, then it
  must be canonical in the sense that calling CANONICAL-REFERENCE on
  it will return the same reference. Don't call this function
  directly. It serves only to extend LOCATE.")
  (:method :around (object locative-type locative-args)
    (let ((*locate-object-object* object)
          (*locate-object-locative* (cons locative-type locative-args)))
      (call-next-method)))
  (:method (object locative-type locative-args)
    (declare (ignore object locative-type locative-args))
    (locate-error)))

(defun locate-error (&rest format-and-args)
  "Call this function to signal a LOCATE-ERROR condition from a
  LOCATE-OBJECT method. FORMAT-AND-ARGS contains a format string and
  args suitable for FORMAT from which the LOCATE-ERROR-MESSAGE is
  constructed. If FORMAT-AND-ARGS is NIL, then the message will be NIL
  too.

  LOCATE-ERROR-OBJECT and LOCATE-ERROR-LOCATIVE are populated
  automatically."
  (error 'locate-error :object *locate-object-object*
         :locative (normalize-locative *locate-object-locative*)
         :message (if format-and-args
                      (apply #'format nil format-and-args)
                      nil)))

(defgeneric canonical-reference (object)
  (:documentation "Return a REFERENCE that RESOLVEs to OBJECT, or
  return NIL if this operation is not defined for OBJECT. Its
  @REFERENCE-DELEGATE is LOCATE-CANONICAL-REFERENCE.")
  (:method :around (object)
    (if (ensure-navigate-loaded)
        (call-next-method)
        (canonical-reference object)))
  (:method (object)
    (declare (ignore object))
    nil))

(defgeneric collect-reachable-objects (object)
  (:documentation "Return a list of objects representing all things to
  be documented in a `(DOCUMENT OBJECT)` call. For a SECTION this is
  simply the union of references reachable from references in its
  SECTION-ENTRIES. The returned objects can be anything provided that
  CANONICAL-REFERENCE works on them. The list need not include OBJECT
  itself.

  One only has to specialize this for new container-like objects. Its
  @REFERENCE-DELEGATE is LOCATE-AND-COLLECT-REACHABLE-OBJECTS.")
  (:method :around (object)
    (if (ensure-navigate-loaded)
        (call-next-method)
        (collect-reachable-objects object))))

(defgeneric document-object (object stream)
  (:documentation "Write OBJECT (and its references recursively) in
  *FORMAT* to STREAM in markdown format. Add methods specializing on
  OBJECT to customize the output of DOCUMENT. Its @REFERENCE-DELEGATE
  is LOCATE-AND-DOCUMENT. This function is for extension, don't call
  it directly."))

(defgeneric docstring (object)
  (:documentation "Return the docstring from the definition of OBJECT
  with [leading indentation stripped][@markdown-indentation]. This
  function serves a similar purpose as CL:DOCUMENTATION, but it works
  with first-class objects when there is one for the corresponding
  definition, and with REFERENCEs when there is not. Its
  @REFERENCE-DELEGATE is LOCATE-DOCSTRING.

  DOCSTRING is used in the implementation of the DOCSTRING locative.
  Some things such as ASDF:SYSTEMS and DECLARATIONs have no
  docstrings. Notably, SECTIONs don't provide access to docstrings.")
  (:method :around (object)
    (if (ensure-navigate-loaded)
        (call-next-method)
        (docstring object))))

;;; This is bound to an EQUAL hash table in MAKE-GIT-SOURCE-URI-FN
;;; to speed up FIND-SOURCE. It's still very slow because some
;;; underlying Swank calls involve reading in the whole source file.
(defvar *find-source-cache* nil)

(defgeneric find-source (object)
  (:documentation """Return the Swank source location for OBJECT. It
  is called by LOCATE-DEFINITIONS-FOR-EMACS, which lies behind the
  `\\M-.` extension (see @NAVIGATING-IN-EMACS). Its
  @REFERENCE-DELEGATE is LOCATE-AND-FIND-SOURCE.

  If successful, the return value should look like one of these:

  ```
  (:LOCATION
    (:FILE "/home/melisgl/own/mgl-pax/src/pax.lisp")
    (:POSITION 3303) NIL)
  (:LOCATION
    (:FILE "/home/melisgl/own/mgl-pax/src/pax.lisp")
    (:OFFSET 1 3303) NIL)
  (:LOCATION
    (:FILE "/home/melisgl/own/mgl-pax/src/pax.lisp")
    (:FUNCTION-NAME "FOO") NIL)
  ```

  The NIL above is the source snippet, which is optional. Note that
  position 1 is the first character in `:FILE`. If unsuccessful, the
  return value is like:

  ```
  (:error "Unknown source location for SOMETHING")
  ```
  """)
  (:method :around (object)
    (if (ensure-navigate-loaded)
        (if *find-source-cache*
            (let ((key (if (typep object 'reference)
                           (list (reference-object object)
                                 (reference-locative object))
                           object)))
              (or (gethash key *find-source-cache*)
                  (setf (gethash key *find-source-cache*)
                        (call-next-method))))
            (call-next-method))
        (find-source object))))


(defsection @reference-based-extensions (:title "Reference Based Extensions")
  """Let's see how to extend DOCUMENT and `\\M-.` navigation if there
  is no first-class object to represent the definition of interest.
  Recall that LOCATE returns a REFERENCE object in this case. The
  generic functions that we have specialized in @NEW-OBJECT-TYPES have
  @REFERENCE-DELEGATEs, which can be specialized based on
  LOCATIVE-TYPE. Here is how the VARIABLE locative is defined:"""
  (variable-example (include (:start (variable locative)
                                     :end (end-of-variable-example variable))
                             :header-nl "```"
                             :footer-nl "```"))
  (@reference-delegate glossary-term)
  (locate-canonical-reference generic-function)
  (locate-and-collect-reachable-objects generic-function)
  (locate-and-document generic-function)
  (locate-docstring generic-function)
  (locate-and-find-source generic-function) 
  "We have covered the basic building blocks of reference based
  extensions. Now let's see how the obscure
  DEFINE-SYMBOL-LOCATIVE-TYPE and
  DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE macros work together to
  simplify the common task of associating definition and documentation
  with symbols in a certain context."
  (define-symbol-locative-type macro)
  (define-definer-for-symbol-locative-type macro))

(define-glossary-term @reference-delegate (:title "reference delegate")
  """CANONICAL-REFERENCE, COLLECT-REACHABLE-OBJECTS, DOCUMENT-OBJECT,
  [DOCSTRING][generic-function], and FIND-SOURCE delegate dealing with
  REFERENCES to another generic function, one each, which is called
  their reference delegate. Each of these delegator functions invokes
  its delegate when a REFERENCE is passed to it (as its OBJECT
  argument), or when there is no method specialized for its arguments,
  in which case it uses the CANONICAL-REFERENCE.

  The net effect is that is that it is sufficient to specialize either
  the delegator for a first-class object or the delegate for a new
  locative type.""")

(defun delegate-reference (delegate reference &key delegator defaults)
  (if (null reference)
      (values-list defaults)
      (let ((values (multiple-value-list
                     (funcall delegate (reference-object reference)
                              (reference-locative-type reference)
                              (reference-locative-args reference)))))
        (if (equal values '(no-delegate))
            (if delegator
                (let ((object (resolve reference)))
                  (if (typep object 'reference)
                      (values-list defaults)
                      (funcall delegator object)))
                (values-list defaults))
            (values-list values)))))


;;;; LOCATE-CANONICAL-REFERENCE

(defmethod canonical-reference ((reference reference))
  (delegate-reference 'locate-canonical-reference reference))

(defgeneric locate-canonical-reference (object locative-type locative-args)
  (:documentation "This is the @REFERENCE-DELEGATE of
  CANONICAL-REFERENCE. The default method calls LOCATE-OBJECT with the
  three arguments. If LOCATE-OBJECT returns a REFERENCE, then that's
  taken to be the canonical reference and is returned, else
  CANONICAL-REFERENCE is invoked with the returned object.")
  (:method (object locative-type locative-args)
    (handler-case
        (let ((located (locate-object object locative-type locative-args)))
          (if (typep located 'reference)
              located
              (canonical-reference located)))
      (locate-error ()
        ;; DISLOCATED, ARGUMENT, and CLHS end up here
        (make-reference object (cons locative-type locative-args))))))


;;;; LOCATE-AND-COLLECT-REACHABLE-OBJECTS

(defmethod collect-reachable-objects (object)
  (delegate-reference 'locate-and-collect-reachable-objects
                      (canonical-reference object)))

(defmethod collect-reachable-objects ((reference reference))
  (delegate-reference 'locate-and-collect-reachable-objects
                      reference :delegator 'collect-reachable-objects))

(defgeneric locate-and-collect-reachable-objects (object locative-type
                                                  locative-args)
  (:documentation "This is the @REFERENCE-DELEGATE of
  COLLECT-REACHABLE-OBJECTS.")
  (:method (object locative-type locative-args)
    (declare (ignore object locative-type locative-args))
    'no-delegate))


;;;; LOCATE-AND-DOCUMENT

(defgeneric locate-and-document (object locative-type locative-args
                                 stream)
  (:documentation "This is the @REFERENCE-DELEGATE of DOCUMENT."))


;;;; LOCATE-DOCSTRING

(defmethod docstring (object)
  (delegate-reference 'locate-docstring (canonical-reference object)))

(defmethod docstring ((reference reference))
  (delegate-reference 'locate-docstring reference :delegator 'docstring))

(defgeneric locate-docstring (object locative-type locative-args)
  (:documentation "This is the @REFERENCE-DELEGATE of [DOCSTRING][
  generic-function].")
  (:method (object locative-type locative-args)
    (declare (ignore object locative-type locative-args))
    'no-delegate))


;;;; LOCATE-AND-FIND-SOURCE

(defmethod find-source (object)
  (delegate-reference 'locate-and-find-source
                      (canonical-reference object)
                      :defaults `((:error "Cannot find source location."))))

(defmethod find-source ((reference reference))
  (delegate-reference 'locate-and-find-source reference
                      :delegator 'find-source
                      :defaults `((:error "Cannot find source location."))))

(defgeneric locate-and-find-source (object locative-type locative-args)
  (:documentation "This is the @REFERENCE-DELEGATE of FIND-SOURCE.")
  (:method (object locative-type locative-args)
    (declare (ignore object locative-type locative-args))
    'no-delegate))


(defvar *locative-source-search-list* ())

(defun add-locative-to-source-search-list (locative)
  """Some locatives are implemented in terms of Lisp definitions, for
  which [SLIME's `\\M-.`][slime-m-.] finds source code of the box. For
  example, DEFSECTIONs are global variables. To be able to list all
  definitions that belong to an @OBJECT, we register locatives to try
  with ADD-LOCATIVE-TO-SOURCE-SEARCH-LIST."""
  (pushnew locative *locative-source-search-list* :test #'equal))

(defmacro symbol-lambda-list-method (symbol locative-type)
  `(find-method* #'symbol-lambda-list () `((eql ,,symbol) (eql ,,locative-type))
                 nil))

(defmacro define-symbol-locative-type (locative-type lambda-list
                                       &body docstring)
  """Similar to DEFINE-LOCATIVE-TYPE but it assumes that all things
  locatable with LOCATIVE-TYPE are going to be just symbols defined
  with a definer defined with DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE.
  It is useful to attach documentation and source location to symbols
  in a particular context. An example will make everything clear:

  ```
  (define-symbol-locative-type direction ()
    "A direction is a symbol. (After this `M-.` on `DIRECTION LOCATIVE`
    works and it can also be included in DEFSECTION forms.)")

  (define-definer-for-symbol-locative-type define-direction direction
    "With DEFINE-DIRECTION one can document what a symbol means when
    interpreted as a direction.")

  (define-direction up ()
    "UP is equivalent to a coordinate delta of (0, -1).")
  ```

  After all this, `(UP DIRECTION)` refers to the `DEFINE-DIRECTION`
  form above."""
  (check-body-docstring docstring)
  `(progn
     (define-locative-type ,locative-type ,lambda-list ,@docstring)
     (defmethod locate-object
         (symbol (locative-type (eql ',locative-type)) locative-args)
       (or (symbol-lambda-list-method symbol ',locative-type)
           (locate-error))
       (make-reference symbol (cons locative-type locative-args)))
     (defmethod locate-and-document
         (symbol (locative-type (eql ',locative-type)) locative-args stream)
       (declare (ignore locative-args))
       (let ((method (symbol-lambda-list-method symbol ',locative-type))
             (lambda-list (symbol-lambda-list symbol ',locative-type)))
         (documenting-reference (stream :arglist lambda-list)
           (with-dislocated-objects (macro-arg-names lambda-list)
             (document-docstring (documentation* method t) stream)))))
     (defmethod locate-docstring
         (symbol (locative-type (eql ',locative-type)) locative-args)
       (declare (ignore locative-args))
       (let ((method (symbol-lambda-list-method symbol ',locative-type)))
         (documentation method t)))
     (defmethod locate-and-find-source
         (symbol (locative-type (eql ',locative-type)) locative-args)
       (declare (ignore locative-args))
       (find-definition*
        (symbol-lambda-list-method symbol ',locative-type)
        'symbol-lambda-list `(method () ((eql ,symbol)
                                         (eql ,locative-type)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun check-body-docstring (docstring)
    (assert (or (endp docstring)
                (and (= 1 (length docstring))
                     (string (first docstring)))))))

;;; A somewhat dummy generic function whose methods are
;;; EQL-specialized on SYMBOL and LOCATIVE-TYPE. The appropriate
;;; method's docstring is the docstring of SYMBOL as LOCATIVE-TYPE. As
;;; an afterthought, this method also returns the LAMBDA-LIST given in
;;; the definition.
(defgeneric symbol-lambda-list (symbol locative-type))

(defmacro define-definer-for-symbol-locative-type
    (name locative-type &body docstring)
  "Define a macro with NAME which can be used to attach documentation,
  a lambda-list and source location to a symbol in the context of
  LOCATIVE-TYPE. The defined macro's arglist is (SYMBOL LAMBDA-LIST
  &OPTIONAL DOCSTRING). LOCATIVE-TYPE is assumed to have been defined
  with DEFINE-SYMBOL-LOCATIVE-TYPE."
  `(defmacro ,name (symbol lambda-list &body docstring)
     ,@docstring
     `,(expand-define-definer-for-symbol-as-locative-definer-body
        symbol ',locative-type lambda-list docstring)))

(defun expand-define-definer-for-symbol-as-locative-definer-body
    (symbol locative-type lambda-list docstring)
  `(defmethod symbol-lambda-list ((symbol (eql ',symbol))
                                  (locative-type (eql ',locative-type)))
     ,@docstring
     ',lambda-list))


(defsection @sections (:title "Sections")
  "[SECTION][class] objects rarely need to be dissected since
  DEFSECTION and DOCUMENT cover most needs. However, it is plausible
  that one wants to subclass them and maybe redefine how they are
  presented."
  (section class)
  (section-name (reader section))
  (section-package (reader section))
  (section-readtable (reader section))
  (section-title (reader section))
  (section-link-title-to (reader section))
  (section-entries (reader section)))


(defsection @glossary-terms (:title "Glossary Terms")
  "[GLOSSARY-TERM][class] objects rarely need to be dissected since
  DEFINE-GLOSSARY-TERM and DOCUMENT cover most needs. However, it is
  plausible that one wants to subclass them and maybe redefine how
  they are presented."
  (glossary-term class)
  (glossary-term-name (reader glossary-term))
  (glossary-term-title (reader glossary-term)))
