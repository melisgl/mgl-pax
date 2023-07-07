(in-package :dref)

(in-readtable pythonic-string-syntax)

(defsection @dref-manual (:title "DRef Manual")
  (@introduction section)
  (@locatives-and-references section)
  (@listing-definitions section)
  (@operations section)
  (@locative-types section)
  (@glossary section)
  (dref-ext::@extending-dref section))

(defsection @introduction (:title "Introduction")
  """_What if definitions were first-class objects?_

  Some [defining forms][clhs] do not create first-class
  [objects][(clhs glossary-term)]. For example, DEFUN creates
  [FUNCTION][class] objects, but DEFVAR does not create variable
  objects as no such thing exist. The main purpose of this library is
  to provide a way to refer to definitions with XREF objects:

  ```cl-transcript
  (make-xref '*my-var* 'variable)
  ==> #<XREF *MY-VAR* VARIABLE>
  ```

  XREFs just package up a @NAME (*MY-VAR*) and a @LOCATIVE (VARIABLE).
  They need not denote existing definitions until we actually want to
  use them:

  ```
  (docstring (make-xref '*my-var* 'variable))
  .. debugger invoked on LOCATE-ERROR:
  ..   Could not locate *MY-VAR* VARIABLE.
  ```

  ```cl-transcript (:dynenv dref-std-env)
  (defvar *my-var* nil
    "This is my var.")

  (docstring (make-xref '*my-var* 'variable))
  => "This is my var."
  ```

  Behind the scenes, the DOCSTRING function LOCATEs the definition
  corresponding to its XREF argument, turning it into a DREF:

  ```cl-transcript
  (locate (make-xref 'print 'function))
  ==> #<DREF PRINT FUNCTION>
  ```

  Within DRef, the DREF-EXT::@DREF-SUBCLASSES form the basis of
  extending DOCSTRING, SOURCE-LOCATION and ARGLIST. Outside DRef,
  [PAX][MGL-PAX::@PAX-MANUAL] makes PAX:DOCUMENT extensible through
  PAX:DOCUMENT-DREF, which has methods specialized on DREFs.

  Finally, existing definitions can be queried with DEFINITIONS and
  DREF-APROPOS:

  ```
  (definitions 'dref-ext:locate-dref)
  ==> (#<DREF LOCATE-DREF GENERIC-FUNCTION>
  -->  #<DREF LOCATE-DREF (METHOD NIL (GLOSSARY-TERM))>
  -->  #<DREF LOCATE-DREF (METHOD NIL (SECTION))>
  -->  #<DREF LOCATE-DREF (METHOD NIL (READTABLE))>
  -->  #<DREF LOCATE-DREF (METHOD NIL (PACKAGE))>
  -->  #<DREF LOCATE-DREF (METHOD NIL (ASDF/SYSTEM:SYSTEM))>
  -->  #<DREF LOCATE-DREF (METHOD NIL (CLASS))>
  -->  #<DREF LOCATE-DREF (METHOD NIL (METHOD))>
  -->  #<DREF LOCATE-DREF (METHOD NIL (GENERIC-FUNCTION))>
  -->  #<DREF LOCATE-DREF (METHOD NIL (FUNCTION))>
  -->  #<DREF LOCATE-DREF (METHOD (:AROUND) (T))>
  -->  #<DREF LOCATE-DREF (METHOD NIL (T))>
  -->  #<DREF LOCATE-DREF (METHOD NIL (XREF))>
  -->  #<DREF LOCATE-DREF (METHOD NIL (DREF))>
  -->  #<DREF LOCATE-DREF (UNKNOWN
  -->                        (DECLAIM LOCATE-DREF
  -->                                 FTYPE))>)
  ```

  ```cl-transcript
  (dref-apropos 'locate-error :package :dref :external-only t)
  ==> (#<DREF LOCATE-ERROR CONDITION> #<DREF LOCATE-ERROR FUNCTION>)

  (dref-apropos "ate-err" :package :dref :external-only t)
  ==> (#<DREF LOCATE-ERROR CONDITION> #<DREF LOCATE-ERROR FUNCTION>)
  ```""")

(defun dref-std-env (fn)
  ;; FIXME: Add all others too.
  (progv '(pax::*document-downcase-uppercase-code*
           pax::*transcribe-check-consistency*)
      '(nil #+sbcl t #-sbcl nil)
    (handler-bind ((warning #'muffle-warning))
      (unwind-protect
           (funcall fn)
        (unintern '*my-var* (find-package :dref))))))


(defsection @glossary (:title "Glossary")
  (@name glossary-term)
  (@locative glossary-term)
  (@locative-type glossary-term)
  (@reference glossary-term)
  (@presentation glossary-term))

(define-glossary-term @name (:title "name")
  "@NAMEs are symbols or strings which name [functions][function
  locative], [types][type locative], [packages][package locative],
  etc. Together with @LOCATIVEs, they form @REFERENCEs.

  See XREF-NAME and DREF-NAME.")

(define-glossary-term @locative (:title "locative")
  "@LOCATIVEs specify a _type_ of definition such as
  [FUNCTION][locative] or [VARIABLE][locative] and together with
  @NAMEs form @REFERENCEs.

  A locative can be a symbol or a list whose CAR is a symbol. In
  either case, the symbol is called the @LOCATIVE-TYPE, and the rest
  of the elements are the _locative arguments_. See the METHOD
  locative or the LOCATIVE locative for examples of locative types
  with arguments.

  See XREF-LOCATIVE and DREF-LOCATIVE.")

(define-glossary-term @locative-type (:title "locative type")
  "The locative type is the part of a @LOCATIVE that identifies
  what kind definition is being referred to. See @LOCATIVE-TYPES for
  the list locative types built into DRef.

  Locative types are similar to Lisp [namespaces][clhs].

  See XREF-LOCATIVE-TYPE and DREF-LOCATIVE-TYPE.")

(define-glossary-term @reference (:title "reference")
  "A @REFERENCE is an @NAME plus a @LOCATIVE, and it identifies a
  possible definition. References are of class XREF. When a reference
  is a DREF, it may also be called a definition.")

(define-glossary-term @presentation (:title "presentation")
  "@REFERENCEs may have arguments (see
  DREF-EXT::@ADDING-NEW-LOCATIVES) that do not affect the behaviour of
  LOCATE and the standard DRef @OPERATIONS, but which may be used for
  other, \"presentation\" purposes. For example, the VARIABLE
  locative's INITFORM argument is used for presentation by
  PAX:DOCUMENT. Presentation arguments are available via
  DREF-EXT:DREF-ORIGIN.")


(defsection @locatives-and-references (:title "Locatives and References")
  "After the @INTRODUCTION, here we get into the details. Of special
  interest are:

  - MAKE-XREF to construct an arbitrary @REFERENCE without any
    checking of validity.

  - LOCATE to construct a syntactically valid reference (matching the
    LAMBDA-LIST in the locative type's [definition]
    [DEFINE-LOCATIVE-TYPE]) that refers to an exisiting definition.

  - RESOLVE to find the first-class (non-XREF) object the definition
    refers to, if any.

  @OPERATIONS (ARGLIST, DOCSTRING, SOURCE-LOCATION) know how to deal
  with references (discussed in the DREF-EXT::@EXTENDING-DREF)."
  (xref class)
  (dref class)
  (make-xref function)
  (xref= function)
  (locate function)
  (resolve function)
  (locate-error condition)
  (resolve-error condition))

(defclass xref ()
  ((name :initarg :name :reader xref-name
         :documentation "The @NAME of the reference.")
   (locative :initarg :locative :reader xref-locative
             :documentation "The @LOCATIVE of the reference.

   The locative is normalized by replacing single-element lists with
   their only element:

  ```cl-transcript
  (make-xref 'print 'function)
  ==> #<XREF PRINT FUNCTION>
  ```
  ```cl-transcript
  (make-xref 'print '(function))
  ==> #<XREF PRINT FUNCTION>
  ```"))
  (:documentation "An XREF (cross-reference) may represent some
  kind of definition of its @NAME in the context given by its
  @LOCATIVE. The definition may not exist and the locative may be
  [malformed] [define-locative-type]. The subclass DREF represents
  definitions that exist."))

(defclass dref (xref)
  ((name
    :reader dref-name
    :documentation "The same as XREF-NAME, but only works on DREFs.
    Use it as a statement of intent.")
   (locative
    :reader dref-locative
    :documentation "The same as XREF-LOCATIVE, but only works on
    DREFs. Use it as a statement of intent.")
   (origin
    :reader dref-origin
    :documentation """The object from which LOCATE constructed this
    DREF. This is an XREF when the LOCATIVE argument to LOCATE was
    non-NIL and the value NAME-OR-OBJECT argument otherwise.
    DREF-ORIGIN may have @PRESENTATION arguments, which are not
    included in LOCATIVE-ARGS as is the case with INITFORM argument of
    the VARIABLE locative:

    ```cl-transcript
    (locate '*standard-output* '(variable "see-below"))
    ==> #<DREF *STANDARD-OUTPUT* VARIABLE>
    ```

    ```cl-transcript
    (dref-origin (locate '*standard-output* '(variable "see-below")))
    ==> #<XREF *STANDARD-OUTPUT* (VARIABLE "see-below")>
    ```

    The INITFORM argument overrides the global binding of
    *STANDARD-OUTPUT* when it's PAX:DOCUMENTed:

    ```cl-transcript (:dynenv dref-std-env)
    (first-line
     (pax:document (locate '*standard-output* '(variable "see-below"))
                   :stream nil))
    => "- [variable] *STANDARD-OUTPUT* \"see-below\""
    ```"""))
  (:documentation "DREFs can be thought of as referring to definitions
  that actually exist, although changes in the system can invalidate
  them (for example, a DREF to a function definition can be
  invalidated by FMAKUNBOUND).

  DREFs must be created with LOCATE, and their purpose is to allow
  easy specialization of other generic functions (see
  DREF-EXT::@EXTENDING-DREF) and to confine locative validation to
  LOCATE."))

;;; Canonicalize it a bit for easier comparison. E.g. (FUNCTION) =>
;;; FUNCTION.
(declaim (inline normalize-locative))
(defun normalize-locative (locative)
  (if (and (listp locative)
           (null (cdr locative)))
      (first locative)
      locative))

(defmethod initialize-instance :after ((xref xref) &key &allow-other-keys)
  (setf (slot-value xref 'locative)
        (normalize-locative (slot-value xref 'locative))))

(defmethod print-object ((xref xref) stream)
  (print-unreadable-object (xref stream :type nil)
    (format stream "~S ~S ~S"
            (if (typep xref 'dref)
                ;; Hide the actual type of DREFs (e.g. FUNCTION-DREF).
                ;; That's an implementation detail, and it's
                ;; determined by the locative, anyway.
                'dref
                (type-of xref))
            (xref-name xref)
            (xref-locative xref))))

(defun make-xref (name locative)
  "A shorthand for `(MAKE-INSTANCE 'XREF :NAME NAME :LOCATIVE :LOCATIVE)`.
  It does no error checking. Use LOCATE to create DREFS."
  (make-instance 'xref :name name :locative locative))

(declaim (inline xref=))
(defun xref= (xref1 xref2)
  "See if XREF1 and XREF2 have the same XREF-NAME and XREF-LOCATIVE
  under EQUAL. Comparing like this makes most sense for DREFs.
  However, two XREFs different under XREF= may denote the same DREF."
  (and (equal (xref-name xref1)
              (xref-name xref2))
       (equal (xref-locative xref1)
              (xref-locative xref2))))

;;; This also checks for EQUALness and not whether NAME is equivalent
;;; to the XREF-NAME of XREF (as in it would resolve to the same thing
;;; with the locative).
(declaim (inline xref-name=))
(defun xref-name= (name xref)
  (equal name (xref-name xref)))

(defun locative-type (locative)
  "Return the first element of LOCATIVE (which may be from
  XREF-LOCATIVE) if it's a list. If it's a symbol, then return that
  symbol itself."
  (if (listp locative)
      (first locative)
      locative))

(defun locative-args (locative)
  "Return the REST of LOCATIVE (which may be from XREF-LOCATIVE)
  if it's a list. If it's a symbol, then return NIL. The locative args
  should match the LAMBDA-LIST of the LOCATIVE-TYPE's
  [definition][define-locative-type], but this is guaranteed only for
  locatives of DREFS and not check for plain XREFs."
  (if (listp locative)
      (rest locative)
      ()))

(declaim (inline xref-locative-type))
(defun xref-locative-type (xref)
  (locative-type (xref-locative xref)))

(declaim (inline xref-locative-args))
(defun xref-locative-args (xref)
  (locative-args (xref-locative xref)))

(declaim (inline dref-locative-type))
(defun dref-locative-type (dref)
  (locative-type (dref-locative dref)))

(declaim (inline dref-locative-args))
(defun dref-locative-args (dref)
  (locative-args (dref-locative dref)))


(define-condition locate-error (error)
  ((object :initarg :object :reader locate-error-object)
   (message :initarg :message :reader locate-error-message))
  (:documentation "Signalled by LOCATE when the definition cannot be
  found, and ERRORP is true.")
  (:report (lambda (condition stream)
             (let ((*package* (find-package :cl))
                   (object (locate-error-object condition)))
               (if (typep object 'xref)
                   (format stream "~@<Could not locate ~S~@[ ~S~].~@[ ~A~]~:@>"
                           (xref-name object) (xref-locative object)
                           (locate-error-message condition))
                   (format stream "~@<Could not locate ~S.~@[ ~A~]~:@>"
                           object
                           (locate-error-message condition)))))))

;;; This gets clobbered with an empty function when DREF/AUTOLOAD is
;;; loaded.
(autoload ensure-dref-loaded '#:dref/full :export nil)

(declaim (ftype function locate-dref)
         (ftype function actualize-dref))

(defun locate (name-or-object &optional locative (errorp t))
  ;; FIXME: revisit after https://github.com/3b/3bmd/issues/57
  """Return a DREF representing the definition given by the arguments.
  In the same [dynamic environment][clhs], two DREFs denote the same
  thing if and only if they are XREF=.

  - When LOCATIVE is non-NIL, NAME-OR-OBJECT is interpreted as the
    @NAME:

  ```cl-transcript (:dynenv dref-std-env)
  (locate '*print-length* 'variable)
  ==> #<DREF *PRINT-LENGTH* VARIABLE>
  ```

  - With LOCATIVE NIL, NAME-OR-OBJECT must be a supported first-class
    object, a DREF, or an XREF:

  ```cl-transcript (:dynenv dref-std-env)
  (locate #'print)
  ==> #<DREF PRINT FUNCTION>
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (locate (locate #'print))
  ==> #<DREF PRINT FUNCTION>
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (locate (make-xref 'print 'function))
  ==> #<DREF PRINT FUNCTION>
  ```

  - LOCATE-ERROR is signalled if LOCATIVE is malformed or if no
    corresponding definition is not found. If ERRORP is NIL, then NIL
    and the LOCATE-ERROR condition are returned instead.

  ```cl-transcript (:dynenv dref-std-env)
  (locate 'no-such-function 'function)
  .. debugger invoked on LOCATE-ERROR:
  ..   Could not locate DREF::NO-SUCH-FUNCTION FUNCTION.
  ..   NO-SUCH-FUNCTION is not a symbol naming a function.
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (locate 'print '(function xxx))
  .. debugger invoked on LOCATE-ERROR:
  ..   Could not locate PRINT #'DREF::XXX.
  ..   Bad arguments (XXX) for locative FUNCTION with lambda list NIL.
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (locate "xxx")
  .. debugger invoked on LOCATE-ERROR:
  ..   Could not locate "xxx".
  ```

  Use the low-level MAKE-XREF to construct an XREF without error
  checking."""
  (ensure-dref-loaded)
  (flet ((locate-1 ()
           (actualize-dref (locate-dref (if locative
                                            (make-xref name-or-object locative)
                                            name-or-object)))))
    (if errorp
        (locate-1)
        (handler-case
            (locate-1)
          (locate-error (error)
            (values nil error))))))

(define-condition resolve-error (error)
  ((dref :initarg :dref :reader resolve-error-dref)
   (message :initarg :message :reader resolve-error-message))
  (:documentation "Signalled by RESOLVE when the object defined cannot
  be returned, and ERRORP is true.")
  (:report (lambda (condition stream)
             (let ((*package* (find-package :cl))
                   (dref (resolve-error-dref condition)))
               (format stream "~@<Could not resolve ~S ~S.~@[ ~A~]~:@>"
                       (dref-name dref) (dref-locative dref)
                       (resolve-error-message condition))))))

(declaim (ftype function resolve-dref))

(defun resolve (object &optional (errorp t))
  ;; FIXME: revisit after https://github.com/3b/3bmd/issues/57
  "- If OBJECT is an XREF, then return the first-class object associated
     with its definition if any. Return OBJECT if it's not an XREF.
     Thus, the value returned is never an XREF.

  ```cl-transcript (:dynenv dref-std-env)
  (resolve (locate 'print 'function))
  ==> #<FUNCTION PRINT>
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (resolve #'print)
  ==> #<FUNCTION PRINT>
  ```

  - If the definition for XREF cannot be LOCATEd, then signal a
    LOCATE-ERROR condition. If there is a defintion, but there is no
    first-class object corresponding to it, then signal a
    RESOLVE-ERROR condition or return NIL depending on ERRORP:

      ```cl-transcript (:dynenv dref-std-env)
      (resolve (locate '*print-length* 'variable))
      .. debugger invoked on RESOLVE-ERROR:
      ..   Could not resolve *PRINT-LENGTH* VARIABLE.
      ```
      ```cl-transcript (:dynenv dref-std-env)
      (resolve (locate '*print-length* 'variable) nil)
      => NIL
      ```"
  (ensure-dref-loaded)
  (cond ((not (typep object 'xref))
         object)
        (errorp
         (resolve-dref (locate object)))
        (t
         (handler-case
             (resolve-dref (locate object))
           ((or locate-error resolve-error) ()
             nil)))))


(defsection @listing-definitions (:title "Listing Definitions")
  (definitions function)
  (dref-apropos function)
  (locative-types function)
  (lisp-locative-types function)
  (pseudo-locative-types function)
  (locative-aliases function))

(defvar *locative-types* ())
(defvar *lisp-locative-types* ())
(defvar *pseudo-locative-types* ())
(defvar *locative-aliases* ())

(defun declare-locative-type (locative-type)
  (pushnew locative-type *locative-types*)
  (pushnew locative-type *lisp-locative-types*)
  (setq *pseudo-locative-types* (remove locative-type *pseudo-locative-types*))
  (setq *locative-aliases* (remove locative-type *locative-aliases*))
  locative-type)

(defun declare-pseudo-locative-type (locative-type)
  (pushnew locative-type *locative-types*)
  (setq *lisp-locative-types* (remove locative-type *lisp-locative-types*))
  (pushnew locative-type *pseudo-locative-types*)
  (setq *locative-aliases* (remove locative-type *locative-aliases*))
  locative-type)

(defun declare-locative-alias (locative-type)
  (setq *locative-types* (remove locative-type *locative-types*))
  (setq *lisp-locative-types* (remove locative-type *lisp-locative-types*))
  (setq *pseudo-locative-types* (remove locative-type *pseudo-locative-types*))
  (pushnew locative-type *locative-aliases*)
  locative-type)

(defun locative-types ()
  "Return a list of non-[alias][locative-aliases] locative types.
  This is the UNION of LISP-LOCATIVE-TYPES and PSEUDO-LOCATIVE-TYPES."
  *locative-types*)

(defun lisp-locative-types ()
  "Return the locative types that correspond to Lisp definitions
  except UNKNOWN. These are the ones defined with
  DEFINE-LOCATIVE-TYPE."
  *lisp-locative-types*)

(defun pseudo-locative-types ()
  "Return the locative types that correspond to non-Lisp definitions
  plus UNKNOWN. These are the ones defined with
  DEFINE-PSEUDO-LOCATIVE-TYPE."
  *pseudo-locative-types*)

(defun locative-aliases ()
  "Return the list of locatives aliases, defined with DEFINE-LOCATIVE-ALIAS."
  *locative-aliases*)


(defsection @operations (:title "Operations")
  "The following functions take a single definition as their argument.
  They all fundamentally work with DREFs and will attempt to convert
  XREFs and other objects to DREFs with LOCATE, which may signal
  LOCATE-ERROR."
  (arglist function)
  (docstring function)
  (source-location function))

;;; The generic functions used for extension are not directly exposed
;;; to the user in order to be able to change the signature more
;;; easily.

(declaim (ftype function dref-arglist))

(defun arglist (object)
  "Return the arglist of the definition of OBJECT or NIL if the
  arglist cannot be determined.

  The second value return value indicates whether the arglist has been
  found. Furthermore, :ORDINARY indicates an [ordinary lambda
  list][clhs], :MACRO a [macro lambda list][clhs], and :DEFTYPE a
  [deftype lambda list][clhs]. Other non-NIL values are also allowed.

  ```cl-transcript (:dynenv dref-std-env)
  (arglist #'arglist)
  => (OBJECT)
  => :ORDINARY
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (arglist (locate 'define-locative-type 'macro))
  => (LOCATIVE-TYPE LAMBDA-LIST &BODY DOCSTRING)
  => :MACRO
  ```

  This function supports MACROs, COMPILER-MACROs, SETF functions,
  FUNCTIONs, GENERIC-FUNCTIONs, METHODs, TYPES, LOCATIVEs and can be
  extended via DREF-ARGLIST.

  Note that ARGLIST depends on the quality of SWANK-BACKEND:ARGLIST.
  With the exception of SBCL, which has perfect support, all Lisp
  implementations have minor omissions:

  - DEFTYPE lambda lists on ABCL, AllegroCL, CLISP, \CCL, CMUCL, ECL;
  - default values in MACRO lambda lists on AllegroCL;
  - various edge cases involving traced functions."
  (ensure-dref-loaded)
  (dref-arglist (locate object)))

(declaim (ftype function dref-docstring))

(defun docstring (object)
  "Return the docstring from the definition of OBJECT.
  As the second value, return the *PACKAGE* that was in effect when
  the docstring was installed or NIL if it cannot be determined. This
  is used by PAX:DOCUMENT when PAX::@PARSING. This function is similar
  in purpose to CL:DOCUMENTATION.

  Can be extended via DREF-DOCSTRING.

  Note that some locative types such as ASDF:SYSTEMS and DECLARATIONs
  have no docstrings, and some Lisp implementations do not record all
  docstrings. The following are known to be missing:

  - COMPILER-MACRO docstrings on ABCL, AllegroCL, \CCL, ECL;
  - METHOD-COMBINATION docstrings on ABCL, AllegroCL."
  (ensure-dref-loaded)
  (dref-docstring (locate object)))

(declaim (ftype function dref-source-location)
         (ftype function source-location-p))

(defun source-location (object &key errorp)
  """Return the Swank source location for the definition of OBJECT.
  If no source location was found, the either an ERROR condition is
  signalled if ERRORP else the [ERROR][condition] is returned as the
  second value (with the first being NIL). The returned Swank location
  object is to be accessed only through the
  DREF-EXT::@SOURCE-LOCATIONS API or to be passed to e.g Slime's
  `slime-goto-source-location`.

  Can be extended via DREF-SOURCE-LOCATION.

  Note that the availability of source location information varies
  greatly across Lisp implementations."""
  (ensure-dref-loaded)
  (flet ((source-location-1 ()
           (let ((location (dref-source-location (locate object))))
             (if (source-location-p location)
                 location
                 (error "~@<Source location of ~S not found.~:@>" object)))))
    (if errorp
        (source-location-1)
        (ignore-errors (source-location-1)))))
