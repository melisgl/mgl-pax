(in-package :dref)

(in-readtable pythonic-string-syntax)

(defsection dref-ext::@adding-new-locatives (:title "Adding New Locatives"
                                             :export :dref-ext)
  "Let's see how to tell DRef about new kinds of definitions through
  the example of the implementation of the CLASS locative. Note that
  this is a verbatim PAX:INCLUDE of the sources. Please ignore any
  internal machinery. The first step is to define the locative type:"
  (nil (include (:start (class locative) :end (class-dref class))
                :header-nl "```" :footer-nl "```"))
  "Next, we define a subclass of [DREF][class] associated with the
  CLASS locative type and specialize LOCATE*:"
  (nil (include (:start (class-dref class)
                 :end (actualize-type-to-class function))
                :header-nl "```" :footer-nl "```"))
  "The first method makes `(LOCATE (FIND-CLASS 'DREF))` work, while
  the second is for `(DREF 'DREF 'CLASS)`. Naturally, for locative
  types that do not define first-class objects, the first method
  cannot be defined.

  Then, with ADD-DREF-ACTUALIZER, we install a function that that runs
  whenever a new [DREF][class] is about to be returned from LOCATE and
  turn the locative TYPE into the locative CLASS if the denoted
  definition is of a class:"
  (nil (include (:start (actualize-type-to-class function)
                 :end (resolve* (method () (class-dref))))
                :header-nl "```" :footer-nl "```"))
  "Finally, we define a RESOLVE* method to recover the
  [CLASS][type] object from a CLASS-DREF. We also specialize
  DOCSTRING* and SOURCE-LOCATION*:"
  (nil (include (:start (resolve* (method () (class-dref)))
                 :end (%end-of-class-example variable))
                :header-nl "```" :footer-nl "```"))
  "We took advantage of having just made the class locative type being
  RESOLVEable, by specializing DOCSTRING* on the CLASS class.
  SOURCE-LOCATION* was specialized on CLASS-DREF to demonstrate how
  this can be done for non-RESOLVEable locative types.

  Classes have no arglist, so no ARGLIST* method is needed. In the
  following, we describe the pieces in detail."
  (define-locative-type macro)
  (define-pseudo-locative-type macro)
  (define-locative-alias macro)
  (define-definition-class macro)
  (locate* generic-function)
  (dref* generic-function)
  (check-locative-args macro)
  (locate-error function)
  (add-dref-actualizer function)
  (remove-dref-actualizer function)
  (resolve* generic-function)
  (resolve-error function)
  (map-definitions generic-function)
  (map-names generic-function)
  (arglist* generic-function)
  (docstring* generic-function)
  (source-location* generic-function))

;;; This is not a function so that constant CLASS-NAMEs can be
;;; optimized by the compiler.
(defmacro %make-dref (class-name name locative &rest args)
  `(make-instance ,class-name :name ,name :locative ,locative ,@args))


(defmacro define-locative-type (locative-type lambda-list &body docstring)
  """Declare LOCATIVE-TYPE as a [LOCATIVE][locative], which is the
  first step in DREF-EXT::@EXTENDING-DREF.

  LAMBDA-LIST is a [destructuring lambda list][clhs]. The
  LOCATIVE-ARGS of [DREF][class]s with @LOCATIVE-TYPE
  LOCATIVE-TYPE (the argument given to this macro) always conform to
  this lambda list. See CHECK-LOCATIVE-ARGS.

  For example, if  have:

  ```
  (define-locative-type dummy (x &key y)
    "Dummy docstring.")
  ```

  then `(LOCATE 'DUMMY 'LOCATIVE)` refers to this definition. That is,
  ARGLIST, [DOCSTRING][function] and SOURCE-LOCATION all work on it.

  Locative types defined with DEFINE-LOCATIVE-TYPE can be listed with
  LISP-LOCATIVE-TYPES."""
  (assert (or (endp docstring)
              (and (= 1 (length docstring))
                   (string (first docstring)))))
  (check-docstring-only-body docstring)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defmethod locative-type-lambda-list ((symbol (eql ',locative-type)))
       (values ',lambda-list ,(first docstring) ,*package*))
     (declare-locative-type ',locative-type)))

(defmacro define-pseudo-locative-type (locative-type lambda-list
                                       &body docstring)
  """Like DEFINE-LOCATIVE-TYPE, but declare that LOCATIVE-TYPE does
  not correspond to definitions in the Lisp system. Definitions with
  pseduo locatives are not listed by default by DEFINITIONS.

  Locative types defined with DEFINE-PSEUDO-LOCATIVE-TYPE can be
  listed with PSEUDO-LOCATIVE-TYPES."""
  (assert (or (endp docstring)
              (and (= 1 (length docstring))
                   (string (first docstring)))))
  (check-docstring-only-body docstring)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defmethod locative-type-lambda-list ((symbol (eql ',locative-type)))
       (values ',lambda-list ,(first docstring) ,*package*))
     (declare-pseudo-locative-type ',locative-type)))

(defun check-docstring-only-body (body)
  (assert (or (endp body)
              (and (= (length body) 1)
                   (stringp (first body))))
          () "BODY must be () or a single docstring."))

(defmacro check-locative-args (locative-type locative-args)
  "Signal a LOCATE-ERROR condition if LOCATIVE-ARGS do not match the
  LAMBDA-LIST argument of LOCATIVE-TYPE (not evaluated)."
  (let ((lambda-list (locative-type-lambda-list locative-type)))
    `(unless (ignore-errors
              (destructuring-bind ,lambda-list ,locative-args
                (declare (ignore ,@(macro-arg-names lambda-list)))
                t))
       (locate-error "Bad arguments ~S for locative ~S with lambda list ~S."
                     ,locative-args ',locative-type ',lambda-list))))

(defmacro define-locative-alias (alias locative-type &body docstring)
  """Define ALIAS as a locative equivalent to LOCATIVE-TYPE (both
  SYMBOLs). LOCATIVE-TYPE must exist (i.e. be among LOCATIVE-TYPES).
  For example, let's define OBJECT as an alias of the CLASS locative:

  ```cl-transcript
  (define-locative-alias object class)
  ```

  Then, LOCATEing with OBJECT will find the CLASS:

  ```cl-transcript
  (dref 'xref 'object)
  ==> #<DREF XREF CLASS>
  ```

  The LOCATIVE-ARGS of OBJECT (none in the above) are passed on to
  CLASS.

  ```cl-transcript
  (arglist (dref 'object 'locative))
  => (&REST ARGS)
  => :DESTRUCTURING
  ```

  Also, see PAX::@LOCATIVE-ALIASES in PAX."""
  (check-docstring-only-body docstring)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defmethod locative-type-lambda-list ((symbol (eql ',alias)))
       (values '(&rest args) ,(first docstring) ,*package*))
     (defmethod dref* (name (locative-type (eql ',alias)) locative-args)
       (dref* name ',locative-type locative-args))
     (declare-locative-alias ',alias)))

;;; A somewhat dummy generic function that provides a source location.
;;; It returns LAMBDA-LIST and DOCSTRING from DEFINE-LOCATIVE-TYPE,
;;; and the *PACKAGE* in effect at macroexpansion time.
(defgeneric locative-type-lambda-list (symbol))

(defmacro define-definition-class (locative-type class-name
                                   &optional (superclasses '(dref)) &body body)
  "Define a subclass of DREF. All definitions with LOCATIVE-TYPE
  must be of this type. If non-NIL, BODY is DEFCLASS' slot definitions
  and other options."
  ;; We could record which DREF subclass corresponds to which locative
  ;; type. This will be needed, for example, for DREF-APROPOS to
  ;; support matching on subtypes. For now we ignore LOCATIVE-TYPE.
  (declare (ignore locative-type))
  `(defclass ,class-name ,superclasses
     ,@(or body '(()))))



(defvar *locating-object*)

(defun locate-error (&rest format-and-args)
  "Call this function to signal a LOCATE-ERROR condition from the
  [dynamic extent][clhs] of a LOCATE* method (which includes
  DREF*). It is an error to call LOCATE-ERROR elsewhere.

  FORMAT-AND-ARGS, if non-NIL, is a format string and arguments
  suitable for FORMAT."
  (error 'locate-error
         :object *locating-object*
         :message format-and-args))

(defvar *dref-actualizers* ())

(defun add-dref-actualizer (name)
  "Add the global function denoted by the symbol NAME to the list
  of actualizers. Actualizers are functions of a single [DREF][class]
  argument. They are called within LOCATE when LOCATE* returns a DREF.
  Their job is to make the DREF more specific."
  (check-type name symbol)
  (setq *dref-actualizers* (cons name (remove name *dref-actualizers*))))

(defun remove-dref-actualizer (name)
  "Remove the global function denoted by the symbol NAME from the
  list of actualizers."
  (check-type name symbol)
  (setq *dref-actualizers* (remove name *dref-actualizers*)))

(defmacro the-dref (form)
  (let ((%dref (gensym)))
    `(let ((,%dref ,form))
       (check-type ,%dref dref)
       ,%dref)))

(defun actualize-dref (dref)
  (loop
    (let ((actualized (loop for actualizer in *dref-actualizers*
                              thereis (funcall actualizer dref))))
      (if actualized
          (setq dref (the-dref actualized))
          (return dref)))))

(defgeneric locate* (object)
  (:documentation "Return a definition of OBJECT as a [DREF][class],
  without [actualizing it][ADD-DREF-ACTUALIZER]. If OBJECT is a DREF
  already, then this function simply returns it. If no definition is
  found for OBJECT, then LOCATE-ERROR is signalled.

  This function is for extending LOCATE. Do not call it directly.")
  (:method :around (object)
    (let* ((*locating-object* object)
           (dref (call-next-method)))
      (declare (type dref dref))
      ;; Don't set ORIGIN if a nested LOCATE already did.
      (unless (slot-boundp dref 'origin)
        (setf (slot-value dref 'origin) object))
      dref))
  (:method (object)
    (declare (ignore object))
    (locate-error))
  (:method ((xref xref))
    (dref* (xref-name xref)
           (xref-locative-type xref)
           (xref-locative-args xref)))
  (:method ((dref dref))
    dref))

(defgeneric dref* (name locative-type locative-args)
  (:documentation "LOCATE* calls this for [XREF][class]s which are not
  [DREF][class]s.

  An EQL-specialized method must be defined for all new locative
  types. This function is for extending LOCATE. Do not call it
  directly.")
  (:method :around (name locative-type locative-args)
    (declare (ignorable name locative-type locative-args))
    (let ((located (call-next-method)))
      (assert (or (not (typep located 'xref))
                  (typep located 'dref)))
      located))
  (:method (name locative-type locative-args)
    (declare (ignorable name locative-type locative-args))
    (locate-error)))

(defvar *resolving-dref*)

(defun resolve-error (&rest format-and-args)
  "Call this function to signal a RESOLVE-ERROR condition from the
  [dynamic extent][clhs] of a RESOLVE* method. It is an error to call
  RESOLVE-ERROR elsewhere.

  FORMAT-AND-ARGS, if non-NIL, is a format string and arguments
  suitable for FORMAT."
  (error 'resolve-error
         :dref *resolving-dref*
         :message (if format-and-args
                      (apply #'format nil format-and-args)
                      nil)))

(defgeneric resolve* (dref)
  (:documentation "Return the object defined by the definition DREF
  refers to. Signal a RESOLVE-ERROR condition by calling the
  RESOLVE-ERROR function if the lookup fails.

  To keep RESOLVE a partial inverse of LOCATE, a specialized LOCATE*
  method or an [actualizer][ add-dref-actualizer] must be defined for
  RESOLVEable definitions. This function is for extending RESOLVE. Do
  not call it directly.

  It is an error for methods of this generic function to return an
  [XREF][class].")
  (:method :around ((dref dref))
    (let* ((*resolving-dref* dref)
           (resolved (call-next-method)))
      (assert (not (typep resolved 'xref)))
      resolved))
  (:method ((dref dref))
    (resolve-error)))

(defgeneric map-definitions (fn name locative-type)
  (:documentation "Call FN with [DREF][class]s which have the given
  NAME and LOCATIVE-TYPE. For most locative types, there is at most
  one definition, but for METHOD, for example, there may be many. The
  default method simply does `(DREF NAME LOCATIVE-TYPE NIL)` and calls
  FN with result if [DREF][function] succeeds.

  This function is for extending DEFINITIONS. Do not call it directly.")
  ;; See DEFINITIONS for how the efficiency hack of returning the
  ;; magic symbol SWANK-DEFINITIONS instead of mapping with FN works.
  (:method (fn name locative-type)
    (let ((located (dref name locative-type nil)))
      (when located
        (funcall fn located)
        (values)))))

(defgeneric map-names (fn locative-type)
  (:documentation "Call FN with @NAMEs that form a [DREF][class] with
  some locative with LOCATIVE-TYPE. The default method tries to form
  DREFs by combining each interned symbol with LOCATIVE-TYPE and no
  LOCATIVE-ARGS.

  This function is for extending DREF-APROPOS. Do not call it
  directly.")
  (:method (fn locative-type)
    (declare (ignore fn locative-type))
    ;; See DREF-APROPOS about the magic symbol TRY-INTERNED-SYMBOLS.
    'try-interned-symbols))

(defgeneric arglist* (object)
  (:documentation "To extend ARGLIST, specialize this on a subclass of
  [DREF][class] if that subclass is not RESOLVEable, else on the type
  of object it resolves to. This function is for extension only. Do
  not call it directly.")
  (:method ((dref dref))
    nil)
  ;; This fallback is necessary in case a locative type is originally
  ;; not RESOLVEable but later becomes so.
  (:method (object)
    (let ((dref (locate object nil)))
      (when dref
        (arglist* dref)))))

(defgeneric docstring* (dref)
  (:documentation "To extend DOCSTRING, specialize this on a subclass
  of [DREF][class] if that subclass is not RESOLVEable, else on the
  type of object it resolves to. This function is for extension only.
  Do not call it directly.")
  (:method ((dref dref))
    nil)
  (:method (object)
    (let ((dref (locate object nil)))
      (when dref
        (docstring* dref)))))

(defgeneric source-location* (dref)
  (:documentation "To extend SOURCE-LOCATION, specialize this on a
  subclass of [DREF][class] if that subclass is not RESOLVEable, else
  on the type of object it resolves to. This function is for extension
  only. Do not call it directly.")
  (:method ((dref dref))
    nil)
  (:method (object)
    (let ((dref (locate object nil)))
      (when dref
        (source-location* dref)))))


(defsection @symbol-locatives (:title "Symbol Locatives"
                               :export :dref-ext)
  "Let's see how the obscure DEFINE-SYMBOL-LOCATIVE-TYPE and
  DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE macros work together to
  simplify the common task of associating definition and documentation
  with symbols in a certain context."
  (define-symbol-locative-type macro)
  (define-definer-for-symbol-locative-type macro))

(declaim (ftype function find-method*))

(defmacro symbol-lambda-list-method (symbol locative-type)
  `(find-method* #'symbol-lambda-list () `((eql ,,symbol) (eql ,,locative-type))
                 nil))

(defclass symbol-locative-dref (dref) ())

(defmacro define-symbol-locative-type (locative-type lambda-list
                                       &body docstring)
  """Similar to DEFINE-LOCATIVE-TYPE, but it assumes that all things
  LOCATEable with LOCATIVE-TYPE are going to be symbols defined with a
  definer defined with DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE. Symbol
  locatives are for attaching a definition (along with arglist,
  documentation and source location) to a symbol in a particular
  context. An example will make everything clear:

  ```
  (define-symbol-locative-type direction ()
    "A direction is a symbol.")

  (define-definer-for-symbol-locative-type define-direction direction
    "With DEFINE-DIRECTION, one can document what a symbol means when
    interpreted as a DIRECTION.")

  (define-direction up ()
    "UP is equivalent to a coordinate delta of (0, -1).")
  ```

  After all this, `(DREF 'UP 'DIRECTION)` refers to the
  `DEFINE-DIRECTION` form above."""
  (check-body-docstring docstring)
  (let ((dref-class (intern (format nil "~A-~A" (symbol-name locative-type)
                                    'dref))))
    `(progn
       (define-locative-type ,locative-type ,lambda-list ,@docstring)
       (defclass ,dref-class (symbol-locative-dref) ())
       (defmethod dref* (symbol (locative-type (eql ',locative-type))
                         locative-args)
         (check-locative-args ,locative-type locative-args)
         ;; Faster than calling SYMBOL-LAMBDA-LIST-METHOD.
         (when (nth-value 1
                          (ignore-errors
                           (values (symbol-lambda-list symbol locative-type))))
           (locate-error))
         (%make-dref ',dref-class symbol (cons locative-type locative-args))))))

;;; SOURCE-LOCATION (method () (symbol-locative-dref)) is defined
;;; later, when Swank is available.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun check-body-docstring (docstring)
    (assert (or (endp docstring)
                (and (= 1 (length docstring))
                     (string (first docstring)))))))

;;; A somewhat dummy generic function whose methods are
;;; EQL-specialized on SYMBOL and LOCATIVE-TYPE. The appropriate
;;; method's docstring is the docstring of SYMBOL as LOCATIVE-TYPE, It
;;; returns the LAMBDA-LIST given in the definition and the *PACKAGE*
;;; that was in effect at the time.
(defgeneric symbol-lambda-list (symbol locative-type))

(defmacro define-definer-for-symbol-locative-type
    (name locative-type &body docstring)
  "Define a macro with NAME that can be used to attach a lambda list,
  documentation, and source location to a symbol in the context of
  LOCATIVE-TYPE. The defined macro's arglist is `(SYMBOL LAMBDA-LIST
  &OPTIONAL DOCSTRING)`. LOCATIVE-TYPE is assumed to have been defined
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
     (values ',lambda-list ,*package*)))
