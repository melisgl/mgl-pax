(in-package :dref)

(in-readtable pythonic-string-syntax)

;;; This is not a function so that constant CLASS-NAMEs can be
;;; optimized by the compiler.
(defmacro %make-dref (class-name name locative &rest args)
  `(make-instance ,class-name :name ,name :locative ,locative ,@args))


;;;; DREF-EXT::@LOCATIVE-TYPE-HIERARCHY

(defun check-locative-type-hierarchy (pseudop locative-type superclasses)
  (check-locative-type-shadowing locative-type)
  (check-lisp-and-pseudo-are-distinct pseudop locative-type superclasses)
  (if pseudop
      (check-pseudo-locative-type-hierarchy locative-type)
      (check-lisp-locative-type-hierarchy locative-type superclasses)))

(defun check-locative-type-shadowing (locative-type)
  "- A non-class Lisp type and a locative type with the same name must
     not exist at the same time."
  (when (and
         ;; FIXME: This cannot detect most DEFTYPEs with non-empty
         ;; arglist.
         #-sbcl (valid-type-specifier-p locative-type)
         #+sbcl (sb-ext:defined-type-name-p locative-type)
         (not (find-class locative-type nil)))
    (error "~@<Cannot define ~S as a ~S because it names a type ~
           that's not a class.~:@>" locative-type 'dref::@locative-type)))

(defun check-lisp-and-pseudo-are-distinct (pseudop locative-type superclasses)
  "- The hierarchies of LISP-LOCATIVE-TYPES and PSEUDO-LOCATIVE-TYPES
     are distinct. That is, the DREF-CLASS of a Lisp one must not be a
     subclass of a PSEUDO one, and vice versa."
  (dolist (l2 (if pseudop
                  *lisp-locative-types*
                  *pseudo-locative-types*))
    (let* ((d2 (dref-class l2))
           (subclass-superclass (find-subclass-of d2 superclasses)))
      (when subclass-superclass
        (cerror "Continue."
                "~@<~S of ~S with superclasses ~S is illegal ~
                because ~S~? is the ~S of ~S, ~
                one of ~S.~:@>"
                (if pseudop
                    'define-pseudo-locative-type
                    'define-locative-type)
                locative-type superclasses
                subclass-superclass
                (if (eq subclass-superclass d2) "" " is a subclass of ~S")
                (if (eq subclass-superclass d2) () `(,d2))
                'dref-class l2
                (if pseudop
                    'lisp-locative-types
                    'pseudo-locative-types))))))

(defun check-lisp-locative-type-hierarchy (locative-type superclasses)
  "- For locative types `L1` and `L2`, both LISP-LOCATIVE-TYPES and
     both naming a CLASS, SUBTYPEP of their respective DREF-CLASSes
     must be the same as `(SUBTYPEP L1 L2)`."
  (let ((c (find-class locative-type nil)))
    ;; Adding a classless locative type is always legal because at the
    ;; time of their definition they have no subtypes, so it cannot
    ;; contradict the Lisp class hierarchy.
    (when c
      ;; Adding a locative type A that also names an existing class
      ;; can contradict the Lisp side. The invariant is A-DREF ->
      ;; B-DREF iff A -> B for all locative types that name classes.
      (dolist (l2 *locative-types*)
        (when (and (not (eq locative-type l2))
                   (find-class l2 nil))
          (let* ((d2 (dref-class l2))
                 (subclass-superclass (find-subclass-of d2 superclasses)))
            (unless (eq (not (subtypep c l2))
                        (null subclass-superclass))
              (inconsistent-hierarchies locative-type superclasses
                                        subclass-superclass l2 d2))))))))

(defun check-pseudo-locative-type-hierarchy (locative-type)
  "- PSEUDO-LOCATIVE-TYPES must not name a CLASS."
  (when (find-class locative-type nil)
    (cerror "Continue."
            "~@<~S of ~S is illegal because ~S names a class.~:@>"
            'define-pseudo-locative-type locative-type locative-type)))

(defun find-subclass-of (class classes)
  (loop for class-1 in classes
        when (subtypep class-1 class)
          return class-1))

(defun inconsistent-hierarchies (locative-type superclasses
                                 subclass-superclass other-locative-type
                                 other-dref-class)
  (apply #'cerror
         "Continue."
         "~@<~S of ~S with superclasses ~S ~
         contradicts the Lisp class hierarchy where ~S is ~Aa subclass ~
         of ~S, whose DRef class is ~S~?.~:@>"
         'define-locative-type locative-type superclasses
         locative-type (if subclass-superclass "not " "")
         other-locative-type other-dref-class
         (cond ((eq subclass-superclass other-dref-class)
                '("" ()))
               ((null subclass-superclass)
                `(", but none of the superclasses is a subclass of ~S"
                  (,other-dref-class)))
               (t
                `(", and ~S is a subclass of ~S"
                  (,subclass-superclass ,other-dref-class))))))

(defun %declare-locative-type (pseudop locative-type)
  (if pseudop
      (declare-pseudo-locative-type locative-type)
      (declare-locative-type locative-type)))

;;; LOCATIVE-TYPE -> (DREF-CLASS-NAME SUPER-CLASS-NAMES
;;;                   LOCATIVE-TYPE-DIRECT-SUPERS LOCATIVE-TYPE-DIRECT-SUBS)
;;;
;;; E.g. READER -> (READER-DREF (METHOD-DREF) (METHOD) (ACCESSOR))
(defvar *locative-type-to-class-info* (make-hash-table))

(defvar *dref-class-to-locative-type* (make-hash-table))

(declaim (inline %locative-type-class-info))
(defun %locative-type-class-info (locative-type)
  (gethash locative-type *locative-type-to-class-info*))

(defun dref-class (locative-type)
  "Return the name of the CLASS used to represent @DEFINITIONs with
  LOCATIVE-TYPE. This is always a subclass of [DREF][class].

  Note that the actual TYPE-OF a DREF is mostly intended for
  DREF-EXT::@EXTENDING-DREF. Hence, it is hidden when a DREF is
  printed:

  ```cl-transcript
  (dref 'print 'function)
  ==> #<DREF PRINT FUNCTION>
  (type-of *)
  => FUNCTION-DREF
  ```

  Due to [actualization][add-dref-actualizer function], the actual
  type may be a proper subtype of DREF-CLASS:

  ```cl-transcript
  (dref 'documentation 'function)
  ==> #<DREF DOCUMENTATION GENERIC-FUNCTION>
  (type-of *)
  => GENERIC-FUNCTION-DREF
  (subtypep 'generic-function-dref 'function-dref)
  => T
  => T
  ```"
  (first (%locative-type-class-info locative-type)))

(defun dref-class-superclasses (locative-type)
  (second (%locative-type-class-info locative-type)))

(defun locative-type-direct-supers (locative-type)
  "List the @LOCATIVE-TYPEs whose DREF-CLASSes are direct superclasses
  of the DREF-CLASS of LOCATIVE-TYPE. These can be considered
  supertypes of LOCATIVE-TYPE in the sense of DTYPEP."
  (third (%locative-type-class-info locative-type)))

(defun locative-type-direct-subs (locative-type)
  "List the @LOCATIVE-TYPEs whose DREF-CLASSes are direct subclasses
  of the DREF-CLASS of LOCATIVE-TYPE. These can be considered subtypes
  of LOCATIVE-TYPE in the sense of DTYPEP."
  (fourth (%locative-type-class-info locative-type)))

(defun dref-class-to-locative-type (dref-class)
  (gethash (if (symbolp dref-class)
               dref-class
               (class-name dref-class))
           *dref-class-to-locative-type*))

(defun update-class-info (locative-type dref-class superclasses)
  (let ((old-supers (locative-type-direct-supers locative-type))
        (new-supers (%locative-type-direct-supers superclasses))
        (subs (%locative-type-direct-subs locative-type dref-class))
        (info-map *locative-type-to-class-info*))
    ;; Update the subs of deleted supers.
    (dolist (deleted-super (set-difference old-supers new-supers))
      (setf (fourth (gethash deleted-super info-map))
            (remove locative-type (fourth (gethash deleted-super info-map)))))
    ;; Update the subs of newly added supers.
    (dolist (added-super (set-difference new-supers old-supers))
      ;; APPEND to keep them ordered by time of definition.
      (setf (fourth (gethash added-super info-map))
            (append (fourth (gethash added-super info-map))
                    (list locative-type))))
    ;; Update LOCATIVE-TYPE's class info.
    (setf (gethash locative-type info-map)
          (list dref-class superclasses new-supers subs)))
  (setf (gethash dref-class *dref-class-to-locative-type*) locative-type))

(defun %locative-type-direct-supers (superclasses)
  (remove nil (mapcar #'dref-class-to-locative-type superclasses)))

(defun %locative-type-direct-subs (locative-type dref-class)
  (loop for locative-type-1 being the hash-key in *locative-type-to-class-info*
          using (hash-value info)
        when (and (not (eq locative-type-1 locative-type))
                  (member dref-class (second info)))
          collect locative-type-1))


;;;; DREF-EXT::@ADDING-NEW-LOCATIVES

(defmacro define-locative-type (locative-type-and-lambda-list
                                locative-supertypes
                                &optional docstring dref-defclass-form)
  """Declare [LOCATIVE-TYPE][argument] as a [LOCATIVE][locative],
  which is the first step in DREF-EXT::@EXTENDING-DREF.

  - *Simple example:* To define a locative type called `DUMMY` that
    takes no arguments and is not a locative subtype of any other
    locative type:

      ```
      (define-locative-type dummy ()
        "Dummy docstring.")
      ```

      With this definition, only the locatives `DUMMY` and its
      equivalent form `(DUMMY)` are valid. The above defines a DREF
      subclass called `DUMMY-DREF` in the current package. All
      definitions with locative type `DUMMY` and its locatives
      subtypes must be instances of `DUMMY-DREF`.

      `(LOCATE 'DUMMY 'LOCATIVE)` refers to this definition. That is,
      ARGLIST, [DOCSTRING][function] and SOURCE-LOCATION all work on
      it.

  - *Complex example:* DUMMY may have arguments `X` and `Y` and
    inherit from locative types `L1` and `L2`:

      ```
      (define-locative-type (dummy x &key y) (l1 l2)
        "Dummy docstring."
        (defclass dummy-dref ()
          ((xxx :initform nil :accessor dummy-xxx))))
      ```

      One may change name of `DUMMY-DREF`, specify superclasses and
      add slots as with DEFCLASS. Behind the scenes, the DREF classes
      of `L1` and `L2` are added automatically to the list of
      superclasses.

  Arguments:

  - The general form of LOCATIVE-TYPE-AND-LAMBDA-LIST
    is (LOCATIVE-TYPE &REST LAMBDA-LIST), where LOCATIVE-TYPE is a
    SYMBOL, and LAMBDA-LIST is a [destructuring lambda list][clhs].
    The LOCATIVE-ARGS of [DREF][class]s with @LOCATIVE-TYPE
    LOCATIVE-TYPE (the argument given to this macro) always conform to
    this lambda list. See CHECK-LOCATIVE-ARGS.

      If LOCATIVE-TYPE-AND-LAMBDA-LIST is a single symbol, then that's
      interpreted as LOCATIVE-TYPE, and LAMBDA-LIST is NIL.

  - LOCATIVE-SUPERTYPES is a list of @LOCATIVE-TYPEs whose DREF
    classes are added to prepended to the list of superclasses this
    definition.

  Locative types defined with DEFINE-LOCATIVE-TYPE can be listed with
  LISP-LOCATIVE-TYPES."""
  `(%define-locative-type nil ,locative-type-and-lambda-list
                          ,locative-supertypes ,docstring ,dref-defclass-form
                          nil))

(defmacro %define-locative-type (pseudop locative-type-and-lambda-list
                                 locative-supertypes docstring
                                 dref-defclass-form extra-superclasses)
  (destructuring-bind (locative-type &rest lambda-list)
      (ensure-list* locative-type-and-lambda-list)
    (destructuring-bind (&optional dref-defclass dref-class dref-superclasses
                           dref-slots)
        dref-defclass-form
      (declare (ignore dref-class))
      (when (and dref-defclass-form
                 (not (eq dref-defclass 'defclass)))
        (error "~@<When defining locative type ~S, the argument ~S ~
               should start with ~S~:@>"
               locative-type dref-defclass-form 'defclass))
      (let* ((superclasses
               (loop for superlocative in locative-supertypes
                     for dref-class = (dref-class superlocative)
                     do (check-locative-type superlocative)
                     when dref-class
                       collect dref-class))
             (extra-superclasses (or extra-superclasses
                                     ;; FIXME: only if necessary
                                     '(dref)))
             (all-superclasses (append superclasses
                                       dref-superclasses
                                       extra-superclasses))
             (dref-class (maybe-default-dref-class locative-type-and-lambda-list
                                                   dref-defclass-form)))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (check-locative-type-hierarchy ,pseudop ',locative-type
                                          ',all-superclasses)
           (%declare-locative-type ,pseudop ',locative-type)
           (defmethod locative-type-lambda-list
               ((symbol (eql ',locative-type)))
             (values ',lambda-list ,docstring ,*package*))
           (defclass ,dref-class ,all-superclasses
             ,dref-slots)
           (update-class-info ',locative-type ',dref-class ',superclasses))))))

(defun maybe-default-dref-class (locative-type-and-lambda-list dref-class-def)
  (or (second dref-class-def)
      (intern
       (let ((locative-type (locative-type locative-type-and-lambda-list)))
         (format nil "~A-~A" (symbol-name locative-type) 'dref)))))

(defmacro define-pseudo-locative-type (locative-type-and-lambda-list
                                       locative-supertypes
                                       &optional docstring dref-defclass-form)
  """Like DEFINE-LOCATIVE-TYPE, but declare that
  [LOCATIVE-TYPE][argument] does not correspond to definitions in the
  running Lisp. Definitions with pseudo locatives are of DTYPE PSEUDO
  and are not listed by default by DEFINITIONS.

  Locative types defined with DEFINE-PSEUDO-LOCATIVE-TYPE can be
  listed with PSEUDO-LOCATIVE-TYPES."""
  `(%define-locative-type t ,locative-type-and-lambda-list ,locative-supertypes
                          ,docstring ,dref-defclass-form nil))

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

;;; Same as CHECK-LOCATIVE-ARGS, but LOCATIVE-TYPE is evaluated and it
;;; signals a SIMPLE-ERROR.
(defun check-locative-args* (locative-type locative-args)
  (let ((lambda-list (locative-type-lambda-list locative-type)))
    (unless (ignore-errors
             (handler-bind ((warning #'muffle-warning))
               (eval `(destructuring-bind ,lambda-list ',locative-args
                        (declare (ignore ,@(macro-arg-names lambda-list)))
                        t))))
      (error "~@<Bad arguments ~S for locative ~S with lambda list ~S.~:@>"
             locative-args locative-type lambda-list))))

(defmacro define-locative-alias (alias locative-type &body docstring)
  """Define ALIAS that can be substituted for LOCATIVE-TYPE (both
  SYMBOLs) for the purposes of LOCATEing. LOCATIVE-TYPE must
  exist (i.e. be among LOCATIVE-TYPES). For example, let's define
  OBJECT as an alias of the CLASS locative:

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

  Note that LOCATIVE-ALIASES are not LOCATIVE-TYPES and are not valid
  DTYPES.

  Also, see PAX::@LOCATIVE-ALIASES in PAX."""
  (check-docstring-only-body docstring)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defmethod locative-type-lambda-list ((symbol (eql ',alias)))
       (values '(&rest args) ,(first docstring) ,*package*))
     (defmethod dref* (name (locative-type (eql ',alias)) locative-args)
       (dref* name ',locative-type locative-args))
     (declare-locative-alias ',alias)))

(defun check-docstring-only-body (body)
  (assert (or (endp body)
              (and (= (length body) 1)
                   (stringp (first body))))
          () "BODY must be () or a single docstring."))

;;; A somewhat dummy generic function that provides a source location.
;;; It returns LAMBDA-LIST and DOCSTRING from DEFINE-LOCATIVE-TYPE,
;;; and the *PACKAGE* in effect at macroexpansion time.
(defgeneric locative-type-lambda-list (symbol))


(defvar *locating-object*)

;;; To speed LOCATE up, when we know that the actual condition object
;;; does not matter (because *IGNORE-LOCATE-ERROR* is true), use this
;;; premade one.
(defvar *dummy-locate-error*
  (make-condition 'locate-error :object nil :message "" :message-args nil))

(defun locate-error (&optional format-control &rest format-args)
  "Call this function to signal a LOCATE-ERROR condition from the
  [dynamic extent][clhs] of a LOCATE* method (which includes
  DREF*). It is an error to call LOCATE-ERROR elsewhere.

  FORMAT-CONTROL, if non-NIL, is a [format control][clhs] for which
  FORMAT-ARGS are suitable."
  (if *ignore-locate-error*
      (error *dummy-locate-error*)
      (error 'locate-error
             :object *locating-object*
             :message (and (not (equal format-control "")) format-control)
             :message-args format-args)))

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

  Furthermore, if OBJECT is an instance of a CLASS that also names a
  @LOCATIVE-TYPE, then LOCATE* must return a definition.

  This function is for extending LOCATE. Do not call it directly.")
  (:method :around (object)
    (let* ((*locating-object* object)
           (dref (call-next-method)))
      (declare (type dref dref))
      (unless (typep object 'dref)
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
  types. Furthermore, if there is a CLASS named LOCATIVE-TYPE, then
  @DEFINITIONs with LOCATIVE-TYPE must be able to represent exactly
  the set of definitions that define objects of CLASS. For
  example, `(DREF NAME 'FUNCTION)` must find the definition of the
  FUNCTION with global NAME if it exists even if it is a
  GENERIC-FUNCTION but not any other kind of definition.

  This function is for extending LOCATE. Do not call it directly.")
  (:method :around (name locative-type locative-args)
    (declare (ignorable name locative-type locative-args))
    (let ((located (call-next-method)))
      (assert (or (not (typep located 'xref))
                  (typep located 'dref)))
      located))
  (:method (name locative-type locative-args)
    (declare (ignorable name locative-type locative-args))
    (if (find-locative-type locative-type)
        (locate-error "~@<~S ~S has no ~S method defined.~:@>"
                      '@locative-type locative-type 'dref*)
        (locate-error "~@<No such ~S as ~S.~:@>"
                      '@locative-type locative-type))))

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

(defgeneric map-definitions-of-name (fn name locative-type)
  (:documentation "Call FN with [DREF][class]s which can be LOCATEd
  with an XREF with NAME, LOCATIVE-TYPE and some LOCATIVE-ARGS. The
  strange wording here is because there may be multiple ways (and thus
  XREFs) that refer to the same definition.
  
  For most locative types, there is at most one such definition, but
  for METHOD, for example, there may be many. The default method
  simply does `(DREF NAME LOCATIVE-TYPE NIL)` and calls FN with result
  if [DREF][function] succeeds.

  FN must not be called with the same (under XREF=) definition
  multiple times.

  This function is for extending DEFINITIONS and DREF-APROPOS. Do not
  call it directly.")
  ;; See DEFINITIONS for how the efficiency hack of returning the
  ;; magic symbol SWANK-DEFINITIONS instead of mapping with FN works.
  (:method (fn name locative-type)
    (let ((located (dref name locative-type nil)))
      (when located
        (funcall fn located)
        (values)))))

(defgeneric map-definitions-of-type (fn locative-type)
  (:documentation "Call FN with [DREF][class]s which can be LOCATEd
  with an XREF with LOCATIVE-TYPE with some NAME and LOCATIVE-ARGS.

  The default method forms XREFs by combining each interned symbol as
  @NAMEs with LOCATIVE-TYPE and no LOCATIVE-ARGS and calls FN if it
  LOCATEs a definition.

  FN may be called with DREFs that are XREF= but differ in the XREF in
  their DREF-ORIGIN.

  This function is for extending DREF-APROPOS. Do not call it
  directly.")
  (:method (fn locative-type)
    (declare (ignore fn locative-type))
    ;; See DREF-APROPOS about the magic symbol TRY-INTERNED-SYMBOLS.
    'try-interned-symbols))

(defun map-names-for-type (fn locative-type)
  ;; This is wasteful in that a DREF is created from an XREF while we
  ;; are only interested in the XREF-NAME.
  (map-definitions-of-type (lambda (dref)
                             (funcall fn (xref-name (dref-origin dref))))
                           locative-type))

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


;;;; @SYMBOL-LOCATIVES

(declaim (ftype function find-method*))

(defmacro symbol-lambda-list-method (symbol locative-type)
  `(find-method* #'symbol-lambda-list () `((eql ,,symbol) (eql ,,locative-type))
                 nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass symbol-locative-dref (dref) ()))

(defmacro define-symbol-locative-type
    (locative-type-and-lambda-list locative-supertypes
     &optional docstring dref-class-def)
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
  (let ((locative-type (locative-type locative-type-and-lambda-list))
        (dref-class (maybe-default-dref-class locative-type-and-lambda-list
                                              dref-class-def)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%define-locative-type nil ,locative-type-and-lambda-list
                              ,locative-supertypes ,docstring ,dref-class-def
                              (symbol-locative-dref))
       (defmethod dref* (symbol (locative-type (eql ',locative-type))
                         locative-args)
         (check-locative-args ,locative-type locative-args)
         ;; Faster than calling SYMBOL-LAMBDA-LIST-METHOD.
         (unless (succeedsp (symbol-lambda-list symbol locative-type))
           (locate-error))
         (%make-dref ',dref-class
                     symbol (cons locative-type locative-args))))))

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
