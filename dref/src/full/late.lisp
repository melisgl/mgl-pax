(cl:in-package :dref)

(in-readtable pythonic-string-syntax)

(defun/autoloaded definitions (name &key (locative-types (lisp-locative-types)))
  """Return all definitions of NAME that match LOCATIVE-TYPES
  as a list of [DREF][class]s.

  The DREF-NAMEs may not be the same as NAME, for example, when NAME
  is a package nickname:

  ```cl-transcript
  (definitions 'pax)
  ==> (#<DREF "MGL-PAX" PACKAGE>)
  ```

  Can be extended via MAP-DEFINITIONS."""
  (delete-duplicates
   (let ((drefs ())
         (swank-locative-types ()))
     (dolist (locative-type locative-types)
       (let ((mapper (map-definitions (lambda (dref)
                                        (push dref drefs))
                                      name locative-type)))
         (when (eq mapper 'swank-definitions)
           (push locative-type swank-locative-types))))
     (append drefs (swank-definitions name swank-locative-types)))
   :test #'xref=))

(defun/autoloaded dref-apropos (name &key package external-only case-sensitive
                                     (locative-types '(:lisp)))
  """Return a list of [DREF][class]s corresponding to existing
  definitions that match the various arguments. First, `(DREF-APROPOS
  NIL :LOCATIVE-TYPES NIL)` lists all definitions in the system.
  Arguments with non-NIL values filter the list of definitions.

  Roughly speaking, when NAME or PACKAGE is a SYMBOL, they must match
  the whole @NAME of the definition:

  ```cl-transcript
  (dref-apropos 'method :package :dref :external-only t)
  ==> (#<DREF METHOD CLASS> #<DREF METHOD LOCATIVE>)
  ```

  On the other hand, when NAME or PACKAGE is a STRING, they are
  matched as substrings:

  ```cl-transcript
  (dref-apropos "method" :package :dref :external-only t)
  ==> (#<DREF METHOD CLASS> #<DREF METHOD LOCATIVE>
  -->  #<DREF METHOD-COMBINATION CLASS> #<DREF METHOD-COMBINATION LOCATIVE>)
  ```

  The list of LOCATIVE-TYPES, if non-NIL, filters out definitions
  whose locative types are not listed:

  ```cl-transcript
  (dref-apropos "method" :package :dref :external-only t
                :locative-types '(class))
  ==> (#<DREF METHOD CLASS> #<DREF METHOD-COMBINATION CLASS>)
  ```

  In the list, the special keywords :ALL, :LISP, :PSEUDO match all
  LOCATIVE-TYPES, LISP-LOCATIVE-TYPES and PSEUDO-LOCATIVE-TYPES,
  respectively.

  When PACKAGE is :NONE, only non-symbol @NAMES are matched:

  ```
  (dref-apropos "dref" :package :none)
  ==> (#<DREF "DREF" PACKAGE> #<DREF "DREF-EXT" PACKAGE>
  -->  #<DREF "DREF-TEST" PACKAGE> #<DREF "dref" ASDF/SYSTEM:SYSTEM>
  -->  #<DREF "dref/full" ASDF/SYSTEM:SYSTEM>
  -->  #<DREF "dref/test" ASDF/SYSTEM:SYSTEM>
  -->  #<DREF "dref/test-autoload" ASDF/SYSTEM:SYSTEM>)
  ```

  The exact rules of filtering are as follows. Let `C` be the @NAME of
  the candidate definition from the list of all definitions that we
  are matching against the arguments and denote its string
  representation `(PRINC-TO-STRING C)` with `P`. Note that
  PRINC-TO-STRING does not print the package of symbols. We say that
  two strings _match_ if CASE-SENSITIVE is NIL and they are EQUALP, or
  CASE-SENSITIVE is true and they are EQUAL. CASE-SENSITIVE affects
  _substring_ comparisons too.

  - If NAME is a SYMBOL, then its SYMBOL-NAME must _match_ `P`.

  - If NAME is a STRING, then it must be a _substring_ of `P`.

  - If PACKAGE is :NONE, then `C` must _not_ be a SYMBOL.

  - If PACKAGE is not NIL or :NONE, then `C` must be a symbol.

  - If PACKAGE is a [PACKAGE][class], it must be EQ to the
    SYMBOL-PACKAGE of `C`.

  - If PACKAGE is a SYMBOL other than :NONE, then its SYMBOL-NAME must
    _match_ the PACKAGE-NAME or one of the PACKAGE-NICKNAMES of
    SYMBOL-PACKAGE of `C`.

  - If PACKAGE is a STRING, then it must be a _substring_ of the
    PACKAGE-NAME of SYMBOL-PACKAGE of `C`.

  - If EXTERNAL-ONLY and `C` is a symbol, then `C` must be external in
    a matching package.

  - If LOCATIVE-TYPES is NIL, then it matches everything.

  - If LOCATIVE-TYPEs is non-NIL, then the LOCATIVE-TYPE of the
    candidate definition must be in it (handling :ALL,
    :LISP, and :PSEUDO as described above).

  Can be extended via MAP-NAMES."""
  (let ((locative-types (expand-apropos-locative-types locative-types))
        (char-test (if case-sensitive #'char= #'char-equal))
        (string-test (if case-sensitive #'string= #'string-equal))
        (matching-names (make-hash-table :test #'equal))
        (locative-types-to-try-with-interned-symbols ()))
    (labels ((matching-name-p (name-1)
               (and (or (null name)
                        (and (symbolp name)
                             (funcall string-test (symbol-name name)
                                      (princ-to-string name-1)))
                        (and (stringp name)
                             (search name (princ-to-string name-1)
                                     :test char-test)))
                    ;; FIXME: Support :ANY.
                    (if (eq package :none)
                        (not (symbolp name-1))
                        (not (and (stringp name-1) package)))))
             (matching-package-p (package-1)
               (or (null package)
                   (and (packagep package)
                        (eq package-1 package))
                   (and (symbolp package)
                        (not (eq package :none))
                        (find (symbol-name package)
                              (cons (package-name package-1)
                                    (package-nicknames package-1))
                              :test string-test))
                   (and (stringp package)
                        (find-if (lambda (package-name-1)
                                   (search package package-name-1
                                           :test char-test))
                                 (cons (package-name package-1)
                                       (package-nicknames package-1))))))
             (matching-reference-p (dref)
               (let ((name (dref-name dref))
                     (locative-type (dref-locative-type dref)))
                 (and (matching-name-p name)
                      (member locative-type locative-types))))
             (consider (name)
               (when (matching-name-p name)
                 (setf (gethash name matching-names) t))))
      ;; Populate MATCHING-NAMES with @NAMEs that combine with some
      ;; locative whose type is in LOCATIVE-TYPES.
      (dolist (locative-type locative-types)
        (let ((mapper (map-names #'consider locative-type)))
          (when (and mapper (symbolp mapper))
            (assert (eq mapper 'try-interned-symbols))
            (push locative-type locative-types-to-try-with-interned-symbols))))
      ;; For many locative types, we need to consider all symbols as
      ;; @NAMEs. Let's do that in a single run. This where MAP-NAMES
      ;; for different locative types that return TRY-INTERNED-SYMBOLS
      ;; join.
      (when (and (not (eq package :none))
                 locative-types-to-try-with-interned-symbols)
        (dolist (package-1 (remove (find-package :keyword)
                                   (list-all-packages)))
          (when (matching-package-p package-1)
            (if external-only
                (with-package-iterator (next package-1 :external)
                  (loop (multiple-value-bind (morep symbol) (next)
                          (if morep
                              (consider symbol)
                              (return)))))
                (with-package-iterator (next package-1 :external :internal)
                  (loop (multiple-value-bind (morep symbol) (next)
                          (if morep
                              (consider symbol)
                              (return)))))))))
      (sort-references
       (remove-duplicate-drefs/nonstable
        ;; Filter again, this time on the canonical form of the DREF.
        (remove-if-not
         #'matching-reference-p
         (loop for name in (hash-table-keys matching-names)
               append (definitions name :locative-types locative-types))))))))

(defun expand-apropos-locative-types (locative-types)
  (if locative-types
      (delete-duplicates (loop for locative-type in locative-types
                               append (case locative-type
                                        ((:all) (locative-types))
                                        ((:lisp) (lisp-locative-types))
                                        ((:pseudo) (pseudo-locative-types))
                                        (t
                                         (list locative-type)))))
      (locative-types)))

;;; Order REFERENCES in an implementation independent way.
;;; PAX:PAX-APROPOS depends on non-symbol names coming first.
(defun sort-references (references)
  (flet ((key (reference)
           (let ((locative-type (xref-locative-type reference)))
             (with-standard-io-syntax*
               ;; Avoid mentions of BASE-CHAR and such.
               (let ((*print-readably* nil))
                 (if (not (symbolp (xref-name reference)))
                     ;; Non-symbol named references go first ("1.")
                     ;; and are sorted by locative type, name,
                     ;; locative args in that order.
                     (format nil "1. ~A ~S ~S"
                             (locative-type-to-sort-key locative-type)
                             (xref-name reference)
                             (xref-locative-args reference))
                     ;; Symbol-based reference go after ("2.") and are
                     ;; sorted by name first.
                     (format nil "2. ~S ~A ~S"
                             (xref-name reference)
                             (locative-type-to-sort-key locative-type)
                             (xref-locative-args reference))))))))
    (sort-list-with-precomputed-key references #'string< :key #'key)))

(defun locative-type-to-sort-key (locative-type)
  (format nil "~S ~S"
          ;; Sort by SYMBOL-NAME before SYMBOL-PACKAGE of LOCATIVE-TYPE.
          (if (eq locative-type 'unknown)
              ;; The unknown references go last for the same @NAME.
              "~~~~"
              (symbol-name locative-type))
          (package-name (symbol-package locative-type))))

;;; Like REMOVE-DUPLICATES but does not maintain a stable order and
;;; faster because it first groups DREFs by name. The length of DREFs
;;; can easily be in the tens of thousands.
(defun remove-duplicate-drefs/nonstable (drefs)
  (let ((name-to-drefs (make-hash-table :test #'equal)))
    (dolist (dref drefs)
      (push dref (gethash (dref-name dref) name-to-drefs)))
    (loop for name being the hash-keys of name-to-drefs
          append (delete-duplicates (gethash name name-to-drefs)
                                    :test #'xref=))))
