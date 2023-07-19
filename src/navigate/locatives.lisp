(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @pax-locatives (:title "PAX Locatives")
  "To the DREF::@LOCATIVE-TYPES defined by DRef, PAX adds a few of its own."
  (section locative)
  (glossary-term locative)
  (dislocated locative)
  (argument locative)
  (include locative)
  (docstring locative)
  (go locative)
  (clhs locative))


;;;; SECTION locative

(define-locative-type section ()
  "Refers to a [SECTION][class] defined by DEFSECTION.

  SECTION is EXPORTABLE-LOCATIVE-TYPE-P but not exported by
  default (see EXPORTABLE-REFERENCE-P).")

(define-definition-class section section-dref (variable-dref))

(defun section-title-or-name (section)
  (or (section-title section)
      (let ((*print-case* :upcase))
        (prin1-to-string (section-name section)))))

(defmethod locate* ((section section))
  (make-instance 'section-dref :name (section-name section) :locative 'section))

(defmethod dref* (symbol (locative-type (eql 'section))
                                       locative-args)
  (check-locative-args section locative-args)
  (unless (and (symbolp symbol)
               (boundp symbol)
               (typep (symbol-value symbol) 'section))
    (locate-error))
  (make-instance 'section-dref :name symbol :locative 'section))

(defmethod resolve* ((dref section-dref))
  (symbol-value (dref-name dref)))

(defun actualize-variable-to-section (dref)
  (when (eq (dref-locative-type dref) 'variable)
    (dref (dref-name dref) 'section nil)))

(add-dref-actualizer 'actualize-variable-to-section)

(defmethod docstring* ((dref section-dref))
  nil)


;;;; GLOSSARY-TERM locative

(defun glossary-term-title-or-name (glossary-term)
  (or (glossary-term-title glossary-term)
      (prin1-to-string (glossary-term-name glossary-term))))

(defmethod print-object ((glossary-term glossary-term) stream)
  (print-unreadable-object (glossary-term stream :type t)
    (format stream "~a" (glossary-term-name glossary-term))))

(define-locative-type glossary-term ()
  "Refers to a [GLOSSARY-TERM][class] defined by DEFINE-GLOSSARY-TERM.

  GLOSSARY-TERM is EXPORTABLE-LOCATIVE-TYPE-P but not exported by
  default (see EXPORTABLE-REFERENCE-P).")

(define-definition-class glossary-term glossary-term-dref (variable-dref))

(defmethod locate* ((glossary-term glossary-term))
  (make-instance 'glossary-term-dref :name (glossary-term-name glossary-term)
                 :locative 'glossary-term))

(defmethod dref*
    (symbol (locative-type (eql 'glossary-term)) locative-args)
  (check-locative-args glossary-term locative-args)
  (unless (and (symbolp symbol)
               (boundp symbol)
               (typep (symbol-value symbol) 'glossary-term))
    (locate-error))
  (make-instance 'glossary-term-dref :name symbol :locative 'glossary-term))

(defmethod resolve* ((dref glossary-term-dref))
  (symbol-value (dref-name dref)))

(defun actualize-variable-to-glossary-term (dref)
  (when (eq (dref-locative-type dref) 'variable)
    (dref (dref-name dref) 'glossary-term nil)))

(add-dref-actualizer 'actualize-variable-to-glossary-term)

(defmethod docstring* ((dref glossary-term-dref))
  (glossary-term-docstring (resolve dref)))


;;;;; GO locative

(define-pseudo-locative-type go ((name locative))
  """Redirect to a definition in the context of the DREF::@REFERENCE
  designated by NAME and LOCATIVE. This pseudolocative is intended for
  things that have no explicit global definition.

  As an example, consider this part of a hypothetical documentation of
  CLOS:

      (defsection @clos ()
        (defmethod macro)
        (call-next-method (go (defmethod macro))))

  The GO reference exports the symbol CALL-NEXT-METHOD and also
  produces a terse redirection message in the documentation.

  GO behaves as described below.

  - A GO reference RESOLVEs to what NAME with LOCATIVE resolves to:

      ```cl-transcript (:dynenv pax-std-env)
      (resolve (dref 'xxx '(go (print function))))
      ==> #<FUNCTION PRINT>
      ```

  - The DOCSTRING of a GO reference is NIL.

  - SOURCE-LOCATION (thus `\\M-.`) returns the source location of the
    embedded reference:

      ```cl-transcript
      (equal (source-location (dref 'xxx '(go (print function))))
             (source-location (dref 'print 'function)))
      => T
      ```""")

(define-definition-class go go-dref (dref)
  ((target-dref :initarg :target-dref :reader go-target-dref)))

(defmethod dref* (name (locative-type (eql 'go)) locative-args)
  (check-locative-args go locative-args)
  (destructuring-bind ((go-name go-locative)) locative-args
    (let ((go-dref (dref go-name go-locative)))
      (make-instance 'go-dref :name name
                              :locative `(go (,(dref-name go-dref)
                                              ,(dref-locative go-dref)))
                              :target-dref go-dref))))

(defmethod map-definitions (name (locative-type (eql 'go)))
   (declare (ignorable name))
  ;; There are no real GO definitions.
  (values))

(defmethod resolve* ((dref go-dref))
  (resolve* (apply #'dref (first (dref-locative-args dref)))))

(defmethod source-location* ((dref go-dref))
  (source-location (apply #'dref (first (dref-locative-args dref)))))


;;;; DISLOCATED locative

(define-pseudo-locative-type dislocated ()
  "Refers to a symbol in a non-specific context. Useful for preventing
  [autolinking][@explicit-and-autolinking section]. For example, if
  there is a function called `FOO` then

      `FOO`

  will be linked (if *DOCUMENT-LINK-CODE*) to its definition. However,

      [`FOO`][dislocated]

  will not be. With a dislocated locative, LOCATE always fails with a
  LOCATE-ERROR condition. Also see @PREVENTING-AUTOLINKING.

  DISLOCATED references do not RESOLVE.")

(defmethod dref* (symbol (locative-type (eql 'dislocated)) locative-args)
  (declare (ignorable symbol locative-args))
  (locate-error "~S can never be located." 'dislocated))


;;;; ARGUMENT locative

(define-pseudo-locative-type argument ()
  """An alias for DISLOCATED, so that one can refer to an argument of
  a macro without accidentally linking to a class that has the same
  name as that argument. In the following example,
  [FORMAT][dislocated] may link to CL:FORMAT (if we generated
  documentation for it):

  ```
  "See FORMAT in DOCUMENT."
  ```

  Since ARGUMENT is a locative, we can prevent that linking by writing:

  ```
  "See the FORMAT argument of DOCUMENT."
  ```

  ARGUMENT references do not RESOLVE.""")

(defmethod dref* (symbol (locative-type (eql 'argument)) locative-args)
  (declare (ignorable symbol locative-args))
  (locate-error "~S can never be located." 'argument))


;;;; DOCSTRING

(define-pseudo-locative-type docstring ()
  "[translate-docstring-links function][docstring]

  There is no way to LOCATE DOCSTRINGs, so nothing to RESOLVE either.")

(defmethod dref* (symbol (locative-type (eql 'docstring)) locative-args)
  (declare (ignorable symbol locative-args))
  (locate-error "DOCSTRING can never be located."))


;;;; INCLUDE locative

(define-pseudo-locative-type include (source &key line-prefix header footer
                                             header-nl footer-nl)
  """This pseudolocative refers to a region of a file. SOURCE can be a
  [STRING][type] or a [PATHNAME][type], in which case the whole file
  is being pointed to, or it can explicitly supply START, END
  locatives. INCLUDE is typically used to include non-lisp files in
  the documentation (say markdown or Elisp as in the next example) or
  regions of Lisp source files. This can reduce clutter and
  duplication.

  ```
  (defsection @example-section ()
    (mgl-pax.el (include #.(asdf:system-relative-pathname
                            :mgl-pax "src/mgl-pax.el")
                         :header-nl "```elisp" :footer-nl "```"))
    (foo-example (include (:start (dref-ext:make-source-location function)
                           :end (dref-ext:source-location-p function))
                          :header-nl "```"
                          :footer-nl "```")))
  ```

  In the above example, when documentation is generated, the entire
  `src/mgl-pax.el` file is included in the markdown output surrounded
  by the strings given as HEADER-NL and FOOTER-NL. The documentation
  of `FOO-EXAMPLE` will be the region of the file from the
  SOURCE-LOCATION of the START reference (inclusive) to the
  SOURCE-LOCATION of the END reference (exclusive). If only one of
  START and END is specified, then they default to the beginning and
  end of the file, respectively.

  Since START and END are literal references, pressing `\\M-.` on
  `PAX.EL` will open the `src/mgl-pax.el` file and put the cursor on
  its first character. `\\M-.` on `FOO-EXAMPLE` will go to the source
  location of the `FOO` function.

  With the LAMBDA locative, one can specify positions in arbitrary
  files as in, for example, PAX::@EMACS-SETUP-FOR-BROWSING.

  - SOURCE is either an absolute pathname designator or a list
    matching the [destructuring lambda list][clhs] `(&KEY START END)`,
    where START and END must be NIL or `(<NAME> <LOCATIVE>)`
    lists (not evaluated) like a DEFSECTION entry. Their
    SOURCE-LOCATIONs constitute the bounds of the region of the file
    to be included. Note that the file of the source location of START
    and END must be the same. If SOURCE is a pathname designator, then
    it must be absolute so that the locative is context independent.
  
  - If specified, LINE-PREFIX is a string that's prepended to each
    line included in the documentation. For example, a string of four
    spaces makes markdown think it's a code block.

  - HEADER and FOOTER, if non-NIL, are printed before the included
    string.

  - HEADER-NL and FOOTER-NL, if non-NIL, are printed between two
    FRESH-LINE calls.

  INCLUDE is not EXPORTABLE-LOCATIVE-TYPE-P, and INCLUDE references do
  not RESOLVE.""")

(define-definition-class include include-dref)

(defmethod dref* (name (locative-type (eql 'include)) locative-args)
  (check-locative-args include locative-args)
  (destructuring-bind (source &key line-prefix header footer
                                header-nl footer-nl) locative-args
    (declare (ignore line-prefix header footer header-nl footer-nl))
    ;; Called mostly for validation.
    (let ((file (include-region source)))
      (unless (and file (probe-file file))
        (locate-error "File ~S does not exist." file)))
    (make-instance 'include-dref
                   :name name
                   :locative (cons locative-type locative-args))))

;;; Return the filename plus the START, END source locations of the
;;; region to be included.
(defun include-region (source)
  (cond ((or (stringp source) (pathnamep source))
         (assert (uiop/pathname:absolute-pathname-p source) ()
                 "Pathnames given as the SOURCE argument of the ~
                 INCLUDE locative must be absolute, but ~S is not."
                 source)
         (values source nil nil))
        ((and source (listp source))
         (destructuring-bind (&key start end) source
           (let ((start* (when start
                           (source-location (section-entry-to-xref start))))
                 (end* (when end
                         (source-location (section-entry-to-xref end)))))
             (when start
               (check-source-location start start*))
             (when end
               (check-source-location end end*))
             (let ((start-file (when start* (source-location-file start*)))
                   (end-file (when end* (source-location-file end*))))
               (when (and start* end*
                          (string/= (namestring (truename start-file))
                                    (namestring (truename end-file))))
                 (locate-error "~S starts in file ~S and ends in ~
                               another file ~S."
                               'include start-file end-file))
               (let ((file (or start-file end-file)))
                 (unless file
                   (locate-error "No file specified."))
                 (values file start* end*))))))
        (t
         (locate-error "Malformed ~S ~S." 'include 'source source))))

(defun check-source-location (ref location)
  (unless (source-location-p location)
    (locate-error "~S of ~S is ~S, which is not SOURCE-LOCATION-P."
                  'source-location ref location)))

(defmethod source-location* ((dref include-dref))
  (handler-case
      (let ((dref::*locating-object* nil))
        (multiple-value-bind (file start-location)
            (include-region (first (dref-locative-args dref)))
          (or start-location
              (make-source-location :file file))))
    (locate-error ()
      nil)))


;;;; CLHS locative

(define-pseudo-locative-type clhs (&optional nested-locative)
  """Refers to sections or definitions in the Common Lisp Hyperspec.
  These have no source location so `\\M-.` will not work. What works
  is linking in documentation, including @BROWSING-LIVE-DOCUMENTATION.
  The generated links are relative to *DOCUMENT-HYPERSPEC-ROOT* and
  work even if *DOCUMENT-LINK-TO-HYPERSPEC* is NIL.

  - *definitions*: These are typically unnecessary as DOCUMENT will
    produce the same link for e.g. `\\PPRINT`, `[PPRINT][function]`,
    or `[PPRINT][]` if *DOCUMENT-LINK-TO-HYPERSPEC* is non-NIL and the
    PPRINT function in the running Lisp is not being DOCUMENTed. When
    @BROWSING-LIVE-DOCUMENTATION, a slight difference is that
    everything is being DOCUMENTed, so using the CLHS link bypasses
    the page with the definition in the running Lisp.

      - *unambiguous*: `[pprint][clhs]` ([pprint][clhs])

      - *ambiguous*: `[function][clhs]` ([function][clhs])

      - *explicit*: `[function][(clhs class)]` ([function][(clhs class)])

  - *glossary terms* (case-insensitive):

      - `[lambda list][(clhs glossary-term)]`
        ([lambda list][(clhs glossary-term)])

  - *issues*:

      - `[ISSUE:AREF-1D][clhs]` ([ISSUE:AREF-1D][clhs])

      - `[ISSUE:AREF-1D][(clhs section)]` ([ISSUE:AREF-1D][clhs])

  - *issue summaries*: These render
     as ([SUMMARY:CHARACTER-PROPOSAL:2-6-5][clhs]):

      - `[SUMMARY:CHARACTER-PROPOSAL:2-6-5][clhs]`

      - `[SUMMARY:CHARACTER-PROPOSAL:2-6-5][(clhs section)]`

      Since these summary ids are not particularly reader friendly,
      the alternative form of the @SPECIFIED-LOCATIVE may be used:

      - `[see this][SUMMARY:CHARACTER-PROPOSAL:2-6-5 (clhs
        section)]` ([see this][SUMMARY:CHARACTER-PROPOSAL:2-6-5 (clhs
        section)])

  - *sections*:

      - *by section number*: `[3.4][clhs]` or `[3.4][(clhs
         section)]` ([3.4][clhs])

      - *by section title* (case-insensitive, substring match):
         `[lambda lists][clhs]` or `[lambda lists][(clhs
         section)]` ([lambda lists][clhs])

      - *by filename*: `[03_d][clhs]` or `[03_d][(clhs
         section)]` ([03_d][clhs])

  As the above examples show, the NESTED-LOCATIVE argument of the CLHS
  locative may be omitted. In that case, definitions, glossary terms,
  issues, issue summaries, and sections are considered in that order.
  Sections are considered last because a substring of a section title
  can be matched by chance easily.

  All examples so far used [explicit][ @explicit-and-autolinking]
  links. Autolinking also works if the @NAME is marked up as code or
  is [codified][ @codification] (e.g. in `COS clhs` (COS clhs).

  As mentioned above, `\M-.` does not do anything over CLHS
  references. Slightly more usefully, the [live documentation
  browser][@browsing-live-documentation] understands CLHS links so one
  can enter inputs like `3.4 clhs`, `"lambda list" clhs` or `error (clhs
  function)`.
  
  CLHS references do not RESOLVE.""")

(define-definition-class clhs clhs-dref)

(defparameter *clhs-substring-match* t)

(defmethod dref* (name (locative-type (eql 'clhs)) locative-args)
  (check-locative-args clhs locative-args)
  (or (let ((name-string (if (stringp name)
                             name
                             (princ-to-string name))))
        (flet ((glossary-term? ()
                 (let ((id (find-hyperspec-glossary-entry-id name-string)))
                   (when id
                     (make-instance 'clhs-dref
                                    :name id
                                    :locative '(clhs glossary-term)))))
               (issue-or-section? ()
                 (or (let ((id (find-hyperspec-issue-id name-string)))
                       (when id
                         (make-instance 'clhs-dref
                                        :name id
                                        :locative '(clhs section))))
                     (let ((id (find-hyperspec-section-id
                                name-string
                                :substring-match *clhs-substring-match*)))
                       (when id
                         (make-instance 'clhs-dref
                                        :name id
                                        :locative '(clhs section))))))
               (definition? ()
                 (multiple-value-bind (url nested-locative)
                     (find-hyperspec-definition-url name locative-args)
                   (when url
                     (make-instance 'clhs-dref
                                    :name name
                                    :locative (if nested-locative
                                                  `(clhs ,nested-locative)
                                                  'clhs))))))
          (cond ((equal locative-args '(glossary-term))
                 (glossary-term?))
                ((equal locative-args '(section))
                 (issue-or-section?))
                ((endp locative-args)
                 (or (definition?) (glossary-term?) (issue-or-section?)))
                (t
                 (definition?)))))
      (locate-error)))
