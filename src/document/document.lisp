(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @generating-documentation (:title "Generating Documentation")
  (@document-function section)
  (mgl-pax/document asdf:system)
  (@browsing-live-documentation section)
  (@markdown-support section)
  (@codification section)
  (@linking-to-code section)
  (@linking-to-the-hyperspec section)
  (@linking-to-sections section)
  (@miscellaneous-documentation-printer-variables section)
  (@documentation-utilities section)
  (@overview-of-escaping section)
  (@output-details section)
  (@document-implementation-notes section))

;;; This is the core of the DOCUMENT function, presented here for an
;;; overview.
;;;
;;; Markdown is generated in 2.5 passes. In the 1st pass, the output
;;; is discarded, but we
;;;
;;; - COLLECT-HEADINGs,
;;; - populate PAGE-DEFINITIONS,
;;; - set PAGE-WRITTEN-Ps.
;;;
;;; In the 2nd pass, output goes to the real output stream(s) (see
;;; PAGE-TEMP-STREAM-SPEC), and with the information gathered in the
;;; first pass, we may
;;;
;;; - PRINT-TOPLEVEL-SECTION-LIST for each PAGE-WRITTEN-P,
;;; - PRINT-TABLE-OF-CONTENTS for top-level sections,
;;; - add prev/next/up links to sections (FANCY-NAVIGATION),
;;; - know what definitions are being documented and thus can be linked to.
;;;
;;; In the 2.5th pass, we add markdown reference link definitions,
;;; headers, footers, and convert markdown to other formats if
;;; necessary.
(defmacro %document (documentable stream page-specs)
  (once-only (documentable stream page-specs)
    `(with-documentable-bindings (documentable)
       (with-link-maps ()
         (let ((*pages* (page-specs-to-pages ,documentable ,stream
                                             ,page-specs)))
           (with-headings ()
             ;; 1st pass
             (let ((*first-pass* t)
                   (*page* (last-elt *pages*)))
               (document-documentable ,documentable (make-broadcast-stream)))
             (finalize-headings)
             (finalize-pages *pages*)
             ;; 2nd pass
             (let ((*first-pass* nil))
               (print-toplevel-section-lists *pages*)
               ;; Initially, send output to the default page (built for
               ;; STREAM). Note that on PAGE-BOUNDARIES, DOCUMENT-OBJECT
               ;; (method () (dref t)) redirects the output.
               (with-temp-output-to-page (,stream (last-elt *pages*))
                 (document-documentable ,documentable ,stream))
               ;; 2.5th pass
               (mapcar #'finalize-page-output *pages*))))))))

(defvar *first-pass*)


;;;; Numbering and collecting headings

(defmacro with-headings (() &body body)
  `(let ((*headings* ())
         (*heading-number* ())
         (*heading-level* 0))
     ,@body))

;;; The section number of the most recent WITH-HEADING. It is a
;;; mutable list of integers, whose length is the nesting depth. The
;;; number of top-level headings is '(), but they append a 0 to the
;;; end of *HEADING-NUMBER* for the processing of headings nested in
;;; them. When such a nested WITH-HEADING is encountered, the last
;;; number is INCFed, and that will be its heading numbers (i.e. (1)
;;; here). Then another 0 is added to the end of *HEADING-NUMBER* for
;;; the BODY of WITH-HEADING, and processing goes on.
(defvar *heading-number* ())

;;; (LENGTH *HEADING-NUMBER*)
(defvar *heading-level* 0)

;;; This is a list of HEADING objects in the order of generation
;;; during the second pass. It's in reverse order while being
;;; accumulated in the first pass.
(defvar *headings* ())

;;; Called at the end of the first pass. Reverse the order of
;;; *HEADINGS*.
(defun finalize-headings ()
  (setq *headings* (reverse *headings*)))

(defstruct heading
  object
  title
  number
  level)

(defun collect-heading (object title)
  (push (make-heading :object object :title title
                      :number (copy-list *heading-number*)
                      :level *heading-level*)
        *headings*))

;;; This is the implementation of the WITH-HEADING macro.
(defun/autoloaded call-with-heading (stream object title link-title-to fn)
  (let ((level *heading-level*)
        (title (process-title title)))
    (when (plusp level)
      (incf (nth (1- level) *heading-number*)))
    (when *first-pass*
      (collect-heading object title))
    (unless *first-pass*
      (print-section-title stream object title link-title-to)
      (print-table-of-contents object stream))
    (let ((*heading-number*
            (append *heading-number*
                    (loop repeat (max 0 (- (1+ level)
                                           (length *heading-number*)))
                          collect 0)))
          (*heading-level* (1+ *heading-level*)))
      (funcall fn stream))))

(defun process-title (string)
  (with-output-to-string (out)
    (print-markdown (codify (parse-markdown string)) out)))

;;; Add this many #\# to markdown section headings in the output. This
;;; is for when a section that is a subsection of another is
;;; documented on its own page by DOCUMENT/OPEN.
(defvar *heading-offset* 0)

(defmacro with-heading-offset ((object) &body body)
  `(let ((*heading-offset* (heading-offset ,object)))
     ,@body))

;;; Determine what SECTION's *HEADING-LEVEL* would be under its root
;;; ancestor.
(defun heading-offset (object)
  (multiple-value-bind (foundp depth) (and (not (stringp object))
                                           (find-root-section object))
    (if foundp
        depth
        ;; OBJECT is not a SECTION (or a reference to one), neither is
        ;; it contained in a SECTION. Start from H2. This only affects
        ;; ASDF:SYSTEMs in stock PAX.
        1)))


;;; A PAGE is basically a single markdown or html file to where the
;;; documentation of some definitions is written. Constructed by
;;; PAGE-SPECS-TO-PAGES.
(defstruct page
  ;; DREFs for the :OBJECTS of the page-spec. In the second pass,
  ;; output is redirected to this page, when encountering one of these
  ;; definitions.
  boundaries
  ;; The second pass writes the markdown output to this stream. It's
  ;; actually STREAM-SPEC (see WITH-OPEN-STREAM-SPEC) to allow the
  ;; temporary stream to
  ;;
  ;; - be created lazily so that no stray files are left around and
  ;;   only a small number of fds are needed even for a huge project;
  ;;
  ;; - be opened multiple times (which is not a given for string
  ;;   streams).
  temp-stream-spec
  ;; FINALIZE-PAGE-OUTPUT may convert the markdown in TEMP-STREAM-SPEC
  ;; to a non-markdown format or do final touchups.
  final-stream-spec
  ;; URI-FRAGMENT is a string such as "doc/manual.html" that specifies
  ;; where the page will be deployed on a webserver. It defines how
  ;; links between pages will look. If it's not specified, and OUTPUT
  ;; refers to a file, then it defaults to the name of the file. If
  ;; URI-FRAGMENT is NIL, then no links will be made to or from that
  ;; page.
  uri-fragment
  ;; See PAGE-SPECS-TO-PAGES for HEADER-FN, FOOTER-FN and
  ;; SOURCE-URI-FN.
  header-fn
  footer-fn
  source-uri-fn
  ;; The DREFs written to this page. Set in the first pass.
  definitions
  ;; Any output written to this page (including plain docstrings)? Set
  ;; in the first pass.
  written-p
  ;; LINKS made from this page. For LINK-TO-DEFINITION and
  ;; WRITE-MARKDOWN-REFERENCE-STYLE-LINK-DEFINITIONS.
  (used-links (make-hash-table :test #'eq) :type hash-table))

;;; All the PAGEs in a DOCUMENT call.
(defvar *pages*)

;;; Return the first page whose PAGE-BOUNDARIES have DREF.
(defun boundary-page (dref)
  (dolist (page *pages*)
    (when (find dref (page-boundaries page) :test #'xref=)
      (return page))))

;;; The current page where output is being sent.
(defvar *page* nil)

(defvar *page-stream*)

(defmacro with-temp-output-to-page ((stream page) &body body)
  (once-only (page)
    (with-gensyms (stream-spec)
      `(flet ((foo (,stream)
                ,@body))
         (if (or (null ,page) (eq ,page *page*))
             (foo *page-stream*)
             (let ((,stream-spec (page-temp-stream-spec ,page)))
               (with-open-stream-spec (,stream ,stream-spec
                                       :direction :output)
                 (let ((*page* ,page)
                       (*page-stream* ,stream))
                   (foo ,stream)))))))))

(defmacro with-temp-input-from-page ((stream page) &body body)
  `(with-open-stream-spec (,stream (page-temp-stream-spec ,page))
     ,@body))

(defmacro with-final-output-to-page ((stream page) &body body)
  `(with-open-stream-spec (,stream (page-final-stream-spec ,page)
                           :direction :output)
     ;; This allows HEADER-FN and FOOTER-FN to support linking
     ;; references with LINK-TO-URI.
     (let ((*page* ,page))
       ,@body)))


;;; A LINK (a possible link target, really) is definition that resides
;;; on PAGE. Note that the same definition may be written to multiple
;;; pages.
(defstruct link
  (definition nil :type dref)
  ;; - STRING pages denote URLs. This is used to link to the hyperspec
  ;;   and to external GLOSSARY-TERMs.
  ;;
  ;; - LINK pages are aliases: the LINK with the reference (PRINT
  ;;   (CLHS FUNCTION)) is the LINK-PAGE of a LINK whose reference is
  ;;   (PRINT FUNCTION).
  ;;
  ;; - NULL pages are to support "pax:" URLs for
  ;;   *DOCUMENT-OPEN-LINKING*.
  (page nil :type (or page string link null)))

;;; Like MAKE-LINK, but override PAGE with GLOSSARY-TERM-URL if
;;; DEFINITION is a GLOSSARY-TERM with an URL.
(defun make-link* (definition page)
  (or (if (eq (dref-locative-type definition) 'glossary-term)
          (when-let (glossary-term (resolve definition nil))
            (when-let (url (glossary-term-url glossary-term))
              (assert (stringp url))
              (make-link :definition definition :page url))))
      (make-link :definition definition :page page)))

;;; A DREF::@NAME to LINKs map (name-to-links map, nlmap). Caches
;;; LINKS-OF within a single DOCUMENT call. If not
;;; *DOCUMENT-OPEN-LINKING*, then only stuff on on *CLOSED-NLMAP* may
;;; be present.
(defvar *open-nlmap*)

;;; After the first pass, this name-to-links map is populated the all
;;; definitions that are being documented. Not used when
;;; *DOCUMENT-OPEN-LINKING*.
(defvar *closed-nlmap*)

;;; Whether definitions present in the running Lisp but not in
;;; *CLOSED-NLMAP* can be linked with magic "pax:" URLs. This is to
;;; support @BROWSING-LIVE-DOCUMENTATION.
(defvar *document-open-linking* nil)

;;; Return all possible LINKs for NAME in the order of linking
;;; preference when there are duplicate LINK-REFERENCEs (e.g CLHS
;;; aliases).
(defgeneric links-of (name)
  (:method (name)
    (let* ((live-definitions (definitions name))
           ;; Gather all different DREF::@NAMEs. For example, if NAME
           ;; is MGL-PAX, then this is '("mgl-pax" "MGL-PAX"), where
           ;; one is for the ASDF:SYSTEM, the other is for the
           ;; PACKAGE.
           (names (remove-duplicates (cons name
                                           (mapcar #'xref-name
                                                   live-definitions))
                                     :test #'equal))
           (closed-links (loop for name in names
                               append (closed-links name)))
           (external-links (loop for name in names
                                 append (external-links name)))
           (preferred-links (if *document-open-linking*
                                closed-links
                                (append closed-links external-links)))
           ;; For all LIVE-DEFINITIONS, use one of PREFFERED-LINKS or
           ;; maybe create a new open link.
           (preferred-live-links
             ;; The order of these does not matter.
             (loop for definition in live-definitions
                   for link = (or (find-among-links definition preferred-links)
                                  (and *document-open-linking*
                                       (make-link* definition nil)))
                   when link
                     collect link)))
      (append preferred-live-links
              ;; The remaining static links (with references for which
              ;; there is no definition in the running Lisp) must
              ;; follow the live ones, so that FIND-LINK will find the
              ;; live ones first. See
              ;; MGL-PAX-TEST::TEST-DOCUMENT/OPEN/LIVE-VS-STATIC.
              (set-difference (append closed-links external-links)
                              preferred-live-links
                              :key #'link-definition :test #'xref=))))
  (:method ((xref xref))
    (let* ((name (xref-name xref))
           (links (links-of name)))
      (loop for link in links
            when (xref= xref (link-definition link))
              collect link))))

(defun closed-links (name)
  (when (boundp '*closed-nlmap*)
    (gethash name *closed-nlmap*)))

(defun find-among-links (reference links)
  (find reference links :key #'link-definition :test #'xref=))

(defun ensure-in-open-nlmap (name)
  (declare (optimize speed))
  (if *first-pass*
      (closed-links name)
      ;; This could be CLRHASHed if it gets too large.
      (let ((open-nlmap *open-nlmap*))
        (multiple-value-bind (links found) (gethash name open-nlmap)
          (if found
              links
              (setf (gethash name open-nlmap) (links-of name)))))))

;;; Iterate over LINKs with DREFs whose XREF-NAME is EQUAL to NAME.
(defmacro do-links ((link name) &body body)
  (once-only (name)
    `(dolist (,link (ensure-in-open-nlmap ,name))
       ,@body)))

(defun add-link (nlmap link &optional name)
  (let* ((dref (link-definition link))
         (name (or name (dref-name dref))))
    (push link (gethash name nlmap))))

;;; A list of references with special rules for linking (see
;;; @LOCAL-REFERENCES). The reference being documented is always on
;;; this list (see DOCUMENTING-REFERENCE). Arguments of functions and
;;; similar typically also are. Bound by WITH-LOCAL-REFERENCES.
(defvar *local-references*)

(defmacro with-link-maps (() &body body)
  `(let ((*open-nlmap* (make-hash-table :test #'equal))
         (*closed-nlmap* (make-hash-table :test #'equal))
         (*local-references* ())
         (*link-to-id* (make-hash-table :test #'eq))
         (*id-to-link* (make-hash-table :test #'equal)))
     (locally ,@body)))

;;; Called at the end of the first pass. Reverse PAGE-DEFINITIONS so
;;; that it's in depth-first order, and add a LINK to *CLOSED-NLMAP*
;;; for all PAGE-DEFINITIONS of PAGES.
(defun finalize-pages (pages)
  (dolist (page pages)
    (setf (page-definitions page) (reverse (page-definitions page)))
    (dolist (dref (page-definitions page))
      (let ((name (dref-name dref)))
        (unless (find dref (gethash name *closed-nlmap*)
                      :key #'link-definition :test #'xref=)
          (add-link *closed-nlmap* (make-link* dref page)))))))


;;;; Querying global and local definitions

(defun find-link (dref)
  (declare (type dref dref))
  (let ((name (dref-name dref))
        (locative (dref-locative dref)))
    (do-links (link name)
      (when (equal locative (dref-locative (link-definition link)))
        (return link)))))

;;; Return a list of all DREFs whose XREF-NAME matches NAME, that is,
;;; with NAME as their XREF-NAME they would resolve to the same thing.
;;;
;;; If LOCAL is NIL, only global definitions are considered for
;;; matching. If LOCAL is :EXCLUDE, then only those global references
;;; which are not local references are considered. If LOCAL is
;;; :INCLUDE, then both global and local references are considered.
(defun definitions-with-name (name &key local)
  (let ((global-refs (global-definitions-with-name name)))
    (if local
        (let ((local-refs (local-definitions-with-name name)))
          (if (eq local :include)
              (nconc global-refs local-refs)
              (set-difference global-refs local-refs :test #'xref=)))
        global-refs)))

(defun global-definitions-with-name (name)
  (let ((result ()))
    (do-links (link name)
      (let ((dref (link-definition link)))
        (push dref result)))
    result))

(defun has-global-definition-p (name)
  (do-links (link name)
    (return link)))

(defun global-definition-p (reference)
  (let ((name (xref-name reference)))
    (do-links (link name)
      (return link))))

(declaim (inline xref-name=))
(defun xref-name= (name ref)
  (equal name (xref-name ref)))

(defun local-definitions-with-name (name)
  (remove-if-not (lambda (ref)
                   (xref-name= name ref))
                 *local-references*))

(defun has-local-reference-p (name)
  (find name *local-references* :test #'xref-name=))

(defun has-reference-p (name)
  (or (has-global-definition-p name)
      (has-local-reference-p name)))


;;; Follow LINK-PAGE if it is a LINK, and return the LINK it
;;; eventually points to.
(defun unaliased-link (link)
  (declare (type link link))
  (if (link-p (link-page link))
      (unaliased-link (link-page link))
      link))

;;; For the LINK to DREF, increment the link counter for the current
;;; page and return the link id.
(defun link-to-definition (dref)
  (declare (type dref dref))
  (let ((link (unaliased-link (find-link dref))))
    (assert link)
    (when (let ((page (link-page link)))
            (or (null page)
                (eq *page* page)
                (stringp page)
                (and (page-uri-fragment *page*)
                     (page-uri-fragment page))))
      (setf (gethash link (page-used-links *page*)) t)
      (ensure-link-id link))))

;;; Link ids are short hashes (as STRINGs), and they go into markdown
;;; reference links. Due to possible collisions, they are
;;; context-dependent, so to keep LINKs immutable, ids are in this
;;; hash table.
(defvar *link-to-id*)
;;; A LINK-ID to LINK hash table for MD5 collision detection.
(defvar *id-to-link*)

(defun link-id (link)
  (gethash link *link-to-id*))

(defun ensure-link-id (link)
  (or (gethash link *link-to-id*)
      (let ((id (hash-link (dref-to-anchor (link-definition link))
                           #'find-link-by-id)))
        (setf (gethash id *id-to-link*) link)
        (setf (gethash link *link-to-id*) id))))

(defun find-link-by-id (id)
  (gethash id *id-to-link*))

(defun definition-page (dref)
  (let ((link (find-link dref)))
    (when link
      (link-page (unaliased-link link)))))


(defsection @linking-to-the-hyperspec (:title "Linking to the Hyperspec")
  (*document-link-to-hyperspec* variable)
  (*document-hyperspec-root* variable))

(defvar *document-link-to-hyperspec* t
  "If true, link symbols found in code to the Common Lisp Hyperspec
  unless there is a definition in the running Lisp that is being
  DOCUMENTed.

  Locatives work as expected (see *DOCUMENT-LINK-CODE*): `\\FIND-IF`
  links to FIND-IF, `\\FUNCTION` links to FUNCTION, and
  `[FUNCTION][type]` links to [FUNCTION][type].

  [Autolinking][ @explicit-and-autolinking section] to T and NIL is
  [suppressed][ @suppressed-links]. If desired, use `[T][]` (that
  links to [T][]) or `[T][constant]` (that links to [T][constant]).

  Note that linking explicitly with the CLHS locative is not subject
  to the value of this variable.")

(defvar *document-hyperspec-root*
  "http://www.lispworks.com/documentation/HyperSpec/"
  """A \URL of the Common Lisp Hyperspec. The default value
  is the canonical location. When [invoked from Emacs][
  @browsing-live-documentation], the Elisp variable
  `common-lisp-hyperspec-root` is in effect.""")

;;; Assumes that REFERENCE is canonical.
(defun find-clhs-url (reference)
  (when (eq (xref-locative-type reference) 'clhs)
    (let* ((name (xref-name reference))
           (locative (xref-locative-args reference))
           (locative-type (locative-type locative)))
      ;; This parallels LOCATE* (METHOD () (T (EQL CLHS) T)).
      (cond ((eq locative-type 'glossary-term)
             (find-hyperspec-glossary-entry-url name
                                                *document-hyperspec-root*))
            ((eq locative-type 'section)
             (or (find-hyperspec-issue-url name *document-hyperspec-root*)
                 (find-hyperspec-section-url name
                                             *document-hyperspec-root*)))
            (t
             (find-hyperspec-definition-url name locative
                                            *document-hyperspec-root*))))))

(defun clhs-definitions (name)
  (mapcar (lambda (locative)
            ;; We could use LOCATE, but MAKE-INSTANCE is more direct.
            (make-instance 'clhs-dref
                           :name name
                           :locative (if locative
                                         `(clhs ,locative)
                                         ;; The Hyperspec disambiguation page
                                         'clhs)))
          (hyperspec-locatives-for-name name)))

(defun clhs-documentations (name)
  (let ((*clhs-substring-match* nil))
    (ensure-list (or (dref name '(clhs glossary-term) nil)
                     (dref name '(clhs section) nil)))))

(defun make-clhs-alias-link (link)
  (let* ((dref (link-definition link))
         (name (dref-name dref))
         (locative (dref-locative dref))
         (locative-args (locative-args locative)))
    (assert (eq (locative-type locative) 'clhs))
    (when locative-args
      (make-link :definition (or (dref name locative-args nil)
                                 ;; There is no live definition. Fake a
                                 ;; DREF because pretty much everything
                                 ;; else is a DREF.
                                 (make-instance 'dref
                                                :name name
                                                :locative locative-args))
                 :page link))))

(defun clhs-definition-links (name)
  (when *document-link-to-hyperspec*
    (loop for dref in (clhs-definitions name)
          append (let ((clhs-link (make-link
                                   :definition dref
                                   :page (find-clhs-url dref))))
                   (let ((alias (make-clhs-alias-link clhs-link)))
                     (if alias
                         (list alias clhs-link)
                         (list clhs-link)))))))

(defun clhs-documentation-links (name)
  (loop for dref in (clhs-documentations name)
        collect (make-link :definition dref
                           :page (find-clhs-url dref))))

(defun clhs-links (name)
  (append (clhs-definition-links name)
          (clhs-documentation-links name)))

(defun filter-clhs-references (refs)
  (remove 'clhs refs :key #'xref-locative-type))


;;;; Beginnings of abstracting CLHS to external references. This could
;;;; serve the needs of e.g. linking to the MOP.

(defun external-locative-p (locative)
  (eq (locative-type locative) 'clhs))

(defun external-reference-p (reference)
  (external-locative-p (xref-locative reference)))

(defun external-reference-url (reference)
  (find-clhs-url (locate reference)))

(defun external-links (name)
  (append (clhs-links name)
          (glossary-term-external-links name)))

(defun glossary-term-external-links (name)
  (when-let (dref (dref name 'glossary-term nil))
    (when-let (url (glossary-term-url (resolve dref)))
      (list (make-link :definition dref :page url)))))

(defun filter-external-references (references)
  (filter-clhs-references references))

(defun find-external-link (xref)
  (let ((*document-link-to-hyperspec* t))
    (dolist (link (external-links (xref-name xref)))
      (when (xref= (link-definition link) xref)
        (return link)))))


(defsection @documentables (:title "Documentables")
 "- The DOCUMENTABLE argument may be a [DREF][class] or anything else
    that is LOCATEable. This includes non-DREF [XREF][class]s and
    first-class objects such as [FUNCTION][class]s.

  - If DOCUMENTABLE is a string, then it is processed like a docstring
    in DEFSECTION. That is, with [docstring sanitization]
    [@markdown-in-docstrings], @CODIFICATION, and linking (see
    @LINKING-TO-CODE, @LINKING-TO-THE-HYPERSPEC).

  - Finally, DOCUMENTABLE may be a nested list of LOCATEable objects
    and docstrings. The structure of the list is unimportant. The
    objects in it are documented in depth-first order.")

(defvar *document-tight* nil)

(defvar *objects-being-documented* ())

;;; Stuff a description of *OBJECTS-BEING-DOCUMENTED* into known
;;; conditions.
(defmacro with-document-context (&body body)
  `(handler-bind ((locate-error
                    (lambda (e)
                      ;; On CMUCL, SLOT-VALUE doesn't seem to work on
                      ;; conditions.
                      #-cmucl
                      (setf (slot-value e 'dref::message)
                            (if (dref::locate-error-message e)
                                (destructuring-bind (format-control
                                                     &rest format-args)
                                    (dref::locate-error-message e)
                                  (cons (concatenate
                                         'string
                                         format-control
                                         (escape-format-control
                                          (print-document-context nil)))
                                        format-args))
                                (list (escape-format-control
                                       (print-document-context nil)))))))
                  (transcription-error
                    (lambda (e)
                      #-cmucl
                      (setf (slot-value e 'message)
                            (concatenate 'string (transcription-error-message e)
                                         (escape-format-control
                                          (print-document-context nil))))))
                  (simple-error
                    (lambda (e)
                      (apply #'error
                             (concatenate 'string
                                          (simple-condition-format-control e)
                                          (escape-format-control
                                           (print-document-context nil)))
                             (simple-condition-format-arguments e))))
                  (simple-warning
                    (lambda (w)
                      (apply #'warn
                             (concatenate 'string
                                          (simple-condition-format-control w)
                                          (escape-format-control
                                           (print-document-context nil)))
                             (simple-condition-format-arguments w))
                      (muffle-warning w))))
     ,@body))

(defun print-document-context (stream)
  (let ((*package* (find-package :keyword))
        (context (loop for object in *objects-being-documented*
                       when (typep object 'dref)
                         collect (list (dref-name object)
                                       (dref-locative object)))))
    (if context
        (format stream "~%  [While documenting ~{~S~^~%   in ~}]~%" context)
        (format stream ""))))

(defun escape-format-control (string)
  (with-output-to-string (out)
    (loop for char across string
          do (when (char= char #\~)
               (write-char #\~ out))
             (write-char char out))))

;;; Basically, call DOCUMENT-OBJECT on every element of DOCUMENTABLE
;;; (see MAP-DOCUMENTABLE) and add extra newlines between them
;;; according to *DOCUMENT-TIGHT*.
(defun document-documentable (documentable stream)
  ;; The newline logic assumes that everything goes to the same page,
  ;; which is currently fine because *DOCUMENT-TIGHT* is not public,
  ;; and it's used only for @BROWSING-LIVE-DOCUMENTATION, which works
  ;; with single pages.
  (with-document-context
    (let ((firstp t)
          (add-blank-p nil))
      (map-documentable
       (lambda (object1)
         (when (or add-blank-p
                   (and (not firstp)
                        (not *document-tight*)))
           (terpri stream))
         (setq firstp nil)
         (with-heading-offset (object1)
           (document-object object1 stream))
         (setq add-blank-p (not *document-tight*)))
       documentable))))

(defmacro with-documentable-bindings ((documentable) &body body)
  (assert (symbolp documentable))
  `(call-with-documentable-bindings ,documentable
                                    (lambda (,documentable)
                                      (declare (ignorable ,documentable))
                                      ,@body)))

;;; Call FN with each thing within DOCUMENTABLE (an argument of the
;;; same name of DOCUMENT). Handle special PROGV forms, which allow
;;; controlling the dynamic environment around DOCUMENT-OBJECT calls.
;;; This is only used by PAX-APROPOS* and is not part of DOCUMENT's
;;; contract.
(defun map-documentable (fn documentable)
  (if (not (listp documentable))
      (funcall fn documentable)
      (with-documentable-bindings (documentable)
        (dolist (element documentable)
          (if (atom element)
              (funcall fn element)
              (map-documentable fn element))))))

;;; If DOCUMENTABLE is a list with a PROGV form as its first element:
;;;
;;;   ((PROGV <symbols-form> <values-form>)
;;;    <reference> <string> ...)
;;;
;;; then establish dynamic variable bindings with PROGV, EVALuating
;;; SYMBOLS-FORM and VALUES-FORM, and call FN with the REST of the
;;; list.
;;;
;;; Else, just call FN with DOCUMENTABLE.
(defun call-with-documentable-bindings (documentable fn)
  (if (and (listp documentable)
           (listp (first documentable))
           (eq (caar documentable) 'progv))
      (destructuring-bind (symbols-form values-form) (rest (first documentable))
        (progv (eval symbols-form) (eval values-form)
          (funcall fn (rest documentable))))
      (funcall fn documentable)))



(defsection @document-function (:title "The DOCUMENT Function")
  (document function)
  (@documentables section)
  (@document-return section)
  (@pages section)
  (@package-and-readtable section))

(defmacro with-format ((format) &body body)
  (with-gensyms (fn)
    `(flet ((,fn ()
              ,@body))
       (call-with-format ,format #',fn))))

(defvar *html-subformat* nil)

(defun/autoloaded document (documentable &key (stream t) pages (format :plain))
  """Write DOCUMENTABLE in FORMAT to STREAM diverting some output to PAGES.
  FORMAT is a [3BMD][3bmd] output format (currently one of :MARKDOWN,
  :HTML and :PLAIN). STREAM may be a [STREAM][type] object, T or NIL
  as with [CL:FORMAT][].

  To look up the documentation of the DOCUMENT function itself:

      (document #'document)

  The same with fancy markup:

      (document #'document :format :markdown)

  To document a SECTION:

      (document @pax-manual)

  To generate the documentation for separate libraries with automatic
  cross-links:

      (document (list pax::@pax-manual dref::@dref-manual) :format :markdown)

  See @DOCUMENTATION-UTILITIES for more.

  Definitions that do not define a first-class object are supported
  via [DRef][dref::@dref-manual]:

      (document (dref:locate 'foo 'type))

  There are quite a few special variables that affect how output is
  generated, see @CODIFICATION, @LINKING-TO-CODE,
  @LINKING-TO-SECTIONS, and
  @MISCELLANEOUS-DOCUMENTATION-PRINTER-VARIABLES.

  For the details, see the following sections, starting with
  @DOCUMENTABLES. Also see @EXTENSION-API and DOCUMENT-OBJECT*."""
  ;; Autoloading mgl-pax/transcribe on demand would be enough for most
  ;; situations, but when documenting PAX itself, it would cause the
  ;; documentables to change from the 1st pass to the 2nd.
  (ensure-transcribe-loaded)
  (with-sections-cache ()
    (with-format (format)
      (let* ((*print-right-margin* (or *print-right-margin* 80))
             (3bmd-code-blocks:*code-blocks* t)
             (3bmd-code-blocks:*code-blocks-default-colorize*
               (and (not (eq *html-subformat* :w3m))
                    :common-lisp))
             (3bmd-code-blocks::*colorize-name-map*
               (if (eq *html-subformat* :w3m)
                   (make-hash-table)
                   (plist-hash-table
                    `("cl-transcript" :common-lisp
                      ,@(hash-table-plist
                         3bmd-code-blocks::*colorize-name-map*))
                    :test #'equal))))
        (document-return stream (%document documentable stream pages))))))

(defun call-with-format (format fn)
  (if (eq format :plain)
      ;; 3BMD's :PLAIN is very broken. Take matters into our hands,
      ;; and make :PLAIN equivalent to :MARKDOWN without all the bells
      ;; and whistles.
      (let ((*format* :markdown)
            (*document-uppercase-is-code* nil)
            (*document-link-code* nil)
            (*document-link-sections* nil)
            (*document-mark-up-signatures* nil)
            (*document-max-numbering-level* 0)
            (*document-max-table-of-contents-level* 0)
            (*document-text-navigation* nil))
        (handler-bind ((unresolvable-reflink #'output-label))
          (funcall fn)))
      (let ((*format* format))
        (funcall fn))))


(defsection @pages (:title "Pages")
  """The PAGES argument of DOCUMENT is to create multi-page documents
  by routing some of the generated output to files, strings or
  streams. PAGES is a list of page specification elements. A page spec
  is a [property list][clhs] with keys :OBJECTS, :OUTPUT,
  :URI-FRAGMENT, :SOURCE-URI-FN, :HEADER-FN and :FOOTER-FN. OBJECTS is
  a list of objects (references are allowed but not required) whose
  documentation is to be sent to :OUTPUT.

  Documentation is initially sent to a default stream (the STREAM
  argument of DOCUMENT), but output is redirected if the thing being
  currently documented is the :OBJECT of a PAGE-SPEC.

  :OUTPUT can be a number things:

  - If it's NIL, then output will be collected in a string.

  - If it's T, then output will be sent to *STANDARD-OUTPUT*.

  - If it's a stream, then output will be sent to that stream.

  - If it's a list whose first element is a string or a pathname, then
    output will be sent to the file denoted by that and the rest of
    the elements of the list are passed on to CL:OPEN. One extra
    keyword argument is :ENSURE-DIRECTORIES-EXIST. If it's true,
    ENSURE-DIRECTORIES-EXIST will be called on the pathname before
    it's opened.

  Note that even if PAGES is specified, STREAM acts as a catch all,
  absorbing the generated documentation for references not claimed by
  any pages.

  :HEADER-FN, if not NIL, is a function of a single stream argument,
  which is called just before the first write to the page. Since
  :FORMAT :HTML only generates HTML fragments, this makes it possible
  to print arbitrary headers, typically setting the title, CSS
  stylesheet, or charset.

  :FOOTER-FN is similar to :HEADER-FN, but it's called after the last
  write to the page. For HTML, it typically just closes the body.

  :URI-FRAGMENT is a string such as `"doc/manual.html"` that specifies
  where the page will be deployed on a webserver. It defines how links
  between pages will look. If it's not specified and :OUTPUT refers to
  a file, then it defaults to the name of the file. If :URI-FRAGMENT
  is NIL, then no links will be made to or from that page.

  Finally, :SOURCE-URI-FN is a function of a single, REFERENCE
  argument. If it returns a value other than NIL, then it must be a
  string representing an \URI. If FORMAT is :HTML and
  *DOCUMENT-MARK-UP-SIGNATURES* is true, then the locative as
  displayed in the signature will be a link to this URI. See
  MAKE-GIT-SOURCE-URI-FN.

  PAGES may look something like this:

  ```
  `((;; The section about SECTIONs and everything below it ...
     :objects (, @sections)
     ;; ... is so boring that it's not worth the disk space, so
     ;; send it to a string.
     :output (nil)
     ;; Explicitly tell other pages not to link to these guys.
     :uri-fragment nil)
    ;; Send the @EXTENSION-API section and everything reachable
    ;; from it ...
    (:objects (, @extension-api)
     ;; ... to build/tmp/pax-extension-api.html.
     :output "build/tmp/pax-extension-api.html"
     ;; However, on the web server html files will be at this
     ;; location relative to some common root, so override the
     ;; default:
     :uri-fragment "doc/dev/pax-extension-api.html"
     ;; Set html page title, stylesheet, charset.
     :header-fn 'write-html-header
     ;; Just close the body.
     :footer-fn 'write-html-footer)
    ;; Catch references that were not reachable from the above. It
    ;; is important for this page spec to be last.
    (:objects (, @pax-manual)
     :output "build/tmp/manual.html"
     ;; Links from the extension api page to the manual page will
     ;; be to ../user/pax-manual#<anchor>, while links going to
     ;; the opposite direction will be to
     ;; ../dev/pax-extension-api.html#<anchor>.
     :uri-fragment "doc/user/pax-manual.html"
     :header-fn 'write-html-header
     :footer-fn 'write-html-footer))
  ```""")

;;; Convert the PAGES argument of DOCUMENT to PAGE objects.
(defun page-specs-to-pages (documentable stream page-specs)
  (mapcar #'page-spec-to-page
          (ensure-default-page-spec page-specs documentable stream)))

(defun ensure-default-page-spec (page-specs documentable stream)
  (let ((default (find :default page-specs :key #'page-spec-objects)))
    (cond (default
           (let ((others (remove default page-specs))
                 (default (copy-list default)))
             (setf (getf default :objects) documentable)
             (setf (getf default :output) (list stream))
             (append others `(,default))))
          (t
           (append page-specs `((:objects ,documentable
                                 :output (,stream))))))))

(defun page-spec-objects (page-spec)
  (getf page-spec :objects))

(defun page-spec-to-page (page)
  (destructuring-bind (&key objects output header-fn footer-fn
                         (uri-fragment nil uri-fragment-p)
                         source-uri-fn)
      page
    (let ((stream-spec (make-stream-spec-from-page-spec-output output)))
      (make-page
       :boundaries (page-spec-objects-to-definitions objects)
       ;; See FINALIZE-PAGE-OUTPUT.
       :temp-stream-spec (if (and (eq *format* :markdown)
                                  (null header-fn)
                                  (null footer-fn))
                             stream-spec
                             (make-instance 'string-stream-spec))
       :final-stream-spec stream-spec
       :uri-fragment (or uri-fragment
                         (if (and (not uri-fragment-p)
                                  (typep stream-spec 'file-stream-spec))
                             (file-stream-spec-pathname stream-spec)
                             nil))
       :header-fn header-fn
       :footer-fn footer-fn
       :source-uri-fn (if (and (listp source-uri-fn)
                               (eq (first source-uri-fn) :maker))
                          (funcall (second source-uri-fn))
                          source-uri-fn)))))

(defun make-stream-spec-from-page-spec-output (output)
  (apply #'make-stream-spec (if (and output (listp output))
                                output
                                (list output))))

(defun page-spec-objects-to-definitions (objects)
  (loop for object in (ensure-list objects)
        for dref = (and (not (stringp object))
                        (locate object nil))
        when dref
          collect dref))


;;;; DOCUMENT-OBJECT

(defvar *document-do-not-follow-references* nil)

(defgeneric document-object (object stream)
  (:method :around (object stream)
    (declare (ignorable stream))
    (let ((*objects-being-documented* (cons object *objects-being-documented*)))
      (call-next-method)))
  (:method (object stream)
    (document-object (locate object) stream))
  (:method ((string string) stream)
    (cond (*first-pass*
           (setf (page-written-p *page*) t))
          (t
           (document-docstring string stream :indentation "" :paragraphp nil)
           (terpri stream))))
  (:method :around ((xref xref) stream)
    (let ((*documenting-reference* xref))
      (if (or (eq *document-do-not-follow-references* t)
              (member (xref-locative-type xref)
                      *document-do-not-follow-references*))
          (documenting-reference (stream
                                  :reference xref
                                  :arglist (xref-locative-args xref)))
          (call-next-method))))
  ;; LOCATE non-DREF XREFs.
  (:method ((xref xref) stream)
    (let ((warn-if-undefined
            (and *document-open-linking*
                 ;; It is an error for explicit arguments to DOCUMENT
                 ;; to have no definition even when open-linking (so
                 ;; that `mgl-pax-document' produces errors when it
                 ;; must), but we want to document what we can even if
                 ;; a section contains undefined stuff.
                 (boundp '*section*))))
      (if warn-if-undefined
          (multiple-value-bind (dref error)
              (locate xref (not warn-if-undefined))
            (if dref
                (document-object dref stream)
                (when *first-pass*
                  (warn "Not documenting ~S: ~A" xref error))))
          (document-object (locate xref) stream))))
  (:method ((dref dref) stream)
    (handler-bind
        ((warning (lambda (warning)
                    (when (sanitize-aggressively-p)
                      (muffle-warning warning)))))
      (let ((page (boundary-page dref)))
        (if *first-pass*
            (let ((*page* (or page *page*)))
              (push dref (page-definitions *page*))
              (setf (page-written-p *page*) t)
              (document-object* (or (resolve dref nil) dref) stream))
            (with-temp-output-to-page (stream page)
              (document-object* (or (resolve dref nil) dref) stream)))))))


(defun finalize-page-output (page)
  (when (page-written-p page)
    ;; Now that markdown output for this PAGE is complete, we may want
    ;; to convert it to the requested *FORMAT*.
    (if (and (eq *format* :markdown)
             (null (page-header-fn page))
             (null (page-footer-fn page)))
        (with-temp-output-to-page (stream page)
          (write-markdown-reference-style-link-definitions stream))
        (let ((markdown-string
                (if (typep (page-temp-stream-spec page) 'string-stream-spec)
                    (string-stream-spec-string (page-temp-stream-spec page))
                    (with-temp-input-from-page (stream page)
                      (read-stream-content-into-string stream))))
              (markdown-reflinks
                (with-output-to-string (stream)
                  (let ((*page* page))
                    (write-markdown-reference-style-link-definitions stream)))))
          (delete-stream-spec (page-temp-stream-spec page))
          (with-final-output-to-page (stream page)
            (when (page-header-fn page)
              (funcall (page-header-fn page) stream))
            (cond ((eq *format* :markdown)
                   (write-string markdown-string stream)
                   (write-string markdown-reflinks stream))
                  (t
                   (reprint-in-format markdown-string markdown-reflinks
                                      stream)))
            (when (page-footer-fn page)
              (funcall (page-footer-fn page) stream)))))
    (unmake-stream-spec (page-final-stream-spec page))))

;;; Process MARKDOWN-STRING block by block to limit maximum memory usage.
(defun reprint-in-format (markdown-string markdown-reflinks stream)
  "- When @BROWSING-LIVE-DOCUMENTATION, the page displayed can be of,
  say, a single function within what would constitute the offline
  documentation of a library. Because markdown reference link
  definitions, for example

          [Daring Fireball]: http://daringfireball.net/

      can be defined anywhere, they wouldn't be resolvable in that
      case, their use is discouraged. Currently, only reflink
      definitions in the vicinity of their uses are resolvable. This
      is left intentionally vague because the specifics are subject to
      change.

      See DEFINE-GLOSSARY-TERM for a better alternative to markdown
      reference links."
  (let ((reflinks-parse-tree (parse-markdown markdown-reflinks))
        ;; The reflink definitions from the most recent
        ;; MAX-N-REFLINK-BLOCKS.
        (reflink-defs ())
        (max-n-reflink-blocks 20)
        ;; Parse trees of the most recent MAX-N-TREES blocks.
        (trees ())
        (max-n-tree-blocks 10))
    (labels
        ((add-parse-tree (tree)
           (when (= (length trees) max-n-tree-blocks)
             (write-tree (first (last trees)))
             (setq trees (nbutlast trees)))
           (push tree trees)
           (push (reflink-defs tree) reflink-defs)
           (setq reflink-defs (subseq reflink-defs
                                      0 (min max-n-reflink-blocks
                                             (length reflink-defs)))))
         (reflinks-around ()
           (apply #'append reflink-defs))
         (write-tree (tree)
           (print-markdown (append tree reflinks-parse-tree (reflinks-around))
                           stream :format *format*)))
      (map-markdown-block-parses (lambda (tree)
                                   (add-parse-tree
                                    (post-process-for-w3m (list tree))))
                                 markdown-string)
      (loop repeat max-n-tree-blocks
            do (add-parse-tree ())))))

(defun reflink-defs (tree)
  (remove-if-not (lambda (tree)
                   (parse-tree-p tree :reference))
                 tree))

;;; Emit markdown definitions for links (in *LINKS*) to REFERENCE
;;; objects that were linked to on the current page.
(defun write-markdown-reference-style-link-definitions (stream)
  (let ((used-links (sort (hash-table-keys (page-used-links *page*))
                          #'string< :key #'link-id))
        (*package* (find-package :keyword)))
    (when used-links
      (format stream "~%")
      (dolist (link used-links)
        (assert (not (link-p (link-page link))))
        ;; The format is [label]: url "title"
        ;; E.g.  [1]: http://example.org/Hobbit#Lifestyle "Hobbit lifestyles"
        (format stream "  [~A]: ~A ~A~%" (link-id link)
                (if (stringp (link-page link))
                    ;; Link to external page.
                    (link-page link)
                    ;; Link to documentation generated in the same run.
                    (link-to-uri link))
                (escape-markdown-reflink-definition-title
                 (markdown-title-name-or-anchor link)))))))

(defun markdown-title-name-or-anchor (link)
  (let* ((dref (link-definition link))
         (resolved (resolve dref nil)))
    (multiple-value-bind (title titledp) (title resolved)
      (cond ((not titledp)
             (princ-to-string (dref-to-anchor dref)))
            (title
             (let ((*package* (or (nth-value 1 (docstring dref))
                                  (guess-package-and-readtable
                                   dref (arglist dref)))))
               (unescape-markdown (process-title (title resolved)))))
            (t
             (process-title (let ((*print-case* :upcase))
                              (prin1-to-string (dref-name dref)))))))))

(defsection @document-return (:title "Return Values")
  "If PAGES are NIL, then DOCUMENT - like CL:FORMAT - returns a
  string (when STREAM is NIL) else NIL.

  If PAGES, then a list of output designators are returned, one for
  each non-empty page (to which some output has been written), which
  are determined as follows.

  - The string itself if the output was to a string.

  - The stream if the output was to a stream.

  - The pathname of the file if the output was to a file.

  If the default page given by the STREAM argument of DOCUMENT was
  written to, then its output designator is the first element of the
  returned list. The rest of the designators correspond to the
  non-empty pages in the PAGES argument of DOCUMENT in that order.")

(defun document-return (stream outputs)
  (let ((default-page-output (last-elt outputs))
        (page-outputs (remove nil (butlast outputs))))
    (cond (page-outputs
           (if default-page-output
               (cons default-page-output page-outputs)
               page-outputs))
          ((null stream)
           default-page-output)
          (t
           nil))))


;;;; URIs of stuff

(defun object-to-uri (object)
  (let ((dref (locate object)))
    (when dref
      (let ((link (find-link dref)))
        (when link
          (link-to-uri link))))))

;;; With w3m, the URLs are like "pax:clhs", but with MGL-PAX/WEB, they
;;; are like "http://localhost:8888/pax:clhs", so we need an extra /.
(defun finalize-pax-url (url)
  (if (eq *html-subformat* :w3m)
      url
      (format nil "/~A" url)))

(defun link-to-uri (link)
  (let ((target-page (link-page link)))
    (if (null target-page)
        (finalize-pax-url (dref-to-pax-url (link-definition link)))
        (let ((target-page-definitions (page-definitions target-page))
              (target-page-uri-fragment (page-uri-fragment target-page)))
          ;; Don't generate anchors when linking to the first
          ;; definition on the page.
          (if (and (xref= (link-definition link)
                                (first target-page-definitions))
                   target-page-uri-fragment)
              (if (eq target-page *page*)
                  ;; "xxx.html"
                  (format nil "~A.~A" (pathname-name target-page-uri-fragment)
                          (pathname-type target-page-uri-fragment))
                  ;; "../xxx.html"
                  (relative-page-uri-fragment target-page *page*))
              (format nil "~A#~A"
                      (if (eq target-page *page*)
                          ""
                          (relative-page-uri-fragment target-page *page*))
                      (anchor-id (link-definition link))))))))

(defun relative-page-uri-fragment (page definition-page)
  (let ((fragment (page-uri-fragment page))
        (reference-fragment (page-uri-fragment definition-page)))
    (assert (and fragment reference-fragment))
    (relativize-pathname fragment reference-fragment)))


(defsection @markdown-support (:title "Markdown Support")
  "The @MARKDOWN in docstrings is processed with the @3BMD library."
  (@markdown-in-docstrings section)
  (@markdown-syntax-highlighting section)
  (@mathjax section))

(defsection @markdown-in-docstrings (:title "Markdown in Docstrings")
  """[ strip-docstring-indent function][docstring]

  [ reprint-in-format function][docstring]

  [ sanitize-aggressively-p function][docstring]

  - [ round-up-indentation function][docstring]
  - [ escape-html-in-docstring function][docstring]
  - [ escape-heading-in-docstring function][docstring]""")

(defun sanitize-aggressively-p ()
  "Docstrings of definitions which do not have a @HOME-SECTION and are
  not SECTIONs themselves are assumed to have been written with no
  knowledge of PAX and to conform to markdown only by accident. These
  docstrings are thus sanitized more aggressively."
  (and (not (boundp '*section*))
       ;; This is implicit in the above, but docstrings passed
       ;; directly to DOCUMENT are not treated aggressively.
       *documenting-reference*
       (null (home-section *documenting-reference*))))

(defvar *document-docstring-key* nil)

(defun/autoloaded document-docstring
    (docstring stream &key (indentation "    ")
               exclude-first-line-p (paragraphp t))
  "Write DOCSTRING to STREAM, [sanitizing the markdown]
  [@markdown-in-docstrings] from it, performing @CODIFICATION and
  @LINKING-TO-CODE, finally prefixing each line with INDENTATION. The
  prefix is not added to the first line if EXCLUDE-FIRST-LINE-P. If
  PARAGRAPHP, then add a newline before and after the output."
  (when (and docstring
             (not (equal docstring ""))
             ;; If the output is going to /dev/null, then skip this
             ;; operation because it's costly.
             (not *first-pass*))
    (let ((docstring (funcall (or *document-docstring-key* #'identity)
                              docstring)))
      (when docstring
        (let* ((docstring (sanitize-docstring
                           docstring :aggressivep (sanitize-aggressively-p)))
               (reindented (prefix-lines
                            indentation (codify-and-link docstring)
                            :exclude-first-line-p exclude-first-line-p)))
          (if paragraphp
              (format stream "~%~A~%" reindented)
              (format stream "~A" reindented)))))))

(defsection @markdown-syntax-highlighting (:title "Syntax Highlighting")
  "For syntax highlighting, Github's @FENCED-CODE-BLOCKS markdown
  extension to mark up code
  blocks with triple backticks is enabled so all you need to do is
  write:

      ```elisp
      (defun foo ())
      ```

  to get syntactically marked up HTML output. Copy `src/style.css`
  from PAX and you are set. The language tag, `elisp` in this example,
  is optional and defaults to `common-lisp`.

  See the documentation of @3BMD and @COLORIZE for the details.")

(define-glossary-term @3bmd (:title "3BMD" :url "https://github.com/3b/3bmd"))

(define-glossary-term @colorize
    (:title "Colorize" :url "https://github.com/redline6561/colorize/"))

(define-glossary-term @fenced-code-blocks
    (:title "fenced code blocks"
     :url "https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks"))

(defsection @mathjax (:title "MathJax")
  """Displaying pretty mathematics in TeX format is supported via
  MathJax. It can be done inline with `$` like this:

      $\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$

  which is displayed as $\int_0^\infty e^{-x^2}
  dx=\frac{\sqrt{\pi}}{2}$, or it can be delimited by `$$` like this:

      $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

  to get: $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

  MathJax will leave code blocks (including those inline with
  backticks) alone. Outside code blocks, escape `$` by prefixing it
  with a backslash to scare MathJax off.

  Escaping all those backslashes in TeX fragments embedded in Lisp
  strings can be a pain. @PYTHONIC-STRING-READER can help with that.""")

(define-glossary-term @pythonic-string-reader
    (:title "Pythonic String Reader"
     :url "https://github.com/smithzvk/pythonic-string-reader"))


;;;; Automatic markup of symbols

;;; Take a string in markdown format. Handle the DOCSTRING locative,
;;; markup symbols as code (if *DOCUMENT-UPPERCASE-IS-CODE*), autolink
;;; (if *DOCUMENT-LINK-SECTIONS*, *DOCUMENT-LINK-CODE*) and always
;;; handle explicit links with locatives (e.g. [FOO][function]).
;;; Finally handle *DOCUMENT-BASE-URL* and return the transformed
;;; string.
(defun codify-and-link (string)
  (when string
    (with-output-to-string (out)
      (print-markdown (add-base-url (link (codify (include-docstrings
                                                   (parse-markdown string)))))
                      out))))


;;;; Post-process the markdown parse tree to make it prettier on w3m
;;;; and maybe make relative links absolute.

(defun post-process-for-w3m (parse-tree)
  (if (eq *html-subformat* :w3m)
      (flet ((translate (parent tree)
               (declare (ignore parent))
               (cond ((eq (first tree) :code)
                      `(:strong ,tree))
                     ((eq (first tree) :verbatim)
                      (values `((:raw-html #.(format nil "<i>~%"))
                                ,(indent-verbatim tree) (:raw-html "</i>"))
                              nil t))
                     (t
                      (values `((:raw-html #.(format nil "<i>~%"))
                                ,(indent-code-block tree)
                                (:raw-html "</i>"))
                              nil t)))))
        (map-markdown-parse-tree '(:code :verbatim 3bmd-code-blocks::code-block)
                                 '() nil #'translate parse-tree))
      parse-tree))


(defun include-docstrings (parse-tree)
  (map-markdown-parse-tree (list :reference-link) () nil
                           #'translate-docstring-links parse-tree))

;;; This is the first of the translator functions, which are those
;;; passed to MAP-MARKDOWN-PARSE-TREE.
(defun translate-docstring-links (parent tree)
  """DOCSTRING is a pseudolocative for including the parse tree of the
  markdown [DOCSTRING][function] of a definition in the parse tree of
  a docstring when generating documentation. It has no source location
  information and only works as an explicit link. This construct is
  intended to allow docstrings to live closer to their implementation,
  which typically involves a non-exported definition.

  ```cl-transcript (:dynenv pax-std-env)
  (defun div2 (x)
    "X must be [even* type][docstring]."
    (/ x 2))

  (deftype even* ()
    "an even integer"
    '(satisfies evenp))

  (document #'div2)
  .. - [function] DIV2 X
  ..
  ..     X must be an even integer.
  ..
  ```"""
  (declare (ignore parent))
  (assert (parse-tree-p tree :reference-link))
  (let ((label (pt-get tree :label))
        (definition (parse-tree-to-text (pt-get tree :definition) :deemph t)))
    (nth-value-or 0
      (when (eq (read-locative-from-noisy-string definition) 'docstring)
        (multiple-value-bind (name locative foundp)
            (read-reference-from-string (parse-tree-to-text label))
          (when foundp
            (if-let (dref (dref name locative nil))
              (when-let (docstring (docstring dref))
                (values (or (parse-markdown (sanitize-docstring docstring))
                            '(""))
                        ;; Detecting circular includes would be hard
                        ;; because the 'recurse' return value is
                        ;; handled in the caller of this function.
                        t t))
              (warn "~@<Including ~S failed because ~S ~S cannot be ~Sd~:@>"
                    'docstring name locative 'locate)))))
      tree)))


(defsection @codification (:title "Codification")
  (*document-uppercase-is-code* variable)
  (@codifiable glossary-term)
  (@interesting glossary-term)
  (*document-downcase-uppercase-code* variable))

(defvar *document-uppercase-is-code* t
  """When true, @INTERESTING @NAMEs extracted from @CODIFIABLE @WORDs
  are assumed to be code as if they were marked up with backticks. For
  example, this docstring

      "T PRINT CLASSes SECTION *PACKAGE* MGL-PAX ASDF
      CaMeL Capital"

  is equivalent to this:

      "`T` `PRINT` `CLASS`es `SECTION` `*PACKAGE*` `MGL-PAX` `ASDF`
      CaMel Capital"

  and renders as

  `T` `PRINT` `CLASS`es `SECTION` `MGL-PAX` `ASDF` CaMel Capital

  where the links are added due to *DOCUMENT-LINK-CODE*.

  [handle-codification-escapes function][docstring]""")

(define-glossary-term @codifiable (:title "codifiable")
  "A @WORD is _codifiable_ iff

  - it has a single uppercase character (e.g. it's `T`) and no
    lowercase characters at all, or

  - there is more than one uppercase character and no lowercase
    characters between them (e.g. CLASSes, nonREADable, CLASS-NAMEs
    but not `Classes` or `aTe`.")

(defun codifiable-word-p (string)
  (and
   ;; Check that it's a @WORD too.
   (notany #'whitespacep string)
   (uppercase-core-bounds string)))

(define-glossary-term @interesting (:title "interesting")
  "A @NAME is _interesting_ iff

  - it names a symbol external to its package, or

  - it is at least 3 characters long and names an interned symbol, or

  - it names a [local reference][@LOCAL-REFERENCES].

  See @PACKAGE-AND-READTABLE.")

(defun interesting-name-p (xref-name name)
  (or (and (symbolp xref-name)
           (or (<= 3 (length name))
               (external-symbol-p xref-name)))
      (has-local-reference-p xref-name)))

;;; The core of the implementation of *DOCUMENT-UPPERCASE-IS-CODE*.
;;;
;;; This is called by MAP-WORDS so the return values are NEW-TREE,
;;; SLICE. Also called by TRANSLATE-EMPH that expects only a single
;;; return value, the new tree.
(defun translate-uppercase-word (parent tree word)
  (declare (ignore parent))
  (let ((emph (and (listp tree) (eq :emph (first tree))))
        (codifiablep (codifiable-word-p word)))
    (nth-value-or 0
      (handle-codification-escapes emph codifiablep word)
      (cond ((or (not *document-uppercase-is-code*)
                 (not codifiablep))
             ;; Don't change anything.
             nil)
            (emph
             (codify-uppercase-word (format nil "*~A*" word)))
            (t
             (codify-uppercase-word word))))))

(defun handle-codification-escapes (emph codifiablep word)
  """To suppress codification, add a backslash to the beginning of the
  a @CODIFIABLE word or right after the leading `\\*` if it would
  otherwise be parsed as markdown emphasis:

      "\\SECTION *\\PACKAGE*"

  The number of backslashes is doubled above because that's how the
  example looks in a docstring. Note that the backslash is discarded
  even if *DOCUMENT-UPPERCASE-IS-CODE* is false."""
  (cond ((and emph codifiablep (eql #\\ (first-elt word)))
         ;; E.g. "*\\DOCUMENT-NORMALIZE-PACKAGES*"
         ;; -> (:EMPH "DOCUMENT-NORMALIZE-PACKAGES")
         (values (list `(:emph ,(subseq word 1))) t))
        ((and codifiablep (eql #\\ (first-elt word)))
         ;; Discard the leading backslash escape.
         ;; E.g. "\\MGL-PAX" -> "MGL-PAX"
         (values (list (subseq word 1)) t))))

;;; Find the [approximately] longest @NAME in WORD. Return a 3bmd
;;; parse tree fragment with that substring marked up as code and the
;;; suffixes downcased (so that CLASSES turns into `CLASS`es).
;;;
;;; Handles the rules laid out in *DOCUMENT-UPPERCASE-IS-CODE* not
;;; already handled in the caller TRANSLATE-UPPERCASE-WORD. Trims
;;; separators and depluralizes.
(defun codify-uppercase-word (word)
  (multiple-value-bind (xref-name name) (parse-uppercase-word word)
    (when (and name (interesting-name-p xref-name name))
      (let ((pos (search name word :test #'char-equal)))
        (assert pos)
        (values `(,@(when (plusp pos)
                      `(,(subseq word 0 pos)))
                  (:code ,(maybe-downcase name))
                  ,@(let ((tail-pos (+ pos (length name))))
                      (when (< tail-pos (length word))
                        ;; CLASSES -> `CLASS`es
                        `(,(string-downcase (subseq word tail-pos))))))
                t)))))

;;; Handle *DOCUMENT-UPPERCASE-IS-CODE* in normal strings and :EMPH
;;; (to recognize *VAR*). Also, perform consistency checking of
;;; cl-transcript code blocks (see @TRANSCRIBING-WITH-EMACS).
(defun codify (parse-tree)
  (map-markdown-parse-tree
   (list :emph '3bmd-code-blocks::code-block :reference-link :code)
   '(:code :verbatim 3bmd-code-blocks::code-block
     :explicit-link :image :mailto :reference)
   t
   #'translate-to-code
   parse-tree))

(defvar *translating-reference-link* nil)

(defun translate-to-code (parent tree)
  (cond ((stringp tree)
         (let ((string tree))
           (values (map-words string
                              (lambda (string start end)
                                (let ((word (subseq string start end)))
                                  (translate-uppercase-word
                                   parent string word))))
                   ;; don't recurse, do slice
                   nil t)))
        ((parse-tree-p tree :emph)
         (translate-emph parent tree))
        ((parse-tree-p tree '3bmd-code-blocks::code-block)
         (translate-code-block parent tree))
        ((parse-tree-p tree :reference-link)
         (let ((replacement (copy-list tree)))
           (setf (pt-get replacement :label)
                 (let ((*translating-reference-link* t))
                   (codify (pt-get tree :label))))
           replacement))
        ((parse-tree-p tree :code)
         `(:code ,(maybe-downcase (second tree))))
        (t
         (error "~@<Unexpected tree type ~S.~:@>" (first tree)))))

;;; CODE-BLOCK looks like this:
;;;
;;;     (3BMD-CODE-BLOCKS::CODE-BLOCK :LANG "commonlisp" :CONTENT "42")
(defun translate-code-block (parent code-block)
  (declare (ignore parent))
  (let ((lang (getf (rest code-block) :lang)))
    (if (starts-with-subseq "cl-transcript" lang)
        (let* ((suffix (subseq lang (length "cl-transcript")))
               (args (read-from-string suffix nil nil))
               (original (getf (rest code-block) :content)))
          ;; The possibly manually tweaked (e.g. comments in output)
          ;; original goes to the output, but retranscribe it just for
          ;; consistency checking.
          (transcribe-code-block original args)
          `(3bmd-code-blocks::code-block :lang "common-lisp"
                                         :content ,original))
        code-block)))

(defun transcribe-code-block (transcript args)
  (let ((dynenv (getf args :dynenv))
        (*transcribe-check-consistency* t))
    (remf args :dynenv)
    (flet ((call-it ()
             (if *document-open-linking*
                 (handler-case
                     (apply #'transcribe transcript nil :update-only t args)
                   (transcription-error (e)
                     (warn "~A" e)
                     transcript))
                 (apply #'transcribe transcript nil :update-only t args))))
      (cond ((and dynenv (ignore-errors (fdefinition dynenv)))
             (funcall dynenv #'call-it))
            (t
             (when dynenv
               (funcall (if *document-open-linking* 'warn 'error)
                        "Undefined :DYNENV function ~S" dynenv))
             (call-it))))))

;;; Undo the :EMPH parsing for code references. E.g. (:EMPH "XXX") ->
;;; "*XXX*" if "*XXX*" is to be codified according to
;;; CODIFY-UPPERCASE-WORD-P.
(defun translate-emph (parent tree)
  (if (= 2 (length tree))
      (let ((translation (translate-uppercase-word parent tree (second tree))))
        (if translation
            ;; Replace TREE with TRANSLATION, don't process
            ;; TRANSLATION again recursively, slice the return value
            ;; into the list of children of PARENT.
            (values translation nil t)
            ;; leave it alone, don't recurse, don't slice
            (values tree nil nil)))
      ;; Tell MAP-MARKDOWN-PARSE-TREE to leave TREE unchanged,
      ;; recurse, don't slice.
      (values tree t nil)))


(defvar *document-downcase-uppercase-code* nil
  """If true, then all Markdown inline code (that is, `stuff between
  backticks`, including those found if *DOCUMENT-UPPERCASE-IS-CODE*)
  which has no lowercase characters is downcased in the output.
  Characters of literal strings in the code may be of any case. If
  this variable is :ONLY-IN-MARKUP and the output format does not
  support markup (e.g. it's :PLAIN), then no downcasing is performed.
  For example,

      `(PRINT "Hello")`

  is downcased to

      `(print "Hello")`

  because it only contains uppercase characters outside the string.
  However,

      `MiXed "RESULTS"`

  is not altered because it has lowercase characters.

  If the first two characters are backslashes, then no downcasing is
  performed, in addition to @PREVENTING-AUTOLINKING. Use this to mark
  inline code that's not Lisp.

      Press `\\M-.` in Emacs.""")

(defun/autoloaded downcasingp ()
  (or (and *document-downcase-uppercase-code*
           (not (eq *document-downcase-uppercase-code*
                    :only-in-markup)))
      (and (eq *document-downcase-uppercase-code*
               :only-in-markup)
           (not (eq *format* :plain)))))

(defun prin1-to-string* (object)
  (let ((*print-case* (if (downcasingp)
                          :downcase
                          :upcase)))
    (prin1-to-string object)))

(defun maybe-downcase (string)
  (if *translating-reference-link*
      (cond ((starts-with-subseq "\\\\" string)
             (subseq string 2))
            ((starts-with-subseq "\\" string)
             (if (downcasingp)
                 (downcase-all-uppercase-code (subseq string 1))
                 (subseq string 1)))
            (t
             (if (downcasingp)
                 (downcase-all-uppercase-code string)
                 string)))
      (cond ((starts-with-subseq "\\\\" string)
             ;; Leave one backslash to escape linking in
             ;; TRANSLATE-TO-LINKS unless we are in a reference link,
             ;; where linking cannot be escaped.
             (subseq string 1))
            ((downcasingp)
             (downcase-all-uppercase-code string))
            (t
             string))))

(defun maybe-downcase-all-uppercase-code (string)
  (if (downcasingp)
      (downcase-all-uppercase-code string)
      string))

(defun downcase-all-uppercase-code (string)
  (with-output-to-string (s)
    (map-code-chars (lambda (char escaped in-string)
                      (when (and (not in-string)
                                 (lower-case-p char))
                        (return-from downcase-all-uppercase-code string))
                      (when escaped
                        (write-char #\\ s))
                      (if in-string
                          (write-char char s)
                          (write-char (char-downcase char) s)))
                    string)))

(defun map-code-chars (fn string)
  (let ((in-string nil)
        (escaped nil))
    (loop for char across string
          do (cond (escaped
                    (funcall fn char t in-string)
                    (setq escaped nil))
                   ((eq char #\\)
                    (setq escaped t))
                   ((eq char #\")
                    (funcall fn char nil in-string)
                    (setq in-string (not in-string)))
                   (t
                    (funcall fn char nil in-string))))))


(defsection @linking-to-code (:title "Linking to Code")
  """In this section, we describe all ways of linking to code
  available when *DOCUMENT-UPPERCASE-IS-CODE* is true.

  _Note that invoking [`\\M-.`][@navigating-in-emacs section] on the
  @NAME of any of the following links will disambiguate based the
  textual context, determining the locative. In a nutshell, if `\\M-.`
  works without popping up a list of choices, then the documentation
  will contain a single link._"""
  (*document-link-code* variable)
  (@specified-locative section)
  (@unspecified-locative section)
  (@explicit-and-autolinking section)
  (@preventing-autolinking section)
  (@unresolvable-reflinks section)
  (@suppressed-links section)
  (@local-references section))

(defvar *document-link-code* t
  """Enable the various forms of links in docstrings described in
  @LINKING-TO-CODE. See the following sections for a description of
  how to use linking.""")

;;; Handle *DOCUMENT-LINK-CODE* (:CODE for `SYMBOL` and
;;; :REFERENCE-LINK for [symbol][locative]). Don't hurt other links.
(defun link (parse-tree)
  (let ((linked-refs (make-array 0 :fill-pointer t :adjustable t
                                   :element-type 'dref)))
    (map-markdown-parse-tree
     '(:code :reference-link)
     '(:explicit-link :image :mailto)
     nil
     (rcurry #'translate-to-links linked-refs)
     parse-tree)))

(defun translate-to-links (parent tree linked-refs)
  (nth-value-or 0
    (maybe-unescape-or-autolink parent tree linked-refs)
    (maybe-translate-explicit-link tree linked-refs)
    (assert nil)))

;;; Check if we are in a position to link to REF and that we are
;;; allowed to.
(defun linkable-ref-p (ref &key page)
  (declare (special *document-link-sections*))
  (let ((ref (replace-go-target ref)))
    (and (or (and *document-link-sections*
                  (eq (xref-locative-type ref) 'section))
             *document-link-code*)
         (let ((page (or page (definition-page ref))))
           (assert (not (link-p page)))
           (or
            ;; These have no pages, but won't result in link anyway.
            ;; Keep them.
            (member (xref-locative-type ref) '(dislocated argument))
            ;; Pax URLs are always linkable.
            (null page)
            ;; Intrapage links always work.
            (eq *page* page)
            ;; Absolute URLs always work.
            (stringp page)
            ;; PAGE is a PAGE structure. We need to know the
            ;; URI-FRAGMENT of both pages. See
            ;; RELATIVE-PAGE-URI-FRAGMENT.
            (and (page-uri-fragment *page*)
                 (page-uri-fragment page)))))))

(defun linkablep (link)
  (linkable-ref-p (link-definition link)
                  :page (link-page (unaliased-link link))))

(defun linkable-references (refs)
  (remove-if-not #'linkable-ref-p refs))

(defsection @specified-locative (:title "Specified Locative")
  """The following examples all render as [DOCUMENT][function].

  - `\[DOCUMENT][function]` (*name + locative, explicit link*)
  - `DOCUMENT function` (*name + locative, autolink*)
  - `function DOCUMENT` (*locative + name, autolink*)

  The Markdown link definition (i.e. `\function` between the second
  set of brackets above) needs no backticks to mark it as code.

  Here and below, the @NAME (`\DOCUMENT`) is uppercased, and we rely
  on *DOCUMENT-UPPERCASE-IS-CODE* being true. Alternatively, the @NAME
  could be explicitly marked up as code with a pair of backticks, and
  then its character case would likely not matter (subject to
  READTABLE-CASE).

  The link text in the above examples is `\DOCUMENT`. To override it,
  this form may be used:

  - `[see this][document function]` (*title + name + locative,
    explicit link*) renders as: [see this][document function].""")

(defun linkables-for-specified-locative (name locative)
  "With a locative specified (e.g. in the explicit link
  `[FOO][function]` or in the text `the FOO function`), a single link
  is made irrespective of any local references."
  (if (member (locative-type locative) '(dislocated argument))
      ;; Handle an explicit [FOO][dislocated] in markdown. This is
      ;; always LINKABLE-REF-P.
      (list (xref name 'dislocated))
      (if-let (dref (dref name locative nil))
        ;; Prefer the live definition.
        (when-let (link (find-link (replace-go-target dref)))
          (when (linkablep link)
            (list (link-definition link))))
        ;; Fall back on an external one.
        (let ((xref (replace-xref-go-target (xref name locative))))
          (when-let (link (find-external-link xref))
            (when (linkablep link)
              (list (link-definition link))))))))

(defun replace-xref-go-target (xref)
  (or (and (eq (xref-locative-type xref) 'go)
           ;; See "accidental GO" in MGL-PAX-TEST::TEST-GO.
           (ignore-errors
            (destructuring-bind (go-name go-locative)
                (first (xref-locative-args xref))
              (xref go-name go-locative))))
      xref))

(defsection @unspecified-locative (:title "Unspecified Locative")
  "[filter-string-based-references function][docstring]

  [filter-method-references function][docstring]

  [filter-locative-references function][docstring]"
  (@unambiguous-unspecificed-locative section)
  (@ambiguous-unspecified-locative section))

(defun linkables-for-unspecified-locative (name &key local)
  (filter-locative-references
   (filter-method-references
    (replace-go-targets
     (filter-external-references
      (filter-string-based-references
       (linkable-references
        (definitions-with-name name :local local))))))))

(defun replace-go-targets (references)
  (mapcar #'replace-go-target references))

(defun replace-go-target (dref)
  (if (eq (xref-locative-type dref) 'go)
      (go-target-dref dref)
      dref))

(defun filter-string-based-references (refs)
  "When only an @NAME is provided without a locative, all
  definitions of the name are considered as possible link targets.
  Then, definitions that are not symbol-based (i.e. whose
  XREF-NAME is not a symbol) are filtered out to prevent
  unrelated PACKAGEs and ASDF:SYSTEMs from cluttering the
  documentation without the control provided by importing symbols."
  (remove-if #'string-based-reference-p refs))

(defun string-based-reference-p (reference)
  ;; We assume that REFERENCE is canonical and only look at the
  ;; name.
  (stringp (xref-name reference)))

(defun filter-method-references (refs)
  "To further reduce clutter, if the definitions include a
  GENERIC-FUNCTION locative, then all references with LOCATIVE-TYPE
  [METHOD][locative], [ACCESSOR][locative], [READER][locative] and
  [WRITER][locative] are removed to avoid linking to a possibly large
  number of methods."
  (flet ((non-method-refs ()
           (remove-if (lambda (ref)
                        (member (xref-locative-type ref)
                                '(accessor reader writer method)))
                      refs)))
    (cond
      ;; If in doubt, prefer the generic function to methods.
      ((find 'generic-function refs :key #'xref-locative-type)
       (non-method-refs))
      ;; No generic function, prefer non-methods to methods.
      ((non-method-refs))
      (t
       refs))))

(defun filter-locative-references (refs)
  "Furthermore, filter out all references with LOCATIVE-TYPE
  LOCATIVE if there are references with other LOCATIVE-TYPEs."
  (or (remove 'locative refs :key #'xref-locative-type)
      refs))

(defsection @unambiguous-unspecificed-locative
    (:title "Unambiguous Unspecified Locative")
  """In the following examples, although no locative is specified,
  `\DOCUMENT` names a single @NAME being DOCUMENTed, so they all
  render as [DOCUMENT][function].

  - `\[DOCUMENT][]` (*name, explicit link*),
  - `DOCUMENT` (*name, autolink*).

  To override the title:

  - `\[see this][document]` (*title + name, explicit link*) renders
    as: [see this][document].""")

(defsection @ambiguous-unspecified-locative
    (:title "Ambiguous Unspecified Locative")
  """These examples all render as [SECTION][], linking to both
  definitions of the @NAME `\SECTION`, the `\CLASS` and the
  `\LOCATIVE`. Note that the rendered output is a single link to a
  disambiguation page when @BROWSING-LIVE-DOCUMENTATION, while
  multiple, numbered links are generated in offline documentation.

  - `[SECTION][]` (*name, explicit link*)
  - `\SECTION` (*name, autolink*)

  To override the title:

  - `\[see this][section]` (*title + name, explicit link*) renders as:
    [see this][section].""")

(defsection @explicit-and-autolinking (:title "Explicit and Autolinking")
  "The examples in the previous sections are marked with *explicit
  link* or *autolink*. Explicit links are those with a Markdown
  reference link spelled out explicitly, while autolinks are those
  without.")

(defun linkables-for-explicitly-unspecified-locative (name)
  "Explicit links with an unspecified locative (e.g. `[FOO][]`) are
  linked to all non-local references."
  (linkables-for-unspecified-locative name :local :exclude))

(defun linkables-for-autolink-with-unspecified-locative (name)
  "Unless a locative is [specified][ @specified-locative section], no
  [autolinking][ @explicit-and-autolinking section] is performed for
  @NAMEs for which there are local references. For example, `FOO` does
  not get any links if there is _any_ local reference with the same
  @NAME."
  (linkables-for-unspecified-locative name))

;;; All returned REFERENCES are for the same name.
(defun linkables-for-autolink (names locatives linked-refs)
  (or
   ;; Use the first of NAMES with which some LOCATIVES form known
   ;; references.
   (loop for name in names
           thereis (loop for locative in locatives
                         append (linkables-for-specified-locative
                                 name locative)))
   ;; Fall back on the no-locative case.
   (loop for name in names
         for refs = (linkables-for-autolink-with-unspecified-locative name)
         until (or (suppressed-link-p name refs linked-refs)
                   (has-local-reference-p name))
           thereis refs
         until (definitions-with-name name :local :include))))


;;;; Explicit links

(defun maybe-translate-explicit-link (tree linked-refs)
  (when (eq :reference-link (first tree))
    (if (or (pt-get tree :definition) (pt-get tree :tail))
        (translate-explicit-link tree linked-refs)
        ;; (:REFERENCE-LINK :LABEL ("xxx") :TAIL NIL), the parse of [xxx].
        (values `(:plain "[" ,@(pt-get tree :label) "]") t nil))))

;;; This translator handles :REFERENCE-LINK nodes:
;;;
;;; - those with an explicit locative (:REFERENCE-LINK :LABEL ((:CODE
;;;   "SOMETHING")) :DEFINITION ("function")), the parse of
;;;   [`SOMETHING`][function],
;;;
;;; - and those with no locative (:REFERENCE-LINK :LABEL ((:CODE
;;;   "SOMETHING")) :TAIL "[]"), the parse of [`SOMETHING`][].
(defun translate-explicit-link (reflink linked-refs)
  ;; Markdown to handle:
  ;; - [`SECTION`][class]
  ;; - [`SECTION`][]
  ;; - [see this][section class]
  ;; - [see this][section]
  ;;
  ;; For example, the tree for [`SECTION`][class] is (:REFERENCE-LINK
  ;; :LABEL ((:CODE "SECTION")) :DEFINITION ("class")).
  (multiple-value-bind (label explicit-label-p name locative pax-link-p)
      (dissect-reflink reflink)
    (cond ((not pax-link-p)
           ;; [something][user-defined-id] or [something]
           reflink)
          ((and (eq name 'not-found)
                (member locative '(dislocated argument)))
           ;; [not code][dislocated]
           (values label nil t))
          (t
           (let ((refs (unless (eq name 'not-found)
                         (if locative
                             (linkables-for-specified-locative name locative)
                             (linkables-for-explicitly-unspecified-locative
                              name)))))
             (cond (refs
                    (dolist (ref refs)
                      (vector-push-extend ref linked-refs))
                    (values (make-reflinks label explicit-label-p refs) nil t))
                   (t
                    (values (if (likely-a-pax-reflink-p name locative reflink)
                                (signal-unresolvable-reflink reflink name
                                                             locative)
                                label)
                            nil t))))))))

(defun likely-a-pax-reflink-p (name locative reflink)
  (or (and locative (symbolp name))
      (zerop (length (getf (rest reflink) :definition)))))

;;; Return 1. the label, 2. whether to use the returned label in the
;;; reference link without further transformations (e.g. replace it
;;; with SECTION-TITLE), 3. name, 4. the locative, 5. whether the
;;; reflink looks like something we should resolve (as opposed to a
;;; user-defined one).
(defun dissect-reflink (tree)
  (assert (parse-tree-p tree :reference-link))
  (destructuring-bind (&key label definition tail) (rest tree)
    (let* ((empty-definition-p (and (zerop (length definition))
                                    (or (null tail)
                                        (equal tail "[]"))))
           (definition (trim-whitespace
                        (parse-tree-to-text definition :deemph t)))
           (locative-from-def
             (and definition (read-locative-from-noisy-string definition)))
           (label-string (trim-whitespace
                          (parse-tree-to-text label :deemph nil))))
      (nth-value-or 0
        (and (or empty-definition-p locative-from-def)
             label-string
             ;; [foo][] or [foo][function]
             (multiple-value-bind (xref-name name)
                 (parse-reflink-label-string label-string locative-from-def)
               (when name
                 (values label nil xref-name locative-from-def t))))
        (and (member locative-from-def '(dislocated argument))
             (values label t 'not-found locative-from-def t))
        ;; [see this][foo]
        (multiple-value-bind (xref-name name)
            (parse-word definition :trim nil :depluralize nil
                                   :only-one (constantly t))
          (when name
            (values label t xref-name nil t)))
        ;; [see this][foo function]
        (multiple-value-bind (name locative foundp)
            (and definition (read-reference-from-string definition))
          (when foundp
            (values label t name locative t)))
        (values label nil 'not-found locative-from-def
                (or empty-definition-p locative-from-def))))))

(defun parse-reflink-label-string (label-string locative)
  (let ((interesting-xref-name nil)
        (interesting-name nil))
    (flet ((good-parse-p (xref-name name)
             (cond ((external-locative-p locative)
                    (let ((reference (dref xref-name locative nil)))
                      (when reference
                        (xref-name reference))))
                   ((has-reference-p xref-name)
                    t)
                   (t
                    ;; Remember the first interesting object ...
                    (when (and (null interesting-name)
                               (interesting-name-p xref-name name))
                      (setq interesting-xref-name xref-name
                            interesting-name name)
                      ;; ... but don't stop looking for a known
                      ;; reference.
                      nil)))))
      (nth-value-or 1
        (parse-word label-string
                    :trim nil :depluralize t
                    :clhs-substring-match t
                    :only-one #'good-parse-p)
        ;; Only consider merely interesting names if there were no
        ;; objects with known references.
        (values interesting-xref-name interesting-name)))))


;;;; Autolinking

;;; This translator handles (:CODE "SOMETHING"), the parse of
;;; `SOMETHING`: looks for any references to "SOMETHING" and tanslates
;;; it to, for example, (:REFERENCE-LINK :LABEL ((:CODE "SOMETHING"))
;;; :DEFINITION ("function")) if there is a single function reference
;;; to it.
(defun autolink (parent tree word linked-refs)
  (let ((refs (linkables-for-autolink
               (parse-word word :trim nil :depluralize t)
               (find-locatives-around parent tree)
               linked-refs)))
    (cond (refs
           (let ((reflinks (make-reflinks `(,tree) nil refs)))
             (dolist (ref refs)
               (vector-push-extend ref linked-refs))
             (values reflinks nil t)))
          (t
           tree))))

;;; Find locatives just before or after TREE in PARENT. For example,
;;; PARENT is (:PLAIN "See" "function" " " (:CODE "FOO")), and TREE is
;;; (:CODE "FOO").
(defun find-locatives-around (parent tree)
  (let ((locatives ()))
    (labels ((try-string (string)
               (let ((locative (read-locative-from-noisy-string string)))
                 (when locative
                   (push locative locatives))))
             (try (element)
               (cond ((stringp element)
                      (try-string element))
                     ((eq :code (first element))
                      (try-string (second element)))
                     ;; (:REFERENCE-LINK :LABEL ((:CODE
                     ;; "CLASS")) :DEFINITION ("0524"))
                     ((eq :reference-link (first element))
                      (try (first (third element)))))))
      ;; Note that (EQ (THIRD REST) TREE) may be true multiple times,
      ;; for example if strings are interned and "FOO" occurs multiple
      ;; times in PARENT.
      (loop for rest on (rest parent)
            do (when (and (eq (third rest) tree)
                          (stringp (second rest))
                          (blankp (second rest)))
                 (try (first rest))
                 (return)))
      ;; For example, (:PLAIN "See" "the" "FOO" " " "function")
      (loop for rest on (rest parent)
            do (when (and (eq (first rest) tree)
                          (stringp (second rest))
                          (blankp (second rest)))
                 (try (third rest))
                 (return))))
    locatives))


(defsection @preventing-autolinking (:title "Preventing Autolinking")
  """In the common case, when [*DOCUMENT-UPPERCASE-IS-CODE*][] is true,
  prefixing an uppercase @WORD with a backslash prevents it from being
  codified and thus also prevents [autolinking][
  @explicit-and-autolinking section] form kicking in. For example,

      \DOCUMENT

  renders as \DOCUMENT. If it should be marked up as code but not
  autolinked, the backslash must be within backticks like this:

      `\DOCUMENT`

  This renders as `\DOCUMENT`. Alternatively, the DISLOCATED or the
  ARGUMENT locative may be used as in `[DOCUMENT][dislocated]`.""")

(defun maybe-unescape-or-autolink (parent tree linked-refs)
  (when (parse-tree-p tree :code)
    (let ((string (second tree)))
      (if (starts-with #\\ string)
          `(:code ,(subseq string 1))
          (autolink parent tree string linked-refs)))))


;;;; Common code for @EXPLICIT-AND-AUTOLINKING

;;; With older versions it's a STRING or NIL.
(defparameter *3bmd-reflink-definition-is-list*
  (not (atom (pt-get (esrap:parse '3bmd-grammar::reference-link "[x][y]")
                     :definition))))

(declaim (inline %make-reflink))
(defun %make-reflink (label definition)
  (if *3bmd-reflink-definition-is-list*
      `(:reference-link :label ,label :definition (,definition))
      `(:reference-link :label ,label :definition ,definition)))

(defgeneric title (object)
  (:method (object)
    (declare (ignore object))
   nil)
  (:method ((section section))
    (values (section-title section) t))
  (:method ((glossary-term glossary-term))
    (values (glossary-term-title glossary-term) t)))

;;; For LABEL (a parse tree fragment) and some references to it
;;; (REFS), return a markdown parse tree fragment to be spliced into a
;;; markdown parse tree.
(defun make-reflinks (label explicit-label-p refs)
  (let ((ref-1 (first refs)))
    (cond ((endp refs)
           ;; All references were filtered out.
           label)
          ((< 1 (length refs))
           (if *document-open-linking*
               ;; [`label`](pax:name)
               `((:explicit-link
                  :label ,label
                  :source ,(finalize-pax-url
                            (urlencode
                             (name-to-ambiguous-pax-url
                              ;; CLHS aliases are XREFs.
                              (xref-name ref-1))))))
               ;; `label`([1][link-id-1] [2][link-id-2])
               `(,@label
                 "("
                 ,@(loop
                     for i upfrom 0
                     for ref in (dref::sort-references refs)
                     append `(,@(unless (zerop i)
                                  '(" "))
                              ,(%make-reflink `(,(code-fragment i))
                                              (link-to-definition ref))))
                 ")")))
          ((member (xref-locative-type ref-1) '(dislocated argument))
           label)
          (explicit-label-p
           `(,(%make-reflink label (link-to-definition ref-1))))
          (t
           (let ((title (title (resolve ref-1 nil))))
             `(,(%make-reflink
                 (if (null title)
                     label
                     (codify (parse-markdown title)))
                 (link-to-definition ref-1))))))))


(defsection @unresolvable-reflinks (:title "Unresolvable Links")
  (unresolvable-reflink condition)
  (output-reflink function)
  (output-label function))

(define-condition unresolvable-reflink (warning)
  ((reflink :initarg :reflink :reader unresolvable-reflink-string)
   (name :initarg :name :reader unresolvable-reflink-name)
   (locative :initarg :locative :reader unresolvable-reflink-locative))
  (:report print-unresolvable-reflink)
  (:documentation """When DOCUMENT encounters an [explicit
  link][@explicit-and-autolinking] such as `[NONEXISTENT][function]`
  that looks like a PAX construct but cannot be resolved, it signals
  and UNRESOLVABLE-REFLINK warning.

  - If the OUTPUT-REFLINK restart is invoked, then no warning is
    printed and the markdown link is left unchanged. MUFFLE-WARNING is
    equivalent to OUTPUT-REFLINK.

  - If the OUTPUT-LABEL restart is invoked, then no warning is printed
    and the markdown link is replaced by its label. For example,
    `[NONEXISTENT][function]` becomes `NONEXISTENT`.

  - If the warning is not handled, then it is printed to
    *ERROR-OUTPUT*. Otherwise, it behaves as OUTPUT-REFLINK."""))

(defun print-unresolvable-reflink (unresolvable-reflink stream)
  (let* ((c unresolvable-reflink)
         (reflink (unresolvable-reflink-string c))
         (name #-cmucl (if (slot-boundp c 'name)
                           (unresolvable-reflink-name c)
                           'not-found)
               #+cmucl (or (ignore-errors
                            (unresolvable-reflink-name c))
                           'not-found))
         (locative #-cmucl (if (slot-boundp c 'locative)
                               (unresolvable-reflink-locative c)
                               nil)
                   #+cmucl (or (ignore-errors
                                (unresolvable-reflink-locative c))
                               'not-found)))
    (cond ((and (not (eq name 'not-found)) locative)
           (format stream "~@<~S cannot be resolved although ~S looks ~
                           like an ~S and ~S like a ~S.~:@>"
                   reflink name '@name locative '@locative))
          ((not (eq name 'not-found))
           (format stream "~@<~S cannot be resolved although ~S looks ~
                           like an ~S.~:@>"
                   reflink name '@name))
          (locative
           (format stream "~@<~S cannot be resolved although ~S looks ~
                           like a ~S.~:@>"
                   reflink locative '@locative))
          (t
           (format stream "~@<~S cannot be resolved.~:@>" reflink)))
    (print-document-context stream)))

(defun signal-unresolvable-reflink (reflink name locative)
  (restart-case
      (let ((string (reflink-to-string reflink)))
        (cond ((and (not (eq name 'not-found)) locative)
               (warn 'unresolvable-reflink
                     :reflink string :name name :locative locative))
              ((not (eq name 'not-found))
               (warn 'unresolvable-reflink :reflink string :name name))
              (locative
               (warn 'unresolvable-reflink :reflink string
                                           :locative locative))
              (t
               (warn 'unresolvable-reflink :reflink string)))
        (pt-get reflink :label))
    (output-label ()
      :report "Output only the label."
      (pt-get reflink :label))
    (output-reflink ()
      :report "Output the whole reflink."
      reflink)))

(defun output-reflink (&optional condition)
  "Invoke the OUTPUT-REFLINK restart. See UNRESOLVABLE-REFLINK."
  (declare (ignore condition))
  (invoke-restart 'output-reflink))

(defun output-label (&optional condition)
  "Invoke the OUTPUT-LABEL restart. See UNRESOLVABLE-REFLINK."
  (declare (ignore condition))
  (invoke-restart 'output-label))

(defun reflink-to-string (tree)
  (with-output-to-string (stream)
    (print-markdown (list tree) stream)))


(defsection @suppressed-links (:title "Suppressed Links")
  """[Autolinking][ @explicit-and-autolinking section] of code (i.e.
  of something like `FOO`) is suppressed if it would create a link
  that was already made within the same docstring. In the following
  docstring, only the first `FOO` will be turned into a link.

      "`FOO` is safe. `FOO` is great."

  However, explicit links (when a DREF::@LOCATIVE was specified or
  found near the @NAME) are never suppressed. In the following, in
  both docstrings, both occurrences `FOO` produce links.

      "`FOO` is safe. [`FOO`][macro] is great."
      "`FOO` is safe. Macro `FOO` is great."

  As an exception, links with [specified][@specified-locative section]
  and [unambiguous][ @unambiguous-unspecificed-locative section]
  locatives to SECTIONs and GLOSSARY-TERMs always produce a link to
  allow their titles to be displayed properly.

  Finally, [autolinking][@explicit-and-autolinking section] to T or
  NIL is suppressed (see *DOCUMENT-LINK-TO-HYPERSPEC*).""")

(defsection @local-references (:title "Local References")
  """To declutter the generated output by reducing the number of
  links, the so-called local references (e.g. references to the very
  definition for which documentation is being generated) are treated
  specially. In the following example, there are local references to
  the function FOO and its arguments, so none of them get turned into
  links:

  ```
  (defun foo (arg1 arg2)
    "FOO takes two arguments: ARG1 and ARG2."
    t)
  ```

  If linking was desired, one could use a @SPECIFIED-LOCATIVE (e.g.
  `[FOO][function]` or `FOO function`), which results in a single
  link. An explicit link with an unspecified locative like `[FOO][]`
  generates links to all references involving the `FOO` symbol except
  the local ones.

  The exact rules for local references are as follows:

  - [linkables-for-autolink-with-unspecified-locative function][docstring]

  - [linkables-for-specified-locative function][docstring]

  - [linkables-for-explicitly-unspecified-locative function][docstring]
  """)

(defun suppressed-link-p (name refs linked-refs)
  (when refs
    (or (member name '(t nil))
        (and
         ;; See if NAME would be linked to any previously linked-to
         ;; reference.
         (loop for ref in refs
                 thereis (find (xref-name ref) linked-refs
                               :test #'xref-name=))
         ;; Replace references to sections and glossary terms with
         ;; their title any number of times.
         (not (and (= (length refs) 1)
                   (typep (resolve (first refs) nil)
                          '(or section glossary-term))))))))


(defsection @linking-to-sections (:title "Linking to Sections")
  "The following variables control how to generate section numbering,
  table of contents and navigation links."
  (*document-link-sections* variable)
  (*document-max-numbering-level* variable)
  (*document-max-table-of-contents-level* variable)
  (*document-text-navigation* variable)
  (*document-fancy-html-navigation* variable))

(defvar *document-link-sections* t
  "When true, HTML anchors are generated before the headings (e.g. of
  sections), which allows the table of contents to contain links and
  also code-like references to sections (like `@FOO-MANUAL`) to be
  translated to links with the section title being the name of the
  link.")

(defvar *document-max-numbering-level* 3
  "A non-negative integer. In their hierarchy, sections on levels less
  than this value get numbered in the format of `3.1.2`. Setting it to
  0 turns numbering off.")

(defvar *document-max-table-of-contents-level* 3
  "An integer that determines the depth of the table of contents.

  - If negative, then no table of contents is generated.

  - If non-negative, and there are multiple top-level sections on a
    page, then they are listed at the top of the page.

  - If positive, then for each top-level section a table of contents
    is printed after its heading, which includes a nested tree of
    section titles whose depth is limited by this value.

  If *DOCUMENT-LINK-SECTIONS* is true, then the tables will link to
  the sections.")

(defvar *document-text-navigation* nil
  "If true, then before each heading a line is printed with links to
  the previous, parent and next section. Needs
  *DOCUMENT-LINK-SECTIONS* to be on to work.")

(defvar *document-fancy-html-navigation* t
  "If true and the output format is HTML, then headings get a
  navigation component that consists of links to the previous, parent,
  next section, a self-link, and a link to the definition in the
  source code if available (see :SOURCE-URI-FN in DOCUMENT). This
  component is normally hidden, it is visible only when the mouse is
  over the heading. Needs *DOCUMENT-LINK-SECTIONS* to be on to work.")

(defun print-toplevel-section-lists (pages)
  (when (<= 0 *document-max-table-of-contents-level*)
    (dolist (page pages)
      (when (page-written-p page)
        (print-toplevel-section-list page)))))

(defun print-toplevel-section-list (page)
  (let ((toplevel-headings (toplevel-headings-on-page page)))
    (when (< 1 (length toplevel-headings))
      (with-temp-output-to-page (stream page)
        (dolist (heading toplevel-headings)
          (print-table-of-contents-entry heading stream))
        (terpri stream)))))

(defun toplevel-headings-on-page (page)
  (loop for heading in *headings*
        when (and (zerop (heading-level heading))
                  (find (locate (heading-object heading))
                        (page-definitions page)
                        :test #'xref=))
          collect heading))

(defun print-table-of-contents (object stream)
  (when (zerop *heading-level*)
    (let ((rest (list-headings object *heading-level*))
          (toc-title-printed nil))
      (flet ((ensure-toc-title ()
               (unless toc-title-printed
                 (heading (+ *heading-level* 1 *heading-offset*) stream)
                 (format stream " Table of Contents~%~%")
                 (setq toc-title-printed t))))
        (loop for heading in (rest rest)
              while (plusp (heading-level heading))
              do (when (<= (heading-level heading)
                           *document-max-table-of-contents-level*)
                   (ensure-toc-title)
                   (print-table-of-contents-entry heading stream))))
      (when toc-title-printed
        (terpri stream)))))

;;; Return the tail of *HEADINGS* from OBJECT at HEADING-LEVEL or NIL.
(defun list-headings (object heading-level)
  ;; OBJECT may be DOCUMENTed multiple times at different depths. See
  ;; MGL-PAX-TEST::TEST-TABLE-OF-CONTENTS-REAPATED-SECTION-DEPTH.
  (member-if (lambda (heading)
               (and (eq (heading-object heading) object)
                    (= (heading-level heading) heading-level)))
             *headings*))

(defun print-table-of-contents-entry (heading stream)
  (let ((object (heading-object heading))
        (title (heading-title heading))
        (level (heading-level heading))
        (number (heading-number heading)))
    (loop repeat (* 4 (1- level))
          do (write-char #\Space stream))
    (let ((link-id (link-to-definition (locate object))))
      (if (and *document-link-sections* link-id)
          (format stream "- [~A~A][~A]" (format-heading-number number) title
                  link-id)
          (format stream "- ~A~A" (format-heading-number number) title)))
    (terpri stream)))

(defun print-section-title (stream section title link-title-to)
  (when *document-link-sections*
    (anchor (locate section) stream)
    (navigation-link section stream)
    (format stream "~A" (fancy-navigation section)))
  (heading (+ *heading-level* *heading-offset*) stream)
  (if (and *document-link-sections*
           (eq *format* :html))
      (print-section-title-link stream section title link-title-to)
      (format stream " ~A~A~%~%" (format-heading-number) title)))

(defun print-section-title-link (stream section title link-title-to)
  (if link-title-to
      ;; Hovering over the section title will show the title of
      ;; LINK-TITLE-TO from the markdown reference link definition.
      (format stream " [~A~A][~A]~%~%"
              (format-heading-number) title
              (link-to-definition link-title-to))
      (format stream " <a href=\"~A\">~A~A</a>~%~%"
              ;; As in PRINT-REFERENCE-BULLET, the object links to a
              ;; separate page when open linking.
              (if *document-open-linking*
                  (finalize-pax-url (dref-to-pax-url (locate section)))
                  (object-to-uri section))
              (format-heading-number) title)))

(defun fancy-navigation (object)
  (if (and *document-fancy-html-navigation*
           *document-link-sections*
           (eq *format* :html))
      (let* ((position (position object *headings* :key #'heading-object))
             (level (heading-level (elt *headings* position)))
             (n (length *headings*))
             (prev (when (and (plusp position)
                              (plusp level))
                     (elt *headings* (1- position))))
             (up (when (plusp level)
                   (find (1- level) (subseq *headings* 0 position)
                         :from-end t :key #'heading-level)))
             (next (when (< position (1- n))
                     (let ((next (elt *headings* (1+ position))))
                       ;; Prev and next stay within the same top-level
                       ;; section.
                       (unless (zerop (heading-level next))
                         next))))
             (source-uri (source-uri (locate object))))
        (format nil "<span class=\"outer-navigation\">~
                    <span class=\"navigation\">~
                    ~@[ [&#8592;][~A]~]~
                    ~@[ [&#8593;][~A]~]~
                    ~@[ [&#8594;][~A]~] ~
                    [&#8634;][~A]~
                    ~A~
                    </span></span>~%"
                (when prev
                  (link-to-definition
                   (locate (heading-object prev))))
                (when up
                  (link-to-definition
                   (locate (heading-object up))))
                (when next
                  (link-to-definition
                   (locate (heading-object next))))
                (link-to-definition (locate object))
                (if source-uri
                    (format nil " <a href=~S>&#955;</a>" source-uri)
                    "")))
      ""))

(defun write-navigation-link (heading stream)
  (let ((link-id (link-to-definition (locate (heading-object heading)))))
    (format stream "[~A][~A]" (heading-title heading) link-id)))

(defun navigation-link (object stream)
  (when (and *document-link-sections* *document-text-navigation*)
    (let* ((position (position object *headings* :key #'heading-object))
           (level (heading-level (elt *headings* position)))
           (n (length *headings*))
           (writtenp nil))
      (when (< position (1- n))
        (format stream "Next: ")
        (write-navigation-link (elt *headings* (1+ position)) stream)
        (setq writtenp t))
      (when (plusp position)
        (when writtenp
          (format stream " "))
        (format stream "Prev: ")
        (write-navigation-link (elt *headings* (1- position)) stream)
        (setq writtenp t))
      (when (plusp level)
        (when writtenp
          (format stream " "))
        (let ((parent (find (1- level) (subseq *headings* 0 position)
                            :from-end t :key #'heading-level)))
          (format stream "Up: ")
          (write-navigation-link parent stream))
        (setq writtenp t))
      (when writtenp
        (format stream "~%~%")))))

(defun format-heading-number (&optional (heading-number *heading-number*))
  (format nil "~@[~{~D~^.~} ~]"
          (when (<= (length heading-number) *document-max-numbering-level*)
            heading-number)))


(defsection @miscellaneous-documentation-printer-variables
    (:title "Miscellaneous Variables")
  (*document-url-versions* variable)
  (*document-min-link-hash-length* variable)
  (*document-mark-up-signatures* variable)
  (*document-base-url* variable))

(defvar *document-url-versions* '(2 1)
  """A list of versions of PAX \URL formats to support in the
  generated documentation. The first in the list is used to generate
  links.

  PAX emits HTML anchors before the documentation of SECTIONs
  (see @LINKING-TO-SECTIONS) and other things (see @LINKING-TO-CODE).
  For the function `FOO`, in the current version (version 2), the
  anchor is `<a id="MGL-PAX:FOO%20FUNCTION">`, and its \URL will end
  with `\\#MGL-PAX:FOO%20FUNCTION`.

  _Note that to make the \URL independent of whether a symbol is
  [internal or external][find-symbol] to their SYMBOL-PACKAGE, single
  colon is printed where a double colon would be expected. Package and
  symbol names are both printed verbatim except for escaping colons
  and spaces with a backslash. For exported symbols with no funny
  characters, this coincides with how PRIN1 would print the symbol,
  while having the benefit of making the \URL independent of the Lisp
  printer's escaping strategy and producing human-readable output for
  mixed-case symbols. No such promises are made for non-ASCII
  characters, and their \URLs may change in future versions. Locatives
  are printed with PRIN1._

  Version 1 is based on the more strict HTML4 standard and the id of
  `FOO` is `"x-28MGL-PAX-3A-3AFOO-20FUNCTION-29"`. This is supported
  by Github-flavoured Markdown. Version 2 has minimal clutter and is
  obviously preferred. However, in order not to break external links,
  by default, both anchors are generated.

  Let's understand the generated Markdown.

  ```
  (defun foo (x))

  (document #'foo :format :markdown)
  => ("<a id=\"x-28MGL-PAX-3AFOO-20FUNCTION-29\"></a>
  <a id=\"MGL-PAX:FOO%20FUNCTION\"></a>
  
  - [function] **FOO** *X*
  ")

  (let ((*document-url-versions* '(1)))
    (document #'foo :format :markdown))
  => ("<a id=\"x-28MGL-PAX-3AFOO-20FUNCTION-29\"></a>

  - [function] **FOO** *X*
  ")
  ```
  """)

(defun anchor (dref stream)
  (let ((v1 (member 1 *document-url-versions*))
        (v2 (member 2 *document-url-versions*)))
    (when (or v1 v2)
      (when v1
        (format stream "<a id=~S></a>~%"
                (html4-safe-name (dref-to-anchor-v1 dref))))
      (when v2
        (format stream "<a id=~S></a>~%"
                (urlencode (dref-to-anchor dref))))
      (terpri stream))))

(defun anchor-id (dref)
  (if (= (first *document-url-versions*) 1)
      (html4-safe-name (dref-to-anchor-v1 dref))
      (urlencode (dref-to-anchor dref))))

;;; Return the unescaped name of the HTML anchor for DREF. See
;;; URLENCODE.
(defun dref-to-anchor (dref)
  (with-standard-io-syntax*
    ;; The locative may not be readable (e.g. methods with EQL
    ;; specializers with unreadable stuff).
    (let ((*print-readably* nil))
      (format nil "~A ~S" (name-to-url-part (dref-name dref))
              (dref-locative dref)))))

(defun dref-to-anchor-v1 (dref)
  (with-standard-io-syntax*
    (let ((*print-readably* nil))
      (format nil "(~A ~S)"
              (name-to-url-part (dref-name dref)) (dref-locative dref)))))

(defun dref-to-pax-url (dref)
  (urlencode
   (with-standard-io-syntax*
     (let ((*print-readably* nil))
       (format nil "pax:~A ~S"
               (name-to-url-part (dref-name dref)) (dref-locative dref))))))

(defun name-to-ambiguous-pax-url (name)
  (with-standard-io-syntax*
    (format nil "pax:~A" (name-to-url-part name))))

;;; If NAME is a symbol, then print it almost as PRIN1 would with
;;; *PACKAGE* were the CL package. Differences:
;;;
;;; - For symbols in other packages, a single #\: is printed even if
;;;   it is an internal symbol.
;;;
;;; - Package and symbol names are printed without the || syntax but
;;;   #\: and #\Space are escaped with backslashes.
(defun name-to-url-part (name)
  (if (symbolp name)
      (let* ((package (symbol-package name))
             (name (symbol-name name))
             (name-url (print-name-for-url name))
             (cl-package (find-package :common-lisp))
             (keyword-package (find-package :keyword)))
        (cond
          ((eq package cl-package)
           (format nil "~A" name-url))
          ((eq package keyword-package)
           (format nil ":~A" name-url))
          (t
           ;; Note the single : character.
           (format nil "~A:~A" (print-name-for-url (package-name package))
                   name-url))))
      (prin1-to-string name)))

;;; Escape #\: and #\Space with a backslash.
(defun print-name-for-url (string)
  (with-output-to-string (s)
    (loop for char across string
          do (when (or (eql char #\:) (eql char #\Space))
               (write-char #\\ s))
             (write-char char s))))


(defvar *document-min-link-hash-length* 4
  "Recall that markdown reference style links (like `[label][id]`) are
  used for linking to sections and code. It is desirable to have ids
  that are short to maintain legibility of the generated markdown, but
  also stable to reduce the spurious diffs in the generated
  documentation, which can be a pain in a version control system.

  Clearly, there is a tradeoff here. This variable controls how many
  characters of the MD5 sum of the full link id (the reference as a
  string) are retained. If collisions are found due to the low number
  of characters, then the length of the hash of the colliding
  reference is increased.

  This variable has no effect on the HTML generated from markdown, but
  it can make markdown output more readable.")

(defun hash-link (string detect-collision-fn
                  &key (min-n-chars *document-min-link-hash-length*))
  (let ((hex (byte-array-to-hex-string (md5:md5sum-string string))))
    (loop for i upfrom min-n-chars below 32
          do (let ((hash (subseq hex 0 (min 32 i))))
               (unless (funcall detect-collision-fn hash)
                 (return-from hash-link hash))))
    (assert nil () "MD5 collision detected.")))

(defun byte-array-to-hex-string (byte-array)
  (declare (type (vector (unsigned-byte 8)) byte-array))
  (let* ((n (length byte-array))
         (s (make-string (* 2 n) :element-type 'base-char))
         (hex-digits "0123456789abcdef"))
    (dotimes (i n)
      (let ((byte (aref byte-array i)))
        (multiple-value-bind (div rem) (floor byte 16)
          (setf (aref s (* 2 i)) (aref hex-digits div))
          (setf (aref s (1+ (* 2 i))) (aref hex-digits rem)))))
    s))


(defvar *document-mark-up-signatures* t
  "When true, some things such as function names and arglists are
  rendered as bold and italic. In :HTML output, locative types become
  links to sources (if :SOURCE-URI-FN is provided, see DOCUMENT), and
  the symbol becomes a self-link for your permalinking pleasure.

  For example, a reference is rendered in markdown roughly as:

      - [function] foo x y

  With this option on, the above becomes:

      - [function] **foo** *x y*

  Also, in HTML `**foo**` will be a link to that very entry and
  `[function]` may turn into a link to sources.")

;;; PRINT REFERENCE to STREAM as:
;;;
;;;     - [locative-type] name
;;;
;;; When generating HTML, link NAME to the anchor of REFERENCE.
(defun/autoloaded print-reference-bullet (reference stream &key name)
  (let ((locative-type (string-downcase
                        (xref-locative-type reference)))
        (name (or name (prin1-to-string* (xref-name reference)))))
    (if *document-mark-up-signatures*
        ;; Insert self links in HTML.
        (let ((locative-type (escape-markdown locative-type))
              (name (escape-markdown name)))
          (if (eq *format* :html)
              (let ((source-uri (source-uri reference)))
                ;; When *DOCUMENT-OPEN-LINKING* (this includes (EQ
                ;; *HTML-SUBFORMAT* :W3M)), the name is linked to
                ;; the a pax: URL (which opens a separate page), else
                ;; they are self-links.
                (cond ((eq *html-subformat* :w3m)
                       (assert *document-open-linking*)
                       (format stream "- **\\[~A]** [~A](~A)"
                               locative-type name
                               (finalize-pax-url (dref-to-pax-url reference))))
                      (*document-open-linking*
                       (format stream
                               "- <span class=reference-bullet>~
                            <span class=reference>~
                            <span class=\"locative-type\">~
                            ~@[<a href=\"~A\" title=\"Edit in Emacs\">~]~
                            \\[~A]~:[~;</a>~]~
                            </span> ~
                            <span class=\"reference-object\">[~A](~A)</span>~
                            </span>"
                               source-uri locative-type source-uri name
                               (finalize-pax-url
                                (dref-to-pax-url reference))))
                      (t
                       (format stream
                               "- <span class=reference-bullet>~
                            <span class=reference>~
                            <span class=\"locative-type\">~
                            ~@[<a href=\"~A\">~]\\[~A]~:[~;</a>~]~
                            </span> ~
                            <span class=\"reference-object\">[~A](#~A)</span>~
                            </span>"
                               source-uri locative-type source-uri name
                               (urlencode (dref-to-anchor reference))))))
              (format stream "- [~A] ~A" locative-type (bold name nil))))
        (format stream "- [~A] ~A" locative-type name))))

(defun print-end-bullet (stream)
  (cond ((eq *html-subformat* :w3m)
         (format stream "~%"))
        ((eq *format* :html)
         ;; end "reference-bullet" span
         (format stream "</span>~%"))
        (t
         (format stream "~%"))))

(defun source-uri (reference)
  (let ((fn (page-source-uri-fn *page*)))
    (if fn
        (funcall fn reference)
        nil)))

(defvar *print-arglist-key* nil)

(defun print-arglist (arglist stream)
  (let* ((string (if (stringp arglist)
                     ;; must be escaped markdown
                     arglist
                     (arglist-to-markdown arglist)))
         (string (if *print-arglist-key*
                     (funcall *print-arglist-key* string)
                     string)))
    (if *document-mark-up-signatures*
        (if (and (eq *format* :html)
                 (not (eq *html-subformat* :w3m)))
            (format stream "<span class=\"locative-args\">~A</span>" string)
            (italic string stream))
        (format stream "~A" string))))

(defun/autoloaded prin1-to-markdown (object &key (escape-inline t)
                                            (escape-html t) (escape-block t))
  "Like PRIN1-TO-STRING, but bind *PRINT-CASE* depending on
  *DOCUMENT-DOWNCASE-UPPERCASE-CODE* and *FORMAT*, and
  ESCAPE-MARKDOWN."
  (escape-markdown (prin1-to-string* object)
                   :escape-inline escape-inline :escape-html escape-html
                   :escape-block escape-block))

;;; Print a lambda list of any kind (ordinary, macro, etc) or a method
;;; arglist to a string. Arg names are printed without the package
;;; prefix, default values with the package prefix. For specializers
;;; in method arglists to be distinguishable from nested macro lambda
;;; lists, they look like
;;;
;;; (:method (x string) y (z (eql 7)) ...)
;;;
;;; else the specializers are printed without package information.
(defun arglist-to-markdown (arglist)
  (with-output-to-string (out)
    (multiple-value-bind (methodp arglist)
        (if (eq (first arglist) :method)
            (values t (rest arglist))
            (values nil arglist))
      (let ((*print-pretty* t)
            (*print-right-margin* 80))
        (labels
            ((resolve* (object)
               (if (and *document-mark-up-signatures*
                        ;; KLUDGE: Github has trouble displaying things
                        ;; like '`*package*`, so disable this.
                        (eq *format* :html))
                   (codify-and-link (prin1-to-markdown object))
                   (prin1-to-markdown object)))
             (print-arg (arg level)
               (declare (special *nesting-possible-p*))
               (cond ((member arg '(&key &optional &rest &body))
                      (when (member arg '(&key &optional))
                        (setq *nesting-possible-p* nil))
                      (format out "~A" (prin1-to-markdown arg)))
                     ((symbolp arg)
                      (format out "~A"
                              (escape-markdown
                               (maybe-downcase-all-uppercase-code
                                (symbol-name arg)))))
                     ((atom arg)
                      (format out "~A" (prin1-to-markdown arg)))
                     (*nesting-possible-p*
                      (print-arglist arg (1+ level)))
                     (t
                      (if (symbolp (first arg))
                          (format out "(~A~{ ~A~})"
                                  (escape-markdown
                                   (maybe-downcase-all-uppercase-code
                                    (symbol-name (first arg))))
                                  (mapcar #'resolve* (rest arg)))
                          (format out "~A" (prin1-to-markdown arg))))))
             (print-arglist (arglist level)
               (let ((*nesting-possible-p* (not methodp)))
                 (declare (special *nesting-possible-p*))
                 (unless (= level 0)
                   (format out "("))
                 (loop for i upfrom 0
                       for rest on arglist
                       do (unless (zerop i)
                            (format out " "))
                          (print-arg (car rest) level)
                          ;; Handle (&WHOLE FORM NAME . ARGS) and similar.
                          (unless (listp (cdr rest))
                            (format out " . ")
                            (print-arg (cdr rest) level)))
                 (unless (= level 0)
                   (format out ")")))))
          (print-arglist arglist 0))))))

(defun map-dotted (fn list*)
  (if (listp list*)
      (loop for rest on list*
            do (funcall fn (car rest) nil)
               (unless (listp (cdr rest))
                 (funcall (cdr rest) t)))
      (funcall fn list* t)))

(defun mapcan-dotted (fn list)
  (let ((results ()))
    (map-dotted (lambda (x dotp)
                  (push (funcall fn x dotp) results))
                list)
    (apply #'append (reverse results))))


(defsection @package-and-readtable (:title "Package and Readtable")
  "While generating documentation, symbols may be read (e.g. from
  docstrings) and printed. What values of *PACKAGE* and *READTABLE*
  are used is determined separately for each definition being
  documented.

  - If the values of *PACKAGE* and *READTABLE* in effect at the time
    of definition were captured (e.g. by DEFINE-LOCATIVE-TYPE and
    DEFSECTION), then they are used.

  - Else, if the definition has a @HOME-SECTION (see below), then the
    home section's SECTION-PACKAGE and SECTION-READTABLE are used.

  - Else, if the definition has an argument list, then the package of
    the first argument that's not external in any package is used.

  - Else, if the definition is DREF::@NAMEd by a symbol, then its
    SYMBOL-PACKAGE is used, and *READTABLE* is set to the standard
    readtable `(NAMED-READTABLES:FIND-READTABLE :COMMON-LISP)`.

  - Else, *PACKAGE* is set to the `CL-USER` package and *READTABLE* to
    the standard readtable.

  The values thus determined come into effect after the name itself is
  printed, for printing of the arglist and the docstring.

      CL-USER> (pax:document #'foo)
      - [function] FOO <!> X Y &KEY (ERRORP T)

          Do something with X and Y.

  In the above, the `<!>` marks the place where *PACKAGE* and
  *READTABLE* are bound."
  (@home-section section)
  (*document-normalize-packages* variable))

(defsection @home-section (:title "Home Section")
  "[home-section function][docstring]")

(defun guess-package-and-readtable (reference arglist)
  (let ((home-section (first (find-parent-sections reference))))
    (if home-section
        (values (section-package home-section)
                (section-readtable home-section))
        (values (or (guess-package-from-arglist arglist)
                    (and (symbolp (xref-name reference))
                         (symbol-package (xref-name reference)))
                    (find-package :cl-user))
                named-readtables::*standard-readtable*))))

;;; Unexported argument names are highly informative about *PACKAGE*
;;; at read time. No one ever uses fully-qualified internal symbols
;;; from another package for arguments, right?
(defun guess-package-from-arglist (arglist)
  (let ((args (or (ignore-errors (dref::function-arg-names arglist))
                  (ignore-errors (dref::macro-arg-names arglist)))))
    (dolist (arg args)
      (unless (external-symbol-in-any-package-p arg)
        (return (symbol-package arg))))))

(defvar *document-normalize-packages* t
  "Whether to print `[in package <package-name>]` in the documentation
  when the package changes.")


(defvar *document-base-url* nil
  """When *DOCUMENT-BASE-URL* is non-NIL, this is prepended to all
  Markdown relative URLs. It must be a valid URL without no query and
  fragment parts (that is, "http://lisp.org/doc/" but not
  "http://lisp.org/doc?a=1" or "http://lisp.org/doc#fragment").
  Note that intra-page links using only URL fragments (e.g. and
  explicit HTML links (e.g. `<a href="...">`) in Markdown are not
  affected.""")

(defun add-base-url (parse-tree)
  (if *document-base-url*
      (flet ((translate (parent tree)
               (declare (ignore parent))
               (ecase (first tree)
                 ((:explicit-link :reference)
                  (let ((source (pt-get tree :source)))
                    (assert source)
                    (unless (urlp source)
                      (setf (pt-get tree :source)
                            (append-to-url *document-base-url* source)))
                    tree)))))
        (multiple-value-bind (scheme authority path query fragment)
            (parse-url *document-base-url*)
          (declare (ignore path))
          (unless (and scheme authority (null query) (null fragment))
            (error "~@<~S should have scheme and authority ~
                   but no query and fragment parts.~:@>"
                   '*document-base-url*)))
        (map-markdown-parse-tree '(:explicit-link :reference)
                                 '() nil #'translate parse-tree))
      parse-tree))


(defsection @overview-of-escaping (:title "Overview of Escaping")
  """Let's recap how escaping @CODIFICATION,
  [downcasing][\*document-downcase-uppercase-code*], and
  @LINKING-TO-CODE works.

  - One backslash in front of a @WORD turns codification off. Use this
    to prevent codification words such as \DOCUMENT, which is all
    uppercase hence @CODIFIABLE, and it names an exported symbol hence
    it is @INTERESTING.

  - One backslash right after an opening backtick turns autolinking
    off.

  - Two backslashes right after an opening backtick turns autolinking
    and downcasing off. Use this for things that are not Lisp code but
    which need to be in a monospace font."""
  """In the following examples capital C/D/A letters mark the presence,
  and a/b/c the absence of codification, downcasing, and autolinking
  assuming all these features are enabled by
  *DOCUMENT-UPPERCASE-IS-CODE*, *DOCUMENT-DOWNCASE-UPPERCASE-CODE*,
  and *DOCUMENT-LINK-CODE*.

      DOCUMENT                => [`document`][1234]    (CDA)
      \DOCUMENT               => DOCUMENT              (cda)
      `\DOCUMENT`             => `document`            (CDa)
      `\\DOCUMENT`            => `DOCUMENT`            (CdA)
      [DOCUMENT][]            => [`document`][1234]    (CDA)
      [\DOCUMENT][]           => [DOCUMENT][1234]      (cdA)
      [`\DOCUMENT`][]         => [`document`][1234]    (CDA) *
      [`\\DOCUMENT`][]        => [`DOCUMENT`][1234]    (CdA)
      [DOCUMENT][dislocated]  => `document`            (CDa)

  Note that in the example marked with `\*`, the single backslash,
  that would normally turn autolinking off, is ignored because it is
  in an explicit link.""")


(defsection @document-implementation-notes
    (:title "Documentation Generation Implementation Notes")
  """Documentation Generation is supported on ABCL, AllegroCL, CLISP,
  \CCL, CMUCL, ECL and SBCL, but their outputs may differ due to the
  lack of some introspective capability. SBCL generates complete
  output. see ARGLIST, DOCSTRING and SOURCE-LOCATION for
  implementation notes.

  In addition, CLISP does not support the ambiguous case of @PAX-URLS
  for @BROWSING-LIVE-DOCUMENTATION because the current implementation
  relies on Swank to list definitions of symbols (as VARIABLE,
  [FUNCTION][locative], etc), and that simply doesn't work.""")


(defun pax-std-env (fn)
  ;; FIXME: Add all others too.
  (let ((*document-downcase-uppercase-code* nil)
        (*transcribe-check-consistency* (featurep :sbcl)))
    (handler-bind ((warning #'muffle-warning))
      (unwind-protect
           (funcall fn)
        (unintern '@example-section :pax)))))
