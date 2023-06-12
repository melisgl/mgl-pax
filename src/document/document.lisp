(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @generating-documentation (:title "Generating Documentation")
  (document function)
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
  (@document-implementation-notes section))

;;; A PAGE is basically a single markdown or html file to where the
;;; documentation of some references is written. See the DOCUMENT
;;; function.
;;;
;;; Documentation starts out being sent to a certain stream, but the
;;; output is redirected to different stream if it is for a reference
;;; on the current page (see REACHABLE-CANONICAL-REFERENCES,
;;; PAGE-REFERENCES). This stream - to which the temporary markdown
;;; output written - is given by PAGE-TEMP-STREAM-SPEC, that's a
;;; stream spec (see WITH-OPEN-STREAM-SPEC) to allow the temporary
;;; stream to
;;;
;;; - be created lazily so that no stray files are left around and
;;;   only a small number of fds are needed even for a huge project;
;;;
;;; - be opened multiple times (which is not a given for string
;;;   streams).
;;;
;;; So, output is written in markdown format to PAGE-TEMP-STREAM-SPEC,
;;; but before we are done, it is converted to the requested output
;;; format, and HEADER-FN, FOOTER-FN are called to write arbitrary
;;; leading and trailing content to the final stream.
;;;
;;; Finally, URI-FRAGMENT is a string such as "doc/manual.html" that
;;; specifies where the page will be deployed on a webserver. It
;;; defines how links between pages will look. If it's not specified
;;; and OUTPUT refers to a file, then it defaults to the name of the
;;; file. If URI-FRAGMENT is NIL, then no links will be made to or
;;; from that page.
(defstruct page
  references
  (used-links (make-hash-table) :type hash-table)
  temp-stream-spec
  final-stream-spec
  uri-fragment
  header-fn
  footer-fn
  source-uri-fn)

;;; The current page where output is being sent.
(defvar *page* nil)

;;; A LINK (a possible link target, really) is a canonical REFERENCE
;;; that resides on PAGE (note that the same REFERENCE may be written
;;; to multiple pages).
(defstruct link
  (reference nil :type reference)
  ;; - STRING pages denote URLs. This is used to link to the
  ;;   hyperspec.
  ;;
  ;; - LINK pages are aliases: the LINK with the reference (PRINT
  ;;   (CLHS FUNCTION)) is the LINK-PAGE of a LINK whose reference is
  ;;   (PRINT FUNCTION).
  ;;
  ;; - NULL pages are to support "pax:" URLs for
  ;;   *DOCUMENT-OPEN-LINKING*.
  (page nil :type (or page string link null)))

;;; This map caches LINKS-OF within a single DOCUMENT call. If not
;;; *DOCUMENT-OPEN-LINKING*, then only references on *CLOSED-OLMAP*.
(defvar *open-olmap*)

;;; This object-to-links map contains the
;;; REACHABLE-CANONICAL-REFERENCES from the DOCUMENTABLE argument of
;;; DOCUMENT. It is populated at the beginning of a DOCUMENT call and
;;; never changed. Not used when *DOCUMENT-OPEN-LINKING*.
(defvar *closed-olmap*)

;;; Whether definitions present in the running Lisp but in
;;; *CLOSED-OLMAP* can be linked with magic "pax:" URLs. This is to
;;; support generating documentation for Emacs.
(defvar *document-open-linking* nil)

;;; Return all possible LINKs for OBJECT in the order of linking
;;; preference when there are duplicate LINK-REFERENCEs (e.g CLHS
;;; aliases).
(defgeneric links-of (object)
  (:method (object)
    (let* ((live-definitions (definitions-of object))
           ;; Gather all different @OBJECTs. For example, if OBJECT is
           ;; MGL-PAX, then this is '("mgl-pax" "MGL-PAX"), where one
           ;; is for the ASDF:SYSTEM, the other is for the PACKAGE.
           (objects (remove-duplicates (cons object
                                             (mapcar #'reference-object
                                                     live-definitions))
                                       :test #'equal))
           (closed-links (loop for object in objects
                               append (closed-links object)))
           (external-links (loop for object in objects
                                 append (external-links object)))
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
                                       (make-link :reference definition
                                                  :page nil)))
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
                              :key #'link-reference :test #'reference=))))
  (:method ((reference reference))
    (let* ((object (reference-object reference))
           (links (links-of object)))
      (loop for link in links
            when (reference= reference (link-reference link))
              collect link))))

(defun closed-links (object)
  (when (boundp '*closed-olmap*)
    (gethash object *closed-olmap*)))

(defun find-among-links (reference links)
  (find reference links :key #'link-reference :test #'reference=))

;;; FIXME: CLRHASH if too large?
(defun ensure-in-open-olmap (object)
  (declare (optimize speed))
  (let ((open-olmap *open-olmap*))
    (multiple-value-bind (links found) (gethash object open-olmap)
      (if found
          links
          (setf (gethash object open-olmap) (links-of object))))))

;;; Iterate over LINKs with CANONICAL-REFERENCEs whose
;;; REFERENCE-OBJECT is EQUAL to OBJECT.
(defmacro do-links ((link object) &body body)
  (alexandria:once-only (object)
    `(dolist (,link (ensure-in-open-olmap ,object))
       ,@body)))

(defun add-link (olmap link &optional object)
  (let* ((reference (link-reference link))
         (object (or object (reference-object reference))))
    (push link (gethash object olmap))))

;;; A list of references with special rules for linking (see
;;; @LOCAL-REFERENCES). The reference being DOCUMENTed is always on
;;; this list. Arguments are typically also are. Bound by
;;; WITH-LOCAL-REFERENCES.
(defvar *local-references*)

;;; Add a LINK to *OBJECT-TO-LINKS-MAPS* for each reference in
;;; PAGE-REFERENCES of PAGES.
(defmacro with-pages ((pages) &body body)
  `(let ((*open-olmap* (make-hash-table :test #'equal))
         (*closed-olmap* (make-hash-table :test #'equal))
         (*local-references* ())
         (*link-to-id* (make-hash-table))
         (*id-to-link* (make-hash-table :test #'equal)))
     (initialize-closed-olmap ,pages)
     (locally ,@body)))

(defun initialize-closed-olmap (pages)
  (dolist (page pages)
    (dolist (reference (page-references page))
      (let ((object (reference-object reference)))
        (unless (find reference (gethash object *closed-olmap*)
                      :key #'link-reference :test #'reference=)
          (add-link *closed-olmap* (make-link :reference reference
                                              :page page)))))))


;;;; Querying global and local references

(defun find-link (reference)
  (let ((object (reference-object reference))
        (locative (reference-locative reference)))
    (do-links (link object)
      (when (equal locative (reference-locative (link-reference link)))
        (return link)))))

;;; Return a list of all REFERENCES whose REFERENCE-OBJECT matches
;;; OBJECT, that is, with OBJECT as their REFERENCE-OBJECT they would
;;; resolve to the same thing.
;;;
;;; If LOCAL is NIL, only global references are considered for
;;; matching. If LOCAL is :EXCLUDE, then only those global references
;;; which are not local references are considered. If LOCAL is
;;; :INCLUDE, then both global and local references are considered.
(defun references-to-object (object &key local)
  (let ((global-refs (global-references-to-object object)))
    (if local
        (let ((local-refs (local-references-to-object object)))
          (if (eq local :include)
              (nconc global-refs local-refs)
              (set-difference global-refs local-refs :test #'reference=)))
        global-refs)))

(defun global-references-to-object (object)
  (let ((result ()))
    (do-links (link object)
      (let ((ref (link-reference link)))
        (push ref result)))
    result))

(defun has-global-reference-p (object)
  (do-links (link object)
    (return link)))

(defun global-reference-p (reference)
  (let ((object (reference-object reference)))
    (do-links (link object)
      (return link))))

(defun local-references-to-object (object)
  (remove-if-not (lambda (ref)
                   (reference-object= object ref))
                 *local-references*))

(defun has-local-reference-p (object)
  (find object *local-references* :test #'reference-object=))

(defun has-reference-p (object)
  (or (has-global-reference-p object)
      (has-local-reference-p object)))


;;; Follow LINK-PAGE if it is a LINK, and return the LINK it
;;; eventually points to.
(defun unaliased-link (link)
  (declare (type link link))
  (if (link-p (link-page link))
      (unaliased-link (link-page link))
      link))

;;; For the link to REFERENCE, increment the link counter for the
;;; current page and return the link id.
(defun link-to-reference (reference)
  (let ((link (unaliased-link (find-link reference))))
    (assert link)
    (when (let ((page (link-page link)))
            (or (null page)
                (eq *page* page)
                (stringp page)
                (and (page-uri-fragment *page*)
                     (page-uri-fragment page))))
      (setf (gethash link (page-used-links *page*)) t)
      (format nil "~A" (ensure-link-id link)))))

;;; Link ids are short hashes, and they go into markdown reference
;;; links. Due to possible collisions, they are context-dependent, so
;;; to keep LINKs immutable, ids are in this hash table.
(defvar *link-to-id*)
;;; A LINK-ID to LINK hash table for MD5 collision detection.
(defvar *id-to-link*)

(defun link-id (link)
  (gethash link *link-to-id*))

(defun ensure-link-id (link)
  (or (gethash link *link-to-id*)
      (let ((id (hash-link (reference-to-anchor (link-reference link))
                           #'find-link-by-id)))
        (setf (gethash id *id-to-link*) link)
        (setf (gethash link *link-to-id*) id))))

(defun find-link-by-id (id)
  (gethash id *id-to-link*))

(defun reference-page (reference)
  (let ((link (find-link reference)))
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
  (when (eq (reference-locative-type reference) 'clhs)
    (let* ((object (reference-object reference))
           (locative (reference-locative-args reference))
           (locative-type (locative-type locative)))
      ;; This parallels LOCATE-OBJECT (METHOD () (T (EQL CLHS) T)).
      (cond ((eq locative-type 'glossary-term)
             (find-hyperspec-glossary-entry-url object
                                                *document-hyperspec-root*))
            ((eq locative-type 'section)
             (or (find-hyperspec-issue-url object *document-hyperspec-root*)
                 (find-hyperspec-section-url object
                                             *document-hyperspec-root*)))
            (t
             (find-hyperspec-definition-url object locative
                                            *document-hyperspec-root*))))))

(defun clhs-definitions (object)
  (mapcar (lambda (locative)
            (if locative
                (make-reference object `(clhs ,locative))
                ;; The Hyperspec disambiguate page
                (make-reference object 'clhs)))
          (hyperspec-locatives-for-object object)))

(defun clhs-documentations (object)
  (let ((*clhs-substring-match* nil))
    (ensure-list (or (locate object '(clhs glossary-term) :errorp nil)
                     (locate object '(clhs section) :errorp nil)))))

(defun make-clhs-alias-link (link)
  (let* ((reference (link-reference link))
         (object (reference-object reference))
         (locative (reference-locative reference)))
    (assert (eq (locative-type locative) 'clhs))
    (when (locative-args locative)
      (make-link :reference (canonical-reference
                             (make-reference object (locative-args locative)))
                 :page link))))

(defun clhs-definition-links (object)
  (when *document-link-to-hyperspec*
    (loop for reference in (clhs-definitions object)
          append (let ((clhs-link (make-link
                                   :reference reference
                                   :page (find-clhs-url reference))))
                   (let ((alias (make-clhs-alias-link clhs-link)))
                     (if alias
                         (list alias clhs-link)
                         (list clhs-link)))))))

(defun clhs-documentation-links (object)
  (loop for reference in (clhs-documentations object)
        collect (make-link :reference reference
                           :page (find-clhs-url reference))))

(defun clhs-links (object)
  (append (clhs-definition-links object)
          (clhs-documentation-links object)))

(defun filter-clhs-references (refs)
  (remove 'clhs refs :key #'reference-locative-type))


;;;; Beginnings of abstracting CLHS to external references. This could
;;;; serve the needs of e.g. linking to the MOP.

(defun external-locative-p (locative)
  (eq (locative-type locative) 'clhs))

(defun external-reference-p (reference)
  (external-locative-p (reference-locative reference)))

(defun external-reference-url (reference)
  (find-clhs-url (resolve reference)))

(defun external-links (object)
  (clhs-links object))

(defun filter-external-references (references)
  (filter-clhs-references references))

(defun find-external-link (reference)
  (let ((*document-link-to-hyperspec* t))
    (dolist (link (external-links (reference-object reference)))
      (when (reference= (link-reference link) reference)
        (return link)))))


(defvar *pages-created*)

(defmacro with-tracking-pages-created (() &body body)
  `(let ((*pages-created* ()))
     ,@body))

(defmacro do-pages-created ((page) &body body)
  `(dolist (,page (reverse *pages-created*))
     ,@body))

(defun mark-page-created (page)
  (pushnew page *pages-created*))

(defmacro with-temp-input-from-page ((stream page) &body body)
  `(with-open-stream-spec (,stream (page-temp-stream-spec ,page))
     ,@body))

(defmacro with-temp-output-to-page ((stream page) &body body)
  (alexandria:once-only (page)
    (alexandria:with-unique-names (stream-spec)
      `(flet ((foo (,stream)
                ,@body))
         (cond (*table-of-contents-stream*
                (foo (make-broadcast-stream)))
               ((or (null ,page) (stringp ,page) (eq ,page *page*))
                (foo ,stream))
               (t
                (let ((,stream-spec (page-temp-stream-spec ,page)))
                  (with-open-stream-spec (,stream ,stream-spec
                                          :direction :output)
                    (let ((*page* ,page))
                      (mark-page-created ,page)
                      (foo ,stream))))))))))

(defmacro with-final-output-to-page ((stream page) &body body)
  `(with-open-stream-spec (,stream (page-final-stream-spec ,page)
                           :direction :output)
     ;; This allows HEADER-FN and FOOTER-FN to support linking
     ;; references with LINK-TO-URI.
     (let ((*page* ,page))
       ,@body)))

(declaim (special *table-of-contents-stream*))
(declaim (special *headings*))
(declaim (special *heading-offset*))

(defmacro with-headings ((object) &body body)
  (alexandria:once-only (object)
    `(let ((*headings* (collect-headings ,object))
           (*heading-offset* (heading-offset ,object)))
       ,@body)))

(defmacro with-format ((format) &body body)
  (alexandria:with-gensyms (fn)
    `(flet ((,fn ()
              ,@body))
       (call-with-format ,format #',fn))))

(defvar *html-subformat* nil)
(defvar *document-tight* nil)

(defun/autoloaded document (documentable &key (stream t) pages (format :plain))
  """Write DOCUMENTABLE in FORMAT to STREAM diverting some output to PAGES.
  FORMAT can be anything [3BMD][3bmd] supports, which is currently
  :MARKDOWN, :HTML and :PLAIN. STREAM may be a [STREAM][type] object,
  T or NIL as with CL:FORMAT.

  Most often, this function is called on SECTION objects as in
  `(DOCUMENT @PAX-MANUAL)`, but it supports all kinds of objects for
  which DOCUMENT-OBJECT is defined. To look up the documentation of
  the DOCUMENT function itself:

      (document #'document)

  The same with fancy markup:

      (document #'document :format :markdown)

  To generate the documentation for separate libraries with automatic
  cross-links:

      (document (list @cube-manual @mat-manual) :format :markdown)

  See @DOCUMENTATION-UTILITIES for more.

  Note that not only first-class objects can have documentation:

      (document (locate 'foo 'type))

  See @LOCATIVES-AND-REFERENCES for more.

  There are quite a few special variables that affect how output is
  generated, see @CODIFICATION, @LINKING-TO-CODE,
  @LINKING-TO-SECTIONS, and
  @MISCELLANEOUS-DOCUMENTATION-PRINTER-VARIABLES.

  If PAGES is NIL and STREAM is NIL, then DOCUMENT returns the output
  as a string. If PAGES is NIL but STREAM is not, then DOCUMENT
  returns NIL. The rest of this description deals with how to generate
  multiple pages.

  ##### Pages

  [page-specs-to-pages function][docstring]

  ##### Packages

  While generating the documentation, symbols may be read (e.g. from
  docstrings) or printed according, which is affected by the values of
  *PACKAGE* and *READTABLE*. See *DOCUMENT-NORMALIZE-PACKAGES* for the
  details.
  """
  (with-all-sections-cached ()
    (with-format (format)
      (let* ((*print-right-margin* (or *print-right-margin* 80))
             (pages (page-specs-to-pages pages documentable stream))
             (default-page (alexandria:last-elt pages))
             (3bmd-code-blocks:*code-blocks* t)
             (3bmd-code-blocks:*code-blocks-default-colorize*
               (and (not (eq *html-subformat* :w3m))
                    :common-lisp))
             (3bmd-code-blocks::*colorize-name-map*
               (if (eq *html-subformat* :w3m)
                   (make-hash-table)
                   (alexandria:plist-hash-table
                    `("cl-transcript" :common-lisp
                      ,@(alexandria:hash-table-plist
                         3bmd-code-blocks::*colorize-name-map*))
                    :test #'equal))))
        (with-tracking-pages-created ()
          (with-pages (pages)
            ;; Write output to DEFAULT-PAGE until a DOCUMENT-OBJECT
            ;; finds a reference that should go to another PAGE.
            (with-temp-output-to-page (stream default-page)
              ;; Call DOCUMENT-OBJECT on each stuff to be documented
              ;; in DOCUMENTABLE, and keep track of where to add extra
              ;; newlines.
              (let ((firstp t)
                    (add-blank-p nil))
                (map-documentable
                 (lambda (object1)
                   (with-headings (object1)
                     (when (or add-blank-p
                               (and (not firstp)
                                    (not *document-tight*)))
                       (terpri stream))
                     (setq firstp nil)
                     (document-object object1 stream)
                     (setq add-blank-p (not *document-tight*))))
                 documentable)))
            (let ((outputs ()))
              (do-pages-created (page)
                ;; Add the markdown reference link definitions for all
                ;; PAGE-USED-LINKS on PAGE.
                (with-temp-output-to-page (stream page)
                  (write-markdown-reference-style-link-definitions stream))
                ;; Now that markdown output for this PAGE is complete,
                ;; we may want to convert it to the requested
                ;; *FORMAT*.
                (unless (eq *format* :markdown)
                  (let ((markdown-string
                          (with-temp-input-from-page (stream page)
                            (alexandria:read-stream-content-into-string
                             stream))))
                    (delete-stream-spec (page-temp-stream-spec page))
                    (with-final-output-to-page (stream page)
                      (when (page-header-fn page)
                        (funcall (page-header-fn page) stream))
                      (print-markdown (post-process-for-w3m
                                       (parse-markdown-fast
                                        markdown-string))
                                      stream :format *format*)
                      (when (page-footer-fn page)
                        (funcall (page-footer-fn page) stream)))))
                (push (unmake-stream-spec (page-final-stream-spec page))
                      outputs))
              (cond ((< 1 (length pages))
                     (reverse outputs))
                    ((null stream)
                     (first outputs))
                    (t
                     nil)))))))))

;;; Call FN with each thing within OBJECT (an argument of the same
;;; name of DOCUMENT). Handle special PROGV forms, which allow
;;; controlling the dynamic environment around DOCUMENT-OBJECT calls.
;;; This is only used by PAX-APROPOS* and is not part of DOCUMENT's
;;; contract.
(defun map-documentable (fn object)
  (cond ((not (listp object))
         (funcall fn object))
        ;; ((progv <symbols-form> <values-form>)
        ;;  <reference> <string> ...)
        ((and (listp (first object))
              (eq (caar object) 'progv))
         (destructuring-bind (symbols-form values-form) (rest (first object))
           (progv (eval symbols-form) (eval values-form)
             (map-documentable fn (rest object)))))
        (t
         (dolist (element object)
           (if (atom element)
               (funcall fn element)
               (map-documentable fn element))))))

;;; Silence SBCL compiler note.
#+sbcl
(define-condition unresolvable-reflink (warning) ())

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

;;; Emit markdown definitions for links (in *LINKS*) to REFERENCE
;;; objects that were linked to on the current page.
(defun write-markdown-reference-style-link-definitions (stream)
  (let ((used-links (sort (alexandria:hash-table-keys (page-used-links *page*))
                          #'string< :key #'link-id))
        (*package* (find-package :keyword)))
    (when used-links
      (format stream "~%")
      (dolist (link used-links)
        (assert (not (link-p (link-page link))))
        (let ((anchor (reference-to-anchor (link-reference link))))
          ;; The format is [label]: url "title"
          ;; E.g.  [1]: http://example.org/Hobbit#Lifestyle "Hobbit lifestyles"
          (if (stringp (link-page link))
              ;; Link to external page.
              (format stream "  [~A]: ~A ~A~%" (link-id link)
                      (link-page link)
                      (escape-markdown-reflink-definition-title
                       (princ-to-string anchor)))
              ;; Link to documentation generated in the same run.
              (format stream "  [~A]: ~A ~A~%"
                      (link-id link)
                      (link-to-uri link)
                      (escape-markdown-reflink-definition-title
                       (let* ((ref (link-reference link))
                              (locative-type (reference-locative-type ref)))
                         (if (not (eq locative-type 'section))
                             (princ-to-string anchor)
                             (markdown-section-title ref)))))))))))

(defun markdown-section-title (ref)
  (let ((section (resolve ref :errorp (not *document-open-linking*))))
    (if (and section (section-title section))
        (let ((*package* (section-package section)))
          (unescape-markdown (process-title (section-title section))))
        (process-title (let ((*print-case* :upcase))
                         (prin1-to-string (reference-object ref)))))))


;;;; URIs of stuff

(defun object-to-uri (object)
  (let ((reference (canonical-reference object)))
    (when reference
      (let ((link (find-link reference)))
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
        (finalize-pax-url (urlencode (reference-to-pax-url
                                      (link-reference link))))
        (let ((target-page-references (page-references target-page))
              (target-page-uri-fragment (page-uri-fragment target-page)))
          ;; Don't generate anchors when linking to the first
          ;; reference on the page (REACHABLE-CANONICAL-REFERENCES).
          (if (and (reference= (link-reference link)
                               (first target-page-references))
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
                      (anchor-id (link-reference link))))))))

(defun relative-page-uri-fragment (page reference-page)
  (let ((fragment (page-uri-fragment page))
        (reference-fragment (page-uri-fragment reference-page)))
    (assert (and fragment reference-fragment))
    (relativize-pathname fragment reference-fragment)))


;;;; Page specs

;;; Convert the PAGES argument of DOCUMENT to PAGE objects.
(defun page-specs-to-pages (page-specs documentable stream)
  """The PAGES argument is to create multi-page documents by routing
  some of the generated output to files, strings or streams. PAGES is
  a list of page specification elements. A page spec is a [property
  list][clhs] with keys :OBJECTS, :OUTPUT, :URI-FRAGMENT,
  :SOURCE-URI-FN, :HEADER-FN and :FOOTER-FN. OBJECTS is a list of
  objects (references are allowed but not required) whose
  documentation is to be sent to :OUTPUT.

  When documentation for an object is generated, the first matching
  page spec is used, where the object matches the page spec if it is
  [reachable][COLLECT-REACHABLE-OBJECTS generic-function] from one of
  its :OBJECTS.

  :OUTPUT can be a number things:

  - If it's a list whose first element is a string or a pathname, then
    output will be sent to the file denoted by that and the rest of
    the elements of the list are passed on to CL:OPEN. One extra
    keyword argument is :ENSURE-DIRECTORIES-EXIST. If it's true,
    ENSURE-DIRECTORIES-EXIST will be called on the pathname before
    it's opened.

  - If it's NIL, then output will be collected in a string.

  - If it's T, then output will be sent to *STANDARD-OUTPUT*.

  - If it's a stream, then output will be sent to that stream.

  If some pages are specified, DOCUMENT returns a list of designators
  for generated output. If a page whose :OUTPUT refers to a file that
  was created (which doesn't happen if nothing would be written to
  it), then the corresponding pathname is included in the list. For
  strings the string itself, while for streams the stream object is
  included in the list. This way it's possible to write some pages to
  files and some to strings and have the return value indicate what
  was created. The output designators in the returned list are ordered
  by creation time.

  Note that even if PAGES is specified, STREAM acts as a catch all,
  taking the generated documentation for references not claimed by any
  pages. Also, the filename, string or stream corresponding to STREAM
  is always the first element in the list of generated things, that is
  the return value.

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
     :output ("build/tmp/pax-extension-api.html")
     ;; However, on the web server html files will be at this
     ;; location relative to some common root, so override the
     ;; default:
     :uri-fragment "doc/dev/pax-extension-api.html"
     ;; Set html page title, stylesheet, charset.
     :header-fn 'write-html-header
     ;; Just close the body.
     :footer-fn 'write-html-footer)
    ;; Catch the reference that were not reachable from the above. It
    ;; is important for this page spec to be last.
    (:objects (, @pax-manual)
     :output ("build/tmp/manual.html")
     ;; Links from the extension api page to the manual page will
     ;; be to ../user/pax-manual#<anchor>, while links going to
     ;; the opposite direction will be to
     ;; ../dev/pax-extension-api.html#<anchor>.
     :uri-fragment "doc/user/pax-manual.html"
     :header-fn 'write-html-header
     :footer-fn 'write-html-footer))
  ```"""
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
    (let ((stream-spec (apply #'make-stream-spec output)))
      (make-page
       :references (reachable-canonical-references objects)
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
       :source-uri-fn source-uri-fn))))

(defun reachable-canonical-references (documentable)
  (let ((reference-lists ()))
    (map-documentable
     (lambda (object)
       (push (loop for object in (cons object
                                       (collect-reachable-objects object))
                   for ref = (canonical-reference object)
                   when ref
                     collect ref)
             reference-lists))
     documentable)
    (apply #'append reference-lists)))


(defsection @markdown-support (:title "Markdown Support")
  "The [Markdown][markdown] in docstrings is processed with the
  [3BMD][3bmd] library."
  (@markdown-indentation section)
  (@markdown-syntax-highlighting section)
  (@mathjax section))

(defsection @markdown-indentation (:title "Indentation")
  """Docstrings can be indented in any of the usual styles. PAX
  normalizes indentation by converting:

      (defun foo ()
        "This is
        indented
        differently")

  to

      (defun foo ()
        "This is
      indented
      differently")

  See [DOCUMENT-OBJECT][(method () (string t))] for the details."""
  (document-object (method () (string t))))

(defmethod document-object ((string string) stream)
  "Print STRING to STREAM as a docstring. That is, [clean up
  indentation][@markdown-indentation], perform @CODIFICATION, and
  linking (see @LINKING-TO-CODE, @LINKING-TO-THE-HYPERSPEC).

  Docstrings in sources are indented in various ways, which can easily
  mess up markdown. To handle the most common cases leave the first
  line alone, but from the rest of the lines strip the longest run of
  leading spaces that is common to all non-blank lines."
  (document-docstring string stream :indentation "" :paragraphp nil)
  (terpri stream))

(defvar *document-docstring-key* nil)

(defun/autoloaded document-docstring
    (docstring stream &key (indentation "    ")
               exclude-first-line-p (paragraphp t))
  "Process and DOCSTRING to STREAM, [stripping
  indentation][@markdown-indentation] from it, performing
  @CODIFICATION and @LINKING-TO-CODE, finally prefixing each line with
  INDENTATION. The prefix is not added to the first line if
  EXCLUDE-FIRST-LINE-P. If PARAGRAPHP, then add a newline before and
  after the output."
  (when (and docstring
             (not (equal docstring ""))
             ;; If the output is going to /dev/null and this is a
             ;; costly operation, skip it.
             (null *table-of-contents-stream*))
    (let ((docstring (funcall (or *document-docstring-key* #'identity)
                              docstring)))
      (when docstring
        (let* ((docstring (strip-docstring-indentation docstring))
               (reindented (prefix-lines
                            indentation (codify-and-link docstring)
                            :exclude-first-line-p exclude-first-line-p)))
          (if paragraphp
              (format stream "~%~A~%" reindented)
              (format stream "~A" reindented)))))))

(defsection @markdown-syntax-highlighting (:title "Syntax Highlighting")
  "For syntax highlighting, Github's [fenced code
  blocks][fenced-code-blocks] markdown extension to mark up code
  blocks with triple backticks is enabled so all you need to do is
  write:

      ```elisp
      (defun foo ())
      ```

  to get syntactically marked up HTML output. Copy `src/style.css`
  from PAX and you are set. The language tag, `elisp` in this example,
  is optional and defaults to `common-lisp`.

  See the documentation of [3BMD][3bmd] and [colorize][colorize] for
  the details.

  [3bmd]: https://github.com/3b/3bmd
  [colorize]: https://github.com/redline6561/colorize/
  [fenced-code-blocks]: https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks")

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
  strings can be a pain. [Pythonic String
  Reader][pythonic-string-reader] can help with that.

    [pythonic-string-reader]: https://github.com/smithzvk/pythonic-string-reader
  """)


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
                      (values `((:RAW-HTML #.(format nil "<i>~%"))
                                ,(indent-verbatim tree) (:RAW-HTML "</i>"))
                              nil t))
                     (t
                      (values `((:RAW-HTML #.(format nil "<i>~%"))
                                ,(indent-code-block tree)
                                (:RAW-HTML "</i>"))
                              nil t)))))
        (map-markdown-parse-tree '(:code :verbatim 3bmd-code-blocks::code-block)
                                 '() nil #'translate parse-tree))
      parse-tree))


(defun include-docstrings (parse-tree)
  (map-markdown-parse-tree (list :reference-link) () nil
                           #'translate-docstring-links parse-tree))

;;; This is the first of the translator functions, which are those
;;; passed to MAP-MARKDOWN-PARSE-TREE.
;;;
;;; FIXME: Circular includes are hard to detect because the 'recurse'
;;; return value is handled in the caller of this function.
(defun translate-docstring-links (parent tree)
  """DOCSTRING is a pseudolocative for including the parse tree of the
  markdown [DOCSTRING][generic-function] of a definition in the parse
  tree of a docstring when generating documentation. It has no source
  location information and only works as an explicit link. This
  construct is intended to allow docstrings live closer to their
  implementation, which typically involves a non-exported definition.

  ```
  (defun div2 (x)
    "X must be an [even type][docstring]."
    (/ x 2))

  (deftype even ()
    "an even integer"
    '(satisfies oddp))
  ```

  In the output of `(DOCUMENT #'DIV2)`, we have that `X must be an an
  even integer`."""
  (declare (ignore parent))
  (assert (parse-tree-p tree :reference-link))
  (let ((label (pt-get tree :label))
        (definition (pt-get tree :definition)))
    (alexandria:nth-value-or 0
      (when (eq (read-locative-from-markdown definition) 'docstring)
        (multiple-value-bind (object locative foundp)
            (read-reference-from-string (parse-tree-to-text label))
          (when foundp
            (let ((docstring (docstring (locate object locative))))
              (when docstring
                (values (parse-markdown
                         (strip-docstring-indentation docstring))
                        t t))))))
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

  Symbols are read in the current *PACKAGE*, which is subject to
  *DOCUMENT-NORMALIZE-PACKAGES*.")

(defun interesting-object-p (object name)
  (or (and (symbolp object)
           (or (<= 3 (length name))
               (external-symbol-p object)))
      (has-local-reference-p object)))

;;; The core of the implementation of *DOCUMENT-UPPERCASE-IS-CODE*.
;;;
;;; This is called by MAP-WORDS so the return values are NEW-TREE,
;;; SLICE. Also called by TRANSLATE-EMPH that expects only a single
;;; return value, the new tree.
(defun translate-uppercase-word (parent tree word)
  (declare (ignore parent))
  (let ((emph (and (listp tree) (eq :emph (first tree))))
        (codifiablep (codifiable-word-p word)))
    (alexandria:nth-value-or 0
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
  (cond ((and emph codifiablep (eql #\\ (alexandria:first-elt word)))
         ;; E.g. "*\\DOCUMENT-NORMALIZE-PACKAGES*"
         ;; -> (:EMPH "DOCUMENT-NORMALIZE-PACKAGES")
         (values (list `(:emph ,(subseq word 1))) t))
        ((and codifiablep (eql #\\ (alexandria:first-elt word)))
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
  (multiple-value-bind (object name)
      (parse-word word :trim t :depluralize t
                       :only-one (lambda (object name)
                                   (declare (ignore object))
                                   (notany #'lower-case-p name)))
    (when (and name (interesting-object-p object name))
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
    (if (alexandria:starts-with-subseq "cl-transcript" lang)
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
             (apply #'transcribe transcript nil :update-only t args)))
      (if dynenv
          (funcall dynenv #'call-it)
          (call-it)))))

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
      (cond ((alexandria:starts-with-subseq "\\\\" string)
             (subseq string 2))
            ((alexandria:starts-with-subseq "\\" string)
             (if (downcasingp)
                 (downcase-all-uppercase-code (subseq string 1))
                 (subseq string 1)))
            (t
             (if (downcasingp)
                 (downcase-all-uppercase-code string)
                 string)))
      (cond ((alexandria:starts-with-subseq "\\\\" string)
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
  @OBJECT of any of the following links will disambiguate based the
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
                                 :element-type 'reference)))
    (map-markdown-parse-tree
     '(:code :reference-link)
     '(:explicit-link :image :mailto)
     nil
     (alexandria:rcurry #'translate-to-links linked-refs)
     parse-tree)))

(defun translate-to-links (parent tree linked-refs)
  (alexandria:nth-value-or 0
    (maybe-unescape-or-autolink parent tree linked-refs)
    (maybe-translate-explicit-link tree linked-refs)
    (assert nil)))

;;; Check if we are in a position to link to REF and that we are
;;; allowed to.
(defun linkable-ref-p (ref &key page)
  (declare (special *document-link-sections*))
  (let ((ref (replace-go-target ref)))
    (and (or (and *document-link-sections*
                  (eq (reference-locative-type ref) 'section))
             *document-link-code*)
         (let ((page (or page (reference-page ref))))
           (assert (not (link-p page)))
           (or
            ;; These have no pages, but won't result in link anyway.
            ;; Keep them.
            (member (reference-locative-type ref) '(dislocated argument))
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
  (linkable-ref-p (link-reference link)
                  :page (link-page (unaliased-link link))))

(defun linkable-references (refs)
  (remove-if-not #'linkable-ref-p refs))

(defsection @specified-locative (:title "Specified Locative")
  """The following examples all render as [DOCUMENT][function].

  - `\[DOCUMENT][function]` (*object + locative, explicit link*)
  - `DOCUMENT function` (*object + locative, autolink*)
  - `function DOCUMENT` (*locative + object, autolink*)

  The Markdown link definition (i.e. `\function` between the second
  set of brackets above) needs no backticks to mark it as code.

  Here and below, the @OBJECT (`\DOCUMENT`) is uppercased, and we rely
  on *DOCUMENT-UPPERCASE-IS-CODE* being true. Alternatively, the
  @OBJECT could be explicitly marked up as code with a pair of
  backticks, and then its character case would likely not
  matter (subject to READTABLE-CASE).

  The link text in the above examples is `\DOCUMENT`. To override it,
  this form may be used:

  - `[see this][document function]` (*title + object + locative,
    explicit link*) renders as: [see this][document function].""")

(defun linkables-for-specified-locative (object locative)
  "With a locative specified (e.g. in the explicit link
  `[FOO][function]` or in the text `the FOO function`), a single link
   is made irrespective of any local references."
  (if (member (locative-type locative) '(dislocated argument))
      ;; Handle an explicit [FOO][dislocated] in markdown. This is
      ;; always LINKABLE-REF-P.
      (list (make-reference object 'dislocated))
      (let ((reference (canonical-reference
                        (replace-go-target
                         (make-reference object locative)))))
        (alexandria:when-let
            (link (or (find-link reference)
                      (find-external-link reference)))
          (when (linkablep link)
            (list (link-reference link)))))))

(defsection @unspecified-locative (:title "Unspecified Locative")
  "[filter-string-based-references function][docstring]

  [filter-method-references function][docstring]

  [filter-locative-references function][docstring]"
  (@unambiguous-unspecificed-locative section)
  (@ambiguous-unspecified-locative section))

(defun linkables-for-unspecified-locative (object &key local)
  (filter-locative-references
   (filter-method-references
    (replace-go-targets
     (filter-external-references
      (filter-string-based-references
       (linkable-references
        (references-to-object object :local local))))))))

(defun replace-go-targets (references)
  (mapcar #'replace-go-target references))

(defun replace-go-target (reference)
  ;; RESOLVE to check the syntax is correct. See the test for "PRINT
  ;; go" in MGL-PAX-TEST::TEST-GO.
  (alexandria:if-let (resolved
                      (and (eq (reference-locative-type reference) 'go)
                           (resolve reference :errorp nil)))
    (apply #'make-reference (first (reference-locative-args resolved)))
    reference))

(defun filter-string-based-references (refs)
  "When only an @OBJECT is provided without a locative, all
  definitions of the object are considered as possible link targets.
  Then, definitions that are not symbol-based (i.e. whose
  REFERENCE-OBJECT is not a symbol) are filtered out to prevent
  unrelated PACKAGEs and ASDF:SYSTEMs from cluttering the
  documentation without the control provided by importing symbols."
  (remove-if #'string-based-reference-p refs))

(defun string-based-reference-p (reference)
  ;; We assume that REFERENCE is canonical and only look at the
  ;; object.
  (stringp (reference-object reference)))

(defun filter-method-references (refs)
  "To further reduce clutter, if the definitions include a
  GENERIC-FUNCTION locative, then all references with LOCATIVE-TYPE
  [METHOD][locative], [ACCESSOR][locative], [READER][locative] and
  [WRITER][locative] are removed to avoid linking to a possibly large
  number of methods."
  (flet ((non-method-refs ()
           (remove-if (lambda (ref)
                        (member (reference-locative-type ref)
                                '(accessor reader writer method)))
                      refs)))
    (cond
      ;; If in doubt, prefer the generic function to methods.
      ((find 'generic-function refs :key #'reference-locative-type)
       (non-method-refs))
      ;; No generic function, prefer non-methods to methods.
      ((non-method-refs))
      (t
       refs))))

(defun filter-locative-references (refs)
  "Furthermore, filter out all references with LOCATIVE-TYPE
  LOCATIVE if there are references with other LOCATIVE-TYPEs."
  (or (remove 'locative refs :key #'reference-locative-type)
      refs))

(defsection @unambiguous-unspecificed-locative
    (:title "Unambiguous Unspecified Locative")
  """In the following examples, although no locative is specified,
  `\DOCUMENT` names a single @OBJECT being DOCUMENTed, so they all
  render as [DOCUMENT][function].

  - `\[DOCUMENT][]` (*object, explicit link*),
  - `DOCUMENT` (*object, autolink*).

  To override the title:

  - `\[see this][document]` (*title + object, explicit link*) renders
    as: [see this][document].""")

(defsection @ambiguous-unspecified-locative
    (:title "Ambiguous Unspecified Locative")
  """These examples all render as [SECTION][], linking to both
  definitions of the @OBJECT `\SECTION`, the `\CLASS` and the
  `\LOCATIVE`.

  - `[SECTION][]` (*object, explicit link*)
  - `\SECTION` (*object, autolink*)

  To override the title:

  - `\[see this][section]` (*title + object, explicit link*) renders
    as: [see this][section].""")

(defsection @explicit-and-autolinking (:title "Explicit and Autolinking")
  "The examples in the previous sections are marked with *explicit
  link* or *autolink*. Explicit links are those with a Markdown
  reference link spelled out explicitly, while autolinks are those
  without.")

(defun linkables-for-explicitly-unspecified-locative (object)
  "Explicit links with an unspecified locative (e.g. `[FOO][]`) are
  linked to all non-local references."
  (linkables-for-unspecified-locative object :local :exclude))

(defun linkables-for-autolink-with-unspecified-locative (object)
  "Unless a locative is [specified][ @specified-locative section], no
  [autolinking][ @explicit-and-autolinking section] is performed for
  @OBJECTS for which there are local references. For example, `FOO`
  does not get any links if there is _any_ local reference with the
  same @OBJECT."
  (linkables-for-unspecified-locative object))

;;; All returned REFERENCES are for the same object.
(defun linkables-for-autolink (objects locatives linked-refs)
  (or
   ;; Use the first of OBJECTS with which some LOCATIVES form known
   ;; references.
   (loop for object in objects
           thereis (loop for locative in locatives
                         append (linkables-for-specified-locative
                                 object locative)))
   ;; Fall back on the no-locative case.
   (loop for object in objects
         for refs = (linkables-for-autolink-with-unspecified-locative object)
         until (or (suppressed-link-p object refs linked-refs)
                   (has-local-reference-p object))
           thereis refs
         until (references-to-object object :local :include))))


;;;; Explicit links

(defun maybe-translate-explicit-link (tree linked-refs)
  (when (eq :reference-link (first tree))
    (translate-explicit-link tree linked-refs)))

;;; This translator handles :REFERENCE-LINK nodes:
;;;
;;;   - those with an explicit locative (:REFERENCE-LINK :LABEL
;;;     ((:CODE "SOMETHING")) :DEFINITION "function"), the parse of
;;;     [`SOMETHING`][function],
;;;
;;;   - and those with no locative (:REFERENCE-LINK :LABEL ((:CODE
;;;     "SOMETHING")) :TAIL "[]"), the parse of [`SOMETHING`][].
(defun translate-explicit-link (reflink linked-refs)
  ;; Markdown to handle: [`SECTION`][class], [`SECTION`][], [see
  ;; this][section class], [see this][section]. For example, the tree
  ;; for [`SECTION`][class] is (:REFERENCE-LINK :LABEL ((:CODE
  ;; "SECTION")) :DEFINITION "class").
  (multiple-value-bind (label explicit-label-p object locative pax-link-p)
      (dissect-reflink reflink)
    (cond ((not pax-link-p)
           ;; [something][user-defined-id]
           reflink)
          ((and (eq object 'not-found)
                (member locative '(dislocated argument)))
           ;; [not code][dislocated]
           (values label nil t))
          (t
           (let ((refs (unless (eq object 'not-found)
                         (if locative
                             (linkables-for-specified-locative object locative)
                             (linkables-for-explicitly-unspecified-locative
                              object)))))
             (cond (refs
                    (dolist (ref refs)
                      (vector-push-extend ref linked-refs))
                    (values (make-reflinks label explicit-label-p refs) nil t))
                   (t
                    (values (if (likely-a-pax-reflink-p object locative reflink)
                                (signal-unresolvable-reflink reflink object
                                                             locative)
                                label)
                            nil t))))))))

(defun likely-a-pax-reflink-p (object locative reflink)
  (or (and locative (symbolp object))
      (zerop (length (getf (rest reflink)
                           :definition)))))

;;; Return 1. the label, 2. whether to use the returned label in the
;;; reference link without further transformations (e.g. replace it
;;; with SECTION-TITLE), 3. object, 4. the locative, 5. whether the
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
             (and definition (read-locative-from-markdown definition)))
           (label-string (trim-whitespace
                          (parse-tree-to-text label :deemph nil))))
      (alexandria:nth-value-or 0
        (and (or empty-definition-p locative-from-def)
             label-string
             ;; [foo][] or [foo][function]
             (multiple-value-bind (object name)
                 (parse-reflink-label-string label-string locative-from-def)
               (when name
                 (values label nil object locative-from-def t))))
        (and (member locative-from-def '(dislocated argument))
             (values label t 'not-found locative-from-def t))
        ;; [see this][foo]
        (multiple-value-bind (object name)
            (parse-word definition :trim nil :depluralize nil
                                   :only-one (constantly t))
          (when name
            (values label t object nil t)))
        ;; [see this][foo function]
        (multiple-value-bind (object locative foundp)
            (and definition (read-reference-from-string definition))
          (when foundp
            (values label t object locative t)))
        (values label nil 'not-found locative-from-def
                (or empty-definition-p locative-from-def))))))

(defun parse-reflink-label-string (label-string locative)
  (let ((interesting-object nil)
        (interesting-name nil))
    (flet ((good-parse-p (object name)
             (cond ((external-locative-p locative)
                    (let ((reference (locate object locative :errorp nil)))
                      (when reference
                        (reference-object reference))))
                   ((has-reference-p object)
                    t)
                   (t
                    ;; Remember the first interesting object ...
                    (when (and (null interesting-name)
                               (interesting-object-p object name))
                      (setq interesting-object object
                            interesting-name name)
                      ;; ... but don't stop looking for a known
                      ;; reference.
                      nil)))))
      (alexandria:nth-value-or 1
        (parse-word label-string
                    :trim nil :depluralize t
                    :clhs-substring-match t
                    :only-one #'good-parse-p)
        ;; Only consider merely interesting objects if there were no
        ;; objects with known references.
        (values interesting-object interesting-name)))))


;;;; Autolinking

;;; This translator handles (:CODE "SOMETHING"), the parse of
;;; `SOMETHING`: looks for any references to "SOMETHING" and tanslates
;;; it to, for example, (:REFERENCE-LINK :LABEL ((:CODE "SOMETHING"))
;;; :DEFINITION "function") if there is a single function reference to
;;; it.
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
               (let ((locative (read-locative-from-markdown string)))
                 (when locative
                   (push locative locatives))))
             (try (element)
               (cond ((stringp element)
                      (try-string element))
                     ((eq :code (first element))
                      (try-string (second element)))
                     ;; (:REFERENCE-LINK :LABEL ((:CODE
                     ;; "CLASS")) :DEFINITION "0524")
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
      (if (alexandria:starts-with #\\ string)
          `(:code ,(subseq string 1))
          (autolink parent tree string linked-refs)))))


;;;; Common code for @EXPLICIT-AND-AUTOLINKING

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
               ;; [`label`](pax:object)
               `((:explicit-link
                  :label ,label
                  :source ,(finalize-pax-url
                            (urlencode
                             (reference-to-ambiguous-pax-url ref-1)))))
               ;; `label`([1][link-id-1] [2][link-id-2])
               `(,@label
                 "("
                 ,@(loop
                     for i upfrom 0
                     for ref in (sort-references refs)
                     append `(,@(unless (zerop i)
                                  '(" "))
                              (:reference-link
                               :label (,(code-fragment i))
                               :definition ,(link-to-reference ref))))
                 ")")))
          ((member (reference-locative-type ref-1) '(dislocated argument))
           label)
          ;; FIXME: TITLE should be a generic function.
          ((eq (reference-locative-type ref-1) 'section)
           (let ((section (resolve ref-1 :errorp nil)))
             `((:reference-link
                :label ,(if (or explicit-label-p
                                (null (section-title section)))
                            label
                            (codify (parse-markdown (section-title section))))
                :definition ,(link-to-reference ref-1)))))
          ((eq (reference-locative-type ref-1) 'glossary-term)
           (let ((glossary-term (resolve ref-1 :errorp nil)))
             `((:reference-link
                :label ,(if (or explicit-label-p
                                (null (glossary-term-title glossary-term)))
                            label
                            (codify (parse-markdown (glossary-term-title
                                                     glossary-term))))
                :definition ,(link-to-reference ref-1)))))
          (t
           `((:reference-link
              :label ,label
              :definition ,(link-to-reference ref-1)))))))

;;; Order REFERENCES in an implementation independent way.
(defun sort-references (references)
  (flet ((key (reference)
           (let ((locative-type (reference-locative-type reference)))
             (with-standard-io-syntax*
               ;; Avoid mentions of BASE-CHAR and such.
               (let ((*print-readably* nil))
                 (format nil "~S ~S ~S ~S"
                         (reference-object reference)
                         ;; Sort by SYMBOL-NAME before SYMBOL-PACKAGE.
                         ;; Currently the package is not even printed in
                         ;; the documentation.
                         (if (eq locative-type 'dspec)
                             ;; Put the unknown DSPEC references last
                             ;; for the same object (for
                             ;; DOCUMENT-FOR-EMACS/AMBIGUOUS).
                             "~~~~"
                             (symbol-name locative-type))
                         (package-name (symbol-package locative-type))
                         (reference-locative-args reference)))))))
    (sort-list-with-precomputed-key references #'string< :key #'key)))

;;; Only compute the key for each element once.
(defun sort-list-with-precomputed-key (list pred &key key)
  (map 'list #'car (sort (map 'vector (lambda (x)
                                        (cons x (funcall key x)))
                              list)
                         pred :key #'cdr)))


(defsection @unresolvable-reflinks (:title "Unresolvable Links")
  (unresolvable-reflink condition)
  (output-reflink function)
  (output-label function))

(define-condition unresolvable-reflink (warning)
  ((reflink :initarg :reflink :reader unresolvable-reflink-string)
   (object :initarg :object :reader unresolvable-reflink-object)
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
         (object #-cmucl (if (slot-boundp c 'object)
                             (unresolvable-reflink-object c)
                             'not-found)
                 #+cmucl (or (ignore-errors
                              (unresolvable-reflink-object c))
                             'not-found))
         (locative #-cmucl (if (slot-boundp c 'locative)
                               (unresolvable-reflink-locative c)
                               nil)
                   #+cmucl (or (ignore-errors
                                (unresolvable-reflink-locative c))
                               'not-found)))
    (cond ((and (not (eq object 'not-found)) locative)
           (format stream "~@<~S cannot be resolved although ~S looks ~
                           like an ~S and ~S like a ~S.~:@>"
                   reflink object '@object locative '@locative))
          ((not (eq object 'not-found))
           (format stream "~@<~S cannot be resolved although ~S looks ~
                           like an ~S.~:@>"
                   reflink object '@object))
          (locative
           (format stream "~@<~S cannot be resolved although ~S looks ~
                           like a ~S.~:@>"
                   reflink locative '@locative))
          (t
           (format stream "~@<~S cannot be resolved.~:@>" reflink)))
    (when *reference-being-documented*
      (format stream "~%~@<(while documenting ~S)~:@>"
              *reference-being-documented*))))

(defun signal-unresolvable-reflink (reflink object locative)
  (restart-case
      (let ((string (reflink-to-string reflink)))
        (cond ((and (not (eq object 'not-found)) locative)
               (warn 'unresolvable-reflink
                     :reflink string :object object :locative locative))
              ((not (eq object 'not-found))
               (warn 'unresolvable-reflink :reflink string :object object))
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
  "Invoke the OUTPUT-REFLINK restart."
  (declare (ignore condition))
  (invoke-restart 'output-reflink))

(defun output-label (&optional condition)
  "Invoke the OUTPUT-L restart."
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

  However, explicit links (when a @LOCATIVE was specified or found
  near the @OBJECT) are never suppressed. In the following, in both
  docstrings, both occurrences `FOO` produce links.

      "`FOO` is safe. [`FOO`][macro] is great."
      "`FOO` is safe. Macro `FOO` is great."

  As an exception, links with [specified][@specified-locative section]
  and [unambiguous][ @unambiguous-unspecificed-locative section]
  locatives to SECTIONs and GLOSSARY-TERMs always produce a link to
  allow their titles to be displayed properly.

  Finally, [autolinking][@explicit-and-autolinking section] to T or
  NIL is suppressed (see *DOCUMENT-LINK-TO-HYPERSPEC*).""")

(defun suppressed-link-p (object refs linked-refs)
  (when refs
    (or (member object '(t nil))
        (and
         ;; See if OBJECT would be linked to any previously linked-to
         ;; reference.
         (loop for ref in refs
                 thereis (find (reference-object ref) linked-refs
                               :test #'reference-object=))
         ;; Replace references to sections and glossary terms with
         ;; their title any number of times.
         (not (and (= (length refs) 1)
                   (typep (resolve (first refs) :errorp nil)
                          '(or section glossary-term))))))))


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
  "A non-negative integer. Top-level sections are given a table of
  contents, which includes a nested tree of section titles whose depth
  is limited by this value. Setting it to 0 turns generation of the
  table of contents off. If *DOCUMENT-LINK-SECTIONS* is true, then the
  table of contents will link to the sections.")

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

(defvar *heading-number* ())

;;; (LENGTH *HEADING-NUMBER*)
(defvar *heading-level* 0)

;;; Add this many #\# to markdown section headings in the output. This
;;; is for when a section that is a subsection of another is
;;; documented on its own page by DOCUMENT/OPEN.
(defvar *heading-offset* 0)

(defvar *collecting-headings-p* nil)

;;; A list of HEADING objects in the order of generation (after
;;; COLLECT-HEADINGS is done).
(defvar *headings* ())

(defstruct heading
  object
  title
  level)

;;; Remember the stream so that it can be restored in time for the
;;; printing of table of contents entries even if the stream is
;;; changed by paging.
(defvar *table-of-contents-stream* nil)

;;; Remember the page so that linking can be done from the right
;;; context.
(defvar *table-of-contents-page* nil)

(defun print-table-of-contents-entry (object string stream)
  (loop repeat (* 4 (1- *heading-level*))
        do (write-char #\Space stream))
  (let ((link-id (let ((*page* *table-of-contents-page*))
                   (link-to-reference (canonical-reference object)))))
    (if (and *document-link-sections* link-id)
        (format stream "- [~A~A][~A]" (heading-number) string link-id)
        (format stream "- ~A~A" (heading-number) string)))
  (terpri stream))

(defun/autoloaded call-with-heading (stream object title link-title-to fn)
  (setq title (process-title title))
  (flet ((foo ()
           ;; When we are generating the table of contents, arrange
           ;; for all output to go to /dev/null
           ;; (MAKE-BROADCAST-STREAM) except for the headings.
           (cond
             (*collecting-headings-p*
              (funcall fn (make-broadcast-stream)))
             (*table-of-contents-stream*
              (when (<= *heading-level* *document-max-table-of-contents-level*)
                (print-table-of-contents-entry object title
                                               *table-of-contents-stream*)
                (funcall fn (make-broadcast-stream))))
             (t
              (print-section-title stream object title link-title-to)
              (when (and (zerop *heading-level*)
                         (plusp *document-max-table-of-contents-level*)
                         ;; Don't generate a table of contents if it's
                         ;; empty.
                         (cdr *headings*))
                (heading (+ *heading-level* 1 *heading-offset*) stream)
                (format stream " Table of Contents~%~%")
                (let ((*table-of-contents-stream* stream)
                      (*table-of-contents-page* *page*)
                      (*heading-number* (copy-list *heading-number*)))
                  (funcall fn (make-broadcast-stream)))
                (terpri stream))
              (funcall fn (if *table-of-contents-stream*
                              (make-broadcast-stream)
                              stream))))))
    (let ((level *heading-level*))
      (when *collecting-headings-p*
        (collect-heading object title))
      (when (plusp level)
        (incf (nth (1- level) *heading-number*)))
      (let ((*heading-number*
              (append *heading-number*
                      (loop repeat (max 0 (- (1+ level)
                                             (length *heading-number*)))
                            collect 0))))
        (foo)))))

(defun process-title (string)
  (with-output-to-string (out)
    (print-markdown (codify (parse-markdown string)) out)))

(defun print-section-title (stream section title link-title-to)
  (when *document-link-sections*
    (anchor section stream)
    (navigation-link section stream)
    (format stream "~A" (fancy-navigation section)))
  (heading (+ *heading-level* *heading-offset*) stream)
  (if (and *document-link-sections*
           (eq *format* :html))
      (print-section-title-link stream section title link-title-to)
      (format stream " ~A~A~%~%" (heading-number) title)))

(defun print-section-title-link (stream section title link-title-to)
  (if link-title-to
      ;; Hovering over the section title will show the title of
      ;; LINK-TITLE-TO from the markdown reference link definition.
      (format stream " [~A~A][~A]~%~%"
              (heading-number) title
              (link-to-reference link-title-to))
      (format stream " <a href=\"~A\">~A~A</a>~%~%"
              ;; As in PRINT-REFERENCE-BULLET, the object links to a
              ;; separate page when open linking.
              (if *document-open-linking*
                  (finalize-pax-url
                   (urlencode (reference-to-pax-url
                               (canonical-reference section))))
                  (object-to-uri section))
              (heading-number) title)))

(defun fancy-navigation (object)
  (if (and *document-fancy-html-navigation*
           *document-link-sections*
           (eq *format* :html))
      (let* ((position (position object *headings* :key #'heading-object))
             (level (heading-level (elt *headings* position)))
             (n (length *headings*))
             (prev (when (plusp position)
                     (elt *headings* (1- position))))
             (up (when (plusp level)
                   (find (1- level) (subseq *headings* 0 position)
                         :from-end t :key #'heading-level)))
             (next (when (< position (1- n))
                     (elt *headings* (1+ position))))
             (source-uri (source-uri (canonical-reference object))))
        (format nil "<span class=\"outer-navigation\">~
                    <span class=\"navigation\">~
                    ~@[ [&#8592;][~A]~]~
                    ~@[ [&#8593;][~A]~]~
                    ~@[ [&#8594;][~A]~] ~
                    [&#8634;][~A]~
                    ~A~
                    </span></span>~%"
                (when prev
                  (link-to-reference
                   (canonical-reference (heading-object prev))))
                (when up
                  (link-to-reference
                   (canonical-reference (heading-object up))))
                (when next
                  (link-to-reference
                   (canonical-reference (heading-object next))))
                (link-to-reference (canonical-reference object))
                (if source-uri
                    (format nil " <a href=~S>&#955;</a>" source-uri)
                    "")))
      ""))

(defun collect-heading (object title)
  (push (make-heading :object object :title title :level *heading-level*)
        *headings*))

(defun collect-headings (object)
  (let ((*collecting-headings-p* t)
        (*headings* ())
        (*table-of-contents-stream* (make-broadcast-stream))
        (*document-max-table-of-contents-level* 0))
    (document-object object (make-broadcast-stream))
    (reverse *headings*)))

;;; Determine what SECTION's *HEADING-LEVEL* would be under its root
;;; ancestor.
(defun heading-offset (object)
  (multiple-value-bind (foundp depth) (find-root-section object)
    (if foundp
        depth
        ;; OBJECT is not a SECTION (or a reference to one), neither is
        ;; it contained in a SECTION. Start from H2. This only affects
        ;; ASDF:SYSTEMs in stock PAX.
        1)))

(defun write-navigation-link (heading stream)
  (let ((link-id (link-to-reference
                  (canonical-reference (heading-object heading)))))
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

(defun heading-number ()
  (format nil "~@[~{~D~^.~} ~]"
          (when (<= (1- (length *heading-number*))
                    *document-max-numbering-level*)
            (butlast *heading-number*))))


(defsection @miscellaneous-documentation-printer-variables
    (:title "Miscellaneous Variables")
  (*document-url-versions* variable)
  (*document-min-link-hash-length* variable)
  (*document-mark-up-signatures* variable)
  (*document-normalize-packages* variable)
  (*document-base-url* variable))


(defvar *document-url-versions* '(2 1)
  """A list of versions of PAX \URL formats to support in the
  generated documentation. The first in the list is used to generate
  links.

  PAX emits HTML anchors before the documentation of SECTIONs
  (see @LINKING-TO-SECTIONS) and other things (see @LINKING-TO-CODE).
  For the function `FOO`, in the current version (version 2), the
  anchor is `<a id="MGL-PAX:FOO%20FUNCTION">` and its \URL will end
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

  (document #'foo)
  => ("<a id=\"x-28MGL-PAX-3AFOO-20FUNCTION-29\"></a>
  <a id=\"MGL-PAX:FOO%20FUNCTION\"></a>
  
  - [function] **FOO** *X*
  ")

  (let ((*document-url-versions* '(1)))
    (document #'foo))
  => ("<a id=\"x-28MGL-PAX-3AFOO-20FUNCTION-29\"></a>
  - [function] **FOO** *X*
  ")
  ```
  """)

(defun anchor (object stream)
  (when (member 1 *document-url-versions*)
    (format stream "<a id=~S></a>~%"
            (html4-safe-name (reference-to-anchor-v1 object))))
  (when (member 2 *document-url-versions*)
    (format stream "<a id=~S></a>~%~%"
            (urlencode (reference-to-anchor object)))))

(defun anchor-id (object)
  (if (= (first *document-url-versions*) 1)
      (html4-safe-name (reference-to-anchor-v1 object))
      (urlencode (reference-to-anchor object))))

;;; Return the unescaped name of the HTML anchor for REFERENCE. See
;;; URLENCODE.
(defun reference-to-anchor (object)
  (let ((reference (canonical-reference object)))
    (with-standard-io-syntax*
      ;; The locative may not be readable (e.g. methods with EQL
      ;; specializers with unreadable stuff).
      (let ((*print-readably* nil))
        (format nil "~A ~S" (object-to-url-part (reference-object reference))
                (reference-locative reference))))))

(defun reference-to-anchor-v1 (object)
  (let ((reference (canonical-reference object)))
    (with-standard-io-syntax*
      (let ((*print-readably* nil))
        (format nil "(~A ~S)"
                (object-to-url-part (reference-object reference))
                (reference-locative reference))))))

(defun reference-to-pax-url (reference)
  (let ((reference (canonical-reference reference)))
    (with-standard-io-syntax*
      (let ((*print-readably* nil))
        (format nil "pax:~A ~S"
                (object-to-url-part (reference-object reference))
                (reference-locative reference))))))

(defun reference-to-ambiguous-pax-url (reference)
  (let ((reference (canonical-reference reference)))
    (with-standard-io-syntax*
      (format nil "pax:~A"
              (object-to-url-part (reference-object reference))))))

;;; If OBJECT is a symbol, then print it almost as PRIN1 would with
;;; *PACKAGE* were the CL package. Differences:
;;;
;;; - For symbols in other packages, a single #\: is printed even if
;;;   it is an internal symbol.
;;;
;;; - Package and symbol names are printed without the || syntax but
;;;   #\: and #\Space are escaped with backslashes.
(defun object-to-url-part (object)
  (if (symbolp object)
      (let* ((package (symbol-package object))
             (name (symbol-name object))
             (name-url (print-name-for-url name))
             (cl-package (symbol-package 'print)))
        (cond
          ((eq package cl-package)
           (format nil "~A" name-url))
          ((eq package (symbol-package :if-exists))
           (format nil ":~A" name-url))
          (t
           ;; Note the single #\:.
           (format nil "~A:~A" (print-name-for-url (package-name package))
                   name-url))))
      (prin1-to-string object)))

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
;;;     - [locative-type] object
;;;
;;; When generating HTML, link OBJECT to the anchor of REFERENCE.
(defun/autoloaded print-reference-bullet (reference stream &key name)
  (let ((locative-type (string-downcase
                        (reference-locative-type reference)))
        (name (or name (prin1-to-string* (reference-object reference)))))
    (if *document-mark-up-signatures*
        ;; insert self links in HTML
        (let ((locative-type (escape-markdown locative-type))
              (name (escape-markdown name)))
          (if (eq *format* :html)
              (let ((source-uri (source-uri reference)))
                ;; When *DOCUMENT-OPEN-LINKING* (this includes (EQ
                ;; *HTML-SUBFORMAT* :W3M)), the object is linked to
                ;; the a pax: URL (which opens a separate page), else
                ;; they are self-links.
                (cond ((eq *html-subformat* :w3m)
                       (assert *document-open-linking*)
                       (format stream "- **\\[~A]** [~A](~A)"
                               locative-type name
                               (finalize-pax-url
                                (urlencode (reference-to-pax-url reference)))))
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
                                (urlencode (reference-to-pax-url reference)))))
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
                               (urlencode (reference-to-anchor reference))))))
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

(defun/autoloaded prin1-to-markdown (object &key (escape-newline t))
  "Like PRIN1-TO-STRING, but bind *PRINT-CASE* depending on
  *DOCUMENT-DOWNCASE-UPPERCASE-CODE* and *FORMAT*, and
  ESCAPE-MARKDOWN."
  (escape-markdown (prin1-to-string* object) :escape-newline escape-newline))

;;; Print arg names without the package prefix to a string. The
;;; default value with prefix. Works for macro arglists too.
(defun arglist-to-markdown (arglist)
  (with-output-to-string (out)
    (let ((*seen-special-p* nil)
          (*print-pretty* t)
          (*print-right-margin* 80))
      (declare (special *seen-special-p*))
      (labels
          ((resolve* (object)
             (if (and *document-mark-up-signatures*
                      ;; KLUDGE: Github has trouble displaying things
                      ;; like '`*package*`, so disable this.
                      (eq *format* :html))
                 (codify-and-link (prin1-to-markdown object))
                 (prin1-to-markdown object)))
           (print-arg (arg level)
             (cond ((member arg '(&key &optional &rest &body))
                    (setq *seen-special-p* t)
                    (format out "~A" (prin1-to-markdown arg)))
                   ((symbolp arg)
                    (format out "~A"
                            (escape-markdown
                             (maybe-downcase-all-uppercase-code
                              (symbol-name arg)))))
                   ((atom arg)
                    (format out "~A" (prin1-to-markdown arg)))
                   (*seen-special-p*
                    (if (symbolp (first arg))
                        (format out "(~A~{ ~A~})"
                                (escape-markdown
                                 (maybe-downcase-all-uppercase-code
                                  (symbol-name (first arg))))
                                (mapcar #'resolve* (rest arg)))
                        (format out "~A"
                                (prin1-to-markdown arg))))
                   (t
                    (foo arg (1+ level)))))
           (foo (arglist level)
             (let ((*seen-special-p* nil))
               (declare (special *seen-special-p*))
               (unless (= level 0)
                 (format out "("))
               (loop for i upfrom 0
                     for rest on arglist
                     do (unless (zerop i)
                          (format out " "))
                        (print-arg (car rest) level)
                        ;; There are arglists like (&WHOLE FORM NAME . ARGS).
                        (unless (listp (cdr rest))
                          (format out " . ")
                          (print-arg (cdr rest) level))))
             (unless (= level 0)
               (format out ")"))))
        (foo arglist 0)))))


(defvar *document-normalize-packages* t
  "Determines what *PACKAGE* and *READTABLE* are when generating
  documentation.

  [documenting-section macro][docstring]

  - In all other cases (i.e. when *DOCUMENT-NORMALIZE-PACKAGES* is
    false or we are not documenting a SECTION nor its
    SECTION-ENTRIES), documenting most other kinds of definitions
    attached to a symbol (e.g. a function), prints the symbol itself
    normally, then binds *PACKAGE* to SYMBOL-PACKAGE for the printing
    of the arglist and the docstring.

            CL-USER> (pax:document #'pax:resolve)
            - [function] MGL-PAX:RESOLVE <!> REFERENCE &KEY (ERRORP T)

                A convenience function to LOCATE REFERENCE's object with its
                locative.

        In the above, the `<!>` marks the place where *PACKAGE* is
        bound to `(SYMBOL-PACKAGE 'PAX:RESOLVE)`. See
        DOCUMENTING-REFERENCE from @EXTENDING-DOCUMENT for the gory
        details.")

(defun guess-package (reference)
  (if (and (not (and (boundp '*section*)
                     *document-normalize-packages*))
           reference
           (symbolp (reference-object reference)))
      (symbol-package (reference-object reference))
      *package*))


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


;;;; Basic DOCUMENT-OBJECT methods

(defmethod document-object :around (object stream)
  (let ((*objects-being-documented* (cons object *objects-being-documented*)))
    (cond ((stringp object)
           (let ((*reference-being-documented* nil))
             (call-next-method)))
          ((typep object 'reference)
           (let ((*reference-being-documented* object))
             (call-next-method)))
          (t
           (let* ((reference (canonical-reference object))
                  (*reference-being-documented* reference))
             ;; FIXME: Redirecting to a page ought to be done on all
             ;; branches here.
             (with-temp-output-to-page (stream (reference-page reference))
               (call-next-method object stream)))))))

(defvar *document-do-not-resolve-references* nil)

(defmethod document-object ((reference reference) stream)
  (if *document-do-not-resolve-references*
      (documenting-reference (stream
                              :reference reference
                              :arglist (reference-locative-args reference)))
      (let* ((reference (canonical-reference reference))
             (warn-if-unresolvable
               (and *document-open-linking*
                    ;; It is an error for explicit arguments to
                    ;; DOCUMENT to be unresolvable (so that
                    ;; `mgl-pax-document' produces errors when it
                    ;; must), but we want to document what we can even
                    ;; if a section contains unresolvable stuff.
                    (boundp '*section*)))
             (resolved-object (resolve reference
                                       :errorp (not warn-if-unresolvable))))
        ;; We are fine because RESOLVE probably never returns NIL
        ;; because (RESOLVE (MAKE-REFERENCE NIL 'CONSTANT)) returns a
        ;; REFERENCE.
        (if (and warn-if-unresolvable
                 (null resolved-object))
            (warn "~@<Not documenting unresolvable ~S~@[ in ~S.~]~:@>"
                  reference (second *objects-being-documented*))
            (if (typep resolved-object 'reference)
                (with-temp-output-to-page (stream (reference-page reference))
                  (let ((locative (reference-locative reference)))
                    (locate-and-document (reference-object reference)
                                         (locative-type locative)
                                         (locative-args locative)
                                         stream)))
                (document-object resolved-object stream))))))


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
  output. Compared to that, the following are not supported:

  - COMPILER-MACRO docstrings on ABCL, AllegroCL, \CCL, ECL;
  - DEFTYPE lambda lists on ABCL, AllegroCL, CLISP, \CCL, CMUCL, ECL;
  - default values in MACRO lambda lists on AllegroCL;
  - METHOD-COMBINATION docstrings on ABCL, AllegroCL.

  In addition, CLISP does not support the ambiguous case of @PAX-URLS
  for @BROWSING-LIVE-DOCUMENTATION because the current implementation
  relies on Swank to list definitions of symbols (as VARIABLE,
  [FUNCTION][locative], etc), and that simply doesn't work.""")
