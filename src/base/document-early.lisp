(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defvar *document-uppercase-is-code*)
(export '*document-uppercase-is-code*)
(defvar *document-link-code*)
(export '*document-link-code*)
(defvar *document-link-sections*)
(export '*document-link-sections*)
(defvar *document-max-numbering-level*)
(export '*document-max-numbering-level*)
(defvar *document-max-table-of-contents-level*)
(export '*document-max-table-of-contents-level*)
(defvar *document-text-navigation*)
(export '*document-text-navigation*)
(defvar *document-fancy-html-navigation*)
(export '*document-fancy-html-navigation*)
(defvar *document-mark-up-signatures*)
(export '*document-mark-up-signatures*)
(defvar *document-normalize-packages*)
(export '*document-normalize-packages*)
(defvar *document-html-top-blocks-of-links*)
(export '*document-html-top-blocks-of-links*)
(defvar *document-html-bottom-blocks-of-links*)
(export '*document-html-bottom-blocks-of-links*)
(defvar *document-html-max-navigation-table-of-contents-level*)
(export '*document-html-max-navigation-table-of-contents-level*)

(autoload find-hyperspec-id '#:mgl-pax/document :export nil)
(autoload downcasingp '#:mgl-pax/document :export nil)
(autoload document '#:mgl-pax/document)
(autoload update-asdf-system-readmes '#:mgl-pax/document)
(autoload update-asdf-system-html-docs '#:mgl-pax/document)
;;; UPDATE-PAX-WORLD generates documentation for PAX itself, so load
;;; MGL-PAX/FULL to have all documentation. Otherwise,
;;; MGL-PAX/DOCUMENT would be enough.
(autoload update-pax-world '#:mgl-pax/full)


(defsection @extending-document (:title "Extending DOCUMENT")
  "The following utilities are for writing new DOCUMENT-OBJECT and
  LOCATE-AND-DOCUMENT methods, which emit markdown."
  (*format* variable)
  (with-heading macro)
  (documenting-reference macro)
  (with-dislocated-objects macro)
  (document-docstring function)
  (documentation* function)
  (escape-markdown function)
  (prin1-to-markdown function))

(defvar *format*)
(setf (documentation '*format* 'variable)
      "Bound by DOCUMENT, this allows markdown output to depend on the
       output format.")

(defmacro with-heading ((stream object title &key link-title-to)
                        &body body)
  "Write a markdown heading with TITLE to STREAM. Nested WITH-HEADINGs
  produce nested headings. If *DOCUMENT-LINK-SECTIONS*, generate
  anchors based on the CANONICAL-REFERENCE of OBJECT. LINK-TITLE-TO
  behaves like the LINK-TITLE-TO argument of DEFSECTION."
  `(call-with-heading ,stream ,object ,title ,link-title-to
                      (lambda (,stream)
                        (let ((*heading-level* (1+ *heading-level*)))
                          ,@body))))
(autoload call-with-heading '#:mgl-pax/document :export nil)
(declaim (special *heading-level*))

(defmacro documenting-reference ((stream &key reference arglist name)
                                 &body body)
  "Write REFERENCE to STREAM as described in
  *DOCUMENT-MARK-UP-SIGNATURES*, and establish REFERENCE as a [local
  reference][@LOCAL-REFERENCES]. Unless ARGLIST is NIL or
  :NOT-AVAILABLE, it is printed after the name of object of REFERENCE.

  If ARGLIST is a list, then it is must be a [lambda list][clhs] and
  is printed without the outermost parens and with the package names
  removed from the argument names.

  If ARGLIST is a string, then it is printed without ESCAPE-MARKDOWN."
  (let ((%stream (gensym))
        (%reference (gensym))
        (%arglist (gensym))
        (%name (gensym)))
    `(let ((,%stream ,stream)
           (,%reference (or ,reference *reference-being-documented*))
           (,%arglist ,arglist)
           (,%name ,name))
       (print-reference-bullet ,%reference ,%stream :name ,%name)
       (when (and ,%arglist (not (eq ,%arglist :not-available)))
         (write-char #\Space ,%stream)
         (print-arglist ,%arglist ,%stream))
       (print-end-bullet ,%stream)
       (with-local-references (if (member (reference-locative-type ,%reference)
                                          '(section glossary-term))
                                  ;; See @SUPPRESSED-LINKS.
                                  ()
                                  ,%reference)
         ,@body))))
(autoload print-reference-bullet '#:mgl-pax/document :export nil)
(declaim (ftype function print-arglist))
(declaim (ftype function print-end-bullet))

(declaim (special *local-references*))
(defmacro with-local-references (refs &body body)
  `(let ((*local-references* (append (ensure-list ,refs) *local-references*)))
     ,@body))

(defun ensure-list (object)
  (if (listp object)
      object
      (list object)))

(defmacro with-dislocated-objects (objects &body body)
  "For each object in OBJECTS, establish a [local
  reference][@local-references] with the DISLOCATED locative, which
  [prevents autolinking][@preventing-autolinking]."
  `(with-local-references (mapcar (lambda (object)
                                    (make-reference object 'dislocated))
                                  (ensure-list ,objects))
     ,@body))

(autoload document-docstring '#:mgl-pax/document)
(autoload documentation* '#:mgl-pax/document)
(autoload escape-markdown '#:mgl-pax/document)
(autoload prin1-to-markdown '#:mgl-pax/document)


;;;; Early non-exported definitions

;;; These are used only by the DOCUMENT-OBJECT for CLASSes.
(declaim (ftype function global-reference-p))
(declaim (ftype function link-to-reference))

;;; We need this for more informative TRANSCRIBE-ERRORs ...
(defvar *reference-being-documented* nil)
;;; ... and this for detecting whether we are in a DOCUMENT call.
(defvar *objects-being-documented* ())


(defsection @github-workflow (:title "Github Workflow")
  "It is generally recommended to commit generated readmes (see
  UPDATE-ASDF-SYSTEM-READMES), so that users have something to read
  without reading the code and sites like github can display them.

  HTML documentation can also be committed, but there is an issue with
  that: when linking to the sources (see MAKE-GITHUB-SOURCE-URI-FN),
  the commit id is in the link. This means that code changes need to
  be committed first, and only then can HTML documentation be
  regenerated and committed in a followup commit.

  The second issue is that github is not very good at serving HTMLs
  files from the repository itself (and
  [http://htmlpreview.github.io](http://htmlpreview.github.io) chokes
  on links to the sources).

  The recommended workflow is to use
  [gh-pages](https://pages.github.com/), which can be made relatively
  painless with the `git worktree` command. The gist of it is to make
  the `doc/` directory a checkout of the branch named `gh-pages`. A
  good description of this process is
  [http://sangsoonam.github.io/2019/02/08/using-git-worktree-to-deploy-github-pages.html](http://sangsoonam.github.io/2019/02/08/using-git-worktree-to-deploy-github-pages.html).
  Two commits needed still, but it is somewhat less painful.

  This way the HTML documentation will be available at
  `http://<username>.github.io/<repo-name>`. It is probably a good
  idea to add sections like the @LINKS section to allow jumping
  between the repository and the gh-pages site."
  (make-github-source-uri-fn function))

(defun make-github-source-uri-fn (asdf-system github-uri &key git-version)
  "Return a function suitable as :SOURCE-URI-FN of a page spec (see
  the PAGES argument of DOCUMENT). The function looks the source
  location of the reference passed to it, and if the location is
  found, the path is made relative to the root directory of
  ASDF-SYSTEM and finally an URI pointing to github is returned. The
  URI looks like this:

      https://github.com/melisgl/mgl-pax/blob/master/src/pax-early.lisp#L12

  \"master\" in the above link comes from GIT-VERSION.

  If GIT-VERSION is NIL, then an attempt is made to determine to
  current commit id from the `.git` in the directory holding
  ASDF-SYSTEM. If no `.git` directory is found, then no links to
  github will be generated.

  A separate warning is signalled whenever source location lookup
  fails or if the source location points to a directory not below the
  directory of ASDF-SYSTEM."
  (let* ((git-version (or git-version (asdf-system-git-version asdf-system)))
         (system-dir (asdf:system-relative-pathname asdf-system "")))
    (if git-version
        (let ((line-file-position-cache (make-hash-table :test #'equal))
              (find-source-cache (make-hash-table :test #'equal)))
          (lambda (reference)
            (let ((*find-source-cache* find-source-cache))
              (multiple-value-bind (relative-path line-number)
                  (convert-source-location (find-source reference)
                                           system-dir reference
                                           line-file-position-cache)
                (when relative-path
                  (format nil "~A/blob/~A/~A#L~S" github-uri git-version
                          relative-path (1+ line-number)))))))
        (warn "No GIT-VERSION given and can't find .git directory ~
              for ASDF system~% ~A. Links to github will not be generated."
              (asdf:component-name (asdf:find-system asdf-system))))))

(defun asdf-system-git-version (system)
  (let ((git-dir
          (merge-pathnames (make-pathname :directory '(:relative ".git"))
                           (asdf:system-relative-pathname
                            (asdf:component-name (asdf:find-system system))
                            ""))))
    (if (uiop/filesystem:directory-exists-p git-dir)
        (git-version git-dir)
        nil)))

(defun git-version (git-dir)
  (multiple-value-bind (version error-output exit-code)
      (uiop:run-program (list "git" "-C" (namestring git-dir)
                              "rev-parse" "HEAD")
                        :output '(:string :stripped t) :ignore-error-status t)
    (declare (ignore error-output))
    (if (zerop exit-code)
        version
        nil)))

(defun convert-source-location (source-location system-dir reference
                                line-file-position-cache)
  (cond ((or
          ;; CCL
          (null source-location)
          ;; SBCL, AllegroCL
          (eq (first source-location) :error))
         (warn "~@<No source location found for reference ~:_~A: ~:_~A~%~@:>"
               reference (second source-location)))
        (t
         (assert (eq (first source-location) :location))
         (let* ((filename (second (assoc :file (rest source-location))))
                (position (second (assoc :position (rest source-location))))
                (relative-path (and filename
                                    (enough-namestring filename system-dir))))
           (if (and relative-path
                    (uiop/pathname:relative-pathname-p relative-path))
               (let ((line-number (file-position-to-line-number
                                   filename position
                                   line-file-position-cache)))
                 (if line-number
                     (values relative-path line-number)
                     (warn "~@<Source location information in file ~S ~
                            is out of date.~@:>" filename)))
               (warn "~@<Source location for ~S is not below system ~
                      directory ~S.~%~@:>" reference system-dir))))))

(defun file-position-to-line-number (filename file-position cache)
  (if (null file-position)
      0
      (let ((line-file-positions (or (gethash filename cache)
                                     (setf (gethash filename cache)
                                           (line-file-positions filename)))))
        (loop for line-number upfrom 0
              for line-file-position in line-file-positions
              do (when (<= file-position line-file-position)
                   (return line-number))))))

;;; This is cached because it is determining the line number for a
;;; given file position would need to traverse the file, which is
;;; extremely expensive. Note that position 0 is not included, but
;;; FILE-LENGTH is.
(defun line-file-positions (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
          for line-number upfrom 0
          while line
          collect (file-position stream))))


(defsection @pax-world (:title "PAX World")
  """\PAX World is a registry of documents, which can generate
  cross-linked HTML documentation pages for all the registered
  documents."""
  (register-doc-in-pax-world function)
  """For example, this is how \PAX registers itself:"""
  (register-doc-example (include (:start (pax-sections function)
                                  :end (end-of-register-doc-example variable))
                                 :header-nl "```"
                                 :footer-nl "```"))
  (update-pax-world function))

(defvar *registered-pax-world-docs* ())

(defun register-doc-in-pax-world (name sections page-specs)
  """Register SECTIONS and PAGE-SPECS under NAME in \PAX World. By
  default, UPDATE-PAX-WORLD generates documentation for all of these."""
  (setq *registered-pax-world-docs*
        (remove name *registered-pax-world-docs* :key #'first))
  (push (list name sections page-specs) *registered-pax-world-docs*))

;;; Register PAX itself.
(defun pax-sections ()
  (list @pax-manual))
(defun pax-pages ()
  `((:objects
     (, @pax-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :mgl-pax
                      "https://github.com/melisgl/mgl-pax"))))
(register-doc-in-pax-world :pax (pax-sections) (pax-pages))
(defvar end-of-register-doc-example)
