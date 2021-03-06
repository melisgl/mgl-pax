(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @mgl-pax-generating-documentation
    (:title "Generating Documentation")
  "Two convenience functions are provided to serve the common case of
  having an ASDF system with some readmes and a directory with for the
  HTML documentation and the default css stylesheet."
  (update-asdf-system-readmes function)
  (update-asdf-system-html-docs function)
  (*document-html-max-navigation-table-of-contents-level* variable)
  (*document-html-top-blocks-of-links* variable)
  (*document-html-bottom-blocks-of-links* variable)
  (@mgl-pax-github-workflow section)
  (@mgl-pax-world section))


(defparameter *default-output-options*
  '(:if-does-not-exist :create
    :if-exists :supersede
    :ensure-directories-exist t))

(defun update-asdf-system-readmes (sections asdf-system)
  "Convenience function to generate two readme files in the directory
  holding the ASDF-SYSTEM definition.

  README.md has anchors, links, inline code, and other markup added.
  Not necessarily the easiest on the eye in an editor, but looks good
  on github.

  README is optimized for reading in text format. Has no links and
  cluttery markup.

  Example usage:

  ```
  (update-asdf-system-readmes @mgl-pax-manual :mgl-pax)
  ```"
  (with-open-file (stream (asdf:system-relative-pathname
                           asdf-system "README.md")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (document sections :stream stream)
    (print-markdown-footer stream))
  (with-open-file (stream (asdf:system-relative-pathname
                           asdf-system "README")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (loop for section in (alexandria:ensure-list sections) do
      (describe section stream))
    (print-markdown-footer stream)))

(defun add-markdown-defaults-to-page-specs (sections page-specs dir)
  (flet ((section-has-page-spec-p (section)
           (some (lambda (page-spec)
                   (member section (getf page-spec :objects)))
                 page-specs)))
    (mapcar (lambda (page-spec)
              (add-markdown-defaults-to-page-spec page-spec dir))
            (append page-specs
                    (mapcar (lambda (section)
                              `(:objects (,section)))
                            (remove-if #'section-has-page-spec-p sections))))))

(defun add-markdown-defaults-to-page-spec (page-spec filename)
  `(,@page-spec
    ,@(unless (getf page-spec :output)
        `(:output (,filename ,@*default-output-options*)))
    ,@(unless (getf page-spec :footer-fn)
        `(:footer-fn ,#'print-markdown-footer))))

(defun print-markdown-footer (stream)
  (format stream "~%* * *~%")
  (format stream "###### \\[generated by ~
                 [MGL-PAX](https://github.com/melisgl/mgl-pax)\\]~%"))


(defun update-asdf-system-html-docs (sections asdf-system &key pages
                                     (target-dir (asdf:system-relative-pathname
                                                  asdf-system "doc/"))
                                     (update-css-p t))
  "Generate pretty HTML documentation for a single ASDF system,
  possibly linking to github. If UPDATE-CSS-P, copy the CSS style
  sheet to TARGET-DIR, as well. Example usage:

  ```commonlisp
  (update-asdf-system-html-docs @mgl-pax-manual :mgl-pax)
  ```

  The same, linking to the sources on github:

  ```commonlisp
  (update-asdf-system-html-docs
    @mgl-pax-manual :mgl-pax
    :pages
    `((:objects
      (,mgl-pax:@mgl-pax-manual)
      :source-uri-fn ,(make-github-source-uri-fn
                       :mgl-pax
                       \"https://github.com/melisgl/mgl-pax\"))))
  ```"
  (document-html sections pages target-dir update-css-p nil))

;;; Generate with the default HTML look
(defun document-html (sections page-specs target-dir update-css-p
                      link-to-pax-world-p)
  (when update-css-p
    (copy-css target-dir))
  (document sections
            :pages (add-html-defaults-to-page-specs
                    (alexandria:ensure-list sections)
                    page-specs target-dir link-to-pax-world-p)
            :format :html))

(defun add-html-defaults-to-page-specs (sections page-specs dir
                                        link-to-pax-world-p)
  (flet ((section-has-page-spec-p (section)
           (some (lambda (page-spec)
                   (member section (getf page-spec :objects)))
                 page-specs)))
    (mapcar (lambda (page-spec)
              (add-html-defaults-to-page-spec page-spec dir
                                              link-to-pax-world-p))
            (append page-specs
                    (mapcar (lambda (section)
                              `(:objects (,section)))
                            (remove-if #'section-has-page-spec-p sections))))))

(defun add-html-defaults-to-page-spec (page-spec dir link-to-pax-world-p)
  (let* ((objects (getf page-spec :objects))
         (section (if (and (= 1 (length objects))
                           (typep (first objects) 'section))
                      (first objects)
                      nil))
         (title (if section
                    (section-title section)
                    nil))
         (filename (sections-to-filename objects dir)))
    (flet ((header (stream)
             (html-header stream :title title
                          :stylesheet "style.css" :charset "UTF-8"
                          :link-to-pax-world-p link-to-pax-world-p))
           (footer (stream)
             (html-footer stream)))
      `(,@page-spec
        ,@(unless (getf page-spec :output)
            `(:output (,filename ,@*default-output-options*)))
        ,@(unless (getf page-spec :header-fn)
            `(:header-fn ,#'header))
        ,@(unless (getf page-spec :footer-fn)
            `(:footer-fn ,#'footer))))))

(defun sections-to-filename (sections dir)
  (flet ((name (section)
           (string-downcase
            (remove-special-chars (symbol-name (section-name section))))))
    (merge-pathnames (format nil "~{~A~^-~}.html"
                             (mapcar #'name sections))
                     dir)))

(defun remove-special-chars (string)
  (remove-if (lambda (char)
               (find char "!@#$%^&*"))
             string))

(defun copy-css (target-dir)
  (ensure-directories-exist target-dir)
  (loop for file in '("src/jquery.min.js" "src/toc.min.js" "src/style.css")
        do (uiop:copy-file (asdf:system-relative-pathname :mgl-pax file)
                           (merge-pathnames (file-namestring file)
                                            target-dir))))

(defvar *document-html-top-blocks-of-links* ()
  "A list of blocks of links to be display on the sidebar on the left,
  above the table of contents. A block is of the form `(&KEY TITLE ID
  LINKS)`, where TITLE will be displayed at the top of the block in a
  HTML `DIV` with `ID`, followed by the links. LINKS is a list
  of `(URI LABEL) elements.`")

(defvar *document-html-bottom-blocks-of-links* ()
  "Like *DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*, only it is displayed
  below the table of contents.")

(defun html-header
    (stream &key title stylesheet (charset "UTF-8")
     link-to-pax-world-p
     (top-blocks-of-links *document-html-top-blocks-of-links*)
     (bottom-blocks-of-links *document-html-bottom-blocks-of-links*))
  (format
   stream
   """<!DOCTYPE html>~%~
   <html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'>~%~
   <head>~%~
   ~@[<title>~A</title>~]~%~
   ~@[<link type='text/css' href='~A' rel='stylesheet'/>~]~%~
   ~@[<meta http-equiv="Content-Type" ~
            content="text/html; ~
   charset=~A"/>~]~%~
   <script src="jquery.min.js"></script>~%~
   <script src="toc.min.js"></script>~%~
   <script type="text/x-mathjax-config">
     MathJax.Hub.Config({
       tex2jax: {
         inlineMath: [['$','$']],
         processEscapes: true
       }
     });
   </script>
   <script type="text/javascript" ~
    src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML">
   </script>
   </head>~%~
   <body>~%~
   <div id="content-container">~%~
     <div id="toc">~%~
       ~A~
       ~:[~;<div id="toc-header"><ul><li><a href="index.html">~
            PAX World</a></li></ul></div>~%~]~
       <div id="page-toc">~%~
       </div>~%~
       ~A~
       <div id="toc-footer">~
         <ul><li><a href="https://github.com/melisgl/mgl-pax">[generated ~
             by MGL-PAX]</a></li></ul>~
       </div>~%~
     </div>~%~
     <div id="content">~%"""
   title stylesheet charset
   (blocks-of-links-to-html-string top-blocks-of-links)
   link-to-pax-world-p
   (blocks-of-links-to-html-string bottom-blocks-of-links)))

(defun blocks-of-links-to-html-string (blocks-of-links)
  (format nil "~{~A~}" (mapcar #'block-of-links-to-html-string
                               blocks-of-links)))

(defun block-of-links-to-html-string (block-of-links)
  (destructuring-bind (&key title id links) block-of-links
    (with-output-to-string (stream)
      (format stream "<div class=\"menu-block\"")
      (when id
        (format stream " id=\"~A\"" id))
      (format stream ">")
      (when title
        (format stream "<span class=\"menu-block-title\">~A</span>" title))
      (format stream "<ul>")
      (dolist (link links)
        (format stream "<li><a href=\"~A\">~A</a></li>"
                (first link)
                (second link)))
      (princ "</ul></div>" stream))))

(defvar *google-analytics-id* nil)

(defun html-footer (stream &key (google-analytics-id *google-analytics-id*))
  (format
   stream
   "  </div>~%~
   </div>~%~
   <script>$('#page-toc').toc(~A);</script>~%~
   ~:[~;<script>
   (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){~
   (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement~
   (o),m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.~
   insertBefore(a,m)})(window,document,'script','//www.google-analytics.com/~
   analytics.js','ga');ga('create', '~A', 'auto');ga('send', 'pageview');~
   </script>~%~]</body>~%</html>~%"
   (toc-options)
   google-analytics-id google-analytics-id))

(defvar *document-html-max-navigation-table-of-contents-level* nil
  "NIL or a non-negative integer. If non-NIL, it overrides
  *DOCUMENT-MAX-NUMBERING-LEVEL* in dynamic HTML table of contents on
  the left of the page.")

(defun toc-options ()
  (let ((max-level (or *document-html-max-navigation-table-of-contents-level*
                       *document-max-table-of-contents-level*)))
    (format nil "{'selectors': '~{~A~^,~}'}"
            (loop for i upfrom 1 upto (1+ max-level)
                  collect (format nil "h~S" i)))))



(defsection @mgl-pax-github-workflow (:title "Github Workflow")
  "It is generally recommended to commit generated readmes (see
  UPDATE-ASDF-SYSTEM-READMES) so that users have something to read
  without reading the code and sites like github can display them.

  HTML documentation can also be committed, but there is an issue with
  that: when linking to the sources (see MAKE-GITHUB-SOURCE-URI-FN),
  the commit id is in the link. This means that code changes need to
  be committed first, then HTML documentation regenerated and
  committed in a followup commit.

  The second issue is that github is not very good at serving HTMLs
  files from the repository itself (and
  [http://htmlpreview.github.io](http://htmlpreview.github.io) chokes
  on links to the sources).

  The recommended workflow is to use
  [gh-pages](https://pages.github.com/), which can be made relatively
  painless with the `git workflow` command. The gist of it is to make
  the `doc/` directory a checkout of the branch named `gh-pages`. A
  good description of this process is
  [http://sangsoonam.github.io/2019/02/08/using-git-worktree-to-deploy-github-pages.html](http://sangsoonam.github.io/2019/02/08/using-git-worktree-to-deploy-github-pages.html).
  Two commits needed still, but it is somewhat less painful.

  This way the HTML documentation will be available at
  `http://<username>.github.io/<repo-name>`. It is probably a good
  idea to add section like the @MGL-PAX-LINKS section to allow jumping
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
                  (convert-source-location (find-source
                                            (mgl-pax:resolve reference))
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
    (if (probe-file git-dir)
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
           (if (and relative-path (cl-fad:pathname-relative-p relative-path))
               (values relative-path
                       (file-position-to-line-number filename position
                                                     line-file-position-cache))
               (warn "Source location for ~S is not below system ~
                     directory ~S.~%" reference system-dir))))))

(defun file-position-to-line-number (filename file-position cache)
  (if (null file-position)
      0
      (let ((line-file-positions (or (gethash filename cache)
                                     (setf (gethash filename cache)
                                           (line-file-positions filename)))))
        (loop for line-number upfrom 0
              for line-file-position in line-file-positions
              do (when (< file-position line-file-position)
                   (return line-number))))))

;;; This is cached because it is determining the line number for a
;;; given file position would need to traverse the file, which is
;;; extremely expesive. Note that position 0 is not included, but
;;; FILE-LENGTH is.
(defun line-file-positions (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
          for line-number upfrom 0
          while line
          collect (file-position stream))))


(defsection @mgl-pax-world (:title "PAX World")
  "PAX World is a registry of documents, which can generate
  cross-linked HTML documentation pages for all the registered
  documents."
  (register-doc-in-pax-world function)
  "For example, this is how PAX registers itself:"
  (register-doc-example (include (:start (pax-sections function)
                                  :end (end-of-register-doc-example variable))
                                 :header-nl "```commonlisp"
                                 :footer-nl "```"))
  (update-pax-world function))

(defvar *registered-pax-world-docs* ())

(defun register-doc-in-pax-world (name sections page-specs)
  "Register SECTIONS and PAGE-SPECS under NAME in PAX World. By
  default, UPDATE-PAX-WORLD generates documentation for all of these."
  (setq *registered-pax-world-docs*
        (remove name *registered-pax-world-docs* :key #'first))
  (push (list name sections page-specs) *registered-pax-world-docs*))

;;; Register PAX itself.
(defun pax-sections ()
  (list @mgl-pax-manual))
(defun pax-pages ()
  `((:objects
     (,mgl-pax:@mgl-pax-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :mgl-pax
                      "https://github.com/melisgl/mgl-pax"))))
(register-doc-in-pax-world :mgl-pax (pax-sections) (pax-pages))
(defvar end-of-register-doc-example)

(defvar *pax-world-dir* nil
  "The default location to which to write the generated documentation.
  Defaults to:

  ```commonlisp
  (asdf:system-relative-pathname :mgl-pax \"world/\")
  ```")

(defun update-pax-world (&key docs dir)
  "Generate HTML documentation for all DOCS. By default, files are
  created in *PAX-WORLD-DIR* or `(asdf:system-relative-pathname
  :mgl-pax \"world/\")`, if NIL. DOCS is a list of entries of the
  form (NAME SECTIONS PAGE-SPECS). The default for DOCS is all the
  sections and pages registered with REGISTER-DOC-IN-PAX-WORLD.

  In the absence of :HEADER-FN :FOOTER-FN, :OUTPUT, every spec in
  PAGE-SPECS is augmented with HTML headers, footers and output
  location specifications (based on the name of the section).

  If necessary a default page spec is created for every section."
  (let ((dir (or dir (asdf:system-relative-pathname :mgl-pax "world/")))
        (docs (or docs (sort (copy-seq *registered-pax-world-docs*) #'string<
                             :key (lambda (entry)
                                    (string (first entry)))))))
    (multiple-value-bind (sections pages) (sections-and-pages docs)
      (create-pax-world sections pages dir t))))

(defun sections-and-pages (registered-docs)
  (values (apply #'append (mapcar #'second registered-docs))
          (apply #'append (mapcar #'third registered-docs))))

;;; This section is not in the documentation of PAX-WORLD itself. It
;;; is dynamically extended with the list of sections for which
;;; UPDATE-PAX-WORLD was called. FIXME: this is not thread-safe.
(defsection @mgl-pax-world-dummy (:title "PAX World")
  "This is a list of documents generated with MGL-PAX in the default
  style. The documents are cross-linked: links to other documents are
  added automatically when a reference is found. Note that clicking on
  the locative type (e.g. `[function]`) will take you to the sources
  on github if possible.")

(defun create-pax-world (sections page-specs dir update-css-p)
  (set-pax-world-list sections)
  (document-html (cons @mgl-pax-world-dummy sections)
                 (cons `(:objects
                         ,(list @mgl-pax-world-dummy)
                         :output (,(merge-pathnames "index.html" dir)
                                  ,@*default-output-options*))
                       page-specs)
                 dir update-css-p t))

(defun set-pax-world-list (objects)
  (setf (slot-value @mgl-pax-world-dummy 'mgl-pax::entries)
        (list (first (section-entries @mgl-pax-world-dummy))
              (with-output-to-string (stream)
                (dolist (object objects)
                  (format stream "- ~S~%~%" (section-name object)))))))

#+nil
(progn
  (update-asdf-system-readmes (pax-sections) :mgl-pax)
  (update-asdf-system-html-docs (pax-sections) :mgl-pax :pages (pax-pages)))

#+nil
(progn
  (asdf:load-system :mgl-mat)
  (asdf:load-system :named-readtables/doc)
  (asdf:load-system :micmac)
  (asdf:load-system :mgl-gpr)
  (asdf:load-system :mgl)
  (asdf:load-system :journal)
  (asdf:load-system :trivial-utf-8/doc))

#+nil
(update-pax-world)
