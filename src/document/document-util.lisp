(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @documentation-utilities
    (:title "Utilities for Generating Documentation")
  "Two convenience functions are provided to serve the common case of
  having an ASDF system with some readmes and a directory with for the
  HTML documentation and the default css stylesheet."
  (update-asdf-system-readmes function)
  (update-asdf-system-html-docs function)
  (*document-html-max-navigation-table-of-contents-level* variable)
  (*document-html-top-blocks-of-links* variable)
  (*document-html-bottom-blocks-of-links* variable)
  (@github-workflow section)
  (@pax-world section))

(defparameter *default-output-options*
  '(:if-does-not-exist :create
    :if-exists :supersede
    :ensure-directories-exist t))

(defun/autoloaded update-asdf-system-readmes (object asdf-system &key
                                   (url-versions '(1)))
  "Convenience function to generate two readme files in the directory
  holding the ASDF-SYSTEM definition. OBJECT is passed on to DOCUMENT.

  `README.md` has anchors, links, inline code, and other markup added.
  Not necessarily the easiest on the eye in an editor, but looks good
  on github.

  `README` is optimized for reading in text format. Has no links and
  less cluttery markup.

  Example usage:

  ```
  (update-asdf-system-readmes @pax-manual :mgl-pax)
  ```

  Note that *DOCUMENT-URL-VERSIONS* is bound to URL-VERSIONS, that
  defaults to using the uglier version 1 style of URL for the sake of
  github."
  (with-open-file (stream (asdf:system-relative-pathname
                           asdf-system "README.md")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (let ((*document-url-versions* url-versions))
      (document object :stream stream))
    (print-markdown-footer stream))
  (with-open-file (stream (asdf:system-relative-pathname
                           asdf-system "README")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (let ((*document-uppercase-is-code* nil)
          (*document-link-code* nil)
          (*document-link-sections* nil)
          (*document-mark-up-signatures* nil)
          (*document-max-numbering-level* 0)
          (*document-max-table-of-contents-level* 0)
          (*document-text-navigation* nil)
          ;; Some Lisps bind it to T in DESCRIBE, some don't.
          (*print-circle* nil))
      (document object :stream stream))
    (print-markdown-footer stream)))

(defun print-markdown-footer (stream)
  (format stream "~%* * *~%")
  (format stream "###### \\[generated by ~
                 [MGL-PAX](https://github.com/melisgl/mgl-pax)\\]~%"))


(defun/autoloaded update-asdf-system-html-docs
    (sections asdf-system &key pages
              (target-dir (asdf:system-relative-pathname
                           asdf-system "doc/"))
              (update-css-p t))
  "Generate pretty HTML documentation for a single ASDF system,
  possibly linking to github. If UPDATE-CSS-P, copy the CSS style
  sheet to TARGET-DIR, as well. Example usage:

  ```commonlisp
  (update-asdf-system-html-docs @pax-manual :mgl-pax)
  ```

  The same, linking to the sources on github:

  ```commonlisp
  (update-asdf-system-html-docs
    @pax-manual :mgl-pax
    :pages
    `((:objects
      (,mgl-pax::@pax-manual)
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
  (loop for file in '("src/document/jquery.min.js"
                      "src/document/toc.min.js"
                      "src/document/style.css")
        do (let ((target-file (merge-pathnames (file-namestring file)
                                               target-dir)))
             (uiop:delete-file-if-exists target-file)
             (uiop:copy-file (asdf:system-relative-pathname :mgl-pax file)
                             target-file))))

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
    src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML">
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


;;;; The autoloaded part of @PAX-WORLD

(defun/autoloaded update-pax-world (&key (docs *registered-pax-world-docs*)
                                         dir)
  "Generate HTML documentation for all DOCS. Files are created in
  DIR (`(asdf:system-relative-pathname :mgl-pax \"world/\")` by
  default if DIR is NIL). DOCS is a list of entries of the form (NAME
  SECTIONS PAGE-SPECS). The default for DOCS is all the sections and
  pages registered with REGISTER-DOC-IN-PAX-WORLD.

  In the absence of :HEADER-FN :FOOTER-FN, :OUTPUT, every spec in
  PAGE-SPECS is augmented with HTML headers, footers and output
  location specifications (based on the name of the section).

  If necessary a default page spec is created for every section."
  (let ((dir (or dir (asdf:system-relative-pathname :mgl-pax "world/"))))
    (multiple-value-bind (sections pages) (sections-and-pages docs)
      (create-pax-world sections pages dir t))))

(defun sections-and-pages (registered-docs)
  (values (apply #'append (mapcar #'second registered-docs))
          (apply #'append (mapcar #'third registered-docs))))

;;; This section is not in the documentation of PAX-WORLD itself. It
;;; is dynamically extended with the list of sections for which
;;; UPDATE-PAX-WORLD was called. FIXME: this is not thread-safe.
(defsection @pax-world-dummy (:title "PAX World")
  "This is a list of documents generated with MGL-PAX in the default
  style. The documents are cross-linked: links to other documents are
  added automatically when a reference is found. Note that clicking on
  the locative type (e.g. `[function]`) will take you to the sources
  on github if possible.")

(defun create-pax-world (sections page-specs dir update-css-p)
  (set-pax-world-list sections)
  (document-html (cons @pax-world-dummy sections)
                 (cons `(:objects
                         ,(list @pax-world-dummy)
                         :output (,(merge-pathnames "index.html" dir)
                                  ,@*default-output-options*))
                       page-specs)
                 dir update-css-p t))

(defun set-pax-world-list (objects)
  (setf (slot-value @pax-world-dummy 'entries)
        (list
         ;; This is the docstring of @PAX-WORLD-DUMMY above.
         (first (section-entries @pax-world-dummy))
         (let ((objects (sort (copy-seq objects) #'string<
                              :key #'section-title-or-name)))
           (with-output-to-string (stream)
             (dolist (object objects)
               (format stream "- ~S~%~%" (section-name object))))))))


;;;; Generate the READMEs and HTML docs.

#+nil
(time
 (progn
   (asdf:load-system :mgl-pax/full)
   (update-asdf-system-readmes (pax-sections) :mgl-pax)
   (let ((*document-downcase-uppercase-code* t))
     (update-asdf-system-html-docs (pax-sections)
                                   :mgl-pax :pages (pax-pages)))))


;;; Load systems that use PAX and generate PAX World in
;;; <mgl-pax-asdf-system-dir>/world/. To update
;;; https://github.com/melisgl/mgl-pax-world, check out its gh-pages
;;; branch in that directory, update pax world, commit and push the
;;; changes to github.
(defun update-pax-world* ()
  ;; KLUDGE: Bind *READTABLE* so that when evaluating in Slime (e.g.
  ;; with C-x C-e) the file's readtable is not used (which leads to a
  ;; reader macro conflict with CL-SYNTAX).
  (let ((*readtable* (named-readtables:find-readtable :standard)))
    (asdf:load-system :mgl-mat)
    (asdf:load-system :named-readtables/doc)
    (asdf:load-system :micmac)
    (asdf:load-system :mgl-gpr)
    (asdf:load-system :mgl)
    (asdf:load-system :journal)
    (asdf:load-system :trivial-utf-8/doc)
    (asdf:load-system :try/doc)
    (asdf:load-system :lmdb))
  (let ((*document-downcase-uppercase-code* t))
    (update-pax-world)))

#+nil
(time (update-pax-world*))
