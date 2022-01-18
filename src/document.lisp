(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @mgl-pax-generating-documentation
    (:title "Generating Documentation")
  (document function)
  (mgl-pax/document asdf:system)
  (@mgl-pax-markdown-support section)
  (@mgl-pax-codification section)
  (@mgl-pax-linking-to-code section)
  (@mgl-pax-linking-to-sections section)
  (@mgl-pax-miscellaneous-documentation-printer-variables section)
  (@mgl-pax-documentation-utilities section)
  (@mgl-pax-document-implementation-notes section))

;;; A PAGE is basically a single markdown or html file, to where the
;;; documentation of some references is written. See the DOCUMENT
;;; function.
;;;
;;; Documentation starts out being sent to a certain stream, but the
;;; output is redirected to different stream if it is for a reference
;;; on the current page (see COLLECT-REACHABLE-OBJECTS,
;;; PAGE-REFERENCES). This stream - to which the temporary markdown
;;; output written - is given by TEMP-STREAM-SPEC that's a stream spec
;;; to allow it to
;;;
;;; - be created lazily so that no stray files are left around and
;;;   only a small number of fds are needed even for a huge project,
;;;
;;; - be opened multiple times (which is not a given for string
;;;   streams)
;;;
;;; So output is generated in markdown format to TEMP-STREAM-SPEC, but
;;; before we are done it is converted to the requested output format
;;; and HEADER-FN, FOOTER-FN are called to write arbitrary leading and
;;; trailing content to the final stream.
;;;
;;; Finally, URI-FRAGMENT is a string such as "doc/manual.html" that
;;; specifies where the page will be deployed on a webserver. It
;;; defines how links between pages will look. If it's not specified
;;; and OUTPUT refers to a file, then it defaults to the name of the
;;; file. If URI-FRAGMENT is NIL, then no links will be made to or
;;; from that page.
(defstruct page
  references
  temp-stream-spec
  final-stream-spec
  uri-fragment
  header-fn
  footer-fn
  source-uri-fn)

;;; The current page where output is being sent.
(defvar *page* nil)

;;; This is a possible link to REFERENCE on PAGE (note that the same
;;; REFERENCE may be written to multiple pages). ID is the markdown
;;; reference link id and PAGE-TO-N-USES is a hash table that counts
;;; how many times this was linked to from each page.
;;;
;;; PAGE may also be a string denoting a URL. This is used to link to
;;; the hyperspec.
(defstruct link
  reference
  (page nil :type (or page string))
  id
  page-to-n-uses)

;;; A list of LINK objects representing all possible things which may
;;; be linked to. Bound only once (by WITH-PAGES) after pages are
;;; created (to the list of links to all reachable references from all
;;; the objects being documented). If a reference occurs multiple
;;; times, earlier links (thus pages earlier in DOCUMENT's PAGES
;;; argument) have precedence.
(defparameter *links* ())

(defun find-link-by-id (id)
  (find id *links* :key #'link-id :test #'equal))

(defun find-link (reference)
  (find reference *links* :key #'link-reference :test #'reference=))

;;; Return the unescaped name of the HTML anchor for REFERENCE. See
;;; HTML-SAFE-NAME.
(defun reference-to-anchor (reference &key canonicalp)
  (let ((reference (if canonicalp
                       reference
                       (canonical-reference reference))))
    (with-standard-io-syntax*
      (prin1-to-string (list (reference-object reference)
                             (reference-locative reference))))))

;;; For the link to REFERENCE, increment the link counter for the
;;; current page and return the link id.
(defun link-to-reference (reference)
  (let ((link (find-link reference)))
    (when (and link
               (or (eq *page* (link-page link))
                   (stringp (link-page link))
                   (and (page-uri-fragment *page*)
                        (page-uri-fragment (link-page link)))))
      (incf (gethash *page* (link-page-to-n-uses link) 0))
      (format nil "~A" (link-id link)))))

(defun link-used-on-current-page-p (link)
  (plusp (gethash *page* (link-page-to-n-uses link) 0)))

(defun reference-page (reference)
  (let ((link (find-link reference)))
    (when link
      (link-page link))))

;;; A list of all the references extracted from *LINKS* for
;;; convenience.
(defparameter *references*
  ())

;;; A list of references not to be autolinked. See
;;; FILTER-REFERENCES-FOR-SYMBOL,
;;; FILTER-REFERENCES-FOR-UNSPECIFIED-LOCATIVE, and
;;; FILTER-REFERENCES-FOR-SPECIFIED-LOCATIVE. The reference being
;;; documented is always on this list. Arguments are typically also
;;; are. Bound by WITH-LOCAL-REFERENCES.
(defparameter *local-references*
  ())

;;; Add a LINK to *LINKS* (and a REFERENCE to *REFERENCES*) for each
;;; reference in PAGE-REFERENCES of PAGE.
(defmacro with-pages ((pages) &body body)
  `(let ((*references* *references*)
         (*local-references* *local-references*)
         (*links* *links*))
     (initialize-links-and-references ,pages)
     (locally ,@body)))

(declaim (special *document-link-to-hyperspec*))

(defun initialize-links-and-references (pages)
  (with-standard-io-syntax*
    (loop for page in pages
          do (dolist (reference (page-references page))
               (unless (find-link reference)
                 (push reference *references*)
                 (push (make-link
                        :reference reference
                        :page page
                        :id (hash-link (reference-to-anchor reference)
                                       #'find-link-by-id)
                        :page-to-n-uses (make-hash-table))
                       *links*))))
    (when *document-link-to-hyperspec*
      (loop for (object locative url) in (hyperspec-external-references)
            do (let ((reference (make-reference object locative)))
                 (unless (find-link reference)
                   (push reference *references*)
                   (push (make-link
                          :reference reference
                          :page url
                          :id (hash-link
                               ;; KLUDGE: ABCL fails the
                               ;; TEST-HYPERSPEC test with a low-level
                               ;; error without :CANONICALP T.
                               (reference-to-anchor reference
                                                    :canonicalp t)
                               #'find-link-by-id)
                          :page-to-n-uses (make-hash-table))
                         *links*)))))))

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
               ((or (null ,page) (eq ,page *page*))
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
     ,@body))

(defvar *format*)
(setf (documentation '*format* 'variable)
      "Bound by DOCUMENT, this allows markdown output to depend on the
       output format.")

(declaim (special *document-normalize-packages*))
(declaim (special *table-of-contents-stream*))
(declaim (special *headings*))

(defmacro with-headings ((object) &body body)
  `(let ((*headings* (collect-headings ,object)))
     ,@body))

(defun document (object &key stream pages (format :markdown))
  """Write OBJECT in FORMAT to STREAM diverting some output to PAGES.
  FORMAT can be anything [3BMD][3bmd] supports, which is currently
  :MARKDOWN, :HTML and :PLAIN. STREAM may be a stream object, T or NIL
  as with CL:FORMAT.

  Most often, this function is called on section objects
  like `(DOCUMENT @MGL-PAX-MANUAL)`, but it supports all kinds of
  objects for which DOCUMENT-OBJECT is defined. To look up the
  documentation of function DOCUMENT:

      (document #'document)

  To generate the documentation for separate libraries with automatic
  cross-links:

      (document (list @cube-manual @mat-manual))

  Note that not only first class objects can have documentation. For
  instance, variables and deftypes are not represented by objects.
  That's why CL:DOCUMENTATION has a DOC-TYPE argument. DOCUMENT does
  not have anything like that, instead it relies on REFERENCE objects
  to carry the extra information. We are going to see later how
  references and locatives work. Until then, here is an example on how
  to look up the documentation of type `FOO`:

      (document (locate 'foo 'type))

  One can call DESCRIBE on [SECTION][class] objects to get
  documentation in markdown format with less markup than the default.
  See DESCRIBE-OBJECT `(METHOD () (SECTION T))`.

  There are quite a few special variables that affect how output is
  generated, see @MGL-PAX-CODIFICATION, @MGL-PAX-LINKING-TO-CODE,
  @MGL-PAX-LINKING-TO-SECTIONS, and
  @MGL-PAX-MISCELLANEOUS-DOCUMENTATION-PRINTER-VARIABLES.

  The rest of this description deals with how to generate multiple
  pages.

  The PAGES argument is to create multi-page documents by routing some
  of the generated output to files, strings or streams. PAGES is a
  list of page specification elements. A page spec is a plist with
  keys :OBJECTS, :OUTPUT, :URI-FRAGMENT, :SOURCE-URI-FN, :HEADER-FN
  and :FOOTER-FN. OBJECTS is a list of objects (references are allowed
  but not required) whose documentation is to be sent to `OUTPUT`.

  When documentation for an object is generated, the first matching
  page spec is used, where the object matches the page spec if it is
  contained in one of its :OBJECTS in the sense of
  COLLECT-REACHABLE-OBJECTS.

  `OUTPUT` can be a number things:

  - If it's a list whose first element is a string or a pathname, then
    output will be sent to the file denoted by that and the rest of
    the elements of the list are passed on as arguments to CL:OPEN.
    One extra keyword argument is :ENSURE-DIRECTORIES-EXIST. If it's
    true, ENSURE-DIRECTORIES-EXIST will be called on the pathname
    before it's opened.

  - If it's NIL, then output will be collected in a string.

  - If it's T, then output will be sent to *STANDARD-OUTPUT*.

  - If it's a stream, then output will be sent to that stream.

  If some pages are specified, DOCUMENT returns a list of designators
  for generated output. If a page whose `OUTPUT` refers to a file that
  was created (which doesn't happen if nothing would be written to
  it), then the corresponding pathname is included in the list. For
  strings the string itself, while for streams the stream object is
  included in the list. This way it's possible to write some pages to
  files and some to strings and have the return value indicate what
  was created. The output designators in the returned list are ordered
  by creation time.

  If no PAGES are specified, DOCUMENT returns a single pathname,
  string or stream object according to the value of the STREAM
  argument.

  Note that even if PAGES is specified, STREAM acts as a catch all
  taking the generated documentation for references not claimed by any
  pages. Also, the filename, string or stream corresponding to STREAM
  is always the first element in list of generated things that is the
  return value.

  HEADER-FN, if not NIL, is a function of a single stream argument,
  which is called just before the first write to the page. Since
  :FORMAT :HTML only generates HTML fragments, this makes it possible
  to print arbitrary headers, typically setting the title, css
  stylesheet, or charset.

  FOOTER-FN is similar to HEADER-FN, but it's called after the last
  write to the page. For HTML, it typically just closes the body.

  URI-FRAGMENT is a string such as `"doc/manual.html"` that specifies
  where the page will be deployed on a webserver. It defines how links
  between pages will look. If it's not specified and `OUTPUT` refers
  to a file, then it defaults to the name of the file. If URI-FRAGMENT
  is NIL, then no links will be made to or from that page.

  Finally, SOURCE-URI-FN is a function of a single, REFERENCE
  argument. If it returns a value other than NIL, then it must be a
  string representing an URI. If FORMAT is :HTML and
  *DOCUMENT-MARK-UP-SIGNATURES* is true, then the locative as
  displayed in the signature will be a link to this uri. See
  MAKE-GITHUB-SOURCE-URI-FN.

  PAGES may look something like this:

  ```commonlisp
  `((;; The section about SECTIONs and everything below it ...
     :objects (, @mgl-pax-sections)
     ;; ... is so boring that it's not worth the disk space, so
     ;; send it to a string.
     :output (nil)
     ;; Explicitly tell other pages not to link to these guys.
     :uri-fragment nil)
    ;; Send the @MGL-PAX-EXTENSION-API section and everything reachable
    ;; from it ...
    (:objects (, @mgl-pax-extension-api)
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
    (:objects (, @mgl-pax-manual)
     :output ("build/tmp/manual.html")
     ;; Links from the extension api page to the manual page will
     ;; be to ../user/pax-manual#<anchor>, while links going to
     ;; the opposite direction will be to
     ;; ../dev/pax-extension-api.html#<anchor>.
     :uri-fragment "doc/user/pax-manual.html"
     :header-fn 'write-html-header
     :footer-fn 'write-html-footer))
  ```"""
  (let ((*format* format)
        (*print-right-margin* (or *print-right-margin* 80))
        (*package* (if *document-normalize-packages*
                       (find-package :keyword)
                       *package*))
        (default-page (translate-page-spec
                       (list :objects (alexandria:ensure-list object)
                             :output (list stream))
                       format))
        (3bmd-code-blocks:*code-blocks* t)
        (3bmd-code-blocks:*code-blocks-default-colorize* :common-lisp)
        (3bmd-code-blocks::*colorize-name-map*
          (alexandria:plist-hash-table
           `("cl-transcript" :common-lisp
                             ,@(alexandria:hash-table-plist
                                3bmd-code-blocks::*colorize-name-map*))
           :test #'equal)))
    (with-tracking-pages-created ()
      (with-pages ((append (translate-page-specs pages format)
                           (list default-page)))
        (with-temp-output-to-page (stream default-page)
          (dolist (object (alexandria:ensure-list object))
            (with-headings (object)
              (document-object object stream))))
        (let ((outputs ()))
          (do-pages-created (page)
            (with-temp-output-to-page (stream page)
              (write-markdown-reference-style-link-definitions stream))
            (unless (eq format :markdown)
              (let ((markdown-string (with-temp-input-from-page (stream page)
                                       (read-stream-into-string stream))))
                (delete-stream-spec (page-temp-stream-spec page))
                (with-final-output-to-page (stream page)
                  (when (page-header-fn page)
                    (funcall (page-header-fn page) stream))
                  (3bmd:parse-string-and-print-to-stream markdown-string stream
                                                         :format format)
                  (when (page-footer-fn page)
                    (funcall (page-footer-fn page) stream)))))
            (push (unmake-stream-spec (page-final-stream-spec page)) outputs))
          (if (and stream (endp pages))
              (first outputs)
              (reverse outputs)))))))

;;; Emit markdown definitions for links (in *LINKS*) to REFERENCE
;;; objects that were linked to on the current page.
(defun write-markdown-reference-style-link-definitions (stream)
  (let ((used-links (sort (remove-if-not #'link-used-on-current-page-p *links*)
                          #'string< :key #'link-id)))
    (when used-links
      (format stream "~%")
      (dolist (link used-links)
        (let ((anchor (reference-to-anchor (link-reference link))))
          ;; The format is [label]: url "title"
          ;; E.g.  [1]: http://example.org/Hobbit#Lifestyle "Hobbit lifestyles"
          (if (stringp (link-page link))
              ;; Link to external page.
              (format stream "  [~A]: ~A ~S~%" (link-id link)
                      (link-page link) (princ-to-string anchor))
              ;; Link to documentation generated in the same run.
              (format stream "  [~A]: ~A#~A ~S~%"
                      (link-id link)
                      (relative-page-uri-fragment (link-page link) *page*)
                      (html-safe-name anchor)
                      (let ((object (resolve (link-reference link))))
                        (if (typep object 'section)
                            (section-title-or-name object)
                            (princ-to-string anchor))))))))))

(defun relative-page-uri-fragment (page reference-page)
  (if (eq page reference-page)
      ""
      (let ((fragment (page-uri-fragment page))
            (reference-fragment (page-uri-fragment reference-page)))
        (assert (and fragment reference-fragment))
        (relativize-pathname fragment reference-fragment))))


;;;; Page specs

;;; Convert the PAGES argument of DOCUMENT to PAGE objects.
(defun translate-page-specs (pages format)
  (mapcar (lambda (page) (translate-page-spec page format))
          pages))

(defun translate-page-spec (page format)
  (destructuring-bind (&key objects output header-fn footer-fn
                       (uri-fragment nil uri-fragment-p)
                       source-uri-fn)
      page
    (let ((stream-spec (apply #'make-stream-spec output)))
      (make-page
       :references (reachable-canonical-references objects)
       :temp-stream-spec (if (and (eq format :markdown)
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

(defun reachable-canonical-references (objects)
  (mapcan (lambda (object)
            (mapcar #'canonical-reference
                    (cons object (collect-reachable-objects object))))
          objects))


(defsection @mgl-pax-markdown-support (:title "Markdown Support")
  "The [Markdown][markdown] in docstrings is processed with the
  [3BMD][3bmd] library."
  (@mgl-pax-markdown-indentation section)
  (@mgl-pax-markdown-syntax-highlighting section)
  (@mgl-pax-mathjax section))

(defsection @mgl-pax-markdown-indentation (:title "Indentation")
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

  See [DOCUMENT-OBJECT][(method () (string t))] for the details.""")

(defsection @mgl-pax-markdown-syntax-highlighting (:title "Syntax Highlighting")
  "For syntax highlighting, github's [fenced code
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

(defsection @mgl-pax-mathjax (:title "MathJax")
  """Displaying pretty mathematics in TeX format is supported via
  MathJax. It can be done inline with `$` like this:

      $\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$

  which is diplayed as $\int_0^\infty e^{-x^2}
  dx=\frac{\sqrt{\pi}}{2}$, or it can be delimited by `$$` like this:

      $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

  to get: $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

  MathJax will leave code blocks (including those inline with
  backticks) alone. Outside code blocks, escape `$` by prefixing it
  with a backslash to scare MathJax off.

  Escaping all those backslashes in TeX fragments embedded in Lisp
  strings can be a pain. [Pythonic String
  Reader](https://github.com/smithzvk/pythonic-string-reader) can help
  with that.""")


;;;; Automatic markup of symbols

;;; Take a string in markdown format and a list of KNOWN-REFERENCES.
;;; Markup symbols as code (if *DOCUMENT-UPPERCASE-IS-CODE*), autolink
;;; (if *DOCUMENT-LINK-SECTIONS*, *DOCUMENT-LINK-CODE*) and always
;;; handle explicit links with locatives (e.g. [FOO][function]).
;;; Return the transformed string.
(defun codify-and-autolink (string &key (known-references *references*))
  (when string
    (autolink (codify string
                      ;; Recognize as code all the things we don't
                      ;; want to link to.
                      (append known-references *local-references*))
              known-references)))


(defsection @mgl-pax-codification (:title "Codification")
  (*document-uppercase-is-code* variable)
  (*document-downcase-uppercase-code* variable))

(defvar *document-uppercase-is-code* t
  """When true, words that

  - have at least 3 characters
  - have at least one ALPHA-CHAR-P character
  - have no lowercase characters
  - name an interned symbol

  are assumed to be code as if they were marked up with backticks,
  which is especially useful when combined with *DOCUMENT-LINK-CODE*.
  For example, this docstring:

      "`FOO` and FOO."

  is equivalent to this:

      "`FOO` and `FOO`."

  iff `FOO` is an interned symbol. To suppress this behavior, add a
  backslash to the beginning of the symbol or right after the leading
  * if it would otherwise be parsed as markdown emphasis:

      "\\MGL-PAX *\\DOCUMENT-NORMALIZE-PACKAGES*"

  The number of backslashes is doubled above because that's how the
  example looks in a docstring. Note that the backslash is discarded
  even if *DOCUMENT-UPPERCASE-IS-CODE* is false.""")

;;; The core of the implementation of *DOCUMENT-UPPERCASE-IS-CODE*.
;;;
;;; This is called by MAP-NAMES so the return values are NEW-TREE,
;;; SLICE, N-CHARS-READ. Also called by TRANSLATE-EMPH that expects
;;; only a single return value: the new tree.
(defun translate-uppercase-name (parent tree name known-references)
  (declare (ignore parent))
  (when (and (no-lowercase-chars-p name)
             (find-if #'alpha-char-p name))
    (flet ((foo (name)
             (multiple-value-bind (refs n-chars-read)
                 (references-for-similar-names name known-references)
               (when refs
                 (values `(,(code-fragment
                             (maybe-downcase (subseq name 0 n-chars-read))))
                         t n-chars-read)))))
      (let ((emph (and (listp tree) (eq :emph (first tree)))))
        ;; *DOCUMENT-UPPERCASE-IS-CODE* escaping
        (cond ((and emph (eql #\\ (alexandria:first-elt name)))
               ;; E.g. "*\\DOCUMENT-NORMALIZE-PACKAGES*"
               ;; -> (:EMPH "DOCUMENT-NORMALIZE-PACKAGES")
               (values (list `(:emph ,(maybe-downcase (subseq name 1))))
                       t (length name)))
              ((eql #\\ (alexandria:first-elt name))
               ;; Discard the leading backslash escape.
               ;; E.g. "\\MGL-PAX" -> "MGL-PAX"
               (values (list (maybe-downcase (subseq name 1))) t (length name)))
              ((not *document-uppercase-is-code*)
               ;; Don't change anything.
               nil)
              (emph
               (foo (format nil "*~A*" name)))
              (t
               (foo name)))))))

;;; Return the references from REFS which are for the symbol to which
;;; NAME parses and the number of characters read. Some trimming is
;;; attempted to handle separators and plurals. Packages and ASDF
;;; systems are treated somewhat differently because their
;;; REFERENCE-OBJECTs may be strings.
;;;
;;; If a symbol (whose name is longer than 2 characters) is read and
;;; no references to it are found, then it gets a DISLOCATED reference
;;; to ensure that it gets codified. FIXME: Should this logic be in
;;; FORMAT-REFERENCES instead?
(defun references-for-similar-names (name refs)
  (multiple-value-bind (symbol n-chars-read)
      (find-definitions-find-symbol-or-package name)
    (when n-chars-read
      (values (references-for-symbol symbol refs n-chars-read) n-chars-read))))

;;; Return the references from REFS which are for SYMBOL or which are
;;; for a non-symbol but resolve to the same object with SYMBOL.
(defun references-for-symbol (symbol refs n-chars-read)
  (let ((symbol-name (symbol-name symbol)))
    (or (remove-if-not (lambda (ref)
                         (or (eq symbol (reference-object ref))
                             ;; This function is only called when
                             ;; there is an interned symbol for
                             ;; something named by a string.
                             ;;
                             ;; KLUDGE: If the object of REF is
                             ;; replaced with SYMBOL, does it resolve
                             ;; to the same object? This is necessary
                             ;; to get packages and asdf systems right
                             ;; because the objects in their canonical
                             ;; references are strings and we compare
                             ;; to symbols.
                             (equalp symbol-name (reference-object ref))))
                       refs)
        (if (or (< 2 n-chars-read)
                (eq symbol t))
            (list (make-reference symbol 'dislocated))
            ;; Don't codify A, I and similar.
            ()))))

;;; Handle *DOCUMENT-UPPERCASE-IS-CODE* in normal strings and :EMPH
;;; (to recognize *VAR*). Also, perform consistency checking of
;;; cl-transcript code blocks (see @MGL-PAX-TRANSCRIBING-WITH-EMACS).
(defun codify (string known-references)
  (map-markdown-parse-tree
   (list :emph '3bmd-code-blocks::code-block)
   '(:code :verbatim 3bmd-code-blocks::code-block
     :reference-link :explicit-link :image :mailto)
   t
   (alexandria:rcurry #'translate-to-code known-references)
   string))

;;; This is called with with a list TREE whose CAR is :EMPH or
;;; 3BMD-CODE-BLOCKS::CODE-BLOCK or with TREE being a string (as per
;;; the MAP-MARKDOWN-PARSE-TREE above).
(defun translate-to-code (parent tree known-references)
  (cond ((stringp tree)
         (let ((string tree))
           (values (map-names string
                              (lambda (string start end)
                                (let ((name (subseq string start end)))
                                  (translate-uppercase-name parent string name
                                                            known-references))))
                   ;; don't recurse, do slice
                   nil t)))
        ((eq :emph (first tree))
         (translate-emph parent tree known-references))
        ((eq '3bmd-code-blocks::code-block (first tree))
         (translate-code-block parent tree))
        (t
         (error "~@<Unexpected tree type ~S.~:@>" (first tree)))))

;;; CODE-BLOCK looks like this:
;;;
;;;     (3BMD-CODE-BLOCKS::CODE-BLOCK :LANG "commonlisp" :CONTENT "42")
(defun translate-code-block (parent code-block)
  (declare (ignore parent))
  (let ((lang (getf (rest code-block) :lang)))
    (if (equal lang "cl-transcript")
        `(3bmd-code-blocks::code-block
          :lang ,lang
          :content ,(transcribe (getf (rest code-block) :content) nil
                                :update-only t :check-consistency t))
        code-block)))

;;; Undo the :EMPH parsing for code references. E.g. (:EMPH "XXX") ->
;;; "*XXX*" (if *XXX* is in a REFERENCE-OBJECT KNOWN-REFERENCES).
(defun translate-emph (parent tree known-references)
  (if (= 2 (length tree))
      (let ((translation (translate-uppercase-name parent tree (second tree)
                                                   known-references)))
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


(defvar *find-definitions-right-trim* ",:.>")
(defparameter *find-definitions-right-trim-2* ",:.>sS")

;;; Lifted from SWANK, and tweaked to return the number of characters
;;; read.
(defun find-definitions-find-symbol-or-package (name)
  (flet ((do-find (name n)
           (multiple-value-bind (symbol found name)
               (with-swank ()
                 (swank::with-buffer-syntax (*package*)
                   (swank::parse-symbol name)))
             (cond (found
                    (return-from find-definitions-find-symbol-or-package
                      (values symbol n)))
                   ;; Packages are not named by symbols, so
                   ;; not-interned symbols can refer to packages
                   ((find-package name)
                    (return-from find-definitions-find-symbol-or-package
                      (values (make-symbol name) n)))))))
    (do-find name (length name))
    (let* ((right-trimmed
             (swank::string-right-trim *find-definitions-right-trim* name))
           (right-trimmed-length (length right-trimmed)))
      (do-find right-trimmed right-trimmed-length))
    (let* ((right-trimmed-2
             (swank::string-right-trim *find-definitions-right-trim-2* name))
           (right-trimmed-2-length (length right-trimmed-2)))
      (do-find right-trimmed-2 right-trimmed-2-length))))


(defvar *document-downcase-uppercase-code* nil
  "If true, then the names of symbols recognized as code (including
  those found if *DOCUMENT-UPPERCASE-IS-CODE*) are downcased in the
  output if they only consist of uppercase characters. If it is
  :ONLY-IN-MARKUP, then if the output format does not support
  markup (e.g. it's :PLAIN), then no downcasing is performed.")

(defun maybe-downcase (string)
  (if (and (or (and *document-downcase-uppercase-code*
                    (not (eq *document-downcase-uppercase-code*
                             :only-in-markup)))
               (and (eq *document-downcase-uppercase-code*
                        :only-in-markup)
                    (not (eq *format* :plain))))
           (no-lowercase-chars-p string))
      (string-downcase string)
      string))

(defun no-lowercase-chars-p (string)
  (notany (lambda (char)
            (char/= char (char-upcase char)))
          ;; Allows plurals as in "FRAMEs" and "FRAMEs."
          (swank::string-right-trim *find-definitions-right-trim-2* string)))


(defsection @mgl-pax-linking-to-code (:title "Linking to Code")
  (*document-link-code* variable)
  (*document-link-to-hyperspec* variable)
  (*document-hyperspec-root* variable)
  (@mgl-pax-reference-resolution section))

(defvar *document-link-code* t
  """When true, during the process of generating documentation for a
  [SECTION][class], HTML anchors are added before the documentation of
  every reference that's not to a section. Also, markdown style
  reference links are added when a piece of inline code found in a
  docstring refers to a symbol that's referenced by one of the
  sections being documented. Assuming `BAR` is defined, the
  documentation for:

  ```commonlisp
  (defsection @foo
    (foo function)
    (bar function))

  (defun foo (x)
    "Calls `BAR` on `X`."
    (bar x))
  ```

  would look like this:

      - [function] FOO X

          Calls [`BAR`][1] on `X`.

  Instead of `BAR`, one can write `[bar][]` or ``[`bar`][]`` as well.
  Since symbol names are parsed according to READTABLE-CASE, character
  case rarely matters.

  Now, if `BAR` has multiple references with different locatives:

  ```commonlisp
  (defsection @foo
    (foo function)
    (bar function)
    (bar type))

  (defun foo (x)
    "Calls `BAR` on `X`."
    (bar x))
  ```

  then documentation would link to all interpretations:

      - [function] FOO X

          Calls `BAR`([`1`][link-id-1] [`2`][link-id-2]) on `X`.

  This situation occurs in PAX with SECTION, which is both a
  class (see [SECTION][class]) and a locative type denoted by a
  symbol (see [SECTION][locative]). Back in the example above,
  clearly, there is no reason to link to type `BAR`, so one may wish
  to select the function locative. There are two ways to do that. One
  is to specify the locative explicitly as the id of a reference link:

      "Calls [BAR][function] on X."

  However, if in the text there is a locative immediately before or
  after the symbol, then that locative is used to narrow down the
  range of possibilities. This is similar to what the `M-.` extension
  does. In a nutshell, if `M-.` works without questions then the
  documentation will contain a single link. So this also works without
  any markup:

      "Calls function `BAR` on X."

  This last option needs backticks around the locative if it's not a
  single symbol.

  Note that *DOCUMENT-LINK-CODE* can be combined with
  *DOCUMENT-UPPERCASE-IS-CODE* to have links generated for uppercase
  names with no quoting required.""")

(defvar *document-link-to-hyperspec* t
  "If true, link symbols found in code to the Common Lisp Hyperspec.

  Locatives work as expected (see *DOCUMENT-LINK-CODE*).
  [FIND-IF][dislocated] links to FIND-IF, [FUNCTION][dislocated] links
  to FUNCTION and `[FUNCTION][type]` links to [function][type].

  Autolinking to T and NIL is suppressed. If desired, use
  `[T][]` (that links to [T][]) or `[T][constant]` (that links to
  [T][constant]).")

(defvar *document-hyperspec-root*
  "http://www.lispworks.com/documentation/HyperSpec/"
  "A URL pointing to an installed Common Lisp Hyperspec. The default
  value of is the canonical location.")

(defun hyperspec-external-references ()
  (mapcar (lambda (entry)
            (destructuring-bind (object locative filename) entry
              (list object (if (eq locative 'operator)
                               'macro
                               locative)
                    (hyperspec-link filename))))
          *hyperpsec-entries*))

(defun hyperspec-link (name)
  ;; FIXME: This does not support anchors as in "26_glo_n#nil", but it
  ;; doesn't matter as long as glossary entries from the hyperspec are
  ;; not included.
  (format nil "~ABody/~A.htm" *document-hyperspec-root* name))

;;; Handle *DOCUMENT-LINK-CODE* (:CODE for `SYMBOL` and
;;; :REFERENCE-LINK for [symbol][locative]). Don't hurt other links.
(defun autolink (string known-references)
  (let ((autolinked (make-hash-table :test #'equal)))
    (map-markdown-parse-tree
     '(:code :reference-link)
     '(:explicit-link :image :mailto)
     nil
     (alexandria:rcurry #'translate-to-links known-references autolinked)
     string)))

;;; This is the first of the translator functions, which are those
;;; passed to MAP-MARKDOWN-PARSE-TREE. This particular translator
;;;
;;; - handles (:CODE "SOMETHING"), the parse of `SOMETHING`: looks for
;;;   any KNOWN-REFERENCES to "something" (which may name a symbol or
;;;   a package) and tanslates it to, for example, (:REFERENCE-LINK
;;;   :LABEL ((:CODE "SOMETHING")) :DEFINITION "function") if there is
;;;   a single function reference to it. See FORMAT-REFERENCES-TO-NAME
;;;   and format-references for all the cases.
;;;
;;; - handles :REFERENCE-LINK nodes:
;;;
;;;   - those with explicit locative given (:REFERENCE-LINK :LABEL
;;;     ((:CODE "SOMETHING")) :DEFINITION "function"), the parse of
;;;     [`SOMETHING`][function],
;;;
;;;   - and those with no locative (:REFERENCE-LINK :LABEL ((:CODE
;;;     "SOMETHING")) :TAIL "[]"), the parse of [`SOMETHING`][].
(defun translate-to-links (parent tree known-references autolinked)
  (cond
    ;; (:CODE "something")
    ((and (eq :code (first tree))
          (= 2 (length tree))
          (stringp (second tree)))
     (let ((name (second tree)))
       (multiple-value-bind (formatted-refs refs)
           (format-references-to-name parent tree name known-references)
         (let ((autolinked-key (cons name formatted-refs))
               (supressedp (and refs
                                (let ((symbol (reference-object (first refs))))
                                  (member symbol '(t nil))))))
           (cond ((and formatted-refs
                       (not supressedp)
                       (or (not (gethash autolinked-key autolinked))
                           ;; Replace references to sections with the
                           ;; title any number of times.
                           (and (= (length refs) 1)
                                (typep (resolve (first refs) :errorp nil)
                                       'section))))
                  (setf (gethash autolinked-key autolinked) t)
                  (values formatted-refs nil t))
                 (t
                  tree))))))
    ;; [section][type], [`section`][type], [*var*][variable], [section][]
    ((and (eq :reference-link (first tree)))
     ;; For example, the tree for [`section`][type] is
     ;; (:REFERENCE-LINK :LABEL ((:CODE "SECTION")) :DEFINITION "type")
     (destructuring-bind (&key label definition tail) (rest tree)
       (let ((name (extract-name-from-reference-link-label label)))
         (multiple-value-bind (symbol n-chars-read)
             (if name
                 (find-definitions-find-symbol-or-package name)
                 nil)
           (if (null n-chars-read)
               tree
               (let* ((references
                        (remove symbol known-references
                                ;; FIXME: EQUAL or EQ?
                                :test-not #'eq
                                :key #'reference-object))
                      (references
                        (if (and (zerop (length definition))
                                 (equal tail "[]"))
                            (filter-references-for-unspecified-locative
                             references)
                            (alexandria:ensure-list
                             (find-reference-by-locative-string
                              definition
                              (filter-references-for-specified-locative
                               references)
                              :if-dislocated symbol)))))
                 (if references
                     (values (format-references name references) nil t)
                     tree)))))))
    (t
     tree)))

(defun extract-name-from-reference-link-label (label)
  (let ((e (first label)))
    (cond ((stringp e)
           ;; ("S") -> "S"
           e)
          ((and (eq :emph (first e))
                (= 2 (length e))
                (stringp (second e)))
           ;; (:EMPH "S") -> "*S*"
           (format nil "*~A*" (second e)))
          ((and (eq :code (first e))
                (= 2 (length e))
                (stringp (second e)))
           ;; (:CODE "S") -> "S"
           (second e)))))

;;; Translate NAME (a string) that's part of TREE (e.g. it's "xxx"
;;; from (:CODE "xxx") or from "xxx,yyy"), or it's constructed from
;;; TREE (e.g. it's "*SYM*" from (:EMPH "SYM")).
(defun format-references-to-name (parent tree name known-references)
  (multiple-value-bind (refs n-chars-read)
      (references-for-similar-names name known-references)
    (when refs
      (flet ((formatted-refs-and-refs (refs)
               (let ((refs (filter-references-for-specified-locative refs)))
                 (values (format-references (maybe-downcase
                                             (subseq name 0 n-chars-read))
                                            refs)
                         refs))))
        ;; If necessary, try to find a locative before or after NAME
        ;; to disambiguate and also to handle the `PRINT argument`
        ;; case.
        (when (and refs (references-for-the-same-symbol-p refs))
          (let ((reference (find-locative-around parent tree refs)))
            (when reference
              (return-from format-references-to-name
                (formatted-refs-and-refs (list reference))))))
        (formatted-refs-and-refs (filter-references-for-symbol refs))))))

;;; NAME-ELEMENT is a child of TREE. It is the name of the symbol or
;;; it contains the name. Find a locative before or after NAME-ELEMENT
;;; with which NAME occurs in POSSIBLE-REFERENCES. Return the matching
;;; REFERENCE, if found. POSSIBLE-REFERENCES must only contain
;;; references to the symbol.
(defun find-locative-around (tree name-element possible-references)
  (let ((possible-references
          (cons (make-reference (reference-object (first possible-references))
                                'argument)
                possible-references)))
    (labels ((try (element)
               (let ((reference
                       (cond ((stringp element)
                              (find-reference-by-locative-string
                               element possible-references))
                             ((eq :code (first element))
                              (find-reference-by-locative-string
                               (second element) possible-references))
                             ;; (:REFERENCE-LINK :LABEL ((:CODE
                             ;; "CLASS")) :DEFINITION "0524")
                             ((eq :reference-link (first element))
                              (try (first (third element)))))))
                 (when reference
                   (return-from find-locative-around reference)))))
      ;; For example, (:PLAIN "See" "function" " " "FOO")
      (loop for rest on tree
            do (when (and (eq (third rest) name-element)
                          (stringp (second rest))
                          (blankp (second rest)))
                 (try (first rest))
                 (return)))
      ;; For example, (:PLAIN "See" "the" "FOO" " " "function")
      (loop for rest on tree
            do (when (and (eq (first rest) name-element)
                          (stringp (second rest))
                          (blankp (second rest)))
                 (try (third rest))
                 (return))))))

(defun find-reference-by-locative-string (locative-string possible-references
                                          &key if-dislocated)
  (let ((locative (read-locative-from-string locative-string)))
    (when locative
      ;; This won't find [SECTION][TYPE] because SECTION is a class.
      ;;
      ;; Reference lookup could look for a different locative which
      ;; leads to the same object/reference, but there is no sane
      ;; generalization of that to locative-types. Do we need
      ;; something like LOCATIVE-SUBTYPE-P?
      (if (and if-dislocated (member locative '(dislocated argument)))
          ;; Handle an explicit [FOO][dislocated] in markdown.
          (make-reference if-dislocated 'dislocated)
          (find locative possible-references
                :test #'equivalent-locative-p)))))

(defun equivalent-locative-p (locative reference)
  (or (locative-equal locative (reference-locative reference))
      (let ((reference
              (make-reference (reference-object reference)
                              (canonical-locative
                               (locative-type (reference-locative reference))
                               (locative-args (reference-locative reference)))))
            (alternative
              (make-reference (reference-object reference)
                              (canonical-locative
                               (locative-type locative)
                               (locative-args locative))))
            object1
            object2)
        (or (reference= reference alternative)
            (and (ignore-errors
                  (setq object1 (resolve alternative :errorp nil)))
                 (setq object2 (resolve reference :errorp nil))
                 (eq object1 object2))))))


(defsection @mgl-pax-reference-resolution (:title "Reference Resolution")
  """Links are generated according to *DOCUMENT-LINK-CODE* in general
  but with some additional heuristics for convenience."""
  (@mgl-pax-filtering-multiple-references section)
  (@mgl-pax-local-references section))

(defsection @mgl-pax-filtering-multiple-references
    (:title "Filtering Multiple References")
  """When there are multiple references to link to - as seen in the
  second example in *DOCUMENT-LINK-CODE* - some references are removed
  by the following rules.

  - References to ASDF:SYSTEMs are removed if there are other
    references which are not to ASDF:SYSTEMs. This is because system
    names often collide with the name of a class or function and are
    rarely useful to link to. Use explicit links to ASDF:SYSTEMs, if
    necessary.

  - If references include a GENERIC-FUNCTION locative, then all
    references with LOCATIVE-TYPE [METHOD][locative],
    [ACCESSOR][locative], [READER][locative] and [WRITER][locative]
    are removed to avoid linking to a possibly large number of
    methods.""")

(defsection @mgl-pax-local-references (:title "Local References")
  """To unclutter the generated output by reducing the number of
  links, the so-called 'local' references (references to things for
  which documentation is being generated) are treated specially. In
  the following example, there are local references to the function
  FOO and its arguments, so none of them get turned into links:

  ```common-lisp
  (defun foo (arg1 arg2)
    "FOO takes two arguments: ARG1 and ARG2.")
  ```

  If linking was desired one could write `[FOO][function]` or `FOO
  function`, both of which result in a single link. An explicit link
  with an unspecified locative like in `[*DOCUMENT-LINK-CODE*][]`
  generates links to all references involving the *DOCUMENT-LINK-CODE*
  symbol except the local ones.

  The exact rules for local references are as follows:

  - Unadorned names in code (e.g. `FOO`) do not get any links if there
    is _any_ local reference with the same symbol.

  - With a locative specified (e.g. in the explicit link
    `[FOO][function]` or in the text `the FOO function`), a single
    link is made irrespective of any local references.

  - Explicit links with an unspecified locative (e.g. `[FOO][]`) are
    linked to all non-local references.""")

;;; A symbol in code like `FOO` will link to all references involving
;;; FOO (given as REFS) unless any reference with FOO is on
;;; *LOCAL-REFERENCES* in which case it will not link to anything.
(defun filter-references-for-symbol (refs)
  (when refs
    (let ((symbol (reference-object (first refs))))
      (unless (find symbol *local-references* :key #'reference-object
                    :test #'equal)
        (let ((refs (filter-asdf-system-references
                     (filter-references-by-format refs))))
          (if (references-for-the-same-symbol-p refs)
              (resolve-generic-function-and-methods refs)
              refs))))))

;;; A reference link like [foo][] will link to all references
;;; involving FOO (given as REFS) which are not on
;;; *LOCAL-REFERENCES*.
(defun filter-references-for-unspecified-locative (refs)
  (let ((refs (filter-asdf-system-references
               (filter-references-by-format
                (set-difference refs *local-references*
                                ;; FIXME: Should compare canonical
                                ;; references?
                                :test #'reference=)))))
    (if (references-for-the-same-symbol-p refs)
        (resolve-generic-function-and-methods refs)
        refs)))

;;; A reference link like [foo][function] or text fragment like
;;; "function FOO" will link to the function FOO irrespective of
;;; *LOCAL-REFERENCES*.
(defun filter-references-for-specified-locative (refs)
  (filter-references-by-format refs))

;;; REFERENCE-OBJECT on a CANONICAL-REFERENCE of ASDF:SYSTEM is a
;;; string, which makes REFERENCES-FOR-THE-SAME-SYMBOL-P return NIL.
;;; It's rare to link to ASDF systems in an ambiguous situation, so
;;; don't.
(defun filter-asdf-system-references (refs)
  (if (< 1 (length refs))
      (remove 'asdf:system refs :key #'reference-locative-type)
      refs))

(defun references-for-the-same-symbol-p (refs)
  (= 1 (length (remove-duplicates (mapcar #'reference-object refs)))))

(defun resolve-generic-function-and-methods (refs)
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

(defun filter-references-by-format (refs)
  (declare (special *document-link-sections*))
  (remove-if-not (lambda (ref)
                   (and (or (and *document-link-sections*
                                 (typep (resolve ref :errorp nil)
                                        'section))
                            *document-link-code*)
                        (let ((page (reference-page ref)))
                          (or
                           ;; These have no pages, but won't result in
                           ;; link anyway. Keep them.
                           (member (reference-locative-type ref)
                                   '(dislocated argument))
                           ;; Intrapage links always work.
                           (eq *page* page)
                           ;; Else we need to know the URI-FRAGMENT of
                           ;; both pages. See
                           ;; RELATIVE-PAGE-URI-FRAGMENT. Or PAGE may
                           ;; also be a string denoted an absolute
                           ;; URL.
                           (or (stringp page)
                               (and (page-uri-fragment *page*)
                                    (page-uri-fragment page)))))))
                 refs))

;;; For NAME (a STRING) and some references to it (REFS), return a
;;; markdown parse tree fragment to be spliced into a markdown parse
;;; tree with NAME formatted as :CODE with :REFERENCE-LINKs.
(defun format-references (name refs)
  (let ((ref-1 (first refs)))
    (cond ((endp refs)
           ;; all references were filtered out
           `(,(code-fragment (maybe-downcase name))))
          ((< 1 (length refs))
           ;; `name`([1][link-id-1] [2][link-id-2])
           (values `(,(code-fragment (maybe-downcase name))
                     "("
                     ,@(loop
                         for i upfrom 0
                         for ref in (sort-references refs)
                         append `(,@(unless (zerop i)
                                      '(" "))
                                  (:reference-link
                                   :label (,(code-fragment i))
                                   :definition ,(link-to-reference ref))))
                     ")")
                   t))
          ((member (reference-locative-type ref-1) '(dislocated argument))
           `(,(code-fragment (maybe-downcase name))))
          ((typep (resolve ref-1 :errorp nil) 'section)
           `((:reference-link :label (,(section-title-or-name (resolve ref-1)))
                              :definition ,(link-to-reference ref-1))))
          ((typep (resolve ref-1 :errorp nil) 'glossary-term)
           `((:reference-link :label (,(glossary-term-title-or-name
                                        (resolve ref-1)))
                              :definition ,(link-to-reference ref-1))))
          (t
           `((:reference-link :label (,(code-fragment (maybe-downcase name)))
                              :definition ,(link-to-reference ref-1)))))))

;;; Order REFERENCES in an implementation independent way. REFERENCES
;;; are all to the same object.
(defun sort-references (references)
  (flet ((locative-string (reference)
           (with-standard-io-syntax*
             (prin1-to-string
              (reference-locative reference)))))
    (sort (copy-seq references) #'string< :key #'locative-string)))


(defsection @mgl-pax-linking-to-sections (:title "Linking to Sections")
  "The following variables control how to generate section numbering,
  table of contents and navigation links."
  (*document-link-sections* variable)
  (*document-max-numbering-level* variable)
  (*document-max-table-of-contents-level* variable)
  (*document-text-navigation* variable)
  (*document-fancy-html-navigation* variable))

(defvar *document-link-sections* t
  "When true, HTML anchors are generated before the heading of
  sections, which allows the table of contents to contain links and
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
  next section and a permalink. This component is normally hidden, it
  is visible only when the mouse is over the heading. Needs
  *DOCUMENT-LINK-SECTIONS* to be on to work.")

(defvar *heading-number* ())

(defvar *heading-level* 0)

(defvar *collecting-headings-p* nil)

;;; A list of HEADING objects in the order of generation.
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
                   (link-to-reference (canonical-reference object))))
        (string (escape-markdown string)))
    (if (and *document-link-sections* link-id)
        (format stream "- [~A~A][~A]" (heading-number) string link-id)
        (format stream "- ~A~A" (heading-number) string)))
  (terpri stream))

(defun call-with-heading (stream object title link-title-to fn)
  (flet ((foo ()
           ;; Arrange for all output to go to /dev/null
           ;; (MAKE-BROADCAST-STREAM) except for the headings when we
           ;; are generating the table of contents.
           (cond
             (*collecting-headings-p*
              (funcall fn (make-broadcast-stream)))
             (*table-of-contents-stream*
              (when (<= *heading-level* *document-max-table-of-contents-level*)
                (print-table-of-contents-entry object title
                                               *table-of-contents-stream*)
                (funcall fn (make-broadcast-stream))))
             (t
              (if *document-link-sections*
                  (let ((anchor (reference-to-anchor object)))
                    (anchor anchor stream)
                    (navigation-link object stream)
                    (format stream "~A" (fancy-navigation object))
                    (heading *heading-level* stream)
                    (if (eq *format* :html)
                        (if link-title-to
                            (format stream " [~A~A][~A]~%~%"
                                    (heading-number) title
                                    (link-to-reference link-title-to))
                            (format stream " <a href=\"#~A\">~A~A</a>~%~%"
                                    (html-safe-name anchor)
                                    (heading-number)
                                    (escape-markdown title)))
                        (format stream " ~A~A~%~%" (heading-number)
                                (escape-markdown title))))
                  (progn
                    (heading *heading-level* stream)
                    (format stream " ~A~A~%~%"
                            (heading-number) (escape-markdown title))))
              (when (and (zerop *heading-level*)
                         (plusp *document-max-table-of-contents-level*))
                (heading (1+ *heading-level*) stream)
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


(defsection @mgl-pax-miscellaneous-documentation-printer-variables
    (:title "Miscellaneous Variables")
  (*document-min-link-hash-length* variable)
  (*document-mark-up-signatures* variable)
  (*document-normalize-packages* variable))


(defparameter *document-min-link-hash-length* 4
  "Recall that markdown reference style links (like `[label][id]`) are
  used for linking to sections and code. It is desirable to have ids
  that are short to maintain legibility of the generated markdown, but
  also stable to reduce the spurious diffs in the generated
  documentation, which can be a pain in a version control system.

  Clearly, there is a tradeoff here. This variable controls how many
  characters of the md5 sum of the full link id (the reference as a
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
    (assert nil () "MD5 collision collision detected.")))

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
;;;     - [locative-type] symbol
;;;
;;; When generating HTML, link SYMBOL to its own anchor.
(defun print-reference-bullet (reference stream &key name)
  (let ((locative-type (string-downcase
                        (reference-locative-type reference)))
        (name (or name (prin1-to-string (reference-object reference)))))
    (if *document-mark-up-signatures*
        ;; insert self links in HTML
        (let ((locative-type (escape-markdown locative-type))
              (name (escape-markdown name)))
          (if (eq *format* :html)
              (let ((source-uri (source-uri reference)))
                (format stream
                        "- <span class=reference-bullet>~
                           <span class=reference>~
                           <span class=\"locative-type\">~
                           ~@[<a href=\"~A\">~]\\[~A]~:[~;</a>~]~
                           </span> ~
                        <span class=\"reference-object\">[~A](#~A)</span>~
                        </span>"
                        source-uri locative-type source-uri name
                        (html-safe-name (reference-to-anchor reference))))
              (format stream "- [~A] ~A" locative-type (bold name nil))))
        (format stream "- [~A] ~A" locative-type name))))

(defun print-end-bullet (stream)
  (if (eq *format* :html)
      ;; end "reference-bullet" span
      (format stream "</span>~%")
      (format stream "~%")))

(defun source-uri (reference)
  (let ((fn (page-source-uri-fn *page*)))
    (if fn
        (funcall fn reference)
        nil)))

(defun locate-and-print-bullet (locative-type locative-args symbol stream
                                &key name)
  (let ((reference
          (canonical-reference (make-reference
                                symbol (cons locative-type locative-args)))))
    (print-reference-bullet reference stream :name name)))

(defun print-bullet (object stream)
  (print-reference-bullet (canonical-reference object) stream))

(defun print-arglist (arglist stream)
  (let ((string (cond ((stringp arglist)
                       ;; must be escaped markdown
                       arglist)
                      ((eq arglist :not-available)
                       "")
                      (t (arglist-to-string arglist)))))
    (if *document-mark-up-signatures*
        (if (eq *format* :html)
            (format stream "<span class=\"locative-args\">~A</span>" string)
            (italic string stream))
        (format stream "~A" string))))

;;; Print arg names without the package prefix to a string. The
;;; default value with prefix. Works for macro arglists too.
(defun arglist-to-string (arglist)
  (with-output-to-string (out)
    (let ((seen-special-p nil)
          (*print-pretty* t)
          (*print-right-margin* nil))
      (labels
          ((resolve* (object)
             (if (and *document-mark-up-signatures*
                      ;; KLUDGE: github has trouble displaying things
                      ;; like '`*package*`, so disable this.
                      (eq *format* :html))
                 (codify-and-autolink (prin1-and-escape-markdown object))
                 (prin1-and-escape-markdown object)))
           (foo (arglist level)
             (unless (= level 0)
               (format out "("))
             (loop for i upfrom 0
                   for arg in arglist
                   do (unless (zerop i)
                        (format out " "))
                      (cond ((member arg '(&key &optional &rest &body))
                             (setq seen-special-p t)
                             (format out "~A" (prin1-and-escape-markdown arg)))
                            ((symbolp arg)
                             (format out "~A" (escape-markdown
                                               (symbol-name arg))))
                            ((atom arg)
                             (format out "~A" (prin1-and-escape-markdown arg)))
                            (seen-special-p
                             (if (symbolp (first arg))
                                 (format out "(~A~{ ~A~})"
                                         (escape-markdown
                                          (symbol-name (first arg)))
                                         (mapcar #'resolve* (rest arg)))
                                 (format out "~A"
                                         (prin1-and-escape-markdown arg))))
                            (t
                             (foo arg (1+ level)))))
             (unless (= level 0)
               (format out ")"))))
        (foo arglist 0)))))


(defvar *document-normalize-packages* t
  "If true, symbols are printed relative to SECTION-PACKAGE of the
  innermost containing section or with full package names if there is
  no containing section. To eliminate ambiguity `[in package ...]`
  messages are printed right after the section heading if necessary.
  If false, symbols are always printed relative to the current
  package.")


;;;; Basic DOCUMENT-OBJECT and DESCRIBE-OBJECT methods

(defmethod document-object :around (object stream)
  (loop
    (return
      (cond ((or (stringp object) (typep object 'reference))
             (call-next-method))
            (t
             (let* ((reference (canonical-reference object))
                    (*reference-being-documented* reference))
               (assert (eq object (resolve reference)))
               (with-temp-output-to-page (stream (reference-page reference))
                 (when (and *document-link-code*
                            (not (typep object 'section))
                            (not (typep object 'asdf:system)))
                   (anchor (reference-to-anchor reference) stream))
                 (call-next-method object stream))))))))

(defmethod document-object ((reference reference) stream)
  "If REFERENCE can be resolved to a non-reference, call
  DOCUMENT-OBJECT with it, else call LOCATE-AND-DOCUMENT-OBJECT on the
  object, locative-type, locative-args of REFERENCE"
  (let* ((reference (canonical-reference reference))
         (resolved-object (resolve reference)))
    (if (typep resolved-object 'reference)
        (with-temp-output-to-page (stream (reference-page reference))
          (when *document-link-code*
            (anchor (reference-to-anchor reference) stream))
          (let ((locative (reference-locative reference)))
            (locate-and-document (reference-object reference)
                                 (locative-type locative)
                                 (locative-args locative)
                                 stream)))
        (document-object resolved-object stream))))


(defmethod describe-object ((section section) stream)
  "[SECTION][class] objects are printed by calling DOCUMENT on them
  with *DOCUMENT-NORMALIZE-PACKAGES* turned off to reduce clutter.
  This method is only defined if MGL-PAX/FULL is loaded to allow
  non-fancy descriptions to be printed when using CL:DESCRIBE."
  (let ((*document-uppercase-is-code* nil)
        (*document-link-code* nil)
        (*document-link-sections* nil)
        (*document-mark-up-signatures* nil)
        (*document-max-numbering-level* 0)
        (*document-max-table-of-contents-level* 0)
        (*document-text-navigation* nil)
        ;; Some Lisps bind it to T in DESCRIBE, some don't.
        (*print-circle* nil))
    (document section :stream stream :format :markdown)))


;;;; High level printing utilities

;;; Print (DOCUMENTATION OBJECT DOC-TYPE) to STREAM in FORMAT. Clean
;;; up docstring indentation, then indent it by four spaces.
;;; Automarkup symbols.
(defun maybe-print-docstring (object doc-type stream)
  (let ((docstring (filter-documentation object doc-type)))
    (when docstring
      (format stream "~%~A~%" (massage-docstring docstring)))))

(defun massage-docstring (docstring &key (indentation "    "))
  (if *table-of-contents-stream*
      ;; The output is going to /dev/null and this is a costly
      ;; operation, skip it.
      ""
      (let ((docstring (strip-docstring-indentation docstring)))
        (prefix-lines indentation (codify-and-autolink docstring)))))

(defun filter-documentation (symbol doc-type)
  (let ((docstring (documentation symbol doc-type)))
    #+sbcl
    (if (member docstring
                '("Return whether debug-block represents elsewhere code."
                  "automatically generated accessor method"
                  "automatically generated reader method"
                  "automatically generated writer method")
                :test #'equal)
        ;; Discard the garbage docstring.
        nil
        docstring)
    #-sbcl
    docstring))


(defsection @mgl-pax-document-implementation-notes
    (:title "Document Generation Implementation Notes")
  """Documentation Generation is supported on ABCL, AllegroCL, CLISP,
  CCL, CMUCL, ECL and SBCL, but their outputs may differ due to the
  lack of some introspective capability. SBCL generates complete
  output. Compared to that, the following are not supported:

  - COMPILER-MACRO docstrings on ABCL, AllegroCL, CCL, ECL
  - DEFTYPE lambda lists on ABCL, AllegroCL, CLISP, CCL, CMUCL, ECL
  - Default values in MACRO lambda lists on AllegroCL
  - Default values in function lambda lists on CCL (needs `(DEBUG 3)`
    on AllegroCL).
  """)
