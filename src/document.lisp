(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @mgl-pax-generating-documentation
    (:title "Generating Documentation")
  (document function)
  (mgl-pax/full asdf:system)
  (@mgl-pax-markdown-support section)
  (@mgl-pax-documentation-printer-variables section)
  (@mgl-pax-documentation-utilities section))

;;; Documentation starts out being sent to a certain stream, but the
;;; output is redirected to different stream if it is for a reference
;;; among PAGE-REFERENCES. This stream is given by TEMP-STREAM-SPEC
;;; that's a stream spec to allow it to
;;;
;;; - be created lazily so that no stray files are left around and
;;;   only a small number of fds are needed even for a huge project
;;;
;;; - be opened multiple times (which is not given for string streams)
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

;;; This is a link target. REFERENCE is the thing it is about, PAGE is
;;; where its documentation will go, ID is the markdown reference link
;;; id and PAGE-TO-N-USES is a hash table that counts how many times
;;; this was linked to for each page.
(defstruct link
  reference
  page
  id
  page-to-n-uses)

;;; A list of LINK objects. If a reference occurs multiple times,
;;; earlier links have precedence.
(defparameter *links* ())

(defun find-link-by-id (id)
  (find id *links* :key #'link-id :test #'equal))

(defun find-link (reference)
  (find reference *links* :key #'link-reference :test #'reference=))

;;; Return the unescaped name of the HTML anchor for REFERENCE. See
;;; HTML-SAFE-NAME.
(defun reference-to-anchor (reference)
  (let ((reference (canonical-reference reference)))
    (with-standard-io-syntax
      (prin1-to-string (list (reference-object reference)
                             (reference-locative reference))))))

;;; For the link to REFERENCE, increment the link counter for the
;;; current page and return the link id.
(defun link-to-reference (reference)
  (let ((link (find-link reference)))
    (when (and link
               (or (eq *page* (link-page link))
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
  ;; KLUDGE: Include T explicitly, because it's oft used and would not
  ;; be recognized without markup because its name is too short. The
  ;; correct solution would be to add links automatically for the
  ;; hyperspec.
  (list (make-reference t 'dislocated)))

;;; Add a LINK to *LINKS* (and a REFERENCE to *REFERENCES*) for each
;;; reference in PAGE-REFERENCES of PAGE.
(defmacro with-pages ((pages) &body body)
  `(let ((*references* *references*)
         (*links* *links*))
     (with-standard-io-syntax
       (loop for page in ,pages
             do (dolist (reference (page-references page))
                  (unless (find-link reference)
                    (push reference *references*)
                    (push (make-link
                           :reference reference
                           :page page
                           :id (hash-link (reference-to-anchor reference)
                                          #'find-link-by-id)
                           :page-to-n-uses (make-hash-table))
                          *links*)))))
     (locally ,@body)))

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

;;; Bound by DOCUMENT, this allows markdown output to depend on the
;;; output format.
(defvar *format*)

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
  generated, see @MGL-PAX-DOCUMENTATION-PRINTER-VARIABLES.

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
  (asdf:load-system :mgl-pax/full)
  (let ((*format* format)
        (*print-right-margin* (or *print-right-margin* 80))
        (*package* (if *document-normalize-packages*
                       (find-package :keyword)
                       *package*))
        (default-page (translate-page-spec
                       (list :objects (alexandria:ensure-list object)
                             :output (list stream))
                       format)))
    (progv (list (symbol-stub "3bmd-code-blocks:*code-blocks*")
                 (symbol-stub "3bmd-code-blocks:*code-blocks-default-colorize*")
                 (symbol-stub "3bmd-code-blocks::*colorize-name-map*"))
        (list t :common-lisp
              (alexandria:plist-hash-table
               `("cl-transcript" :common-lisp
                                 ,@(alexandria:hash-table-plist
                                    (var-stub
                                     "3bmd-code-blocks::*colorize-name-map*")))
               :test #'equal))
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
                (emit-footer stream))
              (unless (eq format :markdown)
                (let ((markdown-string (with-temp-input-from-page (stream page)
                                         (read-stream-into-string stream))))
                  (delete-stream-spec (page-temp-stream-spec page))
                  (with-final-output-to-page (stream page)
                    (when (page-header-fn page)
                      (funcall (page-header-fn page) stream))
                    (call-stub "3bmd:parse-string-and-print-to-stream"
                               markdown-string stream :format format)
                    (when (page-footer-fn page)
                      (funcall (page-footer-fn page) stream)))))
              (push (unmake-stream-spec (page-final-stream-spec page)) outputs))
            (if (and stream (endp pages))
                (first outputs)
                (reverse outputs))))))))

;;; Emit markdown definitions for links to REFERENCE objects that were
;;; linked to.
(defun emit-footer (stream)
  (let ((used-links (sort (remove-if-not #'link-used-on-current-page-p *links*)
                          #'string< :key #'link-id)))
    (when used-links
      (format stream "~%")
      (dolist (link used-links)
        (let ((anchor (reference-to-anchor (link-reference link))))
          (format stream "  [~A]: ~@[~A~]#~A ~S~%"
                  (link-id link)
                  (if (link-page link)
                      (relative-page-uri-fragment (link-page link)
                                                  *page*)
                      nil)
                  (html-safe-name anchor)
                  (let ((object (resolve (link-reference link))))
                    (if (typep object 'section)
                        (section-title-or-name object)
                        (princ-to-string anchor)))))))))

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

;;;; Argument handling

;;; Return the names of the function arguments in ARGLIST that's a
;;; lambda list. Handles &KEY, &OPTIONAL, &REST.
(defun function-arg-names (arglist)
  (unless (eq arglist :not-available)
    (mapcar (lambda (arg)
              (if (and (listp arg)
                       (symbolp (first arg)))
                  (first arg)
                  arg))
            arglist)))

;;; Return the names of the arguments in ARGLIST that's a macro lambda
;;; list.
(defun macro-arg-names (arglist)
  (unless (eq arglist :not-available)
    (let ((names ()))
      (labels ((foo (arglist)
                 (let ((seen-special-p nil))
                   (loop for arg in arglist
                         do (cond ((member arg '(&key &optional &rest &body))
                                   (setq seen-special-p t))
                                  ((symbolp arg)
                                   (push arg names))
                                  (seen-special-p
                                   (when (symbolp (first arg))
                                     (push (first arg) names)))
                                  (t
                                   (foo arg)))))))
        (foo arglist))
      (reverse names))))

;;; Add a dummy page with for references to SYMBOLS whose locative is
;;; ARGUMENT. If an ARGUMENT reference is present for a symbol, it
;;; will surely be marked up as code, but it's not linkified in the
;;; absence of an explicit locative even if it the symbol refers to
;;; other things with different locatives.
(defmacro with-dislocated-symbols ((symbols) &body body)
  `(with-pages ((list (make-page
                       :references (mapcar (lambda (symbol)
                                             (make-reference symbol
                                                             'dislocated))
                                           ,symbols))))
     ,@body))


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


(defsection @mgl-pax-documentation-printer-variables
    (:title "Documentation Printer Variables")
  "Docstrings are assumed to be in markdown format and they are pretty
  much copied verbatim to the documentation subject to a few knobs
  described below."
  (*document-uppercase-is-code* variable)
  (*document-downcase-uppercase-code* variable)
  (*document-link-code* variable)
  (*document-link-sections* variable)
  (*document-min-link-hash-length* variable)
  (*document-mark-up-signatures* variable)
  (*document-max-numbering-level* variable)
  (*document-max-table-of-contents-level* variable)
  (*document-text-navigation* variable)
  (*document-fancy-html-navigation* variable)
  (*document-normalize-packages* variable))

(defvar *document-uppercase-is-code* t
  """When true, words with at least three characters and no lowercase
  characters naming an interned symbol are assumed to be code as if
  they were marked up with backticks, which is especially useful when
  combined with *DOCUMENT-LINK-CODE*. For example, this docstring:

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

(defvar *document-downcase-uppercase-code* nil
  "If true, then the names of symbols recognized as code (including
  those found if *DOCUMENT-UPPERCASE-IS-CODE*) are downcased in the
  output if they only consist of uppercase characters. If it is
  :ONLY-IN-MARKUP, then if the output format does not support
  markup (e.g. it's :PLAIN), then no downcasing is performed.")

;;;; Links

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

  Now, if `BAR` has references with different locatives:

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

  Note that [*DOCUMENT-LINK-CODE*][variable] can be combined with
  [`*DOCUMENT-UPPERCASE-IS-CODE*`][] to have links generated for
  uppercase names with no quoting required.""")

(defvar *document-link-sections* t
  "When true, HTML anchors are generated before the heading of
  sections, which allows the table of contents to contain links and
  also code-like references to sections (like `@FOO-MANUAL`) to be
  translated to links with the section title being the name of the
  link.")

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
  (let ((hex (call-stub "ironclad:byte-array-to-hex-string"
                        (call-stub "ironclad:digest-sequence"
                                   (symbol-stub "ironclad:md5")
                                   (call-stub "babel:string-to-octets"
                                              string)))))
    (loop for i upfrom min-n-chars below 32
          do (let ((hash (subseq hex 0 (min 32 i))))
               (unless (funcall detect-collision-fn hash)
                 (return-from hash-link hash))))
    (assert nil () "MD5 collision collision detected.")))

;;;; Signatures

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
      (labels ((resolve* (object)
                 (if (and *document-mark-up-signatures*
                          ;; KLUDGE: github has trouble displaying
                          ;; things like '`*package*`, so disable
                          ;; this.
                          (eq *format* :html))
                     (replace-known-references
                      (prin1-and-escape-markdown object))
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
                                 (format out "~A"
                                         (prin1-and-escape-markdown arg)))
                                ((symbolp arg)
                                 (format out "~A"
                                         (escape-markdown
                                          (symbol-name arg))))
                                ((atom arg)
                                 (format out "~A"
                                         (prin1-and-escape-markdown arg)))
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

;;;; Section numbering, table of contents and navigation links

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

(defmacro with-heading ((stream object title &key link-title-to)
                        &body body)
  `(call-with-heading ,stream ,object ,title ,link-title-to
                      (lambda (,stream) ,@body)))

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

(defmacro with-nested-headings (() &body body)
  `(let ((*heading-level* (1+ *heading-level*)))
     ,@body))

;;;; Packages

(defvar *document-normalize-packages* t
  "If true, symbols are printed relative to SECTION-PACKAGE of the
  innermost containing section or with full package names if there is
  no containing section. To eliminate ambiguity `[in package ...]`
  messages are printed right after the section heading if necessary.
  If false, symbols are always printed relative to the current
  package.")

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
        (prefix-lines indentation (replace-known-references docstring)))))

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

;;;; Indentation utilities

;;; Normalize indentation of docstrings as it's described in
;;; (METHOD () (STRING T)) DOCUMENT-OBJECT.
(defun strip-docstring-indentation (docstring &key (first-line-special-p t))
  (let ((indentation
          (docstring-indentation docstring
                                 :first-line-special-p first-line-special-p)))
    (values (with-output-to-string (out)
              (with-input-from-string (s docstring)
                (loop for i upfrom 0
                      do (multiple-value-bind (line missing-newline-p)
                             (read-line s nil nil)
                           (unless line
                             (return))
                           (if (and first-line-special-p (zerop i))
                               (write-string line out)
                               (write-string (subseq* line indentation) out))
                           (unless missing-newline-p
                             (terpri out))))))
            indentation)))

(defun n-leading-spaces (line)
  (let ((n 0))
    (loop for i below (length line)
          while (char= (aref line i) #\Space)
          do (incf n))
    n))

;;; Return the minimum number of leading spaces in non-blank lines
;;; after the first.
(defun docstring-indentation (docstring &key (first-line-special-p t))
  (let ((n-min-indentation nil))
    (with-input-from-string (s docstring)
      (loop for i upfrom 0
            for line = (read-line s nil nil)
            while line
            do (when (and (or (not first-line-special-p) (plusp i))
                          (not (blankp line)))
                 (when (or (null n-min-indentation)
                           (< (n-leading-spaces line) n-min-indentation))
                   (setq n-min-indentation (n-leading-spaces line))))))
    (or n-min-indentation 0)))

;;; Add PREFIX to every line in STRING.
(defun prefix-lines (prefix string &key exclude-first-line-p)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for i upfrom 0 do
        (multiple-value-bind (line missing-newline-p) (read-line in nil nil)
          (unless line
            (return))
          (if (and exclude-first-line-p (= i 0))
              (format out "~a" line)
              (format out "~a~a" prefix line))
          (unless missing-newline-p
            (terpri out)))))))

;;;; Automatic markup of symbols

;;; Take a string in markdown format and a list of KNOWN-REFERENCES.
;;; Markup symbols as code (if *DOCUMENT-UPPERCASE-IS-CODE*), autolink
;;; (if *DOCUMENT-LINK-SECTIONS*, *DOCUMENT-LINK-CODE*) and handle
;;; explicit links with locatives (always). Return the transformed
;;; string.
(defun replace-known-references (string &key (known-references *references*))
  (when string
    (let ((string
            ;; Handle *DOCUMENT-UPPERCASE-IS-CODE* in normal strings
            ;; and :EMPH (to recognize *VAR*).
            (map-markdown-parse-tree
             (list :emph (symbol-stub "3bmd-code-blocks::code-block"))
             '(:code :verbatim (symbol-stub "3bmd-code-blocks::code-block")
               :reference-link :explicit-link :image :mailto)
             t
             (alexandria:rcurry #'translate-to-code known-references)
             string)))
      ;; Handle *DOCUMENT-LINK-CODE* (:CODE for `SYMBOL` and
      ;; :REFERENCE-LINK for [symbol][locative]). Don't hurt links.
      (map-markdown-parse-tree
       '(:code :reference-link)
       '(:explicit-link :image :mailto)
       nil
       (alexandria:rcurry #'translate-to-links known-references)
       string))))

(defun translate-to-links (parent tree known-references)
  (cond
    ;; (:CODE "something")
    ((and (eq :code (first tree))
          (= 2 (length tree))
          (stringp (second tree)))
     (let* ((name (second tree))
            (translation (translate-name parent tree name known-references)))
       (if translation
           (values translation nil t)
           tree)))
    ;; [section][type], [`section`][type], [*var*][variable], [section][]
    ((and (eq :reference-link (first tree)))
     ;; For example, the tree for [`section`][type] is
     ;; (:REFERENCE-LINK :LABEL ((:CODE "SECTION")) :DEFINITION "type")
     (destructuring-bind (&key label definition tail) (rest tree)
       (let* ((name (extract-name-from-label label))
              (symbol (if name
                          (find-definitions-find-symbol-or-package name)
                          nil)))
         (if (not symbol)
             tree
             (let* ((references (remove symbol known-references
                                        :test-not #'eq
                                        :key #'reference-object))
                    (references (if (and (zerop (length definition))
                                         (equal tail "[]"))
                                    (filter-references references)
                                    (alexandria:ensure-list
                                     (find-reference-by-locative-string
                                      definition
                                      ;; Explicit references don't
                                      ;; need heuristic conflict
                                      ;; resolution so we don't call
                                      ;; FILTER-REFERENCES.
                                      (filter-references-by-format
                                       references)
                                      :if-dislocated symbol)))))
               (if references
                   (values (format-references name references) nil t)
                   tree))))))
    (t
     tree)))

(defun extract-name-from-label (label)
  (let ((e (first label)))
    (cond ((stringp e)
           e)
          ((and (eq :emph (first e))
                (= 2 (length e))
                (stringp (second e)))
           (format nil "*~A*" (second e)))
          ((and (eq :code (first e))
                (= 2 (length e))
                (stringp (second e)))
           (second e)))))

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

;;; Translate NAME (a string) that's part of TREE (e.g. it's "xxx"
;;; from (:CODE "xxx") or from "xxx,yyy"), or it's constructed from
;;; TREE (e.g. it's "*SYM*" from (:EMPH "SYM")).
(defun translate-name (parent tree name known-references)
  (multiple-value-bind (refs n-chars-read)
      (references-for-similar-names name known-references)
    (when refs
      (let ((refs (filter-references refs)))
        ;; If necessary, try to find a locative before or after NAME
        ;; to disambiguate.
        (when (and (< 1 (length refs))
                   (references-for-the-same-symbol-p refs))
          (let ((reference (find-locative-around parent tree refs)))
            (when reference
              (setq refs (list reference)))))
        (values (format-references
                 (maybe-downcase (subseq name 0 n-chars-read)) refs)
                t n-chars-read)))))

;;; NAME-ELEMENT is a child of TREE. It is the name of the symbol or
;;; it contains the name. Find a locative before or after NAME-ELEMENT
;;; with which NAME occurs in KNOWN-REFERENCES. Return the matching
;;; REFERENCE, if found. KNOWN-REFERENCES must only contain references
;;; to the symbol.
(defun find-locative-around (tree name-element possible-references)
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
               (return)))))

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
      (if (and if-dislocated (eq locative 'dislocated))
          (make-reference if-dislocated 'dislocated)
          (find locative possible-references
                :key #'reference-locative :test #'locative-equal)))))

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
        ((eq (symbol-stub "3bmd-code-blocks::code-block") (first tree))
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
        `(,(symbol-stub "3bmd-code-blocks::code-block")
          :lang ,lang
          :content ,(transcribe (getf (rest code-block) :content) nil
                                :update-only t :check-consistency t))
        code-block)))

;;; Call FN with STRING and START, END indices. FN returns three
;;; values: a replacement parse tree fragment (or NIL, if the subseq
;;; shall not be replaced), whether the replacement shall be sliced
;;; into the result list, and the number of characters replaced (may
;;; be less than (- END START). MAP-NAMES returns a parse tree
;;; fragment that's a list of non-replaced parts of STRING and
;;; replacements (maybe sliced). Consecutive strings are concatenated.
(defun map-names (string fn)
  (let ((translated ())
        (i 0)
        (n (length string)))
    (flet ((add (a)
             (if (and (stringp a)
                      (stringp (first translated)))
                 (setf (first translated)
                       (concatenate 'string (first translated) a))
                 (push a translated ))))
      (loop while (< i n)
            for prev = nil then char
            for char = (aref string i)
            do (let ((replacement nil)
                     (n-chars-replaced nil)
                     (slice nil))
                 (when (and (not (delimiterp char))
                            (or (null prev) (delimiterp prev)))
                   (let ((end (or (position-if #'delimiterp string :start i)
                                  (length string))))
                     (multiple-value-setq (replacement slice n-chars-replaced)
                       (funcall fn string i end))
                     (when replacement
                       (if slice
                           (dolist (a replacement)
                             (add a))
                           (add replacement))
                       (if n-chars-replaced
                           (incf i n-chars-replaced)
                           (setq i end)))))
                 (unless replacement
                   (add (string char))
                   (incf i)))))
    (reverse translated)))

;;; This is called by MAP-NAMES so the return values are NEW-TREE,
;;; SLICE, N-CHARS-READ. Also called by TRANSLATE-TAGGED that expects
;;; only a single return value: the new tree.
(defun translate-uppercase-name (parent tree name known-references)
  (declare (ignore parent))
  (when (no-lowercase-chars-p name)
    (flet ((foo (name)
             (multiple-value-bind (refs n-chars-read)
                 (references-for-similar-names name known-references)
               (when refs
                 (values `(,(code-fragment (maybe-downcase name)))
                         t n-chars-read)))))
      (let ((emph (and (listp tree) (eq :emph (first tree)))))
        (cond ((and emph (eql #\\ (alexandria:first-elt name)))
               (values (list `(:emph ,(maybe-downcase (subseq name 1))))
                       t (length name)))
              ((eql #\\ (alexandria:first-elt name))
               ;; Discard the leading backslash escape.
               (values (list (maybe-downcase (subseq name 1))) t (length name)))
              ((not *document-uppercase-is-code*)
               nil)
              (emph
               (foo (format nil "*~A*" name)))
              (t
               (foo name)))))))

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
      ;; leave it alone, recurse, don't slice
      (values tree t nil)))

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
                             ;; to get package and asdf systems right,
                             ;; because the object in their canonical
                             ;; references are strings and we compare
                             ;; to symbols.
                             (equalp symbol-name (reference-object ref))))
                       refs)
        ;; Don't codify A, I and similar.
        (if (< 2 n-chars-read)
            (list (make-reference symbol 'dislocated))
            ()))))

(defun references-for-similar-names (name refs)
  (multiple-value-bind (symbol n-chars-read)
      (find-definitions-find-symbol-or-package name)
    (when n-chars-read
      (values (references-for-symbol symbol refs n-chars-read) n-chars-read))))

(defvar *find-definitions-right-trim* ",:.>")
(defparameter *find-definitions-right-trim-2* ",:.>sS")

(defun no-lowercase-chars-p (string)
  (notany (lambda (char)
            (char/= char (char-upcase char)))
          ;; Allows plurals as in "FRAMEs" and "FRAMEs."
          (swank::string-right-trim *find-definitions-right-trim-2* string)))

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

;;; Select some references from REFS heuristically.
(defun filter-references (refs)
  (let ((refs (filter-asdf-system-references
               (filter-references-by-format refs))))
    (if (references-for-the-same-symbol-p refs)
        (resolve-generic-function-and-methods
         (resolve-dislocated refs))
        refs)))

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

;;; If there is a DISLOCATED reference, then don't link anywhere
;;; (remove all the other references).
(defun resolve-dislocated (refs)
  (let ((ref (find 'dislocated refs :key #'reference-locative-type)))
    (if ref
        (list ref)
        refs)))

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
  (remove-if-not (lambda (ref)
                   (and (or (and *document-link-sections*
                                 (typep (resolve ref :errorp nil)
                                        'section))
                            *document-link-code*)
                        (let ((page (reference-page ref)))
                          (or
                           ;; These have no pages, but won't result in
                           ;; link anyway. Keep them.
                           (member (reference-locative-type ref) '(dislocated))
                           ;; Intrapage links always work.
                           (eq *page* page)
                           ;; Else we need to know the URI-FRAGMENT of
                           ;; both pages. See
                           ;; RELATIVE-PAGE-URI-FRAGMENT.
                           (and (page-uri-fragment *page*)
                                (page-uri-fragment page))))))
                 refs))

;;; REFS is the list of references for NAME after filtering. Mark it
;;; up as code, create link(s).
(defun format-references (name refs)
  (let ((ref-1 (first refs)))
    (cond ((endp refs)
           ;; all references were filtered out
           `(,(code-fragment name)))
          ((< 1 (length refs))
           ;; `name`([1][link-id-1] [2][link-id-2])
           (values `(,(code-fragment (maybe-downcase name))
                     "("
                     ,@(loop
                         for i upfrom 0
                         for ref in refs
                         append `(,@(unless (zerop i)
                                      '(" "))
                                  (:reference-link
                                   :label (,(code-fragment i))
                                   :definition ,(link-to-reference ref))))
                     ")")
                   t))
          ((member (reference-locative-type ref-1) '(dislocated))
           `(,(code-fragment (maybe-downcase name))))
          ((typep (resolve ref-1) 'section)
           `((:reference-link :label (,(section-title-or-name (resolve ref-1)))
                              :definition ,(link-to-reference ref-1))))
          ((typep (resolve ref-1) 'glossary-term)
           `((:reference-link :label (,(glossary-term-title-or-name
                                        (resolve ref-1)))
                              :definition ,(link-to-reference ref-1))))
          (t
           `((:reference-link :label (,(code-fragment (maybe-downcase name)))
                              :definition ,(link-to-reference ref-1)))))))

(defun delimiterp (char)
  (or (whitespacep char)
      (find char "()'`\"#<")))


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


(defsection @mgl-pax-documentation-utilities
    (:title "Utilities for Generating Documentation")
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
                                            (resolve reference))
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
           (if (and relative-path (call-stub "cl-fad:pathname-relative-p"
                                             relative-path))
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
  (setf (slot-value @mgl-pax-world-dummy 'entries)
        (list (first (section-entries @mgl-pax-world-dummy))
              (with-output-to-string (stream)
                (dolist (object objects)
                  (format stream "- ~S~%~%" (section-name object)))))))

#+nil
(progn
  (update-asdf-system-readmes (pax-sections) :mgl-pax)
  (update-asdf-system-html-docs (pax-sections) :mgl-pax :pages (pax-pages)))

;;; KLUDGE: Bind *READTABLE* so that when evaluating in Slime (e.g.
;;; with C-x C-e) the file's readtable is not used (which leads to a
;;; reader macro conflict with CL-SYNTAX).
#+nil
(let ((*readtable* (named-readtables:find-readtable :standard)))
  (asdf:load-system :mgl-mat)
  (asdf:load-system :named-readtables/doc)
  (asdf:load-system :micmac)
  (asdf:load-system :mgl-gpr)
  (asdf:load-system :mgl)
  (asdf:load-system :journal)
  (asdf:load-system :trivial-utf-8/doc))

#+nil
(update-pax-world)
