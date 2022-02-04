(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

;;; Make Allegro record lambda lists, from which we can extract
;;; default values of arguments.
#+allegro
(eval-when (:compile-toplevel)
  (declaim (optimize (debug 3))))

(defsection @generating-documentation
    (:title "Generating Documentation")
  (document function)
  (mgl-pax/document asdf:system)
  (@markdown-support section)
  (@codification section)
  (@linking-to-code section)
  (@linking-to-the-hyperspec section)
  (@linking-to-sections section)
  (@miscellaneous-documentation-printer-variables section)
  (@documentation-utilities section)
  (@document-implementation-notes section))

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
  (used-links (make-hash-table) :type hash-table)
  temp-stream-spec
  final-stream-spec
  uri-fragment
  header-fn
  footer-fn
  source-uri-fn)

;;; The current page where output is being sent.
(defvar *page* nil)

;;; This is a possible link to REFERENCE on PAGE (note that the same
;;; REFERENCE may be written to multiple pages).
;;;
;;; PAGE may also be a string denoting a URL. This is used to link to
;;; the hyperspec.
(defstruct link
  (reference nil :type reference)
  (page nil :type (or page string)))

;;; A list of hash tables mapping REFERENCE-OBJECTs to LINKs,
;;; representing all possible things which may be linked to. Bound
;;; only once (by WITH-PAGES) after pages are created (to the list of
;;; links to all reachable references from all the objects being
;;; documented). If a reference occurs multiple times, earlier links
;;; (thus pages earlier in DOCUMENT's PAGES argument) have precedence.
(defvar *object-to-links-maps*)
;;; A list of EQ hash tables for symbol objects.
(defvar *symbol-to-links-maps*)

(defun guaranteed-links (object)
  (and (symbolp object)
       (loop for symbol-to-links in *symbol-to-links-maps*
             append (gethash object symbol-to-links))))

(defun possible-links (object)
  (let ((key (string-upcase (string object))))
    (loop for object-to-links in *object-to-links-maps*
          append (gethash key object-to-links))))

;;; A list of references with special rules for linking (see
;;; @LOCAL-REFERENCES). The reference being documented is always on
;;; this list. Arguments are typically also are. Bound by
;;; WITH-LOCAL-REFERENCES.
(defvar *local-references*)

;;; Add a LINK to *OBJECT-TO-LINKS-MAPS* for each reference in
;;; PAGE-REFERENCES of PAGES.
(defmacro with-pages ((pages) &body body)
  `(let ((*object-to-links-maps* (list (make-hash-table :test #'equal)))
         (*symbol-to-links-maps* (list (make-hash-table)))
         (*local-references* ())
         (*link-to-id* (make-hash-table))
         (*id-to-link* (make-hash-table :test #'equal)))
     (initialize-links ,pages)
     (locally ,@body)))

(defun add-link (link)
  (let* ((reference (link-reference link))
         (object (reference-object reference)))
    (if (or (member (reference-locative-type reference) '(package asdf:system))
            (not (symbolp object)))
        (push link (gethash (string-upcase (string object))
                            (first *object-to-links-maps*)))
        (push link (gethash object (first *symbol-to-links-maps*))))))


;;;; Querying global and local references

(defun find-link (reference)
  (let ((object (reference-object reference)))
    (or (find reference (guaranteed-links object)
              :key #'link-reference :test #'reference=)
        (find reference (possible-links object)
              :key #'link-reference :test #'reference=))))

;;; Return a list of all REFERENCES whose REFERENCE-OBJECT matches
;;; OBJECT, that is, with OBJECT as their REFERENCE-OBJECT they would
;;; resolve to the same thing.
;;;
;;; If LOCAL is NIL, only those global references which are not on
;;; *LOCAL-REFERENCES* are considered for matching. If LOCAL is T,
;;; then only the local references are considered. If LOCAL is
;;; :INCLUDE then both global and local references are considered.
(defun references-to-object (object &key local)
  (let ((global-refs (global-references-to-object object)))
    (if local
        (let ((local-refs (local-references-to-object object)))
          (if (eq local :include)
              (nconc global-refs local-refs)
              (set-difference global-refs local-refs :test #'reference=)))
        global-refs)))

(defun global-references-to-object (object)
  (loop for link in (append (guaranteed-links object)
                            (possible-links object))
        for ref = (link-reference link)
        when (reference-object= object ref)
          collect ref))

(defun has-global-reference-p (object)
  (or (loop for link in (guaranteed-links object)
            for ref = (link-reference link)
              thereis (reference-object= object ref))
      (loop for link in (possible-links object)
            for ref = (link-reference link)
              thereis (reference-object= object ref))))

(defun global-reference-p (reference)
  (let ((object (reference-object reference)))
    (or (loop for link in (guaranteed-links object)
              for ref = (link-reference link)
                thereis (reference= reference ref))
        (loop for link in (possible-links object)
              for ref = (link-reference link)
                thereis (reference= reference ref)))))

(defun local-references-to-object (object)
  (remove-if-not (lambda (ref)
                   (reference-object= object ref))
                 *local-references*))

(defun has-local-reference-p (object)
  (find object *local-references* :test #'reference-object=))

(defun has-reference-p (object)
  (or (has-global-reference-p object)
      (has-local-reference-p object)))


;;; For the link to REFERENCE, increment the link counter for the
;;; current page and return the link id.
(defun link-to-reference (reference)
  (let ((link (find-link reference)))
    (when (and link
               (or (eq *page* (link-page link))
                   (stringp (link-page link))
                   (and (page-uri-fragment *page*)
                        (page-uri-fragment (link-page link)))))
      (setf (gethash link (page-used-links *page*)) t)
      (format nil "~A" (ensure-link-id link)))))

;;; Link ids are short hashes, and they go into markdown reference
;;; links. Due to possible collisions they are context dependent, so
;;; to keep LINKs immutable ids are in this hash table.
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

(defun link-used-on-current-page-p (link)
  (gethash link (page-used-links *page*)))

(defun reference-page (reference)
  (let ((link (find-link reference)))
    (when link
      (link-page link))))


(defsection @linking-to-the-hyperspec
    (:title "Linking to the Hyperspec")
  (*document-link-to-hyperspec* variable)
  (*document-hyperspec-root* variable))

(defvar *document-link-to-hyperspec* t
  "If true, link symbols found in code to the Common Lisp Hyperspec.

  Locatives work as expected (see *DOCUMENT-LINK-CODE*).
  [FIND-IF][dislocated] links to FIND-IF, [FUNCTION][dislocated] links
  to FUNCTION and `[FUNCTION][type]` links to [FUNCTION][type].

  [Autolinking][@explicit-and-autolinking section] to T and NIL is
  suppressed. If desired, use `[T][]` (that links to [T][]) or
  `[T][constant]` (that links to [T][constant]).

  Note that linking to sections in the Hyperspec is done with the CLHS
  locative and is not subject to the value of this variable.")

(defvar *document-hyperspec-root*
  "http://www.lispworks.com/documentation/HyperSpec/"
  "A URL pointing to an installed Common Lisp Hyperspec. The default
  value of is the canonical location.")

(defvar *last-hyperspec-root-and-links* nil)

(defun maybe-add-links-to-hyperspec ()
  (when *document-link-to-hyperspec*
    (unless (equal (first *last-hyperspec-root-and-links*)
                   *document-hyperspec-root*)
      (let ((*object-to-links-maps* (list (make-hash-table :test #'equal)))
            (*symbol-to-links-maps* (list (make-hash-table))))
        (loop for (object locative url) in (hyperspec-external-references
                                            *document-hyperspec-root*)
              do (let ((reference (make-reference object locative)))
                   (add-link (make-link :reference reference
                                        :page url))))
        (setq *last-hyperspec-root-and-links* (list *document-hyperspec-root*
                                                    *object-to-links-maps*
                                                    *symbol-to-links-maps*))))
    (destructuring-bind (root object-to-links-maps symbol-to-links-maps)
        *last-hyperspec-root-and-links*
      (declare (ignore root))
      (setq *object-to-links-maps* (append *object-to-links-maps*
                                           object-to-links-maps))
      (setq *symbol-to-links-maps* (append *symbol-to-links-maps*
                                           symbol-to-links-maps)))))


(defun initialize-links (pages)
  (declare (special *document-link-to-hyperspec*)
           (special *document-hyperspec-root*))
  (maybe-add-links-to-hyperspec)
  (loop for page in pages
        do (dolist (reference (page-references page))
             (unless (find-link reference)
               (add-link (make-link :reference reference
                                    :page page))))))

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
  like `(DOCUMENT @MANUAL)`, but it supports all kinds of objects for
  which DOCUMENT-OBJECT is defined. To look up the documentation of
  function DOCUMENT:

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
  generated, see @CODIFICATION, @LINKING-TO-CODE,
  @LINKING-TO-SECTIONS, and
  @MISCELLANEOUS-DOCUMENTATION-PRINTER-VARIABLES.

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
  [reachable][COLLECT-REACHABLE-OBJECTS generic-function] from one of
  its :OBJECTS.
  
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
    (:objects (, @manual)
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
              (let ((markdown-string
                      (with-temp-input-from-page (stream page)
                        (alexandria:read-stream-content-into-string stream))))
                (delete-stream-spec (page-temp-stream-spec page))
                (with-final-output-to-page (stream page)
                  (when (page-header-fn page)
                    (funcall (page-header-fn page) stream))
                  (with-colorize-silenced ()
                    (3bmd:parse-string-and-print-to-stream markdown-string
                                                           stream
                                                           :format format))
                  (when (page-footer-fn page)
                    (funcall (page-footer-fn page) stream)))))
            (push (unmake-stream-spec (page-final-stream-spec page)) outputs))
          (if (and stream (endp pages))
              (first outputs)
              (reverse outputs)))))))

;;; Emit markdown definitions for links (in *LINKS*) to REFERENCE
;;; objects that were linked to on the current page.
(defun write-markdown-reference-style-link-definitions (stream)
  (let ((used-links (sort (alexandria:hash-table-keys (page-used-links *page*))
                          #'string< :key #'link-id))
        (*package* (find-package :keyword)))
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
                      (urlencode anchor)
                      (let* ((ref (link-reference link))
                             (locative-type (reference-locative-type ref)))
                        (if (eq locative-type 'section)
                            (section-title-or-name (resolve ref))
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
            (loop for object in (cons object (collect-reachable-objects object))
                  unless (stringp object)
                    collect (canonical-reference object)))
          objects))


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

  See [DOCUMENT-OBJECT][(method () (string t))] for the details.""")

(defsection @markdown-syntax-highlighting (:title "Syntax Highlighting")
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

(defsection @mathjax (:title "MathJax")
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
  Reader][pythonic-string-reader] can help with that.

    [pythonic-string-reader]: https://github.com/smithzvk/pythonic-string-reader
  """)


;;;; Automatic markup of symbols

;;; Take a string in markdown format. Markup symbols as code (if
;;; *DOCUMENT-UPPERCASE-IS-CODE*), autolink (if
;;; *DOCUMENT-LINK-SECTIONS*, *DOCUMENT-LINK-CODE*) and always handle
;;; explicit links with locatives (e.g. [FOO][function]). Return the
;;; transformed string.
(defun codify-and-link (string)
  (when string
    (let* ((3bmd-grammar:*smart-quotes* nil)
           (parse-tree
             ;; To be able to recognize symbols like FOO* join (...
             ;; "FOO" "*" ...) to look like (... "FOO*" ...).
             (join-consecutive-non-blank-strings-in-parse-tree
              (3bmd-grammar:parse-doc string))))
      (with-output-to-string (out)
        (let ((tree (link (codify parse-tree))))
          (with-colorize-silenced ()
            (3bmd::print-doc-to-stream-using-format tree out :markdown)))))))


(defsection @codification (:title "Codification")
  (*document-uppercase-is-code* variable)
  (@codifiable glossary-term)
  (@interesting glossary-term)
  (*document-downcase-uppercase-code* variable))

(defvar *document-uppercase-is-code* t
  """When true, @CODIFIABLE and @INTERESTING @WORDs are assumed to be
  code as if they were marked up with backticks. For example, this
  docstring

      "T PRINT CLASSes SECTION *PACKAGE* MGL-PAX ASDF
      CaMeL Capital"

  is equivalent to this:

      "`T` `PRINT` `CLASS`es `SECTION` `*PACKAGE*` `MGL-PAX` `ASDF`
      CaMel Capital"

  and renders as

  `T` `PRINT` `CLASS`es `SECTION` `MGL-PAX` `ASDF` CaMel Capital

  where the links are added due to *DOCUMENT-LINK-CODE*.

  To suppress this behavior, add a backslash to the beginning of the
  symbol or right after the leading `\\*` if it would otherwise be
  parsed as markdown emphasis:

      "\\SECTION *\\PACKAGE*"

  The number of backslashes is doubled above because that's how the
  example looks in a docstring. Note that the backslash is discarded
  even if *DOCUMENT-UPPERCASE-IS-CODE* is false.
  """)

(define-glossary-term @codifiable (:title "codifiable")
  "A @WORD is _codifiable_ iff

  - it has at least one uppercase character (e.g. not `<`, `<=` or
    `///`), and
  - it has no lowercase characters (e.g. `\T`, `\*PRINT-LENGTH*`) or
    all lowercase characters immediately follow at least two
    consecutive uppercase characters (e.g. `\CLASSes` but not
    `Capital`).")

(define-glossary-term @interesting (:title "interesting")
  "A @WORD is _interesting_ iff

  - it _names_ a known reference, or
  - it is at least 3 characters long and names a package or a symbol
    external to its package.

  Where we say that a word **names** a known reference if the word
  matches the name of a thing being documented, or it is in the
  hyperspec and *DOCUMENT-UPPERCASE-IS-CODE* is true, or more
  precisely,

  - if the word matches [the object of a reference][REFERENCE-OBJECT
    reader] being documented (see DOCUMENT and
    COLLECT-REACHABLE-OBJECTS), or
  - the a name in the hyperspec if *DOCUMENT-LINK-TO-HYPERSPEC*.

  Symbols are read in the current *PACKAGE*, which is subject to
  *DOCUMENT-NORMALIZE-PACKAGES*.")

(defun codifiable-word-p (word)
  ;; OPT
  (and (notany #'whitespacep word)
       (lowercase-only-in-suffixes-p word)
       (some #'upper-case-p word)))

(defun lowercase-only-in-suffixes-p (string)
  (let ((prev-case nil))
    (loop for char across string
          do (cond ((upper-case-p char)
                    (when (eq prev-case :lower)
                      (return nil))
                    (if (member prev-case '(:upper :upper-2))
                        (setq prev-case :upper-2)
                        (setq prev-case :upper)))
                   ((lower-case-p char)
                    (unless (member prev-case '(:upper-2 :lower))
                      (return nil))
                    (setq prev-case :lower))
                   (t
                    (setq prev-case nil)))
          finally (return t))))

(defun interesting-object-p (object name)
  (flet ((normal-ref-p (ref)
           ;; Matching a CLHS section number (e.g. "A.1" and
           ;; especially "A") must not cause codification of NAME.
           (not (eq (reference-locative-type ref) 'clhs))))
    (or (some #'normal-ref-p
              (references-to-object object :local :include))
        (and (symbolp object)
             (or (<= 3 (length name))
                 (external-symbol-p object)))
        (find-package* object)
        (asdf-system-name-p object))))

;;; The core of the implementation of *DOCUMENT-UPPERCASE-IS-CODE*.
;;;
;;; This is called by MAP-WORDS so the return values are NEW-TREE,
;;; SLICE, N-CHARS-READ. Also called by TRANSLATE-EMPH that expects
;;; only a single return value: the new tree.
(defun translate-uppercase-word (parent tree word)
  (declare (ignore parent))
  (let ((emph (and (listp tree) (eq :emph (first tree)))))
    ;; *DOCUMENT-UPPERCASE-IS-CODE* escaping
    (cond ((and emph (eql #\\ (alexandria:first-elt word)))
           ;; E.g. "*\\DOCUMENT-NORMALIZE-PACKAGES*"
           ;; -> (:EMPH "DOCUMENT-NORMALIZE-PACKAGES")
           (values (list `(:emph ,(subseq word 1))) t (length word)))
          ((eql #\\ (alexandria:first-elt word))
           ;; Discard the leading backslash escape.
           ;; E.g. "\\MGL-PAX" -> "MGL-PAX"
           (values (list (subseq word 1)) t (length word)))
          ((or (not *document-uppercase-is-code*)
               (not (codifiable-word-p word)))
           ;; Don't change anything.
           nil)
          (emph
           (codify-uppercase-word (format nil "*~A*" word)))
          (t
           (codify-uppercase-word word)))))

;;; Find the [approximately] longest @NAME in WORD. Return a 3bmd
;;; parse tree fragment with that substring marked up as code and the
;;; suffixes downcased (so that CLASSES turns into `CLASS`es).
;;;
;;; Handles the rules laid out in *DOCUMENT-UPPERCASE-IS-CODE* not
;;; already handled in the caller TRANSLATE-UPPERCASE-WORD. Trims
;;; separators and depluralizes.
(defun codify-uppercase-word (word)
  (multiple-value-bind (object name)
      (parse-word word :trim t :depluralize t :only-one (constantly t))
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
                t (length word))))))

;;; Handle *DOCUMENT-UPPERCASE-IS-CODE* in normal strings and :EMPH
;;; (to recognize *VAR*). Also, perform consistency checking of
;;; cl-transcript code blocks (see @TRANSCRIBING-WITH-EMACS).
(defun codify (parse-tree)
  (map-markdown-parse-tree
   (list :emph '3bmd-code-blocks::code-block :reference-link :code)
   '(:code :verbatim 3bmd-code-blocks::code-block
     :explicit-link :image :mailto)
   t
   #'translate-to-code
   parse-tree))

;;; This is the first of the translator functions, which are those
;;; passed to MAP-MARKDOWN-PARSE-TREE.
;;;
;;; It is called with with a list TREE whose CAR is :EMPH or
;;; 3BMD-CODE-BLOCKS::CODE-BLOCK or with TREE being a string (as per
;;; the MAP-MARKDOWN-PARSE-TREE above).
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
           (setf (pt-get replacement :label) (codify (pt-get tree :label)))
           replacement))
        ((parse-tree-p tree :code)
         (let ((string (second tree)))
           (if (eql #\\ (alexandria:first-elt string))
               tree
               ;; FIXME: downcase entire code block?
               `(:code ,(maybe-downcase (second tree))))))
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
  ;; FIXME
  "If true, then the names of symbols recognized as code (including
  those found if *DOCUMENT-UPPERCASE-IS-CODE*) are downcased in the
  output if they only consist of uppercase characters. If it is
  :ONLY-IN-MARKUP, then if the output format does not support
  markup (e.g. it's :PLAIN), then no downcasing is performed.")

(defun downcasingp ()
  (or (and *document-downcase-uppercase-code*
           (not (eq *document-downcase-uppercase-code*
                    :only-in-markup)))
      (and (eq *document-downcase-uppercase-code*
               :only-in-markup)
           (not (eq *format* :plain)))))

(defun maybe-downcase (string)
  (if (downcasingp)
      (string-downcase string)
      string))


(defsection @linking-to-code (:title "Linking to Code")
  """In this section, we describe all ways of linking to code
  available when *DOCUMENT-UPPERCASE-IS-CODE* is true.

  _Note that invoking [`M-.`][@navigating-in-emacs section] on the
  @OBJECT of any of the following links will disambiguate based the
  textual context, determining the locative. In a nutshell, if `M-.`
  works without popping up a list of choices, then the documentation
  will contain a single link._
  """
  (*document-link-code* variable)
  (@specified-locative section)
  (@unambiguous-locative section)
  (@ambiguous-locative section)
  (@explicit-and-autolinking section)
  (@preventing-autolinking section)
  (@suppressed-links section)
  (@filtering-ambiguous-references section)
  (@local-references section))

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
    explicit link*) renders as: [see this][document function].
  """)

(defsection @unambiguous-locative (:title "Unambiguous Unspecified Locative")
  """In the following examples, although no locative is specified,
  `\DOCUMENT` names a single @OBJECT being documented, so they all
  render as [DOCUMENT][function].

  - `\[DOCUMENT][]` (*object, explicit link*),
  - `DOCUMENT` (*object, autolink*).

  To override the title:

  - `\[see this][document]` (*title + object, explicit link*) renders
    as: [see this][document].
  """)

(defsection @ambiguous-locative (:title "Ambiguous Unspecified Locative")
  """These examples all render as [SECTION][], linking to both
  definitions of the @OBJECT `\SECTION`, the `\CLASS` and the
  `\LOCATIVE`.

  - `[SECTION][]` (*object, explicit link*)
  - `\SECTION` (*object, autolink*)

  To override the title:

  - `\[see this][section]` (*title + object, explicit link*) renders
    as: [see this][section].
  """)

(defsection @explicit-and-autolinking (:title "Explicit and Autolinking")
  "The examples in the previous sections are marked with *explicit
  link* or *autolink*. Explicit links are those with a Markdown
  reference link spelled out explicitly, while autolinks are those
  without.")

(defsection @preventing-autolinking (:title "Preventing Autolinking")
  """In the common case, when [*DOCUMENT-UPPERCASE-IS-CODE*][] is true,
  prefixing the uppercase @WORD with a backslash prevents it from
  being codified and thus also prevents
  [autolinking][@explicit-and-autolinking section] form
  kicking in. For example, `\\DOCUMENT` renders as \DOCUMENT. If it
  should be marked up as code but not autolinked, the backslash must
  be within backticks like this:

  ```
  `\DOCUMENT`
  ```

  This renders as `\DOCUMENT`. Alternatively, the DISLOCATED or the
  ARGUMENT locative may be used as in `[DOCUMENT][dislocated]`.
  """)

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

;;; This translator
;;;
;;; - handles (:CODE "SOMETHING"), the parse of `SOMETHING`: looks for
;;;   any references to "SOMETHING" (which may name a symbol or a
;;;   package) and tanslates it to, for example, (:REFERENCE-LINK
;;;   :LABEL ((:CODE "SOMETHING")) :DEFINITION "function") if there is
;;;   a single function reference to it.
;;;
;;; - handles :REFERENCE-LINK nodes
;;;
;;;   - those with an explicit locative (:REFERENCE-LINK :LABEL
;;;     ((:CODE "SOMETHING")) :DEFINITION "function"), the parse of
;;;     [`SOMETHING`][function],
;;;
;;;   - and those with no locative (:REFERENCE-LINK :LABEL ((:CODE
;;;     "SOMETHING")) :TAIL "[]"), the parse of [`SOMETHING`][].
(defun translate-to-links (parent tree linked-refs)
  (cond ((parse-tree-p tree :code)
         (let ((string (second tree)))
           (if (alexandria:starts-with #\\ string)
               `(:code ,(subseq string 1))
               (autolink parent tree string linked-refs))))
        ((and (eq :reference-link (first tree)))
         (resolve-reflink tree linked-refs))
        (t
         (assert nil))))

(defun autolink (parent tree word linked-refs)
  (multiple-value-bind (reflinks refs)
      (make-reflinks-to-word parent tree word linked-refs)
    (cond (reflinks
           (dolist (ref refs)
             (vector-push-extend ref linked-refs))
           (values reflinks nil t))
          (t
           tree))))

;;; Translate WORD that's part of TREE (e.g. it's "xxx" from (:CODE
;;; "xxx") or from "xxx,yyy"), or it's constructed from TREE (e.g.
;;; it's "*SYM*" from (:EMPH "SYM")).
(defun make-reflinks-to-word (parent tree word linked-refs)
  (let ((refs (references-for-autolink
               (parse-word word :trim nil :depluralize t)
               (find-locatives-around parent tree)
               linked-refs)))
    (when refs
      (values (make-reflinks `(,tree) nil refs) refs))))

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
      (loop for rest on parent
            do (when (and (eq (third rest) tree)
                          (stringp (second rest))
                          (blankp (second rest)))
                 (try (first rest))
                 (return)))
      ;; For example, (:PLAIN "See" "the" "FOO" " " "function")
      (loop for rest on parent
            do (when (and (eq (first rest) tree)
                          (stringp (second rest))
                          (blankp (second rest)))
                 (try (third rest))
                 (return))))
    locatives))

(defun resolve-reflink (reflink linked-refs)
  ;; Markdown to handle: [`SECTION`][class], [`SECTION`][], [see
  ;; this][section class], [see this][section]. For example, the tree
  ;; for [`SECTION`][class] is (:REFERENCE-LINK :LABEL ((:CODE
  ;; "SECTION")) :DEFINITION "class").
  (multiple-value-bind (label explicit-label-p object locative found)
      (extract-reference-from-reflink reflink)
    (if (not found)
        reflink
        (let ((refs (if locative
                        (references-for-specified-locative object locative)
                        (references-for-explicitly-unspecified-locative
                         object))))
          (cond (refs
                 (dolist (ref refs)
                   (vector-push-extend ref linked-refs))
                 (values (make-reflinks label explicit-label-p refs) nil t))
                (t
                 reflink))))))

;;; Return the label, whether to use the returned label in the
;;; reference link without further transformations (e.g. replace it
;;; with SECTION-TITLE), object, the locative, whether the object and
;;; locative were found.
(defun extract-reference-from-reflink (tree)
  (assert (parse-tree-p tree :reference-link))
  (destructuring-bind (&key label definition tail) (rest tree)
    (let* ((empty-definition-p (and (zerop (length definition))
                                    (or (null tail)
                                        (equal tail "[]"))))
           (definition (parse-tree-to-text definition :deemph t))
           (locative (and definition (read-locative-from-markdown definition)))
           (label-string (parse-tree-to-text label :deemph nil)))
      (alexandria:nth-value-or 0
        (and (or empty-definition-p locative)
             ;; [foo][] or [foo][function]
             (multiple-value-bind (object substring)
                 (and label-string
                      (parse-word
                       label-string :trim nil :depluralize t
                       :only-one (lambda (object)
                                   (if (eq locative 'clhs)
                                       (find-hyperspec-id object
                                                          :substring-match t)
                                       (has-reference-p object)))
                       :clhs-substring-match (eq locative 'clhs)))
               (when substring
                 (values label nil object locative t))))
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
            (values label t object locative t)))))))

(defun label-has-code-p (label)
  (labels ((recurse (e)
             (unless (atom e)
               (when (eq (first e) :code)
                 (return-from label-has-code-p t))
               (mapc #'recurse e))))
    (recurse label)
    nil))

(defun parse-tree-to-text (parse-tree &key deemph)
  (labels
      ((recurse (e)
         (cond ((stringp e)
                ;; "S" -> "S"
                e)
               ((and (listp e)
                     (or (stringp (first e))
                         (listp (first e))))
                ;; ("mgl-pax-test:" (:EMPH "test-variable")) =>
                ;; "mgl-pax-test:*test-variable*"
                (apply #'concatenate 'string (mapcar #'recurse e)))
               ;; Recurse into (:PLAIN ...)
               ((parse-tree-p e :plain)
                (format nil "~A" (recurse (rest e))))
               ;; (:EMPH "S") -> "*S*"
               ((and deemph (parse-tree-p e :emph))
                (format nil "*~A*" (recurse (rest e))))
               ;; (:CODE "S") -> "S"
               ((parse-tree-p e :code)
                (recurse (rest e)))
               (t
                (return-from parse-tree-to-text nil)))))
    (recurse parse-tree)))

;;; For LABEL (a parse tree fragment) and some references to it
;;; (REFS), return a markdown parse tree fragment to be spliced into a
;;; markdown parse tree.
(defun make-reflinks (label explicit-label-p refs)
  (let ((ref-1 (first refs)))
    (cond ((endp refs)
           ;; All references were filtered out.
           label)
          ((< 1 (length refs))
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
             ")"))
          ((member (reference-locative-type ref-1) '(dislocated argument))
           label)
          ;; FIXME: TITLE should be a generic function.
          ((eq (reference-locative-type ref-1) 'section)
           (let ((section (resolve ref-1 :errorp nil)))
             `((:reference-link
                :label ,(if (or explicit-label-p
                                (null (section-title section)))
                            label
                            `(,(section-title section)))
                :definition ,(link-to-reference ref-1)))))
          ((eq (reference-locative-type ref-1) 'glossary-term)
           (let ((glossary-term (resolve ref-1 :errorp nil)))
             `((:reference-link
                :label ,(if (or explicit-label-p
                                (null (glossary-term-title glossary-term)))
                            label
                            `(,(glossary-term-title glossary-term)))
                :definition ,(link-to-reference ref-1)))))
          (t
           `((:reference-link
              :label ,label
              :definition ,(link-to-reference ref-1)))))))

;;; Order REFERENCES in an implementation independent way. REFERENCES
;;; are all to the same object.
(defun sort-references (references)
  (flet ((locative-string (reference)
           (with-standard-io-syntax*
             (prin1-to-string
              (reference-locative reference)))))
    (sort (copy-seq references) #'string< :key #'locative-string)))


(defsection @suppressed-links (:title "Suppressed Links")
  """Within the same docstring,
  [autolinking][@explicit-and-autolinking section] of code (i.e. of
  something like `FOO`) is suppressed if the same @OBJECT was already
  linked to in any way. In the following docstring, only the first
  `FOO` will be turned into a link.

      "`FOO` is safe. `FOO` is great."

  However if a @LOCATIVE was specified or found near the @OBJECT, then
  a link is always made. In the following, in both docstrings, both
  occurrences `FOO` produce links.

      "`FOO` is safe. [`FOO`][macro] is great."
      "`FOO` is safe. Macro `FOO` is great."

  As an exception, links with [specified][@specified-locative section]
  and [unambiguous][@unambiguous-locative section] locatives to
  SECTIONs and GLOSSARY-TERMs always produce a link to allow their
  titles to be displayed properly.

  Finally, [autolinking][@explicit-and-autolinking section] to
  T or NIL is suppressed (see *DOCUMENT-LINK-TO-HYPERSPEC*).
  """)

(defun suppressed-link-p (refs linked-refs)
  (when refs
    ;; Relying on REFS being for the same object (see
    ;; REFERENCES-FOR-AMBIGUOUS-LOCATIVE).
    (let ((object (reference-object (first refs))))
      (or (member object '(t nil))
          (and (loop for ref across linked-refs
                       thereis (reference-object= object ref))
               ;; Replace references to sections and glossary terms
               ;; with their title any number of times.
               (not (and (= (length refs) 1)
                         (typep (resolve (first refs) :errorp nil)
                                '(or section glossary-term)))))))))


(defsection @local-references (:title "Local References")
  """To unclutter the generated output by reducing the number of
  links, the so-called 'local' references (e.g. references to the very
  definition for which documentation is being generated) are treated
  specially. In the following example, there are local references to
  the function FOO and its arguments, so none of them get turned into
  links:

  ```common-lisp
  (defun foo (arg1 arg2)
    "FOO takes two arguments: ARG1 and ARG2."
    t)
  ```

  If linking was desired one could use a @SPECIFIED-LOCATIVE (e.g.
  `[FOO][function]` or `FOO function`), which results in a single
  link. An explicit link with an unspecified locative like `[FOO][]`
  generates links to all references involving the `FOO` symbol except
  the local ones.

  The exact rules for local references are as follows:

  - Unless a locative is [specified][ @specified-locative section], no
    [autolinking][ @explicit-and-autolinking section] is performed for
    @OBJECTS for which there are local references. For example, `FOO`
    does not get any links if there is _any_ local reference with the
    same @OBJECT.

  - With a locative specified (e.g. in the explicit link
    `[FOO][function]` or in the text `the FOO function`), a single
    link is made irrespective of any local references.

  - Explicit links with an unspecified locative (e.g. `[FOO][]`) are
    linked to all non-local references.""")

(defun references-for-specified-locative (object locative)
  (if (member (locative-type locative) '(dislocated argument))
      ;; Handle an explicit [FOO][dislocated] in markdown, for which
      ;; there is no reference in REFS.
      (list (make-reference object 'dislocated))
      (let* ((reference (canonical-reference
                         (make-reference object locative)))
             (object (reference-object reference))
             (ref (find reference (references-to-object object)
                        :test #'reference=)))
        (when (and ref (linkable-ref-p ref))
          (list ref)))))

(defun references-for-explicitly-unspecified-locative (object)
  (resolve-generic-function-and-methods
   (filter-asdf-system-references
    (filter-clhs-references
     (linkable-references
      (references-to-object object :local :exclude))))))

;;; All returned REFERENCES are for the same object.
(defun references-for-autolink (objects locatives linked-refs)
  (or
   ;; Use the first object from OBJECTS with which some LOCATIVES form
   ;; known references.
   (loop for object in objects
           thereis (loop for locative in locatives
                         append (references-for-specified-locative
                                 object locative)))
   ;; Fall back on the no-locative case.
   (loop for object in objects
         for refs = (references-for-autolink-with-unspecified-locative objects)
         until (suppressed-link-p refs linked-refs)
           thereis refs
         until (references-to-object object :local :include))))

(defun references-for-autolink-with-unspecified-locative (objects)
  (loop for object in objects
        ;; The UNTIL heuristic is intended to prevent substrings of
        ;; the name of an argument of a function from getting links.
        ;; This relies on PARSE-WORD returning longer matches first.
        until (has-local-reference-p object)
        append (resolve-generic-function-and-methods
                (filter-asdf-system-references
                 (remove-clhs-references
                  (linkable-references
                   (references-to-object object)))))))

(defun linkable-references (refs)
  (remove-if-not #'linkable-ref-p refs))

(defun linkable-ref-p (ref)
  (declare (special *document-link-sections*))
  (and (or (and *document-link-sections*
                (eq (reference-locative-type ref) 'section))
           *document-link-code*)
       (let ((page (reference-page ref)))
         (or
          ;; These have no pages, but won't result in link anyway.
          ;; Keep them.
          (member (reference-locative-type ref) '(dislocated argument))
          ;; Intrapage links always work.
          (eq *page* page)
          ;; Else we need to know the URI-FRAGMENT of both pages. See
          ;; RELATIVE-PAGE-URI-FRAGMENT. Or PAGE may also be a string
          ;; denoted an absolute URL.
          (or (stringp page)
              (and (page-uri-fragment *page*)
                   (page-uri-fragment page)))))))


(defsection @filtering-ambiguous-references
    (:title "Filtering Ambiguous References")
  """When there are multiple references to link to - as seen in
  @AMBIGUOUS-LOCATIVE - some references are removed by the following
  rules.

  - References to ASDF:SYSTEMs are removed if there are other
    references which are not to ASDF:SYSTEMs. This is because system
    names often collide with the name of a class or function and are
    rarely useful to link to. Use explicit links to ASDF:SYSTEMs, if
    necessary.

  - References to the CLHS are filtered similarly.

  - If references include a GENERIC-FUNCTION locative, then all
    references with LOCATIVE-TYPE [METHOD][locative],
    [ACCESSOR][locative], [READER][locative] and [WRITER][locative]
    are removed to avoid linking to a possibly large number of
    methods.""")

;;; It's rarely useful to link to ASDF systems in an ambiguous
;;; situation, so don't.
(defun filter-asdf-system-references (refs)
  (if (< 1 (length refs))
      (remove 'asdf:system refs :key #'reference-locative-type)
      refs))

(defun filter-clhs-references (refs)
  (if (< 1 (length refs))
      (remove-clhs-references refs)
      refs))

(defun remove-clhs-references (refs)
  (remove 'clhs refs :key #'reference-locative-type))

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


(defsection @linking-to-sections (:title "Linking to Sections")
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
              (cond (*document-link-sections*
                     (anchor object stream)
                     (navigation-link object stream)
                     (format stream "~A" (fancy-navigation object))
                     (heading *heading-level* stream)
                     (if (eq *format* :html)
                         (if link-title-to
                             (format stream " [~A~A][~A]~%~%"
                                     (heading-number) title
                                     (link-to-reference link-title-to))
                             (format stream " <a href=\"#~A\">~A~A</a>~%~%"
                                     (urlencode (reference-to-anchor object))
                                     (heading-number)
                                     (escape-markdown title)))
                         (format stream " ~A~A~%~%" (heading-number)
                                 (escape-markdown title))))
                    (t
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


(defsection @miscellaneous-documentation-printer-variables
    (:title "Miscellaneous Variables")
  (*document-url-versions* variable)
  (*document-min-link-hash-length* variable)
  (*document-mark-up-signatures* variable)
  (*document-normalize-packages* variable))


(defvar *document-url-versions* '(1)
  """A list of versions of PAX [URL][dislocated] formats to support in
  the generated documenation in addition to the latest one.

  PAX emits HTML anchors before the documentation of SECTIONs
  (see @LINKING-TO-SECTIONS) and other things (see @LINKING-TO-CODE).
  For the function `FOO`, in the current version (version 2), the
  anchor is `<a id="MGL-PAX:FOO%20FUNCTION">` and its URL will end
  with `#MGL-PAX:FOO%20FUNCTION`.

  _Note that to make the URL independent of whether a symbol is
  [internal or external][find-symbol] to their SYMBOL-PACKAGE, single
  colon is printed where a double colon would be expected. Package and
  symbol names are both printed verbatim except for escaping colons
  and spaces with a backslash. For exported symbols with no funny
  characters, this coincides with how PRIN1 would print the symbol,
  while having the benefit of making the URL independent of the Lisp
  printer's escaping strategy and producing human-readable output for
  mixed-case symbols. No such promises are made for non-ASCII
  characters, and their URLs may change in future versions. Locatives
  are printed with PRIN1._

  Version 1 was based on the more strict HTML4 standard and the id of
  `FOO` was `"x-28MGL-PAX-3A-3AFOO-20FUNCTION-29"`. V2 has minimal
  clutter and is obviously preferred. However, in order not to break
  external links, by default, both anchors are generated.

  Let's understand the generated Markdown.

  ```
  (defun foo (x))

  (document #'foo)
  => ("<a id=\"x-28MGL-PAX-3AFOO-20FUNCTION-29\"></a>
  <a id=\"MGL-PAX:FOO%20FUNCTION\"></a>
  
  - [function] **FOO** *X*
  ")

  (let ((*document-url-versions* '()))
    (document #'foo))
  => ("<a id=\"MGL-PAX:FOO%20FUNCTION\"></a>
  
  - [function] **FOO** *X*
  ")
  ```
  """)

(defun anchor (object stream)
  (when (member 1 *document-url-versions*)
    (format stream "<a id=~S></a>~%"
            (html4-safe-name (reference-to-anchor-v1 object))))
  (format stream "<a id=~S></a>~%~%"
          (urlencode (reference-to-anchor object))))

;;; Return the unescaped name of the HTML anchor for REFERENCE. See
;;; URLENCODE.
(defun reference-to-anchor (object)
  (let ((reference (canonical-reference object)))
    (with-standard-io-syntax*
      (format nil "~A ~S" (object-to-url-part (reference-object reference))
              (reference-locative reference)))))

(defun reference-to-anchor-v1 (object)
  (let ((reference (canonical-reference object)))
    (with-standard-io-syntax*
      (format nil "(~A ~S)"
              (object-to-url-part (reference-object reference))
              (reference-locative reference)))))

(defun object-to-url-part (object)
  (if (symbolp object)
      (let* ((package (symbol-package object))
             (name (symbol-name object))
             (name-url (print-name-for-url name))
             (cl-package (symbol-package 'print)))
        (cond ((or (eq package cl-package)
                   (multiple-value-bind (cl-symbol status)
                       (find-symbol name cl-package)
                     (and (eq cl-symbol object)
                          (eq status :external))))
               (format nil "~A" name-url))
              ((eq package (symbol-package :if-exists))
               (format nil ":~A" name-url))
              (t
               (format nil "~A:~A"
                       (print-name-for-url (package-name package))
                       name-url))))
      (prin1-to-string object)))

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
                        (urlencode (reference-to-anchor reference))))
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
                 (codify-and-link (prin1-and-escape-markdown object))
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
  "Determines what *PACKAGE* and *READTABLE* are when working with
  generating documentation. If true and documentation is generated for
  a SECTION (including its SECTION-ENTRIES), then SECTION-PACKAGE and
  SECTION-READTABLE of the innermost containing section is used. To
  eliminate ambiguity `[in package ...]` messages are printed right
  after the section heading if necessary. If false, then *PACKAGE* and
  *READTABLE* are left at the current values.")


;;;; Basic DOCUMENT-OBJECT and DESCRIBE-OBJECT methods

(defvar *objects-being-documented* ())

(defmethod document-object :around (object stream)
  (let ((*objects-being-documented* (cons object *objects-being-documented*)))
    (cond ((or (stringp object) (typep object 'reference))
           (call-next-method))
          (t
           (let* ((reference (canonical-reference object))
                  (*reference-being-documented* reference))
             (with-temp-output-to-page (stream (reference-page reference))
               (when (and *document-link-code*
                          (not (typep object 'section))
                          (not (typep object 'asdf:system)))
                 (anchor reference stream))
               (call-next-method object stream)))))))

(defmethod document-object ((reference reference) stream)
  "If REFERENCE can be resolved to a non-reference, call
  DOCUMENT-OBJECT with it, else call LOCATE-AND-DOCUMENT-OBJECT on the
  object, locative-type, locative-args of REFERENCE"
  (let* ((reference (canonical-reference reference))
         (resolved-object (resolve reference)))
    (if (typep resolved-object 'reference)
        (with-temp-output-to-page (stream (reference-page reference))
          (when *document-link-code*
            (anchor reference stream))
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


(defsection @document-implementation-notes
    (:title "Document Generation Implementation Notes")
  """Documentation Generation is supported on ABCL, AllegroCL, CLISP,
  \CCL, CMUCL, ECL and SBCL, but their outputs may differ due to the
  lack of some introspective capability. SBCL generates complete
  output. Compared to that, the following are not supported:

  - COMPILER-MACRO docstrings on ABCL, AllegroCL, \CCL, ECL,
  - DEFTYPE lambda lists on ABCL, AllegroCL, CLISP, \CCL, CMUCL, ECL,
  - default values in MACRO lambda lists on AllegroCL,
  - default values in function lambda lists on \CCL (needs `(DEBUG 3)`
    on AllegroCL),
  - METHOD-COMBINATION docstrings on ABCL, AllegroCL.
  """)
