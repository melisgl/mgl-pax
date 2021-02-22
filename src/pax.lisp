;;;; TODO
;;;;
;;;; - locative aliases for docstrings (object[s] -> class)
;;;;
;;;; - add method-combination locative
;;;;
;;;; - esrap is slow with allegro cl
;;;;
;;;; - link to doc instead of including it if it is referenced
;;;;   multiple times? Which one is the master? Heuristically, the
;;;;   references from sections in the same package are to be
;;;;   preferred.
;;;;
;;;; - todo/comment locative?
;;;;
;;;; - defining link ids doesn't work in function docstrings because
;;;;   it's translated to a list item
;;;;
;;;; - autolinking to the hyperspec
;;;;
;;;; - fake/override documentation for existing stuff
;;;;
;;;; - pretty print lambda-lists (including default values) and values
;;;;   of variables
;;;;
;;;; - make the the documentation generation code easier to understand
;;;;
;;;; - add [link name][(FOO VARIABLE)] kind of link (maybe with a
;;;;   retitling-locative? [FOO][(RETITLE "link name" VARIABLE)])
;;;;
;;;; - port the hyperspec to pax (copyright on lispworks' version is
;;;;   restrictive, allegro?)
;;;;
;;;; - with *DOCUMENT-DOWNCASE-UPPERCASE-CODE* T, noninterned stuff is
;;;;   not downcased, which leads to having to mix casing up in the
;;;;   docstring.
;;;;
;;;; - don't list unexported superclasses?
;;;;
;;;; - include signalled errors in transcript?
;;;;
;;;; - macro names in the docstring link back to their own bullet

(in-package :mgl-pax)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)

(defsection @mgl-pax-manual (:title "PAX Manual")
  "
[![](http://github-actions.40ants.com/svetlyak40wt/mgl-pax/matrix.svg?branch=mgl-pax-minimal)](https://github.com/melisgl/mgl-pax)
"
  (mgl-pax asdf:system)
  (@mgl-pax-links section)
  (@mgl-pax-background section)
  (@mgl-pax-tutorial section)
  (@mgl-pax-emacs-integration section)
  (@mgl-pax-basics section)
  (@mgl-pax-generating-documentation section)
  (@mgl-pax-markdown-support section)
  (@mgl-pax-documentation-printer-variables section)
  (@mgl-pax-locative-types section)
  (@mgl-pax-extension-api section)
  (@mgl-pax-transcript section))

(defsection @mgl-pax-links (:title "Links")
  "Here is the [official
  repository](https://github.com/melisgl/mgl-pax) and the [HTML
  documentation](http://melisgl.github.io/mgl-pax-world/mgl-pax-manual.html)
  for the latest version.")

(defsection @mgl-pax-background (:export nil :title "Background")
  "As a user, I frequently run into documentation that's incomplete
  and out of date, so I tend to stay in the editor and explore the
  code by jumping around with SLIME's [`M-.`][SLIME-M-.]. As a library
  author, I spend a great deal of time polishing code, but precious
  little writing documentation.

  [SLIME-M-.]: http://common-lisp.net/project/slime/doc/html/Finding-definitions.html#Finding-definitions

  In fact, I rarely write anything more comprehensive than docstrings
  for exported stuff. Writing docstrings feels easier than writing a
  separate user manual and they are always close at hand during
  development. The drawback of this style is that users of the library
  have to piece the big picture together themselves.

  That's easy to solve, I thought, let's just put all the narrative
  that holds docstrings together in the code and be a bit like a
  Literate Programming weenie turned inside out. The original
  prototype which did almost everything I wanted was this:

  ```
  (defmacro defsection (name docstring)
    `(defun ,name () ,docstring))
  ```

  Armed with DEFSECTION, I soon found myself organizing code following
  the flow of user level documentation and relegated comments to
  implementational details entirely. However, some portions of
  DEFSECTION docstrings were just listings of all the functions,
  macros and variables related to the narrative, and this list was
  effectively repeated in the DEFPACKAGE form complete with little
  comments that were like section names. A clear violation of
  [OAOO][oaoo], one of them had to go, so DEFSECTION got a list of
  symbols to export.

  [oaoo]: http://c2.com/cgi/wiki?OnceAndOnlyOnce

  That was great, but soon I found that the listing of symbols is
  ambiguous if, for example, a function, a compiler macro and a class
  are named by the same symbol. This did not concern exporting, of
  course, but it didn't help readability. Distractingly, on such
  symbols, `M-.` was popping up selection dialogs. There were two
  birds to kill, and the symbol got accompanied by a type which was
  later generalized into the concept of locatives:

  ```commonlisp
  (defsection @mgl-pax-introduction ()
    \"A single line for one man ...\"
    (foo class)
    (bar function))
  ```

  After a bit of elisp hacking, `M-.` was smart enough to disambiguate
  based on the locative found in the vicinity of the symbol and
  everything was good for a while.

  Then I realized that sections could refer to other sections if there
  were a SECTION locative. Going down that path, I soon began to feel
  the urge to generate pretty documentation as all the necessary
  information was manifest in the DEFSECTION forms. The design
  constraint imposed on documentation generation was that following
  the typical style of upcasing symbols in docstrings there should be
  no need to explicitly mark up links: if `M-.` works, then the
  documentation generator shall also be able find out what's being
  referred to.

  I settled on [Markdown][markdown] as a reasonably non-intrusive
  format, and a few thousand lines later PAX was born.

  [markdown]: https://daringfireball.net/projects/markdown/")

(defsection @mgl-pax-tutorial (:title "Tutorial")
  """PAX provides an extremely poor man's Explorable Programming
  environment. Narrative primarily lives in so called sections that
  mix markdown docstrings with references to functions, variables,
  etc, all of which should probably have their own docstrings.

  The primary focus is on making code easily explorable by using
  SLIME's `M-.` (`slime-edit-definition`). See how to enable some
  fanciness in @MGL-PAX-EMACS-INTEGRATION. Generating documentation
  from sections and all the referenced items in Markdown or HTML
  format is also implemented.

  With the simplistic tools provided, one may accomplish similar
  effects as with Literate Programming, but documentation is generated
  from code, not vice versa and there is no support for chunking yet.
  Code is first, code must look pretty, documentation is code.

  In typical use, PAX packages have no :EXPORT's defined. Instead the
  DEFINE-PACKAGE form gets a docstring which may mention section
  names (defined with DEFSECTION). When the code is loaded into the
  lisp, pressing `M-.` in SLIME on the name of the section will take
  you there. Sections can also refer to other sections, packages,
  functions, etc and you can keep exploring.

  Here is an example of how it all works together:

  ```commonlisp
  (mgl-pax:define-package :foo-random
    (:documentation "This package provides various utilities for
    random. See FOO-RANDOM:@FOO-RANDOM-MANUAL.")
    (:use #:common-lisp #:mgl-pax))

  (in-package :foo-random)

  (defsection @foo-random-manual (:title "Foo Random manual")
    "Here you describe what's common to all the referenced (and
    exported) functions that follow. They work with *FOO-STATE*,
    and have a :RANDOM-STATE keyword arg. Also explain when to
    choose which."
    (foo-random-state class)
    (state (reader foo-random-state))
    "Hey we can also print states!"
    (print-object (method () (foo-random-state t)))
    (*foo-state* variable)
    (gaussian-random function)
    (uniform-random function)
    ;; this is a subsection
    (@foo-random-examples section))

  (defclass foo-random-state ()
    ((state :reader state)))

  (defmethod print-object ((object foo-random-state) stream)
    (print-unreadable-object (object stream :type t)))

  (defvar *foo-state* (make-instance 'foo-random-state)
    "Much like *RANDOM-STATE* but uses the FOO algorithm.")

  (defun uniform-random (limit &key (random-state *foo-state*))
    "Return a random number from the between 0 and LIMIT (exclusive)
    uniform distribution."
    nil)

  (defun gaussian-random (stddev &key (random-state *foo-state*))
    "Return a random number from a zero mean normal distribution with
    STDDEV."
    nil)

  (defsection @foo-random-examples (:title "Examples")
    "Let's see the transcript of a real session of someone working
    with FOO:

    ```cl-transcript
    (values (princ :hello) (list 1 2))
    .. HELLO
    => :HELLO
    => (1 2)

    (make-instance 'foo-random-state)
    ==> #<FOO-RANDOM-STATE >
    ```")
  ```

  Generating documentation in a very stripped down markdown format is
  easy:

  ```commonlisp
  (describe @foo-random-manual)
  ```

  For this example, the generated markdown would look like this:

      # Foo Random manual

      ###### \[in package FOO-RANDOM\]
      Here you describe what's common to all the referenced (and
      exported) functions that follow. They work with *FOO-STATE*,
      and have a :RANDOM-STATE keyword arg. Also explain when to
      choose which.

      - [class] FOO-RANDOM-STATE

      - [reader] STATE FOO-RANDOM-STATE

      Hey we can also print states!

      - [method] PRINT-OBJECT (OBJECT FOO-RANDOM-STATE) STREAM

      - [variable] *FOO-STATE* #<FOO-RANDOM-STATE >

          Much like *RANDOM-STATE* but uses the FOO algorithm.

      - [function] GAUSSIAN-RANDOM STDDEV &KEY (RANDOM-STATE *FOO-STATE*)

          Return a random number from a zero mean normal distribution with
          STDDEV.

      - [function] UNIFORM-RANDOM LIMIT &KEY (RANDOM-STATE *FOO-STATE*)

          Return a random number from the between 0 and LIMIT (exclusive)
          uniform distribution.

      ## Examples

      Let's see the transcript of a real session of someone working
      with FOO:

      ```cl-transcript
      (values (princ :hello) (list 1 2))
      .. HELLO
      => :HELLO
      => (1 2)

      (make-instance 'foo-random-state)
      ==> #<FOO-RANDOM-STATE >

      ```

  More fancy markdown or HTML output with automatic markup and linking
  of uppercase symbol names found in docstrings, section numbering,
  table of contents, etc is possible by calling the DOCUMENT function.

  *One can even generate documentation for different, but related
  libraries at the same time with the output going to different files,
  but with cross-page links being automatically added for symbols
  mentioned in docstrings. See @MGL-PAX-GENERATING-DOCUMENTATION for
  some convenience functions to cover the most common cases.*

  Note how `(VARIABLE *FOO-STATE*)` in the DEFSECTION form both
  exports `*FOO-STATE*` and includes its documentation in
  `@FOO-RANDOM-MANUAL`. The symbols VARIABLE and FUNCTION are just two
  instances of 'locatives' which are used in DEFSECTION to refer to
  definitions tied to symbols. See @MGL-PAX-LOCATIVE-TYPES.

  The transcript in the code block tagged with `cl-transcript` is
  automatically checked for up-to-dateness. See
  @MGL-PAX-TRANSCRIPT.""")

(defsection @mgl-pax-emacs-integration (:title "Emacs Integration")
  "Integration into SLIME's `M-.` (`slime-edit-definition`) allows one
  to visit the source location of the thing that's identified by a
  symbol and the locative before or after the symbol in a buffer. With
  this extension, if a locative is the previous or the next expression
  around the symbol of interest, then `M-.` will go straight to the
  definition which corresponds to the locative. If that fails, `M-.`
  will try to find the definitions in the normal way which may involve
  popping up an xref buffer and letting the user interactively select
  one of possible definitions.

  *Note that the this feature is implemented in terms of
  SWANK-BACKEND:FIND-SOURCE-LOCATION and
  SWANK-BACKEND:FIND-DEFINITIONS whose support varies across the Lisp
  implementations.*

  In the following examples, pressing `M-.` when the cursor is on one
  of the characters of `FOO` or just after `FOO`, will visit the
  definition of function `FOO`:

      function foo
      foo function
      (function foo)
      (foo function)

  In particular, references in a DEFSECTION form are in (SYMBOL
  LOCATIVE) format so `M-.` will work just fine there.

  Just like vanilla `M-.`, this works in comments and docstrings. In
  this example pressing `M-.` on `FOO` will visit `FOO`'s default
  method:

  ```commonlisp
  ;;;; See FOO `(method () (t t t))` for how this all works.
  ;;;; But if the locative has semicolons inside: FOO `(method
  ;;;; () (t t t))`, then it won't, so be wary of line breaks
  ;;;; in comments.
  ```

  With a prefix argument (`C-u M-.`), one can enter a symbol plus a
  locative separated by whitespace to preselect one of the
  possibilities.

  The `M-.` extensions can be enabled by adding this to your Emacs
  initialization file (or loading `src/pax.el`):"
  (pax.el (include #.(asdf:system-relative-pathname :mgl-pax "src/pax.el")
                   :header-nl "```elisp" :footer-nl "```")))

;;; Return one source location for the thing that can be located with
;;; NAME (a string) and LOCATIVE-STRING. Called from the elisp
;;; function slime-locate-definition. It's like LOCATE but takes
;;; string arguments and returns a location suitable for
;;; make-slime-xref.
(defun locate-definition-for-emacs (name locative-string)
  (let ((locative-string (trim-whitespace locative-string)))
    (swank-backend::converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (or
         ;; SECTION class and class SECTION
         ;; SECTION `class` and `class` SECTION
         ;; `SECTION` class and class `SECTION`
         ;; `SECTION` `class` and `class` `SECTION`
         (ignore-errors
          (locate-definition-for-emacs-1 name locative-string))
         ;; [SECTION][(class)] gets here as NAME="[SECTION][",
         ;; LOCATIVE-STRING="(class)".
         (ignore-errors
          (locate-definition-for-emacs-1 (string-trim "[]" name)
                                         locative-string))
         ;; [SECTION][class] gets here as NAME="[SECTION][class]",
         ;; LOCATIVE-STRING=garbage.
         (ignore-errors
          (locate-reference-link-definition-for-emacs name))
         ;; [DEFSECTION][]
         (let* ((swank:*find-definitions-left-trim* "[#:<")
                (swank:*find-definitions-right-trim* "][,:.>sS")
                (locations (swank:find-definitions-for-emacs name)))
           (if (= (length locations) 1)
               (first (rest (first locations)))
               nil)))))))

;;; Handle references with quoted or non-quoted symbols and locatives.
;;; Since SECTION is both a class and and a documented symbol it
;;; serves as a good example.
(defun locate-definition-for-emacs-1 (name locative-string)
  (multiple-value-bind (symbol found)
      (swank::find-definitions-find-symbol-or-package name)
    (when found
      (let ((locative (read-marked-up-locative-from-string locative-string)))
        (when locative
          (let ((thing (locate symbol locative :errorp nil)))
            (when thing
              (find-source thing))))))))

(defun read-marked-up-locative-from-string (string)
  (let ((*read-eval* nil)
        (string (if (or (alexandria:starts-with #\` string)
                        (alexandria:starts-with #\' string))
                    (subseq string 1)
                    string)))
    (read-locative-from-string string)))

;;; Ensure that some Swank internal facilities (such as
;;; SWANK::FIND-DEFINITIONS-FIND-SYMBOL-OR-PACKAGE,
;;; SWANK::WITH-BUFFER-SYNTAX, SWANK::PARSE-SYMBOL) are operational
;;; even when not running under Slime.
(defmacro with-swank (() &body body)
  `(let* ((swank::*buffer-package* (if (boundp 'swank::*buffer-package*)
                                       swank::*buffer-package*
                                       *package*))
          (swank::*buffer-readtable*
            (if (boundp 'swank::*buffer-readtable*)
                swank::*buffer-readtable*
                (swank::guess-buffer-readtable swank::*buffer-package*))))
     ,@body))

;;; Like READ-FROM-STRING, but try to avoid interning symbols.
(defun read-locative-from-string (string)
  (let ((swank::*buffer-package* *package*))
    (multiple-value-bind (symbol found)
        (with-swank ()
          (swank::find-definitions-find-symbol-or-package string))
      (if found
          symbol
          (let ((first-char-pos (position-if-not #'whitespacep string)))
            (when (and first-char-pos
                       (char= #\())
              ;; Looks like a list. The first element must be an
              ;; interned symbol naming a locative.
              (let ((delimiter-pos (position-if #'delimiterp string
                                                :start (1+ first-char-pos))))
                (multiple-value-bind (symbol found)
                    (swank::parse-symbol
                     (subseq string (1+ first-char-pos) delimiter-pos))
                  (declare (ignore symbol))
                  (when found
                    ;; The rest of the symbols in the string need not be
                    ;; already interned, so let's just read it.
                    (ignore-errors (let ((*read-eval* t))
                                     (read-from-string string))))))))))))

(defun locate-reference-link-definition-for-emacs (string)
  (when (and (= 2 (count #\[ string))
             (= 2 (count #\] string)))
    (let ((first-open (position #\[ string))
          (first-close (position #\] string))
          (second-open (position #\[ string :from-end t))
          (second-close (position #\] string :from-end t)))
      (when (< first-open first-close second-open second-close)
        (locate-definition-for-emacs-1
         (string-trim "`" (subseq string (1+ first-open) first-close))
         (subseq string (1+ second-open) second-close))))))


(defsection @mgl-pax-basics (:title "Basics")
  "Now let's examine the most important pieces in detail."
  (defsection macro)
  (*discard-documentation-p* variable)
  (define-package macro)
  (document function))

(defun section-title-or-name (section)
  (or (section-title section)
      (maybe-downcase (prin1-to-string (section-name section)))))

;;;; Generating documentation

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
  FORMAT can be anything [3BMD][3bmd] supports which is
  currently :MARKDOWN, :HTML and :PLAIN. STREAM may be a stream
  object, T or NIL as with CL:FORMAT.

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

  HEADER-FN, if not NIL, is a function of a single stream argument
  which is called just before the first write to the page.
  Since :FORMAT :HTML only generates HTML fragments, this makes it
  possible to print arbitrary headers, typically setting the title,
  css stylesheet, or charset.

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
              (emit-footer stream))
            (unless (eq format :markdown)
              (let ((markdown-string (with-temp-input-from-page (stream page)
                                       (read-stream-into-string stream))))
                (delete-stream-spec (page-temp-stream-spec page))
                (with-final-output-to-page (stream page)
                  (when (page-header-fn page)
                    (funcall (page-header-fn page) stream))
                  (3bmd:parse-string-and-print-to-stream markdown-string
                                                         stream :format format)
                  (when (page-footer-fn page)
                    (funcall (page-footer-fn page) stream)))))
            (push (unmake-stream-spec (page-final-stream-spec page)) outputs))
          (if (and stream (endp pages))
              (first outputs)
              (reverse outputs)))))))

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

(defsection @mgl-pax-markdown-syntax-highlighting (:title "Syntax highlighting")
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
  they were marked up with backticks which is especially useful when
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

  This situation occurs in PAX with SECTION which is both a class (see
  [SECTION][class]) and a locative type denoted by a symbol (see
  [SECTION][locative]). Back in the example above, clearly,
  there is no reason to link to type `BAR`, so one may wish to select
  the function locative. There are two ways to do that. One is to
  specify the locative explicitly as the id of a reference link:

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
  sections which allows the table of contents to contain links and
  also code-like references to sections (like `@FOO-MANUAL`) to be
  translated to links with the section title being the name of the
  link.")

(defparameter *document-min-link-hash-length* 4
  "Recall that markdown reference style links (like `[label][id]`) are
  used for linking to sections and code. It is desirable to have ids
  that are short to maintain legibility of the generated markdown, but
  also stable to reduce the spurious diffs in the generated
  documentation which can be a pain in a version control system.

  Clearly, there is a tradeoff here. This variable controls how many
  characters of the md5 sum of the full link id (the reference as a
  string) are retained. If collisions are found due to the low number
  of characters, then the length of the hash of the colliding
  reference is increased.

  This variable has no effect on the HTML generated from markdown, but
  it can make markdown output more readable.")

(defun hash-link (string detect-collision-fn
                  &key (min-n-chars *document-min-link-hash-length*))
  (let ((hex (ironclad:byte-array-to-hex-string
              (ironclad:digest-sequence 'ironclad:md5
                                        (babel:string-to-octets string)))))
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
  contents which includes a nested tree of section titles whose depth
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
             '(:emph 3bmd-code-blocks::code-block)
             '(:code :verbatim 3bmd-code-blocks::code-block
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
      ;; would lead to the same object/reference, but there is no sane
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


(defsection @mgl-pax-locative-types (:title "Locative Types")
  "These are the locatives type supported out of the box. As all
  locative types, they are symbols and their names should make it
  obvious what kind of things they refer to. Unless otherwise noted,
  locatives take no arguments."
  (asdf:system locative)
  (section locative)
  (variable locative)
  (constant locative)
  (macro locative)
  (compiler-macro locative)
  (function locative)
  (generic-function locative)
  (method locative)
  (accessor locative)
  (reader locative)
  (writer locative)
  (structure-accessor locative)
  (class locative)
  (condition locative)
  (restart locative)
  (define-restart macro)
  (type locative)
  (package locative)
  (dislocated locative)
  (argument locative)
  (locative locative)
  (glossary-term locative)
  (define-glossary-term macro)
  (include locative))


(defsection @mgl-pax-extension-api (:title "Extension API")
  (@mgl-pax-locatives-and-references section)
  (@mgl-pax-new-object-types section)
  (@mgl-pax-reference-based-extensions section)
  (@mgl-pax-sections section))


(defsection @mgl-pax-locatives-and-references
    (:title "Locatives and References")
  "While Common Lisp has rather good introspective abilities, not
  everything is first class. For example, there is no object
  representing the variable defined with `(DEFVAR
  FOO)`. `(MAKE-REFERENCE 'FOO 'VARIABLE)` constructs a REFERENCE that
  captures the path to take from an object (the symbol FOO) to an
  entity of interest (for example, the documentation of the variable).
  The path is called the locative. A locative can be applied to an
  object like this:

  ```
  (locate 'foo 'variable)
  ```

  which will return the same reference as `(MAKE-REFERENCE 'FOO
  'VARIABLE)`. Operations need to know how to deal with references
  which we will see in LOCATE-AND-COLLECT-REACHABLE-OBJECTS,
  LOCATE-AND-DOCUMENT and LOCATE-AND-FIND-SOURCE.

  Naturally, `(LOCATE 'FOO 'FUNCTION)` will simply return `#'FOO`, no
  need to muck with references when there is a perfectly good object."
  (locate function)
  (locate-error condition)
  (locate-error-message (reader locate-error))
  (locate-error-object (reader locate-error))
  (locate-error-locative (reader locate-error))
  (resolve function)
  (reference class)
  (reference-object (reader reference))
  (reference-locative (reader reference))
  (make-reference function)
  (locative-type function)
  (locative-args function))

(defun locate (object locative &key (errorp t))
  "Follow LOCATIVE from OBJECT and return the object it leads to or a
  REFERENCE if there is no first class object corresponding to the
  location. If ERRORP, then a LOCATE-ERROR condition is signaled when
  the lookup fails."
  (handler-case
      (locate-object object (locative-type locative)
                     (locative-args locative))
    (locate-error (e)
      (when errorp
        (error 'locate-error :message (locate-error-message e)
               :object object :locative locative)))))

(define-condition locate-error (error)
  ((message :initarg :message :reader locate-error-message)
   (object :initarg :object :reader locate-error-object)
   (locative :initarg :locative :reader locate-error-locative))
  (:documentation "Signaled by LOCATE when the lookup fails and ERRORP
  is true.")
  (:report (lambda (condition stream)
             (format stream "~@<Could not locate ~A ~A.~@[ ~A~]~:@>"
                     (locate-error-locative condition)
                     (locate-error-object condition)
                     (locate-error-message condition)))))

(defun resolve (reference &key (errorp t))
  "A convenience function to LOCATE REFERENCE's object with its
  locative."
  (locate (reference-object reference) (reference-locative reference)
          :errorp errorp))


(defsection @mgl-pax-new-object-types (:title "Adding New Object Types")
  "One may wish to make the DOCUMENT function and `M-.` navigation
  work with new object types. Extending DOCUMENT can be done by
  defining a DOCUMENT-OBJECT method. To allow these objects to be
  referenced from DEFSECTION, a LOCATE-OBJECT method is to be defined.
  Finally, for `M-.` FIND-SOURCE can be specialized. Finally,
  EXPORTABLE-LOCATIVE-TYPE-P may be overridden if exporting does not
  makes sense. Here is a stripped down example of how all this is done
  for ASDF:SYSTEM:"
  (asdf-example (include (:start (asdf:system locative)
                          :end (end-of-asdf-example variable))
                         :header-nl "```commonlisp"
                         :footer-nl "```"))
  (define-locative-type macro)
  (exportable-locative-type-p generic-function)
  (locate-object generic-function)
  (locate-error function)
  (canonical-reference generic-function)
  (collect-reachable-objects generic-function)
  (collect-reachable-objects (method () (t)))
  (document-object generic-function)
  (document-object (method () (string t)))
  (find-source generic-function))

(defmacro define-locative-type (locative-type lambda-list &body docstring)
  """Declare LOCATIVE-TYPE as a [LOCATIVE][locative]. One gets two
  things in return: first, a place to document the format and
  semantics of LOCATIVE-TYPE (in LAMBDA-LIST and DOCSTRING); second,
  being able to reference `(LOCATIVE-TYPE LOCATIVE)`. For example, if
  you have:

  ```common-lisp
  (define-locative-type variable (&optional initform)
    "Dummy docstring.")
  ```

  then `(VARIABLE LOCATIVE)` refers to this form."""
  (assert (or (endp docstring)
              (and (= 1 (length docstring))
                   (string (first docstring)))))
  `(defmethod locative-lambda-list ((symbol (eql ',locative-type)))
     ,@docstring
     ',lambda-list))

;;; A somewhat dummy generic function on which the docstring can be
;;; hung and which provides a source location. It returns LAMBDA-LIST
;;; from DEFINE-LOCATIVE-TYPE.
(defgeneric locative-lambda-list (symbol))

(defgeneric locate-object (object locative-type locative-args)
  (:documentation "Return the object, to which OBJECT and the locative
  refer. For example, if LOCATIVE-TYPE is the symbol PACKAGE, this
  returns `(FIND-PACKAGE SYMBOL)`. Signal a LOCATE-ERROR condition by
  calling the LOCATE-ERROR function if the lookup fails. Signal other
  errors if the types of the argument are bad, for instance
  LOCATIVE-ARGS is not the empty list in the package example. If a
  REFERENCE is returned then it must be canonical in the sense that
  calling CANONICAL-REFERENCE on it will return the same reference.
  For extension only, don't call this directly."))

(defun locate-error (&rest format-and-args)
  "Call this function to signal a LOCATE-ERROR condition from a
  LOCATE-OBJECT method. FORMAT-AND-ARGS contains a format string and
  args suitable for FORMAT from which the LOCATE-ERROR-MESSAGE is
  constructed. If FORMAT-AND-ARGS is NIL, then the message will be NIL
  too.

  The object and the locative are not specified, they are added by
  LOCATE when it resignals the condition."
  (error 'locate-error :message (if format-and-args
                                    (apply #'format nil format-and-args)
                                    nil)))

(defgeneric canonical-reference (object)
  (:documentation "Return a REFERENCE that resolves to OBJECT."))

(defmethod canonical-reference ((reference reference))
  (handler-case
      (let ((object (resolve reference)))
        (if (typep object 'reference)
            object
            (canonical-reference object)))
    (locate-error ()
      ;; DISLOCATED ends up here
      reference)))

(defgeneric collect-reachable-objects (object)
  (:documentation "Return a list of objects representing all things
  that would be documented in a (DOCUMENT OBJECT) call. For sections
  this is simply the union of references reachable from references in
  SECTION-ENTRIES. The returned objects can be anything provided that
  CANONICAL-REFERENCE works on them. The list need not include OBJECT
  itself.

  One only has to specialize this for new container-like objects."))

(defmethod collect-reachable-objects (object)
  "This default implementation returns the empty list. This means that
  nothing is reachable from OBJECT."
  (declare (ignore object))
  ())

(defgeneric document-object (object stream)
  (:documentation "Write OBJECT (and its references recursively) in
  FORMAT to STREAM.

  The DOCUMENT function calls this generic function with LEVEL 0,
  passing FORMAT on. Add methods specializing on OBJECT to customize
  how objects of that type are presented in the documentation."))

(defmethod document-object ((string string) stream)
  "Print STRING verbatim to STREAM after cleaning up indentation.

  Docstrings in sources are indented in various ways which can easily
  mess up markdown. To handle the most common cases leave the first
  line alone, but from the rest of the lines strip the longest run of
  leading spaces that is common to all non-blank lines."
  (format stream "~a~%"
          (massage-docstring string :indentation "")))

;;; This is bound to an EQUAL hash table in MAKE-GITHUB-SOURCE-URI-FN
;;; to speed up FIND-SOURCE. It's still very slow though.
(defvar *find-source-cache* nil)

(defgeneric find-source (object)
  (:documentation """Like SWANK:FIND-DEFINITION-FOR-THING, but this
  one is a generic function to be extensible. In fact, the default
  implementation simply defers to SWANK:FIND-DEFINITION-FOR-THING.
  This function is called by LOCATE-DEFINITION-FOR-EMACS which lies
  behind the `M-.` extension (see @MGL-PAX-EMACS-INTEGRATION).

  If successful, the return value looks like this:

  ```commonlisp
  (:location (:file "/home/mega/own/mgl/pax/test/test.lisp")
             (:position 24) nil)
  ```

  The NIL is the source snippet which is optional. Note that position
  1 is the first character. If unsuccessful, the return values is
  like:

  ```commonlisp
  (:error "Unknown source location for SOMETHING")
  ```""")
  (:method :around (object)
    (if *find-source-cache*
        (let ((key (if (typep object 'reference)
                       (list (reference-object object)
                             (reference-locative object))
                       object)))
          (or (gethash key *find-source-cache*)
              (setf (gethash key *find-source-cache*)
                    (call-next-method))))
        (call-next-method)))
  (:method (object)
    (swank:find-definition-for-thing object)))

;;; A utility for writing FIND-SOURCE methods. Try FILTER-STRINGS one
;;; by one, and if one matches exactly one of LOCATIONS, then return
;;; that location. Matching is performed by substring search on the
;;; stringified first element of the location.
(defun find-one-location (locations filter-strings)
  (let ((n-matches ()))
    (loop for filter-string in filter-strings
          do (let ((filtered-locations
                     (filter-locations locations filter-string)))
               (cond ((= 1 (length filtered-locations))
                      ;; A location looks like this in SBCL:
                      ;;
                      ;; ((DEFVAR *FOO*)
                      ;;  (:LOCATION
                      ;;   (:BUFFER-AND-FILE "pax.lisp"
                      ;;    "/home/mega/own/mgl/pax/src/pax.lisp")
                      ;;   (:OFFSET 106 0) (:SNIPPET "(defvar *foo*)")))
                      (return-from find-one-location
                        (second (first filtered-locations))))
                     (t
                      (push (length filtered-locations) n-matches)))))
    (error "~@<Could not find a single location in with filters ~S. ~
           Number of matches for each filter ~S.~:@>"
           filter-strings n-matches)))

(defun filter-locations (locations filter-string)
  (remove-if-not (lambda (location)
                   (let ((location-as-string
                           (prin1-to-string (first location))))
                     (search filter-string location-as-string
                             :test #'equalp)))
                 locations))


(defsection @mgl-pax-reference-based-extensions
    (:title "Reference Based Extensions")
  "Let's see how to extend DOCUMENT and `M-.` navigation if there is
  no first class object to represent the thing of interest. Recall
  that LOCATE returns a REFERENCE object in this case. DOCUMENT-OBJECT
  and FIND-SOURCE defer to LOCATE-AND-DOCUMENT and
  LOCATE-AND-FIND-SOURCE, which have LOCATIVE-TYPE in their argument
  list for EQL specializing pleasure. Here is a stripped down example
  of how the VARIABLE locative is defined:"
  (variable-example (include (:start (variable locative)
                                     :end (end-of-variable-example variable))
                             :header-nl "```commonlisp"
                             :footer-nl "```"))
  (collect-reachable-objects (method () (reference)))
  (locate-and-collect-reachable-objects generic-function)
  (locate-and-collect-reachable-objects (method () (t t t)))
  (document-object (method () (reference t)))
  (locate-and-document generic-function)
  (find-source (method () (reference)))
  (locate-and-find-source generic-function)
  (locate-and-find-source (method () (t t t)))
  "We have covered the basic building blocks of reference based
  extensions. Now let's see how the obscure
  DEFINE-SYMBOL-LOCATIVE-TYPE and
  DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE macros work together to
  simplify the common task of associating definition and documentation
  with symbols in a certain context."
  (define-symbol-locative-type macro)
  (define-definer-for-symbol-locative-type macro))

(defmethod collect-reachable-objects ((reference reference))
  "If REFERENCE can be resolved to a non-reference, call
  COLLECT-REACHABLE-OBJECTS with it, else call
  LOCATE-AND-COLLECT-REACHABLE-OBJECTS on the object, locative-type,
  locative-args of REFERENCE"
  (let ((object (resolve reference)))
    (if (typep object 'reference)
        (let ((locative (reference-locative reference)))
          (locate-and-collect-reachable-objects (reference-object reference)
                                                (locative-type locative)
                                                (locative-args locative)))
        (collect-reachable-objects object))))

(defgeneric locate-and-collect-reachable-objects (object locative-type
                                                  locative-args)
  (:documentation "Called by COLLECT-REACHABLE-OBJECTS on REFERENCE
  objects, this function has essentially the same purpose as its
  caller but it has different arguments to allow specializing on
  LOCATIVE-TYPE."))

(defmethod locate-and-collect-reachable-objects (object locative-type
                                                 locative-args)
  "This default implementation returns the empty list. This means that
  nothing is reachable from the reference."
  (declare (ignore object locative-type locative-args))
  ())

;;; We need this for more informative TRANSCRIBE-ERRORs.
(defvar *reference-being-documented* nil)

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

(defgeneric locate-and-document (object locative-type locative-args
                                 stream)
  (:documentation "Called by DOCUMENT-OBJECT on REFERENCE objects,
  this function has essentially the same purpose as DOCUMENT-OBJECT
  but it has different arguments to allow specializing on
  LOCATIVE-TYPE."))

(defmethod find-source ((reference reference))
  "If REFERENCE can be resolved to a non-reference, call FIND-SOURCE
  with it, else call LOCATE-AND-FIND-SOURCE on the object,
  locative-type, locative-args of REFERENCE"
  (let ((locative (reference-locative reference)))
    (locate-and-find-source (reference-object reference)
                            (locative-type locative)
                            (locative-args locative))))

(defgeneric locate-and-find-source (object locative-type locative-args)
  (:documentation "Called by FIND-SOURCE on REFERENCE objects, this
  function has essentially the same purpose as FIND-SOURCE but it has
  different arguments to allow specializing on LOCATIVE-TYPE."))

(defmethod locate-and-find-source (object locative-type locative-args)
  "This default implementation simply calls FIND-SOURCE with OBJECT
  which should cover the common case of a macro expanding to, for
  instance, a defun but having its own locative type."
  (declare (ignore locative-type locative-args))
  (find-source object))

(defmacro define-symbol-locative-type (locative-type lambda-list
                                       &body docstring)
  """Similar to DEFINE-LOCATIVE-TYPE but it assumes that all things
  locatable with LOCATIVE-TYPE are going to be just symbols defined
  with a definer defined with DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE.
  It is useful to attach documentation and source location to symbols
  in a particular context. An example will make everything clear:

  ```commonlisp
  (define-symbol-locative-type direction ()
    "A direction is a symbol. (After this `M-.` on `DIRECTION LOCATIVE`
    works and it can also be included in DEFSECTION forms.)")

  (define-definer-for-symbol-locative-type define-direction direction
    "With DEFINE-DIRECTION one can document what a symbol means when
    interpreted as a direction.")

  (define-direction up ()
    "UP is equivalent to a coordinate delta of (0, -1).")
  ```

  After all this, `(UP DIRECTION)` refers to the `DEFINE-DIRECTION`
  form above."""
  (check-body-docstring docstring)
  `(progn
     (define-locative-type ,locative-type ,lambda-list ,@docstring)
     (defmethod locate-object
         (symbol (locative-type (eql ',locative-type)) locative-args)
       (or (symbol-lambda-list-method symbol ',locative-type)
           (locate-error))
       (make-reference symbol (cons locative-type locative-args)))
     (defmethod locate-and-document
         (symbol (locative-type (eql ',locative-type)) locative-args stream)
       (let ((method (symbol-lambda-list-method symbol ',locative-type))
             (lambda-list (symbol-lambda-list symbol ',locative-type)))
         (locate-and-print-bullet locative-type locative-args symbol stream)
         (with-dislocated-symbols ((macro-arg-names lambda-list))
           (when lambda-list
             (write-char #\Space stream)
             (print-arglist lambda-list stream))
           (print-end-bullet stream)
           (maybe-print-docstring method t stream)))
       (format stream "~&"))
     (defmethod locate-and-find-source
         (symbol (locative-type (eql ',locative-type)) locative-args)
       (find-source (symbol-lambda-list-method symbol ',locative-type)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun check-body-docstring (docstring)
    (assert (or (endp docstring)
                (and (= 1 (length docstring))
                     (string (first docstring)))))))

;;; A somewhat dummy generic function whose methods are
;;; eql-specialized on SYMBOL and LOCATIVE-TYPE. The appropriate
;;; method's docstring is the docstring of SYMBOL as LOCATIVE-TYPE. As
;;; an afterthought, this method also returns the LAMBDA-LIST given in
;;; the definition.
(defgeneric symbol-lambda-list (symbol locative-type))

(defun symbol-lambda-list-method (symbol locative-type)
  (find-method #'symbol-lambda-list () `((eql ,symbol) (eql ,locative-type))
               nil))

(defmacro define-definer-for-symbol-locative-type
    (name locative-type &body docstring)
  "Define a macro with NAME which can be used to attach documentation,
  a lambda-list and source location to a symbol in the context of
  LOCATIVE-TYPE. The defined macro's arglist is (SYMBOL LAMBDA-LIST
  &OPTIONAL DOCSTRING). LOCATIVE-TYPE is assumed to have been defined
  with DEFINE-SYMBOL-LOCATIVE-TYPE."
  `(defmacro ,name (symbol lambda-list &body docstring)
     ,@docstring
     `,(expand-define-definer-for-symbol-as-locative-definer-body
        symbol ',locative-type lambda-list docstring)))

(defun expand-define-definer-for-symbol-as-locative-definer-body
    (symbol locative-type lambda-list docstring)
  `(defmethod symbol-lambda-list ((symbol (eql ',symbol))
                                  (locative-type (eql ',locative-type)))
     ,@docstring
     ',lambda-list))


(defsection @mgl-pax-sections (:title "Sections")
  "[Section][class] objects rarely need to be dissected since
  DEFSECTION and DOCUMENT cover most needs. However, it is plausible
  that one wants to subclass them and maybe redefine how they are
  presented."
  (section class)
  (section-name (reader section))
  (section-package (reader section))
  (section-readtable (reader section))
  (section-title (reader section))
  (section-link-title-to (reader section))
  (section-entries (reader section))
  (describe-object (method () (section t))))

(defmethod describe-object ((section section) stream)
  "[SECTION][class] objects are printed by calling DOCUMENT on them
  with all @MGL-PAX-DOCUMENTATION-PRINTER-VARIABLES, except for
  *DOCUMENT-NORMALIZE-PACKAGES*, turned off to reduce clutter."
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


;;;; LOCATIVE locative

(define-locative-type locative (lambda-list)
  "This is the locative for locatives. When `M-.` is pressed on
  `VARIABLE` in `(VARIABLE LOCATIVE)`, this is what makes it possible
  to land at the `(DEFINE-LOCATIVE-TYPE VARIABLE ...)` form.
  Similarly, `(LOCATIVE LOCATIVE)` leads to this very definition.")

(defmethod locate-object (symbol (locative-type (eql 'locative)) locative-args)
  (assert (endp locative-args))
  (or (ignore-errors (locative-lambda-list-method-for-symbol symbol))
      (locate-error))
  (make-reference symbol (cons locative-type locative-args)))

(defun locative-lambda-list-method-for-symbol (symbol)
  (find-method #'locative-lambda-list () `((eql ,symbol))))

(defmethod locate-and-document (symbol (locative-type (eql 'locative))
                                locative-args stream)
  (let ((method (locative-lambda-list-method-for-symbol symbol))
        (lambda-list (locative-lambda-list symbol)))
    (locate-and-print-bullet locative-type locative-args symbol stream)
    (with-dislocated-symbols ((macro-arg-names lambda-list))
      (when lambda-list
        (write-char #\Space stream)
        (print-arglist lambda-list stream))
      (print-end-bullet stream)
      (with-dislocated-symbols ((list symbol))
        (maybe-print-docstring method t stream))))
  (format stream "~&"))

(defmethod locate-and-find-source (symbol (locative-type (eql 'locative))
                                   locative-args)
  (declare (ignore locative-args))
  (find-source (locative-lambda-list-method-for-symbol symbol)))


;;;; SECTION locative

(define-locative-type section ()
  "Refers to a section defined by DEFSECTION.")

(defmethod locate-object (symbol (locative-type (eql 'section))
                          locative-args)
  (declare (ignore locative-args))
  (assert (typep (symbol-value symbol) 'section))
  (symbol-value symbol))

(defmethod canonical-reference ((section section))
  (make-reference (section-name section) 'section))

(defmethod collect-reachable-objects ((section section))
  (mapcan (lambda (reference)
            (cons reference (collect-reachable-objects reference)))
          (remove-if-not (lambda (entry)
                           (typep entry 'reference))
                         (section-entries section))))

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

(defvar *section*)

(defmethod document-object ((section section) stream)
  (let ((same-package (eq *package* (section-package section)))
        (*package* (if *document-normalize-packages*
                       (section-package section)
                       *package*))
        (*readtable* (section-readtable section))
        (*section* section))
    (with-heading (stream section (section-title-or-name section)
                          :link-title-to (section-link-title-to section))
      (when (and *document-normalize-packages* (not same-package))
        (format stream "###### \\[in package ~A~A\\]~%" (package-name *package*)
                (if (package-nicknames *package*)
                    (format nil " with nicknames ~{~A~^, ~}" (package-nicknames *package*))
                    "")))
      (let ((firstp t))
        (dolist (entry (section-entries section))
          (if firstp
              (setq firstp nil)
              (terpri stream))
          (with-nested-headings ()
            (document-object entry stream)))))))

(defmethod find-source ((section section))
  (locate-and-find-source (section-name section) 'variable ()))


;;;; VARIABLE locative

(define-locative-type variable (&optional initform)
  "Refers to a global special variable. INITFORM, or if not specified,
  the global value of the variable is included in the documentation.")

(defmethod locate-object (symbol (locative-type (eql 'variable)) locative-args)
  (assert (<= (length locative-args) 1))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'variable))
                                locative-args stream)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (locate-and-print-bullet locative-type locative-args symbol stream)
    (write-char #\Space stream)
    (multiple-value-bind (value unboundp) (symbol-global-value symbol)
      (print-arglist (prin1-to-string (cond (initformp initform)
                                            (unboundp "-unbound-")
                                            (t value)))
                     stream))
    (print-end-bullet stream)
    (with-dislocated-symbols ((list symbol))
      (maybe-print-docstring symbol locative-type stream))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'variable))
                                   locative-args)
  (declare (ignore locative-args))
  (find-one-location (swank-backend:find-definitions symbol)
                     '("variable" "defvar" "defparameter"
                       "special-declaration")))

(defvar end-of-variable-example)


;;;; CONSTANT locative

(define-locative-type constant (&optional initform)
  "Refers to a DEFCONSTANT. INITFORM, or if not specified,
  the value of the constant is included in the documentation.")

(defmethod locate-object (symbol (locative-type (eql 'constant)) locative-args)
  (assert (<= (length locative-args) 1))
  (assert (constantp symbol))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'constant))
                                locative-args stream)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (locate-and-print-bullet locative-type locative-args symbol stream)
    (write-char #\Space stream)
    (print-arglist (prin1-to-string (cond (initformp
                                           initform)
                                          ((boundp symbol)
                                           (symbol-value symbol))
                                          (t
                                           "<unbound>")))
                   stream)
    (print-end-bullet stream)
    (with-dislocated-symbols ((list symbol))
      (maybe-print-docstring symbol locative-type stream))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'constant))
                                   locative-args)
  (declare (ignore locative-args))
  (find-one-location (swank-backend:find-definitions symbol)
                     '("defconstant" "constant" "variable")))


;;;; ASDF:SYSTEM locative

(define-locative-type asdf:system ()
  "Refers to an asdf system. The generated documentation will include
  meta information extracted from the system definition. This also
  serves as an example of a symbol that's not accessible in the
  current package and consequently is not exported.")

(defmethod locate-object (symbol (locative-type (eql 'asdf:system))
                          locative-args)
  (assert (endp locative-args))
  ;; FIXME: This is slow as hell.
  (or (asdf:find-system symbol nil)
      (locate-error)))

(defmethod canonical-reference ((system asdf:system))
  (make-reference (asdf::primary-system-name system) 'asdf:system))

(defmethod document-object ((system asdf:system) stream)
  (with-heading (stream system
                        (format nil "~A ASDF System Details"
                                (asdf::primary-system-name system)))
    (flet ((foo (name fn &key type)
             (let ((value (funcall fn system)))
               (when value
                 (case type
                   ((:link)
                    (format stream "- ~A: [~A](~A)~%" name value value))
                   ((:mailto)
                    (format stream "- ~A: [~A](mailto:~A)~%"
                            name value value))
                   ((:source-control)
                    (format stream "- ~A: [~A](~A)"
                            name (first value) (second value)))
                   ((nil)
                    (format stream "- ~A: ~A~%" name value)))))))
      (foo "Version" 'asdf/component:component-version)
      (foo "Description" 'asdf/system:system-description)
      (foo "Licence" 'asdf/system:system-licence)
      (foo "Author" 'asdf/system:system-author)
      (foo "Maintainer" 'asdf/system:system-maintainer)
      (foo "Mailto" 'asdf/system:system-mailto :type :mailto)
      (foo "Homepage" 'asdf/system:system-homepage :type :link)
      (foo "Bug tracker" 'asdf/system:system-bug-tracker :type :link)
      (foo "Source control" 'asdf/system:system-source-control
           :type :source-control)
      (terpri stream))))

(defmethod find-source ((system asdf:system))
  `(:location
    (:file ,(namestring (asdf/system:system-source-file system)))
    (:position 1)
    (:snippet "")))

(defvar end-of-asdf-example)


;;;; STRUCTURE-ACCESSOR locative

(define-locative-type structure-accessor ()
  "This is a synonym of FUNCTION with the difference that the often
  ugly and certainly uninformative lambda list will not be printed.")

(defmethod locate-object ((symbol symbol)
                          (locative-type (eql 'structure-accessor))
                          locative-args)
  ;; Signal an error if it doesn't exist.
  (or (ignore-errors (symbol-function symbol))
      (locate-error))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document ((symbol symbol)
                                (locative-type (eql 'structure-accessor))
                                locative-args stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (print-end-bullet stream)
  (with-dislocated-symbols ((list symbol))
    (maybe-print-docstring symbol 'function stream)))

(defmethod locate-and-find-source (symbol
                                   (locative-type (eql 'structure-accessor))
                                   locative-args)
  (declare (ignore locative-args))
  ;; Some implementations can not find the source location of the
  ;; accessor function, so fall back on FIND-ONE-LOCATION.
  (let ((location (find-source (symbol-function symbol))))
    (if (eq :error (first location))
        (find-one-location (swank-backend:find-definitions symbol)
                           '("function" "operator"))
        location)))


;;;; MACRO locative

(define-locative-type macro ())

(defmethod locate-object (symbol (locative-type (eql 'macro)) locative-args)
  (unless (macro-function symbol)
    (locate-error))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'macro))
                                locative-args stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (write-char #\Space stream)
  (let ((arglist (swank-backend:arglist symbol)))
    (print-arglist arglist stream)
    (print-end-bullet stream)
    (with-dislocated-symbols ((macro-arg-names arglist))
      (maybe-print-docstring symbol 'function stream))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'macro))
                                   locative-args)
  (declare (ignore locative-args))
  (find-source (macro-function symbol)))


;;;; COMPILER-MACRO locative

(define-locative-type compiler-macro ())

(defmethod locate-object (symbol (locative-type (eql 'compiler-macro))
                          locative-args)
  (unless (compiler-macro-function symbol)
    (locate-error))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'compiler-macro))
                                locative-args stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (write-char #\Space stream)
  (let ((arglist (swank-backend:arglist symbol)))
    (print-arglist arglist stream)
    (print-end-bullet stream)
    (with-dislocated-symbols ((macro-arg-names arglist))
      (maybe-print-docstring symbol 'function stream))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'compiler-macro))
                                   locative-args)
  (declare (ignore locative-args))
  #-allegro
  (find-source (compiler-macro-function symbol))
  #+allegro
  (find-one-location (swank-backend:find-definitions symbol)
                     '("compiler-macro")))


;;;; FUNCTION and GENERIC-FUNCTION locatives

(define-locative-type function ()
  "Note that the arglist in the generated documentation depends on
  the quality of SWANK-BACKEND:ARGLIST. It may be that default
  values of optional and keyword arguments are missing.")

(define-locative-type generic-function ())

(defmethod locate-object (symbol (locative-type (eql 'function)) locative-args)
  (declare (ignore locative-args))
  (when (macro-function symbol)
    (locate-error "~S is a macro, not a function." symbol))
  (let ((function (symbol-function symbol)))
    (when (typep function 'generic-function)
      (locate-error "~S is a generic function, not a plain function." symbol))
    function))

(defmethod locate-object (symbol (locative-type (eql 'generic-function))
                          locative-args)
  (declare (ignore locative-args))
  (let ((function (symbol-function symbol)))
    (unless (typep function 'generic-function)
      (locate-error "#'~S is not a generic function." symbol))
    function))

(defmethod canonical-reference ((function function))
  (make-reference (swank-backend:function-name function) 'function))

(defmethod canonical-reference ((function generic-function))
  (make-reference (swank-mop:generic-function-name function) 'generic-function))

(defmethod document-object ((function function) stream)
  (let ((reference (canonical-reference function)))
    (print-bullet reference stream)
    (write-char #\Space stream)
    (let ((arglist (swank-backend:arglist function)))
      (print-arglist arglist stream)
      (print-end-bullet stream)
      (with-dislocated-symbols ((function-arg-names arglist))
        (maybe-print-docstring (reference-object reference) 'function
                               stream)))))


;;;; METHOD locative

(define-locative-type method (method-qualifiers method-specializers)
  "See CL:FIND-METHOD for the description of the arguments.
  To refer to the default method of the three argument generic
  function FOO:

      (foo (method () (t t t)))")

(defmethod locate-object (symbol (locative-type (eql 'method))
                          locative-args)
  (assert (= 2 (length locative-args)))
  (destructuring-bind (qualifiers specializers) locative-args
    (or (ignore-errors
         (find-method (symbol-function symbol) qualifiers
                      (loop for specializer in specializers
                            collect (typecase specializer
                                      ;; SPECIALIZER can be a cons
                                      ;; like (:EQL :SOME-VALUE) ...
                                      (cons specializer)
                                      ;; or a type specifier denoting
                                      ;; a class:
                                      (t (find-class specializer))))))
        (locate-error))))

(defmethod canonical-reference ((method method))
  (make-reference (swank-mop:generic-function-name
                   (swank-mop:method-generic-function method))
                  `(method ,(swank-mop:method-qualifiers method)
                           ,(method-specializers-list method))))

;;; Return the specializers in a format suitable as the second
;;; argument to FIND-METHOD.
(defun method-specializers-list (method)
  (mapcar (lambda (spec)
            (typecase spec
              (swank-mop:eql-specializer
               `(eql ,(swank-mop:eql-specializer-object spec)))
              (t (swank-mop:class-name spec))))
          (swank-mop:method-specializers method)))

(defmethod document-object ((method method) stream)
  (let ((arglist (rest (method-for-inspect-value method))))
    (print-bullet method stream)
    (write-char #\Space stream)
    (print-arglist arglist stream)
    (print-end-bullet stream)
    (with-dislocated-symbols ((function-arg-names arglist))
      (maybe-print-docstring method t stream))))

;;;; These were lifted from the fancy inspector contrib and then
;;;; tweaked.

(defun method-specializers-for-inspect (method)
  """Return a "pretty" list of the method's specializers. Normal
  specializers are replaced by the name of the class, eql specializers
  are replaced by `(eql ,object)."""
  (mapcar (lambda (name spec)
            (let ((name (if (listp name) (first name) name)))
              (if (eq spec t)
                  name
                  (list name spec))))
          (swank-mop:method-lambda-list method)
          (method-specializers-list method)))

(defun method-for-inspect-value (method)
  """Returns a "pretty" list describing METHOD. The first element of
  the list is the name of generic-function method is specialized on,
  the second element is the method qualifiers, the rest of the list is
  the method's specialiazers (as per
  METHOD-SPECIALIZERS-FOR-INSPECT)."""
  (append (list (swank-mop:generic-function-name
                 (swank-mop:method-generic-function method)))
          (swank-mop:method-qualifiers method)
          (method-specializers-for-inspect method)))


;;;; ACCESSOR, READER and WRITER locatives

(define-locative-type accessor (class-name)
  "To refer to an accessor named `FOO-SLOT` of class
  `FOO`:

      (foo-slot (accessor foo))")

(define-locative-type reader (class-name)
  "To refer to a reader named `FOO-SLOT` of class
  `FOO`:

      (foo-slot (reader foo))")

(define-locative-type writer (class-name)
  "To refer to a writer named `FOO-SLOT` of class
  `FOO`:

      (foo-slot (writer foo))")

(defmethod locate-object (symbol (locative-type (eql 'accessor))
                          locative-args)
  (assert (= 1 (length locative-args)) ()
          "The syntax of the ACCESSOR locative is (ACCESSOR <CLASS-NAME>).")
  (find-accessor-slot-definition symbol (first locative-args))
  (make-reference symbol (cons locative-type locative-args)))

(defun find-accessor-slot-definition (accessor-symbol class-symbol)
  (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
    (when (and (find accessor-symbol
                     (swank-mop:slot-definition-readers slot-def))
               (find `(setf ,accessor-symbol)
                     (swank-mop:slot-definition-writers slot-def)
                     :test #'equal))
      (return-from find-accessor-slot-definition slot-def)))
  (locate-error "Could not find accessor ~S for class ~S." accessor-symbol
                class-symbol))

(defmethod locate-object (symbol (locative-type (eql 'reader))
                          locative-args)
  (assert (= 1 (length locative-args)) ()
          "The syntax of the READER locative is (READER <CLASS-NAME>).")
  (find-reader-slot-definition symbol (first locative-args))
  (make-reference symbol (cons locative-type locative-args)))

(defun find-reader-slot-definition (accessor-symbol class-symbol)
  (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
    (when (find accessor-symbol (swank-mop:slot-definition-readers slot-def))
      (return-from find-reader-slot-definition slot-def)))
  (locate-error "Could not find reader ~S for class ~S." accessor-symbol
                class-symbol))

(defmethod locate-object (symbol (locative-type (eql 'writer))
                          locative-args)
  (assert (= 1 (length locative-args)) ()
          "The syntax of the WRITER locative is (WRITER <CLASS-NAME>).")
  (find-writer-slot-definition symbol (first locative-args))
  (make-reference symbol (cons locative-type locative-args)))

(defun find-writer-slot-definition (accessor-symbol class-symbol)
  (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
    (when (find accessor-symbol (swank-mop:slot-definition-writers slot-def))
      (return-from find-writer-slot-definition slot-def)))
  (locate-error "Could not find writer ~S for class ~S." accessor-symbol
                class-symbol))

(defmethod locate-and-document (symbol (locative-type (eql 'accessor))
                                locative-args stream)
  (generate-documentation-for-slot-definition
   symbol (find-accessor-slot-definition symbol (first locative-args))
   locative-type locative-args stream))

(defmethod locate-and-document (symbol (locative-type (eql 'reader))
                                locative-args stream)
  (generate-documentation-for-slot-definition
   symbol (find-reader-slot-definition symbol (first locative-args))
   locative-type locative-args stream))

(defmethod locate-and-document (symbol (locative-type (eql 'writer))
                                locative-args stream)
  (generate-documentation-for-slot-definition
   symbol (find-writer-slot-definition symbol (first locative-args))
   locative-type locative-args stream))

(defun generate-documentation-for-slot-definition
    (symbol slot-def locative-type locative-args stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (write-char #\Space stream)
  (print-arglist locative-args stream)
  (when (or (swank-mop:slot-definition-initargs slot-def)
            (swank-mop:slot-definition-initfunction slot-def))
    (write-char #\Space stream)
    (if (and *document-mark-up-signatures* (eq *format* :html))
        (let ((initarg-strings
                (when (swank-mop:slot-definition-initargs slot-def)
                  (mapcar #'prin1-and-escape-markdown
                          (swank-mop:slot-definition-initargs slot-def)))))
          (print-arglist
           (format nil "(~{~A~^ ~}~A)" initarg-strings
                   (if (swank-mop:slot-definition-initfunction slot-def)
                       (format nil "~A= ~A"
                               (if initarg-strings " " "")
                               (replace-known-references
                                (prin1-and-escape-markdown
                                 (swank-mop:slot-definition-initform
                                  slot-def))))
                       ""))
           stream))
        (print-arglist
         (prin1-to-string
          `(,@(when (swank-mop:slot-definition-initargs slot-def)
                (swank-mop:slot-definition-initargs slot-def))
            ,@(when (swank-mop:slot-definition-initfunction slot-def)
                `(=
                  ,(swank-mop:slot-definition-initform slot-def)))))
         stream)))
  (print-end-bullet stream)
  ;; No documentation for condition accessors, and some
  ;; implementations signal warnings.
  (with-dislocated-symbols ((list symbol))
    (unless (subtypep (find-class (first locative-args)) 'condition)
      (let ((docstring (swank-mop:slot-definition-documentation slot-def)))
        (when docstring
          (format stream "~%~A~%" (massage-docstring docstring)))))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'accessor))
                                   locative-args)
  (find-source (find-method (symbol-function symbol)
                            '() (list (find-class (first locative-args))))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'reader))
                                   locative-args)
  (find-source (find-method (symbol-function symbol)
                            '() (list (find-class (first locative-args))))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'writer))
                                   locative-args)
  (find-source (find-method (symbol-function symbol)
                            '() (mapcar #'find-class
                                        (list t (first locative-args))))))


;;;; TYPE locative

(define-locative-type type ()
  "TYPE can refer to classes as well, but it's better style to use the
  more specific CLASS locative type for that. Another difference to
  CLASS is that an attempt is made at printing the arguments of type
  specifiers.")

(defmethod locate-object (symbol (locative-type (eql 'type)) locative-args)
  (unless (swank-backend:type-specifier-p 'symbol)
    (locate-error))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'type)) locative-args
                                stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (let ((arglist (swank-backend:type-specifier-arglist symbol)))
    (when (and arglist (not (eq arglist :not-available)))
      (write-char #\Space stream)
      (print-arglist arglist stream)))
  (print-end-bullet stream)
  (with-dislocated-symbols ((list symbol))
    (maybe-print-docstring symbol 'type stream)))

(defmethod locate-and-find-source (symbol (locative-type (eql 'type))
                                   locative-args)
  (declare (ignore locative-args))
  (find-one-location (swank-backend:find-definitions symbol)
                     '("type" "class")))


;;;; CLASS and CONDITION locatives

(define-locative-type class ())

(define-locative-type condition ())

(defmethod locate-object (symbol (locative-type (eql 'class)) locative-args)
  (declare (ignore locative-args))
  (or (find-class symbol :errorp nil)
      (locate-error)))

(defmethod locate-object (symbol (locative-type (eql 'condition))
                          locative-args)
  (assert (= 0 (length locative-args)))
  (let ((class (find-class symbol :errorp nil)))
    (unless (subtypep class 'condition)
      (locate-error))
    class))

(defmethod canonical-reference ((class class))
  (if (subtypep class 'condition)
      (make-reference (class-name class) 'condition)
      (make-reference (class-name class) 'class)))

(defmethod document-object ((class class) stream)
  (let* ((conditionp (subtypep class 'condition))
         (symbol (class-name class))
         (superclasses
           (remove-if (lambda (name)
                        (or (eq name 'standard-object)
                            (and conditionp (eq name 'condition))))
                      (mapcar #'class-name
                              (swank-mop:class-direct-superclasses class)))))
    (print-bullet class stream)
    (when superclasses
      (write-char #\Space stream)
      (if *document-mark-up-signatures*
          (print-arglist (mark-up-superclasses superclasses) stream)
          (print-arglist superclasses stream)))
    (print-end-bullet stream)
    (with-dislocated-symbols ((list symbol))
      (maybe-print-docstring class t stream))))

(defun mark-up-superclasses (superclasses)
  (with-output-to-string (stream)
    (loop for class in superclasses
          for i upfrom 0
          do (let ((reference (make-reference class 'class)))
               (let ((name (escape-markdown (prin1-to-string class))))
                 (unless (zerop i)
                   (format stream " "))
                 (if (find-known-reference reference)
                     (format stream "[~A][~A]" name
                             (link-to-reference reference))
                     (format stream "~A" name)))))))

(defun find-known-reference (reference)
  (find reference *references* :test #'reference=))


;;;; RESTART-NAME locative

(define-symbol-locative-type restart ())

(define-definer-for-symbol-locative-type define-restart restart
  """A definer macro to hang the documentation of a restart on a
  symbol.

  ```
  (define-restart my-ignore-error ()
    "Available when MY-ERROR is signalled, MY-IGNORE-ERROR unsafely continues.")
  ```

  Note that while there is a CL:RESTART class, there is no
  corresponding source location or docstring like for CONDITIONs.
  """)


;;;; PACKAGE locative

(define-locative-type package ())

(defmethod locate-object (symbol (locative-type (eql 'package))
                          locative-args)
  (assert (= 0 (length locative-args)))
  (or (find-package symbol) (locate-error)))

(defmethod canonical-reference ((package package))
  (make-reference (package-name package) 'package))

(defmethod document-object ((package package) stream)
  (let ((symbol (package-name package)))
    (print-bullet package stream)
    (print-end-bullet stream)
    (with-dislocated-symbols ((list symbol))
      (maybe-print-docstring package t stream))))


;;;; DISLOCATED locative

(define-locative-type dislocated ()
  "Refers to a symbol in a non-specific context. Useful for preventing
  autolinking. For example, if there is a function called `FOO` then

      `FOO`

  will be linked to (if *DOCUMENT-LINK-CODE*) its definition. However,

      [`FOO`][dislocated]

  will not be. On a dislocated locative LOCATE always fails with a
  LOCATE-ERROR condition.")

(defmethod locate-object (symbol (locative-type (eql 'dislocated))
                          locative-args)
  (declare (ignore symbol locative-args))
  (locate-error))


;;;; ARGUMENT locative

(define-locative-type argument ()
  """An alias for DISLOCATED, so the one can refer to an argument of a
  macro without accidentally linking to a class that has the same name
  as that argument. In the following example, FORMAT may link to
  CL:FORMAT (if we generated documentation for it):

  ```
  "See the FORMAT in DOCUMENT."
  ```

  Since ARGUMENT is a locative, we can prevent that linking by writing:

  ```
  "See the FORMAT argument of DOCUMENT."
  ```""")

(defmethod locate-object (symbol (locative-type (eql 'argument)) locative-args)
  (declare (ignore symbol locative-args))
  (locate-error))


;;;; GLOSSARY-TERM locative

(defclass glossary-term ()
  ((name
    :initarg :name :reader glossary-term-name
    :documentation "The name of the global variable whose value is
    this GLOSSARY-TERM object.")
   (title
    :initarg :title :reader glossary-term-title
    :documentation "Used in generated documentation.")
   (docstring
    :initarg :docstring :reader glossary-term-docstring)))

(defmacro define-glossary-term
    (name (&key title (discard-documentation-p *discard-documentation-p*))
     docstring)
  "Define a global variable with NAME and set it to a glossary term
  object. A glossary term is just a symbol to hang a docstring on. It
  is a bit like a SECTION in that, when linked to, its TITLE will be
  the link text instead of the name of the symbol. Unlike sections
  though, glossary terms are not rendered with headings, but in the
  more lightweight bullet + locative + name/title style.

  When DISCARD-DOCUMENTATION-P (defaults to *DISCARD-DOCUMENTATION-P*)
  is true, DOCSTRING will not be recorded to save memory."
  `(defparameter ,name
     (make-instance 'glossary-term
                    :name ',name :title ,title
                    :docstring ,(unless discard-documentation-p
                                  docstring))))

(defun glossary-term-title-or-name (glossary-term)
  (or (glossary-term-title glossary-term)
      (maybe-downcase (prin1-to-string (glossary-term-name glossary-term)))))

(defmethod print-object ((glossary-term glossary-term) stream)
  (print-unreadable-object (glossary-term stream :type t)
    (format stream "~a" (glossary-term-name glossary-term))))

(define-locative-type glossary-term ()
  "Refers to a glossary term defined by DEFINE-GLOSSARY-TERM.")

(defmethod locate-object (symbol (locative-type (eql 'glossary-term))
                          locative-args)
  (declare (ignore locative-args))
  (assert (typep (symbol-value symbol) 'glossary-term))
  (symbol-value symbol))

(defmethod document-object ((glossary-term glossary-term) stream)
  (let ((symbol (glossary-term-name glossary-term)))
    (locate-and-print-bullet 'glossary-term () symbol stream
                             :name (glossary-term-title-or-name glossary-term))
    (print-end-bullet stream)
    (with-dislocated-symbols ((list symbol))
      (let ((docstring (glossary-term-docstring glossary-term)))
        (when docstring
          (format stream "~%~A~%" (massage-docstring docstring)))))))

(defmethod canonical-reference ((glossary-term glossary-term))
  (make-reference (glossary-term-name glossary-term) 'glossary-term))

(defmethod find-source ((glossary-term glossary-term))
  (locate-and-find-source (glossary-term-name glossary-term) 'variable ()))


;;;; INCLUDE locative

(define-locative-type include (source &key line-prefix header footer
                                      header-nl footer-nl)
  """Refers to a region of a file. SOURCE can be a string or a
  pathname in which case the whole file is being pointed to or it can
  explicitly supply START, END locatives. INCLUDE is typically used to
  include non-lisp files in the documentation (say markdown or elisp
  as in the next example) or regions of lisp source files. This can
  reduce clutter and duplication.

  ```commonlisp
  (defsection example-section ()
    (pax.el (include #.(asdf:system-relative-pathname :mgl-pax "src/pax.el")
                     :header-nl "```elisp" :footer-nl "```"))
    (foo-example (include (:start (foo function)
                           :end (end-of-foo-example variable))
                          :header-nl "```commonlisp"
                          :footer-nl "```"))

  (defun foo (x)
    (1+ x))

  ;;; Since file regions are copied verbatim, comments survive.
  (defmacro bar ())

  ;;; This comment is the last thing in FOO-EXAMPLE's
  ;;; documentation since we use the dummy END-OF-FOO-EXAMPLE
  ;;; variable to mark the end location.
  (defvar end-of-foo-example)

  ;;; More irrelevant code follows.
  ```

  In the above example, pressing `M-.` on PAX.EL will open the
  `src/pax.el` file and put the cursor on its first character. `M-.`
  on `FOO-EXAMPLE` will go to the source location of the `(asdf:system
  locative)` locative.

  When documentation is generated, the entire `pax.el` file is
  included in the markdown surrounded by the strings given as
  HEADER-NL and FOOTER-NL (if any). The trailing newline character is
  assumed implicitly. If that's undesirable, then use HEADER and
  FOOTER instead. The documentation of `FOO-EXAMPLE` will be the
  region of the file from the source location of the START
  locative (inclusive) to the source location of the END
  locative (exclusive). START and END default to the beginning and end
  of the file, respectively.

  Note that the file of the source location of :START and :END must be
  the same. If SOURCE is pathname designator, then it must be absolute
  so that the locative is context independent.

  Finally, if specified LINE-PREFIX is a string that's prepended to
  each line included in the documentation. For example, a string of
  four spaces makes markdown think it's a code block.""")

(defmethod exportable-locative-type-p ((locative-type (eql 'include)))
  nil)

(defmethod locate-object (symbol (locative-type (eql 'include))
                          locative-args)
  (destructuring-bind (source &key line-prefix header footer
                       header-nl footer-nl) locative-args
    (declare (ignore source line-prefix header footer header-nl footer-nl))
    (make-reference symbol (cons locative-type locative-args))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'include))
                                   locative-args)
  (multiple-value-bind (file start) (include-region (first locative-args))
    (assert file)
    `(:location
      (:file ,(namestring file))
      (:position ,(1+ start))
      nil)))

(defmethod locate-and-document (symbol (locative-type (eql 'include))
                                locative-args stream)
  (destructuring-bind (source &key (line-prefix "") header footer
                       header-nl footer-nl) locative-args
    (when header
      (format stream "~A" header))
    (when header-nl
      (format stream "~A~%" header-nl))
    (format stream "~A"
            (prefix-lines line-prefix
                          (multiple-value-call #'file-subseq
                            (include-region source))))
    (when footer
      (format stream "~A" footer))
    (when footer-nl
      (format stream "~A~%" footer-nl))))

;;; Return the filename and start, end positions of the region to be
;;; included.
(defun include-region (source)
  (cond ((or (stringp source) (pathnamep source))
         (assert (uiop/pathname:absolute-pathname-p source) ()
                 "Pathnames given as the SOURCE argument of the ~
                 INCLUDE locative must be absolute, but ~S is not."
                 source)
         (values source 0 nil))
        ((and source (listp source))
         (destructuring-bind (&key start end) source
           (let ((start (find-source (resolve (entry-to-reference start))))
                 (end (find-source (resolve (entry-to-reference end)))))
             (when start
               (check-location start))
             (when end
               (check-location end))
             (let ((start-file (when start (location-file start)))
                   (start-position (when start (location-position start)))
                   (end-file (when end (location-file end)))
                   (end-position (when end (location-position end))))
               (when (and start end)
                 (assert (string= (namestring (truename start-file))
                                  (namestring (truename end-file)))
                         () "Include starts in file ~S and ends in ~
                         another file ~S." start-file end-file))
               (values (or start-file end-file) start-position end-position)))))
        (t
         (error "~@<Malformed include source ~S.~:@>" source))))

;;; Check that LOCATION looks like this:
;;;
;;;     (:location
;;;      (:file "filename")
;;;      (:position 1)
;;;      (:snippet ""))
(defun check-location (location)
  (assert (listp location) () "Location ~S is not a list." location)
  (assert (eq (first location) :location) ()
          "Location ~S does not start with ~S." location :location)
  (assert (and (location-file location)
               (location-position location))
          () "Location ~S should contain: ~S."
          location '(:file :position)))

(defun location-file (location)
  (second (find :file (rest location) :key #'first)))

(defun location-position (location)
  (1- (second (find :position (rest location) :key #'first))))

(defun file-subseq (pathname &optional start end)
  (with-open-file (stream pathname)
    (let ((*print-pretty* nil)
          (start (or start 0))
          (end (or end (file-length stream)))
          (buffer-size 4096))
      (file-position stream start)
      (with-output-to-string (datum)
        (let ((buffer (make-array buffer-size :element-type 'character)))
          (loop
            for bytes-read = (read-sequence
                              buffer stream
                              :end (min buffer-size
                                        (- end (file-position stream))))
            do (write-sequence buffer datum :start 0 :end bytes-read)
            while (= bytes-read buffer-size)))))))
