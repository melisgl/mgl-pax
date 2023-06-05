(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @pax-manual (:title "PAX Manual")
  (@introduction section)
  (@emacs-setup section)
  (@links section)
  (@background section)
  (@basics section)
  (@locative-types section)
  (@navigating-in-emacs section)
  (@generating-documentation section)
  (@transcripts section)
  (@extension-api section))

(defsection @introduction (:title "Introduction")
  """_What if documentation really lived in the code?_

  Docstrings are already there. If some narrative glued them together,
  we'd be able develop and explore the code along with the
  documentation due to their physical proximity. The main tool that
  PAX provides for this is DEFSECTION:

  ```
  (defsection @foo-random-manual (:title "Foo Random manual")
    "Foo Random is a random number generator library."
    (foo-random-state class)
    (uniform-random function)
    (@foo-random-examples section))
  ```

  Like this one, sections can have docstrings and references to
  definitions (e.g. `(UNIFORM-RANDOM FUNCTION)`). These docstrings and
  references are the glue. To support interactive development, PAX

  - makes [SLIME's `\\M-.`][slime-m-.] work with references and

  - adds a documentation browser.

  See @EMACS-SETUP.

  [slime]: https://slime.common-lisp.dev/
  [slime-m-.]: http://common-lisp.net/project/slime/doc/html/Finding-definitions.html#Finding-definitions

  Beyond interactive workflows, @GENERATING-DOCUMENTATION from
  sections and all the referenced items in Markdown or HTML format is
  also implemented.

  With the simplistic tools provided, one may emphasize the narrative
  as with Literate Programming, but documentation is generated from
  code, not vice versa, and there is no support for chunking.

  _Code is first, code must look pretty, documentation is code_.

  ##### Docstrings

  PAX automatically recognizes and [marks up code][@codification] with
  backticks and [links code][@linking-to-code] to their definitions.
  The following @TRANSCRIPTS show the lines of the output (prefixed
  with ` ..`) generated:

  ```
  (document "&KEY arguments such as :IF-EXISTS are common." :format :markdown)
  .. `&KEY` arguments such as `:IF-EXISTS` are common.

  (document "AND denotes a macro and a type specifier.
  Here we focus on the macro AND." :format :markdown)
  .. `AND`([`0`][4954] [`1`][330f]) denotes a macro and a type specifier.
  .. Here we focus on the macro [`AND`][4954].
  ..
  ..   [330f]: http://www.lispworks.com/documentation/HyperSpec/Body/t_and.htm "AND TYPE"
  ..   [4954]: http://www.lispworks.com/documentation/HyperSpec/Body/m_and.htm "AND MGL-PAX:MACRO"
  ```

  These features are designed to handle the most common style of
  docstrings with minimal additional markup. The following is the
  output of `(mgl-pax:document #'abort :format :markdown)`.

      - \[function\] **\ABORT** *\&OPTIONAL \CONDITION*

          Transfer control to a restart named `ABORT`, signalling a
          [`\CONTROL-ERROR`][6bc0] if none exists.

        [6bc0]: http://www.lispworks.com/documentation/HyperSpec/Body/e_contro.htm "\CONTROL-ERROR \CONDITION"

  Note that the docstring of the ABORT function was not written with
  PAX in mind. The above markdown is rendered as

  - \[function\] **\ABORT** *\&OPTIONAL \CONDITION*

      Transfer control to a restart named `ABORT`, signalling a
      [`\CONTROL-ERROR`][6bc0] if none exists.

    [6bc0]: http://www.lispworks.com/documentation/HyperSpec/Body/e_contro.htm "\CONTROL-ERROR \CONDITION"

  ##### A Complete Example

  Here is an example of how it all works together:"""
  (foo-random-example (include #.(asdf:system-relative-pathname
                                  :mgl-pax "src/base/foo-random-example.lisp")
                               :header-nl "```" :footer-nl "```"))
  """Note how `(VARIABLE *FOO-STATE*)` in the DEFSECTION form both
  exports `*FOO-STATE*` and includes its documentation in
  `@FOO-RANDOM-MANUAL`. The symbols [VARIABLE][locative] and
  [FUNCTION][locative] are just two instances of @LOCATIVEs, which are
  used in DEFSECTION to refer to definitions tied to symbols.

  `(DOCUMENT @FOO-RANDOM-MANUAL)` generates fancy markdown or HTML
  output with [automatic markup][\*document-uppercase-is-code\*
  variable] and [autolinks][@linking-to-code section] uppercase @WORDs
  found in docstrings, numbers sections, and creates a table of
  contents.

  One can even generate documentation for different but related
  libraries at the same time with the output going to different files
  but with cross-page links being automatically added for symbols
  mentioned in docstrings. In fact, this is what @PAX-WORLD does. See
  @GENERATING-DOCUMENTATION for some convenience functions to cover
  the most common cases.

  The [transcript][@transcripts] in the code block tagged with
  `cl-transcript` is automatically checked for up-to-dateness when
  documentation is generated.""")

(defsection @emacs-setup (:title "Emacs Setup")
  """Load `src/mgl-pax.el` in Emacs, and maybe set up some key bindings.

  If you installed PAX with Quicklisp, the location of `mgl-pax.el`
  may change with updates, and you may want to copy the current
  version of `mgl-pax.el` to a stable location:

      (mgl-pax:install-pax-elisp "~/quicklisp/")

  Then, assuming the Elisp file is in the quicklisp directory, add
  something like this to your `.emacs`:
  
  ```elisp
  (load "~/quicklisp/mgl-pax.el")
  (mgl-pax-hijack-slime-doc-keys)
  (global-set-key (kbd "C-.") 'mgl-pax-document)
  (global-set-key (kbd "s-x t") 'mgl-pax-transcribe-last-expression)
  (global-set-key (kbd "s-x r") 'mgl-pax-retranscribe-region)
  ```

  For @BROWSING-LIVE-DOCUMENTATION, `mgl-pax-browser-function` can be
  customized in Elisp. To browse within Emacs, choose
  `w3m-browse-url` (see
  [w3m](https://emacs-w3m.github.io/info/emacs-w3m.html)), and make
  sure both the w3m binary and the w3m Emacs package are installed. On
  Debian, simply install the `w3m-el` package. With other browser
  functions, a [HUNCHENTOOT][package] web server is started.

  See @NAVIGATING-IN-EMACS, @GENERATING-DOCUMENTATION and
  @TRANSCRIBING-WITH-EMACS for how to use the relevant features.
  """
  (install-pax-elisp function))

(defun install-pax-elisp (target-dir)
  "Copy `mgl-pax.el` distributed with this package to TARGET-DIR."
  (uiop:copy-file (asdf:system-relative-pathname "mgl-pax" "src/mgl-pax.el")
                  (merge-pathnames "mgl-pax.el"
                                   (uiop:ensure-directory-pathname
                                    target-dir))))

(defun check-pax-elisp-version (version)
  (let ((min-required-version '(0 2 3)))
    (unless (version<= min-required-version version)
      (cerror "Ignore version mismatch."
              "~@<In Emacs, mgl-pax-version is ~S, ~
              which is lower than the required ~S. ~
              You may need to M-x mgl-pax-reload.~:@>"
              version min-required-version '@emacs-setup)))
  t)

(defun version<= (version1 version2)
  (loop for x1 in version1
        for x2 in version2
        do (when (< x1 x2)
             (return t))
           (when (< x2 x1)
             (return nil))
        finally (return t)))

(defsection @links (:title "Links and Systems")
  "Here is the [official
  repository](https://github.com/melisgl/mgl-pax) and the [HTML
  documentation](http://melisgl.github.io/mgl-pax-world/mgl-pax-manual.html)
  for the latest version."
  ("mgl-pax" asdf:system)
  (mgl-pax/full asdf:system))

(defsection @background (:title "Background")
  """As a user, I frequently run into documentation that's incomplete
  and out of date, so I tend to stay in the editor and explore the
  code by jumping around with [SLIME][slime]'s
  [`\\M-.`][slime-m-.] (`slime-edit-definition`). As a library author,
  I spend a great deal of time polishing code but precious little
  writing documentation.

  [slime]: https://slime.common-lisp.dev/
  [slime-m-.]: http://common-lisp.net/project/slime/doc/html/Finding-definitions.html#Finding-definitions

  In fact, I rarely write anything more comprehensive than docstrings
  for exported stuff. Writing docstrings feels easier than writing a
  separate user manual, and they are always close at hand during
  development. The drawback of this style is that users of the library
  have to piece the big picture together themselves.

  That's easy to solve, I thought, let's just put all the narrative
  that holds docstrings together in the code and be a bit like a
  Literate Programmer turned inside out. The original prototype, which
  did almost everything I wanted, was this:

  ```
  (defmacro defsection (name docstring)
    `(defun ,name () ,docstring))
  ```

  Armed with this [DEFSECTION][dislocated], I soon found myself
  organizing code following the flow of user-level documentation and
  relegated comments to implementation details entirely. However, some
  parts of [`DEFSECTION`][dislocated] docstrings were just listings of
  all the functions, macros and variables related to the narrative,
  and this list was repeated in the DEFPACKAGE form complete with
  little comments that were like section names. A clear violation of
  [OAOO][oaoo], one of them had to go, so [DEFSECTION][dislocated] got
  a list of symbols to export.

  [oaoo]: http://c2.com/cgi/wiki?OnceAndOnlyOnce

  That was great, but soon I found that the listing of symbols is
  ambiguous if, for example, a function, a compiler macro and a class
  were named by the same symbol. This did not concern exporting, of
  course, but it didn't help readability. Distractingly, on such
  symbols, `\\M-.` was popping up selection dialogs. There were two
  birds to kill, and the symbol got accompanied by a type, which was
  later generalized into the concept of locatives:

  ```
  (defsection @introduction ()
    "A single line for one man ..."
    (foo class)
    (bar function))
  ```

  After a bit of elisp hacking, `\\M-.` was smart enough to
  disambiguate based on the locative found in the vicinity of the
  symbol, and everything was good for a while.

  Then, I realized that sections could refer to other sections if
  there were a SECTION locative. Going down that path, I soon began to
  feel the urge to generate pretty documentation as all the necessary
  information was available in the DEFSECTION forms. The design
  constraint imposed on documentation generation was that following
  the typical style of upcasing symbols in docstrings, there should be
  no need to explicitly mark up links: if `\\M-.` works, then the
  documentation generator shall also be able figure out what's being
  referred to.

  I settled on [Markdown][markdown] as a reasonably non-intrusive
  format, and a few thousand lines later PAX was born.

  [markdown]: https://daringfireball.net/projects/markdown/""")


(defsection @basics (:title "Basics")
  "Now let's examine the most important pieces."
  (defsection macro)
  (*discard-documentation-p* variable)
  (define-package macro)
  (@locatives-and-references section)
  (@parsing section))

(defmacro define-package (package &rest options)
  "This is like CL:DEFPACKAGE but silences warnings and errors
  signalled when the redefined package is at variance with the current
  state of the package. Typically this situation occurs when symbols
  are exported by calling EXPORT (as is the case with DEFSECTION) as
  opposed to adding :EXPORT forms to the DEFPACKAGE form and the
  package definition is subsequently reevaluated. See the section on
  [package variance](http://www.sbcl.org/manual/#Package-Variance) in
  the SBCL manual.

  The bottom line is that if you rely on DEFSECTION to do the
  exporting, then you'd better use DEFINE-PACKAGE."
  `(eval-when (:compile-toplevel :load-toplevel, :execute)
     (locally
         (declare #+sbcl
                  (sb-ext:muffle-conditions sb-kernel::package-at-variance))
       (handler-bind
           (#+sbcl (sb-kernel::package-at-variance #'muffle-warning))
         (cl:defpackage ,package ,@options)))))


(defsection @locatives-and-references (:title "Locatives and References")
  """To [navigate with `\\M-.`][@NAVIGATING-IN-EMACS] and to [generate
  documentation][@GENERATING-DOCUMENTATION] we need to refer to things
  such as the `FOO` type or the `FOO` function.

  ```
  (deftype foo ()
    "type doc"
    '(or integer real).

  (defun foo ()
    "function doc"
    7)
  ```

  The docstring is available via `(CL:DOCUMENTATION 'FOO 'TYPE)`,
  where `\TYPE` – called `DOC-TYPE` – is what tells CL:DOCUMENTATION
  that we want the docstring of the type named `FOO`. This design
  supports disambiguation and working with things that are not
  first-class, such as types.

  PAX generalizes `DOC-TYPE` to the concept of @LOCATIVEs, which may
  also take arguments. An @OBJECT and a @LOCATIVE together are called
  a @REFERENCE, and they identify a definition. REFERENCEs are actual
  objects, but often they appear as an `(OBJECT LOCATIVE)` list (see
  DEFSECTION) or as `"OBJECT LOCATIVE"` in docstrings (see
  @LINKING-TO-CODE for the various forms possible).

  ```
  (defsection @foos ()
    "We discuss the FOO type and the FOO function."
    (foo type)
    (foo function))
  ```"""
  (@reference glossary-term)
  (@object glossary-term)
  (@locative glossary-term)
  (@locatives-and-references-api section))

(define-glossary-term @reference (:title "reference")
  """A @REFERENCE is an @OBJECT plus a @LOCATIVE, and it identifies a
  definition. For example, the symbol `FOO` as the object and the
  symbol [`FUNCTION`][locative] as the locative together refer to the
  global definition of the function `FOO`.

  REFERENCE objects can be designated by an `(OBJECT LOCATIVE)` list
  as in DEFSECTION entries, or textually as `"FOO function"` where
  `FOO` is a @NAME or similar (see @CODIFICATION and
  @LINKING-TO-CODE).""")

(define-glossary-term @object (:title "object")
  "@OBJECTs are symbols or strings which name [functions][function
  locative], [types][type locative], [packages][package locative],
  etc. Together with @LOCATIVEs, they form @REFERENCEs.")

(define-glossary-term @locative (:title "locative")
  "@LOCATIVEs specify a _type_ of definition such as
  [FUNCTION][locative] or [VARIABLE][locative] and together with
  @OBJECTs form @REFERENCEs.

  A locative can be a symbol or a list whose CAR is a symbol. In
  either case, the symbol is called the [locative type][
  @locative-types section] while the rest of the elements are the
  _locative arguments_. See the METHOD locative or the LOCATIVE
  locative for examples of locative types with arguments.")


(defsection @locatives-and-references-api
    (:title "Locatives and References API")
  "`(MAKE-REFERENCE 'FOO 'VARIABLE)` constructs a REFERENCE that
  captures the path to take from an @OBJECT (the symbol `FOO`) to an
  entity of interest (for example, the documentation of the variable).
  The path is called the @LOCATIVE. A locative can be applied to an
  object like this:

  ```
  (locate 'foo 'variable)
  ```

  which will return the same reference as `(MAKE-REFERENCE 'FOO
  'VARIABLE)`. Operations need to know how to deal with references,
  which we will see in the @EXTENSION-API.

  Naturally, `(LOCATE 'FOO 'FUNCTION)` will simply return `#'FOO`, no
  need to muck with references when there is a perfectly good object."
  (reference class)
  (reference-object (reader reference))
  (reference-locative (reader reference))
  (make-reference function)
  (locative-type function)
  (locative-args function)
  (locate function)
  (locate-error condition)
  (locate-error-message (reader locate-error))
  (locate-error-object (reader locate-error))
  (locate-error-locative (reader locate-error))
  (resolve function))

;;; This gets clobbered with an empty function when MGL-PAX/NAVIGATE
;;; is loaded.
(autoload ensure-navigate-loaded '#:mgl-pax/navigate)

(declaim (ftype function locate-object))

(define-condition locate-error (error)
  ((message :initarg :message :reader locate-error-message)
   (object :initarg :object :reader locate-error-object)
   (locative :initarg :locative :reader locate-error-locative))
  (:documentation "Signalled by LOCATE when the lookup fails and
  ERRORP is true.")
  (:report (lambda (condition stream)
             (format stream "~@<Could not locate ~A ~A.~@[ ~A~]~:@>"
                     (locate-error-object condition)
                     (locate-error-locative condition)
                     (locate-error-message condition)))))

(defun locate (object locative &key (errorp t))
  "Follow LOCATIVE from OBJECT and return the object it leads to or a
  REFERENCE if there is no first-class object corresponding to the
  location. Depending on ERRORP, a LOCATE-ERROR condition is signalled
  or NIL is returned if the lookup fails.

  ```
  (locate 'locate 'function)
  ==> #<FUNCTION LOCATE>

  (locate 'no-such-function 'function)
  .. debugger invoked on LOCATE-ERROR:
  ..   Could not locate NO-SUCH-FUNCTION FUNCTION.
  ..   NO-SUCH-FUNCTION does not name a function.

  (locate 'locate-object 'method)
  .. debugger invoked on LOCATE-ERROR:
  ..   Could not locate LOCATE-OBJECT METHOD.
  ..   The syntax of the METHOD locative is (METHOD <METHOD-QUALIFIERS> <METHOD-SPECIALIZERS>).
  ```"
  (ensure-navigate-loaded)
  (if errorp
      (locate-object object (locative-type locative) (locative-args locative))
      (handler-case
          (locate-object object (locative-type locative)
                         (locative-args locative))
        (locate-error ()
          nil))))

(defun resolve (reference &key (errorp t))
  "A convenience function to LOCATE REFERENCE's object with its
  locative."
  (locate (reference-object reference) (reference-locative reference)
          :errorp errorp))
