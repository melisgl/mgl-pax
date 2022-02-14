(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @pax-manual (:title "PAX Manual")
  ("mgl-pax" asdf:system)
  ("mgl-pax/full" asdf:system)
  (@links section)
  (@background section)
  (@tutorial section)
  (@basics section)
  (@locative-types section)
  (@navigating-in-emacs section)
  (@generating-documentation section)
  (@transcripts section)
  (@extension-api section))

(defsection @links (:title "Links")
  "Here is the [official
  repository](https://github.com/melisgl/mgl-pax) and the [HTML
  documentation](http://melisgl.github.io/mgl-pax-world/mgl-pax-manual.html)
  for the latest version.")

(defsection @background (:title "Background")
  """As a user, I frequently run into documentation that's incomplete
  and out of date, so I tend to stay in the editor and explore the
  code by jumping around with [SLIME][slime]'s [`\\M-.`][slime-m-.].
  As a library author, I spend a great deal of time polishing code but
  precious little writing documentation.

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
  are named by the same symbol. This did not concern exporting, of
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

  Then I realized that sections could refer to other sections if there
  were a SECTION locative. Going down that path, I soon began to feel
  the urge to generate pretty documentation as all the necessary
  information was manifest in the DEFSECTION forms. The design
  constraint imposed on documentation generation was that following
  the typical style of upcasing symbols in docstrings there should be
  no need to explicitly mark up links: if `\\M-.` works, then the
  documentation generator shall also be able find out what's being
  referred to.

  I settled on [Markdown][markdown] as a reasonably non-intrusive
  format, and a few thousand lines later \PAX was born.

  [markdown]: https://daringfireball.net/projects/markdown/""")

(defsection @tutorial (:title "Tutorial")
  """\PAX provides an extremely poor man's Explorable Programming
  environment. Narrative primarily lives in so called sections that
  mix markdown docstrings with references to functions, variables,
  etc, all of which should probably have their own docstrings.

  The primary focus is on making code easily explorable by using
  [SLIME's `\\M-.`][slime-m-.] (`slime-edit-definition`). See how to
  enable some fanciness in @NAVIGATING-IN-EMACS.
  @GENERATING-DOCUMENTATION from sections and all the referenced items
  in Markdown or HTML format is also implemented.

  With the simplistic tools provided, one may accomplish similar
  effects as with Literate Programming, but documentation is generated
  from code, not vice versa and there is no support for chunking. Code
  is first, code must look pretty, documentation is code.

  ##### Docstrings

  \PAX's automatically recognizes and [marks up code][@codification]
  with backticks and [links code][@linking-to-code] to their
  definitions.

  ```
  (document "&KEY arguments such as :IF-EXISTS are common.")
  => ("`&KEY` arguments such as `:IF-EXISTS` are common.
  ")

  (document "AND denotes a macro and a type specifier.
            Here we focus on the macro AND.")
  => ("`AND`([`0`][4954] [`1`][330f]) denotes a macro and a type specifier.
  Here we focus on the macro [`AND`][4954].

    [330f]: http://www.lispworks.com/documentation/HyperSpec/Body/t_and.htm \"AND TYPE\"
    [4954]: http://www.lispworks.com/documentation/HyperSpec/Body/m_and.htm \"AND MGL-PAX:MACRO\"
  ")
  ```

  These features are designed to handle a common style of docstrings
  with minimal additional markup. The following is the output
  of `(mgl-pax:document #'abort)`. Note that the docstring of the
  ABORT function was not written with \PAX in mind.

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
  mentioned in docstrings. See @GENERATING-DOCUMENTATION for some
  convenience functions to cover the most common cases.

  The [transcript][@transcripts] in the code block tagged with
  `cl-transcript` is automatically checked for up-to-dateness when
  documentation is generated.""")


(defsection @basics (:title "Basics")
  "Now let's examine the most important pieces."
  (defsection macro)
  (*discard-documentation-p* variable)
  (define-package macro)
  (@locatives-and-references section)
  (@parsing section))

(defmacro define-package (package &rest options)
  "This is like CL:DEFPACKAGE but silences warnings and errors
  signaled when the redefined package is at variance with the current
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
  where `\TYPE` - called `DOC-TYPE` - is what tells CL:DOCUMENTATION
  that we want the docstring of the type named `FOO`. This design
  supports disambiguation and working with things that are not
  first-class, such as types.

  \PAX generalizes `DOC-TYPE` to the concept of @LOCATIVEs, which may
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

  REFERENCE objects can be represented as an `(OBJECT LOCATIVE)` list
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

(defun locate (object locative &key (errorp t))
  "Follow LOCATIVE from OBJECT and return the object it leads to or a
  REFERENCE if there is no first-class object corresponding to the
  location. Depending on ERRORP, a LOCATE-ERROR condition is signaled
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

(define-condition locate-error (error)
  ((message :initarg :message :reader locate-error-message)
   (object :initarg :object :reader locate-error-object)
   (locative :initarg :locative :reader locate-error-locative))
  (:documentation "Signaled by LOCATE when the lookup fails and ERRORP
  is true.")
  (:report (lambda (condition stream)
             (format stream "~@<Could not locate ~A ~A.~@[ ~A~]~:@>"
                     (locate-error-object condition)
                     (locate-error-locative condition)
                     (locate-error-message condition)))))

(defun resolve (reference &key (errorp t))
  "A convenience function to LOCATE REFERENCE's object with its
  locative."
  (locate (reference-object reference) (reference-locative reference)
          :errorp errorp))
