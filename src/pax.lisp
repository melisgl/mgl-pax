(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @mgl-pax-manual (:title "PAX Manual")
  (mgl-pax asdf:system)
  (mgl-pax/full asdf:system)
  (@mgl-pax-links section)
  (@mgl-pax-background section)
  (@mgl-pax-tutorial section)
  (@mgl-pax-navigating-in-emacs section)
  (@mgl-pax-basics section)
  (@mgl-pax-locative-types section)
  (@mgl-pax-generating-documentation section)
  (@mgl-pax-transcripts section)
  (@mgl-pax-extension-api section))

(defsection @mgl-pax-links (:title "Links")
  "Here is the [official
  repository](https://github.com/melisgl/mgl-pax) and the [HTML
  documentation](http://melisgl.github.io/mgl-pax-world/mgl-pax-manual.html)
  for the latest version.")

(defsection @mgl-pax-background (:export nil :title "Background")
  "As a user, I frequently run into documentation that's incomplete
  and out of date, so I tend to stay in the editor and explore the
  code by jumping around with SLIME's [`M-.`][SLIME-M-.]. As a library
  author, I spend a great deal of time polishing code but precious
  little writing documentation.

  [SLIME-M-.]: http://common-lisp.net/project/slime/doc/html/Finding-definitions.html#Finding-definitions

  In fact, I rarely write anything more comprehensive than docstrings
  for exported stuff. Writing docstrings feels easier than writing a
  separate user manual, and they are always close at hand during
  development. The drawback of this style is that users of the library
  have to piece the big picture together themselves.

  That's easy to solve, I thought, let's just put all the narrative
  that holds docstrings together in the code and be a bit like a
  Literate Programming weenie turned inside out. The original
  prototype, which did almost everything I wanted was this:

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
  birds to kill, and the symbol got accompanied by a type, which was
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
  fanciness in @MGL-PAX-NAVIGATING-IN-EMACS.
  @MGL-PAX-GENERATING-DOCUMENTATION from sections and all the
  referenced items in Markdown or HTML format is also implemented.

  With the simplistic tools provided, one may accomplish similar
  effects as with Literate Programming, but documentation is generated
  from code, not vice versa and there is no support for chunking. Code
  is first, code must look pretty, documentation is code.

  In typical use, PAX packages have no :EXPORT's defined. Instead the
  DEFINE-PACKAGE form gets a docstring, which may mention section
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

  *One can even generate documentation for different but related
  libraries at the same time with the output going to different files
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
  @MGL-PAX-TRANSCRIPTS.""")


(defsection @mgl-pax-basics (:title "Basics")
  "Now let's examine the most important pieces in detail."
  (defsection macro)
  (*discard-documentation-p* variable)
  (define-package macro))

(defmacro define-package (package &rest options)
  "This is like CL:DEFPACKAGE but silences warnings and errors
  signaled when the redefined package is at variance with the current
  state of the package. Typically this situation occurs when symbols
  are exported by calling EXPORT (as is the case with DEFSECTION) as
  opposed to adding :EXPORT forms to the DEFPACKAGE form and the
  package definition is reevaluated. See the section on [package
  variance](http://www.sbcl.org/manual/#Package-Variance) in the SBCL
  manual.

  The bottom line is that if you rely on DEFSECTION to do the
  exporting, then you'd better use DEFINE-PACKAGE."
  `(eval-when (:compile-toplevel :load-toplevel, :execute)
     (locally
         (declare #+sbcl
                  (sb-ext:muffle-conditions sb-kernel::package-at-variance))
       (handler-bind
           (#+sbcl (sb-kernel::package-at-variance #'muffle-warning))
         (cl:defpackage ,package ,@options)))))


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
  'VARIABLE)`. Operations need to know how to deal with references,
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
