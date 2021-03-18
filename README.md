<a id='x-28MGL-PAX-3A-40MGL-PAX-MANUAL-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

# PAX Manual

## Table of Contents

- [1 mgl-pax ASDF System Details][65f5]
- [2 Links][649c]
- [3 Background][1722]
- [4 Tutorial][75cb]
- [5 Emacs Integration][df94]
- [6 Basics][c3d1]
- [7 Generating Documentation][9f28]
    - [7.1 Github Workflow][283b]
    - [7.2 PAX World][a86f]
- [8 Markdown Support][1ea3]
    - [8.1 Indentation][424c]
    - [8.2 Syntax highlighting][bbc9]
    - [8.3 MathJax][1576]
- [9 Documentation Printer Variables][63f8]
- [10 Locative Types][e41b]
- [11 Extension API][9835]
    - [11.1 Locatives and References][6fb5]
    - [11.2 Adding New Object Types][f414]
    - [11.3 Reference Based Extensions][ee12]
    - [11.4 Sections][5db6]
- [12 Transcripts][1095]
    - [12.1 Transcribing with Emacs][f011]
    - [12.2 Transcript API][7dfd]

###### \[in package MGL-PAX with nicknames PAX\]
[![](http://github-actions.40ants.com/svetlyak40wt/mgl-pax/matrix.svg?branch=mgl-pax-minimal)](https://github.com/melisgl/mgl-pax)

<a id='x-28-23A-28-287-29-20BASE-CHAR-20-2E-20-22mgl-pax-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 mgl-pax ASDF System Details

- Version: 0.0.3
- Description: Exploratory programming tool and documentation
  generator.
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)
- Homepage: [http://melisgl.github.io/mgl-pax](http://melisgl.github.io/mgl-pax)
- Bug tracker: [https://github.com/melisgl/mgl-pax/issues](https://github.com/melisgl/mgl-pax/issues)
- Source control: [GIT](https://github.com/melisgl/mgl-pax.git)

<a id='x-28MGL-PAX-3A-40MGL-PAX-LINKS-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 2 Links

Here is the [official
repository](https://github.com/melisgl/mgl-pax) and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/mgl-pax-manual.html)
for the latest version.

<a id='x-28MGL-PAX-3A-40MGL-PAX-BACKGROUND-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 3 Background

As a user, I frequently run into documentation that's incomplete
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

Armed with [`DEFSECTION`][30c2], I soon found myself organizing code following
the flow of user level documentation and relegated comments to
implementational details entirely. However, some portions of
[`DEFSECTION`][30c2] docstrings were just listings of all the functions,
macros and variables related to the narrative, and this list was
effectively repeated in the `DEFPACKAGE` form complete with little
comments that were like section names. A clear violation of
[OAOO][oaoo], one of them had to go, so [`DEFSECTION`][30c2] got a list of
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
  "A single line for one man ..."
  (foo class)
  (bar function))
```

After a bit of elisp hacking, `M-.` was smart enough to disambiguate
based on the locative found in the vicinity of the symbol and
everything was good for a while.

Then I realized that sections could refer to other sections if there
were a [`SECTION`][4fa7] locative. Going down that path, I soon began to feel
the urge to generate pretty documentation as all the necessary
information was manifest in the [`DEFSECTION`][30c2] forms. The design
constraint imposed on documentation generation was that following
the typical style of upcasing symbols in docstrings there should be
no need to explicitly mark up links: if `M-.` works, then the
documentation generator shall also be able find out what's being
referred to.

I settled on [Markdown][markdown] as a reasonably non-intrusive
format, and a few thousand lines later `PAX` was born.

[markdown]: https://daringfireball.net/projects/markdown/ 


<a id='x-28MGL-PAX-3A-40MGL-PAX-TUTORIAL-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 4 Tutorial

`PAX` provides an extremely poor man's Explorable Programming
environment. Narrative primarily lives in so called sections that
mix markdown docstrings with references to functions, variables,
etc, all of which should probably have their own docstrings.

The primary focus is on making code easily explorable by using
SLIME's `M-.` (`slime-edit-definition`). See how to enable some
fanciness in [Emacs Integration][df94]. Generating documentation
from sections and all the referenced items in Markdown or HTML
format is also implemented.

With the simplistic tools provided, one may accomplish similar
effects as with Literate Programming, but documentation is generated
from code, not vice versa and there is no support for chunking yet.
Code is first, code must look pretty, documentation is code.

In typical use, `PAX` packages have no `:EXPORT`'s defined. Instead the
[`DEFINE-PACKAGE`][cb37] form gets a docstring which may mention section
names (defined with [`DEFSECTION`][30c2]). When the code is loaded into the
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
table of contents, etc is possible by calling the [`DOCUMENT`][1eb8] function.

*One can even generate documentation for different, but related
libraries at the same time with the output going to different files,
but with cross-page links being automatically added for symbols
mentioned in docstrings. See [Generating Documentation][9f28] for
some convenience functions to cover the most common cases.*

Note how `(VARIABLE *FOO-STATE*)` in the [`DEFSECTION`][30c2] form both
exports `*FOO-STATE*` and includes its documentation in
`@FOO-RANDOM-MANUAL`. The symbols [`VARIABLE`][2be0] and [`FUNCTION`][6b15] are just two
instances of 'locatives' which are used in [`DEFSECTION`][30c2] to refer to
definitions tied to symbols. See [Locative Types][e41b].

The transcript in the code block tagged with `cl-transcript` is
automatically checked for up-to-dateness. See
[Transcripts][1095].

<a id='x-28MGL-PAX-3A-40MGL-PAX-EMACS-INTEGRATION-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 5 Emacs Integration

Integration into SLIME's `M-.` (`slime-edit-definition`) allows one
to visit the source location of the thing that's identified by a
symbol and the locative before or after the symbol in a buffer. With
this extension, if a locative is the previous or the next expression
around the symbol of interest, then `M-.` will go straight to the
definition which corresponds to the locative. If that fails, `M-.`
will try to find the definitions in the normal way which may involve
popping up an xref buffer and letting the user interactively select
one of possible definitions.

*Note that the this feature is implemented in terms of
`SWANK-BACKEND:FIND-SOURCE-LOCATION` and
`SWANK-BACKEND:FIND-DEFINITIONS` whose support varies across the Lisp
implementations.*

In the following examples, pressing `M-.` when the cursor is on one
of the characters of `FOO` or just after `FOO`, will visit the
definition of function `FOO`:

    function foo
    foo function
    (function foo)
    (foo function)

In particular, references in a [`DEFSECTION`][30c2] form are in (`SYMBOL`
[`LOCATIVE`][12a1]) format so `M-.` will work just fine there.

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
initialization file (or loading `src/pax.el`):

<a id='x-28MGL-PAX-3APAX-2EEL-20-28MGL-PAX-3AINCLUDE-20-23P-22-2Fhome-2Frunner-2Fwork-2Fmgl-pax-2Fmgl-pax-2Fsrc-2Fpax-2Eel-22-20-3AHEADER-NL-20-22-60-60-60elisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29'></a>

```elisp
;;; MGL-PAX M-. integration

(defun slime-edit-locative-definition (name &optional where)
  (or (slime-locate-definition name (slime-locative-before))
      (slime-locate-definition name (slime-locative-after))
      (slime-locate-definition name (slime-locative-after-in-brackets))
      ;; support "foo function" and "function foo" syntax in
      ;; interactive use
      (let ((pos (cl-position ?\s name)))
        (when pos
          (or (slime-locate-definition (cl-subseq name 0 pos)
                                       (cl-subseq name (1+ pos)))
              (slime-locate-definition (cl-subseq name (1+ pos))
                                       (cl-subseq name 0 pos)))))))

(defun slime-locative-before ()
  (ignore-errors (save-excursion
                   (slime-beginning-of-symbol)
                   (slime-last-expression))))

(defun slime-locative-after ()
  (ignore-errors (save-excursion
                   (slime-end-of-symbol)
                   (slime-forward-sexp)
                   (slime-last-expression))))

(defun slime-locative-after-in-brackets ()
  (ignore-errors (save-excursion
                   (slime-end-of-symbol)
                   (skip-chars-forward "`" (+ (point) 1))
                   (when (and (= 1 (skip-chars-forward "\\]" (+ (point) 1)))
                              (= 1 (skip-chars-forward "\\[" (+ (point) 1))))
                     (buffer-substring-no-properties
                      (point)
                      (progn (search-forward "]" nil (+ (point) 1000))
                             (1- (point))))))))

(defun slime-locate-definition (name locative)
  (when locative
    (let ((location
           (slime-eval
            ;; Silently fail if mgl-pax is not loaded.
            `(cl:when (cl:find-package :mgl-pax)
                      (cl:funcall
                       (cl:find-symbol
                        (cl:symbol-name :locate-definition-for-emacs) :mgl-pax)
                       ,name ,locative)))))
      (when (and (consp location)
                 (not (eq (car location) :error)))
        (slime-edit-definition-cont
         (list (make-slime-xref :dspec `(,name)
                                :location location))
         "dummy name"
         where)))))

(add-hook 'slime-edit-definition-hooks 'slime-edit-locative-definition)
```

<a id='x-28MGL-PAX-3A-40MGL-PAX-BASICS-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 6 Basics

Now let's examine the most important pieces in detail.

<a id='x-28MGL-PAX-MINIMAL-3ADEFSECTION-20-28MGL-PAX-MINIMAL-3AMACRO-29-29'></a>

- [macro] **DEFSECTION** *NAME (&KEY (PACKAGE '\*PACKAGE\*) (READTABLE '\*READTABLE\*) (EXPORT T) TITLE LINK-TITLE-TO (DISCARD-DOCUMENTATION-P \*DISCARD-DOCUMENTATION-P\*)) &BODY ENTRIES*

    Define a documentation section and maybe export referenced symbols.
    A bit behind the scenes, a global variable with `NAME` is defined and
    is bound to a [`SECTION`][ba35] object. By convention, section names
    start with the character `@`. See [Tutorial][75cb] for an example.
    
    `ENTRIES` consists of docstrings and references. Docstrings are
    arbitrary strings in markdown format, references are defined in the
    form:
    
        (symbol locative)
    
    For example, `(FOO FUNCTION)` refers to the function `FOO`, `(@BAR
    SECTION)` says that `@BAR` is a subsection of this
    one. `(BAZ (METHOD () (T T T)))` refers to the default method of the
    three argument generic function `BAZ`. `(FOO FUNCTION)` is
    equivalent to `(FOO (FUNCTION))`.
    
    A locative in a reference can either be a symbol or it can be a list
    whose `CAR` is a symbol. In either case, the symbol is the called the
    type of the locative while the rest of the elements are the locative
    arguments. See [Locative Types][e41b] for the list of locative
    types available out of the box.
    
    The same symbol can occur multiple times in a reference, typically
    with different locatives, but this is not required.
    
    The references are not looked up (see [`RESOLVE`][e0d7] in the
    [Extension API][9835]) until documentation is generated, so it is
    allowed to refer to things yet to be defined.
    
    If `EXPORT` is true (the default), the referenced symbols and `NAME` are
    candidates for exporting. A candidate symbol is exported if
    
    - it is accessible in `PACKAGE` (it's not `OTHER-PACKAGE:SOMETHING`)
      and
    
    - there is a reference to it in the section being defined with a
      locative whose type is approved by [`EXPORTABLE-LOCATIVE-TYPE-P`][d5ec].
    
    See [`DEFINE-PACKAGE`][cb37] if you use the export feature. The idea with
    confounding documentation and exporting is to force documentation of
    all exported symbols.
    
    `TITLE` is a non-marked-up string or `NIL`. If non-NIL, it determines
    the text of the heading in the generated output. `LINK-TITLE-TO` is a
    reference given as an
    (`OBJECT` [`LOCATIVE`][12a1]) pair or `NIL`, to which the heading will link when
    generating HTML. If not specified, the heading will link to its own
    anchor.
    
    When `DISCARD-DOCUMENTATION-P` (defaults to [`*DISCARD-DOCUMENTATION-P*`][eb74])
    is true, `ENTRIES` will not be recorded to save memory.

<a id='x-28MGL-PAX-MINIMAL-3A-3A-2ADISCARD-DOCUMENTATION-P-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DISCARD-DOCUMENTATION-P\*** *NIL*

    The default value of [`DEFSECTION`][30c2]'s `DISCARD-DOCUMENTATION-P` argument.
    One may want to set `*DISCARD-DOCUMENTATION-P*` to true before
    building a binary application.

<a id='x-28MGL-PAX-MINIMAL-3ADEFINE-PACKAGE-20-28MGL-PAX-MINIMAL-3AMACRO-29-29'></a>

- [macro] **DEFINE-PACKAGE** *PACKAGE &BODY OPTIONS*

    This is like `CL:DEFPACKAGE` but silences warnings and errors
    signaled when the redefined package is at variance with the current
    state of the package. Typically this situation occurs when symbols
    are exported by calling `EXPORT` (as is the case with [`DEFSECTION`][30c2]) as
    opposed to adding `:EXPORT` forms to the `DEFPACKAGE` form and the
    package definition is reevaluated. See the section on [package
    variance](http://www.sbcl.org/manual/#Package-Variance) in the SBCL
    manual.
    
    The bottom line is that if you rely on [`DEFSECTION`][30c2] to do the
    exporting, then you'd better use [`DEFINE-PACKAGE`][cb37].
    
    To uses this macro in your own `ASDF` systems, add this to the DEFSYSTEM
    form:
    
    ```lisp
    :defsystem-depends-on ("mgl-pax-minimal")
    ```


<a id='x-28MGL-PAX-3ADOCUMENT-20FUNCTION-29'></a>

- [function] **DOCUMENT** *OBJECT &KEY STREAM PAGES (FORMAT :MARKDOWN)*

    Write `OBJECT` in `FORMAT` to `STREAM` diverting some output to `PAGES`.
    `FORMAT` can be anything [3BMD][3bmd] supports which is
    currently `:MARKDOWN`, `:HTML` and `:PLAIN`. `STREAM` may be a stream
    object, `T` or `NIL` as with `CL:FORMAT`.
    
    Most often, this function is called on section objects
    like `(DOCUMENT @MGL-PAX-MANUAL)`, but it supports all kinds of
    objects for which [`DOCUMENT-OBJECT`][a05e] is defined. To look up the
    documentation of function [`DOCUMENT`][1eb8]:
    
        (document #'document)
    
    To generate the documentation for separate libraries with automatic
    cross-links:
    
        (document (list @cube-manual @mat-manual))
    
    Note that not only first class objects can have documentation. For
    instance, variables and deftypes are not represented by objects.
    That's why `CL:DOCUMENTATION` has a `DOC-TYPE` argument. [`DOCUMENT`][1eb8] does
    not have anything like that, instead it relies on [`REFERENCE`][0d20] objects
    to carry the extra information. We are going to see later how
    references and locatives work. Until then, here is an example on how
    to look up the documentation of type `FOO`:
    
        (document (locate 'foo 'type))
    
    One can call `DESCRIBE` on [`SECTION`][ba35] objects to get
    documentation in markdown format with less markup than the default.
    See [`DESCRIBE-OBJECT`][a7c9] `(METHOD () (SECTION T))`.
    
    There are quite a few special variables that affect how output is
    generated, see [Documentation Printer Variables][63f8].
    
    The rest of this description deals with how to generate multiple
    pages.
    
    The `PAGES` argument is to create multi-page documents by routing some
    of the generated output to files, strings or streams. `PAGES` is a
    list of page specification elements. A page spec is a plist with
    keys `:OBJECTS`, `:OUTPUT`, `:URI-FRAGMENT`, `:SOURCE-URI-FN`, `:HEADER-FN`
    and `:FOOTER-FN`. `OBJECTS` is a list of objects (references are allowed
    but not required) whose documentation is to be sent to `OUTPUT`.
    
    When documentation for an object is generated, the first matching
    page spec is used, where the object matches the page spec if it is
    contained in one of its `:OBJECTS` in the sense of
    [`COLLECT-REACHABLE-OBJECTS`][1920].
    
    `OUTPUT` can be a number things:
    
    - If it's a list whose first element is a string or a pathname, then
      output will be sent to the file denoted by that and the rest of
      the elements of the list are passed on as arguments to `CL:OPEN`.
      One extra keyword argument is `:ENSURE-DIRECTORIES-EXIST`. If it's
      true, `ENSURE-DIRECTORIES-EXIST` will be called on the pathname
      before it's opened.
    
    - If it's `NIL`, then output will be collected in a string.
    
    - If it's `T`, then output will be sent to `*STANDARD-OUTPUT*`.
    
    - If it's a stream, then output will be sent to that stream.
    
    If some pages are specified, [`DOCUMENT`][1eb8] returns a list of designators
    for generated output. If a page whose `OUTPUT` refers to a file that
    was created (which doesn't happen if nothing would be written to
    it), then the corresponding pathname is included in the list. For
    strings the string itself, while for streams the stream object is
    included in the list. This way it's possible to write some pages to
    files and some to strings and have the return value indicate what
    was created. The output designators in the returned list are ordered
    by creation time.
    
    If no `PAGES` are specified, [`DOCUMENT`][1eb8] returns a single pathname,
    string or stream object according to the value of the `STREAM`
    argument.
    
    Note that even if `PAGES` is specified, `STREAM` acts as a catch all
    taking the generated documentation for references not claimed by any
    pages. Also, the filename, string or stream corresponding to `STREAM`
    is always the first element in list of generated things that is the
    return value.
    
    `HEADER-FN`, if not `NIL`, is a function of a single stream argument
    which is called just before the first write to the page.
    Since `:FORMAT` `:HTML` only generates HTML fragments, this makes it
    possible to print arbitrary headers, typically setting the title,
    css stylesheet, or charset.
    
    `FOOTER-FN` is similar to `HEADER-FN`, but it's called after the last
    write to the page. For HTML, it typically just closes the body.
    
    `URI-FRAGMENT` is a string such as `"doc/manual.html"` that specifies
    where the page will be deployed on a webserver. It defines how links
    between pages will look. If it's not specified and `OUTPUT` refers
    to a file, then it defaults to the name of the file. If `URI-FRAGMENT`
    is `NIL`, then no links will be made to or from that page.
    
    Finally, `SOURCE-URI-FN` is a function of a single, [`REFERENCE`][0d20]
    argument. If it returns a value other than `NIL`, then it must be a
    string representing an URI. If `FORMAT` is `:HTML` and
    [`*DOCUMENT-MARK-UP-SIGNATURES*`][46ea] is true, then the locative as
    displayed in the signature will be a link to this uri. See
    [`MAKE-GITHUB-SOURCE-URI-FN`][1cc6].
    
    `PAGES` may look something like this:
    
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
    ```


<a id='x-28MGL-PAX-3A-40MGL-PAX-GENERATING-DOCUMENTATION-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 7 Generating Documentation

Two convenience functions are provided to serve the common case of
having an `ASDF` system with some readmes and a directory with for the
HTML documentation and the default css stylesheet.

<a id='x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-READMES-20FUNCTION-29'></a>

- [function] **UPDATE-ASDF-SYSTEM-READMES** *SECTIONS ASDF-SYSTEM*

    Convenience function to generate two readme files in the directory
    holding the `ASDF-SYSTEM` definition.
    
    README.md has anchors, links, inline code, and other markup added.
    Not necessarily the easiest on the eye in an editor, but looks good
    on github.
    
    README is optimized for reading in text format. Has no links and
    cluttery markup.
    
    Example usage:
    
    ```
    (update-asdf-system-readmes @mgl-pax-manual :mgl-pax)
    ```


<a id='x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-HTML-DOCS-20FUNCTION-29'></a>

- [function] **UPDATE-ASDF-SYSTEM-HTML-DOCS** *SECTIONS ASDF-SYSTEM &KEY PAGES (TARGET-DIR (ASDF/SYSTEM:SYSTEM-RELATIVE-PATHNAME ASDF-SYSTEM "doc/")) (UPDATE-CSS-P T)*

    Generate pretty HTML documentation for a single `ASDF` system,
    possibly linking to github. If `UPDATE-CSS-P`, copy the CSS style
    sheet to `TARGET-DIR`, as well. Example usage:
    
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
                         "https://github.com/melisgl/mgl-pax"))))
    ```


<a id='x-28MGL-PAX-3A-2ADOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL\*** *NIL*

    `NIL` or a non-negative integer. If non-NIL, it overrides
    [`*DOCUMENT-MAX-NUMBERING-LEVEL*`][fde0] in dynamic HTML table of contents on
    the left of the page.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-HTML-TOP-BLOCKS-OF-LINKS-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS\*** *NIL*

    A list of blocks of links to be display on the sidebar on the left,
    above the table of contents. A block is of the form `(&KEY TITLE ID
    LINKS)`, where `TITLE` will be displayed at the top of the block in a
    HTML `DIV` with `ID`, followed by the links. `LINKS` is a list
    of `(URI LABEL) elements.`

<a id='x-28MGL-PAX-3A-2ADOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS\*** *NIL*

    Like [`*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*`][be7f], only it is displayed
    below the table of contents.

<a id='x-28MGL-PAX-3A-40MGL-PAX-GITHUB-WORKFLOW-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 7.1 Github Workflow

It is generally recommended to commit generated readmes (see
[`UPDATE-ASDF-SYSTEM-READMES`][2e7a]) so that users have something to read
without reading the code and sites like github can display them.

HTML documentation can also be committed, but there is an issue with
that: when linking to the sources (see [`MAKE-GITHUB-SOURCE-URI-FN`][1cc6]),
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
idea to add section like the [Links][649c] section to allow jumping
between the repository and the gh-pages site.

<a id='x-28MGL-PAX-3AMAKE-GITHUB-SOURCE-URI-FN-20FUNCTION-29'></a>

- [function] **MAKE-GITHUB-SOURCE-URI-FN** *ASDF-SYSTEM GITHUB-URI &KEY GIT-VERSION*

    Return a function suitable as `:SOURCE-URI-FN` of a page spec (see
    the `PAGES` argument of [`DOCUMENT`][1eb8]). The function looks the source
    location of the reference passed to it, and if the location is
    found, the path is made relative to the root directory of
    `ASDF-SYSTEM` and finally an URI pointing to github is returned. The
    URI looks like this:
    
        https://github.com/melisgl/mgl-pax/blob/master/src/pax-early.lisp#L12
    
    "master" in the above link comes from `GIT-VERSION`.
    
    If `GIT-VERSION` is `NIL`, then an attempt is made to determine to
    current commit id from the `.git` in the directory holding
    `ASDF-SYSTEM`. If no `.git` directory is found, then no links to
    github will be generated.
    
    A separate warning is signalled whenever source location lookup
    fails or if the source location points to a directory not below the
    directory of `ASDF-SYSTEM`.

<a id='x-28MGL-PAX-3A-40MGL-PAX-WORLD-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 7.2 PAX World

`PAX` World is a registry of documents, which can generate
cross-linked HTML documentation pages for all the registered
documents.

<a id='x-28MGL-PAX-3AREGISTER-DOC-IN-PAX-WORLD-20FUNCTION-29'></a>

- [function] **REGISTER-DOC-IN-PAX-WORLD** *NAME SECTIONS PAGE-SPECS*

    Register `SECTIONS` and `PAGE-SPECS` under `NAME` in `PAX` World. By
    default, [`UPDATE-PAX-WORLD`][4f82] generates documentation for all of these.

For example, this is how `PAX` registers itself:

<a id='x-28MGL-PAX-3A-3AREGISTER-DOC-EXAMPLE-20-28MGL-PAX-3AINCLUDE-20-28-3ASTART-20-28MGL-PAX-3A-3APAX-SECTIONS-20FUNCTION-29-20-3AEND-20-28MGL-PAX-3A-3AEND-OF-REGISTER-DOC-EXAMPLE-20VARIABLE-29-29-20-3AHEADER-NL-20-22-60-60-60commonlisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29'></a>

```commonlisp
(defun pax-sections ()
  (list @mgl-pax-manual))
(defun pax-pages ()
  `((:objects
     (,mgl-pax:@mgl-pax-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :mgl-pax
                      "https://github.com/melisgl/mgl-pax"))))
(register-doc-in-pax-world :mgl-pax (pax-sections) (pax-pages))
```

<a id='x-28MGL-PAX-3AUPDATE-PAX-WORLD-20FUNCTION-29'></a>

- [function] **UPDATE-PAX-WORLD** *&KEY DOCS DIR*

    Generate HTML documentation for all `DOCS`. By default, files are
    created in `*PAX-WORLD-DIR*` or `(asdf:system-relative-pathname
    :mgl-pax "world/")`, if `NIL`. `DOCS` is a list of entries of the
    form (`NAME` `SECTIONS` `PAGE-SPECS`). The default for `DOCS` is all the
    sections and pages registered with [`REGISTER-DOC-IN-PAX-WORLD`][c5f2].
    
    In the absence of `:HEADER-FN` `:FOOTER-FN`, `:OUTPUT`, every spec in
    `PAGE-SPECS` is augmented with HTML headers, footers and output
    location specifications (based on the name of the section).
    
    If necessary a default page spec is created for every section.

<a id='x-28MGL-PAX-3A-40MGL-PAX-MARKDOWN-SUPPORT-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 8 Markdown Support

The [Markdown][markdown] in docstrings is processed with the
[3BMD][3bmd] library.

<a id='x-28MGL-PAX-3A-40MGL-PAX-MARKDOWN-INDENTATION-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 8.1 Indentation

Docstrings can be indented in any of the usual styles. `PAX`
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

See [`DOCUMENT-OBJECT`][d7eb] for the details.

<a id='x-28MGL-PAX-3A-40MGL-PAX-MARKDOWN-SYNTAX-HIGHLIGHTING-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 8.2 Syntax highlighting

For syntax highlighting, github's [fenced code
blocks][fenced-code-blocks] markdown extension to mark up code
blocks with triple backticks is enabled so all you need to do is
write:

    ```elisp
    (defun foo ())
    ```

to get syntactically marked up HTML output. Copy `src/style.css`
from `PAX` and you are set. The language tag, `elisp` in this example,
is optional and defaults to `common-lisp`.

See the documentation of [3BMD][3bmd] and [colorize][colorize] for
the details.

[3bmd]: https://github.com/3b/3bmd 

[colorize]: https://github.com/redline6561/colorize/ 

[fenced-code-blocks]: https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks 


<a id='x-28MGL-PAX-3A-40MGL-PAX-MATHJAX-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 8.3 MathJax

Displaying pretty mathematics in TeX format is supported via
MathJax. It can be done inline with `$` like this:

    $\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$

which is diplayed as $\int\_0^\infty e^{-x^2}
dx=\frac{\sqrt{\pi}}{2}$, or it can be delimited by `$$` like this:

    $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

to get: $$\int\_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

MathJax will leave code blocks (including those inline with
backticks) alone. Outside code blocks, escape `$` by prefixing it
with a backslash to scare MathJax off.

Escaping all those backslashes in TeX fragments embedded in Lisp
strings can be a pain. [Pythonic String
Reader](https://github.com/smithzvk/pythonic-string-reader) can help
with that.

<a id='x-28MGL-PAX-3A-40MGL-PAX-DOCUMENTATION-PRINTER-VARIABLES-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 9 Documentation Printer Variables

Docstrings are assumed to be in markdown format and they are pretty
much copied verbatim to the documentation subject to a few knobs
described below.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-UPPERCASE-IS-CODE\*** *T*

    When true, words with at least three characters and no lowercase
    characters naming an interned symbol are assumed to be code as if
    they were marked up with backticks which is especially useful when
    combined with [`*DOCUMENT-LINK-CODE*`][1d4c]. For example, this docstring:
    
        "`FOO` and FOO."
    
    is equivalent to this:
    
        "`FOO` and `FOO`."
    
    iff `FOO` is an interned symbol. To suppress this behavior, add a
    backslash to the beginning of the symbol or right after the leading
    \* if it would otherwise be parsed as markdown emphasis:
    
        "\\MGL-PAX *\\DOCUMENT-NORMALIZE-PACKAGES*"
    
    The number of backslashes is doubled above because that's how the
    example looks in a docstring. Note that the backslash is discarded
    even if `*DOCUMENT-UPPERCASE-IS-CODE*` is false.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-DOWNCASE-UPPERCASE-CODE-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-DOWNCASE-UPPERCASE-CODE\*** *NIL*

    If true, then the names of symbols recognized as code (including
    those found if [`*DOCUMENT-UPPERCASE-IS-CODE*`][a282]) are downcased in the
    output if they only consist of uppercase characters. If it is
    `:ONLY-IN-MARKUP`, then if the output format does not support
    markup (e.g. it's `:PLAIN`), then no downcasing is performed.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-LINK-CODE-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-LINK-CODE\*** *T*

    When true, during the process of generating documentation for a
    [`SECTION`][ba35], HTML anchors are added before the documentation of
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
    Since symbol names are parsed according to `READTABLE-CASE`, character
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
    
    This situation occurs in `PAX` with `SECTION`([`0`][ba35] [`1`][4fa7]) which is both a class (see
    [`SECTION`][ba35]) and a locative type denoted by a symbol (see
    [`SECTION`][4fa7]). Back in the example above, clearly,
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
    
    Note that [`*DOCUMENT-LINK-CODE*`][1d4c] can be combined with
    [`*DOCUMENT-UPPERCASE-IS-CODE*`][a282] to have links generated for
    uppercase names with no quoting required.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-LINK-SECTIONS-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-LINK-SECTIONS\*** *T*

    When true, HTML anchors are generated before the heading of
    sections which allows the table of contents to contain links and
    also code-like references to sections (like `@FOO-MANUAL`) to be
    translated to links with the section title being the name of the
    link.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-MIN-LINK-HASH-LENGTH-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-MIN-LINK-HASH-LENGTH\*** *4*

    Recall that markdown reference style links (like `[label][id]`) are
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
    it can make markdown output more readable.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-MARK-UP-SIGNATURES-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-MARK-UP-SIGNATURES\*** *T*

    When true, some things such as function names and arglists are
    rendered as bold and italic. In `:HTML` output, locative types become
    links to sources (if `:SOURCE-URI-FN` is provided, see [`DOCUMENT`][1eb8]), and
    the symbol becomes a self-link for your permalinking pleasure.
    
    For example, a reference is rendered in markdown roughly as:
    
        - [function] foo x y
    
    With this option on, the above becomes:
    
        - [function] **foo** *x y*
    
    Also, in HTML `**foo**` will be a link to that very entry and
    `[function]` may turn into a link to sources.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-MAX-NUMBERING-LEVEL-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-MAX-NUMBERING-LEVEL\*** *3*

    A non-negative integer. In their hierarchy, sections on levels less
    than this value get numbered in the format of `3.1.2`. Setting it to
    0 turns numbering off.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL\*** *3*

    A non-negative integer. Top-level sections are given a table of
    contents which includes a nested tree of section titles whose depth
    is limited by this value. Setting it to 0 turns generation of the
    table of contents off. If [`*DOCUMENT-LINK-SECTIONS*`][f901] is true, then the
    table of contents will link to the sections.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-TEXT-NAVIGATION-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-TEXT-NAVIGATION\*** *NIL*

    If true, then before each heading a line is printed with links to
    the previous, parent and next section. Needs
    [`*DOCUMENT-LINK-SECTIONS*`][f901] to be on to work.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-FANCY-HTML-NAVIGATION-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-FANCY-HTML-NAVIGATION\*** *T*

    If true and the output format is HTML, then headings get a
    navigation component that consists of links to the previous, parent,
    next section and a permalink. This component is normally hidden, it
    is visible only when the mouse is over the heading. Needs
    [`*DOCUMENT-LINK-SECTIONS*`][f901] to be on to work.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-NORMALIZE-PACKAGES-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-NORMALIZE-PACKAGES\*** *T*

    If true, symbols are printed relative to [`SECTION-PACKAGE`][07a3] of the
    innermost containing section or with full package names if there is
    no containing section. To eliminate ambiguity `[in package ...]`
    messages are printed right after the section heading if necessary.
    If false, symbols are always printed relative to the current
    package.

<a id='x-28MGL-PAX-3A-40MGL-PAX-LOCATIVE-TYPES-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 10 Locative Types

These are the locatives type supported out of the box. As all
locative types, they are symbols and their names should make it
obvious what kind of things they refer to. Unless otherwise noted,
locatives take no arguments.

<a id='x-28ASDF-2FSYSTEM-3ASYSTEM-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **ASDF/SYSTEM:SYSTEM**

    Refers to an asdf system. The generated documentation will include
    meta information extracted from the system definition. This also
    serves as an example of a symbol that's not accessible in the
    current package and consequently is not exported.

<a id='x-28MGL-PAX-MINIMAL-3ASECTION-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **SECTION**

    Refers to a section defined by [`DEFSECTION`][30c2].

<a id='x-28VARIABLE-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **VARIABLE** *&OPTIONAL INITFORM*

    Refers to a global special variable. `INITFORM`, or if not specified,
    the global value of the variable is included in the documentation.

<a id='x-28MGL-PAX-3ACONSTANT-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **CONSTANT** *&OPTIONAL INITFORM*

    Refers to a `DEFCONSTANT`. `INITFORM`, or if not specified,
    the value of the constant is included in the documentation.

<a id='x-28MGL-PAX-MINIMAL-3AMACRO-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **MACRO**

<a id='x-28COMPILER-MACRO-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **COMPILER-MACRO**

<a id='x-28FUNCTION-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **FUNCTION**

    Note that the arglist in the generated documentation depends on
    the quality of `SWANK-BACKEND:ARGLIST`. It may be that default
    values of optional and keyword arguments are missing.

<a id='x-28GENERIC-FUNCTION-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **GENERIC-FUNCTION**

<a id='x-28METHOD-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **METHOD** *METHOD-QUALIFIERS METHOD-SPECIALIZERS*

    See `CL:FIND-METHOD` for the description of the arguments.
    To refer to the default method of the three argument generic
    function `FOO`:
    
        (foo (method () (t t t)))


<a id='x-28MGL-PAX-MINIMAL-3AACCESSOR-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **ACCESSOR** *CLASS-NAME*

    To refer to an accessor named `FOO-SLOT` of class
    `FOO`:
    
        (foo-slot (accessor foo))


<a id='x-28MGL-PAX-MINIMAL-3AREADER-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **READER** *CLASS-NAME*

    To refer to a reader named `FOO-SLOT` of class
    `FOO`:
    
        (foo-slot (reader foo))


<a id='x-28MGL-PAX-MINIMAL-3AWRITER-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **WRITER** *CLASS-NAME*

    To refer to a writer named `FOO-SLOT` of class
    `FOO`:
    
        (foo-slot (writer foo))


<a id='x-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **STRUCTURE-ACCESSOR**

    This is a synonym of [`FUNCTION`][6b15] with the difference that the often
    ugly and certainly uninformative lambda list will not be printed.

<a id='x-28CLASS-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **CLASS**

<a id='x-28CONDITION-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **CONDITION**

<a id='x-28RESTART-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **RESTART**

<a id='x-28MGL-PAX-3ADEFINE-RESTART-20-28MGL-PAX-MINIMAL-3AMACRO-29-29'></a>

- [macro] **DEFINE-RESTART** *SYMBOL LAMBDA-LIST &BODY DOCSTRING*

    A definer macro to hang the documentation of a restart on a
    symbol.
    
    ```
    (define-restart my-ignore-error ()
      "Available when MY-ERROR is signalled, MY-IGNORE-ERROR unsafely continues.")
    ```
    
    Note that while there is a [`CL:RESTART`][ba5d] class, there is no
    corresponding source location or docstring like for [`CONDITION`][af3c]s.

<a id='x-28TYPE-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **TYPE**

    `TYPE` can refer to classes as well, but it's better style to use the
    more specific [`CLASS`][0208] locative type for that. Another difference to
    [`CLASS`][0208] is that an attempt is made at printing the arguments of type
    specifiers.

<a id='x-28PACKAGE-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **PACKAGE**

<a id='x-28MGL-PAX-3ADISLOCATED-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **DISLOCATED**

    Refers to a symbol in a non-specific context. Useful for preventing
    autolinking. For example, if there is a function called `FOO` then
    
        `FOO`
    
    will be linked to (if [`*DOCUMENT-LINK-CODE*`][1d4c]) its definition. However,
    
        [`FOO`][dislocated]
    
    will not be. On a dislocated locative [`LOCATE`][b2be] always fails with a
    [`LOCATE-ERROR`][2285] condition.

<a id='x-28MGL-PAX-3AARGUMENT-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **ARGUMENT**

    An alias for [`DISLOCATED`][7078], so the one can refer to an argument of a
    macro without accidentally linking to a class that has the same name
    as that argument. In the following example, `FORMAT` may link to
    `CL:FORMAT` (if we generated documentation for it):
    
    ```
    "See the FORMAT in DOCUMENT."
    ```
    
    Since `ARGUMENT` is a locative, we can prevent that linking by writing:
    
    ```
    "See the FORMAT argument of DOCUMENT."
    ```


<a id='x-28MGL-PAX-3ALOCATIVE-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **LOCATIVE** *LAMBDA-LIST*

    This is the locative for locatives. When `M-.` is pressed on
    [`VARIABLE`][2be0] in `(VARIABLE LOCATIVE)`, this is what makes it possible
    to land at the `(DEFINE-LOCATIVE-TYPE VARIABLE ...)` form.
    Similarly, `(LOCATIVE LOCATIVE)` leads to this very definition.

<a id='x-28MGL-PAX-3AGLOSSARY-TERM-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **GLOSSARY-TERM**

    Refers to a glossary term defined by [`DEFINE-GLOSSARY-TERM`][cb09].

<a id='x-28MGL-PAX-3ADEFINE-GLOSSARY-TERM-20-28MGL-PAX-MINIMAL-3AMACRO-29-29'></a>

- [macro] **DEFINE-GLOSSARY-TERM** *NAME (&KEY TITLE (DISCARD-DOCUMENTATION-P \*DISCARD-DOCUMENTATION-P\*)) DOCSTRING*

    Define a global variable with `NAME` and set it to a glossary term
    object. A glossary term is just a symbol to hang a docstring on. It
    is a bit like a `SECTION`([`0`][ba35] [`1`][4fa7]) in that, when linked to, its `TITLE` will be
    the link text instead of the name of the symbol. Unlike sections
    though, glossary terms are not rendered with headings, but in the
    more lightweight bullet + locative + name/title style.
    
    When `DISCARD-DOCUMENTATION-P` (defaults to [`*DISCARD-DOCUMENTATION-P*`][eb74])
    is true, `DOCSTRING` will not be recorded to save memory.

<a id='x-28MGL-PAX-3AINCLUDE-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **INCLUDE** *SOURCE &KEY LINE-PREFIX HEADER FOOTER HEADER-NL FOOTER-NL*

    Refers to a region of a file. `SOURCE` can be a string or a
    pathname in which case the whole file is being pointed to or it can
    explicitly supply `START`, `END` locatives. `INCLUDE` is typically used to
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
    
    In the above example, pressing `M-.` on [`PAX.EL`][b56d] will open the
    `src/pax.el` file and put the cursor on its first character. `M-.`
    on `FOO-EXAMPLE` will go to the source location of the `(asdf:system
    locative)` locative.
    
    When documentation is generated, the entire [`pax.el`][b56d] file is
    included in the markdown surrounded by the strings given as
    `HEADER-NL` and `FOOTER-NL` (if any). The trailing newline character is
    assumed implicitly. If that's undesirable, then use `HEADER` and
    `FOOTER` instead. The documentation of `FOO-EXAMPLE` will be the
    region of the file from the source location of the `START`
    locative (inclusive) to the source location of the `END`
    locative (exclusive). `START` and `END` default to the beginning and end
    of the file, respectively.
    
    Note that the file of the source location of `:START` and `:END` must be
    the same. If `SOURCE` is pathname designator, then it must be absolute
    so that the locative is context independent.
    
    Finally, if specified `LINE-PREFIX` is a string that's prepended to
    each line included in the documentation. For example, a string of
    four spaces makes markdown think it's a code block.

<a id='x-28MGL-PAX-3A-40MGL-PAX-EXTENSION-API-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 11 Extension API

<a id='x-28MGL-PAX-3A-40MGL-PAX-LOCATIVES-AND-REFERENCES-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 11.1 Locatives and References

While Common Lisp has rather good introspective abilities, not
everything is first class. For example, there is no object
representing the variable defined with `(DEFVAR
FOO)`. `(MAKE-REFERENCE 'FOO 'VARIABLE)` constructs a [`REFERENCE`][0d20] that
captures the path to take from an object (the symbol `FOO`) to an
entity of interest (for example, the documentation of the variable).
The path is called the locative. A locative can be applied to an
object like this:

```
(locate 'foo 'variable)
```

which will return the same reference as `(MAKE-REFERENCE 'FOO
'VARIABLE)`. Operations need to know how to deal with references
which we will see in [`LOCATE-AND-COLLECT-REACHABLE-OBJECTS`][7a11],
[`LOCATE-AND-DOCUMENT`][6c17] and [`LOCATE-AND-FIND-SOURCE`][e9e9].

Naturally, `(LOCATE 'FOO 'FUNCTION)` will simply return `#'FOO`, no
need to muck with references when there is a perfectly good object.

<a id='x-28MGL-PAX-3ALOCATE-20FUNCTION-29'></a>

- [function] **LOCATE** *OBJECT LOCATIVE &KEY (ERRORP T)*

    Follow `LOCATIVE` from `OBJECT` and return the object it leads to or a
    [`REFERENCE`][0d20] if there is no first class object corresponding to the
    location. If `ERRORP`, then a [`LOCATE-ERROR`][2285] condition is signaled when
    the lookup fails.

<a id='x-28MGL-PAX-3ALOCATE-ERROR-20CONDITION-29'></a>

- [condition] **LOCATE-ERROR** *ERROR*

    Signaled by [`LOCATE`][b2be] when the lookup fails and `ERRORP`
    is true.

<a id='x-28MGL-PAX-3ALOCATE-ERROR-MESSAGE-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29'></a>

- [reader] **LOCATE-ERROR-MESSAGE** *LOCATE-ERROR* *(:MESSAGE)*

<a id='x-28MGL-PAX-3ALOCATE-ERROR-OBJECT-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29'></a>

- [reader] **LOCATE-ERROR-OBJECT** *LOCATE-ERROR* *(:OBJECT)*

<a id='x-28MGL-PAX-3ALOCATE-ERROR-LOCATIVE-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29'></a>

- [reader] **LOCATE-ERROR-LOCATIVE** *LOCATE-ERROR* *(:LOCATIVE)*

<a id='x-28MGL-PAX-3ARESOLVE-20FUNCTION-29'></a>

- [function] **RESOLVE** *REFERENCE &KEY (ERRORP T)*

    A convenience function to [`LOCATE`][b2be] `REFERENCE`'s object with its
    locative.

<a id='x-28MGL-PAX-MINIMAL-3AREFERENCE-20CLASS-29'></a>

- [class] **REFERENCE**

    A `REFERENCE` represents a path ([`REFERENCE-LOCATIVE`][4888])
    to take from an object ([`REFERENCE-OBJECT`][1f1e]).

<a id='x-28MGL-PAX-MINIMAL-3AREFERENCE-OBJECT-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-MINIMAL-3AREFERENCE-29-29'></a>

- [reader] **REFERENCE-OBJECT** *REFERENCE* *(:OBJECT)*

<a id='x-28MGL-PAX-MINIMAL-3AREFERENCE-LOCATIVE-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-MINIMAL-3AREFERENCE-29-29'></a>

- [reader] **REFERENCE-LOCATIVE** *REFERENCE* *(:LOCATIVE)*

<a id='x-28MGL-PAX-MINIMAL-3AMAKE-REFERENCE-20FUNCTION-29'></a>

- [function] **MAKE-REFERENCE** *OBJECT LOCATIVE*

<a id='x-28MGL-PAX-MINIMAL-3ALOCATIVE-TYPE-20FUNCTION-29'></a>

- [function] **LOCATIVE-TYPE** *LOCATIVE*

    The first element of [`LOCATIVE`][12a1] if it's a list. If it's a symbol then
    it's that symbol itself. Typically, methods of generic functions
    working with locatives take locative type and locative args as
    separate arguments to allow methods have eql specializers on the
    type symbol.

<a id='x-28MGL-PAX-MINIMAL-3ALOCATIVE-ARGS-20FUNCTION-29'></a>

- [function] **LOCATIVE-ARGS** *LOCATIVE*

    The `REST` of [`LOCATIVE`][12a1] if it's a list. If it's a symbol then
    it's ().

<a id='x-28MGL-PAX-3A-40MGL-PAX-NEW-OBJECT-TYPES-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 11.2 Adding New Object Types

One may wish to make the [`DOCUMENT`][1eb8] function and `M-.` navigation
work with new object types. Extending [`DOCUMENT`][1eb8] can be done by
defining a [`DOCUMENT-OBJECT`][a05e] method. To allow these objects to be
referenced from [`DEFSECTION`][30c2], a [`LOCATE-OBJECT`][acc9] method is to be defined.
Finally, for `M-.` [`FIND-SOURCE`][b417] can be specialized. Finally,
[`EXPORTABLE-LOCATIVE-TYPE-P`][d5ec] may be overridden if exporting does not
makes sense. Here is a stripped down example of how all this is done
for [`ASDF:SYSTEM:`][bf8a]

<a id='x-28MGL-PAX-3AASDF-EXAMPLE-20-28MGL-PAX-3AINCLUDE-20-28-3ASTART-20-28ASDF-2FSYSTEM-3ASYSTEM-20MGL-PAX-3ALOCATIVE-29-20-3AEND-20-28MGL-PAX-3A-3AEND-OF-ASDF-EXAMPLE-20VARIABLE-29-29-20-3AHEADER-NL-20-22-60-60-60commonlisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29'></a>

```commonlisp
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

```

<a id='x-28MGL-PAX-3ADEFINE-LOCATIVE-TYPE-20-28MGL-PAX-MINIMAL-3AMACRO-29-29'></a>

- [macro] **DEFINE-LOCATIVE-TYPE** *LOCATIVE-TYPE LAMBDA-LIST &BODY DOCSTRING*

    Declare `LOCATIVE-TYPE` as a [`LOCATIVE`][12a1]. One gets two
    things in return: first, a place to document the format and
    semantics of `LOCATIVE-TYPE` (in `LAMBDA-LIST` and `DOCSTRING`); second,
    being able to reference `(LOCATIVE-TYPE LOCATIVE)`. For example, if
    you have:
    
    ```common-lisp
    (define-locative-type variable (&optional initform)
      "Dummy docstring.")
    ```
    
    then `(VARIABLE LOCATIVE)` refers to this form.

<a id='x-28MGL-PAX-MINIMAL-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29'></a>

- [generic-function] **EXPORTABLE-LOCATIVE-TYPE-P** *LOCATIVE-TYPE*

    Return true iff symbols in references with
    `LOCATIVE-TYPE` are to be exported by default when they occur in a
    [`DEFSECTION`][30c2]. The default method returns `T`, while the methods for
    [`PACKAGE`][a15b], [`ASDF:SYSTEM`][bf8a] and [`METHOD`][f1a0] return `NIL`.
    
    [`DEFSECTION`][30c2] calls this function to decide what symbols to export when
    its `EXPORT` argument is true.

<a id='x-28MGL-PAX-3ALOCATE-OBJECT-20GENERIC-FUNCTION-29'></a>

- [generic-function] **LOCATE-OBJECT** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    Return the object, to which `OBJECT` and the locative
    refer. For example, if `LOCATIVE-TYPE` is the symbol [`PACKAGE`][a15b], this
    returns `(FIND-PACKAGE SYMBOL)`. Signal a [`LOCATE-ERROR`][2285] condition by
    calling the [`LOCATE-ERROR`][f3b7] function if the lookup fails. Signal other
    errors if the types of the argument are bad, for instance
    `LOCATIVE-ARGS` is not the empty list in the package example. If a
    [`REFERENCE`][0d20] is returned then it must be canonical in the sense that
    calling [`CANONICAL-REFERENCE`][24fc] on it will return the same reference.
    For extension only, don't call this directly.

<a id='x-28MGL-PAX-3ALOCATE-ERROR-20FUNCTION-29'></a>

- [function] **LOCATE-ERROR** *&REST FORMAT-AND-ARGS*

    Call this function to signal a [`LOCATE-ERROR`][2285] condition from a
    [`LOCATE-OBJECT`][acc9] method. `FORMAT-AND-ARGS` contains a format string and
    args suitable for `FORMAT` from which the [`LOCATE-ERROR-MESSAGE`][b4d2] is
    constructed. If `FORMAT-AND-ARGS` is `NIL`, then the message will be `NIL`
    too.
    
    The object and the locative are not specified, they are added by
    [`LOCATE`][b2be] when it resignals the condition.

<a id='x-28MGL-PAX-3ACANONICAL-REFERENCE-20GENERIC-FUNCTION-29'></a>

- [generic-function] **CANONICAL-REFERENCE** *OBJECT*

    Return a [`REFERENCE`][0d20] that resolves to `OBJECT`.

<a id='x-28MGL-PAX-3ACOLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29'></a>

- [generic-function] **COLLECT-REACHABLE-OBJECTS** *OBJECT*

    Return a list of objects representing all things
    that would be documented in a ([`DOCUMENT`][1eb8] `OBJECT`) call. For sections
    this is simply the union of references reachable from references in
    [`SECTION-ENTRIES`][6e6e]. The returned objects can be anything provided that
    [`CANONICAL-REFERENCE`][24fc] works on them. The list need not include `OBJECT`
    itself.
    
    One only has to specialize this for new container-like objects.

<a id='x-28MGL-PAX-3ACOLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-28T-29-29-29'></a>

- [method] **COLLECT-REACHABLE-OBJECTS** *OBJECT*

    This default implementation returns the empty list. This means that
    nothing is reachable from `OBJECT`.

<a id='x-28MGL-PAX-3ADOCUMENT-OBJECT-20GENERIC-FUNCTION-29'></a>

- [generic-function] **DOCUMENT-OBJECT** *OBJECT STREAM*

    Write `OBJECT` (and its references recursively) in
    `FORMAT` to `STREAM`.
    
    The [`DOCUMENT`][1eb8] function calls this generic function with `LEVEL` 0,
    passing `FORMAT` on. Add methods specializing on `OBJECT` to customize
    how objects of that type are presented in the documentation.

<a id='x-28MGL-PAX-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28STRING-20T-29-29-29'></a>

- [method] **DOCUMENT-OBJECT** *(STRING STRING) STREAM*

    Print `STRING` verbatim to `STREAM` after cleaning up indentation.
    
    Docstrings in sources are indented in various ways which can easily
    mess up markdown. To handle the most common cases leave the first
    line alone, but from the rest of the lines strip the longest run of
    leading spaces that is common to all non-blank lines.

<a id='x-28MGL-PAX-3AFIND-SOURCE-20GENERIC-FUNCTION-29'></a>

- [generic-function] **FIND-SOURCE** *OBJECT*

    Like `SWANK:FIND-DEFINITION-FOR-THING`, but this
    one is a generic function to be extensible. In fact, the default
    implementation simply defers to `SWANK:FIND-DEFINITION-FOR-THING`.
    This function is called by `LOCATE-DEFINITION-FOR-EMACS` which lies
    behind the `M-.` extension (see [Emacs Integration][df94]).
    
    If successful, the return value looks like this:
    
    ```commonlisp
    (:location (:file "/home/mega/own/mgl/pax/test/test.lisp")
               (:position 24) nil)
    ```
    
    The `NIL` is the source snippet which is optional. Note that position
    1 is the first character. If unsuccessful, the return values is
    like:
    
    ```commonlisp
    (:error "Unknown source location for SOMETHING")
    ```


<a id='x-28MGL-PAX-3A-40MGL-PAX-REFERENCE-BASED-EXTENSIONS-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 11.3 Reference Based Extensions

Let's see how to extend [`DOCUMENT`][1eb8] and `M-.` navigation if there is
no first class object to represent the thing of interest. Recall
that [`LOCATE`][b2be] returns a [`REFERENCE`][0d20] object in this case. [`DOCUMENT-OBJECT`][a05e]
and [`FIND-SOURCE`][b417] defer to [`LOCATE-AND-DOCUMENT`][6c17] and
[`LOCATE-AND-FIND-SOURCE`][e9e9], which have [`LOCATIVE-TYPE`][e79e] in their argument
list for `EQL` specializing pleasure. Here is a stripped down example
of how the [`VARIABLE`][2be0] locative is defined:

<a id='x-28MGL-PAX-3AVARIABLE-EXAMPLE-20-28MGL-PAX-3AINCLUDE-20-28-3ASTART-20-28VARIABLE-20MGL-PAX-3ALOCATIVE-29-20-3AEND-20-28MGL-PAX-3A-3AEND-OF-VARIABLE-EXAMPLE-20VARIABLE-29-29-20-3AHEADER-NL-20-22-60-60-60commonlisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29'></a>

```commonlisp
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

```

<a id='x-28MGL-PAX-3ACOLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-28MGL-PAX-MINIMAL-3AREFERENCE-29-29-29'></a>

- [method] **COLLECT-REACHABLE-OBJECTS** *(REFERENCE REFERENCE)*

    If `REFERENCE` can be resolved to a non-reference, call
    [`COLLECT-REACHABLE-OBJECTS`][1920] with it, else call
    [`LOCATE-AND-COLLECT-REACHABLE-OBJECTS`][7a11] on the object, locative-type,
    locative-args of `REFERENCE`

<a id='x-28MGL-PAX-3ALOCATE-AND-COLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29'></a>

- [generic-function] **LOCATE-AND-COLLECT-REACHABLE-OBJECTS** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    Called by [`COLLECT-REACHABLE-OBJECTS`][1920] on [`REFERENCE`][0d20]
    objects, this function has essentially the same purpose as its
    caller but it has different arguments to allow specializing on
    `LOCATIVE-TYPE`.

<a id='x-28MGL-PAX-3ALOCATE-AND-COLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-28T-20T-20T-29-29-29'></a>

- [method] **LOCATE-AND-COLLECT-REACHABLE-OBJECTS** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    This default implementation returns the empty list. This means that
    nothing is reachable from the reference.

<a id='x-28MGL-PAX-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28MGL-PAX-MINIMAL-3AREFERENCE-20T-29-29-29'></a>

- [method] **DOCUMENT-OBJECT** *(REFERENCE REFERENCE) STREAM*

    If `REFERENCE` can be resolved to a non-reference, call
    [`DOCUMENT-OBJECT`][a05e] with it, else call LOCATE-AND-DOCUMENT-OBJECT on the
    object, locative-type, locative-args of `REFERENCE`

<a id='x-28MGL-PAX-3ALOCATE-AND-DOCUMENT-20GENERIC-FUNCTION-29'></a>

- [generic-function] **LOCATE-AND-DOCUMENT** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS STREAM*

    Called by [`DOCUMENT-OBJECT`][a05e] on [`REFERENCE`][0d20] objects,
    this function has essentially the same purpose as [`DOCUMENT-OBJECT`][a05e]
    but it has different arguments to allow specializing on
    `LOCATIVE-TYPE`.

<a id='x-28MGL-PAX-3AFIND-SOURCE-20-28METHOD-20NIL-20-28MGL-PAX-MINIMAL-3AREFERENCE-29-29-29'></a>

- [method] **FIND-SOURCE** *(REFERENCE REFERENCE)*

    If `REFERENCE` can be resolved to a non-reference, call [`FIND-SOURCE`][b417]
    with it, else call [`LOCATE-AND-FIND-SOURCE`][e9e9] on the object,
    locative-type, locative-args of `REFERENCE`

<a id='x-28MGL-PAX-3ALOCATE-AND-FIND-SOURCE-20GENERIC-FUNCTION-29'></a>

- [generic-function] **LOCATE-AND-FIND-SOURCE** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    Called by [`FIND-SOURCE`][b417] on [`REFERENCE`][0d20] objects, this
    function has essentially the same purpose as [`FIND-SOURCE`][b417] but it has
    different arguments to allow specializing on `LOCATIVE-TYPE`.

<a id='x-28MGL-PAX-3ALOCATE-AND-FIND-SOURCE-20-28METHOD-20NIL-20-28T-20T-20T-29-29-29'></a>

- [method] **LOCATE-AND-FIND-SOURCE** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    This default implementation simply calls [`FIND-SOURCE`][b417] with `OBJECT`
    which should cover the common case of a macro expanding to, for
    instance, a defun but having its own locative type.

We have covered the basic building blocks of reference based
extensions. Now let's see how the obscure
[`DEFINE-SYMBOL-LOCATIVE-TYPE`][f2d1] and
[`DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE`][c0fa] macros work together to
simplify the common task of associating definition and documentation
with symbols in a certain context.

<a id='x-28MGL-PAX-3ADEFINE-SYMBOL-LOCATIVE-TYPE-20-28MGL-PAX-MINIMAL-3AMACRO-29-29'></a>

- [macro] **DEFINE-SYMBOL-LOCATIVE-TYPE** *LOCATIVE-TYPE LAMBDA-LIST &BODY DOCSTRING*

    Similar to [`DEFINE-LOCATIVE-TYPE`][2c45] but it assumes that all things
    locatable with `LOCATIVE-TYPE` are going to be just symbols defined
    with a definer defined with [`DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE`][c0fa].
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
    form above.

<a id='x-28MGL-PAX-3ADEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE-20-28MGL-PAX-MINIMAL-3AMACRO-29-29'></a>

- [macro] **DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE** *NAME LOCATIVE-TYPE &BODY DOCSTRING*

    Define a macro with `NAME` which can be used to attach documentation,
    a lambda-list and source location to a symbol in the context of
    `LOCATIVE-TYPE`. The defined macro's arglist is (`SYMBOL` `LAMBDA-LIST`
    `&OPTIONAL` `DOCSTRING`). `LOCATIVE-TYPE` is assumed to have been defined
    with [`DEFINE-SYMBOL-LOCATIVE-TYPE`][f2d1].

<a id='x-28MGL-PAX-3A-40MGL-PAX-SECTIONS-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 11.4 Sections

[`Section`][ba35] objects rarely need to be dissected since
[`DEFSECTION`][30c2] and [`DOCUMENT`][1eb8] cover most needs. However, it is plausible
that one wants to subclass them and maybe redefine how they are
presented.

<a id='x-28MGL-PAX-MINIMAL-3ASECTION-20CLASS-29'></a>

- [class] **SECTION**

    [`DEFSECTION`][30c2] stores its `NAME`, `TITLE`, [`PACKAGE`][a15b],
    `READTABLE` and `ENTRIES` in [`SECTION`][ba35] objects.

<a id='x-28MGL-PAX-MINIMAL-3ASECTION-NAME-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-MINIMAL-3ASECTION-29-29'></a>

- [reader] **SECTION-NAME** *SECTION* *(:NAME)*

    The name of the global variable whose value is
    this `SECTION`([`0`][ba35] [`1`][4fa7]) object.

<a id='x-28MGL-PAX-MINIMAL-3ASECTION-PACKAGE-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-MINIMAL-3ASECTION-29-29'></a>

- [reader] **SECTION-PACKAGE** *SECTION* *(:PACKAGE)*

    `*PACKAGE*` will be bound to this package when
    generating documentation for this section.

<a id='x-28MGL-PAX-MINIMAL-3ASECTION-READTABLE-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-MINIMAL-3ASECTION-29-29'></a>

- [reader] **SECTION-READTABLE** *SECTION* *(:READTABLE)*

    `*READTABLE*` will be bound to this when generating
    documentation for this section.

<a id='x-28MGL-PAX-MINIMAL-3ASECTION-TITLE-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-MINIMAL-3ASECTION-29-29'></a>

- [reader] **SECTION-TITLE** *SECTION* *(:TITLE)*

    `STRING` or `NIL`. Used in generated documentation.

<a id='x-28MGL-PAX-MINIMAL-3ASECTION-LINK-TITLE-TO-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-MINIMAL-3ASECTION-29-29'></a>

- [reader] **SECTION-LINK-TITLE-TO** *SECTION* *(:LINK-TITLE-TO = NIL)*

    A [`REFERENCE`][0d20] or `NIL`. Used in generated documentation.

<a id='x-28MGL-PAX-MINIMAL-3ASECTION-ENTRIES-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-MINIMAL-3ASECTION-29-29'></a>

- [reader] **SECTION-ENTRIES** *SECTION* *(:ENTRIES)*

    A list of strings and [`REFERENCE`][0d20] objects in the
    order they occurred in [`DEFSECTION`][30c2].

<a id='x-28DESCRIBE-OBJECT-20-28METHOD-20NIL-20-28MGL-PAX-MINIMAL-3ASECTION-20T-29-29-29'></a>

- [method] **DESCRIBE-OBJECT** *(SECTION SECTION) STREAM*

    [`SECTION`][ba35] objects are printed by calling [`DOCUMENT`][1eb8] on them
    with all [Documentation Printer Variables][63f8], except for
    [`*DOCUMENT-NORMALIZE-PACKAGES*`][ada7], turned off to reduce clutter.

<a id='x-28MGL-PAX-3A-40MGL-PAX-TRANSCRIPT-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 12 Transcripts

What are transcripts for? When writing a tutorial, one often wants
to include a REPL session with maybe a few defuns and a couple of
forms whose output or return values are shown. Also, in a function's
docstring an example call with concrete arguments and return values
speaks volumes. A transcript is a text that looks like a repl
session, but which has a light markup for printed output and return
values, while no markup (i.e. prompt) for lisp forms. The `PAX`
transcripts may include output and return values of all forms, or
only selected ones. In either case the transcript itself can be
easily generated from the source code.

The main worry associated with including examples in the
documentation is that they tend to get out-of-sync with the code.
This is solved by being able to parse back and update transcripts.
In fact, this is exactly what happens during documentation
generation with `PAX`. Code sections tagged `cl-transcript` are
retranscribed and checked for inconsistency (that is, any difference
in output or return values). If the consistency check fails, an
error is signalled that includes a reference to the object being
documented.

Going beyond documentation, transcript consistency checks can be
used for writing simple tests in a very readable form. For example:

```cl-transcript
(+ 1 2)
=> 3

(values (princ :hello) (list 1 2))
.. HELLO
=> :HELLO
=> (1 2)

```

All in all, transcripts are a handy tool especially when combined
with the Emacs support to regenerate them and with
`PYTHONIC-STRING-READER` and its triple-quoted strings that allow one
to work with nested strings with less noise. The triple-quote syntax
can be enabled with:

    (in-readtable pythonic-string-syntax)


<a id='x-28MGL-PAX-3A-40MGL-PAX-TRANSCRIPT-EMACS-INTEGRATION-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 12.1 Transcribing with Emacs

Typical transcript usage from within Emacs is simple: add a lisp
form to a docstring or comment at any indentation level. Move the
cursor right after the end of the form as if you were to evaluate it
with `C-x C-e`. The cursor is marked by `#\^`:

    This is part of a docstring.
    
    ```cl-transcript
    (values (princ :hello) (list 1 2))^
    ```

Note that the use of fenced code blocks with the language tag
`cl-transcript` is only to tell `PAX` to perform consistency checks at
documentation generation time.

Now invoke the elisp function `mgl-pax-transcribe` where the cursor
is and the fenced code block from the docstring becomes:

    (values (princ :hello) (list 1 2))
    .. HELLO
    => :HELLO
    => (1 2)
    ^

Then you change the printed message and add a comment to the second
return value:

    (values (princ :hello-world) (list 1 2))
    .. HELLO
    => :HELLO
    => (1
        ;; This value is arbitrary.
        2)

When generating the documentation you get a
[`TRANSCRIPTION-CONSISTENCY-ERROR`][5a2c] because the printed output and the
first return value changed so you regenerate the documentation by
marking the region of bounded by `#\|` and the cursor at `#\^` in
the example:

    |(values (princ :hello-world) (list 1 2))
    .. HELLO
    => :HELLO
    => (1
        ;; This value is arbitrary.
        2)
    ^

then invoke the elisp function `mgl-pax-retranscribe-region` to get:

    (values (princ :hello-world) (list 1 2))
    .. HELLO-WORLD
    => :HELLO-WORLD
    => (1
        ;; This value is arbitrary.
        2)
    ^

Note how the indentation and the comment of `(1 2)` was left alone
but the output and the first return value got updated.

Alternatively, `C-u 1 mgl-pax-transcribe` will emit commented markup:

    (values (princ :hello) (list 1 2))
    ;.. HELLO
    ;=> :HELLO
    ;=> (1 2)

`C-u 0 mgl-pax-retranscribe-region` will turn commented into
non-commented markup. In general, the numeric prefix argument is the
index of the syntax to be used in [`MGL-PAX`][65f5]:[`*SYNTAXES*`][6a46]. Without a
prefix argument `mgl-pax-retranscribe-region` will not change the
markup style.

Finally, not only do both functions work at any indentation level,
but in comments too:

    ;;;; (values (princ :hello) (list 1 2))
    ;;;; .. HELLO
    ;;;; => :HELLO
    ;;;; => (1 2)

Transcription support in emacs can be enabled by adding this to your
Emacs initialization file (or loading `src/transcribe.el`):

<a id='x-28MGL-PAX-3A-3ATRANSCRIBE-2EEL-20-28MGL-PAX-3AINCLUDE-20-23P-22-2Fhome-2Frunner-2Fwork-2Fmgl-pax-2Fmgl-pax-2Fsrc-2Ftranscribe-2Eel-22-20-3AHEADER-NL-20-22-60-60-60elisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29'></a>

```elisp
;;; MGL-PAX transcription

(defun mgl-pax-transcribe-last-expression ()
  "A bit like C-u C-x C-e (slime-eval-last-expression) that
inserts the output and values of the sexp before the point, this
does the same but with MGL-PAX:TRANSCRIBE. Use a numeric prefix
argument as in index to select one of the Common Lisp
MGL-PAX:*SYNTAXES* as the SYNTAX argument to MGL-PAX:TRANSCRIBE.
Without a prefix argument, the first syntax is used."
  (interactive)
  (insert
   (save-excursion
     (let* ((end (point))
            (start (progn (backward-sexp)
                          (move-beginning-of-line nil)
                          (point))))
       (mgl-pax-transcribe start end (mgl-pax-transcribe-syntax-arg)
                           nil nil nil)))))

(defun mgl-pax-retranscribe-region (start end)
  "Updates the transcription in the current region (as in calling
MGL-PAX:TRANSCRIBE with :UPDATE-ONLY T). Use a numeric prefix
argument as in index to select one of the Common Lisp
MGL-PAX:*SYNTAXES* as the SYNTAX argument to MGL-PAX:TRANSCRIBE.
Without a prefix argument, the syntax of the input will not be
changed."
  (interactive "r")
  (let* ((point-at-start-p (= (point) start))
         (point-at-end-p (= (point) end))
         (transcript (mgl-pax-transcribe start end
                                         (mgl-pax-transcribe-syntax-arg)
                                         t t nil)))
    (if point-at-start-p
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (insert transcript))
      (save-excursion
          (goto-char start)
          (delete-region start end))
      (insert transcript))))

(defun mgl-pax-transcribe-syntax-arg ()
  (if current-prefix-arg
      (prefix-numeric-value current-prefix-arg)
    nil))

(defun mgl-pax-transcribe (start end syntax update-only echo
                                 first-line-special-p)
  (let ((transcription
         (slime-eval
          `(cl:if (cl:find-package :mgl-pax)
                  (cl:funcall
                   (cl:find-symbol
                    (cl:symbol-name :transcribe-for-emacs) :mgl-pax)
                   ,(buffer-substring-no-properties start end)
                   ',syntax ',update-only ',echo ',first-line-special-p)
                  t))))
    (if (eq transcription t)
        (error "MGL-PAX is not loaded.")
      transcription)))
```

<a id='x-28MGL-PAX-3A-40MGL-PAX-TRANSCRIPT-API-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 12.2 Transcript API

<a id='x-28MGL-PAX-3ATRANSCRIBE-20FUNCTION-29'></a>

- [function] **TRANSCRIBE** *INPUT OUTPUT &KEY UPDATE-ONLY (INCLUDE-NO-OUTPUT UPDATE-ONLY) (INCLUDE-NO-VALUE UPDATE-ONLY) (ECHO T) CHECK-CONSISTENCY DEFAULT-SYNTAX (INPUT-SYNTAXES \*SYNTAXES\*) (OUTPUT-SYNTAXES \*SYNTAXES\*)*

    Read forms from `INPUT` and write them (iff `ECHO`) to `OUTPUT`
    followed by any output and return values produced by calling `EVAL` on
    the form. `INPUT` can be a stream or a string, while `OUTPUT` can be a
    stream or `NIL` in which case transcription goes into a string. The
    return value is the `OUTPUT` stream or the string that was
    constructed.
    
    A simple example is this:
    
    ```cl-transcript
    (transcribe "(princ 42) " nil)
    => "(princ 42)
    .. 42
    => 42
    "
    
    ```
    
    However, the above may be a bit confusing since this documentation
    uses [`TRANSCRIBE`][0382] markup syntax in this very example, so let's do it
    differently. If we have a file with these contents:
    
    ```commonlisp
    (values (princ 42) (list 1 2))
    ```
    
    it is transcribed to:
    
    ```commonlisp
    (values (princ 42) (list 1 2))
    .. 42
    => 42
    => (1 2)
    ```
    
    Output to all standard streams is captured and printed with
    the `:OUTPUT` prefix (`".."`). The return values above are printed
    with the `:READABLE` prefix (`"=>"`). Note how these prefixes are
    always printed on a new line to facilitate parsing.
    
    **Updating**
    
    [`TRANSCRIBE`][0382] is able to parse its own output. If we transcribe the
    previous output above, we get it back exactly. However, if we remove
    all output markers, leave only a placeholder value marker and
    pass `:UPDATE-ONLY` `T` with source:
    
    ```commonlisp
    (values (princ 42) (list 1 2))
    =>
    ```
    
    we get this:
    
    ```commonlisp
    (values (princ 42) (list 1 2))
    => 42
    => (1 2)
    ```
    
    With `UPDATE-ONLY`, printed output of a form is only transcribed if
    there were output markers in the source. Similarly, with
    `UPDATE-ONLY`, return values are only transcribed if there were value
    markers in the source.
    
    **No Output/Values**
    
    If the form produces no output or returns no values, then whether or
    not output and values are transcribed is controlled by
    `INCLUDE-NO-OUTPUT` and `INCLUDE-NO-VALUE`, respectively. By default,
    neither is on so:
    
    ```commonlisp
    (values)
    ..
    =>
    ```
    
    is transcribed to
    
    ```commonlisp
    (values)
    ```
    
    With `UPDATE-ONLY` true, we probably wouldn't like to lose those
    markers since they were put there for a reason. Hence, with
    `UPDATE-ONLY`, `INCLUDE-NO-OUTPUT` and `INCLUDE-NO-VALUE` default to true.
    So with `UPDATE-ONLY` the above example is transcribed to:
    
    ```commonlisp
    (values)
    ..
    => ; No value
    ```
    
    where the last line is the `:NO-VALUE` prefix.
    
    **Consistency Checks**
    
    If `CHECK-CONSISTENCY` is true, then [`TRANSCRIBE`][0382] signals a continuable
    [`TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR`][8a71] whenever a form's output as a
    string is different from what was in `INPUT`, provided that `INPUT`
    contained the output. Similary, for values, a continuable
    [`TRANSCRIPTION-VALUES-CONSISTENCY-ERROR`][a73e] is signalled if a value read
    from the source does not print as the as the value returned by `EVAL`.
    This allows readable values to be hand-indented without failing
    consistency checks:
    
    ```commonlisp
    (list 1 2)
    => (1
          2)
    ```
    
    **Unreadable Values**
    
    The above scheme involves `READ`, so consistency of unreadable values
    cannot be treated the same. In fact, unreadable values must even be
    printed differently for transcribe to be able to read them back:
    
    ```commonlisp
    (defclass some-class () ())
    
    (defmethod print-object ((obj some-class) stream)
      (print-unreadable-object (obj stream :type t)
        (format stream \"~%~%end\")))
    
    (make-instance 'some-class)
    ==> #<SOME-CLASS 
    -->
    --> end>
    ```
    
    where `"==>"` is the `:UNREADABLE` prefix and `"-->"` is
    the `:UNREADABLE-CONTINUATION` prefix. As with outputs, a consistency
    check between an unreadable value from the source and the value from
    `EVAL` is performed with `STRING=`. That is, the value from `EVAL` is
    printed to a string and compared to the source value. Hence, any
    change to unreadable values will break consistency checks. This is
    most troublesome with instances of classes with the default
    `PRINT-OBJECT` method printing the memory address. There is currently
    no remedy for that, except for customizing `PRINT-OBJECT` or not
    transcribing that kind of stuff.
    
    **Syntaxes**
    
    Finally, a transcript may employ different syntaxes for the output
    and values of different forms. When `INPUT` is read, the syntax for
    each form is determined by trying to match all prefixes from all
    syntaxes in `INPUT-SYNTAXES` against a line. If there are no output or
    values for a form in `INPUT`, then the syntax remains undetermined.
    
    When `OUTPUT` is written, the prefixes to be used are looked up in
    `DEFAULT-SYNTAX` of `OUTPUT-SYNTAXES`, if `DEFAULT-SYNTAX` is not `NIL`. If
    `DEFAULT-SYNTAX` is `NIL`, then the syntax used by the same form in the
    `INPUT` is used or (if that could not be determined) the syntax of the
    previous form. If there was no previous form, then the first syntax
    if `OUTPUT-SYNTAXES` is used.
    
    To produce a transcript that's executable Lisp code,
    use `:DEFAULT-SYNTAX` `:COMMENTED-1:`
    
    ```commonlisp
    (make-instance 'some-class)
    ;==> #<SOME-CLASS
    ;-->
    ;--> end>
    
    (list 1 2)
    ;=> (1
    ;->    2)
    ```
    
    To translate the above to uncommented syntax,
    use `:DEFAULT-SYNTAX` `:DEFAULT`. If `DEFAULT-SYNTAX` is `NIL` (the
    default), the same syntax will be used in the output as in the input
    as much as possible.

<a id='x-28MGL-PAX-3A-2ASYNTAXES-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*SYNTAXES\*** *((:DEFAULT (:OUTPUT "..") (:NO-VALUE "=> ; No value") (:READABLE "=>")
  (:UNREADABLE "==>") (:UNREADABLE-CONTINUATION "-->"))
 (:COMMENTED-1 (:OUTPUT ";..") (:NO-VALUE ";=> ; No value") (:READABLE ";=>")
  (:READABLE-CONTINUATION ";->") (:UNREADABLE ";==>")
  (:UNREADABLE-CONTINUATION ";-->"))
 (:COMMENTED-2 (:OUTPUT ";;..") (:NO-VALUE ";;=> ; No value")
  (:READABLE ";;=>") (:READABLE-CONTINUATION ";;->") (:UNREADABLE ";;==>")
  (:UNREADABLE-CONTINUATION ";;-->")))*

    The default syntaxes used by [`TRANSCRIBE`][0382] for reading and writing
    lines containing output and values of an evaluated form.
    
    A syntax is a list of of the form `(SYNTAX-ID &REST PREFIXES)` where
    `PREFIXES` is a list of `(PREFIX-ID PREFIX-STRING)` elements. For
    example the syntax `:COMMENTED-1` looks like this:
    
    ```commonlisp
    (:commented-1
     (:output ";..")
     (:no-value ";=>  No value")
     (:readable ";=>")
     (:readable-continuation ";->")
     (:unreadable ";==>")
     (:unreadable-continuation ";-->"))
    ```
    
    All of the above prefixes must be defined for every syntax except
    for `:READABLE-CONTINUATION`. If that's missing (as in the `:DEFAULT`
    syntax), then the following value is read with `READ` and printed with
    `PRIN1` (hence no need to mark up the following lines).
    
    When writing, an extra space is added automatically if the line to
    be prefixed is not empty. Similarly, the first space following the
    prefix discarded when reading.
    
    See [`TRANSCRIBE`][0382] for how the actual syntax to be used is selected.

<a id='x-28MGL-PAX-3ATRANSCRIPTION-ERROR-20CONDITION-29'></a>

- [condition] **TRANSCRIPTION-ERROR** *ERROR*

    Represents syntactic errors in the `SOURCE` argument
    of [`TRANSCRIBE`][0382] and also serves as the superclass of
    [`TRANSCRIPTION-CONSISTENCY-ERROR`][5a2c].

<a id='x-28MGL-PAX-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29'></a>

- [condition] **TRANSCRIPTION-CONSISTENCY-ERROR** *TRANSCRIPTION-ERROR*

    A common superclass for
    [`TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR`][8a71] and
    [`TRANSCRIPTION-VALUES-CONSISTENCY-ERROR`][a73e].

<a id='x-28MGL-PAX-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29'></a>

- [condition] **TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR** *TRANSCRIPTION-CONSISTENCY-ERROR*

    Signaled (with `CERROR`) by [`TRANSCRIBE`][0382] when invoked
    with `:CHECK-CONSISTENCY` and the output of a form is not the same as
    what was parsed.

<a id='x-28MGL-PAX-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29'></a>

- [condition] **TRANSCRIPTION-VALUES-CONSISTENCY-ERROR** *TRANSCRIPTION-CONSISTENCY-ERROR*

    Signaled (with `CERROR`) by [`TRANSCRIBE`][0382] when invoked
    with `:CHECK-CONSISTENCY` and the values of a form are inconsistent
    with their parsed representation.

  [0208]: #x-28CLASS-20-28MGL-PAX-3ALOCATIVE-29-29 "(CLASS (MGL-PAX:LOCATIVE))"
  [0382]: #x-28MGL-PAX-3ATRANSCRIBE-20FUNCTION-29 "(MGL-PAX:TRANSCRIBE FUNCTION)"
  [07a3]: #x-28MGL-PAX-MINIMAL-3ASECTION-PACKAGE-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-MINIMAL-3ASECTION-29-29 "(MGL-PAX-MINIMAL:SECTION-PACKAGE (MGL-PAX-MINIMAL:READER MGL-PAX-MINIMAL:SECTION))"
  [0d20]: #x-28MGL-PAX-MINIMAL-3AREFERENCE-20CLASS-29 "(MGL-PAX-MINIMAL:REFERENCE CLASS)"
  [1095]: #x-28MGL-PAX-3A-40MGL-PAX-TRANSCRIPT-20MGL-PAX-MINIMAL-3ASECTION-29 "Transcripts"
  [12a1]: #x-28MGL-PAX-3ALOCATIVE-20-28MGL-PAX-3ALOCATIVE-29-29 "(MGL-PAX:LOCATIVE (MGL-PAX:LOCATIVE))"
  [1576]: #x-28MGL-PAX-3A-40MGL-PAX-MATHJAX-20MGL-PAX-MINIMAL-3ASECTION-29 "MathJax"
  [1722]: #x-28MGL-PAX-3A-40MGL-PAX-BACKGROUND-20MGL-PAX-MINIMAL-3ASECTION-29 "Background"
  [1920]: #x-28MGL-PAX-3ACOLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29 "(MGL-PAX:COLLECT-REACHABLE-OBJECTS GENERIC-FUNCTION)"
  [1cc6]: #x-28MGL-PAX-3AMAKE-GITHUB-SOURCE-URI-FN-20FUNCTION-29 "(MGL-PAX:MAKE-GITHUB-SOURCE-URI-FN FUNCTION)"
  [1d4c]: #x-28MGL-PAX-3A-2ADOCUMENT-LINK-CODE-2A-20-28VARIABLE-29-29 "(MGL-PAX:*DOCUMENT-LINK-CODE* (VARIABLE))"
  [1ea3]: #x-28MGL-PAX-3A-40MGL-PAX-MARKDOWN-SUPPORT-20MGL-PAX-MINIMAL-3ASECTION-29 "Markdown Support"
  [1eb8]: #x-28MGL-PAX-3ADOCUMENT-20FUNCTION-29 "(MGL-PAX:DOCUMENT FUNCTION)"
  [1f1e]: #x-28MGL-PAX-MINIMAL-3AREFERENCE-OBJECT-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-MINIMAL-3AREFERENCE-29-29 "(MGL-PAX-MINIMAL:REFERENCE-OBJECT (MGL-PAX-MINIMAL:READER MGL-PAX-MINIMAL:REFERENCE))"
  [2285]: #x-28MGL-PAX-3ALOCATE-ERROR-20CONDITION-29 "(MGL-PAX:LOCATE-ERROR CONDITION)"
  [24fc]: #x-28MGL-PAX-3ACANONICAL-REFERENCE-20GENERIC-FUNCTION-29 "(MGL-PAX:CANONICAL-REFERENCE GENERIC-FUNCTION)"
  [283b]: #x-28MGL-PAX-3A-40MGL-PAX-GITHUB-WORKFLOW-20MGL-PAX-MINIMAL-3ASECTION-29 "Github Workflow"
  [2be0]: #x-28VARIABLE-20-28MGL-PAX-3ALOCATIVE-29-29 "(VARIABLE (MGL-PAX:LOCATIVE))"
  [2c45]: #x-28MGL-PAX-3ADEFINE-LOCATIVE-TYPE-20-28MGL-PAX-MINIMAL-3AMACRO-29-29 "(MGL-PAX:DEFINE-LOCATIVE-TYPE (MGL-PAX-MINIMAL:MACRO))"
  [2e7a]: #x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-READMES-20FUNCTION-29 "(MGL-PAX:UPDATE-ASDF-SYSTEM-READMES FUNCTION)"
  [30c2]: #x-28MGL-PAX-MINIMAL-3ADEFSECTION-20-28MGL-PAX-MINIMAL-3AMACRO-29-29 "(MGL-PAX-MINIMAL:DEFSECTION (MGL-PAX-MINIMAL:MACRO))"
  [424c]: #x-28MGL-PAX-3A-40MGL-PAX-MARKDOWN-INDENTATION-20MGL-PAX-MINIMAL-3ASECTION-29 "Indentation"
  [46ea]: #x-28MGL-PAX-3A-2ADOCUMENT-MARK-UP-SIGNATURES-2A-20-28VARIABLE-29-29 "(MGL-PAX:*DOCUMENT-MARK-UP-SIGNATURES* (VARIABLE))"
  [4888]: #x-28MGL-PAX-MINIMAL-3AREFERENCE-LOCATIVE-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-MINIMAL-3AREFERENCE-29-29 "(MGL-PAX-MINIMAL:REFERENCE-LOCATIVE (MGL-PAX-MINIMAL:READER MGL-PAX-MINIMAL:REFERENCE))"
  [4f82]: #x-28MGL-PAX-3AUPDATE-PAX-WORLD-20FUNCTION-29 "(MGL-PAX:UPDATE-PAX-WORLD FUNCTION)"
  [4fa7]: #x-28MGL-PAX-MINIMAL-3ASECTION-20-28MGL-PAX-3ALOCATIVE-29-29 "(MGL-PAX-MINIMAL:SECTION (MGL-PAX:LOCATIVE))"
  [5a2c]: #x-28MGL-PAX-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29 "(MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR CONDITION)"
  [5db6]: #x-28MGL-PAX-3A-40MGL-PAX-SECTIONS-20MGL-PAX-MINIMAL-3ASECTION-29 "Sections"
  [63f8]: #x-28MGL-PAX-3A-40MGL-PAX-DOCUMENTATION-PRINTER-VARIABLES-20MGL-PAX-MINIMAL-3ASECTION-29 "Documentation Printer Variables"
  [649c]: #x-28MGL-PAX-3A-40MGL-PAX-LINKS-20MGL-PAX-MINIMAL-3ASECTION-29 "Links"
  [65f5]: #x-28-23A-28-287-29-20BASE-CHAR-20-2E-20-22mgl-pax-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29 "(#A((7) BASE-CHAR . \"mgl-pax\") ASDF/SYSTEM:SYSTEM)"
  [6a46]: #x-28MGL-PAX-3A-2ASYNTAXES-2A-20-28VARIABLE-29-29 "(MGL-PAX:*SYNTAXES* (VARIABLE))"
  [6b15]: #x-28FUNCTION-20-28MGL-PAX-3ALOCATIVE-29-29 "(FUNCTION (MGL-PAX:LOCATIVE))"
  [6c17]: #x-28MGL-PAX-3ALOCATE-AND-DOCUMENT-20GENERIC-FUNCTION-29 "(MGL-PAX:LOCATE-AND-DOCUMENT GENERIC-FUNCTION)"
  [6e6e]: #x-28MGL-PAX-MINIMAL-3ASECTION-ENTRIES-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-MINIMAL-3ASECTION-29-29 "(MGL-PAX-MINIMAL:SECTION-ENTRIES (MGL-PAX-MINIMAL:READER MGL-PAX-MINIMAL:SECTION))"
  [6fb5]: #x-28MGL-PAX-3A-40MGL-PAX-LOCATIVES-AND-REFERENCES-20MGL-PAX-MINIMAL-3ASECTION-29 "Locatives and References"
  [7078]: #x-28MGL-PAX-3ADISLOCATED-20-28MGL-PAX-3ALOCATIVE-29-29 "(MGL-PAX:DISLOCATED (MGL-PAX:LOCATIVE))"
  [75cb]: #x-28MGL-PAX-3A-40MGL-PAX-TUTORIAL-20MGL-PAX-MINIMAL-3ASECTION-29 "Tutorial"
  [7a11]: #x-28MGL-PAX-3ALOCATE-AND-COLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29 "(MGL-PAX:LOCATE-AND-COLLECT-REACHABLE-OBJECTS GENERIC-FUNCTION)"
  [7dfd]: #x-28MGL-PAX-3A-40MGL-PAX-TRANSCRIPT-API-20MGL-PAX-MINIMAL-3ASECTION-29 "Transcript API"
  [8a71]: #x-28MGL-PAX-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29 "(MGL-PAX:TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR CONDITION)"
  [9835]: #x-28MGL-PAX-3A-40MGL-PAX-EXTENSION-API-20MGL-PAX-MINIMAL-3ASECTION-29 "Extension API"
  [9f28]: #x-28MGL-PAX-3A-40MGL-PAX-GENERATING-DOCUMENTATION-20MGL-PAX-MINIMAL-3ASECTION-29 "Generating Documentation"
  [a05e]: #x-28MGL-PAX-3ADOCUMENT-OBJECT-20GENERIC-FUNCTION-29 "(MGL-PAX:DOCUMENT-OBJECT GENERIC-FUNCTION)"
  [a15b]: #x-28PACKAGE-20-28MGL-PAX-3ALOCATIVE-29-29 "(PACKAGE (MGL-PAX:LOCATIVE))"
  [a282]: #x-28MGL-PAX-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20-28VARIABLE-29-29 "(MGL-PAX:*DOCUMENT-UPPERCASE-IS-CODE* (VARIABLE))"
  [a73e]: #x-28MGL-PAX-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29 "(MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR CONDITION)"
  [a7c9]: #x-28DESCRIBE-OBJECT-20-28METHOD-20NIL-20-28MGL-PAX-MINIMAL-3ASECTION-20T-29-29-29 "(DESCRIBE-OBJECT (METHOD NIL (MGL-PAX-MINIMAL:SECTION T)))"
  [a86f]: #x-28MGL-PAX-3A-40MGL-PAX-WORLD-20MGL-PAX-MINIMAL-3ASECTION-29 "PAX World"
  [acc9]: #x-28MGL-PAX-3ALOCATE-OBJECT-20GENERIC-FUNCTION-29 "(MGL-PAX:LOCATE-OBJECT GENERIC-FUNCTION)"
  [ada7]: #x-28MGL-PAX-3A-2ADOCUMENT-NORMALIZE-PACKAGES-2A-20-28VARIABLE-29-29 "(MGL-PAX:*DOCUMENT-NORMALIZE-PACKAGES* (VARIABLE))"
  [af3c]: #x-28CONDITION-20-28MGL-PAX-3ALOCATIVE-29-29 "(CONDITION (MGL-PAX:LOCATIVE))"
  [b2be]: #x-28MGL-PAX-3ALOCATE-20FUNCTION-29 "(MGL-PAX:LOCATE FUNCTION)"
  [b417]: #x-28MGL-PAX-3AFIND-SOURCE-20GENERIC-FUNCTION-29 "(MGL-PAX:FIND-SOURCE GENERIC-FUNCTION)"
  [b4d2]: #x-28MGL-PAX-3ALOCATE-ERROR-MESSAGE-20-28MGL-PAX-MINIMAL-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29 "(MGL-PAX:LOCATE-ERROR-MESSAGE (MGL-PAX-MINIMAL:READER MGL-PAX:LOCATE-ERROR))"
  [b56d]: #x-28MGL-PAX-3APAX-2EEL-20-28MGL-PAX-3AINCLUDE-20-23P-22-2Fhome-2Frunner-2Fwork-2Fmgl-pax-2Fmgl-pax-2Fsrc-2Fpax-2Eel-22-20-3AHEADER-NL-20-22-60-60-60elisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29 "(MGL-PAX:PAX.EL (MGL-PAX:INCLUDE #P\"/home/runner/work/mgl-pax/mgl-pax/src/pax.el\" :HEADER-NL \"```elisp\" :FOOTER-NL \"```\"))"
  [ba35]: #x-28MGL-PAX-MINIMAL-3ASECTION-20CLASS-29 "(MGL-PAX-MINIMAL:SECTION CLASS)"
  [ba5d]: #x-28RESTART-20-28MGL-PAX-3ALOCATIVE-29-29 "(RESTART (MGL-PAX:LOCATIVE))"
  [bbc9]: #x-28MGL-PAX-3A-40MGL-PAX-MARKDOWN-SYNTAX-HIGHLIGHTING-20MGL-PAX-MINIMAL-3ASECTION-29 "Syntax highlighting"
  [be7f]: #x-28MGL-PAX-3A-2ADOCUMENT-HTML-TOP-BLOCKS-OF-LINKS-2A-20-28VARIABLE-29-29 "(MGL-PAX:*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS* (VARIABLE))"
  [bf8a]: #x-28ASDF-2FSYSTEM-3ASYSTEM-20-28MGL-PAX-3ALOCATIVE-29-29 "(ASDF/SYSTEM:SYSTEM (MGL-PAX:LOCATIVE))"
  [c0fa]: #x-28MGL-PAX-3ADEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE-20-28MGL-PAX-MINIMAL-3AMACRO-29-29 "(MGL-PAX:DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE (MGL-PAX-MINIMAL:MACRO))"
  [c3d1]: #x-28MGL-PAX-3A-40MGL-PAX-BASICS-20MGL-PAX-MINIMAL-3ASECTION-29 "Basics"
  [c5f2]: #x-28MGL-PAX-3AREGISTER-DOC-IN-PAX-WORLD-20FUNCTION-29 "(MGL-PAX:REGISTER-DOC-IN-PAX-WORLD FUNCTION)"
  [cb09]: #x-28MGL-PAX-3ADEFINE-GLOSSARY-TERM-20-28MGL-PAX-MINIMAL-3AMACRO-29-29 "(MGL-PAX:DEFINE-GLOSSARY-TERM (MGL-PAX-MINIMAL:MACRO))"
  [cb37]: #x-28MGL-PAX-MINIMAL-3ADEFINE-PACKAGE-20-28MGL-PAX-MINIMAL-3AMACRO-29-29 "(MGL-PAX-MINIMAL:DEFINE-PACKAGE (MGL-PAX-MINIMAL:MACRO))"
  [d5ec]: #x-28MGL-PAX-MINIMAL-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29 "(MGL-PAX-MINIMAL:EXPORTABLE-LOCATIVE-TYPE-P GENERIC-FUNCTION)"
  [d7eb]: #x-28MGL-PAX-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28STRING-20T-29-29-29 "(MGL-PAX:DOCUMENT-OBJECT (METHOD NIL (STRING T)))"
  [df94]: #x-28MGL-PAX-3A-40MGL-PAX-EMACS-INTEGRATION-20MGL-PAX-MINIMAL-3ASECTION-29 "Emacs Integration"
  [e0d7]: #x-28MGL-PAX-3ARESOLVE-20FUNCTION-29 "(MGL-PAX:RESOLVE FUNCTION)"
  [e41b]: #x-28MGL-PAX-3A-40MGL-PAX-LOCATIVE-TYPES-20MGL-PAX-MINIMAL-3ASECTION-29 "Locative Types"
  [e79e]: #x-28MGL-PAX-MINIMAL-3ALOCATIVE-TYPE-20FUNCTION-29 "(MGL-PAX-MINIMAL:LOCATIVE-TYPE FUNCTION)"
  [e9e9]: #x-28MGL-PAX-3ALOCATE-AND-FIND-SOURCE-20GENERIC-FUNCTION-29 "(MGL-PAX:LOCATE-AND-FIND-SOURCE GENERIC-FUNCTION)"
  [eb74]: #x-28MGL-PAX-MINIMAL-3A-3A-2ADISCARD-DOCUMENTATION-P-2A-20-28VARIABLE-29-29 "(MGL-PAX-MINIMAL::*DISCARD-DOCUMENTATION-P* (VARIABLE))"
  [ee12]: #x-28MGL-PAX-3A-40MGL-PAX-REFERENCE-BASED-EXTENSIONS-20MGL-PAX-MINIMAL-3ASECTION-29 "Reference Based Extensions"
  [f011]: #x-28MGL-PAX-3A-40MGL-PAX-TRANSCRIPT-EMACS-INTEGRATION-20MGL-PAX-MINIMAL-3ASECTION-29 "Transcribing with Emacs"
  [f1a0]: #x-28METHOD-20-28MGL-PAX-3ALOCATIVE-29-29 "(METHOD (MGL-PAX:LOCATIVE))"
  [f2d1]: #x-28MGL-PAX-3ADEFINE-SYMBOL-LOCATIVE-TYPE-20-28MGL-PAX-MINIMAL-3AMACRO-29-29 "(MGL-PAX:DEFINE-SYMBOL-LOCATIVE-TYPE (MGL-PAX-MINIMAL:MACRO))"
  [f3b7]: #x-28MGL-PAX-3ALOCATE-ERROR-20FUNCTION-29 "(MGL-PAX:LOCATE-ERROR FUNCTION)"
  [f414]: #x-28MGL-PAX-3A-40MGL-PAX-NEW-OBJECT-TYPES-20MGL-PAX-MINIMAL-3ASECTION-29 "Adding New Object Types"
  [f901]: #x-28MGL-PAX-3A-2ADOCUMENT-LINK-SECTIONS-2A-20-28VARIABLE-29-29 "(MGL-PAX:*DOCUMENT-LINK-SECTIONS* (VARIABLE))"
  [fde0]: #x-28MGL-PAX-3A-2ADOCUMENT-MAX-NUMBERING-LEVEL-2A-20-28VARIABLE-29-29 "(MGL-PAX:*DOCUMENT-MAX-NUMBERING-LEVEL* (VARIABLE))"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
