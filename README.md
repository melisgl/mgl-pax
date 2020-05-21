<a id='x-28MGL-PAX-3A-40MGL-PAX-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# PAX Manual

## Table of Contents

- [1 mgl-pax ASDF System Details][4918]
- [2 Links][d7e0]
- [3 Background][84ee]
- [4 Tutorial][aa52]
- [5 Emacs Integration][eff4]
- [6 Basics][8059]
- [7 Generating Documentation][063a]
    - [7.1 Github Workflow][2748]
    - [7.2 PAX World][e65c]
- [8 Markdown Support][d58f]
    - [8.1 Indentation][4336]
    - [8.2 Syntax highlighting][32ac]
    - [8.3 MathJax][55dd]
- [9 Documentation Printer Variables][e2a1]
- [10 Locative Types][1fbb]
- [11 Extension API][8ed9]
    - [11.1 Locatives and References][d023]
    - [11.2 Adding New Object Types][5161]
    - [11.3 Reference Based Extensions][00f0]
    - [11.4 Sections][be22]
- [12 Transcripts][7a32]
    - [12.1 Transcribing with Emacs][c694]
    - [12.2 Transcript API][bf16]

###### \[in package MGL-PAX\]
<a id='x-28-22mgl-pax-22-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

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

<a id='x-28MGL-PAX-3A-40MGL-PAX-LINKS-20MGL-PAX-3ASECTION-29'></a>

## 2 Links

Here is the [official
repository](https://github.com/melisgl/mgl-pax) and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/mgl-pax-manual.html)
for the latest version.

<a id='x-28MGL-PAX-3A-40MGL-PAX-BACKGROUND-20MGL-PAX-3ASECTION-29'></a>

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

Armed with [`defsection`][b1e7], I soon found myself organizing code following
the flow of user level documentation and relegated comments to
implementational details entirely. However, some portions of
[`defsection`][b1e7] docstrings were just listings of all the functions,
macros and variables related to the narrative, and this list was
effectively repeated in the `defpackage` form complete with little
comments that were like section names. A clear violation of
[OAOO][oaoo], one of them had to go, so [`defsection`][b1e7] got a list of
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
were a [`section`][53a8] locative. Going down that path, I soon began to feel
the urge to generate pretty documentation as all the necessary
information was manifest in the [`defsection`][b1e7] forms. The design
constraint imposed on documentation generation was that following
the typical style of upcasing symbols in docstrings there should be
no need to explicitly mark up links: if `M-.` works, then the
documentation generator shall also be able find out what's being
referred to.

I settled on [Markdown][markdown] as a reasonably non-intrusive
format, and a few thousand lines later PAX was born.

[markdown]: https://daringfireball.net/projects/markdown/ 


<a id='x-28MGL-PAX-3A-40MGL-PAX-TUTORIAL-20MGL-PAX-3ASECTION-29'></a>

## 4 Tutorial

PAX provides an extremely poor man's Explorable Programming
environment. Narrative primarily lives in so called sections that
mix markdown docstrings with references to functions, variables,
etc, all of which should probably have their own docstrings.

The primary focus is on making code easily explorable by using
SLIME's `M-.` (`slime-edit-definition`). See how to enable some
fanciness in [Emacs Integration][eff4]. Generating documentation
from sections and all the referenced items in Markdown or HTML
format is also implemented.

With the simplistic tools provided, one may accomplish similar
effects as with Literate Programming, but documentation is generated
from code, not vice versa and there is no support for chunking yet.
Code is first, code must look pretty, documentation is code.

In typical use, PAX packages have no `:export`'s defined. Instead the
[`define-package`][34f5] form gets a docstring which may mention section
names (defined with [`defsection`][b1e7]). When the code is loaded into the
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
table of contents, etc is possible by calling the [`document`][1eb8] function.

*One can even generate documentation for different, but related
libraries at the same time with the output going to different files,
but with cross-page links being automatically added for symbols
mentioned in docstrings. See [Generating Documentation][063a] for
some convenience functions to cover the most common cases.*

Note how `(VARIABLE *FOO-STATE*)` in the [`defsection`][b1e7] form both
exports `*FOO-STATE*` and includes its documentation in
`@FOO-RANDOM-MANUAL`. The symbols [`variable`][2be0] and [`function`][6b15] are just two
instances of 'locatives' which are used in [`defsection`][b1e7] to refer to
definitions tied to symbols. See [Locative Types][1fbb].

The transcript in the code block tagged with `cl-transcript` is
automatically checked for up-to-dateness. See
[Transcripts][7a32].

<a id='x-28MGL-PAX-3A-40MGL-PAX-EMACS-INTEGRATION-20MGL-PAX-3ASECTION-29'></a>

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
`swank-backend:find-source-location` and
`swank-backend:find-definitions` whose support varies across the Lisp
implementations.*

In the following examples, pressing `M-.` when the cursor is on one
of the characters of `foo` or just after `foo`, will visit the
definition of function `foo`:

    function foo
    foo function
    (function foo)
    (foo function)

In particular, references in a [`defsection`][b1e7] form are in (`symbol`
[`locative`][12a1]) format so `M-.` will work just fine there.

Just like vanilla `M-.`, this works in comments and docstrings. In
this example pressing `M-.` on `foo` will visit `foo`'s default
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

<a id='x-28MGL-PAX-3APAX-2EEL-20-28MGL-PAX-3AINCLUDE-20-23P-22-2Fhome-2Fmelisgl-2Fown-2Fmgl-pax-2Fsrc-2Fpax-2Eel-22-20-3AHEADER-NL-20-22-60-60-60elisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29'></a>

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

<a id='x-28MGL-PAX-3A-40MGL-PAX-BASICS-20MGL-PAX-3ASECTION-29'></a>

## 6 Basics

Now let's examine the most important pieces in detail.

<a id='x-28MGL-PAX-3ADEFSECTION-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **DEFSECTION** *NAME (&KEY (PACKAGE '\*PACKAGE\*) (READTABLE '\*READTABLE\*) (EXPORT T) TITLE LINK-TITLE-TO (DISCARD-DOCUMENTATION-P \*DISCARD-DOCUMENTATION-P\*)) &BODY ENTRIES*

    Define a documentation section and maybe export referenced symbols.
    A bit behind the scenes, a global variable with `name` is defined and
    is bound to a [`section`][aee8] object. By convention, section names
    start with the character `@`. See [Tutorial][aa52] for an example.
    
    `entries` consists of docstrings and references. Docstrings are
    arbitrary strings in markdown format, references are defined in the
    form:
    
        (symbol locative)
    
    For example, `(FOO FUNCTION)` refers to the function `foo`, `(@BAR
    SECTION)` says that `@BAR` is a subsection of this
    one. `(BAZ (METHOD () (T T T)))` refers to the default method of the
    three argument generic function `BAZ`. `(FOO FUNCTION)` is
    equivalent to `(FOO (FUNCTION))`.
    
    A locative in a reference can either be a symbol or it can be a list
    whose `car` is a symbol. In either case, the symbol is the called the
    type of the locative while the rest of the elements are the locative
    arguments. See [Locative Types][1fbb] for the list of locative
    types available out of the box.
    
    The same symbol can occur multiple times in a reference, typically
    with different locatives, but this is not required.
    
    The references are not looked up (see [`resolve`][e0d7] in the
    [Extension API][8ed9]) until documentation is generated, so it is
    allowed to refer to things yet to be defined.
    
    If `export` is true (the default), the referenced symbols and `name` are
    candidates for exporting. A candidate symbol is exported if
    
    - it is accessible in `package` (it's not `OTHER-PACKAGE:SOMETHING`)
      and
    
    - there is a reference to it in the section being defined with a
      locative whose type is approved by [`exportable-locative-type-p`][96c5].
    
    See [`define-package`][34f5] if you use the export feature. The idea with
    confounding documentation and exporting is to force documentation of
    all exported symbols.
    
    `title` is a non-marked-up string or `nil`. If non-NIL, it determines
    the text of the heading in the generated output. `link-title-to` is a
    reference given as an
    (`object` [`locative`][12a1]) pair or `nil`, to which the heading will link when
    generating HTML. If not specified, the heading will link to its own
    anchor.
    
    When `discard-documentation-p` (defaults to [`*discard-documentation-p*`][1514])
    is true, `entries` will not be recorded to save memory.

<a id='x-28MGL-PAX-3A-2ADISCARD-DOCUMENTATION-P-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DISCARD-DOCUMENTATION-P\*** *NIL*

    The default value of [`defsection`][b1e7]'s `discard-documentation-p` argument.
    One may want to set `*discard-documentation-p*` to true before
    building a binary application.

<a id='x-28MGL-PAX-3ADEFINE-PACKAGE-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **DEFINE-PACKAGE** *PACKAGE &REST OPTIONS*

    This is like `cl:defpackage` but silences warnings and errors
    signaled when the redefined package is at variance with the current
    state of the package. Typically this situation occurs when symbols
    are exported by calling `export` (as is the case with [`defsection`][b1e7]) as
    opposed to adding `:export` forms to the `defpackage` form and the
    package definition is reevaluated. See the section on [package
    variance](http://www.sbcl.org/manual/#Package-Variance) in the SBCL
    manual.
    
    The bottom line is that if you rely on [`defsection`][b1e7] to do the
    exporting, then you'd better use [`define-package`][34f5].

<a id='x-28MGL-PAX-3ADOCUMENT-20FUNCTION-29'></a>

- [function] **DOCUMENT** *OBJECT &KEY STREAM PAGES (FORMAT :MARKDOWN)*

    Write `object` in `format` to `stream` diverting some output to `pages`.
    `format` can be anything [3BMD][3bmd] supports which is
    currently `:markdown`, `:html` and `:plain`. `stream` may be a stream
    object, `t` or `nil` as with `cl:format`.
    
    Most often, this function is called on section objects
    like `(DOCUMENT @MGL-PAX-MANUAL)`, but it supports all kinds of
    objects for which [`document-object`][a05e] is defined. To look up the
    documentation of function [`document`][1eb8]:
    
        (document #'document)
    
    To generate the documentation for separate libraries with automatic
    cross-links:
    
        (document (list @cube-manual @mat-manual))
    
    Note that not only first class objects can have documentation. For
    instance, variables and deftypes are not represented by objects.
    That's why `cl:documentation` has a `doc-type` argument. [`document`][1eb8] does
    not have anything like that, instead it relies on [`reference`][cc37] objects
    to carry the extra information. We are going to see later how
    references and locatives work. Until then, here is an example on how
    to look up the documentation of type `foo`:
    
        (document (locate 'foo 'type))
    
    One can call `describe` on [`section`][aee8] objects to get
    documentation in markdown format with less markup than the default.
    See [`describe-object`][df39] `(METHOD () (SECTION T))`.
    
    There are quite a few special variables that affect how output is
    generated, see [Documentation Printer Variables][e2a1].
    
    The rest of this description deals with how to generate multiple
    pages.
    
    The `pages` argument is to create multi-page documents by routing some
    of the generated output to files, strings or streams. `pages` is a
    list of page specification elements. A page spec is a plist with
    keys `:objects`, `:output`, `:uri-fragment`, `:source-uri-fn`, `:header-fn`
    and `:footer-fn`. `objects` is a list of objects (references are allowed
    but not required) whose documentation is to be sent to `output`.
    
    When documentation for an object is generated, the first matching
    page spec is used, where the object matches the page spec if it is
    contained in one of its `:objects` in the sense of
    [`collect-reachable-objects`][1920].
    
    `output` can be a number things:
    
    - If it's a list whose first element is a string or a pathname, then
      output will be sent to the file denoted by that and the rest of
      the elements of the list are passed on as arguments to `cl:open`.
      One extra keyword argument is `:ensure-directories-exist`. If it's
      true, `ensure-directories-exist` will be called on the pathname
      before it's opened.
    
    - If it's `nil`, then output will be collected in a string.
    
    - If it's `t`, then output will be sent to `*standard-output*`.
    
    - If it's a stream, then output will be sent to that stream.
    
    If some pages are specified, [`document`][1eb8] returns a list of designators
    for generated output. If a page whose `output` refers to a file that
    was created (which doesn't happen if nothing would be written to
    it), then the corresponding pathname is included in the list. For
    strings the string itself, while for streams the stream object is
    included in the list. This way it's possible to write some pages to
    files and some to strings and have the return value indicate what
    was created. The output designators in the returned list are ordered
    by creation time.
    
    If no `pages` are specified, [`document`][1eb8] returns a single pathname,
    string or stream object according to the value of the `stream`
    argument.
    
    Note that even if `pages` is specified, `stream` acts as a catch all
    taking the generated documentation for references not claimed by any
    pages. Also, the filename, string or stream corresponding to `stream`
    is always the first element in list of generated things that is the
    return value.
    
    `header-fn`, if not `nil`, is a function of a single stream argument
    which is called just before the first write to the page.
    Since `:format` `:html` only generates HTML fragments, this makes it
    possible to print arbitrary headers, typically setting the title,
    css stylesheet, or charset.
    
    `footer-fn` is similar to `header-fn`, but it's called after the last
    write to the page. For HTML, it typically just closes the body.
    
    `uri-fragment` is a string such as `"doc/manual.html"` that specifies
    where the page will be deployed on a webserver. It defines how links
    between pages will look. If it's not specified and `output` refers
    to a file, then it defaults to the name of the file. If `uri-fragment`
    is `nil`, then no links will be made to or from that page.
    
    Finally, `source-uri-fn` is a function of a single, [`reference`][cc37]
    argument. If it returns a value other than `nil`, then it must be a
    string representing an URI. If `format` is `:html` and
    [`*document-mark-up-signatures*`][46ea] is true, then the locative as
    displayed in the signature will be a link to this uri. See
    [`make-github-source-uri-fn`][1cc6].
    
    `pages` may look something like this:
    
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


<a id='x-28MGL-PAX-3A-40MGL-PAX-GENERATING-DOCUMENTATION-20MGL-PAX-3ASECTION-29'></a>

## 7 Generating Documentation

Two convenience functions are provided to serve the common case of
having an `asdf` system with some readmes and a directory with for the
HTML documentation and the default css stylesheet.

<a id='x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-READMES-20FUNCTION-29'></a>

- [function] **UPDATE-ASDF-SYSTEM-READMES** *SECTIONS ASDF-SYSTEM*

    Convenience function to generate two readme files in the directory
    holding the `asdf-system` definition.
    
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

    Generate pretty HTML documentation for a single `asdf` system,
    possibly linking to github. If `update-css-p`, copy the CSS style
    sheet to `target-dir`, as well. Example usage:
    
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

    `nil` or a non-negative integer. If non-NIL, it overrides
    [`*document-max-numbering-level*`][fde0] in dynamic HTML table of contents on
    the left of the page.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-HTML-TOP-BLOCKS-OF-LINKS-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS\*** *NIL*

    A list of blocks of links to be display on the sidebar on the left,
    above the table of contents. A block is of the form `(&KEY TITLE ID
    LINKS)`, where `title` will be displayed at the top of the block in a
    HTML `DIV` with `ID`, followed by the links. `links` is a list
    of `(URI LABEL) elements.`

<a id='x-28MGL-PAX-3A-2ADOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS\*** *NIL*

    Like [`*document-html-top-blocks-of-links*`][be7f], only it is displayed
    below the table of contents.

<a id='x-28MGL-PAX-3A-40MGL-PAX-GITHUB-WORKFLOW-20MGL-PAX-3ASECTION-29'></a>

### 7.1 Github Workflow

It is generally recommended to commit generated readmes (see
[`update-asdf-system-readmes`][2e7a]) so that users have something to read
without reading the code and sites like github can display them.

HTML documentation can also be committed, but there is an issue with
that: when linking to the sources (see [`make-github-source-uri-fn`][1cc6]),
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
idea to add section like the [Links][d7e0] section to allow jumping
between the repository and the gh-pages site.

<a id='x-28MGL-PAX-3AMAKE-GITHUB-SOURCE-URI-FN-20FUNCTION-29'></a>

- [function] **MAKE-GITHUB-SOURCE-URI-FN** *ASDF-SYSTEM GITHUB-URI &KEY GIT-VERSION*

    Return a function suitable as `:source-uri-fn` of a page spec (see
    the `pages` argument of [`document`][1eb8]). The function looks the source
    location of the reference passed to it, and if the location is
    found, the path is made relative to the root directory of
    `asdf-system` and finally an URI pointing to github is returned. The
    URI looks like this:
    
        https://github.com/melisgl/mgl-pax/blob/master/src/pax-early.lisp#L12
    
    "master" in the above link comes from `git-version`.
    
    If `git-version` is `nil`, then an attempt is made to determine to
    current commit id from the `.git` in the directory holding
    `asdf-system`. If no `.git` directory is found, then no links to
    github will be generated.
    
    A separate warning is signalled whenever source location lookup
    fails or if the source location points to a directory not below the
    directory of `asdf-system`.

<a id='x-28MGL-PAX-3A-40MGL-PAX-WORLD-20MGL-PAX-3ASECTION-29'></a>

### 7.2 PAX World

PAX World is a registry of documents, which can generate
cross-linked HTML documentation pages for all the registered
documents.

<a id='x-28MGL-PAX-3AREGISTER-DOC-IN-PAX-WORLD-20FUNCTION-29'></a>

- [function] **REGISTER-DOC-IN-PAX-WORLD** *NAME SECTIONS PAGE-SPECS*

    Register `sections` and `page-specs` under `name` in PAX World. By
    default, [`update-pax-world`][4f82] generates documentation for all of these.

For example, this is how PAX registers itself:

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

    Generate HTML documentation for all `docs`. By default, files are
    created in `*pax-world-dir*` or `(asdf:system-relative-pathname
    :mgl-pax "world/")`, if `nil`. `docs` is a list of entries of the
    form (`name` `sections` `page-specs`). The default for `docs` is all the
    sections and pages registered with [`register-doc-in-pax-world`][c5f2].
    
    In the absence of `:header-fn` `:footer-fn`, `:output`, every spec in
    `page-specs` is augmented with HTML headers, footers and output
    location specifications (based on the name of the section).
    
    If necessary a default page spec is created for every section.

<a id='x-28MGL-PAX-3A-40MGL-PAX-MARKDOWN-SUPPORT-20MGL-PAX-3ASECTION-29'></a>

## 8 Markdown Support

The [Markdown][markdown] in docstrings is processed with the
[3BMD][3bmd] library.

<a id='x-28MGL-PAX-3A-40MGL-PAX-MARKDOWN-INDENTATION-20MGL-PAX-3ASECTION-29'></a>

### 8.1 Indentation

Docstrings can be indented in any of the usual styles. PAX
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

See [`document-object`][d7eb] for the details.

<a id='x-28MGL-PAX-3A-40MGL-PAX-MARKDOWN-SYNTAX-HIGHLIGHTING-20MGL-PAX-3ASECTION-29'></a>

### 8.2 Syntax highlighting

For syntax highlighting, github's [fenced code
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

[fenced-code-blocks]: https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks 


<a id='x-28MGL-PAX-3A-40MGL-PAX-MATHJAX-20MGL-PAX-3ASECTION-29'></a>

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

<a id='x-28MGL-PAX-3A-40MGL-PAX-DOCUMENTATION-PRINTER-VARIABLES-20MGL-PAX-3ASECTION-29'></a>

## 9 Documentation Printer Variables

Docstrings are assumed to be in markdown format and they are pretty
much copied verbatim to the documentation subject to a few knobs
described below.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-UPPERCASE-IS-CODE\*** *T*

    When true, words with at least three characters and no lowercase
    characters naming an interned symbol are assumed to be code as if
    they were marked up with backticks which is especially useful when
    combined with [`*document-link-code*`][1d4c]. For example, this docstring:
    
        "`FOO` and FOO."
    
    is equivalent to this:
    
        "`FOO` and `FOO`."
    
    iff `foo` is an interned symbol. To suppress this behavior, add a
    backslash to the beginning of the symbol or right after the leading
    \* if it would otherwise be parsed as markdown emphasis:
    
        "\\MGL-PAX *\\DOCUMENT-NORMALIZE-PACKAGES*"
    
    The number of backslashes is doubled above because that's how the
    example looks in a docstring. Note that the backslash is discarded
    even if `*document-uppercase-is-code*` is false.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-DOWNCASE-UPPERCASE-CODE-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-DOWNCASE-UPPERCASE-CODE\*** *T*

    If true, then the names of symbols recognized as code (including
    those found if [`*document-uppercase-is-code*`][a282]) are downcased in the
    output if they only consist of uppercase characters. If it is
    `:only-in-markup`, then if the output format does not support
    markup (e.g. it's `:plain`), then no downcasing is performed.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-LINK-CODE-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-LINK-CODE\*** *T*

    When true, during the process of generating documentation for a
    [`section`][aee8], HTML anchors are added before the documentation of
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
    Since symbol names are parsed according to `readtable-case`, character
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
    
    This situation occurs in PAX with `section`([`0`][aee8] [`1`][53a8]) which is both a class (see
    [`section`][aee8]) and a locative type denoted by a symbol (see
    [`section`][53a8]). Back in the example above, clearly,
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
    
    Note that [`*document-link-code*`][1d4c] can be combined with
    [`*document-uppercase-is-code*`][a282] to have links generated for
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
    rendered as bold and italic. In `:html` output, locative types become
    links to sources (if `:source-uri-fn` is provided, see [`document`][1eb8]), and
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
    table of contents off. If [`*document-link-sections*`][f901] is true, then the
    table of contents will link to the sections.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-TEXT-NAVIGATION-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-TEXT-NAVIGATION\*** *NIL*

    If true, then before each heading a line is printed with links to
    the previous, parent and next section. Needs
    [`*document-link-sections*`][f901] to be on to work.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-FANCY-HTML-NAVIGATION-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-FANCY-HTML-NAVIGATION\*** *T*

    If true and the output format is HTML, then headings get a
    navigation component that consists of links to the previous, parent,
    next section and a permalink. This component is normally hidden, it
    is visible only when the mouse is over the heading. Needs
    [`*document-link-sections*`][f901] to be on to work.

<a id='x-28MGL-PAX-3A-2ADOCUMENT-NORMALIZE-PACKAGES-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-NORMALIZE-PACKAGES\*** *T*

    If true, symbols are printed relative to [`section-package`][87c7] of the
    innermost containing section or with full package names if there is
    no containing section. To eliminate ambiguity `[in package ...]`
    messages are printed right after the section heading if necessary.
    If false, symbols are always printed relative to the current
    package.

<a id='x-28MGL-PAX-3A-40MGL-PAX-LOCATIVE-TYPES-20MGL-PAX-3ASECTION-29'></a>

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

<a id='x-28MGL-PAX-3ASECTION-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **SECTION**

    Refers to a section defined by [`defsection`][b1e7].

<a id='x-28VARIABLE-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **VARIABLE** *&OPTIONAL INITFORM*

    Refers to a global special variable. `initform`, or if not specified,
    the global value of the variable is included in the documentation.

<a id='x-28MGL-PAX-3ACONSTANT-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **CONSTANT** *&OPTIONAL INITFORM*

    Refers to a `defconstant`. `initform`, or if not specified,
    the value of the constant is included in the documentation.

<a id='x-28MGL-PAX-3AMACRO-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **MACRO**

<a id='x-28COMPILER-MACRO-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **COMPILER-MACRO**

<a id='x-28FUNCTION-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **FUNCTION**

    Note that the arglist in the generated documentation depends on
    the quality of `swank-backend:arglist`. It may be that default
    values of optional and keyword arguments are missing.

<a id='x-28GENERIC-FUNCTION-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **GENERIC-FUNCTION**

<a id='x-28METHOD-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **METHOD** *METHOD-QUALIFIERS METHOD-SPECIALIZERS*

    See `cl:find-method` for the description of the arguments.
    To refer to the default method of the three argument generic
    function `foo`:
    
        (foo (method () (t t t)))


<a id='x-28MGL-PAX-3AACCESSOR-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **ACCESSOR** *CLASS-NAME*

    To refer to an accessor named `FOO-SLOT` of class
    `foo`:
    
        (foo-slot (accessor foo))


<a id='x-28MGL-PAX-3AREADER-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **READER** *CLASS-NAME*

    To refer to a reader named `FOO-SLOT` of class
    `foo`:
    
        (foo-slot (reader foo))


<a id='x-28MGL-PAX-3AWRITER-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **WRITER** *CLASS-NAME*

    To refer to a writer named `FOO-SLOT` of class
    `foo`:
    
        (foo-slot (writer foo))


<a id='x-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **STRUCTURE-ACCESSOR**

    This is a synonym of [`function`][6b15] with the difference that the often
    ugly and certainly uninformative lambda list will not be printed.

<a id='x-28CLASS-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **CLASS**

<a id='x-28CONDITION-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **CONDITION**

<a id='x-28TYPE-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **TYPE**

    `type` can refer to classes as well, but it's better style to use the
    more specific [`class`][0208] locative type for that. Another difference to
    [`class`][0208] is that an attempt is made at printing the arguments of type
    specifiers.

<a id='x-28PACKAGE-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **PACKAGE**

<a id='x-28MGL-PAX-3ADISLOCATED-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **DISLOCATED**

    Refers to a symbol in a non-specific context. Useful for preventing
    autolinking. For example, if there is a function called `foo` then
    
        `FOO`
    
    will be linked to (if [`*document-link-code*`][1d4c]) its definition. However,
    
        [`FOO`][dislocated]
    
    will not be. On a dislocated locative [`locate`][b2be] always fails with a
    [`locate-error`][2285] condition.

<a id='x-28MGL-PAX-3ALOCATIVE-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **LOCATIVE** *LAMBDA-LIST*

    This is the locative for locatives. When `M-.` is pressed on
    [`variable`][2be0] in `(VARIABLE LOCATIVE)`, this is what makes it possible
    to land at the `(DEFINE-LOCATIVE-TYPE VARIABLE ...)` form.
    Similarly, `(LOCATIVE LOCATIVE)` leads to this very definition.

<a id='x-28MGL-PAX-3AGLOSSARY-TERM-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **GLOSSARY-TERM**

    Refers to a glossary term defined by [`define-glossary-term`][0261].

<a id='x-28MGL-PAX-3ADEFINE-GLOSSARY-TERM-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **DEFINE-GLOSSARY-TERM** *NAME (&KEY TITLE (DISCARD-DOCUMENTATION-P \*DISCARD-DOCUMENTATION-P\*)) DOCSTRING*

    Define a global variable with `name` and set it to a glossary term
    object. A glossary term is just a symbol to hang a docstring on. It
    is a bit like a `section`([`0`][aee8] [`1`][53a8]) in that, when linked to, its `title` will be
    the link text instead of the name of the symbol. Unlike sections
    though, glossary terms are not rendered with headings, but in the
    more lightweight bullet + locative + name/title style.
    
    When `discard-documentation-p` (defaults to [`*discard-documentation-p*`][1514])
    is true, `docstring` will not be recorded to save memory.

<a id='x-28MGL-PAX-3AINCLUDE-20-28MGL-PAX-3ALOCATIVE-29-29'></a>

- [locative] **INCLUDE** *SOURCE &KEY LINE-PREFIX HEADER FOOTER HEADER-NL FOOTER-NL*

    Refers to a region of a file. `source` can be a string or a
    pathname in which case the whole file is being pointed to or it can
    explicitly supply `start`, `end` locatives. `include` is typically used to
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
    
    In the above example, pressing `M-.` on [`pax.el`][ff5c] will open the
    `src/pax.el` file and put the cursor on its first character. `M-.`
    on `FOO-EXAMPLE` will go to the source location of the `(asdf:system
    locative)` locative.
    
    When documentation is generated, the entire [`pax.el`][ff5c] file is
    included in the markdown surrounded by the strings given as
    `header-nl` and `footer-nl` (if any). The trailing newline character is
    assumed implicitly. If that's undesirable, then use `header` and
    `footer` instead. The documentation of `FOO-EXAMPLE` will be the
    region of the file from the source location of the `start`
    locative (inclusive) to the source location of the `end`
    locative (exclusive). `start` and `end` default to the beginning and end
    of the file, respectively.
    
    Note that the file of the source location of `:start` and `:end` must be
    the same. If `source` is pathname designator, then it must be absolute
    so that the locative is context independent.
    
    Finally, if specified `line-prefix` is a string that's prepended to
    each line included in the documentation. For example, a string of
    four spaces makes markdown think it's a code block.

<a id='x-28MGL-PAX-3A-40MGL-PAX-EXTENSION-API-20MGL-PAX-3ASECTION-29'></a>

## 11 Extension API

<a id='x-28MGL-PAX-3A-40MGL-PAX-LOCATIVES-AND-REFERENCES-20MGL-PAX-3ASECTION-29'></a>

### 11.1 Locatives and References

While Common Lisp has rather good introspective abilities, not
everything is first class. For example, there is no object
representing the variable defined with `(DEFVAR
FOO)`. `(MAKE-REFERENCE 'FOO 'VARIABLE)` constructs a [`reference`][cc37] that
captures the path to take from an object (the symbol `foo`) to an
entity of interest (for example, the documentation of the variable).
The path is called the locative. A locative can be applied to an
object like this:

```
(locate 'foo 'variable)
```

which will return the same reference as `(MAKE-REFERENCE 'FOO
'VARIABLE)`. Operations need to know how to deal with references
which we will see in [`locate-and-collect-reachable-objects`][7a11],
[`locate-and-document`][6c17] and [`locate-and-find-source`][e9e9].

Naturally, `(LOCATE 'FOO 'FUNCTION)` will simply return `#'FOO`, no
need to muck with references when there is a perfectly good object.

<a id='x-28MGL-PAX-3ALOCATE-20FUNCTION-29'></a>

- [function] **LOCATE** *OBJECT LOCATIVE &KEY (ERRORP T)*

    Follow `locative` from `object` and return the object it leads to or a
    [`reference`][cc37] if there is no first class object corresponding to the
    location. If `errorp`, then a [`locate-error`][2285] condition is signaled when
    the lookup fails.

<a id='x-28MGL-PAX-3ALOCATE-ERROR-20CONDITION-29'></a>

- [condition] **LOCATE-ERROR** *ERROR*

    Signaled by [`locate`][b2be] when the lookup fails and `errorp`
    is true.

<a id='x-28MGL-PAX-3ALOCATE-ERROR-MESSAGE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29'></a>

- [reader] **LOCATE-ERROR-MESSAGE** *LOCATE-ERROR* *(:MESSAGE)*

<a id='x-28MGL-PAX-3ALOCATE-ERROR-OBJECT-20-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29'></a>

- [reader] **LOCATE-ERROR-OBJECT** *LOCATE-ERROR* *(:OBJECT)*

<a id='x-28MGL-PAX-3ALOCATE-ERROR-LOCATIVE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29'></a>

- [reader] **LOCATE-ERROR-LOCATIVE** *LOCATE-ERROR* *(:LOCATIVE)*

<a id='x-28MGL-PAX-3ARESOLVE-20FUNCTION-29'></a>

- [function] **RESOLVE** *REFERENCE &KEY (ERRORP T)*

    A convenience function to [`locate`][b2be] `reference`'s object with its
    locative.

<a id='x-28MGL-PAX-3AREFERENCE-20CLASS-29'></a>

- [class] **REFERENCE**

    A `reference` represents a path ([`reference-locative`][819a])
    to take from an object ([`reference-object`][0412]).

<a id='x-28MGL-PAX-3AREFERENCE-OBJECT-20-28MGL-PAX-3AREADER-20MGL-PAX-3AREFERENCE-29-29'></a>

- [reader] **REFERENCE-OBJECT** *REFERENCE* *(:OBJECT)*

<a id='x-28MGL-PAX-3AREFERENCE-LOCATIVE-20-28MGL-PAX-3AREADER-20MGL-PAX-3AREFERENCE-29-29'></a>

- [reader] **REFERENCE-LOCATIVE** *REFERENCE* *(:LOCATIVE)*

<a id='x-28MGL-PAX-3AMAKE-REFERENCE-20FUNCTION-29'></a>

- [function] **MAKE-REFERENCE** *OBJECT LOCATIVE*

<a id='x-28MGL-PAX-3ALOCATIVE-TYPE-20FUNCTION-29'></a>

- [function] **LOCATIVE-TYPE** *LOCATIVE*

    The first element of `locative` if it's a list. If it's a symbol then
    it's that symbol itself. Typically, methods of generic functions
    working with locatives take locative type and locative args as
    separate arguments to allow methods have eql specializers on the
    type symbol.

<a id='x-28MGL-PAX-3ALOCATIVE-ARGS-20FUNCTION-29'></a>

- [function] **LOCATIVE-ARGS** *LOCATIVE*

    The `rest` of `locative` if it's a list. If it's a symbol then
    it's ().

<a id='x-28MGL-PAX-3A-40MGL-PAX-NEW-OBJECT-TYPES-20MGL-PAX-3ASECTION-29'></a>

### 11.2 Adding New Object Types

One may wish to make the [`document`][1eb8] function and `M-.` navigation
work with new object types. Extending [`document`][1eb8] can be done by
defining a [`document-object`][a05e] method. To allow these objects to be
referenced from [`defsection`][b1e7], a [`locate-object`][acc9] method is to be defined.
Finally, for `M-.` [`find-source`][b417] can be specialized. Finally,
[`exportable-locative-type-p`][96c5] may be overridden if exporting does not
makes sense. Here is a stripped down example of how all this is done
for [`asdf:system:`][bf8a]

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
      (foo "Long description" 'asdf/system:system-long-description)
      (terpri stream))))

(defmethod find-source ((system asdf:system))
  `(:location
    (:file ,(namestring (asdf/system:system-source-file system)))
    (:position 1)
    (:snippet "")))

```

<a id='x-28MGL-PAX-3ADEFINE-LOCATIVE-TYPE-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **DEFINE-LOCATIVE-TYPE** *LOCATIVE-TYPE LAMBDA-LIST &BODY DOCSTRING*

    Declare `locative-type` as a [`locative`][12a1]. One gets two
    things in return: first, a place to document the format and
    semantics of `locative-type` (in `lambda-list` and `docstring`); second,
    being able to reference `(LOCATIVE-TYPE LOCATIVE)`. For example, if
    you have:
    
    ```common-lisp
    (define-locative-type variable (&optional initform)
      "Dummy docstring.")
    ```
    
    then `(VARIABLE LOCATIVE)` refers to this form.

<a id='x-28MGL-PAX-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29'></a>

- [generic-function] **EXPORTABLE-LOCATIVE-TYPE-P** *LOCATIVE-TYPE*

    Return true iff symbols in references with
    `locative-type` are to be exported by default when they occur in a
    [`defsection`][b1e7]. The default method returns `t`, while the methods for
    [`package`][a15b], [`asdf:system`][bf8a] and [`method`][f1a0] return `nil`.
    
    [`defsection`][b1e7] calls this function to decide what symbols to export when
    its `export` argument is true.

<a id='x-28MGL-PAX-3ALOCATE-OBJECT-20GENERIC-FUNCTION-29'></a>

- [generic-function] **LOCATE-OBJECT** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    Return the object, to which `object` and the locative
    refer. For example, if `locative-type` is the symbol [`package`][a15b], this
    returns `(FIND-PACKAGE SYMBOL)`. Signal a [`locate-error`][2285] condition by
    calling the [`locate-error`][f3b7] function if the lookup fails. Signal other
    errors if the types of the argument are bad, for instance
    `locative-args` is not the empty list in the package example. If a
    [`reference`][cc37] is returned then it must be canonical in the sense that
    calling [`canonical-reference`][24fc] on it will return the same reference.
    For extension only, don't call this directly.

<a id='x-28MGL-PAX-3ALOCATE-ERROR-20FUNCTION-29'></a>

- [function] **LOCATE-ERROR** *&REST FORMAT-AND-ARGS*

    Call this function to signal a [`locate-error`][2285] condition from a
    [`locate-object`][acc9] method. `format-and-args` contains a format string and
    args suitable for `format` from which the [`locate-error-message`][81be] is
    constructed. If `format-and-args` is `nil`, then the message will be `nil`
    too.
    
    The object and the locative are not specified, they are added by
    [`locate`][b2be] when it resignals the condition.

<a id='x-28MGL-PAX-3ACANONICAL-REFERENCE-20GENERIC-FUNCTION-29'></a>

- [generic-function] **CANONICAL-REFERENCE** *OBJECT*

    Return a [`reference`][cc37] that resolves to `object`.

<a id='x-28MGL-PAX-3ACOLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29'></a>

- [generic-function] **COLLECT-REACHABLE-OBJECTS** *OBJECT*

    Return a list of objects representing all things
    that would be documented in a ([`document`][1eb8] `object`) call. For sections
    this is simply the union of references reachable from references in
    [`section-entries`][1f66]. The returned objects can be anything provided that
    [`canonical-reference`][24fc] works on them. The list need not include `object`
    itself.
    
    One only has to specialize this for new container-like objects.

<a id='x-28MGL-PAX-3ACOLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-28T-29-29-29'></a>

- [method] **COLLECT-REACHABLE-OBJECTS** *OBJECT*

    This default implementation returns the empty list. This means that
    nothing is reachable from `object`.

<a id='x-28MGL-PAX-3ADOCUMENT-OBJECT-20GENERIC-FUNCTION-29'></a>

- [generic-function] **DOCUMENT-OBJECT** *OBJECT STREAM*

    Write `object` (and its references recursively) in
    `format` to `stream`.
    
    The [`document`][1eb8] function calls this generic function with `level` 0,
    passing `format` on. Add methods specializing on `object` to customize
    how objects of that type are presented in the documentation.

<a id='x-28MGL-PAX-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28STRING-20T-29-29-29'></a>

- [method] **DOCUMENT-OBJECT** *(STRING STRING) STREAM*

    Print `string` verbatim to `stream` after cleaning up indentation.
    
    Docstrings in sources are indented in various ways which can easily
    mess up markdown. To handle the most common cases leave the first
    line alone, but from the rest of the lines strip the longest run of
    leading spaces that is common to all non-blank lines.

<a id='x-28MGL-PAX-3AFIND-SOURCE-20GENERIC-FUNCTION-29'></a>

- [generic-function] **FIND-SOURCE** *OBJECT*

    Like `swank:find-definition-for-thing`, but this
    one is a generic function to be extensible. In fact, the default
    implementation simply defers to `swank:find-definition-for-thing`.
    This function is called by `locate-definition-for-emacs` which lies
    behind the `M-.` extension (see [Emacs Integration][eff4]).
    
    If successful, the return value looks like this:
    
    ```commonlisp
    (:location (:file "/home/mega/own/mgl/pax/test/test.lisp")
               (:position 24) nil)
    ```
    
    The `nil` is the source snippet which is optional. Note that position
    1 is the first character. If unsuccessful, the return values is
    like:
    
    ```commonlisp
    (:error "Unknown source location for SOMETHING")
    ```


<a id='x-28MGL-PAX-3A-40MGL-PAX-REFERENCE-BASED-EXTENSIONS-20MGL-PAX-3ASECTION-29'></a>

### 11.3 Reference Based Extensions

Let's see how to extend [`document`][1eb8] and `M-.` navigation if there is
no first class object to represent the thing of interest. Recall
that [`locate`][b2be] returns a [`reference`][cc37] object in this case. [`document-object`][a05e]
and [`find-source`][b417] defer to [`locate-and-document`][6c17] and
[`locate-and-find-source`][e9e9], which have [`locative-type`][966a] in their argument
list for `eql` specializing pleasure. Here is a stripped down example
of how the [`variable`][2be0] locative is defined:

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
    (terpri stream)
    (with-dislocated-symbols ((list symbol))
      (maybe-print-docstring symbol locative-type stream))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'variable))
                                   locative-args)
  (declare (ignore locative-args))
  (find-one-location (swank-backend:find-definitions symbol)
                     '("variable" "defvar" "defparameter"
                       "special-declaration")))

```

<a id='x-28MGL-PAX-3ACOLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-28MGL-PAX-3AREFERENCE-29-29-29'></a>

- [method] **COLLECT-REACHABLE-OBJECTS** *(REFERENCE REFERENCE)*

    If `reference` can be resolved to a non-reference, call
    [`collect-reachable-objects`][1920] with it, else call
    [`locate-and-collect-reachable-objects`][7a11] on the object, locative-type,
    locative-args of `reference`

<a id='x-28MGL-PAX-3ALOCATE-AND-COLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29'></a>

- [generic-function] **LOCATE-AND-COLLECT-REACHABLE-OBJECTS** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    Called by [`collect-reachable-objects`][1920] on [`reference`][cc37]
    objects, this function has essentially the same purpose as its
    caller but it has different arguments to allow specializing on
    `locative-type`.

<a id='x-28MGL-PAX-3ALOCATE-AND-COLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-28T-20T-20T-29-29-29'></a>

- [method] **LOCATE-AND-COLLECT-REACHABLE-OBJECTS** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    This default implementation returns the empty list. This means that
    nothing is reachable from the reference.

<a id='x-28MGL-PAX-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28MGL-PAX-3AREFERENCE-20T-29-29-29'></a>

- [method] **DOCUMENT-OBJECT** *(REFERENCE REFERENCE) STREAM*

    If `reference` can be resolved to a non-reference, call
    [`document-object`][a05e] with it, else call LOCATE-AND-DOCUMENT-OBJECT on the
    object, locative-type, locative-args of `reference`

<a id='x-28MGL-PAX-3ALOCATE-AND-DOCUMENT-20GENERIC-FUNCTION-29'></a>

- [generic-function] **LOCATE-AND-DOCUMENT** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS STREAM*

    Called by [`document-object`][a05e] on [`reference`][cc37] objects,
    this function has essentially the same purpose as [`document-object`][a05e]
    but it has different arguments to allow specializing on
    `locative-type`.

<a id='x-28MGL-PAX-3AFIND-SOURCE-20-28METHOD-20NIL-20-28MGL-PAX-3AREFERENCE-29-29-29'></a>

- [method] **FIND-SOURCE** *(REFERENCE REFERENCE)*

    If `reference` can be resolved to a non-reference, call [`find-source`][b417]
    with it, else call [`locate-and-find-source`][e9e9] on the object,
    locative-type, locative-args of `reference`

<a id='x-28MGL-PAX-3ALOCATE-AND-FIND-SOURCE-20GENERIC-FUNCTION-29'></a>

- [generic-function] **LOCATE-AND-FIND-SOURCE** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    Called by [`find-source`][b417] on [`reference`][cc37] objects, this
    function has essentially the same purpose as [`find-source`][b417] but it has
    different arguments to allow specializing on `locative-type`.

<a id='x-28MGL-PAX-3ALOCATE-AND-FIND-SOURCE-20-28METHOD-20NIL-20-28T-20T-20T-29-29-29'></a>

- [method] **LOCATE-AND-FIND-SOURCE** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    This default implementation simply calls [`find-source`][b417] with `object`
    which should cover the common case of a macro expanding to, for
    instance, a defun but having its own locative type.

We have covered the basic building blocks of reference based
extensions. Now let's see how the obscure
[`define-symbol-locative-type`][5d5a] and
[`define-definer-for-symbol-locative-type`][ce09] macros work together to
simplify the common task of associating definition and documentation
with symbols in a certain context.

<a id='x-28MGL-PAX-3ADEFINE-SYMBOL-LOCATIVE-TYPE-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **DEFINE-SYMBOL-LOCATIVE-TYPE** *LOCATIVE-TYPE LAMBDA-LIST &BODY DOCSTRING*

    Similar to [`define-locative-type`][b811] but it assumes that all things
    locatable with `locative-type` are going to be just symbols defined
    with a definer defined with [`define-definer-for-symbol-locative-type`][ce09].
    It is useful to attach documentation and source location to symbols
    in a particular context. An example will make everything clear:
    
    ```commonlisp
    (define-symbol-locative-type direction ()
      "A direction is a symbol. (After this `M-.` on `DIRECTION LOCATIVE`
      works and it can also be included in DEFSECTION forms.)")
    
    (define-definer-for-symbol-locative-type define-direction direction ()
      "With DEFINE-DIRECTION one can document what a symbol means when
      interpreted as a direction.")
    
    (define-direction up ()
      "UP is equivalent to a coordinate delta of (0, -1).")
    ```
    
    After all this, `(UP DIRECTION)` refers to the `DEFINE-DIRECTION`
    form above.

<a id='x-28MGL-PAX-3ADEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE** *NAME LOCATIVE-TYPE &BODY DOCSTRING*

    Define a macro with `name` which can be used to attach documentation,
    a lambda-list and source location to a symbol in the context of
    `locative-type`. The defined macro's arglist is (`symbol` `lambda-list`
    `&optional` `docstring`). `locative-type` is assumed to have been defined
    with [`define-symbol-locative-type`][5d5a].

<a id='x-28MGL-PAX-3A-40MGL-PAX-SECTIONS-20MGL-PAX-3ASECTION-29'></a>

### 11.4 Sections

[`Section`][aee8] objects rarely need to be dissected since
[`defsection`][b1e7] and [`document`][1eb8] cover most needs. However, it is plausible
that one wants to subclass them and maybe redefine how they are
presented.

<a id='x-28MGL-PAX-3ASECTION-20CLASS-29'></a>

- [class] **SECTION**

    [`defsection`][b1e7] stores its `name`, `title`, [`package`][a15b],
    `readtable` and `entries` in [`section`][aee8] objects.

<a id='x-28MGL-PAX-3ASECTION-NAME-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29'></a>

- [reader] **SECTION-NAME** *SECTION* *(:NAME)*

    The name of the global variable whose value is
    this `section`([`0`][aee8] [`1`][53a8]) object.

<a id='x-28MGL-PAX-3ASECTION-PACKAGE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29'></a>

- [reader] **SECTION-PACKAGE** *SECTION* *(:PACKAGE)*

    `*package*` will be bound to this package when
    generating documentation for this section.

<a id='x-28MGL-PAX-3ASECTION-READTABLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29'></a>

- [reader] **SECTION-READTABLE** *SECTION* *(:READTABLE)*

    `*readtable*` will be bound to this when generating
    documentation for this section.

<a id='x-28MGL-PAX-3ASECTION-TITLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29'></a>

- [reader] **SECTION-TITLE** *SECTION* *(:TITLE)*

    `string` or `nil`. Used in generated documentation.

<a id='x-28MGL-PAX-3ASECTION-LINK-TITLE-TO-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29'></a>

- [reader] **SECTION-LINK-TITLE-TO** *SECTION* *(:LINK-TITLE-TO = NIL)*

    A [`reference`][cc37] or `nil`. Used in generated documentation.

<a id='x-28MGL-PAX-3ASECTION-ENTRIES-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29'></a>

- [reader] **SECTION-ENTRIES** *SECTION* *(:ENTRIES)*

    A list of strings and [`reference`][cc37] objects in the
    order they occurred in [`defsection`][b1e7].

<a id='x-28DESCRIBE-OBJECT-20-28METHOD-20NIL-20-28MGL-PAX-3ASECTION-20T-29-29-29'></a>

- [method] **DESCRIBE-OBJECT** *(SECTION SECTION) STREAM*

    [`section`][aee8] objects are printed by calling [`document`][1eb8] on them
    with all [Documentation Printer Variables][e2a1], except for
    [`*document-normalize-packages*`][ada7], turned off to reduce clutter.

<a id='x-28MGL-PAX-3A-40MGL-PAX-TRANSCRIPT-20MGL-PAX-3ASECTION-29'></a>

## 12 Transcripts

What are transcripts for? When writing a tutorial, one often wants
to include a REPL session with maybe a few defuns and a couple of
forms whose output or return values are shown. Also, in a function's
docstring an example call with concrete arguments and return values
speaks volumes. A transcript is a text that looks like a repl
session, but which has a light markup for printed output and return
values, while no markup (i.e. prompt) for lisp forms. The PAX
transcripts may include output and return values of all forms, or
only selected ones. In either case the transcript itself can be
easily generated from the source code.

The main worry associated with including examples in the
documentation is that they tend to get out-of-sync with the code.
This is solved by being able to parse back and update transcripts.
In fact, this is exactly what happens during documentation
generation with PAX. Code sections tagged `cl-transcript` are
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
`pythonic-string-reader` and its triple-quoted strings that allow one
to work with nested strings with less noise. The triple-quote syntax
can be enabled with:

    (in-readtable pythonic-string-syntax)


<a id='x-28MGL-PAX-3A-40MGL-PAX-TRANSCRIPT-EMACS-INTEGRATION-20MGL-PAX-3ASECTION-29'></a>

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
`cl-transcript` is only to tell PAX to perform consistency checks at
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
[`transcription-consistency-error`][5a2c] because the printed output and the
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
index of the syntax to be used in [`mgl-pax`][4918]:[`*syntaxes*`][6a46]. Without a
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

<a id='x-28MGL-PAX-3A-3ATRANSCRIBE-2EEL-20-28MGL-PAX-3AINCLUDE-20-23P-22-2Fhome-2Fmelisgl-2Fown-2Fmgl-pax-2Fsrc-2Ftranscribe-2Eel-22-20-3AHEADER-NL-20-22-60-60-60elisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29'></a>

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

<a id='x-28MGL-PAX-3A-40MGL-PAX-TRANSCRIPT-API-20MGL-PAX-3ASECTION-29'></a>

### 12.2 Transcript API

<a id='x-28MGL-PAX-3ATRANSCRIBE-20FUNCTION-29'></a>

- [function] **TRANSCRIBE** *INPUT OUTPUT &KEY UPDATE-ONLY (INCLUDE-NO-OUTPUT UPDATE-ONLY) (INCLUDE-NO-VALUE UPDATE-ONLY) (ECHO T) CHECK-CONSISTENCY DEFAULT-SYNTAX (INPUT-SYNTAXES \*SYNTAXES\*) (OUTPUT-SYNTAXES \*SYNTAXES\*)*

    Read forms from `input` and write them (iff `echo`) to `output`
    followed by any output and return values produced by calling `eval` on
    the form. `input` can be a stream or a string, while `output` can be a
    stream or `nil` in which case transcription goes into a string. The
    return value is the `output` stream or the string that was
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
    uses [`transcribe`][0382] markup syntax in this very example, so let's do it
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
    the `:output` prefix (`".."`). The return values above are printed
    with the `:readable` prefix (`"=>"`). Note how these prefixes are
    always printed on a new line to facilitate parsing.
    
    **Updating**
    
    [`transcribe`][0382] is able to parse its own output. If we transcribe the
    previous output above, we get it back exactly. However, if we remove
    all output markers, leave only a placeholder value marker and
    pass `:update-only` `t` with source:
    
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
    
    With `update-only`, printed output of a form is only transcribed if
    there were output markers in the source. Similarly, with
    `update-only`, return values are only transcribed if there were value
    markers in the source.
    
    **No Output/Values**
    
    If the form produces no output or returns no values, then whether or
    not output and values are transcribed is controlled by
    `include-no-output` and `include-no-value`, respectively. By default,
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
    
    With `update-only` true, we probably wouldn't like to lose those
    markers since they were put there for a reason. Hence, with
    `update-only`, `include-no-output` and `include-no-value` default to true.
    So with `update-only` the above example is transcribed to:
    
    ```commonlisp
    (values)
    ..
    => ; No value
    ```
    
    where the last line is the `:no-value` prefix.
    
    **Consistency Checks**
    
    If `check-consistency` is true, then [`transcribe`][0382] signals a continuable
    [`transcription-output-consistency-error`][8a71] whenever a form's output as a
    string is different from what was in `input`, provided that `input`
    contained the output. Similary, for values, a continuable
    [`transcription-values-consistency-error`][a73e] is signalled if a value read
    from the source does not print as the as the value returned by `eval`.
    This allows readable values to be hand-indented without failing
    consistency checks:
    
    ```commonlisp
    (list 1 2)
    => (1
          2)
    ```
    
    **Unreadable Values**
    
    The above scheme involves `read`, so consistency of unreadable values
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
    
    where `"==>"` is the `:unreadable` prefix and `"-->"` is
    the `:unreadable-continuation` prefix. As with outputs, a consistency
    check between an unreadable value from the source and the value from
    `eval` is performed with `string=`. That is, the value from `eval` is
    printed to a string and compared to the source value. Hence, any
    change to unreadable values will break consistency checks. This is
    most troublesome with instances of classes with the default
    `print-object` method printing the memory address. There is currently
    no remedy for that, except for customizing `print-object` or not
    transcribing that kind of stuff.
    
    **Syntaxes**
    
    Finally, a transcript may employ different syntaxes for the output
    and values of different forms. When `input` is read, the syntax for
    each form is determined by trying to match all prefixes from all
    syntaxes in `input-syntaxes` against a line. If there are no output or
    values for a form in `input`, then the syntax remains undetermined.
    
    When `output` is written, the prefixes to be used are looked up in
    `default-syntax` of `output-syntaxes`, if `default-syntax` is not `nil`. If
    `default-syntax` is `nil`, then the syntax used by the same form in the
    `input` is used or (if that could not be determined) the syntax of the
    previous form. If there was no previous form, then the first syntax
    if `output-syntaxes` is used.
    
    To produce a transcript that's executable Lisp code,
    use `:default-syntax` `:commented-1:`
    
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
    use `:default-syntax` `:default`. If `default-syntax` is `nil` (the
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

    The default syntaxes used by [`transcribe`][0382] for reading and writing
    lines containing output and values of an evaluated form.
    
    A syntax is a list of of the form `(SYNTAX-ID &REST PREFIXES)` where
    `PREFIXES` is a list of `(PREFIX-ID PREFIX-STRING)` elements. For
    example the syntax `:commented-1` looks like this:
    
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
    for `:readable-continuation`. If that's missing (as in the `:default`
    syntax), then the following value is read with `read` and printed with
    `prin1` (hence no need to mark up the following lines).
    
    When writing, an extra space is added automatically if the line to
    be prefixed is not empty. Similarly, the first space following the
    prefix discarded when reading.
    
    See [`transcribe`][0382] for how the actual syntax to be used is selected.

<a id='x-28MGL-PAX-3ATRANSCRIPTION-ERROR-20CONDITION-29'></a>

- [condition] **TRANSCRIPTION-ERROR** *ERROR*

    Represents syntactic errors in the `source` argument
    of [`transcribe`][0382] and also serves as the superclass of
    [`transcription-consistency-error`][5a2c].

<a id='x-28MGL-PAX-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29'></a>

- [condition] **TRANSCRIPTION-CONSISTENCY-ERROR** *TRANSCRIPTION-ERROR*

    A common superclass for
    [`transcription-output-consistency-error`][8a71] and
    [`transcription-values-consistency-error`][a73e].

<a id='x-28MGL-PAX-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29'></a>

- [condition] **TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR** *TRANSCRIPTION-CONSISTENCY-ERROR*

    Signaled (with `cerror`) by [`transcribe`][0382] when invoked
    with `:check-consistency` and the output of a form is not the same as
    what was parsed.

<a id='x-28MGL-PAX-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29'></a>

- [condition] **TRANSCRIPTION-VALUES-CONSISTENCY-ERROR** *TRANSCRIPTION-CONSISTENCY-ERROR*

    Signaled (with `cerror`) by [`transcribe`][0382] when invoked
    with `:check-consistency` and the values of a form are inconsistent
    with their parsed representation.

  [00f0]: #x-28MGL-PAX-3A-40MGL-PAX-REFERENCE-BASED-EXTENSIONS-20MGL-PAX-3ASECTION-29 "Reference Based Extensions"
  [0208]: #x-28CLASS-20-28MGL-PAX-3ALOCATIVE-29-29 "(CLASS (MGL-PAX:LOCATIVE))"
  [0261]: #x-28MGL-PAX-3ADEFINE-GLOSSARY-TERM-20-28MGL-PAX-3AMACRO-29-29 "(MGL-PAX:DEFINE-GLOSSARY-TERM (MGL-PAX:MACRO))"
  [0382]: #x-28MGL-PAX-3ATRANSCRIBE-20FUNCTION-29 "(MGL-PAX:TRANSCRIBE FUNCTION)"
  [0412]: #x-28MGL-PAX-3AREFERENCE-OBJECT-20-28MGL-PAX-3AREADER-20MGL-PAX-3AREFERENCE-29-29 "(MGL-PAX:REFERENCE-OBJECT (MGL-PAX:READER MGL-PAX:REFERENCE))"
  [063a]: #x-28MGL-PAX-3A-40MGL-PAX-GENERATING-DOCUMENTATION-20MGL-PAX-3ASECTION-29 "Generating Documentation"
  [12a1]: #x-28MGL-PAX-3ALOCATIVE-20-28MGL-PAX-3ALOCATIVE-29-29 "(MGL-PAX:LOCATIVE (MGL-PAX:LOCATIVE))"
  [1514]: #x-28MGL-PAX-3A-2ADISCARD-DOCUMENTATION-P-2A-20-28VARIABLE-29-29 "(MGL-PAX:*DISCARD-DOCUMENTATION-P* (VARIABLE))"
  [1920]: #x-28MGL-PAX-3ACOLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29 "(MGL-PAX:COLLECT-REACHABLE-OBJECTS GENERIC-FUNCTION)"
  [1cc6]: #x-28MGL-PAX-3AMAKE-GITHUB-SOURCE-URI-FN-20FUNCTION-29 "(MGL-PAX:MAKE-GITHUB-SOURCE-URI-FN FUNCTION)"
  [1d4c]: #x-28MGL-PAX-3A-2ADOCUMENT-LINK-CODE-2A-20-28VARIABLE-29-29 "(MGL-PAX:*DOCUMENT-LINK-CODE* (VARIABLE))"
  [1eb8]: #x-28MGL-PAX-3ADOCUMENT-20FUNCTION-29 "(MGL-PAX:DOCUMENT FUNCTION)"
  [1f66]: #x-28MGL-PAX-3ASECTION-ENTRIES-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29 "(MGL-PAX:SECTION-ENTRIES (MGL-PAX:READER MGL-PAX:SECTION))"
  [1fbb]: #x-28MGL-PAX-3A-40MGL-PAX-LOCATIVE-TYPES-20MGL-PAX-3ASECTION-29 "Locative Types"
  [2285]: #x-28MGL-PAX-3ALOCATE-ERROR-20CONDITION-29 "(MGL-PAX:LOCATE-ERROR CONDITION)"
  [24fc]: #x-28MGL-PAX-3ACANONICAL-REFERENCE-20GENERIC-FUNCTION-29 "(MGL-PAX:CANONICAL-REFERENCE GENERIC-FUNCTION)"
  [2748]: #x-28MGL-PAX-3A-40MGL-PAX-GITHUB-WORKFLOW-20MGL-PAX-3ASECTION-29 "Github Workflow"
  [2be0]: #x-28VARIABLE-20-28MGL-PAX-3ALOCATIVE-29-29 "(VARIABLE (MGL-PAX:LOCATIVE))"
  [2e7a]: #x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-READMES-20FUNCTION-29 "(MGL-PAX:UPDATE-ASDF-SYSTEM-READMES FUNCTION)"
  [32ac]: #x-28MGL-PAX-3A-40MGL-PAX-MARKDOWN-SYNTAX-HIGHLIGHTING-20MGL-PAX-3ASECTION-29 "Syntax highlighting"
  [34f5]: #x-28MGL-PAX-3ADEFINE-PACKAGE-20-28MGL-PAX-3AMACRO-29-29 "(MGL-PAX:DEFINE-PACKAGE (MGL-PAX:MACRO))"
  [4336]: #x-28MGL-PAX-3A-40MGL-PAX-MARKDOWN-INDENTATION-20MGL-PAX-3ASECTION-29 "Indentation"
  [46ea]: #x-28MGL-PAX-3A-2ADOCUMENT-MARK-UP-SIGNATURES-2A-20-28VARIABLE-29-29 "(MGL-PAX:*DOCUMENT-MARK-UP-SIGNATURES* (VARIABLE))"
  [4918]: #x-28-22mgl-pax-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "(\"mgl-pax\" ASDF/SYSTEM:SYSTEM)"
  [4f82]: #x-28MGL-PAX-3AUPDATE-PAX-WORLD-20FUNCTION-29 "(MGL-PAX:UPDATE-PAX-WORLD FUNCTION)"
  [5161]: #x-28MGL-PAX-3A-40MGL-PAX-NEW-OBJECT-TYPES-20MGL-PAX-3ASECTION-29 "Adding New Object Types"
  [53a8]: #x-28MGL-PAX-3ASECTION-20-28MGL-PAX-3ALOCATIVE-29-29 "(MGL-PAX:SECTION (MGL-PAX:LOCATIVE))"
  [55dd]: #x-28MGL-PAX-3A-40MGL-PAX-MATHJAX-20MGL-PAX-3ASECTION-29 "MathJax"
  [5a2c]: #x-28MGL-PAX-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29 "(MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR CONDITION)"
  [5d5a]: #x-28MGL-PAX-3ADEFINE-SYMBOL-LOCATIVE-TYPE-20-28MGL-PAX-3AMACRO-29-29 "(MGL-PAX:DEFINE-SYMBOL-LOCATIVE-TYPE (MGL-PAX:MACRO))"
  [6a46]: #x-28MGL-PAX-3A-2ASYNTAXES-2A-20-28VARIABLE-29-29 "(MGL-PAX:*SYNTAXES* (VARIABLE))"
  [6b15]: #x-28FUNCTION-20-28MGL-PAX-3ALOCATIVE-29-29 "(FUNCTION (MGL-PAX:LOCATIVE))"
  [6c17]: #x-28MGL-PAX-3ALOCATE-AND-DOCUMENT-20GENERIC-FUNCTION-29 "(MGL-PAX:LOCATE-AND-DOCUMENT GENERIC-FUNCTION)"
  [7a11]: #x-28MGL-PAX-3ALOCATE-AND-COLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29 "(MGL-PAX:LOCATE-AND-COLLECT-REACHABLE-OBJECTS GENERIC-FUNCTION)"
  [7a32]: #x-28MGL-PAX-3A-40MGL-PAX-TRANSCRIPT-20MGL-PAX-3ASECTION-29 "Transcripts"
  [8059]: #x-28MGL-PAX-3A-40MGL-PAX-BASICS-20MGL-PAX-3ASECTION-29 "Basics"
  [819a]: #x-28MGL-PAX-3AREFERENCE-LOCATIVE-20-28MGL-PAX-3AREADER-20MGL-PAX-3AREFERENCE-29-29 "(MGL-PAX:REFERENCE-LOCATIVE (MGL-PAX:READER MGL-PAX:REFERENCE))"
  [81be]: #x-28MGL-PAX-3ALOCATE-ERROR-MESSAGE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29 "(MGL-PAX:LOCATE-ERROR-MESSAGE (MGL-PAX:READER MGL-PAX:LOCATE-ERROR))"
  [84ee]: #x-28MGL-PAX-3A-40MGL-PAX-BACKGROUND-20MGL-PAX-3ASECTION-29 "Background"
  [87c7]: #x-28MGL-PAX-3ASECTION-PACKAGE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29 "(MGL-PAX:SECTION-PACKAGE (MGL-PAX:READER MGL-PAX:SECTION))"
  [8a71]: #x-28MGL-PAX-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29 "(MGL-PAX:TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR CONDITION)"
  [8ed9]: #x-28MGL-PAX-3A-40MGL-PAX-EXTENSION-API-20MGL-PAX-3ASECTION-29 "Extension API"
  [966a]: #x-28MGL-PAX-3ALOCATIVE-TYPE-20FUNCTION-29 "(MGL-PAX:LOCATIVE-TYPE FUNCTION)"
  [96c5]: #x-28MGL-PAX-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29 "(MGL-PAX:EXPORTABLE-LOCATIVE-TYPE-P GENERIC-FUNCTION)"
  [a05e]: #x-28MGL-PAX-3ADOCUMENT-OBJECT-20GENERIC-FUNCTION-29 "(MGL-PAX:DOCUMENT-OBJECT GENERIC-FUNCTION)"
  [a15b]: #x-28PACKAGE-20-28MGL-PAX-3ALOCATIVE-29-29 "(PACKAGE (MGL-PAX:LOCATIVE))"
  [a282]: #x-28MGL-PAX-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20-28VARIABLE-29-29 "(MGL-PAX:*DOCUMENT-UPPERCASE-IS-CODE* (VARIABLE))"
  [a73e]: #x-28MGL-PAX-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29 "(MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR CONDITION)"
  [aa52]: #x-28MGL-PAX-3A-40MGL-PAX-TUTORIAL-20MGL-PAX-3ASECTION-29 "Tutorial"
  [acc9]: #x-28MGL-PAX-3ALOCATE-OBJECT-20GENERIC-FUNCTION-29 "(MGL-PAX:LOCATE-OBJECT GENERIC-FUNCTION)"
  [ada7]: #x-28MGL-PAX-3A-2ADOCUMENT-NORMALIZE-PACKAGES-2A-20-28VARIABLE-29-29 "(MGL-PAX:*DOCUMENT-NORMALIZE-PACKAGES* (VARIABLE))"
  [aee8]: #x-28MGL-PAX-3ASECTION-20CLASS-29 "(MGL-PAX:SECTION CLASS)"
  [b1e7]: #x-28MGL-PAX-3ADEFSECTION-20-28MGL-PAX-3AMACRO-29-29 "(MGL-PAX:DEFSECTION (MGL-PAX:MACRO))"
  [b2be]: #x-28MGL-PAX-3ALOCATE-20FUNCTION-29 "(MGL-PAX:LOCATE FUNCTION)"
  [b417]: #x-28MGL-PAX-3AFIND-SOURCE-20GENERIC-FUNCTION-29 "(MGL-PAX:FIND-SOURCE GENERIC-FUNCTION)"
  [b811]: #x-28MGL-PAX-3ADEFINE-LOCATIVE-TYPE-20-28MGL-PAX-3AMACRO-29-29 "(MGL-PAX:DEFINE-LOCATIVE-TYPE (MGL-PAX:MACRO))"
  [be22]: #x-28MGL-PAX-3A-40MGL-PAX-SECTIONS-20MGL-PAX-3ASECTION-29 "Sections"
  [be7f]: #x-28MGL-PAX-3A-2ADOCUMENT-HTML-TOP-BLOCKS-OF-LINKS-2A-20-28VARIABLE-29-29 "(MGL-PAX:*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS* (VARIABLE))"
  [bf16]: #x-28MGL-PAX-3A-40MGL-PAX-TRANSCRIPT-API-20MGL-PAX-3ASECTION-29 "Transcript API"
  [bf8a]: #x-28ASDF-2FSYSTEM-3ASYSTEM-20-28MGL-PAX-3ALOCATIVE-29-29 "(ASDF/SYSTEM:SYSTEM (MGL-PAX:LOCATIVE))"
  [c5f2]: #x-28MGL-PAX-3AREGISTER-DOC-IN-PAX-WORLD-20FUNCTION-29 "(MGL-PAX:REGISTER-DOC-IN-PAX-WORLD FUNCTION)"
  [c694]: #x-28MGL-PAX-3A-40MGL-PAX-TRANSCRIPT-EMACS-INTEGRATION-20MGL-PAX-3ASECTION-29 "Transcribing with Emacs"
  [cc37]: #x-28MGL-PAX-3AREFERENCE-20CLASS-29 "(MGL-PAX:REFERENCE CLASS)"
  [ce09]: #x-28MGL-PAX-3ADEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE-20-28MGL-PAX-3AMACRO-29-29 "(MGL-PAX:DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE (MGL-PAX:MACRO))"
  [d023]: #x-28MGL-PAX-3A-40MGL-PAX-LOCATIVES-AND-REFERENCES-20MGL-PAX-3ASECTION-29 "Locatives and References"
  [d58f]: #x-28MGL-PAX-3A-40MGL-PAX-MARKDOWN-SUPPORT-20MGL-PAX-3ASECTION-29 "Markdown Support"
  [d7e0]: #x-28MGL-PAX-3A-40MGL-PAX-LINKS-20MGL-PAX-3ASECTION-29 "Links"
  [d7eb]: #x-28MGL-PAX-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28STRING-20T-29-29-29 "(MGL-PAX:DOCUMENT-OBJECT (METHOD NIL (STRING T)))"
  [df39]: #x-28DESCRIBE-OBJECT-20-28METHOD-20NIL-20-28MGL-PAX-3ASECTION-20T-29-29-29 "(DESCRIBE-OBJECT (METHOD NIL (MGL-PAX:SECTION T)))"
  [e0d7]: #x-28MGL-PAX-3ARESOLVE-20FUNCTION-29 "(MGL-PAX:RESOLVE FUNCTION)"
  [e2a1]: #x-28MGL-PAX-3A-40MGL-PAX-DOCUMENTATION-PRINTER-VARIABLES-20MGL-PAX-3ASECTION-29 "Documentation Printer Variables"
  [e65c]: #x-28MGL-PAX-3A-40MGL-PAX-WORLD-20MGL-PAX-3ASECTION-29 "PAX World"
  [e9e9]: #x-28MGL-PAX-3ALOCATE-AND-FIND-SOURCE-20GENERIC-FUNCTION-29 "(MGL-PAX:LOCATE-AND-FIND-SOURCE GENERIC-FUNCTION)"
  [eff4]: #x-28MGL-PAX-3A-40MGL-PAX-EMACS-INTEGRATION-20MGL-PAX-3ASECTION-29 "Emacs Integration"
  [f1a0]: #x-28METHOD-20-28MGL-PAX-3ALOCATIVE-29-29 "(METHOD (MGL-PAX:LOCATIVE))"
  [f3b7]: #x-28MGL-PAX-3ALOCATE-ERROR-20FUNCTION-29 "(MGL-PAX:LOCATE-ERROR FUNCTION)"
  [f901]: #x-28MGL-PAX-3A-2ADOCUMENT-LINK-SECTIONS-2A-20-28VARIABLE-29-29 "(MGL-PAX:*DOCUMENT-LINK-SECTIONS* (VARIABLE))"
  [fde0]: #x-28MGL-PAX-3A-2ADOCUMENT-MAX-NUMBERING-LEVEL-2A-20-28VARIABLE-29-29 "(MGL-PAX:*DOCUMENT-MAX-NUMBERING-LEVEL* (VARIABLE))"
  [ff5c]: #x-28MGL-PAX-3APAX-2EEL-20-28MGL-PAX-3AINCLUDE-20-23P-22-2Fhome-2Fmelisgl-2Fown-2Fmgl-pax-2Fsrc-2Fpax-2Eel-22-20-3AHEADER-NL-20-22-60-60-60elisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29 "(MGL-PAX:PAX.EL (MGL-PAX:INCLUDE #P\"/home/melisgl/own/mgl-pax/src/pax.el\" :HEADER-NL \"```elisp\" :FOOTER-NL \"```\"))"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
