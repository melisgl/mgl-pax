<a id="x-28MGL-PAX-3A-40PAX-MANUAL-20MGL-PAX-3ASECTION-29"></a>

# PAX Manual

## Table of Contents

- [1 Introduction][685e]
- [2 Emacs Setup][8541]
- [3 Links and Systems][ba74]
    - [3.1 The mgl-pax ASDF System][6fdb]
    - [3.2 The mgl-pax/full ASDF System][d761]
- [4 Background][f74b]
- [5 Basics][94c7]
    - [5.1 Parsing][378f]
- [6 PAX Locatives][292a]
- [7 Navigating Sources in Emacs][3386]
    - [7.1 The mgl-pax/navigate ASDF System][f155]
- [8 Generating Documentation][2c93]
    - [8.1 The `DOCUMENT` Function][dc0a]
        - [8.1.1 Documentables][2e45]
        - [8.1.2 Return Values][7dc7]
        - [8.1.3 Pages][9c7d]
        - [8.1.4 Package and Readtable][ab7e]
    - [8.2 The mgl-pax/document ASDF System][4bb8]
    - [8.3 Browsing Live Documentation][a595]
        - [8.3.1 PAX URLs][1e80]
        - [8.3.2 Apropos][b7fc]
        - [8.3.3 Emacs Setup for Browsing][9a7b]
        - [8.3.4 Browsing with w3m][83d5]
        - [8.3.5 Browsing with Other Browsers][c434]
    - [8.4 Markdown Support][c2d3]
        - [8.4.1 Markdown in Docstrings][7bf5]
        - [8.4.2 Syntax Highlighting][bc83]
        - [8.4.3 MathJax][a17d]
    - [8.5 Codification][f1ab]
    - [8.6 Linking to Code][1865]
        - [8.6.1 Specified Locative][8996]
        - [8.6.2 Unspecified Locative][524e]
        - [8.6.3 Explicit and Autolinking][b3cc]
        - [8.6.4 Preventing Autolinking][8c16]
        - [8.6.5 Unresolvable Links][b372]
        - [8.6.6 Suppressed Links][e2e8]
        - [8.6.7 Local References][4c96]
    - [8.7 Linking to the Hyperspec][7cc3]
    - [8.8 Linking to Sections][22c2]
    - [8.9 Miscellaneous Variables][7c82]
    - [8.10 Utilities for Generating Documentation][1b1b]
        - [8.10.1 HTML Output][36e1]
        - [8.10.2 Github Workflow][dff6]
        - [8.10.3 PAX World][1281]
    - [8.11 Overview of Escaping][2634]
    - [8.12 Output Details][af6f]
    - [8.13 Documentation Generation Implementation Notes][d1ca]
- [9 Transcripts][6300]
    - [9.1 The mgl-pax/transcribe ASDF System][5825]
    - [9.2 Transcribing with Emacs][f5bd]
    - [9.3 Transcript API][9dbc]
    - [9.4 Transcript Consistency Checking][f47d]
        - [9.4.1 Finer-Grained Consistency Checks][6e18]
        - [9.4.2 Controlling the Dynamic Environment][6b59]
        - [9.4.3 Utilities for Consistency Checking][8423]
- [10 Writing Extensions][c4ce]
    - [10.1 Adding New Locatives][54d8]
    - [10.2 Locative Aliases][0fa3]
    - [10.3 Extending `DOCUMENT`][574a]
    - [10.4 Sections][8a58]
    - [10.5 Glossary Terms][d1dc]

###### \[in package MGL-PAX with nicknames PAX\]
<a id="x-28MGL-PAX-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>

## 1 Introduction

*What if documentation really lived in the code?*

Docstrings are already there. If some narrative glued them together,
we'd be able develop and explore the code along with the
documentation due to their physical proximity. The main tool that
PAX provides for this is [`DEFSECTION`][72b4]:

```
(defsection @foo-random-manual (:title "Foo Random manual")
  "Foo Random is a random number generator library."
  (foo-random-state class)
  (uniform-random function)
  (@foo-random-examples section))
```

Like this one, sections can have docstrings and
[references][5225] to
definitions (e.g. `(UNIFORM-RANDOM FUNCTION)`). These docstrings and
references are the glue. To support interactive development, PAX

- makes [SLIME][6be7]'s [`M-.`][cb15] work with references and

- adds a documentation browser.

See [Emacs Setup][8541].

Beyond interactive workflows, [Generating Documentation][2c93] from
sections and all the referenced items in Markdown or HTML format is
also implemented.

With the simplistic tools provided, one may emphasize the narrative
as with Literate Programming, but documentation is generated from
code, not vice versa, and there is no support for chunking.

*Code is first, code must look pretty, documentation is code*.

##### Docstrings

PAX automatically recognizes and [marks up code][f1ab] with
backticks and [links code][1865] to their definitions.
Take, for instance, SBCL's [`ABORT`][479a] function, whose docstring is
written in the usual style, uppercasing names of symbols:

```
(docstring #'abort)
=> "Transfer control to a restart named ABORT, signalling a CONTROL-ERROR if
   none exists."
```

Note how in the generated documentation, `ABORT` is set with a
monospace font, while `CONTROL-ERROR` is autolinked:

- \[function\] **ABORT** *\&OPTIONAL CONDITION*

    Transfer control to a restart named `ABORT`, signalling a
    [`CONTROL-ERROR`][6bc0] if none exists.

[6bc0]: http://www.lispworks.com/documentation/HyperSpec/Body/e_contro.htm "CONTROL-ERROR CONDITION"

In the following [transcript][6300], the above output is
rendered from the raw markdown:

```
(document #'abort :format :markdown)
.. - [function] **ABORT** *&OPTIONAL CONDITION*
..
..     Transfer control to a restart named `ABORT`, signalling a [`CONTROL-ERROR`][7c2c] if
..     none exists.
..
..   [7c2c]: http://www.lispworks.com/documentation/HyperSpec/Body/e_contro.htm "CONTROL-ERROR (MGL-PAX:CLHS CONDITION)"
..
```

##### A Complete Example

Here is an example of how it all works together:

```
(mgl-pax:define-package :foo-random
  (:documentation "This package provides various utilities for random.
  See FOO-RANDOM:@FOO-RANDOM-MANUAL.")
  (:use #:common-lisp #:mgl-pax))

(in-package :foo-random)

(defsection @foo-random-manual (:title "Foo Random manual")
  "FOO-RANDOM is a random number generator library inspired by CL:RANDOM.
  Functions such as UNIFORM-RANDOM use *FOO-STATE* and have a
  :RANDOM-STATE keyword arg."
  (foo-random-state class)
  (state (reader foo-random-state))
  "Hey we can also print states!"
  (print-object (method () (foo-random-state t)))
  (*foo-state* variable)
  (gaussian-random function)
  (uniform-random function)
  ;; This is a subsection.
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

Note how `(VARIABLE *FOO-STATE*)` in the [`DEFSECTION`][72b4] form both
exports `*FOO-STATE*` and includes its documentation in
`@FOO-RANDOM-MANUAL`. The symbols [`VARIABLE`][6c83] and
[`FUNCTION`][ba62] are just two instances of [locative][7ac8]s,
which are used in `DEFSECTION` to refer to definitions tied to
symbols.

`(DOCUMENT @FOO-RANDOM-MANUAL)` generates fancy markdown or HTML
output with [automatic markup][f25f] and [autolinks][1865] uppercase [word][d7b0]s
found in docstrings, numbers sections, and creates a table of
contents.

One can even generate documentation for different but related
libraries at the same time with the output going to different files
but with cross-page links being automatically added for symbols
mentioned in docstrings. In fact, this is what [PAX World][1281] does. See
[Generating Documentation][2c93] for some convenience functions to cover
the most common cases.

The [transcript][6300] in the code block tagged with
`cl-transcript` is automatically checked for up-to-dateness when
documentation is generated.

<a id="x-28MGL-PAX-3A-40EMACS-SETUP-20MGL-PAX-3ASECTION-29"></a>

## 2 Emacs Setup

Load `src/mgl-pax.el` in Emacs, and maybe set up some key bindings.

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

For [Browsing Live Documentation][a595], `mgl-pax-browser-function` can be
customized in Elisp. To browse within Emacs, choose
`w3m-browse-url` (see
[w3m](https://emacs-w3m.github.io/info/emacs-w3m.html)), and make
sure both the w3m binary and the w3m Emacs package are installed. On
Debian, simply install the `w3m-el` package. With other browser
functions, a [HUNCHENTOOT][1d5a] web server is started.

See [Navigating Sources in Emacs][3386], [Generating Documentation][2c93] and
[Transcribing with Emacs][f5bd] for how to use the relevant features.

<a id="x-28MGL-PAX-3AINSTALL-PAX-ELISP-20FUNCTION-29"></a>

- [function] **INSTALL-PAX-ELISP** *TARGET-DIR*

    Copy `mgl-pax.el` distributed with this package to `TARGET-DIR`.

<a id="x-28MGL-PAX-3A-40LINKS-20MGL-PAX-3ASECTION-29"></a>

## 3 Links and Systems

Here is the [official
repository](https://github.com/melisgl/mgl-pax) and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/mgl-pax-manual.html)
for the latest version.

PAX is built on top of the [DRef
library][5225] (bundled in the same repository). See
[DRef's HTML
documentation](http://melisgl.github.io/mgl-pax-world/dref-manual.html)

<a id="x-28-22mgl-pax-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

### 3.1 The mgl-pax ASDF System

- Version: 0.3.0
- Description: Documentation system, browser, generator.
- Long Description: The set of dependencies of the MGL-PAX system is
  kept light, and its heavier dependencies are autoloaded via ASDF
  when the relevant functionality is accessed. See the
  `MGL-PAX/NAVIGATE`, `MGL-PAX/DOCUMENT`, `MGL-PAX/TRANSCRIBE` and
  `MGL-PAX/FULL` systems. To keep deployed code small, client systems
  should declare an ASDF dependency on this system, never on the
  others, which are intended for autoloading and interactive use.
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)
- Homepage: [http://melisgl.github.io/mgl-pax](http://melisgl.github.io/mgl-pax)
- Bug tracker: [https://github.com/melisgl/mgl-pax/issues](https://github.com/melisgl/mgl-pax/issues)
- Source control: [GIT](https://github.com/melisgl/mgl-pax.git)

<a id="x-28-22mgl-pax-2Ffull-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

### 3.2 The mgl-pax/full ASDF System

- Description: MGL-PAX with all features preloaded except MGL-PAX/WEB.
- Long Description: Do not declare a dependency on this system. It
  is autoloaded.
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)


<a id="x-28MGL-PAX-3A-40BACKGROUND-20MGL-PAX-3ASECTION-29"></a>

## 4 Background

As a user, I frequently run into documentation that's incomplete
and out of date, so I tend to stay in the editor and explore the
code by jumping around with [SLIME][6be7]'s [`M-.`][cb15] (`slime-edit-definition`).
As a library author, I spend a great deal of time polishing code but
precious little writing documentation.

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

Armed with this `DEFSECTION`, I soon found myself
organizing code following the flow of user-level documentation and
relegated comments to implementation details entirely. However, some
parts of `DEFSECTION` docstrings were just listings of
all the functions, macros and variables related to the narrative,
and this list was repeated in the [`DEFPACKAGE`][9b43] form complete with
little comments that were like section names. A clear violation of
[OAOO][7d18], one of them had to go, so `DEFSECTION` got a list
of symbols to export.

That was great, but soon I found that the listing of symbols is
ambiguous if, for example, a function, a compiler macro and a class
were named by the same symbol. This did not concern exporting, of
course, but it didn't help readability. Distractingly, on such
symbols, `M-.` was popping up selection dialogs. There were two
birds to kill, and the symbol got accompanied by a type, which was
later generalized into the concept of locatives:

```
(defsection @introduction ()
  "A single line for one man ..."
  (foo class)
  (bar function))
```

After a bit of elisp hacking, `M-.` was smart enough to
disambiguate based on the locative found in the vicinity of the
symbol, and everything was good for a while.

Then, I realized that sections could refer to other sections if
there were a [`SECTION`][672f] locative. Going down that path, I soon began to
feel the urge to generate pretty documentation as all the necessary
information was available in the `DEFSECTION` forms. The design
constraint imposed on documentation generation was that following
the typical style of upcasing symbols in docstrings, there should be
no need to explicitly mark up links: if `M-.` works, then the
documentation generator shall also be able figure out what's being
referred to.

I settled on [markdown][a317] as a reasonably non-intrusive format, and a
few thousand lines later PAX was born. Since then, locatives and
references were factored out into the [DRef][5225]
library to let PAX focus on `M-.` and documentation.

<a id="x-28MGL-PAX-3A-40BASICS-20MGL-PAX-3ASECTION-29"></a>

## 5 Basics

Now let's examine the most important pieces.

<a id="x-28MGL-PAX-3ADEFSECTION-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFSECTION** *NAME (&KEY (PACKAGE '\*PACKAGE\*) (READTABLE '\*READTABLE\*) (EXPORT T) TITLE LINK-TITLE-TO (DISCARD-DOCUMENTATION-P \*DISCARD-DOCUMENTATION-P\*)) &BODY ENTRIES*

    Define a documentation section and maybe export referenced symbols.
    A bit behind the scenes, a global variable with `NAME` is defined and
    is bound to a [`SECTION`][5fac] object. By convention, section names
    start with the character `@`. See [Introduction][685e] for an example.
    
    **Entries**
    
    `ENTRIES` consists of docstrings and references in any order.
    Docstrings are arbitrary strings in markdown format.
    
    References are [`XREF`][1538]s given in the form `(NAME LOCATIVE)`.
    For example, `(FOO FUNCTION)` refers to the function `FOO`, `(@BAR
    SECTION)` says that `@BAR` is a subsection of this
    one. `(BAZ (METHOD () (T T T)))` refers to the default method of the
    three argument generic function `BAZ`. `(FOO FUNCTION)` is
    equivalent to `(FOO (FUNCTION))`. See the DRef [Introduction][ad80]
    for more.
    
    The same name may occur in multiple references, typically with
    different locatives, but this is not required.
    
    The references are not [`LOCATE`][8f19]d until documentation is generated, so
    they may refer to things yet to be defined.
    
    **Exporting**
    
    If `EXPORT` is true (the default), `NAME` and the [name][88cf]s of references
    among `ENTRIES` which are [`SYMBOL`][e5af]s are candidates for exporting. A
    candidate symbol is exported if
    
    - it is [accessible][3473] in `PACKAGE`, and
    
    - there is a reference to it in the section being defined which is
      approved by [`EXPORTABLE-REFERENCE-P`][e51f].
    
    See [`DEFINE-PACKAGE`][63f3] if you use the export feature. The idea with
    confounding documentation and exporting is to force documentation of
    all exported symbols.
    
    **Misc**
    
    `TITLE` is a string containing markdown or `NIL`. If non-`NIL`, it
    determines the text of the heading in the generated output.
    `LINK-TITLE-TO` is a reference given as an `(NAME LOCATIVE)` pair or
    `NIL`, to which the heading will link when generating HTML. If not
    specified, the heading will link to its own anchor.
    
    When `DISCARD-DOCUMENTATION-P` (defaults to [`*DISCARD-DOCUMENTATION-P*`][730f])
    is true, `ENTRIES` will not be recorded to save memory.

<a id="x-28MGL-PAX-3A-2ADISCARD-DOCUMENTATION-P-2A-20VARIABLE-29"></a>

- [variable] **\*DISCARD-DOCUMENTATION-P\*** *NIL*

    The default value of [`DEFSECTION`][72b4]'s `DISCARD-DOCUMENTATION-P` argument.
    One may want to set `*DISCARD-DOCUMENTATION-P*` to true before
    building a binary application.

<a id="x-28MGL-PAX-3ADEFINE-PACKAGE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-PACKAGE** *PACKAGE &REST OPTIONS*

    This is like [`CL:DEFPACKAGE`][9b43] but silences warnings and errors
    signalled when the redefined package is at variance with the current
    state of the package. Typically this situation occurs when symbols
    are exported by calling [`EXPORT`][0c4f] (as is the case with [`DEFSECTION`][72b4]) as
    opposed to adding `:EXPORT` forms to the `DEFPACKAGE` form and the
    package definition is subsequently reevaluated. See the section on
    [package variance](http://www.sbcl.org/manual/#Package-Variance) in
    the SBCL manual.
    
    The bottom line is that if you rely on `DEFSECTION` to do the
    exporting, then you'd better use `DEFINE-PACKAGE`.

<a id="x-28MGL-PAX-3ADEFINE-GLOSSARY-TERM-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-GLOSSARY-TERM** *NAME (&KEY TITLE URL (DISCARD-DOCUMENTATION-P \*DISCARD-DOCUMENTATION-P\*)) &OPTIONAL DOCSTRING*

    Define a global variable with `NAME`, and set it to a [`GLOSSARY-TERM`][8251] object. `TITLE`, `URL` and `DOCSTRING` are markdown strings or
    `NIL`. Glossary terms are [`DOCUMENT`][432c]ed in the lightweight bullet +
    locative + name/title style. See the glossary entry [name][88cf] for an
    example.
    
    When a glossary term is linked to in documentation, its `TITLE` will
    be the link text instead of the name of the symbol (as with
    [`SECTION`][5fac]s).
    
    Glossary entries with a non-`NIL` `URL` are like external links: they
    are linked to their `URL` in the generated documentation. These offer
    a more reliable alternative to using markdown reference links and
    are usually not included in `SECTION`s.
    
    When `DISCARD-DOCUMENTATION-P` (defaults to [`*DISCARD-DOCUMENTATION-P*`][730f])
    is true, `DOCSTRING` will not be recorded to save memory.

<a id="x-28MGL-PAX-3A-40PARSING-20MGL-PAX-3ASECTION-29"></a>

### 5.1 Parsing

When [Navigating Sources in Emacs][3386] or [Generating Documentation][2c93], references
are parsed from the buffer content or docstrings, respectively. In
either case, [name][88cf]s are extracted from [word][d7b0]s and then turned into
[DRef names][5fc4] to form [DRef references][43bd] maybe with locatives found next to the [word][d7b0].

<a id="x-28MGL-PAX-3A-40WORD-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **word**

    A *word* is a string from which we want to extract a [name][88cf]. When
    [Navigating][3386], the word is
    `slime-symbol-at-point`. When [Generating Documentation][2c93], it is a
    non-empty string between whitespace characters in a docstring.

<a id="x-28MGL-PAX-3A-40NAME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **name**

    A *name* is a string that denotes a possible [DRef
     name][5fc4] (e.g. an [`INTERN`][b4f0]ed [`SYMBOL`][e5af], the name of a [`PACKAGE`][1d5a]
    or an [`ASDF:SYSTEM`][c097]). Names are constructed from [word][d7b0]s by trimming
    some prefixes and suffixes. For a given word, multiple candidate
    names are considered in the following order.
    
    1. The entire word.
    
    2. Trimming the characters \`#\<{;"'\`\` from the left of the word.
    
    3. Trimming the characters \`,;:.>}"'\`\` from the right of the word.
    
    4. Trimming both of the previous two at the same time.
    
    5. From the result of 4., further removing some plural markers.
    
    6. From the result of 4., further removing non-uppercase prefixes
       and suffixes.
    
    For example, when `M-.` is pressed while point is over
    `nonREADable.`, the last word of the sentence `It may be
    nonREADable.`, the following names are considered until one is found
    with a definition:
    
    1. The entire word, `"nonREADable."`.
    
    2. Trimming left does not produce a new word.
    
    3. Trimming right removes the dot and gives `"nonREADable"`.
    
    4. Trimming both is the same as trimming right.
    
    5. No plural markers are found.
    
    6. The lowercase prefix and suffix is removed around the uppercase
       core, giving `"READ"`. This has a definition, which \`M-.' will
       visit.
    
    The exact rules for steps 5. and 6. are the following.
    
    - If a [word][d7b0] ends with what looks like a plural
    marker (case-insensitive), then a [name][88cf] is created by removing it.
    For example, from the [word][d7b0] `BUSES` the plural marker `ES` is
    removed to produce the [name][88cf] `BUS`. The list of plural markers
    considered is `S` (e.g. [`CARS`][d5a2]), `ES` (e.g. `BUSES`), `SES` (e.g.
    `GASSES`), `ZES` (e.g. `FEZZES`), and `REN` (e.g. `CHILDREN`).
    
    - From a [codifiable][b89a] [word][d7b0], a [name][88cf] is created by removing the prefix
    before the first and the suffix after the last uppercase character
    if they contain at least one lowercase character.


<a id="x-28MGL-PAX-3A-40PAX-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

## 6 PAX Locatives

To the [Locative Types][bf0f] defined by DRef, PAX adds a few of its own.

<a id="x-28MGL-PAX-3ASECTION-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **SECTION**

    Refers to a [`SECTION`][5fac] defined by [`DEFSECTION`][72b4].
    
    `SECTION` is [`EXPORTABLE-LOCATIVE-TYPE-P`][c930] but not exported by
    default (see [`EXPORTABLE-REFERENCE-P`][e51f]).

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **GLOSSARY-TERM**

    Refers to a [`GLOSSARY-TERM`][8251] defined by [`DEFINE-GLOSSARY-TERM`][8ece].
    
    `GLOSSARY-TERM` is [`EXPORTABLE-LOCATIVE-TYPE-P`][c930] but not exported by
    default (see [`EXPORTABLE-REFERENCE-P`][e51f]).

<a id="x-28MGL-PAX-3ADISLOCATED-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **DISLOCATED**

    Refers to a symbol in a non-specific context. Useful for preventing
    [autolinking][b3cc]. For example, if
    there is a function called `FOO` then
    
        `FOO`
    
    will be linked (if [`*DOCUMENT-LINK-CODE*`][d9ee]) to its definition. However,
    
        [`FOO`][dislocated]
    
    will not be. With a dislocated locative, [`LOCATE`][8f19] always fails with a
    [`LOCATE-ERROR`][6334] condition. Also see [Preventing Autolinking][8c16].
    
    `DISLOCATED` references do not [`RESOLVE`][63b4].

<a id="x-28MGL-PAX-3AARGUMENT-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **ARGUMENT**

    An alias for [`DISLOCATED`][e391], so that one can refer to an argument of
    a macro without accidentally linking to a class that has the same
    name as that argument. In the following example,
    `FORMAT` may link to `CL:FORMAT` (if we generated
    documentation for it):
    
    ```
    "See FORMAT in DOCUMENT."
    ```
    
    Since `ARGUMENT` is a locative, we can prevent that linking by writing:
    
    ```
    "See the FORMAT argument of DOCUMENT."
    ```
    
    `ARGUMENT` references do not [`RESOLVE`][63b4].

<a id="x-28MGL-PAX-3AINCLUDE-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **INCLUDE** *SOURCE &KEY LINE-PREFIX HEADER FOOTER HEADER-NL FOOTER-NL*

    This pseudolocative refers to a region of a file. `SOURCE` can be a
    [`STRING`][b93c] or a [`PATHNAME`][0317], in which case the whole file
    is being pointed to, or it can explicitly supply `START`, `END`
    locatives. `INCLUDE` is typically used to include non-lisp files in
    the documentation (say markdown or Elisp as in the next example) or
    regions of Lisp source files. This can reduce clutter and
    duplication.
    
    ```
    (defsection @example-section ()
      (mgl-pax.el (include #.(asdf:system-relative-pathname
                              :mgl-pax "src/mgl-pax.el")
                           :header-nl "```elisp" :footer-nl "```"))
      (foo-example (include (:start (dref-ext:make-source-location function)
                             :end (dref-ext:source-location-p function))
                            :header-nl "```"
                            :footer-nl "```")))
    ```
    
    In the above example, when documentation is generated, the entire
    `src/mgl-pax.el` file is included in the markdown output surrounded
    by the strings given as `HEADER-NL` and `FOOTER-NL`. The documentation
    of `FOO-EXAMPLE` will be the region of the file from the
    [`SOURCE-LOCATION`][32da] of the `START` reference (inclusive) to the
    `SOURCE-LOCATION` of the `END` reference (exclusive). If only one of
    `START` and `END` is specified, then they default to the beginning and
    end of the file, respectively.
    
    Since `START` and `END` are literal references, pressing `M-.` on
    `PAX.EL` will open the `src/mgl-pax.el` file and put the cursor on
    its first character. `M-.` on `FOO-EXAMPLE` will go to the source
    location of the `FOO` function.
    
    With the [`LAMBDA`][4796] locative, one can specify positions in arbitrary
    files as in, for example, [Emacs Setup for Browsing][9a7b].
    
    - `SOURCE` is either an absolute pathname designator or a list
      matching the [destructuring lambda list][6067] `(&KEY START END)`,
      where `START` and `END` must be `NIL` or `(<NAME> <LOCATIVE>)`
      lists (not evaluated) like a [`DEFSECTION`][72b4] entry. Their
      `SOURCE-LOCATION`s constitute the bounds of the region of the file
      to be included. Note that the file of the source location of `START`
      and `END` must be the same. If `SOURCE` is a pathname designator, then
      it must be absolute so that the locative is context independent.
    
    - If specified, `LINE-PREFIX` is a string that's prepended to each
      line included in the documentation. For example, a string of four
      spaces makes markdown think it's a code block.
    
    - `HEADER` and `FOOTER`, if non-`NIL`, are printed before the included
      string.
    
    - `HEADER-NL` and `FOOTER-NL`, if non-`NIL`, are printed between two
      [`FRESH-LINE`][3808] calls.
    
    `INCLUDE` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930], and `INCLUDE` references do
    not [`RESOLVE`][63b4].

<a id="x-28MGL-PAX-3ADOCSTRING-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **DOCSTRING**

    `DOCSTRING` is a pseudolocative for including the parse tree of the
    markdown [`DOCSTRING`][affc] of a definition in the parse tree of
    a docstring when generating documentation. It has no source location
    information and only works as an explicit link. This construct is
    intended to allow docstrings to live closer to their implementation,
    which typically involves a non-exported definition.
    
    ```common-lisp
    (defun div2 (x)
      "X must be [even* type][docstring]."
      (/ x 2))
    
    (deftype even* ()
      "an even integer"
      '(satisfies evenp))
    
    (document #'div2)
    .. - [function] DIV2 X
    ..
    ..     X must be an even integer.
    ..
    ```
    
    There is no way to [`LOCATE`][8f19] `DOCSTRING`s, so nothing to [`RESOLVE`][63b4] either.

<a id="x-28GO-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **GO** *(NAME LOCATIVE)*

    Redirect to a definition in the context of the [reference][43bd]
    designated by `NAME` and `LOCATIVE`. This pseudolocative is intended for
    things that have no explicit global definition.
    
    As an example, consider this part of a hypothetical documentation of
    CLOS:
    
        (defsection @clos ()
          (defmethod macro)
          (call-next-method (go (defmethod macro))))
    
    The `GO` reference exports the symbol [`CALL-NEXT-METHOD`][6832] and also
    produces a terse redirection message in the documentation.
    
    `GO` behaves as described below.
    
    - A `GO` reference [`RESOLVE`][63b4]s to what `NAME` with `LOCATIVE` resolves to:
    
        ```common-lisp
        (resolve (dref 'xxx '(go (print function))))
        ==> #<FUNCTION PRINT>
        ```
    
    - The [`DOCSTRING`][affc] of a `GO` reference is `NIL`.
    
    - [`SOURCE-LOCATION`][32da] (thus `M-.`) returns the source location of the
      embedded reference:
    
        ```common-lisp
        (equal (source-location (dref 'xxx '(go (print function))))
               (source-location (dref 'print 'function)))
        => T
        ```


<a id="x-28MGL-PAX-3ACLHS-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **CLHS** *&OPTIONAL NESTED-LOCATIVE*

    Refers to sections or definitions in the Common Lisp Hyperspec.
    These have no source location so `M-.` will not work. What works
    is linking in documentation, including [Browsing Live Documentation][a595].
    The generated links are relative to [`*DOCUMENT-HYPERSPEC-ROOT*`][f585] and
    work even if [`*DOCUMENT-LINK-TO-HYPERSPEC*`][875e] is `NIL`.
    
    - *definitions*: These are typically unnecessary as [`DOCUMENT`][432c] will
      produce the same link for e.g. `PPRINT`, `[PPRINT][function]`,
      or `[PPRINT][]` if `*DOCUMENT-LINK-TO-HYPERSPEC*` is non-`NIL` and the
      [`PPRINT`][6af6] function in the running Lisp is not being `DOCUMENT`ed. When
      [Browsing Live Documentation][a595], a slight difference is that
      everything is being `DOCUMENT`ed, so using the `CLHS` link bypasses
      the page with the definition in the running Lisp.
    
        - *unambiguous*: `[pprint][clhs]` ([pprint][6af6])
    
        - *ambiguous*: `[function][clhs]` ([function][aeb6])
    
        - *explicit*: `[function][(clhs class)]` ([function][119e])
    
    - *glossary terms* (case-insensitive):
    
        - `[lambda list][(clhs glossary-term)]`
          ([lambda list][98ff])
    
    - *issues*:
    
        - `[ISSUE:AREF-1D][clhs]` ([ISSUE:AREF-1D][63ef])
    
        - `[ISSUE:AREF-1D][(clhs section)]` ([ISSUE:AREF-1D][63ef])
    
    - *issue summaries*: These render
       as ([SUMMARY:CHARACTER-PROPOSAL:2-6-5][935f]):
    
        - `[SUMMARY:CHARACTER-PROPOSAL:2-6-5][clhs]`
    
        - `[SUMMARY:CHARACTER-PROPOSAL:2-6-5][(clhs section)]`
    
        Since these summary ids are not particularly reader friendly,
        the alternative form of the [Specified Locative][8996] may be used:
    
        - `[see this][SUMMARY:CHARACTER-PROPOSAL:2-6-5 (clhs
          section)]` ([see this][935f])
    
    - *sections*:
    
        - *by section number*: `[3.4][clhs]` or `[3.4][(clhs
           section)]` ([3.4][e442])
    
        - *by section title* (case-insensitive, substring match):
           `[lambda lists][clhs]` or `[lambda lists][(clhs
           section)]` ([lambda lists][e442])
    
        - *by filename*: `[03_d][clhs]` or `[03_d][(clhs
           section)]` ([03\_d][e442])
    
    As the above examples show, the `NESTED-LOCATIVE` argument of the [`CLHS`][ed5f]
    locative may be omitted. In that case, definitions, glossary terms,
    issues, issue summaries, and sections are considered in that order.
    Sections are considered last because a substring of a section title
    can be matched by chance easily.
    
    All examples so far used [explicit][b3cc]
    links. Autolinking also works if the [name][88cf] is marked up as code or
    is [codified][f1ab] (e.g. in `COS clhs` ([`COS`][c4a3] clhs).
    
    As mentioned above, `M-.` does not do anything over `CLHS`
    references. Slightly more usefully, the [live documentation
    browser][a595] understands `CLHS` links so one
    can enter inputs like `3.4 clhs`, `"lambda list" clhs` or `error (clhs
    function)`.
    
    `CLHS` references do not [`RESOLVE`][63b4].

<a id="x-28MGL-PAX-3A-40NAVIGATING-IN-EMACS-20MGL-PAX-3ASECTION-29"></a>

## 7 Navigating Sources in Emacs

Integration into [SLIME][6be7]'s [`M-.`][cb15] (`slime-edit-definition`) allows
one to visit the [`SOURCE-LOCATION`][32da] of a definition([`0`][d930] [`1`][7e92]).

The definition is either determined from the buffer content at point
or is prompted. If prompted, then the format is `<NAME> <LOCATIVE>`,
where the locative may be omitted to recover stock Slime behaviour.

When determining the definition from the buffer contents,
`slime-symbol-at-point` is parsed as a [word][d7b0], then candidate
locatives are looked for before and after that word. Thus, if a
locative is the previous or the next expression around the symbol of
interest, then `M-.` will go straight to the definition which
corresponds to the locative. If that fails, `M-.` will try to find
the definitions in the normal way, which may involve popping up an
xref buffer and letting the user interactively select one of
possible definitions.

In the following examples, when the cursor is on one of the
characters of `FOO` or just after `FOO`, pressing `M-.` will visit
the definition of function `FOO`:

    function foo
    foo function
    (function foo)
    (foo function)

In particular, [reference][43bd]s in a [`DEFSECTION`][72b4] form are in (`NAME`
[`LOCATIVE`][0b3a]) format so `M-.` will work just fine there.

Just like vanilla `M-.`, this works in comments and docstrings. In
the next example, pressing `M-.` on `FOO` will visit `FOO`'s
default method:

```
;; See RESOLVE* (method () (dref)) for how this all works.
```

With a prefix argument (`C-u M-.`), one can enter a symbol plus a
locative separated by whitespace to preselect one of the
possibilities.

The `M-.` extensions can be enabled by loading `src/mgl-pax.el`.
See [Emacs Setup][8541]. In addition, the Elisp command
`mgl-pax-edit-parent-section` visits the source location of the
section containing the definition with `point` in it. See
[Browsing Live Documentation][a595].

<a id="x-28-22mgl-pax-2Fnavigate-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

### 7.1 The mgl-pax/navigate ASDF System

- Description: Slime `M-.` support for MGL-PAX.
- Long Description: Autoloaded by Slime's `M-.` when `src/pax.el` is
  loaded. See [Navigating Sources in Emacs][3386].
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)


<a id="x-28MGL-PAX-3A-40GENERATING-DOCUMENTATION-20MGL-PAX-3ASECTION-29"></a>

## 8 Generating Documentation

<a id="x-28MGL-PAX-3A-40DOCUMENT-FUNCTION-20MGL-PAX-3ASECTION-29"></a>

### 8.1 The `DOCUMENT` Function

<a id="x-28MGL-PAX-3ADOCUMENT-20FUNCTION-29"></a>

- [function] **DOCUMENT** *DOCUMENTABLE &KEY (STREAM T) PAGES (FORMAT :PLAIN)*

    Write `DOCUMENTABLE` in `FORMAT` to `STREAM` diverting some output to `PAGES`.
    `FORMAT` is a 3BMD output format (currently one of `:MARKDOWN`,
    `:HTML` and `:PLAIN`). `STREAM` may be a [`STREAM`][d5a9] object, `T` or `NIL`
    as with [`CL:FORMAT`][ad78].
    
    To look up the documentation of the [`DOCUMENT`][432c] function itself:
    
        (document #'document)
    
    The same with fancy markup:
    
        (document #'document :format :markdown)
    
    To document a [`SECTION`][5fac]:
    
        (document @pax-manual)
    
    To generate the documentation for separate libraries with automatic
    cross-links:
    
        (document (list pax::@pax-manual dref::@dref-manual) :format :markdown)
    
    See [Utilities for Generating Documentation][1b1b] for more.
    
    Definitions that do not define a first-class object are supported
    via [DRef][5225]:
    
        (document (dref:locate 'foo 'type))
    
    There are quite a few special variables that affect how output is
    generated, see [Codification][f1ab], [Linking to Code][1865],
    [Linking to Sections][22c2], and
    [Miscellaneous Variables][7c82].
    
    For the details, see the following sections, starting with
    [Documentables][2e45]. Also see [Writing Extensions][c4ce] and [`DOCUMENT-OBJECT*`][8269].

<a id="x-28MGL-PAX-3A-40DOCUMENTABLES-20MGL-PAX-3ASECTION-29"></a>

#### 8.1.1 Documentables

- The `DOCUMENTABLE` argument may be a [`DREF`][d930] or anything else
  that is [`LOCATE`][8f19]able. This includes non-`DREF` [`XREF`][1538]s and
  first-class objects such as [`FUNCTION`][119e]s.

- If `DOCUMENTABLE` is a string, then it is processed like a docstring
  in [`DEFSECTION`][72b4]. That is, with [docstring sanitization][7bf5], [Codification][f1ab], and linking (see
  [Linking to Code][1865], [Linking to the Hyperspec][7cc3]).

- Finally, `DOCUMENTABLE` may be a nested list of `LOCATE`able objects
  and docstrings. The structure of the list is unimportant. The
  objects in it are documented in depth-first order.


<a id="x-28MGL-PAX-3A-40DOCUMENT-RETURN-20MGL-PAX-3ASECTION-29"></a>

#### 8.1.2 Return Values

If `PAGES` are `NIL`, then [`DOCUMENT`][432c] - like [`CL:FORMAT`][ad78] - returns a
string (when [`STREAM`][d5a9] is `NIL`) else `NIL`.

If `PAGES`, then a list of output designators are returned, one for
each non-empty page (to which some output has been written), which
are determined as follows.

- The string itself if the output was to a string.

- The stream if the output was to a stream.

- The pathname of the file if the output was to a file.

If the default page given by the `STREAM` argument of `DOCUMENT` was
written to, then its output designator is the first element of the
returned list. The rest of the designators correspond to the
non-empty pages in the `PAGES` argument of `DOCUMENT` in that order.

<a id="x-28MGL-PAX-3A-40PAGES-20MGL-PAX-3ASECTION-29"></a>

#### 8.1.3 Pages

The `PAGES` argument of [`DOCUMENT`][432c] is to create multi-page documents
by routing some of the generated output to files, strings or
streams. `PAGES` is a list of page specification elements. A page spec
is a [property list][b18e] with keys `:OBJECTS`, `:OUTPUT`,
`:URI-FRAGMENT`, `:SOURCE-URI-FN`, `:HEADER-FN` and `:FOOTER-FN`. `OBJECTS` is
a list of objects (references are allowed but not required) whose
documentation is to be sent to `:OUTPUT`.

Documentation is initially sent to a default stream (the `STREAM`
argument of `DOCUMENT`), but output is redirected if the thing being
currently documented is the `:OBJECT` of a `PAGE-SPEC`.

`:OUTPUT` can be a number things:

- If it's `NIL`, then output will be collected in a string.

- If it's `T`, then output will be sent to [`*STANDARD-OUTPUT*`][e7ee].

- If it's a stream, then output will be sent to that stream.

- If it's a list whose first element is a string or a pathname, then
  output will be sent to the file denoted by that and the rest of
  the elements of the list are passed on to [`CL:OPEN`][6547]. One extra
  keyword argument is `:ENSURE-DIRECTORIES-EXIST`. If it's true,
  [`ENSURE-DIRECTORIES-EXIST`][876d] will be called on the pathname before
  it's opened.

Note that even if `PAGES` is specified, `STREAM` acts as a catch all,
absorbing the generated documentation for references not claimed by
any pages.

`:HEADER-FN`, if not `NIL`, is a function of a single stream argument,
which is called just before the first write to the page. Since
`:FORMAT` `:HTML` only generates HTML fragments, this makes it possible
to print arbitrary headers, typically setting the title, CSS
stylesheet, or charset.

`:FOOTER-FN` is similar to `:HEADER-FN`, but it's called after the last
write to the page. For HTML, it typically just closes the body.

`:URI-FRAGMENT` is a string such as `"doc/manual.html"` that specifies
where the page will be deployed on a webserver. It defines how links
between pages will look. If it's not specified and `:OUTPUT` refers to
a file, then it defaults to the name of the file. If `:URI-FRAGMENT`
is `NIL`, then no links will be made to or from that page.

Finally, `:SOURCE-URI-FN` is a function of a single, `REFERENCE`
argument. If it returns a value other than `NIL`, then it must be a
string representing an URI. If [`FORMAT`][ad78] is `:HTML` and
[`*DOCUMENT-MARK-UP-SIGNATURES*`][8fb6] is true, then the locative as
displayed in the signature will be a link to this `URI`. See
[`MAKE-GIT-SOURCE-URI-FN`][587f].

`PAGES` may look something like this:

```
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
   :output "build/tmp/pax-extension-api.html"
   ;; However, on the web server html files will be at this
   ;; location relative to some common root, so override the
   ;; default:
   :uri-fragment "doc/dev/pax-extension-api.html"
   ;; Set html page title, stylesheet, charset.
   :header-fn 'write-html-header
   ;; Just close the body.
   :footer-fn 'write-html-footer)
  ;; Catch references that were not reachable from the above. It
  ;; is important for this page spec to be last.
  (:objects (, @pax-manual)
   :output "build/tmp/manual.html"
   ;; Links from the extension api page to the manual page will
   ;; be to ../user/pax-manual#<anchor>, while links going to
   ;; the opposite direction will be to
   ;; ../dev/pax-extension-api.html#<anchor>.
   :uri-fragment "doc/user/pax-manual.html"
   :header-fn 'write-html-header
   :footer-fn 'write-html-footer))
```


<a id="x-28MGL-PAX-3A-40PACKAGE-AND-READTABLE-20MGL-PAX-3ASECTION-29"></a>

#### 8.1.4 Package and Readtable

While generating documentation, symbols may be read (e.g. from
docstrings) and printed. What values of [`*PACKAGE*`][5ed1] and [`*READTABLE*`][b79a]
are used is determined separately for each definition being
documented.

- If the values of `*PACKAGE*` and `*READTABLE*` in effect at the time
  of definition were captured (e.g. by [`DEFINE-LOCATIVE-TYPE`][b6c4] and
  [`DEFSECTION`][72b4]), then they are used.

- Else, if the definition has a [Home Section][bdd5] (see below), then the
  home section's [`SECTION-PACKAGE`][a5b1] and [`SECTION-READTABLE`][88a7] are used.

- Else, if the definition has an argument list, then the package of
  the first argument that's not external in any package is used.

- Else, if the definition is [name][5fc4]d by a symbol, then its
  [`SYMBOL-PACKAGE`][e5ab] is used, and `*READTABLE*` is set to the standard
  readtable `(NAMED-READTABLES:FIND-READTABLE :COMMON-LISP)`.

- Else, `*PACKAGE*` is set to the `CL-USER` package and `*READTABLE*` to
  the standard readtable.

The values thus determined come into effect after the name itself is
printed, for printing of the arglist and the docstring.

    CL-USER> (pax:document #'foo)
    - [function] FOO <!> X Y &KEY (ERRORP T)
    
        Do something with X and Y.

In the above, the `<!>` marks the place where `*PACKAGE*` and
`*READTABLE*` are bound.

<a id="x-28MGL-PAX-3A-40HOME-SECTION-20MGL-PAX-3ASECTION-29"></a>

##### Home Section

The home section of an object is a [`SECTION`][5fac] that contains the
object's definition in its [`SECTION-ENTRIES`][d850] or `NIL`. In the
overwhelming majority of cases there should be at most one
containing section.

If there are multiple containing sections, the following apply.

- If the [name][5fc4] of the definition is a non-keyword symbol, only
  those containing sections are considered whose package is closest
  to the [`SYMBOL-PACKAGE`][e5ab] of the name, where closest is defined as
  having the longest common prefix between the two [`PACKAGE-NAME`][db68]s.

- If there are multiple sections with equally long matches or the
  name is not a non-keyword symbol, then it's undefined which one is
  the home section.

For example, `(MGL-PAX:DOCUMENT FUNCTION)` is an entry in the
`MGL-PAX::@BASICS` section. Unless another section that contains
it is defined in the MGL-PAX package, the home section is guaranteed
to be `MGL-PAX::@BASICS` because the `SYMBOL-PACKAGE`s of
[`MGL-PAX:DOCUMENT`][432c] and `MGL-PAX::@BASICS` are the same (hence their
common prefix is maximally long).

This scheme would also work, for example, if the [home package][407c]
of `DOCUMENT` were `MGL-PAX/IMPL`, and it were reexported from
`MGL-PAX` because the only way to externally change the home package
would be to define a containing section in a package like
`MGL-PAX/IMP`.

Thus, relying on the package system makes it possible to find the
intended home section of a definition among multiple containing
sections with high probability. However, for names which are not
symbols, there is no package system to advantage of.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-NORMALIZE-PACKAGES-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-NORMALIZE-PACKAGES\*** *T*

    Whether to print `[in package <package-name>]` in the documentation
    when the package changes.

<a id="x-28-22mgl-pax-2Fdocument-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

### 8.2 The mgl-pax/document ASDF System

- Description: Documentation generation support for MGL-PAX.
- Long Description: Do not declare a dependency on this system. It is
  autoloaded. See [Generating Documentation][2c93].
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)


<a id="x-28MGL-PAX-3A-40BROWSING-LIVE-DOCUMENTATION-20MGL-PAX-3ASECTION-29"></a>

### 8.3 Browsing Live Documentation

Documentation can be browsed live in Emacs or with an external
browser. HTML documentation, complete with [Codification][f1ab] and
[links][1865], is generated from docstrings of all kinds
of Lisp definitions and PAX [`SECTION`][5fac]s.

If [Emacs Setup][8541] has been done, the Elisp function `mgl-pax-document`
generates and displays documentation as a single HTML page. For
example, to view the documentation of this very `SECTION`, one can do:

    M-x mgl-pax-document
    View Documentation of: pax::@browsing-live-documentation

If the empty string is entered, and there is no existing w3m buffer
or w3m is not used, then sections registered in [PAX World][1281] are
listed. If there is a w3m buffer, then entering the empty string
displays that buffer.

If we enter `function` instead, then a disambiguation page will be
shown with the documentation of the [`FUNCTION`][119e] class and the [`FUNCTION`][ba62]
locative. One may then follow the links on the page to navigate to a
page with the documentation the desired definition. If you are
browsing live documentation right now, then the disambiguation page
is like this: `FUNCTION`([`0`][119e] [`1`][81f7]). In offline documentation, multiple links
are shown instead as described in [Ambiguous Unspecified Locative][2f82].

Alternatively, a [locative][7ac8] may be entered as part of the
argument to `mgl-pax-document` as in `function class`, which gives
[this result][119e]. Finally, the definition of [`DEFSECTION`][72b4]
in the context of a single-page [PAX Manual][2415] can be
[viewed](pax:pax::@pax-manual#pax:defsection%20pax:macro) by
entering `pax::@pax-manual#pax:defsection pax:macro`.

In interactive use, `mgl-pax-document` defaults to documenting
`slime-symbol-at-point`, possibly with a nearby locative the same
way as in [Navigating Sources in Emacs][3386]. The convenience function
`mgl-pax-document-current-definition` documents the definition with
point in it.

<a id="x-28MGL-PAX-3A-40PAX-URLS-20MGL-PAX-3ASECTION-29"></a>

#### 8.3.1 PAX URLs

A PAX URL consists of a REFERENCE and an optional `FRAGMENT`
part:

    URL = [REFERENCE] ["#" FRAGMENT]

where REFERENCE names either

- a complete [reference][43bd] (e.g. `"pax:section class"`),

- or the [name][88cf] of a reference (e.g. `"pax:section"`), which
  possibly makes what to document ambiguous.


<a id="x-28MGL-PAX-3A-40APROPOS-20MGL-PAX-3ASECTION-29"></a>

#### 8.3.2 Apropos

The Elisp functions `mgl-pax-apropos`, `mgl-pax-apropos-all`, and
`mgl-pax-apropos-package` can display the results of
[`DREF:DREF-APROPOS`][65b4] in the [live documentation browser][a595]. These parallel the functionality of
`slime-apropos`, `slime-apropos-all`, and `slime-apropos-package`.

`DREF:DREF-APROPOS` itself is similar to [`CL:APROPOS-LIST`][7328], but it
supports more flexible matching – e.g. filtering by
[locative type][a11d]s – and returns [DRef
references][43bd].

The returned references are presented in two groups: those with
non-symbol and those with symbol [name][88cf]s. The non-symbol group is
sorted by locative type then by name. The symbol group is sorted by
name then by locative type.

<a id="x-28MGL-PAX-3A-40EMACS-SETUP-FOR-BROWSING-20MGL-PAX-3ASECTION-29"></a>

#### 8.3.3 Emacs Setup for Browsing

Make sure [Emacs Setup][8541] has been done. In particular, set
`mgl-pax-browser-function` to choose between browsing documentation
with [w3m](https://emacs-w3m.github.io/info/emacs-w3m.html) in an
Emacs buffer, or with an external browser plus a web server in the
Lisp image.

In [Emacs Setup][8541], `(mgl-pax-hijack-slime-doc-keys)` was evaluated,
which handles the common case of binding keys. The Elisp definition
is reproduced here for its docstring.

```elisp
(defun mgl-pax-hijack-slime-doc-keys ()
  "Make the following changes to `slime-doc-map' (assuming it's
bound to `C-c C-d').

- `C-c C-d a': `mgl-pax-apropos' (replaces `slime-apropos')
- `C-c C-d z': `mgl-pax-aproposa-all' (replaces `slime-apropos-all')
- `C-c C-d p': `mgl-pax-apropos-package' (replaces `slime-apropos-package')
- `C-c C-d d': `mgl-pax-document' (replaces `slime-describe-symbol')
- `C-c C-d f': `mgl-pax-document' (replaces `slime-describe-function')
- `C-c C-d c': `mgl-pax-current-definition-toggle-view'

Also, regardless of whether `w3m' is available, add this:

- `C-c C-d u': `mgl-pax-edit-parent-section'

In addition, because it can be almost as useful as `M-.', one may
want to give `mgl-pax-document' a more convenient binding such as
`C-.' or `s-.' if you have a Super key. For example, to bind
`C-.' in all Slime buffers:

    (slime-bind-keys slime-parent-map nil '((\"C-.\" mgl-pax-document)))

To bind `C-.' globally:

    (global-set-key (kbd \"C-.\") 'mgl-pax-document)"
  
...)
```

<a id="x-28MGL-PAX-3A-40BROWSING-WITH-W3M-20MGL-PAX-3ASECTION-29"></a>

#### 8.3.4 Browsing with w3m

With [w3m's default key bindings][1743], moving the cursor between links involves
`TAB` and `S-TAB` (or `<up>` and `<down>`). `RET` and `<right>`
follow a link, while `B` and `<left>` go back in history.

In addition, the following PAX-specific key bindings are available:

- `M-.` visits the source location of the definition corresponding
  to the link under the point.

- Invoking `mgl-pax-document` on a section title link will show the
  documentation of that section on its own page.

- `n` moves to the next PAX definition on the page.

- `p` moves to the previous PAX definition on the page.

- `u` follows the first `Up:` link (to the first containing
  [`SECTION`][5fac]) if any.

- `U` is like `u` but positions the cursor at the top of the page.

- `v` visits the source location of the current definition (the one
  under the cursor or the first one above it).

- `V` visits the source location of the first definition on the
  page.


<a id="x-28MGL-PAX-3A-40BROWSING-WITH-OTHER-BROWSERS-20MGL-PAX-3ASECTION-29"></a>

#### 8.3.5 Browsing with Other Browsers

When the value of the Elisp variable `mgl-pax-browser-function`
is not `w3m-browse-url`, requests are served via a web server
started in the running Lisp, and documentation is most likely
displayed in a separate browser window .

By default, `mgl-pax-browser-function` is `nil`, which makes PAX use
`browse-url-browser-function`. You may want to customize the related
`browse-url-new-window-flag` or, for Chrome, set
`browse-url-chrome-arguments` to `("--new-window")`.

In the browser, clicking on the locative on the left of the
object (e.g. in `- [function] PRINT`) will raise and focus the Emacs
window (if Emacs is not in text mode, and also subject to window
manager focus stealing settings), then go to the corresponding
source location. For sections, clicking on the lambda link will do
the same (see [`*DOCUMENT-FANCY-HTML-NAVIGATION*`][6ab0]).

Finally, note that the `URL`s exposed by the web server are subject to
change.

<a id="x-28MGL-PAX-3A-2ABROWSE-HTML-STYLE-2A-20VARIABLE-29"></a>

- [variable] **\*BROWSE-HTML-STYLE\*** *:CHARTER*

    The HTML style to use for browsing live documentation. Affects only
    non-w3m browsers. See [`*DOCUMENT-HTML-DEFAULT-STYLE*`][90fa] for the possible
    values.

<a id="x-28MGL-PAX-3A-40MARKDOWN-SUPPORT-20MGL-PAX-3ASECTION-29"></a>

### 8.4 Markdown Support

The [markdown][a317] in docstrings is processed with the [3BMD][1904] library.

<a id="x-28MGL-PAX-3A-40MARKDOWN-IN-DOCSTRINGS-20MGL-PAX-3ASECTION-29"></a>

#### 8.4.1 Markdown in Docstrings

- Docstrings can be indented in any of the usual styles. PAX
normalizes indentation by stripping the longest run of leading
spaces common to all non-blank lines except the first. The following
two docstrings are equivalent:

        (defun foo ()
          "This is
          indented
          differently")
        
        (defun foo ()
          "This is
        indented
        differently")

- When [Browsing Live Documentation][a595], the page displayed can be of,
say, a single function within what would constitute the offline
documentation of a library. Because markdown reference link
definitions, for example

        [Daring Fireball]: http://daringfireball.net/

    can be defined anywhere, they wouldn't be resolvable in that
    case, their use is discouraged. Currently, only reflink
    definitions in the vicinity of their uses are resolvable. This
    is left intentionally vague because the specifics are subject to
    change.

    See [`DEFINE-GLOSSARY-TERM`][8ece] for a better alternative to markdown
    reference links.

Docstrings of definitions which do not have a [Home Section][bdd5] and are
not [`SECTION`][5fac]s themselves are assumed to have been written with no
knowledge of PAX and to conform to markdown only by accident. These
docstrings are thus sanitized more aggressively.

- Indentation of what looks like blocks of Lisp code is rounded up to
a multiple of 4. More precisely, non-zero indented lines between
blank lines or the docstring boundaries are reindented if the first
non-space character of the first line is an `(` or a `;` character.

- Special HTML characters `<&` are escaped.

- Furthermore, to reduce the chance of inadvertently introducing a
markdown heading, if a line starts with a string of `#` characters,
then the first one is automatically escaped. Thus, the following two
docstrings are equivalent:

        The characters #\Space, #\Tab and
        #Return are in the whitespace group.
    
        The characters #\Space, #\Tab and
        \#Return are in the whitespace group.


<a id="x-28MGL-PAX-3A-40MARKDOWN-SYNTAX-HIGHLIGHTING-20MGL-PAX-3ASECTION-29"></a>

#### 8.4.2 Syntax Highlighting

For syntax highlighting, Github's [fenced code blocks][1322] markdown
extension to mark up code
blocks with triple backticks is enabled so all you need to do is
write:

    ```elisp
    (defun foo ())
    ```

to get syntactically marked up HTML output. Copy `src/style.css`
from PAX and you are set. The language tag, `elisp` in this example,
is optional and defaults to `common-lisp`.

See the documentation of [3BMD][1904] and [Colorize][3076] for the details.

<a id="x-28MGL-PAX-3A-40MATHJAX-20MGL-PAX-3ASECTION-29"></a>

#### 8.4.3 MathJax

Displaying pretty mathematics in TeX format is supported via
MathJax. It can be done inline with `$` like this:

    $\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$

which is displayed as $\int\_0^\infty e^{-x^2}
dx=\frac{\sqrt{\pi}}{2}$, or it can be delimited by `$$` like this:

    $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

to get: $$\int\_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

MathJax will leave code blocks (including those inline with
backticks) alone. Outside code blocks, escape `$` by prefixing it
with a backslash to scare MathJax off.

Escaping all those backslashes in TeX fragments embedded in Lisp
strings can be a pain. [Pythonic String Reader][d3fc] can help with that.

<a id="x-28MGL-PAX-3A-40CODIFICATION-20MGL-PAX-3ASECTION-29"></a>

### 8.5 Codification

<a id="x-28MGL-PAX-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-UPPERCASE-IS-CODE\*** *T*

    When true, [interesting][7445] [name][88cf]s extracted from [codifiable][b89a] [word][d7b0]s
    are assumed to be code as if they were marked up with backticks. For
    example, this docstring
    
        "T PRINT CLASSes SECTION *PACKAGE* MGL-PAX ASDF
        CaMeL Capital"
    
    is equivalent to this:
    
        "`T` `PRINT` `CLASS`es `SECTION` `*PACKAGE*` `MGL-PAX` `ASDF`
        CaMel Capital"
    
    and renders as
    
    `T` [`PRINT`][d451] [`CLASS`][1f37]es [`SECTION`][5fac] `MGL-PAX` `ASDF` CaMel Capital
    
    where the links are added due to [`*DOCUMENT-LINK-CODE*`][d9ee].
    
    To suppress codification, add a backslash to the beginning of the
    a [codifiable][b89a] word or right after the leading `*` if it would
    otherwise be parsed as markdown emphasis:
    
        "\\SECTION *\\PACKAGE*"
    
    The number of backslashes is doubled above because that's how the
    example looks in a docstring. Note that the backslash is discarded
    even if `*DOCUMENT-UPPERCASE-IS-CODE*` is false.

<a id="x-28MGL-PAX-3A-40CODIFIABLE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **codifiable**

    A [word][d7b0] is *codifiable* iff
    
    - it has a single uppercase character (e.g. it's `T`) and no
      lowercase characters at all, or
    
    - there is more than one uppercase character and no lowercase
      characters between them (e.g. [`CLASS`][1f37]es, non[`READ`][fe58]able, [`CLASS-NAME`][03fa]s
      but not `Classes` or `aTe`.


<a id="x-28MGL-PAX-3A-40INTERESTING-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **interesting**

    A [name][88cf] is *interesting* iff
    
    - it names a symbol external to its package, or
    
    - it is at least 3 characters long and names an interned symbol, or
    
    - it names a [local reference][4c96].
    
    See [Package and Readtable][ab7e].

<a id="x-28MGL-PAX-3A-2ADOCUMENT-DOWNCASE-UPPERCASE-CODE-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-DOWNCASE-UPPERCASE-CODE\*** *NIL*

    If true, then all Markdown inline code (that is, `stuff between
    backticks`, including those found if [`*DOCUMENT-UPPERCASE-IS-CODE*`][f25f])
    which has no lowercase characters is downcased in the output.
    Characters of literal strings in the code may be of any case. If
    this variable is `:ONLY-IN-MARKUP` and the output format does not
    support markup (e.g. it's `:PLAIN`), then no downcasing is performed.
    For example,
    
        `(PRINT "Hello")`
    
    is downcased to
    
        `(print "Hello")`
    
    because it only contains uppercase characters outside the string.
    However,
    
        `MiXed "RESULTS"`
    
    is not altered because it has lowercase characters.
    
    If the first two characters are backslashes, then no downcasing is
    performed, in addition to [Preventing Autolinking][8c16]. Use this to mark
    inline code that's not Lisp.
    
        Press `\\M-.` in Emacs.


<a id="x-28MGL-PAX-3A-40LINKING-TO-CODE-20MGL-PAX-3ASECTION-29"></a>

### 8.6 Linking to Code

In this section, we describe all ways of linking to code
available when [`*DOCUMENT-UPPERCASE-IS-CODE*`][f25f] is true.

*Note that invoking [`M-.`][3386] on the
[name][88cf] of any of the following links will disambiguate based the
textual context, determining the locative. In a nutshell, if `M-.`
works without popping up a list of choices, then the documentation
will contain a single link.*

<a id="x-28MGL-PAX-3A-2ADOCUMENT-LINK-CODE-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-LINK-CODE\*** *T*

    Enable the various forms of links in docstrings described in
    [Linking to Code][1865]. See the following sections for a description of
    how to use linking.

<a id="x-28MGL-PAX-3A-40SPECIFIED-LOCATIVE-20MGL-PAX-3ASECTION-29"></a>

#### 8.6.1 Specified Locative

The following examples all render as [`DOCUMENT`][432c].

- `[DOCUMENT][function]` (*name + locative, explicit link*)

- `DOCUMENT function` (*name + locative, autolink*)

- `function DOCUMENT` (*locative + name, autolink*)

The Markdown link definition (i.e. `function` between the second
set of brackets above) needs no backticks to mark it as code.

Here and below, the [name][88cf] (`DOCUMENT`) is uppercased, and we rely
on [`*DOCUMENT-UPPERCASE-IS-CODE*`][f25f] being true. Alternatively, the [name][88cf]
could be explicitly marked up as code with a pair of backticks, and
then its character case would likely not matter (subject to
[`READTABLE-CASE`][48f1]).

The link text in the above examples is `DOCUMENT`. To override it,
this form may be used:

- `[see this][document function]` (*title + name + locative,
  explicit link*) renders as: [see this][432c].


<a id="x-28MGL-PAX-3A-40UNSPECIFIED-LOCATIVE-20MGL-PAX-3ASECTION-29"></a>

#### 8.6.2 Unspecified Locative

When only an [name][88cf] is provided without a locative, all
definitions of the name are considered as possible link targets.
Then, definitions that are not symbol-based (i.e. whose
[`XREF-NAME`][b88e] is not a symbol) are filtered out to prevent
unrelated [`PACKAGE`][1d5a]s and [`ASDF:SYSTEM`][c097]s from cluttering the
documentation without the control provided by importing symbols.

To further reduce clutter, if the definitions include a
[`GENERIC-FUNCTION`][5875] locative, then all references with [`LOCATIVE-TYPE`][97ba]
[`METHOD`][172e], [`ACCESSOR`][00d4], [`READER`][cc04] and
[`WRITER`][e548] are removed to avoid linking to a possibly large
number of methods.

Furthermore, filter out all references with `LOCATIVE-TYPE`
[`LOCATIVE`][0b3a] if there are references with other `LOCATIVE-TYPE`s.

<a id="x-28MGL-PAX-3A-40UNAMBIGUOUS-UNSPECIFICED-LOCATIVE-20MGL-PAX-3ASECTION-29"></a>

##### Unambiguous Unspecified Locative

In the following examples, although no locative is specified,
`DOCUMENT` names a single [name][88cf] being [`DOCUMENT`][432c]ed, so they all
render as [`DOCUMENT`][432c].

- `[DOCUMENT][]` (*name, explicit link*),

- `DOCUMENT` (*name, autolink*).

To override the title:

- `[see this][document]` (*title + name, explicit link*) renders
  as: [see this][432c].


<a id="x-28MGL-PAX-3A-40AMBIGUOUS-UNSPECIFIED-LOCATIVE-20MGL-PAX-3ASECTION-29"></a>

##### Ambiguous Unspecified Locative

These examples all render as [`SECTION`][5fac], linking to both
definitions of the [name][88cf] `SECTION`, the `CLASS` and the
`LOCATIVE`. Note that the rendered output is a single link to a
disambiguation page when [Browsing Live Documentation][a595], while
multiple, numbered links are generated in offline documentation.

- `[SECTION][]` (*name, explicit link*)

- `SECTION` (*name, autolink*)

To override the title:

- `[see this][section]` (*title + name, explicit link*) renders as:
  [see this][5fac].


<a id="x-28MGL-PAX-3A-40EXPLICIT-AND-AUTOLINKING-20MGL-PAX-3ASECTION-29"></a>

#### 8.6.3 Explicit and Autolinking

The examples in the previous sections are marked with *explicit
link* or *autolink*. Explicit links are those with a Markdown
reference link spelled out explicitly, while autolinks are those
without.

<a id="x-28MGL-PAX-3A-40PREVENTING-AUTOLINKING-20MGL-PAX-3ASECTION-29"></a>

#### 8.6.4 Preventing Autolinking

In the common case, when [`*DOCUMENT-UPPERCASE-IS-CODE*`][f25f] is true,
prefixing an uppercase [word][d7b0] with a backslash prevents it from being
codified and thus also prevents [autolinking][b3cc] form kicking in. For example,

    \DOCUMENT

renders as DOCUMENT. If it should be marked up as code but not
autolinked, the backslash must be within backticks like this:

    `\DOCUMENT`

This renders as `DOCUMENT`. Alternatively, the [`DISLOCATED`][e391] or the
[`ARGUMENT`][8710] locative may be used as in `[DOCUMENT][dislocated]`.

<a id="x-28MGL-PAX-3A-40UNRESOLVABLE-REFLINKS-20MGL-PAX-3ASECTION-29"></a>

#### 8.6.5 Unresolvable Links

<a id="x-28MGL-PAX-3AUNRESOLVABLE-REFLINK-20CONDITION-29"></a>

- [condition] **UNRESOLVABLE-REFLINK** *[WARNING][bcb6]*

    When [`DOCUMENT`][432c] encounters an [explicit
    link][b3cc] such as `[NONEXISTENT][function]`
    that looks like a PAX construct but cannot be resolved, it signals
    and `UNRESOLVABLE-REFLINK` warning.
    
    - If the [`OUTPUT-REFLINK`][2ca9] restart is invoked, then no warning is
      printed and the markdown link is left unchanged. `MUFFLE-WARNING`([`0`][b8b4] [`1`][6f51]) is
      equivalent to `OUTPUT-REFLINK`.
    
    - If the [`OUTPUT-LABEL`][c818] restart is invoked, then no warning is printed
      and the markdown link is replaced by its label. For example,
      `[NONEXISTENT][function]` becomes `NONEXISTENT`.
    
    - If the warning is not handled, then it is printed to
      [`*ERROR-OUTPUT*`][66c6]. Otherwise, it behaves as `OUTPUT-REFLINK`.


<a id="x-28MGL-PAX-3AOUTPUT-REFLINK-20FUNCTION-29"></a>

- [function] **OUTPUT-REFLINK** *&OPTIONAL CONDITION*

    Invoke the `OUTPUT-REFLINK` restart. See [`UNRESOLVABLE-REFLINK`][64be].

<a id="x-28MGL-PAX-3AOUTPUT-LABEL-20FUNCTION-29"></a>

- [function] **OUTPUT-LABEL** *&OPTIONAL CONDITION*

    Invoke the `OUTPUT-LABEL` restart. See [`UNRESOLVABLE-REFLINK`][64be].

<a id="x-28MGL-PAX-3A-40SUPPRESSED-LINKS-20MGL-PAX-3ASECTION-29"></a>

#### 8.6.6 Suppressed Links

[Autolinking][b3cc] of code (i.e.
of something like `FOO`) is suppressed if it would create a link
that was already made within the same docstring. In the following
docstring, only the first `FOO` will be turned into a link.

    "`FOO` is safe. `FOO` is great."

However, explicit links (when a [locative][7ac8] was specified or
found near the [name][88cf]) are never suppressed. In the following, in
both docstrings, both occurrences `FOO` produce links.

    "`FOO` is safe. [`FOO`][macro] is great."
    "`FOO` is safe. Macro `FOO` is great."

As an exception, links with [specified][8996]
and [unambiguous][ad94]
locatives to [`SECTION`][5fac]s and [`GLOSSARY-TERM`][8251]s always produce a link to
allow their titles to be displayed properly.

Finally, [autolinking][b3cc] to `T` or
`NIL` is suppressed (see [`*DOCUMENT-LINK-TO-HYPERSPEC*`][875e]).

<a id="x-28MGL-PAX-3A-40LOCAL-REFERENCES-20MGL-PAX-3ASECTION-29"></a>

#### 8.6.7 Local References

To declutter the generated output by reducing the number of
links, the so-called local references (e.g. references to the very
definition for which documentation is being generated) are treated
specially. In the following example, there are local references to
the function `FOO` and its arguments, so none of them get turned into
links:

```
(defun foo (arg1 arg2)
  "FOO takes two arguments: ARG1 and ARG2."
  t)
```

If linking was desired, one could use a [Specified Locative][8996] (e.g.
`[FOO][function]` or `FOO function`), which results in a single
link. An explicit link with an unspecified locative like `[FOO][]`
generates links to all references involving the `FOO` symbol except
the local ones.

The exact rules for local references are as follows:

- Unless a locative is [specified][8996], no
[autolinking][b3cc] is performed for
[name][88cf]s for which there are local references. For example, `FOO` does
not get any links if there is *any* local reference with the same
[name][88cf].

- With a locative specified (e.g. in the explicit link
`[FOO][function]` or in the text `the FOO function`), a single link
is made irrespective of any local references.

- Explicit links with an unspecified locative (e.g. `[FOO][]`) are
linked to all non-local references.


<a id="x-28MGL-PAX-3A-40LINKING-TO-THE-HYPERSPEC-20MGL-PAX-3ASECTION-29"></a>

### 8.7 Linking to the Hyperspec

<a id="x-28MGL-PAX-3A-2ADOCUMENT-LINK-TO-HYPERSPEC-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-LINK-TO-HYPERSPEC\*** *T*

    If true, link symbols found in code to the Common Lisp Hyperspec
    unless there is a definition in the running Lisp that is being
    [`DOCUMENT`][432c]ed.
    
    Locatives work as expected (see [`*DOCUMENT-LINK-CODE*`][d9ee]): `FIND-IF`
    links to [`FIND-IF`][5884], `FUNCTION` links to `FUNCTION`([`0`][119e] [`1`][81f7]), and
    `[FUNCTION][type]` links to [`FUNCTION`][119e].
    
    [Autolinking][b3cc] to `T` and `NIL` is
    [suppressed][e2e8]. If desired, use `[T][]` (that
    links to `T`([`0`][9172] [`1`][fe21])) or `[T][constant]` (that links to [`T`][fe21]).
    
    Note that linking explicitly with the [`CLHS`][ed5f] locative is not subject
    to the value of this variable.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HYPERSPEC-ROOT-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HYPERSPEC-ROOT\*** *"http://www.lispworks.com/documentation/HyperSpec/"*

    A URL of the Common Lisp Hyperspec. The default value
    is the canonical location. When [invoked from Emacs][a595], the Elisp variable
    `common-lisp-hyperspec-root` is in effect.

<a id="x-28MGL-PAX-3A-40LINKING-TO-SECTIONS-20MGL-PAX-3ASECTION-29"></a>

### 8.8 Linking to Sections

The following variables control how to generate section numbering,
table of contents and navigation links.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-LINK-SECTIONS-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-LINK-SECTIONS\*** *T*

    When true, HTML anchors are generated before the headings (e.g. of
    sections), which allows the table of contents to contain links and
    also code-like references to sections (like `@FOO-MANUAL`) to be
    translated to links with the section title being the name of the
    link.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-MAX-NUMBERING-LEVEL-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-MAX-NUMBERING-LEVEL\*** *3*

    A non-negative integer. In their hierarchy, sections on levels less
    than this value get numbered in the format of `3.1.2`. Setting it to
    0 turns numbering off.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL\*** *3*

    An integer that determines the depth of the table of contents.
    
    - If negative, then no table of contents is generated.
    
    - If non-negative, and there are multiple top-level sections on a
      page, then they are listed at the top of the page.
    
    - If positive, then for each top-level section a table of contents
      is printed after its heading, which includes a nested tree of
      section titles whose depth is limited by this value.
    
    If [`*DOCUMENT-LINK-SECTIONS*`][1b28] is true, then the tables will link to
    the sections.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-TEXT-NAVIGATION-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-TEXT-NAVIGATION\*** *NIL*

    If true, then before each heading a line is printed with links to
    the previous, parent and next section. Needs
    [`*DOCUMENT-LINK-SECTIONS*`][1b28] to be on to work.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-FANCY-HTML-NAVIGATION-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-FANCY-HTML-NAVIGATION\*** *T*

    If true and the output format is HTML, then headings get a
    navigation component that consists of links to the previous, parent,
    next section, a self-link, and a link to the definition in the
    source code if available (see `:SOURCE-URI-FN` in [`DOCUMENT`][432c]). This
    component is normally hidden, it is visible only when the mouse is
    over the heading. Needs [`*DOCUMENT-LINK-SECTIONS*`][1b28] to be on to work.

<a id="x-28MGL-PAX-3A-40MISCELLANEOUS-DOCUMENTATION-PRINTER-VARIABLES-20MGL-PAX-3ASECTION-29"></a>

### 8.9 Miscellaneous Variables

<a id="x-28MGL-PAX-3A-2ADOCUMENT-URL-VERSIONS-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-URL-VERSIONS\*** *(2 1)*

    A list of versions of PAX URL formats to support in the
    generated documentation. The first in the list is used to generate
    links.
    
    PAX emits HTML anchors before the documentation of [`SECTION`][5fac]s
    (see [Linking to Sections][22c2]) and other things (see [Linking to Code][1865]).
    For the function `FOO`, in the current version (version 2), the
    anchor is `<a id="MGL-PAX:FOO%20FUNCTION">`, and its URL will end
    with `#MGL-PAX:FOO%20FUNCTION`.
    
    *Note that to make the URL independent of whether a symbol is
    [internal or external][3473] to their [`SYMBOL-PACKAGE`][e5ab], single
    colon is printed where a double colon would be expected. Package and
    symbol names are both printed verbatim except for escaping colons
    and spaces with a backslash. For exported symbols with no funny
    characters, this coincides with how [`PRIN1`][6384] would print the symbol,
    while having the benefit of making the URL independent of the Lisp
    printer's escaping strategy and producing human-readable output for
    mixed-case symbols. No such promises are made for non-ASCII
    characters, and their URLs may change in future versions. Locatives
    are printed with `PRIN1`.*
    
    Version 1 is based on the more strict HTML4 standard and the id of
    `FOO` is `"x-28MGL-PAX-3A-3AFOO-20FUNCTION-29"`. This is supported
    by Github-flavoured Markdown. Version 2 has minimal clutter and is
    obviously preferred. However, in order not to break external links,
    by default, both anchors are generated.
    
    Let's understand the generated Markdown.
    
    ```
    (defun foo (x))
    
    (document #'foo :format :markdown)
    => ("<a id=\"x-28MGL-PAX-3AFOO-20FUNCTION-29\"></a>
    <a id=\"MGL-PAX:FOO%20FUNCTION\"></a>
    
    - [function] **FOO** *X*
    ")
    
    (let ((*document-url-versions* '(1)))
      (document #'foo :format :markdown))
    => ("<a id=\"x-28MGL-PAX-3AFOO-20FUNCTION-29\"></a>
    
    - [function] **FOO** *X*
    ")
    ```


<a id="x-28MGL-PAX-3A-2ADOCUMENT-MIN-LINK-HASH-LENGTH-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-MIN-LINK-HASH-LENGTH\*** *4*

    Recall that markdown reference style links (like `[label][id]`) are
    used for linking to sections and code. It is desirable to have ids
    that are short to maintain legibility of the generated markdown, but
    also stable to reduce the spurious diffs in the generated
    documentation, which can be a pain in a version control system.
    
    Clearly, there is a tradeoff here. This variable controls how many
    characters of the MD5 sum of the full link id (the reference as a
    string) are retained. If collisions are found due to the low number
    of characters, then the length of the hash of the colliding
    reference is increased.
    
    This variable has no effect on the HTML generated from markdown, but
    it can make markdown output more readable.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-MARK-UP-SIGNATURES-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-MARK-UP-SIGNATURES\*** *T*

    When true, some things such as function names and arglists are
    rendered as bold and italic. In `:HTML` output, locative types become
    links to sources (if `:SOURCE-URI-FN` is provided, see [`DOCUMENT`][432c]), and
    the symbol becomes a self-link for your permalinking pleasure.
    
    For example, a reference is rendered in markdown roughly as:
    
        - [function] foo x y
    
    With this option on, the above becomes:
    
        - [function] **foo** *x y*
    
    Also, in HTML `**foo**` will be a link to that very entry and
    `[function]` may turn into a link to sources.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-BASE-URL-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-BASE-URL\*** *NIL*

    When `*DOCUMENT-BASE-URL*` is non-`NIL`, this is prepended to all
    Markdown relative `URL`s. It must be a valid `URL` without no query and
    fragment parts (that is, "http://lisp.org/doc/" but not
    "http://lisp.org/doc?a=1" or "http://lisp.org/doc#fragment").
    Note that intra-page links using only `URL` fragments (e.g. and
    explicit HTML links (e.g. `<a href="...">`) in Markdown are not
    affected.

<a id="x-28MGL-PAX-3A-40DOCUMENTATION-UTILITIES-20MGL-PAX-3ASECTION-29"></a>

### 8.10 Utilities for Generating Documentation

Two convenience functions are provided to serve the common case of
having an ASDF system with some readmes and a directory with for the
HTML documentation and the default CSS stylesheet.

<a id="x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-READMES-20FUNCTION-29"></a>

- [function] **UPDATE-ASDF-SYSTEM-READMES** *OBJECT ASDF-SYSTEM &KEY (URL-VERSIONS '(1)) (FORMATS '(:MARKDOWN))*

    Convenience function to generate up to two readme files in the
    directory holding the `ASDF-SYSTEM` definition. `OBJECT` is passed on to
    [`DOCUMENT`][432c].
    
    If `:MARKDOWN` is in `FORMATS`, then `README.md` is generated with
    anchors, links, inline code, and other markup added. Not necessarily
    the easiest on the eye in an editor but looks good on github.
    
    If `:PLAIN` is in `FORMATS`, then `README` is generated, which is
    optimized for reading in text format. It has less cluttery markup
    and no [autolinking][b3cc].
    
    Example usage:
    
    ```
    (update-asdf-system-readmes @pax-manual :mgl-pax
                                :formats '(:markdown :plain))
    ```
    
    Note that [`*DOCUMENT-URL-VERSIONS*`][17e0] is bound to `URL-VERSIONS`, which
    defaults to using the uglier, version 1 style of `URL` for the sake of
    github.

<a id="x-28MGL-PAX-3A-40HTML-OUTPUT-20MGL-PAX-3ASECTION-29"></a>

#### 8.10.1 HTML Output

<a id="x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-HTML-DOCS-20FUNCTION-29"></a>

- [function] **UPDATE-ASDF-SYSTEM-HTML-DOCS** *SECTIONS ASDF-SYSTEM &KEY PAGES (TARGET-DIR (ASDF/SYSTEM:SYSTEM-RELATIVE-PATHNAME ASDF-SYSTEM "doc/")) (UPDATE-CSS-P T) (STYLE \*DOCUMENT-HTML-DEFAULT-STYLE\*)*

    Generate pretty HTML documentation for a single ASDF system,
    possibly linking to github. If `UPDATE-CSS-P`, copy the `STYLE` files to
    `TARGET-DIR` (see [`*DOCUMENT-HTML-DEFAULT-STYLE*`][90fa]).
    
    Example usage:
    
    ```
    (update-asdf-system-html-docs @pax-manual :mgl-pax)
    ```
    
    The same, linking to the sources on github:
    
    ```
    (update-asdf-system-html-docs
      @pax-manual :mgl-pax
      :pages
      `((:objects (,mgl-pax::@pax-manual)
         :source-uri-fn ,(make-git-source-uri-fn
                          :mgl-pax
                          "https://github.com/melisgl/mgl-pax"))))
    ```


See the following variables, which control HTML generation.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-DEFAULT-STYLE-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HTML-DEFAULT-STYLE\*** *:DEFAULT*

    The HTML style to use. It's either `STYLE` is either `:DEFAULT` or
    `:CHARTER`. The `:DEFAULT` CSS stylesheet relies on the default
    fonts (sans-serif, serif, monospace), while `:CHARTER` bundles some
    fonts for a more controlled look.
    
    The value of this variable affects the default style of
    [`UPDATE-ASDF-SYSTEM-HTML-DOCS`][bb12]. If you change this variable, you may
    need to do a hard refresh in the browser (often `C-<f5>`). See
    [`*BROWSE-HTML-STYLE*`][e527] for how to control the style used for
    [Browsing Live Documentation][a595].

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL\*** *NIL*

    `NIL` or a non-negative integer. If non-`NIL`, it overrides
    [`*DOCUMENT-MAX-NUMBERING-LEVEL*`][f12d] in the dynamic HTML table of contents
    on the left of the page.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-HEAD-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HTML-HEAD\*** *NIL*

    Stuff to be included in the `<head>` of the generated HTML.
    
    - If `NIL`, nothing is included.
    
    - If a `STRING`([`0`][b93c] [`1`][dae6]), then it is written to the HTML output as is without
      any escaping.
    
    - If a function designator, then it is called with a single
      argument, the HTML stream, where it must write the output.


<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-SIDEBAR-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HTML-SIDEBAR\*** *NIL*

    Stuff to be included in the HTML sidebar.
    
    - If `NIL`, a default sidebar is generated, with
      [`*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*`][e216], followed by the dynamic table
      of contents, and [`*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS*`][0ef0].
    
    - If a `STRING`([`0`][b93c] [`1`][dae6]), then it is written to the HTML output as is without
      any escaping.
    
    - If a function designator, then it is called with a single
      argument, the HTML stream, where it must write the output.


<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-TOP-BLOCKS-OF-LINKS-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS\*** *NIL*

    A list of blocks of links to be displayed on the sidebar on the left,
    above the table of contents. A block is of the form `(&KEY TITLE ID
    LINKS)`, where `TITLE` will be displayed at the top of the block in a
    HTML `DIV` with `ID` followed by the links. `LINKS` is a list of `(URI
    LABEL)` elements, where `URI` maybe a string or an object being
    [`DOCUMENT`][432c]ed or a `REFERENCE` thereof.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS\*** *NIL*

    Like [`*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*`][e216], only it is displayed
    below the table of contents.

<a id="x-28MGL-PAX-3A-40GITHUB-WORKFLOW-20MGL-PAX-3ASECTION-29"></a>

#### 8.10.2 Github Workflow

It is generally recommended to commit generated readmes (see
[`UPDATE-ASDF-SYSTEM-READMES`][13a9]), so that users have something to read
without reading the code and sites like github can display them.

HTML documentation can also be committed, but there is an issue with
that: when linking to the sources (see [`MAKE-GIT-SOURCE-URI-FN`][587f]), the
commit id is in the link. This means that code changes need to be
committed first, and only then can HTML documentation be regenerated
and committed in a followup commit.

The second issue is that github is not very good at serving HTML
files from the repository itself (and
[http://htmlpreview.github.io](http://htmlpreview.github.io) chokes
on links to the sources).

The recommended workflow is to use
[gh-pages](https://pages.github.com/), which can be made relatively
painless with the `git worktree` command. The gist of it is to make
the `doc/` directory a checkout of the branch named `gh-pages`.
There is a [good
description](http://sangsoonam.github.io/2019/02/08/using-git-worktree-to-deploy-github-pages.html)
of this general process. Two commits are needed still, but it is
somewhat less painful.

This way the HTML documentation will be available at

    http://<username>.github.io/<repo-name>

It is probably a good idea to add sections like the [Links and Systems][ba74] section
to allow jumping between the repository and the gh-pages site.

<a id="x-28MGL-PAX-3AMAKE-GITHUB-SOURCE-URI-FN-20FUNCTION-29"></a>

- [function] **MAKE-GITHUB-SOURCE-URI-FN** *ASDF-SYSTEM GITHUB-URI &KEY GIT-VERSION*

    This function is a backward-compatibility wrapper around
    [`MAKE-GIT-SOURCE-URI-FN`][587f], which supersedes `MAKE-GITHUB-SOURCE-URI-FN`.
    All arguments are passed on to `MAKE-GIT-SOURCE-URI-FN`, leaving
    `URI-FORMAT-STRING` at its default, which is suitable for github.

<a id="x-28MGL-PAX-3AMAKE-GIT-SOURCE-URI-FN-20FUNCTION-29"></a>

- [function] **MAKE-GIT-SOURCE-URI-FN** *ASDF-SYSTEM GIT-FORGE-URI &KEY GIT-VERSION (URI-FORMAT-STRING "~A/blob/~A/~A#L~S")*

    Return a function suitable as `:SOURCE-URI-FN` of a page spec (see
    the `PAGES` argument of [`DOCUMENT`][432c]). The function looks at the source
    location of the object passed to it, and if the location is found,
    the path is made relative to the toplevel directory of the git
    checkout containing the file of the `ASDF-SYSTEM` and finally an URI
    pointing to your git forge (such as github) is returned. A warning
    is signalled whenever the source location lookup fails or if the
    source location points to a directory not below the directory of
    `ASDF-SYSTEM`.
    
    If `GIT-FORGE-URI` is `"https://github.com/melisgl/mgl-pax/"` and
    `GIT-VERSION` is `"master"`, then the returned URI may look like this:
    
        https://github.com/melisgl/mgl-pax/blob/master/src/pax-early.lisp#L12
    
    If `GIT-VERSION` is `NIL`, then an attempt is made to determine to
    current commit id from the `.git` in the directory holding
    `ASDF-SYSTEM`. If no `.git` directory is found, then no links to
    the git forge will be generated.
    
    `URI-FORMAT-STRING` is a [`CL:FORMAT`][ad78] control string for four arguments:
    
    - `GIT-FORGE-URI`,
    
    - `GIT-VERSION`,
    
    - the relative path to the file of the source location of the reference,
    
    - and the line number.
    
    The default value of `URI-FORMAT-STRING` is for github. If using a
    non-standard git forge, such as Sourcehut or Gitlab, simply pass a
    suitable `URI-FORMAT-STRING` matching the URI scheme of your forge.

<a id="x-28MGL-PAX-3A-40PAX-WORLD-20MGL-PAX-3ASECTION-29"></a>

#### 8.10.3 PAX World

PAX World is a registry of documents, which can generate
cross-linked HTML documentation pages for all the registered
documents. There is an official [PAX
World](https://melisgl.github.io/mgl-pax-world/).

<a id="x-28MGL-PAX-3AREGISTER-DOC-IN-PAX-WORLD-20FUNCTION-29"></a>

- [function] **REGISTER-DOC-IN-PAX-WORLD** *NAME SECTIONS PAGE-SPECS*

    Register `SECTIONS` and `PAGE-SPECS` under `NAME` (a symbol) in PAX
    World. By default, [`UPDATE-PAX-WORLD`][ee51] generates documentation for all
    of these. `SECTIONS` and `PAGE-SPECS` must be lists of [`SECTION`][5fac]s and
    `PAGE-SPEC`s (SEE [`DOCUMENT`][432c]) or designators of function of no arguments
    that return such lists.

For example, this is how PAX registers itself:

```
(defun pax-sections ()
  (list @pax-manual))

(defun pax-pages ()
  `((:objects ,(pax-sections)
     :source-uri-fn ,(make-git-source-uri-fn
                      :mgl-pax
                      "https://github.com/melisgl/mgl-pax"))))

(register-doc-in-pax-world :pax 'pax-sections 'pax-pages)

```

<a id="x-28MGL-PAX-3AUPDATE-PAX-WORLD-20FUNCTION-29"></a>

- [function] **UPDATE-PAX-WORLD** *&KEY (DOCS \*REGISTERED-PAX-WORLD-DOCS\*) DIR UPDATE-CSS-P (STYLE \*DOCUMENT-HTML-DEFAULT-STYLE\*)*

    Generate HTML documentation for all `DOCS`. Files are created in
    `DIR` (`(asdf:system-relative-pathname :mgl-pax "world/")` by
    default if `DIR` is `NIL`). `DOCS` is a list of entries of the form (`NAME`
    [`SECTIONS`][5fac] `PAGE-SPECS`). The default for `DOCS` is all the sections and
    pages registered with [`REGISTER-DOC-IN-PAX-WORLD`][f4fd].
    
    In the absence of `:HEADER-FN` `:FOOTER-FN`, `:OUTPUT`, every spec in
    `PAGE-SPECS` is augmented with HTML headers, footers and output
    location specifications (based on the name of the section).
    
    If necessary a default page spec is created for every section.

<a id="x-28MGL-PAX-3A-40OVERVIEW-OF-ESCAPING-20MGL-PAX-3ASECTION-29"></a>

### 8.11 Overview of Escaping

Let's recap how escaping [Codification][f1ab],
[downcasing][a5ee], and
[Linking to Code][1865] works.

- One backslash in front of a [word][d7b0] turns codification off. Use this
  to prevent codification words such as DOCUMENT, which is all
  uppercase hence [codifiable][b89a], and it names an exported symbol hence
  it is [interesting][7445].

- One backslash right after an opening backtick turns autolinking
  off.

- Two backslashes right after an opening backtick turns autolinking
  and downcasing off. Use this for things that are not Lisp code but
  which need to be in a monospace font.


In the following examples capital C/D/A letters mark the presence,
and a/b/c the absence of codification, downcasing, and autolinking
assuming all these features are enabled by
[`*DOCUMENT-UPPERCASE-IS-CODE*`][f25f], [`*DOCUMENT-DOWNCASE-UPPERCASE-CODE*`][a5ee],
and [`*DOCUMENT-LINK-CODE*`][d9ee].

    DOCUMENT                => [`document`][1234]    (CDA)
    \DOCUMENT               => DOCUMENT              (cda)
    `\DOCUMENT`             => `document`            (CDa)
    `\\DOCUMENT`            => `DOCUMENT`            (CdA)
    [DOCUMENT][]            => [`document`][1234]    (CDA)
    [\DOCUMENT][]           => [DOCUMENT][1234]      (cdA)
    [`\DOCUMENT`][]         => [`document`][1234]    (CDA) *
    [`\\DOCUMENT`][]        => [`DOCUMENT`][1234]    (CdA)
    [DOCUMENT][dislocated]  => `document`            (CDa)

Note that in the example marked with `*`, the single backslash,
that would normally turn autolinking off, is ignored because it is
in an explicit link.

<a id="x-28MGL-PAX-3A-40OUTPUT-DETAILS-20MGL-PAX-3ASECTION-29"></a>

### 8.12 Output Details

By default, [`DREF`][d930]s are documented in the following format.

```
- [<locative-type>] <name> <arglist>

    <docstring>
```

The line with the bullet is printed with [`DOCUMENTING-REFERENCE`][b8a8]. The
docstring is processed with [`DOCUMENT-DOCSTRING`][7f1f] while
[Local References][4c96] established with [`WITH-DISLOCATED-NAMES`][2d48] are in
effect for all variables locally bound in a definition with [`ARGLIST`][e6bd],
and [`*PACKAGE*`][5ed1] is bound to the second return value of [`DOCSTRING`][affc].

With this default format, PAX supports all locative types, but for
some [Locative Types][bf0f] defined in DRef and the [PAX Locatives][292a],
special provisions have been made.

- For definitions with a [`VARIABLE`][6c83] or [`CONSTANT`][c819] locative, their
initform is printed as their arglist. The initform is the `INITFORM`
argument of the locative if provided, or the global symbol value of
their name. If no `INITFORM` is provided, and the symbol is globally
unbound, then no arglist is printed.

When the printed initform is too long, it is truncated.

- Depending of what the [`SETF`][d83a] locative refers to, the `ARGLIST` of the
[setf expander][35a2], [setf function][99b0], or the method
signature is printed as with the [`METHOD`][172e] locative.

- For definitions with a [`METHOD`][172e] locative, the arglist printed is
the method signature, which consists of the locative's `QUALIFIERS`
and `SPECIALIZERS` appended.

- For definitions with an [`ACCESSOR`][00d4], [`READER`][cc04] or [`WRITER`][e548] locative, the
class on which they are specialized is printed as their arglist.

- For definitions with a [`STRUCTURE-ACCESSOR`][090c] locative, the arglist
printed is the locative's `CLASS-NAME` argument if provided.

- For definitions with a [`CLASS`][2060] locative, the arglist printed is the
list of immediate superclasses with [`STANDARD-OBJECT`][a843], [`CONDITION`][83e1] and
non-exported symbols omitted.

- For definitions with a [`ASDF:SYSTEM`][c097] locative, their most
important slots are printed as an unnumbered list.

- When a definition with the [`SECTION`][672f] locative is being documented,
a new (sub)section is opened (see [`WITH-HEADING`][80e8]), within which
documentation for its each of its [`SECTION-ENTRIES`][d850] is generated. A
fresh line is printed after all entries except the last.

- For definitions with a [`GLOSSARY-TERM`][5119] locative, no arglist is
printed, and if non-`NIL`, [`GLOSSARY-TERM-TITLE`][af78] is printed as name.

- For definitions with a [`GO`][58f6] locative, its [`LOCATIVE-ARGS`][2444] are printed
as its arglist, along with a redirection message.

- See the [`INCLUDE`][5cd7] locative.

- For definitions with a [`CLHS`][ed5f] locative, the `LOCATIVE-ARGS` are printed
as the arglist. There is no docstring.

- For definitions with an [`UNKNOWN`][a951] locative, the `LOCATIVE-ARGS` are
printed as the arglist. There is no docstring.


<a id="x-28MGL-PAX-3A-40DOCUMENT-IMPLEMENTATION-NOTES-20MGL-PAX-3ASECTION-29"></a>

### 8.13 Documentation Generation Implementation Notes

Documentation Generation is supported on ABCL, AllegroCL, CLISP,
CCL, CMUCL, ECL and SBCL, but their outputs may differ due to the
lack of some introspective capability. SBCL generates complete
output. see [`ARGLIST`][e6bd], [`DOCSTRING`][affc] and [`SOURCE-LOCATION`][32da] for
implementation notes.

In addition, CLISP does not support the ambiguous case of [PAX URLs][1e80]
for [Browsing Live Documentation][a595] because the current implementation
relies on Swank to list definitions of symbols (as [`VARIABLE`][6c83],
[`FUNCTION`][ba62], etc), and that simply doesn't work.

<a id="x-28MGL-PAX-3A-40TRANSCRIPTS-20MGL-PAX-3ASECTION-29"></a>

## 9 Transcripts

What are transcripts for? When writing a tutorial, one often wants
to include a REPL session with maybe a few defuns and a couple of
forms whose output or return values are shown. Also, in a function's
docstring an example call with concrete arguments and return values
speaks volumes. A transcript is a text that looks like a REPL
session, but which has a light markup for printed output and return
values, while no markup (i.e. prompt) for Lisp forms. PAX
transcripts may include output and return values of all forms, or
only selected ones. In either case, the transcript itself can be
easily generated from the source code.

The main worry associated with including examples in the
documentation is that they tend to get out-of-sync with the code.
This is solved by being able to parse back and update transcripts.
In fact, this is exactly what happens during documentation
generation with PAX. Code sections tagged with `cl-transcript` are
retranscribed and checked for consistency (that is, no difference in
output or return values). If the consistency check fails, an error
is signalled that includes a reference to the object being
documented.

Going beyond documentation, transcript consistency checks can be
used for writing simple tests in a very readable form. For example:

```common-lisp
(+ 1 2)
=> 3

(values (princ :hello) (list 1 2))
.. HELLO
=> :HELLO
=> (1 2)
```

All in all, transcripts are a handy tool especially when combined
with the Emacs support to regenerate them and with
[PYTHONIC-STRING-READER][c097]'s triple-quoted strings, that
allow one to work with nested strings with less noise. The
triple-quote syntax can be enabled with:

    (in-readtable pythonic-string-syntax)


<a id="x-28-22mgl-pax-2Ftranscribe-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

### 9.1 The mgl-pax/transcribe ASDF System

- Description: Transcription support for MGL-PAX.
- Long Description: Do not declare a dependency on this system.
  It is autoloaded by [`MGL-PAX:TRANSCRIBE`][f1f0] and by the Emacs
  integration (see [Transcripts][6300]).
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)


<a id="x-28MGL-PAX-3A-40TRANSCRIBING-WITH-EMACS-20MGL-PAX-3ASECTION-29"></a>

### 9.2 Transcribing with Emacs

Typical transcript usage from within Emacs is simple: add a Lisp
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

Now invoke the Elisp function `mgl-pax-transcribe` where the cursor
is, and the fenced code block from the docstring becomes:

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
[`TRANSCRIPTION-CONSISTENCY-ERROR`][a249] because the printed output and the
first return value changed, so you regenerate the documentation by
marking the region of bounded by `#\|` and the cursor at `#\^` in
the example:

    |(values (princ :hello-world) (list 1 2))
    .. HELLO
    => :HELLO
    => (1
        ;; This value is arbitrary.
        2)
    ^

then invoke the Elisp function `mgl-pax-retranscribe-region` to get:

    (values (princ :hello-world) (list 1 2))
    .. HELLO-WORLD
    => :HELLO-WORLD
    => (1
        ;; This value is arbitrary.
        2)
    ^

Note how the indentation and the comment of `(1 2)` were left alone,
but the output and the first return value got updated.

Alternatively, `C-u 1 mgl-pax-transcribe` will emit commented markup:

    (values (princ :hello) (list 1 2))
    ;.. HELLO
    ;=> :HELLO
    ;=> (1 2)

`C-u 0 mgl-pax-retranscribe-region` will turn commented into
non-commented markup. In general, the numeric prefix argument is the
index of the syntax to be used in [`*TRANSCRIBE-SYNTAXES*`][ebd3]. Without a
prefix argument, `mgl-pax-retranscribe-region` will not change the
markup style.

Finally, not only do both functions work at any indentation level
but in comments too:

    ;;;; (values (princ :hello) (list 1 2))
    ;;;; .. HELLO
    ;;;; => :HELLO
    ;;;; => (1 2)

The dynamic environment of the transcription is determined by the
`:DYNENV` argument of the enclosing `cl-transcript` code block (see
[Controlling the Dynamic Environment][6b59]).

Transcription support in Emacs can be enabled by loading
`src/mgl-pax.el`. See [Emacs Setup][8541].

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-API-20MGL-PAX-3ASECTION-29"></a>

### 9.3 Transcript API

<a id="x-28MGL-PAX-3ATRANSCRIBE-20FUNCTION-29"></a>

- [function] **TRANSCRIBE** *INPUT OUTPUT &KEY UPDATE-ONLY (INCLUDE-NO-OUTPUT UPDATE-ONLY) (INCLUDE-NO-VALUE UPDATE-ONLY) (ECHO T) (CHECK-CONSISTENCY \*TRANSCRIBE-CHECK-CONSISTENCY\*) DEFAULT-SYNTAX (INPUT-SYNTAXES \*TRANSCRIBE-SYNTAXES\*) (OUTPUT-SYNTAXES \*TRANSCRIBE-SYNTAXES\*) DYNENV*

    Read forms from `INPUT` and write them (iff `ECHO`) to `OUTPUT`
    followed by any output and return values produced by calling [`EVAL`][0d6e] on
    the form. `INPUT` can be a stream or a string, while `OUTPUT` can be a
    stream or `NIL`, in which case output goes into a string. The return
    value is the `OUTPUT` stream or the string that was constructed. Since
    `TRANSCRIBE` `EVAL`uates arbitrary code anyway, forms are read with
    [`*READ-EVAL*`][82f7] `T`.
    
    Go up to [Transcribing with Emacs][f5bd] for nice examples. A more
    mind-bending one is this:
    
    ```common-lisp
    (transcribe "(princ 42) " nil)
    => "(princ 42)
    .. 42
    => 42
    "
    ```
    
    However, the above may be a bit confusing since this documentation
    uses `TRANSCRIBE` markup syntax in this very example, so let's do it
    differently. If we have a file with these contents:
    
    ```
    (values (princ 42) (list 1 2))
    ```
    
    it is transcribed to:
    
    ```
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
    
    `TRANSCRIBE` is able to parse its own output. If we transcribe the
    previous output above, we get it back exactly. However, if we remove
    all output markers, leave only a placeholder value marker and
    pass `:UPDATE-ONLY` `T` with source:
    
    ```
    (values (princ 42) (list 1 2))
    =>
    ```
    
    we get this:
    
    ```
    (values (princ 42) (list 1 2))
    => 42
    => (1 2)
    ```
    
    With `UPDATE-ONLY`, the printed output of a form is transcribed only
    if there were output markers in the source. Similarly, with
    `UPDATE-ONLY`, return values are transcribed only if there were value
    markers in the source.
    
    **No Output/Values**
    
    If the form produces no output or returns no values, then whether or
    not output and values are transcribed is controlled by
    `INCLUDE-NO-OUTPUT` and `INCLUDE-NO-VALUE`, respectively. By default,
    neither is on so:
    
    ```
    (values)
    ..
    =>
    ```
    
    is transcribed to
    
    ```
    (values)
    ```
    
    With `UPDATE-ONLY` true, we probably wouldn't like to lose those
    markers since they were put there for a reason. Hence, with
    `UPDATE-ONLY`, `INCLUDE-NO-OUTPUT` and `INCLUDE-NO-VALUE` default to true.
    So, with `UPDATE-ONLY` the above example is transcribed to:
    
    ```
    (values)
    ..
    => ; No value
    ```
    
    where the last line is the `:NO-VALUE` prefix.
    
    **Consistency Checks**
    
    If `CHECK-CONSISTENCY` is true, then `TRANSCRIBE` signals a continuable
    [`TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR`][8492] whenever a form's output as a
    string is different from what was in `INPUT`, provided that `INPUT`
    contained the output. Similarly, for values, a continuable
    [`TRANSCRIPTION-VALUES-CONSISTENCY-ERROR`][238c] is signalled if a value read
    from the source does not print as the as the value returned by `EVAL`.
    This allows readable values to be hand-indented without failing
    consistency checks:
    
    ```
    (list 1 2)
    => ;; This is commented, too.
       (1
          ;; Funny indent.
          2)
    ```
    
    See [Transcript Consistency Checking][f47d] for the full picture.
    
    **Unreadable Values**
    
    The above scheme involves [`READ`][fe58], so consistency of unreadable values
    cannot be treated the same. In fact, unreadable values must even be
    printed differently for transcribe to be able to read them back:
    
    ```
    (defclass some-class () ())
    
    (defmethod print-object ((obj some-class) stream)
      (print-unreadable-object (obj stream :type t)
        (format stream \"~%~%end\")))
    
    (make-instance 'some-class)
    ==> #<SOME-CLASS 
    -->
    --> end>
    ```
    
    where `"==>"` is the `:UNREADABLE` prefix and `"-->"` is the
    `:UNREADABLE-CONTINUATION` prefix. As with outputs, a consistency
    check between an unreadable value from the source and the value from
    `EVAL` is performed with [`STRING=`][4143] by default. That is, the value from
    `EVAL` is printed to a string and compared to the source value. Hence,
    any change to unreadable values will break consistency checks. This
    is most troublesome with instances of classes with the default
    [`PRINT-OBJECT`][3f2e] method printing the memory address. See
    [Finer-Grained Consistency Checks][6e18].
    
    **Errors**
    
    If an [`ERROR`][d162] condition is signalled, the error is printed to the
    output and no values are returned.
    
    ```common-lisp
    (progn
      (print "hello")
      (error "no greeting"))
    ..
    .. "hello" 
    .. debugger invoked on SIMPLE-ERROR:
    ..   no greeting
    ```
    
    To keep the textual representation somewhat likely to be portable,
    the printing is done with `(FORMAT T "#<~S ~S>" (TYPE-OF
    ERROR) (PRINC-TO-STRING ERROR))`. [`SIMPLE-CONDITION`][f2f5]s are formatted to
    strings with [`SIMPLE-CONDITION-FORMAT-CONTROL`][4841] and
    [`SIMPLE-CONDITION-FORMAT-ARGUMENTS`][da14].
    
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
    
    ```
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
    
    **Dynamic Environment**
    
    If `DYNENV` is non-`NIL`, then it must be a function that establishes
    the dynamic environment in which transcription shall take place. It
    is called with a single argument: a thunk (a function of no
    arguments). See [Controlling the Dynamic Environment][6b59] for an example.

<a id="x-28MGL-PAX-3A-2ATRANSCRIBE-CHECK-CONSISTENCY-2A-20VARIABLE-29"></a>

- [variable] **\*TRANSCRIBE-CHECK-CONSISTENCY\*** *NIL*

    The default value of [`TRANSCRIBE`][f1f0]'s `CHECK-CONSISTENCY` argument.

<a id="x-28MGL-PAX-3A-2ATRANSCRIBE-SYNTAXES-2A-20VARIABLE-29"></a>

- [variable] **\*TRANSCRIBE-SYNTAXES\*** *((:DEFAULT (:OUTPUT "..") (:NO-VALUE "=> ; No value") (:READABLE "=>")
  (:UNREADABLE "==>") (:UNREADABLE-CONTINUATION "-->"))
 (:COMMENTED-1 (:OUTPUT ";..") (:NO-VALUE ";=> ; No value") (:READABLE ";=>")
  (:READABLE-CONTINUATION ";->") (:UNREADABLE ";==>")
  (:UNREADABLE-CONTINUATION ";-->"))
 (:COMMENTED-2 (:OUTPUT ";;..") (:NO-VALUE ";;=> ; No value")
  (:READABLE ";;=>") (:READABLE-CONTINUATION ";;->") (:UNREADABLE ";;==>")
  (:UNREADABLE-CONTINUATION ";;-->")))*

    The default syntaxes used by [`TRANSCRIBE`][f1f0] for reading and writing
    lines containing output and values of an evaluated form.
    
    A syntax is a list of of the form `(SYNTAX-ID &REST PREFIXES)` where
    `PREFIXES` is a list of `(PREFIX-ID PREFIX-STRING)` elements. For
    example the syntax `:COMMENTED-1` looks like this:
    
    ```
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
    syntax), then the following value is read with [`READ`][fe58] and printed with
    [`PRIN1`][6384] (hence no need to mark up the following lines).
    
    When writing, an extra space is added automatically if the line to
    be prefixed is not empty. Similarly, the first space following the
    prefix is discarded when reading.
    
    See `TRANSCRIBE` for how the actual syntax to be used is selected.

<a id="x-28MGL-PAX-3ATRANSCRIPTION-ERROR-20CONDITION-29"></a>

- [condition] **TRANSCRIPTION-ERROR** *[ERROR][d162]*

    Represents syntactic errors in the `SOURCE` argument
    of [`TRANSCRIBE`][f1f0] and also serves as the superclass of
    [`TRANSCRIPTION-CONSISTENCY-ERROR`][a249].

<a id="x-28MGL-PAX-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29"></a>

- [condition] **TRANSCRIPTION-CONSISTENCY-ERROR** *[TRANSCRIPTION-ERROR][b81d]*

    A common superclass for
    [`TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR`][8492] and
    [`TRANSCRIPTION-VALUES-CONSISTENCY-ERROR`][238c].

<a id="x-28MGL-PAX-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29"></a>

- [condition] **TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR** *[TRANSCRIPTION-CONSISTENCY-ERROR][a249]*

    Signalled (with [`CERROR`][4317]) by [`TRANSCRIBE`][f1f0] when invoked
    with `:CHECK-CONSISTENCY` and the output of a form is not the same as
    what was parsed.

<a id="x-28MGL-PAX-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29"></a>

- [condition] **TRANSCRIPTION-VALUES-CONSISTENCY-ERROR** *[TRANSCRIPTION-CONSISTENCY-ERROR][a249]*

    Signalled (with [`CERROR`][4317]) by [`TRANSCRIBE`][f1f0] when invoked
    with `:CHECK-CONSISTENCY` and the values of a form are inconsistent
    with their parsed representation.

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-CONISTENCY-CHECKING-20MGL-PAX-3ASECTION-29"></a>

### 9.4 Transcript Consistency Checking

The main use case for consistency checking is detecting
out-of-date examples in documentation, although using it for writing
tests is also a possibility. Here, we focus on the former.

When a markdown code block tagged `cl-transcript` is processed
during [Generating Documentation][2c93], the code in it is replaced with
the output of with `(TRANSCRIBE <CODE> NIL :UPDATE-ONLY T
:CHECK-CONSISTENCY T)`. Suppose we have the following example of the
function `GREET`, that prints `hello` and returns 7.

    ```cl-transcript
    (greet)
    .. hello
    => 7
    ```

Now, if we change `GREET` to print or return something else, a
[`TRANSCRIPTION-CONSISTENCY-ERROR`][a249] will be signalled during
documentation generation. Then we may fix the documentation or
[`CONTINUE`][1867] from the error.

By default, comparisons of previous to current output, readable and
unreadable return values are performed with [`STRING=`][4143], [`EQUAL`][3fb5], and
`STRING=`, respectively, which is great in the simple case.
Non-determinism aside, exact matching becomes brittle as soon as the
notoriously unportable pretty printer is used or when unreadable
objects are printed with their `#<>` syntax, especially when
[`PRINT-UNREADABLE-OBJECT`][9439] is used with `:IDENTITY T`.

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-FINER-GRAINED-CONSISTENCY-CHECKS-20MGL-PAX-3ASECTION-29"></a>

#### 9.4.1 Finer-Grained Consistency Checks

To get around this problem, consistency checking of output,
readable and unreadable values can be customized individually by
supplying [`TRANSCRIBE`][f1f0] with a `CHECK-CONSISTENCY` argument
like `((:OUTPUT <OUTPUT-CHECK>) (:READABLE
<READABLE-CHECK>) (:UNREADABLE <UNREADABLE-CHECK>))`. In this case,
`<OUTPUT-CHECK>` may be `NIL`, `T`, or a function designator.

- If it's `NIL` or there is no `:OUTPUT` entry in the list, then the
  output is not checked for consistency.

- If it's `T`, then the outputs are compared with the default,
  [`STRING=`][4143].

- If it's a function designator, then it's called with two strings
  and must return whether they are consistent with each other.

The case of `<READABLE-CHECK>` and `<UNREADABLE-CHECK>` is similar.

Code blocks tagged `cl-transcript` can take arguments, which they
pass on to `TRANSCRIBE`. The following shows how to check only the
output.

    ```cl-transcript (:check-consistency ((:output t)))
    (error "Oh, no.")
    .. debugger invoked on SIMPLE-ERROR:
    ..   Oh, no.
    
    (make-condition 'simple-error)
    ==> #<SIMPLE-ERROR {1008A81533}>


<a id="x-28MGL-PAX-3A-40TRANSCRIPT-DYNENV-20MGL-PAX-3ASECTION-29"></a>

#### 9.4.2 Controlling the Dynamic Environment

The dynamic environment in which forms in the transcript are
evaluated can be controlled via the `:DYNENV` argument of
`cl-transcript`.

    ```cl-transcript (:dynenv my-transcript)
    ...
    ```

In this case, instead of calling [`TRANSCRIBE`][f1f0] directly, the call will
be wrapped in a function of no arguments and passed to the function
`MY-TRANSCRIPT`, which establishes the desired dynamic environment
and calls its argument. The following definition of `MY-TRANSCRIPT`
simply packages up oft-used settings to `TRANSCRIBE`.

```
(defun my-transcript (fn)
  (let ((*transcribe-check-consistency*
          '((:output my-transcript-output=)
            (:readable equal)
            (:unreadable nil))))
    (funcall fn)))

(defun my-transcript-output= (string1 string2)
  (string= (my-transcript-normalize-output string1)
           (my-transcript-normalize-output string2)))

(defun my-transcript-normalize-output (string)
  (squeeze-whitespace (delete-trailing-whitespace (delete-comments string))))
```

A more involved solution could rebind global variables set in
transcripts, unintern symbols created or even create a temporary
package for evaluation.

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-UTILITIES-FOR-CONSISTENCY-CHECKING-20MGL-PAX-3ASECTION-29"></a>

#### 9.4.3 Utilities for Consistency Checking

<a id="x-28MGL-PAX-3ASQUEEZE-WHITESPACE-20FUNCTION-29"></a>

- [function] **SQUEEZE-WHITESPACE** *STRING*

    Replace consecutive whitespace characters with a single space in
    `STRING`. This is useful to undo the effects of pretty printing when
    building comparison functions for [`TRANSCRIBE`][f1f0].

<a id="x-28MGL-PAX-3ADELETE-TRAILING-WHITESPACE-20FUNCTION-29"></a>

- [function] **DELETE-TRAILING-WHITESPACE** *STRING*

    Delete whitespace characters after the last non-whitespace
    character in each line in `STRING`.

<a id="x-28MGL-PAX-3ADELETE-COMMENTS-20FUNCTION-29"></a>

- [function] **DELETE-COMMENTS** *STRING &KEY (PATTERN ";")*

    For each line in `STRING` delete the rest of the line after and
    including the first occurrence of `PATTERN`. On changed lines, delete
    trailing whitespace too. This function does not parse `STRING` as Lisp
    forms, hence all occurrences of `PATTERN` (even those seemingly in
    string literals) are recognized as comments.
    
    Let's define a comparison function:
    
    ```common-lisp
    (defun string=/no-comments (string1 string2)
      (string= (delete-comments string1) (delete-comments string2)))
    ```
    
    And use it to check consistency of output:
    
        ```cl-transcript (:check-consistency ((:output string=/no-comments)))
        (format t "hello~%world")
        .. hello     ; This is the first line.
        .. world     ; This is the second line.
        ```
    
    Just to make sure the above example works, here it is without being
    quoted.
    
    ```common-lisp
    (format t "hello~%world")
    .. hello     ; This is the first line.
    .. world     ; This is the second line.
    ```


<a id="x-28MGL-PAX-3A-40EXTENSION-API-20MGL-PAX-3ASECTION-29"></a>

## 10 Writing Extensions

<a id="x-28MGL-PAX-3A-40ADDING-NEW-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 10.1 Adding New Locatives

Once everything in [Adding New Locatives][3cf3] has been done,
there are only a couple of PAX generic functions left to extend.

<a id="x-28MGL-PAX-3ADOCUMENT-OBJECT-2A-20GENERIC-FUNCTION-29"></a>

- [generic-function] **DOCUMENT-OBJECT\*** *OBJECT STREAM*

    Write `OBJECT` in [`*FORMAT*`][3da8] to `STREAM`.
    Specialize this on a subclass of [`DREF`][d930] if that subclass is
    not [`RESOLVE`][63b4]able, else on the type of object it resolves to. This
    function is for extension only. Don't call it directly.

<a id="x-28MGL-PAX-3AEXPORTABLE-REFERENCE-P-20GENERIC-FUNCTION-29"></a>

- [generic-function] **EXPORTABLE-REFERENCE-P** *PACKAGE SYMBOL LOCATIVE-TYPE LOCATIVE-ARGS*

    Return true iff `SYMBOL` is to be exported from
    `PACKAGE` when it occurs in a [`DEFSECTION`][72b4] in a reference with
    `LOCATIVE-TYPE` and `LOCATIVE-ARGS`. `SYMBOL` is [accessible][3473]
    in `PACKAGE`.
    
    The default method calls [`EXPORTABLE-LOCATIVE-TYPE-P`][c930] with
    `LOCATIVE-TYPE` and ignores the other arguments.
    
    By default, [`SECTION`][5fac]s and [`GLOSSARY-TERM`][8251]s are not exported although
    they are `EXPORTABLE-LOCATIVE-TYPE-P`. To export symbols naming
    sections from MGL-PAX, the following method could be added:
    
    ```
    (defmethod exportable-reference-p ((package (eql (find-package 'mgl-pax)))
                                       symbol (locative-type (eql 'section))
                                       locative-args)
      t)
    ```


<a id="x-28MGL-PAX-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29"></a>

- [generic-function] **EXPORTABLE-LOCATIVE-TYPE-P** *LOCATIVE-TYPE*

    Return true iff symbols in references with
    `LOCATIVE-TYPE` are to be exported by default when they occur in a
    [`DEFSECTION`][72b4]. The default method returns `T`, while the methods for
    [`SECTION`][5fac], [`GLOSSARY-TERM`][8251], [`PACKAGE`][1d5a], [`ASDF:SYSTEM`][c097], [`METHOD`][51c3] and [`INCLUDE`][5cd7]
    return `NIL`.
    
    This function is called by the default method of
    [`EXPORTABLE-REFERENCE-P`][e51f] to decide what symbols `DEFSECTION` shall
    export when its `EXPORT` argument is true.

Also note that due to the [Home Section][bdd5] logic, especially for
locative types with string names, [`DREF-EXT:DOCSTRING*`][9fd4] should
probably return a non-`NIL` package.

<a id="x-28MGL-PAX-3A-40LOCATIVE-ALIASES-20MGL-PAX-3ASECTION-29"></a>

### 10.2 Locative Aliases

[`DEFINE-LOCATIVE-ALIAS`][548e] can be used to help [`M-.`][3386] and [autolinking][b3cc]
disambiguate references based on the context of a [name][88cf] as described
on [Parsing][378f] and also in [Specified Locative][8996].

The following example shows how to make docstrings read
more naturally by defining an alias.

```
(defclass my-string ()
  ())

(defgeneric my-string (obj)
  (:documentation "Convert OBJ to MY-STRING."))

;;; This version of FOO has a harder to read docstring because
;;; it needs to disambiguate the MY-STRING reference.
(defun foo (x)
  "FOO takes and argument X, a [MY-STRING][class] object.")

;;; Define OBJECT as an alias for the CLASS locative.
(define-locative-alias object class)

;;; Note how no explicit link is needed anymore.
(defun foo (x)
  "FOO takes an argument X, a MY-CLASS object.")
```

Similary, defining the indefinite articles as aliases of the [`CLASS`][2060]
locative can reduce the need for explicit linking.

```
(define-locative-alias a class)
(define-locative-alias an class)
```

Since these are unlikely to be universally helpful, make sure not to
export the symbols `A` and `AN`.

<a id="x-28MGL-PAX-3A-40EXTENDING-DOCUMENT-20MGL-PAX-3ASECTION-29"></a>

### 10.3 Extending `DOCUMENT`

For all definitions that it encounters, [`DOCUMENT`][432c] calls
[`DOCUMENT-OBJECT*`][8269] to generate documentation. The following utilities
are for writing new `DOCUMENT-OBJECT*` methods, which emit markdown.

<a id="x-28MGL-PAX-3A-2AFORMAT-2A-20VARIABLE-29"></a>

- [variable] **\*FORMAT\***

    Bound by [`DOCUMENT`][432c] to its `FORMAT` argument, this allows markdown
    output to depend on the output format.

<a id="x-28MGL-PAX-3AWITH-HEADING-20MGL-PAX-3AMACRO-29"></a>

- [macro] **WITH-HEADING** *(STREAM OBJECT TITLE &KEY LINK-TITLE-TO) &BODY BODY*

    Write a markdown heading with `TITLE` to `STREAM`. Nested `WITH-HEADING`s
    produce nested headings. If [`*DOCUMENT-LINK-SECTIONS*`][1b28], generate
    anchors based on the [definition of][8f19] `OBJECT`. `LINK-TITLE-TO`
    behaves like the `LINK-TITLE-TO` argument of [`DEFSECTION`][72b4].

<a id="x-28MGL-PAX-3ADOCUMENTING-REFERENCE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DOCUMENTING-REFERENCE** *(STREAM &KEY REFERENCE NAME PACKAGE READTABLE ARGLIST) &BODY BODY*

    Write `REFERENCE` to `STREAM` as described in
    [`*DOCUMENT-MARK-UP-SIGNATURES*`][8fb6], and establish `REFERENCE` as a [local
    reference][4c96] for the processing of `BODY`.
    
    - `REFERENCE` defaults to the reference being [`DOCUMENT`][432c]ed.
    
    - `NAME` defaults to `(XREF-NAME REFERENCE)` and is printed after the
      [`LOCATIVE-TYPE`][97ba].
    
    - [`*PACKAGE*`][5ed1] and [`*READTABLE*`][b79a] are bound to `PACKAGE` and `READTABLE` for
      the duration of printing the `ARGLIST` and the processing of `BODY`.
      If either is `NIL`, then a default value is computed as described in
      [Package and Readtable][ab7e].
    
    - If `ARGLIST` is `NIL`, then it is not printed.
    
    - If `ARGLIST` is a list, then it is must be a [lambda list][98ff] and
      is printed without the outermost parens and with the package names
      removed from the argument names.
    
    - If `ARGLIST` is a string, then it must be valid markdown.
    
    - It is not allowed to have [`WITH-HEADING`][80e8] within the [dynamic
      extent][36e9] of `BODY`.


<a id="x-28MGL-PAX-3AWITH-DISLOCATED-NAMES-20MGL-PAX-3AMACRO-29"></a>

- [macro] **WITH-DISLOCATED-NAMES** *NAMES &BODY BODY*

    For each name in `NAMES`, establish a [local
    reference][4c96] with the [`DISLOCATED`][e391] locative, which
    [prevents autolinking][8c16].

<a id="x-28MGL-PAX-3ADOCUMENT-DOCSTRING-20FUNCTION-29"></a>

- [function] **DOCUMENT-DOCSTRING** *DOCSTRING STREAM &KEY (INDENTATION "    ") EXCLUDE-FIRST-LINE-P (PARAGRAPHP T)*

    Write `DOCSTRING` to `STREAM`, [sanitizing the markdown][7bf5] from it, performing [Codification][f1ab] and
    [Linking to Code][1865], finally prefixing each line with `INDENTATION`. The
    prefix is not added to the first line if `EXCLUDE-FIRST-LINE-P`. If
    `PARAGRAPHP`, then add a newline before and after the output.

<a id="x-28MGL-PAX-3AESCAPE-MARKDOWN-20FUNCTION-29"></a>

- [function] **ESCAPE-MARKDOWN** *STRING &KEY (ESCAPE-INLINE T) (ESCAPE-HTML T) (ESCAPE-BLOCK T)*

    Backslash escape markdown constructs in `STRING`.
    
    - If `ESCAPE-INLINE`, then escape ``*_`[]\`` characters.
    
    - If `ESCAPE-HTML`, then escape `<&` characters.
    
    - If `ESCAPE-BLOCK`, then escape whatever is necessary to avoid
      starting a new markdown block (e.g. a paragraph, heading, etc).


<a id="x-28MGL-PAX-3APRIN1-TO-MARKDOWN-20FUNCTION-29"></a>

- [function] **PRIN1-TO-MARKDOWN** *OBJECT &KEY (ESCAPE-INLINE T) (ESCAPE-HTML T) (ESCAPE-BLOCK T)*

    Like [`PRIN1-TO-STRING`][18e1], but bind [`*PRINT-CASE*`][443b] depending on
    [`*DOCUMENT-DOWNCASE-UPPERCASE-CODE*`][a5ee] and [`*FORMAT*`][3da8], and
    [`ESCAPE-MARKDOWN`][3026].

<a id="x-28MGL-PAX-3A-40SECTIONS-20MGL-PAX-3ASECTION-29"></a>

### 10.4 Sections

[`SECTION`][5fac] objects rarely need to be dissected since
[`DEFSECTION`][72b4] and [`DOCUMENT`][432c] cover most needs. However, it is plausible
that one wants to subclass them and maybe redefine how they are
presented.

<a id="x-28MGL-PAX-3ASECTION-20CLASS-29"></a>

- [class] **SECTION**

    [`DEFSECTION`][72b4] stores its `NAME`, `TITLE`, [`PACKAGE`][1d5a],
    [`READTABLE`][d646] and `ENTRIES` arguments in [`SECTION`][5fac]
    objects.

<a id="x-28MGL-PAX-3ASECTION-NAME-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>

- [reader] **SECTION-NAME** *SECTION (:NAME)*

    The name of the global variable whose value is
    this [`SECTION`][5fac] object.

<a id="x-28MGL-PAX-3ASECTION-PACKAGE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>

- [reader] **SECTION-PACKAGE** *SECTION (:PACKAGE)*

    [`*PACKAGE*`][5ed1] will be bound to this package when
    generating documentation for this section.

<a id="x-28MGL-PAX-3ASECTION-READTABLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>

- [reader] **SECTION-READTABLE** *SECTION (:READTABLE)*

    [`*READTABLE*`][b79a] will be bound to this when generating
    documentation for this section.

<a id="x-28MGL-PAX-3ASECTION-TITLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>

- [reader] **SECTION-TITLE** *SECTION (:TITLE)*

    A markdown string or `NIL`. Used in generated
    documentation.

<a id="x-28MGL-PAX-3ASECTION-LINK-TITLE-TO-20FUNCTION-29"></a>

- [function] **SECTION-LINK-TITLE-TO** *SECTION*

<a id="x-28MGL-PAX-3ASECTION-ENTRIES-20FUNCTION-29"></a>

- [function] **SECTION-ENTRIES** *SECTION*

    A list of markdown docstrings and [`XREF`][1538]s in the order they
    occurred in [`DEFSECTION`][72b4].

<a id="x-28MGL-PAX-3A-40GLOSSARY-TERMS-20MGL-PAX-3ASECTION-29"></a>

### 10.5 Glossary Terms

[`GLOSSARY-TERM`][8251] objects rarely need to be dissected since
[`DEFINE-GLOSSARY-TERM`][8ece] and [`DOCUMENT`][432c] cover most needs. However, it is
plausible that one wants to subclass them and maybe redefine how
they are presented.

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-20CLASS-29"></a>

- [class] **GLOSSARY-TERM**

    See [`DEFINE-GLOSSARY-TERM`][8ece].

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-NAME-20-28MGL-PAX-3AREADER-20MGL-PAX-3AGLOSSARY-TERM-29-29"></a>

- [reader] **GLOSSARY-TERM-NAME** *GLOSSARY-TERM (:NAME)*

    The name of the global variable whose value is
    this [`GLOSSARY-TERM`][8251] object.

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-TITLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3AGLOSSARY-TERM-29-29"></a>

- [reader] **GLOSSARY-TERM-TITLE** *GLOSSARY-TERM (:TITLE)*

    A markdown string or `NIL`. Used in generated
    documentation (see [Output Details][af6f]).

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-URL-20-28MGL-PAX-3AREADER-20MGL-PAX-3AGLOSSARY-TERM-29-29"></a>

- [reader] **GLOSSARY-TERM-URL** *GLOSSARY-TERM (:URL)*

    A string or `NIL`.

  [00d4]: dref/README.md#x-28MGL-PAX-3AACCESSOR-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:ACCESSOR MGL-PAX:LOCATIVE"
  [0317]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pn.htm "PATHNAME (MGL-PAX:CLHS CLASS)"
  [03fa]: http://www.lispworks.com/documentation/HyperSpec/Body/f_class_.htm "CLASS-NAME (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [090c]: dref/README.md#x-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:STRUCTURE-ACCESSOR MGL-PAX:LOCATIVE"
  [0b3a]: dref/README.md#x-28MGL-PAX-3ALOCATIVE-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:LOCATIVE MGL-PAX:LOCATIVE"
  [0c4f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_export.htm "EXPORT (MGL-PAX:CLHS FUNCTION)"
  [0d6e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eval.htm "EVAL (MGL-PAX:CLHS FUNCTION)"
  [0ef0]: #x-28MGL-PAX-3A-2ADOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS* VARIABLE"
  [0fa3]: #x-28MGL-PAX-3A-40LOCATIVE-ALIASES-20MGL-PAX-3ASECTION-29 "Locative Aliases"
  [119e]: http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm "FUNCTION (MGL-PAX:CLHS CLASS)"
  [1281]: #x-28MGL-PAX-3A-40PAX-WORLD-20MGL-PAX-3ASECTION-29 "PAX World"
  [1322]: https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks "fenced code blocks"
  [13a9]: #x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-READMES-20FUNCTION-29 "MGL-PAX:UPDATE-ASDF-SYSTEM-READMES FUNCTION"
  [1538]: dref/README.md#x-28DREF-3AXREF-20CLASS-29 "DREF:XREF CLASS"
  [172e]: dref/README.md#x-28METHOD-20MGL-PAX-3ALOCATIVE-29 "METHOD MGL-PAX:LOCATIVE"
  [1743]: https://emacs-w3m.github.io/info/emacs-w3m_10.html#Key-Binding "w3m's default key bindings"
  [17e0]: #x-28MGL-PAX-3A-2ADOCUMENT-URL-VERSIONS-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-URL-VERSIONS* VARIABLE"
  [1865]: #x-28MGL-PAX-3A-40LINKING-TO-CODE-20MGL-PAX-3ASECTION-29 "Linking to Code"
  [1867]: http://www.lispworks.com/documentation/HyperSpec/Body/r_contin.htm "CONTINUE (MGL-PAX:CLHS RESTART)"
  [18e1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_to_.htm "PRIN1-TO-STRING (MGL-PAX:CLHS FUNCTION)"
  [1904]: https://github.com/3b/3bmd "3BMD"
  [1b1b]: #x-28MGL-PAX-3A-40DOCUMENTATION-UTILITIES-20MGL-PAX-3ASECTION-29 "Utilities for Generating Documentation"
  [1b28]: #x-28MGL-PAX-3A-2ADOCUMENT-LINK-SECTIONS-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-LINK-SECTIONS* VARIABLE"
  [1d5a]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm "PACKAGE (MGL-PAX:CLHS CLASS)"
  [1e80]: #x-28MGL-PAX-3A-40PAX-URLS-20MGL-PAX-3ASECTION-29 "PAX URLs"
  [1f37]: http://www.lispworks.com/documentation/HyperSpec/Body/t_class.htm "CLASS (MGL-PAX:CLHS CLASS)"
  [2060]: dref/README.md#x-28CLASS-20MGL-PAX-3ALOCATIVE-29 "CLASS MGL-PAX:LOCATIVE"
  [22c2]: #x-28MGL-PAX-3A-40LINKING-TO-SECTIONS-20MGL-PAX-3ASECTION-29 "Linking to Sections"
  [238c]: #x-28MGL-PAX-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29 "MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR CONDITION"
  [2415]: README.md "PAX Manual"
  [2444]: dref/README.md#x-28DREF-EXT-3ALOCATIVE-ARGS-20FUNCTION-29 "DREF-EXT:LOCATIVE-ARGS FUNCTION"
  [2634]: #x-28MGL-PAX-3A-40OVERVIEW-OF-ESCAPING-20MGL-PAX-3ASECTION-29 "Overview of Escaping"
  [292a]: #x-28MGL-PAX-3A-40PAX-LOCATIVES-20MGL-PAX-3ASECTION-29 "PAX Locatives"
  [2c93]: #x-28MGL-PAX-3A-40GENERATING-DOCUMENTATION-20MGL-PAX-3ASECTION-29 "Generating Documentation"
  [2ca9]: #x-28MGL-PAX-3AOUTPUT-REFLINK-20FUNCTION-29 "MGL-PAX:OUTPUT-REFLINK FUNCTION"
  [2d48]: #x-28MGL-PAX-3AWITH-DISLOCATED-NAMES-20MGL-PAX-3AMACRO-29 "MGL-PAX:WITH-DISLOCATED-NAMES MGL-PAX:MACRO"
  [2e45]: #x-28MGL-PAX-3A-40DOCUMENTABLES-20MGL-PAX-3ASECTION-29 "Documentables"
  [2f82]: #x-28MGL-PAX-3A-40AMBIGUOUS-UNSPECIFIED-LOCATIVE-20MGL-PAX-3ASECTION-29 "Ambiguous Unspecified Locative"
  [3026]: #x-28MGL-PAX-3AESCAPE-MARKDOWN-20FUNCTION-29 "MGL-PAX:ESCAPE-MARKDOWN FUNCTION"
  [3076]: https://github.com/redline6561/colorize/ "Colorize"
  [32da]: dref/README.md#x-28DREF-3ASOURCE-LOCATION-20FUNCTION-29 "DREF:SOURCE-LOCATION FUNCTION"
  [3386]: #x-28MGL-PAX-3A-40NAVIGATING-IN-EMACS-20MGL-PAX-3ASECTION-29 "Navigating Sources in Emacs"
  [3473]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_s.htm "FIND-SYMBOL (MGL-PAX:CLHS FUNCTION)"
  [35a2]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#setf_expander '"setf expander" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [36e1]: #x-28MGL-PAX-3A-40HTML-OUTPUT-20MGL-PAX-3ASECTION-29 "HTML Output"
  [36e9]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#dynamic_extent '"dynamic extent" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [378f]: #x-28MGL-PAX-3A-40PARSING-20MGL-PAX-3ASECTION-29 "Parsing"
  [3808]: http://www.lispworks.com/documentation/HyperSpec/Body/f_terpri.htm "FRESH-LINE (MGL-PAX:CLHS FUNCTION)"
  [3cf3]: dref/README.md#x-28DREF-EXT-3A-40ADDING-NEW-LOCATIVES-20MGL-PAX-3ASECTION-29 "Adding New Locatives"
  [3da8]: #x-28MGL-PAX-3A-2AFORMAT-2A-20VARIABLE-29 "MGL-PAX:*FORMAT* VARIABLE"
  [3f2e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pr_obj.htm "PRINT-OBJECT (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [3fb5]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm "EQUAL (MGL-PAX:CLHS FUNCTION)"
  [407c]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_h.htm#home_package '"home package" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [4143]: http://www.lispworks.com/documentation/HyperSpec/Body/f_stgeq_.htm "STRING= (MGL-PAX:CLHS FUNCTION)"
  [4317]: http://www.lispworks.com/documentation/HyperSpec/Body/f_cerror.htm "CERROR (MGL-PAX:CLHS FUNCTION)"
  [432c]: #x-28MGL-PAX-3ADOCUMENT-20FUNCTION-29 "MGL-PAX:DOCUMENT FUNCTION"
  [43bd]: dref/README.md#x-28DREF-3A-40REFERENCE-20MGL-PAX-3AGLOSSARY-TERM-29 "reference"
  [443b]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_cas.htm "*PRINT-CASE* (MGL-PAX:CLHS VARIABLE)"
  [4796]: dref/README.md#x-28LAMBDA-20MGL-PAX-3ALOCATIVE-29 "LAMBDA MGL-PAX:LOCATIVE"
  [479a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_abortc.htm "ABORT (MGL-PAX:CLHS FUNCTION)"
  [4841]: http://www.lispworks.com/documentation/HyperSpec/Body/f_smp_cn.htm "SIMPLE-CONDITION-FORMAT-CONTROL (MGL-PAX:CLHS FUNCTION)"
  [48f1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rdtabl.htm "READTABLE-CASE (MGL-PAX:CLHS FUNCTION)"
  [4bb8]: #x-28-22mgl-pax-2Fdocument-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"mgl-pax/document" ASDF/SYSTEM:SYSTEM'
  [4c96]: #x-28MGL-PAX-3A-40LOCAL-REFERENCES-20MGL-PAX-3ASECTION-29 "Local References"
  [5119]: #x-28MGL-PAX-3AGLOSSARY-TERM-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:GLOSSARY-TERM MGL-PAX:LOCATIVE"
  [51c3]: http://www.lispworks.com/documentation/HyperSpec/Body/t_method.htm "METHOD (MGL-PAX:CLHS CLASS)"
  [5225]: dref/README.md "DRef Manual"
  [524e]: #x-28MGL-PAX-3A-40UNSPECIFIED-LOCATIVE-20MGL-PAX-3ASECTION-29 "Unspecified Locative"
  [548e]: dref/README.md#x-28DREF-EXT-3ADEFINE-LOCATIVE-ALIAS-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-LOCATIVE-ALIAS MGL-PAX:MACRO"
  [54d8]: #x-28MGL-PAX-3A-40ADDING-NEW-LOCATIVES-20MGL-PAX-3ASECTION-29 "Adding New Locatives"
  [574a]: #x-28MGL-PAX-3A-40EXTENDING-DOCUMENT-20MGL-PAX-3ASECTION-29 "Extending `DOCUMENT`"
  [5825]: #x-28-22mgl-pax-2Ftranscribe-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"mgl-pax/transcribe" ASDF/SYSTEM:SYSTEM'
  [5875]: dref/README.md#x-28GENERIC-FUNCTION-20MGL-PAX-3ALOCATIVE-29 "GENERIC-FUNCTION MGL-PAX:LOCATIVE"
  [587f]: #x-28MGL-PAX-3AMAKE-GIT-SOURCE-URI-FN-20FUNCTION-29 "MGL-PAX:MAKE-GIT-SOURCE-URI-FN FUNCTION"
  [5884]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_.htm "FIND-IF (MGL-PAX:CLHS FUNCTION)"
  [58f6]: #x-28GO-20MGL-PAX-3ALOCATIVE-29 "GO MGL-PAX:LOCATIVE"
  [5cd7]: #x-28MGL-PAX-3AINCLUDE-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:INCLUDE MGL-PAX:LOCATIVE"
  [5ed1]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pkg.htm "*PACKAGE* (MGL-PAX:CLHS VARIABLE)"
  [5fac]: #x-28MGL-PAX-3ASECTION-20CLASS-29 "MGL-PAX:SECTION CLASS"
  [5fc4]: dref/README.md#x-28DREF-3A-40NAME-20MGL-PAX-3AGLOSSARY-TERM-29 "name"
  [6067]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#destructuring_lambda_list '"destructuring lambda list" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [6300]: #x-28MGL-PAX-3A-40TRANSCRIPTS-20MGL-PAX-3ASECTION-29 "Transcripts"
  [6334]: dref/README.md#x-28DREF-EXT-3ALOCATE-ERROR-20CONDITION-29 "DREF-EXT:LOCATE-ERROR CONDITION"
  [6384]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRIN1 (MGL-PAX:CLHS FUNCTION)"
  [63b4]: dref/README.md#x-28DREF-3ARESOLVE-20FUNCTION-29 "DREF:RESOLVE FUNCTION"
  [63ef]: http://www.lispworks.com/documentation/HyperSpec/Issues/iss009_w.htm '"ISSUE:AREF-1D" (MGL-PAX:CLHS MGL-PAX:SECTION)'
  [63f3]: #x-28MGL-PAX-3ADEFINE-PACKAGE-20MGL-PAX-3AMACRO-29 "MGL-PAX:DEFINE-PACKAGE MGL-PAX:MACRO"
  [64be]: #x-28MGL-PAX-3AUNRESOLVABLE-REFLINK-20CONDITION-29 "MGL-PAX:UNRESOLVABLE-REFLINK CONDITION"
  [6547]: http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm "OPEN (MGL-PAX:CLHS FUNCTION)"
  [65b4]: dref/README.md#x-28DREF-3ADREF-APROPOS-20FUNCTION-29 "DREF:DREF-APROPOS FUNCTION"
  [66c6]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*ERROR-OUTPUT* (MGL-PAX:CLHS VARIABLE)"
  [672f]: #x-28MGL-PAX-3ASECTION-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:SECTION MGL-PAX:LOCATIVE"
  [6832]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defmet.htm "DEFMETHOD (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [685e]: #x-28MGL-PAX-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29 "Introduction"
  [6ab0]: #x-28MGL-PAX-3A-2ADOCUMENT-FANCY-HTML-NAVIGATION-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-FANCY-HTML-NAVIGATION* VARIABLE"
  [6af6]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PPRINT (MGL-PAX:CLHS FUNCTION)"
  [6b59]: #x-28MGL-PAX-3A-40TRANSCRIPT-DYNENV-20MGL-PAX-3ASECTION-29 "Controlling the Dynamic Environment"
  [6be7]: https://slime.common-lisp.dev/ "SLIME"
  [6c83]: dref/README.md#x-28VARIABLE-20MGL-PAX-3ALOCATIVE-29 "VARIABLE MGL-PAX:LOCATIVE"
  [6e18]: #x-28MGL-PAX-3A-40TRANSCRIPT-FINER-GRAINED-CONSISTENCY-CHECKS-20MGL-PAX-3ASECTION-29 "Finer-Grained Consistency Checks"
  [6f51]: http://www.lispworks.com/documentation/HyperSpec/Body/r_muffle.htm "MUFFLE-WARNING (MGL-PAX:CLHS RESTART)"
  [6fdb]: #x-28-22mgl-pax-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"mgl-pax" ASDF/SYSTEM:SYSTEM'
  [72b4]: #x-28MGL-PAX-3ADEFSECTION-20MGL-PAX-3AMACRO-29 "MGL-PAX:DEFSECTION MGL-PAX:MACRO"
  [730f]: #x-28MGL-PAX-3A-2ADISCARD-DOCUMENTATION-P-2A-20VARIABLE-29 "MGL-PAX:*DISCARD-DOCUMENTATION-P* VARIABLE"
  [7328]: http://www.lispworks.com/documentation/HyperSpec/Body/f_apropo.htm "APROPOS-LIST (MGL-PAX:CLHS FUNCTION)"
  [7445]: #x-28MGL-PAX-3A-40INTERESTING-20MGL-PAX-3AGLOSSARY-TERM-29 "interesting"
  [7ac8]: dref/README.md#x-28DREF-3A-40LOCATIVE-20MGL-PAX-3AGLOSSARY-TERM-29 "locative"
  [7bf5]: #x-28MGL-PAX-3A-40MARKDOWN-IN-DOCSTRINGS-20MGL-PAX-3ASECTION-29 "Markdown in Docstrings"
  [7c82]: #x-28MGL-PAX-3A-40MISCELLANEOUS-DOCUMENTATION-PRINTER-VARIABLES-20MGL-PAX-3ASECTION-29 "Miscellaneous Variables"
  [7cc3]: #x-28MGL-PAX-3A-40LINKING-TO-THE-HYPERSPEC-20MGL-PAX-3ASECTION-29 "Linking to the Hyperspec"
  [7d18]: http://c2.com/cgi/wiki?OnceAndOnlyOnce "OAOO"
  [7dc7]: #x-28MGL-PAX-3A-40DOCUMENT-RETURN-20MGL-PAX-3ASECTION-29 "Return Values"
  [7e92]: dref/README.md#x-28DREF-3ADREF-20FUNCTION-29 "DREF:DREF FUNCTION"
  [7f1f]: #x-28MGL-PAX-3ADOCUMENT-DOCSTRING-20FUNCTION-29 "MGL-PAX:DOCUMENT-DOCSTRING FUNCTION"
  [80e8]: #x-28MGL-PAX-3AWITH-HEADING-20MGL-PAX-3AMACRO-29 "MGL-PAX:WITH-HEADING MGL-PAX:MACRO"
  [81f7]: http://www.lispworks.com/documentation/HyperSpec/Body/s_fn.htm "FUNCTION (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [8251]: #x-28MGL-PAX-3AGLOSSARY-TERM-20CLASS-29 "MGL-PAX:GLOSSARY-TERM CLASS"
  [8269]: #x-28MGL-PAX-3ADOCUMENT-OBJECT-2A-20GENERIC-FUNCTION-29 "MGL-PAX:DOCUMENT-OBJECT* GENERIC-FUNCTION"
  [82f7]: http://www.lispworks.com/documentation/HyperSpec/Body/v_rd_eva.htm "*READ-EVAL* (MGL-PAX:CLHS VARIABLE)"
  [83d5]: #x-28MGL-PAX-3A-40BROWSING-WITH-W3M-20MGL-PAX-3ASECTION-29 "Browsing with w3m"
  [83e1]: http://www.lispworks.com/documentation/HyperSpec/Body/e_cnd.htm "CONDITION (MGL-PAX:CLHS CONDITION)"
  [8423]: #x-28MGL-PAX-3A-40TRANSCRIPT-UTILITIES-FOR-CONSISTENCY-CHECKING-20MGL-PAX-3ASECTION-29 "Utilities for Consistency Checking"
  [8492]: #x-28MGL-PAX-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29 "MGL-PAX:TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR CONDITION"
  [8541]: #x-28MGL-PAX-3A-40EMACS-SETUP-20MGL-PAX-3ASECTION-29 "Emacs Setup"
  [8710]: #x-28MGL-PAX-3AARGUMENT-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:ARGUMENT MGL-PAX:LOCATIVE"
  [875e]: #x-28MGL-PAX-3A-2ADOCUMENT-LINK-TO-HYPERSPEC-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-LINK-TO-HYPERSPEC* VARIABLE"
  [876d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ensu_1.htm "ENSURE-DIRECTORIES-EXIST (MGL-PAX:CLHS FUNCTION)"
  [88a7]: #x-28MGL-PAX-3ASECTION-READTABLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29 "MGL-PAX:SECTION-READTABLE (MGL-PAX:READER MGL-PAX:SECTION)"
  [88cf]: #x-28MGL-PAX-3A-40NAME-20MGL-PAX-3AGLOSSARY-TERM-29 "name"
  [8996]: #x-28MGL-PAX-3A-40SPECIFIED-LOCATIVE-20MGL-PAX-3ASECTION-29 "Specified Locative"
  [8a58]: #x-28MGL-PAX-3A-40SECTIONS-20MGL-PAX-3ASECTION-29 "Sections"
  [8c16]: #x-28MGL-PAX-3A-40PREVENTING-AUTOLINKING-20MGL-PAX-3ASECTION-29 "Preventing Autolinking"
  [8ece]: #x-28MGL-PAX-3ADEFINE-GLOSSARY-TERM-20MGL-PAX-3AMACRO-29 "MGL-PAX:DEFINE-GLOSSARY-TERM MGL-PAX:MACRO"
  [8f19]: dref/README.md#x-28DREF-3ALOCATE-20FUNCTION-29 "DREF:LOCATE FUNCTION"
  [8fb6]: #x-28MGL-PAX-3A-2ADOCUMENT-MARK-UP-SIGNATURES-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-MARK-UP-SIGNATURES* VARIABLE"
  [90fa]: #x-28MGL-PAX-3A-2ADOCUMENT-HTML-DEFAULT-STYLE-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-HTML-DEFAULT-STYLE* VARIABLE"
  [9172]: http://www.lispworks.com/documentation/HyperSpec/Body/t_t.htm "T (MGL-PAX:CLHS CLASS)"
  [935f]: http://www.lispworks.com/documentation/HyperSpec/Issues/iss045.htm '"SUMMARY:CHARACTER-PROPOSAL:2-6-5" (MGL-PAX:CLHS MGL-PAX:SECTION)'
  [9439]: http://www.lispworks.com/documentation/HyperSpec/Body/m_pr_unr.htm "PRINT-UNREADABLE-OBJECT (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [94c7]: #x-28MGL-PAX-3A-40BASICS-20MGL-PAX-3ASECTION-29 "Basics"
  [97ba]: dref/README.md#x-28DREF-EXT-3ALOCATIVE-TYPE-20FUNCTION-29 "DREF-EXT:LOCATIVE-TYPE FUNCTION"
  [98ff]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lambda_list '"lambda list" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [99b0]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#setf_function '"setf function" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [9a7b]: #x-28MGL-PAX-3A-40EMACS-SETUP-FOR-BROWSING-20MGL-PAX-3ASECTION-29 "Emacs Setup for Browsing"
  [9b43]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm "DEFPACKAGE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [9c7d]: #x-28MGL-PAX-3A-40PAGES-20MGL-PAX-3ASECTION-29 "Pages"
  [9dbc]: #x-28MGL-PAX-3A-40TRANSCRIPT-API-20MGL-PAX-3ASECTION-29 "Transcript API"
  [9fd4]: dref/README.md#x-28DREF-EXT-3ADOCSTRING-2A-20GENERIC-FUNCTION-29 "DREF-EXT:DOCSTRING* GENERIC-FUNCTION"
  [a11d]: dref/README.md#x-28DREF-3A-40LOCATIVE-TYPE-20MGL-PAX-3AGLOSSARY-TERM-29 "locative type"
  [a17d]: #x-28MGL-PAX-3A-40MATHJAX-20MGL-PAX-3ASECTION-29 "MathJax"
  [a249]: #x-28MGL-PAX-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29 "MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR CONDITION"
  [a317]: https://daringfireball.net/projects/markdown/ "markdown"
  [a595]: #x-28MGL-PAX-3A-40BROWSING-LIVE-DOCUMENTATION-20MGL-PAX-3ASECTION-29 "Browsing Live Documentation"
  [a5b1]: #x-28MGL-PAX-3ASECTION-PACKAGE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29 "MGL-PAX:SECTION-PACKAGE (MGL-PAX:READER MGL-PAX:SECTION)"
  [a5ee]: #x-28MGL-PAX-3A-2ADOCUMENT-DOWNCASE-UPPERCASE-CODE-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-DOWNCASE-UPPERCASE-CODE* VARIABLE"
  [a843]: http://www.lispworks.com/documentation/HyperSpec/Body/t_std_ob.htm "STANDARD-OBJECT (MGL-PAX:CLHS CLASS)"
  [a951]: dref/README.md#x-28MGL-PAX-3AUNKNOWN-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:UNKNOWN MGL-PAX:LOCATIVE"
  [ab7e]: #x-28MGL-PAX-3A-40PACKAGE-AND-READTABLE-20MGL-PAX-3ASECTION-29 "Package and Readtable"
  [ad78]: http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm "FORMAT (MGL-PAX:CLHS FUNCTION)"
  [ad80]: dref/README.md#x-28DREF-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29 "Introduction"
  [ad94]: #x-28MGL-PAX-3A-40UNAMBIGUOUS-UNSPECIFICED-LOCATIVE-20MGL-PAX-3ASECTION-29 "Unambiguous Unspecified Locative"
  [aeb6]: http://www.lispworks.com/documentation/HyperSpec/Body/a_fn.htm "FUNCTION MGL-PAX:CLHS"
  [af6f]: #x-28MGL-PAX-3A-40OUTPUT-DETAILS-20MGL-PAX-3ASECTION-29 "Output Details"
  [af78]: #x-28MGL-PAX-3AGLOSSARY-TERM-TITLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3AGLOSSARY-TERM-29-29 "MGL-PAX:GLOSSARY-TERM-TITLE (MGL-PAX:READER MGL-PAX:GLOSSARY-TERM)"
  [affc]: dref/README.md#x-28MGL-PAX-3ADOCSTRING-20FUNCTION-29 "MGL-PAX:DOCSTRING FUNCTION"
  [b18e]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#property_list '"property list" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [b372]: #x-28MGL-PAX-3A-40UNRESOLVABLE-REFLINKS-20MGL-PAX-3ASECTION-29 "Unresolvable Links"
  [b3cc]: #x-28MGL-PAX-3A-40EXPLICIT-AND-AUTOLINKING-20MGL-PAX-3ASECTION-29 "Explicit and Autolinking"
  [b4f0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_intern.htm "INTERN (MGL-PAX:CLHS FUNCTION)"
  [b6c4]: dref/README.md#x-28DREF-EXT-3ADEFINE-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-LOCATIVE-TYPE MGL-PAX:MACRO"
  [b79a]: http://www.lispworks.com/documentation/HyperSpec/Body/v_rdtabl.htm "*READTABLE* (MGL-PAX:CLHS VARIABLE)"
  [b7fc]: #x-28MGL-PAX-3A-40APROPOS-20MGL-PAX-3ASECTION-29 "Apropos"
  [b81d]: #x-28MGL-PAX-3ATRANSCRIPTION-ERROR-20CONDITION-29 "MGL-PAX:TRANSCRIPTION-ERROR CONDITION"
  [b88e]: dref/README.md#x-28DREF-EXT-3AXREF-NAME-20-28MGL-PAX-3AREADER-20DREF-3AXREF-29-29 "DREF-EXT:XREF-NAME (MGL-PAX:READER DREF:XREF)"
  [b89a]: #x-28MGL-PAX-3A-40CODIFIABLE-20MGL-PAX-3AGLOSSARY-TERM-29 "codifiable"
  [b8a8]: #x-28MGL-PAX-3ADOCUMENTING-REFERENCE-20MGL-PAX-3AMACRO-29 "MGL-PAX:DOCUMENTING-REFERENCE MGL-PAX:MACRO"
  [b8b4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_abortc.htm "MUFFLE-WARNING (MGL-PAX:CLHS FUNCTION)"
  [b93c]: http://www.lispworks.com/documentation/HyperSpec/Body/t_string.htm "STRING (MGL-PAX:CLHS CLASS)"
  [ba62]: dref/README.md#x-28FUNCTION-20MGL-PAX-3ALOCATIVE-29 "FUNCTION MGL-PAX:LOCATIVE"
  [ba74]: #x-28MGL-PAX-3A-40LINKS-20MGL-PAX-3ASECTION-29 "Links and Systems"
  [bb12]: #x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-HTML-DOCS-20FUNCTION-29 "MGL-PAX:UPDATE-ASDF-SYSTEM-HTML-DOCS FUNCTION"
  [bc83]: #x-28MGL-PAX-3A-40MARKDOWN-SYNTAX-HIGHLIGHTING-20MGL-PAX-3ASECTION-29 "Syntax Highlighting"
  [bcb6]: http://www.lispworks.com/documentation/HyperSpec/Body/e_warnin.htm "WARNING (MGL-PAX:CLHS CONDITION)"
  [bdd5]: #x-28MGL-PAX-3A-40HOME-SECTION-20MGL-PAX-3ASECTION-29 "Home Section"
  [bf0f]: dref/README.md#x-28DREF-3A-40LOCATIVE-TYPES-20MGL-PAX-3ASECTION-29 "Locative Types"
  [c097]: dref/README.md#x-28ASDF-2FSYSTEM-3ASYSTEM-20MGL-PAX-3ALOCATIVE-29 "ASDF/SYSTEM:SYSTEM MGL-PAX:LOCATIVE"
  [c2d3]: #x-28MGL-PAX-3A-40MARKDOWN-SUPPORT-20MGL-PAX-3ASECTION-29 "Markdown Support"
  [c434]: #x-28MGL-PAX-3A-40BROWSING-WITH-OTHER-BROWSERS-20MGL-PAX-3ASECTION-29 "Browsing with Other Browsers"
  [c4a3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sin_c.htm "COS (MGL-PAX:CLHS FUNCTION)"
  [c4ce]: #x-28MGL-PAX-3A-40EXTENSION-API-20MGL-PAX-3ASECTION-29 "Writing Extensions"
  [c818]: #x-28MGL-PAX-3AOUTPUT-LABEL-20FUNCTION-29 "MGL-PAX:OUTPUT-LABEL FUNCTION"
  [c819]: dref/README.md#x-28MGL-PAX-3ACONSTANT-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:CONSTANT MGL-PAX:LOCATIVE"
  [c930]: #x-28MGL-PAX-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29 "MGL-PAX:EXPORTABLE-LOCATIVE-TYPE-P GENERIC-FUNCTION"
  [cb15]: http://common-lisp.net/project/slime/doc/html/Finding-definitions.html#Finding-definitions "`M-.`"
  [cc04]: dref/README.md#x-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:READER MGL-PAX:LOCATIVE"
  [d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"
  [d1ca]: #x-28MGL-PAX-3A-40DOCUMENT-IMPLEMENTATION-NOTES-20MGL-PAX-3ASECTION-29 "Documentation Generation Implementation Notes"
  [d1dc]: #x-28MGL-PAX-3A-40GLOSSARY-TERMS-20MGL-PAX-3ASECTION-29 "Glossary Terms"
  [d3fc]: https://github.com/smithzvk/pythonic-string-reader "Pythonic String Reader"
  [d451]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRINT (MGL-PAX:CLHS FUNCTION)"
  [d5a2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_car_c.htm "CAR (MGL-PAX:CLHS FUNCTION)"
  [d5a9]: http://www.lispworks.com/documentation/HyperSpec/Body/t_stream.htm "STREAM (MGL-PAX:CLHS CLASS)"
  [d646]: http://www.lispworks.com/documentation/HyperSpec/Body/t_rdtabl.htm "READTABLE (MGL-PAX:CLHS CLASS)"
  [d761]: #x-28-22mgl-pax-2Ffull-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"mgl-pax/full" ASDF/SYSTEM:SYSTEM'
  [d7b0]: #x-28MGL-PAX-3A-40WORD-20MGL-PAX-3AGLOSSARY-TERM-29 "word"
  [d83a]: dref/README.md#x-28SETF-20MGL-PAX-3ALOCATIVE-29 "SETF MGL-PAX:LOCATIVE"
  [d850]: #x-28MGL-PAX-3ASECTION-ENTRIES-20FUNCTION-29 "MGL-PAX:SECTION-ENTRIES FUNCTION"
  [d930]: dref/README.md#x-28DREF-3ADREF-20CLASS-29 "DREF:DREF CLASS"
  [d9ee]: #x-28MGL-PAX-3A-2ADOCUMENT-LINK-CODE-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-LINK-CODE* VARIABLE"
  [da14]: http://www.lispworks.com/documentation/HyperSpec/Body/f_smp_cn.htm "SIMPLE-CONDITION-FORMAT-ARGUMENTS (MGL-PAX:CLHS FUNCTION)"
  [dae6]: http://www.lispworks.com/documentation/HyperSpec/Body/f_string.htm "STRING (MGL-PAX:CLHS FUNCTION)"
  [db68]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pkg_na.htm "PACKAGE-NAME (MGL-PAX:CLHS FUNCTION)"
  [dc0a]: #x-28MGL-PAX-3A-40DOCUMENT-FUNCTION-20MGL-PAX-3ASECTION-29 "The `DOCUMENT` Function"
  [dff6]: #x-28MGL-PAX-3A-40GITHUB-WORKFLOW-20MGL-PAX-3ASECTION-29 "Github Workflow"
  [e216]: #x-28MGL-PAX-3A-2ADOCUMENT-HTML-TOP-BLOCKS-OF-LINKS-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS* VARIABLE"
  [e2e8]: #x-28MGL-PAX-3A-40SUPPRESSED-LINKS-20MGL-PAX-3ASECTION-29 "Suppressed Links"
  [e391]: #x-28MGL-PAX-3ADISLOCATED-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:DISLOCATED MGL-PAX:LOCATIVE"
  [e442]: http://www.lispworks.com/documentation/HyperSpec/Body/03_d.htm '"3.4" (MGL-PAX:CLHS MGL-PAX:SECTION)'
  [e51f]: #x-28MGL-PAX-3AEXPORTABLE-REFERENCE-P-20GENERIC-FUNCTION-29 "MGL-PAX:EXPORTABLE-REFERENCE-P GENERIC-FUNCTION"
  [e527]: #x-28MGL-PAX-3A-2ABROWSE-HTML-STYLE-2A-20VARIABLE-29 "MGL-PAX:*BROWSE-HTML-STYLE* VARIABLE"
  [e548]: dref/README.md#x-28MGL-PAX-3AWRITER-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:WRITER MGL-PAX:LOCATIVE"
  [e5ab]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_3.htm "SYMBOL-PACKAGE (MGL-PAX:CLHS FUNCTION)"
  [e5af]: http://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm "SYMBOL (MGL-PAX:CLHS CLASS)"
  [e6bd]: dref/README.md#x-28DREF-3AARGLIST-20FUNCTION-29 "DREF:ARGLIST FUNCTION"
  [e7ee]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*STANDARD-OUTPUT* (MGL-PAX:CLHS VARIABLE)"
  [ebd3]: #x-28MGL-PAX-3A-2ATRANSCRIBE-SYNTAXES-2A-20VARIABLE-29 "MGL-PAX:*TRANSCRIBE-SYNTAXES* VARIABLE"
  [ed5f]: #x-28MGL-PAX-3ACLHS-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:CLHS MGL-PAX:LOCATIVE"
  [ee51]: #x-28MGL-PAX-3AUPDATE-PAX-WORLD-20FUNCTION-29 "MGL-PAX:UPDATE-PAX-WORLD FUNCTION"
  [f12d]: #x-28MGL-PAX-3A-2ADOCUMENT-MAX-NUMBERING-LEVEL-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-MAX-NUMBERING-LEVEL* VARIABLE"
  [f155]: #x-28-22mgl-pax-2Fnavigate-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"mgl-pax/navigate" ASDF/SYSTEM:SYSTEM'
  [f1ab]: #x-28MGL-PAX-3A-40CODIFICATION-20MGL-PAX-3ASECTION-29 "Codification"
  [f1f0]: #x-28MGL-PAX-3ATRANSCRIBE-20FUNCTION-29 "MGL-PAX:TRANSCRIBE FUNCTION"
  [f25f]: #x-28MGL-PAX-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-UPPERCASE-IS-CODE* VARIABLE"
  [f2f5]: http://www.lispworks.com/documentation/HyperSpec/Body/e_smp_cn.htm "SIMPLE-CONDITION (MGL-PAX:CLHS CONDITION)"
  [f47d]: #x-28MGL-PAX-3A-40TRANSCRIPT-CONISTENCY-CHECKING-20MGL-PAX-3ASECTION-29 "Transcript Consistency Checking"
  [f4fd]: #x-28MGL-PAX-3AREGISTER-DOC-IN-PAX-WORLD-20FUNCTION-29 "MGL-PAX:REGISTER-DOC-IN-PAX-WORLD FUNCTION"
  [f585]: #x-28MGL-PAX-3A-2ADOCUMENT-HYPERSPEC-ROOT-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-HYPERSPEC-ROOT* VARIABLE"
  [f5bd]: #x-28MGL-PAX-3A-40TRANSCRIBING-WITH-EMACS-20MGL-PAX-3ASECTION-29 "Transcribing with Emacs"
  [f74b]: #x-28MGL-PAX-3A-40BACKGROUND-20MGL-PAX-3ASECTION-29 "Background"
  [fe21]: http://www.lispworks.com/documentation/HyperSpec/Body/v_t.htm "T (MGL-PAX:CLHS MGL-PAX:CONSTANT)"
  [fe58]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm "READ (MGL-PAX:CLHS FUNCTION)"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
