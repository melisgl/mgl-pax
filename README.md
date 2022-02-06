<a id="x-28MGL-PAX-3A-40MANUAL-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@MANUAL%20MGL-PAX:SECTION"></a>

# PAX Manual

## Table of Contents

- [1 MGL-PAX ASDF System Details][6fdb]
- [2 MGL-PAX/FULL ASDF System Details][d761]
- [3 Links][ba74]
- [4 Background][f74b]
- [5 Tutorial][8c3e]
- [6 Basics][94c7]
    - [6.1 Locatives and References][fd7c]
    - [6.2 Parsing][378f]
- [7 Locative Types][6121]
    - [7.1 Locatives for Variables][4c48]
    - [7.2 Locatives for Macros][21f5]
    - [7.3 Locatives for Functions][e248]
    - [7.4 Locatives for Types and Declarations][be47]
    - [7.5 Condition System Locatives][06a9]
    - [7.6 Locatives for Packages and Readtables][f9d2]
    - [7.7 Locatives for PAX Constructs][292a]
    - [7.8 External Locatives][4b78]
- [8 Navigating Sources in Emacs][3386]
    - [8.1 MGL-PAX/NAVIGATE ASDF System Details][f155]
- [9 Generating Documentation][2c93]
    - [9.1 MGL-PAX/DOCUMENT ASDF System Details][4bb8]
    - [9.2 Markdown Support][c2d3]
        - [9.2.1 Indentation][718f]
        - [9.2.2 Syntax Highlighting][bc83]
        - [9.2.3 MathJax][a17d]
    - [9.3 Codification][f1ab]
    - [9.4 Linking to Code][1865]
        - [9.4.1 Specified Locative][8996]
        - [9.4.2 Unambiguous Unspecified Locative][5c74]
        - [9.4.3 Ambiguous Unspecified Locative][2645]
        - [9.4.4 Explicit and Autolinking][b3cc]
        - [9.4.5 Preventing Autolinking][8c16]
        - [9.4.6 Suppressed Links][e2e8]
        - [9.4.7 Filtering Ambiguous References][4c5e]
        - [9.4.8 Local References][4c96]
    - [9.5 Linking to the Hyperspec][7cc3]
    - [9.6 Linking to Sections][22c2]
    - [9.7 Miscellaneous Variables][7c82]
    - [9.8 Utilities for Generating Documentation][1b1b]
        - [9.8.1 Github Workflow][dff6]
        - [9.8.2 PAX World][1281]
    - [9.9 Overview of Escaping][2634]
    - [9.10 Document Generation Implementation Notes][d1ca]
- [10 Transcripts][6300]
    - [10.1 MGL-PAX/TRANSCRIBE ASDF System Details][5825]
    - [10.2 Transcribing with Emacs][f5bd]
    - [10.3 Transcript API][9dbc]
    - [10.4 Transcript Consistency Checking][f47d]
        - [10.4.1 Finer-grained Consistency Checks][6e18]
        - [10.4.2 Controlling the Dynamic Environment][6b59]
        - [10.4.3 Utilities for Consistency Checking][8423]
- [11 Extension API][c4ce]
    - [11.1 Locatives and References API][f389]
    - [11.2 Adding New Object Types][bbf2]
    - [11.3 Reference Based Extensions][69f7]
    - [11.4 Sections][8a58]

###### \[in package MGL-PAX with nicknames PAX\]
<a id="x-28-22mgl-pax-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22mgl-pax%22%20ASDF%2FSYSTEM:SYSTEM"></a>

## 1 MGL-PAX ASDF System Details

- Version: 0.0.4
- Description: Exploratory programming tool and documentation
  generator.
- Long Description: The set of dependencies of the [`MGL-PAX`][6fdb] system is
  kept light, and its heavier dependencies are autoloaded via `ASDF`
  when the relavant functionality is accessed. See the
  [`MGL-PAX/NAVIGATE`][f155], [`MGL-PAX/DOCUMENT`][4bb8], [`MGL-PAX/TRANSCRIBE`][5825] and
  [`MGL-PAX/FULL`][d761] systems. To keep deployed code small, client systems
  should declare an `ASDF` dependency on this system, never on the
  others, which are intended for autoloading and interactive use.
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)
- Homepage: [http://melisgl.github.io/mgl-pax](http://melisgl.github.io/mgl-pax)
- Bug tracker: [https://github.com/melisgl/mgl-pax/issues](https://github.com/melisgl/mgl-pax/issues)
- Source control: [GIT](https://github.com/melisgl/mgl-pax.git)

<a id="x-28-22mgl-pax-2Ffull-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22mgl-pax%2Ffull%22%20ASDF%2FSYSTEM:SYSTEM"></a>

## 2 MGL-PAX/FULL ASDF System Details

- Description: [`MGL-PAX`][6fdb] with all features preloaded.
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)


<a id="x-28MGL-PAX-3A-40LINKS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@LINKS%20MGL-PAX:SECTION"></a>

## 3 Links

Here is the [official
repository](https://github.com/melisgl/mgl-pax) and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/mgl-pax-manual.html)
for the latest version.

<a id="x-28MGL-PAX-3A-40BACKGROUND-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@BACKGROUND%20MGL-PAX:SECTION"></a>

## 4 Background

As a user, I frequently run into documentation that's incomplete
and out of date, so I tend to stay in the editor and explore the
code by jumping around with [SLIME][slime]'s [`M-.`][slime-m-.].
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
Literate Programming weenie turned inside out. The original
prototype, which did almost everything I wanted, was this:

```
(defmacro defsection (name docstring)
  `(defun ,name () ,docstring))
```

Armed with this `DEFSECTION`, I soon found myself
organizing code following the flow of user level documentation and
relegated comments to implementational details entirely. However,
some portions of `DEFSECTION` docstrings were just
listings of all the functions, macros and variables related to the
narrative, and this list was effectively repeated in the [`DEFPACKAGE`][42d7]
form complete with little comments that were like section names. A
clear violation of [OAOO][oaoo], one of them had to go, so
`DEFSECTION` got a list of symbols to export.

[oaoo]: http://c2.com/cgi/wiki?OnceAndOnlyOnce 

That was great, but soon I found that the listing of symbols is
ambiguous if, for example, a function, a compiler macro and a class
are named by the same symbol. This did not concern exporting, of
course, but it didn't help readability. Distractingly, on such
symbols, `M-.` was popping up selection dialogs. There were two
birds to kill, and the symbol got accompanied by a type, which was
later generalized into the concept of locatives:

```commonlisp
(defsection @introduction ()
  "A single line for one man ..."
  (foo class)
  (bar function))
```

After a bit of elisp hacking, `M-.` was smart enough to
disambiguate based on the locative found in the vicinity of the
symbol, and everything was good for a while.

Then I realized that sections could refer to other sections if there
were a [`SECTION`][672f] locative. Going down that path, I soon began to feel
the urge to generate pretty documentation as all the necessary
information was manifest in the `DEFSECTION` forms. The design
constraint imposed on documentation generation was that following
the typical style of upcasing symbols in docstrings there should be
no need to explicitly mark up links: if `M-.` works, then the
documentation generator shall also be able find out what's being
referred to.

I settled on [Markdown][markdown] as a reasonably non-intrusive
format, and a few thousand lines later PAX was born.

[markdown]: https://daringfireball.net/projects/markdown/ 


<a id="x-28MGL-PAX-3A-40TUTORIAL-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TUTORIAL%20MGL-PAX:SECTION"></a>

## 5 Tutorial

PAX provides an extremely poor man's Explorable Programming
environment. Narrative primarily lives in so called sections that
mix markdown docstrings with references to functions, variables,
etc, all of which should probably have their own docstrings.

The primary focus is on making code easily explorable by using
[SLIME's `M-.`][slime-m-.] (`slime-edit-definition`). See how to
enable some fanciness in [Navigating Sources in Emacs][3386].
[Generating Documentation][2c93] from sections and all the referenced items
in Markdown or HTML format is also implemented.

With the simplistic tools provided, one may accomplish similar
effects as with Literate Programming, but documentation is generated
from code, not vice versa and there is no support for chunking. Code
is first, code must look pretty, documentation is code.

In typical use, PAX packages have no `:EXPORT`'s defined. Instead the
[`DEFINE-PACKAGE`][63f3] form gets a docstring, which may mention section
names (defined with [`DEFSECTION`][72b4]). When the code is loaded into the
lisp, pressing `M-.` in SLIME on the name of the section will take
you there. Sections can also refer to other sections, packages,
functions, etc and you can keep exploring.

##### Docstrings

PAX's automatically recognizes and [marks up code][f1ab]
with backticks, and [links code][1865] to their
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
[`ABORT`][1102] function was not written with PAX in mind.

- [function] **ABORT** *&OPTIONAL CONDITION*

    Transfer control to a restart named `ABORT`, signalling a
    [`CONTROL-ERROR`][6bc0] if none exists.

[6bc0]: http://www.lispworks.com/documentation/HyperSpec/Body/e_contro.htm "\\CONTROL-ERROR \\CONDITION"

##### A Complete Example

Here is an example of how it all works together:

<a id="x-28MGL-PAX-3AFOO-RANDOM-EXAMPLE-20-28MGL-PAX-3AINCLUDE-20-23P-22-2Fhome-2Fmelisgl-2Fown-2Fmgl-pax-2Fsrc-2Ffoo-random-example-2Elisp-22-20-3AHEADER-NL-20-22-60-60-60common-lisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29"></a>
<a id="MGL-PAX:FOO-RANDOM-EXAMPLE%20%28MGL-PAX:INCLUDE%20%23P%22%2Fhome%2Fmelisgl%2Fown%2Fmgl-pax%2Fsrc%2Ffoo-random-example.lisp%22%20:HEADER-NL%20%22%60%60%60common-lisp%22%20:FOOTER-NL%20%22%60%60%60%22%29"></a>

```common-lisp
(mgl-pax:define-package :foo-random
  (:documentation "This package provides various utilities for random.
  See FOO-RANDOM:@FOO-RANDOM-MANUAL.")
  (:use #:common-lisp #:mgl-pax))

(in-package :foo-random)

(defsection @foo-random-manual (:title "Foo Random manual")
  "Here you describe what's common to all the referenced (and
  exported) functions that follow. They work with *FOO-STATE*, and
  have a :RANDOM-STATE keyword arg. Also explain when to choose
  which."
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

Generating documentation in a very stripped down markdown format
is easy:

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

Fancier markdown or HTML output with [automatic
markup][f25f] and [linking][1865] of uppercase symbol names found in
docstrings, section numbering, table of contents, etc is possible by
calling the [`DOCUMENT`][432c] function.

*One can even generate documentation for different but related
libraries at the same time with the output going to different files
but with cross-page links being automatically added for symbols
mentioned in docstrings. See [Generating Documentation][2c93] for some
convenience functions to cover the most common cases.*

Note how `(VARIABLE *FOO-STATE*)` in the [`DEFSECTION`][72b4] form both
exports `*FOO-STATE*` and includes its documentation in
`@FOO-RANDOM-MANUAL`. The symbols [`VARIABLE`][6c83] and
[`FUNCTION`][ba62] are just two instances of
[locatives][6121], which are used in `DEFSECTION`
to refer to definitions tied to symbols.

The transcript in the code block tagged with `cl-transcript` is
automatically checked for up-to-dateness. See [Transcripts][6300].

<a id="x-28MGL-PAX-3A-40BASICS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@BASICS%20MGL-PAX:SECTION"></a>

## 6 Basics

Now let's examine the most important pieces.

<a id="x-28MGL-PAX-3ADEFSECTION-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:DEFSECTION%20MGL-PAX:MACRO"></a>

- [macro] **DEFSECTION** *NAME (&KEY (PACKAGE '\*PACKAGE\*) (READTABLE '\*READTABLE\*) (EXPORT T) TITLE LINK-TITLE-TO (DISCARD-DOCUMENTATION-P \*DISCARD-DOCUMENTATION-P\*)) &BODY ENTRIES*

    Define a documentation section and maybe export referenced symbols.
    A bit behind the scenes, a global variable with `NAME` is defined and
    is bound to a [`SECTION`][5fac] object. By convention, section names
    start with the character `@`. See [Tutorial][8c3e] for an example.
    
    ##### Entries
    
    `ENTRIES` consists of docstrings and references in any order.
    Docstrings are arbitrary strings in markdown format.
    
    [`REFERENCES`][1cea] are given in the form `(OBJECT LOCATIVE)`. For example,
    `(FOO FUNCTION)` refers to the function `FOO`, `(@BAR SECTION)` says
    that `@BAR` is a subsection of this one. `(BAZ (METHOD () (T T T)))`
    refers to the default method of the three argument generic function
    `BAZ`. `(FOO FUNCTION)` is equivalent to `(FOO (FUNCTION))`. See
    [Locatives and References][fd7c] for more.
    
    The same object may occur in multiple references, typically with
    different locatives, but this is not required.
    
    The references are not looked up (see [`RESOLVE`][cd9e] in the [Extension API][c4ce])
    until documentation is generated, so it is allowed to refer to
    things yet to be defined.
    
    ##### Exporting
    
    If `EXPORT` is true (the default), `NAME` and the objects which are
    [`SYMBOLs`][7f9f] are candidates for exporting. A candidate symbol is exported
    if
    
    - it is [accessible][e077] in `PACKAGE`, and
    
    - there is a reference to it in the section being defined which is
      approved by [`EXPORTABLE-REFERENCE-P`][e51f].
    
    See [`DEFINE-PACKAGE`][63f3] if you use the export feature. The idea with
    confounding documentation and exporting is to force documentation of
    all exported symbols.
    
    ##### Misc
    
    `TITLE` is a non-marked-up string or `NIL`. If non-NIL, it determines
    the text of the heading in the generated output. `LINK-TITLE-TO` is a
    reference given as an `(OBJECT LOCATIVE)` pair or `NIL`, to which the
    heading will link when generating HTML. If not specified, the
    heading will link to its own anchor.
    
    When `DISCARD-DOCUMENTATION-P` (defaults to [`*DISCARD-DOCUMENTATION-P*`][730f])
    is true, `ENTRIES` will not be recorded to save memory.

<a id="x-28MGL-PAX-3A-2ADISCARD-DOCUMENTATION-P-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DISCARD-DOCUMENTATION-P*%20VARIABLE"></a>

- [variable] **\*DISCARD-DOCUMENTATION-P\*** *NIL*

    The default value of [`DEFSECTION`][72b4]'s `DISCARD-DOCUMENTATION-P` argument.
    One may want to set `*DISCARD-DOCUMENTATION-P*` to true before
    building a binary application.

<a id="x-28MGL-PAX-3ADEFINE-PACKAGE-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:DEFINE-PACKAGE%20MGL-PAX:MACRO"></a>

- [macro] **DEFINE-PACKAGE** *PACKAGE &REST OPTIONS*

    This is like [`CL:DEFPACKAGE`][42d7] but silences warnings and errors
    signaled when the redefined package is at variance with the current
    state of the package. Typically this situation occurs when symbols
    are exported by calling [`EXPORT`][bf07] (as is the case with [`DEFSECTION`][72b4]) as
    opposed to adding `:EXPORT` forms to the `DEFPACKAGE` form and the
    package definition is reevaluated. See the section on [package
    variance](http://www.sbcl.org/manual/#Package-Variance) in the SBCL
    manual.
    
    The bottom line is that if you rely on `DEFSECTION` to do the
    exporting, then you'd better use `DEFINE-PACKAGE`.

<a id="x-28MGL-PAX-3A-40LOCATIVES-AND-REFERENCES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@LOCATIVES-AND-REFERENCES%20MGL-PAX:SECTION"></a>

### 6.1 Locatives and References

To [navigate with `M-.`][3386] and to [generate
documentation][2c93] we need to refer to things
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
where `TYPE` - called `DOC-TYPE` - is what tells [`CL:DOCUMENTATION`][68f1]
that we want the docstring of the type named `FOO`. This design
supports disambiguation and working with things that are not
first-class, such as types.

PAX generalizes `DOC-TYPE` to the concept of [locative][4d92]s, which may
also take arguments. An [object][75ce] and a [locative][4d92] together are called
a [reference][80cd], and they identify a single thing. [`REFERENCEs`][1cea] are
actual objects, but often they appear as an `(OBJECT LOCATIVE)`
list (see [`DEFSECTION`][72b4]) or as `"OBJECT LOCATIVE"` in docstrings (see
[Linking to Code][1865] for the various forms possible).

```
(defsection @foos ()
  "We discuss the FOO type and the FOO function."
  (foo type)
  (foo function))
```


<a id="x-28MGL-PAX-3A-40REFERENCE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@REFERENCE%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **reference**

    A [reference][80cd] is an [object][75ce] plus a [locative][4d92], and it identifies a
    definition. For example, the symbol `FOO` as the object and the
    symbol [`FUNCTION`][ba62] as the locative together refer to the
    global definition of the function `FOO`.
    
    [`REFERENCE`][1cea] objects can be represented as an `(OBJECT LOCATIVE)` list
    as in [`DEFSECTION`][72b4] entries, or textually as `"FOO function"` where
    `FOO` is a [name][88cf] or similar (see [Codification][f1ab] and
    [Linking to Code][1865]).

<a id="x-28MGL-PAX-3A-40OBJECT-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@OBJECT%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **object**

    [object][75ce]s are symbols or strings which name [functions][ba62],
    [types][926d], [packages][4dd7], etc. Together
    with [locative][4d92]s, they form [reference][80cd]s.

<a id="x-28MGL-PAX-3A-40LOCATIVE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@LOCATIVE%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **locative**

    [locative][4d92]s specify a *type* of definition such as
    [`FUNCTION`][ba62] or [`VARIABLE`][6c83] and together with
    [object][75ce]s form [reference][80cd]s.
    
    A locative can be a symbol or a list whose [`CAR`][8c99] is a symbol. In
    either case, the symbol is called the [locative type][6121] while the rest of the elements are the
    *locative arguments*. See the [`METHOD`][172e] locative or the [`LOCATIVE`][0b3a]
    locative for examples of locative types with arguments.

<a id="x-28MGL-PAX-3A-40PARSING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PARSING%20MGL-PAX:SECTION"></a>

### 6.2 Parsing

<a id="x-28MGL-PAX-3A-40WORD-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@WORD%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **word**

    A *word* is a string from which we want to extract an [object][75ce]. When
    [Navigating][3386], the word is
    `slime-symbol-at-point`, when [Generating Documentation][2c93], it is a
    non-empty string between whitespace characters in a docstring.

<a id="x-28MGL-PAX-3A-40NAME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@NAME%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **name**

    A *name* is a string that names an [`INTERN`][c35d]ed [`SYMBOL`][7f9f],
    a `PACKAGE`([`0`][4dd7] [`1`][5fb9]), or an [`ASDF:SYSTEM`][c097], that is, a possible [object][75ce]. Names are
    constructed from [word][d7b0]s by possibly trimming leading and trailing
    punctuation symbols and removing certain plural suffixes.
    
    For example, in `"X and Y must be LISTs."`, although the word is
    `"LISTs."`, it gets trimmed to `"LISTs"`, then the plural suffix
    `"s"` is removed to get `"LIST"`. Out of the three candidates for
    names, `"LISTs."`, `"LISTs"`, and `"LIST"`, the ones that name
    interned symbols and such are retained for purposes for
    [Navigating][3386] and
    [Generating Documentation][2c93].
    
    The punctuation characters for left and right trimming are `#<` and
    `,:.>`, respectively. The plural suffixes considered are `s`, `es`,
    `ses`, `zes`, and `ren` (all case insensitive).
    
    Thus `"CHILDREN"` and `"BUSES"` may have the names `"CHILD"` and
    `"BUS"` in them.

<a id="x-28MGL-PAX-3A-40LOCATIVE-TYPES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@LOCATIVE-TYPES%20MGL-PAX:SECTION"></a>

## 7 Locative Types

As we have already briefly seen in [`DEFSECTION`][72b4] and
[Locatives and References][fd7c], locatives allow us to refer to, document
and find the source location of various definitions beyond what
standard Common Lisp offers. See [Extension API][c4ce] for a more detailed
treatment. The following are the locatives types supported out of
the box. As all locative types, they are symbols, and their names
should make it obvious what kind of things they refer to. Unless
otherwise noted, locatives take no arguments.

When there is a corresponding `CL` type, a locative can be resolved to
a unique object as is the case in `(LOCATE 'FOO 'CLASS)` returning
`#<CLASS FOO>`. Even if there is no such `CL` type, the source
location and the docstring of the defining form is recorded (see
[`LOCATE-AND-FIND-SOURCE`][d6a4], [`LOCATE-AND-DOCUMENT`][6611] in the [Extension API][c4ce]),
which makes navigating the sources with `M-.` (see
[Navigating Sources in Emacs][3386]) and [Generating Documentation][2c93] possible.

<a id="x-28MGL-PAX-3A-40VARIABLELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@VARIABLELIKE-LOCATIVES%20MGL-PAX:SECTION"></a>

### 7.1 Locatives for Variables

<a id="x-28VARIABLE-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="VARIABLE%20MGL-PAX:LOCATIVE"></a>

- [locative] **VARIABLE** *&OPTIONAL INITFORM*

    Refers to a global special variable. `INITFORM`, or if not specified,
    the global value of the variable is included in the documentation.
    
    ```
    ;;; A REFERENCE is returned because there is no such type as VARIABLE.
    (locate '*FORMAT* 'variable)
    ==> #<REFERENCE *FORMAT* VARIABLE>
    ```
    
    For the output of `(DOCUMENT (MAKE-REFERENCE '*FORMAT* 'VARIABLE))`,
    see [`*FORMAT*`][3da8]. Note that `*FORMAT*` is unbound. If the variable is
    [`BOUNDP`][bdf2], then its *current* value is included in the documentation.
    See [`*DOCUMENT-LINK-CODE*`][d9ee] for an example output. To override the
    current value, `INITFORM` may be provided. This is particulary
    useful if the value of the variable is something undesirable such as
    `#<MY-CLASS {100171ED93}>`.

<a id="x-28MGL-PAX-3ACONSTANT-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:CONSTANT%20MGL-PAX:LOCATIVE"></a>

- [locative] **CONSTANT** *&OPTIONAL INITFORM*

    Refers to a [`DEFCONSTANT`][d684]. `INITFORM`, or if not specified,
    the value of the constant is included in the documentation. The
    [`CONSTANT`][c819] locative is like the [`VARIABLE`][6c83] locative, but it also checks
    that its object is [`CONSTANTP`][2ce2].

<a id="x-28MGL-PAX-3A-40MACROLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@MACROLIKE-LOCATIVES%20MGL-PAX:SECTION"></a>

### 7.2 Locatives for Macros

<a id="x-28MGL-PAX-3AMACRO-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:MACRO%20MGL-PAX:LOCATIVE"></a>

- [locative] **MACRO**

    Refers to a global macro, typically defined with [`DEFMACRO`][fc18] or a
    [special operator][f4de]. See the [`FUNCTION`][ba62]
    locative for a note on arglists.

<a id="x-28MGL-PAX-3ASYMBOL-MACRO-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:SYMBOL-MACRO%20MGL-PAX:LOCATIVE"></a>

- [locative] **SYMBOL-MACRO**

    Refers to a global symbol macro, defined with [`DEFINE-SYMBOL-MACRO`][72fd].
    Note that since `DEFINE-SYMBOL-MACRO` does not support docstrings,
    PAX defines methods on the [`DOCUMENTATION`][68f1] generic function
    specialized for `DOC-TYPE` `SYMBOL-MACRO`.
    
    ```
    (define-symbol-macro my-mac 42)
    (setf (documentation 'my-mac 'symbol-macro)
          "This is MY-MAC.")
    (documentation 'my-mac 'symbol-macro)
    ```


<a id="x-28COMPILER-MACRO-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="COMPILER-MACRO%20MGL-PAX:LOCATIVE"></a>

- [locative] **COMPILER-MACRO**

    Refers to a compiler macro, typically defined with
    [`DEFINE-COMPILER-MACRO`][a5de]. See the [`FUNCTION`][ba62] locative for a note on
    arglists.

<a id="x-28MGL-PAX-3A-40FUNCTIONLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@FUNCTIONLIKE-LOCATIVES%20MGL-PAX:SECTION"></a>

### 7.3 Locatives for Functions

<a id="x-28FUNCTION-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="FUNCTION%20MGL-PAX:LOCATIVE"></a>

- [locative] **FUNCTION**

    Refers to a global function, typically defined with [`DEFUN`][9717].
    
    Note that the arglist in the generated documentation depends on the
    quality of `SWANK-BACKEND:ARGLIST`. It [may be][d1ca] that default values of
    optional and keyword arguments are missing.

<a id="x-28GENERIC-FUNCTION-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="GENERIC-FUNCTION%20MGL-PAX:LOCATIVE"></a>

- [locative] **GENERIC-FUNCTION**

    Refers to a [`GENERIC-FUNCTION`][8d65], typically defined with
    [`DEFGENERIC`][8c40].

<a id="x-28METHOD-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="METHOD%20MGL-PAX:LOCATIVE"></a>

- [locative] **METHOD** *METHOD-QUALIFIERS METHOD-SPECIALIZERS*

    See [`CL:FIND-METHOD`][8beb] for the description of the arguments
    `METHOD-QUALIFIERS` and `METHOD-SPECIALIZERS`. For example,
    a `(FOO (METHOD () (T (EQL XXX))))` as a [`DEFSECTION`][72b4] entry refers to
    this method:
    
        (defmethod foo (x (y (eql 'xxx)))
          ...)
    
    `METHOD` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28METHOD-COMBINATION-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="METHOD-COMBINATION%20MGL-PAX:LOCATIVE"></a>

- [locative] **METHOD-COMBINATION**

    Refers to a [`METHOD-COMBINATION`][fc7b], defined with
    [`DEFINE-METHOD-COMBINATION`][fe2b].

<a id="x-28MGL-PAX-3AACCESSOR-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:ACCESSOR%20MGL-PAX:LOCATIVE"></a>

- [locative] **ACCESSOR** *CLASS-NAME*

    To refer to an accessor named `FOO-SLOT` of class
    `FOO`:
    
        (foo-slot (accessor foo))


<a id="x-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:READER%20MGL-PAX:LOCATIVE"></a>

- [locative] **READER** *CLASS-NAME*

    To refer to a reader named `FOO-SLOT` of class
    `FOO`:
    
        (foo-slot (reader foo))


<a id="x-28MGL-PAX-3AWRITER-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:WRITER%20MGL-PAX:LOCATIVE"></a>

- [locative] **WRITER** *CLASS-NAME*

    To refer to a writer named `FOO-SLOT` of class
    `FOO`:
    
        (foo-slot (writer foo))


<a id="x-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:STRUCTURE-ACCESSOR%20MGL-PAX:LOCATIVE"></a>

- [locative] **STRUCTURE-ACCESSOR**

    This is a synonym of [`FUNCTION`][ba62] with the difference that
    the often ugly and certainly uninformative lambda list will not be
    printed.

<a id="x-28MGL-PAX-3A-40TYPELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TYPELIKE-LOCATIVES%20MGL-PAX:SECTION"></a>

### 7.4 Locatives for Types and Declarations

<a id="x-28TYPE-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="TYPE%20MGL-PAX:LOCATIVE"></a>

- [locative] **TYPE**

    This locative can refer to any Lisp type. For types defined with
    [`DEFTYPE`][89d0], an attempt is made at printing the arguments of type
    specifiers. When `TYPE` refers to a [`CL:CLASS`][7e58], the class is
    documented as an opaque type: no mention is made of that it is a
    class or its superclasses. Use the [`CLASS`][2060] locative if those things
    are part of the contract.

<a id="x-28CLASS-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="CLASS%20MGL-PAX:LOCATIVE"></a>

- [locative] **CLASS**

    Naturally, `CLASS` is the locative type for [`CLASS`][7e58]es.
    To refer to a class named `FOO`:
    
        (foo class)
    
    In the generated documention, only superclasses denoted by [external
    symbols][e077] are included.

<a id="x-28DECLARATION-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="DECLARATION%20MGL-PAX:LOCATIVE"></a>

- [locative] **DECLARATION**

    Refers to a declaration, used in [`DECLARE`][ce02], [`DECLAIM`][804d] and [`PROCLAIM`][9674].
    For example, `[DEBUG][declaration]` refers to the standard [`DEBUG`][bcd2]
    declaration and links to the hyperspec if
    [`*DOCUMENT-LINK-TO-HYPERSPEC*`][875e] is true.
    
    User code may also define new declarations with CLTL2 functionality,
    but there is no way to provide a docstring.
    
    ```
    (cl-environments:define-declaration my-decl (&rest things)
      (values :declare (cons 'foo things)))
    ```
    
    Also, `M-.` (see [Navigating Sources in Emacs][3386]) on declarations currently
    only works on SBCL.

<a id="x-28MGL-PAX-3A-40CONDITION-SYSTEM-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@CONDITION-SYSTEM-LOCATIVES%20MGL-PAX:SECTION"></a>

### 7.5 Condition System Locatives

<a id="x-28CONDITION-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="CONDITION%20MGL-PAX:LOCATIVE"></a>

- [locative] **CONDITION**

    `CONDITION` is the locative type for [`CONDITION`][dc76]s. To
    refer to a condition named `FOO`:
    
        (foo condition)
    
    In the generated documention, only superclasses denoted by [external
    symbols][e077] are included.

<a id="x-28RESTART-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="RESTART%20MGL-PAX:LOCATIVE"></a>

- [locative] **RESTART**

    A locative to refer to the definition of a restart defined by
    [`DEFINE-RESTART`][2d9d].

<a id="x-28MGL-PAX-3ADEFINE-RESTART-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:DEFINE-RESTART%20MGL-PAX:MACRO"></a>

- [macro] **DEFINE-RESTART** *SYMBOL LAMBDA-LIST &BODY DOCSTRING*

    A definer macro to hang the documentation of a restart on a
    symbol.
    
    ```
    (define-restart my-ignore-error ()
      "Available when MY-ERROR is signalled, MY-IGNORE-ERROR unsafely continues.")
    ```
    
    Then `(MY-IGNORE-ERROR RESTART)` refers to the above definition.
    Note that while there is a [`CL:RESTART`][ad91] type, there is no
    corresponding source location or docstring like for
    [`CONDITION`][dc76]s.

<a id="x-28MGL-PAX-3A-40PACKAGELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PACKAGELIKE-LOCATIVES%20MGL-PAX:SECTION"></a>

### 7.6 Locatives for Packages and Readtables

<a id="x-28ASDF-2FSYSTEM-3ASYSTEM-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="ASDF%2FSYSTEM:SYSTEM%20MGL-PAX:LOCATIVE"></a>

- [locative] **ASDF/SYSTEM:SYSTEM**

    Refers to an asdf system. The generated documentation will include
    meta information extracted from the system definition. This also
    serves as an example of a symbol that's not accessible in the
    current package and consequently is not exported.
    
    `ASDF:SYSTEM` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28PACKAGE-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="PACKAGE%20MGL-PAX:LOCATIVE"></a>

- [locative] **PACKAGE**

    Refers to a [`PACKAGE`][5fb9], defined by [`DEFPACKAGE`][42d7]. `PACKAGE` is not
    [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28READTABLE-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="READTABLE%20MGL-PAX:LOCATIVE"></a>

- [locative] **READTABLE**

    Refers to a named [`READTABLE`][248b] defined with
    `NAMED-READTABLES:DEFREADTABLE`, which associates a global name and a
    docstring with the readtable object. Unfortunately, source location
    information is not available.

<a id="x-28MGL-PAX-3A-40PAX-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PAX-LOCATIVES%20MGL-PAX:SECTION"></a>

### 7.7 Locatives for PAX Constructs

<a id="x-28MGL-PAX-3ASECTION-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:SECTION%20MGL-PAX:LOCATIVE"></a>

- [locative] **SECTION**

    Refers to a section defined by [`DEFSECTION`][72b4].
    
    `SECTION` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:GLOSSARY-TERM%20MGL-PAX:LOCATIVE"></a>

- [locative] **GLOSSARY-TERM**

    Refers to a glossary term defined by [`DEFINE-GLOSSARY-TERM`][8ece].

<a id="x-28MGL-PAX-3ADEFINE-GLOSSARY-TERM-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:DEFINE-GLOSSARY-TERM%20MGL-PAX:MACRO"></a>

- [macro] **DEFINE-GLOSSARY-TERM** *NAME (&KEY TITLE (DISCARD-DOCUMENTATION-P \*DISCARD-DOCUMENTATION-P\*)) DOCSTRING*

    Define a global variable with `NAME` and set it to a glossary term
    object. A glossary term is just a symbol to hang a docstring on. It
    is a bit like a `SECTION`([`0`][5fac] [`1`][672f]) in that, when linked to, its `TITLE` will be
    the link text instead of the name of the symbol. Unlike sections
    though, glossary terms are not rendered with headings, but in the
    more lightweight bullet + locative + name/title style.
    
    When `DISCARD-DOCUMENTATION-P` (defaults to [`*DISCARD-DOCUMENTATION-P*`][730f])
    is true, `DOCSTRING` will not be recorded to save memory.
    
    [`GLOSSARY-TERM`][5119] is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28MGL-PAX-3ALOCATIVE-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:LOCATIVE%20MGL-PAX:LOCATIVE"></a>

- [locative] **LOCATIVE** *LAMBDA-LIST*

    This is the locative for locatives. When `M-.` is pressed on
    `SOME-NAME` in `(SOME-NAME LOCATIVE)`, this is what makes it
    possible to land at the corresponding [`DEFINE-LOCATIVE-TYPE`][660b] form.
    Similarly, `(LOCATIVE LOCATIVE)` leads to this very definition.

<a id="x-28MGL-PAX-3ADISLOCATED-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:DISLOCATED%20MGL-PAX:LOCATIVE"></a>

- [locative] **DISLOCATED**

    Refers to a symbol in a non-specific context. Useful for preventing
    [autolinking][b3cc]. For example, if
    there is a function called `FOO` then
    
        `FOO`
    
    will be linked (if [`*DOCUMENT-LINK-CODE*`][d9ee]) to its definition. However,
    
        [`FOO`][dislocated]
    
    will not be. With a dislocated locative, [`LOCATE`][ee94] always fails with a
    [`LOCATE-ERROR`][6887] condition. Also see [Preventing Autolinking][8c16].

<a id="x-28MGL-PAX-3AARGUMENT-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:ARGUMENT%20MGL-PAX:LOCATIVE"></a>

- [locative] **ARGUMENT**

    An alias for [`DISLOCATED`][e391], so the one can refer to an argument of a
    macro without accidentally linking to a class that has the same name
    as that argument. In the following example, `FORMAT` may
    link to `CL:FORMAT` (if we generated documentation for it):
    
    ```
    "See the FORMAT in DOCUMENT."
    ```
    
    Since `ARGUMENT` is a locative, we can prevent that linking by writing:
    
    ```
    "See the FORMAT argument of DOCUMENT."
    ```


<a id="x-28MGL-PAX-3AINCLUDE-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:INCLUDE%20MGL-PAX:LOCATIVE"></a>

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
    
    In the above example, pressing `M-.` on `PAX.EL` will open the
    `src/pax.el` file and put the cursor on its first character. `M-.`
    on `FOO-EXAMPLE` will go to the source location of the `(asdf:system
    locative)` locative.
    
    When documentation is generated, the entire `src/pax.el` file is
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
    
    `INCLUDE` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28MGL-PAX-3A-40EXTERNAL-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@EXTERNAL-LOCATIVES%20MGL-PAX:SECTION"></a>

### 7.8 External Locatives

<a id="x-28MGL-PAX-3ACLHS-20MGL-PAX-3ALOCATIVE-29"></a>
<a id="MGL-PAX:CLHS%20MGL-PAX:LOCATIVE"></a>

- [locative] **CLHS**

    Refers to sections in the Common Lisp hyperspec. These have no
    source location so `M-.` will not work. What works is linking. The
    following markdown examples all produce a link to `CLHS` [`3.4`][f945], the
    section 'Lambda Lists', which is in file `03_d.htm`.
    
    ```
    CLHS `3.4`
    `3.4` CLHS
    [3.4][]
    [`3.4`][]
    [3.4][CLHS]
    [Lambda Lists][clhs]
    [03_d][clhs]
    ```
    
    The rules of matching sections are the following. If the object of
    the reference is [`STRING=`][91fd] to the section number string (without the
    trailing dot) or to the name of its file without the `.htm`
    extension, then the reference refers to that section. Else, if the
    object is a case-insensitive substring of the title of some section,
    then the reference refers to the first such section in breadth-first
    order.
    
    To link to issue and issue summary pages, all of the above markdown
    examples work, just make the object of the reference the name of the
    issue prefixed by `ISSUE:` or `SUMMARY:` as appropriate. For
    example, to refer to the `AREF-1D` issue use `[ISSUE:AREF-1D][clhs]`
    and get [ISSUE:AREF-1D][6786]. Similary, `[SUMMARY:AREF-1D][clhs]`
    turns into [SUMMARY:AREF-1D][e256]. Alternatively, matching the name
    of the file also works (`[iss009][clhs]` renders as [iss009][e256])
    
    The generated links are relative to [`*DOCUMENT-HYPERSPEC-ROOT*`][f585].
    
    To detach the discussion from markdown syntax, let's see these cases
    through the programmatic interface.
    
    ```
    (locate "3.4" 'clhs)
    ==> #<REFERENCE "3.4" CLHS>
    (locate "03_d" 'clhs)
    ==> #<REFERENCE "03_d" CLHS>
    (locate "lambda" 'clhs)
    ==> #<REFERENCE "3.4" CLHS>
    (locate "ISSUE:AREF-1D" 'clhs)
    ==> #<REFERENCE "ISSUE:AREF-1D" CLHS>
    (locate "SUMMARY:AREF-1D" 'clhs)
    ==> #<REFERENCE "SUMMARY:AREF-1D" CLHS>
    ```


<a id="x-28MGL-PAX-3A-40NAVIGATING-IN-EMACS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@NAVIGATING-IN-EMACS%20MGL-PAX:SECTION"></a>

## 8 Navigating Sources in Emacs

Integration into [SLIME's `M-.`][slime-m-.]
(`slime-edit-definition`) allows one to visit the source location of
the thing that's identified by `slime-symbol-at-point` parsed as a
[word][d7b0] and the locative before or after the symbol in a buffer. With
this extension, if a locative is the previous or the next expression
around the symbol of interest, then `M-.` will go straight to the
definition which corresponds to the locative. If that fails, `M-.`
will try to find the definitions in the normal way, which may
involve popping up an xref buffer and letting the user interactively
select one of possible definitions.

In the following examples, when the cursor is on one of the
characters of `FOO` or just after `FOO`, pressing `M-.` will visit
the definition of function `FOO`:

    function foo
    foo function
    (function foo)
    (foo function)

In particular, references in a [`DEFSECTION`][72b4] form are in ([`SYMBOL`][7f9f]
[`LOCATIVE`][0b3a]) format so `M-.` will work just fine there.

Just like vanilla `M-.`, this works in comments and docstrings. In
the next example, pressing `M-.` on `FOO` will visit `FOO`'s
default method:

```commonlisp
;;;; See FOO `(method () (t t t))` for how this all works.
;;;; But if the locative has semicolons inside: FOO `(method
;;;; () (t t t))`, then it won't, so be wary of line breaks
;;;; in comments.
```

With a prefix argument (`C-u M-.`), one can enter a symbol plus a
locative separated by whitespace to preselect one of the
possibilities.

The `M-.` extensions can be enabled by loading `src/pax.el`.

<a id="x-28-22mgl-pax-2Fnavigate-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22mgl-pax%2Fnavigate%22%20ASDF%2FSYSTEM:SYSTEM"></a>

### 8.1 MGL-PAX/NAVIGATE ASDF System Details

- Description: Slime `M-.` support for [`MGL-PAX`][6fdb].
- Long Description: Autoloaded by Slime's `M-.` when `src/pax.el` is
  loaded. See [Navigating Sources in Emacs][3386].
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)


<a id="x-28MGL-PAX-3A-40GENERATING-DOCUMENTATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@GENERATING-DOCUMENTATION%20MGL-PAX:SECTION"></a>

## 9 Generating Documentation

<a id="x-28MGL-PAX-3ADOCUMENT-20FUNCTION-29"></a>
<a id="MGL-PAX:DOCUMENT%20FUNCTION"></a>

- [function] **DOCUMENT** *OBJECT &KEY STREAM PAGES (FORMAT :MARKDOWN)*

    Write `OBJECT` in `FORMAT` to `STREAM` diverting some output to `PAGES`.
    `FORMAT` can be anything [`3BMD`][3bmd] supports, which is currently
    `:MARKDOWN`, `:HTML` and `:PLAIN`. `STREAM` may be a stream object, `T` or `NIL`
    as with `CL:FORMAT`.
    
    Most often, this function is called on section objects
    like `(DOCUMENT @MANUAL)`, but it supports all kinds of objects for
    which [`DOCUMENT-OBJECT`][bacc] is defined. To look up the documentation of
    function [`DOCUMENT`][432c]:
    
        (document #'document)
    
    To generate the documentation for separate libraries with automatic
    cross-links:
    
        (document (list @cube-manual @mat-manual))
    
    Note that not only first class objects can have documentation. For
    instance, variables and deftypes are not represented by objects.
    That's why [`CL:DOCUMENTATION`][68f1] has a `DOC-TYPE` argument. `DOCUMENT` does
    not have anything like that, instead it relies on [`REFERENCE`][1cea] objects
    to carry the extra information. We are going to see later how
    references and locatives work. Until then, here is an example on how
    to look up the documentation of type `FOO`:
    
        (document (locate 'foo 'type))
    
    One can call [`DESCRIBE`][38a0] on [`SECTION`][5fac] objects to get
    documentation in markdown format with less markup than the default.
    See [`DESCRIBE-OBJECT`][af96] `(METHOD () (SECTION T))`.
    
    There are quite a few special variables that affect how output is
    generated, see [Codification][f1ab], [Linking to Code][1865],
    [Linking to Sections][22c2], and
    [Miscellaneous Variables][7c82].
    
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
    [reachable][8c95] from one of
    its `:OBJECTS`.
    
    `OUTPUT` can be a number things:
    
    - If it's a list whose first element is a string or a pathname, then
      output will be sent to the file denoted by that and the rest of
      the elements of the list are passed on as arguments to [`CL:OPEN`][117a].
      One extra keyword argument is `:ENSURE-DIRECTORIES-EXIST`. If it's
      true, [`ENSURE-DIRECTORIES-EXIST`][e4b0] will be called on the pathname
      before it's opened.
    
    - If it's `NIL`, then output will be collected in a string.
    
    - If it's `T`, then output will be sent to [`*STANDARD-OUTPUT*`][1864].
    
    - If it's a stream, then output will be sent to that stream.
    
    If some pages are specified, `DOCUMENT` returns a list of designators
    for generated output. If a page whose `OUTPUT` refers to a file that
    was created (which doesn't happen if nothing would be written to
    it), then the corresponding pathname is included in the list. For
    strings the string itself, while for streams the stream object is
    included in the list. This way it's possible to write some pages to
    files and some to strings and have the return value indicate what
    was created. The output designators in the returned list are ordered
    by creation time.
    
    If no `PAGES` are specified, `DOCUMENT` returns a single pathname,
    string or stream object according to the value of the `STREAM`
    argument.
    
    Note that even if `PAGES` is specified, `STREAM` acts as a catch all
    taking the generated documentation for references not claimed by any
    pages. Also, the filename, string or stream corresponding to `STREAM`
    is always the first element in list of generated things that is the
    return value.
    
    `HEADER-FN`, if not `NIL`, is a function of a single stream argument,
    which is called just before the first write to the page. Since
    `:FORMAT` `:HTML` only generates HTML fragments, this makes it possible
    to print arbitrary headers, typically setting the title, css
    stylesheet, or charset.
    
    `FOOTER-FN` is similar to `HEADER-FN`, but it's called after the last
    write to the page. For HTML, it typically just closes the body.
    
    `URI-FRAGMENT` is a string such as `"doc/manual.html"` that specifies
    where the page will be deployed on a webserver. It defines how links
    between pages will look. If it's not specified and `OUTPUT` refers
    to a file, then it defaults to the name of the file. If `URI-FRAGMENT`
    is `NIL`, then no links will be made to or from that page.
    
    Finally, `SOURCE-URI-FN` is a function of a single, `REFERENCE`
    argument. If it returns a value other than `NIL`, then it must be a
    string representing an URI. If `FORMAT` is `:HTML` and
    [`*DOCUMENT-MARK-UP-SIGNATURES*`][8fb6] is true, then the locative as
    displayed in the signature will be a link to this uri. See
    [`MAKE-GITHUB-SOURCE-URI-FN`][f960].
    
    `PAGES` may look something like this:
    
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
    ```


<a id="x-28-22mgl-pax-2Fdocument-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22mgl-pax%2Fdocument%22%20ASDF%2FSYSTEM:SYSTEM"></a>

### 9.1 MGL-PAX/DOCUMENT ASDF System Details

- Description: Documentation generation support for [`MGL-PAX`][6fdb].
- Long Description: Autoloaded by [`MGL-PAX:DOCUMENT`][432c]. See
  [Generating Documentation][2c93].
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)


<a id="x-28MGL-PAX-3A-40MARKDOWN-SUPPORT-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@MARKDOWN-SUPPORT%20MGL-PAX:SECTION"></a>

### 9.2 Markdown Support

The [Markdown][markdown] in docstrings is processed with the
[`3BMD`][3bmd] library.

<a id="x-28MGL-PAX-3A-40MARKDOWN-INDENTATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@MARKDOWN-INDENTATION%20MGL-PAX:SECTION"></a>

#### 9.2.1 Indentation

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

See [`DOCUMENT-OBJECT`][0225] for the details.

<a id="x-28MGL-PAX-3A-40MARKDOWN-SYNTAX-HIGHLIGHTING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@MARKDOWN-SYNTAX-HIGHLIGHTING%20MGL-PAX:SECTION"></a>

#### 9.2.2 Syntax Highlighting

For syntax highlighting, github's [fenced code
blocks][fenced-code-blocks] markdown extension to mark up code
blocks with triple backticks is enabled so all you need to do is
write:

    ```elisp
    (defun foo ())
    ```

to get syntactically marked up HTML output. Copy `src/style.css`
from `PAX` and you are set. The language tag, `elisp` in this
example, is optional and defaults to `common-lisp`.

See the documentation of [`3BMD`][3bmd] and [colorize][colorize] for
the details.

[3bmd]: https://github.com/3b/3bmd 

[colorize]: https://github.com/redline6561/colorize/ 

[fenced-code-blocks]: https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks 


<a id="x-28MGL-PAX-3A-40MATHJAX-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@MATHJAX%20MGL-PAX:SECTION"></a>

#### 9.2.3 MathJax

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
Reader][pythonic-string-reader] can help with that.

[pythonic-string-reader]: https://github.com/smithzvk/pythonic-string-reader 


<a id="x-28MGL-PAX-3A-40CODIFICATION-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@CODIFICATION%20MGL-PAX:SECTION"></a>

### 9.3 Codification

<a id="x-28MGL-PAX-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-UPPERCASE-IS-CODE*%20VARIABLE"></a>

- [variable] **\*DOCUMENT-UPPERCASE-IS-CODE\*** *T*

    When true, [codifiable][b89a] and [interesting][7445] [word][d7b0]s are assumed to be
    code as if they were marked up with backticks. For example, this
    docstring
    
        "T PRINT CLASSes SECTION *PACKAGE* MGL-PAX ASDF
        CaMeL Capital"
    
    is equivalent to this:
    
        "`T` `PRINT` `CLASS`es `SECTION` `*PACKAGE*` `MGL-PAX` `ASDF`
        CaMel Capital"
    
    and renders as
    
    `T` [`PRINT`][fdd1] `CLASS`([`0`][7e58] [`1`][2060])es `SECTION`([`0`][5fac] [`1`][672f]) [`MGL-PAX`][6fdb] `ASDF` CaMel Capital
    
    where the links are added due to [`*DOCUMENT-LINK-CODE*`][d9ee].
    
    To suppress this behavior, add a backslash to the beginning of the a
    [codifiable][b89a] word or right after the leading `*` if it would
    otherwise be parsed as markdown emphasis:
    
        "\\SECTION *\\PACKAGE*"
    
    The number of backslashes is doubled above because that's how the
    example looks in a docstring. Note that the backslash is discarded
    even if `*DOCUMENT-UPPERCASE-IS-CODE*` is false.

<a id="x-28MGL-PAX-3A-40CODIFIABLE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@CODIFIABLE%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **codifiable**

    A [word][d7b0] is *codifiable* iff
    
    - it has at least one uppercase character (e.g. not [`<`][5800], [`<=`][bb77] or
      [`///`][889e]), and
    
    - it has no lowercase characters (e.g. `T`, [`*PRINT-LENGTH*`][727b]) or
      all lowercase characters immediately follow at least two
      consecutive uppercase characters (e.g. `CLASSes`([`0`][7e58] [`1`][2060]) but not
      `Capital`).


<a id="x-28MGL-PAX-3A-40INTERESTING-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
<a id="MGL-PAX:@INTERESTING%20MGL-PAX:GLOSSARY-TERM"></a>

- [glossary-term] **interesting**

    A [word][d7b0] is *interesting* iff
    
    - it *names* a known reference, or
    
    - it is at least 3 characters long and names a package or a symbol
      external to its package.
    
    Where we say that a word **names** a known reference if the word
    matches the name of a thing being documented, or it is in the
    hyperspec and [`*DOCUMENT-UPPERCASE-IS-CODE*`][f25f] is true, or more
    precisely,
    
    - if the word matches [the object of a reference][REFERENCE-OBJECT
      reader] being documented (see [`DOCUMENT`][432c] and
      [`COLLECT-REACHABLE-OBJECTS`][8c95]), or
    
    - the a name in the hyperspec if [`*DOCUMENT-LINK-TO-HYPERSPEC*`][875e].
    
    Symbols are read in the current [`*PACKAGE*`][d2c1], which is subject to
    [`*DOCUMENT-NORMALIZE-PACKAGES*`][440e].

<a id="x-28MGL-PAX-3A-2ADOCUMENT-DOWNCASE-UPPERCASE-CODE-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-DOWNCASE-UPPERCASE-CODE*%20VARIABLE"></a>

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
<a id="MGL-PAX:@LINKING-TO-CODE%20MGL-PAX:SECTION"></a>

### 9.4 Linking to Code

In this section, we describe all ways of linking to code
available when [`*DOCUMENT-UPPERCASE-IS-CODE*`][f25f] is true.

*Note that invoking [`M-.`][3386] on the
[object][75ce] of any of the following links will disambiguate based the
textual context, determining the locative. In a nutshell, if `M-.`
works without popping up a list of choices, then the documentation
will contain a single link.*

<a id="x-28MGL-PAX-3A-2ADOCUMENT-LINK-CODE-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-LINK-CODE*%20VARIABLE"></a>

- [variable] **\*DOCUMENT-LINK-CODE\*** *T*

    Enable the various forms of links in docstrings described in
    [Linking to Code][1865]. See the following sections for a description of
    how to use linking.

<a id="x-28MGL-PAX-3A-40SPECIFIED-LOCATIVE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@SPECIFIED-LOCATIVE%20MGL-PAX:SECTION"></a>

#### 9.4.1 Specified Locative

The following examples all render as [`DOCUMENT`][432c].

- `[DOCUMENT][function]` (*object + locative, explicit link*)

- `DOCUMENT function` (*object + locative, autolink*)

- `function DOCUMENT` (*locative + object, autolink*)

The Markdown link definition (i.e. `function` between the second
set of brackets above) needs no backticks to mark it as code.

Here and below, the [object][75ce] (`DOCUMENT`) is uppercased, and we rely
on [`*DOCUMENT-UPPERCASE-IS-CODE*`][f25f] being true. Alternatively, the
[object][75ce] could be explicitly marked up as code with a pair of
backticks, and then its character case would likely not
matter (subject to [`READTABLE-CASE`][a328]).

The link text in the above examples is `DOCUMENT`. To override it,
this form may be used:

- `[see this][document function]` (*title + object + locative,
  explicit link*) renders as: [see this][432c].


<a id="x-28MGL-PAX-3A-40UNAMBIGUOUS-LOCATIVE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@UNAMBIGUOUS-LOCATIVE%20MGL-PAX:SECTION"></a>

#### 9.4.2 Unambiguous Unspecified Locative

In the following examples, although no locative is specified,
`DOCUMENT` names a single [object][75ce] being documented, so they all
render as [`DOCUMENT`][432c].

- `[DOCUMENT][]` (*object, explicit link*),

- `DOCUMENT` (*object, autolink*).

To override the title:

- `[see this][document]` (*title + object, explicit link*) renders
  as: [see this][432c].


<a id="x-28MGL-PAX-3A-40AMBIGUOUS-LOCATIVE-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@AMBIGUOUS-LOCATIVE%20MGL-PAX:SECTION"></a>

#### 9.4.3 Ambiguous Unspecified Locative

These examples all render as `SECTION`([`0`][5fac] [`1`][672f]), linking to both
definitions of the [object][75ce] `SECTION`, the `CLASS` and the
`LOCATIVE`.

- `[SECTION][]` (*object, explicit link*)

- `SECTION` (*object, autolink*)

To override the title:

- `[see this][section]` (*title + object, explicit link*) renders
  as: see this([`0`][5fac] [`1`][672f]).


<a id="x-28MGL-PAX-3A-40EXPLICIT-AND-AUTOLINKING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@EXPLICIT-AND-AUTOLINKING%20MGL-PAX:SECTION"></a>

#### 9.4.4 Explicit and Autolinking

The examples in the previous sections are marked with *explicit
link* or *autolink*. Explicit links are those with a Markdown
reference link spelled out explicitly, while autolinks are those
without.

<a id="x-28MGL-PAX-3A-40PREVENTING-AUTOLINKING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PREVENTING-AUTOLINKING%20MGL-PAX:SECTION"></a>

#### 9.4.5 Preventing Autolinking

In the common case, when [`*DOCUMENT-UPPERCASE-IS-CODE*`][f25f] is true,
prefixing the uppercase [word][d7b0] with a backslash prevents it from
being codified and thus also prevents [autolinking][b3cc] form kicking in. For example,

    \DOCUMENT

renders as DOCUMENT. If it should be marked up as code but not
autolinked, the backslash must be within backticks like this:

    `\DOCUMENT`

This renders as `DOCUMENT`. Alternatively, the [`DISLOCATED`][e391] or the
[`ARGUMENT`][8710] locative may be used as in `[DOCUMENT][dislocated]`.

<a id="x-28MGL-PAX-3A-40SUPPRESSED-LINKS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@SUPPRESSED-LINKS%20MGL-PAX:SECTION"></a>

#### 9.4.6 Suppressed Links

Within the same docstring, [autolinking][b3cc] of code (i.e. of something like
`FOO`) is suppressed if the same [object][75ce] was already linked to in
any way. In the following docstring, only the first `FOO` will be
turned into a link.

    "`FOO` is safe. `FOO` is great."

However if a [locative][4d92] was specified or found near the [object][75ce], then
a link is always made. In the following, in both docstrings, both
occurrences `FOO` produce links.

    "`FOO` is safe. [`FOO`][macro] is great."
    "`FOO` is safe. Macro `FOO` is great."

As an exception, links with [specified][8996]
and [unambiguous][5c74] locatives to
`SECTIONs`([`0`][5fac] [`1`][672f]) and [`GLOSSARY-TERM`][5119]s always produce a link to allow their
titles to be displayed properly.

Finally, [autolinking][b3cc] to `T` or
`NIL` is suppressed (see [`*DOCUMENT-LINK-TO-HYPERSPEC*`][875e]).

<a id="x-28MGL-PAX-3A-40FILTERING-AMBIGUOUS-REFERENCES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@FILTERING-AMBIGUOUS-REFERENCES%20MGL-PAX:SECTION"></a>

#### 9.4.7 Filtering Ambiguous References

When there are multiple references to link to - as seen in
[Ambiguous Unspecified Locative][2645] - some references are removed by the following
rules.

- References to [`ASDF:SYSTEM`][c097]s are removed if there are other
  references which are not to `ASDF:SYSTEM`s. This is because system
  names often collide with the name of a class or function and are
  rarely useful to link to. Use explicit links to `ASDF:SYSTEM`s, if
  necessary.

- References to the [`CLHS`][ed5f] are filtered similarly.

- If references include a [`GENERIC-FUNCTION`][5875] locative, then all
  references with [`LOCATIVE-TYPE`][3200] [`METHOD`][172e],
  [`ACCESSOR`][00d4], [`READER`][cc04] and [`WRITER`][e548]
  are removed to avoid linking to a possibly large number of
  methods.


<a id="x-28MGL-PAX-3A-40LOCAL-REFERENCES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@LOCAL-REFERENCES%20MGL-PAX:SECTION"></a>

#### 9.4.8 Local References

To unclutter the generated output by reducing the number of
links, the so-called 'local' references (e.g. references to the very
definition for which documentation is being generated) are treated
specially. In the following example, there are local references to
the function `FOO` and its arguments, so none of them get turned into
links:

```common-lisp
(defun foo (arg1 arg2)
  "FOO takes two arguments: ARG1 and ARG2."
  t)
```

If linking was desired one could use a [Specified Locative][8996] (e.g.
`[FOO][function]` or `FOO function`), which results in a single
link. An explicit link with an unspecified locative like `[FOO][]`
generates links to all references involving the `FOO` symbol except
the local ones.

The exact rules for local references are as follows:

- Unless a locative is [specified][8996], no
  [autolinking][b3cc] is performed for
  [object][75ce]s for which there are local references. For example, `FOO`
  does not get any links if there is *any* local reference with the
  same [object][75ce].

- With a locative specified (e.g. in the explicit link
  `[FOO][function]` or in the text `the FOO function`), a single
  link is made irrespective of any local references.

- Explicit links with an unspecified locative (e.g. `[FOO][]`) are
  linked to all non-local references.


<a id="x-28MGL-PAX-3A-40LINKING-TO-THE-HYPERSPEC-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@LINKING-TO-THE-HYPERSPEC%20MGL-PAX:SECTION"></a>

### 9.5 Linking to the Hyperspec

<a id="x-28MGL-PAX-3A-2ADOCUMENT-LINK-TO-HYPERSPEC-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-LINK-TO-HYPERSPEC*%20VARIABLE"></a>

- [variable] **\*DOCUMENT-LINK-TO-HYPERSPEC\*** *T*

    If true, link symbols found in code to the Common Lisp Hyperspec.
    
    Locatives work as expected (see [`*DOCUMENT-LINK-CODE*`][d9ee]).
    `FIND-IF` links to `FIND-IF`, `FUNCTION` links
    to `FUNCTION` and `[FUNCTION][type]` links to [`FUNCTION`][3de5].
    
    [Autolinking][b3cc] to `T` and `NIL` is
    suppressed. If desired, use `[T][]` (that links to `T`([`0`][08f7] [`1`][26cf])) or
    `[T][constant]` (that links to [`T`][08f7]).
    
    Note that linking to sections in the Hyperspec is done with the [`CLHS`][ed5f]
    locative and is not subject to the value of this variable.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HYPERSPEC-ROOT-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-HYPERSPEC-ROOT*%20VARIABLE"></a>

- [variable] **\*DOCUMENT-HYPERSPEC-ROOT\*** *"http://www.lispworks.com/documentation/HyperSpec/"*

    A URL pointing to an installed Common Lisp Hyperspec. The default
    value of is the canonical location.

<a id="x-28MGL-PAX-3A-40LINKING-TO-SECTIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@LINKING-TO-SECTIONS%20MGL-PAX:SECTION"></a>

### 9.6 Linking to Sections

The following variables control how to generate section numbering,
table of contents and navigation links.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-LINK-SECTIONS-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-LINK-SECTIONS*%20VARIABLE"></a>

- [variable] **\*DOCUMENT-LINK-SECTIONS\*** *T*

    When true, HTML anchors are generated before the heading of
    sections, which allows the table of contents to contain links and
    also code-like references to sections (like `@FOO-MANUAL`) to be
    translated to links with the section title being the name of the
    link.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-MAX-NUMBERING-LEVEL-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-MAX-NUMBERING-LEVEL*%20VARIABLE"></a>

- [variable] **\*DOCUMENT-MAX-NUMBERING-LEVEL\*** *3*

    A non-negative integer. In their hierarchy, sections on levels less
    than this value get numbered in the format of `3.1.2`. Setting it to
    0 turns numbering off.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL*%20VARIABLE"></a>

- [variable] **\*DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL\*** *3*

    A non-negative integer. Top-level sections are given a table of
    contents, which includes a nested tree of section titles whose depth
    is limited by this value. Setting it to 0 turns generation of the
    table of contents off. If [`*DOCUMENT-LINK-SECTIONS*`][1b28] is true, then the
    table of contents will link to the sections.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-TEXT-NAVIGATION-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-TEXT-NAVIGATION*%20VARIABLE"></a>

- [variable] **\*DOCUMENT-TEXT-NAVIGATION\*** *NIL*

    If true, then before each heading a line is printed with links to
    the previous, parent and next section. Needs
    [`*DOCUMENT-LINK-SECTIONS*`][1b28] to be on to work.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-FANCY-HTML-NAVIGATION-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-FANCY-HTML-NAVIGATION*%20VARIABLE"></a>

- [variable] **\*DOCUMENT-FANCY-HTML-NAVIGATION\*** *T*

    If true and the output format is HTML, then headings get a
    navigation component that consists of links to the previous, parent,
    next section and a permalink. This component is normally hidden, it
    is visible only when the mouse is over the heading. Needs
    [`*DOCUMENT-LINK-SECTIONS*`][1b28] to be on to work.

<a id="x-28MGL-PAX-3A-40MISCELLANEOUS-DOCUMENTATION-PRINTER-VARIABLES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@MISCELLANEOUS-DOCUMENTATION-PRINTER-VARIABLES%20MGL-PAX:SECTION"></a>

### 9.7 Miscellaneous Variables

<a id="x-28MGL-PAX-3A-2ADOCUMENT-URL-VERSIONS-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-URL-VERSIONS*%20VARIABLE"></a>

- [variable] **\*DOCUMENT-URL-VERSIONS\*** *(1)*

    A list of versions of PAX URL formats to support in the
    generated documenation in addition to the latest one.
    
    PAX emits HTML anchors before the documentation of `SECTIONs`([`0`][5fac] [`1`][672f])
    (see [Linking to Sections][22c2]) and other things (see [Linking to Code][1865]).
    For the function `FOO`, in the current version (version 2), the
    anchor is `<a id="MGL-PAX:FOO%20FUNCTION">` and its URL will end
    with `#MGL-PAX:FOO%20FUNCTION`.
    
    *Note that to make the URL independent of whether a symbol is
    [internal or external][e077] to their [`SYMBOL-PACKAGE`][964b], single
    colon is printed where a double colon would be expected. Package and
    symbol names are both printed verbatim except for escaping colons
    and spaces with a backslash. For exported symbols with no funny
    characters, this coincides with how [`PRIN1`][1aee] would print the symbol,
    while having the benefit of making the URL independent of the Lisp
    printer's escaping strategy and producing human-readable output for
    mixed-case symbols. No such promises are made for non-ASCII
    characters, and their URLs may change in future versions. Locatives
    are printed with `PRIN1`.*
    
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


<a id="x-28MGL-PAX-3A-2ADOCUMENT-MIN-LINK-HASH-LENGTH-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-MIN-LINK-HASH-LENGTH*%20VARIABLE"></a>

- [variable] **\*DOCUMENT-MIN-LINK-HASH-LENGTH\*** *4*

    Recall that markdown reference style links (like `[label][id]`) are
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
    it can make markdown output more readable.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-MARK-UP-SIGNATURES-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-MARK-UP-SIGNATURES*%20VARIABLE"></a>

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

<a id="x-28MGL-PAX-3A-2ADOCUMENT-NORMALIZE-PACKAGES-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-NORMALIZE-PACKAGES*%20VARIABLE"></a>

- [variable] **\*DOCUMENT-NORMALIZE-PACKAGES\*** *T*

    Determines what [`*PACKAGE*`][d2c1] and [`*READTABLE*`][a916] are when working with
    generating documentation. If true and documentation is generated for
    a `SECTION`([`0`][5fac] [`1`][672f]) (including its [`SECTION-ENTRIES`][9450]), then [`SECTION-PACKAGE`][a5b1] and
    [`SECTION-READTABLE`][88a7] of the innermost containing section is used. To
    eliminate ambiguity `[in package ...]` messages are printed right
    after the section heading if necessary. If false, then `*PACKAGE*` and
    `*READTABLE*` are left at the current values.

<a id="x-28MGL-PAX-3A-40DOCUMENTATION-UTILITIES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@DOCUMENTATION-UTILITIES%20MGL-PAX:SECTION"></a>

### 9.8 Utilities for Generating Documentation

Two convenience functions are provided to serve the common case of
having an `ASDF` system with some readmes and a directory with for the
HTML documentation and the default css stylesheet.

<a id="x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-READMES-20FUNCTION-29"></a>
<a id="MGL-PAX:UPDATE-ASDF-SYSTEM-READMES%20FUNCTION"></a>

- [function] **UPDATE-ASDF-SYSTEM-READMES** *SECTIONS ASDF-SYSTEM*

    Convenience function to generate two readme files in the directory
    holding the `ASDF-SYSTEM` definition.
    
    `README.md` has anchors, links, inline code, and other markup added.
    Not necessarily the easiest on the eye in an editor, but looks good
    on github.
    
    `README` is optimized for reading in text format. Has no links and
    less cluttery markup.
    
    Example usage:
    
    ```
    (update-asdf-system-readmes @manual :mgl-pax)
    ```


<a id="x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-HTML-DOCS-20FUNCTION-29"></a>
<a id="MGL-PAX:UPDATE-ASDF-SYSTEM-HTML-DOCS%20FUNCTION"></a>

- [function] **UPDATE-ASDF-SYSTEM-HTML-DOCS** *SECTIONS ASDF-SYSTEM &KEY PAGES (TARGET-DIR (ASDF/SYSTEM:SYSTEM-RELATIVE-PATHNAME ASDF-SYSTEM "doc/")) (UPDATE-CSS-P T)*

    Generate pretty HTML documentation for a single `ASDF` system,
    possibly linking to github. If `UPDATE-CSS-P`, copy the CSS style
    sheet to `TARGET-DIR`, as well. Example usage:
    
    ```commonlisp
    (update-asdf-system-html-docs @manual :mgl-pax)
    ```
    
    The same, linking to the sources on github:
    
    ```commonlisp
    (update-asdf-system-html-docs
      @manual :mgl-pax
      :pages
      `((:objects
        (,mgl-pax::@manual)
        :source-uri-fn ,(make-github-source-uri-fn
                         :mgl-pax
                         "https://github.com/melisgl/mgl-pax"))))
    ```


<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL*%20VARIABLE"></a>

- [variable] **\*DOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL\*** *NIL*

    `NIL` or a non-negative integer. If non-NIL, it overrides
    [`*DOCUMENT-MAX-NUMBERING-LEVEL*`][f12d] in dynamic HTML table of contents on
    the left of the page.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-TOP-BLOCKS-OF-LINKS-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*%20VARIABLE"></a>

- [variable] **\*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS\*** *NIL*

    A list of blocks of links to be display on the sidebar on the left,
    above the table of contents. A block is of the form `(&KEY TITLE ID
    LINKS)`, where `TITLE` will be displayed at the top of the block in a
    HTML `DIV` with `ID`, followed by the links. `LINKS` is a list
    of `(URI LABEL) elements.`

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS*%20VARIABLE"></a>

- [variable] **\*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS\*** *NIL*

    Like [`*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*`][e216], only it is displayed
    below the table of contents.

<a id="x-28MGL-PAX-3A-40GITHUB-WORKFLOW-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@GITHUB-WORKFLOW%20MGL-PAX:SECTION"></a>

#### 9.8.1 Github Workflow

It is generally recommended to commit generated readmes (see
[`UPDATE-ASDF-SYSTEM-READMES`][13a9]) so that users have something to read
without reading the code and sites like github can display them.

HTML documentation can also be committed, but there is an issue with
that: when linking to the sources (see [`MAKE-GITHUB-SOURCE-URI-FN`][f960]),
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
idea to add section like the [Links][ba74] section to allow jumping between
the repository and the gh-pages site.

<a id="x-28MGL-PAX-3AMAKE-GITHUB-SOURCE-URI-FN-20FUNCTION-29"></a>
<a id="MGL-PAX:MAKE-GITHUB-SOURCE-URI-FN%20FUNCTION"></a>

- [function] **MAKE-GITHUB-SOURCE-URI-FN** *ASDF-SYSTEM GITHUB-URI &KEY GIT-VERSION*

    Return a function suitable as `:SOURCE-URI-FN` of a page spec (see
    the `PAGES` argument of [`DOCUMENT`][432c]). The function looks the source
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

<a id="x-28MGL-PAX-3A-40PAX-WORLD-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@PAX-WORLD%20MGL-PAX:SECTION"></a>

#### 9.8.2 PAX World

`PAX` World is a registry of documents, which can generate
cross-linked HTML documentation pages for all the registered
documents.

<a id="x-28MGL-PAX-3AREGISTER-DOC-IN-PAX-WORLD-20FUNCTION-29"></a>
<a id="MGL-PAX:REGISTER-DOC-IN-PAX-WORLD%20FUNCTION"></a>

- [function] **REGISTER-DOC-IN-PAX-WORLD** *NAME SECTIONS PAGE-SPECS*

    Register `SECTIONS` and `PAGE-SPECS` under `NAME` in `PAX` World. By
    default, [`UPDATE-PAX-WORLD`][ee51] generates documentation for all of these.

For example, this is how `PAX` registers itself:

<a id="x-28MGL-PAX-3AREGISTER-DOC-EXAMPLE-20-28MGL-PAX-3AINCLUDE-20-28-3ASTART-20-28MGL-PAX-3A-3APAX-SECTIONS-20FUNCTION-29-20-3AEND-20-28MGL-PAX-3A-3AEND-OF-REGISTER-DOC-EXAMPLE-20VARIABLE-29-29-20-3AHEADER-NL-20-22-60-60-60commonlisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29"></a>
<a id="MGL-PAX:REGISTER-DOC-EXAMPLE%20%28MGL-PAX:INCLUDE%20%28:START%20%28MGL-PAX::PAX-SECTIONS%20FUNCTION%29%20:END%20%28MGL-PAX::END-OF-REGISTER-DOC-EXAMPLE%20VARIABLE%29%29%20:HEADER-NL%20%22%60%60%60commonlisp%22%20:FOOTER-NL%20%22%60%60%60%22%29"></a>

```commonlisp
(defun pax-sections ()
  (list @manual))
(defun pax-pages ()
  `((:objects
     (, @manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :mgl-pax
                      "https://github.com/melisgl/mgl-pax"))))
(register-doc-in-pax-world :mgl-pax (pax-sections) (pax-pages))
```

<a id="x-28MGL-PAX-3AUPDATE-PAX-WORLD-20FUNCTION-29"></a>
<a id="MGL-PAX:UPDATE-PAX-WORLD%20FUNCTION"></a>

- [function] **UPDATE-PAX-WORLD** *&KEY (DOCS \*REGISTERED-PAX-WORLD-DOCS\*) DIR*

    Generate HTML documentation for all `DOCS`. By default, files are
    created in [`*PAX-WORLD-DIR*`][dfe3] or `(asdf:system-relative-pathname
    :mgl-pax "world/")`, if `NIL`. `DOCS` is a list of entries of the
    form (`NAME` `SECTIONS`([`0`][5fac] [`1`][672f]) `PAGE-SPECS`). The default for `DOCS` is all the
    sections and pages registered with [`REGISTER-DOC-IN-PAX-WORLD`][f4fd].
    
    In the absence of `:HEADER-FN` `:FOOTER-FN`, `:OUTPUT`, every spec in
    `PAGE-SPECS` is augmented with HTML headers, footers and output
    location specifications (based on the name of the section).
    
    If necessary a default page spec is created for every section.

<a id="x-28MGL-PAX-3A-2APAX-WORLD-DIR-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*PAX-WORLD-DIR*%20VARIABLE"></a>

- [variable] **\*PAX-WORLD-DIR\*** *NIL*

    The default location to which to write the generated documentation.
    If `NIL` it defaults to:
    
    ```commonlisp
    (asdf:system-relative-pathname :mgl-pax "world/")
    ```


<a id="x-28MGL-PAX-3A-40OVERVIEW-OF-ESCAPING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@OVERVIEW-OF-ESCAPING%20MGL-PAX:SECTION"></a>

### 9.9 Overview of Escaping

Let's recap how escaping [Codification][f1ab],
[downcasing][a5ee], and
[Linking to Code][1865] works.

- One backslash in front of a [word][d7b0] turns codification off. Use this
  to prevent codification words such as PAX, which is all uppercase
  hence [codifiable][b89a] and it names a package hence it is [interesting][7445].

- One backslash right after an opening backtick turns autolinking
  off.

- Two backslashes right after an opening backtick turns autolinking
  and downcasing off. Use this for things that are not Lisp code but
  which need to be in a monospace font.


In the following examples capital C/D/A letters mark the presence,
and a/b/c the absence of codification, downcasing, and autolinking
assuming all these features are enabled by
[`*DOCUMENT-UPPERCASE-IS-CODE*`][f25f]. [`*DOCUMENT-DOWNCASE-UPPERCASE-CODE*`][a5ee],
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

<a id="x-28MGL-PAX-3A-40DOCUMENT-IMPLEMENTATION-NOTES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@DOCUMENT-IMPLEMENTATION-NOTES%20MGL-PAX:SECTION"></a>

### 9.10 Document Generation Implementation Notes

Documentation Generation is supported on ABCL, AllegroCL, CLISP,
CCL, CMUCL, ECL and SBCL, but their outputs may differ due to the
lack of some introspective capability. SBCL generates complete
output. Compared to that, the following are not supported:

- [`COMPILER-MACRO`][41fd] docstrings on ABCL, AllegroCL, CCL, ECL,

- [`DEFTYPE`][89d0] lambda lists on ABCL, AllegroCL, CLISP, CCL, CMUCL, ECL,

- default values in [`MACRO`][f3cc] lambda lists on AllegroCL,

- default values in function lambda lists on CCL (needs `(DEBUG 3)`
  on AllegroCL),

- `METHOD-COMBINATION`([`0`][fc7b] [`1`][82e0]) docstrings on ABCL, AllegroCL.


<a id="x-28MGL-PAX-3A-40TRANSCRIPTS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TRANSCRIPTS%20MGL-PAX:SECTION"></a>

## 10 Transcripts

What are transcripts for? When writing a tutorial, one often wants
to include a REPL session with maybe a few defuns and a couple of
forms whose output or return values are shown. Also, in a function's
docstring an example call with concrete arguments and return values
speaks volumes. A transcript is a text that looks like a repl
session, but which has a light markup for printed output and return
values, while no markup (i.e. prompt) for Lisp forms. `PAX`
transcripts may include output and return values of all forms, or
only selected ones. In either case, the transcript itself can be
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
`PYTHONIC-STRING-READER`'s triple-quoted strings, that allow one to
work with nested strings with less noise. The triple-quote syntax
can be enabled with:

    (in-readtable pythonic-string-syntax)


<a id="x-28-22mgl-pax-2Ftranscribe-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22mgl-pax%2Ftranscribe%22%20ASDF%2FSYSTEM:SYSTEM"></a>

### 10.1 MGL-PAX/TRANSCRIBE ASDF System Details

- Description: Transcription support for [`MGL-PAX`][6fdb].
- Long Description: Autoloaded by [`MGL-PAX:TRANSCRIBE`][f1f0] and by the Emacs
  integration (see [Transcripts][6300]).
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)


<a id="x-28MGL-PAX-3A-40TRANSCRIBING-WITH-EMACS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TRANSCRIBING-WITH-EMACS%20MGL-PAX:SECTION"></a>

### 10.2 Transcribing with Emacs

Typical transcript usage from within Emacs is simple: add a lisp
form to a docstring or comment at any indentation level. Move the
cursor right after the end of the form as if you were to evaluate it
with `C-x C-e`. The cursor is marked by `#\^`:

    This is part of a docstring.
    
    ```cl-transcript
    (values (princ :hello) (list 1 2))^
    ```

Note that the use of fenced code blocks with the language tag
`cl-transcript` is only to tell PAX to perform consistency checks
at documentation generation time.

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
[`TRANSCRIPTION-CONSISTENCY-ERROR`][a249] because the printed output and the
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
index of the syntax to be used in [`MGL-PAX`][6fdb]:[`*TRANSCRIBE-SYNTAXES*`][ebd3].
Without a prefix argument `mgl-pax-retranscribe-region` will not
change the markup style.

Finally, not only do both functions work at any indentation level,
but in comments too:

    ;;;; (values (princ :hello) (list 1 2))
    ;;;; .. HELLO
    ;;;; => :HELLO
    ;;;; => (1 2)

Transcription support in emacs can be enabled by loading
`src/transcribe.el`.

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-API-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TRANSCRIPT-API%20MGL-PAX:SECTION"></a>

### 10.3 Transcript API

<a id="x-28MGL-PAX-3ATRANSCRIBE-20FUNCTION-29"></a>
<a id="MGL-PAX:TRANSCRIBE%20FUNCTION"></a>

- [function] **TRANSCRIBE** *INPUT OUTPUT &KEY UPDATE-ONLY (INCLUDE-NO-OUTPUT UPDATE-ONLY) (INCLUDE-NO-VALUE UPDATE-ONLY) (ECHO T) (CHECK-CONSISTENCY \*TRANSCRIBE-CHECK-CONSISTENCY\*) DEFAULT-SYNTAX (INPUT-SYNTAXES \*TRANSCRIBE-SYNTAXES\*) (OUTPUT-SYNTAXES \*TRANSCRIBE-SYNTAXES\*)*

    Read forms from `INPUT` and write them (iff `ECHO`) to `OUTPUT`
    followed by any output and return values produced by calling [`EVAL`][c1eb] on
    the form. `INPUT` can be a stream or a string, while `OUTPUT` can be a
    stream or `NIL` in which case transcription goes into a string. The
    return value is the `OUTPUT` stream or the string that was
    constructed.
    
    A simple example is this:
    
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
    
    `TRANSCRIBE` is able to parse its own output. If we transcribe the
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
    
    With `UPDATE-ONLY`, the printed output of a form is only transcribed
    if there were output markers in the source. Similarly, with
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
    
    If `CHECK-CONSISTENCY` is true, then `TRANSCRIBE` signals a continuable
    [`TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR`][8492] whenever a form's output as a
    string is different from what was in `INPUT`, provided that `INPUT`
    contained the output. Similary, for values, a continuable
    [`TRANSCRIPTION-VALUES-CONSISTENCY-ERROR`][238c] is signalled if a value read
    from the source does not print as the as the value returned by `EVAL`.
    This allows readable values to be hand-indented without failing
    consistency checks:
    
    ```commonlisp
    (list 1 2)
    => ;; This is commented, too.
       (1
          ;; Funny indent.
          2)
    ```
    
    See [Transcript Consistency Checking][f47d] for the full picture.
    
    **Unreadable Values**
    
    The above scheme involves [`READ`][3d3c], so consistency of unreadable values
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
    
    where `"==>"` is the `:UNREADABLE` prefix and `"-->"` is the
    `:UNREADABLE-CONTINUATION` prefix. As with outputs, a consistency
    check between an unreadable value from the source and the value from
    `EVAL` is performed with [`STRING=`][91fd] by default. That is, the value from
    `EVAL` is printed to a string and compared to the source value. Hence,
    any change to unreadable values will break consistency checks. This
    is most troublesome with instances of classes with the default
    [`PRINT-OBJECT`][eafc] method printing the memory address. See @ no remedy for
    that, except for customizing `PRINT-OBJECT` or not transcribing that
    kind of stuff.
    
    **Errors**
    
    If an [`ERROR`][1895] condition is signalled, the error is printed to the
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
    ERROR) (PRINC-TO-STRING ERROR))`. [`SIMPLE-CONDITION`][6c1f]s are formatted to
    strings with [`SIMPLE-CONDITION-FORMAT-CONTROL`][493e] and
    [`SIMPLE-CONDITION-FORMAT-ARGUMENTS`][a668].
    
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

<a id="x-28MGL-PAX-3A-2ATRANSCRIBE-CHECK-CONSISTENCY-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*TRANSCRIBE-CHECK-CONSISTENCY*%20VARIABLE"></a>

- [variable] **\*TRANSCRIBE-CHECK-CONSISTENCY\*** *NIL*

    The default value of [`TRANSCRIBE`][f1f0]'s `CHECK-CONSISTENCY` argument.

<a id="x-28MGL-PAX-3A-2ATRANSCRIBE-SYNTAXES-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*TRANSCRIBE-SYNTAXES*%20VARIABLE"></a>

- [variable] **\*TRANSCRIBE-SYNTAXES\*** *((:DEFAULT (:OUTPUT "..") (:NO-VALUE "=\> ; No value") (:READABLE "=\>")
  (:UNREADABLE "==\>") (:UNREADABLE-CONTINUATION "--\>"))
 (:COMMENTED-1 (:OUTPUT ";..") (:NO-VALUE ";=\> ; No value") (:READABLE ";=\>")
  (:READABLE-CONTINUATION ";-\>") (:UNREADABLE ";==\>")
  (:UNREADABLE-CONTINUATION ";--\>"))
 (:COMMENTED-2 (:OUTPUT ";;..") (:NO-VALUE ";;=\> ; No value")
  (:READABLE ";;=\>") (:READABLE-CONTINUATION ";;-\>") (:UNREADABLE ";;==\>")
  (:UNREADABLE-CONTINUATION ";;--\>")))*

    The default syntaxes used by [`TRANSCRIBE`][f1f0] for reading and writing
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
    syntax), then the following value is read with [`READ`][3d3c] and printed with
    [`PRIN1`][1aee] (hence no need to mark up the following lines).
    
    When writing, an extra space is added automatically if the line to
    be prefixed is not empty. Similarly, the first space following the
    prefix is discarded when reading.
    
    See `TRANSCRIBE` for how the actual syntax to be used is selected.

<a id="x-28MGL-PAX-3ATRANSCRIPTION-ERROR-20CONDITION-29"></a>
<a id="MGL-PAX:TRANSCRIPTION-ERROR%20CONDITION"></a>

- [condition] **TRANSCRIPTION-ERROR** *ERROR*

    Represents syntactic errors in the `SOURCE` argument
    of [`TRANSCRIBE`][f1f0] and also serves as the superclass of
    [`TRANSCRIPTION-CONSISTENCY-ERROR`][a249].

<a id="x-28MGL-PAX-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29"></a>
<a id="MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR%20CONDITION"></a>

- [condition] **TRANSCRIPTION-CONSISTENCY-ERROR** *TRANSCRIPTION-ERROR*

    A common superclass for
    [`TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR`][8492] and
    [`TRANSCRIPTION-VALUES-CONSISTENCY-ERROR`][238c].

<a id="x-28MGL-PAX-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29"></a>
<a id="MGL-PAX:TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR%20CONDITION"></a>

- [condition] **TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR** *TRANSCRIPTION-CONSISTENCY-ERROR*

    Signaled (with [`CERROR`][69b7]) by [`TRANSCRIBE`][f1f0] when invoked
    with `:CHECK-CONSISTENCY` and the output of a form is not the same as
    what was parsed.

<a id="x-28MGL-PAX-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29"></a>
<a id="MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR%20CONDITION"></a>

- [condition] **TRANSCRIPTION-VALUES-CONSISTENCY-ERROR** *TRANSCRIPTION-CONSISTENCY-ERROR*

    Signaled (with [`CERROR`][69b7]) by [`TRANSCRIBE`][f1f0] when invoked
    with `:CHECK-CONSISTENCY` and the values of a form are inconsistent
    with their parsed representation.

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-CONISTENCY-CHECKING-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TRANSCRIPT-CONISTENCY-CHECKING%20MGL-PAX:SECTION"></a>

### 10.4 Transcript Consistency Checking

The main use case for consistency checking is detecting
out-of-date examples in documentation, although using it for writing
tests is also a possiblity. Here, we focus on the former.

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
[`CONTINUE`][3ae8] from the error.

By default, comparisons of previous to current ouput, readable and
unreadable return values are performed with [`STRING=`][91fd], [`EQUAL`][96d0], and
`STRING=`, respectively, which is great in the simple case.
Non-determinism aside, exact matching becomes brittle as soon as the
notoriously unportable pretty printer is used or when unreadable
objects are printed with their `#<>` syntax, especially when
[`PRINT-UNREADABLE-OBJECT`][cfbb] is used with `:IDENTITY T`.

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-FINER-GRAINED-CONSISTENCY-CHECKS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@TRANSCRIPT-FINER-GRAINED-CONSISTENCY-CHECKS%20MGL-PAX:SECTION"></a>

#### 10.4.1 Finer-grained Consistency Checks

To get around this problem, consistency checking of output,
readable and unreadable values can be customized individually by
supplying [`TRANSCRIBE`][f1f0] with a `CHECK-CONSISTENCY` argument
like `((:OUTPUT <OUTPUT-CHECK>) (:READABLE
<READABLE-CHECK>) (:UNREADABLE <UNREADABLE-CHECK>))`. In this case,
`<OUTPUT-CHECK>` may be `NIL`, `T`, or a function designator.

- If it's `NIL` or there is no `:OUTPUT` entry in the list, then the
  output is not checked for consistency.

- If it's `T`, then the outputs are compared with the default,
  [`STRING=`][91fd].

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
<a id="MGL-PAX:@TRANSCRIPT-DYNENV%20MGL-PAX:SECTION"></a>

#### 10.4.2 Controlling the Dynamic Environment

The dynamic enviroment in which forms in the transcript are
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
<a id="MGL-PAX:@TRANSCRIPT-UTILITIES-FOR-CONSISTENCY-CHECKING%20MGL-PAX:SECTION"></a>

#### 10.4.3 Utilities for Consistency Checking

<a id="x-28MGL-PAX-3ASQUEEZE-WHITESPACE-20FUNCTION-29"></a>
<a id="MGL-PAX:SQUEEZE-WHITESPACE%20FUNCTION"></a>

- [function] **SQUEEZE-WHITESPACE** *STRING*

    Replace consecutive whitespace characters with a single space in
    `STRING`. This is useful to do undo the effects of pretty printing
    when building comparison functions for [`TRANSCRIBE`][f1f0].

<a id="x-28MGL-PAX-3ADELETE-TRAILING-WHITESPACE-20FUNCTION-29"></a>
<a id="MGL-PAX:DELETE-TRAILING-WHITESPACE%20FUNCTION"></a>

- [function] **DELETE-TRAILING-WHITESPACE** *STRING*

    Delete whitespace characters after the last non-whitespace
    character in each line in `STRING`.

<a id="x-28MGL-PAX-3ADELETE-COMMENTS-20FUNCTION-29"></a>
<a id="MGL-PAX:DELETE-COMMENTS%20FUNCTION"></a>

- [function] **DELETE-COMMENTS** *STRING &KEY (PATTERN ";")*

    For each line in `STRING` delete the rest of the line after and
    including the first occurrence of `PATTERN`. On changed lines, delete
    trailing whitespace too. Let's define a comparison function:
    
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
    
    Just to make sure the above example works, here it is without the being
    quoted.
    
    ```common-lisp
    (format t "hello~%world")
    .. hello     ; This is the first line.
    .. world     ; This is the second line.
    ```


<a id="x-28MGL-PAX-3A-40EXTENSION-API-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@EXTENSION-API%20MGL-PAX:SECTION"></a>

## 11 Extension API

<a id="x-28MGL-PAX-3A-40LOCATIVES-AND-REFERENCES-API-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@LOCATIVES-AND-REFERENCES-API%20MGL-PAX:SECTION"></a>

### 11.1 Locatives and References API

`(MAKE-REFERENCE 'FOO 'VARIABLE)` constructs a [`REFERENCE`][1cea] that
captures the path to take from an object (the symbol `FOO`) to an
entity of interest (for example, the documentation of the variable).
The path is called the locative. A locative can be applied to an
object like this:

```
(locate 'foo 'variable)
```

which will return the same reference as `(MAKE-REFERENCE 'FOO
'VARIABLE)`. Operations need to know how to deal with references,
which we will see in [`LOCATE-AND-COLLECT-REACHABLE-OBJECTS`][46ec],
[`LOCATE-AND-DOCUMENT`][6611] and [`LOCATE-AND-FIND-SOURCE`][d6a4].

Naturally, `(LOCATE 'FOO 'FUNCTION)` will simply return `#'FOO`, no
need to muck with references when there is a perfectly good object.

<a id="x-28MGL-PAX-3ALOCATE-20FUNCTION-29"></a>
<a id="MGL-PAX:LOCATE%20FUNCTION"></a>

- [function] **LOCATE** *OBJECT LOCATIVE &KEY (ERRORP T)*

    Follow `LOCATIVE` from `OBJECT` and return the object it leads to or a
    [`REFERENCE`][1cea] if there is no first class object corresponding to the
    location. If `ERRORP`, then a [`LOCATE-ERROR`][6887] condition is signaled when
    the lookup fails.

<a id="x-28MGL-PAX-3ALOCATE-ERROR-20CONDITION-29"></a>
<a id="MGL-PAX:LOCATE-ERROR%20CONDITION"></a>

- [condition] **LOCATE-ERROR** *ERROR*

    Signaled by [`LOCATE`][ee94] when the lookup fails and `ERRORP`
    is true.

<a id="x-28MGL-PAX-3ALOCATE-ERROR-MESSAGE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29"></a>
<a id="MGL-PAX:LOCATE-ERROR-MESSAGE%20%28MGL-PAX:READER%20MGL-PAX:LOCATE-ERROR%29"></a>

- [reader] **LOCATE-ERROR-MESSAGE** *LOCATE-ERROR* *(:MESSAGE)*

<a id="x-28MGL-PAX-3ALOCATE-ERROR-OBJECT-20-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29"></a>
<a id="MGL-PAX:LOCATE-ERROR-OBJECT%20%28MGL-PAX:READER%20MGL-PAX:LOCATE-ERROR%29"></a>

- [reader] **LOCATE-ERROR-OBJECT** *LOCATE-ERROR* *(:OBJECT)*

<a id="x-28MGL-PAX-3ALOCATE-ERROR-LOCATIVE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29"></a>
<a id="MGL-PAX:LOCATE-ERROR-LOCATIVE%20%28MGL-PAX:READER%20MGL-PAX:LOCATE-ERROR%29"></a>

- [reader] **LOCATE-ERROR-LOCATIVE** *LOCATE-ERROR* *(:LOCATIVE)*

<a id="x-28MGL-PAX-3ARESOLVE-20FUNCTION-29"></a>
<a id="MGL-PAX:RESOLVE%20FUNCTION"></a>

- [function] **RESOLVE** *REFERENCE &KEY (ERRORP T)*

    A convenience function to [`LOCATE`][ee94] `REFERENCE`'s object with its
    locative.

<a id="x-28MGL-PAX-3AREFERENCE-20CLASS-29"></a>
<a id="MGL-PAX:REFERENCE%20CLASS"></a>

- [class] **REFERENCE**

    A `REFERENCE` represents a path ([`REFERENCE-LOCATIVE`][02de])
    to take from an object ([`REFERENCE-OBJECT`][8c7d]).

<a id="x-28MGL-PAX-3AREFERENCE-OBJECT-20-28MGL-PAX-3AREADER-20MGL-PAX-3AREFERENCE-29-29"></a>
<a id="MGL-PAX:REFERENCE-OBJECT%20%28MGL-PAX:READER%20MGL-PAX:REFERENCE%29"></a>

- [reader] **REFERENCE-OBJECT** *REFERENCE* *(:OBJECT)*

<a id="x-28MGL-PAX-3AREFERENCE-LOCATIVE-20-28MGL-PAX-3AREADER-20MGL-PAX-3AREFERENCE-29-29"></a>
<a id="MGL-PAX:REFERENCE-LOCATIVE%20%28MGL-PAX:READER%20MGL-PAX:REFERENCE%29"></a>

- [reader] **REFERENCE-LOCATIVE** *REFERENCE* *(:LOCATIVE)*

<a id="x-28MGL-PAX-3AMAKE-REFERENCE-20FUNCTION-29"></a>
<a id="MGL-PAX:MAKE-REFERENCE%20FUNCTION"></a>

- [function] **MAKE-REFERENCE** *OBJECT LOCATIVE*

<a id="x-28MGL-PAX-3ALOCATIVE-TYPE-20FUNCTION-29"></a>
<a id="MGL-PAX:LOCATIVE-TYPE%20FUNCTION"></a>

- [function] **LOCATIVE-TYPE** *LOCATIVE*

    The first element of `LOCATIVE` if it's a list. If it's a symbol then
    it's that symbol itself. Typically, methods of generic functions
    working with locatives take locative type and locative args as
    separate arguments to allow methods have eql specializers on the
    type symbol.

<a id="x-28MGL-PAX-3ALOCATIVE-ARGS-20FUNCTION-29"></a>
<a id="MGL-PAX:LOCATIVE-ARGS%20FUNCTION"></a>

- [function] **LOCATIVE-ARGS** *LOCATIVE*

    The [`REST`][3f15] of `LOCATIVE` if it's a list. If it's a symbol then
    it's ().

<a id="x-28MGL-PAX-3A-40NEW-OBJECT-TYPES-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@NEW-OBJECT-TYPES%20MGL-PAX:SECTION"></a>

### 11.2 Adding New Object Types

One may wish to make the [`DOCUMENT`][432c] function and `M-.` navigation
work with new object types. Extending `DOCUMENT` can be done by
defining a [`DOCUMENT-OBJECT`][bacc] method. To allow these objects to be
referenced from [`DEFSECTION`][72b4], a [`LOCATE-OBJECT`][185d] method is to be defined.
For `M-.` [`FIND-SOURCE`][4355] can be specialized. Finally,
[`EXPORTABLE-LOCATIVE-TYPE-P`][c930] may be overridden if exporting does not
makes sense. Here is a stripped down example of how all this is done
for [`ASDF:SYSTEM:`][c097]

<a id="x-28MGL-PAX-3AASDF-EXAMPLE-20-28MGL-PAX-3AINCLUDE-20-28-3ASTART-20-28ASDF-2FSYSTEM-3ASYSTEM-20MGL-PAX-3ALOCATIVE-29-20-3AEND-20-28MGL-PAX-3A-3AEND-OF-ASDF-EXAMPLE-20VARIABLE-29-29-20-3AHEADER-NL-20-22-60-60-60commonlisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29"></a>
<a id="MGL-PAX:ASDF-EXAMPLE%20%28MGL-PAX:INCLUDE%20%28:START%20%28ASDF%2FSYSTEM:SYSTEM%20MGL-PAX:LOCATIVE%29%20:END%20%28MGL-PAX::END-OF-ASDF-EXAMPLE%20VARIABLE%29%29%20:HEADER-NL%20%22%60%60%60commonlisp%22%20:FOOTER-NL%20%22%60%60%60%22%29"></a>

```commonlisp
(define-locative-type asdf:system ()
  "Refers to an asdf system. The generated documentation will include
  meta information extracted from the system definition. This also
  serves as an example of a symbol that's not accessible in the
  current package and consequently is not exported.

  ASDF:SYSTEM is not EXPORTABLE-LOCATIVE-TYPE-P.")

(defmethod locate-object (name (locative-type (eql 'asdf:system))
                          locative-args)
  (or (and (endp locative-args)
           ;; FIXME: This is slow as hell.
           (asdf:find-system (string-downcase (string name)) nil))
      (locate-error "~S does not name an asdf system." name)))

(defmethod canonical-reference ((system asdf:system))
  (make-reference (character-string (slot-value system 'asdf::name))
                  'asdf:system))

;;; For testing
(defvar *omit-asdf-slots* nil)

(defmethod document-object ((system asdf:system) stream)
  (with-heading (stream system
                        (format nil "~A ASDF System Details"
                                (string-upcase
                                 (slot-value system 'asdf::name))))
    (flet ((foo (name fn &key type)
             (let ((value (funcall fn system)))
               (when (and value (not (equal value "")))
                 (case type
                   ((:link)
                    (format stream "- ~A: [~A](~A)~%" name value value))
                   ((:mailto)
                    (format stream "- ~A: [~A](mailto:~A)~%"
                            name value value))
                   ((:source-control)
                    (format stream "- ~A: [~A](~A)"
                            name (first value) (second value)))
                   ((:docstring)
                    (format stream "- ~A: ~A~%" name
                            (massage-docstring value :indentation "  "
                                               :exclude-first-line-p t)))
                   ((nil)
                    (format stream "- ~A: ~A~%" name value)))))))
      (unless *omit-asdf-slots*
        (foo "Version" 'asdf/component:component-version)
        (foo "Description" 'asdf/system:system-description :type :docstring)
        (foo "Long Description" 'asdf/system:system-long-description
             :type :docstring)
        (foo "Licence" 'asdf/system:system-licence)
        (foo "Author" 'asdf/system:system-author)
        (foo "Maintainer" 'asdf/system:system-maintainer)
        (foo "Mailto" 'asdf/system:system-mailto :type :mailto)
        (foo "Homepage" 'asdf/system:system-homepage :type :link)
        (foo "Bug tracker" 'asdf/system:system-bug-tracker :type :link)
        (foo "Source control" 'asdf/system:system-source-control
             :type :source-control)
        (terpri stream)))))

(defmethod find-source ((system asdf:system))
  `(:location
    (:file ,(namestring (asdf/system:system-source-file system)))
    (:position 1)
    (:snippet "")))

(add-locative-to-source-search-list 'asdf:system)

```

<a id="x-28MGL-PAX-3ADEFINE-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:DEFINE-LOCATIVE-TYPE%20MGL-PAX:MACRO"></a>

- [macro] **DEFINE-LOCATIVE-TYPE** *LOCATIVE-TYPE LAMBDA-LIST &BODY DOCSTRING*

    Declare `LOCATIVE-TYPE` as a [`LOCATIVE`][0b3a]. One gets two
    things in return: first, a place to document the format and
    semantics of `LOCATIVE-TYPE` (in `LAMBDA-LIST` and `DOCSTRING`); second,
    being able to reference `(LOCATIVE-TYPE LOCATIVE)`. For example, if
    you have:
    
    ```common-lisp
    (define-locative-type variable (&optional initform)
      "Dummy docstring.")
    ```
    
    then `(VARIABLE LOCATIVE)` refers to this form.

<a id="x-28MGL-PAX-3ADEFINE-LOCATIVE-ALIAS-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:DEFINE-LOCATIVE-ALIAS%20MGL-PAX:MACRO"></a>

- [macro] **DEFINE-LOCATIVE-ALIAS** *ALIAS LOCATIVE-TYPE*

    Define `ALIAS` as a locative equivalent to `LOCATIVE-TYPE` (both
    [`SYMBOLs`][7f9f]). The following example shows how to make docstrings read
    more naturally by defining an alias.
    
    ```common-lisp
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


<a id="x-28MGL-PAX-3AEXPORTABLE-REFERENCE-P-20GENERIC-FUNCTION-29"></a>
<a id="MGL-PAX:EXPORTABLE-REFERENCE-P%20GENERIC-FUNCTION"></a>

- [generic-function] **EXPORTABLE-REFERENCE-P** *PACKAGE SYMBOL LOCATIVE-TYPE LOCATIVE-ARGS*

    Return true iff `SYMBOL` is to be exported from
    `PACKAGE` when it occurs in a [`DEFSECTION`][72b4] as a reference with
    `LOCATIVE-TYPE` and `LOCATIVE-ARGS`. `SYMBOL` is [accessible][e077]
    in `PACKAGE`.
    
    The default method ignores calls [`EXPORTABLE-LOCATIVE-TYPE-P`][c930] with
    `LOCATIVE-TYPE` and ignores the other arguments.
    
    For example, to not export `SECTIONs`([`0`][5fac] [`1`][672f]) from [`MGL-PAX`][6fdb] the following
    method is defined.
    
    ```
    (defmethod exportable-reference-p ((package (eql (find-package 'mgl-pax)))
                                       symbol (locative-type (eql 'section))
                                       locative-args)
      nil)
    ```


<a id="x-28MGL-PAX-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29"></a>
<a id="MGL-PAX:EXPORTABLE-LOCATIVE-TYPE-P%20GENERIC-FUNCTION"></a>

- [generic-function] **EXPORTABLE-LOCATIVE-TYPE-P** *LOCATIVE-TYPE*

    Return true iff symbols in references with
    `LOCATIVE-TYPE` are to be exported by default when they occur in a
    [`DEFSECTION`][72b4]. The default method returns `T`, while the methods for
    `SECTION`([`0`][5fac] [`1`][672f]), [`GLOSSARY-TERM`][5119], `PACKAGE`([`0`][4dd7] [`1`][5fb9]), [`ASDF:SYSTEM`][c097], `METHOD`([`0`][172e] [`1`][6831]) and [`INCLUDE`][5cd7]
    return `NIL`.
    
    This function is called by the default method of
    [`EXPORTABLE-REFERENCE-P`][e51f] to decide what symbols `DEFSECTION` shall
    export when its `EXPORT` argument is true.

<a id="x-28MGL-PAX-3ALOCATE-OBJECT-20GENERIC-FUNCTION-29"></a>
<a id="MGL-PAX:LOCATE-OBJECT%20GENERIC-FUNCTION"></a>

- [generic-function] **LOCATE-OBJECT** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    Return the object to which `OBJECT` and the locative
    refer. For example, if `LOCATIVE-TYPE` is the symbol
    `PACKAGE`, this returns `(FIND-PACKAGE SYMBOL)`. Signal
    a [`LOCATE-ERROR`][6887] condition by calling the [`LOCATE-ERROR`][0019] function if the
    lookup fails. Signal other errors if the types of the argument are
    bad, for instance `LOCATIVE-ARGS` is not the empty list in the package
    example. If a [`REFERENCE`][1cea] is returned then it must be canonical in the
    sense that calling [`CANONICAL-REFERENCE`][32f5] on it will return the same
    reference. For extension only, don't call this directly.

<a id="x-28MGL-PAX-3ALOCATE-ERROR-20FUNCTION-29"></a>
<a id="MGL-PAX:LOCATE-ERROR%20FUNCTION"></a>

- [function] **LOCATE-ERROR** *&REST FORMAT-AND-ARGS*

    Call this function to signal a [`LOCATE-ERROR`][6887] condition from a
    [`LOCATE-OBJECT`][185d] method. `FORMAT-AND-ARGS` contains a format string and
    args suitable for [`FORMAT`][1f28] from which the [`LOCATE-ERROR-MESSAGE`][7da5] is
    constructed. If `FORMAT-AND-ARGS` is `NIL`, then the message will be `NIL`
    too.
    
    The object and the locative are not specified, they are added by
    [`LOCATE`][ee94] when it resignals the condition.

<a id="x-28MGL-PAX-3ACANONICAL-REFERENCE-20GENERIC-FUNCTION-29"></a>
<a id="MGL-PAX:CANONICAL-REFERENCE%20GENERIC-FUNCTION"></a>

- [generic-function] **CANONICAL-REFERENCE** *OBJECT*

    Return a [`REFERENCE`][1cea] that resolves to `OBJECT`.
    
    If `OBJECT` is a `REFERENCE`, then:
    
    - if it can be [`RESOLVE`][cd9e]d, `CANONICAL-REFERENCE` is called on the
      resolved object,
    
    - else, an equivalent reference is returned.


<a id="x-28MGL-PAX-3ACOLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29"></a>
<a id="MGL-PAX:COLLECT-REACHABLE-OBJECTS%20GENERIC-FUNCTION"></a>

- [generic-function] **COLLECT-REACHABLE-OBJECTS** *OBJECT*

    Return a list of objects representing all things
    that would be documented in a ([`DOCUMENT`][432c] `OBJECT`) call. For `SECTIONS`([`0`][5fac] [`1`][672f])
    this is simply the union of references reachable from references in
    [`SECTION-ENTRIES`][9450]. The returned objects can be anything provided that
    [`CANONICAL-REFERENCE`][32f5] works on them. The list need not include `OBJECT`
    itself.
    
    One only has to specialize this for new container-like objects.

<a id="x-28MGL-PAX-3ACOLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-28T-29-29-29"></a>
<a id="MGL-PAX:COLLECT-REACHABLE-OBJECTS%20%28METHOD%20NIL%20%28T%29%29"></a>

- [method] **COLLECT-REACHABLE-OBJECTS** *OBJECT*

    This default implementation returns the empty list. This means that
    nothing is reachable from `OBJECT`.

<a id="x-28MGL-PAX-3A-2AFORMAT-2A-20VARIABLE-29"></a>
<a id="MGL-PAX:*FORMAT*%20VARIABLE"></a>

- [variable] **\*FORMAT\*** 

    Bound by [`DOCUMENT`][432c], this allows markdown output to depend on the
    output format.

<a id="x-28MGL-PAX-3ADOCUMENT-OBJECT-20GENERIC-FUNCTION-29"></a>
<a id="MGL-PAX:DOCUMENT-OBJECT%20GENERIC-FUNCTION"></a>

- [generic-function] **DOCUMENT-OBJECT** *OBJECT STREAM*

    Write `OBJECT` (and its references recursively) in
    [`*FORMAT*`][3da8] to `STREAM`.
    
    Add methods specializing on `OBJECT` to customize how objects of that
    type are presented in the documentation.

<a id="x-28MGL-PAX-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28STRING-20T-29-29-29"></a>
<a id="MGL-PAX:DOCUMENT-OBJECT%20%28METHOD%20NIL%20%28STRING%20T%29%29"></a>

- [method] **DOCUMENT-OBJECT** *(STRING STRING) STREAM*

    Print `STRING` to `STREAM` as a docstring. That is, [clean up
    indentation][718f], perform [Codification][f1ab], and
    linking (see [Linking to Code][1865], [Linking to the Hyperspec][7cc3]).
    
    Docstrings in sources are indented in various ways, which can easily
    mess up markdown. To handle the most common cases leave, the first
    line alone but from the rest of the lines strip the longest run of
    leading spaces that is common to all non-blank lines.

<a id="x-28MGL-PAX-3AFIND-SOURCE-20GENERIC-FUNCTION-29"></a>
<a id="MGL-PAX:FIND-SOURCE%20GENERIC-FUNCTION"></a>

- [generic-function] **FIND-SOURCE** *OBJECT*

    Return the Swank source location for `OBJECT`. It
    is called by `LOCATE-DEFINITIONS-FOR-EMACS`, which lies behind the
    `M-.` extension (see [Navigating Sources in Emacs][3386]).
    
    If successful, the return value should look like one of these:
    
    ```commonlisp
    (:LOCATION
      (:FILE "/home/melisgl/own/mgl-pax/src/pax.lisp")
      (:POSITION 3303) NIL)
    (:LOCATION
      (:FILE "/home/melisgl/own/mgl-pax/src/pax.lisp")
      (:OFFSET 1 3303) NIL)
    (:LOCATION
      (:FILE "/home/melisgl/own/mgl-pax/src/pax.lisp")
      (:FUNCTION-NAME "FOO") NIL)
    ```
    
    The `NIL` above is the source snippet, which is optional. Note that
    position 1 is the first character in `:FILE`. If unsuccessful, the
    return value is like:
    
    ```commonlisp
    (:error "Unknown source location for SOMETHING")
    ```


<a id="x-28MGL-PAX-3A-40REFERENCE-BASED-EXTENSIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@REFERENCE-BASED-EXTENSIONS%20MGL-PAX:SECTION"></a>

### 11.3 Reference Based Extensions

Let's see how to extend [`DOCUMENT`][432c] and `M-.` navigation if there is
no first class object to represent the thing of interest. Recall
that [`LOCATE`][ee94] returns a [`REFERENCE`][1cea] object in this case. [`DOCUMENT-OBJECT`][bacc]
and [`FIND-SOURCE`][4355] defer to [`LOCATE-AND-DOCUMENT`][6611] and
[`LOCATE-AND-FIND-SOURCE`][d6a4], which have [`LOCATIVE-TYPE`][3200] in their argument
list for [`EQL`][01e5] specializing pleasure. Here is a stripped down
example of how the [`VARIABLE`][6c83] locative is defined:

<a id="x-28MGL-PAX-3AVARIABLE-EXAMPLE-20-28MGL-PAX-3AINCLUDE-20-28-3ASTART-20-28VARIABLE-20MGL-PAX-3ALOCATIVE-29-20-3AEND-20-28MGL-PAX-3A-3AEND-OF-VARIABLE-EXAMPLE-20VARIABLE-29-29-20-3AHEADER-NL-20-22-60-60-60commonlisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29"></a>
<a id="MGL-PAX:VARIABLE-EXAMPLE%20%28MGL-PAX:INCLUDE%20%28:START%20%28VARIABLE%20MGL-PAX:LOCATIVE%29%20:END%20%28MGL-PAX::END-OF-VARIABLE-EXAMPLE%20VARIABLE%29%29%20:HEADER-NL%20%22%60%60%60commonlisp%22%20:FOOTER-NL%20%22%60%60%60%22%29"></a>

```commonlisp
(define-locative-type variable (&optional initform)
  """Refers to a global special variable. INITFORM, or if not specified,
  the global value of the variable is included in the documentation.

  ```
  ;;; A REFERENCE is returned because there is no such type as VARIABLE.
  (locate '*FORMAT* 'variable)
  ==> #<REFERENCE *FORMAT* VARIABLE>
  ```

  For the output of `(DOCUMENT (MAKE-REFERENCE '*FORMAT* 'VARIABLE))`,
  see *FORMAT*. Note that *FORMAT* is unbound. If the variable is
  BOUNDP, then its _current_ value is included in the documentation.
  See *DOCUMENT-LINK-CODE* for an example output. To override the
  current value, `INITFORM` may be provided. This is particulary
  useful if the value of the variable is something undesirable such as
  `\\#<MY-CLASS {100171ED93}>`.
  """)

(defmethod locate-object (symbol (locative-type (eql 'variable)) locative-args)
  (unless (<= (length locative-args) 1)
    (locate-error "The lambda list of the VARIABLE locative is ~
                   (&OPTIONAL INITFORM)."))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'variable))
                                locative-args stream)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (locate-and-print-bullet locative-type locative-args symbol stream)
    (write-char #\Space stream)
    (multiple-value-bind (value unboundp) (symbol-global-value symbol)
      (when (or initformp (not unboundp))
        (print-arglist (prin1-and-escape-markdown (if initformp
                                                      initform
                                                      value))
                       stream)))
    (print-end-bullet stream)
    (with-local-references ((list (make-reference symbol 'variable)))
      (maybe-print-docstring symbol locative-type stream))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'variable))
                                   locative-args)
  (declare (ignore locative-args))
  (find-definition symbol (swank-variable-dspecs symbol)))

```

<a id="x-28MGL-PAX-3ACOLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-28MGL-PAX-3AREFERENCE-29-29-29"></a>
<a id="MGL-PAX:COLLECT-REACHABLE-OBJECTS%20%28METHOD%20NIL%20%28MGL-PAX:REFERENCE%29%29"></a>

- [method] **COLLECT-REACHABLE-OBJECTS** *(REFERENCE REFERENCE)*

    Call [`LOCATE-AND-COLLECT-REACHABLE-OBJECTS`][46ec] on the object, locative-type,
    locative-args of `REFERENCE`. If there is a specialized method for it,
    then return what it returns. If not and `REFERENCE` can be resolved to
    a non-reference, call `COLLECT-REACHABLE-OBJECTS` with it, else return
    `NIL`.

<a id="x-28MGL-PAX-3ALOCATE-AND-COLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29"></a>
<a id="MGL-PAX:LOCATE-AND-COLLECT-REACHABLE-OBJECTS%20GENERIC-FUNCTION"></a>

- [generic-function] **LOCATE-AND-COLLECT-REACHABLE-OBJECTS** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    Called by [`COLLECT-REACHABLE-OBJECTS`][8c95] on [`REFERENCE`][1cea]
    objects, this function has essentially the same purpose as its
    caller but it has different arguments to allow specializing on
    `LOCATIVE-TYPE`.

<a id="x-28MGL-PAX-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28MGL-PAX-3AREFERENCE-20T-29-29-29"></a>
<a id="MGL-PAX:DOCUMENT-OBJECT%20%28METHOD%20NIL%20%28MGL-PAX:REFERENCE%20T%29%29"></a>

- [method] **DOCUMENT-OBJECT** *(REFERENCE REFERENCE) STREAM*

    If `REFERENCE` can be resolved to a non-reference, call
    `DOCUMENT-OBJECT` with it, else call LOCATE-AND-DOCUMENT-OBJECT on the
    object, locative-type, locative-args of `REFERENCE`

<a id="x-28MGL-PAX-3ALOCATE-AND-DOCUMENT-20GENERIC-FUNCTION-29"></a>
<a id="MGL-PAX:LOCATE-AND-DOCUMENT%20GENERIC-FUNCTION"></a>

- [generic-function] **LOCATE-AND-DOCUMENT** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS STREAM*

    Called by [`DOCUMENT-OBJECT`][bacc] on [`REFERENCE`][1cea] objects,
    this function has essentially the same purpose as `DOCUMENT-OBJECT`
    but it has different arguments to allow specializing on
    `LOCATIVE-TYPE`.

<a id="x-28MGL-PAX-3ALOCATE-AND-FIND-SOURCE-20GENERIC-FUNCTION-29"></a>
<a id="MGL-PAX:LOCATE-AND-FIND-SOURCE%20GENERIC-FUNCTION"></a>

- [generic-function] **LOCATE-AND-FIND-SOURCE** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    This function serves the same purpose as
    [`FIND-SOURCE`][4355], but it has different arguments to allow specializing on
    `LOCATIVE-TYPE`. Methods defined as extensions must be
    `EQL`([`0`][8517] [`1`][01e5])-specialized on a particular locative type and return a Swank
    `location` as documented in `FIND-SOURCE`.
    
    See [`FIND-SOURCE`][5908] `(method () (t))` and
    [`FIND-SOURCE`][b1ce] `(method () (reference))` for a description of when this
    function is called. Don't call this function directly.

<a id="x-28MGL-PAX-3AFIND-SOURCE-20-28METHOD-20NIL-20-28T-29-29-29"></a>
<a id="MGL-PAX:FIND-SOURCE%20%28METHOD%20NIL%20%28T%29%29"></a>

- [method] **FIND-SOURCE** *OBJECT*

    Call [`LOCATE-AND-FIND-SOURCE`][d6a4] with the appropriate parts of
    [`CANONICAL-REFERENCE`][32f5] for `OBJECT`.

<a id="x-28MGL-PAX-3AFIND-SOURCE-20-28METHOD-20NIL-20-28MGL-PAX-3AREFERENCE-29-29-29"></a>
<a id="MGL-PAX:FIND-SOURCE%20%28METHOD%20NIL%20%28MGL-PAX:REFERENCE%29%29"></a>

- [method] **FIND-SOURCE** *(REFERENCE REFERENCE)*

    Call [`LOCATE-AND-FIND-SOURCE`][d6a4] with the appropriate parts of
    `REFERENCE`. If there is no method specialized on the [locative
    type][fd7c], then attempt to [`RESOLVE`][cd9e] `REFERENCE`
    to a non-`REFERENCE` object and invoke `FIND-SOURCE` on it.
    
    Thus for new locative types, only `FIND-SOURCE` or
    `LOCATE-AND-FIND-SOURCE` needs to be specialized.

We have covered the basic building blocks of reference based
extensions. Now let's see how the obscure
[`DEFINE-SYMBOL-LOCATIVE-TYPE`][7584] and
[`DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE`][a85e] macros work together to
simplify the common task of associating definition and documentation
with symbols in a certain context.

<a id="x-28MGL-PAX-3ADEFINE-SYMBOL-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:DEFINE-SYMBOL-LOCATIVE-TYPE%20MGL-PAX:MACRO"></a>

- [macro] **DEFINE-SYMBOL-LOCATIVE-TYPE** *LOCATIVE-TYPE LAMBDA-LIST &BODY DOCSTRING*

    Similar to [`DEFINE-LOCATIVE-TYPE`][660b] but it assumes that all things
    locatable with `LOCATIVE-TYPE` are going to be just symbols defined
    with a definer defined with [`DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE`][a85e].
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

<a id="x-28MGL-PAX-3ADEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>
<a id="MGL-PAX:DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE%20MGL-PAX:MACRO"></a>

- [macro] **DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE** *NAME LOCATIVE-TYPE &BODY DOCSTRING*

    Define a macro with `NAME` which can be used to attach documentation,
    a lambda-list and source location to a symbol in the context of
    `LOCATIVE-TYPE`. The defined macro's arglist is ([`SYMBOL`][7f9f] `LAMBDA-LIST`
    `&OPTIONAL` `DOCSTRING`). `LOCATIVE-TYPE` is assumed to have been defined
    with [`DEFINE-SYMBOL-LOCATIVE-TYPE`][7584].

<a id="x-28MGL-PAX-3A-40SECTIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="MGL-PAX:@SECTIONS%20MGL-PAX:SECTION"></a>

### 11.4 Sections

[`SECTION`][5fac] objects rarely need to be dissected since
[`DEFSECTION`][72b4] and [`DOCUMENT`][432c] cover most needs. However, it is plausible
that one wants to subclass them and maybe redefine how they are
presented.

<a id="x-28MGL-PAX-3ASECTION-20CLASS-29"></a>
<a id="MGL-PAX:SECTION%20CLASS"></a>

- [class] **SECTION**

    [`DEFSECTION`][72b4] stores its `NAME`, `TITLE`, [`PACKAGE`][5fb9],
    [`READTABLE`][248b] and `ENTRIES` arguments in [`SECTION`][5fac]
    objects.

<a id="x-28MGL-PAX-3ASECTION-NAME-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
<a id="MGL-PAX:SECTION-NAME%20%28MGL-PAX:READER%20MGL-PAX:SECTION%29"></a>

- [reader] **SECTION-NAME** *SECTION* *(:NAME)*

    The name of the global variable whose value is
    this `SECTION`([`0`][5fac] [`1`][672f]) object.

<a id="x-28MGL-PAX-3ASECTION-PACKAGE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
<a id="MGL-PAX:SECTION-PACKAGE%20%28MGL-PAX:READER%20MGL-PAX:SECTION%29"></a>

- [reader] **SECTION-PACKAGE** *SECTION* *(:PACKAGE)*

    [`*PACKAGE*`][d2c1] will be bound to this package when
    generating documentation for this section if
    [`*DOCUMENT-NORMALIZE-PACKAGES*`][440e].

<a id="x-28MGL-PAX-3ASECTION-READTABLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
<a id="MGL-PAX:SECTION-READTABLE%20%28MGL-PAX:READER%20MGL-PAX:SECTION%29"></a>

- [reader] **SECTION-READTABLE** *SECTION* *(:READTABLE)*

    [`*READTABLE*`][a916] will be bound to this when generating
    documentation for this section if [`*DOCUMENT-NORMALIZE-PACKAGES*`][440e].

<a id="x-28MGL-PAX-3ASECTION-TITLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
<a id="MGL-PAX:SECTION-TITLE%20%28MGL-PAX:READER%20MGL-PAX:SECTION%29"></a>

- [reader] **SECTION-TITLE** *SECTION* *(:TITLE)*

    A [`STRING`][4267] or `NIL`. Used in generated
    documentation.

<a id="x-28MGL-PAX-3ASECTION-LINK-TITLE-TO-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
<a id="MGL-PAX:SECTION-LINK-TITLE-TO%20%28MGL-PAX:READER%20MGL-PAX:SECTION%29"></a>

- [reader] **SECTION-LINK-TITLE-TO** *SECTION* *(:LINK-TITLE-TO = NIL)*

    A [`REFERENCE`][1cea] or `NIL`. Used in generated documentation.

<a id="x-28MGL-PAX-3ASECTION-ENTRIES-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
<a id="MGL-PAX:SECTION-ENTRIES%20%28MGL-PAX:READER%20MGL-PAX:SECTION%29"></a>

- [reader] **SECTION-ENTRIES** *SECTION* *(:ENTRIES)*

    A list of strings and [`REFERENCE`][1cea] objects in the
    order they occurred in [`DEFSECTION`][72b4].

<a id="x-28DESCRIBE-OBJECT-20-28METHOD-20NIL-20-28MGL-PAX-3ASECTION-20T-29-29-29"></a>
<a id="DESCRIBE-OBJECT%20%28METHOD%20NIL%20%28MGL-PAX:SECTION%20T%29%29"></a>

- [method] **DESCRIBE-OBJECT** *(SECTION SECTION) STREAM*

    [`SECTION`][5fac] objects are printed by calling [`DOCUMENT`][432c] on them
    with [`*DOCUMENT-NORMALIZE-PACKAGES*`][440e] turned off to reduce clutter.
    This method is only defined if [`MGL-PAX/FULL`][d761] is loaded to allow
    non-fancy descriptions to be printed when using [`CL:DESCRIBE`][38a0].

  [0019]: #MGL-PAX:LOCATE-ERROR%20FUNCTION "MGL-PAX:LOCATE-ERROR FUNCTION"
  [00d4]: #MGL-PAX:ACCESSOR%20MGL-PAX:LOCATIVE "MGL-PAX:ACCESSOR MGL-PAX:LOCATIVE"
  [01e5]: http://www.lispworks.com/documentation/HyperSpec/Body/t_eql.htm "EQL TYPE"
  [0225]: #MGL-PAX:DOCUMENT-OBJECT%20%28METHOD%20NIL%20%28STRING%20T%29%29 "MGL-PAX:DOCUMENT-OBJECT (METHOD NIL (STRING T))"
  [02de]: #MGL-PAX:REFERENCE-LOCATIVE%20%28MGL-PAX:READER%20MGL-PAX:REFERENCE%29 "MGL-PAX:REFERENCE-LOCATIVE (MGL-PAX:READER MGL-PAX:REFERENCE)"
  [06a9]: #MGL-PAX:@CONDITION-SYSTEM-LOCATIVES%20MGL-PAX:SECTION "Condition System Locatives"
  [08f7]: http://www.lispworks.com/documentation/HyperSpec/Body/v_t.htm "T MGL-PAX:CONSTANT"
  [0b3a]: #MGL-PAX:LOCATIVE%20MGL-PAX:LOCATIVE "MGL-PAX:LOCATIVE MGL-PAX:LOCATIVE"
  [1102]: http://www.lispworks.com/documentation/HyperSpec/Body/f_abortc.htm "ABORT FUNCTION"
  [117a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm "OPEN FUNCTION"
  [1281]: #MGL-PAX:@PAX-WORLD%20MGL-PAX:SECTION "PAX World"
  [13a9]: #MGL-PAX:UPDATE-ASDF-SYSTEM-READMES%20FUNCTION "MGL-PAX:UPDATE-ASDF-SYSTEM-READMES FUNCTION"
  [172e]: #METHOD%20MGL-PAX:LOCATIVE "METHOD MGL-PAX:LOCATIVE"
  [185d]: #MGL-PAX:LOCATE-OBJECT%20GENERIC-FUNCTION "MGL-PAX:LOCATE-OBJECT GENERIC-FUNCTION"
  [1864]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*STANDARD-OUTPUT* VARIABLE"
  [1865]: #MGL-PAX:@LINKING-TO-CODE%20MGL-PAX:SECTION "Linking to Code"
  [1895]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR CONDITION"
  [1aee]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRIN1 FUNCTION"
  [1b1b]: #MGL-PAX:@DOCUMENTATION-UTILITIES%20MGL-PAX:SECTION "Utilities for Generating Documentation"
  [1b28]: #MGL-PAX:*DOCUMENT-LINK-SECTIONS*%20VARIABLE "MGL-PAX:*DOCUMENT-LINK-SECTIONS* VARIABLE"
  [1cea]: #MGL-PAX:REFERENCE%20CLASS "MGL-PAX:REFERENCE CLASS"
  [1f28]: http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm "FORMAT FUNCTION"
  [2060]: #CLASS%20MGL-PAX:LOCATIVE "CLASS MGL-PAX:LOCATIVE"
  [21f5]: #MGL-PAX:@MACROLIKE-LOCATIVES%20MGL-PAX:SECTION "Locatives for Macros"
  [22c2]: #MGL-PAX:@LINKING-TO-SECTIONS%20MGL-PAX:SECTION "Linking to Sections"
  [238c]: #MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR%20CONDITION "MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR CONDITION"
  [248b]: http://www.lispworks.com/documentation/HyperSpec/Body/t_rdtabl.htm "READTABLE TYPE"
  [2634]: #MGL-PAX:@OVERVIEW-OF-ESCAPING%20MGL-PAX:SECTION "Overview of Escaping"
  [2645]: #MGL-PAX:@AMBIGUOUS-LOCATIVE%20MGL-PAX:SECTION "Ambiguous Unspecified Locative"
  [26cf]: http://www.lispworks.com/documentation/HyperSpec/Body/t_t.htm "T TYPE"
  [292a]: #MGL-PAX:@PAX-LOCATIVES%20MGL-PAX:SECTION "Locatives for PAX Constructs"
  [2c93]: #MGL-PAX:@GENERATING-DOCUMENTATION%20MGL-PAX:SECTION "Generating Documentation"
  [2ce2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_consta.htm "CONSTANTP FUNCTION"
  [2d9d]: #MGL-PAX:DEFINE-RESTART%20MGL-PAX:MACRO "MGL-PAX:DEFINE-RESTART MGL-PAX:MACRO"
  [3200]: #MGL-PAX:LOCATIVE-TYPE%20FUNCTION "MGL-PAX:LOCATIVE-TYPE FUNCTION"
  [32f5]: #MGL-PAX:CANONICAL-REFERENCE%20GENERIC-FUNCTION "MGL-PAX:CANONICAL-REFERENCE GENERIC-FUNCTION"
  [3386]: #MGL-PAX:@NAVIGATING-IN-EMACS%20MGL-PAX:SECTION "Navigating Sources in Emacs"
  [378f]: #MGL-PAX:@PARSING%20MGL-PAX:SECTION "Parsing"
  [38a0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_descri.htm "DESCRIBE FUNCTION"
  [3ae8]: http://www.lispworks.com/documentation/HyperSpec/Body/r_contin.htm "CONTINUE RESTART"
  [3d3c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm "READ FUNCTION"
  [3da8]: #MGL-PAX:*FORMAT*%20VARIABLE "MGL-PAX:*FORMAT* VARIABLE"
  [3de5]: http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm "FUNCTION TYPE"
  [3f15]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rest.htm "REST FUNCTION"
  [41fd]: #COMPILER-MACRO%20MGL-PAX:LOCATIVE "COMPILER-MACRO MGL-PAX:LOCATIVE"
  [4267]: http://www.lispworks.com/documentation/HyperSpec/Body/t_string.htm "STRING TYPE"
  [42d7]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm "DEFPACKAGE MGL-PAX:MACRO"
  [432c]: #MGL-PAX:DOCUMENT%20FUNCTION "MGL-PAX:DOCUMENT FUNCTION"
  [4355]: #MGL-PAX:FIND-SOURCE%20GENERIC-FUNCTION "MGL-PAX:FIND-SOURCE GENERIC-FUNCTION"
  [440e]: #MGL-PAX:*DOCUMENT-NORMALIZE-PACKAGES*%20VARIABLE "MGL-PAX:*DOCUMENT-NORMALIZE-PACKAGES* VARIABLE"
  [46ec]: #MGL-PAX:LOCATE-AND-COLLECT-REACHABLE-OBJECTS%20GENERIC-FUNCTION "MGL-PAX:LOCATE-AND-COLLECT-REACHABLE-OBJECTS GENERIC-FUNCTION"
  [493e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_smp_cn.htm "SIMPLE-CONDITION-FORMAT-CONTROL FUNCTION"
  [4b78]: #MGL-PAX:@EXTERNAL-LOCATIVES%20MGL-PAX:SECTION "External Locatives"
  [4bb8]: #%22mgl-pax%2Fdocument%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax/document\" ASDF/SYSTEM:SYSTEM"
  [4c48]: #MGL-PAX:@VARIABLELIKE-LOCATIVES%20MGL-PAX:SECTION "Locatives for Variables"
  [4c5e]: #MGL-PAX:@FILTERING-AMBIGUOUS-REFERENCES%20MGL-PAX:SECTION "Filtering Ambiguous References"
  [4c96]: #MGL-PAX:@LOCAL-REFERENCES%20MGL-PAX:SECTION "Local References"
  [4d92]: #MGL-PAX:@LOCATIVE%20MGL-PAX:GLOSSARY-TERM "MGL-PAX:@LOCATIVE MGL-PAX:GLOSSARY-TERM"
  [4dd7]: #PACKAGE%20MGL-PAX:LOCATIVE "PACKAGE MGL-PAX:LOCATIVE"
  [5119]: #MGL-PAX:GLOSSARY-TERM%20MGL-PAX:LOCATIVE "MGL-PAX:GLOSSARY-TERM MGL-PAX:LOCATIVE"
  [5800]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq_sle.htm "< FUNCTION"
  [5825]: #%22mgl-pax%2Ftranscribe%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax/transcribe\" ASDF/SYSTEM:SYSTEM"
  [5875]: #GENERIC-FUNCTION%20MGL-PAX:LOCATIVE "GENERIC-FUNCTION MGL-PAX:LOCATIVE"
  [5908]: #MGL-PAX:FIND-SOURCE%20%28METHOD%20NIL%20%28T%29%29 "MGL-PAX:FIND-SOURCE (METHOD NIL (T))"
  [5c74]: #MGL-PAX:@UNAMBIGUOUS-LOCATIVE%20MGL-PAX:SECTION "Unambiguous Unspecified Locative"
  [5cd7]: #MGL-PAX:INCLUDE%20MGL-PAX:LOCATIVE "MGL-PAX:INCLUDE MGL-PAX:LOCATIVE"
  [5fac]: #MGL-PAX:SECTION%20CLASS "MGL-PAX:SECTION CLASS"
  [5fb9]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm "PACKAGE TYPE"
  [6121]: #MGL-PAX:@LOCATIVE-TYPES%20MGL-PAX:SECTION "Locative Types"
  [6300]: #MGL-PAX:@TRANSCRIPTS%20MGL-PAX:SECTION "Transcripts"
  [63f3]: #MGL-PAX:DEFINE-PACKAGE%20MGL-PAX:MACRO "MGL-PAX:DEFINE-PACKAGE MGL-PAX:MACRO"
  [660b]: #MGL-PAX:DEFINE-LOCATIVE-TYPE%20MGL-PAX:MACRO "MGL-PAX:DEFINE-LOCATIVE-TYPE MGL-PAX:MACRO"
  [6611]: #MGL-PAX:LOCATE-AND-DOCUMENT%20GENERIC-FUNCTION "MGL-PAX:LOCATE-AND-DOCUMENT GENERIC-FUNCTION"
  [672f]: #MGL-PAX:SECTION%20MGL-PAX:LOCATIVE "MGL-PAX:SECTION MGL-PAX:LOCATIVE"
  [6786]: http://www.lispworks.com/documentation/HyperSpec/Issues/iss009_w.htm "\"ISSUE:AREF-1D\" MGL-PAX:CLHS"
  [6831]: http://www.lispworks.com/documentation/HyperSpec/Body/t_method.htm "METHOD TYPE"
  [6887]: #MGL-PAX:LOCATE-ERROR%20CONDITION "MGL-PAX:LOCATE-ERROR CONDITION"
  [68f1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_docume.htm "DOCUMENTATION GENERIC-FUNCTION"
  [69b7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_cerror.htm "CERROR FUNCTION"
  [69f7]: #MGL-PAX:@REFERENCE-BASED-EXTENSIONS%20MGL-PAX:SECTION "Reference Based Extensions"
  [6b59]: #MGL-PAX:@TRANSCRIPT-DYNENV%20MGL-PAX:SECTION "Controlling the Dynamic Environment"
  [6c1f]: http://www.lispworks.com/documentation/HyperSpec/Body/e_smp_cn.htm "SIMPLE-CONDITION CONDITION"
  [6c83]: #VARIABLE%20MGL-PAX:LOCATIVE "VARIABLE MGL-PAX:LOCATIVE"
  [6e18]: #MGL-PAX:@TRANSCRIPT-FINER-GRAINED-CONSISTENCY-CHECKS%20MGL-PAX:SECTION "Finer-grained Consistency Checks"
  [6fdb]: #%22mgl-pax%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax\" ASDF/SYSTEM:SYSTEM"
  [718f]: #MGL-PAX:@MARKDOWN-INDENTATION%20MGL-PAX:SECTION "Indentation"
  [727b]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_lev.htm "*PRINT-LENGTH* VARIABLE"
  [72b4]: #MGL-PAX:DEFSECTION%20MGL-PAX:MACRO "MGL-PAX:DEFSECTION MGL-PAX:MACRO"
  [72fd]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_1.htm "DEFINE-SYMBOL-MACRO MGL-PAX:MACRO"
  [730f]: #MGL-PAX:*DISCARD-DOCUMENTATION-P*%20VARIABLE "MGL-PAX:*DISCARD-DOCUMENTATION-P* VARIABLE"
  [7445]: #MGL-PAX:@INTERESTING%20MGL-PAX:GLOSSARY-TERM "MGL-PAX:@INTERESTING MGL-PAX:GLOSSARY-TERM"
  [7584]: #MGL-PAX:DEFINE-SYMBOL-LOCATIVE-TYPE%20MGL-PAX:MACRO "MGL-PAX:DEFINE-SYMBOL-LOCATIVE-TYPE MGL-PAX:MACRO"
  [75ce]: #MGL-PAX:@OBJECT%20MGL-PAX:GLOSSARY-TERM "MGL-PAX:@OBJECT MGL-PAX:GLOSSARY-TERM"
  [7c82]: #MGL-PAX:@MISCELLANEOUS-DOCUMENTATION-PRINTER-VARIABLES%20MGL-PAX:SECTION "Miscellaneous Variables"
  [7cc3]: #MGL-PAX:@LINKING-TO-THE-HYPERSPEC%20MGL-PAX:SECTION "Linking to the Hyperspec"
  [7da5]: #MGL-PAX:LOCATE-ERROR-MESSAGE%20%28MGL-PAX:READER%20MGL-PAX:LOCATE-ERROR%29 "MGL-PAX:LOCATE-ERROR-MESSAGE (MGL-PAX:READER MGL-PAX:LOCATE-ERROR)"
  [7e58]: http://www.lispworks.com/documentation/HyperSpec/Body/t_class.htm "CLASS CLASS"
  [7f9f]: http://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm "SYMBOL TYPE"
  [804d]: http://www.lispworks.com/documentation/HyperSpec/Body/m_declai.htm "DECLAIM MGL-PAX:MACRO"
  [80cd]: #MGL-PAX:@REFERENCE%20MGL-PAX:GLOSSARY-TERM "MGL-PAX:@REFERENCE MGL-PAX:GLOSSARY-TERM"
  [82e0]: #METHOD-COMBINATION%20MGL-PAX:LOCATIVE "METHOD-COMBINATION MGL-PAX:LOCATIVE"
  [8423]: #MGL-PAX:@TRANSCRIPT-UTILITIES-FOR-CONSISTENCY-CHECKING%20MGL-PAX:SECTION "Utilities for Consistency Checking"
  [8492]: #MGL-PAX:TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR%20CONDITION "MGL-PAX:TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR CONDITION"
  [8517]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eql.htm "EQL FUNCTION"
  [8710]: #MGL-PAX:ARGUMENT%20MGL-PAX:LOCATIVE "MGL-PAX:ARGUMENT MGL-PAX:LOCATIVE"
  [875e]: #MGL-PAX:*DOCUMENT-LINK-TO-HYPERSPEC*%20VARIABLE "MGL-PAX:*DOCUMENT-LINK-TO-HYPERSPEC* VARIABLE"
  [889e]: http://www.lispworks.com/documentation/HyperSpec/Body/v_sl_sls.htm "/// VARIABLE"
  [88a7]: #MGL-PAX:SECTION-READTABLE%20%28MGL-PAX:READER%20MGL-PAX:SECTION%29 "MGL-PAX:SECTION-READTABLE (MGL-PAX:READER MGL-PAX:SECTION)"
  [88cf]: #MGL-PAX:@NAME%20MGL-PAX:GLOSSARY-TERM "MGL-PAX:@NAME MGL-PAX:GLOSSARY-TERM"
  [8996]: #MGL-PAX:@SPECIFIED-LOCATIVE%20MGL-PAX:SECTION "Specified Locative"
  [89d0]: http://www.lispworks.com/documentation/HyperSpec/Body/m_deftp.htm "DEFTYPE MGL-PAX:MACRO"
  [8a58]: #MGL-PAX:@SECTIONS%20MGL-PAX:SECTION "Sections"
  [8beb]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_m.htm "FIND-METHOD GENERIC-FUNCTION"
  [8c16]: #MGL-PAX:@PREVENTING-AUTOLINKING%20MGL-PAX:SECTION "Preventing Autolinking"
  [8c3e]: #MGL-PAX:@TUTORIAL%20MGL-PAX:SECTION "Tutorial"
  [8c40]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defgen.htm "DEFGENERIC MGL-PAX:MACRO"
  [8c7d]: #MGL-PAX:REFERENCE-OBJECT%20%28MGL-PAX:READER%20MGL-PAX:REFERENCE%29 "MGL-PAX:REFERENCE-OBJECT (MGL-PAX:READER MGL-PAX:REFERENCE)"
  [8c95]: #MGL-PAX:COLLECT-REACHABLE-OBJECTS%20GENERIC-FUNCTION "MGL-PAX:COLLECT-REACHABLE-OBJECTS GENERIC-FUNCTION"
  [8c99]: http://www.lispworks.com/documentation/HyperSpec/Body/f_car_c.htm "CAR FUNCTION"
  [8d65]: http://www.lispworks.com/documentation/HyperSpec/Body/t_generi.htm "GENERIC-FUNCTION CLASS"
  [8ece]: #MGL-PAX:DEFINE-GLOSSARY-TERM%20MGL-PAX:MACRO "MGL-PAX:DEFINE-GLOSSARY-TERM MGL-PAX:MACRO"
  [8fb6]: #MGL-PAX:*DOCUMENT-MARK-UP-SIGNATURES*%20VARIABLE "MGL-PAX:*DOCUMENT-MARK-UP-SIGNATURES* VARIABLE"
  [91fd]: http://www.lispworks.com/documentation/HyperSpec/Body/f_stgeq_.htm "STRING= FUNCTION"
  [926d]: #TYPE%20MGL-PAX:LOCATIVE "TYPE MGL-PAX:LOCATIVE"
  [9450]: #MGL-PAX:SECTION-ENTRIES%20%28MGL-PAX:READER%20MGL-PAX:SECTION%29 "MGL-PAX:SECTION-ENTRIES (MGL-PAX:READER MGL-PAX:SECTION)"
  [94c7]: #MGL-PAX:@BASICS%20MGL-PAX:SECTION "Basics"
  [964b]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_3.htm "SYMBOL-PACKAGE FUNCTION"
  [9674]: http://www.lispworks.com/documentation/HyperSpec/Body/f_procla.htm "PROCLAIM FUNCTION"
  [96d0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm "EQUAL FUNCTION"
  [9717]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm "DEFUN MGL-PAX:MACRO"
  [9dbc]: #MGL-PAX:@TRANSCRIPT-API%20MGL-PAX:SECTION "Transcript API"
  [a17d]: #MGL-PAX:@MATHJAX%20MGL-PAX:SECTION "MathJax"
  [a249]: #MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR%20CONDITION "MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR CONDITION"
  [a328]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rdtabl.htm "READTABLE-CASE FUNCTION"
  [a5b1]: #MGL-PAX:SECTION-PACKAGE%20%28MGL-PAX:READER%20MGL-PAX:SECTION%29 "MGL-PAX:SECTION-PACKAGE (MGL-PAX:READER MGL-PAX:SECTION)"
  [a5de]: http://www.lispworks.com/documentation/HyperSpec/Body/m_define.htm "DEFINE-COMPILER-MACRO MGL-PAX:MACRO"
  [a5ee]: #MGL-PAX:*DOCUMENT-DOWNCASE-UPPERCASE-CODE*%20VARIABLE "MGL-PAX:*DOCUMENT-DOWNCASE-UPPERCASE-CODE* VARIABLE"
  [a668]: http://www.lispworks.com/documentation/HyperSpec/Body/f_smp_cn.htm "SIMPLE-CONDITION-FORMAT-ARGUMENTS FUNCTION"
  [a85e]: #MGL-PAX:DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE%20MGL-PAX:MACRO "MGL-PAX:DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE MGL-PAX:MACRO"
  [a916]: http://www.lispworks.com/documentation/HyperSpec/Body/v_rdtabl.htm "*READTABLE* VARIABLE"
  [ad91]: http://www.lispworks.com/documentation/HyperSpec/Body/t_rst.htm "RESTART TYPE"
  [af96]: #DESCRIBE-OBJECT%20%28METHOD%20NIL%20%28MGL-PAX:SECTION%20T%29%29 "DESCRIBE-OBJECT (METHOD NIL (MGL-PAX:SECTION T))"
  [b1ce]: #MGL-PAX:FIND-SOURCE%20%28METHOD%20NIL%20%28MGL-PAX:REFERENCE%29%29 "MGL-PAX:FIND-SOURCE (METHOD NIL (MGL-PAX:REFERENCE))"
  [b3cc]: #MGL-PAX:@EXPLICIT-AND-AUTOLINKING%20MGL-PAX:SECTION "Explicit and Autolinking"
  [b89a]: #MGL-PAX:@CODIFIABLE%20MGL-PAX:GLOSSARY-TERM "MGL-PAX:@CODIFIABLE MGL-PAX:GLOSSARY-TERM"
  [ba62]: #FUNCTION%20MGL-PAX:LOCATIVE "FUNCTION MGL-PAX:LOCATIVE"
  [ba74]: #MGL-PAX:@LINKS%20MGL-PAX:SECTION "Links"
  [bacc]: #MGL-PAX:DOCUMENT-OBJECT%20GENERIC-FUNCTION "MGL-PAX:DOCUMENT-OBJECT GENERIC-FUNCTION"
  [bb77]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq_sle.htm "<= FUNCTION"
  [bbf2]: #MGL-PAX:@NEW-OBJECT-TYPES%20MGL-PAX:SECTION "Adding New Object Types"
  [bc83]: #MGL-PAX:@MARKDOWN-SYNTAX-HIGHLIGHTING%20MGL-PAX:SECTION "Syntax Highlighting"
  [bcd2]: http://www.lispworks.com/documentation/HyperSpec/Body/d_optimi.htm "DEBUG DECLARATION"
  [bdf2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_boundp.htm "BOUNDP FUNCTION"
  [be47]: #MGL-PAX:@TYPELIKE-LOCATIVES%20MGL-PAX:SECTION "Locatives for Types and Declarations"
  [bf07]: http://www.lispworks.com/documentation/HyperSpec/Body/f_export.htm "EXPORT FUNCTION"
  [c097]: #ASDF%2FSYSTEM:SYSTEM%20MGL-PAX:LOCATIVE "ASDF/SYSTEM:SYSTEM MGL-PAX:LOCATIVE"
  [c1eb]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eval.htm "EVAL FUNCTION"
  [c2d3]: #MGL-PAX:@MARKDOWN-SUPPORT%20MGL-PAX:SECTION "Markdown Support"
  [c35d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_intern.htm "INTERN FUNCTION"
  [c4ce]: #MGL-PAX:@EXTENSION-API%20MGL-PAX:SECTION "Extension API"
  [c819]: #MGL-PAX:CONSTANT%20MGL-PAX:LOCATIVE "MGL-PAX:CONSTANT MGL-PAX:LOCATIVE"
  [c930]: #MGL-PAX:EXPORTABLE-LOCATIVE-TYPE-P%20GENERIC-FUNCTION "MGL-PAX:EXPORTABLE-LOCATIVE-TYPE-P GENERIC-FUNCTION"
  [cc04]: #MGL-PAX:READER%20MGL-PAX:LOCATIVE "MGL-PAX:READER MGL-PAX:LOCATIVE"
  [cd9e]: #MGL-PAX:RESOLVE%20FUNCTION "MGL-PAX:RESOLVE FUNCTION"
  [ce02]: http://www.lispworks.com/documentation/HyperSpec/Body/s_declar.htm "DECLARE MGL-PAX:MACRO"
  [cfbb]: http://www.lispworks.com/documentation/HyperSpec/Body/m_pr_unr.htm "PRINT-UNREADABLE-OBJECT MGL-PAX:MACRO"
  [d1ca]: #MGL-PAX:@DOCUMENT-IMPLEMENTATION-NOTES%20MGL-PAX:SECTION "Document Generation Implementation Notes"
  [d2c1]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pkg.htm "*PACKAGE* VARIABLE"
  [d684]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defcon.htm "DEFCONSTANT MGL-PAX:MACRO"
  [d6a4]: #MGL-PAX:LOCATE-AND-FIND-SOURCE%20GENERIC-FUNCTION "MGL-PAX:LOCATE-AND-FIND-SOURCE GENERIC-FUNCTION"
  [d761]: #%22mgl-pax%2Ffull%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax/full\" ASDF/SYSTEM:SYSTEM"
  [d7b0]: #MGL-PAX:@WORD%20MGL-PAX:GLOSSARY-TERM "MGL-PAX:@WORD MGL-PAX:GLOSSARY-TERM"
  [d9ee]: #MGL-PAX:*DOCUMENT-LINK-CODE*%20VARIABLE "MGL-PAX:*DOCUMENT-LINK-CODE* VARIABLE"
  [dc76]: http://www.lispworks.com/documentation/HyperSpec/Body/e_cnd.htm "CONDITION CONDITION"
  [dfe3]: #MGL-PAX:*PAX-WORLD-DIR*%20VARIABLE "MGL-PAX:*PAX-WORLD-DIR* VARIABLE"
  [dff6]: #MGL-PAX:@GITHUB-WORKFLOW%20MGL-PAX:SECTION "Github Workflow"
  [e077]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_s.htm "FIND-SYMBOL FUNCTION"
  [e216]: #MGL-PAX:*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*%20VARIABLE "MGL-PAX:*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS* VARIABLE"
  [e248]: #MGL-PAX:@FUNCTIONLIKE-LOCATIVES%20MGL-PAX:SECTION "Locatives for Functions"
  [e256]: http://www.lispworks.com/documentation/HyperSpec/Issues/iss009.htm "\"SUMMARY:AREF-1D\" MGL-PAX:CLHS"
  [e2e8]: #MGL-PAX:@SUPPRESSED-LINKS%20MGL-PAX:SECTION "Suppressed Links"
  [e391]: #MGL-PAX:DISLOCATED%20MGL-PAX:LOCATIVE "MGL-PAX:DISLOCATED MGL-PAX:LOCATIVE"
  [e4b0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ensu_1.htm "ENSURE-DIRECTORIES-EXIST FUNCTION"
  [e51f]: #MGL-PAX:EXPORTABLE-REFERENCE-P%20GENERIC-FUNCTION "MGL-PAX:EXPORTABLE-REFERENCE-P GENERIC-FUNCTION"
  [e548]: #MGL-PAX:WRITER%20MGL-PAX:LOCATIVE "MGL-PAX:WRITER MGL-PAX:LOCATIVE"
  [eafc]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pr_obj.htm "PRINT-OBJECT GENERIC-FUNCTION"
  [ebd3]: #MGL-PAX:*TRANSCRIBE-SYNTAXES*%20VARIABLE "MGL-PAX:*TRANSCRIBE-SYNTAXES* VARIABLE"
  [ed5f]: #MGL-PAX:CLHS%20MGL-PAX:LOCATIVE "MGL-PAX:CLHS MGL-PAX:LOCATIVE"
  [ee51]: #MGL-PAX:UPDATE-PAX-WORLD%20FUNCTION "MGL-PAX:UPDATE-PAX-WORLD FUNCTION"
  [ee94]: #MGL-PAX:LOCATE%20FUNCTION "MGL-PAX:LOCATE FUNCTION"
  [f12d]: #MGL-PAX:*DOCUMENT-MAX-NUMBERING-LEVEL*%20VARIABLE "MGL-PAX:*DOCUMENT-MAX-NUMBERING-LEVEL* VARIABLE"
  [f155]: #%22mgl-pax%2Fnavigate%22%20ASDF%2FSYSTEM:SYSTEM "\"mgl-pax/navigate\" ASDF/SYSTEM:SYSTEM"
  [f1ab]: #MGL-PAX:@CODIFICATION%20MGL-PAX:SECTION "Codification"
  [f1f0]: #MGL-PAX:TRANSCRIBE%20FUNCTION "MGL-PAX:TRANSCRIBE FUNCTION"
  [f25f]: #MGL-PAX:*DOCUMENT-UPPERCASE-IS-CODE*%20VARIABLE "MGL-PAX:*DOCUMENT-UPPERCASE-IS-CODE* VARIABLE"
  [f389]: #MGL-PAX:@LOCATIVES-AND-REFERENCES-API%20MGL-PAX:SECTION "Locatives and References API"
  [f3cc]: #MGL-PAX:MACRO%20MGL-PAX:LOCATIVE "MGL-PAX:MACRO MGL-PAX:LOCATIVE"
  [f47d]: #MGL-PAX:@TRANSCRIPT-CONISTENCY-CHECKING%20MGL-PAX:SECTION "Transcript Consistency Checking"
  [f4de]: http://www.lispworks.com/documentation/HyperSpec/Body/f_specia.htm "SPECIAL-OPERATOR-P FUNCTION"
  [f4fd]: #MGL-PAX:REGISTER-DOC-IN-PAX-WORLD%20FUNCTION "MGL-PAX:REGISTER-DOC-IN-PAX-WORLD FUNCTION"
  [f585]: #MGL-PAX:*DOCUMENT-HYPERSPEC-ROOT*%20VARIABLE "MGL-PAX:*DOCUMENT-HYPERSPEC-ROOT* VARIABLE"
  [f5bd]: #MGL-PAX:@TRANSCRIBING-WITH-EMACS%20MGL-PAX:SECTION "Transcribing with Emacs"
  [f74b]: #MGL-PAX:@BACKGROUND%20MGL-PAX:SECTION "Background"
  [f945]: http://www.lispworks.com/documentation/HyperSpec/Body/03_d.htm "\"3.4\" MGL-PAX:CLHS"
  [f960]: #MGL-PAX:MAKE-GITHUB-SOURCE-URI-FN%20FUNCTION "MGL-PAX:MAKE-GITHUB-SOURCE-URI-FN FUNCTION"
  [f9d2]: #MGL-PAX:@PACKAGELIKE-LOCATIVES%20MGL-PAX:SECTION "Locatives for Packages and Readtables"
  [fc18]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defmac.htm "DEFMACRO MGL-PAX:MACRO"
  [fc7b]: http://www.lispworks.com/documentation/HyperSpec/Body/t_meth_1.htm "METHOD-COMBINATION CLASS"
  [fd7c]: #MGL-PAX:@LOCATIVES-AND-REFERENCES%20MGL-PAX:SECTION "Locatives and References"
  [fdd1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRINT FUNCTION"
  [fe2b]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_4.htm "DEFINE-METHOD-COMBINATION MGL-PAX:MACRO"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
