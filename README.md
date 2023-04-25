<a id="x-28MGL-PAX-3A-40PAX-MANUAL-20MGL-PAX-3ASECTION-29"></a>
# PAX Manual

## Table of Contents

- [1 `MGL-PAX` ASDF System][6fdb]
- [2 `MGL-PAX/FULL` ASDF System][d761]
- [3 Links][ba74]
- [4 Background][f74b]
- [5 Tutorial][8c3e]
- [6 Basics][94c7]
    - [6.1 Locatives and References][fd7c]
        - [6.1.1 Locatives and References API][f389]
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
    - [8.1 `MGL-PAX/NAVIGATE` ASDF System][f155]
- [9 Generating Documentation][2c93]
    - [9.1 `MGL-PAX/DOCUMENT` ASDF System][4bb8]
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
        - [9.4.6 Unresolvable Links][b372]
        - [9.4.7 Suppressed Links][e2e8]
        - [9.4.8 Filtering Ambiguous References][4c5e]
        - [9.4.9 Local References][4c96]
    - [9.5 Linking to the Hyperspec][7cc3]
    - [9.6 Linking to Sections][22c2]
    - [9.7 Miscellaneous Variables][7c82]
    - [9.8 Utilities for Generating Documentation][1b1b]
        - [9.8.1 HTML Output][36e1]
        - [9.8.2 Github Workflow][dff6]
        - [9.8.3 PAX World][1281]
    - [9.9 Overview of Escaping][2634]
    - [9.10 Document Generation Implementation Notes][d1ca]
- [10 Transcripts][6300]
    - [10.1 `MGL-PAX/TRANSCRIBE` ASDF System][5825]
    - [10.2 Transcribing with Emacs][f5bd]
    - [10.3 Transcript API][9dbc]
    - [10.4 Transcript Consistency Checking][f47d]
        - [10.4.1 Finer-Grained Consistency Checks][6e18]
        - [10.4.2 Controlling the Dynamic Environment][6b59]
        - [10.4.3 Utilities for Consistency Checking][8423]
- [11 Writing Extensions][c4ce]
    - [11.1 Adding New Object Types][bbf2]
    - [11.2 Reference Based Extensions][69f7]
    - [11.3 Extending `DOCUMENT`][574a]
    - [11.4 Extending `FIND-SOURCE`][3eb4]
    - [11.5 Sections][8a58]
    - [11.6 Glossary Terms][d1dc]

###### \[in package MGL-PAX with nicknames PAX\]
<a id="x-28-22mgl-pax-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
## 1 `MGL-PAX` ASDF System

- Version: 0.1.0
- Description: Exploratory programming tool and documentation
  generator.
- Long Description: The set of dependencies of the [`MGL-PAX`][6fdb] system is
  kept light, and its heavier dependencies are autoloaded via ASDF
  when the relavant functionality is accessed. See the
  [`MGL-PAX/NAVIGATE`][f155], [`MGL-PAX/DOCUMENT`][4bb8], [`MGL-PAX/TRANSCRIBE`][5825] and
  [`MGL-PAX/FULL`][d761] systems. To keep deployed code small, client systems
  should declare an ASDF dependency on this system, never on the
  others, which are intended for autoloading and interactive use.
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)
- Homepage: [http://melisgl.github.io/mgl-pax](http://melisgl.github.io/mgl-pax)
- Bug tracker: [https://github.com/melisgl/mgl-pax/issues](https://github.com/melisgl/mgl-pax/issues)
- Source control: [GIT](https://github.com/melisgl/mgl-pax.git)

<a id="x-28-22mgl-pax-2Ffull-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
## 2 `MGL-PAX/FULL` ASDF System

- Description: [`MGL-PAX`][6fdb] with all features preloaded.
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)


<a id="x-28MGL-PAX-3A-40LINKS-20MGL-PAX-3ASECTION-29"></a>
## 3 Links

Here is the [official
repository](https://github.com/melisgl/mgl-pax) and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/mgl-pax-manual.html)
for the latest version.

<a id="x-28MGL-PAX-3A-40BACKGROUND-20MGL-PAX-3ASECTION-29"></a>
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
and this list was repeated in the [`DEFPACKAGE`][42d7] form complete with
little comments that were like section names. A clear violation of
[OAOO][oaoo], one of them had to go, so `DEFSECTION` got
a list of symbols to export.

[oaoo]: http://c2.com/cgi/wiki?OnceAndOnlyOnce 

That was great, but soon I found that the listing of symbols is
ambiguous if, for example, a function, a compiler macro and a class
are named by the same symbol. This did not concern exporting, of
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

##### Docstrings

PAX automatically recognizes and [marks up code][f1ab]
with backticks and [links code][1865] to their
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

- \[function\] **ABORT** *&OPTIONAL CONDITION*

    Transfer control to a restart named `ABORT`, signalling a
    [`CONTROL-ERROR`][6bc0] if none exists.

[6bc0]: http://www.lispworks.com/documentation/HyperSpec/Body/e_contro.htm "\\CONTROL-ERROR \\CONDITION"

##### A Complete Example

Here is an example of how it all works together:

<a id="x-28MGL-PAX-3AFOO-RANDOM-EXAMPLE-20-28MGL-PAX-3AINCLUDE-20-23P-22-2Fhome-2Fmelisgl-2Fown-2Fmgl-pax-2Fsrc-2Fbase-2Ffoo-random-example-2Elisp-22-20-3AHEADER-NL-20-22-60-60-60-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29"></a>
```
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
[`FUNCTION`][ba62] are just two instances of [locative][4d92]s, which are
used in `DEFSECTION` to refer to definitions tied to symbols.

`(DOCUMENT @FOO-RANDOM-MANUAL)` generates fancy markdown or HTML
output with [automatic markup][f25f] and [autolinks][1865] uppercase [word][d7b0]s
found in docstrings, numbers sections, and creates a table of
contents.

One can even generate documentation for different but related
libraries at the same time with the output going to different files
but with cross-page links being automatically added for symbols
mentioned in docstrings. See [Generating Documentation][2c93] for some
convenience functions to cover the most common cases.

The [transcript][6300] in the code block tagged with
`cl-transcript` is automatically checked for up-to-dateness when
documentation is generated.

<a id="x-28MGL-PAX-3A-40BASICS-20MGL-PAX-3ASECTION-29"></a>
## 6 Basics

Now let's examine the most important pieces.

<a id="x-28MGL-PAX-3ADEFSECTION-20MGL-PAX-3AMACRO-29"></a>
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
    
    The references are not looked up (see [`RESOLVE`][cd9e] in the
    [Locatives and References API][f389]) until documentation is generated, so
    it is allowed to refer to things yet to be defined.
    
    ##### Exporting
    
    If `EXPORT` is true (the default), `NAME` and the [object][75ce]s of references
    among `ENTRIES` which are [`SYMBOL`][7f9f]s are candidates for exporting. A
    candidate symbol is exported if
    
    - it is [accessible][e077] in `PACKAGE`, and
    
    - there is a reference to it in the section being defined which is
      approved by [`EXPORTABLE-REFERENCE-P`][e51f].
    
    See [`DEFINE-PACKAGE`][63f3] if you use the export feature. The idea with
    confounding documentation and exporting is to force documentation of
    all exported symbols.
    
    ##### Misc
    
    `TITLE` is a string containing markdown or `NIL`. If non-NIL, it
    determines the text of the heading in the generated output.
    `LINK-TITLE-TO` is a reference given as an `(OBJECT LOCATIVE)` pair or
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

    This is like [`CL:DEFPACKAGE`][42d7] but silences warnings and errors
    signaled when the redefined package is at variance with the current
    state of the package. Typically this situation occurs when symbols
    are exported by calling [`EXPORT`][bf07] (as is the case with [`DEFSECTION`][72b4]) as
    opposed to adding `:EXPORT` forms to the `DEFPACKAGE` form and the
    package definition is subsequently reevaluated. See the section on
    [package variance](http://www.sbcl.org/manual/#Package-Variance) in
    the SBCL manual.
    
    The bottom line is that if you rely on `DEFSECTION` to do the
    exporting, then you'd better use `DEFINE-PACKAGE`.

<a id="x-28MGL-PAX-3A-40LOCATIVES-AND-REFERENCES-20MGL-PAX-3ASECTION-29"></a>
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
a [reference][80cd], and they identify a definition. [`REFERENCE`][1cea]s are actual
objects, but often they appear as an `(OBJECT LOCATIVE)` list (see
[`DEFSECTION`][72b4]) or as `"OBJECT LOCATIVE"` in docstrings (see
[Linking to Code][1865] for the various forms possible).

```
(defsection @foos ()
  "We discuss the FOO type and the FOO function."
  (foo type)
  (foo function))
```


<a id="x-28MGL-PAX-3A-40REFERENCE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
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
- [glossary-term] **object**

    [object][75ce]s are symbols or strings which name [functions][ba62], [types][926d], [packages][4dd7],
    etc. Together with [locative][4d92]s, they form [reference][80cd]s.

<a id="x-28MGL-PAX-3A-40LOCATIVE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **locative**

    [locative][4d92]s specify a *type* of definition such as
    [`FUNCTION`][ba62] or [`VARIABLE`][6c83] and together with
    [object][75ce]s form [reference][80cd]s.
    
    A locative can be a symbol or a list whose [`CAR`][8c99] is a symbol. In
    either case, the symbol is called the [locative type][6121] while the rest of the elements are the
    *locative arguments*. See the [`METHOD`][172e] locative or the [`LOCATIVE`][0b3a]
    locative for examples of locative types with arguments.

<a id="x-28MGL-PAX-3A-40LOCATIVES-AND-REFERENCES-API-20MGL-PAX-3ASECTION-29"></a>
#### 6.1.1 Locatives and References API

`(MAKE-REFERENCE 'FOO 'VARIABLE)` constructs a [`REFERENCE`][1cea] that
captures the path to take from an [object][75ce] (the symbol `FOO`) to an
entity of interest (for example, the documentation of the variable).
The path is called the [locative][4d92]. A locative can be applied to an
object like this:

```
(locate 'foo 'variable)
```

which will return the same reference as `(MAKE-REFERENCE 'FOO
'VARIABLE)`. Operations need to know how to deal with references,
which we will see in the [Writing Extensions][c4ce].

Naturally, `(LOCATE 'FOO 'FUNCTION)` will simply return `#'FOO`, no
need to muck with references when there is a perfectly good object.

<a id="x-28MGL-PAX-3AREFERENCE-20CLASS-29"></a>
- [class] **REFERENCE**

    A `REFERENCE` represents a path ([`REFERENCE-LOCATIVE`][02de])
    to take from an object ([`REFERENCE-OBJECT`][8c7d]).

<a id="x-28MGL-PAX-3AREFERENCE-OBJECT-20-28MGL-PAX-3AREADER-20MGL-PAX-3AREFERENCE-29-29"></a>
- [reader] **REFERENCE-OBJECT** *REFERENCE (:OBJECT)*

<a id="x-28MGL-PAX-3AREFERENCE-LOCATIVE-20-28MGL-PAX-3AREADER-20MGL-PAX-3AREFERENCE-29-29"></a>
- [reader] **REFERENCE-LOCATIVE** *REFERENCE (:LOCATIVE)*

<a id="x-28MGL-PAX-3AMAKE-REFERENCE-20FUNCTION-29"></a>
- [function] **MAKE-REFERENCE** *OBJECT LOCATIVE*

<a id="x-28MGL-PAX-3ALOCATIVE-TYPE-20FUNCTION-29"></a>
- [function] **LOCATIVE-TYPE** *LOCATIVE*

    Return the first element of `LOCATIVE` if it's a list. If it's a symbol,
    then return that symbol itself.

<a id="x-28MGL-PAX-3ALOCATIVE-ARGS-20FUNCTION-29"></a>
- [function] **LOCATIVE-ARGS** *LOCATIVE*

    The [`REST`][3f15] of `LOCATIVE` if it's a list. If it's a symbol then it's
    `NIL`.

<a id="x-28MGL-PAX-3ALOCATE-20FUNCTION-29"></a>
- [function] **LOCATE** *OBJECT LOCATIVE &KEY (ERRORP T)*

    Follow `LOCATIVE` from `OBJECT` and return the object it leads to or a
    [`REFERENCE`][1cea] if there is no first-class object corresponding to the
    location. Depending on `ERRORP`, a [`LOCATE-ERROR`][6887] condition is signaled
    or `NIL` is returned if the lookup fails.
    
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
    ```


<a id="x-28MGL-PAX-3ALOCATE-ERROR-20CONDITION-29"></a>
- [condition] **LOCATE-ERROR** *ERROR*

    Signaled by [`LOCATE`][ee94] when the lookup fails and `ERRORP`
    is true.

<a id="x-28MGL-PAX-3ALOCATE-ERROR-MESSAGE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29"></a>
- [reader] **LOCATE-ERROR-MESSAGE** *LOCATE-ERROR (:MESSAGE)*

<a id="x-28MGL-PAX-3ALOCATE-ERROR-OBJECT-20-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29"></a>
- [reader] **LOCATE-ERROR-OBJECT** *LOCATE-ERROR (:OBJECT)*

<a id="x-28MGL-PAX-3ALOCATE-ERROR-LOCATIVE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29"></a>
- [reader] **LOCATE-ERROR-LOCATIVE** *LOCATE-ERROR (:LOCATIVE)*

<a id="x-28MGL-PAX-3ARESOLVE-20FUNCTION-29"></a>
- [function] **RESOLVE** *REFERENCE &KEY (ERRORP T)*

    A convenience function to [`LOCATE`][ee94] `REFERENCE`'s object with its
    locative.

<a id="x-28MGL-PAX-3A-40PARSING-20MGL-PAX-3ASECTION-29"></a>
### 6.2 Parsing

<a id="x-28MGL-PAX-3A-40WORD-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **word**

    A *word* is a string from which we want to extract an [object][75ce]. When
    [Navigating][3386], the word is
    `slime-symbol-at-point`. When [Generating Documentation][2c93], it is a
    non-empty string between whitespace characters in a docstring.

<a id="x-28MGL-PAX-3A-40NAME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
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
## 7 Locative Types

As we have already briefly seen in [`DEFSECTION`][72b4] and
[Locatives and References][fd7c], locatives allow us to refer to, document,
and find the source location of various definitions beyond what
standard Common Lisp offers. See [Writing Extensions][c4ce] for a more detailed
treatment. The following are the locatives types supported out of
the box. As all locative types, they are named by symbols, which
should make it obvious what kind of things they refer to. Unless
otherwise noted, locatives take no arguments.

When there is a corresponding CL type, a locative can be resolved to
a unique object as is the case in `(LOCATE 'FOO 'CLASS)` returning
`#<CLASS FOO>`. Even if there is no such CL type, the source
location and the docstring of the defining form is recorded (see
[`LOCATE-AND-FIND-SOURCE`][d6a4], [`LOCATE-AND-DOCUMENT`][6611] in the [Writing Extensions][c4ce]),
which makes navigating the sources with `M-.` (see
[Navigating Sources in Emacs][3386]) and [Generating Documentation][2c93] possible.

<a id="x-28MGL-PAX-3A-40VARIABLELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
### 7.1 Locatives for Variables

<a id="x-28VARIABLE-20MGL-PAX-3ALOCATIVE-29"></a>
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
- [locative] **CONSTANT** *&OPTIONAL INITFORM*

    Refers to a variable defined with [`DEFCONSTANT`][d684]. `INITFORM`, or if not
    specified, the value of the constant is included in the
    documentation. The [`CONSTANT`][c819] locative is like the [`VARIABLE`][6c83] locative,
    but it also checks that its object is [`CONSTANTP`][2ce2].

<a id="x-28MGL-PAX-3A-40MACROLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
### 7.2 Locatives for Macros

<a id="x-28MGL-PAX-3AMACRO-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **MACRO**

    Refers to a global macro, typically defined with [`DEFMACRO`][fc18] or a
    [special operator][f4de]. See the [`FUNCTION`][ba62]
    locative for a note on arglists.

<a id="x-28MGL-PAX-3ASYMBOL-MACRO-20MGL-PAX-3ALOCATIVE-29"></a>
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
    => "This is MY-MAC."
    ```


<a id="x-28COMPILER-MACRO-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **COMPILER-MACRO**

    Refers to a compiler macro, typically defined with
    [`DEFINE-COMPILER-MACRO`][a5de]. See the [`FUNCTION`][ba62] locative for a note on
    arglists.

<a id="x-28MGL-PAX-3A-40FUNCTIONLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
### 7.3 Locatives for Functions

<a id="x-28FUNCTION-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **FUNCTION**

    Refers to a global function, typically defined with [`DEFUN`][9717].
    
    Note that the arglist in the generated documentation depends on the
    quality of `SWANK-BACKEND:ARGLIST`. It [may be][d1ca] that default values of
    optional and keyword arguments are missing.

<a id="x-28GENERIC-FUNCTION-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **GENERIC-FUNCTION**

    Refers to a [`GENERIC-FUNCTION`][8d65], typically defined with
    [`DEFGENERIC`][8c40].

<a id="x-28METHOD-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **METHOD** *METHOD-QUALIFIERS METHOD-SPECIALIZERS*

    See [`CL:FIND-METHOD`][8beb] for the description of the arguments
    `METHOD-QUALIFIERS` and `METHOD-SPECIALIZERS`. For example, a
    `(FOO (METHOD () (T (EQL XXX))))` as a [`DEFSECTION`][72b4] entry refers to
    this method:
    
        (defmethod foo (x (y (eql 'xxx)))
          ...)
    
    `METHOD` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28METHOD-COMBINATION-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **METHOD-COMBINATION**

    Refers to a [`METHOD-COMBINATION`][fc7b], defined with
    [`DEFINE-METHOD-COMBINATION`][fe2b].

<a id="x-28MGL-PAX-3AACCESSOR-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **ACCESSOR** *CLASS-NAME*

    To refer to an accessor named `FOO-SLOT` of class
    `FOO`:
    
        (foo-slot (accessor foo))


<a id="x-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **READER** *CLASS-NAME*

    To refer to a reader named `FOO-SLOT` of class
    `FOO`:
    
        (foo-slot (reader foo))


<a id="x-28MGL-PAX-3AWRITER-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **WRITER** *CLASS-NAME*

    To refer to a writer named `FOO-SLOT` of class
    `FOO`:
    
        (foo-slot (writer foo))


<a id="x-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **STRUCTURE-ACCESSOR**

    This is a synonym of [`FUNCTION`][ba62] with the difference that
    the often ugly and certainly uninformative lambda list will not be
    printed.

<a id="x-28MGL-PAX-3A-40TYPELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
### 7.4 Locatives for Types and Declarations

<a id="x-28TYPE-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **TYPE**

    This locative can refer to any Lisp type. For types defined with
    [`DEFTYPE`][89d0], an attempt is made at printing the arguments of type
    specifiers. When `TYPE` refers to a [`CL:CLASS`][7e58], the class is
    documented as an opaque type: no mention is made of that it is a
    class or its superclasses. Use the [`CLASS`][2060] locative if those things
    are part of the contract.

<a id="x-28CLASS-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **CLASS**

    Naturally, `CLASS` is the locative type for [`CLASS`][7e58]es.
    To refer to a class named `FOO`:
    
        (foo class)
    
    In the generated documention, only superclasses denoted by [external
    symbols][e077] are included.

<a id="x-28DECLARATION-20MGL-PAX-3ALOCATIVE-29"></a>
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
### 7.5 Condition System Locatives

<a id="x-28CONDITION-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **CONDITION**

    `CONDITION` is the locative type for [`CONDITION`][dc76]s. To
    refer to a condition named `FOO`:
    
        (foo condition)
    
    In the generated documention, only superclasses denoted by [external
    symbols][e077] are included.

<a id="x-28RESTART-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **RESTART**

    A locative to refer to the definition of a restart defined by
    [`DEFINE-RESTART`][2d9d].

<a id="x-28MGL-PAX-3ADEFINE-RESTART-20MGL-PAX-3AMACRO-29"></a>
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
### 7.6 Locatives for Packages and Readtables

<a id="x-28ASDF-2FSYSTEM-3ASYSTEM-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **ASDF/SYSTEM:SYSTEM**

    Refers to an asdf system. The generated documentation will include
    meta information extracted from the system definition. This also
    serves as an example of a symbol that's not accessible in the
    current package and consequently is not exported.
    
    `ASDF:SYSTEM` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28PACKAGE-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **PACKAGE**

    Refers to a [`PACKAGE`][5fb9], defined by [`DEFPACKAGE`][42d7]. `PACKAGE` is not
    [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28READTABLE-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **READTABLE**

    Refers to a named [`READTABLE`][248b] defined with
    `NAMED-READTABLES:DEFREADTABLE`, which associates a global name and a
    docstring with the readtable object. Unfortunately, source location
    information is not available.

<a id="x-28MGL-PAX-3A-40PAX-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
### 7.7 Locatives for PAX Constructs

<a id="x-28MGL-PAX-3ASECTION-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **SECTION**

    Refers to a [`SECTION`][5fac] defined by [`DEFSECTION`][72b4].
    
    `SECTION` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **GLOSSARY-TERM**

    Refers to a [`GLOSSARY-TERM`][8251] defined by
    [`DEFINE-GLOSSARY-TERM`][8ece].

<a id="x-28MGL-PAX-3ADEFINE-GLOSSARY-TERM-20MGL-PAX-3AMACRO-29"></a>
- [macro] **DEFINE-GLOSSARY-TERM** *NAME (&KEY TITLE (DISCARD-DOCUMENTATION-P \*DISCARD-DOCUMENTATION-P\*)) DOCSTRING*

    Define a global variable with `NAME` and set it to a
    [`GLOSSARY-TERM`][8251] object. A glossary term is just a symbol to
    hang a docstring on. It is a bit like a `SECTION`([`0`][5fac] [`1`][672f]) in that, when linked
    to, its `TITLE` will be the link text instead of the name of the
    symbol. Also as with sections, both `TITLE` and `DOCSTRING` are markdown
    strings or `NIL`.
    
    Unlike sections though, glossary terms are not rendered with
    headings, but in the more lightweight bullet + locative + name/title
    style. See the glossary entry [name][88cf] for an example.
    
    When `DISCARD-DOCUMENTATION-P` (defaults to [`*DISCARD-DOCUMENTATION-P*`][730f])
    is true, `DOCSTRING` will not be recorded to save memory.

<a id="x-28MGL-PAX-3ALOCATIVE-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **LOCATIVE** *LAMBDA-LIST*

    This is the locative for locatives. When `M-.` is pressed on
    `SOME-NAME` in `(SOME-NAME LOCATIVE)`, this is what makes it
    possible to land at the corresponding [`DEFINE-LOCATIVE-TYPE`][660b] form.
    Similarly, `(LOCATIVE LOCATIVE)` leads to this very definition.

<a id="x-28MGL-PAX-3ADISLOCATED-20MGL-PAX-3ALOCATIVE-29"></a>
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


<a id="x-28MGL-PAX-3AINCLUDE-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **INCLUDE** *SOURCE &KEY LINE-PREFIX HEADER FOOTER HEADER-NL FOOTER-NL*

    This pseudolocative refers to a region of a file. `SOURCE` can be a
    [`STRING`][4267] or a [`PATHNAME`][241f] in which case the whole file is
    being pointed to, or it can explicitly supply `START`, `END` locatives.
    `INCLUDE` is typically used to include non-lisp files in the
    documentation (say markdown or elisp as in the next example) or
    regions of lisp source files. This can reduce clutter and
    duplication.
    
    ```
    (defsection example-section ()
      (pax.el (include #.(asdf:system-relative-pathname :mgl-pax "src/pax.el")
                       :header-nl "```elisp" :footer-nl "```"))
      (foo-example (include (:start (foo function)
                             :end (end-of-foo-example variable))
                            :header-nl "```"
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
    the same. If `SOURCE` is a pathname designator, then it must be
    absolute so that the locative is context independent.
    
    Finally, if specified, `LINE-PREFIX` is a string that's prepended to
    each line included in the documentation. For example, a string of
    four spaces makes markdown think it's a code block.
    
    `INCLUDE` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28MGL-PAX-3ADOCSTRING-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **DOCSTRING**

    `DOCSTRING` is a pseudolocative for including the parse tree of the
    markdown [`DOCSTRING`][e2bb] of a definition in the parse
    tree of a docstring when generating documentation. It has no source
    location information and only works as an explicit link. This
    construct is intended to allow docstrings live closer to their
    implementation, which typically involves a non-exported definition.
    
    ```
    (defun div2 (x)
      "X must be an [even type][docstring]."
      (/ x 2))
    
    (deftype even ()
      "an even integer"
      '(satisfies oddp))
    ```
    
    In the output of `(DOCUMENT #'DIV2)`, we have that `X must be an an
    even integer`.

<a id="x-28MGL-PAX-3A-40EXTERNAL-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>
### 7.8 External Locatives

<a id="x-28MGL-PAX-3ACLHS-20MGL-PAX-3ALOCATIVE-29"></a>
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
## 8 Navigating Sources in Emacs

Integration into [SLIME's `M-.`][slime-m-.]
(`slime-edit-definition`) allows one to visit the source location of
the definition that's identified by `slime-symbol-at-point` parsed
as a [word][d7b0] and the locative before or after the symbol in a buffer.
With this extension, if a locative is the previous or the next
expression around the symbol of interest, then `M-.` will go
straight to the definition which corresponds to the locative. If
that fails, `M-.` will try to find the definitions in the normal
way, which may involve popping up an xref buffer and letting the
user interactively select one of possible definitions.

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

```
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
### 8.1 `MGL-PAX/NAVIGATE` ASDF System

- Description: Slime `M-.` support for [`MGL-PAX`][6fdb].
- Long Description: Autoloaded by Slime's `M-.` when `src/pax.el` is
  loaded. See [Navigating Sources in Emacs][3386].
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)


<a id="x-28MGL-PAX-3A-40GENERATING-DOCUMENTATION-20MGL-PAX-3ASECTION-29"></a>
## 9 Generating Documentation

<a id="x-28MGL-PAX-3ADOCUMENT-20FUNCTION-29"></a>
- [function] **DOCUMENT** *OBJECT &KEY (STREAM T) PAGES (FORMAT :PLAIN)*

    Write `OBJECT` in `FORMAT` to `STREAM` diverting some output to `PAGES`.
    `FORMAT` can be anything [3BMD][3bmd] supports, which is currently
    `:MARKDOWN`, `:HTML` and `:PLAIN`. `STREAM` may be a [`STREAM`][cbf2] object,
    `T` or `NIL` as with `CL:FORMAT`.
    
    Most often, this function is called on `SECTION`([`0`][5fac] [`1`][672f]) objects as in
    `(DOCUMENT @PAX-MANUAL)`, but it supports all kinds of objects for
    which [`DOCUMENT-OBJECT`][bacc] is defined. To look up the documentation of
    the [`DOCUMENT`][432c] function itself:
    
        (document #'document)
    
    The same with fancy markup:
    
        (document #'document :format :markdown)
    
    To generate the documentation for separate libraries with automatic
    cross-links:
    
        (document (list @cube-manual @mat-manual) :format :markdown)
    
    See [Utilities for Generating Documentation][1b1b] for more.
    
    Note that not only first-class objects can have documentation:
    
        (document (locate 'foo 'type))
    
    See [Locatives and References][fd7c] for more.
    
    There are quite a few special variables that affect how output is
    generated, see [Codification][f1ab], [Linking to Code][1865],
    [Linking to Sections][22c2], and
    [Miscellaneous Variables][7c82].
    
    If `PAGES` is `NIL` and `STREAM` is `NIL`, then `DOCUMENT` returns the output
    as a string. If `PAGES` is `NIL` but `STREAM` is not, then `DOCUMENT`
    returns `NIL`. The rest of this description deals with how to generate
    multiple pages.
    
    ##### Pages
    
    The `PAGES` argument is to create multi-page documents by routing some
    of the generated output to files, strings or streams. `PAGES` is a
    list of page specification elements. A page spec is a plist with
    keys `:OBJECTS`, `:OUTPUT`, `:URI-FRAGMENT`, `:SOURCE-URI-FN`, `:HEADER-FN`
    and `:FOOTER-FN`. `OBJECTS` is a list of objects (references are allowed
    but not required) whose documentation is to be sent to `:OUTPUT`.
    
    When documentation for an object is generated, the first matching
    page spec is used, where the object matches the page spec if it is
    [reachable][8c95] from one of
    its `:OBJECTS`.
    
    `:OUTPUT` can be a number things:
    
    - If it's a list whose first element is a string or a pathname, then
      output will be sent to the file denoted by that and the rest of
      the elements of the list are passed on to [`CL:OPEN`][117a]. One extra
      keyword argument is `:ENSURE-DIRECTORIES-EXIST`. If it's true,
      [`ENSURE-DIRECTORIES-EXIST`][e4b0] will be called on the pathname before
      it's opened.
    
    - If it's `NIL`, then output will be collected in a string.
    
    - If it's `T`, then output will be sent to [`*STANDARD-OUTPUT*`][1864].
    
    - If it's a stream, then output will be sent to that stream.
    
    If some pages are specified, `DOCUMENT` returns a list of designators
    for generated output. If a page whose `:OUTPUT` refers to a file that
    was created (which doesn't happen if nothing would be written to
    it), then the corresponding pathname is included in the list. For
    strings the string itself, while for streams the stream object is
    included in the list. This way it's possible to write some pages to
    files and some to strings and have the return value indicate what
    was created. The output designators in the returned list are ordered
    by creation time.
    
    Note that even if `PAGES` is specified, `STREAM` acts as a catch all,
    taking the generated documentation for references not claimed by any
    pages. Also, the filename, string or stream corresponding to `STREAM`
    is always the first element in the list of generated things, that is
    the return value.
    
    `:HEADER-FN`, if not `NIL`, is a function of a single stream argument,
    which is called just before the first write to the page. Since
    `:FORMAT` `:HTML` only generates HTML fragments, this makes it possible
    to print arbitrary headers, typically setting the title, css
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
    string representing an URI. If `FORMAT` is `:HTML` and
    [`*DOCUMENT-MARK-UP-SIGNATURES*`][8fb6] is true, then the locative as
    displayed in the signature will be a link to this uri. See
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
      (:objects (, @pax-manual)
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
### 9.1 `MGL-PAX/DOCUMENT` ASDF System

- Description: Documentation generation support for [`MGL-PAX`][6fdb].
- Long Description: Autoloaded by [`MGL-PAX:DOCUMENT`][432c]. See
  [Generating Documentation][2c93].
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)


<a id="x-28MGL-PAX-3A-40MARKDOWN-SUPPORT-20MGL-PAX-3ASECTION-29"></a>
### 9.2 Markdown Support

The [Markdown][markdown] in docstrings is processed with the
[3BMD][3bmd] library.

<a id="x-28MGL-PAX-3A-40MARKDOWN-INDENTATION-20MGL-PAX-3ASECTION-29"></a>
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

<a id="x-28MGL-PAX-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28STRING-20T-29-29-29"></a>
- [method] **DOCUMENT-OBJECT** *(STRING STRING) STREAM*

    Print `STRING` to `STREAM` as a docstring. That is, [clean up
    indentation][718f], perform [Codification][f1ab], and
    linking (see [Linking to Code][1865], [Linking to the Hyperspec][7cc3]).
    
    Docstrings in sources are indented in various ways, which can easily
    mess up markdown. To handle the most common cases leave the first
    line alone, but from the rest of the lines strip the longest run of
    leading spaces that is common to all non-blank lines.

<a id="x-28MGL-PAX-3A-40MARKDOWN-SYNTAX-HIGHLIGHTING-20MGL-PAX-3ASECTION-29"></a>
#### 9.2.2 Syntax Highlighting

For syntax highlighting, github's [fenced code
blocks][fenced-code-blocks] markdown extension to mark up code
blocks with triple backticks is enabled so all you need to do is
write:

    ```elisp
    (defun foo ())
    ```

to get syntactically marked up HTML output. Copy `src/style.css`
from PAX and you are set. The language tag, `elisp` in this
example, is optional and defaults to `common-lisp`.

See the documentation of [3BMD][3bmd] and [colorize][colorize] for
the details.

[3bmd]: https://github.com/3b/3bmd 

[colorize]: https://github.com/redline6561/colorize/ 

[fenced-code-blocks]: https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks 


<a id="x-28MGL-PAX-3A-40MATHJAX-20MGL-PAX-3ASECTION-29"></a>
#### 9.2.3 MathJax

Displaying pretty mathematics in TeX format is supported via
MathJax. It can be done inline with `$` like this:

    $\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$

which is diplayed as $\int\_0^\infty e^\{-x^2\}
dx=\frac\{\sqrt\{\pi\}\}\{2\}$, or it can be delimited by `$$` like this:

    $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

to get: $$\int\_0^\infty e^\{-x^2\} dx=\frac\{\sqrt\{\pi\}\}\{2\}$$

MathJax will leave code blocks (including those inline with
backticks) alone. Outside code blocks, escape `$` by prefixing it
with a backslash to scare MathJax off.

Escaping all those backslashes in TeX fragments embedded in Lisp
strings can be a pain. [Pythonic String
Reader][pythonic-string-reader] can help with that.

[pythonic-string-reader]: https://github.com/smithzvk/pythonic-string-reader 


<a id="x-28MGL-PAX-3A-40CODIFICATION-20MGL-PAX-3ASECTION-29"></a>
### 9.3 Codification

<a id="x-28MGL-PAX-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20VARIABLE-29"></a>
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
- [glossary-term] **codifiable**

    A [word][d7b0] is *codifiable* iff
    
    - it has at least one uppercase character (e.g. it's not [`<`][5800], [`<=`][bb77]
      or [`///`][889e]), and
    
    - it has no lowercase characters (e.g. it's `T`, or
      [`*PRINT-LENGTH*`][727b]) or all lowercase characters immediately follow
      at least two consecutive uppercase characters (e.g. in `CLASSes`([`0`][7e58] [`1`][2060])
      but in not `Capital`).


<a id="x-28MGL-PAX-3A-40INTERESTING-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **interesting**

    A [word][d7b0] is *interesting* iff it *names*
    
    - a known reference, or
    
    - a symbol external to its package, or
    
    - it is at least 3 characters long and names an interned symbol.
    
    Where we say that a word **names** a known reference if the word
    matches the name of a thing being documented, or it is in the
    hyperspec and [`*DOCUMENT-LINK-TO-HYPERSPEC*`][875e] is true. More
    precisely, a word names a known reference, if it matches
    
    - [the object of a reference][8c7d] being
      documented (see [`DOCUMENT`][432c] and [`COLLECT-REACHABLE-OBJECTS`][8c95]), or
    
    - a name in the hyperspec if `*DOCUMENT-LINK-TO-HYPERSPEC*`.
    
    Symbols are read in the current [`*PACKAGE*`][d2c1], which is subject to
    [`*DOCUMENT-NORMALIZE-PACKAGES*`][440e].

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
### 9.4 Linking to Code

In this section, we describe all ways of linking to code
available when [`*DOCUMENT-UPPERCASE-IS-CODE*`][f25f] is true.

*Note that invoking [`M-.`][3386] on the
[object][75ce] of any of the following links will disambiguate based the
textual context, determining the locative. In a nutshell, if `M-.`
works without popping up a list of choices, then the documentation
will contain a single link.*

<a id="x-28MGL-PAX-3A-2ADOCUMENT-LINK-CODE-2A-20VARIABLE-29"></a>
- [variable] **\*DOCUMENT-LINK-CODE\*** *T*

    Enable the various forms of links in docstrings described in
    [Linking to Code][1865]. See the following sections for a description of
    how to use linking.

<a id="x-28MGL-PAX-3A-40SPECIFIED-LOCATIVE-20MGL-PAX-3ASECTION-29"></a>
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
#### 9.4.4 Explicit and Autolinking

The examples in the previous sections are marked with *explicit
link* or *autolink*. Explicit links are those with a Markdown
reference link spelled out explicitly, while autolinks are those
without.

<a id="x-28MGL-PAX-3A-40PREVENTING-AUTOLINKING-20MGL-PAX-3ASECTION-29"></a>
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

<a id="x-28MGL-PAX-3A-40UNRESOLVABLE-REFLINKS-20MGL-PAX-3ASECTION-29"></a>
#### 9.4.6 Unresolvable Links

<a id="x-28MGL-PAX-3AUNRESOLVABLE-REFLINK-20CONDITION-29"></a>
- [condition] **UNRESOLVABLE-REFLINK** *WARNING*

    When [`DOCUMENT`][432c] encounters an [explicit
    link][b3cc] such as `[NONEXISTENT][function]`
    that looks like a PAX construct but cannot be resolved, it signals
    and `UNRESOLVABLE-REFLINK` warning.
    
    - If the [`OUTPUT-REFLINK`][2ca9] restart is invoked, then no warning is
      printed and the markdown link is left unchanged. `MUFFLE-WARNING`([`0`][6dd5] [`1`][8263]) is
      equivalent to `OUTPUT-REFLINK`.
    
    - If the [`OUTPUT-LABEL`][c818] restart is invoked, then no warning is printed
      and the markdown link is replaced by its label. For example,
      `[NONEXISTENT][function]` becomes `NONEXISTENT`.
    
    - If the warning is not handled, then it is printed to
      [`*ERROR-OUTPUT*`][7f37]. Otherwise, it behaves as `OUTPUT-REFLINK`.


<a id="x-28MGL-PAX-3AOUTPUT-REFLINK-20FUNCTION-29"></a>
- [function] **OUTPUT-REFLINK** *&OPTIONAL CONDITION*

    Invoke the `OUTPUT-REFLINK` restart.

<a id="x-28MGL-PAX-3AOUTPUT-LABEL-20FUNCTION-29"></a>
- [function] **OUTPUT-LABEL** *&OPTIONAL CONDITION*

    Invoke the OUTPUT-L restart.

<a id="x-28MGL-PAX-3A-40SUPPRESSED-LINKS-20MGL-PAX-3ASECTION-29"></a>
#### 9.4.7 Suppressed Links

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
`SECTION`([`0`][5fac] [`1`][672f])s and `GLOSSARY-TERM`([`0`][8251] [`1`][5119])s always produce a link to allow their
titles to be displayed properly.

Finally, [autolinking][b3cc] to `T` or
`NIL` is suppressed (see [`*DOCUMENT-LINK-TO-HYPERSPEC*`][875e]).

<a id="x-28MGL-PAX-3A-40FILTERING-AMBIGUOUS-REFERENCES-20MGL-PAX-3ASECTION-29"></a>
#### 9.4.8 Filtering Ambiguous References

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
#### 9.4.9 Local References

To unclutter the generated output by reducing the number of
links, the so-called 'local' references (e.g. references to the very
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
  [object][75ce]s for which there are local references. For example, `FOO`
  does not get any links if there is *any* local reference with the
  same [object][75ce].

- With a locative specified (e.g. in the explicit link
  `[FOO][function]` or in the text `the FOO function`), a single
  link is made irrespective of any local references.

- Explicit links with an unspecified locative (e.g. `[FOO][]`) are
  linked to all non-local references.


<a id="x-28MGL-PAX-3A-40LINKING-TO-THE-HYPERSPEC-20MGL-PAX-3ASECTION-29"></a>
### 9.5 Linking to the Hyperspec

<a id="x-28MGL-PAX-3A-2ADOCUMENT-LINK-TO-HYPERSPEC-2A-20VARIABLE-29"></a>
- [variable] **\*DOCUMENT-LINK-TO-HYPERSPEC\*** *T*

    If true, link symbols found in code to the Common Lisp Hyperspec.
    
    Locatives work as expected (see [`*DOCUMENT-LINK-CODE*`][d9ee]):
    `FIND-IF` links to `FIND-IF`, `FUNCTION` links
    to `FUNCTION` and `[FUNCTION][type]` links to [`FUNCTION`][3de5].
    
    [Autolinking][b3cc] to `T` and `NIL` is
    suppressed. If desired, use `[T][]` (that links to `T`([`0`][08f7] [`1`][26cf])) or
    `[T][constant]` (that links to [`T`][08f7]).
    
    Note that linking to sections in the Hyperspec is done with the [`CLHS`][ed5f]
    locative and is not subject to the value of this variable.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HYPERSPEC-ROOT-2A-20VARIABLE-29"></a>
- [variable] **\*DOCUMENT-HYPERSPEC-ROOT\*** *"http://www.lispworks.com/documentation/HyperSpec/"*

    A URL pointing to an installed Common Lisp Hyperspec. The default
    value of is the canonical location.

<a id="x-28MGL-PAX-3A-40LINKING-TO-SECTIONS-20MGL-PAX-3ASECTION-29"></a>
### 9.6 Linking to Sections

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

    A non-negative integer. Top-level sections are given a table of
    contents, which includes a nested tree of section titles whose depth
    is limited by this value. Setting it to 0 turns generation of the
    table of contents off. If [`*DOCUMENT-LINK-SECTIONS*`][1b28] is true, then the
    table of contents will link to the sections.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-TEXT-NAVIGATION-2A-20VARIABLE-29"></a>
- [variable] **\*DOCUMENT-TEXT-NAVIGATION\*** *NIL*

    If true, then before each heading a line is printed with links to
    the previous, parent and next section. Needs
    [`*DOCUMENT-LINK-SECTIONS*`][1b28] to be on to work.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-FANCY-HTML-NAVIGATION-2A-20VARIABLE-29"></a>
- [variable] **\*DOCUMENT-FANCY-HTML-NAVIGATION\*** *T*

    If true and the output format is HTML, then headings get a
    navigation component that consists of links to the previous, parent,
    next section and a permalink. This component is normally hidden, it
    is visible only when the mouse is over the heading. Needs
    [`*DOCUMENT-LINK-SECTIONS*`][1b28] to be on to work.

<a id="x-28MGL-PAX-3A-40MISCELLANEOUS-DOCUMENTATION-PRINTER-VARIABLES-20MGL-PAX-3ASECTION-29"></a>
### 9.7 Miscellaneous Variables

<a id="x-28MGL-PAX-3A-2ADOCUMENT-URL-VERSIONS-2A-20VARIABLE-29"></a>
- [variable] **\*DOCUMENT-URL-VERSIONS\*** *(2 1)*

    A list of versions of PAX URL formats to support in the
    generated documenation. The first in the list is used to generate
    links.
    
    PAX emits HTML anchors before the documentation of `SECTION`([`0`][5fac] [`1`][672f])s
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
    
    Version 1 is based on the more strict HTML4 standard and the id of
    `FOO` is `"x-28MGL-PAX-3A-3AFOO-20FUNCTION-29"`. This is supported
    by Github flavoured Markdown. Version 2 has minimal clutter and is
    obviously preferred. However, in order not to break external links,
    by default, both anchors are generated.
    
    Let's understand the generated Markdown.
    
    ```
    (defun foo (x))
    
    (document #'foo)
    => ("<a id=\"x-28MGL-PAX-3AFOO-20FUNCTION-29\"></a>
    <a id=\"MGL-PAX:FOO%20FUNCTION\"></a>
    
    - [function] **FOO** *X*
    ")
    
    (let ((*document-url-versions* '(1)))
      (document #'foo))
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
    characters of the md5 sum of the full link id (the reference as a
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

<a id="x-28MGL-PAX-3A-2ADOCUMENT-NORMALIZE-PACKAGES-2A-20VARIABLE-29"></a>
- [variable] **\*DOCUMENT-NORMALIZE-PACKAGES\*** *T*

    Determines what [`*PACKAGE*`][d2c1] and [`*READTABLE*`][a916] are when working with
    generating documentation. If true and documentation is generated for
    a `SECTION`([`0`][5fac] [`1`][672f]) (including its [`SECTION-ENTRIES`][9450]), then [`SECTION-PACKAGE`][a5b1] and
    [`SECTION-READTABLE`][88a7] of the innermost containing section is used. To
    eliminate ambiguity `[in package ...]` messages are printed right
    after the section heading if necessary. If false, then `*PACKAGE*` and
    `*READTABLE*` are left at the current values.

<a id="x-28MGL-PAX-3A-40DOCUMENTATION-UTILITIES-20MGL-PAX-3ASECTION-29"></a>
### 9.8 Utilities for Generating Documentation

Two convenience functions are provided to serve the common case of
having an ASDF system with some readmes and a directory with for the
HTML documentation and the default CSS stylesheet.

<a id="x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-READMES-20FUNCTION-29"></a>
- [function] **UPDATE-ASDF-SYSTEM-READMES** *OBJECT ASDF-SYSTEM &KEY (URL-VERSIONS '(1))*

    Convenience function to generate two readme files in the directory
    holding the `ASDF-SYSTEM` definition. `OBJECT` is passed on to [`DOCUMENT`][432c].
    
    `README.md` has anchors, links, inline code, and other markup added.
    Not necessarily the easiest on the eye in an editor but looks good
    on github.
    
    `README` is optimized for reading in text format. It has less
    cluttery markup and no [autolinking][b3cc].
    
    Example usage:
    
    ```
    (update-asdf-system-readmes @pax-manual :mgl-pax)
    ```
    
    Note that [`*DOCUMENT-URL-VERSIONS*`][17e0] is bound to `URL-VERSIONS`, which
    defaults to using the uglier, version 1 style of `URL` for the sake of
    github.

<a id="x-28MGL-PAX-3A-40HTML-OUTPUT-20MGL-PAX-3ASECTION-29"></a>
#### 9.8.1 HTML Output

<a id="x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-HTML-DOCS-20FUNCTION-29"></a>
- [function] **UPDATE-ASDF-SYSTEM-HTML-DOCS** *SECTIONS ASDF-SYSTEM &KEY PAGES (TARGET-DIR (ASDF/SYSTEM:SYSTEM-RELATIVE-PATHNAME ASDF-SYSTEM "doc/")) (UPDATE-CSS-P T)*

    Generate pretty HTML documentation for a single ASDF system,
    possibly linking to github. If `UPDATE-CSS-P`, copy the CSS style
    sheet to `TARGET-DIR` as well. Example usage:
    
    ```
    (update-asdf-system-html-docs @pax-manual :mgl-pax)
    ```
    
    The same, linking to the sources on github:
    
    ```
    (update-asdf-system-html-docs
      @pax-manual :mgl-pax
      :pages
      `((:objects
        (,mgl-pax::@pax-manual)
        :source-uri-fn ,(make-git-source-uri-fn
                         :mgl-pax
                         "https://github.com/melisgl/mgl-pax"))))
    ```


See the following variables, which control HTML generation.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL-2A-20VARIABLE-29"></a>
- [variable] **\*DOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL\*** *NIL*

    `NIL` or a non-negative integer. If non-NIL, it overrides
    [`*DOCUMENT-MAX-NUMBERING-LEVEL*`][f12d] in the dynamic HTML table of contents
    on the left of the page.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-HEAD-2A-20VARIABLE-29"></a>
- [variable] **\*DOCUMENT-HTML-HEAD\*** *NIL*

    Stuff to be included in the `<head>` of the generated HTML.
    
    - If `NIL`, nothing is included.
    
    - If a `STRING`([`0`][7bd4] [`1`][4267]), then it is written to the HTML output as is without
      any escaping.
    
    - If a function designator, then it is called with a single
      argument, the HTML stream, where it must write the output.


<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-SIDEBAR-2A-20VARIABLE-29"></a>
- [variable] **\*DOCUMENT-HTML-SIDEBAR\*** *NIL*

    Stuff to be included in the HTML sidebar.
    
    - If `NIL`, a default sidebar is generated, with
      [`*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*`][e216], followed by the dynamic table
      of contents, and [`*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS*`][0ef0].
    
    - If a `STRING`([`0`][7bd4] [`1`][4267]), then it is written to the HTML output as is without
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
    [`DOCUMENT`][432c]ed or a [`REFERENCE`][1cea] thereof.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS-2A-20VARIABLE-29"></a>
- [variable] **\*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS\*** *NIL*

    Like [`*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*`][e216], only it is displayed
    below the table of contents.

<a id="x-28MGL-PAX-3A-40GITHUB-WORKFLOW-20MGL-PAX-3ASECTION-29"></a>
#### 9.8.2 Github Workflow

It is generally recommended to commit generated readmes (see
[`UPDATE-ASDF-SYSTEM-READMES`][13a9]), so that users have something to read
without reading the code and sites like github can display them.

HTML documentation can also be committed, but there is an issue with
that: when linking to the sources (see [`MAKE-GIT-SOURCE-URI-FN`][587f]), the
commit id is in the link. This means that code changes need to be
committed first, and only then can HTML documentation be regenerated
and committed in a followup commit.

The second issue is that github is not very good at serving HTMLs
files from the repository itself (and
[http://htmlpreview.github.io](http://htmlpreview.github.io) chokes
on links to the sources).

The recommended workflow is to use
[gh-pages](https://pages.github.com/), which can be made relatively
painless with the `git worktree` command. The gist of it is to make
the `doc/` directory a checkout of the branch named `gh-pages`. A
good description of this process is
[http://sangsoonam.github.io/2019/02/08/using-git-worktree-to-deploy-github-pages.html](http://sangsoonam.github.io/2019/02/08/using-git-worktree-to-deploy-github-pages.html).
Two commits needed still, but it is somewhat less painful.

This way the HTML documentation will be available at
`http://<username>.github.io/<repo-name>`. It is probably a good
idea to add sections like the [Links][ba74] section to allow jumping
between the repository and the gh-pages site.

<a id="x-28MGL-PAX-3AMAKE-GITHUB-SOURCE-URI-FN-20FUNCTION-29"></a>
- [function] **MAKE-GITHUB-SOURCE-URI-FN** *ASDF-SYSTEM GITHUB-URI &KEY GIT-VERSION*

    This function is a backward-compatibility wrapper around
    [`MAKE-GIT-SOURCE-URI-FN`][587f], which supersedes `MAKE-GITHUB-SOURCE-URI-FN`.
    All arguments are passed on to `MAKE-GIT-SOURCE-URI-FN`, leaving
    `URI-FORMAT-STRING` at its default, which is suitable for github.

<a id="x-28MGL-PAX-3AMAKE-GIT-SOURCE-URI-FN-20FUNCTION-29"></a>
- [function] **MAKE-GIT-SOURCE-URI-FN** *ASDF-SYSTEM GIT-FORGE-URI &KEY GIT-VERSION (URI-FORMAT-STRING "~A/blob/~A/~A#L~S")*

    Return a function suitable as `:SOURCE-URI-FN` of a page spec (see the
    `PAGES` argument of [`DOCUMENT`][432c]). The function looks at the source
    location of the [`REFERENCE`][1cea] passed to it, and if the location is
    found, the path is made relative to the root directory of
    `ASDF-SYSTEM` and finally an URI pointing to your git forge (such as
    github) is returned. A warning is signalled whenever the source
    location lookup fails or if the source location points to a
    directory not below the directory of `ASDF-SYSTEM`.
    
    If `GIT-FORGE-URI` is `"https://github.com/melisgl/mgl-pax/"` and
    `GIT-VERSION` is `"master"`, then the returned URI may look like this:
    
        https://github.com/melisgl/mgl-pax/blob/master/src/pax-early.lisp#L12
    
    If `GIT-VERSION` is `NIL`, then an attempt is made to determine to
    current commit id from the `.git` in the directory holding
    `ASDF-SYSTEM`. If no `.git` directory is found, then no links to
    the git forge will be generated.
    
    `URI-FORMAT-STRING` is a [`CL:FORMAT`][1f28] control string for four arguments:
    
    - `GIT-FORGE-URI`,
    
    - `GIT-VERSION`,
    
    - the relative path to the file of the source location of the reference,
    
    - and the line number.
    
    The default value of `URI-FORMAT-STRING` is for github. If using a
    non-standard git forge, such as Sourcehut or Gitlab, simply pass a
    suitable `URI-FORMAT-STRING` matching the URI scheme of your forge.

<a id="x-28MGL-PAX-3A-40PAX-WORLD-20MGL-PAX-3ASECTION-29"></a>
#### 9.8.3 PAX World

PAX World is a registry of documents, which can generate
cross-linked HTML documentation pages for all the registered
documents.

<a id="x-28MGL-PAX-3AREGISTER-DOC-IN-PAX-WORLD-20FUNCTION-29"></a>
- [function] **REGISTER-DOC-IN-PAX-WORLD** *NAME SECTIONS PAGE-SPECS*

    Register `SECTIONS` and `PAGE-SPECS` under `NAME` in PAX World. By
    default, [`UPDATE-PAX-WORLD`][ee51] generates documentation for all of these.

For example, this is how PAX registers itself:

<a id="x-28MGL-PAX-3AREGISTER-DOC-EXAMPLE-20-28MGL-PAX-3AINCLUDE-20-28-3ASTART-20-28MGL-PAX-3A-3APAX-SECTIONS-20FUNCTION-29-20-3AEND-20-28MGL-PAX-3A-3AEND-OF-REGISTER-DOC-EXAMPLE-20VARIABLE-29-29-20-3AHEADER-NL-20-22-60-60-60-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29"></a>
```
(defun pax-sections ()
  (list @pax-manual))
(defun pax-pages ()
  `((:objects
     (, @pax-manual)
     :source-uri-fn ,(make-git-source-uri-fn
                      :mgl-pax
                      "https://github.com/melisgl/mgl-pax"))))
(register-doc-in-pax-world :pax (pax-sections) (pax-pages))
```

<a id="x-28MGL-PAX-3AUPDATE-PAX-WORLD-20FUNCTION-29"></a>
- [function] **UPDATE-PAX-WORLD** *&KEY (DOCS \*REGISTERED-PAX-WORLD-DOCS\*) DIR*

    Generate HTML documentation for all `DOCS`. Files are created in
    `DIR` (`(asdf:system-relative-pathname :mgl-pax "world/")` by
    default if `DIR` is `NIL`). `DOCS` is a list of entries of the form (`NAME`
    `SECTIONS`([`0`][5fac] [`1`][672f]) `PAGE-SPECS`). The default for `DOCS` is all the sections and
    pages registered with [`REGISTER-DOC-IN-PAX-WORLD`][f4fd].
    
    In the absence of `:HEADER-FN` `:FOOTER-FN`, `:OUTPUT`, every spec in
    `PAGE-SPECS` is augmented with HTML headers, footers and output
    location specifications (based on the name of the section).
    
    If necessary a default page spec is created for every section.

<a id="x-28MGL-PAX-3A-40OVERVIEW-OF-ESCAPING-20MGL-PAX-3ASECTION-29"></a>
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
### 9.10 Document Generation Implementation Notes

Documentation Generation is supported on ABCL, AllegroCL, CLISP,
CCL, CMUCL, ECL and SBCL, but their outputs may differ due to the
lack of some introspective capability. SBCL generates complete
output. Compared to that, the following are not supported:

- [`COMPILER-MACRO`][41fd] docstrings on ABCL, AllegroCL, CCL, ECL,

- [`DEFTYPE`][89d0] lambda lists on ABCL, AllegroCL, CLISP, CCL, CMUCL, ECL,

- default values in [`MACRO`][f3cc] lambda lists on AllegroCL,

- `METHOD-COMBINATION`([`0`][fc7b] [`1`][82e0]) docstrings on ABCL, AllegroCL.


<a id="x-28MGL-PAX-3A-40TRANSCRIPTS-20MGL-PAX-3ASECTION-29"></a>
## 10 Transcripts

What are transcripts for? When writing a tutorial, one often wants
to include a REPL session with maybe a few defuns and a couple of
forms whose output or return values are shown. Also, in a function's
docstring an example call with concrete arguments and return values
speaks volumes. A transcript is a text that looks like a repl
session, but which has a light markup for printed output and return
values, while no markup (i.e. prompt) for Lisp forms. PAX
transcripts may include output and return values of all forms, or
only selected ones. In either case, the transcript itself can be
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
PYTHONIC-STRING-READER's triple-quoted strings, that allow one to
work with nested strings with less noise. The triple-quote syntax
can be enabled with:

    (in-readtable pythonic-string-syntax)


<a id="x-28-22mgl-pax-2Ftranscribe-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
### 10.1 `MGL-PAX/TRANSCRIBE` ASDF System

- Description: Transcription support for [`MGL-PAX`][6fdb].
- Long Description: Autoloaded by [`MGL-PAX:TRANSCRIBE`][f1f0] and by the Emacs
  integration (see [Transcripts][6300]).
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)


<a id="x-28MGL-PAX-3A-40TRANSCRIBING-WITH-EMACS-20MGL-PAX-3ASECTION-29"></a>
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
### 10.3 Transcript API

<a id="x-28MGL-PAX-3ATRANSCRIBE-20FUNCTION-29"></a>
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
    
    With `UPDATE-ONLY`, the printed output of a form is only transcribed
    if there were output markers in the source. Similarly, with
    `UPDATE-ONLY`, return values are only transcribed if there were value
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
    So with `UPDATE-ONLY` the above example is transcribed to:
    
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
    contained the output. Similary, for values, a continuable
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
    
    The above scheme involves [`READ`][3d3c], so consistency of unreadable values
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

<a id="x-28MGL-PAX-3A-2ATRANSCRIBE-CHECK-CONSISTENCY-2A-20VARIABLE-29"></a>
- [variable] **\*TRANSCRIBE-CHECK-CONSISTENCY\*** *NIL*

    The default value of [`TRANSCRIBE`][f1f0]'s `CHECK-CONSISTENCY` argument.

<a id="x-28MGL-PAX-3A-2ATRANSCRIBE-SYNTAXES-2A-20VARIABLE-29"></a>
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
    syntax), then the following value is read with [`READ`][3d3c] and printed with
    [`PRIN1`][1aee] (hence no need to mark up the following lines).
    
    When writing, an extra space is added automatically if the line to
    be prefixed is not empty. Similarly, the first space following the
    prefix is discarded when reading.
    
    See `TRANSCRIBE` for how the actual syntax to be used is selected.

<a id="x-28MGL-PAX-3ATRANSCRIPTION-ERROR-20CONDITION-29"></a>
- [condition] **TRANSCRIPTION-ERROR** *ERROR*

    Represents syntactic errors in the `SOURCE` argument
    of [`TRANSCRIBE`][f1f0] and also serves as the superclass of
    [`TRANSCRIPTION-CONSISTENCY-ERROR`][a249].

<a id="x-28MGL-PAX-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29"></a>
- [condition] **TRANSCRIPTION-CONSISTENCY-ERROR** *TRANSCRIPTION-ERROR*

    A common superclass for
    [`TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR`][8492] and
    [`TRANSCRIPTION-VALUES-CONSISTENCY-ERROR`][238c].

<a id="x-28MGL-PAX-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29"></a>
- [condition] **TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR** *TRANSCRIPTION-CONSISTENCY-ERROR*

    Signaled (with [`CERROR`][69b7]) by [`TRANSCRIBE`][f1f0] when invoked
    with `:CHECK-CONSISTENCY` and the output of a form is not the same as
    what was parsed.

<a id="x-28MGL-PAX-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29"></a>
- [condition] **TRANSCRIPTION-VALUES-CONSISTENCY-ERROR** *TRANSCRIPTION-CONSISTENCY-ERROR*

    Signaled (with [`CERROR`][69b7]) by [`TRANSCRIBE`][f1f0] when invoked
    with `:CHECK-CONSISTENCY` and the values of a form are inconsistent
    with their parsed representation.

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-CONISTENCY-CHECKING-20MGL-PAX-3ASECTION-29"></a>
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
#### 10.4.1 Finer-Grained Consistency Checks

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
#### 10.4.3 Utilities for Consistency Checking

<a id="x-28MGL-PAX-3ASQUEEZE-WHITESPACE-20FUNCTION-29"></a>
- [function] **SQUEEZE-WHITESPACE** *STRING*

    Replace consecutive whitespace characters with a single space in
    `STRING`. This is useful to do undo the effects of pretty printing
    when building comparison functions for [`TRANSCRIBE`][f1f0].

<a id="x-28MGL-PAX-3ADELETE-TRAILING-WHITESPACE-20FUNCTION-29"></a>
- [function] **DELETE-TRAILING-WHITESPACE** *STRING*

    Delete whitespace characters after the last non-whitespace
    character in each line in `STRING`.

<a id="x-28MGL-PAX-3ADELETE-COMMENTS-20FUNCTION-29"></a>
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
## 11 Writing Extensions

<a id="x-28MGL-PAX-3A-40NEW-OBJECT-TYPES-20MGL-PAX-3ASECTION-29"></a>
### 11.1 Adding New Object Types

One may wish to make the [`DOCUMENT`][432c] function and `M-.` navigation
work with new object types. `DOCUMENT` can be extended by defining a
[`DOCUMENT-OBJECT`][bacc] method specialized on that type. To allow these
objects to be referenced from [`DEFSECTION`][72b4], [`LOCATE-OBJECT`][185d] method is to
be defined. If there are multiple equivalent references possible for
the same thing, then [`CANONICAL-REFERENCE`][32f5] must be specialized. For
the [`DOCSTRING`][ce75] locative to work on the new type, a `DOCSTRING` method
is needed. For `M-.` [`FIND-SOURCE`][4355] can be specialized. Finally,
[`EXPORTABLE-LOCATIVE-TYPE-P`][c930] may be overridden if exporting does not
makes sense. Here is how all this is done for [`ASDF:SYSTEM:`][c097]

<a id="x-28MGL-PAX-3AASDF-EXAMPLE-20-28MGL-PAX-3AINCLUDE-20-28-3ASTART-20-28ASDF-2FSYSTEM-3ASYSTEM-20MGL-PAX-3ALOCATIVE-29-20-3AEND-20-28MGL-PAX-3A-3AEND-OF-ASDF-EXAMPLE-20VARIABLE-29-29-20-3AHEADER-NL-20-22-60-60-60-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29"></a>
```
(define-locative-type asdf:system ()
  "Refers to an asdf system. The generated documentation will include
  meta information extracted from the system definition. This also
  serves as an example of a symbol that's not accessible in the
  current package and consequently is not exported.

  ASDF:SYSTEM is not EXPORTABLE-LOCATIVE-TYPE-P.")

(defmethod locate-object (name (locative-type (eql 'asdf:system))
                          locative-args)
  (or (and (endp locative-args)
           ;; ASDF:FIND-SYSTEM is slow as hell.
           (asdf:find-system (string-downcase (string name)) nil))
      (locate-error "~S does not name an asdf system." name)))

(defmethod canonical-reference ((system asdf:system))
  (make-reference (character-string (slot-value system 'asdf::name))
                  'asdf:system))

;;; For testing
(defvar *omit-asdf-slots* nil)

(defmethod document-object ((system asdf:system) stream)
  (with-heading (stream system
                        (format nil "~A \\ASDF System"
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
                    (format stream "- ~A: " name)
                    (document-docstring value stream
                                        :indentation "  "
                                        :exclude-first-line-p t
                                        :paragraphp nil)
                    (terpri stream))
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

(defmethod docstring ((system asdf:system))
  nil)

(defmethod find-source ((system asdf:system))
  `(:location
    (:file ,(namestring (asdf/system:system-source-file system)))
    (:position 1)
    (:snippet "")))

(add-locative-to-source-search-list 'asdf:system)

```

<a id="x-28MGL-PAX-3ADEFINE-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>
- [macro] **DEFINE-LOCATIVE-TYPE** *LOCATIVE-TYPE LAMBDA-LIST &BODY DOCSTRING*

    Declare `LOCATIVE-TYPE` as a [`LOCATIVE`][0b3a]. One gets two
    things in return: first, a place to document the format and
    semantics of `LOCATIVE-TYPE` (in `LAMBDA-LIST` and `DOCSTRING`); second,
    being able to reference `(LOCATIVE-TYPE LOCATIVE)`. For example, if
    you have:
    
    ```
    (define-locative-type variable (&optional initform)
      "Dummy docstring.")
    ```
    
    then `(VARIABLE LOCATIVE)` refers to this form.

<a id="x-28MGL-PAX-3ADEFINE-LOCATIVE-ALIAS-20MGL-PAX-3AMACRO-29"></a>
- [macro] **DEFINE-LOCATIVE-ALIAS** *ALIAS LOCATIVE-TYPE*

    Define `ALIAS` as a locative equivalent to `LOCATIVE-TYPE` (both
    [`SYMBOL`][7f9f]s). The following example shows how to make docstrings read
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


<a id="x-28MGL-PAX-3ALOCATE-OBJECT-20GENERIC-FUNCTION-29"></a>
- [generic-function] **LOCATE-OBJECT** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    Return the object to which `OBJECT` and the locative
    refer. Signal a [`LOCATE-ERROR`][6887] condition by calling the [`LOCATE-ERROR`][0019]
    function if the lookup fails. If a [`REFERENCE`][1cea] is returned, then it
    must be canonical in the sense that calling [`CANONICAL-REFERENCE`][32f5] on
    it will return the same reference. Don't call this function
    directly. It serves only to extend [`LOCATE`][ee94].

<a id="x-28MGL-PAX-3ALOCATE-ERROR-20FUNCTION-29"></a>
- [function] **LOCATE-ERROR** *&REST FORMAT-AND-ARGS*

    Call this function to signal a [`LOCATE-ERROR`][6887] condition from a
    [`LOCATE-OBJECT`][185d] method. `FORMAT-AND-ARGS` contains a format string and
    args suitable for [`FORMAT`][1f28] from which the [`LOCATE-ERROR-MESSAGE`][7da5] is
    constructed. If `FORMAT-AND-ARGS` is `NIL`, then the message will be `NIL`
    too.
    
    [`LOCATE-ERROR-OBJECT`][7b2c] and [`LOCATE-ERROR-LOCATIVE`][9974] are populated
    automatically.

<a id="x-28MGL-PAX-3ACANONICAL-REFERENCE-20GENERIC-FUNCTION-29"></a>
- [generic-function] **CANONICAL-REFERENCE** *OBJECT*

    Return a [`REFERENCE`][1cea] that [`RESOLVE`][cd9e]s to `OBJECT`, or
    return `NIL` if this operation is not defined for `OBJECT`. Its
    [reference delegate][e403] is [`LOCATE-CANONICAL-REFERENCE`][c267].

<a id="x-28MGL-PAX-3ACOLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29"></a>
- [generic-function] **COLLECT-REACHABLE-OBJECTS** *OBJECT*

    Return a list of objects representing all things to
    be documented in a `(DOCUMENT OBJECT)` call. For a `SECTION`([`0`][5fac] [`1`][672f]) this is
    simply the union of references reachable from references in its
    [`SECTION-ENTRIES`][9450]. The returned objects can be anything provided that
    [`CANONICAL-REFERENCE`][32f5] works on them. The list need not include `OBJECT`
    itself.
    
    One only has to specialize this for new container-like objects. Its
    [reference delegate][e403] is [`LOCATE-AND-COLLECT-REACHABLE-OBJECTS`][46ec].

<a id="x-28MGL-PAX-3ADOCUMENT-OBJECT-20GENERIC-FUNCTION-29"></a>
- [generic-function] **DOCUMENT-OBJECT** *OBJECT STREAM*

    Write `OBJECT` (and its references recursively) in
    [`*FORMAT*`][3da8] to `STREAM` in markdown format. Add methods specializing on
    `OBJECT` to customize the output of [`DOCUMENT`][432c]. Its [reference delegate][e403]
    is [`LOCATE-AND-DOCUMENT`][6611]. This function is for extension, don't call
    it directly.

<a id="x-28MGL-PAX-3ADOCSTRING-20GENERIC-FUNCTION-29"></a>
- [generic-function] **DOCSTRING** *OBJECT*

    Return the docstring from the definition of `OBJECT`
    with [leading indentation stripped][718f]. This
    function serves a similar purpose as [`CL:DOCUMENTATION`][68f1], but it works
    with first-class objects when there is one for the corresponding
    definition, and with [`REFERENCE`][1cea]s when there is not. Its
    [reference delegate][e403] is [`LOCATE-DOCSTRING`][5c39].
    
    `DOCSTRING` is used in the implementation of the [`DOCSTRING`][ce75] locative.
    Some things such as [`ASDF:SYSTEM`][c097]s and `DECLARATION`([`0`][47a3] [`1`][6e04])s have no
    docstrings. Notably `SECTION`([`0`][5fac] [`1`][672f])s don't provide access to docstrings.

<a id="x-28MGL-PAX-3AFIND-SOURCE-20GENERIC-FUNCTION-29"></a>
- [generic-function] **FIND-SOURCE** *OBJECT*

    Return the Swank source location for `OBJECT`. It
    is called by `LOCATE-DEFINITIONS-FOR-EMACS`, which lies behind the
    `M-.` extension (see [Navigating Sources in Emacs][3386]). Its
    [reference delegate][e403] is [`LOCATE-AND-FIND-SOURCE`][d6a4].
    
    If successful, the return value should look like one of these:
    
    ```
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
    
    ```
    (:error "Unknown source location for SOMETHING")
    ```


<a id="x-28MGL-PAX-3AEXPORTABLE-REFERENCE-P-20GENERIC-FUNCTION-29"></a>
- [generic-function] **EXPORTABLE-REFERENCE-P** *PACKAGE SYMBOL LOCATIVE-TYPE LOCATIVE-ARGS*

    Return true iff `SYMBOL` is to be exported from
    `PACKAGE` when it occurs in a [`DEFSECTION`][72b4] as a reference with
    `LOCATIVE-TYPE` and `LOCATIVE-ARGS`. `SYMBOL` is [accessible][e077]
    in `PACKAGE`.
    
    The default method calls [`EXPORTABLE-LOCATIVE-TYPE-P`][c930] with
    `LOCATIVE-TYPE` and ignores the other arguments.
    
    By default, `SECTION`([`0`][5fac] [`1`][672f])s and `GLOSSARY-TERM`([`0`][8251] [`1`][5119])s are not exported although
    they are `EXPORTABLE-LOCATIVE-TYPE-P`. To export symbols naming
    section from [`MGL-PAX`][6fdb], the following method could be added:
    
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
    `SECTION`([`0`][5fac] [`1`][672f]), `GLOSSARY-TERM`([`0`][8251] [`1`][5119]), `PACKAGE`([`0`][4dd7] [`1`][5fb9]), [`ASDF:SYSTEM`][c097], `METHOD`([`0`][172e] [`1`][6831]) and [`INCLUDE`][5cd7]
    return `NIL`.
    
    This function is called by the default method of
    [`EXPORTABLE-REFERENCE-P`][e51f] to decide what symbols `DEFSECTION` shall
    export when its `EXPORT` argument is true.

<a id="x-28MGL-PAX-3A-40REFERENCE-BASED-EXTENSIONS-20MGL-PAX-3ASECTION-29"></a>
### 11.2 Reference Based Extensions

Let's see how to extend [`DOCUMENT`][432c] and `M-.` navigation if there
is no first-class object to represent the definition of interest.
Recall that [`LOCATE`][ee94] returns a [`REFERENCE`][1cea] object in this case. The
generic functions that we have specialized in [Adding New Object Types][bbf2] have
[reference delegate][e403]s, which can be specialized based on
[`LOCATIVE-TYPE`][3200]. Here is how the [`VARIABLE`][6c83] locative is defined:

<a id="x-28MGL-PAX-3AVARIABLE-EXAMPLE-20-28MGL-PAX-3AINCLUDE-20-28-3ASTART-20-28VARIABLE-20MGL-PAX-3ALOCATIVE-29-20-3AEND-20-28MGL-PAX-3A-3AEND-OF-VARIABLE-EXAMPLE-20VARIABLE-29-29-20-3AHEADER-NL-20-22-60-60-60-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29"></a>
```
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
  `\\#<MY-CLASS {100171ED93}>`.""")

(defmethod locate-object (symbol (locative-type (eql 'variable)) locative-args)
  (unless (<= (length locative-args) 1)
    (locate-error "The lambda list of the VARIABLE locative is ~
                   (&OPTIONAL INITFORM)."))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'variable))
                                locative-args stream)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (let ((arglist (multiple-value-bind (value unboundp)
                       (symbol-global-value symbol)
                     (when (or initformp (not unboundp))
                       (let ((*print-pretty* t))
                         (prin1-to-markdown (if initformp
                                                initform
                                                value)))))))
      (documenting-reference (stream :arglist arglist)
        (document-docstring (documentation* symbol 'variable) stream)))))

(defmethod locate-docstring (symbol (locative-type (eql 'variable))
                             locative-args)
  (declare (ignore locative-args))
  (documentation* symbol 'variable))

(defmethod locate-and-find-source (symbol (locative-type (eql 'variable))
                                   locative-args)
  (declare (ignore locative-args))
  (find-definition symbol 'variable))

```

<a id="x-28MGL-PAX-3A-40REFERENCE-DELEGATE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **reference delegate**

    [`CANONICAL-REFERENCE`][32f5], [`COLLECT-REACHABLE-OBJECTS`][8c95], [`DOCUMENT-OBJECT`][bacc],
    [`DOCSTRING`][e2bb], and [`FIND-SOURCE`][4355] delegate dealing with
    [`REFERENCES`][1cea] to another generic function, one each, which is called
    their reference delegate. Each of these delegator functions invokes
    its delegate when a `REFERENCE` is passed to it (as its `OBJECT`
    argument), or there is no method specialized for its arguments, in
    which case it uses the `CANONICAL-REFERENCE`.
    
    The net effect is that is that it is sufficient to specialize either
    the delegator for a first-class object or the delegate for a new
    locative type.

<a id="x-28MGL-PAX-3ALOCATE-CANONICAL-REFERENCE-20GENERIC-FUNCTION-29"></a>
- [generic-function] **LOCATE-CANONICAL-REFERENCE** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    This is the [reference delegate][e403] of
    [`CANONICAL-REFERENCE`][32f5]. The default method calls [`LOCATE-OBJECT`][185d] with the
    three arguments. If `LOCATE-OBJECT` returns a [`REFERENCE`][1cea], then that's
    taken to be the canonical reference and is returned, else
    `CANONICAL-REFERENCE` is invoked with the returned object.

<a id="x-28MGL-PAX-3ALOCATE-AND-COLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29"></a>
- [generic-function] **LOCATE-AND-COLLECT-REACHABLE-OBJECTS** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    This is the [reference delegate][e403] of
    [`COLLECT-REACHABLE-OBJECTS`][8c95].

<a id="x-28MGL-PAX-3ALOCATE-AND-DOCUMENT-20GENERIC-FUNCTION-29"></a>
- [generic-function] **LOCATE-AND-DOCUMENT** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS STREAM*

    This is the [reference delegate][e403] of [`DOCUMENT`][432c].

<a id="x-28MGL-PAX-3ALOCATE-DOCSTRING-20GENERIC-FUNCTION-29"></a>
- [generic-function] **LOCATE-DOCSTRING** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    This is the [reference delegate][e403] of [`DOCSTRING`][e2bb].

<a id="x-28MGL-PAX-3ALOCATE-AND-FIND-SOURCE-20GENERIC-FUNCTION-29"></a>
- [generic-function] **LOCATE-AND-FIND-SOURCE** *OBJECT LOCATIVE-TYPE LOCATIVE-ARGS*

    This is the [reference delegate][e403] of [`FIND-SOURCE`][4355].

We have covered the basic building blocks of reference based
extensions. Now let's see how the obscure
[`DEFINE-SYMBOL-LOCATIVE-TYPE`][7584] and
[`DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE`][a85e] macros work together to
simplify the common task of associating definition and documentation
with symbols in a certain context.

<a id="x-28MGL-PAX-3ADEFINE-SYMBOL-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>
- [macro] **DEFINE-SYMBOL-LOCATIVE-TYPE** *LOCATIVE-TYPE LAMBDA-LIST &BODY DOCSTRING*

    Similar to [`DEFINE-LOCATIVE-TYPE`][660b] but it assumes that all things
    locatable with `LOCATIVE-TYPE` are going to be just symbols defined
    with a definer defined with [`DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE`][a85e].
    It is useful to attach documentation and source location to symbols
    in a particular context. An example will make everything clear:
    
    ```
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
- [macro] **DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE** *NAME LOCATIVE-TYPE &BODY DOCSTRING*

    Define a macro with `NAME` which can be used to attach documentation,
    a lambda-list and source location to a symbol in the context of
    `LOCATIVE-TYPE`. The defined macro's arglist is ([`SYMBOL`][7f9f] `LAMBDA-LIST`
    `&OPTIONAL` `DOCSTRING`). `LOCATIVE-TYPE` is assumed to have been defined
    with [`DEFINE-SYMBOL-LOCATIVE-TYPE`][7584].

<a id="x-28MGL-PAX-3A-40EXTENDING-DOCUMENT-20MGL-PAX-3ASECTION-29"></a>
### 11.3 Extending `DOCUMENT`

The following utilities are for writing new [`DOCUMENT-OBJECT`][bacc] and
[`LOCATE-AND-DOCUMENT`][6611] methods, which emit markdown.

<a id="x-28MGL-PAX-3A-2AFORMAT-2A-20VARIABLE-29"></a>
- [variable] **\*FORMAT\***

    Bound by [`DOCUMENT`][432c], this allows markdown output to depend on the
    output format.

<a id="x-28MGL-PAX-3AWITH-HEADING-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-HEADING** *(STREAM OBJECT TITLE &KEY LINK-TITLE-TO) &BODY BODY*

    Write a markdown heading with `TITLE` to `STREAM`. Nested `WITH-HEADING`s
    produce nested headings. If [`*DOCUMENT-LINK-SECTIONS*`][1b28], generate
    anchors based on the [`CANONICAL-REFERENCE`][32f5] of `OBJECT`. `LINK-TITLE-TO`
    behaves like the `LINK-TITLE-TO` argument of [`DEFSECTION`][72b4].

<a id="x-28MGL-PAX-3ADOCUMENTING-REFERENCE-20MGL-PAX-3AMACRO-29"></a>
- [macro] **DOCUMENTING-REFERENCE** *(STREAM &KEY REFERENCE ARGLIST NAME) &BODY BODY*

    Write `REFERENCE` to `STREAM` as described in
    [`*DOCUMENT-MARK-UP-SIGNATURES*`][8fb6], and establish `REFERENCE` as a [local
    reference][4c96]. Unless `ARGLIST` is `NIL` or
    `:NOT-AVAILABLE`, it is printed after the name of object of `REFERENCE`.
    
    If `ARGLIST` is a list, then it is must be a [lambda list][f945] and
    is printed without the outermost parens and with the package names
    removed from the argument names.
    
    If `ARGLIST` is a string, then it is printed without [`ESCAPE-MARKDOWN`][3026].

<a id="x-28MGL-PAX-3AWITH-DISLOCATED-OBJECTS-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-DISLOCATED-OBJECTS** *OBJECTS &BODY BODY*

    For each object in `OBJECTS`, establish a [local
    reference][4c96] with the [`DISLOCATED`][e391] locative, which
    [prevents autolinking][8c16].

<a id="x-28MGL-PAX-3ADOCUMENT-DOCSTRING-20FUNCTION-29"></a>
- [function] **DOCUMENT-DOCSTRING** *DOCSTRING STREAM &KEY (INDENTATION "    ") EXCLUDE-FIRST-LINE-P (PARAGRAPHP T)*

    Process and `DOCSTRING` to `STREAM`, [stripping
    indentation][718f] from it, performing
    [Codification][f1ab] and [Linking to Code][1865], finally prefixing each line with
    `INDENTATION`. The prefix is not added to the first line if
    `EXCLUDE-FIRST-LINE-P`. If `PARAGRAPHP`, then add a newline before and
    after the output.

<a id="x-28MGL-PAX-3ADOCUMENTATION-2A-20FUNCTION-29"></a>
- [function] **DOCUMENTATION\*** *OBJECT DOC-TYPE*

    A small wrapper around [`CL:DOCUMENTATION`][68f1] to smooth over differences
    between implementations.

<a id="x-28MGL-PAX-3AESCAPE-MARKDOWN-20FUNCTION-29"></a>
- [function] **ESCAPE-MARKDOWN** *STRING*

    Construct a new string from `STRING` by adding a backslash before
    each special markdown character:
    
        *_`<>[]


<a id="x-28MGL-PAX-3APRIN1-TO-MARKDOWN-20FUNCTION-29"></a>
- [function] **PRIN1-TO-MARKDOWN** *OBJECT*

    Like [`PRIN1-TO-STRING`][8bee], but bind [`*PRINT-CASE*`][a39d] depending on
    [`*DOCUMENT-DOWNCASE-UPPERCASE-CODE*`][a5ee] and [`*FORMAT*`][3da8], and
    [`ESCAPE-MARKDOWN`][3026].

<a id="x-28MGL-PAX-3A-40EXTENDING-FIND-SOURCE-20MGL-PAX-3ASECTION-29"></a>
### 11.4 Extending `FIND-SOURCE`

The following utilities are for writing new [`FIND-SOURCE`][4355] and
[`LOCATE-AND-FIND-SOURCE`][d6a4] methods. Their locative arguments are
translated to Swank `dspecs`, and it is an error if there is no
translation. In general, Swank supports Common Lisp
definitions (hence the [`VARIABLE`][6c83] and `FUNCTION`([`0`][ba62] [`1`][2d97] [`2`][3de5]) locatives, for example)
but not PAX- and user-defined additions (e.g. `SECTION`([`0`][5fac] [`1`][672f]),
[`ASDF:SYSTEM`][c097]).

<a id="x-28MGL-PAX-3AFIND-DEFINITION-20FUNCTION-29"></a>
- [function] **FIND-DEFINITION** *OBJECT &REST LOCATIVES*

    Return a Swank source location for a definition of `OBJECT`. Try
    forming [reference][80cd]s with `OBJECT` and one of `LOCATIVES`. Stop at the
    first locative with which a definition is found, and return its
    location. If no location was found, then return the usual
    Swank `(:ERROR ...)`. The implementation is based on the rather
    expensive `SWANK-BACKEND:FIND-DEFINITIONS` function.

<a id="x-28MGL-PAX-3AFIND-DEFINITION-2A-20FUNCTION-29"></a>
- [function] **FIND-DEFINITION\*** *OBJECT REFERENCE-OBJECT &REST LOCATIVES*

    Like [`FIND-DEFINITION`][ff68], but tries to get the definition of
    `OBJECT` (for example a `FUNCTION`([`0`][ba62] [`1`][2d97] [`2`][3de5]) or `METHOD`([`0`][172e] [`1`][6831]) object) with the fast but
    not widely supported `SWANK-BACKEND:FIND-SOURCE-LOCATION` before
    calling the much slower but more complete
    `SWANK-BACKEND:FIND-DEFINITIONS`.

<a id="x-28MGL-PAX-3A-40SECTIONS-20MGL-PAX-3ASECTION-29"></a>
### 11.5 Sections

[`SECTION`][5fac] objects rarely need to be dissected since
[`DEFSECTION`][72b4] and [`DOCUMENT`][432c] cover most needs. However, it is plausible
that one wants to subclass them and maybe redefine how they are
presented.

<a id="x-28MGL-PAX-3ASECTION-20CLASS-29"></a>
- [class] **SECTION**

    [`DEFSECTION`][72b4] stores its `NAME`, `TITLE`, [`PACKAGE`][5fb9],
    [`READTABLE`][248b] and `ENTRIES` arguments in [`SECTION`][5fac]
    objects.

<a id="x-28MGL-PAX-3ASECTION-NAME-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
- [reader] **SECTION-NAME** *SECTION (:NAME)*

    The name of the global variable whose value is
    this `SECTION`([`0`][5fac] [`1`][672f]) object.

<a id="x-28MGL-PAX-3ASECTION-PACKAGE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
- [reader] **SECTION-PACKAGE** *SECTION (:PACKAGE)*

    [`*PACKAGE*`][d2c1] will be bound to this package when
    generating documentation for this section if
    [`*DOCUMENT-NORMALIZE-PACKAGES*`][440e].

<a id="x-28MGL-PAX-3ASECTION-READTABLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
- [reader] **SECTION-READTABLE** *SECTION (:READTABLE)*

    [`*READTABLE*`][a916] will be bound to this when generating
    documentation for this section if [`*DOCUMENT-NORMALIZE-PACKAGES*`][440e].

<a id="x-28MGL-PAX-3ASECTION-TITLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
- [reader] **SECTION-TITLE** *SECTION (:TITLE)*

    A markdown string or `NIL`. Used in generated
    documentation.

<a id="x-28MGL-PAX-3ASECTION-LINK-TITLE-TO-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
- [reader] **SECTION-LINK-TITLE-TO** *SECTION (:LINK-TITLE-TO = NIL)*

    A [`REFERENCE`][1cea] or `NIL`. Used in generated documentation.

<a id="x-28MGL-PAX-3ASECTION-ENTRIES-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>
- [reader] **SECTION-ENTRIES** *SECTION (:ENTRIES)*

    A list of markdown docstrings and [`REFERENCE`][1cea]
    objects in the order they occurred in [`DEFSECTION`][72b4].

<a id="x-28MGL-PAX-3A-40GLOSSARY-TERMS-20MGL-PAX-3ASECTION-29"></a>
### 11.6 Glossary Terms

[`GLOSSARY-TERM`][8251] objects rarely need to be dissected since
[`DEFINE-GLOSSARY-TERM`][8ece] and [`DOCUMENT`][432c] cover most needs. However, it is
plausible that one wants to subclass them and maybe redefine how
they are presented.

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-20CLASS-29"></a>
- [class] **GLOSSARY-TERM**

    [`DEFINE-GLOSSARY-TERM`][8ece] instantiates a `GLOSSARY-TERM`
    with its `NAME` and `TITLE` arguments.

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-NAME-20-28MGL-PAX-3AREADER-20MGL-PAX-3AGLOSSARY-TERM-29-29"></a>
- [reader] **GLOSSARY-TERM-NAME** *GLOSSARY-TERM (:NAME)*

    The name of the global variable whose value is
    this `GLOSSARY-TERM`([`0`][8251] [`1`][5119]) object.

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-TITLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3AGLOSSARY-TERM-29-29"></a>
- [reader] **GLOSSARY-TERM-TITLE** *GLOSSARY-TERM (:TITLE)*

    A markdown string or `NIL`. Used in generated
    documentation.

  [0019]: #x-28MGL-PAX-3ALOCATE-ERROR-20FUNCTION-29 "MGL-PAX:LOCATE-ERROR FUNCTION"
  [00d4]: #x-28MGL-PAX-3AACCESSOR-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:ACCESSOR MGL-PAX:LOCATIVE"
  [0225]: #x-28MGL-PAX-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28STRING-20T-29-29-29 "MGL-PAX:DOCUMENT-OBJECT (METHOD NIL (STRING T))"
  [02de]: #x-28MGL-PAX-3AREFERENCE-LOCATIVE-20-28MGL-PAX-3AREADER-20MGL-PAX-3AREFERENCE-29-29 "MGL-PAX:REFERENCE-LOCATIVE (MGL-PAX:READER MGL-PAX:REFERENCE)"
  [06a9]: #x-28MGL-PAX-3A-40CONDITION-SYSTEM-LOCATIVES-20MGL-PAX-3ASECTION-29 "Condition System Locatives"
  [08f7]: http://www.lispworks.com/documentation/HyperSpec/Body/v_t.htm "T MGL-PAX:CONSTANT"
  [0b3a]: #x-28MGL-PAX-3ALOCATIVE-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:LOCATIVE MGL-PAX:LOCATIVE"
  [0ef0]: #x-28MGL-PAX-3A-2ADOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS* VARIABLE"
  [1102]: http://www.lispworks.com/documentation/HyperSpec/Body/f_abortc.htm "ABORT FUNCTION"
  [117a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm "OPEN FUNCTION"
  [1281]: #x-28MGL-PAX-3A-40PAX-WORLD-20MGL-PAX-3ASECTION-29 "PAX World"
  [13a9]: #x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-READMES-20FUNCTION-29 "MGL-PAX:UPDATE-ASDF-SYSTEM-READMES FUNCTION"
  [172e]: #x-28METHOD-20MGL-PAX-3ALOCATIVE-29 "METHOD MGL-PAX:LOCATIVE"
  [17e0]: #x-28MGL-PAX-3A-2ADOCUMENT-URL-VERSIONS-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-URL-VERSIONS* VARIABLE"
  [185d]: #x-28MGL-PAX-3ALOCATE-OBJECT-20GENERIC-FUNCTION-29 "MGL-PAX:LOCATE-OBJECT GENERIC-FUNCTION"
  [1864]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*STANDARD-OUTPUT* VARIABLE"
  [1865]: #x-28MGL-PAX-3A-40LINKING-TO-CODE-20MGL-PAX-3ASECTION-29 "Linking to Code"
  [1895]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR CONDITION"
  [1aee]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRIN1 FUNCTION"
  [1b1b]: #x-28MGL-PAX-3A-40DOCUMENTATION-UTILITIES-20MGL-PAX-3ASECTION-29 "Utilities for Generating Documentation"
  [1b28]: #x-28MGL-PAX-3A-2ADOCUMENT-LINK-SECTIONS-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-LINK-SECTIONS* VARIABLE"
  [1cea]: #x-28MGL-PAX-3AREFERENCE-20CLASS-29 "MGL-PAX:REFERENCE CLASS"
  [1f28]: http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm "FORMAT FUNCTION"
  [2060]: #x-28CLASS-20MGL-PAX-3ALOCATIVE-29 "CLASS MGL-PAX:LOCATIVE"
  [21f5]: #x-28MGL-PAX-3A-40MACROLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for Macros"
  [22c2]: #x-28MGL-PAX-3A-40LINKING-TO-SECTIONS-20MGL-PAX-3ASECTION-29 "Linking to Sections"
  [238c]: #x-28MGL-PAX-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29 "MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR CONDITION"
  [241f]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pn.htm "PATHNAME TYPE"
  [248b]: http://www.lispworks.com/documentation/HyperSpec/Body/t_rdtabl.htm "READTABLE TYPE"
  [2634]: #x-28MGL-PAX-3A-40OVERVIEW-OF-ESCAPING-20MGL-PAX-3ASECTION-29 "Overview of Escaping"
  [2645]: #x-28MGL-PAX-3A-40AMBIGUOUS-LOCATIVE-20MGL-PAX-3ASECTION-29 "Ambiguous Unspecified Locative"
  [26cf]: http://www.lispworks.com/documentation/HyperSpec/Body/t_t.htm "T TYPE"
  [292a]: #x-28MGL-PAX-3A-40PAX-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for PAX Constructs"
  [2c93]: #x-28MGL-PAX-3A-40GENERATING-DOCUMENTATION-20MGL-PAX-3ASECTION-29 "Generating Documentation"
  [2ca9]: #x-28MGL-PAX-3AOUTPUT-REFLINK-20FUNCTION-29 "MGL-PAX:OUTPUT-REFLINK FUNCTION"
  [2ce2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_consta.htm "CONSTANTP FUNCTION"
  [2d97]: http://www.lispworks.com/documentation/HyperSpec/Body/s_fn.htm "FUNCTION MGL-PAX:MACRO"
  [2d9d]: #x-28MGL-PAX-3ADEFINE-RESTART-20MGL-PAX-3AMACRO-29 "MGL-PAX:DEFINE-RESTART MGL-PAX:MACRO"
  [3026]: #x-28MGL-PAX-3AESCAPE-MARKDOWN-20FUNCTION-29 "MGL-PAX:ESCAPE-MARKDOWN FUNCTION"
  [3200]: #x-28MGL-PAX-3ALOCATIVE-TYPE-20FUNCTION-29 "MGL-PAX:LOCATIVE-TYPE FUNCTION"
  [32f5]: #x-28MGL-PAX-3ACANONICAL-REFERENCE-20GENERIC-FUNCTION-29 "MGL-PAX:CANONICAL-REFERENCE GENERIC-FUNCTION"
  [3386]: #x-28MGL-PAX-3A-40NAVIGATING-IN-EMACS-20MGL-PAX-3ASECTION-29 "Navigating Sources in Emacs"
  [36e1]: #x-28MGL-PAX-3A-40HTML-OUTPUT-20MGL-PAX-3ASECTION-29 "HTML Output"
  [378f]: #x-28MGL-PAX-3A-40PARSING-20MGL-PAX-3ASECTION-29 "Parsing"
  [3ae8]: http://www.lispworks.com/documentation/HyperSpec/Body/r_contin.htm "CONTINUE RESTART"
  [3d3c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm "READ FUNCTION"
  [3da8]: #x-28MGL-PAX-3A-2AFORMAT-2A-20VARIABLE-29 "MGL-PAX:*FORMAT* VARIABLE"
  [3de5]: http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm "FUNCTION TYPE"
  [3eb4]: #x-28MGL-PAX-3A-40EXTENDING-FIND-SOURCE-20MGL-PAX-3ASECTION-29 "Extending `FIND-SOURCE`"
  [3f15]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rest.htm "REST FUNCTION"
  [41fd]: #x-28COMPILER-MACRO-20MGL-PAX-3ALOCATIVE-29 "COMPILER-MACRO MGL-PAX:LOCATIVE"
  [4267]: http://www.lispworks.com/documentation/HyperSpec/Body/t_string.htm "STRING TYPE"
  [42d7]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm "DEFPACKAGE MGL-PAX:MACRO"
  [432c]: #x-28MGL-PAX-3ADOCUMENT-20FUNCTION-29 "MGL-PAX:DOCUMENT FUNCTION"
  [4355]: #x-28MGL-PAX-3AFIND-SOURCE-20GENERIC-FUNCTION-29 "MGL-PAX:FIND-SOURCE GENERIC-FUNCTION"
  [440e]: #x-28MGL-PAX-3A-2ADOCUMENT-NORMALIZE-PACKAGES-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-NORMALIZE-PACKAGES* VARIABLE"
  [46ec]: #x-28MGL-PAX-3ALOCATE-AND-COLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29 "MGL-PAX:LOCATE-AND-COLLECT-REACHABLE-OBJECTS GENERIC-FUNCTION"
  [47a3]: http://www.lispworks.com/documentation/HyperSpec/Body/d_declar.htm "DECLARATION DECLARATION"
  [493e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_smp_cn.htm "SIMPLE-CONDITION-FORMAT-CONTROL FUNCTION"
  [4b78]: #x-28MGL-PAX-3A-40EXTERNAL-LOCATIVES-20MGL-PAX-3ASECTION-29 "External Locatives"
  [4bb8]: #x-28-22mgl-pax-2Fdocument-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"mgl-pax/document" ASDF/SYSTEM:SYSTEM'
  [4c48]: #x-28MGL-PAX-3A-40VARIABLELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for Variables"
  [4c5e]: #x-28MGL-PAX-3A-40FILTERING-AMBIGUOUS-REFERENCES-20MGL-PAX-3ASECTION-29 "Filtering Ambiguous References"
  [4c96]: #x-28MGL-PAX-3A-40LOCAL-REFERENCES-20MGL-PAX-3ASECTION-29 "Local References"
  [4d92]: #x-28MGL-PAX-3A-40LOCATIVE-20MGL-PAX-3AGLOSSARY-TERM-29 "MGL-PAX:@LOCATIVE MGL-PAX:GLOSSARY-TERM"
  [4dd7]: #x-28PACKAGE-20MGL-PAX-3ALOCATIVE-29 "PACKAGE MGL-PAX:LOCATIVE"
  [5119]: #x-28MGL-PAX-3AGLOSSARY-TERM-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:GLOSSARY-TERM MGL-PAX:LOCATIVE"
  [574a]: #x-28MGL-PAX-3A-40EXTENDING-DOCUMENT-20MGL-PAX-3ASECTION-29 "Extending `DOCUMENT`"
  [5800]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq_sle.htm "< FUNCTION"
  [5825]: #x-28-22mgl-pax-2Ftranscribe-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"mgl-pax/transcribe" ASDF/SYSTEM:SYSTEM'
  [5875]: #x-28GENERIC-FUNCTION-20MGL-PAX-3ALOCATIVE-29 "GENERIC-FUNCTION MGL-PAX:LOCATIVE"
  [587f]: #x-28MGL-PAX-3AMAKE-GIT-SOURCE-URI-FN-20FUNCTION-29 "MGL-PAX:MAKE-GIT-SOURCE-URI-FN FUNCTION"
  [5c39]: #x-28MGL-PAX-3ALOCATE-DOCSTRING-20GENERIC-FUNCTION-29 "MGL-PAX:LOCATE-DOCSTRING GENERIC-FUNCTION"
  [5c74]: #x-28MGL-PAX-3A-40UNAMBIGUOUS-LOCATIVE-20MGL-PAX-3ASECTION-29 "Unambiguous Unspecified Locative"
  [5cd7]: #x-28MGL-PAX-3AINCLUDE-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:INCLUDE MGL-PAX:LOCATIVE"
  [5fac]: #x-28MGL-PAX-3ASECTION-20CLASS-29 "MGL-PAX:SECTION CLASS"
  [5fb9]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm "PACKAGE TYPE"
  [6121]: #x-28MGL-PAX-3A-40LOCATIVE-TYPES-20MGL-PAX-3ASECTION-29 "Locative Types"
  [6300]: #x-28MGL-PAX-3A-40TRANSCRIPTS-20MGL-PAX-3ASECTION-29 "Transcripts"
  [63f3]: #x-28MGL-PAX-3ADEFINE-PACKAGE-20MGL-PAX-3AMACRO-29 "MGL-PAX:DEFINE-PACKAGE MGL-PAX:MACRO"
  [660b]: #x-28MGL-PAX-3ADEFINE-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29 "MGL-PAX:DEFINE-LOCATIVE-TYPE MGL-PAX:MACRO"
  [6611]: #x-28MGL-PAX-3ALOCATE-AND-DOCUMENT-20GENERIC-FUNCTION-29 "MGL-PAX:LOCATE-AND-DOCUMENT GENERIC-FUNCTION"
  [672f]: #x-28MGL-PAX-3ASECTION-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:SECTION MGL-PAX:LOCATIVE"
  [6786]: http://www.lispworks.com/documentation/HyperSpec/Issues/iss009_w.htm '"ISSUE:AREF-1D" MGL-PAX:CLHS'
  [6831]: http://www.lispworks.com/documentation/HyperSpec/Body/t_method.htm "METHOD TYPE"
  [6887]: #x-28MGL-PAX-3ALOCATE-ERROR-20CONDITION-29 "MGL-PAX:LOCATE-ERROR CONDITION"
  [68f1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_docume.htm "DOCUMENTATION GENERIC-FUNCTION"
  [69b7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_cerror.htm "CERROR FUNCTION"
  [69f7]: #x-28MGL-PAX-3A-40REFERENCE-BASED-EXTENSIONS-20MGL-PAX-3ASECTION-29 "Reference Based Extensions"
  [6b59]: #x-28MGL-PAX-3A-40TRANSCRIPT-DYNENV-20MGL-PAX-3ASECTION-29 "Controlling the Dynamic Environment"
  [6c1f]: http://www.lispworks.com/documentation/HyperSpec/Body/e_smp_cn.htm "SIMPLE-CONDITION CONDITION"
  [6c83]: #x-28VARIABLE-20MGL-PAX-3ALOCATIVE-29 "VARIABLE MGL-PAX:LOCATIVE"
  [6dd5]: http://www.lispworks.com/documentation/HyperSpec/Body/f_abortc.htm "MUFFLE-WARNING FUNCTION"
  [6e04]: #x-28DECLARATION-20MGL-PAX-3ALOCATIVE-29 "DECLARATION MGL-PAX:LOCATIVE"
  [6e18]: #x-28MGL-PAX-3A-40TRANSCRIPT-FINER-GRAINED-CONSISTENCY-CHECKS-20MGL-PAX-3ASECTION-29 "Finer-Grained Consistency Checks"
  [6fdb]: #x-28-22mgl-pax-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"mgl-pax" ASDF/SYSTEM:SYSTEM'
  [718f]: #x-28MGL-PAX-3A-40MARKDOWN-INDENTATION-20MGL-PAX-3ASECTION-29 "Indentation"
  [727b]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_lev.htm "*PRINT-LENGTH* VARIABLE"
  [72b4]: #x-28MGL-PAX-3ADEFSECTION-20MGL-PAX-3AMACRO-29 "MGL-PAX:DEFSECTION MGL-PAX:MACRO"
  [72fd]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_1.htm "DEFINE-SYMBOL-MACRO MGL-PAX:MACRO"
  [730f]: #x-28MGL-PAX-3A-2ADISCARD-DOCUMENTATION-P-2A-20VARIABLE-29 "MGL-PAX:*DISCARD-DOCUMENTATION-P* VARIABLE"
  [7445]: #x-28MGL-PAX-3A-40INTERESTING-20MGL-PAX-3AGLOSSARY-TERM-29 "MGL-PAX:@INTERESTING MGL-PAX:GLOSSARY-TERM"
  [7584]: #x-28MGL-PAX-3ADEFINE-SYMBOL-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29 "MGL-PAX:DEFINE-SYMBOL-LOCATIVE-TYPE MGL-PAX:MACRO"
  [75ce]: #x-28MGL-PAX-3A-40OBJECT-20MGL-PAX-3AGLOSSARY-TERM-29 "MGL-PAX:@OBJECT MGL-PAX:GLOSSARY-TERM"
  [7b2c]: #x-28MGL-PAX-3ALOCATE-ERROR-OBJECT-20-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29 "MGL-PAX:LOCATE-ERROR-OBJECT (MGL-PAX:READER MGL-PAX:LOCATE-ERROR)"
  [7bd4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_string.htm "STRING FUNCTION"
  [7c82]: #x-28MGL-PAX-3A-40MISCELLANEOUS-DOCUMENTATION-PRINTER-VARIABLES-20MGL-PAX-3ASECTION-29 "Miscellaneous Variables"
  [7cc3]: #x-28MGL-PAX-3A-40LINKING-TO-THE-HYPERSPEC-20MGL-PAX-3ASECTION-29 "Linking to the Hyperspec"
  [7da5]: #x-28MGL-PAX-3ALOCATE-ERROR-MESSAGE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29 "MGL-PAX:LOCATE-ERROR-MESSAGE (MGL-PAX:READER MGL-PAX:LOCATE-ERROR)"
  [7e58]: http://www.lispworks.com/documentation/HyperSpec/Body/t_class.htm "CLASS CLASS"
  [7f37]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*ERROR-OUTPUT* VARIABLE"
  [7f9f]: http://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm "SYMBOL TYPE"
  [804d]: http://www.lispworks.com/documentation/HyperSpec/Body/m_declai.htm "DECLAIM MGL-PAX:MACRO"
  [80cd]: #x-28MGL-PAX-3A-40REFERENCE-20MGL-PAX-3AGLOSSARY-TERM-29 "MGL-PAX:@REFERENCE MGL-PAX:GLOSSARY-TERM"
  [8251]: #x-28MGL-PAX-3AGLOSSARY-TERM-20CLASS-29 "MGL-PAX:GLOSSARY-TERM CLASS"
  [8263]: http://www.lispworks.com/documentation/HyperSpec/Body/r_muffle.htm "MUFFLE-WARNING RESTART"
  [82e0]: #x-28METHOD-COMBINATION-20MGL-PAX-3ALOCATIVE-29 "METHOD-COMBINATION MGL-PAX:LOCATIVE"
  [8423]: #x-28MGL-PAX-3A-40TRANSCRIPT-UTILITIES-FOR-CONSISTENCY-CHECKING-20MGL-PAX-3ASECTION-29 "Utilities for Consistency Checking"
  [8492]: #x-28MGL-PAX-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29 "MGL-PAX:TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR CONDITION"
  [8710]: #x-28MGL-PAX-3AARGUMENT-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:ARGUMENT MGL-PAX:LOCATIVE"
  [875e]: #x-28MGL-PAX-3A-2ADOCUMENT-LINK-TO-HYPERSPEC-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-LINK-TO-HYPERSPEC* VARIABLE"
  [889e]: http://www.lispworks.com/documentation/HyperSpec/Body/v_sl_sls.htm "/// VARIABLE"
  [88a7]: #x-28MGL-PAX-3ASECTION-READTABLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29 "MGL-PAX:SECTION-READTABLE (MGL-PAX:READER MGL-PAX:SECTION)"
  [88cf]: #x-28MGL-PAX-3A-40NAME-20MGL-PAX-3AGLOSSARY-TERM-29 "MGL-PAX:@NAME MGL-PAX:GLOSSARY-TERM"
  [8996]: #x-28MGL-PAX-3A-40SPECIFIED-LOCATIVE-20MGL-PAX-3ASECTION-29 "Specified Locative"
  [89d0]: http://www.lispworks.com/documentation/HyperSpec/Body/m_deftp.htm "DEFTYPE MGL-PAX:MACRO"
  [8a58]: #x-28MGL-PAX-3A-40SECTIONS-20MGL-PAX-3ASECTION-29 "Sections"
  [8beb]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_m.htm "FIND-METHOD GENERIC-FUNCTION"
  [8bee]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_to_.htm "PRIN1-TO-STRING FUNCTION"
  [8c16]: #x-28MGL-PAX-3A-40PREVENTING-AUTOLINKING-20MGL-PAX-3ASECTION-29 "Preventing Autolinking"
  [8c3e]: #x-28MGL-PAX-3A-40TUTORIAL-20MGL-PAX-3ASECTION-29 "Tutorial"
  [8c40]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defgen.htm "DEFGENERIC MGL-PAX:MACRO"
  [8c7d]: #x-28MGL-PAX-3AREFERENCE-OBJECT-20-28MGL-PAX-3AREADER-20MGL-PAX-3AREFERENCE-29-29 "MGL-PAX:REFERENCE-OBJECT (MGL-PAX:READER MGL-PAX:REFERENCE)"
  [8c95]: #x-28MGL-PAX-3ACOLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29 "MGL-PAX:COLLECT-REACHABLE-OBJECTS GENERIC-FUNCTION"
  [8c99]: http://www.lispworks.com/documentation/HyperSpec/Body/f_car_c.htm "CAR FUNCTION"
  [8d65]: http://www.lispworks.com/documentation/HyperSpec/Body/t_generi.htm "GENERIC-FUNCTION CLASS"
  [8ece]: #x-28MGL-PAX-3ADEFINE-GLOSSARY-TERM-20MGL-PAX-3AMACRO-29 "MGL-PAX:DEFINE-GLOSSARY-TERM MGL-PAX:MACRO"
  [8fb6]: #x-28MGL-PAX-3A-2ADOCUMENT-MARK-UP-SIGNATURES-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-MARK-UP-SIGNATURES* VARIABLE"
  [91fd]: http://www.lispworks.com/documentation/HyperSpec/Body/f_stgeq_.htm "STRING= FUNCTION"
  [926d]: #x-28TYPE-20MGL-PAX-3ALOCATIVE-29 "TYPE MGL-PAX:LOCATIVE"
  [9450]: #x-28MGL-PAX-3ASECTION-ENTRIES-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29 "MGL-PAX:SECTION-ENTRIES (MGL-PAX:READER MGL-PAX:SECTION)"
  [94c7]: #x-28MGL-PAX-3A-40BASICS-20MGL-PAX-3ASECTION-29 "Basics"
  [964b]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_3.htm "SYMBOL-PACKAGE FUNCTION"
  [9674]: http://www.lispworks.com/documentation/HyperSpec/Body/f_procla.htm "PROCLAIM FUNCTION"
  [96d0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm "EQUAL FUNCTION"
  [9717]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm "DEFUN MGL-PAX:MACRO"
  [9974]: #x-28MGL-PAX-3ALOCATE-ERROR-LOCATIVE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATE-ERROR-29-29 "MGL-PAX:LOCATE-ERROR-LOCATIVE (MGL-PAX:READER MGL-PAX:LOCATE-ERROR)"
  [9dbc]: #x-28MGL-PAX-3A-40TRANSCRIPT-API-20MGL-PAX-3ASECTION-29 "Transcript API"
  [a17d]: #x-28MGL-PAX-3A-40MATHJAX-20MGL-PAX-3ASECTION-29 "MathJax"
  [a249]: #x-28MGL-PAX-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29 "MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR CONDITION"
  [a328]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rdtabl.htm "READTABLE-CASE FUNCTION"
  [a39d]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_cas.htm "*PRINT-CASE* VARIABLE"
  [a5b1]: #x-28MGL-PAX-3ASECTION-PACKAGE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29 "MGL-PAX:SECTION-PACKAGE (MGL-PAX:READER MGL-PAX:SECTION)"
  [a5de]: http://www.lispworks.com/documentation/HyperSpec/Body/m_define.htm "DEFINE-COMPILER-MACRO MGL-PAX:MACRO"
  [a5ee]: #x-28MGL-PAX-3A-2ADOCUMENT-DOWNCASE-UPPERCASE-CODE-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-DOWNCASE-UPPERCASE-CODE* VARIABLE"
  [a668]: http://www.lispworks.com/documentation/HyperSpec/Body/f_smp_cn.htm "SIMPLE-CONDITION-FORMAT-ARGUMENTS FUNCTION"
  [a85e]: #x-28MGL-PAX-3ADEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29 "MGL-PAX:DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE MGL-PAX:MACRO"
  [a916]: http://www.lispworks.com/documentation/HyperSpec/Body/v_rdtabl.htm "*READTABLE* VARIABLE"
  [ad91]: http://www.lispworks.com/documentation/HyperSpec/Body/t_rst.htm "RESTART TYPE"
  [b372]: #x-28MGL-PAX-3A-40UNRESOLVABLE-REFLINKS-20MGL-PAX-3ASECTION-29 "Unresolvable Links"
  [b3cc]: #x-28MGL-PAX-3A-40EXPLICIT-AND-AUTOLINKING-20MGL-PAX-3ASECTION-29 "Explicit and Autolinking"
  [b89a]: #x-28MGL-PAX-3A-40CODIFIABLE-20MGL-PAX-3AGLOSSARY-TERM-29 "MGL-PAX:@CODIFIABLE MGL-PAX:GLOSSARY-TERM"
  [ba62]: #x-28FUNCTION-20MGL-PAX-3ALOCATIVE-29 "FUNCTION MGL-PAX:LOCATIVE"
  [ba74]: #x-28MGL-PAX-3A-40LINKS-20MGL-PAX-3ASECTION-29 "Links"
  [bacc]: #x-28MGL-PAX-3ADOCUMENT-OBJECT-20GENERIC-FUNCTION-29 "MGL-PAX:DOCUMENT-OBJECT GENERIC-FUNCTION"
  [bb77]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq_sle.htm "<= FUNCTION"
  [bbf2]: #x-28MGL-PAX-3A-40NEW-OBJECT-TYPES-20MGL-PAX-3ASECTION-29 "Adding New Object Types"
  [bc83]: #x-28MGL-PAX-3A-40MARKDOWN-SYNTAX-HIGHLIGHTING-20MGL-PAX-3ASECTION-29 "Syntax Highlighting"
  [bcd2]: http://www.lispworks.com/documentation/HyperSpec/Body/d_optimi.htm "DEBUG DECLARATION"
  [bdf2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_boundp.htm "BOUNDP FUNCTION"
  [be47]: #x-28MGL-PAX-3A-40TYPELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for Types and Declarations"
  [bf07]: http://www.lispworks.com/documentation/HyperSpec/Body/f_export.htm "EXPORT FUNCTION"
  [c097]: #x-28ASDF-2FSYSTEM-3ASYSTEM-20MGL-PAX-3ALOCATIVE-29 "ASDF/SYSTEM:SYSTEM MGL-PAX:LOCATIVE"
  [c1eb]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eval.htm "EVAL FUNCTION"
  [c267]: #x-28MGL-PAX-3ALOCATE-CANONICAL-REFERENCE-20GENERIC-FUNCTION-29 "MGL-PAX:LOCATE-CANONICAL-REFERENCE GENERIC-FUNCTION"
  [c2d3]: #x-28MGL-PAX-3A-40MARKDOWN-SUPPORT-20MGL-PAX-3ASECTION-29 "Markdown Support"
  [c35d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_intern.htm "INTERN FUNCTION"
  [c4ce]: #x-28MGL-PAX-3A-40EXTENSION-API-20MGL-PAX-3ASECTION-29 "Writing Extensions"
  [c818]: #x-28MGL-PAX-3AOUTPUT-LABEL-20FUNCTION-29 "MGL-PAX:OUTPUT-LABEL FUNCTION"
  [c819]: #x-28MGL-PAX-3ACONSTANT-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:CONSTANT MGL-PAX:LOCATIVE"
  [c930]: #x-28MGL-PAX-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29 "MGL-PAX:EXPORTABLE-LOCATIVE-TYPE-P GENERIC-FUNCTION"
  [cbf2]: http://www.lispworks.com/documentation/HyperSpec/Body/t_stream.htm "STREAM TYPE"
  [cc04]: #x-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:READER MGL-PAX:LOCATIVE"
  [cd9e]: #x-28MGL-PAX-3ARESOLVE-20FUNCTION-29 "MGL-PAX:RESOLVE FUNCTION"
  [ce02]: http://www.lispworks.com/documentation/HyperSpec/Body/s_declar.htm "DECLARE MGL-PAX:MACRO"
  [ce75]: #x-28MGL-PAX-3ADOCSTRING-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:DOCSTRING MGL-PAX:LOCATIVE"
  [cfbb]: http://www.lispworks.com/documentation/HyperSpec/Body/m_pr_unr.htm "PRINT-UNREADABLE-OBJECT MGL-PAX:MACRO"
  [d1ca]: #x-28MGL-PAX-3A-40DOCUMENT-IMPLEMENTATION-NOTES-20MGL-PAX-3ASECTION-29 "Document Generation Implementation Notes"
  [d1dc]: #x-28MGL-PAX-3A-40GLOSSARY-TERMS-20MGL-PAX-3ASECTION-29 "Glossary Terms"
  [d2c1]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pkg.htm "*PACKAGE* VARIABLE"
  [d684]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defcon.htm "DEFCONSTANT MGL-PAX:MACRO"
  [d6a4]: #x-28MGL-PAX-3ALOCATE-AND-FIND-SOURCE-20GENERIC-FUNCTION-29 "MGL-PAX:LOCATE-AND-FIND-SOURCE GENERIC-FUNCTION"
  [d761]: #x-28-22mgl-pax-2Ffull-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"mgl-pax/full" ASDF/SYSTEM:SYSTEM'
  [d7b0]: #x-28MGL-PAX-3A-40WORD-20MGL-PAX-3AGLOSSARY-TERM-29 "MGL-PAX:@WORD MGL-PAX:GLOSSARY-TERM"
  [d9ee]: #x-28MGL-PAX-3A-2ADOCUMENT-LINK-CODE-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-LINK-CODE* VARIABLE"
  [dc76]: http://www.lispworks.com/documentation/HyperSpec/Body/e_cnd.htm "CONDITION CONDITION"
  [dff6]: #x-28MGL-PAX-3A-40GITHUB-WORKFLOW-20MGL-PAX-3ASECTION-29 "Github Workflow"
  [e077]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_s.htm "FIND-SYMBOL FUNCTION"
  [e216]: #x-28MGL-PAX-3A-2ADOCUMENT-HTML-TOP-BLOCKS-OF-LINKS-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS* VARIABLE"
  [e248]: #x-28MGL-PAX-3A-40FUNCTIONLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for Functions"
  [e256]: http://www.lispworks.com/documentation/HyperSpec/Issues/iss009.htm '"SUMMARY:AREF-1D" MGL-PAX:CLHS'
  [e2bb]: #x-28MGL-PAX-3ADOCSTRING-20GENERIC-FUNCTION-29 "MGL-PAX:DOCSTRING GENERIC-FUNCTION"
  [e2e8]: #x-28MGL-PAX-3A-40SUPPRESSED-LINKS-20MGL-PAX-3ASECTION-29 "Suppressed Links"
  [e391]: #x-28MGL-PAX-3ADISLOCATED-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:DISLOCATED MGL-PAX:LOCATIVE"
  [e403]: #x-28MGL-PAX-3A-40REFERENCE-DELEGATE-20MGL-PAX-3AGLOSSARY-TERM-29 "MGL-PAX:@REFERENCE-DELEGATE MGL-PAX:GLOSSARY-TERM"
  [e4b0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ensu_1.htm "ENSURE-DIRECTORIES-EXIST FUNCTION"
  [e51f]: #x-28MGL-PAX-3AEXPORTABLE-REFERENCE-P-20GENERIC-FUNCTION-29 "MGL-PAX:EXPORTABLE-REFERENCE-P GENERIC-FUNCTION"
  [e548]: #x-28MGL-PAX-3AWRITER-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:WRITER MGL-PAX:LOCATIVE"
  [eafc]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pr_obj.htm "PRINT-OBJECT GENERIC-FUNCTION"
  [ebd3]: #x-28MGL-PAX-3A-2ATRANSCRIBE-SYNTAXES-2A-20VARIABLE-29 "MGL-PAX:*TRANSCRIBE-SYNTAXES* VARIABLE"
  [ed5f]: #x-28MGL-PAX-3ACLHS-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:CLHS MGL-PAX:LOCATIVE"
  [ee51]: #x-28MGL-PAX-3AUPDATE-PAX-WORLD-20FUNCTION-29 "MGL-PAX:UPDATE-PAX-WORLD FUNCTION"
  [ee94]: #x-28MGL-PAX-3ALOCATE-20FUNCTION-29 "MGL-PAX:LOCATE FUNCTION"
  [f12d]: #x-28MGL-PAX-3A-2ADOCUMENT-MAX-NUMBERING-LEVEL-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-MAX-NUMBERING-LEVEL* VARIABLE"
  [f155]: #x-28-22mgl-pax-2Fnavigate-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"mgl-pax/navigate" ASDF/SYSTEM:SYSTEM'
  [f1ab]: #x-28MGL-PAX-3A-40CODIFICATION-20MGL-PAX-3ASECTION-29 "Codification"
  [f1f0]: #x-28MGL-PAX-3ATRANSCRIBE-20FUNCTION-29 "MGL-PAX:TRANSCRIBE FUNCTION"
  [f25f]: #x-28MGL-PAX-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-UPPERCASE-IS-CODE* VARIABLE"
  [f389]: #x-28MGL-PAX-3A-40LOCATIVES-AND-REFERENCES-API-20MGL-PAX-3ASECTION-29 "Locatives and References API"
  [f3cc]: #x-28MGL-PAX-3AMACRO-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:MACRO MGL-PAX:LOCATIVE"
  [f47d]: #x-28MGL-PAX-3A-40TRANSCRIPT-CONISTENCY-CHECKING-20MGL-PAX-3ASECTION-29 "Transcript Consistency Checking"
  [f4de]: http://www.lispworks.com/documentation/HyperSpec/Body/f_specia.htm "SPECIAL-OPERATOR-P FUNCTION"
  [f4fd]: #x-28MGL-PAX-3AREGISTER-DOC-IN-PAX-WORLD-20FUNCTION-29 "MGL-PAX:REGISTER-DOC-IN-PAX-WORLD FUNCTION"
  [f585]: #x-28MGL-PAX-3A-2ADOCUMENT-HYPERSPEC-ROOT-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-HYPERSPEC-ROOT* VARIABLE"
  [f5bd]: #x-28MGL-PAX-3A-40TRANSCRIBING-WITH-EMACS-20MGL-PAX-3ASECTION-29 "Transcribing with Emacs"
  [f74b]: #x-28MGL-PAX-3A-40BACKGROUND-20MGL-PAX-3ASECTION-29 "Background"
  [f945]: http://www.lispworks.com/documentation/HyperSpec/Body/03_d.htm '"3.4" MGL-PAX:CLHS'
  [f9d2]: #x-28MGL-PAX-3A-40PACKAGELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for Packages and Readtables"
  [fc18]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defmac.htm "DEFMACRO MGL-PAX:MACRO"
  [fc7b]: http://www.lispworks.com/documentation/HyperSpec/Body/t_meth_1.htm "METHOD-COMBINATION CLASS"
  [fd7c]: #x-28MGL-PAX-3A-40LOCATIVES-AND-REFERENCES-20MGL-PAX-3ASECTION-29 "Locatives and References"
  [fdd1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRINT FUNCTION"
  [fe2b]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_4.htm "DEFINE-METHOD-COMBINATION MGL-PAX:MACRO"
  [ff68]: #x-28MGL-PAX-3AFIND-DEFINITION-20FUNCTION-29 "MGL-PAX:FIND-DEFINITION FUNCTION"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
