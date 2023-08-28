<a id="x-28DREF-3A-40DREF-MANUAL-20MGL-PAX-3ASECTION-29"></a>

# DRef Manual

## Table of Contents

- [1 Introduction][ad80]
- [2 Locatives and References][7e8c]
- [3 Listing Definitions][e1d4]
- [4 Operations][5dd9]
- [5 Locative Types][bf0f]
    - [5.1 Locatives for Variables][462c]
    - [5.2 Locatives for Macros][d45d]
    - [5.3 Locatives for Functions][1d59]
    - [5.4 Locatives for Types and Declarations][7a04]
    - [5.5 Locatives for the Condition System][408d]
    - [5.6 Locatives for Packages and Readtables][c339]
    - [5.7 DRef Locatives][da93]
- [6 Glossary][d061]
- [7 Extending DRef][68fb]
    - [7.1 References][509d]
    - [7.2 Adding New Locatives][3cf3]
    - [7.3 Symbol Locatives][59c9]
    - [7.4 `DREF` Subclasses][0b7c]
    - [7.5 Source Locations][a078]

###### \[in package DREF\]
<a id="x-28DREF-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>

## 1 Introduction

*What if definitions were first-class objects?*

Some [defining forms][23a8] do not create first-class
[objects][3c8a]. For example, [`DEFUN`][f472] creates
[`FUNCTION`][119e] objects, but [`DEFVAR`][7334] does not create variable
objects as no such thing exists. The main purpose of this library is
to fill this gap with the introduction of [`XREF`][1538] objects:

```common-lisp
(xref '*my-var* 'variable)
==> #<XREF *MY-VAR* VARIABLE>
```

`XREF`s just package up a [name][5fc4] (`*MY-VAR*`) and a
[locative][7ac8] ([`VARIABLE`][6c83]). They need not denote existing definitions
until we actually want to use them:

```
(docstring (xref '*my-var* 'variable))
.. debugger invoked on LOCATE-ERROR:
..   Could not locate *MY-VAR* VARIABLE.
```

```common-lisp
(defvar *my-var* nil
  "This is my var.")

(docstring (xref '*my-var* 'variable))
=> "This is my var."
```

Behind the scenes, the [`DOCSTRING`][affc] function [`LOCATE`][8f19]s the definition
corresponding to its `XREF` argument, turning it into a [`DREF`][d930]:

```common-lisp
(locate (xref '*my-var* 'variable))
==> #<DREF *MY-VAR* VARIABLE>
```

Within DRef, the [`DREF` Subclasses][0b7c] form the basis of
extending `DOCSTRING`, [`SOURCE-LOCATION`][32da] and [`ARGLIST`][e6bd]. Outside DRef,
[`PAX`][2415] makes [`PAX:DOCUMENT`][432c] extensible through
[`PAX:DOCUMENT-OBJECT*`][8269], which has methods specialized on `DREF`s.

Finally, existing definitions can be queried with [`DEFINITIONS`][e196] and
[`DREF-APROPOS`][65b4]:

```
(definitions 'dref-ext:locate*)
==> (#<DREF LOCATE* GENERIC-FUNCTION>
-->  #<DREF LOCATE* (METHOD NIL (GLOSSARY-TERM))>
-->  #<DREF LOCATE* (METHOD NIL (SECTION))>
-->  #<DREF LOCATE* (METHOD NIL (READTABLE))>
-->  #<DREF LOCATE* (METHOD NIL (PACKAGE))>
-->  #<DREF LOCATE* (METHOD NIL (ASDF/SYSTEM:SYSTEM))>
-->  #<DREF LOCATE* (METHOD NIL (CLASS))>
-->  #<DREF LOCATE* (METHOD NIL (METHOD))>
-->  #<DREF LOCATE* (METHOD NIL (GENERIC-FUNCTION))>
-->  #<DREF LOCATE* (METHOD NIL (FUNCTION))>
-->  #<DREF LOCATE* (METHOD (:AROUND) (T))>
-->  #<DREF LOCATE* (METHOD NIL (T))> #<DREF LOCATE* (METHOD NIL (XREF))>
-->  #<DREF LOCATE* (METHOD NIL (DREF))>)
```

```common-lisp
(dref-apropos 'locate-error :package :dref :external-only t)
==> (#<DREF LOCATE-ERROR CONDITION> #<DREF LOCATE-ERROR FUNCTION>)

(dref-apropos "ate-err" :package :dref :external-only t)
==> (#<DREF LOCATE-ERROR CONDITION> #<DREF LOCATE-ERROR FUNCTION>)
```


<a id="x-28DREF-3A-40LOCATIVES-AND-REFERENCES-20MGL-PAX-3ASECTION-29"></a>

## 2 Locatives and References

After the [Introduction][ad80], here we get into the details. Of special
interest are:

- The [`XREF`][cda7] function to construct an arbitrary [reference][43bd] without any
  checking of validity.

- [`LOCATE`][8f19] and [`DREF`][7e92] to construct a syntactically valid
  reference (matching the `LAMBDA-LIST` in the locative type's
  [definition][b6c4]) that refers to an exisiting
  definition.

- [`RESOLVE`][63b4] to find the first-class (non-[`XREF`][1538]) object the
  definition refers to, if any.

[Operations][5dd9] ([`ARGLIST`][e6bd], [`DOCSTRING`][affc], [`SOURCE-LOCATION`][32da]) know how to deal
with references (discussed in the [Extending DRef][68fb]).

<a id="x-28DREF-3AXREF-20CLASS-29"></a>

- [class] **XREF**

    An `XREF` (cross-reference) may represent some
    kind of definition of its [name][5fc4] in the context given by its
    [locative][7ac8]. The definition may not exist and the locative may be
    [malformed][b6c4]. The subclass [`DREF`][d930]
    represents definitions that exist.

<a id="x-28DREF-3ADREF-20CLASS-29"></a>

- [class] **DREF** *[XREF][1538]*

    [`DREF`][d930]s can be thought of as referring to
    definitions that actually exist, although changes in the system can
    invalidate them (for example, a `DREF` to a function definition can be
    invalidated by [`FMAKUNBOUND`][609c]).
    
    `DREF`s must be created with [`LOCATE`][8f19], and their purpose is to allow
    easy specialization of other generic functions (see
    [Extending DRef][68fb]) and to confine locative validation to
    `LOCATE`.

<a id="x-28DREF-3AXREF-20FUNCTION-29"></a>

- [function] **XREF** *NAME LOCATIVE*

    A shorthand for `(MAKE-INSTANCE 'XREF :NAME NAME :LOCATIVE LOCATIVE)`.
    It does no error checking: the [`LOCATIVE-TYPE`][97ba] of `LOCATIVE-TYPE` need
    not be defined, and the [`LOCATIVE-ARGS`][2444] need not be valid. Use [`LOCATE`][8f19]
    or the [`DREF`][7e92] function to create [`DREF`][d930] objects.

<a id="x-28DREF-3AXREF-3D-20FUNCTION-29"></a>

- [function] **XREF=** *XREF1 XREF2*

    See if `XREF1` and `XREF2` have the same [`XREF-NAME`][b88e] and [`XREF-LOCATIVE`][f486]
    under [`EQUAL`][3fb5]. Comparing like this makes most sense for
    [`DREF`][d930]s. However, two [`XREF`][1538]s different under `XREF=`
    may denote the same [`DREF`][d930]s.

<a id="x-28DREF-3ALOCATE-20FUNCTION-29"></a>

- [function] **LOCATE** *OBJECT &OPTIONAL (ERRORP T)*

    Return a [`DREF`][d930] representing the definition given by the arguments.
    In the same [dynamic environment][62e7], two `DREF`s denote the same
    thing if and only if they are [`XREF=`][0617].
    
    `OBJECT` must be a supported first-class object, a `DREF`, or an
    [`XREF`][1538]:
    
    ```common-lisp
    (locate #'print)
    ==> #<DREF PRINT FUNCTION>
    ```
    
    ```common-lisp
    (locate (locate #'print))
    ==> #<DREF PRINT FUNCTION>
    ```
    
    ```common-lisp
    (locate (xref 'print 'function))
    ==> #<DREF PRINT FUNCTION>
    ```
    
    `LOCATE-ERROR`([`0`][6334] [`1`][6932]) is signalled if `OBJECT` is an `XREF` with malformed
    [`LOCATIVE-ARGS`][2444], or if no corresponding definition is found. If `ERRORP`
    is `NIL`, then `NIL` and the [`LOCATE-ERROR`][6334] condition are returned
    instead.
    
    ```common-lisp
    (locate (xref 'no-such-function 'function))
    .. debugger invoked on LOCATE-ERROR:
    ..   Could not locate NO-SUCH-FUNCTION FUNCTION.
    ..   NO-SUCH-FUNCTION is not a symbol naming a function.
    ```
    
    ```common-lisp
    (locate (xref 'print '(function xxx)))
    .. debugger invoked on LOCATE-ERROR:
    ..   Could not locate PRINT #'XXX.
    ..   Bad arguments (XXX) for locative FUNCTION with lambda list NIL.
    ```
    
    ```common-lisp
    (locate "xxx")
    .. debugger invoked on LOCATE-ERROR:
    ..   Could not locate "xxx".
    ```
    
    Use the low-level `XREF` to construct an `XREF` without error
    checking.
    
    Can be extended via [`LOCATE*`][76c4].

<a id="x-28DREF-3ADREF-20FUNCTION-29"></a>

- [function] **DREF** *NAME LOCATIVE &OPTIONAL (ERRORP T)*

    Shorthand for `(LOCATE (XREF NAME LOCATIVE) ERRORP)`.

<a id="x-28DREF-3ARESOLVE-20FUNCTION-29"></a>

- [function] **RESOLVE** *OBJECT &OPTIONAL (ERRORP T)*

    If `OBJECT` is an [`XREF`][1538], then return the first-class object
    associated with its definition if any. Return `OBJECT` if it's not an
    `XREF`. Thus, the value returned is never an `XREF`.
    
    ```common-lisp
    (resolve (dref 'print 'function))
    ==> #<FUNCTION PRINT>
    ```
    
    ```common-lisp
    (resolve #'print)
    ==> #<FUNCTION PRINT>
    ```
    
    If `OBJECT` is an `XREF`, and the definition for it cannot be [`LOCATE`][8f19]d,
    then signal a [`LOCATE-ERROR`][6334] condition.
    
    ```common-lisp
    (resolve (xref 'undefined 'variable))
    .. debugger invoked on LOCATE-ERROR:
    ..   Could not locate UNDEFINED VARIABLE.
    ```
    
    If there is a definition, but there is no first-class object
    corresponding to it, then signal a [`RESOLVE-ERROR`][0660] condition or return
    `NIL` depending on `ERRORP`:
    
    ```common-lisp
    (resolve (dref '*print-length* 'variable))
    .. debugger invoked on RESOLVE-ERROR:
    ..   Could not resolve *PRINT-LENGTH* VARIABLE.
    ```
    
    ```common-lisp
    (resolve (dref '*print-length* 'variable) nil)
    => NIL
    ```
    
    `RESOLVE` is a partial inverse of `LOCATE`: if a [`DREF`][d930] is
    `RESOLVE`able, then `LOCATE`ing the object it resolves to recovers the
    `DREF` equivalent to the original ([`XREF=`][0617] and of the same type but not
    [`EQ`][5a82]).
    
    Can be extended via [`RESOLVE*`][d3b3].

<a id="x-28DREF-EXT-3ALOCATE-ERROR-20CONDITION-29"></a>

- [condition] **LOCATE-ERROR** *[ERROR][d162]*

    Signalled by [`LOCATE`][8f19] when the definition cannot be
    found, and `ERRORP` is true.

<a id="x-28DREF-3ARESOLVE-ERROR-20CONDITION-29"></a>

- [condition] **RESOLVE-ERROR** *[ERROR][d162]*

    Signalled by [`RESOLVE`][63b4] when the object defined cannot
    be returned, and `ERRORP` is true.

<a id="x-28DREF-3A-40LISTING-DEFINITIONS-20MGL-PAX-3ASECTION-29"></a>

## 3 Listing Definitions

<a id="x-28DREF-3ADEFINITIONS-20FUNCTION-29"></a>

- [function] **DEFINITIONS** *NAME &KEY (LOCATIVE-TYPES (LISP-LOCATIVE-TYPES))*

    Return all definitions of `NAME` that match `LOCATIVE-TYPES`
    as a list of [`DREF`][d930]s.
    
    The [`DREF-NAME`][1cf6]s may not be the same as `NAME`, for example, when `NAME`
    is a package nickname:
    
    ```common-lisp
    (definitions 'pax)
    ==> (#<DREF "MGL-PAX" PACKAGE>)
    ```
    
    Can be extended via [`MAP-DEFINITIONS`][0e9c].

<a id="x-28DREF-3ADREF-APROPOS-20FUNCTION-29"></a>

- [function] **DREF-APROPOS** *NAME &KEY PACKAGE EXTERNAL-ONLY CASE-SENSITIVE (LOCATIVE-TYPES '(:LISP))*

    Return a list of [`DREF`][d930]s corresponding to existing
    definitions that match the various arguments. First, `(DREF-APROPOS
    NIL :LOCATIVE-TYPES NIL)` lists all definitions in the system.
    Arguments with non-`NIL` values filter the list of definitions.
    
    Roughly speaking, when `NAME` or `PACKAGE` is a [`SYMBOL`][e5af], they must match
    the whole [name][5fc4] of the definition:
    
    ```common-lisp
    (dref-apropos 'method :package :dref :external-only t)
    ==> (#<DREF METHOD CLASS> #<DREF METHOD LOCATIVE>)
    ```
    
    On the other hand, when `NAME` or `PACKAGE` is a `STRING`([`0`][b93c] [`1`][dae6]), they are
    matched as substrings:
    
    ```common-lisp
    (dref-apropos "method" :package :dref :external-only t)
    ==> (#<DREF METHOD CLASS> #<DREF METHOD LOCATIVE>
    -->  #<DREF METHOD-COMBINATION CLASS> #<DREF METHOD-COMBINATION LOCATIVE>)
    ```
    
    The list of `LOCATIVE-TYPES`, if non-`NIL`, filters out definitions
    whose locative types are not listed:
    
    ```common-lisp
    (dref-apropos "method" :package :dref :external-only t
                  :locative-types '(class))
    ==> (#<DREF METHOD CLASS> #<DREF METHOD-COMBINATION CLASS>)
    ```
    
    In the list, the special keywords `:ALL`, `:LISP`, `:PSEUDO` match all
    `LOCATIVE-TYPES`, [`LISP-LOCATIVE-TYPES`][30ad] and [`PSEUDO-LOCATIVE-TYPES`][c340],
    respectively.
    
    When `PACKAGE` is `:NONE`, only non-symbol [name][5fc4]s are matched:
    
    ```
    (dref-apropos "dref" :package :none)
    ==> (#<DREF "DREF" PACKAGE> #<DREF "DREF-EXT" PACKAGE>
    -->  #<DREF "DREF-TEST" PACKAGE> #<DREF "dref" ASDF/SYSTEM:SYSTEM>
    -->  #<DREF "dref/full" ASDF/SYSTEM:SYSTEM>
    -->  #<DREF "dref/test" ASDF/SYSTEM:SYSTEM>
    -->  #<DREF "dref/test-autoload" ASDF/SYSTEM:SYSTEM>)
    ```
    
    The exact rules of filtering are as follows. Let `C` be the [name][5fc4] of
    the candidate definition from the list of all definitions that we
    are matching against the arguments and denote its string
    representation `(PRINC-TO-STRING C)` with `P`. Note that
    [`PRINC-TO-STRING`][a541] does not print the package of symbols. We say that
    two strings *match* if `CASE-SENSITIVE` is `NIL` and they are [`EQUALP`][2ff3], or
    `CASE-SENSITIVE` is true and they are [`EQUAL`][3fb5]. `CASE-SENSITIVE` affects
    *substring* comparisons too.
    
    - If `NAME` is a `SYMBOL`, then its [`SYMBOL-NAME`][0d07] must *match* `P`.
    
    - If `NAME` is a `STRING`, then it must be a *substring* of `P`.
    
    - If `PACKAGE` is `:NONE`, then `C` must *not* be a `SYMBOL`.
    
    - If `PACKAGE` is not `NIL` or `:NONE`, then `C` must be a symbol.
    
    - If `PACKAGE` is a [`PACKAGE`][1d5a], it must be [`EQ`][5a82] to the
      [`SYMBOL-PACKAGE`][e5ab] of `C`.
    
    - If `PACKAGE` is a `SYMBOL` other than `:NONE`, then its `SYMBOL-NAME` must
      *match* the [`PACKAGE-NAME`][db68] or one of the [`PACKAGE-NICKNAMES`][4b93] of
      `SYMBOL-PACKAGE` of `C`.
    
    - If `PACKAGE` is a `STRING`, then it must be a *substring* of the
      `PACKAGE-NAME` of `SYMBOL-PACKAGE` of `C`.
    
    - If `EXTERNAL-ONLY` and `C` is a symbol, then `C` must be external in
      a matching package.
    
    - If `LOCATIVE-TYPES` is `NIL`, then it matches everything.
    
    - If [`LOCATIVE-TYPE`][97ba]s is non-`NIL`, then the `LOCATIVE-TYPE` of the
      candidate definition must be in it (handling `:ALL`,
      `:LISP`, and `:PSEUDO` as described above).
    
    Can be extended via [`MAP-NAMES`][811f].

<a id="x-28DREF-3ALOCATIVE-TYPES-20FUNCTION-29"></a>

- [function] **LOCATIVE-TYPES**

    Return a list of non-[alias][94d1] locative types.
    This is the [`UNION`][d6c7] of [`LISP-LOCATIVE-TYPES`][30ad] and [`PSEUDO-LOCATIVE-TYPES`][c340].

<a id="x-28DREF-3ALISP-LOCATIVE-TYPES-20FUNCTION-29"></a>

- [function] **LISP-LOCATIVE-TYPES**

    Return the locative types that correspond to Lisp definitions
    except [`UNKNOWN`][a951]. These are the ones defined with
    [`DEFINE-LOCATIVE-TYPE`][b6c4].

<a id="x-28DREF-3APSEUDO-LOCATIVE-TYPES-20FUNCTION-29"></a>

- [function] **PSEUDO-LOCATIVE-TYPES**

    Return the locative types that correspond to non-Lisp definitions
    plus [`UNKNOWN`][a951]. These are the ones defined with
    [`DEFINE-PSEUDO-LOCATIVE-TYPE`][68b4].

<a id="x-28DREF-3ALOCATIVE-ALIASES-20FUNCTION-29"></a>

- [function] **LOCATIVE-ALIASES**

    Return the list of locatives aliases, defined with [`DEFINE-LOCATIVE-ALIAS`][548e].

<a id="x-28DREF-3A-40OPERATIONS-20MGL-PAX-3ASECTION-29"></a>

## 4 Operations

The following functions take a single object definition as their argument.
They may try to [`LOCATE`][8f19] the definition of the object, which may
signal a [`LOCATE-ERROR`][6334] condition.

<a id="x-28DREF-3AARGLIST-20FUNCTION-29"></a>

- [function] **ARGLIST** *OBJECT*

    Return the arglist of the definition of `OBJECT` or `NIL` if the
    arglist cannot be determined.
    
    The second return value indicates whether the arglist has been
    found. Furthermore, `:ORDINARY` indicates an [ordinary lambda
    list][059c], `:MACRO` a [macro lambda list][cc32], `:DEFTYPE` a [deftype
    lambda list][817d], and `:DESTRUCTURING` a [destructuring lambda
    list][6067]. Other non-`NIL` values are also allowed.
    
    ```common-lisp
    (arglist #'arglist)
    => (OBJECT)
    => :ORDINARY
    ```
    
    ```common-lisp
    (arglist (dref 'define-locative-type 'macro))
    => (LOCATIVE-TYPE LAMBDA-LIST &BODY DOCSTRING)
    => :MACRO
    ```
    
    ```common-lisp
    (arglist (dref 'method 'locative))
    => (METHOD-QUALIFIERS METHOD-SPECIALIZERS)
    => :DESTRUCTURING
    ```
    
    This function supports [`MACRO`][f3cc]s, [`COMPILER-MACRO`][41fd]s, [`SETF`][a138] functions,
    `FUNCTION`([`0`][119e] [`1`][81f7])s, [`GENERIC-FUNCTION`][efe2]s, [`METHOD`][51c3]s, [`TYPE`][7c9f]s, [`LOCATIVE`][0b3a]s. Note that
    `ARGLIST` depends on the quality of `SWANK-BACKEND:ARGLIST`. With the
    exception of SBCL, which has perfect support, all Lisp
    implementations have minor omissions:
    
    - [`DEFTYPE`][7f9a] lambda lists on ABCL, AllegroCL, CLISP, CCL, CMUCL, ECL;
    
    - default values in `MACRO` lambda lists on AllegroCL; various edge
    
    - cases involving traced functions.
    
    Can be extended via [`ARGLIST*`][0a96]

<a id="x-28MGL-PAX-3ADOCSTRING-20FUNCTION-29"></a>

- [function] **DOCSTRING** *OBJECT*

    Return the docstring from the definition of `OBJECT`.
    As the second value, return the [`*PACKAGE*`][5ed1] that was in effect when
    the docstring was installed or `NIL` if it cannot be determined (this
    is used by [`PAX:DOCUMENT`][432c] when [Parsing][378f] the docstring). This
    function is similar in purpose to [`CL:DOCUMENTATION`][c5ae].
    
    Note that some locative types such as [`ASDF:SYSTEM`][c097]s and [`DECLARATION`][d07c]s
    have no docstrings, and some Lisp implementations do not record all
    docstrings. The following are known to be missing:
    
    - [`COMPILER-MACRO`][41fd] docstrings on ABCL, AllegroCL, CCL, ECL;
    
    - [`METHOD-COMBINATION`][9b70] docstrings on ABCL, AllegroCL.
    
    Can be extended via [`DOCSTRING*`][9fd4].

<a id="x-28DREF-3ASOURCE-LOCATION-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION** *OBJECT &KEY ERRORP*

    Return the Swank source location for the [defining form][23a8]
    of `OBJECT`. If no source location was found, then either an [`ERROR`][d162]
    condition is signalled if `ERRORP` else the [`ERROR`][d162] is
    returned as the second value (with the first being `NIL`). The
    returned Swank location object is to be accessed only through the
    [Source Locations][a078] API or to be passed to e.g Slime's
    `slime-goto-source-location`.
    
    Note that the availability of source location information varies
    greatly across Lisp implementations.
    
    Can be extended via [`SOURCE-LOCATION*`][444d].

<a id="x-28DREF-3A-40LOCATIVE-TYPES-20MGL-PAX-3ASECTION-29"></a>

## 5 Locative Types

The following are the [locative type][a11d]s supported out of the
box. As all locative types, they are named by symbols. When there is
a CL type corresponding to the reference's locative type, the
references can be [`RESOLVE`][63b4]d to a unique object as is the case in

```common-lisp
(resolve (dref 'print 'function))
==> #<FUNCTION PRINT>
```

Even if there is no such CL type, the [`ARGLIST`][e6bd], the [`DOCSTRING`][affc], and
the [`SOURCE-LOCATION`][32da] of the defining form is usually recorded unless
otherwise noted.

<a id="x-28DREF-3A-40VARIABLELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 5.1 Locatives for Variables

<a id="x-28VARIABLE-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **VARIABLE** *&OPTIONAL INITFORM*

    Refers to a global special variable.
    `INITFORM`, or if not specified, the global value of the variable is
    to be used for [presentation][ade6].
    
    ```common-lisp
    (dref '*print-length* 'variable)
    ==> #<DREF *PRINT-LENGTH* VARIABLE>
    ```
    
    `VARIABLE` references do not [`RESOLVE`][63b4].

<a id="x-28MGL-PAX-3ACONSTANT-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **CONSTANT** *&OPTIONAL INITFORM*

    Refers to a constant variable defined with [`DEFCONSTANT`][8934]. `INITFORM`,
    or if not specified, the value of the constant is included in the
    documentation. The [`CONSTANT`][c819] locative is like the [`VARIABLE`][6c83] locative,
    but it also checks that its object is [`CONSTANTP`][a26f].
    
    `CONSTANT` references do not [`RESOLVE`][63b4].

<a id="x-28DREF-3A-40MACROLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 5.2 Locatives for Macros

<a id="x-28MGL-PAX-3AMACRO-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **MACRO**

    Refers to a global macro, typically defined with [`DEFMACRO`][14cb], or to a
    [special operator][9a71].
    
    `MACRO` references do not [`RESOLVE`][63b4].

<a id="x-28MGL-PAX-3ASYMBOL-MACRO-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **SYMBOL-MACRO**

    Refers to a global symbol macro, defined with [`DEFINE-SYMBOL-MACRO`][46c0].
    Note that since `DEFINE-SYMBOL-MACRO` does not support docstrings, `PAX`
    defines methods on the [`DOCUMENTATION`][c5ae] generic function specialized on
    `(DOC-TYPE (EQL 'SYMBOL-MACRO))`.
    
    ```
    (define-symbol-macro my-mac 42)
    (setf (documentation 'my-mac 'symbol-macro)
          "This is MY-MAC.")
    (documentation 'my-mac 'symbol-macro)
    => "This is MY-MAC."
    ```
    
    `SYMBOL-MACRO` references do not [`RESOLVE`][63b4].

<a id="x-28COMPILER-MACRO-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **COMPILER-MACRO**

    Refers to a compiler macro, typically defined with
    [`DEFINE-COMPILER-MACRO`][23d5].
    
    `COMPILER-MACRO` references do not [`RESOLVE`][63b4].

<a id="x-28SETF-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **SETF** *&OPTIONAL METHOD*

    Refers to a [setf expander][35a2] (see [`DEFSETF`][66dc] and [`DEFINE-SETF-EXPANDER`][d2cb])
    or a [setf function][99b0] (e.g. `(DEFUN (SETF NAME) ...)` or the
    same with [`DEFGENERIC`][c7f7]). The format in [`DEFSECTION`][72b4] is `(<NAME> SETF)`
    in all these cases.
    
    To refer to methods of a setf generic function, use a [`METHOD`][172e]
    locative inside `SETF` like this:
    
        (dref 'documentation '(setf (method () (t symbol (eql function))))
    
    References to setf functions [`RESOLVE`][63b4] to the function object. Setf
    expander references do not `RESOLVE`.

<a id="x-28DREF-3A-40FUNCTIONLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 5.3 Locatives for Functions

<a id="x-28FUNCTION-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **FUNCTION**

    Refers to a global function, typically defined with [`DEFUN`][f472]. The
    [name][5fc4] must be a [`SYMBOL`][e5af] (see the [`SETF`][d83a] locative for how to reference
    [setf functions][99b0]). It is also allowed to reference
    [`GENERIC-FUNCTION`][efe2]s as `FUNCTION`s:
    
    ```common-lisp
    (dref 'docstring 'function)
    ==> #<DREF DOCSTRING FUNCTION>
    ```


<a id="x-28GENERIC-FUNCTION-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **GENERIC-FUNCTION**

    Refers to a [`GENERIC-FUNCTION`][efe2], typically defined with
    [`DEFGENERIC`][c7f7]. The [name][5fc4] must be a [`SYMBOL`][e5af] (see the [`SETF`][d83a] locative for
    how to reference [setf functions][99b0]).

<a id="x-28METHOD-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **METHOD** *METHOD-QUALIFIERS METHOD-SPECIALIZERS*

    Refers to `METHOD`s named by [`SYMBOL`][e5af]s (for [`SETF`][a138] methods,
    see the `SETF` locative). `METHOD-QUALIFIERS` and `METHOD-SPECIALIZERS`
    are similar to the [`CL:FIND-METHOD`][6d46]'s arguments of the same names. For
    example, the method
    
    ```common-lisp
    (defgeneric foo-gf (x y z)
      (:method :around (x (y (eql 'xxx)) (z string))
        (values x y z)))
    ```
    
    can be referred to as
    
    ```common-lisp
    (dref 'foo-gf '(method (:around) (t (eql xxx) string)))
    ==> #<DREF FOO-GF (METHOD (:AROUND) (T (EQL XXX) STRING))>
    ```
    
    `METHOD` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28METHOD-COMBINATION-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **METHOD-COMBINATION**

    Refers to a [`METHOD-COMBINATION`][9b70], defined with
    [`DEFINE-METHOD-COMBINATION`][006c].
    
    `METHOD-COMBINATION` references do not [`RESOLVE`][63b4].

<a id="x-28MGL-PAX-3AACCESSOR-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **ACCESSOR** *CLASS-NAME*

    Refers to an `:ACCESSOR` in a [`DEFCLASS`][ead6]:
    
    ```common-lisp
    (defclass foo ()
      ((xxx :accessor foo-xxx)))
    
    (dref 'foo-xxx '(accessor foo))
    ==> #<DREF FOO-XXX (ACCESSOR FOO)>
    ```
    
    An `:ACCESSOR` in `DEFCLASS` creates a reader and a writer method.
    Somewhat arbitrarily, `ACCESSOR` references [`RESOLVE`][63b4] to the writer
    method but can be [`LOCATE`][8f19]d with either.

<a id="x-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **READER** *CLASS-NAME*

    Like [`ACCESSOR`][00d4], but refers to a `:READER` method in a [`DEFCLASS`][ead6].

<a id="x-28MGL-PAX-3AWRITER-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **WRITER** *CLASS-NAME*

    Like [`ACCESSOR`][00d4], but refers to a `:WRITER` method in a [`DEFCLASS`][ead6].

<a id="x-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **STRUCTURE-ACCESSOR** *&OPTIONAL STRUCTURE-CLASS-NAME*

    Refers to an accessor function generated by [`DEFSTRUCT`][eac1].
    A [`LOCATE-ERROR`][6334] condition is signalled if the wrong
    `STRUCTURE-CLASS-NAME` is provided.
    
    Note that there is no portable way to detect structure accessors,
    and on some platforms, `(LOCATE #'MY-ACCESSOR)`, [`DEFINITIONS`][e196] and
    [`DREF-APROPOS`][65b4] will return `FUNCTION`([`0`][119e] [`1`][81f7]) references instead. On such
    platforms, `STRUCTURE-ACCESSOR` references do not [`RESOLVE`][63b4].

<a id="x-28DREF-3A-40TYPELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 5.4 Locatives for Types and Declarations

<a id="x-28TYPE-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **TYPE**

    This locative can refer to any Lisp type and [compound type
    specifiers][515e] such as [`AND`][dd55]. For types defined with
    [`DEFTYPE`][7f9a], their [`ARGLIST`][e6bd] is available. [`CLASS`es][1f37] and
    [`CONDITION`s][83e1] may be referred to as `TYPE`s:
    
    ```common-lisp
    (dref 'xref 'type)
    ==> #<DREF XREF CLASS>
    ```
    
    ```common-lisp
    (dref 'locate-error 'type)
    ==> #<DREF LOCATE-ERROR CONDITION>
    ```
    
    `TYPE` references do not [`RESOLVE`][63b4].

<a id="x-28CLASS-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **CLASS**

    Naturally, `CLASS` is the locative type for [`CLASS`][1f37]es.
    [`CONDITION`s][83e1] may be referred to as `CLASS`es:
    
    ```common-lisp
    (dref 'locate-error 'class)
    ==> #<DREF LOCATE-ERROR CONDITION>
    ```


<a id="x-28DECLARATION-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **DECLARATION**

    Refers to a declaration, used in [`DECLARE`][1574], [`DECLAIM`][ebea] and [`PROCLAIM`][d3e1].
    
    User code may also define new declarations with CLTL2 functionality,
    but there is currently no way to provide a docstring, and their
    [`ARGLIST`][e6bd] is always `NIL`.
    
    ```
    (cl-environments:define-declaration my-decl (&rest things)
      (values :declare (cons 'foo things)))
    ```
    
    `DECLARATION` references do not [`RESOLVE`][63b4].
    
    Also, [`SOURCE-LOCATION`][32da] on declarations currently only works on SBCL.

<a id="x-28DREF-3A-40CONDITION-SYSTEM-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 5.5 Locatives for the Condition System

<a id="x-28CONDITION-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **CONDITION**

    `CONDITION` is the locative type for [`CONDITION`][83e1]s.

<a id="x-28RESTART-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **RESTART**

    A locative to refer to the definition of a restart defined by
    [`DEFINE-RESTART`][bb23].

<a id="x-28DREF-3ADEFINE-RESTART-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-RESTART** *SYMBOL LAMBDA-LIST &BODY DOCSTRING*

    Associate a definition with the name of a restart, which must be a symbol.
    `LAMBDA-LIST` should be what calls like `(INVOKE-RESTART '<SYMBOL>
    ...)` must conform to, but this not enforced.
    
    `PAX` "defines" standard CL restarts such as `USE-VALUE`([`0`][5406] [`1`][cf08]) with
    `DEFINE-RESTART`:
    
    ```common-lisp
    (first-line (source-location-snippet
                 (source-location (dref 'use-value 'restart))))
    => "(define-restart use-value (value)"
    ```
    
    Note that while there is a [`CL:RESTART`][38e4] class, its instances have no
    docstring or source location.

<a id="x-28DREF-3A-40PACKAGELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 5.6 Locatives for Packages and Readtables

<a id="x-28ASDF-2FSYSTEM-3ASYSTEM-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **ASDF/SYSTEM:SYSTEM**

    Refers to a registered `ASDF:SYSTEM`.
    The [name][5fc4] may be anything `ASDF:FIND-SYSTEM` supports.
    
    `ASDF:SYSTEM` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28PACKAGE-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **PACKAGE**

    Refers to a [`PACKAGE`][1d5a], defined by [`DEFPACKAGE`][9b43] or [`MAKE-PACKAGE`][6e6e].
    The [name][5fc4] may be anything [`FIND-PACKAGE`][4dc9] supports.
    
    `PACKAGE` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28READTABLE-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **READTABLE**

    Refers to a named [`READTABLE`][d646] defined with
    `NAMED-READTABLES:DEFREADTABLE`, which associates a global name and a
    docstring with the readtable object. The [name][5fc4] may be anything
    `FIND-READTABLE` supports. Unfortunately, [`SOURCE-LOCATION`][32da] information
    is not available.
    
    `READTABLE` references [`RESOLVE`][63b4] to `FIND-READTABLE` on their [name][5fc4].

<a id="x-28DREF-3A-40DREF-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 5.7 DRef Locatives

<a id="x-28MGL-PAX-3ALOCATIVE-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **LOCATIVE**

    This is the locative for [locative type][a11d]s defined with
    [`DEFINE-LOCATIVE-TYPE`][b6c4], [`DEFINE-PSEUDO-LOCATIVE-TYPE`][68b4] and
    [`DEFINE-LOCATIVE-ALIAS`][548e].
    
    ```
    (first-line (source-location-snippet
                 (source-location (dref 'macro 'locative))))
    => "(define-locative-type macro ()"
    ```


<a id="x-28MGL-PAX-3AUNKNOWN-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **UNKNOWN** *DSPEC*

    This [pseudo locative type][c340] is to allow `PAX`
    to work in a limited way with locatives it doesn't know. `UNKNOWN`
    definitions come from [`DEFINITIONS`][e196], which uses
    `SWANK/BACKEND:FIND-DEFINITIONS`. The following examples show `PAX`
    stuffing the Swank dspec `(:DEFINE-ALIEN-TYPE DOUBLE-FLOAT)` into an
    [`UNKNOWN`][a951] locative on SBCL.
    
    ```common-lisp
    (definitions 'double-float :locative-types (locative-types))
    ==> (#<DREF DOUBLE-FLOAT CLASS> #<DREF DOUBLE-FLOAT (CLHS TYPE)>
    -->  #<DREF DOUBLE-FLOAT (UNKNOWN (:DEFINE-ALIEN-TYPE DOUBLE-FLOAT))>)
    ```
    
    ```common-lisp
    (dref 'double-float '(unknown (:define-alien-type double-float)))
    ==> #<DREF DOUBLE-FLOAT (UNKNOWN (:DEFINE-ALIEN-TYPE DOUBLE-FLOAT))>
    ```
    
    [`ARGLIST`][e6bd] and [`DOCSTRING`][affc] return `NIL` for `UNKNOWN`s, but [`SOURCE-LOCATION`][32da]
    works.

<a id="x-28LAMBDA-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **LAMBDA** *&KEY ARGLIST ARGLIST-TYPE DOCSTRING DOCSTRING-PACKAGE FILE FILE-POSITION SNIPPET &ALLOW-OTHER-KEYS*

    A [pseudo locative type][c340] that carries its
    `ARGLIST`, `DOCSTRING` and [`SOURCE-LOCATION`][32da] in the locative itself. See
    [`MAKE-SOURCE-LOCATION`][3bdc] for the description of `FILE`, `FILE-POSITION`, and
    `SNIPPET`. `LAMBDA` references do not [`RESOLVE`][63b4]. The [name][5fc4] must be `NIL`.
    
    ```common-lisp
    (arglist (dref nil '(lambda :arglist ((x y) z)
                                       :arglist-type :macro)))
    => ((X Y) Z)
    => :MACRO
    ```
    
    ```common-lisp
    (docstring (dref nil '(lambda :docstring "xxx"
                                         :docstring-package :dref)))
    => "xxx"
    ==> #<PACKAGE "DREF">
    ```
    
    ```common-lisp
    (source-location-file
     (source-location (dref nil '(lambda :file "xxx.el"))))
    => "xxx.el"
    ```
    
    See the [`PAX:INCLUDE`][5cd7] locative for an example.

<a id="x-28DREF-3A-40GLOSSARY-20MGL-PAX-3ASECTION-29"></a>

## 6 Glossary

<a id="x-28DREF-3A-40NAME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **name**

    Names are symbols or strings which name [functions][ba62], [types][926d], [packages][4dd7],
    etc. Together with [locative][7ac8]s, they form [reference][43bd]s.
    
    See [`XREF-NAME`][b88e] and [`DREF-NAME`][1cf6].

<a id="x-28DREF-3A-40LOCATIVE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **locative**

    Locatives specify a *type* of definition such as
    [`FUNCTION`][ba62] or [`VARIABLE`][6c83] and together with
    [name][5fc4]s form [reference][43bd]s.
    
    A locative can be a symbol or a list whose [`CAR`][d5a2] is a symbol. In
    either case, the symbol is called the [locative type][a11d], and the rest
    of the elements are the *locative arguments* (for example, see the
    [`METHOD`][51c3] locative).
    
    See [`XREF-LOCATIVE`][f486] and [`DREF-LOCATIVE`][3d59].

<a id="x-28DREF-3A-40LOCATIVE-TYPE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **locative type**

    The locative type is the part of a [locative][7ac8] that identifies
    what kind definition is being referred to. See [Locative Types][bf0f] for
    the list locative types built into DRef.
    
    Locative types are similar to Lisp [namespaces][c8c1].
    
    See [`XREF-LOCATIVE-TYPE`][882a] and [`DREF-LOCATIVE-TYPE`][6801].

<a id="x-28DREF-3A-40REFERENCE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **reference**

    A reference is an [name][5fc4] plus a [locative][7ac8], and it identifies a
    possible definition. References are of class [`XREF`][1538]. When a reference
    is a [`DREF`][d930], it may also be called a definition.

<a id="x-28DREF-3A-40PRESENTATION-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **presentation**

    [reference][43bd]s may have arguments (see
    [Adding New Locatives][3cf3]) that do not affect the behaviour of
    [`LOCATE`][8f19] and the standard DRef [Operations][5dd9], but which may be used for
    other, "presentation" purposes. For example, the [`VARIABLE`][6c83]
    locative's `INITFORM` argument is used for presentation by
    [`PAX:DOCUMENT`][432c]. Presentation arguments are available via
    [`DREF-EXT:DREF-ORIGIN`][c938].

<a id="x-28DREF-EXT-3A-40EXTENDING-DREF-20MGL-PAX-3ASECTION-29"></a>

## 7 Extending DRef

<a id="x-28DREF-EXT-3A-40REFERENCES-20MGL-PAX-3ASECTION-29"></a>

### 7.1 References

<a id="x-28DREF-EXT-3AXREF-NAME-20-28MGL-PAX-3AREADER-20DREF-3AXREF-29-29"></a>

- [reader] **XREF-NAME** *XREF (:NAME)*

    The [name][5fc4] of the reference.

<a id="x-28DREF-EXT-3AXREF-LOCATIVE-20-28MGL-PAX-3AREADER-20DREF-3AXREF-29-29"></a>

- [reader] **XREF-LOCATIVE** *XREF (:LOCATIVE)*

    The [locative][7ac8] of the reference.
    
    The locative is normalized by replacing single-element lists with
     their only element:
    
    ```common-lisp
    (xref 'print 'function)
    ==> #<XREF PRINT FUNCTION>
    ```
    
    ```common-lisp
    (xref 'print '(function))
    ==> #<XREF PRINT FUNCTION>
    ```


<a id="x-28DREF-EXT-3ADREF-NAME-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29"></a>

- [reader] **DREF-NAME** *DREF*

    The same as [`XREF-NAME`][b88e], but only works on
    [`DREF`][d930]s. Use it as a statement of intent.

<a id="x-28DREF-EXT-3ADREF-LOCATIVE-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29"></a>

- [reader] **DREF-LOCATIVE** *DREF*

    The same as [`XREF-LOCATIVE`][f486], but only works on
    [`DREF`][d930]s. Use it as a statement of intent.

<a id="x-28DREF-EXT-3ADREF-ORIGIN-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29"></a>

- [reader] **DREF-ORIGIN** *DREF*

    The object from which [`LOCATE`][8f19] constructed this
    [`DREF`][d930]. This is an [`XREF`][1538] when the `LOCATIVE` argument
    to `LOCATE` was non-`NIL` and the value NAME-OR-OBJECT argument
    otherwise. `DREF-ORIGIN` may have [presentation][ade6] arguments, which are
    not included in [`LOCATIVE-ARGS`][2444] as is the case with `INITFORM`
    argument of the [`VARIABLE`][6c83] locative:
    
    ```common-lisp
    (dref '*standard-output* '(variable "see-below"))
    ==> #<DREF *STANDARD-OUTPUT* VARIABLE>
    ```
    
    ```common-lisp
    (dref-origin (dref '*standard-output* '(variable "see-below")))
    ==> #<XREF *STANDARD-OUTPUT* (VARIABLE "see-below")>
    ```
    
    The `INITFORM` argument overrides the global binding of
    [`*STANDARD-OUTPUT*`][e7ee] when it's [`PAX:DOCUMENT`][432c]ed:
    
    ```common-lisp
    (first-line
     (pax:document (dref '*standard-output* '(variable "see-below"))
                   :stream nil))
    => "- [variable] *STANDARD-OUTPUT* \"see-below\""
    ```


<a id="x-28DREF-EXT-3ALOCATIVE-TYPE-20FUNCTION-29"></a>

- [function] **LOCATIVE-TYPE** *LOCATIVE*

    Return [locative type][a11d] of `LOCATIVE` (which may be from
    [`XREF-LOCATIVE`][f486]). This is the first element of `LOCATIVE` if it's a
    list. If it's a symbol, it's that symbol itself.

<a id="x-28DREF-EXT-3ALOCATIVE-ARGS-20FUNCTION-29"></a>

- [function] **LOCATIVE-ARGS** *LOCATIVE*

    Return the [`REST`][fe9f] of `LOCATIVE` (which may be from [`XREF-LOCATIVE`][f486])
    if it's a list. If it's a symbol, then return `NIL`. The locative args
    should match the `LAMBDA-LIST` of the [`LOCATIVE-TYPE`][97ba]'s
    [definition][b6c4], but this is guaranteed only for
    locatives of [`DREF`][d930]s and is not checked for plain
    [`XREF`][1538]s.

The following convenience functions are compositions of
{[`LOCATIVE-TYPE`][97ba], [`LOCATIVE-ARGS`][2444]} and {[`XREF-LOCATIVE`][f486],
[`DREF-LOCATIVE`][3d59]}.

<a id="x-28DREF-EXT-3AXREF-LOCATIVE-TYPE-20FUNCTION-29"></a>

- [function] **XREF-LOCATIVE-TYPE** *XREF*

<a id="x-28DREF-EXT-3AXREF-LOCATIVE-ARGS-20FUNCTION-29"></a>

- [function] **XREF-LOCATIVE-ARGS** *XREF*

<a id="x-28DREF-EXT-3ADREF-LOCATIVE-TYPE-20FUNCTION-29"></a>

- [function] **DREF-LOCATIVE-TYPE** *DREF*

<a id="x-28DREF-EXT-3ADREF-LOCATIVE-ARGS-20FUNCTION-29"></a>

- [function] **DREF-LOCATIVE-ARGS** *DREF*

<a id="x-28DREF-EXT-3A-40ADDING-NEW-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 7.2 Adding New Locatives

Let's see how to tell DRef about new kinds of definitions through
the example of the implementation of the [`CLASS`][2060] locative. Note that
this is a verbatim [`PAX:INCLUDE`][5cd7] of the sources. Please ignore any
internal machinery. The first step is to define the locative type:

```
(define-locative-type class ()
  "Naturally, CLASS is the locative type for [CLASS][class]es.
  [CONDITIONs][type] may be referred to as CLASSes:

  ```cl-transcript
  (dref 'locate-error 'class)
  ==> #<DREF LOCATE-ERROR CONDITION>
  ```")

```

Next, we define a subclass of [`DREF`][d930] associated with the
[`CLASS`][2060] locative type and specialize [`LOCATE*`][76c4]:

```
(define-definition-class class class-dref)

(defmethod locate* ((class class))
  (make-instance 'class-dref :name (class-name class) :locative 'class))

(defmethod dref* (symbol (locative-type (eql 'class)) locative-args)
  (check-locative-args class locative-args)
  (unless (and (symbolp symbol)
               (find-class symbol nil))
    (locate-error "~S does not name a class." symbol))
  (make-instance 'class-dref :name symbol :locative 'class))

```

The first method makes `(LOCATE (FIND-CLASS 'DREF))` work, while
the second is for `(DREF 'DREF 'CLASS)`. Naturally, for locative
types that do not define first-class objects, the first method
cannot be defined.

Then, with [`ADD-DREF-ACTUALIZER`][8490], we install a function that that runs
whenever a new [`DREF`][d930] is about to be returned from [`LOCATE`][8f19] and
turn the locative [`TYPE`][926d] into the locative [`CLASS`][2060] if the denoted
definition is of a class:

```
(defun actualize-type-to-class (dref)
  (when (eq (dref-locative-type dref) 'type)
    (dref (dref-name dref) 'class nil)))

(add-dref-actualizer 'actualize-type-to-class)

```

Finally, we define a [`RESOLVE*`][d3b3] method to recover the
[`CLASS`][1f37] object from a [`CLASS-DREF`][b3a7]. We also specialize
[`DOCSTRING*`][9fd4] and [`SOURCE-LOCATION*`][444d]:

```
(defmethod resolve* ((dref class-dref))
  (find-class (dref-name dref)))

(defmethod docstring* ((class class))
  (documentation* class t))

(defmethod source-location* ((dref class-dref))
  (swank-source-location* (resolve dref) (dref-name dref) 'class))

```

We took advantage of having just made the class locative type being
[`RESOLVE`][63b4]able, by specializing [`DOCSTRING*`][9fd4] on the [`CLASS`][1f37] class.
[`SOURCE-LOCATION*`][444d] was specialized on [`CLASS-DREF`][b3a7] to demonstrate how
this can be done for non-`RESOLVE`able locative types.

Classes have no arglist, so no [`ARGLIST*`][0a96] method is needed. In the
following, we describe the pieces in detail.

<a id="x-28DREF-EXT-3ADEFINE-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-LOCATIVE-TYPE** *LOCATIVE-TYPE LAMBDA-LIST &BODY DOCSTRING*

    Declare `LOCATIVE-TYPE` as a [`LOCATIVE`][0b3a], which is the
    first step in [Extending DRef][68fb].
    
    `LAMBDA-LIST` is a [destructuring lambda list][6067]. The
    [`LOCATIVE-ARGS`][2444] of [`DREF`][d930]s with [locative type][a11d]
    `LOCATIVE-TYPE` (the argument given to this macro) always conform to
    this lambda list. See [`CHECK-LOCATIVE-ARGS`][10a7].
    
    For example, if  have:
    
    ```
    (define-locative-type dummy (x &key y)
      "Dummy docstring.")
    ```
    
    then `(LOCATE 'DUMMY 'LOCATIVE)` refers to this definition. That is,
    [`ARGLIST`][e6bd], [`DOCSTRING`][affc] and [`SOURCE-LOCATION`][32da] all work on it.
    
    Locative types defined with `DEFINE-LOCATIVE-TYPE` can be listed with
    [`LISP-LOCATIVE-TYPES`][30ad].

<a id="x-28DREF-EXT-3ADEFINE-PSEUDO-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-PSEUDO-LOCATIVE-TYPE** *LOCATIVE-TYPE LAMBDA-LIST &BODY DOCSTRING*

    Like [`DEFINE-LOCATIVE-TYPE`][b6c4], but declare that `LOCATIVE-TYPE` does
    not correspond to definitions in the Lisp system. Definitions with
    pseduo locatives are not listed by default by [`DEFINITIONS`][e196].
    
    Locative types defined with `DEFINE-PSEUDO-LOCATIVE-TYPE` can be
    listed with [`PSEUDO-LOCATIVE-TYPES`][c340].

<a id="x-28DREF-EXT-3ADEFINE-LOCATIVE-ALIAS-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-LOCATIVE-ALIAS** *ALIAS LOCATIVE-TYPE &BODY DOCSTRING*

    Define `ALIAS` as a locative equivalent to `LOCATIVE-TYPE` (both
    [`SYMBOL`][e5af]s). `LOCATIVE-TYPE` must exist (i.e. be among [`LOCATIVE-TYPES`][99b0a]).
    For example, let's define `OBJECT` as an alias of the [`CLASS`][2060] locative:
    
    ```common-lisp
    (define-locative-alias object class)
    ```
    
    Then, [`LOCATE`][8f19]ing with `OBJECT` will find the `CLASS`:
    
    ```common-lisp
    (dref 'xref 'object)
    ==> #<DREF XREF CLASS>
    ```
    
    The [`LOCATIVE-ARGS`][2444] of `OBJECT` (none in the above) are passed on to
    `CLASS`.
    
    ```common-lisp
    (arglist (dref 'object 'locative))
    => (&REST ARGS)
    => :DESTRUCTURING
    ```
    
    Also, see [Locative Aliases][0fa3] in `PAX`.

<a id="x-28DREF-EXT-3ADEFINE-DEFINITION-CLASS-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-DEFINITION-CLASS** *LOCATIVE-TYPE CLASS-NAME &OPTIONAL (SUPERCLASSES '(DREF)) &BODY BODY*

    Define a subclass of `DREF`([`0`][d930] [`1`][7e92]). All definitions with `LOCATIVE-TYPE`
    must be of this type. If non-`NIL`, `BODY` is [`DEFCLASS`][ead6]' slot definitions
    and other options.

<a id="x-28DREF-EXT-3ALOCATE-2A-20GENERIC-FUNCTION-29"></a>

- [generic-function] **LOCATE\*** *OBJECT*

    Return a definition of `OBJECT` as a [`DREF`][d930],
    without [actualizing it][8490]. If `OBJECT` is a `DREF`
    already, then this function simply returns it. If no definition is
    found for `OBJECT`, then `LOCATE-ERROR`([`0`][6334] [`1`][6932]) is signalled.
    
    This function is for extending [`LOCATE`][8f19]. Do not call it directly.

<a id="x-28DREF-EXT-3ADREF-2A-20GENERIC-FUNCTION-29"></a>

- [generic-function] **DREF\*** *NAME LOCATIVE-TYPE LOCATIVE-ARGS*

    [`LOCATE*`][76c4] calls this for [`XREF`][1538]s which are not
    [`DREF`][d930]s.
    
    An `EQL`([`0`][db03] [`1`][5fd4])-specialized method must be defined for all new locative
    types. This function is for extending [`LOCATE`][8f19]. Do not call it
    directly.

<a id="x-28DREF-EXT-3ACHECK-LOCATIVE-ARGS-20MGL-PAX-3AMACRO-29"></a>

- [macro] **CHECK-LOCATIVE-ARGS** *LOCATIVE-TYPE LOCATIVE-ARGS*

    Signal a [`LOCATE-ERROR`][6334] condition if `LOCATIVE-ARGS` do not match the
    `LAMBDA-LIST` argument of `LOCATIVE-TYPE` (not evaluated).

<a id="x-28DREF-EXT-3ALOCATE-ERROR-20FUNCTION-29"></a>

- [function] **LOCATE-ERROR** *&REST FORMAT-AND-ARGS*

    Call this function to signal a [`LOCATE-ERROR`][6334] condition from the
    [dynamic extent][36e9] of a [`LOCATE*`][76c4] method (which includes
    [`DREF*`][ee40]). It is an error to call `LOCATE-ERROR` elsewhere.
    
    `FORMAT-AND-ARGS`, if non-`NIL`, is a format string and arguments
    suitable for [`FORMAT`][ad78].

<a id="x-28DREF-EXT-3AADD-DREF-ACTUALIZER-20FUNCTION-29"></a>

- [function] **ADD-DREF-ACTUALIZER** *NAME*

    Add the global function denoted by the symbol `NAME` to the list
    of actualizers. Actualizers are functions of a single [`DREF`][d930]
    argument. They are called within [`LOCATE`][8f19] when [`LOCATE*`][76c4] returns a `DREF`.
    Their job is to make the `DREF` more specific.

<a id="x-28DREF-EXT-3AREMOVE-DREF-ACTUALIZER-20FUNCTION-29"></a>

- [function] **REMOVE-DREF-ACTUALIZER** *NAME*

    Remove the global function denoted by the symbol `NAME` from the
    list of actualizers.

<a id="x-28DREF-EXT-3ARESOLVE-2A-20GENERIC-FUNCTION-29"></a>

- [generic-function] **RESOLVE\*** *DREF*

    Return the object defined by the definition `DREF`
    refers to. Signal a [`RESOLVE-ERROR`][0660] condition by calling the
    [`RESOLVE-ERROR`][f70b] function if the lookup fails.
    
    To keep [`RESOLVE`][63b4] a partial inverse of [`LOCATE`][8f19], a specialized [`LOCATE*`][76c4]
    method or an [actualizer][8490] must be defined for
    `RESOLVE`able definitions. This function is for extending `RESOLVE`. Do
    not call it directly.
    
    It is an error for methods of this generic function to return an
    [`XREF`][1538].

<a id="x-28DREF-3ARESOLVE-ERROR-20FUNCTION-29"></a>

- [function] **RESOLVE-ERROR** *&REST FORMAT-AND-ARGS*

    Call this function to signal a [`RESOLVE-ERROR`][0660] condition from the
    [dynamic extent][36e9] of a [`RESOLVE*`][d3b3] method. It is an error to call
    `RESOLVE-ERROR` elsewhere.
    
    `FORMAT-AND-ARGS`, if non-`NIL`, is a format string and arguments
    suitable for [`FORMAT`][ad78].

<a id="x-28DREF-3AMAP-DEFINITIONS-20GENERIC-FUNCTION-29"></a>

- [generic-function] **MAP-DEFINITIONS** *FN NAME LOCATIVE-TYPE*

    Call `FN` with [`DREF`][d930]s which have the given
    `NAME` and `LOCATIVE-TYPE`. For most locative types, there is at most
    one definition, but for [`METHOD`][51c3], for example, there may be many. The
    default method simply does `(DREF NAME LOCATIVE-TYPE NIL)` and calls
    `FN` with result if [`DREF`][7e92] succeeds.
    
    This function is for extending [`DEFINITIONS`][e196]. Do not call it directly.

<a id="x-28DREF-3AMAP-NAMES-20GENERIC-FUNCTION-29"></a>

- [generic-function] **MAP-NAMES** *FN LOCATIVE-TYPE*

    Call `FN` with [name][5fc4]s that form a [`DREF`][d930] with
    some locative with `LOCATIVE-TYPE`. The default method tries to form
    `DREF`s by combining each interned symbol with `LOCATIVE-TYPE` and no
    [`LOCATIVE-ARGS`][2444].
    
    This function is for extending [`DREF-APROPOS`][65b4]. Do not call it
    directly.

<a id="x-28DREF-EXT-3AARGLIST-2A-20GENERIC-FUNCTION-29"></a>

- [generic-function] **ARGLIST\*** *OBJECT*

    To extend [`ARGLIST`][e6bd], specialize this on a subclass of
    [`DREF`][d930] if that subclass is not [`RESOLVE`][63b4]able, else on the type
    of object it resolves to. This function is for extension only. Do
    not call it directly.

<a id="x-28DREF-EXT-3ADOCSTRING-2A-20GENERIC-FUNCTION-29"></a>

- [generic-function] **DOCSTRING\*** *DREF*

    To extend [`DOCSTRING`][affc], specialize this on a subclass
    of [`DREF`][d930] if that subclass is not [`RESOLVE`][63b4]able, else on the
    type of object it resolves to. This function is for extension only.
    Do not call it directly.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-2A-20GENERIC-FUNCTION-29"></a>

- [generic-function] **SOURCE-LOCATION\*** *DREF*

    To extend [`SOURCE-LOCATION`][32da], specialize this on a
    subclass of [`DREF`][d930] if that subclass is not [`RESOLVE`][63b4]able, else
    on the type of object it resolves to. This function is for extension
    only. Do not call it directly.

<a id="x-28DREF-EXT-3A-40SYMBOL-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 7.3 Symbol Locatives

Let's see how the opaque [`DEFINE-SYMBOL-LOCATIVE-TYPE`][ee9b] and the
obscure [`DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE`][3b96] macros work together
to simplify the common task of associating definition with a symbol
in a certain context.

<a id="x-28DREF-EXT-3ADEFINE-SYMBOL-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-SYMBOL-LOCATIVE-TYPE** *LOCATIVE-TYPE LAMBDA-LIST &BODY DOCSTRING*

    Similar to [`DEFINE-LOCATIVE-TYPE`][b6c4], but it assumes that all things
    [`LOCATE`][8f19]able with `LOCATIVE-TYPE` are going to be symbols defined with a
    definer defined with [`DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE`][3b96]. Symbol
    locatives are for attaching a definition (along with arglist,
    documentation and source location) to a symbol in a particular
    context. An example will make everything clear:
    
    ```
    (define-symbol-locative-type direction ()
      "A direction is a symbol.")
    
    (define-definer-for-symbol-locative-type define-direction direction
      "With DEFINE-DIRECTION, one can document what a symbol means when
      interpreted as a DIRECTION.")
    
    (define-direction up ()
      "UP is equivalent to a coordinate delta of (0, -1).")
    ```
    
    After all this, `(DREF 'UP 'DIRECTION)` refers to the
    `DEFINE-DIRECTION` form above.

<a id="x-28DREF-EXT-3ADEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE** *NAME LOCATIVE-TYPE &BODY DOCSTRING*

    Define a macro with `NAME` that can be used to attach a lambda list,
    documentation, and source location to a symbol in the context of
    `LOCATIVE-TYPE`. The defined macro's arglist is `(SYMBOL LAMBDA-LIST
    &OPTIONAL DOCSTRING)`. `LOCATIVE-TYPE` is assumed to have been defined
    with [`DEFINE-SYMBOL-LOCATIVE-TYPE`][ee9b].

<a id="x-28DREF-EXT-3A-40DREF-SUBCLASSES-20MGL-PAX-3ASECTION-29"></a>

### 7.4 `DREF` Subclasses

These are the [`DREF`][d930] subclasses corresponding to
[Locative Types][bf0f]. They are exported to make it possible to go
beyond the standard [Operations][5dd9] (e.g. [`PAX:DOCUMENT-OBJECT*`][8269]) and for
subclassing.

**for Variables**

<a id="x-28DREF-EXT-3AVARIABLE-DREF-20CLASS-29"></a>

- [class] **VARIABLE-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3ACONSTANT-DREF-20CLASS-29"></a>

- [class] **CONSTANT-DREF** *[VARIABLE-DREF][ad35]*

**for Macros**

<a id="x-28DREF-EXT-3AMACRO-DREF-20CLASS-29"></a>

- [class] **MACRO-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3ASYMBOL-MACRO-DREF-20CLASS-29"></a>

- [class] **SYMBOL-MACRO-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3ACOMPILER-MACRO-DREF-20CLASS-29"></a>

- [class] **COMPILER-MACRO-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3ASETF-DREF-20CLASS-29"></a>

- [class] **SETF-DREF** *[DREF][d930]*

**for Functions**

<a id="x-28DREF-EXT-3AFUNCTION-DREF-20CLASS-29"></a>

- [class] **FUNCTION-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3AGENERIC-FUNCTION-DREF-20CLASS-29"></a>

- [class] **GENERIC-FUNCTION-DREF** *[FUNCTION-DREF][e576]*

<a id="x-28DREF-EXT-3AMETHOD-DREF-20CLASS-29"></a>

- [class] **METHOD-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3AMETHOD-COMBINATION-DREF-20CLASS-29"></a>

- [class] **METHOD-COMBINATION-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3AACCESSOR-DREF-20CLASS-29"></a>

- [class] **ACCESSOR-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3AREADER-DREF-20CLASS-29"></a>

- [class] **READER-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3AWRITER-DREF-20CLASS-29"></a>

- [class] **WRITER-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3ASTRUCTURE-ACCESSOR-DREF-20CLASS-29"></a>

- [class] **STRUCTURE-ACCESSOR-DREF** *[DREF][d930]*

**for Types and Declarations**

<a id="x-28DREF-EXT-3ATYPE-DREF-20CLASS-29"></a>

- [class] **TYPE-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3ACLASS-DREF-20CLASS-29"></a>

- [class] **CLASS-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3ADECLARATION-DREF-20CLASS-29"></a>

- [class] **DECLARATION-DREF** *[DREF][d930]*

**for the Condition System**

<a id="x-28DREF-EXT-3ACONDITION-DREF-20CLASS-29"></a>

- [class] **CONDITION-DREF** *[CLASS-DREF][b3a7]*

<a id="x-28DREF-EXT-3ARESTART-DREF-20CLASS-29"></a>

- [class] **RESTART-DREF** *[SYMBOL-LOCATIVE-DREF][34b9]*

**for Packages and Readtables**

<a id="x-28DREF-EXT-3AASDF-SYSTEM-DREF-20CLASS-29"></a>

- [class] **ASDF-SYSTEM-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3APACKAGE-DREF-20CLASS-29"></a>

- [class] **PACKAGE-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3AREADTABLE-DREF-20CLASS-29"></a>

- [class] **READTABLE-DREF** *[DREF][d930]*

**for DRef Locatives**

<a id="x-28DREF-EXT-3ALOCATIVE-DREF-20CLASS-29"></a>

- [class] **LOCATIVE-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3ASYMBOL-LOCATIVE-DREF-20CLASS-29"></a>

- [class] **SYMBOL-LOCATIVE-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3AUNKNOWN-DREF-20CLASS-29"></a>

- [class] **UNKNOWN-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3ALAMBDA-DREF-20CLASS-29"></a>

- [class] **LAMBDA-DREF** *[DREF][d930]*

<a id="x-28DREF-EXT-3A-40SOURCE-LOCATIONS-20MGL-PAX-3ASECTION-29"></a>

### 7.5 Source Locations

These represent the file or buffer position of a [defining
form][23a8] and are returned by the [`SOURCE-LOCATION`][32da] function. For
the details, see the Elisp function `slime-goto-source-location`.

<a id="x-28DREF-EXT-3AMAKE-SOURCE-LOCATION-20FUNCTION-29"></a>

- [function] **MAKE-SOURCE-LOCATION** *&KEY FILE FILE-POSITION BUFFER BUFFER-POSITION SNIPPET*

    Make a Swank source location. The ultimate reference is `slime.el`.
    When `SNIPPET` is provided, the match nearest to `FILE-POSITION` is
    determined (see the Elisp `slime-isearch` and
    [`SOURCE-LOCATION-ADJUSTED-FILE-POSITION`][daac]).

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-P-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION-P** *OBJECT*

    See if `OBJECT` is a source location object.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-FILE-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION-FILE** *LOCATION*

    Return the name of the file of the [defining form][23a8].
    This may be `NIL`, for example, if `LOCATION` is of a [defining
    form][23a8] that was entered at the REPL, or compiled in the
    `*slime-scratch*` buffer.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-FILE-POSITION-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION-FILE-POSITION** *LOCATION*

    Return the file position of the [defining form][23a8] or `NIL`
    if it's not available. The first position is 0.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-BUFFER-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION-BUFFER** *LOCATION*

    Return the name of the Emacs buffer of the [defining form][23a8] or
    `NIL` if there is no such Emacs buffer.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-BUFFER-POSITION-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION-BUFFER-POSITION** *LOCATION*

    Return the position of the [defining form][23a8] in
    [`SOURCE-LOCATION-BUFFER`][39c2] or `NIL` if it's not available. The first
    position is 1.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-SNIPPET-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION-SNIPPET** *LOCATION*

    Return the [defining form][23a8] or a prefix of it as a string or `NIL`
    if it's not available.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-ADJUSTED-FILE-POSITION-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION-ADJUSTED-FILE-POSITION** *LOCATION*

    Return the actual file position `LOCATION` points to allowing for 
    some deviation from the raw [`SOURCE-LOCATION-FILE-POSITION`][be18], which is
    adjusted by searching for the nearest occurrence of
    [`SOURCE-LOCATION-SNIPPET`][6ec3] in the file. Needless to say, this can be a
    very expensive operation.
    
    If [`SOURCE-LOCATION-FILE`][ae5a] is `NIL`, `NIL` is returned. If there is no
    snippet, or it doesn't match, then `SOURCE-LOCATION-FILE-POSITION` (or
    0 if that's `NIL`) is returned.
    
    This is a non-interactive companion to the Elisp function
    `slime-location-offset`, supporting only file positions and
    non-partial matching of snippets.

  [006c]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_4.htm "DEFINE-METHOD-COMBINATION (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [00d4]: #x-28MGL-PAX-3AACCESSOR-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:ACCESSOR MGL-PAX:LOCATIVE"
  [059c]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_o.htm#ordinary_lambda_list '"ordinary lambda list" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [0617]: #x-28DREF-3AXREF-3D-20FUNCTION-29 "DREF:XREF= FUNCTION"
  [0660]: #x-28DREF-3ARESOLVE-ERROR-20CONDITION-29 "DREF:RESOLVE-ERROR CONDITION"
  [0a96]: #x-28DREF-EXT-3AARGLIST-2A-20GENERIC-FUNCTION-29 "DREF-EXT:ARGLIST* GENERIC-FUNCTION"
  [0b3a]: #x-28MGL-PAX-3ALOCATIVE-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:LOCATIVE MGL-PAX:LOCATIVE"
  [0b7c]: #x-28DREF-EXT-3A-40DREF-SUBCLASSES-20MGL-PAX-3ASECTION-29 "`DREF` Subclasses"
  [0d07]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_2.htm "SYMBOL-NAME (MGL-PAX:CLHS FUNCTION)"
  [0e9c]: #x-28DREF-3AMAP-DEFINITIONS-20GENERIC-FUNCTION-29 "DREF:MAP-DEFINITIONS GENERIC-FUNCTION"
  [0fa3]: ../README.md#x-28MGL-PAX-3A-40LOCATIVE-ALIASES-20MGL-PAX-3ASECTION-29 "Locative Aliases"
  [10a7]: #x-28DREF-EXT-3ACHECK-LOCATIVE-ARGS-20MGL-PAX-3AMACRO-29 "DREF-EXT:CHECK-LOCATIVE-ARGS MGL-PAX:MACRO"
  [119e]: http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm "FUNCTION (MGL-PAX:CLHS CLASS)"
  [14cb]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defmac.htm "DEFMACRO (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [1538]: #x-28DREF-3AXREF-20CLASS-29 "DREF:XREF CLASS"
  [1574]: http://www.lispworks.com/documentation/HyperSpec/Body/s_declar.htm "DECLARE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [172e]: #x-28METHOD-20MGL-PAX-3ALOCATIVE-29 "METHOD MGL-PAX:LOCATIVE"
  [1cf6]: #x-28DREF-EXT-3ADREF-NAME-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29 "DREF-EXT:DREF-NAME (MGL-PAX:READER DREF:DREF)"
  [1d59]: #x-28DREF-3A-40FUNCTIONLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for Functions"
  [1d5a]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm "PACKAGE (MGL-PAX:CLHS CLASS)"
  [1f37]: http://www.lispworks.com/documentation/HyperSpec/Body/t_class.htm "CLASS (MGL-PAX:CLHS CLASS)"
  [2060]: #x-28CLASS-20MGL-PAX-3ALOCATIVE-29 "CLASS MGL-PAX:LOCATIVE"
  [23a8]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#defining_form '"defining form" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [23d5]: http://www.lispworks.com/documentation/HyperSpec/Body/m_define.htm "DEFINE-COMPILER-MACRO (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [2415]: ../README.md "PAX Manual"
  [2444]: #x-28DREF-EXT-3ALOCATIVE-ARGS-20FUNCTION-29 "DREF-EXT:LOCATIVE-ARGS FUNCTION"
  [2ff3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equalp.htm "EQUALP (MGL-PAX:CLHS FUNCTION)"
  [30ad]: #x-28DREF-3ALISP-LOCATIVE-TYPES-20FUNCTION-29 "DREF:LISP-LOCATIVE-TYPES FUNCTION"
  [32da]: #x-28DREF-3ASOURCE-LOCATION-20FUNCTION-29 "DREF:SOURCE-LOCATION FUNCTION"
  [34b9]: #x-28DREF-EXT-3ASYMBOL-LOCATIVE-DREF-20CLASS-29 "DREF-EXT:SYMBOL-LOCATIVE-DREF CLASS"
  [35a2]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#setf_expander '"setf expander" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [36e9]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#dynamic_extent '"dynamic extent" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [378f]: ../README.md#x-28MGL-PAX-3A-40PARSING-20MGL-PAX-3ASECTION-29 "Parsing"
  [38e4]: http://www.lispworks.com/documentation/HyperSpec/Body/t_rst.htm "RESTART (MGL-PAX:CLHS CLASS)"
  [39c2]: #x-28DREF-EXT-3ASOURCE-LOCATION-BUFFER-20FUNCTION-29 "DREF-EXT:SOURCE-LOCATION-BUFFER FUNCTION"
  [3b96]: #x-28DREF-EXT-3ADEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE MGL-PAX:MACRO"
  [3bdc]: #x-28DREF-EXT-3AMAKE-SOURCE-LOCATION-20FUNCTION-29 "DREF-EXT:MAKE-SOURCE-LOCATION FUNCTION"
  [3c8a]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_o.htm#object '"object" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [3cf3]: #x-28DREF-EXT-3A-40ADDING-NEW-LOCATIVES-20MGL-PAX-3ASECTION-29 "Adding New Locatives"
  [3d59]: #x-28DREF-EXT-3ADREF-LOCATIVE-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29 "DREF-EXT:DREF-LOCATIVE (MGL-PAX:READER DREF:DREF)"
  [3fb5]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm "EQUAL (MGL-PAX:CLHS FUNCTION)"
  [408d]: #x-28DREF-3A-40CONDITION-SYSTEM-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for the Condition System"
  [41fd]: #x-28COMPILER-MACRO-20MGL-PAX-3ALOCATIVE-29 "COMPILER-MACRO MGL-PAX:LOCATIVE"
  [432c]: ../README.md#x-28MGL-PAX-3ADOCUMENT-20FUNCTION-29 "MGL-PAX:DOCUMENT FUNCTION"
  [43bd]: #x-28DREF-3A-40REFERENCE-20MGL-PAX-3AGLOSSARY-TERM-29 "reference"
  [444d]: #x-28DREF-EXT-3ASOURCE-LOCATION-2A-20GENERIC-FUNCTION-29 "DREF-EXT:SOURCE-LOCATION* GENERIC-FUNCTION"
  [462c]: #x-28DREF-3A-40VARIABLELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for Variables"
  [46c0]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_1.htm "DEFINE-SYMBOL-MACRO (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [4b93]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pkg_ni.htm "PACKAGE-NICKNAMES (MGL-PAX:CLHS FUNCTION)"
  [4dc9]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_p.htm "FIND-PACKAGE (MGL-PAX:CLHS FUNCTION)"
  [4dd7]: #x-28PACKAGE-20MGL-PAX-3ALOCATIVE-29 "PACKAGE MGL-PAX:LOCATIVE"
  [509d]: #x-28DREF-EXT-3A-40REFERENCES-20MGL-PAX-3ASECTION-29 "References"
  [515e]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#compound_type_specifier '"compound type specifier" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [51c3]: http://www.lispworks.com/documentation/HyperSpec/Body/t_method.htm "METHOD (MGL-PAX:CLHS CLASS)"
  [5406]: http://www.lispworks.com/documentation/HyperSpec/Body/f_abortc.htm "USE-VALUE (MGL-PAX:CLHS FUNCTION)"
  [548e]: #x-28DREF-EXT-3ADEFINE-LOCATIVE-ALIAS-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-LOCATIVE-ALIAS MGL-PAX:MACRO"
  [59c9]: #x-28DREF-EXT-3A-40SYMBOL-LOCATIVES-20MGL-PAX-3ASECTION-29 "Symbol Locatives"
  [5a82]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm "EQ (MGL-PAX:CLHS FUNCTION)"
  [5cd7]: ../README.md#x-28MGL-PAX-3AINCLUDE-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:INCLUDE MGL-PAX:LOCATIVE"
  [5dd9]: #x-28DREF-3A-40OPERATIONS-20MGL-PAX-3ASECTION-29 "Operations"
  [5ed1]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pkg.htm "*PACKAGE* (MGL-PAX:CLHS VARIABLE)"
  [5fc4]: #x-28DREF-3A-40NAME-20MGL-PAX-3AGLOSSARY-TERM-29 "name"
  [5fd4]: http://www.lispworks.com/documentation/HyperSpec/Body/t_eql.htm "EQL (MGL-PAX:CLHS TYPE)"
  [6067]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#destructuring_lambda_list '"destructuring lambda list" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [609c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_fmakun.htm "FMAKUNBOUND (MGL-PAX:CLHS FUNCTION)"
  [62e7]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#dynamic_environment '"dynamic environment" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [6334]: #x-28DREF-EXT-3ALOCATE-ERROR-20CONDITION-29 "DREF-EXT:LOCATE-ERROR CONDITION"
  [63b4]: #x-28DREF-3ARESOLVE-20FUNCTION-29 "DREF:RESOLVE FUNCTION"
  [65b4]: #x-28DREF-3ADREF-APROPOS-20FUNCTION-29 "DREF:DREF-APROPOS FUNCTION"
  [66dc]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defset.htm "DEFSETF (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [6801]: #x-28DREF-EXT-3ADREF-LOCATIVE-TYPE-20FUNCTION-29 "DREF-EXT:DREF-LOCATIVE-TYPE FUNCTION"
  [68b4]: #x-28DREF-EXT-3ADEFINE-PSEUDO-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-PSEUDO-LOCATIVE-TYPE MGL-PAX:MACRO"
  [68fb]: #x-28DREF-EXT-3A-40EXTENDING-DREF-20MGL-PAX-3ASECTION-29 "Extending DRef"
  [6932]: #x-28DREF-EXT-3ALOCATE-ERROR-20FUNCTION-29 "DREF-EXT:LOCATE-ERROR FUNCTION"
  [6c83]: #x-28VARIABLE-20MGL-PAX-3ALOCATIVE-29 "VARIABLE MGL-PAX:LOCATIVE"
  [6d46]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_m.htm "FIND-METHOD (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [6e6e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_pkg.htm "MAKE-PACKAGE (MGL-PAX:CLHS FUNCTION)"
  [6ec3]: #x-28DREF-EXT-3ASOURCE-LOCATION-SNIPPET-20FUNCTION-29 "DREF-EXT:SOURCE-LOCATION-SNIPPET FUNCTION"
  [72b4]: ../README.md#x-28MGL-PAX-3ADEFSECTION-20MGL-PAX-3AMACRO-29 "MGL-PAX:DEFSECTION MGL-PAX:MACRO"
  [7334]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpar.htm "DEFVAR (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [76c4]: #x-28DREF-EXT-3ALOCATE-2A-20GENERIC-FUNCTION-29 "DREF-EXT:LOCATE* GENERIC-FUNCTION"
  [7a04]: #x-28DREF-3A-40TYPELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for Types and Declarations"
  [7ac8]: #x-28DREF-3A-40LOCATIVE-20MGL-PAX-3AGLOSSARY-TERM-29 "locative"
  [7c9f]: http://www.lispworks.com/documentation/HyperSpec/Body/d_type.htm "TYPE (MGL-PAX:CLHS DECLARATION)"
  [7e8c]: #x-28DREF-3A-40LOCATIVES-AND-REFERENCES-20MGL-PAX-3ASECTION-29 "Locatives and References"
  [7e92]: #x-28DREF-3ADREF-20FUNCTION-29 "DREF:DREF FUNCTION"
  [7f9a]: http://www.lispworks.com/documentation/HyperSpec/Body/m_deftp.htm "DEFTYPE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [811f]: #x-28DREF-3AMAP-NAMES-20GENERIC-FUNCTION-29 "DREF:MAP-NAMES GENERIC-FUNCTION"
  [817d]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#deftype_lambda_list '"deftype lambda list" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [81f7]: http://www.lispworks.com/documentation/HyperSpec/Body/s_fn.htm "FUNCTION (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [8269]: ../README.md#x-28MGL-PAX-3ADOCUMENT-OBJECT-2A-20GENERIC-FUNCTION-29 "MGL-PAX:DOCUMENT-OBJECT* GENERIC-FUNCTION"
  [83e1]: http://www.lispworks.com/documentation/HyperSpec/Body/e_cnd.htm "CONDITION (MGL-PAX:CLHS CONDITION)"
  [8490]: #x-28DREF-EXT-3AADD-DREF-ACTUALIZER-20FUNCTION-29 "DREF-EXT:ADD-DREF-ACTUALIZER FUNCTION"
  [882a]: #x-28DREF-EXT-3AXREF-LOCATIVE-TYPE-20FUNCTION-29 "DREF-EXT:XREF-LOCATIVE-TYPE FUNCTION"
  [8934]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defcon.htm "DEFCONSTANT (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [8f19]: #x-28DREF-3ALOCATE-20FUNCTION-29 "DREF:LOCATE FUNCTION"
  [926d]: #x-28TYPE-20MGL-PAX-3ALOCATIVE-29 "TYPE MGL-PAX:LOCATIVE"
  [94d1]: #x-28DREF-3ALOCATIVE-ALIASES-20FUNCTION-29 "DREF:LOCATIVE-ALIASES FUNCTION"
  [97ba]: #x-28DREF-EXT-3ALOCATIVE-TYPE-20FUNCTION-29 "DREF-EXT:LOCATIVE-TYPE FUNCTION"
  [99b0]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#setf_function '"setf function" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [99b0a]: #x-28DREF-3ALOCATIVE-TYPES-20FUNCTION-29 "DREF:LOCATIVE-TYPES FUNCTION"
  [9a71]: http://www.lispworks.com/documentation/HyperSpec/Body/f_specia.htm "SPECIAL-OPERATOR-P (MGL-PAX:CLHS FUNCTION)"
  [9b43]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm "DEFPACKAGE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [9b70]: http://www.lispworks.com/documentation/HyperSpec/Body/t_meth_1.htm "METHOD-COMBINATION (MGL-PAX:CLHS CLASS)"
  [9fd4]: #x-28DREF-EXT-3ADOCSTRING-2A-20GENERIC-FUNCTION-29 "DREF-EXT:DOCSTRING* GENERIC-FUNCTION"
  [a078]: #x-28DREF-EXT-3A-40SOURCE-LOCATIONS-20MGL-PAX-3ASECTION-29 "Source Locations"
  [a11d]: #x-28DREF-3A-40LOCATIVE-TYPE-20MGL-PAX-3AGLOSSARY-TERM-29 "locative type"
  [a138]: http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm "SETF (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [a26f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_consta.htm "CONSTANTP (MGL-PAX:CLHS FUNCTION)"
  [a541]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_to_.htm "PRINC-TO-STRING (MGL-PAX:CLHS FUNCTION)"
  [a951]: #x-28MGL-PAX-3AUNKNOWN-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:UNKNOWN MGL-PAX:LOCATIVE"
  [ad35]: #x-28DREF-EXT-3AVARIABLE-DREF-20CLASS-29 "DREF-EXT:VARIABLE-DREF CLASS"
  [ad78]: http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm "FORMAT (MGL-PAX:CLHS FUNCTION)"
  [ad80]: #x-28DREF-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29 "Introduction"
  [ade6]: #x-28DREF-3A-40PRESENTATION-20MGL-PAX-3AGLOSSARY-TERM-29 "presentation"
  [ae5a]: #x-28DREF-EXT-3ASOURCE-LOCATION-FILE-20FUNCTION-29 "DREF-EXT:SOURCE-LOCATION-FILE FUNCTION"
  [affc]: #x-28MGL-PAX-3ADOCSTRING-20FUNCTION-29 "MGL-PAX:DOCSTRING FUNCTION"
  [b3a7]: #x-28DREF-EXT-3ACLASS-DREF-20CLASS-29 "DREF-EXT:CLASS-DREF CLASS"
  [b6c4]: #x-28DREF-EXT-3ADEFINE-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-LOCATIVE-TYPE MGL-PAX:MACRO"
  [b88e]: #x-28DREF-EXT-3AXREF-NAME-20-28MGL-PAX-3AREADER-20DREF-3AXREF-29-29 "DREF-EXT:XREF-NAME (MGL-PAX:READER DREF:XREF)"
  [b93c]: http://www.lispworks.com/documentation/HyperSpec/Body/t_string.htm "STRING (MGL-PAX:CLHS CLASS)"
  [ba62]: #x-28FUNCTION-20MGL-PAX-3ALOCATIVE-29 "FUNCTION MGL-PAX:LOCATIVE"
  [bb23]: #x-28DREF-3ADEFINE-RESTART-20MGL-PAX-3AMACRO-29 "DREF:DEFINE-RESTART MGL-PAX:MACRO"
  [be18]: #x-28DREF-EXT-3ASOURCE-LOCATION-FILE-POSITION-20FUNCTION-29 "DREF-EXT:SOURCE-LOCATION-FILE-POSITION FUNCTION"
  [bf0f]: #x-28DREF-3A-40LOCATIVE-TYPES-20MGL-PAX-3ASECTION-29 "Locative Types"
  [c097]: #x-28ASDF-2FSYSTEM-3ASYSTEM-20MGL-PAX-3ALOCATIVE-29 "ASDF/SYSTEM:SYSTEM MGL-PAX:LOCATIVE"
  [c339]: #x-28DREF-3A-40PACKAGELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for Packages and Readtables"
  [c340]: #x-28DREF-3APSEUDO-LOCATIVE-TYPES-20FUNCTION-29 "DREF:PSEUDO-LOCATIVE-TYPES FUNCTION"
  [c5ae]: http://www.lispworks.com/documentation/HyperSpec/Body/f_docume.htm "DOCUMENTATION (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [c7f7]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defgen.htm "DEFGENERIC (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [c819]: #x-28MGL-PAX-3ACONSTANT-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:CONSTANT MGL-PAX:LOCATIVE"
  [c8c1]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_n.htm#namespace '"namespace" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [c930]: ../README.md#x-28MGL-PAX-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29 "MGL-PAX:EXPORTABLE-LOCATIVE-TYPE-P GENERIC-FUNCTION"
  [c938]: #x-28DREF-EXT-3ADREF-ORIGIN-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29 "DREF-EXT:DREF-ORIGIN (MGL-PAX:READER DREF:DREF)"
  [cc32]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_m.htm#macro_lambda_list '"macro lambda list" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [cda7]: #x-28DREF-3AXREF-20FUNCTION-29 "DREF:XREF FUNCTION"
  [cf08]: http://www.lispworks.com/documentation/HyperSpec/Body/r_use_va.htm "USE-VALUE (MGL-PAX:CLHS RESTART)"
  [d061]: #x-28DREF-3A-40GLOSSARY-20MGL-PAX-3ASECTION-29 "Glossary"
  [d07c]: http://www.lispworks.com/documentation/HyperSpec/Body/d_declar.htm "DECLARATION (MGL-PAX:CLHS DECLARATION)"
  [d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"
  [d2cb]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_3.htm "DEFINE-SETF-EXPANDER (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [d3b3]: #x-28DREF-EXT-3ARESOLVE-2A-20GENERIC-FUNCTION-29 "DREF-EXT:RESOLVE* GENERIC-FUNCTION"
  [d3e1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_procla.htm "PROCLAIM (MGL-PAX:CLHS FUNCTION)"
  [d45d]: #x-28DREF-3A-40MACROLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for Macros"
  [d5a2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_car_c.htm "CAR (MGL-PAX:CLHS FUNCTION)"
  [d646]: http://www.lispworks.com/documentation/HyperSpec/Body/t_rdtabl.htm "READTABLE (MGL-PAX:CLHS CLASS)"
  [d6c7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_unionc.htm "UNION (MGL-PAX:CLHS FUNCTION)"
  [d83a]: #x-28SETF-20MGL-PAX-3ALOCATIVE-29 "SETF MGL-PAX:LOCATIVE"
  [d930]: #x-28DREF-3ADREF-20CLASS-29 "DREF:DREF CLASS"
  [da93]: #x-28DREF-3A-40DREF-LOCATIVES-20MGL-PAX-3ASECTION-29 "DRef Locatives"
  [daac]: #x-28DREF-EXT-3ASOURCE-LOCATION-ADJUSTED-FILE-POSITION-20FUNCTION-29 "DREF-EXT:SOURCE-LOCATION-ADJUSTED-FILE-POSITION FUNCTION"
  [dae6]: http://www.lispworks.com/documentation/HyperSpec/Body/f_string.htm "STRING (MGL-PAX:CLHS FUNCTION)"
  [db03]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eql.htm "EQL (MGL-PAX:CLHS FUNCTION)"
  [db68]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pkg_na.htm "PACKAGE-NAME (MGL-PAX:CLHS FUNCTION)"
  [dd55]: http://www.lispworks.com/documentation/HyperSpec/Body/t_and.htm "AND (MGL-PAX:CLHS TYPE)"
  [e196]: #x-28DREF-3ADEFINITIONS-20FUNCTION-29 "DREF:DEFINITIONS FUNCTION"
  [e1d4]: #x-28DREF-3A-40LISTING-DEFINITIONS-20MGL-PAX-3ASECTION-29 "Listing Definitions"
  [e576]: #x-28DREF-EXT-3AFUNCTION-DREF-20CLASS-29 "DREF-EXT:FUNCTION-DREF CLASS"
  [e5ab]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_3.htm "SYMBOL-PACKAGE (MGL-PAX:CLHS FUNCTION)"
  [e5af]: http://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm "SYMBOL (MGL-PAX:CLHS CLASS)"
  [e6bd]: #x-28DREF-3AARGLIST-20FUNCTION-29 "DREF:ARGLIST FUNCTION"
  [e7ee]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*STANDARD-OUTPUT* (MGL-PAX:CLHS VARIABLE)"
  [eac1]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defstr.htm "DEFSTRUCT (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [ead6]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defcla.htm "DEFCLASS (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [ebea]: http://www.lispworks.com/documentation/HyperSpec/Body/m_declai.htm "DECLAIM (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [ee40]: #x-28DREF-EXT-3ADREF-2A-20GENERIC-FUNCTION-29 "DREF-EXT:DREF* GENERIC-FUNCTION"
  [ee9b]: #x-28DREF-EXT-3ADEFINE-SYMBOL-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-SYMBOL-LOCATIVE-TYPE MGL-PAX:MACRO"
  [efe2]: http://www.lispworks.com/documentation/HyperSpec/Body/t_generi.htm "GENERIC-FUNCTION (MGL-PAX:CLHS CLASS)"
  [f3cc]: #x-28MGL-PAX-3AMACRO-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:MACRO MGL-PAX:LOCATIVE"
  [f472]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm "DEFUN (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [f486]: #x-28DREF-EXT-3AXREF-LOCATIVE-20-28MGL-PAX-3AREADER-20DREF-3AXREF-29-29 "DREF-EXT:XREF-LOCATIVE (MGL-PAX:READER DREF:XREF)"
  [f70b]: #x-28DREF-3ARESOLVE-ERROR-20FUNCTION-29 "DREF:RESOLVE-ERROR FUNCTION"
  [fe9f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rest.htm "REST (MGL-PAX:CLHS FUNCTION)"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
