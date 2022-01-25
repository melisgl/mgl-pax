<a id='x-28MGL-PAX-TEST-3A-3A-40TEST-20MGL-PAX-3ASECTION-29'></a>

# @TEST

## Table of Contents

- 1 @TEST-EXAMPLES
- [2 test other title][22dd]
- [3 Link to @TEST-OTHER][1dbc]
- [4 Link to @TEST][2820]
- [5 \`CODE\` \*italic\* \_italic2\_ \*bold\* \[link\]\[sdf\] \<thing\>][d484]
- [6 MGL-PAX-TEST::@STEALING-FROM-OTHER-PACKAGE][a48a]

###### \[in package MGL-PAX-TEST\]
[`*TEST-VARIABLE*`][1037]

[`*TEST-VARIABLE*`][1037]

[`*test-variable*`][1037]

[`*test-variable*`][1037]

[mgl-pax-test:*test-variable*][]

[`FOO`][4244] function,

function [`FOO`][4244],

[`FOO`][4244] function,

function [`FOO`][4244],

[`FOO`][4244] `function`,

`function` [`FOO`][4244],

[`FOO`][4244] `function`,

`function` [`FOO`][4244],

[`foo`][4244],

[`foo`][4244],

[`FOO`][4244],

[`FOO`][4244],

[`foo`][4244],

[`foo`][4244],

[`FOO`][4244],

[`FOO`][4244],

[`FOO-A`][6483] `(accessor foo)`,

`(accessor foo)` [`FOO-A`][6483],

[`FOO-A`][6483] `(accessor foo)`,

`(accessor foo)` [`FOO-A`][6483],

[`foo-a`][6483],

[`foo-a`][6483],

[`FOO-A`][6483],

[`FOO-A`][6483],

[`foo-a`][6483],

[`foo-a`][6483],

[`FOO-A`][6483],

[`FOO-A`][6483]

`->MAX`

Escaped: FOO `FOO` *NAVIGATION-TEST-CASES*
Non escaped: `FOO`([`0`][22be] [`1`][4ded] [`2`][4244]) [`*TEST-VARIABLE*`][1037]
[test other title][22dd]

This should be no link because the page of `@TEST-EXAMPLES`
has `:URI-FRAGMENT` `NIL`.

This is code: `T`

Plural uppercase ambiguous symbol: see `FOO`([`0`][22be] [`1`][4ded] [`2`][4244])s

Plural uppercase symbol: [`TEST-GF`][efc1]s

Plural uppercase dislocated symbol: `->MAX`s

See
[`FOO`][4ded] compiler-macro

See [`FOO`][4ded]
compiler-macro

See
compiler-macro [`FOO`][4ded]

See compiler-macro
[`FOO`][4ded]

See
compiler-macro 
[`FOO`][4ded]

See
`FOO`([`0`][22be] [`1`][4ded] [`2`][4244])

```common-lisp
(values (print (1+ 2)) :aaa)
..
.. 3 
=> 3
=> :AAA
```

```common-lisp
(values '(1 2) '(3 4))
;=> (1 2)
;=> (3
;->  4)
```

```common-lisp
(make-array 12 :initial-element 0d0)
=> #(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
     0.0d0)
```

In documentation, when the only ambiguity is between a generic
function and its methods, it's resolved in favor if the gf:
[`TEST-GF`][efc1].

<a id='x-28MGL-PAX-TEST-3A-3AFOO-20FUNCTION-29'></a>

- [function] **FOO** *OOK X*

    `FOO` has args `OOK` and `X`.
    
    This function [`FOO`][4244] is related to compiler-macro [`FOO`][4ded].
    
    Or [`foo`][4ded], if you prefer.
    
    Now, `foo`([`0`][22be] [`1`][4ded]) should link to [`foo`][4ded] and [`foo`][22be]
    but not to [`foo`][4244].

<a id='x-28MGL-PAX-TEST-3A-3AFOO-20COMPILER-MACRO-29'></a>

- [compiler-macro] **FOO** *OOK X*

    Docstring of a compiler macro.

<a id='x-28MGL-PAX-TEST-3A-3AFOO-20CLASS-29'></a>

- [class] **FOO**

[`FOO`][22be] instance

and [`FOO`][22be] object

type-of [`BAR`][cece]

<a id='x-28MGL-PAX-TEST-3A-3AFOO-A-20-28MGL-PAX-3AACCESSOR-20MGL-PAX-TEST-3A-3AFOO-29-29'></a>

- [accessor] **FOO-A** *FOO*

<a id='x-28MGL-PAX-TEST-3A-3ABAR-20MGL-PAX-3AMACRO-29'></a>

- [macro] **BAR** *X Y &KEY (Z 7)*

    `BAR` has args `X`, `Y` and `Z`.

<a id='x-28MGL-PAX-TEST-3A-3ABAR-20TYPE-29'></a>

- [type] **BAR** *X &REST R*

    `BAR` has args `X` and `R`.

<a id='x-28MGL-PAX-TEST-3A-3ABAR-20MGL-PAX-3ACONSTANT-29'></a>

- [constant] **BAR** *2*

    `BAR` is not a link.

<a id='x-28MGL-PAX-TEST-3A-3ABAZ-20TYPE-29'></a>

- [type] **BAZ**

<a id='x-28MGL-PAX-TEST-3A-3A-2ATEST-VARIABLE-2A-20VARIABLE-29'></a>

- [variable] **\*TEST-VARIABLE\*** *(XXX 34)*

    `*TEST-VARIABLE*` is not a link.

<a id='x-28MGL-PAX-TEST-3A-3A-2ASOME-VAR-2A-20-28VARIABLE-20-28QUOTE-20MGL-PAX-TEST-3A-3A-2ANEEDS-MARKDOWN-ESCAPE-2A-29-29-29'></a>

- [variable] **\*SOME-VAR\*** *'\*NEEDS-MARKDOWN-ESCAPE\**

<a id='x-28MGL-PAX-TEST-3A-3ASOME-RESTART-20RESTART-29'></a>

- [restart] **SOME-RESTART** *ARG1*

    This is `SOME-RESTART` with `ARG1`.

<a id='x-28MGL-PAX-TEST-3A-3AMY-ERROR-20CONDITION-29'></a>

- [condition] **MY-ERROR** *ERROR*

    This is `MY-ERROR`.



<a id='x-28MGL-PAX-TEST-3A-3ATEST-GF-20GENERIC-FUNCTION-29'></a>

- [generic-function] **TEST-GF** *X*

    `TEST-GF` is not a link.

<a id='x-28MGL-PAX-TEST-3A-3ATEST-GF-20-28METHOD-20NIL-20-28NUMBER-29-29-29'></a>

- [method] **TEST-GF** *(X NUMBER)*

    [`TEST-GF`][efc1] links to the generic function. `X` is not a link.

<a id='x-28MGL-PAX-TEST-3A-3ATEST-GF-20-28METHOD-20NIL-20-28-28EQL-207-29-29-29-29'></a>

- [method] **TEST-GF** *(X (EQL 7))*

<a id='x-28MGL-PAX-TEST-3A-3ASOME-TERM-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **SOME-TERM**

    `SOME-TERM` is not a link.

<a id='x-28MGL-PAX-TEST-3A-40TEST-SECTION-WITH-LINK-TO-OTHER-PAGE-IN-TITLE-20MGL-PAX-3ASECTION-29'></a>

## 3 Link to @TEST-OTHER

Same link in docstring to [test other title][22dd].

<a id='x-28MGL-PAX-TEST-3A-40TEST-SECTION-WITH-LINK-TO-SAME-PAGE-IN-TITLE-20MGL-PAX-3ASECTION-29'></a>

## 4 Link to @TEST

Same link in docstring to [@TEST][a755].

<a id='x-28MGL-PAX-TEST-3A-3A-40TEST-TRICKY-TITLE-20MGL-PAX-3ASECTION-29'></a>

## 5 \`CODE\` \*italic\* \_italic2\_ \*bold\* \[link\]\[sdf\] \<thing\>

backlink [@TEST][a755]

<a id='x-28MGL-PAX-TEST-3A-3A-40STEALING-FROM-OTHER-PACKAGE-20MGL-PAX-3ASECTION-29'></a>

## 6 MGL-PAX-TEST::@STEALING-FROM-OTHER-PACKAGE

###### \[in package MGL-PAX with nicknames PAX\]
<a id='x-28METHOD-20MGL-PAX-3ALOCATIVE-29'></a>

- [locative] **METHOD** *METHOD-QUALIFIERS METHOD-SPECIALIZERS*

    See `CL:FIND-METHOD` for the description of the arguments
    `METHOD-QUALIFIERS` and `METHOD-SPECIALIZERS`. For example,
    a `(FOO (METHOD () (T (EQL XXX))))` as a `DEFSECTION` entry refers to
    this method:
    
        (defmethod foo (x (y (eql 'xxx)))
          ...)
    
    `METHOD` is not `EXPORTABLE-LOCATIVE-TYPE-P`.

<a id='x-28MGL-PAX-TEST-3A-3AFUNCTION-WITH-OPTIONAL-ARGS-20FUNCTION-29'></a>

- [function] **FUNCTION-WITH-OPTIONAL-ARGS** *X &OPTIONAL O1 (O2 7)*

<a id='x-28MGL-PAX-TEST-3A-3AFUNCTION-WITH-KEYWORD-ARGS-20FUNCTION-29'></a>

- [function] **FUNCTION-WITH-KEYWORD-ARGS** *X &KEY K1 (K2 14) (K3 21 K3P)*

<a id='x-28MGL-PAX-TEST-3A-3AENCAPSULATED-FUNCTION-20FUNCTION-29'></a>

- [function] **ENCAPSULATED-FUNCTION** *X &REST ARGS*

    This may be encapsulated by `TRACE`.

<a id='x-28MGL-PAX-TEST-3A-3AENCAPSULATED-GENERIC-FUNCTION-20GENERIC-FUNCTION-29'></a>

- [generic-function] **ENCAPSULATED-GENERIC-FUNCTION** *X*

    This may also be encapsulated by `TRACE`.

  [1037]: #x-28MGL-PAX-TEST-3A-3A-2ATEST-VARIABLE-2A-20VARIABLE-29 "(MGL-PAX-TEST::*TEST-VARIABLE* VARIABLE)"
  [1dbc]: #x-28MGL-PAX-TEST-3A-40TEST-SECTION-WITH-LINK-TO-OTHER-PAGE-IN-TITLE-20MGL-PAX-3ASECTION-29 "Link to @TEST-OTHER"
  [22be]: #x-28MGL-PAX-TEST-3A-3AFOO-20CLASS-29 "(MGL-PAX-TEST::FOO CLASS)"
  [22dd]: other/test-other.md#x-28MGL-PAX-TEST-3A-3A-40TEST-OTHER-20MGL-PAX-3ASECTION-29 "test other title"
  [2820]: #x-28MGL-PAX-TEST-3A-40TEST-SECTION-WITH-LINK-TO-SAME-PAGE-IN-TITLE-20MGL-PAX-3ASECTION-29 "Link to @TEST"
  [4244]: #x-28MGL-PAX-TEST-3A-3AFOO-20FUNCTION-29 "(MGL-PAX-TEST::FOO FUNCTION)"
  [4ded]: #x-28MGL-PAX-TEST-3A-3AFOO-20COMPILER-MACRO-29 "(MGL-PAX-TEST::FOO COMPILER-MACRO)"
  [6483]: #x-28MGL-PAX-TEST-3A-3AFOO-A-20-28MGL-PAX-3AACCESSOR-20MGL-PAX-TEST-3A-3AFOO-29-29 "(MGL-PAX-TEST::FOO-A (MGL-PAX:ACCESSOR MGL-PAX-TEST::FOO))"
  [a48a]: #x-28MGL-PAX-TEST-3A-3A-40STEALING-FROM-OTHER-PACKAGE-20MGL-PAX-3ASECTION-29 "MGL-PAX-TEST::@STEALING-FROM-OTHER-PACKAGE"
  [a755]: #x-28MGL-PAX-TEST-3A-3A-40TEST-20MGL-PAX-3ASECTION-29 "MGL-PAX-TEST::@TEST"
  [cece]: #x-28MGL-PAX-TEST-3A-3ABAR-20TYPE-29 "(MGL-PAX-TEST::BAR TYPE)"
  [d484]: #x-28MGL-PAX-TEST-3A-3A-40TEST-TRICKY-TITLE-20MGL-PAX-3ASECTION-29 "`CODE` *italic* _italic2_ *bold* [link][sdf] <thing>"
  [efc1]: #x-28MGL-PAX-TEST-3A-3ATEST-GF-20GENERIC-FUNCTION-29 "(MGL-PAX-TEST::TEST-GF GENERIC-FUNCTION)"
