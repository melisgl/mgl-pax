<a id='x-28MGL-PAX-TEST-3A-3A-40TEST-20MGL-PAX-3ASECTION-29'></a>

# @TEST

## Table of Contents

- 1 @TEST-EXAMPLES
- [2 test other title][22dd]
- [3 Link to @TEST-OTHER][1dbc]
- [4 Link to @TEST][2820]
- [5 \`CODE\` \*italic\* \_italic2\_ \*bold\* \[link\]\[sdf\] \<thing\>][d484]

###### \[in package MGL-PAX-TEST\]
[`*TEST-VARIABLE*`][c066]
[`*TEST-VARIABLE*`][c066]
[`*test-variable*`][c066]
[`*test-variable*`][c066]
[mgl-pax-test:*test-variable*][]
[`FOO`][4244] function, function [`FOO`][4244],
[`FOO`][4244] function, function [`FOO`][4244],
[`FOO`][4244] `function`, `function` [`FOO`][4244],
[`FOO`][4244] `function`, `function` [`FOO`][4244],
[`foo`][4244],
[`foo`][4244],
[`FOO`][4244],
[`FOO`][4244],
[`foo`][4244],
[`foo`][4244],
[`FOO`][4244],
[`FOO`][4244],

[`FOO-A`][6483] `(accessor foo)`, `(accessor foo)` [`FOO-A`][6483],
[`FOO-A`][6483] `(accessor foo)`, `(accessor foo)` [`FOO-A`][6483],
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
Non escaped: `FOO`([`0`][f1a9] [`1`][4244]) [`*TEST-VARIABLE*`][c066]
[test other title][22dd]

This should be no link because the page of `@TEST-EXAMPLES`
has `:URI-FRAGMENT` `NIL`.

This is code: `T`

See
[`FOO`][f1a9] compiler-macro

See [`FOO`][f1a9]
compiler-macro

See
compiler-macro [`FOO`][f1a9]

See compiler-macro
[`FOO`][f1a9]

See
compiler-macro 
[`FOO`][f1a9]

See
`FOO`([`0`][f1a9] [`1`][4244])

```cl-transcript
(values (print (1+ 2)) :aaa)
..
.. 3 
=> 3
=> :AAA

```

```cl-transcript
(values '(1 2) '(3 4))
;=> (1 2)
;=> (3
;->  4)

```

```cl-transcript
(make-array 12 :initial-element 0d0)
=> #(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
     0.0d0)

```

In documentation, when the only ambiguity is between a generic
function and its methods, it's resolved in favor if the gf:
[`TEST-GF`][efc1].

<a id='x-28MGL-PAX-TEST-3A-3AFOO-20FUNCTION-29'></a>

- [function] **FOO** 

<a id='x-28MGL-PAX-TEST-3A-3AFOO-20-28COMPILER-MACRO-29-29'></a>

- [compiler-macro] **FOO** 

<a id='x-28MGL-PAX-TEST-3A-3AFOO-A-20-28MGL-PAX-3AACCESSOR-20MGL-PAX-TEST-3A-3AFOO-29-29'></a>

- [accessor] **FOO-A** *FOO*

<a id='x-28MGL-PAX-TEST-3A-3A-2ATEST-VARIABLE-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*TEST-VARIABLE\*** *(XXX 34)*



<a id='x-28MGL-PAX-TEST-3A-3ATEST-GF-20GENERIC-FUNCTION-29'></a>

- [generic-function] **TEST-GF** *X*

<a id='x-28MGL-PAX-TEST-3A-3ATEST-GF-20-28METHOD-20NIL-20-28NUMBER-29-29-29'></a>

- [method] **TEST-GF** *(X NUMBER)*

<a id='x-28MGL-PAX-TEST-3A-40TEST-SECTION-WITH-LINK-TO-OTHER-PAGE-IN-TITLE-20MGL-PAX-3ASECTION-29'></a>

## 3 Link to @TEST-OTHER

Same link in docstring to [test other title][22dd].

<a id='x-28MGL-PAX-TEST-3A-40TEST-SECTION-WITH-LINK-TO-SAME-PAGE-IN-TITLE-20MGL-PAX-3ASECTION-29'></a>

## 4 Link to @TEST

Same link in docstring to [@TEST][a755].

<a id='x-28MGL-PAX-TEST-3A-3A-40TEST-TRICKY-TITLE-20MGL-PAX-3ASECTION-29'></a>

## 5 \`CODE\` \*italic\* \_italic2\_ \*bold\* \[link\]\[sdf\] \<thing\>

backlink [@TEST][a755]

  [1dbc]: #x-28MGL-PAX-TEST-3A-40TEST-SECTION-WITH-LINK-TO-OTHER-PAGE-IN-TITLE-20MGL-PAX-3ASECTION-29 "Link to @TEST-OTHER"
  [22dd]: other/test-other.md#x-28MGL-PAX-TEST-3A-3A-40TEST-OTHER-20MGL-PAX-3ASECTION-29 "test other title"
  [2820]: #x-28MGL-PAX-TEST-3A-40TEST-SECTION-WITH-LINK-TO-SAME-PAGE-IN-TITLE-20MGL-PAX-3ASECTION-29 "Link to @TEST"
  [4244]: #x-28MGL-PAX-TEST-3A-3AFOO-20FUNCTION-29 "(MGL-PAX-TEST::FOO FUNCTION)"
  [6483]: #x-28MGL-PAX-TEST-3A-3AFOO-A-20-28MGL-PAX-3AACCESSOR-20MGL-PAX-TEST-3A-3AFOO-29-29 "(MGL-PAX-TEST::FOO-A (MGL-PAX:ACCESSOR MGL-PAX-TEST::FOO))"
  [a755]: #x-28MGL-PAX-TEST-3A-3A-40TEST-20MGL-PAX-3ASECTION-29 "MGL-PAX-TEST::@TEST"
  [c066]: #x-28MGL-PAX-TEST-3A-3A-2ATEST-VARIABLE-2A-20-28VARIABLE-29-29 "(MGL-PAX-TEST::*TEST-VARIABLE* (VARIABLE))"
  [d484]: #x-28MGL-PAX-TEST-3A-3A-40TEST-TRICKY-TITLE-20MGL-PAX-3ASECTION-29 "`CODE` *italic* _italic2_ *bold* [link][sdf] <thing>"
  [efc1]: #x-28MGL-PAX-TEST-3A-3ATEST-GF-20GENERIC-FUNCTION-29 "(MGL-PAX-TEST::TEST-GF GENERIC-FUNCTION)"
  [f1a9]: #x-28MGL-PAX-TEST-3A-3AFOO-20-28COMPILER-MACRO-29-29 "(MGL-PAX-TEST::FOO (COMPILER-MACRO))"
