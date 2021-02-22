<a id='x-28MGL-PAX-TEST-3A-3A-40TEST-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

# @TEST

## Table of Contents

- 1 @TEST-EXAMPLES
- [2 test other title][2429]
- [3 Link to @TEST-OTHER][ca9d]
- [4 Link to @TEST][82a6]
- [5 \`CODE\` \*italic\* \_italic2\_ \*bold\* \[link\]\[sdf\] \<thing\>][b232]

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

[`FOO-A`][9c76] `(accessor foo)`, `(accessor foo)` [`FOO-A`][9c76],
[`FOO-A`][9c76] `(accessor foo)`, `(accessor foo)` [`FOO-A`][9c76],
[`foo-a`][9c76],
[`foo-a`][9c76],
[`FOO-A`][9c76],
[`FOO-A`][9c76],
[`foo-a`][9c76],
[`foo-a`][9c76],
[`FOO-A`][9c76],
[`FOO-A`][9c76]

`->MAX`

Escaped: FOO `FOO` *NAVIGATION-TEST-CASES*
Non escaped: `FOO`([`0`][f1a9] [`1`][4244]) [`*TEST-VARIABLE*`][c066]
[test other title][2429]

This should be no link because the page of `@TEST-EXAMPLES`
has `:URI-FRAGMENT` `NIL`.

This is code: `T`

Plural uppercase ambiguous symbol: see `FOO`([`0`][f1a9] [`1`][4244])s

Plural uppercase symbol: [`TEST-GF`][efc1]s

Plural uppercase dislocated symbol: `->MAX`s

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

<a id='x-28MGL-PAX-TEST-3A-3AFOO-A-20-28MGL-PAX-MINIMAL-3AACCESSOR-20MGL-PAX-TEST-3A-3AFOO-29-29'></a>

- [accessor] **FOO-A** *FOO*

<a id='x-28MGL-PAX-TEST-3A-3A-2ATEST-VARIABLE-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*TEST-VARIABLE\*** *(XXX 34)*



<a id='x-28MGL-PAX-TEST-3A-3ATEST-GF-20GENERIC-FUNCTION-29'></a>

- [generic-function] **TEST-GF** *X*

<a id='x-28MGL-PAX-TEST-3A-3ATEST-GF-20-28METHOD-20NIL-20-28NUMBER-29-29-29'></a>

- [method] **TEST-GF** *(X NUMBER)*

<a id='x-28MGL-PAX-TEST-3A-3ATEST-GF-20-28METHOD-20NIL-20-28-28EQL-207-29-29-29-29'></a>

- [method] **TEST-GF** *(X (EQL 7))*

<a id='x-28MGL-PAX-TEST-3A-40TEST-SECTION-WITH-LINK-TO-OTHER-PAGE-IN-TITLE-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 3 Link to @TEST-OTHER

Same link in docstring to [test other title][2429].

<a id='x-28MGL-PAX-TEST-3A-40TEST-SECTION-WITH-LINK-TO-SAME-PAGE-IN-TITLE-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 4 Link to @TEST

Same link in docstring to [@TEST][32b2].

<a id='x-28MGL-PAX-TEST-3A-3A-40TEST-TRICKY-TITLE-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 5 \`CODE\` \*italic\* \_italic2\_ \*bold\* \[link\]\[sdf\] \<thing\>

backlink [@TEST][32b2]

  [2429]: other/test-other.md#x-28MGL-PAX-TEST-3A-3A-40TEST-OTHER-20MGL-PAX-MINIMAL-3ASECTION-29 "test other title"
  [32b2]: #x-28MGL-PAX-TEST-3A-3A-40TEST-20MGL-PAX-MINIMAL-3ASECTION-29 "MGL-PAX-TEST::@TEST"
  [4244]: #x-28MGL-PAX-TEST-3A-3AFOO-20FUNCTION-29 "(MGL-PAX-TEST::FOO FUNCTION)"
  [82a6]: #x-28MGL-PAX-TEST-3A-40TEST-SECTION-WITH-LINK-TO-SAME-PAGE-IN-TITLE-20MGL-PAX-MINIMAL-3ASECTION-29 "Link to @TEST"
  [9c76]: #x-28MGL-PAX-TEST-3A-3AFOO-A-20-28MGL-PAX-MINIMAL-3AACCESSOR-20MGL-PAX-TEST-3A-3AFOO-29-29 "(MGL-PAX-TEST::FOO-A (MGL-PAX-MINIMAL:ACCESSOR MGL-PAX-TEST::FOO))"
  [b232]: #x-28MGL-PAX-TEST-3A-3A-40TEST-TRICKY-TITLE-20MGL-PAX-MINIMAL-3ASECTION-29 "`CODE` *italic* _italic2_ *bold* [link][sdf] <thing>"
  [c066]: #x-28MGL-PAX-TEST-3A-3A-2ATEST-VARIABLE-2A-20-28VARIABLE-29-29 "(MGL-PAX-TEST::*TEST-VARIABLE* (VARIABLE))"
  [ca9d]: #x-28MGL-PAX-TEST-3A-40TEST-SECTION-WITH-LINK-TO-OTHER-PAGE-IN-TITLE-20MGL-PAX-MINIMAL-3ASECTION-29 "Link to @TEST-OTHER"
  [efc1]: #x-28MGL-PAX-TEST-3A-3ATEST-GF-20GENERIC-FUNCTION-29 "(MGL-PAX-TEST::TEST-GF GENERIC-FUNCTION)"
  [f1a9]: #x-28MGL-PAX-TEST-3A-3AFOO-20-28COMPILER-MACRO-29-29 "(MGL-PAX-TEST::FOO (COMPILER-MACRO))"
