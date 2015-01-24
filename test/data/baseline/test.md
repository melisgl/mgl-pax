<a name='x-28MGL-PAX-TEST-3A-3A-40TEST-20MGL-PAX-3ASECTION-29'></a>

# @TEST

## Table of Contents

- 1 @TEST-EXAMPLES
- [2 test other title][22dd]

###### \[in package MGL-PAX-TEST\]
[`*NAVIGATION-TEST-CASES*`][799d]
[`*NAVIGATION-TEST-CASES*`][799d]
[`*navigation-test-cases*`][799d]
[`*navigation-test-cases*`][799d]
[mgl-pax-test:*navigation-test-cases*][]
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
Non escaped: `FOO`([`0`][4ded] [`1`][4244]) [`*NAVIGATION-TEST-CASES*`][799d]
[test other title][22dd]

This should be no link because the page of `@TEST-EXAMPLES`
has `:URI-FRAGMENT` `NIL`.

This is code: `T`

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
`FOO`([`0`][4ded] [`1`][4244])

In documentation, when the only ambiguity is between a generic
function and its methods, it's resolved in favor if the gf:
[`TEST-GF`][efc1].

<a name='x-28MGL-PAX-TEST-3A-3AFOO-20FUNCTION-29'></a>

- [function] **FOO** 

<a name='x-28MGL-PAX-TEST-3A-3AFOO-20COMPILER-MACRO-29'></a>

- [compiler-macro] **FOO** 

<a name='x-28MGL-PAX-TEST-3A-3AFOO-A-20-28MGL-PAX-3AACCESSOR-20MGL-PAX-TEST-3A-3AFOO-29-29'></a>

- [accessor] **FOO-A** *FOO*

<a name='x-28MGL-PAX-TEST-3A-3A-2ANAVIGATION-TEST-CASES-2A-20VARIABLE-29'></a>

- [variable] **\*NAVIGATION-TEST-CASES\*** *((FOO FUNCTION (DEFUN FOO)) (FOO TYPE (DEFCLASS FOO))
 (FOO CLASS (DEFCLASS FOO)) (FOO COMPILER-MACRO (DEFINE-COMPILER-MACRO FOO))
 (FOO-A (ACCESSOR FOO) (DEFCLASS FOO)) (FOO-R (READER FOO) (DEFCLASS FOO))
 (FOO-W (WRITER FOO) (DEFCLASS FOO)) (FOO-A VARIABLE (DEFVAR FOO-A))
 (FOO-B VARIABLE (DEFVAR FOO-B)) (FOO-C VARIABLE (DEFVAR FOO-C))
 (BAR MACRO (DEFMACRO BAR)) (BAR TYPE (DEFTYPE BAR))
 (BAR CONSTANT (DEFCONSTANT BAR)) (BAZ GENERIC-FUNCTION (DEFGENERIC BAZ))
 (BAZ VARIABLE (DEFVAR BAZ))
 (@MGL-PAX-MANUAL SECTION (DEFSECTION @MGL-PAX-MANUAL))
 (BAZ-AAA STRUCTURE-ACCESSOR (DEFSTRUCT BAZ))
 (MGL-PAX PACKAGE (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)))
 (MGL-PAX ASDF/SYSTEM:SYSTEM NIL)
 (TEST-GF GENERIC-FUNCTION (DEFGENERIC TEST-GF))
 (TEST-GF (METHOD NIL (NUMBER)) (DEFMETHOD TEST-GF)))*



<a name='x-28MGL-PAX-TEST-3A-3ATEST-GF-20GENERIC-FUNCTION-29'></a>

- [generic-function] **TEST-GF** *X*

<a name='x-28MGL-PAX-TEST-3A-3ATEST-GF-20-28METHOD-20NIL-20-28NUMBER-29-29-29'></a>

- [method] **TEST-GF** *(X NUMBER)*

  [22dd]: other/test-other.md#x-28MGL-PAX-TEST-3A-3A-40TEST-OTHER-20MGL-PAX-3ASECTION-29 "(MGL-PAX-TEST::@TEST-OTHER MGL-PAX:SECTION)"
  [4244]: #x-28MGL-PAX-TEST-3A-3AFOO-20FUNCTION-29 "(MGL-PAX-TEST::FOO FUNCTION)"
  [4ded]: #x-28MGL-PAX-TEST-3A-3AFOO-20COMPILER-MACRO-29 "(MGL-PAX-TEST::FOO COMPILER-MACRO)"
  [6483]: #x-28MGL-PAX-TEST-3A-3AFOO-A-20-28MGL-PAX-3AACCESSOR-20MGL-PAX-TEST-3A-3AFOO-29-29 "(MGL-PAX-TEST::FOO-A (MGL-PAX:ACCESSOR MGL-PAX-TEST::FOO))"
  [799d]: #x-28MGL-PAX-TEST-3A-3A-2ANAVIGATION-TEST-CASES-2A-20VARIABLE-29 "(MGL-PAX-TEST::*NAVIGATION-TEST-CASES* VARIABLE)"
  [efc1]: #x-28MGL-PAX-TEST-3A-3ATEST-GF-20GENERIC-FUNCTION-29 "(MGL-PAX-TEST::TEST-GF GENERIC-FUNCTION)"
