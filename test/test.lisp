(in-package :mgl-pax-test)

;;; Make Allegro record lambda lists, from which we can extract
;;; default values of arguments.
#+allegro
(eval-when (:compile-toplevel)
  (declaim (optimize (debug 3))))

(mgl-pax:define-locative-alias instance class)
(mgl-pax:define-locative-alias object class)
(mgl-pax:define-locative-alias type-of type)

(defsection @test (:export nil)
  "[*TEST-VARIABLE*][]"
  "[`*TEST-VARIABLE*`][]"
  "[*test-variable*][]"
  "[`*test-variable*`][]"
  "[mgl-pax-test:*test-variable*][]"
  "FOO function,"
  "function FOO,"
  "`FOO` function,"
  "function `FOO`,"
  "FOO `function`,"
  "`function` FOO,"
  "`FOO` `function`,"
  "`function` `FOO`,"
  "[foo][function],"
  "[foo][FUNCTION],"
  "[FOO][function],"
  "[FOO][FUNCTION],"
  "[`foo`][function],"
  "[`foo`][FUNCTION],"
  "[`FOO`][function],"
  "[`FOO`][FUNCTION],"

  "FOO-A `(accessor foo)`,"
  "`(accessor foo)` FOO-A,"
  "`FOO-A` `(accessor foo)`,"
  "`(accessor foo)` `FOO-A`,"
  "[foo-a][(accessor foo)],"
  "[foo-a][(ACCESSOR FOO)],"
  "[FOO-A][(accessor foo)],"
  "[FOO-A][(ACCESSOR FOO)],"
  "[`foo-a`][(accessor foo)],"
  "[`foo-a`][(ACCESSOR FOO)],"
  "[`FOO-A`][(accessor foo)],"
  "[`FOO-A`][(ACCESSOR FOO)]

  ->MAX

  Escaped: \\FOO [`FOO`][dislocated] *\\NAVIGATION-TEST-CASES*
  Non escaped: FOO *TEST-VARIABLE*
  @TEST-OTHER

  This should be no link because the page of @TEST-EXAMPLES
  has :URI-FRAGMENT NIL.

  This is code: T"

  "Plural uppercase ambiguous symbol: see FOOs"
  "Plural uppercase symbol: TEST-GFs"
  "Plural uppercase dislocated symbol: ->MAXs"

  "See
  FOO compiler-macro"
  "See FOO
  compiler-macro"
  "See
  compiler-macro FOO"
  "See compiler-macro
  FOO"
  "See
  compiler-macro 
  FOO"

  "See
  FOO"

  "```cl-transcript
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
  TEST-GF."
  (foo function)
  (foo compiler-macro)
  (foo class)
  ;; aliases defined above
  "FOO instance" "and FOO object"
  "type-of BAR"
  (foo-a (accessor foo))
  (bar macro)
  (bar type)
  (bar constant)
  (baz type)
  (*test-variable* variable)
  (*some-var* (variable '*needs-markdown-escape*))
  (some-restart restart)
  (my-error condition)
  (@test-examples section)
  (@test-other section)
  (test-gf generic-function)
  (test-gf (method () (number)))
  (test-gf (method () ((eql 7))))
  (some-term glossary-term)
  (@test-section-with-link-to-other-page-in-title section)
  (@test-section-with-link-to-same-page-in-title section)
  (@test-tricky-title section)
  (@stealing-from-other-package section)
  (function-with-optional-args function)
  (function-with-keyword-args function)
  (encapsulated-function function)
  (encapsulated-generic-function generic-function))

(defsection @stealing-from-other-package (:package (find-package :mgl-pax))
  (method locative))

(defsection @test-examples (:export nil)
  "example section")

(defsection @test-other (:export nil :title "test other title")
  "backlink @TEST")

(defsection @test-section-with-link-to-other-page-in-title
    (:title "Link to @TEST-OTHER"
            :link-title-to (@test-other section))
  "Same link in docstring to @TEST-OTHER.")

(defsection @test-section-with-link-to-same-page-in-title
    (:title "Link to @TEST" :link-title-to (@test section))
  "Same link in docstring to @TEST.")

(defsection @test-tricky-title
    (:export nil :title "`CODE` *italic* _italic2_ *bold* [link][sdf] <thing>")
  "backlink @TEST")

(defun foo (ook x)
  "FOO has args OOK and X.

  This function FOO is related to compiler-macro FOO.

  Or [foo][compiler-macro], if you prefer.

  Now, [foo][] should link to [foo][compiler-macro] and [foo][class]
  but not to [foo][function]."
  (declare (ignore ook x))
  nil)
(define-compiler-macro foo ()
  "Docstring of a compiler macro."
  nil)
(defclass foo (unexported-class)
  ((a :accessor foo-a)
   (r :reader foo-r)
   (w :writer foo-w)))
(defclass unexported-class () ())
(defvar foo-a)
(defvar foo-b)
(defvar foo-c)

(defparameter *test-variable*
  '(xxx 34)
  "*TEST-VARIABLE* is not a link.")
(defvar *some-var*)

(define-restart some-restart (arg1)
  "This is SOME-RESTART with ARG1.")

(define-condition my-error (error)
  ()
  (:documentation "This is MY-ERROR."))
(defun my-error ())

(defmacro bar (x y &key (z 7))
  "BAR has args X, Y and Z."
  (declare (ignore x y z))
  nil)
(deftype bar (x &rest r)
  "BAR has args X and R."
  (declare (ignore x r))
  'null)
(defconstant bar 2
  "BAR is not a link.")

(defgeneric baz ())
(defvar baz)
(defstruct baz
  aaa)

(defgeneric test-gf (x)
  (:documentation "TEST-GF is not a link."))
(defmethod test-gf ((x number))
  "TEST-GF links to the generic function. X is not a link."
  nil)
(defmethod test-gf ((x (eql 7))))

(define-glossary-term some-term ()
  "SOME-TERM is not a link.")

(defun ->max ())

(defun function-with-optional-args (x &optional o1 (o2 7))
  (declare (ignore x o1 o2)))

(defun function-with-keyword-args (x &key k1 (k2 14) (k3 21 k3p))
  (declare (ignore x k1 k2 k3 k3p)))

(when (fboundp 'encapsulated-function)
  (untrace encapsulated-function))
(defun encapsulated-function (x &rest args)
  "This may be encapsulated by TRACE."
  (declare (ignore x args))
  nil)
(trace encapsulated-function)

(when (fboundp 'encapsulated-generic-function)
  (untrace encapsulated-generic-function))
(defgeneric encapsulated-generic-function (x)
  (:documentation "This may also be encapsulated by TRACE."))
(trace encapsulated-generic-function)

(defmacro define-declaration (decl-name (decl-spec env) &body body)
  #+sbcl
  `(sb-cltl2:define-declaration ,decl-name (,decl-spec ,env)
     ,@body)
  #-sbcl
  (declare (ignore decl-name decl-spec env body)))

(define-declaration test-declaration (decl-spec env)
  (declare (ignore env))
  (values :declare decl-spec))

(unless (named-readtables:find-readtable 'xxx-rt)
  (named-readtables:defreadtable xxx-rt
    "ddd"))

(defparameter *navigation-test-cases*
  '((foo function (defun foo))
    (foo type (defclass foo))
    (foo class (defclass foo))
    (foo compiler-macro (define-compiler-macro foo))
    (foo-a (accessor foo) (defclass foo) (a :accessor foo-a))
    (foo-r (reader foo) (defclass foo) (r :reader foo-r))
    (foo-w (writer foo) (defclass foo) (w :writer foo-w))
    (foo-a variable (defvar foo-a))
    (foo-b variable (defvar foo-b))
    (foo-c variable (defvar foo-c))
    (bar macro (defmacro bar))
    (bar type (deftype bar))
    (bar constant (defconstant bar))
    (baz generic-function (defgeneric baz))
    (baz variable (defvar baz))
    (some-restart restart (define-restart some-restart))
    (my-error condition (define-condition my-error))
    (my-error type (define-condition my-error))
    (@mgl-pax-manual section (defsection @mgl-pax-manual))
    (baz-aaa structure-accessor (defstruct baz))
    (mgl-pax package
     (eval-when (:compile-toplevel :load-toplevel :execute))
     (cl:defpackage))
    (mgl-pax asdf:system ())
    (test-gf generic-function (defgeneric test-gf))
    (test-gf (method () (number)) (defmethod test-gf))
    (test-declaration declaration (define-declaration test-declaration))
    (xxx-rt readtable (defreadtable xxx-rt))))

(defun working-locative-p (locative)
  (declare (ignorable locative))
  (cond ((eq locative 'declaration)
         (alexandria:featurep :sbcl))
        ((eq locative 'readtable)
         nil)
        (t
         ;; AllegroCL doesn't store source location for DEFPACKAGE and
         ;; is off by one form for DEFGENERIC.
         #+allegro (not (member locative '(package generic-function)))
         #-allegro t)))

(deftest test-navigation ()
  ;; For CMUCL, SWANK-BACKEND:FIND-SOURCE-LOCATION is not implemented.
  (when (alexandria:featurep :cmucl)
    (skip-trial))
  ;; ABCL, CLISP and ECL do not provide source location information
  ;; for many things.
  (with-failure-expected ((alexandria:featurep '(:or :abcl :clisp :ecl)))
    (dolist (test-case *navigation-test-cases*)
      (destructuring-bind
          (symbol locative prefix &optional alternative-prefix) test-case
        (with-test ((format nil "navigate to (~S ~S)" symbol locative))
          (when (working-locative-p locative)
            (let* ((located (locate symbol locative))
                   (location (ignore-errors (find-source located))))
              (when (is (and location (not (eq :error (first location))))
                        :msg `("Source location for (~S ~S) can be found."
                               ,symbol ,locative))
                (multiple-value-bind (file position)
                    (extract-source-location location)
                  (when (is position)
                    (let ((form
                            (let ((*package* (find-package :mgl-pax-test)))
                              (read-form-from-file-position file position))))
                      (is (and (listp form)
                               (or (alexandria:starts-with-subseq
                                    prefix form :test #'equal)
                                   (and alternative-prefix
                                        (alexandria:starts-with-subseq
                                         alternative-prefix form
                                         :test #'equal))))
                          :msg `("Can find prefix ~S~@[ or ~S~] ~
                                  at source location~%~S~% ~
                                  for reference (~S ~S).~%~
                                  Form found was:~%~S."
                                 ,prefix ,alternative-prefix
                                 ,location ,symbol ,locative
                                 ,form)))))))))))))

;;; Extract the filename and 3303 from
;;;     (:LOCATION
;;;         (:FILE "/home/melisgl/own/mgl-pax/test/test.lisp")
;;;         (:POSITION 3303) NIL)
;;; or
;;;     (:LOCATION
;;;         (:FILE "/home/melisgl/own/mgl-pax/test/test.lisp")
;;;         (:OFFSET 1 3303) NIL)
(defun extract-source-location (location)
  (let ((file-entry (find :file (rest location) :key #'first))
        (position-entry (find :position (rest location) :key #'first))
        (offset-entry (find :offset (rest location) :key #'first)))
    (values (second file-entry)
            (cond (position-entry
                   (1- (second position-entry)))
                  (offset-entry
                   (1- (third offset-entry)))))))

(defun read-form-from-file-position (filename position)
  (with-open-file (stream filename :direction :input)
    (file-position stream position)
    (read stream)))

(deftest test-codify-and-autolink ()
  (mgl-pax::with-pages (())
    (is (string= "`FOO`"
                 (mgl-pax::codify-and-autolink "`FOO`"
                                               :known-references ())))))

(deftest test-transform-tree ()
  (is (equal '(1)
             (mgl-pax::transform-tree (lambda (parent a)
                                        (declare (ignore parent))
                                        (values a (listp a) nil))
                                      '(1))))

  (is (equal '(2 (3 (4 5)))
             (mgl-pax::transform-tree (lambda (parent a)
                                        (declare (ignore parent))
                                        (values (if (listp a) a (1+ a))
                                                (listp a)
                                                nil))
                                      '(1 (2 (3 4))))))

  (is (equal '(1 2 (2 3 (3 4 4 5)))
             (mgl-pax::transform-tree (lambda (parent a)
                                        (declare (ignore parent))
                                        (values (if (listp a)
                                                    a
                                                    (list a (1+ a)))
                                                (listp a)
                                                (not (listp a))))
                                      '(1 (2 (3 4)))))))

(deftest test-macro-arg-names ()
  (is (equal '(x a b c)
             (mgl-pax::macro-arg-names '((&key (x y)) (a b) &key (c d))))))

(defparameter *baseline-dirname*
  #-(or abcl allegro ccl clisp cmucl ecl) "baseline"
  #+abcl "abcl-baseline"
  #+allegro "acl-baseline"
  #+ccl "ccl-baseline"
  #+clisp "clisp-baseline"
  #+cmucl "cmucl-baseline"
  #+ecl "ecl-baseline")

;;; set by test.sh
(defvar *update-baseline* nil)

(deftest test-document (format)
  (let* ((*document-link-to-hyperspec* nil)
         (outputs (write-test-document-files
                   (asdf:system-relative-pathname :mgl-pax "test/data/tmp/")
                   format)))
    (is (= 4 (length outputs)))
    ;; the default page corresponding to :STREAM is empty
    (is (string= "" (first outputs)))
    (is (= 2 (count-if #'pathnamep outputs)))
    (dolist (output outputs)
      (when (pathnamep output)
        (let ((baseline (make-pathname
                         :directory (substitute *baseline-dirname* "tmp"
                                                (pathname-directory output)
                                                :test #'equal)
                         :defaults output)))
          (unless (string= (alexandria:read-file-into-string baseline)
                           (alexandria:read-file-into-string output))
            (unless *update-baseline*
              (restart-case
                  ;; KLUDGE: PROGN prevents the restart from being
                  ;; associated with the condition. Thus the restart
                  ;; is visible when TRY resignals the condition as
                  ;; TRY:UNHANDLED-ERROR.
                  (progn
                    (error "~@<Output ~S ~_differs from baseline ~S.~@:>"
                           output baseline))
                (update-output-file ()
                  :report "Update output file.")))
            (update-test-document-baseline format)))))))

(defun write-test-document-files (basedir format)
  (flet ((rebase (pathname)
           (merge-pathnames pathname
                            (make-pathname
                             :type (if (eq format :markdown) "md" "html")
                             :directory (pathname-directory basedir)))))
    (let ((open-args '(:if-exists :supersede :ensure-directories-exist t))
          (*document-downcase-uppercase-code* (eq format :html)))
      (document @test
                :pages `((:objects
                          ,(list @test-examples)
                          :output (nil))
                         (:objects
                          ,(list @test-other)
                          :output (,(rebase "other/test-other") ,@open-args))
                         (:objects
                          ,(list @test)
                          :output (,(rebase "test") ,@open-args)))
                :format format))))

(defun update-test-document-baseline (format)
  (write-test-document-files (asdf:system-relative-pathname
                              :mgl-pax
                              (format nil "test/data/~A/" *baseline-dirname*))
                             format))


(deftest test-hyperspec ()
  "Locatives work as expected (see *DOCUMENT-LINK-CODE*).
  [FIND-IF][dislocated] links to FIND-IF, [LIST][dislocated] links
  to LIST and `[LIST][type]` links to [list][type].

  Autolinking to T and NIL is suppressed. If desired, use
  `[T][]` (that links to [T][]) or `[T][constant]` (that links to
  [T][constant])."
  (is
   (null
    (mismatch%
     (let ((*document-hyperspec-root* "CLHS/")
           (*package* (find-package :mgl-pax-test)))
       (first (document #'test-hyperspec)))
     "<a id='x-28MGL-PAX-TEST-3A-3ATEST-HYPERSPEC-20FUNCTION-29'></a>

- [function] **TEST-HYPERSPEC** *&REST REST*

    Locatives work as expected (see `*DOCUMENT-LINK-CODE*`).
    `FIND-IF` links to [`FIND-IF`][badc], `LIST` links
    to `LIST`([`0`][df43] [`1`][7def]) and `[LIST][type]` links to [`list`][7def].
    
    Autolinking to `T` and `NIL` is suppressed. If desired, use
    `[T][]` (that links to `T`([`0`][b743] [`1`][cb19])) or `[T][constant]` (that links to
    [`T`][b743]).

  [7def]: CLHS/Body/t_list.htm \"(LIST TYPE)\"
  [94b1]: CLHS/Body/t_nil.htm \"(NIL TYPE)\"
  [9d3a]: CLHS/Body/v_nil.htm \"(NIL MGL-PAX:CONSTANT)\"
  [b743]: CLHS/Body/v_t.htm \"(T MGL-PAX:CONSTANT)\"
  [badc]: CLHS/Body/f_find_.htm \"(FIND-IF FUNCTION)\"
  [cb19]: CLHS/Body/t_t.htm \"(T TYPE)\"
  [df43]: CLHS/Body/f_list_.htm \"(LIST FUNCTION)\"
"))))


(defsection @clhs-test ()
  "A.1"
  "`A.1`"
  "CLHS A.1"
  "CLHS 3.4"
  "CLHS `3.4`"
  "`3.4` CLHS"
  "[3.4][]"
  "[`3.4`][]"
  "[3.4][CLHS]"
  "[Lambda Lists][clhs]"
  "[03_d][clhs]")

(deftest test-clhs ()
  (let ((*document-hyperspec-root* "CLHS/")
        (*package* (find-package :mgl-pax-test)))
    (first (document @clhs-test)))
  "<a id='x-28MGL-PAX-TEST-3A-40CLHS-TEST-20MGL-PAX-3ASECTION-29'></a>

# @CLHS-TEST

## Table of Contents


###### \\[in package MGL-PAX-TEST\\]
A.1

`A.1`

`CLHS` A.1

`CLHS` 3.4

`CLHS` [`3.4`][76476]

[`3.4`][76476] `CLHS`

[`3.4`][76476]

[`3.4`][76476]

[`3.4`][76476]

[`Lambda Lists`][76476]

[`03_d`][76476]

  [76476]: CLHS/Body/03_d.htm \"(\\\"3.4\\\" MGL-PAX:CLHS)\"
")


(defsection @argument-test ()
  "[PRINT][argument]

   PRINT argument")

(deftest test-argument ()
  (is
   (null
    (mismatch%
     (let ((*document-max-table-of-contents-level* 0)
           (*document-max-numbering-level* 0)
           (*document-text-navigation* nil)
           (*document-link-sections* nil))
       (first (document @argument-test)))
     "# @ARGUMENT-TEST

###### \\[in package MGL-PAX-TEST\\]
`PRINT`

`PRINT` argument
"))))


(defsection @declaration-test ()
  "SAFETY"
  "SAFETY declaration"
  "[safety][declaration]")

(deftest test-declaration ()
  (is
   (null
    (mismatch%
     (let ((*document-max-table-of-contents-level* 0)
           (*document-max-numbering-level* 0)
           (*document-text-navigation* nil)
           (*document-link-sections* nil))
       (first (document @declaration-test)))
     "# @DECLARATION-TEST

###### \\[in package MGL-PAX-TEST\\]
[`SAFETY`][9f0e]

[`SAFETY`][9f0e] declaration

[`safety`][9f0e]

  [9f0e]: http://www.lispworks.com/documentation/HyperSpec/Body/d_optimi.htm \"(SAFETY DECLARATION)\"
"))))


(deftest test-readtable ()
  "[xxx-rt][readtable]"
  (is
   (null
    (mismatch%
     (let ((*package* (find-package :mgl-pax-test)))
       (first (document (list #'test-readtable
                              (named-readtables:find-readtable 'xxx-rt)))))
     "<a id='x-28MGL-PAX-TEST-3A-3ATEST-READTABLE-20FUNCTION-29'></a>

- [function] **TEST-READTABLE** *&REST REST*

    [`xxx-rt`][9ac2]
<a id='x-28MGL-PAX-TEST-3A-3AXXX-RT-20READTABLE-29'></a>

- [readtable] **XXX-RT**

    ddd

  [9ac2]: #x-28MGL-PAX-TEST-3A-3AXXX-RT-20READTABLE-29 \"(MGL-PAX-TEST::XXX-RT READTABLE)\"
"))))


(defsection @test-package ()
  "INTERNED-PKG-NAME"
  "NON-INTERNED-PKG-NAME"
  "[NON-INTERNED-PKG-NAME][]"
  "[NON-INTERNED-PKG-NAME][package]"
  (interned-pkg-name package)
  (#:non-interned-pkg-name package))

(defpackage interned-pkg-name)
(defpackage #:non-interned-pkg-name)

(deftest test-package ()
  (is
   (null
    (mismatch%
     (let ((*document-max-table-of-contents-level* 0)
           (*document-max-numbering-level* 0)
           (*document-text-navigation* nil)
           (*document-link-sections* nil))
       (first (document @test-package)))
     "# @TEST-PACKAGE

###### \\[in package MGL-PAX-TEST\\]
[`INTERNED-PKG-NAME`][2509]

[`NON-INTERNED-PKG-NAME`][11d0]

[`NON-INTERNED-PKG-NAME`][11d0]

[`NON-INTERNED-PKG-NAME`][11d0]

<a id='x-28-22INTERNED-PKG-NAME-22-20PACKAGE-29'></a>

- [package] **\"INTERNED-PKG-NAME\"**

<a id='x-28-22NON-INTERNED-PKG-NAME-22-20PACKAGE-29'></a>

- [package] **\"NON-INTERNED-PKG-NAME\"**

  [11d0]: #x-28-22NON-INTERNED-PKG-NAME-22-20PACKAGE-29 \"(\\\"NON-INTERNED-PKG-NAME\\\" PACKAGE)\"
  [2509]: #x-28-22INTERNED-PKG-NAME-22-20PACKAGE-29 \"(\\\"INTERNED-PKG-NAME\\\" PACKAGE)\"
"))))


(defsection @test-asdf-system ()
  "MGL-PAX/FULL"
  "MGL-PAX/TEST"
  "[MGL-PAX/TEST][]"
  "[MGL-PAX/TEST][asdf:system]"
  (mgl-pax/full asdf:system)
  (#:mgl-pax/test asdf:system))

(deftest test-asdf-system ()
  (is (find-symbol (string '#:mgl-pax/full) '#:mgl-pax-test))
  (is (null (find-symbol (string '#:mgl-pax/test) '#:mgl-pax-test)))
  (is
   (null
    (mismatch%
     (let ((*document-max-table-of-contents-level* 0)
           (*document-max-numbering-level* 0)
           (*document-text-navigation* nil)
           (*document-link-sections* nil)
           (mgl-pax::*omit-asdf-slots* t))
       (first (document @test-asdf-system)))
     "# @TEST-ASDF-SYSTEM

###### \\[in package MGL-PAX-TEST\\]
[`MGL-PAX/FULL`][0785]

[`MGL-PAX/TEST`][4b83]

[`MGL-PAX/TEST`][4b83]

[`MGL-PAX/TEST`][4b83]

## MGL-PAX/FULL ASDF System Details


## MGL-PAX/TEST ASDF System Details


  [0785]: #x-28-22mgl-pax-2Ffull-22-20ASDF-2FSYSTEM-3ASYSTEM-29 \"(\\\"mgl-pax/full\\\" ASDF/SYSTEM:SYSTEM)\"
  [4b83]: #x-28-22mgl-pax-2Ftest-22-20ASDF-2FSYSTEM-3ASYSTEM-29 \"(\\\"mgl-pax/test\\\" ASDF/SYSTEM:SYSTEM)\"
"))))


(deftest test-all ()
  (test-transcribe)
  (test-navigation)
  (test-codify-and-autolink)
  (test-transform-tree)
  (test-macro-arg-names)
  (test-document :markdown)
  (test-document :html)
  (test-hyperspec)
  (test-clhs)
  (test-argument)
  (test-declaration)
  (test-readtable)
  (test-package)
  (test-asdf-system))

(defun test (&key (debug nil) (print 'unexpected) (describe 'unexpected))
  ;; Bind *PACKAGE* so that names of tests printed have package names,
  ;; and M-. works on them in Slime.
  (let ((*package* (find-package :common-lisp))
        (*print-duration* nil)
        (*print-compactly* nil)
        (*defer-describe* nil))
    (warn-on-tests-not-run ((find-package :mgl-pax-test))
      (print (try 'test-all :debug debug :print print :describe describe)))))

#+nil
(test)
