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

(defun encapsulated-function (x &rest args)
  "This may be encapsulated by TRACE."
  (declare (ignore x args))
  nil)
(trace encapsulated-function)

(defgeneric encapsulated-generic-function (x)
  (:documentation "This may also be encapsulated by TRACE."))
(trace encapsulated-generic-function)

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
    (test-gf (method () (number)) (defmethod test-gf))))

(defun working-locative-p (locative)
  (declare (ignorable locative))
  ;; AllegroCL doesn't store source location for DEFPACKAGE and is off
  ;; by one form for DEFGENERIC.
  #+allegro (not (member locative '(package generic-function)))
  #-allegro t)

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
                                 prefix alternative-prefix
                                 location symbol locative form)))))))))))))

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
  (is (string= "`FOO`"
               (mgl-pax::codify-and-autolink "`FOO`"
                                             :known-references ()))))

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


(defsection @hyperspec-test ()
  "Locatives work as expected (see *DOCUMENT-LINK-CODE*).
  [FIND-IF][dislocated] links to FIND-IF, [LIST][dislocated] links
  to LIST and `[LIST][type]` links to [list][type].

  Autolinking to T and NIL is suppressed. If desired, use
  `[T][]` (that links to [T][]) or `[T][constant]` (that links to
  [T][constant]).")

(deftest test-hyperspec ()
  (is
   (null
    (mismatch%
     (let ((*document-hyperspec-root* "CLHS/"))
       (first (document @hyperspec-test)))
     "<a id='x-28MGL-PAX-TEST-3A-40HYPERSPEC-TEST-20MGL-PAX-3ASECTION-29'></a>

# @HYPERSPEC-TEST

## Table of Contents


###### \\[in package MGL-PAX-TEST\\]
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


(deftest test-all ()
  (test-transcribe)
  (test-navigation)
  (test-codify-and-autolink)
  (test-transform-tree)
  (test-macro-arg-names)
  (test-document :markdown)
  (test-document :html)
  (test-hyperspec))

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
