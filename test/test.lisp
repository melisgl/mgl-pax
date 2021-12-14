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
  "[*TEST-VARIABLE*][]
  [`*TEST-VARIABLE*`][]
  [*test-variable*][]
  [`*test-variable*`][]
  [mgl-pax-test:*test-variable*][]
  FOO function, function FOO,
  `FOO` function, function `FOO`,
  FOO `function`, `function` FOO,
  `FOO` `function`, `function` `FOO`,
  [foo][function],
  [foo][FUNCTION],
  [FOO][function],
  [FOO][FUNCTION],
  [`foo`][function],
  [`foo`][FUNCTION],
  [`FOO`][function],
  [`FOO`][FUNCTION],

  FOO-A `(accessor foo)`, `(accessor foo)` FOO-A,
  `FOO-A` `(accessor foo)`, `(accessor foo)` `FOO-A`,
  [foo-a][(accessor foo)],
  [foo-a][(ACCESSOR FOO)],
  [FOO-A][(accessor foo)],
  [FOO-A][(ACCESSOR FOO)],
  [`foo-a`][(accessor foo)],
  [`foo-a`][(ACCESSOR FOO)],
  [`FOO-A`][(accessor foo)],
  [`FOO-A`][(ACCESSOR FOO)]

  ->MAX

  Escaped: \\FOO [`FOO`][dislocated] *\\NAVIGATION-TEST-CASES*
  Non escaped: FOO *TEST-VARIABLE*
  @TEST-OTHER

  This should be no link because the page of @TEST-EXAMPLES
  has :URI-FRAGMENT NIL.

  This is code: T

  Plural uppercase ambiguous symbol: see FOOs

  Plural uppercase symbol: TEST-GFs

  Plural uppercase dislocated symbol: ->MAXs
  
  See
  FOO compiler-macro

  See FOO
  compiler-macro

  See
  compiler-macro FOO

  See compiler-macro
  FOO

  See
  compiler-macro 
  FOO

  See
  FOO

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
  TEST-GF."
  (foo function)
  (foo compiler-macro)
  (foo class)
  ;; aliases defined above
  "FOO instance and FOO object"
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
(defclass foo ()
  ((a :accessor foo-a)
   (r :reader foo-r)
   (w :writer foo-w)))
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
    ;; Allegro has the location off by one form.
    #-allegro
    (test-gf generic-function (defgeneric test-gf))
    #+allegro
    (test-gf generic-function (defmethod test-gf))
    (test-gf (method () (number)) (defmethod test-gf))))

(defun working-locative-p (locative)
  (declare (ignorable locative))
  ;; AllegroCL doesn't store source location for DEFPACKAGE.
  #+allegro (not (eq locative 'package))
  #-allegro t)

(defun test-navigation ()
  (loop for test-case in *navigation-test-cases*
        do (destructuring-bind
               (symbol locative prefix &optional alternative-prefix) test-case
             (when (working-locative-p locative)
               (let ((location (find-source (locate symbol locative))))
                 (assert (not (eq :error (first location))) ()
                         "Could not find source location for (~S ~S)"
                         symbol locative)
                 (multiple-value-bind (file position)
                     (extract-source-location location)
                   (let ((form (let ((*package* (find-package :mgl-pax-test)))
                                 (read-form-from-file-position file position))))
                     (assert
                      (or (alexandria:starts-with-subseq prefix form
                                                         :test #'equal)
                          (and alternative-prefix
                               (alexandria:starts-with-subseq
                                alternative-prefix form :test #'equal)))
                      () "Could not find prefix ~S~@[ or ~S~] ~
                     at source location~%~S~%for reference (~S ~S).~%~
                     Form found was:~%~S."
                      prefix alternative-prefix
                      location symbol locative form))))))))

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
            (1- (if position-entry
                    (second position-entry)
                    (third offset-entry))))))

(defun read-form-from-file-position (filename position)
  (with-open-file (stream filename :direction :input)
    (file-position stream position)
    (read stream)))

(defun test-codify-and-autolink ()
  (assert (string= "`FOO`"
                   (mgl-pax::codify-and-autolink "`FOO`"
                                                 :known-references ()))))

(defun test-transform-tree ()
  (assert (equal '(1)
                 (mgl-pax::transform-tree (lambda (parent a)
                                            (declare (ignore parent))
                                            (values a (listp a) nil))
                                          '(1))))

  (assert (equal '(2 (3 (4 5)))
                 (mgl-pax::transform-tree (lambda (parent a)
                                            (declare (ignore parent))
                                            (values (if (listp a) a (1+ a))
                                                    (listp a)
                                                    nil))
                                          '(1 (2 (3 4))))))

  (assert (equal '(1 2 (2 3 (3 4 4 5)))
                 (mgl-pax::transform-tree (lambda (parent a)
                                            (declare (ignore parent))
                                            (values (if (listp a)
                                                        a
                                                        (list a (1+ a)))
                                                    (listp a)
                                                    (not (listp a))))
                                          '(1 (2 (3 4)))))))

(defun test-macro-arg-names ()
  (assert (equal '(x a b c)
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

(defun test-document (format)
  (let ((outputs (write-test-document-files
                  (asdf:system-relative-pathname :mgl-pax "test/data/tmp/")
                  format)))
    (assert (= 4 (length outputs)))
    ;; the default page corresponding to :STREAM is empty
    (assert (string= "" (first outputs)))
    (assert (= 2 (count-if #'pathnamep outputs)))
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
              (cerror "Update output file."
                      "~@<Output ~S ~_differs from baseline ~S.~@:>"
                      output baseline))
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


(defun test ()
  (test-transcribe)
  ;; ABCL, CLISP and ECL do not provide source location information
  ;; for too many things to make this test worthwile. For CMUCL,
  ;; SWANK-BACKEND:FIND-SOURCE-LOCATION is not implemented.
  #-(or abcl clisp cmucl ecl)
  (test-navigation)
  (test-codify-and-autolink)
  (test-transform-tree)
  (test-macro-arg-names)
  (test-document :markdown)
  (test-document :html))

#+nil
(test)
