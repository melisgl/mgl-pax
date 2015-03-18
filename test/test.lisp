(in-package :mgl-pax-test)

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
  (foo-a (accessor foo))
  (*test-variable* variable)
  (@test-examples section)
  (@test-other section)
  (test-gf generic-function)
  (test-gf (method () (number))))

(defsection @test-examples (:export nil)
  "example section")

(defsection @test-other (:export nil :title "test other title")
  "backlink @TEST")

(defun foo ())
(define-compiler-macro foo ())
(defclass foo ()
  ((a :accessor foo-a)
   (r :reader foo-r)
   (w :writer foo-w)))
(defvar foo-a)
(defvar foo-b)
(defvar foo-c)

(defparameter *test-variable*
  '(xxx 34))

(defmacro bar ())
(deftype bar () 'null)
(defconstant bar 2)

(defgeneric baz ())
(defvar baz)
(defstruct baz
  aaa)

(defgeneric test-gf (x))
(defmethod test-gf ((x number)))

(defun ->max ())

(defparameter *navigation-test-cases*
  '((foo function (defun foo))
    (foo type (defclass foo))
    (foo class (defclass foo))
    (foo compiler-macro (define-compiler-macro foo))
    (foo-a (accessor foo) (defclass foo))
    (foo-r (reader foo) (defclass foo))
    (foo-w (writer foo) (defclass foo))
    (foo-a variable (defvar foo-a))
    (foo-b variable (defvar foo-b))
    (foo-c variable (defvar foo-c))
    (bar macro (defmacro bar))
    (bar type (deftype bar))
    (bar constant (defconstant bar))
    (baz generic-function (defgeneric baz))
    (baz variable (defvar baz))
    (@mgl-pax-manual section (defsection @mgl-pax-manual))
    (baz-aaa structure-accessor (defstruct baz))
    (mgl-pax package
     #-ccl (eval-when (:compile-toplevel :load-toplevel :execute))
     #+ccl (cl:defpackage))
    (mgl-pax asdf:system ())
    ;; Allegro has the location off by one form.
    #-allegro
    (test-gf generic-function (defgeneric test-gf))
    (test-gf (method () (number)) (defmethod test-gf))))

(defun working-locative-p (locative)
  (declare (ignorable locative))
  ;; AllegroCL doesn't store source location for DEFPACKAGE.
  #+allegro (not (eq locative 'package))
  #-allegro t)

(defun test-navigation ()
  (loop for (symbol locative prefix) in *navigation-test-cases*
        do (when (working-locative-p locative)
             (let ((location (find-source (locate symbol locative))))
               (assert (not (eq :error (first location))) ()
                       "Could not find source location for (~S ~S)"
                       symbol locative)
               (let* ((file (second (second location)))
                      (position (1- (second (third location))))
                      (form (let ((*package* (find-package :mgl-pax-test)))
                              (read-form-from-file-position file position))))
                 (assert
                  (alexandria:starts-with-subseq prefix form :test #'equal)
                  () "Could not find prefix ~S at source location ~S ~
                     for reference (~S ~S). Form was: ~S."
                  prefix location symbol locative form))))))

(defun read-form-from-file-position (filename position)
  (with-open-file (stream filename :direction :input)
    (file-position stream position)
    (read stream)))

(defun test-replace-known-references ()
  (assert (string= "`FOO`"
                   (mgl-pax::replace-known-references "`FOO`"
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

(defun test-document ()
  (let ((outputs (write-test-document-files
                  (asdf:system-relative-pathname :mgl-pax "test/data/tmp/"))))
    (assert (= 4 (length outputs)))
    ;; the default page corresponding to :STREAM is empty
    (assert (string= "" (first outputs)))
    (assert (= 2 (count-if #'pathnamep outputs)))
    (dolist (output outputs)
      (when (pathnamep output)
        (let ((baseline (make-pathname
                         :directory (substitute "baseline" "tmp"
                                                (pathname-directory output)
                                                :test #'equal)
                         :defaults output)))
          (unless (string= (alexandria:read-file-into-string baseline)
                           (alexandria:read-file-into-string output))
            (cerror "Update output file." "Output differs from ~S." output)
            (update-test-document-files)))))))

(defun write-test-document-files (basedir)
  (flet ((rebase (pathname)
           (merge-pathnames pathname basedir)))
    (let ((open-args '(:if-exists :supersede :ensure-directories-exist t)))
      (document @test
                :pages `((:objects
                          ,(list @test-examples)
                          :output (nil))
                         (:objects
                          ,(list @test-other)
                          :output (,(rebase "other/test-other.md") ,@open-args))
                         (:objects
                          ,(list @test)
                          :output (,(rebase "test.md") ,@open-args)))))))

(defun update-test-document-files ()
  (write-test-document-files
   (asdf:system-relative-pathname :mgl-pax "test/data/baseline/")))


(defun test ()
  (test-transcribe)
  (test-navigation)
  (test-replace-known-references)
  (test-transform-tree)
  (test-macro-arg-names)
  (test-document))
