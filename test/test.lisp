(in-package :mgl-pax-test)

(defsection @test (:export nil)
  "[*NAVIGATION-TEST-CASES*][]
  [`*NAVIGATION-TEST-CASES*`][]
  [*navigation-test-cases*][]
  [`*navigation-test-cases*`][]
  [mgl-pax-test:*navigation-test-cases*][]
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

  Escaped: \\FOO *\\NAVIGATION-TEST-CASES*
  Non escaped: FOO *NAVIGATION-TEST-CASES*
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

  In documentation, when the only ambiguity is between a generic
  function and its methods, it's resolved in favor if the gf:
  TEST-GF."
  (foo function)
  (foo compiler-macro)
  (foo-a (accessor foo))
  (*navigation-test-cases* variable)
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

(defmacro bar ())
(deftype bar () 'null)
(defconstant bar 2)

(defgeneric baz ())
(defvar baz)
(defstruct baz
  aaa)

(defgeneric test-gf (x))
(defmethod test-gf ((x number)))

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
    (mgl-pax package (eval-when (:compile-toplevel :load-toplevel :execute)))
    (mgl-pax asdf:system ())
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
                   (mgl-pax::replace-known-references "`FOO`" ()))))

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
                         :directory (substitute "baseline" "new"
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


(defparameter *transcribe-source-file*
  (asdf:system-relative-pathname
   :mgl-pax "test/data/baseline/transcribe-source.lisp"))

(defparameter *transcribe-transcription-file*
  (asdf:system-relative-pathname
   :mgl-pax "test/data/baseline/transcribe-transcription.lisp"))

(defun test-transcribe ()
  (test-transcribe-from-source)
  (test-transcribe-stability)
  (test-transcribe-cases))

(defun test-transcribe-from-source ()
  (check-transcription *transcribe-source-file*
                       *transcribe-transcription-file*
                       :check-consistency nil))

;;; Check that repeated transcription produces the same results.
(defun test-transcribe-stability ()
  (check-transcription *transcribe-transcription-file*
                       *transcribe-transcription-file*
                       :check-consistency t))

(defun check-transcription (source-file transcription-file
                            &key check-consistency debug)
  (unless (string= (alexandria:read-file-into-string transcription-file)
                   (with-output-to-string (transcription)
                     (with-open-file (source source-file)
                       (transcribe source transcription :update-only t
                                   :check-consistency check-consistency
                                   :debug debug))))
    (cerror "Update transcription file." "Transcription differs from ~S."
            transcription-file)
    (with-input-from-string (source (alexandria:read-file-into-string
                                     source-file))
      (with-open-file (transcription transcription-file :direction :output
                                     :if-exists :rename-and-delete)
        (transcribe source transcription :update-only t
                    :check-consistency check-consistency)))))

(defun test-transcribe-cases ()
  (flet ((foo (input expected-output
               &key expected-errors expected-inconsistencies
               check-consistency update-only
               include-no-output include-no-value)
           (format t "case: ~S ~S ~S ~S ~S ~S ~S ~S~%" input expected-output
                   expected-errors expected-inconsistencies
                   check-consistency update-only
                   include-no-output include-no-value)
           (let ((inconsistencies ())
                 (errors ()))
             (catch 'here
               (handler-bind
                   ((transcription-consistency-error
                      (lambda (e)
                        (push (mgl-pax::transcription-error-file-position e)
                              inconsistencies)
                        (continue)))
                    (transcription-error
                      (lambda (e)
                        (push (mgl-pax::transcription-error-file-position e)
                              errors)
                        (throw 'here nil))))
                 (assert
                  (string= (format nil expected-output)
                           (transcribe (format nil input) nil
                                       :check-consistency check-consistency
                                       :update-only update-only
                                       :include-no-output include-no-output
                                       :include-no-value include-no-value)))))
             (assert (equal (reverse inconsistencies) expected-inconsistencies))
             (assert (equal (reverse errors) expected-errors)))))
    (foo "42" "42~%=> 42~%")
    (foo "42~%" "42~%=> 42~%")
    ;; The spaces are discarded.
    (foo "42  ~%" "42~%=> 42~%")
    ;; No preceeding "==>" error.
    (foo "42~%-->" "" :expected-errors '(3))
    ;; No-value marker after values.
    (foo "42~%=> 42~%=> ; No value" "" :expected-errors '(9))
    ;; No whitespace after form.
    (foo "42;comment" "42~%=> 42~%;comment")
    ;; Unexpected eof on non-empty value line.
    (foo "42~%=> ;" "" :expected-errors '(3))
    ;; Multiple forms on the same line.
    (foo "(list)(values 1 2)" "(list)~%=> NIL~%(values 1 2)~%=> 1~%=> 2~%")
    ;; Multiple forms on the same line with space.
    (foo "(list) (values 1 2)" "(list)~%=> NIL~%(values 1 2)~%=> 1~%=> 2~%")
    ;; No value included.
    (foo "(values)" "(values)~%=> ; No value~%" :include-no-value t)
    ;; No value not included.
    (foo "(values)" "(values)~%")
    ;; No output included.
    (foo "(values)" "(values)~%..~%" :include-no-output t)
    ;; No output not included.
    (foo "(values)" "(values)~%")
    ;; Capturing *ERROR-OUTPUT*.
    (foo "(princ 42 *error-output*)"
         "(princ 42 *error-output*)~%.. 42~%=> 42~%")
    ;; Capturing *TRACE-OUTPUT*.
    (foo "(princ 42 *trace-output*)"
         "(princ 42 *trace-output*)~%.. 42~%=> 42~%")
    ;; Capturing *DEBUG-IO*.
    (foo "(princ 42 *debug-io*)"
         "(princ 42 *debug-io*)~%.. 42~%=> 42~%")
    ;; Capturing *QUERY-IO*.
    (foo "(princ 42 *query-io*)"
         "(princ 42 *query-io*)~%.. 42~%=> 42~%")
    ;; Capturing *TERMINAL-IO*.
    (foo "(princ 42 *terminal-io*)"
         "(princ 42 *terminal-io*)~%.. 42~%=> 42~%")))


(defun test ()
  (test-navigation)
  (test-replace-known-references)
  (test-transform-tree)
  (test-macro-arg-names)
  (test-document)
  (test-transcribe))
