(in-package :dref-test)

(deftest test-util ()
  (test-has-setf-p))

(deftest test-has-setf-p ()
  (is (null (dref::has-setf-p 'undefined)))
  (is (dref::has-setf-p 'documentation))
  (is (dref::has-setf-p 'has-setf-expander))
  (is (dref::has-setf-p 'has-setf-function))
  (is (dref::has-setf-p 'has-setf-generic-function))
  (is (null (dref::has-setf-p 'defun)))
  (with-failure-expected
      ((and (alexandria:featurep '(:not (:or :ccl :clisp :sbcl)))
            'failure))
    (is (dref::has-setf-p 'full-setf))))


(deftest test-definitions ()
  (check-ref-sets (definitions 'undefined) ())
  (with-failure-expected ((and (alexandria:featurep '(:or :clisp))
                               'failure))
    (check-ref-sets (definitions '*some-var*)
                    `(,(make-xref '*some-var* 'variable))))
  (with-failure-expected ((and (alexandria:featurep '(:or :ccl :clisp))
                               'failure))
    (check-ref-sets (definitions 'my-smac)
                    `(,(make-xref 'my-smac 'symbol-macro))))
  (check-ref-sets (definitions 'foo2) (list (make-xref 'foo2 'function)))
  (check-ref-sets (definitions 'foo) (list (make-xref 'foo 'function)
                                           (make-xref 'foo 'class)
                                           (make-xref 'foo 'compiler-macro)))
  (check-ref-sets (definitions 'dref) (list (locate 'dref 'package)
                                            (make-xref "dref" 'asdf:system)
                                            (make-xref 'dref 'class)))
  (check-ref-sets (definitions "dref")
                  (if (string= (package-name 'dref) "dref")
                      (list (make-xref "dref" 'package)
                            (make-xref "dref" 'asdf:system))
                      (list (make-xref "dref" 'asdf:system))))
  (with-failure-expected ((and (alexandria:featurep '(:or :ecl :clisp))
                               'failure))
    (check-ref-sets (definitions 'bar)
                    `(,(make-xref 'bar 'macro)
                      ,(make-xref 'bar 'constant)
                      ,(make-xref 'bar 'type))))
  (with-failure-expected ((and (alexandria:featurep
                                '(:or :abcl :ccl :clisp :cmucl :ecl))
                               'failure))
    (check-ref-sets (definitions 'has-setf-expander)
                    `(,(make-xref 'has-setf-expander 'setf))))
  (with-failure-expected ((and (alexandria:featurep '(:or :abcl :clisp))
                               'failure))
    (check-ref-sets (definitions 'test-gf)
                    `(,(make-xref 'test-gf 'generic-function)
                      ,(make-xref 'test-gf '(method () (number)))
                      ,(make-xref 'test-gf '(method () ((eql 7))))
                      ,(make-xref 'test-gf '(method ()
                                             ((eql #.(find-package :cl))))))))
  (with-failure-expected ((and (alexandria:featurep
                                '(:or :abcl :clisp :cmucl :ecl))
                               'failure))
    (check-ref-sets (definitions 'my-comb)
                    `(,(make-xref 'my-comb 'method-combination))))
  ;; There _may_ be a GENERIC-FUNCTION and a SETF generic function too.
  (with-failure-expected ((and (alexandria:featurep '(:or :abcl :clisp :ecl))
                               'failure))
    (check-ref-sets (remove-if (lambda (dref)
                                 (member (dref-locative-type dref)
                                         '(generic-function setf)))
                               (definitions 'foo-a))
                    `(,(make-xref 'foo-a 'variable)
                      ,(make-xref 'foo-a '(accessor foo))))
    (check-ref-sets (remove 'generic-function (definitions 'foo-r)
                            :key #'dref-locative-type)
                    `(,(make-xref 'foo-r 'variable)
                      ,(make-xref 'foo-r '(reader foo))))
    (check-ref-sets (remove 'generic-function (definitions 'foo-w)
                            :key #'dref-locative-type)
                    `(,(make-xref 'foo-w 'variable)
                      ,(make-xref 'foo-w '(writer foo)))))
  ;; recognized as function
  (with-failure-expected (t)
    (check-ref-sets (definitions 'baz-aaa)
                    `(,(make-xref 'baz-aaa '(structure-accessor baz)))))
  (with-failure-expected ((alexandria:featurep '(:not :sbcl)))
    (check-ref-sets (definitions 'dynamic-extent)
                    `(,(make-xref 'dynamic-extent 'declaration))))
  (check-ref-sets (definitions 'my-error)
                  `(,(make-xref 'my-error 'condition)
                    ,(make-xref 'my-error 'function)))
  (with-failure-expected ((alexandria:featurep '(:not :sbcl)))
    (check-ref-sets (definitions 'test-declaration)
                    `(,(make-xref 'test-declaration 'declaration))))
  (check-ref-sets (definitions 'xxx-rt)
                  `(,(make-xref 'xxx-rt 'readtable)))
  (check-ref-sets (definitions 'my-loc)
                  `(,(make-xref 'my-loc 'locative)))
  #+sbcl
  (check-ref-sets (definitions 'print
                               :locative-types (cons 'unknown
                                                     (lisp-locative-types)))
                  `(,(make-xref 'print 'function)
                    ,(make-xref 'print '(unknown
                                         (:defoptimizer print
                                             sb-c:derive-type)))
                    ,(make-xref 'print '(unknown
                                         (:define-vop print print)))
                    ,(make-xref 'print '(unknown
                                         (declaim print sb-c:defknown)))))
  ;; This is a primitive object, that (SWANK-BACKEND:FIND-DEFINITIONS
  ;; 'SB-C::CATCH-BLOCK) returns as a TYPE.
  #+sbcl
  (signals-not (locate-error)
    (definitions 'sb-c::catch-block)))

(defun check-ref-sets (refs expected-refs)
  (is (match-values (diff-sets (capture refs) (capture expected-refs)
                               :test #'xref=)
        (endp *)
        (endp *))))

(defun diff-sets (set1 set2 &key (test 'eql))
  (values (set-difference set1 set2 :test test)
          (set-difference set2 set1 :test test)))


(deftest test-dspec ()
  (check-dspec-roundtrip (locate '*some-var* 'variable))
  (check-dspec-roundtrip (locate 'bar 'constant))
  (check-dspec-roundtrip (locate 'bar 'macro))
  (check-dspec-roundtrip (locate 'block 'macro))
  (check-dspec-roundtrip (locate 'foo 'compiler-macro))
  (check-dspec-roundtrip (locate 'my-smac 'symbol-macro))
  (check-dspec-roundtrip (locate 'has-setf-expander 'setf))
  (check-dspec-roundtrip (locate 'has-setf-function 'setf))
  (check-dspec-roundtrip (locate 'foo 'function))
  (check-dspec-roundtrip (locate 'test-gf 'generic-function))
  (with-failure-expected ((alexandria:featurep '(:or :ecl)))
    (check-dspec-roundtrip (locate 'test-gf '(method () ((eql 7)))))
    (check-dspec-roundtrip (locate 'test-gf
                                   '(method () ((eql #.(find-package :cl)))))))
  (check-dspec-roundtrip (locate 'gf2 '(method (:around) (t))))
  (check-dspec-roundtrip (locate 'gf2 '(method (:after) (t))))
  (check-dspec-roundtrip (locate 'my-comb 'method-combination))
  (check-dspec-roundtrip (locate 'bar 'type))
  (check-dspec-roundtrip (locate 'foo 'class))
  (check-dspec-roundtrip (locate 'array 'class))
  (check-dspec-roundtrip (locate 'my-error 'condition))
  (check-dspec-roundtrip (locate  'dref 'package))
  (let ((dref (locate 'print '(unknown (:define-vop print print)) nil)))
    (when dref
      (check-dspec-roundtrip dref))))

(defun check-dspec-roundtrip (dref &optional (expected-result dref))
  (let* ((dspec (dref::definition-to-dspec dref))
         (roundtripped (dref::dspec-to-definition dspec (dref-name dref))))
    (when (is roundtripped
              :ctx ("DREF = ~S~%DPSEC = ~S" dref dspec))
      (is (xref= (capture roundtripped) (capture expected-result))
          :ctx ("DREF = ~S~%DPSEC = ~S" dref dspec)))))


(deftest test-arglist ()
  (test-macro-arg-names)
  (test-function-arg-names)
  (is (null (arglist (locate 'my-smac 'symbol-macro))))
  (test-arglist/macro)
  (test-arglist/compiler-macro)
  (test-arglist/setf)
  (test-arglist/function)
  (test-arglist/generic-function)
  (test-arglist/method)
  (test-arglist/type)
  (test-arglist/locative)
  (test-arglist/symbol-locative)
  (test-arglist/lambda))

(deftest test-macro-arg-names ()
  (is (equal '(x a b c)
             (dref::macro-arg-names '((&key (x y)) (a b) &key (c d))))))

(deftest test-function-arg-names ()
  (is (equal (dref::function-arg-names '(x &optional (o 1) &key (k 2 kp)))
             '(x o k kp)))
  (is (equal (dref::function-arg-names '(&key ((:xxx xxx))))
             '(xxx)))
  (is (equal (dref::function-arg-names '(&key ((:xxx xxx) nil xxxp)))
             '(xxx xxxp)))
  (is (equal (dref::function-arg-names '(&key ((:xxx xxx) nil xxxp)
                                         &allow-other-keys &aux (a 7)))
             '(xxx xxxp a))))

(deftest test-arglist/macro ()
  (with-failure-expected ((and (alexandria:featurep
                                '(:or :abcl :allegro :clisp))
                               'failure))
    (is (or (equal (% (arglist (locate 'macro-with-fancy-args 'macro)))
                   '(x &optional (o 1) &key (k 2 kp)))
            (equal (arglist (locate 'macro-with-fancy-args 'macro))
                   '(x &optional (o 1) &key (k 2))))))
  (with-test ("special operator")
    (is (match-values (arglist (locate 'function 'macro))
          (null *)))))

(deftest test-arglist/compiler-macro ()
  (with-failure-expected ((and (alexandria:featurep
                                '(:or :abcl :ccl :clisp :ecl))
                               'failure))
    (is (match-values (arglist (locate 'cmac 'compiler-macro))
          (equal * '(x &rest y))
          (eq * :macro)))))

(deftest test-arglist/setf ()
  (is (null (arglist (locate 'has-setf-expander 'setf))))
  (is (match-values (arglist (locate 'has-setf-function 'setf))
        (equal * '(v))
        (eq * :ordinary)))
  (is (match-values (arglist (locate 'has-setf-generic-function 'setf))
        (equal * '(v))
        (eq * :ordinary)))
  (is (match-values (arglist (locate 'has-setf-generic-function
                                     '(setf (method () (string)))))
        (equal * '(v))
        (eq * :ordinary))))

(deftest test-arglist/function ()
  (is (match-values (arglist (locate 'function-with-fancy-args 'function))
        (equal * '(x &optional (o 1) &key (k 2 kp)))
        (eq * :ordinary)))
  (with-failure-expected ((and (alexandria:featurep '(:or :abcl :clisp :ecl))
                               'failure))
    (is (match-values (arglist (locate 'traced-foo 'function))
          (equal * '(x))
          (eq * :ordinary)))))

(deftest test-arglist/generic-function ()
  (is (match-values (arglist (locate 'test-gf 'generic-function))
        (equal * '(x))
        (eq * :ordinary)))
  (with-failure-expected ((and (alexandria:featurep '(:or :clisp :ecl))
                               'failure))
    (is (match-values (arglist (locate 'traced-gf 'function))
          (equal * '(x))
          (eq * :ordinary)))))

(deftest test-arglist/method ()
  (is (match-values (arglist (locate 'gf2 '(method (:around) (t))))
        (equal * '(x &key))
        (eq * :ordinary)))
  (is (match-values (arglist (locate 'gf2 '(method (:after) (t))))
        (equal * '(x &key y))
        (eq * :ordinary))))

(deftest test-arglist/type ()
  (with-failure-expected ((alexandria:featurep '(:or :abcl :allegro :clisp
                                                 :ccl :cmucl :ecl)))
    (is (match-values (arglist (locate 'bar 'type))
          (equal * '(x &rest r))
          (eq * :deftype)))))

(deftest test-arglist/locative ()
  (is (match-values (arglist (locate 'loc-with-args 'locative))
        (equal * '((x y) &key z))
        (eq * :macro))))

(deftest test-arglist/symbol-locative ()
  (is (match-values (arglist (locate 'sloc1 'sloc))
        (equal * '(&key z))
        (eq * :macro))))

(deftest test-arglist/lambda ()
  (is (match-values (arglist (locate nil '(lambda :arglist ((x y) z)
                                           :arglist-type :macro)))
        (equal * '((x y) z))
        (eq * :macro))))


(deftest test-docstring ()
  (is (equal (docstring (make-xref 'foo-a 'variable))
             "FOO-A variable"))
  (is (equal (docstring (locate 'foo-a 'variable)) "FOO-A variable"))
  (is (equal (docstring (locate 'bar 'constant)) "BAR constant"))
  (is (equal (docstring (locate 'bar 'macro)) "BAR macro"))
  (signals-not (error)
    (docstring (locate 'function 'macro)))
  (is (equal (docstring (locate 'my-smac 'symbol-macro))
             "MY-SMAC symbol-macro"))
  (with-failure-expected ((and (alexandria:featurep
                                '(:or :abcl :allegro :ccl :ecl))
                               'failure))
    (is (equal (docstring (locate 'foo 'compiler-macro)) "FOO compiler-macro")))
  (with-failure-expected ((and (alexandria:featurep '(:or :abcl))
                               'failure))
    (is (equal (docstring (locate 'has-setf-expander 'setf))
               "HAS-SETF-EXPANDER setf"))
    (is (equal (docstring (locate 'has-setf-function 'setf))
               "HAS-SETF-FUNCTION setf")))
  (with-failure-expected ((and (alexandria:featurep '(:or :cmucl))
                               'failure))
    (is (equal (docstring (locate 'has-setf-generic-function 'setf))
               "HAS-SETF-GENERIC-FUNCTION setf")))
  (is (equal (docstring (locate 'has-setf-generic-function
                                '(setf (method () (string)))))
             "HAS-SETF-GENERIC-FUNCTION (setf (method () (string)))"))
  (with-failure-expected ((and (alexandria:featurep '(:or :ecl))
                               'failure))
    (is (equal (docstring (locate 'foo 'function)) "FOO function")))
  (is (equal (docstring (locate 'traced-foo 'function))
             "TRACED-FOO function"))
  (is (equal (docstring (locate 'test-gf 'generic-function))
             "TEST-GF generic-function"))
  (with-failure-expected ((and (alexandria:featurep '(:or :abcl :ecl))
                               'failure))
    (is (equal (docstring (locate 'traced-gf 'function))
               "TRACED-GF generic-function")))
  (is (equal (docstring (locate 'test-gf '(method () (number))))
             "TEST-GF (method () (number))"))
  (with-failure-expected ((alexandria:featurep '(:or :abcl :allegro)))
    (is (equal (docstring (locate 'my-comb 'method-combination))
               "MY-COMB method-combination")))
  (is (equal (docstring (locate 'foo-a '(accessor foo)))
             "FOO-A (accessor foo)"))
  (is (equal (docstring (locate 'foo-r '(reader foo)))
             "FOO-R (reader foo)"))
  (is (equal (docstring (locate 'foo-w '(writer foo)))
             "FOO-W (writer foo)"))
  (with-failure-expected ((and (alexandria:featurep '(:or :clisp :ecl))
                               'failure))
    (is (equal (docstring (locate 'baz-aaa '(structure-accessor baz)))
               "BAZ-AAA (structure-accessor baz)")))
  (with-failure-expected ((and (alexandria:featurep '(:or :abcl))
                               'failure))
    (is (equal (docstring (locate 'bar 'type))
               "BAR type")))
  (is (equal (docstring (locate 'foo 'class))
             "FOO class"))
  (is (equal (docstring (locate 'my-error 'condition))
             "MY-ERROR condition"))
  (is (null (docstring (locate 'test-declaration 'declaration))))
  (is (null (docstring (locate 'dref 'asdf:system))))
  (with-failure-expected ((and (alexandria:featurep '(:or :abcl))
                               'failure))
    (is (equal (docstring (locate 'dref 'package))
               "See DREF::@DREF-MANUAL.")))
  (is (match-values (docstring (locate 'sloc 'locative))
        (equal * "SLOC locative")
        (eq * (find-package :dref-test))))
  (is (match-values (docstring (locate 'sloc1 'sloc))
        (equal * "SLOC1 sloc")
        (eq * (find-package :dref-test))))
  (is (match-values (docstring (locate 'var 'locative))
        (equal * "VAR locative")
        (eq * (find-package :dref-test))))
  (dolist (dref (definitions 'print :locative-types '(unknown)))
    (is (null (docstring dref))))
  (is (match-values (docstring (locate nil '(lambda :docstring "xxx"
                                             :docstring-package :dref)))
        (equal * "xxx")
        (eq * (find-package :dref)))))


;;;; TEST-SOURCE-LOCATION

(defparameter *source-location-test-cases*
  '(;; @VARIABLELIKE-LOCATIVES
    (foo-a variable (defvar foo-a))
    (foo-r variable (defvar foo-r))
    (foo-w variable (defvar foo-w))
    (bar constant (defconstant bar))
    ;; @MACROLIKE-LOCATIVES
    (bar macro (defmacro bar))
    (my-smac symbol-macro (define-symbol-macro my-smac))
    (foo compiler-macro (define-compiler-macro foo))
    (has-setf-expander setf (defsetf has-setf-expander) nil
     ;; No source location for DEFSETF on any implementation.
     t)
    (has-setf-function setf (defun (setf has-setf-function)) nil
     #.(alexandria:featurep '(:or :allegro :cmucl)))
    ;; @FUNCTIONLIKE-LOCATIVES
    (foo function (defun foo))
    (traced-foo function (defun traced-foo))
    (test-gf generic-function (defgeneric test-gf))
    (test-gf (method () (number)) (defmethod test-gf))
    (my-comb method-combination (define-method-combination my-comb))
    (foo-a (accessor foo) (defclass foo) (a :accessor foo-a))
    (foo-r (reader foo) (defclass foo) (r :reader foo-r))
    (foo-w (writer foo) (defclass foo) (w :writer foo-w))
    (baz-aaa structure-accessor (defstruct baz) nil #+cmucl t)
    ;; @TYPELIKE-LOCATIVES
    (bar type (deftype bar))
    (foo type (defclass foo))
    (my-error type (define-condition my-error))
    (foo class (defclass foo))
    (test-declaration declaration (define-declaration test-declaration))
    ;; @CONDITION-SYSTEM-LOCATIVES
    (my-error condition (define-condition my-error))
    (some-restart restart (define-restart some-restart))
    ;; @PACKAGELIKE-LOCATIVES
    (mgl-pax asdf:system ())
    (mgl-pax package
     (eval-when (:compile-toplevel :load-toplevel :execute))
     (cl:defpackage))
    (xxx-rt readtable (defreadtable xxx-rt))
    ;; @PAX-LOCATIVES
    (my-loc locative (define-locative-type my-loc))))

(deftest test-source-location ()
  (test-make-source-location)
  (let ((*package* (find-package :dref-test)))
    (dolist (test-case *source-location-test-cases*)
      (apply #'check-source-location test-case)))
  (signals-not (error)
    (source-location (make-xref 'function 'locative)))
  (with-failure-expected ()
    (signals-not (error)
      (source-location (make-xref 'locative 'function)))))

(deftest test-make-source-location ()
  (let ((loc (make-source-location :file #P"xxx"
                                   :file-position 7
                                   :buffer "bbb"
                                   :snippet "(defun foo ())")))
    (is (equal (source-location-file loc) "xxx"))
    (is (eql (source-location-file-position loc) 7))
    (is (equal (source-location-buffer loc) "bbb"))
    (is (equal (source-location-buffer-position loc) 8))
    (is (equal (source-location-snippet loc) "(defun foo ())"))))

(defun working-locative-p (locative)
  (let ((type (locative-type locative)))
    (cond ((alexandria:featurep :abcl)
           nil)
          ((alexandria:featurep :clisp)
           nil)
          ((eq type 'symbol-macro)
           (alexandria:featurep '(:not :ccl)))
          ((eq type 'declaration)
           (alexandria:featurep :sbcl))
          ((eq type 'readtable)
           nil)
          ((eq type 'generic-function)
           ;; AllegroCL is off by one form.
           (alexandria:featurep '(:not :allegro)))
          ((eq type 'method-combination)
           (alexandria:featurep '(:not (:or :abcl :cmucl :ecl))))
          ((member type '(reader writer accessor))
           (alexandria:featurep '(:not (:or :abcl :cmucl :ecl))))
          ((eq type 'structure-accessor)
           (alexandria:featurep '(:not (:or :abcl :allegro :ecl))))
          ((eq type 'type)
           (alexandria:featurep '(:not :ecl)))
          ((eq type 'package)
           (alexandria:featurep '(:not (:or :abcl :allegro :clisp :cmucl
                                            :ecl))))
          (t
           t))))

(defun check-source-location (name locative prefix &optional alternative-prefix
                                                     failure-expected-p)
  (let* ((ref (make-xref name locative))
         (resolved (resolve ref nil)))
    ;; Test SOURCE-LOCATION with a REFERENCE and a resolved object if
    ;; there is one.
    (dolist (target (if resolved
                        (list ref resolved)
                        (list ref)))
      (with-test ((format nil "navigate to ~S" target))
        (with-failure-expected ((or (not (working-locative-p locative))
                                    failure-expected-p))
          (signals-not (error :msg (format nil "SOURCE-LOCATION ~S runs"
                                           target))
            (let ((location (source-location target)))
              (when (is (and (% location) (not (eq :error (first location))))
                        :msg `("Find source location for (~S ~S)."
                               ,name ,locative))
                (multiple-value-bind (file position)
                    (extract-source-location location)
                  (when (is file)
                    (let ((content (if (stringp prefix)
                                       (read-string-from-file-position
                                        file (or position 0)
                                        (max (length prefix)
                                             (length alternative-prefix)))
                                       (read-form-from-file-position
                                        file (or position 0)))))
                      (is (and (eq (not (listp prefix))
                                   (not (listp content)))
                               (or (alexandria:starts-with-subseq
                                    prefix content :test #'equal)
                                   (and alternative-prefix
                                        (alexandria:starts-with-subseq
                                         alternative-prefix content
                                         :test #'equal))))
                          :msg `("Find prefix ~S~@[ or ~S~] ~
                                    at source location~%~S~% ~
                                    for reference (~S ~S).~%~
                                    Form found was:~%~S."
                                 ,prefix ,alternative-prefix
                                 ,location ,name ,locative
                                 ,content)))))))))))))

(defun extract-source-location (location)
  (values (source-location-file location)
          (source-location-file-position location)))

(defun read-string-from-file-position (filename position n)
  (with-open-file (stream filename
                          :direction :input
                          :external-format dref::*utf-8-external-format*)
    (file-position stream position)
    (let ((string (make-string n)))
      (read-sequence string stream)
      string)))

(defun read-form-from-file-position (filename position)
  (with-open-file (stream filename
                          :direction :input
                          :external-format dref::*utf-8-external-format*)
    (file-position stream position)
    (read stream)))


;;;; TEST-APROPOS

(defun mgl-pax::%test9jwern% ())
(defun %test9jwern% ())

(deftest test-apropos ()
  (with-failure-expected ((alexandria:featurep '(:or :abcl :clisp)))
    (with-test ("NAME is NIL")
      (check-ref-sets (dref-apropos nil :package :dref-test :external-only t)
                      `(,(make-xref 'test 'function)
                        ,(make-xref 'check-ref 'function)
                        ,(make-xref 'check-ref-sets 'function)
                        ,(make-xref 'check-source-location 'function)))
      (is (null (find-if (lambda (dref)
                           (stringp (dref-name dref)))
                         (dref-apropos nil :package (find-package :dref))))))
    (with-test ("NAME is a SYMBOL")
      (with-test ("PACKAGE is a symbol")
        (check-ref-sets (dref-apropos '%test9jwern% :package :mgl-pax)
                        `(,(make-xref 'mgl-pax::%test9jwern% 'function))))
      (with-test ("PACKAGE is a symbol matching a nickname")
        (check-ref-sets (dref-apropos '%test9jwern% :package :pax)
                        `(,(make-xref 'mgl-pax::%test9jwern% 'function))))
      (with-test ("NAME is a lower-case symbol")
        (check-ref-sets (dref-apropos '#:|%test9jwern%| :package :dref-test)
                        `(,(make-xref '%test9jwern% 'function)))))
    (with-test ("NAME is STRING")
      (with-test ("full match")
        (check-ref-sets (dref-apropos "test" :package :dref-test
                                             :external-only t)
                        `(,(make-xref 'dref-test:test 'function))))
      (with-test ("partial match")
        (check-ref-sets (dref-apropos "es" :package :dref-test
                                           :external-only t)
                        `(,(make-xref 'dref-test:test 'function))))
      (with-test ("case-sensitive")
        (with-test ("no match")
          (check-ref-sets (dref-apropos "es" :package :dref-test
                                             :external-only t
                                             :case-sensitive t)
                          ()))
        (with-test ("match")
          (check-ref-sets (dref-apropos "ES" :package :dref-test
                                             :external-only t
                                             :case-sensitive t)
                          `(,(make-xref 'dref-test:test 'function))))))
    (with-test ("PACKAGE is NIL")
      (check-ref-sets (dref-apropos '%test9jwern%)
                      `(,(make-xref 'dref-test::%test9jwern% 'function)
                        ,(make-xref 'mgl-pax::%test9jwern% 'function))))
    (with-test ("PACKAGE is STRING")
      (with-test ("full match")
        (check-ref-sets (dref-apropos '%test9jwern% :package "dref-test")
                        `(,(make-xref 'dref-test::%test9jwern% 'function))))
      (with-test ("partial match")
        (check-ref-sets (dref-apropos '%test9jwern% :package "dref")
                        `(,(make-xref 'dref-test::%test9jwern% 'function))))
      (with-test ("case-sensitive")
        (with-test ("no match")
          (check-ref-sets (dref-apropos '%test9jwern% :package "dref"
                                                      :case-sensitive t)
                          ()))
        (with-test ("match")
          (check-ref-sets (dref-apropos '%test9jwern% :package "DREF"
                                                      :case-sensitive t)
                          `(,(make-xref 'dref-test::%test9jwern%
                                        'function))))))
    (with-test ("PACKAGE is :NONE")
      (check-ref-sets (dref-apropos "dref" :package :none)
                      `(,(make-xref (package-name :dref) 'package)
                        ,(make-xref (package-name :dref-ext) 'package)
                        ,(make-xref (package-name :dref-test) 'package)
                        ,(make-xref "dref" 'asdf:system)
                        ,(make-xref "dref/full" 'asdf:system)
                        ,(make-xref "dref/test" 'asdf:system)
                        ,(make-xref "dref/test-autoload" 'asdf:system)))))
  (with-test ("LOCATIVE-TYPES")
    (with-test ("only asdf systems")
      (is (= (length (dref-apropos nil :locative-types '(asdf:system)))
             (length (asdf:registered-systems)))))
    (with-test ("only packages")
      (is (= (length (dref-apropos nil :locative-types '(package)))
             (length (list-all-packages)))))
    (with-test ("asdf systems and packages")
      (is (= (length (dref-apropos nil :locative-types '(asdf:system package)))
             (+ (length (asdf:registered-systems))
                (length (list-all-packages))))))
    #+sbcl
    (with-test (":PSEUDO")
      (is (plusp (length (% (dref-apropos 'print
                                          :locative-types '(:pseudo)))))))
    #+sbcl
    (with-test (":ALL")
      (is (= (+ (length (% (dref-apropos 'print
                                         :locative-types '(:lisp))))
                (length (% (dref-apropos 'print
                                         :locative-types '(:pseudo)))))
             (length (% (dref-apropos 'print
                                      :locative-types '(:all)))))))))


(deftest test-all ()
  (test-util)
  (test-locate)
  (test-definitions)
  (test-dspec)
  (test-arglist)
  (test-docstring)
  (test-source-location)
  (test-apropos))

(defun test (&key (debug nil) (print 'unexpected) (describe *describe*))
  ;; Bind *PACKAGE* so that names of tests printed have package names,
  ;; and M-. works on them in Slime.
  (let ((*package* (find-package :common-lisp))
        (*print-duration* nil)
        (*print-compactly* t)
        (*defer-describe* t))
    (warn-on-tests-not-run ((find-package :dref-test))
      (print (try 'test-all :debug debug :print print :describe describe)))))

#+nil
(test)

#+nil
(test-all)
