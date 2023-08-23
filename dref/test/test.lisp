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
                    `(,(xref '*some-var* 'variable))))
  (with-failure-expected ((and (alexandria:featurep '(:or :clisp))
                               'failure))
    (check-ref-sets (definitions 'my-smac)
                    `(,(xref 'my-smac 'symbol-macro))))
  (check-ref-sets (definitions 'foo2) (list (xref 'foo2 'function)))
  (check-ref-sets (definitions 'foo) (list (xref 'foo 'function)
                                           (xref 'foo 'class)
                                           (xref 'foo 'compiler-macro)))
  (check-ref-sets (definitions 'dref) (list (dref 'dref 'function)
                                            (dref 'dref 'package)
                                            (xref "dref" 'asdf:system)
                                            (xref 'dref 'class)))
  (check-ref-sets (definitions "dref")
                  (if (string= (package-name 'dref) "dref")
                      (list (xref "dref" 'package)
                            (xref "dref" 'asdf:system))
                      (list (xref "dref" 'asdf:system))))
  (with-failure-expected ((and (alexandria:featurep '(:or :ecl :clisp))
                               'failure))
    (check-ref-sets (definitions 'bar)
                    `(,(xref 'bar 'macro)
                      ,(xref 'bar 'constant)
                      ,(xref 'bar 'type))))
  (with-failure-expected ((and (alexandria:featurep
                                '(:or :abcl :clisp :cmucl :ecl))
                               'failure))
    (check-ref-sets (definitions 'has-setf-expander)
                    `(,(xref 'has-setf-expander 'setf))))
  (with-failure-expected ((and (alexandria:featurep '(:or :abcl :clisp))
                               'failure))
    (check-ref-sets (definitions 'test-gf)
                    `(,(xref 'test-gf 'generic-function)
                      ,(xref 'test-gf '(method () (number)))
                      ,(xref 'test-gf '(method () ((eql 7))))
                      ,(xref 'test-gf '(method ()
                                             ((eql #.(find-package :cl))))))))
  (with-failure-expected ((and (alexandria:featurep
                                '(:or :abcl :clisp :cmucl :ecl))
                               'failure))
    (check-ref-sets (definitions 'my-comb)
                    `(,(xref 'my-comb 'method-combination))))
  ;; There _may_ be a GENERIC-FUNCTION and a SETF generic function too.
  (with-failure-expected ((and (alexandria:featurep '(:or :abcl :clisp :ecl))
                               'failure))
    (check-ref-sets (remove-if (lambda (dref)
                                 (member (dref-locative-type dref)
                                         '(generic-function setf)))
                               (definitions 'foo-a))
                    `(,(xref 'foo-a 'variable)
                      ,(xref 'foo-a '(accessor foo))))
    (check-ref-sets (remove 'generic-function (definitions 'foo-r)
                            :key #'dref-locative-type)
                    `(,(xref 'foo-r 'variable)
                      ,(xref 'foo-r '(reader foo))))
    (check-ref-sets (remove 'generic-function (definitions 'foo-w)
                            :key #'dref-locative-type)
                    `(,(xref 'foo-w 'variable)
                      ,(xref 'foo-w '(writer foo)))))
  ;; BAZ-AAA is not recognized as a structure accessor on most Lisps.
  ;; On CCL, (SETF BAZ-AAA) shows up as well. FIXME: Maybe that's how
  ;; it should be?
  (with-failure-expected ((and (alexandria:featurep '(:not (:or :sbcl)))
                               'failure))
    (check-ref-sets (definitions 'baz-aaa)
                    `(,(xref 'baz-aaa '(structure-accessor baz)))))
  (with-failure-expected ((alexandria:featurep '(:not :sbcl)))
    (check-ref-sets (definitions 'dynamic-extent)
                    `(,(xref 'dynamic-extent 'declaration))))
  (check-ref-sets (definitions 'my-error)
                  `(,(xref 'my-error 'condition)
                    ,(xref 'my-error 'function)))
  (with-failure-expected ((alexandria:featurep '(:not :sbcl)))
    (check-ref-sets (definitions 'test-declaration)
                    `(,(xref 'test-declaration 'declaration))))
  (check-ref-sets (definitions 'xxx-rt)
                  `(,(xref 'xxx-rt 'readtable)))
  (check-ref-sets (definitions 'my-loc)
                  `(,(xref 'my-loc 'locative)))
  #+sbcl
  (check-ref-sets (definitions 'print
                               :locative-types (cons 'unknown
                                                     (lisp-locative-types)))
                  `(,(xref 'print 'function)
                    ,(xref 'print '(unknown
                                         (:defoptimizer print
                                             sb-c:derive-type)))
                    ,(xref 'print '(unknown
                                         (:define-vop print print)))
                    ,(xref 'print '(unknown
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
  (check-dspec-roundtrip (dref '*some-var* 'variable))
  (check-dspec-roundtrip (dref 'bar 'constant))
  (check-dspec-roundtrip (dref 'bar 'macro))
  (check-dspec-roundtrip (dref 'block 'macro))
  (check-dspec-roundtrip (dref 'foo 'compiler-macro))
  (check-dspec-roundtrip (dref 'my-smac 'symbol-macro))
  (check-dspec-roundtrip (dref 'has-setf-expander 'setf))
  (check-dspec-roundtrip (dref 'has-setf-function 'setf))
  (check-dspec-roundtrip (dref 'foo 'function))
  (check-dspec-roundtrip (dref 'test-gf 'generic-function))
  (with-failure-expected ((alexandria:featurep '(:or :ecl)))
    (check-dspec-roundtrip (dref 'test-gf '(method () ((eql 7)))))
    (check-dspec-roundtrip
     (dref 'test-gf '(method () ((eql #.(find-package :cl)))))))
  (check-dspec-roundtrip (dref 'gf2 '(method (:around) (t))))
  (check-dspec-roundtrip (dref 'gf2 '(method (:after) (t))))
  (check-dspec-roundtrip (dref 'my-comb 'method-combination))
  (check-dspec-roundtrip (dref 'bar 'type))
  (check-dspec-roundtrip (dref 'foo 'class))
  (check-dspec-roundtrip (dref 'array 'class))
  (check-dspec-roundtrip (dref 'my-error 'condition))
  (check-dspec-roundtrip (dref  'dref 'package))
  (let ((dref (dref 'print '(unknown (:define-vop print print)) nil)))
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
  (is (null (arglist (dref 'my-smac 'symbol-macro))))
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
    (is (or (equal (% (arglist (dref 'macro-with-fancy-args 'macro)))
                   '(x &optional (o 1) &key (k 2 kp)))
            (equal (arglist (dref 'macro-with-fancy-args 'macro))
                   '(x &optional (o 1) &key (k 2))))))
  (with-test ("special operator")
    (is (match-values (arglist (dref 'function 'macro))
          (null *)))))

(deftest test-arglist/compiler-macro ()
  (with-failure-expected ((and (alexandria:featurep
                                '(:or :abcl :clisp :ecl))
                               'failure))
    (is (match-values (arglist (dref 'cmac 'compiler-macro))
          (equal * '(x &rest y))
          (eq * :macro)))))

(deftest test-arglist/setf ()
  (is (null (arglist (dref 'has-setf-expander 'setf))))
  (is (match-values (arglist (dref 'has-setf-function 'setf))
        (equal * '(v))
        (eq * :ordinary)))
  (is (match-values (arglist (dref 'has-setf-generic-function 'setf))
        (equal * '(v))
        (eq * :ordinary)))
  (is (match-values (arglist (dref 'has-setf-generic-function
                                          '(setf (method () (string)))))
        (equal * '(v))
        (eq * :ordinary))))

(deftest test-arglist/function ()
  (is (match-values (arglist (dref 'function-with-fancy-args 'function))
        (equal * '(x &optional (o 1) &key (k 2 kp)))
        (eq * :ordinary)))
  (with-failure-expected ((and (alexandria:featurep '(:or :abcl :clisp :ecl))
                               'failure))
    (is (match-values (arglist (dref 'traced-foo 'function))
          (equal * '(x))
          (eq * :ordinary)))))

(deftest test-arglist/generic-function ()
  (is (match-values (arglist (dref 'test-gf 'generic-function))
        (equal * '(x))
        (eq * :ordinary)))
  (with-failure-expected ((and (alexandria:featurep '(:or :clisp :ecl))
                               'failure))
    (is (match-values (arglist (dref 'traced-gf 'function))
          (equal * '(x))
          (eq * :ordinary)))))

(deftest test-arglist/method ()
  (is (match-values (arglist (dref 'gf2 '(method (:around) (t))))
        (equal * '(x &key))
        (eq * :ordinary)))
  (is (match-values (arglist (dref 'gf2 '(method (:after) (t))))
        (equal * '(x &key y))
        (eq * :ordinary)))
  (is (match-values (arglist (dref 'gf2 '(method () (number))))
        (equal * '(x &key ((:x y) t)))
        (eq * :ordinary))))

(deftest test-arglist/type ()
  (with-failure-expected ((alexandria:featurep '(:or :abcl :allegro :clisp
                                                 :ccl :cmucl :ecl)))
    (is (match-values (arglist (dref 'bar 'type))
          (equal * '(x &rest r))
          (eq * :deftype)))))

(deftest test-arglist/locative ()
  (is (match-values (arglist (dref 'loc-with-args 'locative))
        (equal * '((x y) &key z))
        (eq * :destructuring))))

(deftest test-arglist/symbol-locative ()
  (is (match-values (arglist (dref 'sloc1 'sloc))
        (equal * '(&key z))
        (eq * :macro))))

(deftest test-arglist/lambda ()
  (is (match-values (arglist (dref nil '(lambda :arglist ((x y) z)
                                                :arglist-type :macro)))
        (equal * '((x y) z))
        (eq * :macro))))


(deftest test-docstring ()
  (is (equal (docstring (xref 'foo-a 'variable))
             "FOO-A variable"))
  (is (equal (docstring (dref 'foo-a 'variable)) "FOO-A variable"))
  (is (equal (docstring (dref 'bar 'constant)) "BAR constant"))
  (is (equal (docstring (dref 'bar 'macro)) "BAR macro"))
  (signals-not (error)
    (docstring (dref 'function 'macro)))
  (is (equal (docstring (dref 'my-smac 'symbol-macro))
             "MY-SMAC symbol-macro"))
  (with-failure-expected ((and (alexandria:featurep
                                '(:or :abcl :allegro :ccl :ecl))
                               'failure))
    (is (equal (docstring (dref 'foo 'compiler-macro))
               "FOO compiler-macro")))
  (with-failure-expected ((and (alexandria:featurep '(:or :abcl))
                               'failure))
    (is (equal (docstring (dref 'has-setf-expander 'setf))
               "HAS-SETF-EXPANDER setf"))
    (is (equal (docstring (dref 'has-setf-function 'setf))
               "HAS-SETF-FUNCTION setf")))
  (with-failure-expected ((and (alexandria:featurep '(:or :cmucl))
                               'failure))
    (is (equal (docstring (dref 'has-setf-generic-function 'setf))
               "HAS-SETF-GENERIC-FUNCTION setf")))
  (is (equal (docstring (dref 'has-setf-generic-function
                                     '(setf (method () (string)))))
             "HAS-SETF-GENERIC-FUNCTION (setf (method () (string)))"))
  (with-failure-expected ((and (alexandria:featurep '(:or :ecl))
                               'failure))
    (is (equal (docstring (dref 'foo 'function)) "FOO function")))
  (is (equal (docstring (dref 'traced-foo 'function))
             "TRACED-FOO function"))
  (is (equal (docstring (dref 'test-gf 'generic-function))
             "TEST-GF generic-function"))
  (with-failure-expected ((and (alexandria:featurep '(:or :abcl :ecl))
                               'failure))
    (is (equal (docstring (dref 'traced-gf 'function))
               "TRACED-GF generic-function")))
  (is (equal (docstring (dref 'test-gf '(method () (number))))
             "TEST-GF (method () (number))"))
  (with-failure-expected ((alexandria:featurep '(:or :abcl :allegro)))
    (is (equal (docstring (dref 'my-comb 'method-combination))
               "MY-COMB method-combination")))
  (is (equal (docstring (dref 'foo-a '(accessor foo)))
             "FOO-A (accessor foo)"))
  (is (equal (docstring (dref 'foo-r '(reader foo)))
             "FOO-R (reader foo)"))
  (is (equal (docstring (dref 'foo-w '(writer foo)))
             "FOO-W (writer foo)"))
  (with-failure-expected ((and (alexandria:featurep '(:or :clisp :ecl))
                               'failure))
    (is (equal (docstring (dref 'baz-aaa '(structure-accessor baz)))
               "BAZ-AAA (structure-accessor baz)")))
  (with-failure-expected ((and (alexandria:featurep '(:or :abcl))
                               'failure))
    (is (equal (docstring (dref 'bar 'type))
               "BAR type")))
  (is (equal (docstring (dref 'foo 'class))
             "FOO class"))
  (is (equal (docstring (dref 'my-error 'condition))
             "MY-ERROR condition"))
  (is (null (docstring (dref 'test-declaration 'declaration))))
  (is (null (docstring (dref 'dref 'asdf:system))))
  (with-failure-expected ((and (alexandria:featurep '(:or :abcl))
                               'failure))
    (is (equal (docstring (dref 'dref-test 'package))
               "Test package for DRef.")))
  (is (match-values (docstring (dref 'sloc 'locative))
        (equal * "SLOC locative")
        (eq * (find-package :dref-test))))
  (is (match-values (docstring (dref 'sloc1 'sloc))
        (equal * "SLOC1 sloc")
        (eq * (find-package :dref-test))))
  (is (match-values (docstring (dref 'var 'locative))
        (equal * "VAR locative")
        (eq * (find-package :dref-test))))
  (dolist (dref (definitions 'print :locative-types '(unknown)))
    (is (null (docstring dref))))
  (is (match-values (docstring (dref nil '(lambda :docstring "xxx"
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
    (source-location (xref 'function 'locative)))
  (with-failure-expected ()
    (signals-not (error)
      (source-location (xref 'locative 'function)))))

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
  (let* ((ref (xref name locative))
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
                      `(,(xref 'test 'function)
                        ,(xref 'check-ref 'function)
                        ,(xref 'check-ref-sets 'function)
                        ,(xref 'check-source-location 'function)))
      (is (null (find-if (lambda (dref)
                           (stringp (dref-name dref)))
                         (dref-apropos nil :package (find-package :dref))))))
    (with-test ("NAME is a SYMBOL")
      (with-test ("PACKAGE is a symbol")
        (check-ref-sets (dref-apropos '%test9jwern% :package :mgl-pax)
                        `(,(xref 'mgl-pax::%test9jwern% 'function))))
      (with-test ("PACKAGE is a symbol matching a nickname")
        (check-ref-sets (dref-apropos '%test9jwern% :package :pax)
                        `(,(xref 'mgl-pax::%test9jwern% 'function))))
      (with-test ("NAME is a lower-case symbol")
        (check-ref-sets (dref-apropos '#:|%test9jwern%| :package :dref-test)
                        `(,(xref '%test9jwern% 'function)))))
    (with-test ("NAME is STRING")
      (with-test ("full match")
        (check-ref-sets (dref-apropos "test" :package :dref-test
                                             :external-only t)
                        `(,(xref 'dref-test:test 'function))))
      (with-test ("partial match")
        (check-ref-sets (dref-apropos "es" :package :dref-test
                                           :external-only t)
                        `(,(xref 'dref-test:test 'function))))
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
                          `(,(xref 'dref-test:test 'function))))))
    (with-test ("PACKAGE is NIL")
      (check-ref-sets (dref-apropos '%test9jwern%)
                      `(,(xref 'dref-test::%test9jwern% 'function)
                        ,(xref 'mgl-pax::%test9jwern% 'function))))
    (with-test ("PACKAGE is STRING")
      (with-test ("full match")
        (check-ref-sets (dref-apropos '%test9jwern% :package "dref-test")
                        `(,(xref 'dref-test::%test9jwern% 'function))))
      (with-test ("partial match")
        (check-ref-sets (dref-apropos '%test9jwern% :package "dref")
                        `(,(xref 'dref-test::%test9jwern% 'function))))
      (with-test ("case-sensitive")
        (with-test ("no match")
          (check-ref-sets (dref-apropos '%test9jwern% :package "dref"
                                                      :case-sensitive t)
                          ()))
        (with-test ("match")
          (check-ref-sets (dref-apropos '%test9jwern% :package "DREF"
                                                      :case-sensitive t)
                          `(,(xref 'dref-test::%test9jwern%
                                        'function))))))
    (with-test ("PACKAGE is :NONE")
      (check-ref-sets (dref-apropos "dref" :package :none)
                      `(,(xref (package-name :dref) 'package)
                        ,(xref (package-name :dref-ext) 'package)
                        ,(xref (package-name :dref-test) 'package)
                        ,(xref "dref" 'asdf:system)
                        ,(xref "dref/full" 'asdf:system)
                        ,(xref "dref-test" 'asdf:system)
                        ,(xref "dref-test/autoload" 'asdf:system)))))
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
