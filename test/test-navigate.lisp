(in-package :mgl-pax-test)

(deftest test-navigate ()
  (test-dspecs)
  (test-read-locative-from-string)
  (test-read-object-from-string)
  (test-read-reference-from-string)
  (test-locate-definitions-for-emacs)
  (test-navigation-to-source)
  (test-misc))

(deftest test-read-locative-from-string ()
  (let ((*package* (find-package :mgl-pax-test)))
    (unintern (read-from-string "non-interned"))
    (unintern (read-from-string "yyy"))
    (is (null (mgl-pax::read-locative-from-string "non-interned")))
    (is (null (find-symbol (string '#:non-interned))))
    (is (null (mgl-pax::read-locative-from-string "find")))
    (is (null (mgl-pax::read-locative-from-string "")))
    (is (match-values (mgl-pax::read-locative-from-string "function")
          (eq * 'function)
          (= * 8)))
    (is (match-values (mgl-pax::read-locative-from-string " function")
          (eq *'function)
          (= * 9)))
    (is (match-values (mgl-pax::read-locative-from-string "function ")
          (eq * 'function)
          (= * 9)))
    (is (match-values (mgl-pax::read-locative-from-string "function junk")
          (null *)))
    (is (match-values (mgl-pax::read-locative-from-string "function junk"
                                                          :junk-allowed t)
          (eq * 'function)
          (= * 9)))
    (let ((locative (mgl-pax::read-locative-from-string "(function yyy)")))
      (is (eq (first locative) 'function))
      (is (string= (symbol-name (second locative)) (string '#:yyy)))
      (is (eq (symbol-package (second locative)) *package*)))))

(defun test-read-object-from-string ()
  (let ((*package* (find-package :mgl-pax-test)))
    (is (eq (mgl-pax::read-object-from-string "deftest") 'deftest))
    (is (eq (mgl-pax::read-object-from-string "MGL-PAX::@CODIFIABLE")
            'mgl-pax::@codifiable))))

(deftest test-read-reference-from-string ()
  (let ((*package* (find-package :mgl-pax-test)))
    (unintern (read-from-string "non-interned"))
    (unintern (read-from-string "yyy"))
    (is (null (mgl-pax::read-reference-from-string "yyy non-interned")))
    (is (null (find-symbol (string '#:non-interned))))
    (is (null (find-symbol (string '#:yyy))))
    (is (null (mgl-pax::read-reference-from-string "yyy (non-interned)")))
    (is (null (find-symbol (string '#:non-interned))))
    (is (null (find-symbol (string '#:yyy))))
    (is (null (mgl-pax::read-reference-from-string "yyy find")))
    (is (null (find-symbol (string '#:yyy))))
    (is (match-values (mgl-pax::read-reference-from-string "yyy function")
          (string= * "YYY")
          (eq * 'function)
          (eq * t)
          (null *)))
    (is (match-values (mgl-pax::read-reference-from-string " yyy  function ")
          (string= * "YYY")
          (eq * 'function)
          (eq * t)
          (null *)))
    (is (match-values
            (mgl-pax::read-reference-from-string " yyy  function  xxx ")
          (null *)
          (null *)
          (null *)
          (string= * "function  xxx")))
    (is (mgl-pax::read-reference-from-string "mgl-pax:@codification section")
        :msg "internal symbol with single :")
    (with-test ("multiple locatives")
      (is (match-values
              (mgl-pax::read-reference-from-string "foo function type"
                                                   :multiple-locatives-p t)
            (eq * 'foo)
            (equal * '(function type))
            (eq * t)
            (null *)))
      (is (match-values
              (mgl-pax::read-reference-from-string "foo function xxx"
                                                   :multiple-locatives-p t)
            (eq * 'foo)
            (equal * '(function))
            (eq * t)
            (string= * "xxx")))
      (is (match-values
              (mgl-pax::read-reference-from-string "foo xxx type"
                                                   :multiple-locatives-p t)
            (eq * nil)
            (equal * '())
            (eq * nil)
            (string= * "xxx type"))))))

;;; We have no Emacs based tests. This tests the CL side of `M-.' when
;;; it's invoked with point on FOO in the test cases below. The actual
;;; OBJECT-AND-LOCATIVES-LIST argument that mgl-pax.el would send is
;;; reproduced explicitly.
(deftest test-locate-definitions-for-emacs ()
  (let ((*package* (find-package '#:mgl-pax-test)))
    ;; xxx FOO function
    (check-ldfe '(("FOO" ("xxx" "function")))
                '(("MGL-PAX-TEST::FOO" "FUNCTION")))
    ;; function FOO xxx
    (check-ldfe '(("FOO" ("function" "xxx")))
                '(("MGL-PAX-TEST::FOO" "FUNCTION")))
    ;; xxx [foo][function] xxx
    (check-ldfe '(("[foo][function]" ("xxx" "xxx"))
                  ("foo" ("function")))
                '(("MGL-PAX-TEST::FOO" "FUNCTION")))
    ;; function FOO compiler-macro
    (check-ldfe '(("FOO" ("function" "compiler-macro")))
                '(("MGL-PAX-TEST::FOO" "FUNCTION")
                  ("MGL-PAX-TEST::FOO" "COMPILER-MACRO")))
    (with-failure-expected ((alexandria:featurep :clisp))
      ;; xxx FOO xxx
      (check-ldfe '(("FOO" ("xxx" "xxx")))
                  '(("MGL-PAX-TEST::FOO" "COMPILER-MACRO")
                    ("MGL-PAX-TEST::FOO" "FUNCTION")
                    ("MGL-PAX-TEST::FOO" "CLASS")))
      ;; xxx [foo][] xxx
      (check-ldfe '(("[foo][]" ("xxx" "xxx"))
                    ("foo" ("xxx")))
                  '(("MGL-PAX-TEST::FOO" "COMPILER-MACRO")
                    ("MGL-PAX-TEST::FOO" "FUNCTION")
                    ("MGL-PAX-TEST::FOO" "CLASS")))
      ;; xxx [foo][xxx] xxx
      (check-ldfe '(("[foo][xxx]" ("xxx" "xxx"))
                    ("foo" ("xxx")))
                  '(("MGL-PAX-TEST::FOO" "COMPILER-MACRO")
                    ("MGL-PAX-TEST::FOO" "FUNCTION")
                    ("MGL-PAX-TEST::FOO" "CLASS"))))
    (with-failure-expected ((alexandria:featurep :abcl))
      ;; pax
      (check-ldfe '(("pax" ()))
                  '(("MGL-PAX" "PACKAGE")))
      ;; "MGL-PAX"
      (check-ldfe '(("\"MGL-PAX\"" ()))
                  '(("MGL-PAX" "PACKAGE")
                    ("mgl-pax" "ASDF/SYSTEM:SYSTEM")))
      ;; MGL-PAX
      (check-ldfe '(("MGL-PAX" ()))
                  '(("MGL-PAX" "PACKAGE")
                    ("mgl-pax" "ASDF/SYSTEM:SYSTEM")))
      ;; :MGL-PAX
      (check-ldfe '((":MGL-PAX" ()))
                  '(("MGL-PAX" "PACKAGE")
                    ("mgl-pax" "ASDF/SYSTEM:SYSTEM"))))))

(defun check-ldfe (object-and-locatives-list expected-emacsrefs)
  (let ((emacsrefs (sort-emacsrefs
                    (pax::locate-definitions-for-emacs-1
                     object-and-locatives-list :as-ref t)))
        (expected-emacsrefs (sort-emacsrefs expected-emacsrefs)))
    (is (equal emacsrefs expected-emacsrefs)
        :ctx ("OBJECT-AND-LOCATIVES-LIST = ~S" object-and-locatives-list))))

(defun sort-emacsrefs (emacsrefs)
  (sort (copy-seq emacsrefs) (lambda (r1 r2)
                               (or (string< (first r1) (first r2))
                                   (and (string= (first r1) (first r2))
                                        (string< (second r1) (second r2)))))))


(defclass ccc ()
  ((r :reader ccc-r)
   (w :writer ccc-w)
   (a :accessor ccc-a)))
(defmethod ccc-r2 ((ccc ccc))
  (slot-value ccc 'r))
(defmethod set-ccc-w2 (value (ccc ccc))
  (setf (slot-value ccc 'w) value))
(define-condition cocc () ())
(defmacro mmm ())
(defgeneric ggg ())
(define-symbol-macro symmac ())

(deftest test-dspecs ()
  (check-dspec-roundtrip (make-reference 'foo 'variable))
  (with-test ("non-existent constant")
    (with-failure-expected ((alexandria:featurep :allegro))
      (check-dspec-roundtrip (make-reference 'foo 'constant))))
  (with-test ("existing constant")
    (check-dspec-roundtrip (make-reference 'pi 'constant)))
  (with-test ("non-existent macro")
    (with-failure-expected ((alexandria:featurep '(:or :allegro :ccl)))
      (check-dspec-roundtrip (make-reference 'foo 'macro))))
  (check-dspec-roundtrip (make-reference 'block 'macro))
  (with-test ("existing macro")
    (check-dspec-roundtrip (make-reference 'mmm 'macro)))
  (check-dspec-roundtrip (make-reference 'foo 'compiler-macro))
  (check-dspec-roundtrip (make-reference 'symmac 'symbol-macro))
  (check-dspec-roundtrip (make-reference 'foo 'function))
  (with-test ("non-existent generic-function")
    (with-failure-expected ((alexandria:featurep '(:or :allegro :ccl)))
      (check-dspec-roundtrip (make-reference 'foo 'generic-function))))
  (with-test ("existing generic-function")
    (check-dspec-roundtrip (make-reference 'ggg 'generic-function)))
  (check-dspec-roundtrip (make-reference 'foo '(method () ((eql 5) t))))
  (check-dspec-roundtrip (make-reference 'foo '(method (:around) ((eql 5) t))))
  (check-dspec-roundtrip (make-reference 'foo '(method (:around) ((eql 5) t))))
  (check-dspec-roundtrip (make-reference 'foo 'method-combination))
  ;; Based on the dspec only, we often can't tell a method defined
  ;; with :READER, :WRITER, or :ACCESSOR in DEFLCASS from a method
  ;; defined with DEFMETHOD.
  (with-test ("existing reader")
    (with-failure-expected ((alexandria:featurep '(:or :abcl :clisp :ecl)))
      (check-dspec-roundtrip (make-reference 'ccc-r '(reader ccc)))))
  (with-test ("non-existent reader")
    (with-failure-expected ((alexandria:featurep '(:or :abcl :clisp :ecl)))
      (check-dspec-roundtrip (make-reference 'foo '(reader ccc))
                             #-(or ccl cmucl)
                             (make-reference 'foo '(method () (ccc))))))
  (with-test ("existing writer")
    (with-failure-expected ((alexandria:featurep '(:or :abcl :clisp :ecl)))
      (check-dspec-roundtrip (make-reference 'ccc-w '(writer ccc)))))
  (with-test ("non-existent writer")
    (with-failure-expected ((alexandria:featurep '(:or :abcl :clisp :ecl)))
      (check-dspec-roundtrip (make-reference 'foo '(writer ccc))
                             #-(or ccl cmucl)
                             (make-reference 'foo '(method () (t ccc))))))
  (with-test ("existing accessor")
    (with-failure-expected ((alexandria:featurep '(:or :abcl :clisp :ecl)))
      (check-dspec-roundtrip (make-reference 'ccc-a '(accessor ccc)))))
  (with-test ("non-existent accessor")
    (with-failure-expected ((alexandria:featurep '(:or :abcl :clisp :ecl)))
      (check-dspec-roundtrip (make-reference 'foo '(accessor ccc))
                             #+(or ccl cmucl)
                             (make-reference 'foo '(writer ccc))
                             #-(or ccl cmucl)
                             (make-reference 'foo '(method () (t ccc))))))
  (check-dspec-roundtrip (make-reference 'nnn 'type))
  (with-test ("existing class")
    (check-dspec-roundtrip (make-reference 'ccc 'class)))
  (with-test ("non-existent class")
    (check-dspec-roundtrip (make-reference 'nnn 'class)
                           #+allegro
                           (make-reference 'nnn 'type)))
  (check-dspec-roundtrip (make-reference 'array 'class))
  (with-test ("non-existent condition")
    (with-failure-expected ((not (alexandria:featurep :sbcl)))
      (check-dspec-roundtrip (make-reference 'foo 'condition))))
  (with-test ("existing condition")
    (check-dspec-roundtrip (make-reference 'cocc 'condition)))
  (check-dspec-roundtrip (make-reference 'foo 'package))
  (check-dspec-roundtrip (make-reference "foo" 'package))
  (check-dspec-roundtrip (make-reference 'pax::@links 'variable)
                         (make-reference 'pax::@links 'section))
  (check-dspec-roundtrip (make-reference 'pax::@object 'variable)
                         (make-reference 'pax::@object 'glossary-term)))

(defun check-dspec-roundtrip (reference &optional (expected-result reference))
  (let* ((dspec (mgl-pax::reference-to-dspec reference))
         (roundtripped (mgl-pax::dspec-to-reference
                        dspec (reference-object reference))))
    (when (is roundtripped
              :ctx ("REFERENCE = ~S~%DPSEC = ~S" reference dspec))
      (is (mgl-pax::reference= (capture roundtripped)
                               (capture expected-result))
          :ctx ("REFERENCE = ~S~%DPSEC = ~S" reference dspec)))))


;;; Keep this and `mgl-pax-edit-definitions/test-defs' in test.el in
;;; sync.
(defparameter *navigation-test-cases*
  '(;; @VARIABLELIKE-LOCATIVES
    (foo-a variable (defvar foo-a))
    (foo-r variable (defvar foo-r))
    (foo-w variable (defvar foo-w))
    (bar constant (defconstant bar))
    ;; @MACROLIKE-LOCATIVES
    (bar macro (defmacro bar))
    (my-smac symbol-macro (define-symbol-macro my-smac))
    (foo compiler-macro (define-compiler-macro foo))
    ;; @FUNCTIONLIKE-LOCATIVES
    (foo function (defun foo))
    (traced-foo function (defun traced-foo))
    (test-gf generic-function (defgeneric test-gf))
    (test-gf (method () (number)) (defmethod test-gf))
    (exportable-reference-p
     (method nil ((eql #.(find-package '#:mgl-pax-test)) T T T))
     (defmethod exportable-reference-p) (defmethod exportable-reference-p)
     #.(alexandria:featurep :ecl))
    (my-comb method-combination (define-method-combination my-comb))
    (foo-a (accessor foo) (defclass foo) (a :accessor foo-a))
    (foo-r (reader foo) (defclass foo) (r :reader foo-r))
    (foo-w (writer foo) (defclass foo) (w :writer foo-w))
    (baz-aaa structure-accessor (defstruct baz))
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
    (mgl-pax::@pax-manual section (defsection @pax-manual))
    (some-term glossary-term (define-glossary-term some-term))
    (my-loc locative (define-locative-type my-loc))))

(deftest test-navigation-to-source ()
  (dolist (test-case *navigation-test-cases*)
    (apply #'check-navigation test-case))
  (signals-not (error)
    (find-source (make-reference 'function 'locative)))
  (with-failure-expected ()
    (signals-not (error)
      (find-source (make-reference 'locative 'function)))))

(defun working-locative-p (locative)
  (let ((type (locative-type locative)))
    (cond ((and (alexandria:featurep :abcl)
                (member type '(variable constant method type restart
                               section locative glossary-term)))
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
           (alexandria:featurep '(:not (:or :abcl :ecl))))
          ((eq type 'type)
           (alexandria:featurep '(:not :ecl)))
          ((eq type 'package)
           (alexandria:featurep '(:not (:or :abcl :allegro :clisp :cmucl
                                        :ecl))))
          (t
           t))))

(defun check-navigation (symbol locative prefix &optional alternative-prefix
                                                  failure-expected-p)
  (let* ((ref (make-reference symbol locative))
         (located (resolve ref)))
    ;; Test FIND-SOURCE with a REFERENCE and a resolved object if
    ;; there is one.
    (dolist (target (if (and (typep located 'reference)
                             (mgl-pax::reference= located ref))
                        (list ref)
                        (list ref located)))
      (with-test ((format nil "navigate to ~S" target))
        (with-failure-expected ((or (not (working-locative-p locative))
                                    failure-expected-p))
          (let ((location (ignore-errors (find-source target))))
            (when (is (and location (not (eq :error (first location))))
                      :msg `("Find source location for (~S ~S)."
                             ,symbol ,locative))
              (multiple-value-bind (file position function-name)
                  (extract-source-location location)
                (is (or position function-name))
                (when position
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
                        :msg `("Find prefix ~S~@[ or ~S~] ~
                                    at source location~%~S~% ~
                                    for reference (~S ~S).~%~
                                    Form found was:~%~S."
                               ,prefix ,alternative-prefix
                               ,location ,symbol ,locative
                               ,form))))))))))))

(defun extract-source-location (location)
  (let ((file-entry (find :file (rest location) :key #'first))
        (position-entry (find :position (rest location) :key #'first))
        (offset-entry (find :offset (rest location) :key #'first))
        (function-name-entry (find :function-name (rest location)
                                   :key #'first)))
    (values (second file-entry)
            (cond (position-entry
                   (1- (second position-entry)))
                  (offset-entry
                   (1- (third offset-entry))))
            (second function-name-entry))))

(defun read-form-from-file-position (filename position)
  (with-open-file (stream filename
                          :direction :input
                          :external-format pax::*utf-8-external-format*)
    (file-position stream position)
    (read stream)))


(deftest test-misc ()
  ;; This is a primitive object, that (SWANK-BACKEND:FIND-DEFINITIONS
  ;; 'SB-C::CATCH-BLOCK) returns as a TYPE.
  #+sbcl
  (signals-not (locate-error)
    (map nil #'resolve
         (pax::definitions-of 'sb-c::catch-block))))
