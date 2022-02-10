(in-package :mgl-pax-test)

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
    (test-gf generic-function (defgeneric test-gf))
    (test-gf (method () (number)) (defmethod test-gf))
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

(deftest test-navigate ()
  (test-read-locative-from-string)
  (test-read-reference-from-string)
  (dolist (test-case *navigation-test-cases*)
    (apply #'check-navigation test-case)))

(deftest test-read-locative-from-string ()
  (let ((*package* (find-package :mgl-pax-test)))
    (unintern (read-from-string "non-interned"))
    (unintern (read-from-string "yyy"))
    (is (null (mgl-pax::read-locative-from-string "non-interned")))
    (is (null (find-symbol (string '#:non-interned))))
    (is (null (mgl-pax::read-locative-from-string "find")))
    (is (eq (mgl-pax::read-locative-from-string "function") 'function))
    (is (eq (mgl-pax::read-locative-from-string " function") 'function))
    (is (eq (mgl-pax::read-locative-from-string "function ") 'function))
    (is (null (mgl-pax::read-locative-from-string "function junk")))
    (let ((locative (mgl-pax::read-locative-from-string "(function yyy)")))
      (is (eq (first locative) 'function))
      (is (string= (symbol-name (second locative)) (string '#:yyy)))
      (is (eq (symbol-package (second locative)) *package*)))))

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
          (and (string= (symbol-name *) (string '#:yyy))
               (eq (symbol-package *) *package*))
          (eq * 'function)
          (eq * t)))
    (is (match-values (mgl-pax::read-reference-from-string " yyy  function ")
          (and (string= (symbol-name *) (string '#:yyy))
               (eq (symbol-package *) *package*))
          (eq * 'function)
          (eq * t)))
    (is (null (mgl-pax::read-reference-from-string "yyy function junk")))))

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

(defun check-navigation (symbol locative prefix &optional alternative-prefix)
  (let* ((ref (make-reference symbol locative))
         (located (resolve ref)))
    ;; Test FIND-SOURCE with a REFERENCE and a resolved object if
    ;; there is one.
    (dolist (target (if (and (typep located 'reference)
                             (mgl-pax::reference= located ref))
                        (list ref)
                        (list ref located)))
      (with-test ((format nil "navigate to ~S" target))
        (with-failure-expected ((not (working-locative-p locative)))
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
  (with-open-file (stream filename :direction :input)
    (file-position stream position)
    (read stream)))
