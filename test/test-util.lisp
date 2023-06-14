(in-package :mgl-pax-test)

(deftest test-util ()
  (test-has-setf-p))

(deftest test-has-setf-p ()
  (is (null (pax::has-setf-p 'undefined)))
  (is (pax::has-setf-p 'documentation))
  (is (pax::has-setf-p 'has-setf-expander))
  (is (pax::has-setf-p 'has-setf-function))
  (is (pax::has-setf-p 'has-setf-generic-function))
  (is (null (pax::has-setf-p 'defun)))
  (with-failure-expected
      ((and (alexandria:featurep '(:not (:or :ccl :clisp :sbcl)))
            'failure))
    (is (pax::has-setf-p 'full-setf))))
