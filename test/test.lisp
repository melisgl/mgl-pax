(in-package :mgl-pax-test)

(deftest test-all ()
  (let ((*check-locate* t))
    (flet ((all ()
             (test-util)
             (test-navigate)
             (test-document)
             (test-transcribe)))
      (when (backend-available-p :swank)
        (with-test ("with swank")
          (with-backend (:swank)
            (is (eq (backend) :swank))
            (all))))
      (when (backend-available-p nil)
        (with-test ("without swank")
          (with-backend (nil)
            (is (null (backend)))
            (all))))
      #-sbcl
      (all))))

(defun test (&key (debug nil) (print 'leaf) (describe *describe*))
  (handler-bind ((warning (lambda (c)
                            (when (expected-style-warning-p c)
                              (muffle-warning)))))
    (with-compilation-unit (:override t)
      (pax::with-sections-cache ()
        ;; Bind *PACKAGE* so that names of tests printed have package
        ;; names, and M-. works on them in Slime.
        (let ((*package* (find-package :common-lisp))
              (*print-duration* nil)
              (*print-compactly* t)
              (*print-parent* nil)
              (*defer-describe* t))
          (warn-on-tests-not-run ((find-package :mgl-pax-test))
            (print (try 'test-all :debug debug :print print
                                  :describe describe))))))))

(defun expected-style-warning-p (c)
  (search "junk" (princ-to-string c) :test #'equalp))

(defun stress-test ()
  (handler-bind ((error #'continue))
    (time (document (pax::pax-apropos* "" nil "" nil nil)
                    :stream (make-broadcast-stream) :format :markdown))))

#+nil
(test)

#+nil
(test-all)

#+nil
(stress-test)
