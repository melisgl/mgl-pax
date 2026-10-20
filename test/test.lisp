(in-package :mgl-pax-test)

(deftest test-install-pax-elisp ()
  (let ((dir (ensure-directories-exist
              (merge-pathnames (format nil "mgl-pax-elisp-test-~A/"
                                       (get-universal-time))
                               (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (pax:install-pax-elisp dir)
           (dolist (file '("mgl-pax.el" "mgl-pax-slime.el" "mgl-pax-sly.el"))
             (is (probe-file (merge-pathnames file dir))
                 :msg ("~S is installed." file)))
           (with-test ("the version is filled in")
             (let ((text (alexandria:read-file-into-string
                          (merge-pathnames "mgl-pax.el" dir))))
               ;; A comment also quotes the line, so look for it at
               ;; the beginning of a line.
               (is (not
                    (search
                     (format nil
                             "~%(setq mgl-pax-version (mgl-pax-read-version))")
                     text)))
               (is (search "(setq mgl-pax-version '" text)))))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(deftest test-all ()
  (let ((*check-locate* t)
        (*document-index-sections* nil)
        (*document-downcase-uppercase-code* nil)
        (pax::*test-fast-parse* t))
    (pax::with-sections-cache ()
      (test-interaction-mode)
      (test-install-pax-elisp)
      (flet ((all ()
               (test-util)
               (test-hyperspec)
               (test-navigate)
               (test-markdown)
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
        (all)))))

(defun test (&key (debug nil) (print 'leaf) (describe *describe*))
  (handler-bind ((warning (lambda (c)
                            (when (expected-style-warning-p c)
                              (muffle-warning)))))
    (with-compilation-unit (:override t)
      ;; Bind *PACKAGE* so that names of tests printed have package
      ;; names, and M-. works on them in Slime.
      (let ((*package* (find-package :common-lisp))
            (*print-compactly* t)
            (*print-parent* nil)
            (*defer-describe* t))
        (warn-on-tests-not-run ((find-package :mgl-pax-test))
          (print (try 'test-all :debug debug :print print
                      :describe describe)))))))

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
