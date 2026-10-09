(in-package :mgl-pax-test)

;;;; Tests for the Lisp side of the Sly support. They simulate what
;;;; Slynk does when Emacs evaluates a form (see SLYNK::EVAL-FOR-EMACS):
;;;; SLYNK-API:*BUFFER-PACKAGE* and SLYNK-API:*BUFFER-READTABLE* are
;;;; bound, but *PACKAGE* is not. No Emacs connection is needed.
;;;;
;;;; These need Slynk to be loaded, hence the separate system.

(deftest test-sly ()
  (test-sly-buffer-package)
  (test-sly-swank-compatibility)
  (test-sly-locate-definitions)
  (test-sly-names-or-locatives)
  (test-sly-default-connection)
  (test-sly-buffer-package-outside-a-request)
  (test-sly-redocument-for-emacs))

(defmacro with-simulated-slynk-request ((package &key (readtable '*readtable*))
                                        &body body)
  `(let ((slynk-api:*buffer-package* (find-package ,package))
         (slynk-api:*buffer-readtable* ,readtable)
         ;; Slynk does not bind this one to the buffer's package.
         (*package* (find-package :common-lisp-user)))
     (pax::with-lisp-interaction-mode (:sly)
       ,@body)))

(defun sly-test-function (x)
  "Used by TEST-SLY. It is only visible from MGL-PAX-TEST."
  x)

(deftest test-sly-buffer-package ()
  (with-simulated-slynk-request (:mgl-pax-test)
    (with-test ("the buffer's package is Slynk's, not *PACKAGE*")
      (is (eq (pax::buffer-package) (find-package :mgl-pax-test)))
      (is (not (eq (pax::buffer-package) *package*)))))
  (let ((readtable (copy-readtable)))
    (with-simulated-slynk-request (:cl-user :readtable readtable)
      (with-test ("the buffer's readtable is Slynk's")
        (is (eq (pax::buffer-readtable) readtable)))))
  (with-test ("without Sly, it is *PACKAGE* (that is what tests rely on)")
    (let ((*package* (find-package :mgl-pax-test))
          (pax::*current-interaction-mode* nil))
      (is (eq (pax::buffer-package) (find-package :mgl-pax-test))))))

(deftest test-sly-swank-compatibility ()
  (let ((readtable (copy-readtable)))
    (with-test ("Swank's variables are bound from Slynk's")
      (with-simulated-slynk-request (:mgl-pax-test :readtable readtable)
        (pax::with-swank-compatibility ()
          (is (eq swank::*buffer-package* (find-package :mgl-pax-test)))
          (is (eq swank::*buffer-readtable* readtable)))))
    (with-test ("and are not left bound")
      (let ((boundp-before (boundp 'swank::*buffer-package*)))
        (with-simulated-slynk-request (:mgl-pax-test)
          (pax::with-swank-compatibility ()))
        (is (eq (boundp 'swank::*buffer-package*) boundp-before)))))
  (with-test ("values bound by an enclosing Swank server are respected")
    (let ((swank::*buffer-package* (find-package :cl-user)))
      (with-simulated-slynk-request (:mgl-pax-test)
        (pax::with-swank-compatibility ()
          (is (eq swank::*buffer-package* (find-package :cl-user))))))))

(deftest test-sly-locate-definitions ()
  (with-test ("a name visible from the buffer's package is found")
    (with-simulated-slynk-request (:mgl-pax-test)
      (let ((result (pax::locate-definitions-for-emacs
                     '(("sly-test-function" ())))))
        (is (= (length result) 1))
        (is (search "SLY-TEST-FUNCTION" (first (first result))))
        (is (eq (first (second (first result))) :location)))))
  (with-test ("with a locative")
    (with-simulated-slynk-request (:mgl-pax-test)
      (is (= 1 (length (pax::locate-definitions-for-emacs
                        '(("sly-test-function" ("function")))))))))
  (with-test ("the same name is not visible from another buffer package")
    (with-simulated-slynk-request (:cl-user)
      (is (null (pax::locate-definitions-for-emacs
                 '(("sly-test-function" ())))))))
  (with-test ("a package-qualified name works from any buffer package")
    (with-simulated-slynk-request (:cl-user)
      (is (= 1 (length (pax::locate-definitions-for-emacs
                        '(("mgl-pax-test::sly-test-function" ()))))))))
  (with-test ("an unknown name")
    (with-simulated-slynk-request (:mgl-pax-test)
      (is (null (pax::locate-definitions-for-emacs
                 '(("no-such-thing-at-all" ()))))))))

(deftest test-sly-names-or-locatives ()
  (with-simulated-slynk-request (:mgl-pax-test)
    (is (equal (pax::names-or-locatives-for-emacs "sly-test-function" "fun")
               '(:names ("function")))))
  (with-simulated-slynk-request (:cl-user)
    (is (equal (pax::names-or-locatives-for-emacs "sly-test-function" "fun")
               '(:names ())))))

(deftest test-sly-default-connection ()
  ;; No Emacs is connected to the test image.
  (pax::with-lisp-interaction-mode (:sly)
    (is (null (pax::default-connection)))))

;;; Slynk binds the buffer's package for every request that comes from
;;; Emacs, but PAX can also be entered from other threads (e.g. the
;;; web server's) with the mode bound but no request in progress.
(deftest test-sly-buffer-package-outside-a-request ()
  (pax::with-lisp-interaction-mode (:sly)
    (let ((*package* (find-package :mgl-pax-test)))
      (is (eq (pax::buffer-package) (find-package :mgl-pax-test))))))

;;; REDOCUMENT-FOR-EMACS is used to read SWANK::*BUFFER-PACKAGE* without
;;; WITH-SWANK-COMPATIBILITY, since this is unbound with Slynk.
(deftest test-sly-redocument-for-emacs ()
  (with-simulated-slynk-request (:mgl-pax-test)
    (let ((result (pax::redocument-for-emacs "file:///no/such/doc.html"
                                             "/no/such/dir/")))
      (is (not (search "BUFFER-PACKAGE" (princ-to-string result)))))))
