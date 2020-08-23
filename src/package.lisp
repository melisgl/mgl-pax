;;; This is is basically MGL-PAX:DEFINE-PACKAGE but we don't have it
;;; defined yet.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally
      (declare #+sbcl
               (sb-ext:muffle-conditions sb-kernel::package-at-variance))
    (handler-bind
        (#+sbcl (sb-kernel::package-at-variance #'muffle-warning))
      (cl:defpackage :mgl-pax
        (:documentation "See MGL-PAX:@MGL-PAX-MANUAL.")
        (:use #:common-lisp #:named-readtables #:pythonic-string-reader)
        (:nicknames #:pax)))))
