;;; This is is basically MGL-PAX:DEFINE-PACKAGE but we don't have it
;;; defined yet.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally
      (declare #+sbcl
               (sb-ext:muffle-conditions sb-kernel::package-at-variance))
    (handler-bind
        (#+sbcl (sb-kernel::package-at-variance #'muffle-warning))
      ;; We need this separate package to make it easier
      ;; to use MGL-PAX with package inferred ASDF systems
      ;; where a system named "foo" should have a corresponding
      ;; package "foo".
      (cl:defpackage :mgl-pax-minimal
        (:documentation "See MGL-PAX:@MGL-PAX-MANUAL.")
        (:use #:common-lisp)
        (:export #:define-package
                 #:defsection
                 #:exportable-locative-type-p
                 #:locative-args
                 #:locative-type
                 #:make-reference
                 #:reference
                 #:reference-object
                 #:reference-locative
                 #:section
                 #:section-name
                 #:section-package
                 #:section-readtable
                 #:section-title
                 #:section-link-title-to
                 #:section-entries
                 ;; Locatives for these symbols will be defined
                 ;; in the full mgl-pax package:
                 #:reader
                 #:writer
                 #:accessor
                 #:macro)))))
