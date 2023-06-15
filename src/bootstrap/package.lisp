;;; This is basically MGL-PAX:DEFINE-PACKAGE, which is not defined yet.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally
      (declare #+sbcl
               (sb-ext:muffle-conditions sb-kernel::package-at-variance))
    (handler-bind
        (#+sbcl (sb-kernel::package-at-variance #'muffle-warning))
      (cl:defpackage :mgl-pax
        (:documentation "See MGL-PAX::@PAX-MANUAL.")
        (:use #:common-lisp)
        ;; FIXME: Swap this with the package name.
        (:nicknames #:pax)
        ;; These are the exports when only the MGL-PAX/BOOTSTRAP ASDF
        ;; system is loaded.
        (:export #:define-package
         #:defsection #:section
         #:define-glossary-term #:glossary-term
         #:make-github-source-uri-fn
         #:make-git-source-uri-fn
         #:register-doc-in-pax-world)))))
