;;;; -*- mode: Lisp -*-

;;; See MGL-PAX:@MGL-PAX-MANUAL for the user guide.
(asdf:defsystem #:mgl-pax-minimal
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://melisgl.github.io/mgl-pax"
  :bug-tracker "https://github.com/melisgl/mgl-pax/issues"
  :source-control (:git "https://github.com/melisgl/mgl-pax.git")
  :description "This system provides DEFSECTION macro of MGL-PAX documentation generator."
  ;; We try to keep zero dependencies
  ;; for mgl-pax-minimal,
  ;; because it will be used by other libraries
  ;; at runtime:
  :depends-on ()
  :components ((:module "src"
                :serial t
                :components ((:file "minimal-package")
                             (:file "pax-early")))))
