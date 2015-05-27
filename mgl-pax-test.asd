;;;; -*- mode: Lisp -*-

(asdf:defsystem mgl-pax-test
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://quotenil.com"
  :description "Test system for MGL-PAX."
  :depends-on (#:mgl-pax)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test-transcribe")
                             (:file "test")))))
