;;;; -*- mode: Lisp -*-

(asdf:defsystem mgl-pax-test
  :depends-on (#:mgl-pax)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test-transcribe")
                             (:file "test")))))
