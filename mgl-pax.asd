;;;; -*- mode: Lisp -*-

;;; See MGL-PAX:@MGL-PAX-MANUAL for the user guide.
(asdf:defsystem #:mgl-pax
  :licence "MIT, see COPYING."
  :version "0.0.3"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://melisgl.github.io/mgl-pax"
  :bug-tracker "https://github.com/melisgl/mgl-pax/issues"
  :source-control (:git "https://github.com/melisgl/mgl-pax.git")
  :description "Exploratory programming tool and documentation
  generator."
  :depends-on (:3bmd :3bmd-ext-code-blocks :alexandria :babel :cl-fad :colorize
               :ironclad :named-readtables :pythonic-string-reader :swank
               :mgl-pax-minimal)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "utility")
                             (:file "pax")
                             (:file "doc")
                             (:file "transcribe"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax/test"))))

(asdf:defsystem mgl-pax/test
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
                             (:file "test"))))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:mgl-pax-test '#:test)))
