;;;; -*- mode: Lisp -*-

;;; See MGL-PAX:@MGL-PAX-MANUAL for the user guide.
(asdf:defsystem #:mgl-pax
  :licence "MIT, see COPYING."
  :version "0.0.4"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://melisgl.github.io/mgl-pax"
  :bug-tracker "https://github.com/melisgl/mgl-pax/issues"
  :source-control (:git "https://github.com/melisgl/mgl-pax.git")
  :description "Exploratory programming tool and documentation
  generator."
  ;; These are *all* the dependencies as currently only
  ;; PYTHONIC-STRING-READER has a dependency (on NAMED-READTABLES).
  ;; All the heavy-weight dependencies are loaded on demand or eagerly
  ;; by MGL-PAX/FULL.
  :depends-on (:alexandria :named-readtables :pythonic-string-reader :swank)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "pax-early")
                             (:file "pax")
                             (:file "extension-api")
                             (:file "navigate")
                             (:file "transcribe")
                             (:file "document")
                             (:file "document-util")
                             (:file "locatives"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax/test"))))

(asdf:defsystem #:mgl-pax/full
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :description "MGL-PAX with all dependencies preloaded."
  :long-description "To ease deployment, the set of dependencies of
  the MGL-PAX system is kept light and its heavier dependencies are
  loaded on demand. If this is undesirable, then the MGL-PAX/FULL
  system can be loaded."
  :depends-on (:mgl-pax :3bmd :3bmd-ext-code-blocks
                        :babel :cl-fad :colorize :ironclad)
  :components ((:module "src"
                :serial t
                :components ((:file "describe"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax/test"))))

(asdf:defsystem mgl-pax/test
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :description "Test system for MGL-PAX."
  :depends-on (#:mgl-pax)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test-transcribe")
                             (:file "test"))))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:mgl-pax-test '#:test)))
