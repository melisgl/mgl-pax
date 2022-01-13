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
  :long-description "The set of dependencies of the MGL-PAX system is
  kept light, and its heavier dependencies are autoloaded via ASDF
  when the relavant functionality is accessed. See the
  MGL-PAX/NAVIGATE, MGL-PAX/DOCUMENT, MGL-PAX/TRANSCRIBE and
  MGL-PAX/FULL systems. To keep deployed code small, client systems
  should declare an ASDF dependency on this system, never on the
  others, which are intended for autoloading and interactive use."
  :depends-on (:alexandria :named-readtables :pythonic-string-reader :swank)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "pax-early")
                             (:file "pax")
                             (:file "extension-api")
                             (:file "document-early")
                             (:file "autoload")
                             (:file "locatives"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax/test"))))

(asdf:defsystem #:mgl-pax/navigate
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :description "Slime `M-.` support for MGL-PAX."
  :long-description "Autoloaded by Slime's `M-.` when `src/pax.el` is
  loaded. See MGL-PAX:@MGL-PAX-NAVIGATING-IN-EMACS."
  :depends-on (:mgl-pax)
  :components ((:module "src"
                :serial t
                :components ((:file "navigate"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax/test"))))

(asdf:defsystem #:mgl-pax/document
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :description "Documentation generation support for MGL-PAX."
  :long-description "Autoloaded by MGL-PAX:DOCUMENT. See
  MGL-PAX:@MGL-PAX-GENERATING-DOCUMENTATION."
  :depends-on (:mgl-pax/navigate :3bmd :3bmd-ext-code-blocks :colorize :md5)
  :components ((:module "src"
                :serial t
                :components ((:file "markdown")
                             (:file "document")
                             (:file "document-util"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax/test"))))

(asdf:defsystem #:mgl-pax/transcribe
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :description "Transcription support for MGL-PAX."
  :long-description "Autoloaded by MGL-PAX:TRANSCRIBE and by the Emacs
  integration (see MGL-PAX:@MGL-PAX-TRANSCRIPTS)."
  :depends-on (:mgl-pax)
  :components ((:module "src"
                :serial t
                :components ((:file "transcribe"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax/test"))))

(asdf:defsystem #:mgl-pax/full
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :description "MGL-PAX with all features preloaded."
  :depends-on (:mgl-pax/navigate :mgl-pax/document :mgl-pax/transcribe)
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax/test"))))

;;; FIXME: Reinstate once Try is in quicklisp.
#+nil
(asdf:defsystem mgl-pax/test
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :description "Test system for MGL-PAX."
  :depends-on (#:mgl-pax/full #:try)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test-transcribe")
                             (:file "test"))))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:mgl-pax-test '#:test)))
