;;;; -*- mode: Lisp -*-

;;; See MGL-PAX:@MGL-PAX-MANUAL for the user guide.
(asdf:defsystem #:mgl-pax
  :licence "MIT, see COPYING."
  :version "0.0.2"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://quotenil.com"
  :description "Exploratory programming tool and documentation
  generator."
  :depends-on (:3bmd :3bmd-ext-code-blocks :alexandria :babel :cl-fad :colorize
                     :ironclad :named-readtables :pythonic-string-reader :swank)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "utility")
                             (:file "pax-early")
                             (:file "pax")
                             (:file "transcribe")))))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system '#:mgl-pax))))
  (asdf:oos 'asdf:load-op '#:mgl-pax-test)
  (funcall (intern (symbol-name '#:test) (find-package '#:mgl-pax-test))))
