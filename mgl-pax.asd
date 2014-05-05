;;;; -*- mode: Lisp -*-

;;; See MGL-PAX:@PAX-MANUAL for the user guide.
(asdf:defsystem #:mgl-pax
  :licence "MIT, see COPYING."
  :version "0.0.1"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://quotenil.com"
  :description "Documentation generator and browser."
  :depends-on (:3bmd :3bmd-ext-code-blocks :alexandria :babel :colorize
                     :ironclad :swank)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "3bmd")
                             (:file "utility")
                             (:file "pax-early")
                             (:file "pax")))))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system '#:mgl-pax))))
  (asdf:oos 'asdf:test-op '#:mgl-pax-test)
  (funcall (intern (symbol-name '#:test) (find-package '#:mgl-pax))))

(defmethod asdf:operation-done-p ((o asdf:test-op)
                                  (c (eql (asdf:find-system '#:mgl-pax))))
  (values nil))
