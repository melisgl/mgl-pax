;;;; -*- mode: Lisp -*-

;;; This is in a separate .asd file help OS-level packaging by making
;;; the dependency graph of .asd files (as opposed to just ASDF
;;; systems) acyclic. See https://github.com/melisgl/try/issues/5.
(asdf:defsystem "autoload-doc"
  :licence ""
  :author ""
  :mailto ""
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :description "Parts of [`autoload`][asdf:system] that depend on
  [`mgl-pax`][asdf:system]. This is split off [`autoload`][
  pax:dislocated] because [`mgl-pax-bootstrap`][asdf:system] depends
  on `autoload`. Note that [`mgl-pax/navigate`][asdf:system] and
  [`mgl-pax/document`][asdf:system] depend on this system, which
  renders most of this an implementation detail."
  :depends-on ("autoload" "mgl-pax")
  :serial t
  :components ((:file "doc")))
