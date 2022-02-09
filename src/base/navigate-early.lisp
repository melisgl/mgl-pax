(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @extending-find-source (:title "Extending FIND-SOURCE")
  "The following utilities are for writing new FIND-SOURCE and
  LOCATE-AND-FIND-SOURCE methods."
  (find-definition function)
  (find-definition* function))

(autoload find-definition '#:mgl-pax/navigate)
(autoload find-definition* '#:mgl-pax/navigate)
(autoload locate-definitions-for-emacs '#:mgl-pax/navigate :export nil)

(declaim (ftype function find-method*))
(declaim (ftype function macro-arg-names))
