(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(autoload find-definition '#:mgl-pax/navigate)
(autoload find-definition* '#:mgl-pax/navigate)
(autoload locate-definitions-for-emacs '#:mgl-pax/navigate :export nil)

(declaim (ftype function find-method*))
(declaim (ftype function macro-arg-names))
