(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(autoload locate-definitions-for-emacs '#:mgl-pax/navigate :export nil)
(autoload find-parent-section-for-emacs '#:mgl-pax/navigate :export nil)

(declaim (ftype function find-method*))
