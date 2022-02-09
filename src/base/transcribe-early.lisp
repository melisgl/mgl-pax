(in-package :mgl-pax)

(defvar *transcribe-check-consistency*)
(export '*transcribe-check-consistency*)
(defvar *transcribe-syntaxes*)
(export '*transcribe-syntaxes*)

(autoload transcribe '#:mgl-pax/transcribe)
(autoload transcribe-for-emacs '#:mgl-pax/transcribe :export nil)
(autoload squeeze-whitespace '#:mgl-pax/transcribe)
(autoload delete-trailing-whitespace '#:mgl-pax/transcribe)
(autoload delete-comments '#:mgl-pax/transcribe)
