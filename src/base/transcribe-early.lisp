(in-package :mgl-pax)

;;; Silence SBCL compiler notes.
#+sbcl
(define-condition transcription-error (error condition-context-mixin) ())

#+sbcl
(define-condition transcription-consistency-error (transcription-error)
  ())

#+sbcl
(define-condition transcription-values-consistency-error
    (transcription-consistency-error)
  ())

#+sbcl
(define-condition transcription-output-consistency-error
    (transcription-consistency-error)
  ())
