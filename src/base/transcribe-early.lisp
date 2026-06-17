(in-package :mgl-pax)

;;; Silence SBCL compiler notes about undefined types when these are
;;; used in a condition handler.
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-class 'transcription-error nil)
    (define-condition transcription-error (error condition-context-mixin) ())))
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-class 'transcription-consistency-error nil)
    (define-condition transcription-consistency-error (transcription-error)
      ())))
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-class 'transcription-values-consistency-error nil)
    (define-condition transcription-values-consistency-error
        (transcription-consistency-error)
      ())))
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-class 'transcription-output-consistency-error nil)
    (define-condition transcription-output-consistency-error
        (transcription-consistency-error)
      ())))
