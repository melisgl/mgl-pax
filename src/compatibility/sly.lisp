(in-package :mgl-pax)

;;; Replaces stub defined in interaction-mode.lisp
(defun/auto load-sly-glue ()
  nil)

(defimpl default-connection :sly ()
  (slynk-api:default-connection))

(defimpl call-with-connection :sly (connection thunk)
  (slynk-api:with-connection (connection)
    (funcall thunk)))

(defimpl eval-in-emacs :sly (form &optional nowait)
  (slynk:eval-in-emacs form nowait))

;; Various Lisp entry points involve reading SWANK::*BUFFER-PACKAGE*.
;; These methods mean that CALL-WITH-SWANK-COMPATIBILITY binds the SWANK variables from Slynk's
;; the BOUNDP check if defensive, and meant to handle a possible call site where
;; slynk-api:*buffer-package* is unbound: CALL-NEXT-METHOD calls the default method defined
;; in src/compatibility/interaction-mode.lisp, i.e. BUFFER-PACKAGE.
(defimpl buffer-package :sly ()
  (if (boundp 'slynk-api:*buffer-package*)
      slynk-api:*buffer-package*
      (call-next-method)))

;; CALL-NEXT-METHOD calls the default method which returns
;; `(SWANK::GUESS-BUFFER-READTABLE SWANK::*BUFFER-PACKAGE*)'
(defimpl buffer-readtable :sly ()
  (if (boundp 'slynk-api:*buffer-readtable*)
      slynk-api:*buffer-readtable*
      (call-next-method)))
