(in-package :mgl-pax)

;;;; DREF's default backend is :SWANK only if SWANK is loaded when
;;;; DREF is loaded, which happens with Slime but not Sly. With lisps
;;;; other than SBCL, Swank is required. With SBCL it's not, but
;;;; without it, the backend is NIL, and SB-INTROSPECT gets used
;;;; directly, with reduced functionality. If we're using Sly mainly
;;;; but have Swank available, then Swank gets used as an
;;;; introspection library in effect. It would be worth setting things
;;;; up so that Swank isn't required at all if Sly provides the main
;;;; Lisp image, but this would require modifying the DREF library.
;;;; Currently, SWANK should be loadable even if using Sly.

(when (and (null (dref:backend)) (dref:backend-available-p :swank))
  (setf (dref:backend) :swank))

;;; Replaces stub defined in interaction-mode.lisp.
(defun/auto ensure-sly-glue-loaded ()
  nil)

(defimpl default-connection :sly ()
  (slynk-api:default-connection))

(defimpl call-with-connection :sly (connection thunk)
  (slynk-api:with-connection (connection)
    (funcall thunk)))

(defimpl eval-in-emacs :sly (form &optional nowait)
  (slynk:eval-in-emacs form nowait))

;;; Various Lisp entry points involve reading SWANK::*BUFFER-PACKAGE*.
;;; These methods mean that CALL-WITH-SWANK-COMPATIBILITY binds the
;;; SWANK variables from Slynk's the BOUNDP check if defensive, and
;;; meant to handle a possible call site where
;;; slynk-api:*buffer-package* is unbound: CALL-NEXT-METHOD calls the
;;; default method defined in src/compatibility/interaction-mode.lisp,
;;; i.e. BUFFER-PACKAGE.
(defimpl buffer-package :sly ()
  (if (boundp 'slynk-api:*buffer-package*)
      slynk-api:*buffer-package*
      (call-next-method)))

;;; CALL-NEXT-METHOD calls the default method which returns
;;; `(SWANK::GUESS-BUFFER-READTABLE SWANK::*BUFFER-PACKAGE*)'.
(defimpl buffer-readtable :sly ()
  (if (boundp 'slynk-api:*buffer-readtable*)
      slynk-api:*buffer-readtable*
      (call-next-method)))
