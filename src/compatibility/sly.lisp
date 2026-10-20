(in-package :mgl-pax)

;;; KLUDGE: DREF's default backend is :SWANK only if SWANK is loaded
;;; when DREF is loaded, which happens with Slime but not SLY. With
;;; Lisps other than SBCL, Swank is required. With SBCL it's not, but
;;; without it, the backend is NIL, and SB-INTROSPECT gets used
;;; directly, with reduced functionality. If we're using SLY mainly
;;; but have Swank available, then Swank gets used as an introspection
;;; library in effect. It would be worth setting things up so that
;;; Swank isn't required at all if SLY provides the main Lisp image,
;;; but this would require modifying the DREF library. Currently,
;;; SWANK should be loadable even if using SLY.
(when (and (null (dref:backend))
           (dref:backend-available-p :swank))
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

(defimpl buffer-package :sly ()
  slynk-api:*buffer-package*)

(defimpl buffer-readtable :sly ()
  slynk-api:*buffer-readtable*)
