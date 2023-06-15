(in-package :dref)

(defvar *dref-loaded* nil)

(defun/autoloaded ensure-dref-loaded ()
  (prog1 *dref-loaded*
    (setq *dref-loaded* t)))
