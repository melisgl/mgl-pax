;;;; -*- lexical-binding: t -*-

(eval-and-compile
  (require 'mgl-pax)
  (require 'slime))


(defvar mgl-pax-backend :slime) ; default if not already set

;;;; SLIME implementation of the Lisp interaction mode protocol

(mgl-pax-defimpl mgl-pax-eval :slime (sexp &optional package)
  (slime-eval (mgl-pax-propagating-lisp-interaction-mode :slime sexp) package))

(mgl-pax-defimpl mgl-pax-remote-execute-sexp :slime (sexp package &key ok abort)
  (slime-rex (ok abort)
      ((mgl-pax-propagating-lisp-interaction-mode :slime sexp) package)
    ((:ok result) (when ok (funcall ok result)))
    ((:abort condition) (when abort (funcall abort condition)))))

(mgl-pax-defimpl mgl-pax-check-connected :slime ()
  (slime-check-connected))

(mgl-pax-defimpl mgl-pax-sexp-at-point :slime (&optional interactive)
  (declare (ignore interactive))
  (slime-sexp-at-point))

(mgl-pax-defimpl mgl-pax-bounds-of-sexp-at-point :slime ()
  (slime-bounds-of-sexp-at-point))

(mgl-pax-defimpl mgl-pax-bounds-of-symbol-at-point :slime ()
  (slime-bounds-of-symbol-at-point))

(mgl-pax-defimpl mgl-pax-symbol-start-pos :slime ()
  (slime-symbol-start-pos))

(mgl-pax-defimpl mgl-pax-last-expression :slime ()
  (slime-last-expression))

(mgl-pax-defimpl mgl-pax-forward-sexp :slime ()
  (slime-forward-sexp))

(mgl-pax-defimpl mgl-pax-visit-lisp-location :slime (dspec-and-location-list)
  (slime-edit-definition-cont
   (slime-postprocess-xrefs dspec-and-location-list)
   "dummy name" nil))

(mgl-pax-defimpl mgl-pax-read-lisp-from-minibuffer :slime (prompt &optional initial-value history)
  (slime-read-from-minibuffer prompt initial-value history))

(mgl-pax-defimpl mgl-pax-current-package :slime ()
  (slime-current-package))

(mgl-pax-defimpl mgl-pax-set-buffer-package :slime (package)
  (setq slime-buffer-package package))

(mgl-pax-defimpl mgl-pax-read-package-name :slime (prompt &optional initial-value)
  (slime-read-package-name prompt initial-value))

(mgl-pax-defimpl mgl-pax-list-all-package-names :slime ()
  (slime-eval `(swank:list-all-package-names t)))

(mgl-pax-defimpl mgl-pax-set-up-backend-doc-keybindings :slime ()
  (local-set-key (kbd "M-.") 'slime-edit-definition)
  (local-set-key (kbd "M-,") 'slime-pop-find-definition-stack)
  (local-set-key (kbd "C-c C-d") 'slime-doc-map))


;;;; Cleanup

;;; This is called automatically by (unload-feature 'mgl-pax-slime).
(defun mgl-pax-slime-unload-function ()
  (mgl-pax-unhijack-slime-doc-keys)
  (advice-remove 'slime-edit-definition #'slime-edit-definition@mgl-pax)
  (setq mgl-pax-loaded-backends (delq 'mgl-pax-slime mgl-pax-loaded-backends)))


;;;; MGL-PAX::@SLIME-SETUP (also see MGL-PAX::@EMACS-SETUP)

(defcustom mgl-pax-hijack-slime-doc-keys t
  "If true, then bind mgl-pax functions in `slime-mode-map' by
  `mgl-pax-hijack-slime-doc-keys' upon loading `mgl-pax-slime'. See
  MGL-PAX::@EMACS-KEYS for details."
  :type 'boolean
  :group 'mgl-pax)

(defvar mgl-pax-slime-doc-map-overrides
  '((?a slime-apropos mgl-pax-apropos)
    (?z slime-apropos-all mgl-pax-apropos-all)
    (?p slime-apropos-package mgl-pax-apropos-package)
    (?d slime-describe-symbol mgl-pax-document)
    (?f slime-describe-function mgl-pax-document)
    (?c nil mgl-pax-current-definition-toggle-view)
    (?u nil mgl-pax-edit-parent-section)))

(defun mgl-pax-hijack-slime-doc-keys ()
  "See MGL-PAX::@EMACS-SETUP."
  (interactive)
  (mgl-pax-override-keys slime-doc-map mgl-pax-slime-doc-map-overrides))

(defun mgl-pax-unhijack-slime-doc-keys ()
  "See MGL-PAX::@EMACS-SETUP."
  (interactive)
  (mgl-pax-override-keys slime-doc-map mgl-pax-slime-doc-map-overrides t))

(when mgl-pax-hijack-slime-doc-keys
  (mgl-pax-hijack-slime-doc-keys))


;;;; Integration with `M-.'

(define-advice slime-edit-definition (:around (oldfun &optional name where) mgl-pax)
  (interactive)
  (mgl-pax-edit-definition-advice (called-interactively-p 'any)
                                  oldfun name where))

(add-hook 'slime-edit-definition-hooks 'mgl-pax-run-edit-lisp-definition-hooks)


;;;; Locative completion

(add-to-list 'slime-completion-at-point-functions
             'mgl-pax-completions-at-point)


(add-to-list 'mgl-pax-loaded-backends 'mgl-pax-slime)
(provide 'mgl-pax-slime)
