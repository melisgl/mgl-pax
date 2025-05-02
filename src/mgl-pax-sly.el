;;;; -*- lexical-binding: t; -*-

(eval-and-compile
  (require 'mgl-pax)
  (require 'sly))



(defvar mgl-pax-backend :sly)

;;;; Sly implementation of the Lisp interaction mode protocol

(mgl-pax-defimpl mgl-pax-eval :sly (sexp &optional package)
  (sly-eval (mgl-pax-propagating-lisp-interaction-mode :sly sexp) package))

(mgl-pax-defimpl mgl-pax-remote-execute-sexp :sly (sexp package &key ok abort)
  (sly-rex (ok abort)
      ((mgl-pax-propagating-lisp-interaction-mode :sly sexp) package)
    ((:ok result) (when ok (funcall ok result)))
    ((:abort condition) (when abort (funcall abort condition)))))

(mgl-pax-defimpl mgl-pax-check-connected :sly ()
  (sly-check-connected))

(mgl-pax-defimpl mgl-pax-bounds-of-sexp-at-point :sly ()
  ;; Differently from `slime-bounds-of-sexp-at-point', this will
  ;; perform a `backward-sexp' and return the bounds of that if there
  ;; is no sexp immediately at point, which would make our patch to
  ;; `sly-edit-definition' not prompt the user even when the point is
  ;; outside a sexp.
  (let ((bounds (sly-bounds-of-sexp-at-point)))
    (when (and bounds (>= (cdr bounds) (point)))
      bounds)))

(mgl-pax-defimpl mgl-pax-sexp-at-point :sly (&optional interactive)
  (when-let (bounds (mgl-pax-bounds-of-sexp-at-point))
    (when interactive ; mimic `sly-sexp-at-point'
      (sly-flash-region (car bounds) (cdr bounds)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(mgl-pax-defimpl mgl-pax-symbol-start-pos :sly ()
  (sly-symbol-start-pos))

(mgl-pax-defimpl mgl-pax-last-expression :sly ()
  (sly-last-expression))

(mgl-pax-defimpl mgl-pax-forward-sexp :sly ()
  (sly-forward-sexp))

(defun mgl-pax-sly-edit-xrefs (xrefs &optional method)
  ;; Copied verbatim from the body of `sly-edit-definition', as Sly
  ;; offers no way of directly jumping to a xref
  (cl-destructuring-bind (1loc file-alist) (sly-analyze-xrefs xrefs)
    (cond (1loc
           (sly-push-definition-stack)
           (sly--pop-to-source-location
            (sly-xref.location (car xrefs)) method))
          ((null (cdr xrefs))      ; ((:error "..."))
           (error "%s" xrefs))
          (t
           (sly-push-definition-stack)
           (sly-xref--show-results file-alist 'definition name
                                   (sly-current-package)
                                   (cons (selected-window)
                                         method))))))

(mgl-pax-defimpl mgl-pax-visit-lisp-location :sly (dspec-and-location-list)
  (mgl-pax-sly-edit-xrefs dspec-and-location-list))

(mgl-pax-defimpl mgl-pax-read-lisp-from-minibuffer :sly (prompt &optional initial-value history)
  (sly-read-from-minibuffer prompt initial-value history))

(mgl-pax-defimpl mgl-pax-current-package :sly ()
  (sly-current-package))

(mgl-pax-defimpl mgl-pax-set-buffer-package :sly (package)
  (setq sly-buffer-package package))

(mgl-pax-defimpl mgl-pax-read-package-name :sly (prompt &optional initial-value)
  (sly-read-package-name prompt initial-value))

(mgl-pax-defimpl mgl-pax-list-all-package-names :sly ()
  (sly-eval `(slynk:list-all-package-names t)))

(mgl-pax-defimpl mgl-pax-set-up-backend-doc-keybindings :sly ()
  (local-set-key (kbd "M-.") 'sly-edit-definition)
  (local-set-key (kbd "M-,") 'sly-pop-find-definition-stack)
  (local-set-key (kbd "C-c C-d") 'sly-doc-map))


;;;; Cleanup

;;; This is called automatically by (unload-feature 'mgl-pax-sly).
(defun mgl-pax-sly-unload-function ()
  (mgl-pax-unhijack-sly-doc-keys)
  (advice-remove 'sly-edit-definition #'sly-edit-definition@mgl-pax)
  (setq mgl-pax-loaded-backends (delq 'mgl-pax-sly mgl-pax-loaded-backends)))


;;;; MGL-PAX::@SLY-SETUP (also see MGL-PAX::@EMACS-SETUP)

(defcustom mgl-pax-hijack-sly-doc-keys t
  "If true, then bind mgl-pax functions in `sly-mode-map' by
  `mgl-pax-hijack-sly-doc-keys' upon loading `mgl-pax-sly'. See
  MGL-PAX::@EMACS-KEYS for details."
  :type 'boolean
  :group 'mgl-pax)

(defvar mgl-pax-sly-doc-map-overrides
  '((?a sly-apropos mgl-pax-apropos)
    (?z sly-apropos-all mgl-pax-apropos-all)
    (?p sly-apropos-package mgl-pax-apropos-package)
    (?d sly-describe-symbol mgl-pax-document)
    (?f sly-describe-function mgl-pax-document)
    (?c nil mgl-pax-current-definition-toggle-view)
    (?u nil mgl-pax-edit-parent-section)))

(defun mgl-pax-hijack-sly-doc-keys ()
  "See MGL-PAX::@EMACS-SETUP."
  (interactive)
  (mgl-pax-override-keys sly-doc-map mgl-pax-sly-doc-map-overrides))

(defun mgl-pax-unhijack-sly-doc-keys ()
  (interactive)
  (mgl-pax-override-keys sly-doc-map mgl-pax-sly-doc-map-overrides t))

(when mgl-pax-hijack-sly-doc-keys
  (mgl-pax-hijack-sly-doc-keys))


;;;; Integration with `M-.'

(define-advice sly-edit-definition (:around (oldfun &optional name where) mgl-pax)
  (interactive)
  (mgl-pax-edit-definition-advice
   (called-interactively-p 'any)
   (lambda (name where)
     (or (mgl-pax-run-edit-lisp-definition-hooks name where)
         (funcall oldfun name where)))
   name where))



;;;; Locative completion

(defun mgl-pax-sly-set-up-completion-at-point-functions ()
  (cl-pushnew 'mgl-pax-completions-at-point completion-at-point-functions))

(add-hook 'sly-mode-hook 'mgl-pax-sly-set-up-completion-at-point-functions)



(add-to-list 'mgl-pax-loaded-backends 'mgl-pax-sly)
(provide 'mgl-pax-sly)

;; TODO web documentation requires `sly-enable-evaluate-in-emacs' to
;; be true for edit in Emacs to work; this should probably be left to
;; the user to decide.

