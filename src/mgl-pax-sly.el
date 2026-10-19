;;;; mgl-pax-sly.el --- Sly client -*- lexical-binding: t -*-

(eval-and-compile
  (require 'mgl-pax))

;;; Set this before requiring `sly' since that triggers loading the
;;; default backend.
(defvar mgl-pax-backend :sly) ; default if not already set

(eval-and-compile
  (require 'sly))


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

(mgl-pax-defimpl mgl-pax-flash-region :sly (start end &optional timeout)
  "`sly-flash-region' takes a :PATTERN of durations. :TIMEOUT cannot be
combined with its default pattern."
  (if timeout
      (sly-flash-region start end :pattern (list timeout))
    (sly-flash-region start end)))

(mgl-pax-defimpl mgl-pax-call-with-buffer-package :sly (package function)
  (let ((sly-buffer-package package))
    (funcall function)))

(mgl-pax-defimpl mgl-pax-symbol-start-pos :sly ()
  (sly-symbol-start-pos))

(mgl-pax-defimpl mgl-pax-last-expression :sly ()
  (sly-last-expression))

(mgl-pax-defimpl mgl-pax-forward-sexp :sly ()
  (sly-forward-sexp))

(defun mgl-pax-sly-edit-xrefs (xrefs &optional method)
  "Takes XREFS from PAX and displays them.
If 'window or 'frame is passed as METHOD, displays to another window
or a new frame."
  (cl-destructuring-bind (1loc file-alist) (sly-analyze-xrefs xrefs)
    (cond (1loc
           (sly-push-definition-stack)
           (sly--pop-to-source-location
            (sly-xref.location (car xrefs)) method))
          ((null (cdr xrefs))
           (let ((xref (car xrefs)))
             (if (eq (car-safe (sly-xref.location xref)) :error)
                 (error "No source location for %s: %s"
                        (sly-xref.dspec xref)
                        (cadr (sly-xref.location xref)))
               (error "%s" xrefs))))
          (t
           (sly-push-definition-stack)
           (sly-xref--show-results file-alist 'definition nil
                                   (sly-current-package)
                                   (cons (selected-window)
                                         method))))))

(mgl-pax-defimpl mgl-pax-visit-lisp-location :sly (dspec-and-location-list)
  (mgl-pax-sly-edit-xrefs dspec-and-location-list))

(mgl-pax-defimpl mgl-pax-read-lisp-from-minibuffer :sly
  (prompt &optional initial-value history)
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

(mgl-pax-defimpl mgl-pax-bounds-of-symbol-at-point :sly ()
  (sly-bounds-of-symbol-at-point))


;;;; Cleanup

;;; This is called automatically by (unload-feature 'mgl-pax-sly).
(defun mgl-pax-sly-unload-function ()
  "Hook into (unload-feature 'mgl-pax-sly)."
  (mgl-pax-unhijack-sly-doc-keys)
  (advice-remove 'sly-edit-definition #'sly-edit-definition@mgl-pax)
  (advice-remove 'sly--setup-completion #'sly--setup-completion@mgl-pax)
  (setq mgl-pax-loaded-backends (delq 'mgl-pax-sly mgl-pax-loaded-backends)))


;;;; MGL-PAX::@SLY-SETUP (also see MGL-PAX::@EMACS-SETUP)

(defcustom mgl-pax-hijack-sly-doc-keys t
  "Variable for user option that controls PAX-SLIME keybindings.
If true, then bind mgl-pax functions in `sly-doc-map' with
`mgl-pax-hijack-sly-doc-keys' upon loading `mgl-pax-sly'.  See
MGL-PAX::@EMACS-KEYS for details."
  :Type 'boolean
  :group 'mgl-pax)

(defvar mgl-pax-sly-doc-map-overrides
  '((?a sly-apropos mgl-pax-apropos)
    (?z sly-apropos-all mgl-pax-apropos-all)
    (?p sly-apropos-package mgl-pax-apropos-package)
    (?f sly-describe-function mgl-pax-document)
    (?d sly-describe-symbol mgl-pax-hideshow-documentation)
    (?c nil mgl-pax-hideshow-comments)
    (?v nil mgl-pax-current-definition-toggle-view)
    (?u nil mgl-pax-edit-parent-section)))

(defun mgl-pax-sly--override-plain-doc-keys (&optional reversep)
  "Bind (with REVERSEP unbind) the plain versions of keys.
Uses `MGL-PAX-SLY-DOC-MAP-OVERRIDES'.  Because Sly only binds control
variants and so there's no old binding, `MGL-PAX-OVERRIDE-KEYS'
doesn't handle this."
  (cl-loop for (key old new) in mgl-pax-sly-doc-map-overrides
           when old
           do (let ((k `[,key]))
                (if reversep
                    (when (eq (lookup-key sly-doc-map k) new)
                      (define-key sly-doc-map k nil))
                  (unless (lookup-key sly-doc-map k)
                    (define-key sly-doc-map k new))))))


(defun mgl-pax-hijack-sly-doc-keys ()
  "See MGL-PAX::@EMACS-SETUP."
  (interactive)
  (mgl-pax-override-keys sly-doc-map mgl-pax-sly-doc-map-overrides)
  (mgl-pax-sly--override-plain-doc-keys))

(defun mgl-pax-unhijack-sly-doc-keys ()
  "See MGL-PAX::@EMACS-SETUP."
  (interactive)
  ;; Plain keys must come first.
  (mgl-pax-sly--override-plain-doc-keys t)
  (mgl-pax-override-keys sly-doc-map mgl-pax-sly-doc-map-overrides t))

(when mgl-pax-hijack-sly-doc-keys
  (mgl-pax-hijack-sly-doc-keys))


;;;; Integration with `M-.'

(define-advice sly-edit-definition (:around
                                    (oldfun &optional name where)
                                    mgl-pax)
  (interactive)
  (mgl-pax-edit-definition-advice
   (called-interactively-p 'any)
   (lambda (name where)
     (or (mgl-pax-run-edit-lisp-definition-hooks name where)
         (funcall oldfun name where)))
   name where))



;;;; Locative completion

(define-advice sly--setup-completion (:after () mgl-pax)
  ;; `mgl-pax-completing-for' is bound by `mgl-pax-read-from-minibuffer'.
  (when mgl-pax-completing-for
    (add-hook 'completion-at-point-functions
              #'mgl-pax-completions-at-point nil t)))



(add-to-list 'mgl-pax-loaded-backends 'mgl-pax-sly)


(provide 'mgl-pax-sly)
