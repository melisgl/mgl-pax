;;; MGL-PAX M-. integration

(defun mgl-pax-edit-locative-definitions (name &optional where)
  (or (mgl-pax-locate-definition name (mgl-pax-locative-before) where)
      (mgl-pax-locate-definition name (mgl-pax-locative-after) where)
      (mgl-pax-locate-definition name (mgl-pax-locative-after-in-brackets)
                                 where)
      ;; Support "foo function" and "function foo" syntax in
      ;; interactive use.
      (let ((pos (cl-position ?\s name)))
        (when pos
          (or (mgl-pax-locate-definition (cl-subseq name 0 pos)
                                         (cl-subseq name (1+ pos))
                                         where)
              (mgl-pax-locate-definition (cl-subseq name (1+ pos))
                                         (cl-subseq name 0 pos)
                                         where))))
      ;; This catches pluralized symbols without locatives e.g
      ;; (MGL-PAX:SECTIONs) and is also responsible for dealing with multiple
      ;; references.
      (mgl-pax-locate-definition name "" where)))

(defun mgl-pax-locative-before ()
  (ignore-errors (save-excursion
                   (slime-beginning-of-symbol)
                   (slime-last-expression))))

(defun mgl-pax-locative-after ()
  (ignore-errors (save-excursion
                   (slime-end-of-symbol)
                   (slime-forward-sexp)
                   (slime-last-expression))))

(defun mgl-pax-locative-after-in-brackets ()
  (ignore-errors (save-excursion
                   (slime-end-of-symbol)
                   (skip-chars-forward "`" (+ (point) 1))
                   (when (and (= 1 (skip-chars-forward "\\]" (+ (point) 1)))
                              (= 1 (skip-chars-forward "\\[" (+ (point) 1))))
                     (buffer-substring-no-properties
                      (point)
                      (progn (search-forward "]" nil (+ (point) 1000))
                             (1- (point))))))))

(defun mgl-pax-locate-definition (name locative where)
  (when locative
    (let ((locations
           (slime-eval
            ;; Silently fail if mgl-pax is not loaded.
            `(cl:when (cl:find-package :mgl-pax)
                      (cl:funcall
                       (cl:find-symbol
                        (cl:symbol-name :locate-definitions-for-emacs) :mgl-pax)
                       ,name ,locative)))))
      (when (and (consp locations)
                 (not (eq (car locations) :error)))
        (slime-edit-definition-cont
         (slime-postprocess-xrefs locations)
         "dummy name"
         where)))))

(add-hook 'slime-edit-definition-hooks 'mgl-pax-edit-locative-definitions)
