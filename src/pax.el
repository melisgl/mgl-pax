;;; MGL-PAX M-. integration

(defun mgl-pax-edit-locative-definition (name &optional where)
  (or (mgl-pax-locate-definition name (mgl-pax-locative-before))
      (mgl-pax-locate-definition name (mgl-pax-locative-after))
      (mgl-pax-locate-definition name (mgl-pax-locative-after-in-brackets))
      ;; support "foo function" and "function foo" syntax in
      ;; interactive use
      (let ((pos (cl-position ?\s name)))
        (when pos
          (or (mgl-pax-locate-definition (cl-subseq name 0 pos)
                                       (cl-subseq name (1+ pos)))
              (mgl-pax-locate-definition (cl-subseq name (1+ pos))
                                       (cl-subseq name 0 pos)))))
      ;; This catches pluralized symbols without locatives e.g
      ;; (MGL-PAX:SECTIONs).
      (mgl-pax-locate-definition name "")))

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

(defun mgl-pax-locate-definition (name locative)
  (when locative
    (let ((location
           (slime-eval
            ;; Silently fail if mgl-pax is not loaded.
            `(cl:when (cl:find-package :mgl-pax)
                      (cl:funcall
                       (cl:find-symbol
                        (cl:symbol-name :locate-definition-for-emacs) :mgl-pax)
                       ,name ,locative)))))
      (when (and (consp location)
                 (not (eq (car location) :error)))
        (slime-edit-definition-cont
         (list (make-slime-xref :dspec `(,name)
                                :location location))
         "dummy name"
         where)))))

(add-hook 'slime-edit-definition-hooks 'mgl-pax-edit-locative-definition)
