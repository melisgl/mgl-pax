;;; MGL-PAX transcription

(defun mgl-pax-transcribe-last-expression ()
  "A bit like C-u C-x C-e (slime-eval-last-expression) that
inserts the output and values of the sexp before the point,
this does the same but with MGL-PAX:TRANSCRIBE."
  (interactive)
  (insert
   (save-excursion
     (let* ((end (point))
            (start (progn (backward-sexp)
                          (move-beginning-of-line nil)
                          (point))))
       (mgl-pax-transcribe start end nil nil nil)))))

(defun mgl-pax-retranscribe-region (start end)
  "Updates the transcription in the current region (as in calling
MGL-PAX:TRANSCRIBE with :UPDATE-ONLY T.)"
  (interactive "r")
  (let* ((point-at-start-p (= (point) start))
         (point-at-end-p (= (point) end))
         (transcript (mgl-pax-transcribe start end t t t)))
    (if point-at-start-p
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (insert transcript))
      (save-excursion
          (goto-char start)
          (delete-region start end))
      (insert transcript))))

(defun mgl-pax-transcribe (start end update-only echo first-line-special-p)
  (let ((transcription
         (slime-eval
          `(cl:if (cl:find-package :mgl-pax)
                  (cl:funcall
                   (cl:find-symbol
                    (cl:symbol-name :transcribe-for-emacs) :mgl-pax)
                   ,(buffer-substring-no-properties start end)
                   ,update-only ,echo ,first-line-special-p)
                  t))))
    (if (eq transcription t)
        (error "MGL-PAX is not loaded.")
      transcription)))
