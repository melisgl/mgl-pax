;;; MGL-PAX transcription

(defun mgl-pax-transcribe-last-expression ()
  "A bit like C-u C-x C-e (slime-eval-last-expression) that
inserts the output and values of the sexp before the point, this
does the same but with MGL-PAX:TRANSCRIBE. With a prefix
arg (except -), it inserts a ; character before the markup."
  (interactive)
  (insert
   (save-excursion
     (let* ((end (point))
            (start (progn (backward-sexp)
                          (move-beginning-of-line nil)
                          (point))))
       (mgl-pax-transcribe start end (mgl-pax-transcribe-comment-arg)
                           nil nil nil)))))

(defun mgl-pax-retranscribe-region (start end)
  "Updates the transcription in the current region (as in calling
MGL-PAX:TRANSCRIBE with :UPDATE-ONLY T). With a prefix arg of -,
the output transcript will use non-commented
prefixes (corresponding to :COMMENT nil). With other prefix args,
the commented prefixes will be used (:COMMENT T). Without a
prefix arg, the prefixes will not be changed (:COMMENT :KEEP)."
  (interactive "r")
  (let* ((point-at-start-p (= (point) start))
         (point-at-end-p (= (point) end))
         (transcript (mgl-pax-transcribe start end
                                         (mgl-pax-transcribe-comment-arg)
                                         t t nil)))
    (if point-at-start-p
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (insert transcript))
      (save-excursion
          (goto-char start)
          (delete-region start end))
      (insert transcript))))

(defun mgl-pax-transcribe-comment-arg ()
  (cond ((eq current-prefix-arg '-) nil)
        (current-prefix-arg t)
        (t :keep)))

(defun mgl-pax-transcribe (start end comment update-only echo
                                 first-line-special-p)
  (let ((transcription
         (slime-eval
          `(cl:if (cl:find-package :mgl-pax)
                  (cl:funcall
                   (cl:find-symbol
                    (cl:symbol-name :transcribe-for-emacs) :mgl-pax)
                   ,(buffer-substring-no-properties start end)
                   ',comment ',update-only ',echo ',first-line-special-p)
                  t))))
    (if (eq transcription t)
        (error "MGL-PAX is not loaded.")
      transcription)))
