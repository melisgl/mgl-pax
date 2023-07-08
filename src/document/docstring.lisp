(in-package :mgl-pax)

;;; Normalize indentation of docstrings as described in (METHOD ()
;;; (STRING T)) DOCUMENT-OBJECT.
(defun strip-docstring-indentation (docstring &key (first-line-special-p t))
  (let ((indentation
          (docstring-indentation docstring
                                 :first-line-special-p first-line-special-p)))
    (values (round-up-indentation
             (with-output-to-string (out)
               (with-input-from-string (s docstring)
                 (loop for i upfrom 0
                       do (multiple-value-bind (line missing-newline-p)
                              (read-line s nil nil)
                            (unless line
                              (return))
                            (if (and first-line-special-p (zerop i))
                                (write-string line out)
                                (write-string (subseq* line indentation) out))
                            (unless missing-newline-p
                              (terpri out)))))))
            indentation)))

(defun n-leading-spaces (line)
  (let ((n 0))
    (loop for i below (length line)
          while (char= (aref line i) #\Space)
          do (incf n))
    n))

;;; Return the minimum number of leading spaces in non-blank lines
;;; after the first.
(defun docstring-indentation (docstring &key (first-line-special-p t))
  (let ((n-min-indentation nil))
    (with-input-from-string (s docstring)
      (loop for i upfrom 0
            for line = (read-line s nil nil)
            while line
            do (when (and (or (not first-line-special-p) (plusp i))
                          (not (blankp line)))
                 (when (or (null n-min-indentation)
                           (< (n-leading-spaces line) n-min-indentation))
                   (setq n-min-indentation (n-leading-spaces line))))))
    (or n-min-indentation 0)))

(defun docstring-indentation* (lines &key (first-line-special-p t))
  (let ((n-min-indentation nil))
    (loop for i upfrom 0
          for line in lines
          while line
          do (when (and (or (not first-line-special-p) (plusp i))
                        (not (blankp line)))
               (when (or (null n-min-indentation)
                         (< (n-leading-spaces line) n-min-indentation))
                 (setq n-min-indentation (n-leading-spaces line)))))
    (or n-min-indentation 0)))

;;; Many docstrings out in the wild indent code by 1-3 spaces instead
;;; of 4. Round the indentation of consecutive non-zero indented lines
;;; up to a multiple of 4.
(defun round-up-indentation (docstring)
  (with-output-to-string (out)
    (with-input-from-string (in docstring)
      (let ((indented-lines :none))
        (flet
            ((flush-indented-lines ()
               (let* ((lines (reverse indented-lines))
                      (prefix (extra-indent-for-lisp-code lines)))
                 #+nil
                 (format t "FLUSH: ~S~%  ~S~%" last-blank-line indented-lines)
                 (dolist (line (reverse indented-lines))
                   (write-string prefix out)
                   (write-string line out)))
               (setq indented-lines :none)))
          (loop for (line missing-newline-p) = (multiple-value-list
                                                (read-line in nil nil))
                while line
                do (let ((blankp (blankp line))
                         (indentedp (starts-with #\Space line))
                         (line (if missing-newline-p
                                   line
                                   (format nil "~A~%" line))))
                     #+nil
                     (format t "LINE: ~S ~S ~S~%  ~S~%"
                             blankp indentedp indented-lines line)
                     ;; End the indented block on blanks and
                     ;; non-indented lines.
                     (when (and (or blankp (not indentedp))
                                (not (eq indented-lines :none)))
                       (flush-indented-lines))
                     (if (eq indented-lines :none)
                         (write-string line out)
                         (push line indented-lines))
                     ;; Start an indented block.
                     (when blankp
                       (setq indented-lines ()))))
          (unless (eq indented-lines :none)
            (flush-indented-lines)))))))

(defun extra-indent-for-lisp-code (lines)
  (if (lines-looking-like-lisp-code-p lines)
      (let* ((indentation (docstring-indentation* lines
                                                  :first-line-special-p nil))
             (new-indentation (round-up-to-multiple-of indentation 4)))
        (make-string (- new-indentation indentation) :initial-element #\Space))
      ""))

(defun lines-looking-like-lisp-code-p (lines)
  (and lines
       (let* ((line (first lines))
              (n-spaces (n-leading-spaces line)))
         (and (<= n-spaces (length line))
              (member (aref line n-spaces) '(#\( #\;))))))

(defun round-up-to-multiple-of (n m)
  (* (ceiling n m) m))
