;;;; MGL-PAX M-. integration

;;; When it's on slime-edit-definition-hooks, M-. calls this function
;;; with (slime-symbol-at-point) as NAME. slime-symbol-at-point works
;;; fine in code, but in printed representations and docstrings
;;; heuristics are needed (just think "SYM." and "#<SYM"). This part
;;; we leave for the the Common Lisp function
;;; MGL-PAX:LOCATE-DEFINITION-FOR-EMACS. However, we handle here the
;;; complications caused by Markdown, whose code (`code`) and
;;; reference link syntax [title][id] is used by PAX, maybe both at
;;; the same time as in [`FOO`][function] or [FOO][`function`]. ?` is
;;; a delimiter, but ?\[ is not, which means that pressing M-. on FOO
;;; will result in NAME being "FOO" or "[FOO][". "[FOO][" is a valid
;;; symbol name, so we definitely want to look up definitions for it.
;;; In addition, we also look up definitions for the symbol whose name
;;; has the parts beyond [] cut off.
(defun mgl-pax-edit-definitions (name &optional where)
  ;; Clobber stuff from old PAX that may be on
  ;; slime-edit-definition-hooks.
  (defun mgl-pax-edit-locative-definitions (name &optional where)
    ())
  (let ((name-in-buffer (slime-symbol-at-point)))
    (if (string= name name-in-buffer)
        (mgl-pax-edit-buffer-definitions name where)
      (mgl-pax-edit-interactive-definitions name where))))

(defun mgl-pax-edit-buffer-definitions (name where)
  (let* ((bounds (slime-bounds-of-symbol-at-point))
         (locatives (mgl-pax-find-locatives bounds))
         (reflink-name-and-locatives (mgl-pax-parse-reflink bounds)))
    (mgl-pax-locate-definitions (remove nil `((,name ,locatives)
                                              ,reflink-name-and-locatives))
                                where)))

;;; Return the sexps before and after (slime-symbol-at-point),
;;; skipping some markup.
(cl-defun mgl-pax-find-locatives
    (&optional (bounds (slime-bounds-of-symbol-at-point)))
  (remove nil (list (mgl-pax-locative-before (car bounds))
                    (mgl-pax-locative-after (cdr bounds)))))

;;; Around POINT, parse "[FOO][function]" as a Markdown reference
;;; link. Return the name and the locative string as a list like
;;; ("FOO" ("function")).
(cl-defun mgl-pax-parse-reflink
    (&optional (bounds (slime-bounds-of-symbol-at-point)))
  (when bounds
    (let* ((symbol-start (car bounds))
           (symbol-end (cdr bounds))
           (ref-start (1+ (or (save-excursion
                                (search-backward "[" symbol-start t))
                              (1- symbol-start))))
           (ref-end (1- (or (save-excursion
                              (search-forward "]" symbol-end t))
                            (1+ symbol-end)))))
      ;; If the name contains ?\[ or ?\] ...
      (unless (and (= ref-start symbol-start)
                   (= ref-end symbol-end))
        ;; ... then cut off anything beyond those characters to get a
        ;; new name.
        (let ((name (buffer-substring-no-properties ref-start ref-end))
              (locative (mgl-pax-locative-after ref-end)))
          (list name (if locative
                         (list locative)
                       ())))))))

(defun mgl-pax-edit-interactive-definitions (string &optional where)
  (let ((pos (cl-position ?\s string)))
    (if pos
        (let ((first (cl-subseq string 0 pos))
              (second (cl-subseq string (1+ pos))))
          ;; "FOO function" or "function FOO"
          (mgl-pax-locate-definitions `((,first (,second))
                                        (,second (,first)))
                                      where))
      ;; "FOO"
      (mgl-pax-locate-definitions `((,string ())) where))))

(cl-defun mgl-pax-locative-before (&optional (point (point)))
  (ignore-errors
    (save-excursion
      (goto-char (1- point))
      (skip-chars-backward ";` \n\t")
      (slime-last-expression))))

(cl-defun mgl-pax-locative-after (&optional (point (point)))
  (ignore-errors
    (save-excursion
      (goto-char point)
      (skip-chars-forward "[];`\" \n\t")
      (if (equal (string (char-after)) "(")
          ;; [FOO][(function)]
          (save-excursion
            (slime-forward-sexp)
            (slime-last-expression))
        ;; [FOO][function], [`FOO`][function], [FOO ][function]
        (let ((end-pos+1 (save-excursion
                           (search-forward-regexp "\\(\\]\\|`\\)"
                                                  (+ (point) 1000)
                                                  t))))
          (if end-pos+1
              (save-restriction
                (narrow-to-region (point) (1- end-pos+1))
                (slime-forward-sexp)
                (slime-last-expression))
            (slime-forward-sexp)
            (slime-last-expression)))))))

(defun mgl-pax-locate-definitions (name-and-locatives-list where)
  (let ((locations (mgl-pax-call-locate-definitions-for-emacs
                    name-and-locatives-list)))
    (when (and (consp locations)
               (not (eq (car locations) :error)))
      (slime-edit-definition-cont
       (slime-postprocess-xrefs locations)
       "dummy name"
       where))))

(defun mgl-pax-call-locate-definitions-for-emacs (name-and-locatives-list)
  (slime-eval
   ;; Silently fail if MGL-PAX is not loaded.
   `(cl:when (cl:find-package :mgl-pax)
             (cl:funcall (cl:find-symbol (cl:symbol-name
                                          :locate-definitions-for-emacs)
                                         :mgl-pax)
                         ',name-and-locatives-list))))

(add-hook 'slime-edit-definition-hooks 'mgl-pax-edit-definitions)
