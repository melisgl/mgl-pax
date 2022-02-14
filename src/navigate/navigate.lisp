(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defvar *navigate-loaded* nil)

(defun/autoloaded ensure-navigate-loaded ()
  (prog1 *navigate-loaded*
    (setq *navigate-loaded* t)))

(defsection @navigating-in-emacs (:title "Navigating Sources in Emacs")
  """Integration into [SLIME's `\\M-.`][slime-m-.]
  (`slime-edit-definition`) allows one to visit the source location of
  the thing that's identified by `slime-symbol-at-point` parsed as a
  @WORD and the locative before or after the symbol in a buffer. With
  this extension, if a locative is the previous or the next expression
  around the symbol of interest, then `\\M-.` will go straight to the
  definition which corresponds to the locative. If that fails, `\\M-.`
  will try to find the definitions in the normal way, which may
  involve popping up an xref buffer and letting the user interactively
  select one of possible definitions.

  In the following examples, when the cursor is on one of the
  characters of `FOO` or just after `FOO`, pressing `\\M-.` will visit
  the definition of function `FOO`:

      function foo
      foo function
      (function foo)
      (foo function)

  In particular, references in a DEFSECTION form are in (SYMBOL
  LOCATIVE) format so `\\M-.` will work just fine there.

  Just like vanilla `\\M-.`, this works in comments and docstrings. In
  the next example, pressing `\\M-.` on `FOO` will visit `FOO`'s
  default method:

  ```commonlisp
  ;;;; See FOO `(method () (t t t))` for how this all works.
  ;;;; But if the locative has semicolons inside: FOO `(method
  ;;;; () (t t t))`, then it won't, so be wary of line breaks
  ;;;; in comments.
  ```

  With a prefix argument (`C-u M-.`), one can enter a symbol plus a
  locative separated by whitespace to preselect one of the
  possibilities.

  The `\\M-.` extensions can be enabled by loading `src/pax.el`."""
  (mgl-pax/navigate asdf:system))

;;; List Swank source locations (suitable for make-slime-xref) for the
;;; things that the Elisp side considers possible around the point
;;; when M-. is invoked. OBJECT-AND-LOCATIVES-LIST is a list like
;;;
;;; (("[section][" ("junk-before" "class"))
;;;  ("section" ("class")))
;;;
;;; where each element in the list is a consist of a @WORD and a list
;;; of possible @LOCATIVEs found next to it. All strings may need to
;;; be trimmed of punctuation characters, but [ and ] are dealt with
;;; by providing two possiblities as in the above example.
;;;
;;; If none of the resulting references cannot be resolved (including
;;; if no locatives are specified), then list all possible
;;; definitions.
(defun/autoloaded locate-definitions-for-emacs (object-and-locatives-list)
  (with-swank ()
    (swank-backend::converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (stringify-dspec
         (or (loop for (object-word locative-strings)
                     in object-and-locatives-list
                   append (loop for locative-string in locative-strings
                                append (ignore-errors
                                        (locate-definition-for-emacs-1
                                         object-word locative-string))))
             (loop for entry in object-and-locatives-list
                   append (ignore-errors
                           (locate-all-definitions-for-emacs
                            (first entry))))))))))

(defun stringify-dspec (dspec-and-location-list)
  (loop for entry in dspec-and-location-list
        collect `(,(prin1-to-string (first entry)) ,(second entry))))

(defun locate-all-definitions-for-emacs (word)
  ;; For locatives not supported by Swank, we try locatives on
  ;; *LOCATIVE-SOURCE-SEARCH-LIST* one by one and see if they lead
  ;; somewhere from the @NAMEs in WORD.
  (loop for object in (parse-word word)
          thereis (append
                   ;; Standard stuff supported by Swank.
                   (cond ((stringp object)
                          (swank-backend:find-definitions (make-symbol object)))
                         ((symbolp object)
                          (swank-backend:find-definitions object)))
                   (mapcan (lambda (locative)
                             (ignore-errors
                              (let ((thing (locate object locative
                                                   :errorp nil)))
                                (when thing
                                  (let ((location (find-source thing)))
                                    `((,(reference-to-dspec
                                         (canonical-reference thing))
                                       ,location)))))))
                           *locative-source-search-list*))))

(defun reference-to-dspec (reference)
  (list (reference-object reference) (reference-locative reference)))

(defun locate-definition-for-emacs-1 (word locative-string)
  (let ((locative (read-locative-from-markdown locative-string)))
    (when locative
      (loop for object in (parse-word word)
              thereis (let* ((ref (make-reference object locative))
                             (location (find-source ref)))
                        (when (eq (first location) :location)
                          ;; List of one Swank dspec and location.
                          `((,(reference-to-dspec ref) ,location))))))))

(defun read-locative-from-markdown (string)
  (read-locative-from-string
   ;; It is assumed that names of locative types are not funny, and we
   ;; can trim aggressively.
   (string-trim ":`',." string)))

;;; Parse "LOCATIVE-TYPE" and "(LOCATIVE-TYPE ...)" like
;;; READ-FROM-STRING, but only intern stuff if LOCATIVE-TYPE is a
;;; valid locative.
(defun read-locative-from-string (string)
  (let ((swank::*buffer-package* *package*))
    (multiple-value-bind (symbol found)
        (with-swank ()
          (swank::parse-symbol (string-trim *whitespace-chars* string)))
      (if found
          (when (locate symbol 'locative :errorp nil)
            symbol)
          (let ((first-char-pos (position-if-not #'whitespacep string)))
            (when (and first-char-pos
                       (char= (elt string first-char-pos) #\())
              ;; Looks like a list. The first element must be an
              ;; interned symbol naming a locative.
              (let ((delimiter-pos (position-if #'delimiterp string
                                                :start (1+ first-char-pos))))
                (multiple-value-bind (symbol found)
                    (swank::parse-symbol
                     (subseq string (1+ first-char-pos) delimiter-pos))
                  (when (and found (locate symbol 'locative :errorp nil))
                    ;; The rest of the symbols in the string need not
                    ;; be already interned, so let's just read it.
                    (ignore-errors
                     ;; FIXME: check that there is no junk left.
                     (read-from-string string)))))))))))

;;; Parse "OBJECT LOCATIVE-TYPE" or "OBJECT (LOCATIVE-TYPE ...))" but
;;; only intern stuff if LOCATIVE-TYPE is a valid locative.
(defun read-reference-from-string (string)
  (handler-case
      ;; Skip whatever OBJECT may be ...
      (let* ((pos (nth-value 1 (let ((*read-suppress* t))
                                 (read-from-string string))))
             ;; ... then just try to parse the locative.
             (locative (read-locative-from-string (subseq string pos))))
        (when locative
          (values (read-from-string string) locative t)))
    ((or reader-error end-of-file) ()
      nil)))

(defun delimiterp (char)
  (or (whitespacep char) (find char "()'`\"")))

