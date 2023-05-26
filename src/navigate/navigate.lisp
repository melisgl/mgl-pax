(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defvar *navigate-loaded* nil)

(defun/autoloaded ensure-navigate-loaded ()
  (prog1 *navigate-loaded*
    (setq *navigate-loaded* t)))

(defsection @navigating-in-emacs (:title "Navigating Sources in Emacs")
  """Integration into [SLIME's `\\M-.`][slime-m-.]
  (`slime-edit-definition`) allows one to visit the source location of
  the definition that's identified by `slime-symbol-at-point` parsed
  as a @WORD and the locative before or after the symbol in a buffer.
  With this extension, if a locative is the previous or the next
  expression around the symbol of interest, then `\\M-.` will go
  straight to the definition which corresponds to the locative. If
  that fails, `\\M-.` will try to find the definitions in the normal
  way, which may involve popping up an xref buffer and letting the
  user interactively select one of possible definitions.

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

  ```
  ;;;; See FOO `(method () (t t t))` for how this all works.
  ;;;; But if the locative has semicolons inside: FOO `(method
  ;;;; () (t t t))`, then it won't, so be wary of line breaks
  ;;;; in comments.
  ```

  With a prefix argument (`C-u M-.`), one can enter a symbol plus a
  locative separated by whitespace to preselect one of the
  possibilities.

  The `\\M-.` extensions can be enabled by loading `src/mgl-pax.el`.
  See @EMACS-SETUP."""
  (mgl-pax/navigate asdf:system))

;;; Ensure that some Swank internal facilities (such as
;;; SWANK::FIND-DEFINITIONS-FIND-SYMBOL-OR-PACKAGE,
;;; SWANK::WITH-BUFFER-SYNTAX, SWANK::PARSE-SYMBOL) are operational
;;; even when not running under Slime.
(defmacro with-swank (() &body body)
  `(let* ((swank::*buffer-package* (if (boundp 'swank::*buffer-package*)
                                       swank::*buffer-package*
                                       *package*))
          (swank::*buffer-readtable*
            (if (boundp 'swank::*buffer-readtable*)
                swank::*buffer-readtable*
                (swank::guess-buffer-readtable swank::*buffer-package*))))
     ,@body))

;;; List Swank source locations (suitable for `make-slime-xref') for
;;; the things that the Elisp side considers possible around the point
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
;;; If none of the resulting references can be resolved (including if
;;; no locatives are specified), then list all possible definitions.
;;;
;;; Return (DSPEC LOCATION) (with DSPEC as a string). If AS-REF, then
;;; return a list of (OBJECT LOCATIVE) elements (both strings), where
;;; LOCATIVE is NIL when a dspec returned by
;;; SWANK-BACKEND:FIND-DEFINITIONS cannot be converted to a reference
;;; with DSPEC-TO-REFERENCE.
(defun/autoloaded locate-definitions-for-emacs (object-and-locatives-list
                                                &key as-ref)
  (with-swank ()
    (swank::converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (locate-definitions-for-emacs-1 object-and-locatives-list
                                        :as-ref as-ref)))))

(defun locate-definitions-for-emacs-1 (object-and-locatives-list &key as-ref)
  (convert-for-emacs
   (or (loop for (object-word locative-strings) in object-and-locatives-list
             append (loop for locative-string in locative-strings
                          append (locate-word-definition-for-emacs
                                  object-word locative-string
                                  :include-no-source as-ref)))
       (loop for entry in object-and-locatives-list
             append (locate-all-definitions-for-emacs (first entry))))
   :as-ref as-ref))

(defun convert-for-emacs (emacsref-dspec-and-location-list &key as-ref)
  (if as-ref
      (loop for (emacsref dspec location) in emacsref-dspec-and-location-list
            collect emacsref)
      (loop for (emacsref dspec location) in emacsref-dspec-and-location-list
            collect `(,(prin1-to-string dspec) ,location))))

(defun reference-for-emacs (reference)
  (with-standard-io-syntax*
    (list (let ((object (reference-object reference)))
            (if (stringp object)
                object
                (prin1-to-string object)))
          ;; The locative may not be readable (e.g. methods with EQL
          ;; specializers with unreadable stuff).
          (let ((*print-readably* nil))
            (prin1-to-string (reference-locative reference))))))

(defun ambiguous-reference-for-emacs (object)
  (with-standard-io-syntax*
    (list (prin1-to-string object) nil)))

;;; Return a list of (EMACSREF DSPEC LOCATION) objects.
(defun locate-all-definitions-for-emacs (word)
  (loop for object in (parse-word word)
          thereis (append
                   ;; Standard stuff supported by Swank.
                   (mapcar (lambda (dspec-and-location)
                             (destructuring-bind (dspec location)
                                 dspec-and-location
                               (let ((reference (dspec-to-reference dspec
                                                                    object)))
                                 (list (if reference
                                           (reference-for-emacs reference)
                                           (ambiguous-reference-for-emacs
                                            object))
                                       (if reference
                                           ;; Replace (DEFCLASS FOO)
                                           ;; with (FOO CLASS).
                                           (reference-to-fake-dspec reference)
                                           dspec)
                                       location))))
                           (swank-find-definitions-for-object object))
                   ;; For locatives not supported by Swank, we try
                   ;; locatives on *LOCATIVE-SOURCE-SEARCH-LIST* one
                   ;; by one and see if they lead somewhere from the
                   ;; @NAMEs in WORD.
                   (mapcan (lambda (locative)
                             (let ((thing (locate object locative
                                                  :errorp nil)))
                               (when thing
                                 (let ((location (find-source thing))
                                       (ref (canonical-reference thing)))
                                   `((,(reference-for-emacs ref)
                                      ,(reference-to-fake-dspec ref)
                                      ,location))))))
                           *locative-source-search-list*))))

(defun swank-find-definitions-for-object (object)
  (cond ((stringp object)
         ;; E.g. to find the package when OBJECT is "MGL-PAX".
         (swank-backend:find-definitions
          (make-symbol (adjust-string-case object))))
        ((symbolp object)
         (swank-backend:find-definitions object))))

(defun locate-word-definition-for-emacs (word locative-string
                                         &key include-no-source)
  (let ((locative (read-locative-from-markdown locative-string)))
    (when locative
      (loop for object in (parse-word word)
              thereis (let* ((ref (make-reference object locative))
                             ;; See MGL-PAX-TEST::TEST-NAVIGATION-TO-SOURCE.
                             (location (ignore-errors (find-source ref))))
                        (when (or include-no-source
                                  (eq (first location) :location))
                          `((,(reference-for-emacs ref)
                             ,(reference-to-fake-dspec ref)
                             ,location))))))))

(defun read-locative-from-markdown (string)
  (read-locative-from-string
   ;; It is assumed that names of locative types are not funny, and we
   ;; can trim aggressively.
   (string-trim ":`',." string)))

;;; Parse "LOCATIVE-TYPE" and "(LOCATIVE-TYPE ...)" like
;;; READ-FROM-STRING, but try to minimize the chance of interning
;;; junk. That is, don't intern LOCATIVE-TYPE (it must be already) or
;;; anything in "..." if LOCATIVE-TYPE is not a valid locative type.
(defun read-locative-from-string (string &key junk-allowed)
  (handler-case
      (multiple-value-bind (symbol pos)
          (read-interned-symbol-from-string string)
        (if pos
            (when (and (or junk-allowed
                           (not (find-if-not #'whitespacep string :start pos)))
                       (locate symbol 'locative :errorp nil))
              (values symbol pos))
            (let ((first-char-pos (position-if-not #'whitespacep string)))
              (when (and first-char-pos (char= (elt string first-char-pos) #\())
                ;; Looks like a list. The first element must be an
                ;; interned symbol naming a locative.
                (let ((delimiter-pos (position-if #'delimiterp string
                                                  :start (1+ first-char-pos))))
                  (multiple-value-bind (symbol found)
                      (swank::parse-symbol
                       (subseq string (1+ first-char-pos) delimiter-pos))
                    (when (and found (locate symbol 'locative :errorp nil))
                      ;; The rest of the symbols in the string need not be
                      ;; already interned, so let's just READ.
                      (multiple-value-bind (locative position)
                          (ignore-errors (read-from-string string))
                        (when locative
                          (values locative position))))))))))
    ((or reader-error end-of-file) ()
      nil)))

(defun read-locatives-from-string (string)
  (let ((locatives ())
        (start 0))
    (loop
      (multiple-value-bind (locative pos)
          (read-locative-from-string (subseq string start) :junk-allowed t)
        (cond (locative
               (push locative locatives)
               (incf start pos))
              (t
               (return)))))
    (values (reverse locatives) start)))

;;; Parse "OBJECT LOCATIVE-TYPE" or "OBJECT (LOCATIVE-TYPE ...))", but
;;; only intern stuff if LOCATIVE-TYPE is interned. Return 1. object,
;;; 2. locative, 3. whether at least one locative was found, 4. the
;;; unparsable junk if any unread non-whitespace characters are left.
(defun read-reference-from-string (string &key multiple-locatives-p)
  (flet ((maybe-junk (start)
           (let ((locstring (string-trim *whitespace-chars*
                                         (subseq string start))))
             (if (zerop (length locstring))
                 nil
                 locstring))))
    (handler-case
        ;; Skip whatever OBJECT may be ...
        (let ((object-end-pos (n-chars-would-read string)))
          ;; ... then just try to parse the locative.
          (multiple-value-bind (one-or-more-locatives pos)
              (if multiple-locatives-p
                  (read-locatives-from-string (subseq string object-end-pos))
                  (read-locative-from-string (subseq string object-end-pos)))
            (if one-or-more-locatives
                (values (read-object-from-string
                         (subseq string 0 object-end-pos))
                        one-or-more-locatives t
                        (maybe-junk (+ object-end-pos pos)))
                (values nil nil nil (maybe-junk object-end-pos)))))
      ((or reader-error end-of-file) ()
        nil))))

(defun read-object-from-string (string)
  (let ((string (string-trim *whitespace-chars* string)))
    (if (alexandria:starts-with #\" string)
        (read-from-string string)
        (multiple-value-bind (symbol found)
            (swank::parse-symbol
             ;; Make "PAX:@PAX-MANUAL SECTION" work with a single
             ;; colon even though @PAX-MANUAL is an internal symbol.
             (double-single-colon string))
          (if found
              symbol
              (adjust-string-case string))))))

(defun double-single-colon (string)
  (let ((pos (position #\: string)))
    (if (and pos (not (search "::" string)))
        (concatenate 'string (subseq string 0 pos) ":" (subseq string pos))
        string)))

(defun delimiterp (char)
  (or (whitespacep char) (find char "()'`\"")))

;;; The returned dspec is fake because it doesn't necessarily match
;;; what SWANK/BACKEND:FIND-DEFINITIONS would produce, but it doesn't
;;; matter because it's only used as a label to show to the user.
(defun reference-to-fake-dspec (reference)
  (list (reference-object reference) (reference-locative reference)))
