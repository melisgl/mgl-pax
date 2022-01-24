(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @mgl-pax-navigating-in-emacs (:title "Navigating Sources in Emacs")
  "Integration into SLIME's `M-.` (`slime-edit-definition`) allows one
  to visit the source location of the thing that's identified by a
  symbol and the locative before or after the symbol in a buffer. With
  this extension, if a locative is the previous or the next expression
  around the symbol of interest, then `M-.` will go straight to the
  definition which corresponds to the locative. If that fails, `M-.`
  will try to find the definitions in the normal way, which may
  involve popping up an xref buffer and letting the user interactively
  select one of possible definitions.

  *Note that the this feature is implemented in terms of
  SWANK-BACKEND:FIND-SOURCE-LOCATION and
  SWANK-BACKEND:FIND-DEFINITIONS, whose support varies across the Lisp
  implementations. In particular, ABCL, CLISP, CMUCL and ECL have no
  or rather spotty support for it. Everything works fine on AllegroCL,
  CCL and SBCL.*

  In the following examples, pressing `M-.` when the cursor is on one
  of the characters of `FOO` or just after `FOO`, will visit the
  definition of function `FOO`:

      function foo
      foo function
      (function foo)
      (foo function)

  In particular, references in a DEFSECTION form are in (SYMBOL
  LOCATIVE) format so `M-.` will work just fine there.

  Just like vanilla `M-.`, this works in comments and docstrings. In
  the next example, pressing `M-.` on `FOO` will visit `FOO`'s default
  method:

  ```commonlisp
  ;;;; See FOO `(method () (t t t))` for how this all works.
  ;;;; But if the locative has semicolons inside: FOO `(method
  ;;;; () (t t t))`, then it won't, so be wary of line breaks
  ;;;; in comments.
  ```

  With a prefix argument (`C-u M-.`), one can enter a symbol plus a
  locative separated by whitespace to preselect one of the
  possibilities.

  The `M-.` extensions can be enabled by loading `src/pax.el`."
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

;;; Return one source location for the thing that can be located with
;;; NAME (a string) and LOCATIVE-STRING. Called from the elisp
;;; function slime-locate-definition. It's like LOCATE but takes
;;; string arguments and returns a location suitable for
;;; make-slime-xref.
(defun locate-definitions-for-emacs (name locative-string)
  (with-swank ()
    (let ((locative-string (string-trim " `" locative-string)))
      (swank-backend::converting-errors-to-error-location
        (swank::with-buffer-syntax ()
          (or
           ;; SECTION class and class SECTION
           ;; SECTION `class` and `class` SECTION
           ;; `SECTION` class and class `SECTION`
           ;; `SECTION` `class` and `class` `SECTION`
           (ignore-errors
            (locate-definition-for-emacs-1 name locative-string))
           ;; [SECTION][(class)] gets here as NAME="[SECTION][",
           ;; LOCATIVE-STRING="(class)".
           (ignore-errors
            (locate-definition-for-emacs-1 (string-trim "[]" name)
                                           locative-string))
           ;; [SECTION][class] gets here as NAME="[SECTION][class]",
           ;; LOCATIVE-STRING=garbage.
           (ignore-errors
            (locate-reference-link-definition-for-emacs name))
           ;; [title][section class] gets here as
           ;; NAME="[title][section", LOCATIVE-STRING="class]".
           (ignore-errors
            (locate-definition-for-emacs-1
             (subseq name (1+ (position #\[ name :from-end t)))
             (string-trim "[]" locative-string)))
           ;; [DEFSECTION][]
           (and (equal locative-string "")
                (locate-all-definitions-for-emacs name))))))))

(defun locate-all-definitions-for-emacs (name)
  (let* ((swank:*find-definitions-left-trim* "[#:<")
         (swank:*find-definitions-right-trim* "][,:.>sS"))
    (append
     ;; Standard stuff supported by the swank backend.
     (swank:find-definitions-for-emacs name)
     ;; For locatives not supported by swank above, we try locatives
     ;; on *LOCATIVE-SOURCE-SEARCH-LIST* one by one and see if they
     ;; lead somewhere from NAME.
     (multiple-value-bind (symbol found)
         (swank::find-definitions-find-symbol-or-package name)
       (when found
         (mapcan (lambda (locative)
                   (ignore-errors
                    (let ((thing (locate symbol locative :errorp nil)))
                      (when thing
                        (let ((location (find-source thing)))
                          (list (list (reference-to-dspec
                                       (canonical-reference thing))
                                      location)))))))
                 *locative-source-search-list*))))))

(defun reference-to-dspec (reference)
  (format nil "~S"
          (list (reference-object reference)
                (let ((locative (reference-locative reference)))
                  (if (and (listp locative) (= (length locative) 1))
                      (first locative)
                      locative)))))

;;; Handle references with quoted or non-quoted symbols and locatives.
;;; Since SECTION is both a class and and a documented symbol it
;;; serves as a good example.
(defun locate-definition-for-emacs-1 (name locative-string)
  (multiple-value-bind (symbol found)
      (swank::find-definitions-find-symbol-or-package name)
    (when found
      (let ((locative (read-marked-up-locative-from-string locative-string)))
        (when locative
          (let ((thing (locate symbol locative :errorp nil)))
            (when thing
              (let ((location (find-source thing)))
                (when location
                  ;; LOCATIVE-STRING acts as Slime's DSPEC.
                  (list (list locative-string location)))))))))))

(defun read-marked-up-locative-from-string (string)
  (let ((*read-eval* nil)
        (string (if (or (alexandria:starts-with #\` string)
                        (alexandria:starts-with #\' string))
                    (subseq string 1)
                    string)))
    (read-locative-from-string string)))

;;; Parse "LOCATIVE-TYPE" and "(LOCATIVE-TYPE ...)" like
;;; READ-FROM-STRING, but only intern stuff if LOCATIVE-TYPE is a
;;; valid locative.
(defun read-locative-from-string (string)
  (let ((swank::*buffer-package* *package*))
    (multiple-value-bind (symbol found)
        (with-swank ()
          (swank::find-definitions-find-symbol-or-package
           (string-trim *whitespace-chars* string)))
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
                    (ignore-errors (let ((*read-eval* nil))
                                     ;; FIXME: check that there is no
                                     ;; junk left.
                                     (read-from-string string))))))))))))

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
  (or (whitespacep char)
      (find char "()'`\"#<")))

(defun locate-reference-link-definition-for-emacs (string)
  (when (and (= 2 (count #\[ string))
             (= 2 (count #\] string)))
    (let ((first-open (position #\[ string))
          (first-close (position #\] string))
          (second-open (position #\[ string :from-end t))
          (second-close (position #\] string :from-end t)))
      (when (< first-open first-close second-open second-close)
        (locate-definition-for-emacs-1
         (string-trim "`" (subseq string (1+ first-open) first-close))
         (subseq string (1+ second-open) second-close))))))
