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

;;; List Swank source locations (suitable for make-slime-xref) for the
;;; things that the Elisp side considers possible around the point
;;; when M-. is invoked. NAME-AND-LOCATIVES-LIST is a list like
;;;
;;; (("[section][" ("junk-before" "class"))
;;;  ("section" ("class")))
;;;
;;; where each element in the list is a consist of a NAME and a list
;;; of possible locatives found next to it. All strings may need to be
;;; trimmed of punctuation characters, but [ and ] are dealt with by
;;; providing two possiblities as in the above example.
;;;
;;; If none of the resulting references cannot be resolved (including
;;; if no locatives are specified), then list all possible
;;; definitions.
(defun locate-definitions-for-emacs (name-and-locatives-list)
  (with-swank ()
    (swank-backend::converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (stringify-dspec
         (or (loop for (name locative-strings) in name-and-locatives-list
                   append (loop for locative-string in locative-strings
                                append (ignore-errors
                                        (locate-definition-for-emacs-1
                                         name locative-string))))
             (loop for entry in name-and-locatives-list
                   append (ignore-errors
                           (locate-all-definitions-for-emacs
                            (first entry))))))))))

(defun stringify-dspec (dspec-and-location-list)
  (loop for entry in dspec-and-location-list
        collect `(,(prin1-to-string (first entry)) ,(second entry))))

(defun locate-all-definitions-for-emacs (name)
  ;; For locatives not supported by Swank, we try locatives on
  ;; *LOCATIVE-SOURCE-SEARCH-LIST* one by one and see if they lead
  ;; somewhere from NAME.
  (loop for object in (find-candidate-objects name)
        thereis (append
                ;; Standard stuff supported by Swank.
                (swank-backend:find-definitions object)
                (mapcan (lambda (locative)
                          (ignore-errors
                           (let ((thing (locate object locative :errorp nil)))
                             (when thing
                               (let ((location (find-source thing)))
                                 `((,(reference-to-dspec
                                      (canonical-reference thing))
                                    ,location)))))))
                        *locative-source-search-list*))))

(defun reference-to-dspec (reference)
  (list (reference-object reference) (reference-locative reference)))

(defun locate-definition-for-emacs-1 (name locative-string)
  (let ((locative (read-locative-from-markdown locative-string)))
    (when locative
      (loop for object in (find-candidate-objects name)
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
  (or (whitespacep char) (find char "()'`\"")))


;;; If CODIFYINGP, then try trimming trailing punctuation and
;;; stuff to find a candidate object. If not CODIFYINGP, then allow
;;; clhs lookup to match substrings of titles.


;;; If not, try trimming from the right with the character bag
;;; LEFT-TRIM, then trimming right with RIGHT-TRIM, finally try
;;; trimming left and right.
(defun find-candidate-objects (name &key clhs-substring-match)
  (let ((left-trim swank::*find-definitions-left-trim*)
        (right-trim swank::*find-definitions-right-trim*)
        (objects ()))
    (with-swank ()
      (swank::with-buffer-syntax (*package*)
        (flet ((find-it (name &optional untrimmed)
                 (when (and (plusp (length name))
                            ;; No point if nothing was trimmed.
                            (not (equalp name untrimmed)))
                   (multiple-value-bind (object found)
                       (candidate-object-p name clhs-substring-match)
                     (when found
                       (push object objects))))))
          (find-it name)
          (let* ((left-trimmed (string-left-trim left-trim name))
                 (right-trimmed (string-right-trim right-trim name))
                 (both-trimmed (string-right-trim left-trim left-trimmed)))
            (find-it left-trimmed name)
            (find-it right-trimmed name)
            (find-it both-trimmed left-trimmed)
            (dolist (depluralized (strip-plural both-trimmed))
              (find-it depluralized both-trimmed)))))
      objects)))

(defun strip-plural (string)
  ;; Mostly following https://www.grammarly.com/blog/plural-nouns/
  (let ((l (length string))
        (r ()))
    (labels ((suffixp (suffix)
               (alexandria:ends-with-subseq suffix string :test #'char-equal))
             (%desuffix (suffix)
               (subseq string 0 (- l (length suffix))))
             (desuffix (suffix)
               (when (suffixp suffix)
                 (push (%desuffix suffix) r)))
             (replace-suffix (old new)
               (when (suffixp old)
                 (push (format nil "~A~A" (%desuffix old) new) r))))
      ;; cars -> car
      (desuffix "s")
      ;; buses -> bus
      (desuffix "es")
      ;; analyses -> analysis
      (replace-suffix "es" "is")
      ;; cities -> city
      (replace-suffix "ies" "y")
      ;; gasses -> gas
      (desuffix "ses")
      ;; fezzes -> fez
      (desuffix "zes")
      ;; phenomena -> phenomenon, criteria -> criterion
      (replace-suffix "on" "a")
      ;; indices -> index
      (replace-suffix "ces" "x")
      ;; women -> woman
      (replace-suffix "men" "man")
      ;; wives -> wife
      (replace-suffix "ves" "fe")
      ;; elves -> elf
      (replace-suffix "ves" "f")
      (replace-suffix "children" "child")
      (replace-suffix "geese" "goose")
      (replace-suffix "teeth" "tooth")
      (replace-suffix "feet" "foot")
      (replace-suffix "mice" "mouse")
      (replace-suffix "people" "person")
      r)))

;;; See if NAME looks like a possible REFERENCE-OBJECT (e.g. it names
;;; an interned symbol, package, asdf system, or something in the
;;; CLHS).
;;;
;;; FIXME: This should be a generic function extensible by
;;; LOCATIVE-TYPE.
(defun candidate-object-p (name clhs-substring-match)
  (multiple-value-bind (symbol found) (swank::parse-symbol name)
    (cond (found
           (values symbol t))
          ((or (find-package* name)
               (asdf-system-name-p name)
               (find-hyperspec-id
                name :substring-match clhs-substring-match))
           (values name t)))))

(defun asdf-system-name-p (string)
  (declare (special *object-to-links*))
  (let ((ref (make-reference string 'asdf:system)))
    ;; KLUDGE: While generating documentation, we only consider
    ;; references to asdf systems being documented because
    ;; ASDF:FIND-SYSTEM, that's behind RESOLVE below, is very
    ;; expensive.
    (if (boundp '*object-to-links*)
        (not (null (find-link ref)))
        (resolve ref :errorp nil))))
