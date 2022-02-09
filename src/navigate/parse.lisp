(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @parsing (:title "Parsing" :export nil)
  (@word glossary-term)
  (@name glossary-term))

(define-glossary-term @word (:title "word")
  "A _word_ is a string from which we want to extract an @OBJECT. When
  [Navigating][@navigating-in-emacs section], the word is
  `slime-symbol-at-point`, when @GENERATING-DOCUMENTATION, it is a
  non-empty string between whitespace characters in a docstring.")

(define-glossary-term @name (:title "name")
  """A _name_ is a string that names an `INTERN`ed SYMBOL,
  a PACKAGE, or an ASDF:SYSTEM, that is, a possible @OBJECT. Names are
  constructed from @WORDs by possibly trimming leading and trailing
  punctuation symbols and removing certain plural suffixes.

  For example, in `"X and Y must be LISTs."`, although the word is
  `"LISTs."`, it gets trimmed to `"LISTs"`, then the plural suffix
  `"s"` is removed to get `"LIST"`. Out of the three candidates for
  names, `"LISTs."`, `"LISTs"`, and `"LIST"`, the ones that name
  interned symbols and such are retained for purposes for
  [Navigating][@navigating-in-emacs section] and
  @GENERATING-DOCUMENTATION.

  The punctuation characters for left and right trimming are `#<` and
  `,:.>`, respectively. The plural suffixes considered are `s`, `es`,
  `ses`, `zes`, and `ren` (all case insensitive).

  Thus `"CHILDREN"` and `"BUSES"` may have the names `"CHILD"` and
  `"BUS"` in them.
  """)

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

;;; Return a list of @OBJECTs and a list of @NAMEs naming them in
;;; WORD. If ONLY-ONE matches, then return only a single object and a
;;; single name.
(defun parse-word (word &key (trim t) (depluralize t) only-one
                   clhs-substring-match)
  (let ((left-trim "#<")
        (right-trim ",:.>")
        (objects ())
        (names ()))
    (with-swank ()
      (swank::with-buffer-syntax (*package*)
        (flet ((find-it (name)
                 (when (plusp (length name))
                   (multiple-value-bind (object found)
                       (namep name clhs-substring-match)
                     ;; FIXME: (SWANK::PARSE-SYMBOL "PAX:SECTION:") is
                     ;; broken. Only add if the _string_ read is
                     ;; shorter than the one we may already have for
                     ;; OBJECT in NAMES.
                     (when found
                       (cond (only-one
                              (when (funcall only-one object name)
                                (return-from parse-word (values object name))))
                             (t
                              (push object objects)
                              (push name names))))))))
          (find-it word)
          (if trim
              (let* ((left-trimmed (string-left-trim left-trim word))
                     (right-trimmed (string-right-trim right-trim word))
                     (both-trimmed (string-right-trim right-trim left-trimmed)))
                (unless (string= left-trimmed word)
                  (find-it left-trimmed))
                (unless (string= right-trimmed word)
                  (find-it right-trimmed))
                (unless (or (string= both-trimmed left-trimmed)
                            (string= both-trimmed right-trimmed))
                  (find-it both-trimmed))
                (when depluralize
                  (dolist (depluralized (strip-plural both-trimmed))
                    (unless (string= depluralized both-trimmed)
                      (find-it depluralized)))))
              (when depluralize
                (dolist (depluralized (strip-plural word))
                  (unless (string= depluralized word)
                    (find-it depluralized)))))))
      (unless only-one
        (values (nreverse objects) (nreverse names))))))

(defun strip-plural (string)
  ;; Mostly following https://www.grammarly.com/blog/plural-nouns/ but
  ;; keeping only the rules that remove suffixes (e.g. cities -> city
  ;; is not allowed).
  (let ((l (length string))
        (r ()))
    (labels ((suffixp (suffix)
               (alexandria:ends-with-subseq suffix string :test #'char-equal))
             (%desuffix (suffix)
               (subseq string 0 (- l (length suffix))))
             (desuffix (suffix)
               (when (suffixp suffix)
                 (push (%desuffix suffix) r))))
      ;; cars -> car
      (desuffix "s")
      ;; buses -> bus
      (desuffix "es")
      ;; gasses -> gas
      (desuffix "ses")
      ;; fezzes -> fez
      (desuffix "zes")
      (when (equalp string "children")
        (desuffix "ren"))
      r)))

;;; See if STRING looks like a possible REFERENCE-OBJECT (e.g. it
;;; names an interned symbol, package, asdf system, or something in
;;; the CLHS).
;;;
;;; FIXME: This should be a generic function extensible by
;;; LOCATIVE-TYPE.
(defun namep (string clhs-substring-match)
  (multiple-value-bind (symbol found) (swank::parse-symbol string)
    (cond (found
           (values symbol t))
          ((or (find-package* string)
               (asdf-system-name-p string)
               (find-hyperspec-id
                string :substring-match clhs-substring-match))
           (values string t)))))

(defun asdf-system-name-p (string)
  (let ((ref (make-reference string 'asdf:system)))
    ;; KLUDGE: While generating documentation, we only consider
    ;; references to asdf systems being documented because
    ;; ASDF:FIND-SYSTEM, that's behind RESOLVE below, is very
    ;; expensive.
    (if *objects-being-documented*
        (not (null (global-reference-p ref)))
        (resolve ref :errorp nil))))
