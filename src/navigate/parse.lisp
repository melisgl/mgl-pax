(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @parsing (:title "Parsing")
  "When @NAVIGATING-IN-EMACS or @GENERATING-DOCUMENTATION, references
  are parsed from the buffer content or docstrings, respectively. In
  either case, @NAMEs are extracted from @WORDs and then turned into
  [DRef names][DREF::@NAME] to form [DRef references]
  [DREF::@REFERENCE] maybe with locatives found next to the @WORD."
  (@word glossary-term)
  (@name glossary-term))

(define-glossary-term @word (:title "word")
  "A _word_ is a string from which we want to extract a @NAME. When
  [Navigating][@navigating-in-emacs section], the word is
  `slime-symbol-at-point`. When @GENERATING-DOCUMENTATION, it is a
  non-empty string between whitespace characters in a docstring.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *name-left-trim* "#<{;\"'`")
  (defparameter *name-right-trim* ",;:.>}\"'`"))

(define-glossary-term @name (:title "name")
  #.(format nil """A _name_ is a string that denotes a possible [DRef
   name] [DREF::@NAME] (e.g. an INTERNed SYMBOL, the name of a PACKAGE
  or an ASDF:SYSTEM). Names are constructed from @WORDs by trimming
  some prefixes and suffixes. For a given word, multiple candidate
  names are considered in the following order.

  1. The entire word.

  2. Trimming the characters `~A` from the left of the word.

  3. Trimming the characters `~A` from the right of the word.

  4. Trimming both of the previous two at the same time.

  5. From the result of 4., further removing some plural markers.

  6. From the result of 4., further removing non-uppercase prefixes
     and suffixes.

  For example, when `\M-.` is pressed while point is over
  `nonREADable.`, the last word of the sentence `It may be
  nonREADable.`, the following names are considered until one is found
  with a definition:

  1. The entire word, `"nonREADable."`.

  2. Trimming left does not produce a new word.

  3. Trimming right removes the dot and gives `"nonREADable"`.

  4. Trimming both is the same as trimming right.

  5. No plural markers are found.

  6. The lowercase prefix and suffix is removed around the uppercase
     core, giving `"READ"`. This has a definition, which `\M-.' will
     visit.

  The exact rules for steps 5. and 6. are the following.

  - [depluralize function][docstring]

  - [trim-uppercase-core function][docstring]
  """ *name-left-trim* *name-right-trim*))

;;; Return a list of DREF::@NAMEs and a list of @NAMEs naming them in
;;; WORD. If ONLY-ONE matches, then return only a single DREF::@NAME
;;; and a single @NAME.
;;;
;;; We trim only in CODIFY-UPPERCASE-WORD. Within markdown :CODE (see
;;; AUTOLINK) and :REFLINK :LABEL (see PARSE-REFLINK-LABEL-STRING), we
;;; only depluralize to catch the common mistake of writing "CLASSES"
;;; instead of "CLASSes".
(defun parse-word (word &key (trim t) (depluralize t)
                          clhs-substring-match only-one)
  (let ((left-trim *name-left-trim*)
        (right-trim *name-right-trim*)
        (xref-names ())
        (names ()))
    (flet ((consider (xref-name name)
             (cond (only-one
                    (when (funcall only-one xref-name name)
                      (return-from parse-word (values xref-name name))))
                   (t
                    (push xref-name xref-names)
                    (push name names)))))
      (flet ((find-it (name)
               (when (plusp (length name))
                 ;; Consider NAME as a symbol.
                 (multiple-value-bind (object found)
                     (swank::parse-symbol name)
                   ;; FIXME: (SWANK::PARSE-SYMBOL "PAX:SECTION:")
                   ;; is broken. Only add if the _string_ read is
                   ;; shorter than the one we may already have for
                   ;; OBJECT in NAMES.
                   (when found
                     (consider object name)))
                 ;; Consider NAME as a string.
                 (when (or (definitions name)
                           ;; CLHS is a pseudo locative. DEFINITIONS
                           ;; does not look for it. It's fine though
                           ;; because we need a hack for substring
                           ;; matching anyway.
                           (let ((*clhs-substring-match* clhs-substring-match))
                             (declare (special *clhs-substring-match*))
                             (dref name 'clhs nil)))
                   (consider name name))
                 ;; Consider NAME as a string, but adjust its case
                 ;; according to the readtable case. This handles
                 ;; [pax][package], for example.
                 (let ((adjusted (dref::adjust-string-case name)))
                   (when (and (string/= adjusted name)
                              (definitions adjusted))
                     (consider (dref::adjust-string-case name) name))))))
        (find-it word)
        (if trim
            (let* ((left-trimmed (string-left-trim left-trim word))
                   (right-trimmed (string-right-trim right-trim word))
                   (both-trimmed
                     (string-right-trim right-trim left-trimmed)))
              (unless (string= left-trimmed word)
                (find-it left-trimmed))
              (unless (string= right-trimmed word)
                (find-it right-trimmed))
              (unless (or (string= both-trimmed left-trimmed)
                          (string= both-trimmed right-trimmed))
                (find-it both-trimmed))
              (when depluralize
                (dolist (depluralized (depluralize both-trimmed))
                  (unless (string= depluralized both-trimmed)
                    (find-it depluralized))))
              (when-let (trimmed (trim-uppercase-core both-trimmed))
                (find-it trimmed)))
            (when depluralize
              (dolist (depluralized (depluralize word))
                (unless (string= depluralized word)
                  (find-it depluralized))))))
      (unless only-one
        (values (nreverse xref-names) (nreverse names))))))

(defun depluralize (string)
  "If a @WORD ends with what looks like a plural
  marker (case-insensitive), then a @NAME is created by removing it.
  For example, from the @WORD `BUSES` the plural marker `ES` is
  removed to produce the @NAME `BUS`. The list of plural markers
  considered is `S` (e.g. `CARS`), `ES` (e.g. `BUSES`), `SES` (e.g.
  `GASSES`), `ZES` (e.g. `FEZZES`), and `REN` (e.g. `CHILDREN`)."
  ;; Mostly following https://www.grammarly.com/blog/plural-nouns/ but
  ;; keeping only the rules that remove suffixes (e.g. cities -> city
  ;; is not allowed) because that would result in `CITY`s, which looks
  ;; bad.
  (let ((l (length string))
        (r ()))
    (labels ((suffixp (suffix)
               (ends-with-subseq suffix string :test #'char-equal))
             (%desuffix (suffix)
               (subseq string 0 (- l (length suffix))))
             (desuffix (suffix)
               (when (suffixp suffix)
                 (push (%desuffix suffix) r))))
      (desuffix "s")
      (desuffix "es")
      (desuffix "ses")
      (desuffix "zes")
      (desuffix "ren")
      r)))

(defun uppercase-core-bounds (string)
  (let* ((first-uppercase-pos (position-if #'upper-case-p string))
         (last-uppercase-pos (position-if #'upper-case-p string
                                          :from-end t)))
    (when (and first-uppercase-pos
               (if (= last-uppercase-pos first-uppercase-pos)
                   (notany #'lower-case-p string)
                   (not (find-if #'lower-case-p string
                                 :start (1+ first-uppercase-pos)
                                 :end last-uppercase-pos))))
      (values first-uppercase-pos (1+ last-uppercase-pos)))))

(defun trim-uppercase-core (string)
  "From a @CODIFIABLE @WORD, a @NAME is created by removing the prefix
  before the first and the suffix after the last uppercase character
  if they contain at least one lowercase character."
  (multiple-value-bind (uppercase-start uppercase-end)
      (uppercase-core-bounds string)
    (when uppercase-start
      ;; If there is no lowercase character before the first, then
      ;; don't trim anything. This prevents %FOO -> FOO.
      (unless (find-if #'lower-case-p string :end uppercase-start)
        (setq uppercase-start 0))
      (unless (find-if #'lower-case-p string :start uppercase-end)
        (setq uppercase-end nil))
      (when (or (plusp uppercase-start) uppercase-end)
        (subseq string uppercase-start uppercase-end)))))


;;; Read locative form a markdown docstring for DOCUMENT or from any
;;; Emacs buffer with strange stuff in it for M-..
(defun read-locative-from-noisy-string (string)
  (read-locative-from-string
   ;; It is assumed that names of locative types are not funny, and we
   ;; can trim aggressively.
   (string-left-trim *name-left-trim*
                     (string-right-trim *name-right-trim* string))))

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
                       (dref symbol 'locative nil))
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
                    (when (and found (dref symbol 'locative nil))
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
    (if (starts-with #\" string)
        (read-from-string string)
        (multiple-value-bind (symbol found)
            (swank::parse-symbol
             ;; Make "PAX:@PAX-MANUAL SECTION" work with a single
             ;; colon even though @PAX-MANUAL is an internal symbol.
             (double-single-colon string))
          (if found
              symbol
              (dref::adjust-string-case string))))))

(defun double-single-colon (string)
  (let ((pos (position #\: string)))
    (if (and pos (not (search "::" string)))
        (concatenate 'string (subseq string 0 pos) ":" (subseq string pos))
        string)))

(defun delimiterp (char)
  (or (whitespacep char) (find char "()'`\"")))
