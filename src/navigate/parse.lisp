(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @parsing (:title "Parsing" :export nil)
  (@word glossary-term)
  (@name glossary-term))

(define-glossary-term @word (:title "word")
  "A _word_ is a string from which we want to extract an @OBJECT. When
  [Navigating][@navigating-in-emacs section], the word is
  `slime-symbol-at-point`. When @GENERATING-DOCUMENTATION, it is a
  non-empty string between whitespace characters in a docstring.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *name-left-trim* "#<;\"")
  (defparameter *name-right-trim* ",;:.>\""))

(define-glossary-term @name (:title "name")
  #.(format nil """A _name_ is a string that names a possible @OBJECT
  (e.g. an INTERNed SYMBOL, a PACKAGE, or an ASDF:SYSTEM). Names are
  constructed from @WORDs by trimming some prefixes and suffixes. For
  a given word, multiple candidate names are considered in the
  following order.

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
;;;
;;; We trim only in CODIFY-UPPERCASE-WORD. Within markdown :CODE (see
;;; AUTOLINK) and :REFLINK :LABEL (see PARSE-REFLINK-LABEL-STRING), we
;;; only depluralize to catch the common mistake of writing "CLASSES"
;;; instead of "CLASSes".
(defun parse-word (word &key (trim t) (depluralize t) only-one
                          clhs-substring-match)
  (with-swank ()
    (swank::with-buffer-syntax (*package*)
      (let ((left-trim *name-left-trim*)
            (right-trim *name-right-trim*)
            (objects ())
            (names ()))
        (flet ((consider (object name)
                 (cond (only-one
                        (when (funcall only-one object name)
                          (return-from parse-word (values object name))))
                       (t
                        (push object objects)
                        (push name names)))))
          (flet ((find-it (name)
                   (when (plusp (length name))
                     (multiple-value-bind (object found)
                         (swank::parse-symbol name)
                       ;; FIXME: (SWANK::PARSE-SYMBOL "PAX:SECTION:")
                       ;; is broken. Only add if the _string_ read is
                       ;; shorter than the one we may already have for
                       ;; OBJECT in NAMES.
                       (when found
                         (consider object name)))
                     (when (or (find-package* name)
                               (locate (string-downcase name) 'asdf:system
                                       :errorp nil)
                               (find-hyperspec-id
                                name :substring-match clhs-substring-match))
                       (consider name name)))))
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
                  (alexandria:when-let (trimmed (trim-uppercase-core
                                                 both-trimmed))
                    (find-it trimmed)))
                (when depluralize
                  (dolist (depluralized (depluralize word))
                    (unless (string= depluralized word)
                      (find-it depluralized))))))
          (unless only-one
            (values (nreverse objects) (nreverse names))))))))

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
               (alexandria:ends-with-subseq suffix string :test #'char-equal))
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
