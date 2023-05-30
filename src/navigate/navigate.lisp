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
  See @EMACS-SETUP. In addition, the Elisp command
  `mgl-pax-edit-parent-section` visits the source location of the
  section containing the definition with `point` in it. See
  @DOCUMENTATION-KEY-BINDINGS."""
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
  (loop
    for object in (parse-word word)
      thereis (append
               ;; Standard stuff supported by Swank.
               (loop for dspec-and-location
                       in (swank-find-definitions object)
                     for edl = (emacsref-dspec-and-location
                                object dspec-and-location
                                ;; SWANK-BACKEND::FIND-DEFINITIONS'
                                ;; support for :MGL-PAX, PAX, "PAX" is
                                ;; uneven across implementations.
                                :filter '(package))
                     when edl
                       collect edl)
               ;; For locatives not supported by Swank, we try
               ;; locatives on *LOCATIVE-SOURCE-SEARCH-LIST* one by
               ;; one and see if they lead somewhere from the @NAMEs
               ;; in WORD.
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

(defun emacsref-dspec-and-location (object dspec-and-location &key filter)
  (destructuring-bind (dspec location) dspec-and-location
    (let ((reference (dspec-to-reference dspec object)))
      (unless (and reference
                   (member (reference-locative-type reference) filter))
        (list (if reference
                  (reference-for-emacs reference)
                  (ambiguous-reference-for-emacs
                   object))
              (if reference
                  ;; Replace (DEFCLASS FOO) with (FOO CLASS).
                  (reference-to-fake-dspec reference)
                  dspec)
              location)))))

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


;;;; Utilities for listing SECTIONs

;;; This is slow but fast enough not to bother with a SECTION-NAME to
;;; SECTION weak hash table.
(defun list-all-sections ()
  (let ((sections ()))
    (do-all-symbols (symbol sections)
      (when (boundp symbol)
        (let ((value (symbol-value symbol)))
          (when (and (typep value 'section)
                     ;; Filter out normal variables with SECTION values.
                     (eq (section-name value) symbol))
            (pushnew value sections)))))))

(defun sections-that-contain (sections reference)
  (remove-if-not (lambda (section)
                   (find-if (lambda (entry)
                              (and (typep entry 'reference)
                                   (reference= reference entry)))
                            (section-entries section)))
                 sections))

;;; As a heuristic to find the "home" section, move sections whose
;;; name is closer to the package of OBJECT to the front.
(defun sort-by-proximity (sections reference)
  (let ((object (reference-object reference)))
    (if (symbolp object)
        (let ((package-name (package-name (symbol-package object))))
          (sort (copy-list sections) #'>
                :key (lambda (section)
                       (or (mismatch package-name
                                     (package-name
                                      (symbol-package
                                       (section-name section))))
                           most-positive-fixnum))))
        sections)))

(defun find-parent-sections (reference)
  (let ((sections (sections-that-contain (list-all-sections) reference)))
    (sort-by-proximity sections reference)))


;;;; The Common Lisp side of mgl-pax-find-parent-section

(defun/autoloaded find-parent-section-for-emacs (buffer filename possibilities)
  (with-swank ()
    (swank::converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (let ((reference (find-current-definition buffer filename
                                                  possibilities)))
          (if (not reference)
              '(:error "Cannot determine current definition.")
              (let ((sections (find-parent-sections reference)))
                (if sections
                    (loop for section in sections
                          collect (dspec-and-source-location-for-emacs
                                   (canonical-reference section)))
                    `(:error ,(format nil "Cannot find parent section of ~S ~S."
                                      (reference-object reference)
                                      (reference-locative reference)))))))))))

(defun dspec-and-source-location-for-emacs (reference)
  (let ((location (ignore-errors (find-source reference))))
    (if (null location)
        `(,(prin1-to-string (reference-for-emacs reference))
          (:error "No source location."))
        `(,(prin1-to-string (reference-for-emacs reference))
          ,location))))

;;; This is also used by CURRENT-DEFINITION-PAX-URL-FOR-EMACS.
(defun find-current-definition (buffer filename possibilities)
  (loop for (name snippet pos) in possibilities
        for object = (ignore-errors (read-from-string name))
          thereis (and object (guess-current-definition
                               object buffer
                               filename snippet
                               pos))))

;;; Return the definition of OBJECT in BUFFER (a string) and FILE (a
;;; string or NIL) whose source location information from FIND-SOURCE
;;; matches SNIPPET or is otherwise closest to buffer positions POS
;;; (1-based indexing).
(defun guess-current-definition (object buffer file snippet pos)
  (flet ((snippets-match (loc-snippet)
           (or (null loc-snippet)
               ;; E.g. ASDF:SYSTEMs on SBCL
               (equal loc-snippet "")
               (< (2nd-whitespace-position snippet)
                  (or (mismatch loc-snippet snippet)
                      (1+ (length snippet)))))))
    ;; The following algorithm is heuristic.
    (let ((closest-definition nil)
          ;; Limit the chance of finding an unrelated definition just
          ;; because its @OBJECT is used as the first arg of some form
          ;; by not accepting position-based matches farther than 2000
          ;; characters from POS.
          (closest-pos 2000))
      (dolist (reference (definitions-of object))
        (let ((location (ignore-errors (find-source reference))))
          (if (eq (first location) :location)
              (let ((loc-file (location-file location))
                    (loc-buffer (location-buffer location))
                    (loc-pos (location-position location :adjustment 0))
                    (loc-snippet (location-snippet location)))
                (when (and
                       ;; The files must always match (even if NIL).
                       (equal file loc-file)
                       ;; The buffers must match, but LOC-BUFFER may be
                       ;; NIL (e.g. if the file wasn't compiled via
                       ;; Slime).
                       (or (null loc-buffer) (equal buffer loc-buffer))
                       ;; A match in LOCATION-SNIPPET is most
                       ;; trustworthy, but it's not always available.
                       (if loc-snippet
                           (snippets-match loc-snippet)
                           (<= (abs (- loc-pos pos))
                               (abs (- closest-pos pos)))))
                  (setq closest-definition reference
                        closest-pos loc-pos)))
              ;; No source location
              (when (reference-and-snippet-match-p reference snippet)
                (setq closest-definition reference
                      closest-pos pos)))))
      closest-definition)))

;;; Return all definitions of OBJECT in the running Lisp as a list of
;;; canonical REFERENCEs. REFERENCE-OBJECTs may not be the same as
;;; OBJECT (for example, when OBJECT is a package nickname).
(defun definitions-of (object)
  (remove-duplicates
   (append (when (or (symbolp object) (stringp object))
             (loop for dspec in (find-dspecs (if (stringp object)
                                                 (make-symbol object)
                                                 object))
                   for ref = (dspec-to-reference dspec object)
                   when (and ref
                             (not (and (keywordp object)
                                       (eq (reference-locative-type ref)
                                           'constant))))
                     collect (canonical-reference ref)))
           (mapcan (lambda (locative)
                     (let ((thing (locate object locative
                                          :errorp nil)))
                       (when thing
                         `(,(canonical-reference thing)))))
                   *locative-source-search-list*))
   :test #'reference=))

(defun 2nd-whitespace-position (string)
  (or (alexandria:when-let (pos (position-if #'whitespacep string))
        (position-if #'whitespacep string :start (1+ pos)))
      (length string)))

;;; This could use macroexpanded form instead of the snippet and a
;;; generic function specialized on the locative type, but since it's
;;; a fallback mechanism for the no-source-location case, that may be
;;; an overkill.
(defun reference-and-snippet-match-p (reference snippet)
  (let ((patterns (case (reference-locative-type reference)
                    ((variable) '("defvar" "defparameter"))
                    ((constant) '("defconstant" "define-constant"))
                    ((macro) '("defmacro"))
                    ((symbol-macro '("define-symbol-macro")))
                    ((compiler-macro '("define-compiler-macro")))
                    ((function) '("defun"))
                    ((generic-function) '("defgeneric"))
                    ;; Can't find :METHOD in DEFGENERIC.
                    ((method) '("defmethod"))
                    ((method-combination '("define-method-combination")))
                    ;; Can't find :READER, :WRITER, :ACCESSOR in DEFCLASS.
                    ;; Can't find STRUCTURE-ACCESSOR.
                    ((type) '("deftype"))
                    ((class) '("defclass"))
                    ((condition) '("define-condition"))
                    ((declaration) '("define-declaration"))
                    ((restart) '("define-restart"))
                    ((asdf:system) '("defsystem"))
                    ((package) '("defpackage" "define-package"))
                    ((readtable) '("defreadtable"))
                    ((section) '("defsection"))
                    ((glossary-term) '("define-glossary-term"))
                    ((locative) '("define-locative-type")))))
    (loop for pattern in patterns
            thereis (search pattern snippet :test #'char-equal))))
