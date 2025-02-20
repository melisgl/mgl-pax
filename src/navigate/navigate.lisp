(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defvar *navigate-loaded* nil)

(defun/autoloaded ensure-navigate-loaded ()
  (prog1 *navigate-loaded*
    (setq *navigate-loaded* t)))


;;; An acronym for Word-And-Locatives-List. This is what
;;; `mgl-pax-wall-at-point' returns. It may look like this:
;;;
;;;     (("[section][" ("junk-before" "class"))
;;;      ("section" ("class")))
(deftype wall () 'list)

(defun definitions-of-wall (wall &key (definitions #'definitions))
  (delete-duplicates
   (or
    ;; First, try with the given locatives.
    (loop for (word locative-strings) in wall
          append (loop for locative-string in locative-strings
                       append (ensure-list (wal-dref word locative-string))))
    ;; Then, fall back on the no-locative case.
    (loop for entry in wall
          append (find-name definitions (first entry) :trim t :depluralize t)))
   :test #'xref=))

(defun wal-dref (word locative-string)
  (when-let (locative (parse-locative/noisy locative-string :junk-allowed t))
    (find-name (rcurry #'dref locative nil) word :trim t :depluralize t)))


(defsection @navigating-in-emacs (:title "Navigating Sources in Emacs")
  """Integration into @SLIME's @M-. (`slime-edit-definition`) allows
  one to visit the SOURCE-LOCATION of a [definition][DREF].

  The definition is either determined from the buffer content at point
  or is prompted. If prompted, then the format is `<NAME> <LOCATIVE>`,
  where the locative may be omitted to recover stock Slime behaviour.

  When determining the definition from the buffer contents,
  `(slime-symbol-at-point)` is parsed as a @WORD, then candidate
  locatives are looked for before and after that word. Thus, if a
  locative is the previous or the next expression around the symbol of
  interest, then `\\M-.` will go straight to the definition which
  corresponds to the locative. If that fails, `\\M-.` will try to find
  the definitions in the normal way, which may involve popping up an
  xref buffer and letting the user interactively select one of
  possible definitions. For more details, see @PARSING.

  In the following examples, when the cursor is on one of the
  characters of `FOO` or just after `FOO`, pressing `\\M-.` will visit
  the definition of function `FOO`:

      function foo
      foo function
      (function foo)
      (foo function)

  In particular, DREF::@REFERENCEs in a DEFSECTION form are in (NAME
  LOCATIVE) format so `\\M-.` will work just fine there. `\\M-.` also
  recognizes the `[foo][function]` and similar forms of @LINKING in
  docstrings.

  Just like vanilla `\\M-.`, this works in comments and docstrings. In
  the next example, pressing `\\M-.` on `FOO` will visit `FOO`'s
  default method:

  ```
  ;; See RESOLVE* (method () (dref)) for how this all works.
  ```

  With a prefix argument (`C-u M-.`), one can enter a symbol plus a
  locative separated by whitespace to preselect one of the
  possibilities.

  The `\\M-.` extensions can be enabled by loading `src/mgl-pax.el`.
  See @EMACS-SETUP. In addition, the Elisp command
  `mgl-pax-edit-parent-section` visits the source location of the
  section containing the definition with `point` in it. See
  @BROWSING-LIVE-DOCUMENTATION."""
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
;;; when M-. is invoked. The return value is a list of (DSPEC
;;; LOCATION) elements (with DSPEC as a string).
;;;
;;; If none of the resulting references can be resolved (including if
;;; no locatives are specified), then list all possible DEFINITIONS.
;;;
;;; Each element in the list WALL consists of a @WORD and a list of
;;; possible DREF::@LOCATIVEs found next to it in the Emacs buffer.
(defun/autoloaded locate-definitions-for-emacs (wall)
  (with-swank ()
    (swank/backend:converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (locate-definitions-for-emacs-1 wall)))))

(defun locate-definitions-for-emacs-1 (wall)
  (loop for definition in (definitions-of-wall wall)
        for location = (source-location definition)
        collect `(,(prin1-to-string (dref::definition-to-dspec definition))
                  ,location)))


;;;; The Common Lisp side of `mgl-pax-find-parent-section'

(defun/autoloaded find-parent-section-for-emacs (buffer filename possibilities)
  (with-swank ()
    (swank/backend:converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (let ((dref (find-current-definition buffer filename possibilities)))
          (if (null dref)
              '(:error "Cannot determine current definition.")
              (let ((sections (find-parent-sections dref)))
                (if sections
                    (loop for section in sections
                          collect (dspec-and-source-location-for-emacs
                                   (locate section)))
                    `(:error ,(format nil "Cannot find parent section of ~S ~S."
                                      (dref-name dref)
                                      (dref-locative dref)))))))))))

(defun dspec-and-source-location-for-emacs (dref)
  (let ((location (source-location dref)))
    `(,(prin1-to-string (dref::definition-to-dspec dref))
      ,(if (null location)
           '(:error "No source location.")
           location))))

;;; This is also used by CURRENT-DEFINITION-PAX-URL-FOR-EMACS.
(defun find-current-definition (buffer filename possibilities)
  (loop for (name snippet pos) in possibilities
        for object = (ignore-errors (read-name-without-interning name))
          thereis (and object (guess-current-definition
                               object buffer
                               filename snippet
                               pos))))

(defun read-name-without-interning (string)
  (let ((string (trim-whitespace string)))
    (if (or (starts-with-subseq "#:" string)
            (starts-with #\" string))
        (read-from-string string)
        (read-interned-symbol-from-string string))))

;;; Return the definition of OBJECT in BUFFER (a string) and FILE (a
;;; string or NIL) whose source location information from SOURCE-LOCATION
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
          ;; because its @NAME is used as the first arg of some form
          ;; by not accepting position-based matches farther than 2000
          ;; characters from POS.
          (closest-pos 2000))
      (dolist (dref
               ;; It is likely that only LISP-LOCATIVE-TYPES have
               ;; source location (except UNKNOWN).
               (definitions object))
        (let ((location (source-location dref)))
          (if (source-location-p location)
              (let ((loc-file (source-location-file location))
                    (loc-buffer (source-location-buffer location))
                    (loc-pos (source-location-buffer-position location))
                    (loc-snippet (source-location-snippet location)))
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
                  (setq closest-definition dref
                        closest-pos loc-pos)))
              ;; No source location
              (when (reference-and-snippet-match-p dref snippet)
                (setq closest-definition dref
                      closest-pos pos)))))
      closest-definition)))

(defun 2nd-whitespace-position (string)
  (or (when-let (pos (position-if #'whitespacep string))
        (position-if #'whitespacep string :start (1+ pos)))
      (length string)))

;;; This could use the macroexpanded form instead of the snippet and a
;;; generic function specialized on the locative type, but since it's
;;; a fallback mechanism for the no-source-location case, that may be
;;; an overkill.
(defun reference-and-snippet-match-p (ref snippet)
  (let ((patterns (case (xref-locative-type ref)
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
