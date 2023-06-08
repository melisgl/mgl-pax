(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defvar *navigate-loaded* nil)

(defun/autoloaded ensure-navigate-loaded ()
  (prog1 *navigate-loaded*
    (setq *navigate-loaded* t)))

;;; Return all definitions of OBJECT in the running Lisp as a list of
;;; canonical REFERENCEs. REFERENCE-OBJECTs may not be the same as
;;; OBJECT (for example, when OBJECT is a package nickname).
(defun definitions-of (object &key (allow-dspec t))
  (remove-duplicates
   (append (when (or (symbolp object) (stringp object))
             (loop for dspec in (find-dspecs (if (stringp object)
                                                 (make-symbol object)
                                                 object))
                   for ref = (dspec-to-reference dspec object)
                   for locative-type = (reference-locative-type ref)
                   when (and (or allow-dspec
                                 (not (eq locative-type 'dspec)))
                             ;; (:XXX CONSTANT) is trivial.
                             (not (and (keywordp object)
                                       (eq locative-type 'constant)))
                             ;; SWANK-BACKEND::FIND-DEFINITIONS'
                             ;; support for :MGL-PAX, PAX, "PAX" is
                             ;; uneven across implementations.
                             (not (eq locative-type 'package)))
                     collect (canonical-reference ref)))
           ;; For locatives not supported by Swank, we try locatives
           ;; on *LOCATIVE-SOURCE-SEARCH-LIST* one by one.
           (mapcan (lambda (locative)
                     (let ((thing (locate object locative :errorp nil)))
                       (when thing
                         `(,(canonical-reference thing)))))
                   *locative-source-search-list*))
   :test #'reference=))

;;; An acronym for Word-And-Locatives-List. This is what
;;; `mgl-pax-object-and-locatives-list-at-point' returns. It may look
;;; like this:
;;;
;;;     (("[section][" ("junk-before" "class"))
;;;      ("section" ("class")))
(deftype wall () 'list)

(defvar *definitions-of-fn*)

;;; List all definitions (as REFERENCEs) of WALL. Specify
;;; DEFINITIONS-OF (a function designator) to change how the
;;; no-locative case is handled.
(defun definitions-of-wall (wall &key (definitions-of 'definitions-of))
  (let ((*definitions-of-fn* definitions-of))
    (or
     ;; First, try with the given locatives.
     (loop for (word locative-strings) in wall
           append (loop for locative-string in locative-strings
                        append (definitions-of-word-with-locative
                                word locative-string)))
     ;; Then, fall back on the no locative case.
     (loop for entry in wall
           append (definitions-of-word (first entry))))))

(defun definitions-of-word-with-locative (word locative-string)
  (let ((locative (read-locative-from-markdown locative-string)))
    (when locative
      (loop for object in (parse-word word)
              thereis (alexandria:when-let
                          (ref (locate object locative :errorp nil))
                        (list (canonical-reference ref)))))))

(defun definitions-of-word (word)
  (loop for object in (parse-word word)
          thereis (funcall *definitions-of-fn* object)))


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
  @BROWSING-WITH-W3M."""
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
;;; when M-. is invoked.
;;;
;;; where each element in the list is a consist of a @WORD and a list
;;; of possible @LOCATIVEs found next to it. All strings may need to
;;; be trimmed of punctuation characters, but [ and ] are dealt with
;;; by providing two possiblities as in the above example.
;;;
;;; If none of the resulting references can be resolved (including if
;;; no locatives are specified), then list all possible definitions.
;;;
;;; Return a list of (DSPEC LOCATION) elements (with DSPEC as a
;;; string).
(defun/autoloaded locate-definitions-for-emacs (wall)
  (with-swank ()
    (swank/backend:converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (locate-definitions-for-emacs-1 wall)))))

(defun locate-definitions-for-emacs-1 (wall)
  (loop for definition in (definitions-of-wall wall)
        for location = (ignore-errors (find-source definition))
        unless (eq (first location) :error)
          collect `(,(prin1-to-string (reference-to-dspec definition))
                    ,location)))


;;;; Utilities for listing SECTIONs

(defvar *all-sections*)

;;; Lazily cache results of LIST-ALL-SECTIONS in BODY.
(defmacro with-all-sections-cached (() &body body)
  `(progv (unless (boundp '*all-sections*) '(*all-sections*))
       (unless (boundp '*all-sections*) '(:not-computed))
     ,@body))

;;; This is slow but fast enough not to bother with a SECTION-NAME to
;;; SECTION weak hash table. Some implementations (e.g. SBCL) have
;;; scaling issues with weak pointers.
(defun list-all-sections ()
  (if (boundp '*all-sections*)
      (if (eq *all-sections* :not-computed)
          (setq *all-sections* (list-all-sections-1))
          *all-sections*)
      (list-all-sections-1)))

(defun list-all-sections-1 ()
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

(defun find-parent-sections (object)
  (let ((reference (canonical-reference object)))
    (when reference
      (let ((sections (sections-that-contain (list-all-sections) reference)))
        (sort-by-proximity sections reference)))))

(defun find-root-section (object)
  (let ((sectionp (or (typep object 'section)
                      (and (typep object 'reference)
                           (typep (resolve object :errorp nil) 'section)))))
    (multiple-value-bind (section depth)
        (if sectionp
            (values object 0)
            (values (first (find-parent-sections object)) 1))
      (loop for parent = (first (find-parent-sections section))
            while parent
            do (setq section parent)
               (incf depth)
            finally (return (values section depth))))))


;;;; The Common Lisp side of mgl-pax-find-parent-section

(defun/autoloaded find-parent-section-for-emacs (buffer filename possibilities)
  (with-swank ()
    (swank/backend:converting-errors-to-error-location
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
    `(,(prin1-to-string (reference-to-dspec reference))
      ,(if (null location)
           '(:error "No source location.")
           location))))

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
