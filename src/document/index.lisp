(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @indexing (:title "Indexing")
  "PAX can create indices that cross-reference DREF::@DEFINITIONs and
  @CONCEPT-KEYs with where they are mentioned in the documentation."
  (@referrer glossary-term)
  (@referent glossary-term)
  (@indexing-definitions section)
  (@indexing-concepts section)
  (@configuring-indices section))

(defsection @indexing-definitions (:title "Indexing Definitions")
  """PAX generates indices with entries like this:

      foo [my-lib] (fn)
        ↩ d: My Lib Utilities
        ↩ f: bar, baz

  `MY-LIB:FOO` here is a function whose documentation has been [linked
  to][@linking] by the `My Lib Utilities` SECTION as well as the `BAR`
  and `BAZ` functions (grouped together under `f`). For example, BAR
  could be defined like this:

      (defun bar (x)
        "Extends FOO."
        (1+ (foo x)))

  In this example, `MY-LIB:FOO` is the @REFERENT and the others are
  the @REFERRERs. If there are no referrers or they are in the same
  group, then a single line is printed. Here is an example for the
  latter:

      foo [my-lib] (fn) ↩ f: bar, baz""")

(define-glossary-term @referrer (:title "referrer")
  "If the documentation of a DREF::@DEFINITION [links to][@linking]
  another definition, the former is considered a referrer to the
  latter. See @REFERENT.")

(define-glossary-term @referent (:title "referent")
  "A DREF::@DEFINITION is considered a referent of another definition
  if the latter's documentation [links to][@linking] it. See
  @REFERRER.")

(defsection @indexing-concepts (:title "Indexing Concepts")
  """Let's first DEFINE-CONCEPTs:

  ```cl-transcript
  (define-concept @albert-einstein (:title "Albert Einstein"
                                    :keys ("Einstein, Albert")))

  (define-concept @einstein-summation (:title "Einstein summation"
                                       :keys ("Einstein summation")))
  ```

  CONCEPTs are DREF::@DEFINITIONs, but in the generated documentation,
  [links][@linking] to them are [_hidden_][concept locative], and they
  are not meant to be documented themselves. They exist to endow their
  @REFERRERs with @CONCEPT-KEYs:

  ```cl-transcript (:dynenv pax-std-env)
  (defun einsum (x)
    "Implement @EINSTEIN-SUMMATION by @ALBERT-EINSTEIN."
    x)
  ```

  This makes `\EINSUM` a @REFERRER to the concept
  `\@EINSTEIN-SUMMATION`, and thus will be indexed under the concept
  keys of `\@EINSTEIN-SUMMATION`.

  DEFSECTION and DEFINE-GLOSSARY-TERM can associate concepts with the
  section or glossary term being defined:

  ```cl-transcript (:dynenv pax-std-env)
  (defsection @famous-people (:title "Famous People"
                              :concepts (@albert-einstein))
    "Let's start with one of the most famous, Albert Einstein.")
  ```

  This is like the mention in `\EINSUM`, but it also makes the section
  a _primary source_ of information. These are typeset more
  prominently in the generated documentation. If the section only
  mentions Einstein in passing, then don't use the :CONCEPTS argument,
  just mention `\@ALBERT-EINSTEIN` in a section docstring as `\EINSUM`
  does.

  Now, let's define a section to hold them together, configure a
  minimal index, and see what comes out:

  ```cl-transcript (:dynenv pax-std-env)
  (defsection @test (:title "Test" :export nil)
    (@famous-people section)
    (einsum function))

  (defun print-concept-index (documentable)
    (let* ((*document-index-formats* '(:plain))
           (*document-indices* '((:concepts t :title "Concept Index")))
           (output (document documentable :stream nil)))
      (princ (subseq output (search "## Concept Index" output)))))

  (print-concept-index @test)
  .. ## Concept Index
  ..
  .. - Einstein summation ↩ f: EINSUM
  ..
  .. - Einstein, Albert
  ..
  ..     - ↩ d: Famous People
  ..
  ..     - ↩ f: EINSUM
  ..
  ```

  This is okay, but let's group `summation` and `Albert` under
  `Einstein` by breaking the string keys into lists of strings:

  ```cl-transcript (:dynenv pax-std-env)
  (define-concept @albert-einstein (:title "Albert Einstein"
                                    :keys (("Einstein" "Albert"))))

  (define-concept @einstein-summation (:title "Einstein summation"
                                       :keys (("Einstein" "summation"))))

  (print-concept-index @test)
  .. ## Concept Index
  ..
  .. - Einstein
  ..
  ..     - Albert
  ..
  ..         - ↩ d: Famous People
  ..
  ..         - ↩ f: EINSUM
  ..
  ..     - summation ↩ f: EINSUM
  ..
  ```
  """"""
  See DEFINE-CONCEPT about _pure multiplexer concepts_ (those
  without :TITLEs) and how concepts can inherit the keys of other
  concepts.

  Also, see @CONCEPT-SUBKEY on how to order concepts in the output."""
  (define-concept macro)
  (@concept-key glossary-term)
  (@concept-subkey glossary-term))

(define-glossary-term @concept-key (:title "concept key")
  """A concept key identifies an entry in an index.
  It is a @CONCEPT-SUBKEY or a list of @CONCEPT-SUBKEYs. Concept keys
  occur in DEFINE-CONCEPT, DEFSECTION and DEFINE-GLOSSARY-TERM.

  - _Single subkey_: For example, the key `"Einstein, Albert"`
    consists of a single subkey. A single subkey is equivalent to a
    one-element list.

  - _List of subkeys_: When Einstein summation is also referenced, we
    can instead use the lists `("Einstein," "Albert")` and `("Einstein,"
    "summation")` to ensure that their @REFERRERs are grouped together
    under `"Einstein,"`.

      List concept keys can be arbitrarily long, although the
      @PDF-OUTPUT imposes a limit of 9.""")

(define-glossary-term @concept-subkey (:title "concept subkey")
  """A concept subkey (a part of a @CONCEPT-KEY) can be a string or a
  cons of strings. In the latter form, the CAR is for printing, and
  the CDR is for sorting. For example, use `("Éclair" . "Eclair")` to
  have `"Éclair"` printed but sorted as if it were `"Eclair"`. Sorting
  is done with plain STRING<, which is not Unicode-aware.

  Note that print names are ESCAPE-MARKDOWNed.""")

(defsection @configuring-indices (:title "Configuring Indices")
  (*document-index-sections* variable)
  (*document-index-formats* variable)
  (*document-indices* (variable "<value omitted>"))
  (*document-index-referent-locative-type-abbrevs* variable)
  (*document-index-referrer-groups* (variable "<value omitted>")))


;;;; Indices

(defvar/auto *document-indices*
  '((:title "Indices"
     :document-referrer-groups t
     :children ((:dtype (or section pseudo)
                 :index nil)
                (:dtype (or function macro compiler-macro)
                 :title "Function and Macro Index")
                (:dtype (and variable (not glossary-term))
                 :title "Variable and Constant Index")
                (:dtype type
                 :title "Type Index")
                (:dtype (not glossary-term)
                 :title "Misc Index")
                (:concepts t
                 :dtype glossary-term
                 :title "Concept Index"))))
  """A nested list of index specifications. Each spec is of the form

      (&key title documentation document-referrer-groups
            dtype concepts children)

  Non-`CONCEPT` @REFERENTs recorded during documentation generation
  are assigned to the first (in depth-first order) spec with a
  [matching][dtypep] `\DTYPE`. For @REFERENTs that are concepts, their
  keys are assigned to the first spec with :CONCEPTS true.

  Specs are processed in the following way and order:

  - If a spec and all its :CHILDREN are empty (nothing was assigned to
    them above), then they are not output.

  - Else, a dynamically generated SECTION with TITLE (a Markdown
    string) is entered.

  - If DOCUMENTATION is non-NIL, then it is a Markdown docstring and
    is written as is to the output.

  - If `DOCUMENT-REFERRER-GROUPS` is true, then the `\ABBREV`s and
    `\DOCUMENTATION`s in *DOCUMENT-INDEX-REFERRER-GROUPS* are listed
    in the output.

  - The @REFERENTs assigned to this spec are output along with their
    @REFERRERs (see the examples in @INDEXING-CONCEPTS).

  - The `CHILDREN` specs are processed.

  - The dynamically generated section is closed.

  The default value specifies separate indices for

  - functions and macros
  - variables
  - types
  - other things
  - CONCEPTs and GLOSSARY-TERMs.

  They are all grouped under an "Indices" section, that has
  :DOCUMENT-REFERRER-GROUPS.""")

(defun map-indices (fn &optional (indices *document-indices*) (depth 0))
  (dolist (index indices)
    (funcall fn index depth)
    (when-let (children (getf index :children))
      (map-indices fn children (1+ depth)))))

(defmacro do-indices ((index &optional (depth (gensym))) &body body)
  `(map-indices (lambda (,index ,depth)
                  (declare (ignorable ,depth))
                  ,@body)))

(defun find-index (dref)
  (do-indices (index depth)
    (when-let (dtype (getf index :dtype))
      (when (dtypep dref dtype)
        (return-from find-index index)))))

(defun find-index-for-key (key)
  (declare (ignore key))
  (do-indices (index depth)
    (when (getf index :concepts)
      (return-from find-index-for-key index))))


;;;; Index keys are @CONCEPT-KEYs are stringified definitions.

(declaim (inline make-index-subkey))
(defun make-index-subkey (name sort-as)
  (cons name sort-as))

(declaim (inline index-subkey-name))
(defun index-subkey-name (subkey)
  (if (consp subkey)
      (car subkey)
      subkey))

(declaim (inline index-subkey-sort-as))
(defun index-subkey-sort-as (subkey)
  (if (consp subkey)
      (cdr subkey)
      subkey))

(defun index-subkey-name-= (subkey1 subkey2)
  (string= (index-subkey-name subkey1)
           (index-subkey-name subkey2)))

(defun index-subkey-sorts-before-p (subkey1 subkey2)
  (string-lessp (index-subkey-sort-as subkey1)
                (index-subkey-sort-as subkey2)))

(defun index-subkey-name-< (subkey1 subkey2)
  (string< (index-subkey-name subkey1)
           (index-subkey-name subkey2)))

(defun index-key-sorts-before-p (key1 key2)
  (or (lexicographic< #'index-subkey-sorts-before-p key1 key2)
      ;; If the sort-as parts match, then fall back on sorting by
      ;; names to have something deterministic.
      (and (index-key-sort-the-same-p key1 key2)
           (lexicographic< #'index-subkey-name-< key1 key2))))

(defun index-key-sort-the-same-p (key1 key2)
  (and (= (length key1) (length key2))
       (loop for subkey1 in key1
             for subkey2 in key2
             always (string-equal (index-subkey-sort-as subkey1)
                                  (index-subkey-sort-as subkey2)))))

(defun non-list-cons-p (object)
  (and (consp object)
       (cdr object)
       (atom (cdr object))))

(defun escape-markdown-in-concept-key (key)
  (loop for subkey in key
        collect (cons (escape-markdown (index-subkey-name subkey))
                      (index-subkey-sort-as subkey))))


;;;; Generating indices

(defvar/auto *document-index-sections* :homeless-documentable
  "Controls what sections can get an index (if *DOCUMENT-INDEX-FORMATS*
  also allows it and they are non-empty).

  - NIL: No indices are generated.

  - :DOCUMENTABLE: Sections that appear in @DOCUMENTABLE (at any
    level) can get indices. Their subsections cannot.

  - :HOMELESS-DOCUMENTABLE: Sections that appear in @DOCUMENTABLE and
    have no HOME-SECTION can get indices.")

(defvar/auto *document-index-formats* '(:html :pdf)
  "The list of @OUTPUT-FORMATS for which index generation is allowed.")

(defun maybe-generate-indices (stream)
  (when (eq *section* *indexing-section*)
    (let ((index-to-indexables
            (group-indexables-per-index
             (hash-table-values *indexing-definitions*)
             (hash-table-keys *indexing-concept-key-to-referrers*))))
      (format stream "~&~%")
      (dolist (index *document-indices*)
        (maybe-generate-index index stream index-to-indexables)))))

(defun group-indexables-per-index (drefs concept-keys)
  (let ((grouping (make-hash-table)))
    (dolist (dref drefs)
      (when-let (index (find-index dref))
        (push dref (gethash index grouping))))
    (dolist (key concept-keys)
      (when-let (index (find-index-for-key key))
        (push key (gethash index grouping))))
    (let ((groups ()))
      (do-indices (index)
        (when (and (getf index :index t)
                   (gethash index grouping))
          (push (list index (gethash index grouping)) groups)))
      (nreverse groups))))

(defun maybe-generate-index (index stream index-to-indexables)
  (when (index-alive-p index index-to-indexables)
    ;; Index sections are not INDEXABLE-REFERRER-P although (and
    ;; because) they link to lots of stuff.
    ;;
    ;; They are also IN-CONTEXT-ONLY-P because the current
    ;; implementation depends on *TOP-LEVEL-SECTION-DEFINITIONS*
    ;; having been populated.
    (with-dynamic-section (stream :title (getf index :title)
                           :indexable-referrer nil :in-context-only t)
      (unless *first-pass*
        (when-let (documentation (getf index :documentation))
          (format stream "~A~%~%" documentation))
        (when (getf index :document-referrer-groups)
          (write-referrer-abbrevs stream)
          (terpri stream))
        (write-index (second (find index index-to-indexables :key #'first))
                     stream))
      (dolist (child (getf index :children))
        (maybe-generate-index child stream index-to-indexables)))))

(defun index-alive-p (index index-to-indexables)
  (or
   ;; FIXME: For the sake of the table of contents, we should know
   ;; already in the first pass whether INDEX is alive. For
   ;; non-concept indices, we only need *INDEXING-DEFINITIONS*, which
   ;; is populated in both passes. However, whether a concept index is
   ;; alive depends on the links made in docstrings, but docstrings
   ;; are not processed in the first pass (for performance reasons).
   ;; Thus, we always say "not alive" in the first pass, leading
   ;; mismatch between the toc and the reality of the second pass.
   (find index index-to-indexables :key #'first)
   (loop for child in (getf index :children)
           thereis (index-alive-p child index-to-indexables))))

(defun write-index (indexables stream)
  (let ((key-and-indexable-pairs ()))
    (dolist (indexable indexables)
      (let ((key (if (typep indexable 'dref)
                     (let ((dref indexable))
                       (list (dref-name-index-subkey dref nil)
                             (dref-index-referent-abbrev dref)))
                     (escape-markdown-in-concept-key indexable))))
        (push (cons key indexable) key-and-indexable-pairs)))
    (setq key-and-indexable-pairs
          (sort (coerce key-and-indexable-pairs 'vector)
                #'index-key-sorts-before-p :key #'car))
    (write-begin-index *format* stream)
    (let ((keys (map 'vector #'car key-and-indexable-pairs))
          (indexables (map 'vector #'cdr key-and-indexable-pairs)))
      ;; If there were duplicates (which RADIX-TREE discards), INDEX
      ;; below will get out of sync with KEYS. This cannot happen
      ;; because
      ;;
      ;; - DREF INDEXABLES are unique due to coming from the
      ;;   *INDEXING-DEFINITIONS* hash table;
      ;;
      ;; - concept key INDEXABLES come from CONCEPT-KEY-TO-REFERRERS,
      ;;   which uses a hash table keyed on concept key;
      ;;
      ;; - there can be no collisions between DREF names and concept
      ;;   keys because DREF-NAME-INDEX-SUBKEY start with #\[ and
      ;;   concept keys are ESCAPE-MARKDOWN-IN-CONCEPT-KEYed.
      (flet ((print-referrers* (key index depth)
               ;; Check that the radix tree is not out of sync with KEYS.
               (assert (equal key (aref keys index)))
               (print-referrers (aref indexables index) depth stream)))
        (print-index-key-tree (radix-tree keys :test #'index-subkey-name-=)
                              stream #'print-referrers*)))
    (write-end-index *format* stream)))

(defgeneric write-begin-index (format stream)
  (:method (format stream))
  (:method ((format (eql :html)) stream)
    (format stream "~% <div class=\"pax-index\">~%~%")))

(defgeneric write-end-index (format stream)
  (:method (format stream))
  (:method ((format (eql :html)) stream)
    (format stream "~% </div>~%")))

(defun dref-name-index-subkey (dref primaryp)
  (let ((title (document-definition-title dref :deemph t)))
    (cons (dref-name-index-subkey-name dref title (link-to-definition dref)
                                       primaryp)
          (dref-name-index-subkey-sort-as dref title))))

(defun dref-name-index-subkey-name (dref title target-id primaryp)
  (let ((*print-readably* nil)
        (name (dref-name dref)))
    (flet ((maybe-strong (string)
             (if primaryp
                 (md-strong string)
                 string)))
      (cond (title
             (format nil "[~A][~A]" (maybe-strong title) target-id))
            ((symbolp name)
             (format nil "[~A][~A]~@[ \\[~A\\]~]"
                     (maybe-strong
                      (maybe-downcase (md-code (symbol-name name))))
                     target-id
                     (unless (let ((package (symbol-package name)))
                               (or (eq package #.(find-package :cl))
                                   (eq package *package*)))
                       (maybe-downcase
                        (md-code (package-name (symbol-package name)))))))
            ((stringp name)
             (format nil "[~A][~A]" (maybe-strong (md-code name)) target-id))
            (t
             (format nil "[~A][~A]"
                     (maybe-strong (md-code (prin1-to-string name)))
                     target-id))))))

;;; Reproduce DREF-NAME-INDEX-SUBKEY-NAME's rendered, user-visible
;;; output. No downcasing is needed as sorting is case-insensitive.
(defun dref-name-index-subkey-sort-as (dref title)
  (let ((*print-readably* nil)
        (name (dref-name dref)))
    (cond (title)
          ((symbolp name)
           (format nil "~A~@[ [~A]~]" (sort-as-symbol-name name)
                   (unless (let ((package (symbol-package name)))
                             (or (eq package #.(find-package :cl))
                                 (eq package *package*)))
                     (package-name (symbol-package name)))))
          ((stringp name) name)
          (t
           (prin1-to-string name)))))

(defun sort-as-symbol-name (symbol)
  (let ((name (symbol-name symbol)))
    (string-downcase (subseq name (or (position-if #'alphanumericp name) 0)))))

(defun print-referrers (indexable depth stream)
  (let ((drefs* (if (typep indexable 'dref)
                    (dref-to-referrers indexable)
                    (concept-key-to-referrers indexable)))
        (*package* (maybe-dref-name-package indexable))
        (groups (make-hash-table :test #'equal)))
    (dolist (dref* drefs*)
      (multiple-value-bind (primaryp dref) (if (listp dref*)
                                               (values t (second dref*))
                                               (values nil dref*))
        (push (dref-name-index-subkey dref primaryp)
              (gethash (dref-index-referrer-abbrev dref) groups))))
    (let* ((abbrevs (sort (hash-table-keys groups)
                          #'index-subkey-sorts-before-p))
           (itemizep (cdr abbrevs)))
      (dolist (abbrev abbrevs)
        (when itemizep
          (format stream "~%~%")
          (md-indent depth stream)
          (write-char #\- stream))
        (format stream " ~A" (index-subkey-name abbrev))
        (loop for name in (sort (gethash abbrev groups)
                                #'index-subkey-sorts-before-p)
              for i upfrom 0
              do (unless (zerop i)
                   (write-char #\, stream))
                 (format stream " ~A" (index-subkey-name name)))))))

(defun maybe-dref-name-package (indexable)
  (if (and (typep indexable 'dref)
           (symbolp (dref-name indexable)))
      (symbol-package (dref-name indexable))
      *package*))

(defun print-index-key-tree (tree stream fn)
  (let ((i 0))
    (map-radix-tree (lambda (key-runs completep)
                      (let ((depth (1- (length key-runs))))
                        (when (< depth 1)
                          (assert (equal key-runs '(()))))
                        ;; Skip the empty root root
                        (when (plusp depth)
                          (md-indent (1- depth) stream)
                          (format stream "-~{ ~A~}"
                                  (mapcar #'index-subkey-name
                                          (first key-runs)))
                          (when completep
                            (let ((key (loop for run in (reverse key-runs)
                                             append run)))
                              (funcall fn key i depth)
                              (incf i)))
                          (format stream "~%~%"))))
                    tree :complete-only nil)))


(defvar/auto *document-index-referent-locative-type-abbrevs*
  '((generic-function "_(gf)_")
    (function "_(fn)_")
    (variable "_(var)_")
    (asdf:system "_(asdf:system)_"))
  "A list of `(LOCATIVE-TYPE ABBREV)` elements, where `ABBREV` (a
  Markdown string) is printed instead of the @REFERENT's locative when
  its DREF-LOCATIVE-TYPE is LOCATIVE-TYPE.

  The default behaviour is to bind *PACKAGE* to the package of the
  symbol naming the definition (if it's a symbol) and print the
  locative with *PRINT-CASE* :DOWNCASE, omitting the package of the
  locative type.")

(defun dref-index-referent-abbrev (dref)
  (or (second (find (dref-locative-type dref)
                    *document-index-referent-locative-type-abbrevs*
                    :key #'first))
      (let* ((locative-type (dref-locative-type dref))
             (locative-args (dref-locative-args dref))
             (*package* (maybe-dref-name-package dref))
             (*print-case* :downcase))
        (format nil "_(~A~{ ~S~})_" locative-type locative-args))))


(defvar/auto *document-index-referrer-groups*
  ;; These are by "namespace".
  '(((or function macro compiler-macro method) "↩ _f_:"
     :documentation "_f_: for definitions in the function namespace
(macros, compiler macros and also methods)")
    (type "↩ _t_:"
     :documentation "_t_: `DEFTYPE`s, classes, conditions, structs")
    ((or section glossary-term) "↩ _d_:"
     :documentation "_d_: documentation sections and glossary terms")
    ((or locative dtype) "↩ _l_:"
     :documentation "_l_: definitions of definition types")
    (asdf:system "↩ _s_:"
     :documentation "_s_: ASDF systems")
    (package "↩ _p_:"
     :documentation "_p_: packages")
    (readtable "↩ _n_:"
     :documentation "_n_: named readtables")
    (variable "↩ _v_:"
     :documentation "_v_: special variables and constants")
    (restart "↩ _r_:"
     :documentation "_r_: restarts")
    (t "↩ _?_:"
     :documentation "_?_: other"))
  """A list of `(DTYPE ABBREV &KEY DOCUMENTATION)` elements that defines
  the grouping of @REFERRERs of similar types in referrer lists. For
  example, the default value of this variable has this element:

      ((or function macro compiler-macro method) "↩ _f_:"
         :documentation "_f_: for definitions in the function namespace ...")

  Thus, in the following example, the definitions of `BAR` and `BAZ`
  are grouped together under `↩ f:` because they are both DTYPEP `(OR
  FUNCTION MACRO COMPILER-MACRO METHOD)`:

      foo [my-lib] (fn)
        ↩ d: My Lib Utilities
        ↩ f: bar, baz

  - In general, the first matching `\DTYPE` wins. See DREF::@DTYPES.

  - ABBREV may be a Markdown string or a cons of strings to control
    sort order in the manner of @CONCEPT-SUBKEYs.

  - `\DOCUMENTATION` is a Markdown string for
    :DOCUMENT-REFERRER-GROUPS in *DOCUMENT-INDICES*.""")

(defun write-referrer-abbrevs (stream)
  (format stream "Referrer definition type abbreviations:~%~%")
  (dolist (entry *document-index-referrer-groups*)
    (destructuring-bind (dtype abbrev &rest plist) entry
      (if-let (doc (getf plist :documentation))
        (format stream "- ~A~%" doc)
        (format stream "- ~A ~A~%" (md-code (prin1-to-string abbrev))
                (md-code (prin1-to-string dtype)))))))

(defun dref-index-referrer-abbrev (dref)
  (let ((abbrev (second (find dref *document-index-referrer-groups*
                              :key #'first :test #'dtypep))))
    (when abbrev
      (if (or (stringp abbrev) (consp abbrev))
          abbrev
          (funcall abbrev dref)))))
