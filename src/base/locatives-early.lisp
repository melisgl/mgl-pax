(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @variablelike-locatives (:title "Locatives for Variables")
  (variable locative)
  (constant locative))

(defsection @macrolike-locatives (:title "Locatives for Macros")
  (macro locative)
  (symbol-macro locative)
  (compiler-macro locative))

(defsection @functionlike-locatives (:title "Locatives for Functions")
  (function locative)
  (generic-function locative)
  (method locative)
  (method-combination locative)
  (accessor locative)
  (reader locative)
  (writer locative)
  (structure-accessor locative))

(defsection @typelike-locatives (:title "Locatives for Types and Declarations")
  (type locative)
  (class locative)
  (declaration locative))

(defsection @condition-system-locatives (:title "Condition System Locatives")
  (condition locative)
  (restart locative)
  (define-restart macro))

(defsection @packagelike-locatives
    (:title "Locatives for Packages and Readtables")
  (asdf:system locative)
  (package locative)
  (readtable locative))

(defsection @pax-locatives (:title "Locatives for PAX Constructs")
  (section locative)
  (glossary-term locative)
  (define-glossary-term macro)
  (locative locative)
  (dislocated locative)
  (argument locative)
  (include locative)
  (docstring locative))

(defsection @external-locatives (:title "External Locatives")
  (clhs locative))

(define-definer-for-symbol-locative-type define-restart restart
  """A definer macro to hang the documentation of a restart on a
  symbol.

  ```
  (define-restart my-ignore-error ()
    "Available when MY-ERROR is signalled, MY-IGNORE-ERROR unsafely continues.")
  ```

  Then `(MY-IGNORE-ERROR RESTART)` refers to the above definition.
  Note that while there is a CL:RESTART type, there is no
  corresponding source location or docstring like for
  [CONDITION][condition]s.
  """)


;;;; These must be available even if only the base mgl-pax system is
;;;; loaded.

(defvar *symbol-macro-docstrings* (make-hash-table))

(defmethod documentation ((symbol symbol) (doc-type (eql 'symbol-macro)))
  (gethash symbol *symbol-macro-docstrings*))

(defmethod (setf documentation) (docstring (symbol symbol)
                                 (doc-type (eql 'symbol-macro)))
  (setf (gethash symbol *symbol-macro-docstrings*) docstring))
