(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

;;; Used by [mgl-pax][asdf:system] dependency [autoload][asdf:system].
;;; So, let's keep this around for (AUTOLOAD:RECORD-LOADDEFS
;;; "mgl-pax").
(defsection @pax-locatives (:title "PAX Locatives")
  "In addition DRef's [own][dref::@basic-locative-types],
  PAX defines a few locative types using the facilities in described
  in @ADDING-NEW-LOCATIVES. [Locative][DREF::@LOCATIVE]s allow
  DREF::@REFERENCEing definitions, which is used in DEFSECTION,
  @NAVIGATING-IN-EMACS and docstrings (see @CODIFICATION and @LINKING
  in the context of @GENERATING-DOCUMENTATION)."
  (section locative)
  (glossary-term locative)
  (note locative)
  (dislocated locative)
  (argument locative)
  (include locative)
  (docstring locative)
  (go locative)
  (clhs locative))
