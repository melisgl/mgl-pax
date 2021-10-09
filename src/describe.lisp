(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defmethod describe-object ((section section) stream)
  "[SECTION][class] objects are printed by calling DOCUMENT on them
  with all @MGL-PAX-DOCUMENTATION-PRINTER-VARIABLES, except for
  *DOCUMENT-NORMALIZE-PACKAGES* turned off to reduce clutter. This
  method is only defined if MGL-PAX/FULL is loaded to allow non-fancy
  descriptions to be printed when using CL:DESCRIBE."
  (let ((*document-uppercase-is-code* nil)
        (*document-link-code* nil)
        (*document-link-sections* nil)
        (*document-mark-up-signatures* nil)
        (*document-max-numbering-level* 0)
        (*document-max-table-of-contents-level* 0)
        (*document-text-navigation* nil)
        ;; Some Lisps bind it to T in DESCRIBE, some don't.
        (*print-circle* nil))
    (document section :stream stream :format :markdown)))
