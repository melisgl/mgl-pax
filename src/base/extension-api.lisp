(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @extension-api (:title "Writing Extensions")
  (@adding-new-locatives section)
  (@locative-aliases section)
  (@extending-document section)
  (@sections section)
  (@glossary-terms section))


(defsection @adding-new-locatives (:title "Adding New Locatives")
  "Once everything in DREF-EXT::@ADDING-NEW-LOCATIVES has been done,
  there are only a couple of PAX generic functions left to extend."
  (document-dref generic-function)
  (exportable-reference-p generic-function)
  (exportable-locative-type-p generic-function)
  "Also note that due to the @HOME-SECTION logic, especially for
  locative types with string names, DREF-EXT:DREF-DOCSTRING should
  probably return a non-NIL package.")

;;; This gets clobbered with an empty function when MGL-PAX/NAVIGATE
;;; is loaded.
(autoload ensure-navigate-loaded '#:mgl-pax/navigate)

(defgeneric document-dref (dref stream)
  (:documentation "Write DREF in *FORMAT* to STREAM.
  Add methods specializing on a subclass of DREF to customize the
  output of DOCUMENT. This function is for extension only. Don't call
  it directly."))


(defsection @locative-aliases (:title "Locative Aliases")
  """DEFINE-LOCATIVE-ALIAS can be used to help [`\\M-.`][
  @navigating-in-emacs] and [autolinking][ @explicit-and-autolinking]
  disambiguate references based on the context of a @NAME as described
  on @PARSING and also in @SPECIFIED-LOCATIVE.

  The following example shows how to make docstrings read
  more naturally by defining an alias.

  ```
  (defclass my-string ()
    ())

  (defgeneric my-string (obj)
    (:documentation "Convert OBJ to MY-STRING."))

  ;;; This version of FOO has a harder to read docstring because
  ;;; it needs to disambiguate the MY-STRING reference.
  (defun foo (x)
    "FOO takes and argument X, a [MY-STRING][class] object.")

  ;;; Define OBJECT as an alias for the CLASS locative.
  (define-locative-alias object class)

  ;;; Note how no explicit link is needed anymore.
  (defun foo (x)
    "FOO takes an argument X, a MY-CLASS object.")
  ```

  Similary, defining the indefinite articles as aliases of the CLASS
  locative can reduce the need for explicit linking.

  ```
  (define-locative-alias a class)
  (define-locative-alias an class)
  ```

  Since these are unlikely to be universally helpful, make sure not to
  export the symbols `A` and `AN`.""")


(defsection @sections (:title "Sections")
  "[SECTION][class] objects rarely need to be dissected since
  DEFSECTION and DOCUMENT cover most needs. However, it is plausible
  that one wants to subclass them and maybe redefine how they are
  presented."
  (section class)
  (section-name (reader section))
  (section-package (reader section))
  (section-readtable (reader section))
  (section-title (reader section))
  (section-link-title-to function)
  (section-entries function))


(defsection @glossary-terms (:title "Glossary Terms")
  "[GLOSSARY-TERM][class] objects rarely need to be dissected since
  DEFINE-GLOSSARY-TERM and DOCUMENT cover most needs. However, it is
  plausible that one wants to subclass them and maybe redefine how
  they are presented."
  (glossary-term class)
  (glossary-term-name (reader glossary-term))
  (glossary-term-title (reader glossary-term))
  (glossary-term-url (reader glossary-term)))
