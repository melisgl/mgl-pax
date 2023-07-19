;;;; Normally, the DEFSECTION forms, which do the actual exporting,
;;;; would live close to the definitions they refer to. However, there
;;;; are self-imposed requirements:
;;;;
;;;; - We want to export some symbols from the DREF and some from
;;;;   DREF-EXT package.
;;;;
;;;; - We want the home packages of those symbols to be the package
;;;;   from which they are exported.
;;;;
;;;; - Some of the definitions for both packages are autoloaded via
;;;;   the DREF/FULL ASDF:SYSTEM.
;;;;
;;;; One option to fulfill these requirements would be to :EXPORT from
;;;; the DEFINE-PACKAGE forms below, but 1. that would duplicate
;;;; information in the DEFSECTION forms, 2. the documentation
;;;; wouldn't be available when DREF/FULL is not yet loaded. On the
;;;; other hand, 3. DEFSECTIONs could live near the code, where they
;;;; belong.
;;;;
;;;; So instead, the SECTIONs for DREF/FULL are defined in this file
;;;; for both packages, addressing 1. and 2 at the cost of 3.

(mgl-pax:define-package :dref-ext
  (:documentation "See DREF-EXT::@EXTENDING-DREF.")
  (:use #:common-lisp
        #:mgl-pax #:named-readtables #:pythonic-string-reader))

(mgl-pax:define-package :dref
  (:documentation "See DREF::@DREF-MANUAL.")
  (:use #:common-lisp #:dref-ext
        #:mgl-pax #:named-readtables #:pythonic-string-reader))


;;;; Foreshadowing of what the DREF/FULL system defines in the
;;;; DREF-EXT package.
;;;;
;;;; We want the [home package][clhs] of e.g. DREF-EXT:RESOLVE* to be
;;;; DREF-EXT, so we first export everything from DREF-EXT, which is
;;;; :USEd by DREF.

(in-package :dref-ext)

(defun dref-std-env (fn)
  (let ((*package* (find-package :dref)))
    ;; FIXME: Add all others too.
    (progv '(pax::*document-downcase-uppercase-code*
             pax::*transcribe-check-consistency*)
        '(nil #+sbcl t #-sbcl nil)
      (handler-bind ((warning #'muffle-warning))
        (unwind-protect
             (funcall fn)
          (unintern '*my-var* (find-package :dref)))))))

(in-readtable pythonic-string-syntax)

(defsection @extending-dref (:title "Extending DRef"
                             :package :dref
                             :export :dref-ext)
  (@references section)
  (@adding-new-locatives section)
  (@symbol-locatives section)
  (@dref-subclasses section)
  (@source-locations section))

(defsection @references (:title "References"
                         :package :dref
                         :export :dref-ext)
  (xref-name (reader dref::xref))
  (xref-locative (reader dref::xref))
  (dref-name (reader dref::dref))
  (dref-locative (reader dref::dref))
  (dref-origin (reader dref::dref))
  (locative-type function)
  (locative-args function)
  "The following convenience functions are compositions of
  {`LOCATIVE-TYPE`, `LOCATIVE-ARGS`} and {`XREF-LOCATIVE`,
  `DREF-LOCATIVE`}."
  (xref-locative-type function)
  (xref-locative-args function)
  (dref-locative-type function)
  (dref-locative-args function))

(defsection @adding-new-locatives (:export :dref-ext)
  (define-locative-type macro)
  (define-pseudo-locative-type macro)
  (check-locative-args macro)
  (define-locative-alias macro)
  (define-definition-class macro)
  (locate-error function)
  (locate* generic-function)
  (dref* generic-function)
  (add-dref-actualizer function)
  (remove-dref-actualizer function)
  (resolve* generic-function)
  (arglist* generic-function)
  (docstring* generic-function)
  (source-location* generic-function))

(defsection @symbol-locatives (:title "Symbol Locatives"
                               :package :dref
                               :export :dref-ext)
  "Let's see how the opaque DEFINE-SYMBOL-LOCATIVE-TYPE and the
  obscure DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE macros work together
  to simplify the common task of associating definition with a symbol
  in a certain context."
  (define-symbol-locative-type macro)
  (define-definer-for-symbol-locative-type macro))

(defsection @dref-subclasses (:title "DREF Subclasses"
                              :package :dref
                              :export :dref-ext)
  "These are the [DREF][class] subclasses corresponding to
  DREF::@LOCATIVE-TYPES. They are exported to make it possible to go
  beyond the standard @OPERATIONS (e.g. PAX:DOCUMENT-OBJECT*) and for
  subclassing."
  "**for Variables**"
  (variable-dref class)
  (constant-dref class)
  "**for Macros**"
  (macro-dref class)
  (symbol-macro-dref class)
  (compiler-macro-dref class)
  (setf-dref class)
  "**for Functions**"
  (function-dref class)
  (generic-function-dref class)
  (method-dref class)
  (method-combination-dref class)
  (accessor-dref class)
  (reader-dref class)
  (writer-dref class)
  (structure-accessor-dref class)
  "**for Types and Declarations**"
  (type-dref class)
  (class-dref class)
  (declaration-dref class)
  "**for the Condition System**"
  (condition-dref class)
  (restart-dref class)
  "**for Packages and Readtables**"
  (asdf-system-dref class)
  (package-dref class)
  (readtable-dref class)
  "**for DRef Locatives**"
  (locative-dref class)
  (symbol-locative-dref class)
  (unknown-dref class)
  (lambda-dref class))

(defsection @source-locations (:title "Source Locations"
                               :package :dref
                               :export :dref-ext)
  "These represent the file or buffer position of a [defining
  form][clhs] and are returned by the SOURCE-LOCATION function. For
  the details, see the Elisp function `slime-goto-source-location`."
  (make-source-location function)
  (source-location-p function)
  (source-location-file function)
  (source-location-file-position function)
  (source-location-buffer function)
  (source-location-buffer-position function)
  (source-location-snippet function)
  (source-location-adjusted-file-position function))


;;;; Foreshadowing of what the DREF/FULL system defines in the DREF
;;;; package.

(in-package :dref)

(import 'dref-ext::dref-std-env)

(in-readtable pythonic-string-syntax)

(defsection @locative-types (:title "Locative Types")
  """The following are the @LOCATIVE-TYPEs supported out of the
  box. As all locative types, they are named by symbols. When there is
  a CL type corresponding to the reference's locative type, the
  references can be RESOLVEd to a unique object as is the case in

  ```cl-transcript (:dynenv dref-std-env)
  (resolve (dref 'print 'function))
  ==> #<FUNCTION PRINT>
  ```

  Even if there is no such CL type, the ARGLIST, the DOCSTRING, and
  the SOURCE-LOCATION of the defining form is usually recorded unless
  otherwise noted."""
  (@variablelike-locatives section)
  (@macrolike-locatives section)
  (@functionlike-locatives section)
  (@typelike-locatives section)
  (@condition-system-locatives section)
  (@packagelike-locatives section)
  (@dref-locatives section))

(defsection @variablelike-locatives (:title "Locatives for Variables")
  (variable locative)
  (constant locative))

(defsection @macrolike-locatives (:title "Locatives for Macros")
  (macro locative)
  (symbol-macro locative)
  (compiler-macro locative)
  (setf locative))

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

(defsection @condition-system-locatives
    (:title "Locatives for the Condition System")
  (condition locative)
  (restart locative)
  (define-restart macro))

(defsection @packagelike-locatives
    (:title "Locatives for Packages and Readtables")
  (asdf:system locative)
  (package locative)
  (readtable locative))

(defsection @dref-locatives (:title "DRef Locatives")
  (locative locative)
  (unknown locative)
  (lambda locative))
