;;;; Early definitions for DREF/FULL.

(in-package :dref)

(define-symbol-locative-type restart ()
  "A locative to refer to the definition of a restart defined by
  DEFINE-RESTART.")

(define-definer-for-symbol-locative-type define-restart restart
  "Associate a definition with the name of a restart, which must be a symbol.
  LAMBDA-LIST should be what calls like `(INVOKE-RESTART '<SYMBOL>
  ...)` must conform to, but this not enforced.

  PAX \"defines\" standard CL restarts such as USE-VALUE with
  DEFINE-RESTART:

  ```cl-transcript (:dynenv dref-std-env)
  (first-line (source-location-snippet
               (source-location (dref 'use-value 'restart))))
  => \"(define-restart use-value (value)\"
  ```

  Note that while there is a CL:RESTART class, its instances have no
  docstring or source location.")

(autoload make-source-location '#:dref/full)
(autoload source-location-p '#:dref/full)
(autoload source-location-file '#:dref/full)
(autoload source-location-file-position '#:dref/full)
(autoload source-location-buffer '#:dref/full)
(autoload source-location-buffer-position '#:dref/full)
(autoload source-location-snippet '#:dref/full)
(autoload source-location-adjusted-file-position '#:dref/full)

(autoload definitions '#:dref/full)
(autoload dref-apropos '#:dref/full)
