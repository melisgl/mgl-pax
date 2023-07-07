;;;; Early definitions for DREF/FULL.

(in-package :dref)

(define-symbol-locative-type restart ()
  "A locative to refer to the definition of a restart defined by
  DEFINE-RESTART.")

(define-definer-for-symbol-locative-type define-restart restart
  "Associate a definition with the name of a restart, which must be a symbol.
  CL restarts need no definition for INVOKE-RESTART to work.
  DEFINE-RESTART offers a place to hang the DOCSTRING on and also
  provides SOURCE-LOCATION.

  PAX 'defines' standard CL restarts such as USE-VALUE with
  DEFINE-RESTART:

  ```cl-transcript (:dynenv dref-std-env)
  (first-line (source-location-snippet
               (source-location (locate 'use-value 'restart))))
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
(autoload source-location-file-position-offset '#:dref/full)

(autoload definitions '#:dref/full)
(autoload dref-apropos '#:dref/full)
