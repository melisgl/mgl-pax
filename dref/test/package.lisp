(mgl-pax:define-package :dref-test
  (:use #:common-lisp #:dref #:dref-ext #:try)
  (:export #:test
           #:check-ref
           #:check-ref-sets
           #:check-source-location))
