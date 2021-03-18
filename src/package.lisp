(mgl-pax-minimal:define-package :mgl-pax
  (:documentation "See MGL-PAX:@MGL-PAX-MANUAL.")
  (:use #:common-lisp #:mgl-pax-minimal)
  ;; These symbols are internal in the MGL-PAX-MINIMAL
  ;; but we want to reuse them in MGL-PAX:
  (:import-from #:mgl-pax-minimal
                #:entry-to-reference
                #:locative-equal
                #:reference-locative
                #:reference-locative-type
                #:reference-object
                #:reference=
                #:*discard-documentation-p*
                #:reader
                #:writer
                #:accessor)
  (:nicknames #:pax))
