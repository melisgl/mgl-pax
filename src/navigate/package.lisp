(in-package :mgl-pax)

;;; The MGL-PAX package is created in the MGL-PAX asdf:system, which
;;; does not depend on alexandria. The MGL-PAX/NAVIGATE system does
;;; depend on ALEXANDRIA, and we add the imports belatedly here.
(let ((imports '(alexandria:if-let alexandria:when-let alexandria:nth-value-or
                 alexandria:first-elt alexandria:last-elt
                 alexandria:starts-with alexandria:starts-with-subseq
                 alexandria:ends-with
                 alexandria:once-only alexandria:with-gensyms
                 alexandria:hash-table-keys
                 alexandria:rcurry)))
  (dolist (symbol imports)
    (import symbol)))
