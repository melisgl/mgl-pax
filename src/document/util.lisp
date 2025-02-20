(in-package :mgl-pax)

;;;; Cached DREF:DEFINITIONS with all LOCATIVE-TYPES.

(defvar *definitions-cache*)

(defmacro with-definitions-cached (&body body)
  `(let ((*definitions-cache* (make-hash-table :test 'equal))
         (*non-local-locative-types* (set-difference (locative-types)
                                                     *local-locative-types*)))
     ,@body))

(defparameter *local-locative-types* '(argument dislocated))

(defvar *non-local-locative-types*)

(defun definitions* (name)
  (if (boundp '*definitions-cache*)
      (multiple-value-bind (drefs presentp) (gethash name *definitions-cache*)
        (unless presentp
          (setq drefs (definitions name
                                   :locative-types *non-local-locative-types*))
          (setf (gethash name *definitions-cache*) drefs))
        ;; @LOCAL-DEFINITIONs are not cacheable.
        (append drefs (definitions name
                                   :locative-types *local-locative-types*)))
      (definitions name :locative-types (locative-types))))


;;;; Like the DREF function and DEFINITIONS*, but for references and
;;;; name given as strings, respectively.

;;; Parse "NAME LOCATIVE-TYPE" or "NAME (LOCATIVE-TYPE ...)", but only
;;; intern stuff if LOCATIVE-TYPE is interned. Return:
;;;
;;; 1. the corresponding DREF if found,
;;  2. the locative if a valid one was found,
;;; 3. the invalid locative as a string or nil.
(defun parse-dref (string)
  (flet ((maybe-junk (start)
           (let ((locstring (trim-whitespace (subseq string start))))
             (if (zerop (length locstring))
                 nil
                 locstring))))
    (handler-case
        ;; Skip whatever NAME may be ...
        (let* ((name-end-pos (n-chars-would-read string))
               (raw (subseq string 0 name-end-pos)))
          ;; ... then just try to parse the locative.
          (multiple-value-bind (locative pos)
              (parse-locative (subseq string name-end-pos))
            (if locative
                (values (find-name (rcurry #'dref locative nil)
                                   (trim-whitespace raw))
                        locative
                        (maybe-junk (+ name-end-pos pos)))
                (values nil nil (maybe-junk name-end-pos)))))
      ((or reader-error end-of-file) ()
        nil))))

(defun parse-definitions* (string)
  (find-name #'definitions* (trim-whitespace string)))
