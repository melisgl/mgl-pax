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


;;;; Funny printing of @NAMEs

;;; If NAME is a symbol, then print it almost as PRIN1 would with
;;; *PACKAGE* were the CL package. Differences:
;;;
;;; - For symbols in other packages, a single #\: is printed even if
;;;   it is an internal symbol.
;;;
;;; - Package and symbol names are printed without the || syntax but
;;;   #\: and #\Space are escaped with backslashes.
(defun prin1-funny (name &optional (stream *standard-output*))
  (etypecase name
    (symbol
     (let* ((package (symbol-package name))
            (name (symbol-name name))
            (cl-package #.(find-package :common-lisp))
            (keyword-package #.(find-package :keyword)))
       (cond
         ((eq package cl-package)
          (prin1-funny* name stream))
         ((eq package keyword-package)
          (write-char #\: stream)
          (prin1-funny* name stream))
         (t
          (prin1-funny* (package-name package) stream)
          ;; Note the single : character.
          (write-char #\: stream)
          (prin1-funny* name stream)))))
    (string
     (prin1 name stream))))

;;; Escape #\:, #\Space, #\\ with a backslash.
(defun prin1-funny* (string &optional (stream *standard-output*))
  (loop for char across string
        do (when (or (eql char #\:) (eql char #\Space) (eql char #\\))
             (write-char #\\ stream))
           (write-char char stream)))

;;; Like READ, but do not INTERN.
(defun read-funny (stream &optional (eof-error-p t) eof-value)
  (if (eql (peek-char t stream eof-value eof-value) #\")
      (read stream eof-error-p eof-value)
      (let ((name-1 (read-funny* stream))
            (next-char (peek-char nil stream nil)))
        (cond ((eql next-char #\:)
               (read-char stream)
               (find-symbol (read-funny* stream)
                            (if (zerop (length name-1))
                                #.(find-package :keyword)
                                (find-package name-1))))
              (t
               (find-symbol name-1 #.(find-package :cl)))))))

(defun read-funny* (stream)
  (with-output-to-string (s)
    (loop for char = (read-char stream nil)
          while char
          do ;; These would be escaped if they were part of the name.
             (when (or (eql char #\:) (eql char #\Space))
               (unread-char char stream)
               (return))
             (when (eql char #\\)
               ;; EOF is invalid syntax.
               (setq char (read-char stream)))
             (write-char char s))))

(defun prin1-funny-to-string (name)
  (with-output-to-string (stream)
    (prin1-funny name stream)))

(defun prin1-funny*-to-string (string)
  (with-output-to-string (stream)
    (prin1-funny* string stream)))

(defun read-funny-from-string (string)
  (with-input-from-string (stream string)
    (values (read-funny stream)
            (file-position stream))))

(defun read-funny*-from-string (string)
  (with-input-from-string (stream string)
    (values (read-funny* stream)
            (file-position stream))))
