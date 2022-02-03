(in-package :mgl-pax)

(declaim (special *table-of-contents-stream*))

;;; Print (DOCUMENTATION OBJECT DOC-TYPE) to STREAM in FORMAT. Clean
;;; up docstring indentation, then indent it by four spaces.
;;; Automarkup symbols.
(defun maybe-print-docstring (object doc-type stream)
  ;; If the output is going to /dev/null and this is a costly
  ;; operation, skip it.
  (unless *table-of-contents-stream*
    (let ((docstring (filter-documentation object doc-type)))
      (when docstring
        (format stream "~%~A~%" (massage-docstring docstring))))))

(defun massage-docstring (docstring &key (indentation "    ")
                          exclude-first-line-p)
  (if *table-of-contents-stream*
      ""
      (let ((docstring (strip-docstring-indentation docstring)))
        (prefix-lines indentation (codify-and-link docstring)
                      :exclude-first-line-p exclude-first-line-p))))

;;; Normalize indentation of docstrings as it's described in
;;; (METHOD () (STRING T)) DOCUMENT-OBJECT.
(defun strip-docstring-indentation (docstring &key (first-line-special-p t))
  (let ((indentation
          (docstring-indentation docstring
                                 :first-line-special-p first-line-special-p)))
    (values (with-output-to-string (out)
              (with-input-from-string (s docstring)
                (loop for i upfrom 0
                      do (multiple-value-bind (line missing-newline-p)
                             (read-line s nil nil)
                           (unless line
                             (return))
                           (if (and first-line-special-p (zerop i))
                               (write-string line out)
                               (write-string (subseq* line indentation) out))
                           (unless missing-newline-p
                             (terpri out))))))
            indentation)))

(defun n-leading-spaces (line)
  (let ((n 0))
    (loop for i below (length line)
          while (char= (aref line i) #\Space)
          do (incf n))
    n))

;;; Return the minimum number of leading spaces in non-blank lines
;;; after the first.
(defun docstring-indentation (docstring &key (first-line-special-p t))
  (let ((n-min-indentation nil))
    (with-input-from-string (s docstring)
      (loop for i upfrom 0
            for line = (read-line s nil nil)
            while line
            do (when (and (or (not first-line-special-p) (plusp i))
                          (not (blankp line)))
                 (when (or (null n-min-indentation)
                           (< (n-leading-spaces line) n-min-indentation))
                   (setq n-min-indentation (n-leading-spaces line))))))
    (or n-min-indentation 0)))


(defun filter-documentation (symbol doc-type)
  (let ((docstring (documentation symbol doc-type)))
    #+sbcl
    (if (member docstring
                '("Return whether debug-block represents elsewhere code."
                  "automatically generated accessor method"
                  "automatically generated reader method"
                  "automatically generated writer method")
                :test #'equal)
        ;; Discard the garbage docstring.
        nil
        docstring)
    #-sbcl
    docstring))
