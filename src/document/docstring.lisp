(in-package :mgl-pax)

(declaim (special *table-of-contents-stream*))

;;; Normalize indentation of docstrings as described in (METHOD ()
;;; (STRING T)) DOCUMENT-OBJECT.
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


(defun/autoloaded documentation* (object doc-type)
  "A small wrapper around CL:DOCUMENTATION to smooth over differences
  between implementations."
  ;; KLUDGE: Some just can't decide where the documentation is. Traced
  ;; generic functions complicate things.
  (when (functionp object)
    #+(or ccl ecl)
    (when (and (eq doc-type 'function)
               (null (documentation object 'function)))
      (setq object (function-name object)))
    #+cmucl
    (setq object (function-name object)))
  #+cmucl
  (when (typep object 'class)
    (setq object (class-name object)
          doc-type 'type))
  (let ((docstring (documentation object doc-type)))
    #+sbcl
    (setq docstring (filter-junk-docstrings docstring))
    docstring))

#+sbcl
(defun filter-junk-docstrings (docstring)
  (if (member docstring
              '("Return whether debug-block represents elsewhere code."
                "automatically generated accessor method"
                "automatically generated reader method"
                "automatically generated writer method")
              :test #'equal)
      nil
      docstring))
