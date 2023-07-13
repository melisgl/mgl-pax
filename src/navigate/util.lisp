(in-package :mgl-pax)

;;;; I/O

(defmacro with-standard-io-syntax* (&body body)
  `(with-standard-io-syntax
     ;; With *PRINT-READABLY*, CLISP insists on printing FOO as |FOO|.
     (let (#+clisp (*print-readably* nil))
       ,@body)))

(defparameter *utf-8-external-format*
  #+abcl :utf-8
  #+allegro :utf-8
  #+clisp charset:utf-8
  #-(or abcl allegro clisp) :default)

;;; Return the number of characters that would be read by
;;; READ-FROM-STRING. May signal READER-ERROR or END-OF-FILE.
(defun n-chars-would-read (string)
  (nth-value 1 (let ((*read-suppress* t))
                 (read-from-string string))))


;;;; Symbols

(defun read-interned-symbol-from-string (string)
  (let ((pos (n-chars-would-read string)))
    (multiple-value-bind (symbol foundp)
        (swank::parse-symbol (trim-whitespace (subseq string 0 pos)))
      (when foundp
        (multiple-value-bind (symbol2 pos) (read-from-string string)
          (assert (eq symbol symbol2))
          (values symbol2 pos))))))

(defun external-symbol-p (symbol &optional (package (symbol-package symbol)))
  (and package
       (multiple-value-bind (symbol* status)
           (find-symbol (symbol-name symbol) package)
         (and (eq status :external)
              (eq symbol symbol*)))))

(defun external-symbol-in-any-package-p (symbol)
  (loop for package in (list-all-packages)
          thereis (external-symbol-p symbol package)))

(defun symbol-other-packages (symbol)
  (loop for package in (list-all-packages)
        when (and (external-symbol-p symbol package)
                  (not (eq package (symbol-package symbol))))
          collect package))

(defun symbol-global-value (symbol)
  #+allegro
  (multiple-value-bind (value bound) (sys:global-symbol-value symbol)
    (values value (eq bound :unbound)))
  #+ccl
  (let ((value (ccl::%sym-global-value symbol)))
    (values value (eq value (ccl::%unbound-marker))))
  #+sbcl
  (ignore-errors (sb-ext:symbol-global-value symbol))
  #-(or allegro ccl sbcl)
  (ignore-errors (symbol-value symbol)))


(defmacro with-debugger-hook (fn &body body)
  (with-gensyms (prev-debugger-hook condition this-hook)
    `(let* ((,prev-debugger-hook *debugger-hook*)
            (*debugger-hook* (lambda (,condition ,this-hook)
                               (declare (ignore ,this-hook))
                               (funcall ,fn ,condition)
                               (let ((*debugger-hook* ,prev-debugger-hook))
                                 (invoke-debugger ,condition)))))
       ,@body)))


;;;; Sequences

(defun subseq* (seq start)
  (subseq seq (min (length seq) start)))


;;;; Pathnames

(defun relativize-pathname (pathname reference-pathname)
  "Return a pathname that's equivalent to PATHNAME but relative to
  REFERENCE-PATHNAME if possible. Like ENOUGH-NAMESTRING, but inserts
  :UP components if necessary."
  (let ((pathname (merge-pathnames pathname *default-pathname-defaults*))
        (reference-pathname (merge-pathnames reference-pathname
                                             *default-pathname-defaults*)))
    (assert (equal (pathname-host pathname)
                   (pathname-host reference-pathname)))
    (assert (equal (pathname-device pathname)
                   (pathname-device reference-pathname)))
    (let* ((dir (remove :relative (pathname-directory pathname)))
           (ref-dir (remove :relative (pathname-directory reference-pathname)))
           (mismatch-index (or (mismatch dir ref-dir :test #'equal)
                               (length dir))))
      (normalize-pathname
       (make-pathname :directory (nconc (list :relative)
                                        (make-list (- (length ref-dir)
                                                      mismatch-index)
                                                   :initial-element :up)
                                        (subseq dir mismatch-index))
                      :defaults pathname)))))

(defun normalize-pathname (pathname)
  (if (equal '(:relative) (pathname-directory pathname))
      ;; Some implementations print (:RELATIVE) as "", some as "./",
      ;; no such troubles with the equivalent ().
      (make-pathname :directory () :defaults pathname)
      pathname))


;;;; Strings

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *whitespace-chars*
    '(#\Space #\Tab #\Return #\Newline #\Linefeed #\Page)))

(defun whitespacep (char)
  (member char *whitespace-chars*))

(defun blankp (string)
  (every #'whitespacep string))

(defun trim-whitespace (string)
  (string-trim #.(format nil "~{~A~}" *whitespace-chars*) string))

;;; Add PREFIX to every line in STRING.
(defun prefix-lines (prefix string &key exclude-first-line-p)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for i upfrom 0 do
        (multiple-value-bind (line missing-newline-p) (read-line in nil nil)
          (unless line
            (return))
          (if (and exclude-first-line-p (= i 0))
              (format out "~a" line)
              (format out "~a~a" prefix line))
          (unless missing-newline-p
            (terpri out)))))))

(defun shorten-string (string &key n-lines n-chars ellipsis)
  (let ((shortened string))
    (when n-lines
      (setq shortened (first-lines shortened n-lines)))
    (when (and n-chars (< n-chars (length shortened)))
      (setq shortened (subseq shortened 0 n-chars)))
    (if (and ellipsis (< (length shortened) (length string)))
        (concatenate 'string shortened ellipsis)
        shortened)))

(defun first-lines (string &optional (n-lines 1))
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for i below n-lines do
        (multiple-value-bind (line missing-newline-p) (read-line in nil nil)
          (when line
            (if missing-newline-p
                (write-string line out)
                (write-line line out))))))))
