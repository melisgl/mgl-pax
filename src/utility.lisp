(in-package :mgl-pax)

(defun symbol-global-value (symbol)
  #+sbcl
  (ignore-errors (sb-ext:symbol-global-value symbol))
  #+allegro
  (multiple-value-bind (value bound) (sys:global-symbol-value symbol)
    (values value (eq bound :unbound)))
  #-(or sbcl allegro)
  (ignore-errors (symbol-value symbol)))

(defun read-stream-into-string (stream &key (buffer-size 4096))
  (let ((*print-pretty* nil))
    (with-output-to-string (datum)
      (let ((buffer (make-array buffer-size :element-type 'character)))
        (loop
          :for bytes-read = (read-sequence buffer stream)
          :do (write-sequence buffer datum :start 0 :end bytes-read)
          :while (= bytes-read buffer-size))))))

(defun subseq* (seq start)
  (subseq seq (min (length seq) start)))

(defun relativize-pathname (pathname reference-pathname)
  "Return a pathname that's equivalent to PATHNAME but relative to
  REFERENCE-PATHNAME if possible. Like ENOUGH-NAMESTRING, but inserts
  ..'s if necessary."
  (let ((pathname (merge-pathnames pathname *default-pathname-defaults*))
        (reference-pathname (merge-pathnames reference-pathname
                                             *default-pathname-defaults*)))
    (assert (equal (pathname-host pathname)
                   (pathname-host reference-pathname)))
    (assert (equal (pathname-device pathname)
                   (pathname-device reference-pathname)))
    (let* ((dir (pathname-directory pathname))
           (ref-dir (pathname-directory reference-pathname))
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

;;;; Stream specs

(defgeneric make-stream-spec (object &rest args))

(defgeneric unmake-stream-spec (stream-spec))

(defgeneric call-with-open-stream-spec (stream-spec direction fn))

(defgeneric delete-stream-spec (stream-spec))

(defmacro with-open-stream-spec ((stream stream-spec &key (direction :input))
                                 &body body)
  `(call-with-open-stream-spec ,stream-spec ,direction
                               (lambda (,stream) ,@body)))

;;;; STRING-STREAM-SPEC

(defclass string-stream-spec ()
  ((string :initform "" :initarg :string :accessor string-stream-spec-string)))

(defmethod make-stream-spec ((object null) &rest args)
  (assert (endp args))
  (make-instance 'string-stream-spec))

(defmethod unmake-stream-spec ((spec string-stream-spec))
  (string-stream-spec-string spec))

(defmethod call-with-open-stream-spec ((spec string-stream-spec)
                                       (direction (eql :input)) fn)
  (funcall fn (make-string-input-stream (string-stream-spec-string spec))))

(defmethod call-with-open-stream-spec ((spec string-stream-spec)
                                       (direction (eql :output)) fn)
  (let ((output-stream (make-string-output-stream)))
    (unwind-protect
         (funcall fn output-stream)
      (setf (string-stream-spec-string spec)
            (concatenate 'string (string-stream-spec-string spec)
                         (get-output-stream-string output-stream))))))

(defmethod delete-stream-spec ((spec string-stream-spec))
  (setf (string-stream-spec-string spec) ""))

;;;; FILE-STREAM-SPEC

(defclass file-stream-spec ()
  ((pathname :initarg :pathname
             :reader file-stream-spec-pathname)
   (open-args :initform () :initarg :open-args
              :reader file-stream-spec-open-args)))

(defmethod print-object ((spec file-stream-spec) stream)
  (print-unreadable-object (spec stream :type t)
    (format stream "~S" (file-stream-spec-pathname spec))))

(defmethod make-stream-spec ((object string) &rest args)
  (make-instance 'file-stream-spec :pathname object
                 ;; Copy ARGS, because we'll call REMF on it.
                 :open-args (copy-list args)))

(defmethod make-stream-spec ((object pathname) &rest args)
  (make-instance 'file-stream-spec :pathname object
                 ;; Copy ARGS, because we'll call REMF on it.
                 :open-args (copy-list args)))

(defmethod unmake-stream-spec ((spec file-stream-spec))
  (file-stream-spec-pathname spec))

(defmethod call-with-open-stream-spec ((spec file-stream-spec) direction fn)
  (let ((open-args (file-stream-spec-open-args spec))
        (pathname (file-stream-spec-pathname spec)))
    (when (getf open-args :ensure-directories-exist)
      (ensure-directories-exist pathname))
    (remf open-args :ensure-directories-exist)
    (unwind-protect
         (with-open-stream (stream (apply #'open pathname
                                          :direction direction
                                          open-args))
           (funcall fn stream))
      ;; Subsequent opens must append.
      (loop while (remf (slot-value spec 'open-args) :if-exists))
      (setf (slot-value spec 'open-args)
            (append (list :if-exists :append) (slot-value spec 'open-args))))))

(defmethod delete-stream-spec ((spec file-stream-spec))
  (delete-file (file-stream-spec-pathname spec)))

;;;; STREAM-STREAM-SPEC

(defmethod make-stream-spec ((stream stream) &rest args)
  (assert (endp args))
  stream)

(defmethod unmake-stream-spec ((stream stream))
  stream)

(defmethod call-with-open-stream-spec ((stream stream) direction fn)
  (ecase direction
    ((:input) (assert (input-stream-p stream)))
    ((:output) (assert (output-stream-p stream))))
  (funcall fn stream))

;;;; T

(defmethod make-stream-spec ((spec (eql t)) &rest args)
  (assert (endp args))
  *standard-output*)

;;;; Hotpatching

#+allegro
(progn
  swank-backend::
  (unless (get 'function-name 'implementation)
    (defimplementation function-name (f)
      (check-type f function)
      (cross-reference::object-to-function-name f))))

#+allegro
(progn
  swank-backend::
  (unless (get 'find-source-location 'implementation)
    (defimplementation find-source-location (obj)
      (first (rest (first (fspec-definition-locations obj)))))))


;;;; String utilities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *whitespace-chars*
    '(#\Space #\Tab #\Return #\Newline #\Linefeed #\Page)))

(defun whitespacep (char)
  (member char *whitespace-chars*))

(defun blankp (string)
  (every #'whitespacep string))

(defun trim-whitespace (string)
  (string-trim #.(format nil "~{~A~}" *whitespace-chars*) string))


;;;; Escaping of HTML ID and NAME

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun _mark-range (array start end)
    (loop for a from (char-code start) to (char-code end) do
      (setf (sbit array a) 1)))

  (defun _mark-one (array ch)
    (setf (sbit array (char-code ch)) 1)))

(defparameter +first-name-characters+ 
  (let ((array (make-array 255 :element-type 'bit :initial-element 0)))
    (_mark-range array #\a #\z)
    (_mark-range array #\A #\Z)
    array))

(defparameter +name-characters+ 
  (let ((array (copy-seq +first-name-characters+)))
    (_mark-range array #\0 #\9)
    (_mark-one array #\-)
    ;; Encode these as well to work around github markdown bug which
    ;; would otherwise break links.
    #+nil (_mark-one array #\_)
    #+nil (_mark-one array #\.)
    #+nil (_mark-one array #\:)
    array))

(defun html-safe-name (name)
  ;; Copied from HTML-Encode
  ;;?? this is very consy
  ;;?? crappy name
  (declare (type simple-string name))
  (let ((output (make-array (truncate (length name) 2/3)
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0))
	(first? t))
    (with-output-to-string (out output)
      (loop for char across name
            for code = (char-code char)
            for valid = +first-name-characters+ then +name-characters+
            do (cond ((and (< code 255)
                           (= (sbit valid code) 1))
                      (write-char char out))
                     (t
                      ;; See http://www.w3.org/TR/html4/types.html#h-6.2
                      ;; ID and NAME tokens must begin with a letter ([A-Za-z]) 
                      ;; and may be followed by any number of letters, 
                      ;; digits ([0-9]), hyphens ("-"), underscores ("_"), 
                      ;; colons (":"), and periods (".").
                      (when first?
                        (write-char #\x out)) 
                      (format out "-~:@(~16,r~)" code)))
               (setf first? nil)))
    (coerce output 'simple-string)))


;;;; Text based HTML fragments

(defun anchor (anchor stream)
  (format stream "<a id='~A'></a>~%~%" (html-safe-name anchor)))


;;;; Text based markdown fragments

(defun heading (level stream)
  (loop repeat (1+ level) do (write-char #\# stream)))

(defun code (string)
  (if (zerop (length string))
      ""
      (format nil "`~A`" string)))

(defun markdown-special-char-p (char)
  (member char '(#\* #\_ #\` #\< #\>)))

(defun prin1-and-escape-markdown (object)
  (escape-markdown (prin1-to-string object)))

(defun escape-markdown (string)
  (with-output-to-string (stream)
    (dotimes (i (length string))
      (let ((char (aref string i)))
        (when (markdown-special-char-p char)
          (write-char #\\ stream))
        (write-char char stream)))))

(defun bold (string stream)
  (if (zerop (length string))
      ""
      (format stream "**~A**" string)))

(defun italic (string stream)
  (if (zerop (length string))
      ""
      (format stream "*~A*" string)))



;;;; Parse tree based markdown fragments

(defun code-fragment (name)
  `(:code ,(princ-to-string name)))


;;;; Markdown parse tree transformation

;;; Perform a depth first traversal of TREE. Call FN with the parent
;;; of each node and the node itself. FN returns three values: a new
;;; tree to be substituted for the node, a recurse and slice flag. If
;;; slice, then the new tree is sliced into parent. If recurse (and
;;; the new tree is not a leaf), then traversal goes recurses into the
;;; new tree.
(defun transform-tree (fn tree)
  (labels ((foo (parent tree)
             (multiple-value-bind (new-tree recurse slice)
                 (funcall fn parent tree)
               (assert (or (not slice) (listp new-tree)))
               (if (or (atom new-tree)
                       (not recurse))
                   (values new-tree slice)
                   (values (loop for sub-tree in new-tree
                                 append (multiple-value-bind
                                              (new-sub-tree slice)
                                            (foo new-tree sub-tree)
                                          (if slice
                                              new-sub-tree
                                              (list new-sub-tree))))
                           slice)))))
    (foo nil tree)))

(defun defer-tag-handling (tags stop-tags handle-strings fn parent tree)
  (cond ((or (and (listp tree)
                  (member (first tree) tags))
             (and handle-strings
                  (stringp tree)))
         (funcall fn parent tree))
        ((and (listp tree)
              (member (first tree) stop-tags))
         (values tree nil nil))
        (t
         (values tree (and tree (listp tree)) nil))))

(defun map-markdown-parse-tree (tags stop-tags handle-strings fn string)
  (let* ((3bmd-grammar:*smart-quotes* nil)
         (parse-tree
           ;; To be able to recognize symbols like FOO* join (...
           ;; "FOO" "*" ...) to look like (... "FOO*" ...).
           (join-consecutive-non-blank-strings-in-parse-tree
            (3bmd-grammar:parse-doc string))))
    (with-output-to-string (out)
      (3bmd::print-doc-to-stream-using-format
       (transform-tree (lambda (parent tree)
                         (defer-tag-handling tags stop-tags handle-strings
                           fn parent tree))
                       parse-tree)
       out :markdown))))

(defun join-consecutive-non-blank-strings-in-parse-tree (parse-tree)
  (transform-tree
   (lambda (parent tree)
     (declare (ignore parent))
     (if (listp tree)
         (values (join-consecutive-non-blank-strings-in-list tree) t nil)
         tree))
   parse-tree))

(defun join-consecutive-non-blank-strings-in-list (list)
  (let ((result ()))
    (dolist (element list)
      (if (and (stringp element)
               (stringp (first result))
               (not (blankp element))
               (not (blankp (first result))))
          (setf (first result)
                (concatenate 'string (first result) element))
          (push element result)))
    (reverse result)))

