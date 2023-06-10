(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

;;;; Escaping V2 of URLs and HTML ID (for HTML5)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun _mark-range (array start end)
    (loop for a from (char-code start) to (char-code end) do
      (setf (sbit array a) 1)))

  (defun _mark-one (array ch)
    (setf (sbit array (char-code ch)) 1)))

(defparameter *unreserved-url-characters*
  (let ((array (make-array 256 :element-type 'bit :initial-element 0)))
    ;; RFC3986 unreserved characters
    (_mark-range array #\a #\z)
    (_mark-range array #\A #\Z)
    (_mark-range array #\0 #\9)
    (_mark-one array #\-)
    (_mark-one array #\_)
    (_mark-one array #\.)
    (_mark-one array #\~)
    ;; Include some reserved characters used by major sites
    ;; (https://stackoverflow.com/a/42287988/532597), which violates
    ;; RFC3986.
    (_mark-one array #\:)
    (_mark-one array #\@)
    (_mark-one array #\+)
    (_mark-one array #\*)
    array))

(defparameter *url-escape-char* #\%)

;;; This is adapted from HTML-Encode. Note that we also use this in
;;; html <a id="...">, which is valid in HTML5 because there is no
;;; space in it.
(defun urlencode (string)
  (declare (type string string))
  (let* ((bytes (trivial-utf-8:string-to-utf-8-bytes string))
         (output (make-array (truncate (length bytes) 2/3)
                             :element-type 'character
                             :adjustable t
                             :fill-pointer 0))
         (unreserved *unreserved-url-characters*)
         (escape-char *url-escape-char*)
         (escape-char-code (char-code escape-char)))
    (declare (type (simple-array bit (256)) unreserved)
             (type character escape-char))
    (with-output-to-string (out output)
      (loop for code across bytes
            do (cond ((and (= (sbit unreserved code) 1)
                           (/= code escape-char-code))
                      (write-char (code-char code) out))
                     (t
                      (format out "~A~:@(~16r~)" escape-char code)))))
    (coerce output 'simple-string)))

(declaim (inline hex-digit-to-int))
(defun hex-digit-to-int (char)
  (let ((code (char-code char)))
    (cond ((<= #.(char-code #\0) code #.(char-code #\9))
           (- code #.(char-code #\0)))
          ((<= #.(char-code #\A) code #.(char-code #\F))
           (+ (- code #.(char-code #\A)) 10))
          (t
           (assert nil () "~S is not a hex digit." char)))))

(defun urldecode (url)
  (declare (type string url))
  (let* ((escape-char *url-escape-char*)
         (n (count escape-char url)))
    (if (= n 0)
        ;; No consing if no escaped chars.
        url
        (let ((bytes (make-array (- (length url) (* 2 n))))
              (b 0))
          (with-input-from-string (in url)
            (loop for char = (read-char in nil nil)
                  while char
                  do (let ((byte (if (eql char escape-char)
                                     (+ (ash (hex-digit-to-int (read-char in))
                                             4)
                                        (hex-digit-to-int (read-char in)))
                                     (char-code char))))
                       (setf (aref bytes b) byte)
                       (incf b))))
          (trivial-utf-8:utf-8-bytes-to-string bytes)))))


;;;; Escaping V1 of URLs and HTML IDs (for HTML4)

(defparameter +html4-first-name-characters+
  (let ((array (make-array 255 :element-type 'bit :initial-element 0)))
    (_mark-range array #\a #\z)
    (_mark-range array #\A #\Z)
    array))

(defparameter +html4-name-characters+
  (let ((array (copy-seq +html4-first-name-characters+)))
    (_mark-range array #\0 #\9)
    (_mark-one array #\-)
    ;; Encode these as well to work around github markdown bug which
    ;; would otherwise break links.
    #+nil (_mark-one array #\_)
    #+nil (_mark-one array #\.)
    #+nil (_mark-one array #\:)
    array))

(defun html4-safe-name (name)
  (declare (type simple-string name))
  (let ((output (make-array (truncate (length name) 2/3)
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0))
	(first? t))
    (with-output-to-string (out output)
      (loop for char across name
            for code = (char-code char)
            for valid = +html4-first-name-characters+
              then +html4-name-characters+
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
                      (format out "-~:@(~16r~)" code)))
               (setf first? nil)))
    (coerce output 'simple-string)))


(defun escape-html (string)
  (with-output-to-string (out)
    (loop for char across string
          do (format out "~A" (case char
                                ((#\<) "&lt;")
                                ((#\>) "&gt;")
                                ((#\&) "&amp;")
                                (t char))))))


(defun parse-url (string)
  (let ((scheme-end (position #\: string))
        (pos 0)
        (len (length string)))
    (unless scheme-end
      (error "~S has no URL scheme." string))
    (values
     ;; scheme
     (prog1 (subseq string 0 scheme-end)
       (setq pos (1+ scheme-end)))
     ;; authority
     (if (and pos
              (< (+ pos 2) len)
              (char= (aref string pos) #\/)
              (char= (aref string (1+ pos)) #\/))
         (let ((authority-end
                 (position #\/ string :start (+ pos 2))))
           (prog1 (urldecode (subseq string (+ pos 2) authority-end))
             (setq pos authority-end)))
         nil)
     ;; path
     (when (and pos (< pos len))
       (let ((path-end (position-if (lambda (char)
                                      (member char '(#\# #\?)))
                                    string :start pos)))
         (prog1 (urldecode (subseq string pos path-end))
           (setq pos (if path-end
                         (1+ path-end)
                         nil)))))
     ;; query
     (when (and pos (< pos len))
       (let ((query-end (position #\# string :start pos)))
         (prog1 (urldecode (subseq string pos query-end))
           (setq pos (if query-end
                         (1+ query-end)
                         nil)))))
     ;; fragment
     (when (and pos (< pos len))
       (urldecode (subseq string pos))))))

(defun urlp (string)
  (ignore-errors (parse-url string)))

(defun append-to-url (url suffix)
  (if (or (alexandria:ends-with #\/ url)
          (alexandria:starts-with #\/ suffix))
      (format nil "~A~A" url suffix)
      (format nil "~A/~A" url suffix)))


(defun pathname-to-file-url (pathname)
  (flet ((sanitize (namestring)
           ;; On CCL, (NAMESTRING (MAKE-PATHNAME :NAME ":")) => "\\:".
           #+ccl (remove #\\ namestring)
           #-ccl namestring))
    (let* ((pathname (pathname pathname))
           (pathname-directory (pathname-directory pathname)))
      (format nil "file://~A"
              (sanitize
               (namestring
                (make-pathname
                 :directory (and pathname-directory
                                 (cons (first pathname-directory)
                                       (mapcar #'urlencode
                                               (rest pathname-directory))))
                 :name (and (pathname-name pathname)
                            (urlencode (sanitize (pathname-name pathname))))
                 :type (and (pathname-type pathname)
                            (urlencode (pathname-type pathname)))
                 :defaults pathname)))))))

(defun file-url-to-pathname (url)
  (when (alexandria:starts-with-subseq "file://" url)
    (let* ((pathname (pathname (subseq url 7)))
           (pathname-directory (pathname-directory pathname)))
      (make-pathname
       :directory (and pathname-directory
                       (cons (first pathname-directory)
                             (mapcar #'urldecode
                                     (rest pathname-directory))))
       :name (and (pathname-name pathname)
                  (urldecode (pathname-name pathname)))
       :type (and (pathname-type pathname)
                  (urldecode (pathname-type pathname)))
       :defaults pathname))))
