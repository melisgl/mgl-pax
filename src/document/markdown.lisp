(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

;;;; Escaping V2 of URLs and HTML ID (for HTML5)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun _mark-range (array start end)
    (loop for a from (char-code start) to (char-code end) do
      (setf (sbit array a) 1)))

  (defun _mark-one (array ch)
    (setf (sbit array (char-code ch)) 1)))

(defparameter +unreserved-url-characters+
  (let ((array (make-array 255 :element-type 'bit :initial-element 0)))
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

;;; This is adapted from HTML-Encode. Note that we also use this as it
;;; in html <a id="...">, which is valid in HTML5 because there is no
;;; space in it.
(defun urlencode (string)
  (declare (type string string))
  (let ((output (make-array (truncate (length string) 2/3)
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0)))
    (with-output-to-string (out output)
      (loop for char across string
            for code = (char-code char)
            for valid = +unreserved-url-characters+
            do (cond ((and (< code 255)
                           (= (sbit valid code) 1))
                      (write-char char out))
                     (t
                      (format out "%~:@(~16r~)" code)))))
    (coerce output 'simple-string)))


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


;;;; Text based markdown fragments

(defun parse-markdown (string)
  (let ((3bmd-grammar:*smart-quotes* nil))
    ;; To be able to recognize symbols like FOO* join (... "FOO" "*"
    ;; ...) to look like (... "FOO*" ...).
    (join-consecutive-non-blank-strings-in-parse-tree
     (3bmd-grammar:parse-doc string))))

(defmacro with-colorize-silenced (() &body body)
  `(let ((*trace-output* (make-broadcast-stream)))
     ,@body))

(defun print-markdown (parse-tree stream)
  (with-colorize-silenced ()
    (3bmd::print-doc-to-stream-using-format parse-tree stream :markdown)))

(defun heading (level stream)
  (loop repeat (1+ level) do (write-char #\# stream)))

(defun code (string)
  (if (zerop (length string))
      ""
      (format nil "`~A`" string)))

(defun bold (string stream)
  (if (zerop (length string))
      ""
      (format stream "**~A**" string)))

(defun italic (string stream)
  (if (zerop (length string))
      ""
      (format stream "*~A*" string)))

(defun markdown-special-char-p (char)
  (member char '(#\* #\_ #\` #\< #\> #\[ #\])))

(defun/autoloaded escape-markdown (string)
  "Construct a new string from STRING by adding a backslash before
  each special markdown character:

      *_`<>[]"
  (with-output-to-string (stream)
    (dotimes (i (length string))
      (let ((char (aref string i)))
        (when (markdown-special-char-p char)
          (write-char #\\ stream))
        (write-char char stream)))))

;;; This is workaround for "\"mgl-pax\" ASFD:SYSTEM" being displayed
;;; with the backslashes.
(defun escape-markdown-reflink-definition-title (string)
  (cond ((not (find #\" string))
         (format nil "~S" string))
        ((not (find #\' string))
         (format nil "'~A'" string))
        ((not (find #\) string))
         (format nil "(~A)" string))
        (t
         (format nil "~S" string))))


;;;; Parse tree based markdown fragments

(declaim (inline parse-tree-tag-p))
(defun parse-tree-p (parse-tree tag)
  (and (listp parse-tree)
       (eq (first parse-tree) tag)))

(defmacro pt-get (parse-tree attr)
  `(getf (rest ,parse-tree) ,attr))

(defun code-fragment (string)
  `(:code ,(princ-to-string string)))


;;;; Markdown parse tree transformation

;;; Perform a depth first traversal of TREE. Call FN with the parent
;;; of each node and the node itself. FN returns three values: a new
;;; tree to be substituted for the node, a recurse and a slice flag.
;;; If slice, then the new tree is sliced into parent. If recurse (and
;;; the new tree is not a leaf), then traversal recurses into the new
;;; tree.
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

;;; When used as the FN argument to TRANSFORM-TREE, leave the tree
;;; intact except for subtrees (lists) whose CAR is in TAGS, whose
;;; transformation is deferred to FN. FN must return the three values
;;; TRANSFORM-TREE expects. If HANDLE-STRINGS, then FN is called with
;;; STRING nodes, too.
;;;
;;; If the CAR of a subtree is in STOP-TAGS, then the entire subtree
;;; is included in the output without further processing.
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

(defun map-markdown-parse-tree (tags stop-tags handle-strings fn parse-tree)
  (transform-tree (lambda (parent tree)
                    (defer-tag-handling tags stop-tags
                      handle-strings fn parent tree))
                  parse-tree))

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

;;; Call FN with STRING and START, END indices of @WORDS.
;;;
;;; FN must return two values: a replacement markdown parse tree
;;; fragment (or NIL, if the subseq shall not be replaced), whether
;;; the replacement shall be sliced into the result list. MAP-WORDS
;;; returns a parse tree fragment that's a list of non-replaced parts
;;; of STRING and replacements (maybe sliced). Consecutive strings are
;;; concatenated.
(defun map-words (string fn)
  (declare (type string string))
  (let ((translated ())
        (n (length string))
        (start 0))
    (flet ((add (a)
             (if (and (stringp a)
                      (stringp (first translated)))
                 (setf (first translated)
                       (concatenate 'string (first translated) a))
                 (push a translated))))
      (loop while (< start n)
            do (let* ((at-delimiter-p (delimiterp (aref string start)))
                      (end (or (if at-delimiter-p
                                   (position-if-not #'delimiterp string
                                                    :start start)
                                   (position-if #'delimiterp string
                                                :start start))
                               n)))
                 (if at-delimiter-p
                     (add (subseq string start end))
                     (multiple-value-bind (replacement slice)
                         (funcall fn string start end)
                       (cond ((null replacement)
                              (add (subseq string start end)))
                             (slice
                              (dolist (a replacement)
                                (add a)))
                             (t
                              (add replacement)))))
                 (setq start end))))
    (nreverse translated)))
