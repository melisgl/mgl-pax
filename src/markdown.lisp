(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

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
  (member char '(#\* #\_ #\` #\< #\> #\[ #\])))

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

(declaim (inline parse-tree-tag-p))
(defun parse-tree-p (parse-tree tag)
  (and (listp parse-tree)
       (eq (first parse-tree) tag)))

(defmacro pt-get (parse-tree attr)
  `(getf (rest ,parse-tree) ,attr))

(defun code-fragment (name)
  `(:code ,(princ-to-string name)))


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

;;; Call FN with STRING and START, END indices. END always points to
;;; the next DELIMETERP character after START or to end of the string
;;; if there are no delimiters left. START starts at 0, then advances
;;; to FIXME
;;;
;;; FN must return three values: a replacement markdown parse tree
;;; fragment (or NIL, if the subseq shall not be replaced), whether
;;; the replacement shall be sliced into the result list, and the
;;; number of characters replaced (may be less than (- END START),
;;; which is assumed if this value is NIL). MAP-NAMES returns a parse
;;; tree fragment that's a list of non-replaced parts of STRING and
;;; replacements (maybe sliced). Consecutive strings are concatenated.
(defun map-names (string fn)
  (let ((translated ())
        (i 0)
        (n (length string))
        ;; FIXME
        (prev nil))
    (flet ((add (a)
             (if (and (stringp a)
                      (stringp (first translated)))
                 ;; FIXME: optimize this
                 (setf (first translated)
                       (concatenate 'string (first translated) a))
                 (push a translated))))
      (loop while (< i n)
            do (let ((char (aref string i))
                     (replacement nil)
                     (n-chars-replaced nil)
                     (slice nil))
                 (when (and (not (delimiterp char))
                            (or (null prev) (delimiterp prev)))
                   (let ((end (or (position-if #'delimiterp string :start i)
                                  (length string))))
                     (multiple-value-setq (replacement slice n-chars-replaced)
                       (funcall fn string i end))
                     (when replacement
                       (if slice
                           (dolist (a replacement)
                             (add a))
                           (add replacement))
                       (cond (n-chars-replaced
                              (assert (plusp n-chars-replaced))
                              (incf i n-chars-replaced))
                             (t
                              (setq i end))))))
                 (unless replacement
                   (add (string char))
                   (incf i))
                 (setq prev char))))
    (nreverse translated)))
