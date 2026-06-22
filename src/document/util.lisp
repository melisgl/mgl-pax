(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defun list-of-one-p (list)
  (and list (null (cdr list))))

(defun backslash-escape (string chars)
  (with-output-to-string (stream)
    (dotimes (i (length string))
      (let ((char (aref string i)))
        (when (find char chars)
          (write-char #\\ stream))
        (write-char char stream)))))

(defun lexicographic< (pred x y)
  (cond ((null y) nil)
        ((null x) t)
        ((funcall pred (car x) (car y)) t)
        ((funcall pred (car y) (car x)) nil)
        (t (lexicographic< pred (cdr x) (cdr y)))))


(defun definitions* (name)
  (definitions name :dtype 'top :sort nil))


;;;; Like the DREF function and DEFINITIONS*, but for references and
;;;; name given as strings, respectively.

;;; Parse "NAME LOCATIVE-TYPE" or "NAME (LOCATIVE-TYPE ...)", but do
;;; not intern stuff. Return:
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
        (let* ((name-end-pos (skip-sexp string))
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

;;; Symbols in names (they can be lists) are printed almost as PRIN1
;;; would with *PACKAGE* were the CL package. Differences:
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
     (prin1 name stream))
    (list
     (write-char #\( stream)
     (loop for el in name
           for firstp = t then nil
           do (unless firstp
                (write-char #\Space stream))
              (prin1-funny el stream))
     (write-char #\) stream))))

;;; Escape #\:, #\Space, #\(, #\), #\\ with a backslash.
(defun prin1-funny* (string &optional (stream *standard-output*))
  (loop for char across string
        do (when (member char '(#\: #\Space #\( #\) #\\))
             (write-char #\\ stream))
           (write-char char stream)))

;;; Like READ, but do not INTERN.
(defun read-funny (stream &optional (eof-error-p t) eof-value)
  (case (peek-char t stream eof-value eof-value)
    ((#\))
     (error "~@<Unpaired closing paren.~:@>"))
    ((#\()
     (read-funny-list stream))
    ((#\")
     (read stream eof-error-p eof-value))
    (t
     (let ((name-1 (read-funny* stream)))
       (cond ((eql (peek-char nil stream nil) #\:)
              (read-char stream)
              (let ((name-2 (read-funny* stream))
                    (package (if (zerop (length name-1))
                                 #.(find-package :keyword)
                                 (find-package name-1))))
                (or (find-symbol name-2 package)
                    (error "~@<Symbol with name ~S not found in package ~S.~:@>"
                           name-2 (package-name package)))))
             (t
              (let ((package #.(find-package :cl)))
                (or (find-symbol name-1 package)
                    (error "~@<Unqualified symbol with name ~S not found in ~
                         package ~S.~:@>" name-1 (package-name package))))))))))


(defun read-funny* (stream)
  (with-output-to-string (s)
    (loop for char = (read-char stream nil)
          while char
          do ;; These are escaped by PRIN1-FUNNY*.
             (when (member char '(#\: #\Space #\( #\)))
               (unread-char char stream)
               (return))
             (when (eql char #\\)
               ;; EOF is invalid syntax.
               (setq char (read-char stream)))
             (write-char char s))))

(defun read-funny-list (stream)
  (assert (char= (read-char stream) #\())
  (flet ((closep ()
           (when (eql (peek-char t stream nil nil) #\))
             (read-char stream)
             t)))
    (loop until (closep)
          collect (read-funny stream))))

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


;;;; Determine the ASDF system a definition belongs to.

(defvar *filename-to-asdf-system-name-map*)

;;; Amortize the cost of ASDF-SYSTEM-NAME-OF and ASDF-SYSTEM-NAME-OF*
;;; calls within the dynamic scope of BODY.
(defmacro with-filename-to-asdf-system-name-map (&body body)
  `(let ((*filename-to-asdf-system-name-map*
           (if (boundp '*filename-to-asdf-system-name-map*)
               *filename-to-asdf-system-name-map*
               (filename-to-asdf-system-name-map))))
     ,@body))

;;;; Return the ASDF:SYSTEM DREF is defined in or NIL.
(defun asdf-system-name-of (dref)
  (when-let (source-location (source-location dref))
    (when-let (file (source-location-file source-location))
      (asdf-system-name-of-filename file))))

;;; Like ASDF-SYSTEM-NAME-OF, but if DREF has no SOURCE-LOCATION then
;;; fall back to the source location of the home package of DREF-NAME
;;; (if it's a symbol).
(defun asdf-system-name-of* (dref)
  (if-let (file (file-of-dref dref))
    (asdf-system-name-of-filename file)
    (when (symbolp (dref-name dref))
      (when-let (package-dref (locate (symbol-package (dref-name dref))))
        (when-let (file (file-of-dref package-dref))
          (asdf-system-name-of-filename file))))))

(defun truenameish (filename)
  (or (ignore-errors (truename filename))
      (namestring filename)))

(defun asdf-system-name-of-filename (filename)
  (let ((filename (truenameish filename)))
    (if (boundp '*filename-to-asdf-system-name-map*)
        (gethash filename *filename-to-asdf-system-name-map*)
        (filename-to-asdf-system-name filename))))

(defmacro do-asdf-files ((system-name filename) &body body)
  (alexandria:with-unique-names (system component)
    `(dolist (,system-name (asdf:registered-systems))
       (let ((,system (find-system* ,system-name)))
         (dolist (,component (asdf/component:sub-components ,system))
           (when (typep ,component 'asdf:cl-source-file)
             (when-let (,filename (truenameish
                                   (asdf:component-pathname ,component)))
               ,@body)))))))

(defun filename-to-asdf-system-name-map ()
  (let ((h (make-hash-table :test #'equal)))
    (do-asdf-files (system-name filename)
      (setf (gethash filename h) system-name))
    h))

(defun filename-to-asdf-system-name (filename)
  (let ((filename (truenameish filename)))
    (do-asdf-files (system-name filename-1)
      ;; KLUDGE: Compare namestrings so that e.g. NIL vs :NEWEST in
      ;; PATHNAME-VERSION is hopefully not a diference.
      (when (equal filename-1 filename)
        (return-from filename-to-asdf-system-name system-name)))))

(defun file-of-dref (dref)
  (when-let (source-location (source-location dref))
    (source-location-file source-location)))


;;;; Radix trees

;;; Turn KEYS (a sequence) into a radix tree whose depth-first
;;; ordering reproduces KEYS (if unique). Each key in KEYS is a
;;; sequence of subkeys. TEST is the comparison function for subkeys.
;;;
;;; KEYS is assumed to be lexicographically sorted in ascending order
;;; (so that "a" is before "ab") in a way compatible with TEST.
;;;
;;; Each node in the tree is of the form (SUBKEY-LIST &REST CHILDREN).
;;; SUBKEY-LIST maybe empty. As an exception, a child can be NIL,
;;; signifying the end of the key. This is necessary to distinguish
;;; ("ab") from ("a" "ab").
(defun radix-tree (keys &key (test #'eql))
  (let ((children (radix-tree*
                   ;; Make it a list of lists.
                   (map 'list (lambda (subkey)
                                (coerce subkey 'list))
                        keys)
                   test)))
    ;; Add root
    (cons () children)))

(defun radix-tree* (keys test)
  (loop
    while keys
    collect (when-let (key (pop keys))
              (destructuring-bind (lead &rest suffix) key
                ;; Collect the suffixes after the same LEAD. This is
                ;; why KEYS must be sorted.
                (let ((suffixes
                        (cons suffix
                              (loop while (and keys (funcall test lead
                                                             (caar keys)))
                                    when (rest (pop keys))
                                      collect it))))
                  (merge-rt-nodes (radix-tree* suffixes test) key))))))

(defun merge-rt-nodes (children &optional key)
  (assert children)
  (cond
    ;; Multiple children
    ((cdr children)
     (cons (list (first key))
           ;; We choose to leave identical twins alone.
           children))
    ;; A single, empty child
    ((null (first children))
     (list key ()))
    ;; A single, non-empty child
    (t
     ;; We are not making a trie: merge non-branching branches.
     (let ((child (first children)))
       (push (first key) (car child))
       child))))

(defun map-radix-tree (fn tree &key (complete-only t))
  (labels ((recurse (tree key-runs)
             (cond ((null tree)
                    (funcall fn key-runs t))
                   (t
                    (let ((key-runs (cons (first tree) key-runs))
                          (completep (and (cdr tree)
                                          (null (second tree)))))
                      (when (and (not complete-only)
                                 (not completep))
                        (funcall fn key-runs nil))
                      (dolist (child (rest tree))
                        (recurse child key-runs)))))))
    (recurse tree ())))


(defun git-repository-root (filename)
  (let* ((path (uiop:ensure-absolute-pathname (pathname filename)
                                              (uiop:getcwd)))
         (dir (uiop:pathname-directory-pathname path))
         (native-path (uiop:native-namestring path)))
    (when (eql 0 (nth-value 2 (uiop:run-program
                               (list "git" "ls-files" "--error-unmatch"
                                     native-path)
                               :directory dir
                               :output nil
                               :error-output nil
                               :ignore-error-status t)))
      (let ((output (trim-whitespace
                     (uiop:run-program '("git" "rev-parse" "--show-toplevel")
                                       :directory dir
                                       :output :string
                                       :error-output nil
                                       :ignore-error-status t))))
        (when (plusp (length output))
          (uiop:ensure-directory-pathname output))))))

(defparameter *git-info-sh*
  """#!/usr/bin/env bash

if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    echo "Error: Not a git repository." >&2
    exit 1
fi

commit=$(git rev-parse --short HEAD 2>/dev/null)
# This will be empty with a detached HEAD
branch=$(git branch --show-current 2>/dev/null)
remote_url=""
remote_branch=""

if [ -n "$branch" ]; then
    remote_name=$(
        git config --get "branch.${branch}.pushRemote" || \
        git config --get "remote.pushDefault" || \
        git config --get "branch.${branch}.remote"
    )
    remote_name=${remote_name:-origin}
    remote_url=$(git config --get "remote.${remote_name}.url" 2>/dev/null)

    # Get the raw tracking branch (e.g. refs/heads/main)
    merge_ref=$(git config --get "branch.${branch}.merge" 2>/dev/null)

    if [ -n "$merge_ref" ]; then
        # Strip the 'refs/heads/' prefix to get just the branch name
        remote_branch=${merge_ref#refs/heads/}
    else
        # Predict the remote branch name (assuming push.default=simple)
        remote_branch="${branch}"
    fi
else
    # Fallback for detached HEAD
    remote_url=$(git config --get "remote.origin.url" 2>/dev/null)
fi

echo "${remote_url}"
echo "${remote_branch}"
echo "${commit}"
""")

(defun git-info (dir)
  (let ((output (trim-whitespace
                 (with-input-from-string (script-stream *git-info-sh*)
                   (uiop:run-program '("bash" "-s")
                                     :input script-stream
                                     :directory dir
                                     :output :string
                                     :error-output nil
                                     :ignore-error-status t)))))
    (with-input-from-string (s output)
      (values (read-line s)
              (let ((line (read-line s)))
                (and (plusp (length line))
                     line))
              (read-line s)))))
