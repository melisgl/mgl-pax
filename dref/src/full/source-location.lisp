(in-package :dref)

(in-readtable pythonic-string-syntax)

(defun/autoloaded make-source-location (&key file file-position
                                             buffer buffer-position snippet)
  "Make a Swank source location. The ultimate reference is `slime.el`.
  When SNIPPET is provided, the match nearest to FILE-POSITION is
  determined (see the Elisp `slime-isearch` and
  SOURCE-LOCATION-FILE-POSITION-OFFSET)."
  (list :location
        (cond ((and file buffer)
               (list :buffer-and-file buffer (namestring file)))
              (file
               (list :file (namestring file)))
              (buffer
               (list :buffer buffer)))
        (list :position (if file-position
                            (1+ file-position)
                            buffer-position))
        (list :snippet snippet)))

(defun/autoloaded source-location-p (object)
  "See if OBJECT is a source location object."
  (and (listp object)
       (eq (first object) :location)))

(defun/autoloaded source-location-file (location)
  "Return the name of the file of the [defining form][clhs].
  This may be NIL, for example, if LOCATION is of a [defining
  form][clhs] that was evaluated or it was compiled in the
  `*slime-scratch*` buffer."
  (let ((file-entry (find :file (rest location) :key #'first))
        (buffer-and-file-entry (find :buffer-and-file (rest location)
                                     :key #'first)))
    (or (second file-entry)
        (third buffer-and-file-entry))))

(defun/autoloaded source-location-file-position (location)
  "Return the file position of the [defining form][clhs] or NIL
  if it's not available. The first position is 0."
  (let ((pos (source-location-buffer-position location)))
    (if pos
        (1- pos)
        pos)))

(defun/autoloaded source-location-buffer (location)
  "Return the name of Emacs buffer of the [defining form][clhs] or NIL
  if there is no such Emacs buffer."
  (let ((buffer-entry (find :buffer (rest location) :key #'first))
        (buffer-and-file-entry (find :buffer-and-file (rest location)
                                     :key #'first)))
    (or (second buffer-entry)
        (second buffer-and-file-entry))))

(defun/autoloaded source-location-buffer-position (location)
  "Return the position of the [defining form][clhs] in
  SOURCE-LOCATION-BUFFER or NIL if it's not available. The first
  position is 1."
  (let ((position-entry (find :position (rest location) :key #'first))
        (offset-entry (find :offset (rest location) :key #'first)))
    (cond (position-entry
           (second position-entry))
          (offset-entry
           (+ (second offset-entry) (third offset-entry))))))

(defun/autoloaded source-location-snippet (location)
  "Return the [defining form][clhs] as a string or NIL if it's not available."
  (and (eq (first location) :location)
       (getf (fourth location) :snippet)))

(defun/autoloaded source-location-file-position-offset (location)
  "Return the actual file position LOCATION points to.
  This is different from SOURCE-LOCATION-FILE-POSITION, which is a raw
  accessor, in that it adjusts the position by searching for the
  nearest occurrence of SOURCE-LOCATION-SNIPPET. If
  SOURCE-LOCATION-FILE is NIL or the snippet doesn't match, NIL is
  returned.

  This is a non-interactive companion to the Elisp function
  `slime-location-offset`, supporting only file positions and
  non-partial matching of snippets."
  (let ((file (source-location-file location))
        (pos (or (source-location-file-position location) 0))
        (snippet (source-location-snippet location)))
    (when (and file snippet)
      (let* ((string (slurp-file file))
             ;; Unlike `slime-isearch', we only do full matches.
             (pos-before (search snippet string :end2 pos :from-end t))
             (pos-after (search snippet string :start2 pos :from-end t)))
        (or (and pos-before pos-after
                 (if (< (- pos pos-before)
                        (- pos-after pos))
                     pos-before
                     pos-after))
            pos-before pos-after)))))

(defparameter *utf-8-external-format*
  #+abcl :utf-8
  #+allegro :utf-8
  #+clisp charset:utf-8
  #-(or abcl allegro clisp) :default)

(defun slurp-file (file)
  ;; FIXME: Determine the external format somehow?
  (alexandria:read-file-into-string
   file :external-format *utf-8-external-format*))
