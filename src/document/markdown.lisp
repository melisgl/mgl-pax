(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)


;;;; Text based Markdown fragments

(defun heading (level stream)
  (loop repeat (1+ level) do (write-char #\# stream)))

;;; Wraps STRING in the correct number of Markdown backticks, padding
;;; with spaces if necessary.
(defun md-code (object)
  (let* ((string (if (stringp object)
                     object
                     (prin1-to-string object)))
         (n (length string)))
    (if (zerop n)
        ""
        (let ((max-ticks 0)
              (n-ticks 0))
          ;; Find the longest contiguous sequence of backticks in the string.
          (loop for char across string
                do (if (char= char #\`)
                       (incf n-ticks)
                       (setq n-ticks 0))
                do (setq max-ticks (max max-ticks n-ticks)))
          ;; The code delimiters must have one more backtick.
          (let* ((ticks (make-string (if (zerop max-ticks) 1 (1+ max-ticks))
                                     :initial-element #\`))
                 ;; Prevent backticks in STRING from touching the delimiters.
                 (left-pad (if (and (plusp n)
                                    (char= (char string 0) #\`))
                               " "
                               ""))
                 (right-pad (if (and (plusp n)
                                     (char= (char string (1- n)) #\`))
                                " "
                                "")))
            (format nil "~A~A~A~A~A"
                    ticks left-pad string right-pad ticks))))))

(defun md-emph (object &optional (escape t))
  (%md-emph-or-strong object t escape))

(defun md-strong (object &optional (escape t))
  (%md-emph-or-strong object nil escape))

(defun %md-emph-or-strong (object emphp escape)
  (let ((string (if (stringp object)
                    object
                    (prin1-to-string object))))
    (if (zerop (length string))
        ""
        (with-output-to-string (out)
          (write-string (if emphp "*" "**") out)
          (if escape
              (loop for char across string
                    do (when (char= char #\*)
                         (write-char #\\ out))
                       (write-char char out))
              (write-string string out))
          (write-string (if emphp "*" "**") out)))))

(defun md-indent (level stream)
  (loop repeat (* 4 level)
        do (write-char #\Space stream)))

(defun markdown-special-inline-char-p (char)
  (member char '(#\\ #\* #\_ #\` #\[ #\])))

(defun markdown-special-html-char-p (char)
  (member char '(#\< #\&)))

(defun markdown-special-block-char-p (char)
  (member char '(#\# #\Return #\Newline)))

(defun/auto escape-markdown
    (string &key (escape-inline t) (escape-mathjax t) (escape-html t)
            (escape-block t))
  "Backslash escape Markdown constructs in STRING.

  - If ESCAPE-INLINE, then escape the following characters:

          *_`[]\\
  - If ESCAPE-MATHJAX, then escape `$` characters.

  - If ESCAPE-HTML, then escape the following characters:

          <&

  - If ESCAPE-BLOCK, then escape whatever is necessary to avoid
    starting a new Markdown block (e.g. a paragraph, heading, etc)."
  (flet ((blank-line-until-p (pos)
           (loop for i downfrom (1- pos) downto 0
                 for char = (aref string i)
                 do (when (char= char #\Newline)
                      (return t))
                 do (unless (whitespacep char)
                      (return nil))
                 finally (return t))))
    (with-output-to-string (stream)
      (dotimes (i (length string))
        (let ((char (aref string i)))
          (cond ((and escape-html (char= char #\&))
                 ;; If there is no semicolon or there is no whitespace
                 ;; between & and ;, then it's not parsed as an
                 ;; entity, so don't escape it to reduce clutter.
                 (let ((semicolon-pos (position #\; string :start (1+ i)))
                       (whitespace-pos (position-if #'whitespacep string
                                                    :start (1+ i))))
                   (if (and semicolon-pos (or (null whitespace-pos)
                                              (< semicolon-pos whitespace-pos)))
                       (write-string "\\&" stream)
                       (write-char char stream))))
                (t
                 (when (or (and escape-inline
                                (markdown-special-inline-char-p char))
                           (and escape-mathjax
                                (char= char #\$))
                           (and escape-html
                                (markdown-special-html-char-p char))
                           (and escape-block
                                (markdown-special-block-char-p char)
                                (blank-line-until-p i)))
                   (write-char #\\ stream))
                 (write-char char stream))))))))

;;; Escape URL in explicit links [LABEL](URL).
(defun escape-markdown-for-link-destination (string)
  (with-output-to-string (out)
    (loop for char across string
          do (case char
               ((#\() (write-string "%28" out))
               ((#\)) (write-string "%29" out))
               ((#\Space) (write-string "%20" out))
               (t (write-char char out))))))

;;; Escape #\<, #\> and #\Space characters in URLs in Markdown <URL>.
(defun escape-markdown-for-angle-brackets (string)
  (with-output-to-string (out)
    (loop for char across string
          do (case char
               ((#\<)
                (write-string #.(urlencode "<") out))
               ((#\>)
                (write-string #.(urlencode ">") out))
               ((#\Space)
                (write-string #.(urlencode " ") out))
               (t
                (write-char char out))))))


;;;; Parse tree based Markdown fragments

(declaim (inline parse-tree-p))
(defun parse-tree-p (parse-tree tag)
  (and (listp parse-tree)
       (eq (first parse-tree) tag)))

(defmacro pt-get (parse-tree attr)
  `(getf (rest ,parse-tree) ,attr))

(defun code-fragment (string)
  `(:code ,(princ-to-string string)))

(defun indent-verbatim (tree)
  (assert (eq (first tree) :verbatim))
  `(:verbatim ,(prefix-lines "  " (second tree))))

(defun indent-code-block (tree)
  (assert (eq (first tree) '3bmd-code-blocks::code-block))
  (let ((tree (copy-list tree)))
    (setf (pt-get tree :content)
          (prefix-lines "  " (pt-get tree :content)))
    tree))

;;; This may return NIL (e.g for ((:EMPH "XXX")) :DEEMPH NIL).
(defun parse-tree-to-text (parse-tree &key deemph)
  (labels
      ((recurse (e)
         (cond ((stringp e)
                ;; "S" -> "S"
                e)
               ((and (listp e)
                     (or (stringp (first e))
                         (listp (first e))))
                ;; ("mgl-pax-test:" (:EMPH "test-variable")) =>
                ;; "mgl-pax-test:*test-variable*"
                (apply #'concatenate 'string (mapcar #'recurse e)))
               ;; Recurse into (:PLAIN ...)
               ((parse-tree-p e :plain)
                (format nil "~A" (recurse (rest e))))
               ;; (:EMPH "S") -> "*S*"
               ((and deemph (parse-tree-p e :emph))
                (format nil "*~A*" (recurse (rest e))))
               ;; (:CODE "S") -> "S"
               ((parse-tree-p e :code)
                (let ((string (second e)))
                  (cond ((starts-with-subseq "\\\\" string)
                         (recurse (subseq string 2)))
                        ((starts-with-subseq "\\" string)
                         (recurse (subseq string 1)))
                        (t
                         (recurse string)))))
               ((and (listp e) (null (cdr e)))
                (recurse (first e)))
               (t
                (return-from parse-tree-to-text nil)))))
    (recurse parse-tree)))


;;;; TeX

(defun inline-pandoc-latex (&rest args)
  `((:code ,(with-output-to-string (s)
              (dolist (arg args)
                (when arg
                  (write-string arg s)))))
    "{=latex}"))


;;;; Markdown parse tree transformation

;;; Perform a depth-first traversal of TREE. Call FN with the parent
;;; of each node and the node itself. FN returns three values:
;;;
;;; 1. New tree: It's substituted for the node.
;;;
;;; 2. Recurse flag: If recurse and the new tree is not a leaf, then
;;;    traversal recurses into the new tree.
;;;
;;; 3. Slice flag: If slice, then instead of adding the new tree as an
;;;    element to the transformed output (of the parent), add all
;;;    elements of the new tree (which must be a LIST). No slice is
;;;    like MAPCAR, slice is is MAPCAN.
(defun transform-tree (fn tree)
  (declare (optimize speed))
  (let ((fn (coerce fn 'function)))
    (labels ((foo (parent tree)
               (multiple-value-bind (new-tree recurse slice)
                   (funcall fn parent tree)
                 (assert (or (not slice) (listp new-tree)))
                 (if (or (atom new-tree)
                         (not recurse))
                     (values new-tree slice)
                     (values (loop for sub-tree in new-tree
                                   nconc (multiple-value-bind
                                               (new-sub-tree slice)
                                             (foo new-tree sub-tree)
                                           (if slice
                                               (copy-list new-sub-tree)
                                               (list new-sub-tree))))
                             slice)))))
      (foo nil tree))))

;;; When used as the FN argument to TRANSFORM-TREE, leave the tree
;;; intact except for subtrees (lists) whose CAR is in TAGS, whose
;;; transformation is deferred to FN. FN must return the three values
;;; TRANSFORM-TREE expects. If HANDLE-STRINGS, then FN is called with
;;; STRING nodes, too.
;;;
;;; If the CAR of a subtree is in STOP-TAGS, then the entire subtree
;;; is included in the output without further processing.
(defun defer-tag-handling (tags stop-tags handle-strings fn parent tree)
  (cond ((or (and (consp tree)
                  (member (first tree) tags))
             (and handle-strings
                  (stringp tree)))
         (funcall fn parent tree))
        ((and (consp tree)
              (member (first tree) stop-tags))
         (values tree nil nil))
        (t
         (values tree t nil))))

(defun map-markdown-parse-tree (tags stop-tags handle-strings fn parse-tree)
  (transform-tree (lambda (parent tree)
                    (defer-tag-handling tags stop-tags
                      handle-strings fn parent tree))
                  parse-tree))


;;; Call FN with STRING and START, END indices of @WORDS.
;;;
;;; FN must return two values: a replacement Markdown parse tree
;;; fragment (or NIL, if the subseq shall not be replaced), whether
;;; the replacement shall be sliced into the result list. MAP-WORDS
;;; returns a parse tree fragment that's a list of non-replaced parts
;;; of STRING and replacements (maybe sliced). Consecutive strings are
;;; concatenated.
(defun map-words (string fn)
  (declare (type string string))
  (let ((translated ())
        (n (length string))
        (start 0)
        (fn (coerce fn 'function)))
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

(defun delimiterp (char)
  (or (whitespacep char) (find char "()'`\"")))


;;;; Printing parse trees

(defmacro with-colorize-silenced (() &body body)
  `(let ((*trace-output* (make-broadcast-stream)))
     ,@body))

(defun print-markdown (parse-tree stream &key (format :markdown))
  (with-colorize-silenced ()
    (3bmd::print-doc-to-stream-using-format
     (preprocess-parse-tree-for-printing parse-tree format) stream format)))

(defun preprocess-parse-tree-for-printing (parse-tree format)
  (let ((stop-tags '(:code :verbatim 3bmd-code-blocks::code-block
                     :entity :image :explicit-link :math-inline-1
                     :math-inline-2 :math-inline-3)))
    (if (eq format :markdown)
        (map-markdown-parse-tree '() stop-tags t
                                 #'fudge-markdown-strings parse-tree)
        parse-tree)))

(defun fudge-markdown-strings (parent string)
  (declare (ignore parent))
  ;; KLUDGE: 3BMD parse-print roundtrip loses backslash escapes:
  ;;
  ;; (with-output-to-string (out)
  ;;   (3bmd::print-doc-to-stream-using-format
  ;;    (3bmd-grammar:parse-doc "[\\\\][x]")
  ;;    out :markdown))
  ;; => "[\\][x]"
  ;;
  ;; (3bmd-grammar:parse-doc "[\\][x]")
  ;; => ((:PLAIN "[" "]" (:REFERENCE-LINK :LABEL ("x") :TAIL NIL)))
  (when (ends-with #\\ string)
    (setq string (concatenate 'string string "\\")))
  ;; While we are here, also add a workaround for \$ not escaping math
  ;; on GitHub. On the other hand, GitHub parses a$x$ as normal text,
  ;; so let's add a zero-width space before $ characters at the
  ;; beginning of a word.
  (when (and (null *subformat*)
             (starts-with #\$ string))
    (let ((escape #.(ignore-errors (string (code-char 8203)))))
      (when escape
        (setq string (concatenate 'string escape string)))))
  string)

;;; Post-process the Markdown parse tree to make it prettier on w3m
;;; and maybe make relative links absolute.
(defun prepare-parse-tree-for-printing-to-w3m (parse-tree)
  (flet ((translate (parent tree)
           (cond ((eq (first tree) :code)
                  (if (parse-tree-p parent :strong)
                      tree
                      `(:strong ,tree)))
                 ((eq (first tree) :verbatim)
                  (values `((:raw-html #.(format nil "<i>~%"))
                            ,(indent-verbatim tree) (:raw-html "</i>"))
                          nil t))
                 (t
                  (values `((:raw-html #.(format nil "<i>~%"))
                            ,(indent-code-block tree)
                            (:raw-html "</i>"))
                          nil t)))))
    (map-markdown-parse-tree '(:code :verbatim 3bmd-code-blocks::code-block)
                             '() nil #'translate parse-tree)))

(defun prepare-parse-tree-for-printing-to-plain (parse-tree)
  (note @plain-strip-markup
    "
    - Markup for @MARKDOWN/EMPHASIS, @MARKDOWN/INLINE-CODE,
      @MARKDOWN/REFLINKs and @FENCED-CODE-BLOCKS is stripped from
      the output."
    (flet ((translate (parent tree)
             (declare (ignore parent))
             (ecase (first tree)
               ((:emph :strong :code)
                (values (rest tree) t t))
               ((:reference-link)
                (values (pt-get tree :label) t t))
               ((3bmd-code-blocks::code-block)
                (values `(:verbatim ,(pt-get tree :content)))))))
      (map-markdown-parse-tree '(:emph :strong :code :reference-link
                                 3bmd-code-blocks::code-block)
                               '() nil #'translate parse-tree))))


;;;; Parsing Markdown

(defun parse-markdown (string)
  (clean-up-parsed-parse-tree (parse-markdown-fast string)))

(defun clean-up-parsed-parse-tree (parse-tree)
  (transform-tree (lambda (parent tree)
                    (declare (ignore parent))
                    (if (and (listp tree)
                             (not (parse-tree-p tree :verbatim)))
                        (values (join-stuff-in-list tree) t nil)
                        tree))
                  parse-tree))

(defun join-stuff-in-list (tree)
  (let ((result ()))
    (dolist (element tree)
      (let ((prev (first result)))
        (cond
          ;; Join consecutive non-blank strings: "x" "y" -> "xy"
          ((and (stringp element) (not (blankp element))
                (stringp prev) (not (blankp prev)))
           (setf (first result) (concatenate 'string prev element)))
          ;; Join consecutive blank strings
          ((and (stringp element) (blankp element)
                (stringp prev) (blankp prev))
           (setf (first result) (concatenate 'string prev element)))
          ;; "CL:" (:EMPH "*FEATURES*") - > "CL:*FEATURES*"
          ((and (parse-tree-p element :emph)
                (stringp prev)
                (ends-with #\: prev)
                (= 2 (length (join-stuff-in-list element))))
           (setf (first result)
                 (format nil "~A*~A*" prev
                         (second (join-stuff-in-list element)))))
          (t
           (push element result)))))
    (reverse result)))

;;; We parse 3BMD-GRAMMAR::%BLOCKs like 3BMD-GRAMMAR:PARSE-DOC's
;;; currently commented out alternative implementation, which is much
;;; faster on long strings but perhaps slower on short strings. This
;;; still has a performance problem with large lists, which are
;;; processed in one block, hence the workaround in PAX-APROPOS*.
(defun parse-markdown-fast (string &key (start 0) end)
  (loop for start1 = start then pos
        for (%block pos) = (multiple-value-list
                            (%parse-md-block string :start start1 :end end
                                             :junk-allowed t))
        while %block
        collect %block
        while pos))

(defun %parse-md-block (string &key (start 0) end junk-allowed)
  (esrap:parse '#.(if (esrap:find-rule '3bmd-grammar::%block)
                      '3bmd-grammar::%block
                      ;; Make it work with old 3BMD.
                      '3bmd-grammar::block)
               string :start start :end end :junk-allowed junk-allowed))


;;;; Mixed I/O for Markdown strings and parse trees
;;;;
;;;; @EXTENDING-DOCUMENT expects Markdown to be output. Thus, Markdown
;;;; is our extension API. This makes some sense since docstrings are
;;;; written in Markdown anyway. As a consequence, we have to first
;;;; generate a complete Markdown output and REPRINT-IN-FORMAT if the
;;;; final output format is not Markdown. Note that most Markdown
;;;; strings are parsed and their parse tree is transformed (e.g. by
;;;; CODIFY-AND-LINK-TREE). Printing these parse trees as Markdown
;;;; only for REPRINT-IN-FORMAT to parse them slows DOCUMENT down 2x
;;;; (parsing the is main bottleneck).
;;;;
;;;; Thus, we allow parse trees to be output in the middle of Markdown
;;;; to avoid the re-parsing penalty. In the future, we could even
;;;; stream the output (modulo the reflink definitions).
;;;;
;;;; This is not as easy as it sounds because Markdown strings cannot
;;;; be parsed in arbitrary segmentations. For example, splitting a
;;;; list item and its indented child causes the child to be parsed as
;;;; an indented code block.

(declaim (inline %ensure-md-paragraph))
(defun %ensure-md-paragraph (stream)
  (write-string #.(with-standard-io-syntax
                    (format nil "~A " #\Soh))
                stream))

(defun/auto ensure-md-paragraph (stream)
  "Ensure that output previously written to STREAM is in a separate
  Markdown paragraph than the output that follows. Calling this
  function multiple times without writing anything else to STREAM in
  between is equivalent to calling it once."
  (%ensure-md-paragraph stream))

(defun write-markdown-pt (tree paragraphp level stream)
  (assert (<= 0 level))
  (when paragraphp
    (%ensure-md-paragraph stream))
  (when tree
    (with-standard-io-syntax
      (let ((*print-readably* nil))
        (format stream "~A~S ~S " #\Soh level tree)))
    (when paragraphp
      (%ensure-md-paragraph stream))))

(defun %read-soh (string start)
  (assert (char= (aref string start) #\Soh))
  (if (char= (aref string (1+ start)) #\Space)
      (values -1 () (+ start 2))
      (with-standard-io-syntax
        (multiple-value-bind (level pos)
            (read-from-string string t nil :start (1+ start))
          (multiple-value-bind (blocks pos)
              (read-from-string string t nil :start pos
                                ;; Without this, some implementations
                                ;; read the following space, some don't.
                                :preserve-whitespace t)
            (assert (char= (aref string pos) #\Space))
            (values level blocks (1+ pos)))))))

(defparameter *debug-parse-mixed-markdown* nil)

(defun parse-mixed-markdown (string)
  (let ((%blocks ())
        (%blocks-last nil)
        (paragraph-boundary t)
        (start 0))
    (labels
        ((add-blocks (blocks new-paragraph level)
           (when *debug-parse-mixed-markdown*
             (let ((*print-length* 4)
                   (*print-level* 8))
               (format t "pt: ~S ~S ~S~%" new-paragraph level blocks)))
           (if %blocks
               (add-to-forest %blocks-last blocks new-paragraph level)
               (setq %blocks blocks))
           (finish-update))
         (replace-last-block (string start end blocks)
           (when *debug-parse-mixed-markdown*
             (format t "md: ~S~%" (subseq string start end)))
           (if %blocks
               (setf (car %blocks-last) (car blocks)
                     (cdr %blocks-last) (cdr blocks))
               (setq %blocks blocks))
           (finish-update))
         (finish-update ()
           (setq %blocks-last (last (or %blocks-last %blocks)))
           (when *debug-parse-mixed-markdown*
             (let ((*print-length* 4)
                   (*print-level* 8))
               (format t "=> ~S~%" %blocks-last)))))
      (loop
        do (if (and (< start (length string))
                    (char= (aref string start) #\Soh))
               (multiple-value-bind (level blocks pos)
                   (%read-soh string start)
                 (cond ((eql level -1)
                        (setq paragraph-boundary t))
                       (blocks
                        (add-blocks blocks paragraph-boundary level)
                        (setq paragraph-boundary nil)))
                 (setq start pos))
               (let* ((end (position #\Soh string :start start))
                      (blocks (parse-md-after-tree
                               %blocks-last string :start start :end end
                               :paragraphp paragraph-boundary)))
                 (replace-last-block string start end blocks)
                 (setq paragraph-boundary
                       ;; FIXME: Some parses (e.g. fenced code blocks)
                       ;; end the paragraph in all cases.
                       (ends-with-blank-p string start end))
                 (setq start end)))
        while (and start (< start (length string)))))
    (clean-up-parsed-parse-tree %blocks)))

;;; Parse a Markdown STRING as if it followed what has been parsed as
;;; TREE.
(defun parse-md-after-tree (tree string &key (start 0) end (paragraphp t))
  (if (and paragraphp (zerop (pt-nesting-level (first tree))))
      ;; TREE does not affect the parse of STRING.
      (let ((parse-tree (parse-markdown-fast string :start start
                                             :end end)))
        (when *debug-parse-mixed-markdown*
          (format t "contextless md: ~S~%" (subseq string start end)))
        (close-open-paragraph tree)
        (append tree parse-tree))
      ;; Fall back to the inefficient case: print TREE, start a new
      ;; paragraph if PARAGRAPHP, add the relevant substring of
      ;; STRING, and parse the whole thing.
      ;;
      ;; There are very few cases where this branch currently
      ;; triggers. Within PAX, it's better to fix these cases by using
      ;; WRITE-MARKDOWN-PT instead of outputting Markdown strings.
      ;;
      ;; If this ever becomes an bottleneck, a possible optimization
      ;; is to print only a minimal structural equivalent of TREE
      ;; (i.e. what has the same nesting structure and maybe some
      ;; trailing strings if PARAGRAPHP is NIL), but then TREE is no
      ;; longer simply replaced by the new parse.
      (let ((context (tree-context tree paragraphp)))
        (when *debug-parse-mixed-markdown*
          (format t "context: ~S~%md: ~S~%" context (subseq string start end)))
        (parse-markdown-fast (concatenate 'string context
                                          (subseq string start end))))))

(defun tree-context (tree paragraphp)
  (let ((context (string-right-trim #.(format nil "~%")
                                    (with-output-to-string (s)
                                      (3bmd:print-doc-to-stream
                                       tree s :format :markdown)))))
    
    (if paragraphp
        (concatenate 'string context #.(format nil "~%~%"))
        context)))

(defun ends-with-blank-p (string start end)
  (when-let (pos2 (position #\Newline string :from-end t
                            :start start :end end))
    (when-let (pos1 (position #\Newline string :from-end t
                              :start start :end pos2))
      (blankp string :start pos1 :end pos2))))

;;; Add (destructively) the parse of a string (NEW-FOREST) to the
;;; parse of a previous string (FOREST) such that we get the parse of
;;; the concatenation of the two strings (with the latter indented by
;;; 4*LEVEL spaces).
;;;
;;; If not NEW-PARAGRAPH, then extend PT-OPEN-PARAGRAPH of the last
;;; tree in FOREST.
;;;
;;; Since this is called only from freshly READ object, we can mutate
;;; freely.
(defun add-to-forest (forest new-forest new-paragraph level)
  (assert forest)
  (when new-forest
    (let ((last-tree (first (last forest))))
      (cond ((not new-paragraph)
             (let ((open-paragraph (pt-open-paragraph (first (last forest))))
                   (first-new-tree (first new-forest)))
               (cond
                 ((null open-paragraph)
                  (nconc forest new-forest))
                 ;; (... (:PLAIN "X")) + (:PARAGRAPH "Y") =>
                 ;; (... (:PARAGRAPH "X" "Y"))
                 ((or (parse-tree-p first-new-tree :plain)
                      (parse-tree-p first-new-tree :paragraph))
                  (setf (first open-paragraph) (first first-new-tree))
                  (nconc open-paragraph (rest first-new-tree))
                  (nconc forest (rest new-forest)))
                 (t
                  (setf (first open-paragraph) :paragraph)
                  (nconc forest new-forest)))))
            ((zerop level)
             (let ((last-tree (first (last forest))))
               (when (parse-tree-p last-tree :plain)
                 (setf (first last-tree) :paragraph))
               (let ((tag (first last-tree)))
                 ;; ((:BULLET-LIST (:LIST-ITEM <X>))) + ((:BULLET-LIST
                 ;; (:LIST-ITEM <Y>))) => ((:BULLET-LIST (:LIST-ITEM
                 ;; <X>) (:LIST-ITEM <Y>)))
                 (when (or (eq tag :bullet-list)
                           (eq tag :counted-list))
                   (loop while (eq (first (first new-forest)) tag)
                         do (nconc last-tree (rest (first new-forest)))
                            (pop new-forest))))
               (nconc forest new-forest)))
            (t
             (let ((list-item (pt-last-list-item last-tree)))
               ;; FIXME: soften this?
               (assert list-item ()
                       "~@<Cannot add to ~S because ~
                       it is not a Markdown list.~:@>"
                       last-tree)
               (setf (cdr list-item)
                     (add-to-forest (cdr list-item) new-forest new-paragraph
                                    (1- level)))
               forest))))))

;;; Return the "open" :PLAIN or :PARAGRAPH subtree of TREE or NIL.
;;; This is the paragraph to which stuff could have been added had the
;;; string from which TREE was parsed been longer.
(defun pt-open-paragraph (tree)
  (cond ((atom tree)
         nil)
        ((or (parse-tree-p tree :plain)
             (parse-tree-p tree :paragraph))
         tree)
        (t
         (pt-open-paragraph (first (last tree))))))

(defun close-open-paragraph (forest)
  (when-let (open-paragraph (pt-open-paragraph (first (last forest))))
    (setf (first open-paragraph) :paragraph)))

(defun pt-last-list-item (tree)
  (when (or (parse-tree-p tree :bullet-list)
            (parse-tree-p tree :counted-list))
    (let ((elt (first (last tree))))
      (assert (parse-tree-p elt :list-item))
      elt)))

(defun pt-nesting-level (tree)
  (if (or (parse-tree-p tree :bullet-list)
          (parse-tree-p tree :counted-list))
      (1+ (let ((elt (first (last tree))))
            (assert (parse-tree-p elt :list-item))
            (pt-nesting-level (first (last elt)))))
      0))
