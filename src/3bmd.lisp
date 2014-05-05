(in-package #:3bmd)

(defvar *md-indent*)
(defvar *md-list-item*)
(defvar *md-in-paragraph*)
(defvar *md-paragraph-seen-p*)

(defmacro with-md-indent ((indent) &body body)
  `(let ((*md-indent* (+ *md-indent* ,indent)))
     ,@body))

(defun md-indent (stream)
  (loop repeat *md-indent* do (write-char #\Space stream)))

(defun ensure-paragraph (stream)
  (unless *md-in-paragraph*
    (when *md-paragraph-seen-p*
      (format stream "~%"))
    (md-indent stream)
    (setq *md-in-paragraph* t)
    (setq *md-paragraph-seen-p* t)))

(defun end-paragraph (stream)
  (format stream "~%")
  (setq *md-in-paragraph* nil))

(defun print-md-escaped (string stream)
  (loop for char across string
        do (when (and (not *in-code*) (find char "*_`"))
             (write-char #\\ stream))
           (write-char char stream)
           (when (char= char #\Newline)
             (md-indent stream))))

(defun print-md (string stream)
  (loop for char across string
        do (write-char char stream)
           (when (char= char #\Newline)
             (md-indent stream))))

(defmethod print-md-tagged-element ((tag (eql :heading)) stream rest)
  (setq *md-indent* 0)
  (ensure-paragraph stream)
  (loop repeat (getf rest :level) do (write-char #\# stream))
  (write-char #\Space stream)
  (map nil (lambda (a) (print-md-element a stream)) (getf rest :contents))
  (end-paragraph stream))

(defmethod print-md-tagged-element ((tag (eql :paragraph)) stream rest)
  (ensure-paragraph stream)
  (map nil (lambda (a) (print-md-element a stream)) rest)
  (end-paragraph stream))

(defmethod print-md-tagged-element ((tag (eql :block-quote)) stream rest)
  (with-md-indent (4)
    (map nil (lambda (a) (print-md-element a stream)) rest)))

(defmethod print-md-tagged-element ((tag (eql :plain)) stream rest)
  (ensure-paragraph stream)
  (map nil (lambda (a) (print-md-element a stream)) rest))

(defmethod print-md-tagged-element ((tag (eql :emph)) stream rest)
  (format stream "*")
  (map nil (lambda (a) (print-md-element a stream)) rest)
  (format stream "*"))

(defmethod print-md-tagged-element ((tag (eql :strong)) stream rest)
  (format stream "**")
  (map nil (lambda (a) (print-md-element a stream)) rest)
  (format stream "**"))

(defmethod print-md-tagged-element ((tag (eql :link)) stream rest)
  (format stream "<")
  (map nil (lambda (a) (print-md-element a stream)) rest)
  (format stream ">"))

(defmethod print-md-tagged-element ((tag (eql :mailto)) stream rest)
  (format stream "<")
  (map nil (lambda (a)
             (if (and (stringp a)
                      (alexandria:starts-with-subseq "mailto:" "mailto:dsafds"))
                 (print-md-element (subseq a (length "mailto:")) stream)
                 (print-md-element a stream)))
       rest)
  (format stream ">"))

(defmethod print-md-tagged-element ((tag (eql :explicit-link)) stream rest)
  (format stream "[")
  (map nil (lambda (a) (print-md-element a stream)) (getf rest :label))
  (format stream "](~a~@[ ~s~])" (getf rest :source) (getf rest :title)))

(defmethod print-md-tagged-element ((tag (eql :reference-link)) stream rest)
  (format stream "[")
  (map nil (lambda (a) (print-md-element a stream)) (getf rest :label))
  (format stream "][~a]" (or (getf rest :definition) "")))

(defmethod print-md-tagged-element ((tag (eql :image)) stream rest)
  (format stream "!")
  (mapcar (lambda (a) (print-md-element a stream)) rest))

(defmethod print-md-tagged-element ((tag (eql :counted-list)) stream rest)
  (let ((*md-list-item* 1))
    (map nil (lambda (a) (print-md-element a stream)) rest)))

(defmethod print-md-tagged-element ((tag (eql :bullet-list)) stream rest)
  (let ((*md-list-item* "-"))
    (map nil (lambda (a) (print-md-element a stream)) rest)))

(defmethod print-md-tagged-element ((tag (eql :list-item)) stream rest)
  (ensure-paragraph stream)
  (cond ((numberp *md-list-item*)
         (format stream "~D. " *md-list-item*)
         (incf *md-list-item*))
        (t
         (format stream "~a " *md-list-item*)))
  (print-md-element (first rest) stream)
  (with-md-indent (4)
    (map nil (lambda (a) (print-md-element a stream)) (rest rest))))

(defmethod print-md-tagged-element ((tag (eql :line-break)) stream rest)
  (print-md-escaped "  
" stream))

(defmethod print-md-tagged-element ((tag (eql :horizontal-rule)) stream rest)
  (ensure-paragraph stream)
  (print-md-escaped "---" stream)
  (end-paragraph stream))

(defmethod print-md-tagged-element ((tag (eql :html)) stream rest)
  (format stream "~{~a~}" rest))

(defmethod print-md-tagged-element ((tag (eql :raw-html)) stream rest)
  (format stream "~{~a~}" rest))

(defmethod print-md-tagged-element ((tag (eql :entity)) stream rest)
  (format stream "~{~a~}" rest))

(defun remove-ending-newline (string)
  (if (alexandria:ends-with #\Newline string)
      (subseq string 0 (1- (length string)))
      string))

(defmethod print-md-tagged-element ((tag (eql :verbatim)) stream rest)
  (with-md-indent (4)
    (ensure-paragraph stream)
    (map nil (lambda (a) (print-md a stream)) (butlast rest))
    (print-md (remove-ending-newline (first (last rest))) stream)
    (end-paragraph stream)))

(defmethod print-md-tagged-element ((tag (eql '3bmd-code-blocks::code-block))
                                    stream rest)
  (ensure-paragraph stream)
  (format stream "```~a~%~a~%```" (getf rest :lang) (getf rest :content))
  (end-paragraph stream))

;;; :UNESCAPED-STRING is not produced by the parser, but applications
;;; that manipulate the parse tree might find it convenient.
(defmethod print-md-tagged-element ((tag (eql :unescaped-string)) stream rest)
  (map nil (lambda (a) (print-md a stream)) rest))

(defun max-n-consecutive-char (char string)
  (let ((n 0)
        (max 0))
    (loop for char-2 across string
          do (cond ((char= char char-2)
                    (incf n)
                    (setq max (max n max)))
                   (t
                    (setq n 0))))
    max))

(defun max-n-consecutive-backticks (parse-tree)
  (let ((n 0))
    (labels ((foo (parse-tree)
               (if (stringp parse-tree)
                   (setq n (max n (max-n-consecutive-char #\` parse-tree)))
                   (map nil #'foo parse-tree))))
      (foo parse-tree))
    n))

(defmethod print-md-tagged-element ((tag (eql :code)) stream rest)
  (let ((n (max-n-consecutive-backticks rest)))
    (loop repeat (1+ n) do (write-char #\` stream))
    (let ((*in-code* t))
      (map nil (lambda (a) (print-md-element a stream)) rest))
    (loop repeat (1+ n) do (write-char #\` stream))))

(defmacro define-smart-quote-md-translation (name replacement)
  `(defmethod print-md-tagged-element ((tag (eql ,name)) stream rest)
     (write-string ,replacement stream)))

(define-smart-quote-md-translation :single-quoted "'")
(define-smart-quote-md-translation :double-quoted "\"")
(define-smart-quote-md-translation :en-dash "--")
(define-smart-quote-md-translation :em-dash "---")
(define-smart-quote-md-translation :left-right-single-arrow "<->")
(define-smart-quote-md-translation :left-single-arrow "<-")
(define-smart-quote-md-translation :right-single-arrow "->")
(define-smart-quote-md-translation :left-right-double-arrow "<=>")
(define-smart-quote-md-translation :left-double-arrow "<=")
(define-smart-quote-md-translation :right-double-arrow "=>")
(define-smart-quote-md-translation :ellipsis "...")

(defmethod print-md-tagged-element ((tag (eql :reference)) stream rest)
  (format stream "~%[~a]: ~a ~@[~s~]~%" (first (getf rest :label))
          (getf rest :source) (getf rest :title)))

(defmethod print-md-element ((elem (eql :apostrophe)) stream)
  (write-string "'" stream))

(defmethod print-md-element ((elem string) stream)
  (print-md-escaped elem stream))

(defmethod print-md-element ((elem cons) stream)
  (if (symbolp (car elem))
      (print-md-tagged-element (car elem) stream (cdr elem))
      (error "unknown cons? ~s" elem)))

(defmethod print-doc-to-stream-using-format (doc stream
                                             (format (eql :markdown)))
  (let ((*references* (extract-refs doc))
        (*md-indent* 0)
        (*md-in-paragraph* nil)
        (*md-paragraph-seen-p* nil))
    (dolist (element doc)
      (print-md-element element stream))))

#|

(defparameter *test-cases*
  '("\\*notemph\\* \\_notemph2\\_ \\ \\`notcode\\`
"
    "### heading

- outer item

    dsfdsf *emph* **strong** ***strongemph***
    kjdsf [asdf](#xxx \"*title*\") <http://quotenil.com> <mega@retes.hu>

    - b1

        inside `````co````de`````

            codeblock 234
            sdfkj kjsdf

    - b2

    dsfdsf
    llll

    1. xxx

    2. yyy

    ---

    para [ref-link][sfd] ![image](#dsfa \"img\") ![img][32]
    sddsf <a href=\"#xxx\">ddd</a> <!-- comment -->

em-dash -- en-dash --- lrsa <-> <- -> <=> <= => ... foo's

[id1]: http://some.link.com/ \"with some title\"

plain
"))

(defun check-roundtrip (original)
  (let* ((reconstruction (with-output-to-string (out)
                           (let ((3bmd-grammar:*smart-quotes* t))
                             (3bmd:parse-string-and-print-to-stream
                              original out :format :markdown))))
         (position (mismatch reconstruction original)))
    (when position
      (format t "Mismatch found. Common part:~%~S~%-- Original:~%~S~%~
                -- Reconstruction:~%~S~%"
              (subseq original 0 position)
              (subseq original position)
              (subseq reconstruction position))
      (error "Mismatch found at position ~S." position))))

(map nil #'check-roundtrip *test-cases*)

(let ((3bmd-grammar:*smart-quotes* t))
  (3bmd:parse-string-and-print-to-stream "\\'" *standard-output*))

(3bmd-grammar:parse-doc "[id1]: http://some.link.com/ \"with some title\"")

|#
