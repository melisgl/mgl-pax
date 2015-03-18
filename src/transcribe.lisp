;;;; TODO
;;;;
;;;; - Maybe implement inline commands that can change prefixes and
;;;;   other parameters.
;;;;
;;;; - special comment?

(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @mgl-pax-transcript (:title "Transcripts")
  "What are transcripts for? When writing a tutorial, one often wants
  to include a REPL session with maybe a few defuns and a couple of
  forms whose output or return values are shown. Also, in a function's
  docstring an example call with concrete arguments and return values
  speaks volumes. A transcript is a text that looks like a repl
  session, but which has a light markup for printed output and return
  values, while no markup (i.e. prompt) for lisp forms. The PAX
  transcripts may include output and return values of all forms, or
  only selected ones. In either case the transcript itself can be
  easily generated from the source code.

  The main worry associated with including examples in the
  documentation is that they tend to get out-of-sync with the code.
  This is solved by being able to parse back and update transcripts.
  In fact, this is exactly what happens during documentation
  generation with PAX. Code sections tagged `\"cl-transcript\"` are
  retranscribed and checked for inconsistency (that is, any difference
  in output or return values). If the consistency check fails, an
  error is signalled that includes a reference to the object being
  documented.

  Going beyond documentation, transcript consistency checks can be
  used for writing simple tests in a very readable form. For example:

  ```cl-transcript
  (+ 1 2)
  => 3
  
  (values (princ :hello) (list 1 2))
  .. HELLO
  => :HELLO
  => (1 2)
  ```

  All in all, transcripts are a handy tool especially when combined
  with the Emacs support to regenerate them and with
  PYTHONIC-STRING-READER and its triple-quoted strings that allow one
  to work with nested strings with less noise. The triple-quote syntax
  can be enabled with:

      (in-readtable pythonic-string-syntax)"
  (@mgl-pax-transcript-emacs-integration section)
  (@mgl-pax-transcript-api section))


(defsection @mgl-pax-transcript-api (:title "Transcript API")
  (transcribe function)
  (*prefixes* variable)
  (transcription-error condition)
  (transcription-consistency-error condition)
  (transcription-output-consistency-error condition)
  (transcription-values-consistency-error condition))

(defparameter *prefixes*
  '((:output "..")
    (:commented-output ";..")
    ;; To give precedence to these no value markers, they are listed
    ;; before :READABLE and :COMMENTED-READABLE.
    (:no-value "=> ; No value")
    (:commented-no-value ";=> ; No value")
    (:readable "=>")
    (:commented-readable ";=>")
    ;; There is no need for :UNREADABLE-CONTINUATION because we READ
    ;; an entire form following this prefix.
    (:commented-readable-continuation ";->")
    (:unreadable "==>")
    (:commented-unreadable ";==>")
    (:unreadable-continuation "-->")
    (:commented-unreadable-continuation ";-->"))
  "The default prefixes used by TRANSCRIBE for reading and writing
  lines containing output and values of an evaluated form. When
  writing, an extra space is added automatically if the line to be
  prefixed is not empty. Similarly, the first space following the
  prefix discarded when reading.")

(defun transcribe (input output &key update-only
                   (comment :keep)
                   (include-no-output update-only)
                   (include-no-value update-only)
                   (echo t) check-consistency
                   (input-prefixes *prefixes*)
                   (output-prefixes *prefixes*))
  "Read forms from INPUT and write them (iff ECHO) to OUTPUT followed
  by any output and return values produced by calling EVAL on the
  form. INPUT can be a stream or a string, while OUTPUT can be a
  stream or NIL in which case transcription goes into a string. The
  return value is the OUTPUT stream or the string that was
  constructed.

  A simple example is this:

  ```cl-transcript
  (transcribe \"(princ 42) \" nil)
  => \"(princ 42)
  .. 42
  => 42
  \"
  ```

  However, it may be a bit confusing since this documentation uses
  TRANSCRIBE markup syntax in this very example, so let's do it
  differently. If we have a file with these contents:

      (values (princ 42) (list 1 2))

  they are transcribed to:

      (values (princ 42) (list 1 2))
      .. 42
      => 42
      => (1 2)

  Output to all standard streams is captured and printed with
  the :OUTPUT prefix (defaults to `\"..\"`, looked up in
  OUTPUT-PREFIXES). The return values above are printed with
  the :READABLE prefix (`\"=>\"`). Note how these prefixes are always
  printed on a new line to facilitate parsing.

  TRANSCRIBE is able to parse its own output (using INPUT-PREFIXES).
  If we transcribe the previous output above, we get it back exactly.
  However, if we remove all output markers, leave only a placeholder
  value marker and pass :UPDATE-ONLY T with source:

      (values (princ 42) (list 1 2))
      =>

  we get this:

      (values (princ 42) (list 1 2))
      => 42
      => (1 2)

  With UPDATE-ONLY, printed output of a form is only transcribed if
  there were output markers in the source. Similarly, with
  UPDATE-ONLY, return values are only transcribed if there were value
  markers in the source.

  If the form produces no output or returns no values then whether
  output and values are transcribed is controlled by INCLUDE-NO-OUTPUT
  and INCLUDE-NO-VALUE, respectively. By default, neither is on so:

      (values)
      ..
      =>

  is transcribed to

      (values)

  With UPDATE-ONLY true, we probably wouldn't like to lose those
  markers since they were put there for a reason. Hence, with
  UPDATE-ONLY, INCLUDE-NO-OUTPUT and INCLUDE-NO-VALUE default to true.
  So with UPDATE-ONLY the above example is transcribed to:

      (values)
      ..
      => ; No value

  where `\"=> ; No value\"` is the :NO-VALUE prefix.

  If CHECK-CONSISTENCY is true, then TRANSCRIBE signals a continuable
  TRANSCRIPTION-CONSISTENCY-ERROR whenever a form's output is
  different between the source and the evaluation. Similary, for
  values, a consistency error is signalled if a value read from the
  source does not print as the same string as the value returned by
  EVAL. This allows readable values to be hand-indented without
  failing consistency checks:

      (list 1 2)
      => (1
            2)

  The above scheme involves READ, so unreadable values cannot be
  treated the same. In fact, unreadable values must even be printed
  differently for transcribe to be able to read them back:

      (defclass some-class () ())
      
      (defmethod print-object ((obj some-class) stream)
        (print-unreadable-object (obj stream :type t)
          (format stream \"~%~%end\")))

      (make-instance 'some-class)
      ==> #<SOME-CLASS 
      -->
      --> end>

  `\"==>\"` is UNREADABLE-VALUE-PREFIX and `\"-->\"` is
  UNREADABLE-VALUE-CONTINUATION-PREFIX. As with outputs, a consistency
  check between a unreadable value from the source and the value from
  EVAL is performed with STRING=. That is, the value from EVAL is
  printed to a string and compared to the source value. Hence, any
  change to unreadable values will break consistency checks. This is
  most troublesome with instances of classes with the default
  PRINT-OBJECT method printing the memory address. There is currently
  no remedy for that, except for customizing PRINT-OBJECT or not
  transcribing that kind of stuff.

  Finally, one may want to produce a transcript that's can be
  evaluated. If the COMMENT argument is T, then instead of :OUTPUT
  the :COMMENTED-OUTPUT prefix is used. Similar translations are
  peformed for other prefixes. For example:

      (make-instance 'some-class)
      ;==> #<SOME-CLASS
      ;-->
      ;--> end>

      (list 1 2)
      ;=> (1
      ;->    2)

  If COMMENT is false, then the transcribed output will use the
  non-commented prefixes. If COMMENT is :KEEP (the default), then an
  effort will be made to maintain the commenting style of the input."
  (write-transcript (read-transcript input :prefixes input-prefixes)
                    output
                    :update-only update-only
                    :check-consistency check-consistency
                    :include-no-output include-no-output
                    :include-no-value include-no-value
                    :echo echo
                    :comment comment
                    :prefixes output-prefixes))


;;;; Prefix utilities

(defun find-prefix (id)
  (or (second (find id *prefixes* :key #'first))
      (error "Cannot find prefix with id ~S~%" id)))

(defun continuation-prefix-id-p (id)
  (or (eq id :commented-readable-continuation)
      (eq id :unreadable-continuation)
      (eq id :commented-unreadable-continuation)))

(defun start-prefix (id)
  (find-prefix
   (ecase id
     ((:commented-readable-continuation) :commented-readable)
     ((:unreadable-continuation) :unreadable)
     ((:commented-unreadable-continuation) :commented-unreadable))))

(defun continuation-prefix (id)
  (find-prefix
   (ecase id
     ((:commented-readable) :commented-readable-continuation)
     ((:unreadable) :unreadable-continuation)
     ((:commented-unreadable) :commented-unreadable-continuation))))

(defun commented-prefix-id (id)
  (ecase id
    ((:output) :commented-output)
    ((:readable) :commented-readable)
    ((:unreadable) :commented-unreadable)))

(defun commented-prefix-id-p (id)
  (member id '(:commented-output :commented-readable :commented-unreadable)))

(defun match-prefixes (line)
  (values-list (find-if (lambda (entry)
                          (alexandria:starts-with-subseq (second entry) line))
                        *prefixes*)))


;;;; READ-TRANSCRIPT constructs a parse tree that's fed into
;;;; WRITE-TRANSCRIPT by TRANSCRIBE. This parse tree is simply called
;;;; _transcript_ and is a list of transcript commands.
;;;;
;;;; A transcript command, or simply _command_, is the parsed
;;;; representation of a single top-level form together with its
;;;; output and values. The following transcript of one command:
;;;;
;;;;     ;;; This is a comment before the form.
;;;;     (values (find-package :keyword) (princ 42))
;;;;     .. 42
;;;;     ==> #<PACKAGE "KEYWORD">
;;;;     => 42
;;;;
;;;; is parsed as:
;;;;
;;;;     ((((VALUES (FIND-PACKAGE :KEYWORD) (PRINC 42))
;;;;        ";;; This is a comment before the form.
;;;;     (values (find-package :keyword) (princ 42))")
;;;;       (:OUTPUT "42")
;;;;       (:UNREADABLE "#<PACKAGE \"KEYWORD\">")
;;;;       (:READABLE (42 "42"))))
;;;;
;;;; Note how the command contains both the sexp and the original
;;;; string (including preceeding comments). It also has a variable
;;;; number of output (0 or 1) and value captures.

(defun command-form (command)
  (first (first command)))

(defun command-string (command)
  (second (first command)))

(defun command-captures (command)
  (rest command))

(defun command-output-capture (command)
  (let ((captures
          (remove-if-not #'output-capture-p (command-captures command))))
    (when (< 1 (length captures))
      (transcription-error* "Multiple output captures found."))
    (first captures)))

(defun command-value-captures (command)
  (remove-if-not #'value-capture-p (command-captures command)))

(defun check-command-values (command)
  (when (and (some #'no-value-capture-p (command-captures command))
             (< 1 (count-if #'value-capture-p (command-captures command))))
    (transcription-error* "Found no-value-marker and other values.")))

(defun capture-id (capture)
  (first capture))

(defun capture-value (capture)
  (second capture))

(defun filter-captures (captures &rest ids)
  (remove-if-not (lambda (capture)
                   (member (capture-id capture) ids))
                 captures))

(defun output-capture-p (capture)
  (member (capture-id capture) '(:output :commented-output)))

(defun output-string (output-capture)
  (assert (output-capture-p output-capture))
  (capture-value output-capture))

(defun value-capture-p (capture)
  (or (no-value-capture-p capture)
      (readable-capture-p capture)
      (unreadable-capture-p capture)))

(defun no-value-capture-p (capture)
  (member (capture-id capture) '(:no-value :commented-no-value)))

(defun readable-capture-p (capture)
  (member (capture-id capture) '(:readable :commented-readable)))

(defun readable-object (readable-capture)
  (assert (readable-capture-p readable-capture))
  (first (capture-value readable-capture)))

(defun readable-string (readable-capture)
  (assert (readable-capture-p readable-capture))
  (second (capture-value readable-capture)))

(defun unreadable-capture-p (capture)
  (member (capture-id capture) '(:unreadable :commented-unreadable)))

(defun unreadable-string (unreadable-capture)
  (assert (unreadable-capture-p unreadable-capture))
  (capture-value unreadable-capture))

(defun transcript-has-commented-p (transcript)
  (some (lambda (command)
          (some (lambda (capture)
                  (commented-prefix-id-p (capture-id capture)))
                (command-captures command)))
        transcript))


;;;; READ-TRANSCRIPT implementation

(defmacro with-input-stream ((stream input) &body body)
  `(call-with-input-stream (lambda (,stream)
                             ,@body)
                           ,input))

(defun call-with-input-stream (fn input)
  (cond ((typep input 'stream)
         ;; There is no way to guarantee that FILE-POSITION will work
         ;; on a stream so let's just read the entire INPUT into a
         ;; string.
         (with-input-from-string (stream (read-stream-into-string input))
           (funcall fn stream)))
        ((typep input 'string)
         (with-input-from-string (input input)
           (funcall fn input)))
        (t
         ;; CHECK-TYPE in READ-TRANSCRIPT makes this impossible.
         (assert nil))))

(defmacro with-load-environment ((stream) &body body)
  (alexandria:once-only (stream)
    `(let* ((*readtable* *readtable*)
            (*package* *package*)
            (*load-pathname* (handler-case (pathname ,stream)
                               (error () nil)))
            (*load-truename* (when *load-pathname*
                               (handler-case (truename ,stream)
                                 (file-error () nil))))
            #+sbcl
            (sb-c::*policy* sb-c::*policy*))
       ,@body)))

(defun read-transcript (input &key (prefixes *prefixes*))
  (check-type input (or stream string))
  (with-input-stream (stream input)
    (with-load-environment (stream)
      (let ((*prefixes* prefixes)
            (transcript ())
            (partial-line-p nil)
            ;; file position of the beginning of LINE
            (file-position (file-position stream)))
        (multiple-value-bind (line missing-newline-p)
            (read-line stream nil nil)
          (handler-case
              (loop while line do
                (multiple-value-bind (prefix-id prefix)
                    (and (not partial-line-p) (match-prefixes line))
                  (let ((match-length (length prefix))
                        value
                        n-lines-read
                        file-position-1)
                    (file-position stream (+ file-position match-length))
                    (multiple-value-setq
                        (value n-lines-read
                               line missing-newline-p file-position-1
                               partial-line-p)
                      (parse-transcript-element stream prefix-id line
                                                match-length))
                    ;; Forms create a new entry, form output and values are
                    ;; pushed into that entry.
                    (cond (prefix-id
                           (when (endp transcript)
                             (transcription-error* "No open form."))
                           (setf (cdr (first transcript))
                                 (append (cdr (first transcript))
                                         (list (list prefix-id value))))
                           (check-command-values (first transcript)))
                          (t
                           (push (list value) transcript)))
                    (setq file-position file-position-1))))
            (transcription-error (e)
              (apply #'transcription-error
                     stream file-position
                     (second (first (first transcript)))
                     (transcription-error-message e)
                     (transcription-error-message-args e)))))
        (nreverse transcript)))))

(defun parse-transcript-element (stream prefix-id first-line match-length)
  (cond ((null prefix-id)
         (parse-form stream))
        ((eq prefix-id :output)
         (parse-prefixed stream :output))
        ((eq prefix-id :commented-output)
         (parse-prefixed stream :commented-output))
        ((eq prefix-id :readable)
         ;; It may be that there is no value following the :READEABLE
         ;; prefix, because it was put there as a placeholder for
         ;; UPDATE-ONLY to fill in.
         (cond ((every #'whitespacep (subseq first-line match-length))
                (read-line stream nil nil)
                (values-list `(,(list nil nil)
                               1
                               ,@(multiple-value-list
                                  (read-line* stream nil nil))
                               nil)))
               (t
                (parse-readable stream))))
        ((eq prefix-id :commented-readable)
         (parse-commented-readable stream))
        ((eq prefix-id :unreadable)
         (parse-prefixed stream :unreadable-continuation))
        ((eq prefix-id :commented-unreadable)
         (parse-prefixed stream :commented-unreadable-continuation))
        ((or (eq prefix-id :no-value)
             (eq prefix-id :commented-no-value))
         (when (< match-length (length first-line))
           (transcription-error* "Trailing junk after ~S."
                                 (find-prefix prefix-id)))
         (read-line stream nil nil)
         (values-list `(nil
                        1
                        ,@(multiple-value-list
                           (read-line* stream nil nil))
                        nil)))
        ((continuation-prefix-id-p prefix-id)
         (transcription-error* "Prefix ~S must be preceeded by ~S."
                               (find-prefix prefix-id)
                               (start-prefix prefix-id)))
        (t
         (transcription-error* "Unknown prefix id ~S in *PREFIXES*."
                               prefix-id))))

(defun parse-form (stream)
  (let ((form-and-string (read-form-and-string stream 'eof
                                               :preserve-whitespace-p t)))
    (cond ((eq form-and-string 'eof) nil)
          (t
           (let ((at-bol-p (skip-white-space-till-end-of-line stream)))
             (values-list `(,form-and-string
                            ;; FIXME: N-LINES-READ
                            1
                            ,@(multiple-value-list (read-line* stream nil nil))
                            ,(not at-bol-p))))))))

(defun parse-prefixed (stream prefix-id)
  (read-prefixed-lines stream (find-prefix prefix-id)
                       :first-line-prefix ""))

(defun parse-readable (stream)
  ;; We are after a readable prefix. Eat a single space if any so that
  ;; "=>1" is parsed the same as "=> 1".
  (when (eql (peek-char nil stream nil nil) #\Space)
    (read-char stream))
  (let ((form-and-string (read-form-and-string stream 'eof
                                               :preserve-whitespace-p t)))
    (when (eq (first form-and-string) 'eof)
      (transcription-error* "Unexpected EOF while parsing readable value."))
    (unless (skip-white-space-till-end-of-line stream)
      (transcription-error* "Trailing junk after readable value ~S."
                            (second form-and-string)))
    (values-list `(,form-and-string
                   ;; FIXME: N-LINES-READ
                   1
                   ,@(multiple-value-list (read-line* stream nil nil))
                   nil))))

(defun parse-commented-readable (stream)
  (multiple-value-bind (string n-lines-read
                        next-line missing-newline-p file-position)
      (read-prefixed-lines stream
                           (find-prefix :commented-readable-continuation)
                           :first-line-prefix "")
    ;; FIXME: eof?
    (let ((form (first (with-input-from-string (stream string)
                         (read-form-and-string stream nil)))))
      (values (list form string) n-lines-read
              next-line missing-newline-p file-position
              nil))))

;;; Read a sexp from STREAM or return EOF. The second value is a
;;; string of all of the characters that were read even if EOF
;;; (whitespace, comments).
(defun read-form-and-string (stream eof &key preserve-whitespace-p)
  (let* ((old-file-position (file-position  stream))
         (form (handler-case
                   (funcall (if preserve-whitespace-p
                                #'read-preserving-whitespace
                                #'read)
                            stream nil eof nil)
                 (error (e)
                   (transcription-error* "READ failed with error:~%~A"
                                         (princ-to-string e)))))
         (new-file-position (file-position  stream))
         (n (- new-file-position old-file-position))
         (form-as-string (make-string n)))
    (file-position  stream old-file-position)
    (read-sequence form-as-string stream)
    (assert (= (file-position  stream) new-file-position))
    (list form form-as-string)))

;;; Read all whitespace chars until the first non-whitespace char or
;;; the end of the line. Return :EOF on EOF, T on hitting the end of
;;; the line, and NIL if on running into a non-whitespace char.
(defun skip-white-space-till-end-of-line (stream)
  (loop for char = (peek-char nil stream nil nil)
        do (unless char
             (return :eof))
           (unless (whitespacep char)
             (return nil))
           (read-char stream nil nil)
           (when (char= char #\Newline)
             (return t))))


;;;; WRITE-TRANSCRIPT implementation

(defmacro with-output-stream ((stream output) &body body)
  `(call-with-output-stream (lambda (,stream)
                              ,@body)
                            ,output))

(defun call-with-output-stream (fn output)
  (if output
      (funcall fn output)
      (with-output-to-string (output)
        (funcall fn output))))

(defun write-transcript (transcript output &key update-only
                         (comment :keep)
                         (include-no-output update-only)
                         (include-no-value update-only)
                         (echo t) check-consistency
                         (prefixes *prefixes*))
  (check-type output (or stream null))
  (check-type comment (member nil :keep t))
  (with-output-stream (stream output)
    (let ((*prefixes* prefixes)
          (comment (if (and (eq comment :keep)
                            (transcript-has-commented-p transcript))
                       :keep-t
                       comment)))
      (dolist (command transcript)
        (let ((form (command-form command))
              (form-as-string (command-string command)))
          (when echo
            (format stream "~A" (command-string command))
            (unless (eq form 'eof)
              (terpri stream)))
          (unless (eq form 'eof)
            (multiple-value-bind (form-output form-values)
                (eval-and-capture form)
              (let ((output-capture (command-output-capture command)))
                (when (and check-consistency output-capture)
                  (check-output-consistency
                   nil form-as-string form-output
                   (output-string output-capture)))
                (transcribe-output stream form-output output-capture
                                   comment update-only include-no-output))
              (let ((value-captures (command-value-captures command)))
                (when check-consistency
                  (check-values-consistency nil form-as-string
                                            form-values value-captures))
                (transcribe-values stream form-values value-captures comment
                                   update-only include-no-value)))))))))

(defun readable-object-p (object)
  (null
   (nth-value 1 (ignore-errors
                 (values (read-from-string (prin1-to-string object)))))))

(defun check-output-consistency (stream form-as-string output captured-output)
  (check-type output string)
  (check-type captured-output (or string null))
  (unless (string= output (or captured-output ""))
    (consistency-error
     'transcription-output-consistency-error
     stream form-as-string
     "Inconsistent output found.~%~%Source: ~:_~S~%~%Output: ~:_~S~%"
     captured-output output)))

(defun check-values-consistency (stream form-as-string values value-captures)
  (when value-captures
    (let ((value-captures (if (and (= 1 (length value-captures))
                                   (no-value-capture-p (first value-captures)))
                              ()
                              value-captures)))
      (cond ((/= (length values) (length value-captures))
             (consistency-error
              'transcription-values-consistency-error
              stream form-as-string
              "Source had ~S return values ~:_while there are actually ~S."
              (length value-captures) (length values)))
            (t
             (loop for value in values
                   for value-capture in value-captures
                   do (check-value-consistency stream form-as-string
                                               value value-capture)))))))

(defmacro with-transcription-syntax (() &body body)
  (alexandria:with-gensyms (package)
    `(let ((,package *package*))
       (with-standard-io-syntax
         (let ((*package* ,package)
               (*print-readably* nil)
               (*print-pretty* t)
               (*print-right-margin* 72))
           ,@body)))))

(defun check-value-consistency (stream form-as-string value value-capture)
  (assert (not (no-value-capture-p value-capture)))
  (flet ((stringify (object)
           (with-transcription-syntax ()
             (prin1-to-string object))))
    (let ((value-readable-p (readable-object-p value)))
      (cond ((and value-readable-p
                  (not (readable-capture-p value-capture)))
             (consistency-error
              'transcription-values-consistency-error
              stream form-as-string
              "Unreadable value ~:_~S ~:_in source became readable ~:_~S."
              (unreadable-string value-capture) value))
            ((and (not value-readable-p)
                  (not (unreadable-capture-p value-capture)))
             (consistency-error
              'transcription-values-consistency-error
              stream form-as-string
              "Readable value ~:_~S~:_ in source became unreadable ~:_~S."
              (readable-string value-capture) value))
            ;; At this point we know that both are readable or both are
            ;; unreadable.
            (value-readable-p
             (unless (string= (stringify value)
                              (stringify (readable-object value-capture)))
               (consistency-error
                'transcription-values-consistency-error
                stream form-as-string
                "Readable value ~:_~S ~:_in source does not print the ~
                same as ~:_~S." (stringify (readable-object value-capture))
                (stringify value))))
            ((not (string= (stringify value) (unreadable-string value-capture)))
             (consistency-error
              'transcription-values-consistency-error
              stream form-as-string
              "Unreadable value ~:_~S ~:_in source does not print the ~
              same as ~:_~S." (unreadable-string value-capture)
              (stringify value)))))))

(defun eval-and-capture (form)
  (let* ((buffer (make-array 0 :element-type 'character
                             :fill-pointer 0 :adjustable t))
         (values (multiple-value-list
                  (with-output-to-string (output buffer)
                    (with-transcription-syntax ()
                      (let ((*standard-output* output)
                            (*error-output* output)
                            (*trace-output* output)
                            (*debug-io* output)
                            (*query-io* output)
                            (*terminal-io* output))
                        (eval form)))))))
    (values buffer values)))

(defun transcribed-prefix-id (id capture-id comment)
  (cond ((and (or (eq comment :keep) (eq comment :keep-t))
              capture-id)
         (if (commented-prefix-id-p capture-id)
             (commented-prefix-id id)
             id))
        ((eq comment :keep)
         id)
        (comment
         (commented-prefix-id id))
        (t id)))

(defun transcribed-prefix (id capture-id comment)
  (find-prefix (transcribed-prefix-id id capture-id comment)))

(defun transcribe-output (stream output capture comment
                          update-only include-no-output)
  (when (if update-only
            (not (null capture))
            (or include-no-output (plusp (length output))))
    (let ((prefix (transcribed-prefix :output (capture-id capture) comment)))
      (write-prefixed-lines output prefix stream))))

(defun transcribe-values (stream values captures comment
                          update-only include-no-value)
  (when (if update-only
            captures
            (or include-no-value values))
    (with-transcription-syntax ()
      (if (endp values)
          (when include-no-value
            (format stream "~A~%"
                    (transcribed-prefix :no-value
                                        (capture-id (first captures))
                                        comment)))
          (loop for value in values
                for i upfrom 0
                for capture = (if (< i (length captures))
                                  (elt captures i)
                                  nil)
                do (if (readable-object-p value)
                       (transcribe-readable-value
                        stream value capture comment)
                       (transcribe-unreadable-value
                        stream value capture comment)))))))

;;; Assuming that OBJECT prints readably, check that whether CAPTURE
;;; is readable and it prints the same.
(defun readably-consistent-p (object capture)
  (and (readable-capture-p capture)
       (with-standard-io-syntax
         (string= (prin1-to-string object)
                  (prin1-to-string (readable-object capture))))))

(defun transcribe-readable-value (stream value capture comment)
  (let* ((prefix-id
           (transcribed-prefix-id :readable (capture-id capture) comment))
         (prefix (find-prefix prefix-id)))
    (if (or (null capture)
            (not (readably-consistent-p value capture)))
        (if (eq prefix-id :readable)
            (format stream "~A ~S~%" prefix value)
            ;; FIXME: indentation can be wrong with multiline
            (write-prefixed-lines
             (princ-to-string value)
             (find-prefix :commented-readable-continuation)
             stream :first-line-prefix prefix))
        ;; They print the same, so use the parsed string, because
        ;; it might have been hand-indented.
        (if (eq prefix-id :readable)
            (format stream "~A ~A~%" prefix (readable-string capture))
            (write-prefixed-lines
             (readable-string capture)
             (find-prefix :commented-readable-continuation)
             stream :first-line-prefix prefix)))))

(defun transcribe-unreadable-value (stream object capture comment)
  (let ((first-line-prefix-id
          (transcribed-prefix-id :unreadable (capture-id capture) comment)))
    (write-prefixed-lines (prin1-to-string object)
                          (continuation-prefix first-line-prefix-id)
                          stream
                          :first-line-prefix (find-prefix
                                              first-line-prefix-id))))


;;;; Conditions

(define-condition transcription-error (error)
  (;; This is usually the source stream object on which TRANSCRIBE
   ;; failed, but it can also be a REFERENCE object if transcription
   ;; was invoked by DOCUMENT to check the consistency of a
   ;; transcript.
   (on :initarg :on :reader transcription-error-on)
   ;; The file position at which the error was encountered or NIL if
   ;; unknown.
   (file-position
    :initarg :file-position
    :reader transcription-error-file-position)
   ;; The lisp form that was being processed when the error happened.
   ;; It also includes leading whitespace and comments.
   (form-as-string
    :initarg :form-as-string
    :reader transcription-error-form-as-string)
   ;; A string detailing the circumstances of the error.
   (message
    :initarg :message
    :reader transcription-error-message)
   (message-args
    :initarg :message-args
    :reader transcription-error-message-args))
  (:documentation "Represents syntactic errors in the SOURCE argument
  of TRANSCRIBE and also serves as the superclass of
  TRANSCRIPTION-CONSISTENCY-ERROR.")
  (:report (lambda (condition stream)
             (let ((on (transcription-error-on condition)))
               (format stream
                       "~@<Transcription error~@[ in ~:_~A~]~
                       ~@[ ~:_at position ~A~].~
                       ~:_ ~?~%~
                       Form: ~:_~S~:@>"
                       (if (typep on 'reference)
                           ;; Allow M-. to work in the slime debugger.
                           (print-reference-with-package on)
                           ;; Don't print the stream, since it's
                           ;; likely to be constructed by
                           ;; WITH-INPUT-STREAM which is meaningless
                           ;; for the user.
                           nil)
                       (transcription-error-file-position condition)
                       (transcription-error-message condition)
                       (transcription-error-message-args condition)
                       (transcription-error-form-as-string condition))))))

(defun print-reference-with-package (reference)
  (let ((*package* (find-package :keyword)))
    (format nil "~S ~S" (reference-object reference)
            (reference-locative reference))))

(defun transcription-error (stream file-position form-as-string
                            message &rest message-args)
  (error 'transcription-error
         :on (or *reference-being-documented* stream)
         :file-position (if *reference-being-documented* nil file-position)
         :form-as-string form-as-string
         :message message
         :message-args message-args))

(defun transcription-error* (message &rest message-args)
  (apply #'transcription-error nil nil nil message message-args))

(define-condition transcription-consistency-error (transcription-error)
  ()
  (:documentation "A common superclass for
  TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR and
  TRANSCRIPTION-VALUES-CONSISTENCY-ERROR."))

(define-condition transcription-output-consistency-error
    (transcription-consistency-error)
  ()
  (:documentation "Signaled (with CERROR) by TRANSCRIBE when invoked
  with :CHECK-CONSISTENCY and the output of a form is not the same as
  what was parsed."))

(define-condition transcription-values-consistency-error
    (transcription-consistency-error)
  ()
  (:documentation "Signaled (with CERROR) by TRANSCRIBE when invoked
  with :CHECK-CONSISTENCY and the values of a form are inconsistent
  with their parsed representation."))

(defun consistency-error (class stream form-as-string
                          message &rest message-args)
  (cerror "Continue." class
          :on (or *reference-being-documented* stream)
          :file-position (cond (*reference-being-documented*
                                nil)
                               (stream
                                (file-position stream))
                               (t nil))
          :form-as-string form-as-string
          :message message
          :message-args message-args))


(defsection @mgl-pax-transcript-emacs-integration
    (:title "Transcribing with Emacs")
  """Typical transcript usage from within Emacs is simple: add a lisp
  form to a docstring at any indentation level. Move the cursor right
  after the end of the form as if you were to evaluate it with `C-x
  C-e`. The cursor is marked by `#\^`:

      This is part of a docstring.

      ```cl-transcript
      (values (princ :hello) (list 1 2))^
      ```

  Note that the use of fenced code blocks with the language tag
  `cl-transcript` is only to tell PAX to perform consistency checks at
  documentation generation time.

  Now invoke the elisp function `mgl-pax-transcribe` where the cursor
  is and the fenced code block from the docstring becomes:

      (values (princ :hello) (list 1 2))
      .. HELLO
      => :HELLO
      => (1 2)
      ^

  Then you change the printed message and add a comment to the second
  return value:

      (values (princ :hello-world) (list 1 2))
      .. HELLO
      => :HELLO
      => (1
          ;; This value is arbitrary.
          2)

  When generating the documentation you get a
  TRANSCRIPTION-CONSISTENCY-ERROR because the printed output and the
  first return value changed so you regenerate the documentation by
  marking the region of bounded by `#\|` and the cursor at `#\^` in
  the example:

      |(values (princ :hello-world) (list 1 2))
      .. HELLO
      => :HELLO
      => (1
          ;; This value is arbitrary.
          2)
      ^

  then invoke the elisp function `mgl-pax-retranscribe-region` to get:

      (values (princ :hello-world) (list 1 2))
      .. HELLO-WORLD
      => :HELLO-WORLD
      => (1
          ;; This value is arbitrary.
          2)
      ^

  Note how the indentation and the comment of `(1 2)` was left alone
  but the output and the first return value got updated.

  Alternatively, `C-u mgl-pax-transcribe` will emit commented markup:

      (values (princ :hello) (list 1 2))
      ;.. HELLO
      ;=> HELLO
      ;=> (1 2)

  `C-u - mgl-pax-retranscribe-region` will turn commented into
  non-commented markup. Without a prefix argument
  `mgl-pax-retranscribe-region` will not change the markup style.

  Transcription support in emacs can be enabled by adding this to your
  Emacs initialization file (or loading `src/transcribe.el`):"""
  (transcribe.el (include
                  #.(asdf:system-relative-pathname :mgl-pax "src/transcribe.el")
                  :header-nl "```elisp"
                  :footer-nl "```")))

(defun transcribe-for-emacs (string comment update-only echo
                             first-line-special-p)
  (swank::with-buffer-syntax ()
    (multiple-value-bind (string indentation)
        (strip-docstring-indentation string
                                     :first-line-special-p first-line-special-p)
      (let ((transcript
              (prefix-lines (make-string indentation :initial-element #\Space)
                            (transcribe string nil :comment comment
                                        :update-only update-only :echo echo)
                            :exclude-first-line-p first-line-special-p)))
        (if echo
            transcript
            (format nil "~%~A" transcript))))))
