;;;; TODO
;;;;
;;;; - Maybe implement inline commands that can change prefixes and
;;;;   other parameters.
;;;;
;;;; - lightweight syntax instead of ```cl-transcript?

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
  generation with PAX. Code sections tagged \"cl-transcript\" are
  retranscribed and checked for inconsistency (that is, any difference
  in output or return values). If the consistency check fails, an
  error message is signalled that includes the reference to the object
  being documented.

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
  (transcription-error condition)
  (transcription-consistency-error condition))

(defmacro with-transcription-streams ((input output) &body body)
  `(call-with-transcription-streams (lambda (,input ,output)
                                      ,@body)
                                    ,input ,output))

(defun call-with-transcription-streams (fn input output)
  (flet ((call-with-input (input)
           (if output
               (funcall fn input output)
               (with-output-to-string (output)
                 (funcall fn input output)))))
    (cond ((typep input 'stream)
           ;; There is no way to guarantee that FILE-POSITION will
           ;; work on a string so let's just read the entire INPUT
           ;; into a string.
           (with-input-from-string (input (read-stream-into-string input))
             (call-with-input input)))
          ((typep input 'string)
           (with-input-from-string (input input)
             (call-with-input input)))
          (t
           ;; CHECK-TYPE in TRANSCRIBE makes this impossible.
           (assert nil)))))

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

(defun transcribe (source transcript &key
                   update-only
                   check-consistency
                   (include-no-output update-only)
                   (include-no-value update-only)
                   (echo t)
                   debug
                   (prefix-prefix "")
                   (output-prefix ".. ")
                   (value-prefix "=> ")
                   (unreadable-value-prefix "==> ")
                   (unreadable-value-continuation-prefix "--> ")
                   (transcribed-prefix-prefix prefix-prefix)
                   (transcribed-output-prefix output-prefix)
                   (transcribed-value-prefix value-prefix)
                   (transcribed-unreadable-value-prefix unreadable-value-prefix)
                   (transcribed-unreadable-value-continuation-prefix
                    unreadable-value-continuation-prefix)
                   (no-value-marker "; No value"))
  """Read forms from SOURCE and write them (iff ECHO) to TRANSCRIPT
  followed by any output and return values produced by calling EVAL on
  the form. SOURCE can be a stream or a string, while TRANSCRIPT can
  be a stream or NIL in which case transcription goes into a string.
  The return value is the TRANSCRIPT stream or the string that was
  constructed.

  A simple example is this:

  ```cl-transcript
  (transcribe "(princ 42) " nil)
  => "(princ 42)
  .. 42
  => 42
  "
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
  OUTPUT-PREFIX (".. " above). Return values are printed with
  VALUE-PREFIX ("=> "). Note how these prefixes are always printed on
  a new line to facilitate parsing.

  TRANSCRIBE is able to parse its own output. If we transcribe the
  previous output above, we get it back exactly. However, if we remove
  all output markers, leave only a placeholder value marker and
  pass :UPDATE-ONLY T with source:

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

  where "\; No value" is the default NO-VALUE-MARKER.

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
          (format stream "~%~%end")))

      (make-instance 'some-class)
      ==> #<SOME-CLASS 
      -->
      --> end>

  "==> " is UNREADABLE-VALUE-PREFIX and "--> " is
  UNREADABLE-VALUE-CONTINUATION-PREFIX. As with outputs, a consistency
  check between a unreadable value from the source and the value from
  EVAL is performed with STRING=. That is, the value from EVAL is
  printed to a string and compared to the source value. Hence, any
  change to unreadable values will break consistency checks. This is
  most troublesome with instances of classes with the default
  PRINT-OBJECT method printing the memory address. There is currently
  no remedy for that, except for customizing PRINT-OBJECT or not
  transcribing that kind of stuff.

  Trailing whitespaces are never printed unless the output or the
  values have trailing spaces themselves. This means that all prefix
  strings are right trimmed if the rest of the line is empty.

  Finally, one may want to produce a transcript that's valid Common
  Lisp. This can be achieved by adding a semicolon character to all
  prefixes used for markup like this which can be done with
  :PREFIX-PREFIX "\;". One can even translate a transcription from the
  default markup to the one with semicolons
  with :TRANSCRIBED-PREFIX-PREFIX "\;". In general, there is a set of
  prefix arguments used when writing the transcript that mirror those
  for parsing SOURCE."""
  (check-type source (or stream string))
  (check-type transcript (or stream null))
  (with-transcription-streams (source transcript)
    (let ((output-prefix (concatenate 'string prefix-prefix output-prefix))
          (value-prefix (concatenate 'string prefix-prefix value-prefix))
          (unreadable-value-prefix
            (concatenate 'string prefix-prefix unreadable-value-prefix))
          (unreadable-value-continuation-prefix
            (concatenate 'string prefix-prefix
                         unreadable-value-continuation-prefix))
          (transcribed-output-prefix
            (concatenate 'string
                         transcribed-prefix-prefix transcribed-output-prefix))
          (transcribed-value-prefix
            (concatenate 'string
                         transcribed-prefix-prefix transcribed-value-prefix))
          (transcribed-unreadable-value-prefix
            (concatenate 'string transcribed-prefix-prefix
                         transcribed-unreadable-value-prefix))
          (transcribed-unreadable-value-continuation-prefix
            (concatenate 'string transcribed-prefix-prefix
                         transcribed-unreadable-value-continuation-prefix))
          (rtrimmed-output-prefix (rtrim-whitespace output-prefix))
          (rtrimmed-value-prefix (rtrim-whitespace value-prefix))
          (rtrimmed-unreadable-value-prefix
            (rtrim-whitespace unreadable-value-prefix))
          (rtrimmed-unreadable-value-continuation-prefix
            (rtrim-whitespace unreadable-value-continuation-prefix)))
      (with-load-environment (source)
        (let* ((eof (gensym))
               (at-bol-p t)
               (form nil)
               (form-as-string nil)
               (form-output nil)
               (form-values nil)
               (form-old-output nil)
               (form-old-values ()))
          (flet ((flush-form (&key (add-new-line-p t))
                   (when form-as-string
                     (when echo
                       (write-string form-as-string transcript))
                     (when (and echo add-new-line-p)
                       (terpri transcript))
                     (unless (eq form eof)
                       (when (or (not update-only) form-old-output)
                         (when check-consistency
                           (check-output-consistency source
                                                     form-as-string
                                                     form-output
                                                     form-old-output))
                         (transcribe-output transcript
                                            form-output form-old-output
                                            update-only include-no-output
                                            output-prefix
                                            transcribed-output-prefix))
                       (when (or (not update-only) form-old-values)
                         (when check-consistency
                           (check-values-consistency source
                                                     form-as-string
                                                     form-values
                                                     form-old-values))
                         (transcribe-values
                          transcript form-values form-old-values
                          include-no-value
                          transcribed-value-prefix
                          transcribed-unreadable-value-prefix
                          transcribed-unreadable-value-continuation-prefix
                          no-value-marker)))
                     (setq form nil
                           form-as-string nil
                           form-output nil
                           form-values nil
                           form-old-output nil
                           form-old-values nil))))
            (loop
              (let ((file-position (file-position  source)))
                (multiple-value-bind (line-at-bol missing-newline-p)
                    (when at-bol-p (read-line source nil nil))
                  (when (and debug line-at-bol)
                    (format debug "LINE: ~S~%" line-at-bol))
                  (cond
                    ;; EOF at beginning-of-line, bail out.
                    ((and at-bol-p (null line-at-bol))
                     (flush-form)
                     (return))
                    ;; Looking at a line of output, remember it. The
                    ;; actual output is needed for CHECK-CONSISTENCY.
                    ;; The information of the presence of any output
                    ;; marker is needed when UPDATE-ONLY.
                    ((and at-bol-p
                          (alexandria:starts-with-subseq
                           rtrimmed-output-prefix line-at-bol))
                     (let ((output
                             (subseq* line-at-bol (length output-prefix))))
                       (when debug
                         (format debug "OUTPUT: ~S~%" output))
                       (push output form-old-output)
                       (unless missing-newline-p
                         (push #.(format nil "~%") output)))
                     (setq at-bol-p t))
                    ;; Looking at a line with a return value.
                    ((and at-bol-p
                          (alexandria:starts-with-subseq
                           rtrimmed-value-prefix line-at-bol))
                     (cond
                       ;; It may be that there is no value following
                       ;; the value marker, because it was put there
                       ;; as a placeholder.
                       ((every #'whitespacep
                               (subseq line-at-bol
                                       (length rtrimmed-value-prefix)))
                        (push (list :readable nil nil) form-old-values))
                       ((string= no-value-marker
                                 (subseq* line-at-bol (length value-prefix)))
                        (unless (endp form-old-values)
                          (transcription-error
                           source file-position form-as-string
                           "No-value marker ~S seen after values."
                           no-value-marker))
                        (setq form-old-values :no-value))
                       (t
                        (file-position  source (+ file-position
                                                  (length value-prefix)))
                        ;; Don't preserve whitespace (the newline),
                        ;; but remember we are at the beginning of a
                        ;; line (because we print a newline after
                        ;; readable values).
                        (multiple-value-bind (value string)
                            (read-form-and-string source eof
                                                  :preserve-whitespace-p t)
                          (when (eq value eof)
                            (transcription-error
                             source file-position form-as-string
                             "Unexpected eof when reading from non-empty ~
                             value line."))
                          (skip-white-space-till-end-of-line source)
                          (when debug
                            (format debug "READABLE VALUE: ~S~%" value))
                          (push (list :readable value string)
                                form-old-values))))
                     (setq at-bol-p t))
                    ;; Looking at a line with an unreadable return value.
                    ((and at-bol-p
                          (alexandria:starts-with-subseq
                           rtrimmed-unreadable-value-prefix line-at-bol))
                     (push (cons :unreadable
                                 (list
                                  (subseq* line-at-bol
                                           (length unreadable-value-prefix))))
                           form-old-values))
                    ;; Looking at a line with an unreadable return value
                    ;; but it's not the first line of its printed
                    ;; representation.
                    ((and at-bol-p
                          (alexandria:starts-with-subseq
                           rtrimmed-unreadable-value-continuation-prefix
                           line-at-bol))
                     (let ((most-recent-value (first form-old-values)))
                       (unless (eq (car most-recent-value) :unreadable)
                         (transcription-error
                          source file-position form-as-string
                          "~S found with no preceeding ~S."
                          rtrimmed-unreadable-value-continuation-prefix
                          rtrimmed-unreadable-value-prefix))
                       (push (subseq* line-at-bol
                                      (length
                                       unreadable-value-continuation-prefix))
                             (cdr most-recent-value))))
                    ;; No special markers found.
                    (t
                     (flush-form)
                     (when at-bol-p
                       ;; Undo do damage caused by READ-LINE above.
                       (file-position  source file-position))
                     (multiple-value-setq (form form-as-string)
                       (read-form-and-string source eof
                                             :preserve-whitespace-p t))
                     (when debug
                       (format debug "READ: ~S~%" form-as-string))
                     (when (eq form eof)
                       (flush-form :add-new-line-p nil)
                       (return))
                     ;; Now read that whitespace char that we
                     ;; preserved. This logic ensures that even if
                     ;; forms appear on the same line, they are
                     ;; transcribed properly.
                     (skip-white-space-till-end-of-line source)
                     (multiple-value-setq (form-output form-values)
                       (eval-and-capture form)))))))
            transcript))))))

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
   ;; A string detailed the circumstances of the error.
   (message
    :initarg :message
    :reader transcription-error-message))
  (:documentation "Represents syntactic errors in the SOURCE argument
  of TRANSCRIBE and also serves as the superclass of
  TRANSCRIPTION-CONSISTENCY-ERROR.")
  (:report (lambda (condition stream)
             (let ((on (transcription-error-on condition)))
               (format stream
                       "Transcription error in ~A~@[ at position ~A~].~%~
                       ~A~%~
                       Form:~%~S~%"
                       (if (typep on 'reference)
                           ;; Allow M-. to work in the slime debugger.
                           (print-reference-with-package on)
                           on)
                       (transcription-error-file-position condition)
                       (transcription-error-message condition)
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
         :message (apply #'format nil message message-args)))

(define-condition transcription-consistency-error (transcription-error)
  ()
  (:documentation "Signaled by TRANSCRIBE (with CERROR) when a
  consistency check fails."))

(defun consistency-error (stream form-as-string message &rest message-args)
  (cerror "Continue." 'transcription-consistency-error
          :on (or *reference-being-documented* stream)
          :file-position (if *reference-being-documented*
                             nil
                             (file-position stream))
          :form-as-string form-as-string
          :message (apply #'format nil message message-args)))

(defun readable-object-p (object)
  (null
   (nth-value 1 (ignore-errors
                 (values (read-from-string (prin1-to-string object)))))))

(defun check-output-consistency (stream form-as-string
                                 output-string output-lines)
  (let ((joined (join-collected-lines output-lines)))
    (unless (string= output-string joined)
      (consistency-error
       stream form-as-string
       "Inconsistent output found. Source:~%~S~%Output:~%~S~%"
       joined output-string))))

(defun check-values-consistency (stream form-as-string values old-values)
  (if (null old-values)
      (consistency-error stream form-as-string
                         "No return value found in source.")
      (let ((old-values (if (eq old-values :no-value) () old-values)))
        (assert (listp old-values))
        (cond ((/= (length values) (length old-values))
               (consistency-error
                stream form-as-string
                "Source had ~S return values while there are actually ~S."
                (length old-values) (length values)))
              (t
               (loop for value in values
                     for old-value in (reverse old-values)
                     do (check-value-consistency stream form-as-string
                                                 value old-value)))))))

(defun check-value-consistency (stream form-as-string value old-value)
  (let ((value-readable-p (readable-object-p value))
        (old-value-value
          (if (eq (car old-value) :readable)
              (second old-value)
              (join-collected-lines (cdr old-value)))))
    (cond ((and value-readable-p
                (not (eq (car old-value) :readable)))
           (consistency-error
            stream form-as-string
            "Unreadable value ~S in source became readable ~S."
            old-value-value value))
          ((and (not value-readable-p)
                (not (eq (car old-value) :unreadable)))
           (consistency-error
            stream form-as-string
            "Readable value ~S in source became unreadable ~S."
            old-value-value value))
          ;; At this point we know that both are readable or both are
          ;; unreadable.
          (value-readable-p
           (unless (with-standard-io-syntax
                     (string= (prin1-to-string value)
                              (prin1-to-string old-value-value)))
             (consistency-error
              stream form-as-string
              "Readable value ~S in source does not print the same as ~S."
              old-value-value value)))
          ((not (string= (prin1-to-string value)
                         old-value-value))
           (consistency-error
            stream form-as-string
            "Unreadable value ~S in source does not print the same as ~S."
            old-value-value value)))))

(defun join-collected-lines (lines)
  (let ((n (length lines)))
    (with-output-to-string (stream)
      (loop for i upfrom 0
            for line in (reverse lines)
            do (write-string line stream)
               (when (< i (1- n))
                 (terpri stream))))))

;;; Read a sexp from STREAM or return EOF. The second value is a
;;; string of all of the characters that were read even if EOF
;;; (whitespace, comments).
(defun read-form-and-string (stream eof &key preserve-whitespace-p)
  (let* ((old-file-position (file-position  stream))
         (form (funcall (if preserve-whitespace-p
                            #'read-preserving-whitespace
                            #'read)
                        stream nil eof nil))
         (new-file-position (file-position  stream))
         (n (- new-file-position old-file-position))
         (form-as-string (make-string n)))
    (file-position  stream old-file-position)
    (read-sequence form-as-string stream)
    (assert (= (file-position  stream) new-file-position))
    (values form form-as-string)))

(defun eval-and-capture (form)
  (let* ((buffer (make-array 0 :element-type 'character
                             :fill-pointer 0 :adjustable t))
         (values (multiple-value-list
                  (with-output-to-string (output buffer)
                    (let ((*standard-output* output)
                          (*error-output* output)
                          (*trace-output* output)
                          (*debug-io* output)
                          (*query-io* output)
                          (*terminal-io* output))
                      (eval form))))))
    (values buffer values)))

(defun rtrim-whitespace (string)
  (string-right-trim *whitespace-chars* string))

(defun transcribe-output (stream output old-output
                          update-only include-no-output
                          output-prefix transcribed-output-prefix)
  (let ((rtrimmed-output-prefix (rtrim-whitespace output-prefix)))
    (if (zerop (length output))
        (when (if update-only
                  (and old-output include-no-output)
                  include-no-output)
          (format stream "~A~%" rtrimmed-output-prefix))
        (when (or (plusp (length output)) old-output)
          (let ((last-newline-missing-p nil))
            (with-input-from-string (s output)
              (loop
                (multiple-value-bind (line missing-newline-p)
                    (read-line s nil nil)
                  (unless line
                    (return))
                  (setq last-newline-missing-p missing-newline-p)
                  (if (zerop (length line))
                      (format stream "~A~%" rtrimmed-output-prefix)
                      (format stream "~A~A~%"
                              transcribed-output-prefix line)))))
            (unless last-newline-missing-p
              (format stream "~A~%" rtrimmed-output-prefix)))))))

(defun transcribe-values (stream values old-values
                          include-no-value
                          transcribed-value-prefix
                          transcribed-unreadable-value-prefix
                          transcribed-unreadable-value-continuation-prefix
                          no-value-marker)
  (let ((old-values (if (eq old-values :no-value)
                        ()
                        (reverse old-values))))
    (if (endp values)
        (when include-no-value
          (format stream "~A~A~%"
                  transcribed-value-prefix no-value-marker))
        (loop for value in values
              for i upfrom 0
              do (if (readable-object-p value)
                     (transcribe-readable-value
                      stream transcribed-value-prefix
                      value (ignore-errors (elt old-values i)))
                     (transcribe-unreadable-value
                      stream value
                      transcribed-unreadable-value-prefix
                      transcribed-unreadable-value-continuation-prefix))))))

(defun transcribe-readable-value (stream prefix value old-value)
  (if (and old-value (readably-consistent-p value old-value))
      ;; They print the same, so use the string representation from
      ;; the source, because it might have been hand-indented.
      (format stream "~A~A~%" prefix (third old-value))
      (format stream "~A~S~%" prefix value)))

;;; Assuming VALUE prints readably, check that whether OLD-VALUE is
;;; readable and it prints the same.
(defun readably-consistent-p (value old-value)
  (and (eq (first old-value) :readable)
       (with-standard-io-syntax
         (string= (prin1-to-string value)
                  (prin1-to-string (second old-value))))))

(defun transcribe-unreadable-value (stream object first-line-prefix
                                    continuation-prefix)
  ;; FIXME: printer control vars?
  (with-input-from-string (s (prin1-to-string object))
    (loop for i upfrom 0
          for line = (read-line s nil nil)
          while line
          do (format stream "~A~A~%"
                     (funcall (if (zerop (length line))
                                  #'rtrim-whitespace
                                  #'identity)
                              (if (zerop i)
                                  first-line-prefix
                                  continuation-prefix))
                     line))))

(defun skip-white-space-till-end-of-line (stream)
  (loop for char = (peek-char nil stream nil nil)
        while (and char (whitespacep char))
        do (read-char stream nil nil)
        until (char= char #\Newline)))


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

  Note that the usage of fenced code blocks with the language tag
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

  Transcription support in emacs can be enabled by adding this to your
  Emacs initialization file:

  ```elisp
  (defun mgl-pax-transcribe-last-expression ()
    "A bit like C-u C-x C-e (slime-eval-last-expression) that
  inserts the output and values of the sexp before the point,
  this does the same but with MGL-PAX:TRANSCRIBE."
    (interactive)
    (insert
     (save-excursion
       (let* ((end (point))
              (start (progn (backward-sexp)
                            (move-beginning-of-line nil)
                            (point))))
         (mgl-pax-transcribe start end nil nil nil)))))

  (defun mgl-pax-retranscribe-region (start end)
    "Updates the transcription in the current region (as in calling
  MGL-PAX:TRANSCRIBE with :UPDATE-ONLY T.)"
    (interactive "r")
    (let* ((point-at-start-p (= (point) start))
           (point-at-end-p (= (point) end))
           (transcript (mgl-pax-transcribe start end t t t)))
      (if point-at-start-p
          (save-excursion
            (goto-char start)
            (delete-region start end)
            (insert transcript))
        (save-excursion
            (goto-char start)
            (delete-region start end))
        (insert transcript))))

  (defun mgl-pax-transcribe (start end update-only echo first-line-special-p)
    (slime-eval
     `(cl:when (cl:find-package :mgl-pax)
               (cl:funcall
                (cl:find-symbol
                 (cl:symbol-name :transcribe-for-emacs) :mgl-pax)
                ,(buffer-substring-no-properties start end)
                ,update-only ,echo ,first-line-special-p))))
  ```""")

(defun transcribe-for-emacs (string update-only echo first-line-special-p)
  (swank::with-buffer-syntax ()
    (multiple-value-bind (string indentation)
        (strip-docstring-indentation string
                                     :first-line-special-p first-line-special-p)
      (let ((transcript
              (prefix-lines (make-string indentation :initial-element #\Space)
                            (transcribe string nil :update-only update-only
                                        :echo echo)
                            :exclude-first-line-p first-line-special-p)))
        (if echo
            transcript
            (format nil "~%~A" transcript))))))
