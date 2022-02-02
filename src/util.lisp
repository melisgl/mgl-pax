(in-package :mgl-pax)

(defmacro with-standard-io-syntax* (&body body)
  `(with-standard-io-syntax
     ;; With *PRINT-READABLY*, CLISP insists on printing FOO as |FOO|.
     (let (#+clisp (*print-readably* nil))
       ,@body)))

(defun prin1-to-string/fully-qualified (object)
  (let ((*package* (find-package :keyword)))
    (prin1-to-string object)))

(defun find-package* (name)
  ;; On AllegroCL, FIND-PACKAGE will signal an error if a relative
  ;; package name has too many leading dots.
  (ignore-errors (find-package name)))

(defun external-symbol-p (symbol)
  (eq (nth-value 1 (find-symbol (symbol-name symbol) (symbol-package symbol)))
      :external))

(defun symbol-global-value (symbol)
  #+allegro
  (multiple-value-bind (value bound) (sys:global-symbol-value symbol)
    (values value (eq bound :unbound)))
  #+ccl
  (let ((value (ccl::%sym-global-value symbol)))
    (values value (eq value (ccl::%unbound-marker))))
  #+sbcl
  (ignore-errors (sb-ext:symbol-global-value symbol))
  #-(or allegro ccl sbcl)
  (ignore-errors (symbol-value symbol)))

;;; Like SYMBOL-FUNCTION*, but sees through encapsulated functions.
(defun symbol-function* (symbol)
  #+cmucl
  (eval `(function ,symbol))
  #-cmucl
  (unencapsulated-function (symbol-function symbol)))

(defun unencapsulated-function (function)
  (or #+abcl (system::untraced-function function)
      #+ccl (ccl::find-unencapsulated-definition function)
      #+clisp (system::get-traced-definition function)
      #+ecl (find-type-in-sexp (function-lambda-expression function) 'function)
      #+sbcl (maybe-find-encapsulated-function function)
      function))

#+sbcl
;;; Tracing typically encapsulate a function in a closure. The
;;; function we need is at the end of the encapsulation chain.
(defun maybe-find-encapsulated-function (function)
  (declare (type function function))
  (if (eq (sb-impl::%fun-name function) 'sb-impl::encapsulation)
      (maybe-find-encapsulated-function
       (sb-impl::encapsulation-info-definition
        (sb-impl::encapsulation-info function)))
      function))

#+ecl
(defun find-type-in-sexp (form type)
  (dolist (x form)
    (cond ((listp x)
           (let ((r (find-type-in-sexp x type)))
             (when r
               (return-from find-type-in-sexp r))))
          ((typep x type)
           (return-from find-type-in-sexp x))
          (t
           nil))))

(defun function-name (function)
  (let* ((function (unencapsulated-function function))
         (name #+clisp (system::function-name function)
               #-clisp (swank-backend:function-name function)))
    ;; ABCL has function names like (FOO (SYSTEM::INTERPRETED)).
    (if (listp name)
        (first name)
        name)))

(defun arglist (function-designator)
  (let ((function-designator
          (if (symbolp function-designator)
              function-designator
              (unencapsulated-function function-designator))))
    #+abcl
    (multiple-value-bind (arglist foundp)
        (extensions:arglist function-designator)
      (cond (foundp arglist)
            ((typep function-designator 'generic-function)
             (mop:generic-function-lambda-list function-designator))
            ((and (symbolp function-designator)
                  (typep (symbol-function* function-designator)
                         'generic-function))
             (mop:generic-function-lambda-list
              (symbol-function* function-designator)))))
    #+allegro
    (handler-case
        (let* ((symbol (if (symbolp function-designator)
                           function-designator
                           (function-name function-designator)))
               (lambda-expression (ignore-errors
                                   (function-lambda-expression
                                    (symbol-function symbol)))))
          (if lambda-expression
              (second lambda-expression)
              (excl:arglist symbol)))
      (simple-error () :not-available))
    #+ccl
    (let ((arglist (swank-backend:arglist function-designator)))
      (if (listp arglist)
          ;; &KEY arguments are given as keywords, which screws up
          ;; WITH-DISLOCATED-SYMBOLS when generating documentation for
          ;; functions.
          (mapcar (lambda (x)
                    (if (keywordp x)
                        (intern (string x))
                        x))
                  arglist)
          arglist))
    #-(or abcl allegro ccl)
    (swank-backend:arglist function-designator)))


(defmacro find-definition* (object name dspecs)
  `(or (find-source-location ,object)
       (find-definition ,name ,dspecs)))

;;; SWANK-BACKEND:FIND-SOURCE-LOCATION is much faster than
;;; SWANK-BACKEND:FIND-DEFINITIONS, but it is not widely supported.
(defun find-source-location (object)
  (when object
    (let ((location (swank-backend:find-source-location object)))
      (when (and (listp location)
                 (eq (first location) :location))
        location))))


;;; Return a definition among (SWANK-BACKEND:FIND-DEFINITIONS NAME)
;;; that matches one of DSPECS. Normally there should be at most one
;;; anyway.
(defun find-definition (name dspecs)
  (let* ((dspec-and-location-list (ignore-errors
                                   (swank-backend:find-definitions name)))
         (entry (loop for dspec in dspecs
                        thereis (find dspec dspec-and-location-list
                                      :key #'first :test #'match-dspec))))
    (if entry
        (second entry)
        `(:error (format nil "Could not find source location for ~S."
                         dspecs)))))

(defmacro define-dspecs (name lambda-list &body body)
  (multiple-value-bind (clauses declarations) (alexandria:parse-body body)
    (let ((dspec-forms (loop for (feature-expr dspec-form) on clauses
                             by #'cddr
                             when (alexandria:featurep feature-expr)
                               collect dspec-form)))
      `(defun ,name ,lambda-list
         ,@declarations
         (list ,@(or dspec-forms
                     (loop for clause on clauses by #'cddr
                           collect (second clause))))))))

(defun match-dspec (dspec-prefix dspec)
  (and (listp dspec)
       (let (#+ecl (dspec (normalize-method-dspec dspec)))
         (alexandria:starts-with-subseq dspec-prefix dspec :test #'equal))))

;;; (DEFMETHOD TEST-GF (X NUMBER) (Y (EQL 'F)) => (DEFMETHOD TEST-GF
;;; NUMBER (EQL F)) so that it matches SWANK-METHOD-DSPECS.
#+ecl
(defun normalize-method-dspec (dspec)
  (flet ((remove-arg-name (specifier)
           (if (and (listp specifier)
                    (= (length specifier) 2))
               (second specifier)
               specifier))
         (remove-quote-from-eql (specifier)
           (if (and (listp specifier)
                    (= (length specifier) 2)
                    (eq (first specifier) 'eql)
                    ;; OBJ is (QUOTE F) in the above example.
                    (let ((obj (second specifier)))
                      (and (listp obj)
                           (= (length obj) 2)
                           (eq (first obj) 'quote))))
               `(eql ,(second (second specifier)))
               specifier)))
    (if (eq (first dspec) 'defmethod)
        (list* 'defmethod (second dspec)
               (mapcar (lambda (specifier)
                         (remove-quote-from-eql
                          (remove-arg-name specifier)))
                       (cddr dspec)))
        dspec)))

(define-dspecs swank-variable-dspecs (name)
  (:or :abcl :ecl :sbcl) `(defvar ,name)
  :allegro `(:special-declaration ,name)
  :allegro `(:variable ,name)
  :ccl `(variable ,name)
  :cmucl `(variable :special ,name)
  :ecl `(defparameter ,name))

(define-dspecs swank-constant-dspecs (name)
  (:or :abcl :ecl :sbcl) `(defconstant ,name)
  :allegro `(:variable ,name)
  #+ccl :ccl #+ccl `(ccl::constant ,name)
  :cmucl `(variable :constant ,name))

(define-dspecs swank-macro-dspecs (name)
  (:or :abcl :ecl :cmucl :sbcl) `(defmacro ,name)
  :allegro `(:operator ,name)
  :ccl `(function ,name))

(define-dspecs swank-compiler-macro-dspecs (name)
  (:or :abcl :ecl :cmucl :sbcl) `(define-compiler-macro ,name)
  :allegro `(:compiler-macro ,name)
  :ccl `(compiler-macro ,name))

(define-dspecs swank-symbol-macro-dspecs (name)
  (:or :abcl :ecl :sbcl) `(define-symbol-macro ,name)
  :allegro `(:symbol-macro ,name)
  :cmucl `(variable :macro ,name))

(define-dspecs swank-function-dspecs (name)
  (:or :abcl :ecl :sbcl) `(defun ,name)
  :allegro `(:operator ,name)
  (:or :ccl :cmucl) `(function ,name))

(define-dspecs swank-generic-function-dspecs (name)
  (:or :abcl :cmucl :ecl :sbcl) `(defgeneric ,name)
  :allegro `(:operator ,name)
  :ccl `(function ,name))

;;; QUALIFIERS and SPECIALIZERS are straight from the DEFMETHOD form.
;;; That is, SPECIALIZERS contains names such as NUMBER or (EQL 7),
;;; not class objects and eql specializer objects.
(define-dspecs swank-method-dspecs (name qualifiers specializers)
  (:or :abcl :ecl :sbcl) `(defmethod ,name ,@qualifiers ,@specializers)
  :allegro `(:operator (method ,name ,@qualifiers ,specializers))
  :ccl `(:method ,name ,@qualifiers
          ,(mapcar 'specializer-to-object specializers))
  :cmucl `(method ,name ,@qualifiers ,specializers))

(define-dspecs swank-method-combination-dspecs (name)
  :sbcl `(define-method-combination ,name)
  :allegro `(:define-method-combination ,name)
  :ccl `(method-combination ,name))

(define-dspecs swank-accessor-dspecs (name class-name writerp)
  :allegro `(:type (method ,name (,@(when writerp '(t))
                                  ,class-name)))
  #+ccl :ccl #+ccl `(,(if writerp
                          'ccl::writer-method
                          'ccl::reader-method)
                     (:method ,name
                       (,@(when writerp (list (find-class t)))
                        ,(find-class class-name))))
  :cmucl `(method ,name () (,@(when writerp '(t))
                            ,class-name))
  :sbcl `(defmethod ,name ,@(when writerp '(t)) ,class-name))

(define-dspecs swank-type-dspecs (name)
  (:or :abcl :cmucl :sbcl) `(deftype ,name)
  :allegro `(:type ,name)
  :ccl `(type ,name)
  :cmucl `(variable :macro ,name))

(define-dspecs swank-class-dspecs (name)
  (:or :abcl :cmucl :ecl :sbcl) `(defclass ,name)
  :allegro `(:type ,name)
  :ccl `(class ,name)
  :cmucl `(variable :macro ,name))

(define-dspecs swank-condition-dspecs (name)
  :sbcl `(define-condition ,name)
  (:or :abcl :ecl) `(defclass ,name)
  :allegro `(:type ,name)
  (:or :ccl :cmucl) `(class ,name))

(define-dspecs swank-package-dspecs (name)
  ;; Name may be a string or a symbol. MATCH-DSPEC wouldn't work if we
  ;; had NAME in the prefix.
  (declare (ignore name))
  :sbcl `(defpackage)
  :ccl `(package))


(defun find-method* (function-designator qualifiers specializers
                     &optional (errorp t))
  (find-method (if (symbolp function-designator)
                   (symbol-function* function-designator)
                   function-designator)
               qualifiers
               (specializers-to-objects specializers)
               errorp))

(defun specializers-to-objects (specializers)
  #-(or allegro ccl clisp) specializers
  #+(or allegro ccl clisp) (mapcar #'specializer-to-object specializers))

#+(or allegro ccl clisp)
(defun specializer-to-object (specializer)
  (cond ((symbolp specializer)
         (find-class specializer))
        ((and (listp specializer)
              (= (length specializer) 2)
              (eq (first specializer) 'eql))
         #+allegro (aclmop:intern-eql-specializer (second specializer))
         #+ccl (ccl:intern-eql-specializer (second specializer))
         #+clisp specializer)
        (t specializer)))


(defmacro with-debugger-hook (fn &body body)
  (alexandria:with-gensyms (prev-debugger-hook condition this-hook)
    `(let* ((,prev-debugger-hook *debugger-hook*)
            (*debugger-hook* (lambda (,condition ,this-hook)
                               (declare (ignore ,this-hook))
                               (funcall ,fn ,condition)
                               (let ((*debugger-hook* ,prev-debugger-hook))
                                 (invoke-debugger ,condition)))))
       ,@body)))


;;; FIXME: Drop in favour of the one in alexandria.
(defun read-stream-into-string (stream &key (buffer-size 4096))
  (let ((*print-pretty* nil))
    (with-output-to-string (datum)
      (let ((buffer (make-array buffer-size :element-type 'character)))
        (loop for bytes-read = (read-sequence buffer stream)
              do (write-sequence buffer datum :start 0 :end bytes-read)
              while (= bytes-read buffer-size))))))

;;; Convert to full width character string. Useful for prettier
;;; printing and ensuring canonical form.
(defun character-string (string)
  (make-array (length string) :element-type 'character
              :initial-contents string))

(defun subseq* (seq start)
  (subseq seq (min (length seq) start)))

(defun relativize-pathname (pathname reference-pathname)
  "Return a pathname that's equivalent to PATHNAME but relative to
  REFERENCE-PATHNAME if possible. Like ENOUGH-NAMESTRING, but inserts
  :UP components if necessary."
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

(defun strip-longest-common-prefix (string chars &key (first-line-special-p t))
  (let ((prefix (longest-common-prefix
                 string chars :first-line-special-p first-line-special-p)))
    (values
     (with-output-to-string (output)
       (with-input-from-string (s string)
         (loop for i upfrom 0
               for line = (read-line s nil nil)
               while line
               do (if (and first-line-special-p (zerop i))
                      (write-line line output)
                      (write-line (subseq line (length prefix)) output)))))
     prefix)))

;;; Return the longest common prefix of lines of STRING, where the
;;; prefix is made of CHARS.
(defun longest-common-prefix (string chars &key (first-line-special-p t))
  (let ((longest-prefix nil))
    (with-input-from-string (s string)
      (loop for i upfrom 0
            for line = (read-line s nil nil)
            while line
            do (when (or (not first-line-special-p) (plusp i))
                 (let ((prefix (matching-prefix line chars)))
                   (setq longest-prefix
                         (if longest-prefix
                             (subseq longest-prefix
                                     0 (or (mismatch longest-prefix prefix)
                                           (length longest-prefix)))
                             prefix))))))
    longest-prefix))

(defun matching-prefix (string chars)
  (let ((position (position-if-not (lambda (char)
                                     (find char chars))
                                   string)))
    (if position
        (subseq string 0 position)
        string)))

;;; Read as many consucutive lines starting with PREFIX from STREAM as
;;; possible. From each mathing line, strip the prefix and join them
;;; into a non-prefixed string conserving the newlines. As the second
;;; value, return the number of lines read.
;;;
;;; As the third value, return the first non-matching line (without
;;; the newline) or NIL at eof. The fourth value is whether the first
;;; non-matching line returned as the thrid value had a missing
;;; newline. The fifth value is file position of the start of the line
;;; returned as the third value.
;;;
;;; Note that reading (with prefix "..")
;;;
;;;     .. 1
;;;     .. 2
;;;
;;; gives "1~%2". If you want to end with a newline, then:
;;;
;;;     .. 1
;;;     .. 2
;;;     ..
(defun read-prefixed-lines (stream prefix &key (first-line-prefix prefix)
                            (eat-one-space-p t))
  (with-output-to-string (output)
    (loop for n-lines-read upfrom 0 do
      (multiple-value-bind (line missing-newline-p file-position)
          (read-line* stream nil nil)
        (let ((prefix (if (zerop n-lines-read) first-line-prefix prefix)))
          (when (or (null line)
                    (not (alexandria:starts-with-subseq prefix line)))
            (return-from read-prefixed-lines
              (values (get-output-stream-string output) n-lines-read
                      line missing-newline-p file-position)))
          (unless (zerop n-lines-read)
            (terpri output))
          (let ((line (subseq line (length prefix))))
            (format output "~A" (if (and eat-one-space-p
                                         (plusp (length line))
                                         (char= (aref line 0) #\Space))
                                    (subseq line 1)
                                    line))))))))

(defun read-line* (stream &optional (eof-error-p t) eof-value)
  (let ((file-position (file-position stream)))
    (multiple-value-bind (line missing-newline-p)
        (read-line stream eof-error-p eof-value)
      (values line missing-newline-p file-position))))

;;; The inverse of READ-PREFIXED-LINES. If ADD-ONE-SPACE-P, a space
;;; character is printed after the prefix if the line is zero length.
(defun write-prefixed-lines (string prefix stream &key (add-one-space-p t)
                             (first-line-prefix prefix))
  (let ((last-newline-missing-p nil))
    (with-input-from-string (s string)
      (loop for n-lines-read upfrom 0 do
        (multiple-value-bind (line missing-newline-p) (read-line s nil nil)
          (unless line
            (return))
          (setq last-newline-missing-p missing-newline-p)
          (if (zerop (length line))
              (write-line prefix stream)
              (format stream "~A~A~A~%"
                      (if (zerop n-lines-read) first-line-prefix prefix)
                      (if add-one-space-p " " "")
                      line)))))
    (unless last-newline-missing-p
      (write-line prefix stream))))

;;; Add PREFIX to every line in STRING.
(defun prefix-lines (prefix string &key exclude-first-line-p)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for i upfrom 0 do
        (multiple-value-bind (line missing-newline-p) (read-line in nil nil)
          (unless line
            (return))
          (if (and exclude-first-line-p (= i 0))
              (format out "~a" line)
              (format out "~a~a" prefix line))
          (unless missing-newline-p
            (terpri out)))))))


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
                      (format out "-~:@(~16r~)" code)))
               (setf first? nil)))
    (coerce output 'simple-string)))
