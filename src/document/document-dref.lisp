(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(declaim (ftype function document-object))

(defsection @output-details (:title "Output Details")
  "[document-dref (method () (dref t))][docstring]

  With this default format, PAX supports all locative types, but for
  some DREF::@LOCATIVE-TYPES defined in DRef and the @PAX-LOCATIVES,
  special provisions have been made.

  - [document-dref (method () (variable-dref t))][docstring]
  - [document-dref (method () (setf-dref t))][docstring]
  - [document-dref (method () (method-dref t))][docstring]
  - [document-dref (method () (accessor-dref t))][docstring]
  - [document-dref (method () (structure-accessor-dref t))][docstring]
  - [document-dref (method () (class-dref t))][docstring]
  - [document-dref (method () (asdf-system-dref t))][docstring]
  - [document-dref (method () (section-dref t))][docstring]
  - [document-dref (method () (glossary-term-dref t))][docstring]
  - [document-dref (method () (go-dref t))][docstring]
  - [document-dref (method () (include-dref t))][docstring]
  - [document-dref (method () (clhs-dref t))][docstring]
  - [document-dref (method () (unknown-dref t))][docstring]")

(defmethod document-dref ((dref dref) stream)
  "By default, DREFs are documented in the following format.

  ```
  - [<locative-type>] <name> <arglist>

      <docstring>
  ```

  The line with the bullet is printed with DOCUMENTING-REFERENCE. The
  docstring is processed with DOCUMENT-DOCSTRING while
  @LOCAL-REFERENCES established with WITH-DISLOCATED-NAMES are in
  effect for all variables locally bound in a definition with ARGLIST,
  and *PACKAGE* is bound to the second return value of DOCSTRING."
  (multiple-value-bind (arglist arglist-type) (arglist dref)
    (multiple-value-bind (docstring package) (docstring dref)
      (documenting-reference (stream :arglist arglist :package package)
        (with-dislocated-names (case arglist-type
                                 ((:macro :deftype)
                                  (dref::macro-arg-names arglist))
                                 ((:ordinary)
                                  (or
                                   (ignore-errors
                                    (dref::function-arg-names arglist))
                                   (dref::macro-arg-names arglist))))
          (document-docstring docstring stream))))))

(declaim (ftype function prin1-to-string*))

(defmethod document-dref ((dref variable-dref) stream)
  "For definitions with a VARIABLE or CONSTANT locative, their
  initform is printed as their arglist. The initform is the INITFORM
  argument of the locative if provided, or the global symbol value of
  their name. If no INITFORM is provided, and the symbol is globally
  unbound, then no arglist is printed.

  When the printed initform is too long, it is truncated."
  (let ((symbol (dref-name dref)))
    (destructuring-bind (&optional (initform nil initformp))
        (xref-locative-args (dref-origin dref))
      (let ((arglist (multiple-value-bind (value unboundp)
                         (symbol-global-value symbol)
                       (when (or initformp (not unboundp))
                         (let ((*print-pretty* t))
                           (escape-markdown
                            (shorten-string
                             (prin1-to-string* (if initformp initform value))
                             :n-lines 10 :n-chars 512 :ellipsis " ...")))))))
        (documenting-reference (stream :arglist arglist)
          (document-docstring (docstring dref) stream))))))

(defmethod document-dref ((dref setf-dref) stream)
  "Depending of what the SETF locative refers to, the ARGLIST of the
  [setf expander][clhs], [setf function][clhs], or the method
  signature is printed as with the METHOD locative."
  (let ((resolved (resolve dref nil)))
    (if (typep resolved 'method)
        (%document-method dref stream)
        (call-next-method))))

(defmethod document-dref ((dref method-dref) stream)
  "For definitions with a METHOD locative, the arglist printed is
  the method signature, which consists of the locative's `QUALIFIERS`
  and `SPECIALIZERS` appended."
  (%document-method dref stream))

(defun %document-method (dref stream)
  (declare (type (or method-dref setf-dref) dref))
  (let ((arglist (rest (dref::method-for-inspect-value (resolve dref)))))
    (documenting-reference (stream :arglist arglist)
      (with-dislocated-names (dref::function-arg-names (arglist dref))
        (document-docstring (docstring dref) stream)))))


;;;; ACCESSOR, READER and WRITER locatives

(defmethod document-dref ((dref accessor-dref) stream)
  "For definitions with an ACCESSOR, READER or WRITER locative, the
  class on which they are specialized is printed as their arglist."
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (generate-documentation-for-slot-definition
     (dref::find-accessor-slot-definition symbol (first locative-args))
     (first locative-args) stream)))

(defmethod document-dref ((dref reader-dref) stream)
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (generate-documentation-for-slot-definition
     (dref::find-reader-slot-definition symbol (first locative-args))
     (first locative-args) stream)))

(defmethod document-dref ((dref writer-dref) stream)
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (generate-documentation-for-slot-definition
     (dref::find-writer-slot-definition symbol (first locative-args))
     (first locative-args) stream)))

(defun generate-documentation-for-slot-definition (slot-def class stream)
  (let ((arglist (format nil "~A~@[ ~A~]" (prin1-to-markdown class)
                         (slot-def-to-string slot-def))))
    (documenting-reference (stream :arglist arglist)
      ;; There is no documentation for condition accessors, and some
      ;; implementations signal warnings.
      (unless (subtypep (find-class class) 'condition)
        (document-docstring (ignore-errors
                             (swank-mop:slot-definition-documentation slot-def))
                            stream)))))

(defun slot-def-to-string (slot-def)
  (when (and slot-def
             (or (swank-mop:slot-definition-initargs slot-def)
                 (swank-mop:slot-definition-initfunction slot-def)))
    (if (and *document-mark-up-signatures* (eq *format* :html))
        (let ((initarg-strings
                (when (swank-mop:slot-definition-initargs slot-def)
                  (mapcar #'prin1-to-markdown
                          (swank-mop:slot-definition-initargs slot-def)))))
          (format nil "(~{~A~^ ~}~A)" initarg-strings
                  (if (swank-mop:slot-definition-initfunction slot-def)
                      (format nil "~A= ~A"
                              (if initarg-strings " " "")
                              (prin1-to-markdown
                               (swank-mop:slot-definition-initform
                                slot-def)))
                      "")))
        (prin1-to-markdown
         `(,@(when (swank-mop:slot-definition-initargs slot-def)
               (swank-mop:slot-definition-initargs slot-def))
           ,@(when (swank-mop:slot-definition-initfunction slot-def)
               `(=
                 ,(swank-mop:slot-definition-initform slot-def))))))))


(defmethod document-dref ((dref structure-accessor-dref) stream)
  "For definitions with a STRUCTURE-ACCESSOR locative, the arglist
  printed is the locative's CLASS-NAME argument if provided."
  (documenting-reference (stream :arglist (dref-locative-args dref))
    (document-docstring (docstring dref) stream)))

(defmethod document-dref ((dref class-dref) stream)
  "For definitions with a CLASS locative, the arglist printed is the
  list of immediate superclasses with STANDARD-OBJECT, CONDITION and
  non-exported symbols omitted."
  (let* ((class (find-class (dref-name dref)))
         (conditionp (subtypep class 'condition))
         (superclasses
           (remove-if (lambda (name)
                        (or (eq name 'standard-object)
                            (and conditionp (eq name 'condition))
                            ;; Omit non-exported superclasses.
                            (not (eq (nth-value
                                      1 (find-symbol (symbol-name name)
                                                     (symbol-package name)))
                                     :external))))
                      (mapcar #'class-name
                              (swank-mop:class-direct-superclasses class))))
         (arglist (when superclasses
                    (if *document-mark-up-signatures*
                        (mark-up-superclasses superclasses)
                        superclasses))))
    (documenting-reference (stream :arglist arglist)
      (document-docstring (docstring dref) stream))))

(defun mark-up-superclasses (superclasses)
  (with-output-to-string (stream)
    (loop for class in superclasses
          for i upfrom 0
          do (let ((dref (locate class 'class)))
               (let ((name (prin1-to-markdown class)))
                 (unless (zerop i)
                   (format stream " "))
                 (if (global-definition-p dref)
                     (format stream "[~A][~A]" name (link-to-definition dref))
                     (format stream "~A" name)))))))


;;;; ASDF:SYSTEM locative

;;; For testing
(defvar *omit-asdf-slots* nil)

;; FIXME: should not be WITH-HEADING
(defmethod document-dref ((dref asdf-system-dref) stream)
  "For definitions with a ASDF:SYSTEM locative, their most
  important slots are printed as an unnumbered list."
  (let ((system (resolve dref)))
    (with-heading (stream system
                          (format nil "The ~A \\ASDF System"
                                  (escape-markdown (slot-value system
                                                               'asdf::name))))
      (flet ((foo (name fn &key type)
               (let ((value (funcall fn system)))
                 (when (and value (not (equal value "")))
                   (case type
                     ((:link)
                      (format stream "- ~A: [~A](~A)~%" name value value))
                     ((:mailto)
                      (format stream "- ~A: [~A](mailto:~A)~%"
                              name value value))
                     ((:source-control)
                      (format stream "- ~A: [~A](~A)"
                              name (first value) (second value)))
                     ((:docstring)
                      (format stream "- ~A: " name)
                      (document-docstring value stream
                                          :indentation "  "
                                          :exclude-first-line-p t
                                          :paragraphp nil)
                      (terpri stream))
                     ((nil)
                      (format stream "- ~A: ~A~%" name value)))))))
        (unless *omit-asdf-slots*
          (foo "Version" 'asdf/component:component-version)
          (foo "Description" 'asdf/system:system-description :type :docstring)
          (foo "Long Description" 'asdf/system:system-long-description
               :type :docstring)
          (foo "Licence" 'asdf/system:system-licence)
          (foo "Author" 'asdf/system:system-author)
          (foo "Maintainer" 'asdf/system:system-maintainer)
          (foo "Mailto" 'asdf/system:system-mailto :type :mailto)
          (foo "Homepage" 'asdf/system:system-homepage :type :link)
          (foo "Bug tracker" 'asdf/system:system-bug-tracker :type :link)
          (foo "Source control" 'asdf/system:system-source-control
               :type :source-control)
          (terpri stream))))))


;;;; SECTION locative

(defvar *section*)

(defmacro documenting-section ((section stream) &body body)
  (with-gensyms (same-package)
    (once-only (section)
      `(let ((,same-package (and (eq *package* (section-package ,section))
                                 (or (boundp '*section*)
                                     *document-open-linking*)))
             (*package* (section-package ,section))
             (*readtable* (section-readtable ,section))
             (*section* ,section))
         (with-heading (,stream ,section (section-title-or-name ,section)
                        :link-title-to (section-link-title-to ,section))
           (when (and (not ,same-package) *document-normalize-packages*)
             (format-in-package *package* ,stream))
           ,@body)))))

(defmethod document-dref ((dref section-dref) stream)
  "When a definition with the SECTION locative is being documented,
  a new (sub)section is opened (see WITH-HEADING), within which
  documentation for its each of its SECTION-ENTRIES is generated. A
  fresh line is printed after all entries except the last."
  (let ((section (resolve dref)))
    (documenting-section (section stream)
      (let ((firstp t))
        (dolist (entry (section-entries section))
          (if firstp
              (setq firstp nil)
              (terpri stream))
          (document-object entry stream))))))

(defun format-in-package (package stream)
  (format stream "###### \\[in package ~A~A\\]~%"
          (escape-markdown (package-name package))
          (if (package-nicknames *package*)
              (format nil " with nicknames ~{~A~^, ~}"
                      (mapcar #'escape-markdown (package-nicknames package)))
              "")))


(defmethod document-dref ((dref glossary-term-dref) stream)
  "For definitions with a GLOSSARY-TERM locative, no arglist is
  printed, and if non-NIL, GLOSSARY-TERM-TITLE is printed as name."
  (let* ((glossary-term (resolve dref))
         (name (glossary-term-title-or-name glossary-term)))
    (documenting-reference (stream :name name)
      (when (glossary-term-url glossary-term)
        (document-docstring
         (format nil "External link to [~A](~A)."
                 (escape-markdown (glossary-term-url glossary-term))
                 (glossary-term-url glossary-term))
         stream))
      (document-docstring (glossary-term-docstring glossary-term) stream))))

(defmethod document-dref ((dref go-dref) stream)
  "For definitions with a GO locative, its LOCATIVE-ARGS are printed
  as its arglist, along with a redirection message."
  (let ((locative-args (dref-locative-args dref)))
    (documenting-reference (stream :arglist locative-args)
      (document-docstring
       (format nil "See ~A." (apply #'md-reflink-from
                                    (first (dref-locative-args dref))))
       stream))))

(defun md-reflink-from (object locative)
  (format nil "[~A][~A]" (if (stringp object)
                             (escape-markdown object)
                             (prin1-to-markdown object))
          (let ((*print-readably* nil))
            (prin1-to-markdown locative))))


;;;; INCLUDE locative

(defmethod document-dref ((dref include-dref) stream)
  "See the INCLUDE locative."
  (let ((locative-args (dref-locative-args dref)))
    (destructuring-bind (source &key (line-prefix "") header footer
                                  header-nl footer-nl) locative-args
      (multiple-value-bind (file start-loc end-loc) (include-region source)
        (let ((start (or (source-location-file-position-offset start-loc)
                         (source-location-file-position start-loc)))
              (end (or (source-location-file-position-offset end-loc)
                       (source-location-file-position end-loc))))
          (cond ((and start-loc (null start))
                 (warn-in-document-context "~S cannot find ~S ~S"
                                           'include :start start-loc))
                ((and end-loc (null end))
                 (warn-in-document-context "~S cannot find ~S ~S"
                                           'include :end end-loc))
                (t
                 (let ((text (file-subseq file start end)))
                   (when header
                     (format stream "~A" header))
                   (when header-nl
                     (format stream "~&")
                     (format stream header-nl)
                     (format stream "~%"))
                   (format stream "~A" (prefix-lines line-prefix text))
                   (when footer
                     (format stream footer))
                   (when footer-nl
                     (format stream "~&")
                     (format stream footer-nl)
                     (format stream "~%"))))))))))

(defun file-subseq (pathname &optional start end)
  (with-open-file (stream pathname)
    (let ((*print-pretty* nil)
          (start (or start 0))
          (end (or end (file-length stream)))
          (buffer-size 4096))
      (file-position stream start)
      (with-output-to-string (datum)
        (let ((buffer (make-array buffer-size :element-type 'character)))
          (loop
            for bytes-read = (read-sequence
                              buffer stream
                              :end (min buffer-size
                                        (- end (file-position stream))))
            do (write-sequence buffer datum :start 0 :end bytes-read)
            while (= bytes-read buffer-size)))))))


(defmethod document-dref ((dref clhs-dref) stream)
  "For definitions with a CLHS locative, the LOCATIVE-ARGS are printed
  as the arglist. There is no docstring."
  (documenting-reference (stream :arglist (dref-locative-args dref))))

(defmethod document-dref ((dref unknown-dref) stream)
  "For definitions with an UNKNOWN locative, the LOCATIVE-ARGS are
  printed as the arglist. There is no docstring."
  (let ((locative-args (dref-locative-args dref)))
    (documenting-reference
        (stream :arglist (escape-markdown
                          (with-standard-io-syntax*
                            ;; Are dspecs readable?
                            (let ((*print-readably* nil))
                              (prin1-to-string (first locative-args)))))))))


(defun warn-in-document-context (format-control &rest format-args)
  (warn-or-error-in-document-context #'warn format-control format-args))

(defun error-in-document-context (format-control &rest format-args)
  (warn-or-error-in-document-context #'error format-control format-args))

#+nil
(defun warn-or-error-in-document-context (fn format-control format-args)
  (declare (special *objects-being-documented*))
  (apply fn (concatenate 'string
                         "~@<" format-control
                         "~{ while documenting ~S~^~%~}~:@>")
         (append format-args
                 (list (loop for object in *objects-being-documented*
                             when (typep object 'dref)
                               collect object)))))

(defun warn-or-error-in-document-context (fn format-control format-args)
  (declare (special *objects-being-documented*))
  (apply fn(with-output-to-string (out)
             (write-string "~@<" out)
             (write-string format-control out)
             (dolist (object *objects-being-documented*)
               (when (typep object 'dref)
                 (let ((*package* (find-package :keyword)))
                   (format out "~%  while documenting ~S" object))))
             (write-string "~:@>" out))
         format-args))
