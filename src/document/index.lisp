(in-package :mgl-pax)

;;;; TODO:
;;;;
;;;; - docs
;;;;
;;;; - use concepts in PAX?
;;;;
;;;; - add the "owner" of the concepts as a referrer to its concepts?


;;;; Dynamically generated sections

;;; Create a SECTION, and generate documentation for it.
(defmacro with-dynamic-section ((stream &key title (indexable t)
                                 in-context-only)
                                &body body)
  (assert (symbolp stream))
  `(document-object (make-instance 'dynamic-section-dref
                                   :title ,title
                                   :indexable ,indexable
                                   :in-context-only ,in-context-only
                                   :locative 'dyn
                                   :fn (lambda (,stream)
                                         ,@body))
                    ,stream))

(defclass dynamic-section-dref (dref)
  ((title :initarg :title :reader doctitle*)
   (dref::name :initarg :name :initform (princ-to-string (next-dyn-id)))
   (fn :initarg :fn :reader fn-of)
   (indexable :initform t :initarg :indexable :reader indexablep)
   (in-context-only :initform nil :initarg :in-context-only
                    :reader in-context-only-p)))

(defmethod document-object* ((dref dynamic-section-dref) stream)
  (with-heading (stream :dref dref)
    (funcall (fn-of dref) stream)))


(in-readtable pythonic-string-syntax)

(defsection @indexing ()
  (@referrer glossary-term)
  (@referee glossary-term)
  (@index-key glossary-term)
  (@index-subkey glossary-term)
  (*document-index-sections* variable)
  (*document-index-formats* variable)
  (*document-indices* variable)
  (*document-index-referee-locative-type-abbrevs* variable)
  (*document-index-referrer-dtype-abbrevs* variable))

(define-glossary-term @referrer (:title "referrer")
  )

(define-glossary-term @referee (:title "referee")
  )

(define-glossary-term @index-key (:title "index key")
  )

(define-glossary-term @index-subkey (:title "index subkey")
  )


;;;; Indices

(defvar/auto *document-indices*
  '((:title "Indices"
     :document-referrer-abbrevs t
     :children ((:dtype (or section glossary-term pseudo)
                 :index nil)
                (:dtype (or function macro compiler-macro)
                 :title "Function and Macro Index")
                (:dtype variable
                 :title "Variable and Constant Index")
                (:dtype type
                 :title "Type Index")
                (:dtype t
                 :title "Misc Index")
                (:concepts t
                 :title "Concept Index")))))

(defun map-indices (fn &optional (indices *document-indices*) (depth 0))
  (dolist (index indices)
    (funcall fn index depth)
    (when-let (children (getf index :children))
      (map-indices fn children (1+ depth)))))

(defmacro do-indices ((index &optional (depth (gensym))) &body body)
  `(map-indices (lambda (,index ,depth)
                  (declare (ignorable ,depth))
                  ,@body)))

(defun find-index (dref)
  (do-indices (index depth)
    (when-let (dtype (getf index :dtype))
      (when (dtypep dref dtype)
        (return-from find-index index)))))

(defun find-index-for-concept (concept)
  (declare (ignore concept))
  (do-indices (index depth)
    (when (getf index :concepts)
      (return-from find-index-for-concept index))))


(defun concepts (object)
  (nth-value-or-with-obj-or-def (object 0)
    (concepts* object)))


(declaim (inline make-index-subkey))
(defun make-index-subkey (name sort-as)
  (cons name sort-as))

(declaim (inline index-subkey-name))
(defun index-subkey-name (subkey)
  (if (consp subkey)
      (car subkey)
      subkey))

(declaim (inline index-subkey-sort-as))
(defun index-subkey-sort-as (subkey)
  (if (consp subkey)
      (cdr subkey)
      subkey))

(defun index-subkey-= (subkey1 subkey2)
  (string= (index-subkey-name subkey1)
           (index-subkey-name subkey2)))

(defun index-subkey-< (subkey1 subkey2)
  (string< (index-subkey-sort-as subkey1)
           (index-subkey-sort-as subkey2)))

(defun index-key-< (key1 key2)
  (lexicographic< #'index-subkey-< key1 key2))

(defun non-list-cons-p (object)
  (and (consp object)
       (cdr object)
       (atom (cdr object))))


;;;; Generating indices

(defvar/auto *document-index-sections* :homeless-documentable
  "Controls what sections get an @INDEX.

  - NIL: No indices are generated.

  - :DOCUMENTABLE: Sections that are appear in @DOCUMENTABLE (at any
    level) get indices. Their subsections do not.

  - :HOMELESS-DOCUMENTABLE: Sections that appear in @DOCUMENTABLE and
    have no HOME-SECTION get indices.")

(defvar/auto *document-index-formats* '(:html :pdf)
  "The list of @OUTPUT-FORMATS for which index generation is enabled.")

(defun maybe-generate-indices (stream)
  (when (eq *section* *indexing-section*)
    (let ((index-to-indexables
            (group-indexables-per-index
             *indexing-definitions*
             (hash-table-keys *indexing-concept-to-referrers*))))
      (format stream "~&~%")
      (dolist (index *document-indices*)
        (maybe-generate-index index stream index-to-indexables)))))

(defun group-indexables-per-index (drefs concepts)
  (let ((grouping (make-hash-table)))
    (dolist (dref drefs)
      (when-let (index (find-index dref))
        (push dref (gethash index grouping))))
    (dolist (concept concepts)
      (when-let (index (find-index-for-concept concept))
        (push concept (gethash index grouping))))
    (let ((groups ()))
      (do-indices (index)
        (when (and (getf index :index t)
                   (gethash index grouping))
          (push (list index (gethash index grouping)) groups)))
      (nreverse groups))))

(defun maybe-generate-index (index stream index-to-indexables)
  (when (index-alive-p index index-to-indexables)
    ;; Index sections are not INDEXABLEP although (and because) they
    ;; link to lots of stuff.
    ;;
    ;; They are also IN-CONTEXT-ONLY-P because the current
    ;; implementation depends on *TOP-LEVEL-SECTION-DEFINITIONS*
    ;; having been populated.
    (with-dynamic-section (stream :title (getf index :title)
                           :indexable nil :in-context-only t)
      (unless *first-pass*
        (when-let (documentation (getf index :documentation))
          (format stream "~A~%~%" documentation))
        (when (getf index :document-referrer-abbrevs)
          (write-referrer-abbrevs stream)
          (terpri stream))
        (write-index (second (find index index-to-indexables :key #'first))
                     stream))
      (dolist (child (getf index :children))
        (maybe-generate-index child stream index-to-indexables)))))

(defun index-alive-p (index index-to-indexables)
  (or
   ;; FIXME: For the sake of the table of contents, we should know
   ;; already in the first pass whether INDEX is alive. For
   ;; non-concept indices, we only need *INDEXING-DEFINITIONS*, which
   ;; is populated in both passes. However, whether a concept index is
   ;; alive depends on the links made in docstrings, but docstrings
   ;; are not processed in the first pass (for performance reasons).
   ;; Thus, we always say "not alive" in the first pass, leading
   ;; mismatch between the toc and the reality of the second pass.
   (find index index-to-indexables :key #'first)
   (loop for child in (getf index :children)
           thereis (index-alive-p child index-to-indexables))))

(defun write-index (indexables stream)
  (let ((key-and-indexable-pairs ()))
    (dolist (indexable indexables)
      (let ((key (if (typep indexable 'dref)
                     (let ((dref indexable))
                       (list (dref-index-name dref)
                             (dref-index-referee-abbrev dref)))
                     indexable)))
        (push (cons key indexable) key-and-indexable-pairs)))
    (setq key-and-indexable-pairs
          (sort (coerce key-and-indexable-pairs 'vector) #'index-key-<
                :key #'car))
    (write-begin-index *format* stream)
    (let ((keys (map 'vector #'car key-and-indexable-pairs))
          (indexables (map 'vector #'cdr key-and-indexable-pairs)))
      (flet ((print-referrers* (key index depth)
               ;; Check that the radix tree kept the order.
               (assert (equal key (aref keys index)))
               (print-referrers (aref indexables index) depth stream)))
        (print-index-key-tree (radix-tree keys :test #'index-subkey-=) stream
                              #'print-referrers*)))
    (write-end-index *format* stream)
    (terpri stream)))

(defgeneric write-begin-index (format stream)
  (:method (format stream))
  (:method ((format (eql :html)) stream)
    (format stream "~% <div class=\"pax-index\">~%~%")))

(defgeneric write-end-index (format stream)
  (:method (format stream))
  (:method ((format (eql :html)) stream)
    (format stream "~% </div>~%")))

(defun dref-index-name (dref)
  (let ((*print-readably* nil)
        (name (dref-name dref))
        (title (document-definition-title dref)))
    (flet ((foo (target-id symbol-name-fn)
             (cond (title
                    (format nil "[~A][~A]" title target-id))
                   ((symbolp name)
                    (format nil "[`~A`][~A]~@[ \\[`~A`\\]~]"
                            ;; FIXME: Cannot escape #\`, but it can be
                            ;; replaced maybe.
                            (maybe-downcase (funcall symbol-name-fn name))
                            target-id
                            (unless (let ((package (symbol-package name)))
                                      (or (eq package #.(find-package :cl))
                                          (eq package *package*)
                                          ;; FIXME: Is this misguided?
                                          #+nil
                                          (member package (package-use-list
                                                           *package*))))
                              (maybe-downcase
                               (package-name (symbol-package name))))))
                   ((stringp name)
                    (format nil "[`~A`][~A]" name target-id))
                   (t
                    (format nil "[`~S`][~A]" name target-id)))))
      (make-index-subkey (foo (link-to-definition dref) 'symbol-name)
                         (foo "" 'sort-as-symbol-name)))))

(defun sort-as-symbol-name (symbol)
  (let ((name (symbol-name symbol)))
    (subseq name (or (position-if #'alphanumericp name) 0))))

(defun print-referrers (indexable depth stream)
  (let ((drefs
          (if (typep indexable 'dref)
              (dref-to-referrers indexable)
              (concept-to-referrers indexable)))
        (*package* (maybe-dref-name-package indexable))
        (groups (make-hash-table :test #'equal)))
    (dolist (dref drefs)
      (push (dref-index-name dref)
            (gethash (dref-index-referrer-abbrev dref) groups)))
    (let* ((abbrevs (sort (hash-table-keys groups) #'index-subkey-<))
           (itemizep (cdr abbrevs)))
      (when itemizep
        (format stream "~%~%"))
      (dolist (abbrev abbrevs)
        (when itemizep
          (loop repeat (* 4 depth)
                do (write-char #\Space stream))
          (write-char #\- stream))
        (format stream " ~A" (index-subkey-name abbrev))
        (loop for name in (sort (gethash abbrev groups) #'index-subkey-<)
              for i upfrom 0
              do (unless (zerop i)
                   (write-char #\, stream))
                 (format stream " ~A" (index-subkey-name name)))
        (when itemizep
          (format stream "~%"))))))

(defun maybe-dref-name-package (indexable)
  (if (and (typep indexable 'dref)
           (symbolp (dref-name indexable)))
      (symbol-package (dref-name indexable))
      *package*))

(defun print-index-key-tree (tree stream fn)
  (let ((i 0))
    (map-radix-tree (lambda (key-runs completep)
                      (let ((depth (1- (length key-runs))))
                        (when (< depth 1)
                          (assert (equal key-runs '(()))))
                        ;; Skip the empty root root
                        (when (plusp depth)
                          (loop repeat (* 4 (1- depth))
                                do (write-char #\Space stream))
                          (format stream "-~{ ~A~}"
                                  (mapcar #'index-subkey-name
                                          (first key-runs)))
                          (when completep
                            (let ((key (loop for run in (reverse key-runs)
                                             append run)))
                              (funcall fn key i depth))
                            (incf i))
                          (terpri stream))))
                    tree :complete-only nil)))


;;;; Abbreviations

(defvar/auto *document-index-referee-locative-type-abbrevs*
  '((generic-function "_(gf)_")
    (function "_(fn)_")
    (variable "_(var)_")
    (asdf:system "_(asdf:system)_")))

(defvar/auto *document-index-referrer-dtype-abbrevs*
  ;; These are by "namespace".
  '(((or function macro compiler-macro method) "↩ _f_:"
     :documentation "_f_: for definitions in the function namespace
(macros, compiler macros and also methods)")
    (type "↩ _t_:"
     :documentation "_t_: `DEFTYPE`s, classes, conditions, structs")
    ((or section glossary-term) "↩ _d_:"
     :documentation "_d_: documentation sections and glossary terms")
    ((or locative dtype) "↩ _l_:"
     :documentation "_l_: definitions of definition types")
    (asdf:system "↩ _s_:"
     :documentation "_s_: ASDF systems")
    (package "↩ _p_:"
     :documentation "_p_: packages")
    (readtable "↩ _n_:"
     :documentation "_n_: named readtables")
    (variable "↩ _v_:"
     :documentation "_v_: special variables and constants")
    (restart "↩ _r_:"
     :documentation "_r_: restarts")
    (t "↩ _?_:"
     :documentation "_?_: other"))
  ;; FIXME
  "When @INDEXING, a . A list of (DTYPE SUBKEY) elements. See DREF::@DTYPES.")

(defun write-referrer-abbrevs (stream)
  (format stream "Referrer definition type abbreviations:~%~%")
  (dolist (entry *document-index-referrer-dtype-abbrevs*)
    (destructuring-bind (dtype abbrev &rest plist) entry
      (if-let (doc (getf plist :documentation))
        (format stream "- ~A~%" doc)
        (format stream "- `~S` `~A`~%" abbrev (prin1-to-markdown dtype))))))

(defun dref-index-referee-abbrev (dref)
  (or (second (find (dref-locative-type dref)
                    *document-index-referee-locative-type-abbrevs*
                    :key #'first))
      (let* ((locative-type (dref-locative-type dref))
             (locative-args (dref-locative-args dref))
             (*package* (maybe-dref-name-package dref)))
        (string-downcase (format nil "_(~A~{ ~S~})_"
                                 locative-type locative-args)))))

(defun dref-index-referrer-abbrev (dref)
  (let ((abbrev (second (find dref *document-index-referrer-dtype-abbrevs*
                              :key #'first :test #'dtypep))))
    (when abbrev
      (if (or (stringp abbrev) (consp abbrev))
          abbrev
          (funcall abbrev dref)))))
