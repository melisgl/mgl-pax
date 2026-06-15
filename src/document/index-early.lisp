(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

;;; Whether this DREF should be recorded when its generated
;;; documentation refers to another definition. For example, index
;;; sections are not indexable although they link to lots of stuff.
(defgeneric indexablep (dref)
  (:method (dref)
    t))

;;; Whether documentation can be generated for DREF in isolation (e.g.
;;; outside the context of its parent). A definition can be
;;; IN-CONTEXT-ONLY-P if its documentation depends on the
;;; documentation of its parent. See MAYBE-GENERATE-INDICES.
(defgeneric in-context-only-p (dref)
  (:method (dref)
    nil))

;;; The outermost section being documented. It's always explicitly on
;;; @DOCUMENTABLE.
(defvar *top-level-section* nil)
;;; The root of the indexing context.
(defvar *indexing-section* nil)
;;; Like PAGE-DEFINITIONS, but only lists the definitions documented
;;; under *INDEXING-SECTION* in the 2nd pass.
(defvar *indexing-definitions* nil)
;;; Map DREFs in *INDEXING-DEFINITIONS* to the list of DREFs whose
;;; docstrings reference them. For constructing indices. Populated in
;;; the 2nd pass.
(defvar *indexing-dref-to-referrers* nil)
;;; Like the previous, but keyed by index keys.
(defvar *indexing-key-to-referrers* nil)

(declaim (special *section*))

(defun indexing-context-boundary-p ()
  (and (null *indexing-section*)
       (member *real-format* *document-index-formats*)
       (ecase *document-index-sections*
         ((nil) nil)
         ((:documentable)
          (eq *section* *top-level-section*))
         ((:homeless-documentable)
          (and (eq *section* *top-level-section*)
               (null (home-section *section*)))))))

(defmacro with-indexing-context (() &body body)
  (with-gensyms (vars values)
    `(let ((*top-level-section* (or *top-level-section* *section*)))
       (multiple-value-bind (,vars ,values)
           (when (indexing-context-boundary-p)
             (values '(*indexing-section*
                       *indexing-definitions*
                       *indexing-dref-to-referrers*
                       *indexing-key-to-referrers*)
                     (list *section*
                           ()
                           (make-hash-table :test #'equal)
                           (make-hash-table :test #'equal))))
         (progv ,vars ,values
           ,@body)))))

(defmacro dref-to-referrers (dref)
  `(gethash ,(once-only (dref)
               `(cons (dref-name ,dref) (dref-locative ,dref)))
            *indexing-dref-to-referrers*))

(defmacro index-key-to-referrers (concept)
  `(gethash ,concept *indexing-key-to-referrers*))

(defun index-keys (object)
  (resolve-concept-symbols
   (nth-value-or-with-obj-or-def (object 0)
     (index-keys* object))))

(defun multiplexing-index-keys (object)
  (resolve-concept-symbols
   (nth-value-or-with-obj-or-def (object 0)
     (multiplexing-index-keys* object))))

(defun resolve-concept-symbols (list)
  (if (find-if #'symbolp list)
      (loop for x in list
            append (if (symbolp x)
                       (with-errors-downgraded-when-open-linking ()
                         (multiplexing-index-keys (dref x 'concept)))
                       (list x)))
      list))

(defun maybe-index-link (source-dref target-dref)
  (when (and *indexing-section*
             (indexablep source-dref)
             (not (xref= source-dref target-dref)))
    (pushnew source-dref (dref-to-referrers target-dref))
    (dolist (key (multiplexing-index-keys target-dref))
      (pushnew source-dref (index-key-to-referrers key)))))

(defun maybe-index-dref (dref)
  (when (and *indexing-section*
             (indexablep dref))
    (dolist (key (index-keys dref))
      (pushnew dref (index-key-to-referrers key)))))
