(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

;;;; For DYNAMIC-SECTION-DREF

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
(defvar *indexing-section* nil)
;;; Like PAGE-DEFINITIONS, but only lists the definitions documented
;;; under *INDEXING-SECTION* in the 2nd pass.
(defvar *indexing-definitions* nil)
;;; Map DREFs in *INDEXING-DEFINITIONS* to the list of DREFs whose
;;; docstrings reference them. For constructing indices. Populated in
;;; the 2nd pass.
(defvar *indexing-dref-to-referrers* nil)
;;; Like PAGE-REFERRERS, but the keyed by concept.
(defvar *indexing-concept-to-referrers* nil)

(defmacro dref-to-referrers (dref)
  `(gethash ,(once-only (dref)
               `(cons (dref-name ,dref) (dref-locative ,dref)))
            *indexing-dref-to-referrers*))

(defmacro concept-to-referrers (concept)
  `(gethash ,concept *indexing-concept-to-referrers*))

(defmacro with-indexing-context ((section) &body body)
  (with-gensyms (vars values)
    `(multiple-value-bind (,vars ,values)
         (when (and *document-index-sections*
                    (null *indexing-section*)
                    (member *real-format* *document-index-formats*))
           (values '(*indexing-section*
                     *indexing-definitions*
                     *indexing-dref-to-referrers*
                     *indexing-concept-to-referrers*)
                   (list ,section
                         ()
                         (make-hash-table :test #'equal)
                         (make-hash-table :test #'equal))))
       (progv ,vars ,values
         ,@body))))
