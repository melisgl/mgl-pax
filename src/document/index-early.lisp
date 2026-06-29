(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

;;; Whether this DREF should be recorded when its generated
;;; documentation refers to another definition. For example, links
;;; from index sections are not to be indexed.
(defgeneric indexable-referrer-p (dref)
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
;;; Like the previous, but keyed by concept keys.
(defvar *indexing-concept-key-to-referrers* nil)

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
                       *indexing-concept-key-to-referrers*)
                     (list *section*
                           (make-hash-table :test #'equal)
                           (make-hash-table :test #'equal)
                           (make-hash-table :test #'equal))))
         (progv ,vars ,values
           ,@body)))))

(defmacro dref-to-referrers (dref)
  `(gethash (dref-ht-key ,dref) *indexing-dref-to-referrers*))

(defmacro concept-key-to-referrers (concept)
  ;; We consider (("x" . "a")) and (("x" . "b")) different. No good
  ;; semantics for that in sight.
  `(gethash ,concept *indexing-concept-key-to-referrers*))

(defun concept-keys (object)
  (resolve-concept-symbols-and-normalize
   (nth-value-or-with-obj-or-def (object 0)
     (concept-keys* object))))

(defun multiplexing-concept-keys (object)
  (resolve-concept-symbols-and-normalize
   (nth-value-or-with-obj-or-def (object 0)
     (multiplexing-concept-keys* object))))

(defun resolve-concept-symbols-and-normalize (keys)
  (loop for key in keys
        append (cond ((symbolp key)
                      (with-errors-downgraded-when-live ()
                        (multiplexing-concept-keys (dref key 'concept))))
                     ((index-subkey-p key)
                      (list (list key)))
                     ((atom key)
                      (assert nil () "Invalid concept key ~S" key))
                     (t
                      (list key)))))

(defun maybe-record-index-referent (dref)
  (when *indexing-section*
    (setf (gethash (dref-ht-key dref) *indexing-definitions*)
          dref)))

(defun maybe-index-link (source-dref target-dref)
  (when (and *indexing-section*
             (indexable-referrer-p source-dref)
             (not (xref= source-dref target-dref)))
    (pushnew source-dref (dref-to-referrers target-dref))
    (dolist (key (multiplexing-concept-keys target-dref))
      (pushnew source-dref (concept-key-to-referrers key)))))

(defun maybe-index-dref (dref)
  (when *indexing-section*
    (dolist (key (concept-keys dref))
      (pushnew (list :primary dref) (concept-key-to-referrers key)))))
