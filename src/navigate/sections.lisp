(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

;;;; Utilities for listing SECTIONs

(defvar *all-sections*)
(defvar *definition-to-parents*)

(defun new-section-cache ()
  (let ((all-sections (mapcar #'resolve
                              (dref-apropos nil :dtype 'section :sort nil)))
        (definition-to-parents (make-hash-table :test #'equal)))
    (dolist (section all-sections)
      (dolist (entry (section-entries section))
        (when-let (dref (and (typep entry 'xref)
                             (locate entry nil)))
          (pushnew section (gethash (dref-ht-key dref)
                                    definition-to-parents)))))
    (list all-sections definition-to-parents)))

(defmacro with-sections-cache (() &body body)
  `(progv (unless (boundp '*all-sections*)
            '(*all-sections* *definition-to-parents*))
       (unless (boundp '*all-sections*)
         (new-section-cache))
     ,@body))

(defun list-all-sections ()
  *all-sections*)

(defun find-parent-sections (object)
  (let ((dref (locate object)))
    (sort-by-proximity (gethash (dref-ht-key dref) *definition-to-parents*)
                       (dref-name dref))))

(defun sort-by-proximity (sections object)
  (if (and (symbolp object)
           (not (keywordp object)))
      (let ((package-name (package-name (symbol-package object))))
        (sort (copy-list sections) #'>
              :key (lambda (section)
                     (or (mismatch package-name
                                   (package-name
                                    (symbol-package
                                     (section-name section))))
                         most-positive-fixnum))))
      sections))

(defun find-root-section (object)
  (let ((sectionp (typep (resolve object nil) 'section))
        (visited (make-hash-table :test #'eq)))
    (multiple-value-bind (section depth)
        (if sectionp
            (values object 0)
            (values (first (find-parent-sections object)) 1))
      (loop for parent = (and section (first (find-parent-sections section)))
            while parent
            do (when (gethash parent visited)
                 (warn "~@<Cycle in section hierarchy involving ~S and ~S.~:@>"
                       section parent)
                 (return (values section depth)))
               (setf (gethash parent visited) t)
               (setq section parent)
               (incf depth)
            finally (return (values section depth))))))
