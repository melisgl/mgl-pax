(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

;;;; Dynamically generated sections

(define-locative-type dynamic-section (section)
  "nothing"
  (defclass dynamic-section-dref ()
    ((title :initarg :title :reader doctitle*)
     (numberedp :initform t :initarg :numbered :reader numberedp)
     (indexable-referrer :initform t :initarg :indexable-referrer
                         :reader indexable-referrer-p)
     (in-context-only :initform nil :initarg :in-context-only
                      :reader in-context-only-p)
     (anchor :initform nil)
     (fn :initarg :fn :reader fn-of))))

(defmethod resolve* ((dref dynamic-section-dref))
  (resolve-error))

;;; Create a SECTION, and generate documentation for it.
(defmacro with-dynamic-section ((stream &key title (numbered t)
                                 (indexable-referrer t) in-context-only)
                                &body body)
  (assert (symbolp stream))
  `(call-with-dynamic-section ,stream ,title ,numbered ,indexable-referrer
                              ,in-context-only (lambda (,stream) ,@body)))

(defun call-with-dynamic-section (stream title numbered indexable-referrer
                                  in-context-only fn)
  (document-object
   (let* ((id (incf (page-next-dyn-id *page*)))
          ;; Dynamic section from the previous pass, if any.
          (prev (gethash id (page-id-to-dynamic-section *page*))))
     (cond (prev
            (assert (equal title (doctitle* prev)))
            (assert (eq (not indexable-referrer)
                        (not (indexable-referrer-p prev))))
            (assert (eq (not in-context-only) (not (in-context-only-p prev))))
            (setf (slot-value prev 'fn) fn)
            prev)
           (t
            (setf (gethash id (page-id-to-dynamic-section *page*))
                  (make-instance 'dynamic-section-dref
                                 ;; Keep the DREF-HT-KEY unique across pages.
                                 :name (format nil "~S-~S"
                                               (position *page* *pages*)
                                               id)
                                 :locative 'dynamic-section
                                 :title title
                                 :numbered numbered
                                 :indexable-referrer indexable-referrer
                                 :in-context-only in-context-only
                                 :fn fn)))))
   stream))

(defmethod document-object* ((dref dynamic-section-dref) stream)
  (with-heading (stream :dref dref :numbered (numberedp dref))
    (funcall (fn-of dref) stream)))

(defmethod dref-to-anchor ((dref dynamic-section-dref))
  (dynamic-section-anchor dref))

(defmethod dref-to-anchor-v1 ((dref dynamic-section-dref))
  (dynamic-section-anchor dref))

(defun dynamic-section-anchor (dref)
  (with-slots (anchor title) dref
    (or anchor
        (let ((proposed-anchor title)
              (used (page-used-dynamic-anchors *page*)))
          (loop for i upfrom 1
                while (gethash proposed-anchor used)
                do (setq proposed-anchor (format nil "~A-~A" title i)))
          (setf (gethash proposed-anchor used) t)
          (setf anchor proposed-anchor)))))
