(in-package :dref)

;;; ALEXANDRIA is not yet avaiable.
(defun hash-table-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

;;; Return the names of the arguments in the [macro lambda list][clhs]
;;; ARGLIST.
(defun macro-arg-names (arglist)
  (unless (eq arglist :not-available)
    (let ((names ()))
      (labels ((foo (arglist)
                 (let ((seen-special-p nil))
                   (loop for rest on arglist
                         do (let ((arg (car rest)))
                              (cond ((member arg '(&key &optional &rest &body
                                                   &allow-other-keys))
                                     (setq seen-special-p t))
                                    ((symbolp arg)
                                     (push arg names))
                                    (seen-special-p
                                     (when (symbolp (first arg))
                                       (push (first arg) names)))
                                    (t
                                     (foo arg))))
                            (unless (listp (cdr rest))
                              (push (cdr rest) names))))))
        (foo arglist))
      (reverse names))))

