;;;; An abstraction over SLIME and SLY

(in-package :mgl-pax)

;;;; Machinery

(defvar *current-interaction-mode* nil)
(defvar *latest-interaction-mode* nil)

(defun call-with-lisp-interaction-mode (interaction-mode thunk)
  (when (eq interaction-mode :sly)
    (check-sly))
  (let ((*current-interaction-mode* interaction-mode))
    (setq *latest-interaction-mode* *current-interaction-mode*)
    (dref:with-backend (:swank)
      (funcall thunk))))

(defmacro with-lisp-interaction-mode ((interaction-mode) &body body)
  `(call-with-lisp-interaction-mode ,interaction-mode (lambda () ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun backend-function-name-for (symbol)
    (intern (format nil "~a~a" symbol '/backend) (symbol-package symbol))))

(defmacro definterface (name (&rest lambda-list) &body defgeneric-body)
  (multiple-value-bind (required optional rest keyword aok? aux key?)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare (ignore aux))
    (labels ((ensure-suppliedp (binding prefix)
               (destructuring-bind (name default-value suppliedp) binding
                 (list name default-value
                       (or suppliedp (gensym (string prefix)))))))
      (let* ((generic-function (backend-function-name-for name))
             (optional (mapcar
                        (lambda (arg) (ensure-suppliedp arg (first arg)))
                        optional))
             (keyword (mapcar
                       (lambda (arg)
                         (ensure-suppliedp arg (second (first arg)))) keyword))
             (wrapper-lambda-list
               `(,@required
                 ,@(when optional `(&optional ,@optional))
                 ,@(when rest `(&rest ,rest))
                 ,@(when key? `(&key ,@keyword))
                 ,@(when aok? `(&allow-other-keys)))))
        `(progn
           (defgeneric ,generic-function (backend ,@lambda-list)
             ,@defgeneric-body)
           (defun ,name ,wrapper-lambda-list
             (multiple-value-call #',generic-function
               *current-interaction-mode*
               ,@required
               ,@(loop for (value nil providedp) in optional
                       collect `(if ,providedp (values ,value) (values)))
               ,@(if rest
                     `((values-list ,rest))
                     (loop for ((keyword value) nil providedp) in keyword
                           collect `(if ,providedp
                                        (values ,keyword ,value)
                                        (values))))))
           ',name)))))

(defmacro defimpl (name interaction-mode (&rest lambda-list) &body body)
  `(defmethod ,(backend-function-name-for name)
       ((backend (eql ',interaction-mode)) ,@lambda-list)
     ,@body))


;;;; Interface

(definterface buffer-package ()
  (:method (backend)
    *package*))

(definterface buffer-readtable ()
  (:method (backend)
    (swank::guess-buffer-readtable swank::*buffer-package*)))

(definterface call-with-swank-compatibility (thunk)
  (:method (backend thunk)
    (with-binding-defaults* ((swank::*buffer-package* (buffer-package))
                             (swank::*buffer-readtable* (buffer-readtable)))
      (funcall thunk))))

(defmacro with-swank-compatibility (() &body body)
  `(call-with-swank-compatibility (lambda () ,@body)))

(definterface default-connection ()
  (:method (backend) nil))

(definterface call-with-connection (connection thunk))

(defmacro with-connection ((connection) &body body)
  `(call-with-connection ,connection (lambda () ,@body)))

(definterface eval-in-emacs (form &optional nowait))


;;;; SLIME implementation

(defimpl default-connection :slime ()
  (swank::default-connection))

(defimpl call-with-connection :slime (connection thunk)
  (swank::with-connection (connection)
    (funcall thunk)))

(defimpl eval-in-emacs :slime (form &optional nowait)
  (swank::eval-in-emacs form nowait))


;;;; SLY implementation
;;;;
;;;; We only need SLYNK if running under SLY in Emacs, in which case
;;;; SLYNK is loaded already. So, PAX systems get away with no
;;;; dependency on SLYNK, but we need to wrap the SLYNK access in
;;;; READ-FROM-STRING to make things work when SLYNK is not loaded.
;;;;
;;;; This soft dependency on SLYNK allows MGL-PAX/FULL to include SLY
;;;; support regardless of the presence of SLYNK.

(defun check-sly ()
  (unless (find-package :slynk)
    (error "~@<In Emacs, the value of mgl-pax-backend is :sly, ~
           but there is no SLYNK package on the CL side.~:@>")))

(defimpl default-connection :sly ()
  (funcall (read-from-string "slynk-api:default-connection")))

(let ((fn nil))
  (defimpl call-with-connection :sly (connection thunk)
    (unless fn
      (setq fn (let ((*package* (find-package :mgl-pax)))
                 (compile nil (read-from-string
                               "(lambda (connection thunk)
                                  (slynk-api:with-connection (connection)
                                    (funcall thunk)))")))))
    (funcall fn connection thunk)))

(defimpl eval-in-emacs :sly (form &optional nowait)
  (funcall (read-from-string "slynk:eval-in-emacs") form nowait))

(defimpl buffer-package :sly ()
  (symbol-value (read-from-string "slynk-api:*buffer-package*")))

(defimpl buffer-readtable :sly ()
  (symbol-value (read-from-string "slynk-api:*buffer-readtable*")))
