(in-package :mgl-pax)

(defstruct box
  (value nil))

(defmacro with-autoloading
    ((asdf-system-name &optional (name asdf-system-name)) &body body)
  (let* ((box (gensym "BOX")) (actual-body (gensym "ACTUAL-BODY"))
         (args (gensym "ARGS"))

         (stub `(lambda (,box ,actual-body)
                  ;; Prevent this stub from being called again in case
                  ;; the system fails to load.
                  (setf (box-value ,box)
                        (lambda (&rest ,args)
                          (declare (ignore ,args))
                          (error "Autoloading ~S failed." ',name)))
                  (when (asdf:load-system ',asdf-system-name)
                    (setf (box-value ,box) ,actual-body))
                  (funcall (box-value ,box) ,box))))
    `(let ((,box (load-time-value (make-box :value ,stub))))
       (funcall (box-value ,box) ,box
                (lambda (&rest ,args)
                  (declare (ignore ,args))
                  ,@body)))))

;;; Define a function with NAME that loads ASDF-SYSTEM-NAME (neither
;;; evaluated) that calls the function of the same name, which is
;;; expected to have been redefined by the loaded system. If not
;;; redefined, then an error will be signalled and all subsequent
;;; calls to the function will produce the same error without
;;; attempting to load the system again.
(defmacro autoload (name asdf-system-name &key (export t))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (fboundp ',name)
       (declaim (notinline ,name))
       ;; Workaround for the CMUCL bug that results in "Function with
       ;; declared result type NIL returned" errors when the real
       ;; function definition returns.
       #+cmucl
       (declaim (ftype function ,name))
       (defun ,name (&rest args)
         ,(format nil "Autoloaded function in system [~A][asdf:system]."
                  ;; ESCAPE-MARKDOWN, which should be used here, is
                  ;; itself autoloaded. This should be fine because
                  ;; asdf system names are rarely funny.
                  asdf-system-name)
         (with-autoloading (,asdf-system-name ,name)
           ;; Make sure that the function redefined by LOAD-SYSTEM is
           ;; invoked and not this stub, which could be the case without
           ;; the SYMBOL-FUNCTION call.
           (apply (symbol-function ',name) args)))
       ,@(when export
           `((export ',name))))))

(defmacro without-redefinition-warnings (&body body)
  #+sbcl
  `(locally
       (declare (sb-ext:muffle-conditions sb-kernel:redefinition-warning))
     (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
       ,@body))
  #-sbcl
  `(progn ,@body))

;;; Like DEFUN but silences redefinition warnings. We could also
;;; remember autoloaded functions (in an :AROUND-COMPILE in the ASDF
;;; system definition) and generate autoload definitions.
(defmacro defun/autoloaded (name lambda-list &body body)
  (unless (ignore-errors (fdefinition name))
    (warn "~S function ~S not defined." 'defun/autoloaded name))
  `(without-redefinition-warnings
     (defun ,name ,lambda-list
       ,@body)))
