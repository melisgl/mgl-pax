(in-package :mgl-pax)

;;; DEFUN name to be a function that loads ASDF-SYSTEM-NAME then calls
;;; the function of the same name (presumed to have been redefined by
;;; the loaded system).
(defmacro autoload (name asdf-system-name &key (export t))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (fboundp ',name)
       (declaim (notinline ,name))
       (defun ,name (&rest args)
         ;; Prevent infinite recursion which would happen if the loaded
         ;; system doesn't redefine the function.
         (setf (symbol-function ',name)
               (lambda (&rest args)
                 (declare (ignore args))
                 (error "Autoloading ~S failed." ',name)))
         (asdf:load-system ,asdf-system-name)
         ;; Make sure that the function redefined by LOAD-SYSTEM is
         ;; invoked and not this stub, which could be the case without
         ;; the SYMBOL-FUNCTION call.
         (apply (symbol-function ',name) args))
       ,@(when export
           `((export ',name))))))

(defmacro without-redefinition-warnings (&body body)
  `(locally
       (declare #+sbcl
                (sb-ext:muffle-conditions sb-kernel:redefinition-warning))
     (handler-bind
         (#+sbcl (sb-kernel:redefinition-warning #'muffle-warning))
       ,@body)))

;;; Like DEFUN, but silences redefinition warnings. We could also
;;; remember autoloaded functions (in an :AROUND-COMPILE in the ASDF
;;; system definition) and generate autoload definitions.
(defmacro defun/autoloaded (name lambda-list &body body)
  `(without-redefinition-warnings
     (defun ,name ,lambda-list
       ,@body)))
