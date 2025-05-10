(in-package :mgl-pax)

(defstruct box
  (value nil))

(defmacro with-autoloading
    ((asdf-system-name
      &optional (name asdf-system-name)
        (first-time-p (gensym "FIRST-TIME-P") first-time-provided-p))
     &body body)
  (let* ((box (gensym "BOX")) (actual-body (gensym "ACTUAL-BODY"))
         (args (gensym "ARGS"))

         (stub `(lambda (,box ,first-time-p ,actual-body)
                  (declare (ignore ,first-time-p))
                  ;; Prevent this stub from being called again in case
                  ;; the system fails to load.
                  (setf (box-value ,box)
                        (lambda (&rest ,args)
                          (declare (ignore ,args))
                          (error "Autoloading ~S failed." ',name)))

                  (when (asdf:load-system ',asdf-system-name)
                    ;; If the system loads correctly, set the function
                    ;; within BOX to a trampoline to the actual body of
                    ;; the WITH-AUTOLOADING form. Note that we cannot
                    ;; simply store the current value of ACTUAL-BODY
                    ;; within BOX because that would mean the lexical
                    ;; scope of BODY would always correspond to the
                    ;; outer scope at the time of the *first*
                    ;; invocation, and not any subsequent ones.
                    (setf (box-value ,box)
                          (lambda (,box ,first-time-p ,actual-body)
                            (declare (ignore ,box))
                            (funcall ,actual-body ,first-time-p))))

                  ;; Finally, call the actual body, as appropriate.
                  (funcall (box-value ,box) ,box t ,actual-body))))
    ;; At load time, we create a BOX object containing a function
    ;; corresponding to the body to be executed by this WITH-AUTOLOADING
    ;; form. The initial value is a function that attempts to load the
    ;; ASDF system ASDF-SYSTEM-NAME, setting the value within BOX to
    ;; either a function signalling that autoloading failed, or a
    ;; trampoline to the provided BODY, depending on whether the system
    ;; loaded successfully or not. Further executions will always invoke
    ;; either the signalling function or the trampoline, without
    ;; attempting to load the ASDF system again.
    ;;
    ;; The value of the load-time-constructed box object can be thought
    ;; as behaving similarly to a C `static' variable - a binding whose
    ;; value persists between invocations.
    `(let ((,box (load-time-value (make-box :value ,stub))))
       (funcall (box-value ,box) ,box nil
                ;; We have to pass the actual body as an argument
                ;; because it needs to be compiled within the lexical
                ;; scope of the body it is enclosed under, rather than
                ;; the null lexical scope enforced within the
                ;; LOAD-TIME-VALUE form.
                (lambda (,first-time-p)
                  (declare (ignore ,box ,args)
                           ,@(unless first-time-provided-p
                               (list `(ignore ,first-time-p))))
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
         (with-autoloading (,asdf-system-name ,name first-time-p)
           (if first-time-p
               ;; Make sure that the function redefined by LOAD-SYSTEM
               ;; is invoked and not this stub, which could be the case
               ;; without the SYMBOL-FUNCTION call.
               (apply (symbol-function ',name) args)
               ;; Avoid infinite recursion in case the loaded system
               ;; fails to redefine this function.
               (error "ASDF system ~A loaded successfully, but did not ~
                       redefine ~A." ',asdf-system-name ',name))))
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
