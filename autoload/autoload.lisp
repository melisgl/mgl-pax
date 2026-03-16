(in-package :autoload)

(defmacro autoload (name asdf-system-name &key (export t))
  "Unless NAME already has an FDEFINITION, define a stub function with
  NAME to autoload ASDF-SYSTEM-NAME. When called, this stub will do
  the following:

  1. Set the FDEFINITION of NAME to a function that signals an error.
  2. Call ASDF:LOAD-SYSTEM on ASDF-SYSTEM-NAME.
  3. Call NAME with the original arguments.

  Thus, the loaded system is expected to redefine the stub. If it
  doesn't, then an error will be signalled and all subsequent calls to
  the function will produce the same error without attempting to load
  the system again.

  When EXPORT is true and NAME is a symbol, then also EXPORT NAME from
  *PACKAGE*.

  The stub is not defined at [compile time][pax:clhs], which matches
  the required semantics of DEFUN. Exporting behaves similary."
  `(progn
     (eval-when (:compile-toplevel)
       ;; This is mainly to prevent undefined function compilation
       ;; warnings about NAME.
       ;;
       ;; Also, this works around a CMUCL bug that results in
       ;; "Function with declared result type NIL returned" errors
       ;; when the real function definition returns.
       (declaim (ftype function ,name)))
     (eval-when (:load-toplevel :execute)
       (unless (ignore-errors (fdefinition ',name))
         (declaim (notinline ,name))
         (defun ,name (&rest args)
           ;; ~A ASDF:SYSTEM is autolinked by PAX in generated
           ;; documentation. To be really correct, we should
           ;; PAX:ESCAPE-MARKDOWN the name, but we cannot because
           ;; mgl-pax-bootstrap depends on autoload. If needed, PAX
           ;; could patch this up, but ASDF system names are rarely
           ;; funny.
           ,(format nil "Autoloaded function in the ~A ASDF:SYSTEM."
                    asdf-system-name)
           ;; Prevent infinite recursion which would happen if the
           ;; loaded system doesn't redefine the function.
           (setf (fdefinition ',name)
                 (lambda (&rest args)
                   (declare (ignore args))
                   (error "~@<Autoloaded function ~S was not redefined ~
                         by the ~A ASDF:SYSTEM.~:@>"
                          ',name ,asdf-system-name)))
           (asdf:load-system ,asdf-system-name)
           ;; Make sure that the function redefined by ASDF:LOAD-SYSTEM
           ;; is invoked and not this stub, which could be the case
           ;; without the FDEFINITION call.
           (apply (fdefinition ',name) args)))
       ,@(when (and export (symbolp name))
           `((export ',name))))))

(defmacro without-redefinition-warnings (&body body)
  #+sbcl
  `(locally
       (declare (sb-ext:muffle-conditions sb-kernel:redefinition-warning))
     (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
       ,@body))
  #-sbcl
  `(progn ,@body))

(defmacro defun/autoloaded (name lambda-list &body body)
  "Like DEFUN, but silence redefinition warnings."
  ;; We could also remember autoloaded functions (e.g. in an
  ;; :AROUND-COMPILE in the ASDF system definition) and generate
  ;; autoload definitions.
  `(progn
     (unless (ignore-errors (fdefinition ',name))
       (warn "~S function ~S not defined." 'defun/autoloaded ',name))
     (without-redefinition-warnings
       (defun ,name ,lambda-list
         ,@body))))

(defmacro defvar/autoloaded (var &optional (val nil valp) (doc nil docp))
  "Like DEFVAR, but works with the global binding on Lisps that
  support it (currently Allegro, CCL, ECL, SBCL). This is to
  handle the case when a system that uses DEFVAR with a default value
  is autoloaded while that variable is locally bound:

  ```cl-transcript
  ;; Some base system only foreshadows *X*.
  (declaim (special *x*))
  (let ((*x* 1))
    ;; Imagine that the system that defines *X* is autoload here.
    (defvar/autoloaded *x* 2)
    *x*)
  => 1
  ```"
  (assert (special-variable-name-p var))
  `(progn
     (defvar ,var)
     ,@(when valp
         `((unless (symbol-globally-boundp ',var)
             (setf (symbol-global-value ',var) ,val))))
     ,@(when docp
         `((setf (documentation ',var 'variable) ,doc)))))

(defun special-variable-name-p (obj)
  (and (symbolp obj)
       #+abcl (ext:special-variable-p obj)
       #+allegro (eq (sys:variable-information obj) :special)
       #+ccl (eq (ccl::variable-information obj) :special)
       #+clisp (and (ext:special-variable-p obj)
                    (not (constant-variable-name-p obj)))
       #+cmucl (eq (ext:info :variable :kind obj) :special)
       #+ecl (or (si:specialp obj)
                 (constant-variable-name-p obj))
       #+sbcl (member (sb-int:info :variable :kind obj) '(:special))))

(defun constant-variable-name-p (obj)
  (and (symbolp obj)
       (not (keywordp obj))
       ;; CONSTANTP may detect constant symbol macros, for example.
       (boundp obj)
       (constantp obj)))


;;;; Global bindings of specials
;;;;
;;;; On Lisps that don't support access to global bindings, we fall
;;;; back to the current binding.

(defun symbol-globally-boundp (symbol)
  #-ecl (null (nth-value 1 (symbol-global-value symbol)))
  #+ecl (ffi:c-inline (symbol) (:object) :object
                      "(#0->symbol.value == OBJNULL) ? ECL_NIL : ECL_T"
                      :one-liner t))

(defun symbol-global-value (symbol)
  (check-type symbol symbol)
  #+allegro
  (multiple-value-bind (value bound) (sys:global-symbol-value symbol)
    (values value (eq bound :unbound)))
  #+ccl
  (let ((value (ccl::%sym-global-value symbol)))
    (values value (eq value (ccl::%unbound-marker))))
  #+ecl
  (if (symbol-globally-boundp symbol)
      (values (ffi:c-inline (symbol) (:object) :object
                            "#0->symbol.value" :one-liner t)
              nil)
      (values nil t))
  #+sbcl
  (ignore-errors (sb-ext:symbol-global-value symbol))
  #-(or allegro ccl ecl sbcl)
  (ignore-errors (symbol-value symbol)))

(defun set-symbol-global-value (symbol value)
  #+allegro
  (setf (sys:global-symbol-value symbol) value)
  #+ccl
  (ccl::%set-sym-global-value symbol value)
  #+ecl
  (progn (ffi:c-inline (symbol value) (:object :object) :void
                       "#0->symbol.value = #1"
                       :one-liner t)
         value)
  #+sbcl
  (setf (sb-ext:symbol-global-value symbol) value)
  #-(or allegro ccl ecl sbcl)
  (setf (symbol-value symbol) value))

(defsetf symbol-global-value set-symbol-global-value)
