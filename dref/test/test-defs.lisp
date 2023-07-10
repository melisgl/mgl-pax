(in-package :dref-test)

(define-locative-type my-loc ()
  "This is MY-LOC.")

(define-locative-type loc-with-args ((x y) &key z)
  "LOC-WITH-ARGS locative")

(defun foo2 (ook x)
  "FOO2 has args OOK and X."
  (declare (ignore ook x))
  nil)

(defun traced-foo (x)
  "TRACED-FOO function"
  x)
(trace traced-foo)
(defgeneric traced-gf (x)
  (:documentation "TRACED-GF generic-function"))
(trace traced-gf)
(defun foo (ook x)
  "FOO function"
  (declare (ignore ook x))
  nil)
(define-compiler-macro foo ()
  "FOO compiler-macro"
  nil)
(define-compiler-macro cmac (x &rest y)
  "CMAC compiler-macro"
  (declare (ignore x y))
  nil)
(defclass foo (unexported-class)
  ((a :accessor foo-a
      :documentation "FOO-A (accessor foo)")
   (r :reader foo-r
      :documentation "FOO-R (reader foo)")
   (w :writer foo-w
      :documentation "FOO-W (writer foo)"))
  (:documentation "FOO class"))
(defclass unexported-class () ())
(defvar foo-a nil
  "FOO-A variable")
(defvar foo-r)
(defvar foo-w)

(defparameter *test-variable*
  '(xxx 34)
  "*TEST-VARIABLE* is not a link.")
(defvar *some-var*)

(define-restart some-restart (arg1)
  "This is SOME-RESTART with ARG1.")

(define-condition my-error (error)
  ()
  (:documentation "MY-ERROR condition"))
(defun my-error ()
  "This is MY-ERROR."
  t)

(defmacro bar (x y &key (z 7))
  "BAR macro"
  (declare (ignore x y z))
  nil)
(deftype bar (x &rest r)
  "BAR type"
  (declare (ignore x r))
  'null)
(defconstant bar 2
  "BAR constant")

(define-symbol-macro my-smac 42)
(setf (documentation 'my-smac 'symbol-macro)
      "MY-SMAC symbol-macro")

(defgeneric baz ())
;; KLUDGE: CMUCL clobbers the DEFVAR's source location with that of
;; the DEFSTRUCT if they have the same name.
(defvar bazz)
(defstruct baz
  aaa)
(setf (documentation 'baz-aaa 'function)
      "BAZ-AAA (structure-accessor baz)")

(defgeneric test-gf (x)
  (:documentation "TEST-GF generic-function"))
(defmethod test-gf ((x number))
  "TEST-GF (method () (number))"
  nil)
(defmethod test-gf ((x (eql 7))))
(defmethod test-gf ((x (eql #.(find-package :common-lisp)))))

(defgeneric gf2 (x &key &allow-other-keys)
  (:method :around (x &key)
    (declare (ignore x)))
  (:method :after (x &key y)
    (declare (ignore x y)))
  (:method ((x number) &key ((:x y) t))
    (declare (ignore y))))

(define-method-combination my-comb :identity-with-one-argument t
  :documentation "MY-COMB method-combination")

(defun ->max ())

#+sbcl
(require :sb-cltl2)

(defmacro define-declaration (name)
  #+allegro
  `(system:define-declaration ,name () nil :both)
  #+ccl
  `(ccl:define-declaration ,name (decl-spec env)
     (declare (ignore env))
     (values :declare decl-spec))
  #+sbcl
  `(sb-cltl2:define-declaration ,name (decl-spec env)
     (declare (ignore env))
     (values :declare decl-spec)))

(define-declaration test-declaration)

(unless (named-readtables:find-readtable 'xxx-rt)
  (named-readtables:defreadtable xxx-rt
    ;; KLUDGE: ABCL bundles an older named-readtables version that
    ;; does not support docstrings.
    #-abcl
    "ddd"))

(defsetf has-setf-expander some-setter "HAS-SETF-EXPANDER setf")

(defun (setf has-setf-function) (v)
  "HAS-SETF-FUNCTION setf"
  (declare (ignore v)))

(defgeneric (setf has-setf-generic-function) (v)
  (:documentation "HAS-SETF-GENERIC-FUNCTION setf"))

(defmethod (setf has-setf-generic-function) ((v string))
  "HAS-SETF-GENERIC-FUNCTION (setf (method () (string)))"
  ())

(define-setf-expander full-setf (x)
  (declare (ignore x)))

(defun full-setf ())

(setf (macro-function 'setfed-macro)
      (lambda (whole env)
        (declare (ignore whole env))))

(defmacro macro-with-fancy-args (x &optional (o 1) &key (k 2 kp))
  (declare #+sbcl (sb-ext:muffle-conditions style-warning)
           (ignore x o k kp))
  ())

(defmacro macro-with-local-key ((&key a) (b print))
  (declare #+sbcl (sb-ext:muffle-conditions style-warning)
           (ignore a b print))
  ())

(defun function-with-fancy-args (x &optional (o 1) &key (k 2 kp))
  (declare #+sbcl (sb-ext:muffle-conditions style-warning)
           (ignore x o k kp))
  nil)

(define-symbol-locative-type sloc (&optional nested)
  "SLOC locative")

(define-definer-for-symbol-locative-type define-sloc sloc)

(define-sloc sloc1 (&key z)
  "SLOC1 sloc")

(define-locative-alias object class)
(define-locative-alias var variable
  "VAR locative")
