(in-package :mgl-pax-test)

;;;; Tests for the Lisp interaction mode protocol (DEFINTERFACE,
;;;; DEFIMPL, WITH-LISP-INTERACTION-MODE). These do not need SLIME,
;;;; SLY, Swank backends or an Emacs connection.

(deftest test-interaction-mode ()
  (test-with-lisp-interaction-mode)
  (test-definterface)
  (test-with-binding-defaults*))


;;;; A made-up interface to test the macros with

(pax::definterface %test-iface (a &optional b)
  (:method (backend a &optional b)
    (list :default backend a b)))

(pax::definterface %test-iface-key (a &key b)
  (:method (backend a &key b)
    (list :default backend a b)))

(pax::defimpl %test-iface :test-mode (a &optional b)
  (list :impl a b))

(pax::defimpl %test-iface-key :test-mode (a &key b)
  (list :impl a b))

(deftest test-definterface ()
  (with-test ("default method, the mode is NIL")
    (is (equal (%test-iface 1) '(:default nil 1 nil)))
    (is (equal (%test-iface 1 2) '(:default nil 1 2)))
    (is (equal (%test-iface-key 1) '(:default nil 1 nil)))
    (is (equal (%test-iface-key 1 :b 3) '(:default nil 1 3))))
  (with-test ("an implementation is selected by the current mode")
    (pax::with-lisp-interaction-mode (:test-mode)
      (is (equal (%test-iface 1) '(:impl 1 nil)))
      (is (equal (%test-iface 1 2) '(:impl 1 2)))
      (is (equal (%test-iface-key 1) '(:impl 1 nil)))
      (is (equal (%test-iface-key 1 :b 3) '(:impl 1 3)))))
  (with-test ("a mode without an implementation gets the default method")
    (pax::with-lisp-interaction-mode (:no-such-mode)
      (is (equal (%test-iface 1 2) '(:default :no-such-mode 1 2)))
      (is (equal (%test-iface-key 1 :b 3) '(:default :no-such-mode 1 3)))))
  (with-test ("the generic function takes the backend as its first argument")
    (is (equal (%test-iface/backend :test-mode 1 2) '(:impl 1 2)))
    (is (equal (%test-iface/backend :other 1) '(:default :other 1 nil)))
    (is (equal (%test-iface-key/backend :test-mode 1 :b 3) '(:impl 1 3)))))


;;;; WITH-LISP-INTERACTION-MODE

(deftest test-with-lisp-interaction-mode ()
  (let ((saved pax::*latest-interaction-mode*))
    (unwind-protect
         (progn
           (is (null pax::*current-interaction-mode*))
           (pax::with-lisp-interaction-mode (:foo)
             (is (eq pax::*current-interaction-mode* :foo))
             (is (eq pax::*latest-interaction-mode* :foo))
             (pax::with-lisp-interaction-mode (:bar)
               (is (eq pax::*current-interaction-mode* :bar)))
             (is (eq pax::*current-interaction-mode* :foo)))
           (with-test ("the current mode is dynamically scoped")
             (is (null pax::*current-interaction-mode*)))
           (with-test ("the latest mode outlives the dynamic extent (the web
server thread relies on this)")
             (is (eq pax::*latest-interaction-mode* :bar))))
      (setf pax::*latest-interaction-mode* saved))))


;;;; WITH-BINDING-DEFAULTS*

(defvar *test-unbound-variable*)
(defvar *test-bound-variable* :global)

(deftest test-with-binding-defaults* ()
  (with-test ("unbound variables get the default, bound ones keep their value")
    (is (equal (pax::with-binding-defaults* ((*test-unbound-variable* 1)
                                             (*test-bound-variable* 2))
                 (list *test-unbound-variable* *test-bound-variable*))
               '(1 :global))))
  (with-test ("the bindings are undone afterwards")
    (is (not (boundp '*test-unbound-variable*)))
    (is (eq *test-bound-variable* :global)))
  (with-test ("later defaults can refer to earlier variables")
    (is (equal (pax::with-binding-defaults* ((*test-unbound-variable* 1)
                                             (*test-bound-variable*
                                              (1+ *test-unbound-variable*)))
                 (list *test-unbound-variable* *test-bound-variable*))
               '(1 :global)))))
