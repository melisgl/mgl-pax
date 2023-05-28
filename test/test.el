(require 'mgl-pax)
(require 'ert)
(require 'cl-lib)

(cl-defmacro with-temp-lisp-buffer (&body body)
  `(with-temp-buffer
     (lisp-mode)
     ,@body))


;;;; test `mgl-pax-object-and-locatives-list-at-point'

(ert-deftest test-mgl-pax-object-and-locatives-list-at-point/simple-1 ()
  (with-temp-lisp-buffer
   (should (equal (mgl-pax-object-and-locatives-list-at-point)
                  '()))
   (insert "xxx")
   (should (equal (mgl-pax-object-and-locatives-list-at-point)
                  '(("xxx" ()))))
   (insert " sym")
   (should (equal (mgl-pax-object-and-locatives-list-at-point)
                  '(("sym" ("xxx")))))
   (save-excursion
     (insert " function"))
   (should (equal (mgl-pax-object-and-locatives-list-at-point)
                  '(("sym" ("xxx" "function")))))))

;;; xxx (FOO) yyy
(ert-deftest test-mgl-pax-object-and-locatives-list-at-point/simple-2 ()
  (with-temp-lisp-buffer
   (insert "xxx (foo")
   (save-excursion
     (insert ") yyy"))
   (should (equal (mgl-pax-object-and-locatives-list-at-point)
                  '(("foo" ()))))))

;;; xxx ((FOO)) yyy
(ert-deftest test-mgl-pax-object-and-locatives-list-at-point/simple-3 ()
  (with-temp-lisp-buffer
   (insert "xxx ((foo")
   (save-excursion
     (insert ")) yyy"))
   (should (equal (mgl-pax-object-and-locatives-list-at-point)
                  '(("foo" ()))))))

;;; xxx [FOO][function] yyy
(ert-deftest test-mgl-pax-object-and-locatives-list-at-point/reflink-1 ()
  (with-temp-lisp-buffer
   (insert "xxx [foo")
   (save-excursion
     (insert "][function] yyy"))
   (should (equal (mgl-pax-object-and-locatives-list-at-point)
                  '(("[foo][function]" ("xxx" "yyy"))
                    ("foo" ("function")))))))

;;; xxx [FOO][(function)] yyy
(ert-deftest test-mgl-pax-object-and-locatives-list-at-point/reflink-2 ()
  (with-temp-lisp-buffer
   (insert "xxx [foo")
   (save-excursion
     (insert "][(function)] yyy"))
   (should (equal (mgl-pax-object-and-locatives-list-at-point)
                  '(("[foo][" ("xxx" "(function)"))
                    ("foo" ("(function)")))))))

;;; xxx [`FOO`][function] yyy
(ert-deftest test-mgl-pax-object-and-locatives-list-at-point/reflink-3 ()
  (with-temp-lisp-buffer
   (insert "xxx [`foo")
   (save-excursion
     (insert "`][function] yyy"))
   (should (equal (mgl-pax-object-and-locatives-list-at-point)
                  '(("foo" ("[" "function")))))))

;;; xxx [FOO ][function] yyy
(ert-deftest test-mgl-pax-object-and-locatives-list-at-point/reflink-4 ()
  (with-temp-lisp-buffer
   (insert "xxx [foo")
   (save-excursion
     (insert " ][function] yyy"))
   (should (equal (mgl-pax-object-and-locatives-list-at-point)
                  '(("[foo" ("xxx" "function"))
                    ("foo" ("function")))))))

;;; xxx [FOO][ function] yyy
(ert-deftest test-mgl-pax-object-and-locatives-list-at-point/reflink-5 ()
  (with-temp-lisp-buffer
   (insert "xxx [foo")
   (save-excursion
     (insert "][ function] yyy"))
   (should (equal (mgl-pax-object-and-locatives-list-at-point)
                  '(("[foo][" ("xxx" "function"))
                    ("foo" ("function")))))))

;;; xxx [see also][FOO function] zzz
(ert-deftest test-mgl-pax-object-and-locatives-list-at-point/reflink-6 ()
  (with-temp-lisp-buffer
   (insert "xxx [see also][foo")
   (save-excursion
     (insert " function] yyy"))
   (should (equal (mgl-pax-object-and-locatives-list-at-point)
                  '(("also][foo" ("[see" "function"))
                    ("foo" ("function")))))))


;;;; test `mgl-pax-current-definition-possible-names'

(ert-deftest test-mgl-pax-current-definition-possible-names/simple ()
  (with-temp-lisp-buffer
   (let* ((s "(defun foo () t)")
          (l (length s)))
     (insert " ")
     (insert s)
     (insert " ")
     (goto-char 1)
     (should (equal (mgl-pax-current-definition-possible-names)
                    ()))
     (cl-loop for pos upfrom 2 below (+ 2 l)
              do (goto-char pos)
              (should (equal (mgl-pax-current-definition-possible-names)
                             '(("foo" "(defun foo () t)" 2)))))
     (goto-char (+ 2 l))
     (should (equal (mgl-pax-current-definition-possible-names)
                    ())))))

(ert-deftest test-mgl-pax-current-definition-possible-names/string-name ()
  (with-temp-lisp-buffer
   (insert "(defsystem \"a-name\" () t)")
   (goto-char 1)
   (should (equal (mgl-pax-current-definition-possible-names)
                  '(("\"a-name\"" "(defsystem \"a-name\" () t)" 1))))))

(ert-deftest test-mgl-pax-current-definition-possible-names/list-name ()
  (with-temp-lisp-buffer
   (insert "(list () foo t)")
   (goto-char 1)
   (should (equal (mgl-pax-current-definition-possible-names)
                  '()))))

(ert-deftest test-mgl-pax-current-definition-possible-names/wrapped ()
  (with-temp-lisp-buffer
   (insert "(locally (defun foo () t)")
   (goto-char 13)
   (should (equal (mgl-pax-current-definition-possible-names)
                  '(("foo" "(defun foo () t)" 10))))))

(ert-deftest test-mgl-pax-current-definition-possible-names/wrapped-2 ()
  (with-temp-lisp-buffer
   (insert "(locally (deftype foo () t) (defun foo () t)")
   (goto-char 13)
   (should (equal (mgl-pax-current-definition-possible-names)
                  '(("foo" "(deftype foo () t)" 10))))
   (goto-char 30)
   (should (equal (mgl-pax-current-definition-possible-names)
                  '(("foo" "(defun foo () t)" 29))))))
