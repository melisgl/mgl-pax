(require 'mgl-pax)
(require 'ert)
(require 'cl-lib)

(cl-defmacro with-temp-lisp-buffer (&body body)
  `(with-temp-buffer
     (lisp-mode)
     ,@body))

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
