(require 'mgl-pax)
(require 'ert)
(require 'cl-lib)

(cl-defmacro with-temp-lisp-buffer (&body body)
  `(with-temp-buffer
     (lisp-mode)
     ,@body))


;;;; Test `mgl-pax-object-and-locatives-list-at-point'

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


;;;; Test `mgl-pax-current-definition-possible-names'

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


;;;; Test `mgl-pax-transcribe-last-expression'

(ert-deftest test-mgl-pax-transcribe-last-expression/simple-1 ()
  (with-temp-lisp-buffer
   (insert "(1+ 2)")
   (should (equal (point) 7))
   (mgl-pax-transcribe-last-expression)
   (accept-process-output nil 1)
   (should (equal (point) 7))
   (should (equal (buffer-string) "(1+ 2)\n=> 3\n"))))

(ert-deftest test-mgl-pax-transcribe-last-expression/simple-2 ()
  (with-temp-lisp-buffer
   (insert "(1+ 2)\n")
   (should (equal (point) 8))
   (mgl-pax-transcribe-last-expression)
   (accept-process-output nil 1)
   (should (equal (point) 13))
   (should (equal (buffer-string) "(1+ 2)\n=> 3\n"))))

(ert-deftest test-mgl-pax-transcribe-last-expression/output-1 ()
  (with-temp-lisp-buffer
   (insert "(princ 'xxx)")
   (mgl-pax-transcribe-last-expression)
   (accept-process-output nil 1)
   (should (equal (point) 13))
   (should (equal (buffer-string) "(princ 'xxx)\n.. XXX\n=> XXX\n"))))

(ert-deftest test-mgl-pax-transcribe-last-expression/comment ()
  (with-temp-lisp-buffer
   (insert ";; (1+ 2)")
   (save-excursion (insert "\nxxx\n"))
   (mgl-pax-transcribe-last-expression)
   (accept-process-output nil 1)
   (should (equal (point) 10))
   (should (equal (buffer-string) ";; (1+ 2)\n;; => 3\nxxx\n"))))
