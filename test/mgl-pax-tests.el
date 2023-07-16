(require 'mgl-pax)
(require 'ert)
(require 'cl-lib)
(require 'w3m-load)
(require 'slime-tests)

(defun load-mgl-pax-test-system ()
  (slime-eval '(cl:unless (cl:find-package '#:mgl-pax-test)
                          (asdf:load-system "mgl-pax/test"))))

(cl-defmacro with-temp-lisp-buffer (&body body)
  `(with-temp-buffer
     (lisp-mode)
     (load-mgl-pax-test-system)
     ,@body))

(cl-defmacro with-temp-lisp-and-non-lisp-buffer (&body body)
  `(progn
     (with-temp-lisp-buffer
      ,@body)
     (with-temp-buffer
       ,@body)))

(cl-defmacro with-browsers (&body body)
  `(progn
     (let ((mgl-pax-browser-function 'w3m-browse-url))
       ,@body)
     (let ((mgl-pax-browser-function 'w3m-browse-url*))
       ,@body)))

;;; Fake non-w3m browser to test the web server.
(defun w3m-browse-url* (url)
  (w3m-browse-url url)
  (cl-loop repeat 20
           until (eq major-mode 'w3m-mode)
           do (sit-for 0.1))
  ;; Wait for fontification to finish.
  (sit-for 0.1))

(defun substringp (sub string)
  (string-match-p (regexp-quote sub) string))

(defun should-be-looking-at (string)
  (slime-check
    ("In buffer %S, looking at: %S" (buffer-name (current-buffer))
     (buffer-substring-no-properties (point)
                                     (min (point-max)
                                          (+ (point) 40))))
    (looking-at (regexp-quote string))))

;;; Redefine this without truncate-string-to-width.
(defun slime-test-ert-test-for (name input i doc _body fails-for style fname)
  `(define-slime-ert-test
     ,(intern (format "%s-%d" name i)) ()
     ,(format "For input %s, %s" input
              ;; (truncate-string-to-width
              ;;  (format "%s" input)
              ;;  15 nil nil 'ellipsis)
              (replace-regexp-in-string "^.??\\(\\w+\\)"
                                        (lambda (s) (downcase s))
                                        doc
                                        t))
     ,@(if fails-for
           `(:expected-result '(satisfies
                                (lambda (result)
                                  (ert-test-result-type-p
                                   result
                                   (if (member
                                        (slime-lisp-implementation-name)
                                        ',fails-for)
                                       :failed
                                     :passed))))))

     ,@(when style
         `((let ((style (slime-communication-style)))
             (when (not (member style ',style))
               (slime-skip-test (format "test not applicable for style %s"
                                        style))))))
     (apply #',fname ',input)))


;;;; Test `mgl-pax-wall-at-point'

(ert-deftest test-mgl-pax-wall-at-point/simple-1 ()
  (with-temp-lisp-and-non-lisp-buffer
   (should (equal (mgl-pax-wall-at-point)
                  '()))
   (insert "xxx")
   (should (equal (mgl-pax-wall-at-point)
                  '(("xxx" ()))))
   (insert " sym")
   (should (equal (mgl-pax-wall-at-point)
                  '(("sym" ("xxx")))))
   (save-excursion
     (insert " function"))
   (should (equal (mgl-pax-wall-at-point)
                  '(("sym" ("xxx" "function")))))))

;;; xxx (FOO) yyy
(ert-deftest test-mgl-pax-wall-at-point/simple-2 ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "xxx (foo")
   (save-excursion
     (insert ") yyy"))
   (should (equal (mgl-pax-wall-at-point)
                  '(("foo" ()))))))

;;; xxx ((FOO)) yyy
(ert-deftest test-mgl-pax-wall-at-point/simple-3 ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "xxx ((foo")
   (save-excursion
     (insert ")) yyy"))
   (should (equal (mgl-pax-wall-at-point)
                  '(("foo" ()))))))

;;; xxx `FOO` yyy
(ert-deftest test-mgl-pax-wall-at-point/simple-4 ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "xxx `foo")
   (save-excursion
     (insert "` yyy"))
   (should (equal (mgl-pax-wall-at-point)
                  '(("foo" ("xxx" "yyy")))))))

;;; ;;; xxx FOO yyy
(ert-deftest test-mgl-pax-wall-at-point/comment-1 ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert ";;; xxx foo")
   (save-excursion
     (insert " yyy"))
   (should (equal (mgl-pax-wall-at-point)
                  '(("foo" ("xxx" "yyy")))))))

;;; ;;; xxx FOO (method ()
;;; ;;;          (number))
(ert-deftest test-mgl-pax-wall-at-point/comment-2 ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert ";;; xxx foo")
   (save-excursion
     (insert " (method ()\n;;; (number))"))
   (should (equal (mgl-pax-wall-at-point)
                  '(("foo" ("xxx" "(method ()\n(number))")))))))

;;; xxx [FOO][function] yyy
(ert-deftest test-mgl-pax-wall-at-point/reflink-1 ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "xxx [foo")
   (save-excursion
     (insert "][function] yyy"))
   (should (equal (mgl-pax-wall-at-point)
                  '(("[foo][function]" ("xxx" "yyy"))
                    ("foo" ("function")))))))

;;; xxx [FOO][(function)] yyy
(ert-deftest test-mgl-pax-wall-at-point/reflink-2 ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "xxx [foo")
   (save-excursion
     (insert "][(function)] yyy"))
   (should (equal (mgl-pax-wall-at-point)
                  '(("[foo][" ("xxx" "(function)"))
                    ("foo" ("(function)")))))))

;;; xxx [`FOO`][function] yyy
(ert-deftest test-mgl-pax-wall-at-point/reflink-3 ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "xxx [`foo")
   (save-excursion
     (insert "`][function] yyy"))
   (should (equal (mgl-pax-wall-at-point)
                  '(("foo" ("[" "function"))
                    ("`foo`" ("function")))))))

;;; xxx [FOO ][function] yyy
(ert-deftest test-mgl-pax-wall-at-point/reflink-4 ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "xxx [foo")
   (save-excursion
     (insert " ][function] yyy"))
   (should (equal (mgl-pax-wall-at-point)
                  '(("[foo" ("xxx" "function"))
                    ("foo" ("function")))))))

;;; xxx [FOO][ function] yyy
(ert-deftest test-mgl-pax-wall-at-point/reflink-5 ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "xxx [foo")
   (save-excursion
     (insert "][ function] yyy"))
   (should (equal (mgl-pax-wall-at-point)
                  '(("[foo][" ("xxx" "function"))
                    ("foo" ("function")))))))

;;; xxx [see also][FOO function] zzz
(ert-deftest test-mgl-pax-wall-at-point/reflink-6 ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "xxx [see also][foo")
   (save-excursion
     (insert " function] yyy"))
   (should (equal (mgl-pax-wall-at-point)
                  '(("also][foo" ("[see" "function"))
                    ("foo function" ("yyy"))
                    ("foo" ("function")))))))

;;; xxx [see also][
;;;   FOO] yyy
(ert-deftest test-mgl-pax-wall-at-point/reflink-7 ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "xxx [see also][\n  foo")
   (save-excursion
     (insert "] yyy"))
   (should (equal (mgl-pax-wall-at-point)
                  '(("foo]" ("also][" "yyy"))
                    ("foo" ("yyy")))))))


;;;; Test `mgl-pax-edit-definitions'

(defun post-slime-edit-definition ()
  (slime-sync-to-top-level 1)
  (mgl-pax-sync-current-buffer))

(def-slime-test mgl-pax-edit-definitions/test-defs
    (name snippet &optional snippet2)
    "Test `M-.' on PAX references."
    ;; These mirror MGL-PAX-TEST::*NAVIGATION-TEST-CASES*.
    '(("variable foo-a" "(defvar foo-a)")
      ("variable foo-r" "(defvar foo-r)")
      ("variable foo-w" "(defvar foo-w)")
      ("constant bar" "(defconstant bar ")
      ("macro bar" "(defmacro bar ")
      ("symbol-macro my-smac" "(define-symbol-macro my-smac ")
      ("function foo" "(defun foo ")
      ("traced-foo" "(defun traced-foo ")
      ("generic-function test-gf" "(defgeneric test-gf ")
      ("(method () (number)) test-gf" "(defmethod test-gf ")
      ("(method nil ((eql #.(find-package '#:mgl-pax-test)) T T T)) exportable-reference-p"
       "(defmethod exportable-reference-p")
      ("method-combination my-comb" "(define-method-combination my-comb ")
      ("(accessor foo) foo-a" "(defclass foo " "(a :accessor foo-a)")
      ("(reader foo) foo-r" "(defclass foo " "(r :reader foo-r)")
      ("(writer foo) foo-w" "(defclass foo " "(w :writer foo-w)")
      ("type bar" "(deftype bar ")
      ("type foo" "(defclass foo ")
      ("type my-error" "(define-condition my-error ")
      ("class foo" "(defclass foo ")
      ("declaration test-declaration"
       "(define-declaration test-declaration ")
      ("condition my-error" "(define-condition my-error ")
      ("restart some-restart" "(define-restart some-restart ")
      ("asdf:system mgl-pax" "")
      ("package mgl-pax"
       "(eval-when (:compile-toplevel :load-toplevel :execute)"
       "(defpackage :mgl-pax")
      ;; Source location not implemented
      ;;("readtable xxx-rt" "(defreadtable xxx-rt")
      ("section mgl-pax::@pax-manual" "(defsection @pax-manual ")
      ("glossary-term some-term" "(define-glossary-term some-term ")
      ("locative my-loc" "(define-locative-type my-loc "))
  (load-mgl-pax-test-system)
  (let ((slime-buffer-package "MGL-PAX-TEST")
        (test-name (format "%S visits snippet %S or %S."
                           name snippet snippet2)))
    (message "Test case: %s" test-name)
    (with-temp-lisp-buffer
     (slime-check-top-level)
     (insert name)
     (let ((tmpbuffer (current-buffer))
           (start-pos (point)))
       (call-interactively 'slime-edit-definition)
       (post-slime-edit-definition)
       (slime-check ("%s: %S is found in another buffer" test-name name)
         (not (eq tmpbuffer (current-buffer))))
       (format "%S visits snippet %S or %S.\n"
               name snippet snippet2 (buffer-name)
               (buffer-substring-no-properties (point)
                                               (min (point-max)
                                                    (+ (point) 40))))
       (unwind-protect
           (slime-check
             ("%s: In buffer %S, looking at: %S" test-name buffer-name
              (buffer-substring-no-properties (point)
                                              (min (point-max)
                                                   (+ (point) 40))))
             (or (looking-at snippet)
                 (and snippet2 (looking-at snippet2))))
         (slime-pop-find-definition-stack))
       (should (eq (current-buffer) tmpbuffer))
       (should (= (point) start-pos)))
     (slime-check-top-level))))


;;;; Test `mgl-pax-current-definition-possible-names'

(ert-deftest test-mgl-pax-current-definition-possible-names/simple ()
  (with-temp-lisp-and-non-lisp-buffer
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
  (with-temp-lisp-and-non-lisp-buffer
   (insert "(defsystem \"a-name\" () t)")
   (goto-char 1)
   (should (equal (mgl-pax-current-definition-possible-names)
                  '(("\"a-name\"" "(defsystem \"a-name\" () t)" 1))))))

(ert-deftest test-mgl-pax-current-definition-possible-names/list-name ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "(list () foo t)")
   (goto-char 1)
   (should (equal (mgl-pax-current-definition-possible-names)
                  '()))))

(ert-deftest test-mgl-pax-current-definition-possible-names/wrapped ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "(locally (defun foo () t)")
   (goto-char 13)
   (should (equal (mgl-pax-current-definition-possible-names)
                  '(("foo" "(defun foo () t)" 10))))))

(ert-deftest test-mgl-pax-current-definition-possible-names/wrapped-2 ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "(locally (deftype foo () t) (defun foo () t)")
   (goto-char 13)
   (should (equal (mgl-pax-current-definition-possible-names)
                  '(("foo" "(deftype foo () t)" 10))))
   (goto-char 30)
   (should (equal (mgl-pax-current-definition-possible-names)
                  '(("foo" "(defun foo () t)" 29))))))


;;;; Test `mgl-pax-edit-parent-section'

(ert-deftest test-mgl-pax-edit-parent-section/simple ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "(pax:defsection @test-xxx ()\n  (foo function)\n  (foo type))\n")
   (slime-compile-defun)
   (slime-sync-to-top-level 1)
   (insert "(defun foo () t)")
   ;; Interpreted FOO
   (slime-eval-last-expression)
   (slime-sync-to-top-level 1)
   (backward-char)
   (mgl-pax-edit-parent-section)
   (slime-sync-to-top-level 1)
   (should (looking-at "(pax:defsection @test-xxx"))
   (slime-pop-find-definition-stack)
   (should (looking-at ")"))
   ;; Compiled FOO
   (slime-compile-defun)
   (slime-sync-to-top-level 1)
   (mgl-pax-edit-parent-section)
   (slime-sync-to-top-level 1)
   (should (looking-at "(pax:defsection @test-xxx"))
   (slime-pop-find-definition-stack)
   (should (looking-at ")"))))

(ert-deftest test-mgl-pax-edit-parent-section/wrapped ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "(pax:defsection @test-xxx ()\n  (foo function)\n  (foo type))\n")
   (slime-compile-defun)
   (slime-sync-to-top-level 1)
   (insert "(locally (defun foo () t))")
   ;; Interpreted FOO
   (slime-eval-last-expression)
   (slime-sync-to-top-level 1)
   (backward-char 3)
   (mgl-pax-edit-parent-section)
   (slime-sync-to-top-level 1)
   (should (looking-at "(pax:defsection @test-xxx"))
   (slime-pop-find-definition-stack)
   (should (looking-at "t))"))
   ;; Compiled FOO
   (slime-compile-defun)
   (slime-sync-to-top-level 1)
   (mgl-pax-edit-parent-section)
   (slime-sync-to-top-level 1)
   (should (looking-at "(pax:defsection @test-xxx"))
   (slime-pop-find-definition-stack)
   (should (looking-at "t))"))))

(ert-deftest test-mgl-pax-edit-parent-section/wrapped-2 ()
  (with-temp-lisp-and-non-lisp-buffer
   (insert "(pax:defsection @test-xxx ()\n  (foo function)\n  (foo type))\n")
   (slime-compile-defun)
   (slime-sync-to-top-level 1)
   (insert "(locally (defun foo () t) (deftype foo () 'fixnum))")
   ;; Interpreted
   (slime-eval-last-expression)
   (slime-sync-to-top-level 1)
   ;; DEFTYPE
   (search-backward "deftype")
   (mgl-pax-edit-parent-section)
   (slime-sync-to-top-level 1)
   (should (looking-at "(pax:defsection @test-xxx"))
   (slime-pop-find-definition-stack)
   (should (looking-at "deftype"))
   ;; DEFUN
   (search-backward "defun")
   (mgl-pax-edit-parent-section)
   (slime-sync-to-top-level 1)
   (should (looking-at "(pax:defsection @test-xxx"))
   (slime-pop-find-definition-stack)
   (should (looking-at "defun"))
   ;; Compiled
   (slime-compile-defun)
   (slime-sync-to-top-level 1)
   ;; DEFUN
   (mgl-pax-edit-parent-section)
   (slime-sync-to-top-level 1)
   (should (looking-at "(pax:defsection @test-xxx"))
   (slime-pop-find-definition-stack)
   (should (looking-at "defun"))
   ;; DEFTYPE
   (search-forward "deftype")
   (mgl-pax-edit-parent-section)
   (slime-sync-to-top-level 1)
   (should (looking-at "(pax:defsection @test-xxx"))
   (slime-pop-find-definition-stack)
   (should (looking-at " foo () 'fixnum"))))


;;;; Test `mgl-pax-document'

(ert-deftest test-mgl-pax-document/simple-1 ()
  (with-browsers
   (with-temp-lisp-and-non-lisp-buffer
    (insert "(defun foo-simple () \"docstring\" t)")
    (slime-compile-defun)
    (slime-sync-to-top-level 1)
    (mgl-pax-document "pax:foo-simple")
    (slime-sync-to-top-level 1)
    (mgl-pax-sync-current-buffer)
    (should (eq major-mode 'w3m-mode))
    (should (substringp "* [function] FOO-SIMPLE" (w3m-contents)))
    (kill-buffer))))

(ert-deftest test-mgl-pax-document/simple-2 ()
  (with-browsers
   (with-temp-lisp-and-non-lisp-buffer
    (insert "(defun foo-simple () \"docstring\" t)")
    (slime-compile-defun)
    (slime-sync-to-top-level 1)
    (unwind-protect
        (progn
          (mgl-pax-document "pax:foo-simple")
          (slime-sync-to-top-level 1)
          (mgl-pax-sync-current-buffer)
          (should (eq major-mode 'w3m-mode))
          (should (substringp "* [function] FOO-SIMPLE" (w3m-contents))))
      (when (eq major-mode 'w3m-mode)
        (kill-buffer))))))

(ert-deftest test-mgl-pax-document/url-encoding/interactive ()
  (with-browsers
   (with-temp-lisp-and-non-lisp-buffer
    (insert "(defun %foo-simple () \"docstring\" t)")
    (slime-compile-defun)
    (slime-sync-to-top-level 1)
    (unwind-protect
        (progn
          (insert "%foo-simple")
          (call-interactively 'mgl-pax-document)
          (slime-sync-to-top-level 1)
          (mgl-pax-sync-current-buffer)
          (should (eq major-mode 'w3m-mode))
          (should (substringp "* [function] %FOO-SIMPLE" (w3m-contents))))
      (when (eq major-mode 'w3m-mode)
        (kill-buffer))))))

(ert-deftest test-mgl-pax-document/url-encoding/non-interactive ()
  (with-browsers
   (with-temp-lisp-and-non-lisp-buffer
    (insert "(defun %foo-simple () \"docstring\" t)")
    (slime-compile-defun)
    (slime-sync-to-top-level 1)
    (unwind-protect
        (progn
          (mgl-pax-document (concat "pax:" (url-hexify-string "%foo-simple")))
          (slime-sync-to-top-level 1)
          (mgl-pax-sync-current-buffer)
          (should (eq major-mode 'w3m-mode))
          (should (substringp "* [function] %FOO-SIMPLE" (w3m-contents)))
          (mgl-pax-doc-reload)
          (slime-sync-to-top-level 1))
      (when (eq major-mode 'w3m-mode)
        (kill-buffer))))))

(ert-deftest test-mgl-pax-document/external ()
  (let ((common-lisp-hyperspec-root
         "http://www.lispworks.com/documentation/HyperSpec/"))
    (with-browsers
     (unwind-protect
         (progn
           (mgl-pax-document "pax:readably")
           (slime-sync-to-top-level 1)
           (mgl-pax-sync-current-buffer)
           (should (eq major-mode 'w3m-mode))
           ;; Wait for 5 seconds for the page to load and render.
           (cl-loop repeat 50
                    until (looking-at "readably adv.")
                    do (sit-for 0.1))
           (should-be-looking-at "readably adv.")))))
  (when (eq major-mode 'w3m-mode)
    (kill-buffer)))

(ert-deftest test-mgl-pax-document/external/context ()
  (let ((common-lisp-hyperspec-root
         "http://www.lispworks.com/documentation/HyperSpec/"))
    (with-browsers
     (with-temp-lisp-and-non-lisp-buffer
      (insert "readably")
      (unwind-protect
          (progn
            (call-interactively 'mgl-pax-document)
            (slime-sync-to-top-level 1)
            (mgl-pax-sync-current-buffer)
            (should (eq major-mode 'w3m-mode))
            ;; Wait for 5 seconds for the page to load and render.
            (cl-loop repeat 50
                     until (looking-at "readably adv.")
                     do (sit-for 0.1))
            (should-be-looking-at "readably adv.")))
      (when (eq major-mode 'w3m-mode)
        (kill-buffer))))))

(ert-deftest test-mgl-pax-document/go ()
  (with-browsers
   (unwind-protect
       (progn
         (mgl-pax-document (concat "pax:"
                                   (w3m-url-encode-string
                                    "defun (go (print function))")))
         (slime-sync-to-top-level 1)
         (mgl-pax-sync-current-buffer)
         (should (eq major-mode 'w3m-mode))
         (should (substringp "  * [function] PRINT" (w3m-contents))))
     (when (eq major-mode 'w3m-mode)
       (kill-buffer)))))

(ert-deftest test-mgl-pax-document/go/context ()
  (with-browsers
   (with-temp-lisp-and-non-lisp-buffer
    (unwind-protect
        (progn
          (insert "(go (print function)) defun")
          (call-interactively 'mgl-pax-document)
          (slime-sync-to-top-level 1)
          (mgl-pax-sync-current-buffer)
          (should (eq major-mode 'w3m-mode))
          (should (substringp "  * [function] PRINT" (w3m-contents))))
      (when (eq major-mode 'w3m-mode)
        (kill-buffer))))))

(defun w3m-contents ()
  (save-excursion
    (goto-char 1)
    (forward-line)
    (buffer-substring-no-properties (point) (point-max))))


;;;; Test `mgl-pax-current-definition-toggle-view'

(ert-deftest test-mgl-pax-current-definition-toggle-view ()
  (with-browsers
   (with-temp-lisp-buffer
    (let ((tmpbuffer (current-buffer)))
      (insert "(defun foo-simple () \"docstring\" t)")
      ;; Interpreted
      (slime-eval-last-expression)
      (slime-sync-to-top-level 1)
      (backward-char)
      (mgl-pax-current-definition-toggle-view)
      (slime-sync-to-top-level 1)
      (mgl-pax-sync-current-buffer)
      (should (eq major-mode 'w3m-mode))
      (should (substringp "* [function] FOO-SIMPLE" (w3m-contents)))
      (kill-buffer)
      (switch-to-buffer tmpbuffer)
      (mgl-pax-sync-current-buffer)
      ;; Compiled
      (slime-compile-defun)
      (slime-sync-to-top-level 1)
      (mgl-pax-current-definition-toggle-view)
      (slime-sync-to-top-level 1)
      (mgl-pax-sync-current-buffer)
      (should (eq major-mode 'w3m-mode))
      (should (substringp "* [function] FOO-SIMPLE" (w3m-contents)))
      (kill-buffer)
      (mgl-pax-sync-current-buffer)))))

(ert-deftest test-mgl-pax-current-definition-toggle-view/url-encoding ()
  (with-browsers
   (with-temp-lisp-buffer
    (let ((tmpbuffer (current-buffer)))
      (insert "(defun %foo-simple () \"docstring\" t)")
      ;; Interpreted
      (slime-eval-last-expression)
      (slime-sync-to-top-level 1)
      (backward-char)
      (mgl-pax-current-definition-toggle-view)
      (slime-sync-to-top-level 1)
      (mgl-pax-sync-current-buffer)
      (should (eq major-mode 'w3m-mode))
      (should (substringp "* [function] %FOO-SIMPLE" (w3m-contents)))
      (kill-buffer)
      (switch-to-buffer tmpbuffer)
      (mgl-pax-sync-current-buffer)
      ;; Compiled
      (slime-compile-defun)
      (slime-sync-to-top-level 1)
      (mgl-pax-current-definition-toggle-view)
      (slime-sync-to-top-level 1)
      (mgl-pax-sync-current-buffer)
      (should (eq major-mode 'w3m-mode))
      (should (substringp "* [function] %FOO-SIMPLE" (w3m-contents)))
      (kill-buffer)
      (mgl-pax-sync-current-buffer)))))

(ert-deftest test-mgl-pax-current-definition-toggle-view/string ()
  (with-browsers
   (with-temp-lisp-buffer
    (let ((tmpbuffer (current-buffer)))
      (insert "(defpackage \"MGL-PAX-TEST\")")
      ;; Interpreted
      (slime-eval-last-expression)
      (slime-sync-to-top-level 1)
      (backward-char)
      (mgl-pax-current-definition-toggle-view)
      (slime-sync-to-top-level 1)
      (mgl-pax-sync-current-buffer)
      (should (eq major-mode 'w3m-mode))
      (should (substringp "* [package] \"MGL-PAX-TEST\"" (w3m-contents)))
      (kill-buffer)
      (switch-to-buffer tmpbuffer)
      (mgl-pax-sync-current-buffer)
      ;; Compiled
      (slime-compile-defun)
      (slime-sync-to-top-level 1)
      (mgl-pax-current-definition-toggle-view)
      (slime-sync-to-top-level 1)
      (mgl-pax-sync-current-buffer)
      (should (eq major-mode 'w3m-mode))
      (should (substringp "* [package] \"MGL-PAX-TEST\"" (w3m-contents)))
      (kill-buffer)
      (mgl-pax-sync-current-buffer)))))

(ert-deftest test-mgl-pax-current-definition-toggle-view/uninterned ()
  (with-browsers
   (with-temp-lisp-buffer
    (let ((tmpbuffer (current-buffer)))
      (insert "(defpackage #:mgl-pax-test)")
      ;; Interpreted
      (slime-eval-last-expression)
      (slime-sync-to-top-level 1)
      (backward-char)
      (mgl-pax-current-definition-toggle-view)
      (slime-sync-to-top-level 1)
      (mgl-pax-sync-current-buffer)
      (should (eq major-mode 'w3m-mode))
      (should (substringp "* [package] \"MGL-PAX-TEST\"" (w3m-contents)))
      (kill-buffer)
      (switch-to-buffer tmpbuffer)
      (mgl-pax-sync-current-buffer)
      ;; Compiled
      (slime-compile-defun)
      (slime-sync-to-top-level 1)
      (mgl-pax-current-definition-toggle-view)
      (slime-sync-to-top-level 1)
      (mgl-pax-sync-current-buffer)
      (should (eq major-mode 'w3m-mode))
      (should (substringp "* [package] \"MGL-PAX-TEST\"" (w3m-contents)))
      (kill-buffer)
      (mgl-pax-sync-current-buffer)))))


;;; Test `mgl-pax-apropos'

(ert-deftest test-mgl-pax-apropos ()
  (with-browsers
   (mgl-pax-apropos "install-pax-elisp")
   (slime-sync-to-top-level 1)
   (mgl-pax-sync-current-buffer)
   (should (eq major-mode 'w3m-mode))
   (let ((contents (w3m-contents)))
     (should (substringp "Apropos" contents))
     (should (substringp "* [function] MGL-PAX:INSTALL-PAX-ELISP" contents)))
   (kill-buffer)
   (mgl-pax-sync-current-buffer)))


;;;; Test `mgl-pax-transcribe-last-expression'

(ert-deftest test-mgl-pax-transcribe-last-expression/simple-1 ()
  (load-mgl-pax-test-system)
  (with-temp-lisp-buffer
   (insert "(1+ 2)")
   (should (equal (point) 7))
   (mgl-pax-transcribe-last-expression)
   (accept-process-output nil 1)
   (should (equal (point) 7))
   (should (equal (buffer-string) "(1+ 2)\n=> 3\n"))))

(ert-deftest test-mgl-pax-transcribe-last-expression/simple-2 ()
  (load-mgl-pax-test-system)
  (with-temp-lisp-buffer
   (insert "(1+ 2)\n")
   (should (equal (point) 8))
   (mgl-pax-transcribe-last-expression)
   (accept-process-output nil 1)
   (should (equal (point) 13))
   (should (equal (buffer-string) "(1+ 2)\n=> 3\n"))))

(ert-deftest test-mgl-pax-transcribe-last-expression/output-1 ()
  (load-mgl-pax-test-system)
  (with-temp-lisp-buffer
   (insert "(princ 'xxx)")
   (mgl-pax-transcribe-last-expression)
   (accept-process-output nil 1)
   (should (equal (point) 13))
   (should (equal (buffer-string) "(princ 'xxx)\n.. XXX\n=> XXX\n"))))

(ert-deftest test-mgl-pax-transcribe-last-expression/comment ()
  (load-mgl-pax-test-system)
  (with-temp-lisp-buffer
   (insert ";; (1+ 2)")
   (save-excursion (insert "\nxxx\n"))
   (mgl-pax-transcribe-last-expression)
   (accept-process-output nil 1)
   (should (equal (point) 10))
   (should (equal (buffer-string) ";; (1+ 2)\n;; => 3\nxxx\n"))))


;;;; Test `mgl-pax-retranscribe-region'

(ert-deftest test-mgl-pax-retranscribe-region/simple ()
  (load-mgl-pax-test-system)
  (with-temp-lisp-buffer
   (insert "(1+ 2)\n=> 7\n")
   (mark-whole-buffer)
   (call-interactively 'mgl-pax-retranscribe-region)
   (accept-process-output nil 1)
   (should (equal (buffer-string) "(1+ 2)\n=> 3\n"))))

(ert-deftest test-mgl-pax-retranscribe-region/blank-line ()
  (load-mgl-pax-test-system)
  (with-temp-lisp-buffer
   (insert "  (1+ 2)\n  => 7\n\n")
   (mark-whole-buffer)
   (call-interactively 'mgl-pax-retranscribe-region)
   (accept-process-output nil 1)
   (should (equal (buffer-string) "  (1+ 2)\n  => 3\n  \n"))))


(provide 'mgl-pax-tests)
