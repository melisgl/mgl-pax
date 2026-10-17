;;; mgl-pax-test-client.el --- Client neutral names for the Elisp tests -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ert)

(defvar mgl-pax-test-client 'slime
  "The Lisp interaction client for tests, e.g. `slime' or `sly'")

(eval-and-compile
  (defun mgl-pax-test-client-symbol (name)
    "Return the symbol NAME prefixed with the client name."
    (intern (format "%s-%s" mgl-pax-test-client name))))

;; A client NAME needs there to be a NAME-tests library that provides
;; NAME-check, def-NAME-test, and the other functions aliased here.

(require (intern (format "%s-tests" mgl-pax-test-client)))

(dolist (name '("eval" "sync-to-top-level" "check-top-level" "compile-defun"
                "eval-last-expression" "pop-find-definition-stack"
                "edit-definition" "lisp-implementation-name"
                "communication-style" "skip-test"
                ;; Macros.
                "check"))
  (defalias (intern (format "mgl-pax-test-%s" name))
    (mgl-pax-test-client-symbol name)))

(defalias 'mgl-pax-test-def-test
  (intern (format "def-%s-test" mgl-pax-test-client)))

(defmacro mgl-pax-test-with-buffer-package (package &rest body)
  "Bind the client's `buffer-package' variable to PACKAGE around BODY."
  (declare (indent 1))
  `(let ((,(mgl-pax-test-client-symbol "buffer-package") ,package))
     ,@body))

(defun mgl-pax-test-batch-test (&optional test-name randomize)
  "Test by selecting by TEST-NAME in batch mode with the client.
If RANDOMIZE, they get run in random order.  This shuffles the test names rather
than letting the client do it since this would put the test objects (including
function bodies) in the selector that gets printed by ERT, which makes for rather
messy test outputs."
  (funcall (mgl-pax-test-client-symbol "batch-test")
           (if randomize
               `(member ,@(mapcar #'ert-test-name
                                  (funcall (mgl-pax-test-client-symbol
                                            "shuffle-list")
                                           (ert-select-tests (or test-name t)
                                                             t))))
             test-name)
           nil))

(defun mgl-pax-test-ert-test-for (name input i doc _body fails-for style fname)
  `(ert-deftest
       ,(intern (format "%s-%d" name i)) ()
     ,(format "For input %s, %s" input
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
                                        (mgl-pax-test-lisp-implementation-name)
                                        ',fails-for)
                                       :failed
                                     :passed))))))
     ,@(when style
         `((let ((style (mgl-pax-test-communication-style)))
             (when (not (member style ',style))
               (mgl-pax-test-skip-test
                (format "test not applicable for style %s" style))))))
     (apply #',fname ',input)))

(defalias (mgl-pax-test-client-symbol "test-ert-test-for")
  #'mgl-pax-test-ert-test-for)

(provide 'mgl-pax-test-client)
;;; mgl-pax-test-client.el ends here
