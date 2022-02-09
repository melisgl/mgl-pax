(mgl-pax:define-package :mgl-pax-test-extension
  (:use #:common-lisp #:mgl-pax #:try)
  (:export #:test))

(in-package :mgl-pax-test-extension)

(define-symbol-locative-type aaa ())

(define-definer-for-symbol-locative-type define-aaa aaa)

(define-aaa aaa1 ()
  "This is AAA1.")

(defun navigate-system-loaded-p ()
  (boundp 'pax::@navigating-in-emacs))

(defun document-system-loaded-p ()
  (boundp 'pax::@generating-documentation))

(defun transcribe-system-loaded-p ()
  (boundp 'pax::@transcripts))

;;;; Autoload tests must be run one-by-one in a fresh lisp after the
;;;; MGL-PAX/TEST-EXTENSION system has been loaded.

(deftest test-locate-autoload ()
  (is (eq (locate 'locate 'function) (symbol-function 'locate)))
  (is (navigate-system-loaded-p)))

(deftest test-canonical-reference-autoload ()
  (is (pax::reference= (canonical-reference (make-reference 'aaa1 'aaa))
                       (make-reference 'aaa1 'aaa)))
  (is (navigate-system-loaded-p)))

(deftest test-collect-reachable-objects-autoload ()
  (is (collect-reachable-objects pax::@locatives-and-references))
  (is (navigate-system-loaded-p)))

(deftest test-document-autoload ()
  (signals-not (error)
    (document (make-reference 'aaa1 'aaa)))
  (is (document-system-loaded-p)))

(deftest test-docstring-autoload ()
  (is (equal (docstring (make-reference 'aaa1 'aaa))
             "This is AAA1."))
  (is (navigate-system-loaded-p)))

(deftest test-find-source-autoload ()
  (is (eq (first (find-source (make-reference 'aaa1 'aaa))) :location))
  (is (navigate-system-loaded-p)))
