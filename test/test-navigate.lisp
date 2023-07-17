(in-package :mgl-pax-test)

(deftest test-navigate ()
  (test-read-locative-from-string)
  (test-read-object-from-string)
  (test-read-reference-from-string)
  (test-definitions-of-wall)
  (test-locate)
  (test-navigation-to-source))

(deftest test-read-locative-from-string ()
  (let ((*package* (find-package :mgl-pax-test)))
    (unintern (read-from-string "non-interned"))
    (unintern (read-from-string "yyy"))
    (is (null (mgl-pax::read-locative-from-string "non-interned")))
    (is (null (find-symbol (string '#:non-interned))))
    (is (null (mgl-pax::read-locative-from-string "find")))
    (is (null (mgl-pax::read-locative-from-string "")))
    (is (match-values (mgl-pax::read-locative-from-string "function")
          (eq * 'function)
          (= * 8)))
    (is (match-values (mgl-pax::read-locative-from-string " function")
          (eq *'function)
          (= * 9)))
    (is (match-values (mgl-pax::read-locative-from-string "function ")
          (eq * 'function)
          (= * 9)))
    (is (match-values (mgl-pax::read-locative-from-string "function junk")
          (null *)))
    (is (match-values (mgl-pax::read-locative-from-string "function junk"
                                                          :junk-allowed t)
          (eq * 'function)
          (= * 9)))
    (let ((locative (mgl-pax::read-locative-from-string "(function yyy)")))
      (is (eq (first locative) 'function))
      (is (string= (symbol-name (second locative)) (string '#:yyy)))
      (is (eq (symbol-package (second locative)) *package*)))
    (with-test ("markdown and M-.")
      (dolist (string '("function." "function," "function;" "function:"
                        "function`" "function'" "function>" "<function>"
                        "\"function\""))
        (is (eq (mgl-pax::read-locative-from-noisy-string (% string))
                'function))))))

(defun test-read-object-from-string ()
  (let ((*package* (find-package :mgl-pax-test)))
    (is (eq (mgl-pax::read-object-from-string "deftest") 'deftest))
    (is (eq (mgl-pax::read-object-from-string "MGL-PAX::@CODIFIABLE")
            'mgl-pax::@codifiable))))

(deftest test-read-reference-from-string ()
  (let ((*package* (find-package :mgl-pax-test)))
    (unintern (read-from-string "non-interned"))
    (unintern (read-from-string "yyy"))
    (is (null (mgl-pax::read-reference-from-string "yyy non-interned")))
    (is (null (find-symbol (string '#:non-interned))))
    (is (null (find-symbol (string '#:yyy))))
    (is (null (mgl-pax::read-reference-from-string "yyy (non-interned)")))
    (is (null (find-symbol (string '#:non-interned))))
    (is (null (find-symbol (string '#:yyy))))
    (is (null (mgl-pax::read-reference-from-string "yyy find")))
    (is (null (find-symbol (string '#:yyy))))
    (is (match-values (mgl-pax::read-reference-from-string "yyy function")
          (string= * "YYY")
          (eq * 'function)
          (eq * t)
          (null *)))
    (is (match-values (mgl-pax::read-reference-from-string " yyy  function ")
          (string= * "YYY")
          (eq * 'function)
          (eq * t)
          (null *)))
    (is (match-values
            (mgl-pax::read-reference-from-string " yyy  function  xxx ")
          (null *)
          (null *)
          (null *)
          (string= * "function  xxx")))
    (is (mgl-pax::read-reference-from-string "mgl-pax:@codification section")
        :msg "internal symbol with single :")
    (with-test ("multiple locatives")
      (is (match-values
              (mgl-pax::read-reference-from-string "foo function type"
                                                   :multiple-locatives-p t)
            (eq * 'foo)
            (equal * '(function type))
            (eq * t)
            (null *)))
      (is (match-values
              (mgl-pax::read-reference-from-string "foo function xxx"
                                                   :multiple-locatives-p t)
            (eq * 'foo)
            (equal * '(function))
            (eq * t)
            (string= * "xxx")))
      (is (match-values
              (mgl-pax::read-reference-from-string "foo xxx type"
                                                   :multiple-locatives-p t)
            (eq * nil)
            (equal * '())
            (eq * nil)
            (string= * "xxx type"))))))

;;; We have no Emacs based tests. This tests the CL side of `M-.' when
;;; it's invoked with point on FOO in the test cases below. The actual
;;; OBJECT-AND-LOCATIVES-LIST argument that mgl-pax.el would send is
;;; reproduced explicitly.
(deftest test-definitions-of-wall ()
  (let ((*package* (find-package '#:mgl-pax-test)))
    ;; xxx FOO function
    (check-dowall '(("FOO" ("xxx" "function")))
                  '((foo function)))
    ;; function FOO xxx
    (check-dowall '(("FOO" ("function" "xxx")))
                  '((foo function)))
    ;; xxx [foo][function] xxx
    (check-dowall '(("[foo][function]" ("xxx" "xxx"))
                    ("foo" ("function")))
                  '((foo function)))
    ;; function FOO compiler-macro
    (check-dowall '(("FOO" ("function" "compiler-macro")))
                  '((foo compiler-macro)
                    (foo function)))
    (with-failure-expected ((alexandria:featurep :clisp))
      ;; xxx FOO xxx
      (check-dowall '(("FOO" ("xxx" "xxx")))
                    '((foo class)
                      (foo compiler-macro)
                      (foo function)))
      ;; xxx [foo][] xxx
      (check-dowall '(("[foo][]" ("xxx" "xxx"))
                      ("foo" ("xxx")))
                    '((foo class)
                      (foo compiler-macro)
                      (foo function)))
      ;; xxx [foo][xxx] xxx
      (check-dowall '(("[foo][xxx]" ("xxx" "xxx"))
                      ("foo" ("xxx")))
                    '((foo class)
                      (foo compiler-macro)
                      (foo function))))
    (with-failure-expected ((alexandria:featurep :abcl))
      ;; pax
      (check-dowall '(("pax" ()))
                    '(("MGL-PAX" package)))
      ;; "MGL-PAX"
      (check-dowall '(("\"MGL-PAX\"" ()))
                    '(("MGL-PAX" package)
                      ("mgl-pax" asdf:system)))
      ;; MGL-PAX
      (check-dowall '(("MGL-PAX" ()))
                    '(("MGL-PAX" package)
                      ("mgl-pax" asdf:system)))
      ;; :MGL-PAX
      (check-dowall '((":MGL-PAX" ()))
                    '(("MGL-PAX" package)
                      ("mgl-pax" asdf:system))))
    (with-test ("prefer uppercase")
      (check-dowall '(("DREF::@LOCATIVE-TYPES" ()))
                    '((dref::@locative-types section)))
      (check-dowall '(("DREF::@LOCATIVE-TYPEs" ()))
                    '((dref::@locative-type glossary-term))))))

(defun check-dowall (wall expected-refs)
  (let ((refs (mapcar (lambda (ref)
                        (list (xref-name ref)
                              (xref-locative ref)))
                      (dref::sort-references (pax::definitions-of-wall wall)))))
    (is (equal refs expected-refs)
        :ctx ("WORD-AND-LOCATIVES-LIST = ~S" wall))))

(defun sort-emacsrefs (emacsrefs)
  (sort (copy-seq emacsrefs) (lambda (r1 r2)
                               (or (string< (first r1) (first r2))
                                   (and (string= (first r1) (first r2))
                                        (string< (second r1) (second r2)))))))


(deftest test-locate ()
  (test-locate/section)
  (test-locate/glossary-term)
  (test-locate/go)
  (test-locate/include))

(deftest test-locate/section ()
  (check-ref-sets (definitions '@test-examples)
                  `(,(xref '@test-examples 'section))))

(deftest test-locate/glossary-term ()
  (check-ref-sets (definitions 'some-term)
                  `(,(xref 'some-term 'glossary-term))))

(deftest test-locate/go ()
  (check-ref (dref 'xxx '(go (foo function)))
             'xxx '(go (foo function)) 'pax::go-dref)
  (check-ref (dref "xxx" '(go (foo function)))
             "xxx" '(go (foo function)) 'pax::go-dref)
  (signals (locate-error)
    (dref 'xxx '(go (undefined function))))
  (signals (locate-error :pred "Bad arguments")
    (dref 'xxx '(go 1 2))))

(deftest test-locate/include ()
  (check-ref (dref nil '(include #.(asdf:system-relative-pathname
                                      "mgl-pax" "HACKING.md")))
             nil '(include #.(asdf:system-relative-pathname
                              "mgl-pax" "HACKING.md"))
             'pax::include-dref)
  (signals (locate-error :pred "/non-existent")
    (dref nil '(include "/non-existent/file")))
  (with-failure-expected ((and (alexandria:featurep '(:or :clisp))
                               'failure))
    (signals-not (locate-error)
      (dref nil '(include (:start (*some-var* variable)))))
    (signals-not (locate-error)
      (dref nil '(include (:end (*some-var* variable))))))
  (signals (locate-error :pred "UNDEFINED")
    (dref nil '(include (:start (undefined variable)))))
  (signals (locate-error :pred "UNDEFINED")
    (dref nil '(include (:end (undefined variable))))))


;;; Keep *NAVIGATION-TEST-CASES* plus
;;; DREF-TEST::*SOURCE-LOCATION-TEST-CASES* and
;;; `mgl-pax-edit-definitions/test-defs' in test.el in sync.
(defparameter *navigation-test-cases*
  '((mgl-pax::@pax-manual section (defsection @pax-manual))
    (some-term glossary-term (define-glossary-term some-term))
    (mgl-pax.el (include #.(asdf:system-relative-pathname
                            :mgl-pax "src/mgl-pax.el")
                 :header-nl "```elisp" :footer-nl "```")
     ";; -*- lexical-binding: t -*-")
    (foo-example (include (:start (dref-ext:make-source-location function)
                           :end (dref-ext:source-location-p function))
                  :header-nl "```"
                  :footer-nl "```")
     (defun/autoloaded make-source-location))))

(deftest test-navigation-to-source ()
  (let ((*package* (find-package :mgl-pax-test)))
    (dolist (test-case *navigation-test-cases*)
      (apply #'check-source-location test-case)))
  (signals-not (error)
    (source-location (xref 'function 'locative)))
  (with-failure-expected ()
    (signals-not (error)
      (source-location (xref 'locative 'function)))))
