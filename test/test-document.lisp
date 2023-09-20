(in-package :mgl-pax-test)

(defun check-document (input expected
                       &key (package (find-package :mgl-pax-test)) msg
                         (url-versions '(2)) (format :markdown))
  (let ((output (let ((*package* package)
                      (*document-hyperspec-root* "CLHS/")
                      (*document-url-versions* url-versions))
                  (document input :stream nil :format format))))
    (is (null (mismatch% output expected))
        :msg msg
        :ctx ("Input: ~S" input))))

(defun document* (object &key (format :markdown) w3m)
  (let ((warnings ())
        (pax::*html-subformat* (and w3m :w3m)))
    (handler-bind ((warning (lambda (w)
                              (push (princ-to-string w) warnings)
                              (muffle-warning w))))
      (values (funcall (if w3m
                           #'pax::document/open
                           #'document)
                       object :stream nil :format format)
              warnings))))

(defun check-head (input expected &key (format :markdown) msg (n-lines 1)
                                    (warnings 0) package w3m)
  (let* ((*package* (or package (find-package :mgl-pax-test)))
         (*document-hyperspec-root* "CLHS/")
         (*document-url-versions* '(2))
         (n-expected-warnings warnings))
    (multiple-value-bind (full-output warnings)
        (document* input :format format :w3m w3m)
      (let ((got (dref::first-lines full-output n-lines))
            (expected (format nil expected)))
        (is (equal got expected)
            :msg msg
            :ctx ("Input: ~S~%Full output:~%~S" input full-output))
        (is (= (length (% warnings)) n-expected-warnings)
            :ctx ("Input: ~S~%Full output:~%~S" input full-output))))))

(defun check-pred (input pred &key msg w3m (format :markdown))
  (let* ((*package* (find-package :mgl-pax-test))
         (*document-hyperspec-root* "CLHS/")
         (*document-url-versions* '(2))
         (full-output (document* input :format format :w3m w3m))
         (pred* (if (stringp pred)
                    (lambda (string)
                      (search pred string))
                    pred)))
    (is (funcall pred* full-output)
        :msg msg
        :ctx ("Input: ~S~%Pred: ~S~%" input pred))))

(defun internedp (name)
  (find-symbol (string name) :mgl-pax-test))


(deftest test-document ()
  (test-urlencode)
  (test-transform-tree)
  (test-sanitize-docstring-aggressively)
  (test-codify)
  (test-names)
  (test-downcasing)
  (test-link)
  (test-headings)
  (test-base-url)
  (test-url-versions)
  (test-pages)
  (test-locate-error)
  ;; PAX::@VARIABLELIKE-LOCATIVES
  (test-variable)
  (test-constant)
  ;; PAX::@MACROLIKE-LOCATIVES
  (test-macro)
  (test-symbol-macro)
  (test-setf)
  ;; PAX::@FUNCTIONLIKE-LOCATIVES
  (test-function)
  (test-generic-function)
  (test-method-combination)
  (test-method)
  (test-accessor)
  (test-reader)
  (test-writer)
  (test-structure-accessor)
  ;; PAX::@TYPELIKE-LOCATIVES
  (test-declaration)
  ;; PAX::@CONDITION-SYSTEM-LOCATIVES
  (test-condition)
  (test-restart)
  ;; PAX::@PACKAGELIKE-LOCATIVES
  (test-asdf-system)
  (test-package)
  (test-readtable)
  ;; PAX::@PAX-LOCATIVES
  (test-locative)
  (test-glossary-term)
  (test-go)
  (test-docstring)
  (test-hyperspec)
  (test-clhs-definitions)
  (test-clhs-section)
  (test-clhs-glossary-entries)
  (test-clhs-issue)
  (test-argument)
  (test-define-locative-alias)
  (test-cl-transcript)
  (test-document/open)
  (test-map-documentable)
  (test-table-of-contents))

(deftest test-urlencode ()
  (is (equal (mgl-pax::urlencode "hello") "hello"))
  (is (equal (mgl-pax::urlencode "@hello section") "@hello%20section"))
  (is (equal (mgl-pax::urlencode "\"") "%22"))
  (is (equal (mgl-pax::urlencode "á") "%C3%A1")))

(deftest test-transform-tree ()
  (is (equal '(1)
             (mgl-pax::transform-tree (lambda (parent a)
                                        (declare (ignore parent))
                                        (values a (listp a) nil))
                                      '(1))))

  (is (equal '(2 (3 (4 5)))
             (mgl-pax::transform-tree (lambda (parent a)
                                        (declare (ignore parent))
                                        (values (if (listp a) a (1+ a))
                                                (listp a)
                                                nil))
                                      '(1 (2 (3 4))))))

  (is (equal '(1 2 (2 3 (3 4 4 5)))
             (mgl-pax::transform-tree (lambda (parent a)
                                        (declare (ignore parent))
                                        (values (if (listp a)
                                                    a
                                                    (list a (1+ a)))
                                                (listp a)
                                                (not (listp a))))
                                      '(1 (2 (3 4)))))))


(deftest test-sanitize-docstring-aggressively ()
  (flet ((test1 (test-name in out)
           (with-test (nil :name test-name)
             (let ((input (format nil in))
                   (expected (format nil out)))
               (is (equal (pax::sanitize-docstring
                           input :aggressivep t :first-line-special-p nil)
                          expected))))))
    (test1 "0"   "xxx~%~%(1~%2~%"           "xxx~%~%(1~%2~%")
    (test1 "1"   "xxx~%~% (1~% 2~%"         "xxx~%~%    (1~%    2~%")
    (test1 "2"   "xxx~%~%  (1~%  2~%"       "xxx~%~%    (1~%    2~%")
    (test1 "3"   "xxx~%~%   (1~%   2~%"     "xxx~%~%    (1~%    2~%")
    (test1 "4"   "xxx~%~%    (1~%    2~%"   "xxx~%~%    (1~%    2~%")
    (test1 "5"   "xxx~%~%     (1~%     2~%" "xxx~%~%        (1~%        2~%")
    (test1 "1/6" "xxx~%~% (1~%      2~%"    "xxx~%~%    (1~%         2~%")
    (test1 "1/6" "xxx~%~% (1~%      2~%"    "xxx~%~%    (1~%         2~%")
    (test1 "no newline"   "xxx"             "xxx")
    (test1 "no newline 2" "xxx~%~% (1"      "xxx~%~%    (1")
    (test1 "consecutive blocks"
           "xxx~%~% (1~%~% (2~%"
           "xxx~%~%    (1~%~%    (2~%")
    (test1 "simple"  "xxx~%~% (1~%~%xxx"    "xxx~%~%    (1~%~%xxx")
    (test1 "comment" "xxx~%~% ;1~%~%xxx"    "xxx~%~%    ;1~%~%xxx")
    (test1 "html"    "<x>&amp;&"                 "\\<x>\\&amp;&")
    (test1 "html in verbatim" "x~%~% (<x>&" "x~%~%    (<x>&")
    (test1 "heading"  "#x"                  "\\#x")
    (test1 "heading2" "# x"                 "\\# x")
    (test1 "heading3" "x~%~%    # x"        "x~%~%    \\# x")))


(deftest test-codify ()
  (with-test ("unadorned")
    (with-test ("uninterned")
      (with-test ("len=1")
        (is (not (internedp "U")))
        (check-head "U" "U")
        (check-head "\\U" "U")
        (check-head "-" "-"))
      (with-test ("len=2")
        (is (not (internedp "UN")))
        (check-head "UN" "UN")
        (check-head "\\UN" "UN")
        (check-head "/=" "/="))
      (with-test ("len=3")
        (is (not (internedp "UNI")))
        (check-head "UNI" "UNI")
        (check-head "\\UNI" "UNI")
        (is (not (internedp "*U*")))
        (check-head "*U*" "*U*")
        (check-head "*\\U*" "*U*")
        (check-head "///" "///")
        (check-head "Uni" "Uni")
        (check-head "UnI" "UnI")))
    (with-test ("internal")
      (with-test ("len=1")
        (is (not (mgl-pax::external-symbol-p 'q)))
        (check-head "Q" "Q")
        (check-head "\\Q" "Q"))
      (with-test ("len=2")
        (is (not (mgl-pax::external-symbol-p 'qq)))
        (check-head "QQ" "QQ")
        (check-head "\\QQ" "QQ"))
      (with-test ("len=3")
        (is (not (mgl-pax::external-symbol-p 'qqq)))
        (check-head "QQQ" "`QQQ`")
        (check-head "\\QQQ" "QQQ")
        (is (not (mgl-pax::external-symbol-p '*q*)))
        (check-head "*Q*" "`*Q*`")
        (check-head "*\\Q*" "*Q*")))
    (with-test ("external")
      (let ((*document-link-to-hyperspec* nil))
        (check-head "T" "`T`")
        (check-head "\\T" "T")
        (check-head "DO" "`DO`")
        (check-head "\\DO" "DO")
        (check-head "COS" "`COS`")
        (check-head "\\COS" "COS")))
    (with-test ("external with ref")
      ;; T is not autolinked.
      (check-head "T" "`T`")
      (check-head "\\T" "T")
      (check-head "DO" "[`DO`][5d2b]")
      (check-head "\\DO" "DO")
      (check-head "COS" "[`COS`][c4a3]")
      (check-head "\\COS" "COS"))
    (with-test (":EMPH")
      (let ((*document-link-code* nil))
        (check-head "*PACKAGE*" "`*PACKAGE*`")
        (check-head "CL:*PACKAGE*" "`CL:*PACKAGE*`"))))
  (with-test ("reflink")
    (with-test ("no refs")
      (check-head "[U]" "\\[U\\]")))
  (with-test ("in :REFERENCE")
    (check-document "xxx
xxx

  [some]: PRINT \"DO\""
                    "xxx
xxx

[some]: PRINT \"DO\"

"))
  (is (internedp 'references))
  (check-head "REFERENCEs" "`REFERENCE`s" :msg "interned lowercase plural"))

(defun q ())
(defun qq ())
(defun qqq ())
(defvar *q*)


(deftest test-names ()
  (with-test ("Uppercase name with uppercase plural.")
    (check-head "CARS" "[`CAR`][d5a2]s")
    (check-head "CARS." "[`CAR`][d5a2]s.")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "CLASSES" "[`CLASS`][1f37]es")
      (check-head "CLASSES." "[`CLASS`][1f37]es."))
    (check-head "ARRAY-DIMENSIONS" "[`ARRAY-DIMENSIONS`][b315]")
    (check-head "ARRAY-DIMENSIONS." "[`ARRAY-DIMENSIONS`][b315]."))
  (with-test ("Uppercase name with lowercase plural.")
    (check-head "CARs" "[`CAR`][d5a2]s")
    (check-head "CARs." "[`CAR`][d5a2]s.")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "CLASSes" "[`CLASS`][1f37]es")
      (check-head "CLASSes." "[`CLASS`][1f37]es."))
    (check-head "ARRAY-DIMENSIONs" "[`ARRAY-DIMENSION`][6c28]s")
    (check-head "ARRAY-DIMENSIONs." "[`ARRAY-DIMENSION`][6c28]s."))
  (with-test ("Uppercase code + lowercase plural.")
    (check-head "`CAR`s" "[`CAR`][d5a2]s")
    (check-head "`CAR`s." "[`CAR`][d5a2]s.")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "`CLASS`es" "[`CLASS`][1f37]es")
      (check-head "`CLASS`es." "[`CLASS`][1f37]es."))
    (check-head "`ARRAY-DIMENSION`s" "[`ARRAY-DIMENSION`][6c28]s")
    (check-head "`ARRAY-DIMENSION`s." "[`ARRAY-DIMENSION`][6c28]s."))
  (with-test ("Lowercase code + lowercase plural.")
    (check-head "`car`s" "[`car`][d5a2]s")
    (check-head "`car`s." "[`car`][d5a2]s.")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "`class`es" "[`class`][1f37]es")
      (check-head "`class`es." "[`class`][1f37]es."))
    (check-head "`array-dimension`s" "[`array-dimension`][6c28]s")
    (check-head "`array-dimension`s." "[`array-dimension`][6c28]s."))
  (with-test ("Lowercase code with lowercase plural.")
    (check-head "`cars`" "[`cars`][d5a2]")
    (check-head "`cars.`" "`cars.`")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "`classes`" "[`classes`][1f37]"))
    (check-head "`classes.`" "`classes.`")
    (check-head "`array-dimensions`" "[`array-dimensions`][b315]")
    (check-head "`array-dimensions.`" "`array-dimensions.`"))
  (with-test ("Uppercase name with uppercase plural in reflink.")
    (check-head "[CARS][]" "[`CAR`s][d5a2]")
    (check-head "[CARS.][]" "`CAR`s." :warnings 1)
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "[CLASSES][]" "[`CLASS`es][1f37]"))
    (check-head "[CLASSES.][]" "`CLASS`es." :warnings 1)
    (check-head "[ARRAY-DIMENSIONS][]" "[`ARRAY-DIMENSIONS`][b315]")
    (check-head "[ARRAY-DIMENSIONS.][]" "`ARRAY-DIMENSIONS`." :warnings 1))
  (with-test ("Uppercase name with lowercase plural in reflink.")
    (check-head "[CARs][]" "[`CAR`s][d5a2]")
    (check-head "[CARs.][]" "`CAR`s." :warnings 1)
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "[CLASSes][]" "[`CLASS`es][1f37]"))
    (check-head "[CLASSes.][]" "`CLASS`es." :warnings 1)
    ;; Somewhat surprisingly, the ARRAY-DIMENSIONS is to be linked as
    ;; the PAX::@NAME is determined by PARSE-TREE-TO-TEXT.
    (check-head "[ARRAY-DIMENSIONs][]" "[`ARRAY-DIMENSION`s][b315]")
    (check-head "[ARRAY-DIMENSIONs.][]" "`ARRAY-DIMENSION`s." :warnings 1))
  (with-test ("Uppercase code + lowercase plural in reflink.")
    (check-head "[`CAR`s][]" "[`CAR`s][d5a2]")
    (check-head "[`CAR`s.][]" "`CAR`s." :warnings 1)
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "[`CLASS`es][]" "[`CLASS`es][1f37]"))
    (check-head "[`CLASS`es.][]" "`CLASS`es." :warnings 1)
    (check-head "[`ARRAY-DIMENSION`s][]" "[`ARRAY-DIMENSION`s][b315]")
    (check-head "[`ARRAY-DIMENSION`s.][]" "`ARRAY-DIMENSION`s." :warnings 1))
  (with-test ("Trimming")
    (check-head "`#<CLASS>`" "`#<CLASS>`")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "\\#\\<CLASS>" "\\#\\<[`CLASS`][1f37]>")))
  (check-head "*PRINT-LENGTH*s" "[`*PRINT-LENGTH*`][8f7a]s")
  (check-head "\\Delta" "\\Delta" :msg "mathjax")
  (check-head "T." "`T`.")
  (check-head "`doc/`" "`doc/`")
  (check-head "non-NIL" "non-`NIL`")
  (check-head "nonNIL" "non`NIL`")
  (with-test ("READable")
    (check-head "READable" "[`READ`][fe58]able")
    (check-head "[READable][]" "`READ`able" :warnings 1)
    (check-head "[`READ`able][]" "`READ`able" :warnings 1))
  (with-test ("nonREADable")
    (check-head "nonREADable" "non[`READ`][fe58]able")
    (check-head "[nonREADable][]" "non`READ`able" :warnings 1)
    (check-head "[non`READ`able][]" "non`READ`able" :warnings 1))
  (with-test ("non-NIL")
    (check-head "non-NIL" "non-`NIL`")
    (check-head "-NIL" "-NIL" :msg "no lowercase before first uppercase")
    (check-head "NIL-" "NIL-" :msg "no lowercase after last uppercase")
    (check-head "NILable" "`NIL`able")
    (is (internedp '%nil))
    (check-head "%NILable" "`%NIL`able"))
  (with-test ("one uppercase character only")
    (check-head "anT" "anT")
    (check-head "Tea" "Tea")
    (check-head "Ts" "Ts")
    (check-head "T=3" "T=3"))
  (check-head "Classes" "Classes")
  (with-failure-expected ((alexandria:featurep :clisp))
    (check-head "`Classes`" "[`Classes`][1f37]"))
  (check-head "`\"=>\"`" "`\"=>\"`")
  (with-test ("no uppercase")
    (check-head "non-nil" "non-nil"))
  (with-test ("uppercase too short")
    (check-head "nonXX" "nonXX")))


(deftest test-downcasing ()
  (test-downcasing-in-docstrings)
  (test-downcasing-of-section-names))

(defsection @section-without-title (:export nil))

(deftest test-downcasing-in-docstrings ()
  (with-test ("unadorned")
    (check-downcasing "NOT-INTERNED" "NOT-INTERNED")
    (check-downcasing "CaMeL" "CaMeL")
    ;; has no refs
    (check-downcasing "TEST" "`test`")
    ;; has refs
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-downcasing "CLASS" "[`class`][1f37]"))
    ;; has no refs
    (check-downcasing "*FORMAT*" "`*format*`")
    ;; has refs
    (check-downcasing "*PACKAGE*" "[`*package*`][5ed1]")
    ;; section with refs
    (check-downcasing (list "@SECTION-WITHOUT-TITLE" @section-without-title)
                      "[`@section-without-title`][eeac]"))
  (with-test ("escaped unadorned")
    (check-downcasing "\\NOT-INTERNED" "NOT-INTERNED")
    (check-downcasing "\\CaMeL" "\\CaMeL")
    (check-downcasing "*\\CaMeL*" "*\\CaMeL*")
    (check-downcasing "\\TEST" "TEST")
    (check-downcasing "\\CLASS" "CLASS")
    (check-downcasing "*\\FORMAT*" "*FORMAT*")
    (check-downcasing "*\\PACKAGE*" "*PACKAGE*")
    (check-downcasing (list "\\@SECTION-WITHOUT-TITLE" @section-without-title)
                      "@SECTION-WITHOUT-TITLE"))
  (with-test ("code")
    (check-downcasing "`NOT-INTERNED`" "`not-interned`")
    (check-downcasing "`CaMeL`" "`CaMeL`")
    (check-downcasing "`TEST`" "`test`")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-downcasing "`CLASS`" "[`class`][1f37]"))
    (check-downcasing "`*FORMAT*`" "`*format*`")
    (check-downcasing "`*PACKAGE*`" "[`*package*`][5ed1]")
    (check-downcasing (list "`@SECTION-WITHOUT-TITLE`" @section-without-title)
                      "[`@section-without-title`][eeac]"))
  (with-test ("escaped code")
    (check-downcasing "`\\NOT-INTERNED`" "`not-interned`")
    (check-downcasing "`\\CaMeL`" "`CaMeL`")
    (check-downcasing "`\\TEST`" "`test`")
    (check-downcasing "`\\CLASS`" "`class`")
    (check-downcasing "`\\*FORMAT*`" "`*format*`")
    (check-downcasing "`\\*PACKAGE*`" "`*package*`")
    (check-downcasing (list "`\\@SECTION-WITHOUT-TITLE`"
                            @section-without-title)
                      "`@section-without-title`"))
  (with-test ("doubly escaped code")
    (check-downcasing "`\\\\NOT-INTERNED`" "`NOT-INTERNED`")
    (check-downcasing "`\\\\CaMeL`" "`CaMeL`")
    (check-downcasing "`\\\\TEST`" "`TEST`")
    (check-downcasing "`\\\\CLASS`" "`CLASS`")
    (check-downcasing "`\\\\*FORMAT*`" "`*FORMAT*`")
    (check-downcasing "`\\\\*PACKAGE*`" "`*PACKAGE*`")
    (check-downcasing (list "`\\\\@SECTION-WITHOUT-TITLE`"
                            @section-without-title)
                      "`@SECTION-WITHOUT-TITLE`"))
  (with-test ("reflink unadorned")
    (check-downcasing "[NOT-INTERNED][]" "NOT-INTERNED" :warnings 1)
    (check-downcasing "[CaMeL][]" "CaMeL" :warnings 1)
    (check-downcasing "[TEST][]" "`test`" :warnings 1)
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-downcasing "[CLASS][]" "[`class`][1f37]"))
    (check-downcasing "[*FORMAT*][]" "`*format*`" :warnings 1)
    (check-downcasing "[*PACKAGE*][]" "[`*package*`][5ed1]")
    (check-downcasing (list "[@SECTION-WITHOUT-TITLE][]"
                            @section-without-title)
                      "[`@section-without-title`][eeac]"))
  (with-test ("reflink code")
    (check-downcasing "[`NOT-INTERNED`][]" "`not-interned`" :warnings 1)
    (check-downcasing "[`CaMeL`][]" "`CaMeL`" :warnings 1)
    (check-downcasing "[`TEST`][]" "`test`" :warnings 1)
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-downcasing "[`CLASS`][]" "[`class`][1f37]"))
    (check-downcasing "[`*FORMAT*`][]" "`*format*`" :warnings 1)
    (check-downcasing "[`*PACKAGE*`][]" "[`*package*`][5ed1]")
    (check-downcasing (list "[`@SECTION-WITHOUT-TITLE`][]"
                            @section-without-title)
                      "[`@section-without-title`][eeac]"))
  (with-test ("reflink escaped code")
    (check-downcasing "[`\\NOT-INTERNED`][]" "`not-interned`" :warnings 1)
    (check-downcasing "[`\\CaMeL`][]" "`CaMeL`" :warnings 1)
    (check-downcasing "[`\\TEST`][]" "`test`" :warnings 1)
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-downcasing "[`\\CLASS`][]" "[`class`][1f37]"))
    (check-downcasing "[`\\*FORMAT*`][]" "`*format*`" :warnings 1)
    (check-downcasing "[`\\*PACKAGE*`][]" "[`*package*`][5ed1]")
    (check-downcasing (list "[`\\@SECTION-WITHOUT-TITLE`][]"
                            @section-without-title)
                      "[`@section-without-title`][eeac]"))
  (with-test ("reflink doubly escaped code")
    (check-downcasing "[`\\\\NOT-INTERNED`][]" "`NOT-INTERNED`" :warnings 1)
    (check-downcasing "[`\\\\CaMeL`][]" "`CaMeL`" :warnings 1)
    (check-downcasing "[`\\\\TEST`][]" "`TEST`" :warnings 1)
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-downcasing "[`\\\\CLASS`][]" "[`CLASS`][1f37]"))
    (check-downcasing "[`\\\\*FORMAT*`][]" "`*FORMAT*`" :warnings 1)
    (check-downcasing "[`\\\\*PACKAGE*`][]" "[`*PACKAGE*`][5ed1]")
    (check-downcasing (list "[`\\\\@SECTION-WITHOUT-TITLE`][]"
                            @section-without-title)
                      "[`@SECTION-WITHOUT-TITLE`][eeac]"))
  (with-test ("multiple symbols")
    (check-downcasing "`(LIST :XXX 'PRINT)`" "`(list :xxx 'print)`")
    (check-downcasing "`(PRINT \"hello\")`" "`(print \"hello\")`")
    (check-downcasing "`(PRINT '\\\"hello\\\")`" "`(PRINT '\\\"hello\\\")`"))
  (with-test ("no-uppercase-is-code")
    (let ((*document-uppercase-is-code* nil))
      (check-downcasing "XXX" "XXX")
      (check-downcasing "`XXX`" "`xxx`")
      (check-downcasing "`(PRINT \"hello\")`" "`(print \"hello\")`"))))

(defsection @parent-section-without-title (:export nil)
  (@section-without-title section))

(deftest test-downcasing-of-section-names ()
  (let ((*document-downcase-uppercase-code* t))
    (check-document @parent-section-without-title
                    "<a id=\"MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION\"></a>

# `@parent-section-without-title`

## Table of Contents

- [1 `@section-without-title`][eeac]

###### \\[in package MGL-PAX-TEST\\]
<a id=\"MGL-PAX-TEST:@SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION\"></a>

## 1 `@section-without-title`


  [eeac]: #MGL-PAX-TEST:@SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION \"`mgl-pax-test::@section-without-title`\"
")))

(defun check-downcasing (docstring expected &key (warnings 0))
  (let ((*document-downcase-uppercase-code* t))
    (check-head docstring expected :warnings warnings)))


(deftest test-link ()
  (test-autolink)
  (test-resolve-reflink)
  (test-explicit-label)
  (test-suppressed-links))


(deftest test-autolink ()
  (with-test ("object with multiple refs")
    (check-head (list "macro BAR function"
                      (dref 'bar 'type)
                      (dref 'bar 'macro))
                ;; "3e5e" is the id of the macro.
                "macro [`BAR`][3e5e] function"
                :msg "locative before, irrelavant locative after")
    (check-head (list "function BAR macro"
                      (dref 'bar 'type)
                      (dref 'bar 'macro))
                "function [`BAR`][3e5e] macro"
                :msg "locative after, irrelavant locative before")
    (check-head (list "macro BAR type"
                      (dref 'bar 'type)
                      (dref 'bar 'macro)
                      (dref 'bar 'constant))
                ;; "e2a5" is the the id of the type.
                "macro `BAR`([`0`][3e5e] [`1`][e2a5]) type"
                :msg "ambiguous locative"))
  (with-test ("locative in backticks")
    (check-head (list "`TEST-GF` `(method t (number))`"
                      (dref 'test-gf '(method () (number))))
                "[`TEST-GF`][044a] `(method t (number))`")
    (check-head (list "`(method t (number))` `TEST-GF`"
                      (dref 'test-gf '(method () (number))))
                "`(method t (number))` [`TEST-GF`][044a]"))
  (with-test ("escaped autolinking")
    (check-head "`\\PRINT`" "`PRINT`"))
  (with-test ("used to fail")
    (check-head " :KEY xxx" " `:KEY` xxx")))

(deftest test-resolve-reflink ()
  (with-test ("label is a single name")
    (check-head "[*PACKAGE*][]" "[`*PACKAGE*`][5ed1]")
    (check-head "[*PACKAGE*][variable]" "[`*PACKAGE*`][5ed1]")
    (check-head "[ *PACKAGE*][]" "[ `*PACKAGE*`][5ed1]")
    (check-head "[*PACKAGE*][ variable]" "[`*PACKAGE*`][5ed1]")
    (check-head "[*PACKAGE*]" "\\[[`*PACKAGE*`][5ed1]\\]")
    (check-head "[*PACKAGE*][normaldef]" "[`*PACKAGE*`][normaldef]")
    (check-head "[*FORMAT*][]" "`*FORMAT*`" :warnings 1))
  (with-test ("definition is a reference")
    (check-head "[see this][car function]" "[see this][d5a2]")
    (check-head "[`see` *this*][car function]" "[`see` *this*][d5a2]"))
  (with-test ("definition is an object")
    (check-head "[see this][print]" "[see this][d451]")
    (check-head "[ see this ][print]" "[ see this ][d451]")
    (check-head "[see this][ print]" "[see this][d451]")
    (check-head "[see this][\\*package*]" "[see this][5ed1]")
    (check-head "[see this][nil]" "see this([`0`][9990] [`1`][4df2])"))
  (with-test ("definition is both a locative and an object")
    (check-head (list "[see this][section]"
                      (dref 'section 'class)
                      (dref 'section 'locative))
                "[see this][5fac]")
    (check-head (list "[FORMAT][dislocated]"
                      (dref 'dislocated 'locative)
                      (dref 'pax::@explicit-and-autolinking
                              'section))
                "`FORMAT`"
                :package (find-package '#:mgl-pax))
    (check-head (list "[NOT-CODE][dislocated]"
                      (dref 'dislocated 'locative)
                      (dref 'pax::@explicit-and-autolinking
                              'section))
                "NOT-CODE"
                :package (find-package '#:mgl-pax))
    (check-head (list "[`SOME-CODE`][dislocated]"
                      (dref 'dislocated 'locative)
                      (dref 'pax::@explicit-and-autolinking
                              'section))
                "`SOME-CODE`"
                :package (find-package '#:mgl-pax))
    (check-head "[locative][dislocated]" "locative")
    (check-head "[LOCATIVE][dislocated]" "`LOCATIVE`"))
  (with-test ("name with reference hiding in interesting object")
    (is (internedp 'sections))
    (check-head (list "[SECTIONS][class]"
                      (dref 'section 'class))
                "[`SECTIONS`][5fac]")
    (check-head (list "[SECTIONs][class]"
                      (dref 'section 'class))
                "[`SECTION`s][5fac]")
    (check-head (list "[SECTION][class]"
                      (dref 'section 'class))
                "[`SECTION`][5fac]"))
  (with-test ("normal markdown reference link")
    (with-test ("simple")
      (check-head "[see this][ddd]

  [ddd]: #ttt"
                  "[see this][ddd]"))
    (with-test ("definition is also an interned symbol")
      (is (internedp 'references))
      (check-head "[see this][references]

  [references]: #ttt"
                  "see this"))
    (with-test ("definition is an interned symbol with a definition")
      (check-head "[see this][print]

  [print]: #ttt"
                  "[see this][d451]")))
  (when pax::*3bmd-reflink-definition-is-list*
    (with-test ("emph in reflink definition")
      (check-head "[xxx][*print-length* variable]" "[xxx][8f7a]"))
    (with-test ("backtick in reflink definition")
      (check-head "[xxx][`*print-length*` variable]" "[xxx][8f7a]"))))


(defsection @section-with-title (:title "My `Title`" :export nil))
(define-glossary-term @gt-with-title (:title "My `Title`") "")

(deftest test-explicit-label ()
  (with-test ("section")
    (check-downcasing (list "@SECTION-WITH-TITLE" @section-with-title)
                      "[My `Title`][619a]")
    (check-downcasing (list "`@SECTION-WITH-TITLE`" @section-with-title)
                      "[My `Title`][619a]")
    (check-downcasing (list "[@SECTION-WITH-TITLE][]" @section-with-title)
                      "[My `Title`][619a]")
    (check-downcasing (list "[`@SECTION-WITH-TITLE`][]" @section-with-title)
                      "[My `Title`][619a]"))
  (with-test ("glossary-term")
    (check-downcasing (list "@GT-WITH-TITLE" @gt-with-title)
                      "[My `Title`][fecf]")
    (check-downcasing (list "`@GT-WITH-TITLE`" @gt-with-title)
                      "[My `Title`][fecf]")
    (check-downcasing (list "[@GT-WITH-TITLE][]" @gt-with-title)
                      "[My `Title`][fecf]")
    (check-downcasing (list "[`@GT-WITH-TITLE`][]" @gt-with-title)
                      "[My `Title`][fecf]")))


(deftest test-suppressed-links ()
  (test-t-and-nil-links)
  (test-repeated-links)
  (test-self-referencing-links))

(deftest test-t-and-nil-links ()
  (check-head "T" "`T`")
  (check-head "NIL" "`NIL`"))

(deftest test-repeated-links ()
  (check-head "PRINT PRINT" "[`PRINT`][d451] `PRINT`")
  (check-head (list "PRINT" "PRINT") "[`PRINT`][d451]~%~%[`PRINT`][d451]"
              :n-lines 3)
  (check-head "[STRING][function] STRING" "[`STRING`][dae6] `STRING`")
  (check-head "[STRING][dislocated] STRING" "`STRING` `STRING`")
  (check-head "[STRING][function] STRING function"
              "[`STRING`][dae6] [`STRING`][dae6] function"))

(defun self-referencing ()
  "This is SELF-REFERENCING."
  ())

(define-glossary-term @self-referencing-term (:title "Self-referencing Term")
  "This is @SELF-REFERENCING-TERM.")

(defsection @self-referencing (:export nil :title "Self-referencing")
  "This is @SELF-REFERENCING.")

(deftest test-self-referencing-links ()
  (check-document #'self-referencing
                  "<a id=\"MGL-PAX-TEST:SELF-REFERENCING%20FUNCTION\"></a>

- [function] **SELF-REFERENCING**

    This is `SELF-REFERENCING`.
")
  (check-document @self-referencing-term
                  "<a id=\"MGL-PAX-TEST:@SELF-REFERENCING-TERM%20MGL-PAX:GLOSSARY-TERM\"></a>

- [glossary-term] **Self-referencing Term**

    This is [Self-referencing Term][a79b].

  [a79b]: #MGL-PAX-TEST:@SELF-REFERENCING-TERM%20MGL-PAX:GLOSSARY-TERM \"Self-referencing Term\"
")
  (let ((*document-max-table-of-contents-level* 0))
    (check-document @self-referencing
                    "<a id=\"MGL-PAX-TEST:@SELF-REFERENCING%20MGL-PAX:SECTION\"></a>

# Self-referencing

###### \\[in package MGL-PAX-TEST\\]
This is [Self-referencing][e042].

  [e042]: #MGL-PAX-TEST:@SELF-REFERENCING%20MGL-PAX:SECTION \"Self-referencing\"
")))


(deftest test-headings ()
  ;; See PAX::HEADING-OFFSET.
  (check-pred pax::@emacs-setup (lambda (string)
                                  (search (format nil "~%## Emacs Setup~%")
                                          string))))


(deftest test-base-url ()
  (dolist (*document-base-url* '("http://example.com" "http://example.com/"))
    (check-pred "[xxx](x.html)" "http://example.com/x.html"
                :msg ":EXPLICIT-LINK")
    (check-pred (format nil "[xxx][def]~%~%  [def]: x.html")
                "http://example.com/x.html"
                :msg ":REFERENCE")
    (check-pred "![xxx](x.jpg)" "http://example.com/x.jpg"
                :msg ":IMG :EXPLICIT-LINK")
    (check-pred (format nil "![xxx][def]~%~%  [def]: x.jpg")
                "http://example.com/x.jpg"
                :msg ":IMG :REFERENCE")
    (check-pred (list "FOO function" (dref 'foo 'function))
                "[bc64]: #MGL-PAX-TEST:FOO%20FUNCTION"
                :msg "intra-page")
    (check-pred "<a href='relative'>link</a>"
                "<a href='relative'>link</a>"
                :msg "explicit html link"))
  (signals (error :pred "no query and fragment")
    (let ((*document-base-url* "http://example.com/?q"))
      (document "xxx")))
  (signals (error :pred "no query and fragment")
    (let ((*document-base-url* "http://example.com/#z"))
      (document "xxx"))))


(deftest test-url-versions ()
  (check-document (xref 'foo2 'function)
                  "<a id=\"x-28MGL-PAX-TEST-3AFOO2-20FUNCTION-29\"></a>

- [function] **FOO2** *OOK X*

    `FOO2` has args `OOK` and `X`.
"
                  :url-versions '(1))
  (check-document (xref 'foo2 'function)
                  "<a id=\"MGL-PAX-TEST:FOO2%20FUNCTION\"></a>

- [function] **FOO2** *OOK X*

    `FOO2` has args `OOK` and `X`.
"
                  :url-versions '(2))
  (check-document (xref 'foo2 'function)
                  "<a id=\"x-28MGL-PAX-TEST-3AFOO2-20FUNCTION-29\"></a>
<a id=\"MGL-PAX-TEST:FOO2%20FUNCTION\"></a>

- [function] **FOO2** *OOK X*

    `FOO2` has args `OOK` and `X`.
"
                  :url-versions '(1 2)))

(deftest test-pages ()
  (let ((*package* (find-package (find-package :mgl-pax-test))))
    (let ((outputs (document (list #'foo #'->max) :stream nil
                                                  :pages `((:objects (,#'foo)
                                                            :output nil)
                                                           (:objects ()
                                                            :output (nil))
                                                           (:objects (,#'->max)
                                                            :output (nil))))))
      (when (is (= (length outputs) 2))
        (with-failure-expected ((and (alexandria:featurep '(:or :ecl))
                                     'failure))
          (is (equal (first outputs)
                     "- [function] FOO OOK X

    FOO has args OOK and X.
")))
        (is (equal (second outputs)
                   "- [function] ->MAX
"))))))


(defsection @section-with-undefined-stuff ()
  (undefined undefined))

(deftest test-locate-error ()
  (signals (locate-error)
    (document '@section-with-undefined-stuff)))


(defparameter *nasty-var* (coerce '(#\Space #\Linefeed #\Tab #\Newline
                                    #\Page #\Return)
                                  'string)
  "docstring")

(deftest test-variable ()
  (with-failure-expected ((alexandria:featurep :clisp))
    (check-document (dref '*nasty-var* 'variable)
                    "<p><a id=\"MGL-PAX-TEST:*NASTY-VAR*%20VARIABLE\"></a></p>
<ul>
<li><p><span class=reference-bullet><span class=reference><span class=\"locative-type\">[variable]</span> <span class=\"reference-object\"><a href=\"#MGL-PAX-TEST:*NASTY-VAR*%20VARIABLE\" >*NASTY-VAR*</a></span></span> <span class=\"locative-args\">&quot; 
\\
\\&quot;</span></span></p>

<p>docstring</p></li>
</ul>
"
                    :format :html))
  (with-test ("initform")
    (check-pred (dref '*some-var* '(variable 7))
                "- [variable] **\\*SOME-VAR\\*** *7*")))


(deftest test-constant ()
  (check-pred (dref 'bar 'constant)
              "- [constant] **BAR** *2*")
  (with-test ("actualizing")
    (check-pred (dref 'bar 'variable)
                "- [constant] **BAR** *2*"))
  (with-test ("actualizing and initform")
    (check-pred (dref 'bar '(variable 7))
                "- [constant] **BAR** *7*")))


(deftest test-macro ()
  (test-macro/arglist))

(deftest test-macro/arglist ()
  ;; C is a parameter. If it were treated as a default value, then
  ;; *DOCUMENT-MARK-UP-SIGNATURES* would be accessed, and this would
  ;; fail.
  (progv (list '*document-mark-up-signatures*) ()
    (is (equal (mgl-pax::arglist-to-markdown '((&key a) (b c)))
               "(&KEY A) (B C)")))
  (with-test ("macro-with-whole-and-dot")
    (is (equal (mgl-pax::arglist-to-markdown '(name . args))
               "NAME . ARGS")))
  (with-test ("macro-with-body-and-dot")
    (is (equal (mgl-pax::arglist-to-markdown '(&body (name . args)))
               "&BODY (NAME . ARGS)")))
  (with-test ("macro-with-rest-and-dot")
    (is (equal (mgl-pax::arglist-to-markdown '(&rest (x y . z)))
               "&REST (X Y . Z)"))))


(defsection @test-symbol-macro (:export nil)
  (my-smac symbol-macro))

(deftest test-symbol-macro ()
  (check-document (dref 'my-smac 'symbol-macro)
                  "<a id=\"MGL-PAX-TEST:MY-SMAC%20MGL-PAX:SYMBOL-MACRO\"></a>

- [symbol-macro] **MY-SMAC**

    This is `MY-SMAC`.
"))


(deftest test-setf ()
  (is (null (dref 'undefined 'setf nil)))
  (test-setf/expander)
  (test-setf/function)
  (test-setf/generic-function)
  (test-setf/method))

(deftest test-setf/expander ()
  (with-failure-expected ((and (alexandria:featurep :abcl) 'failure))
    (check-document (dref 'has-setf-expander 'setf)
                    "<a id=\"MGL-PAX-TEST:HAS-SETF-EXPANDER%20SETF\"></a>

- [setf] **HAS-SETF-EXPANDER**

    ddd
")))

(deftest test-setf/function ()
  (with-failure-expected
      ((and (alexandria:featurep '(:or :abcl)) 'failure))
    (check-document (dref 'has-setf-function 'setf)
                    "<a id=\"MGL-PAX-TEST:HAS-SETF-FUNCTION%20SETF\"></a>

- [setf] **HAS-SETF-FUNCTION** *V*

    eee
")))

(deftest test-setf/generic-function ()
  (with-failure-expected ((and (alexandria:featurep '(:or :cmucl))
                               'failure))
    (check-document (dref 'has-setf-generic-function 'setf)
                    "<a id=\"MGL-PAX-TEST:HAS-SETF-GENERIC-FUNCTION%20SETF\"></a>

- [setf] **HAS-SETF-GENERIC-FUNCTION** *V*

    fff
")))

(deftest test-setf/method ()
  (with-failure-expected ((alexandria:featurep :clisp))
    (signals-not (locate-error)
      (check-document
       (dref 'has-setf-generic-function '(setf (method () (string))))
       "<a id=\"MGL-PAX-TEST:HAS-SETF-GENERIC-FUNCTION%20%28SETF%20%28METHOD%20NIL%20%28STRING%29%29%29\"></a>

- [setf] **HAS-SETF-GENERIC-FUNCTION** *(V STRING)*

    ggg
"))))


(deftest test-function ()
  (test-function-args)
  (test-function/encapsulated)
  (test-non-function-function-arglist))

(deftest test-function-args ()
  (with-failure-expected ((alexandria:featurep :clisp))
    (check-document #'foo2 "<a id=\"MGL-PAX-TEST:FOO2%20FUNCTION\"></a>

- [function] **FOO2** *OOK X*

    `FOO2` has args `OOK` and `X`.
")))

(when (fboundp 'encapsulated-function)
  (untrace encapsulated-function))
(defun encapsulated-function (x &rest args)
  "This may be encapsulated."
  (declare (ignore x args))
  nil)
(trace encapsulated-function)

(when (fboundp 'encapsulated-generic-function)
  (untrace encapsulated-generic-function))
(defgeneric encapsulated-generic-function (x)
  (:documentation "This may be encapsulated."))
(trace encapsulated-generic-function)

(deftest test-function/encapsulated ()
  (let ((expected "<a id=\"MGL-PAX-TEST:ENCAPSULATED-FUNCTION%20FUNCTION\"></a>

- [function] **ENCAPSULATED-FUNCTION** *X &REST ARGS*

    This may be encapsulated.
"))
    (with-failure-expected ((alexandria:featurep '(:or :abcl :clisp :ecl)))
      (check-document (dref 'encapsulated-function 'function nil)
                      expected)
      (signals-not (locate-error)
        (check-document #'encapsulated-function expected))))
  (let ((expected "<a id=\"MGL-PAX-TEST:ENCAPSULATED-GENERIC-FUNCTION%20GENERIC-FUNCTION\"></a>

- [generic-function] **ENCAPSULATED-GENERIC-FUNCTION** *X*

    This may be encapsulated.
"))
    (with-failure-expected ((and (alexandria:featurep '(:or :abcl :clisp :ecl))
                                 'failure))
      (check-document (dref 'encapsulated-generic-function
                              'generic-function nil)
                      expected))
    (with-failure-expected ((and (alexandria:featurep '(:or :abcl))
                                 'failure))
      (signals-not (locate-error)
        (with-failure-expected ((and (alexandria:featurep '(:or :clisp :ecl))
                                     'failure))
          (check-document #'encapsulated-generic-function expected))))))

(deftest test-non-function-function-arglist ()
  #+sbcl
  (is (match-values (arglist (dref 'sb-c::ir1-convert-nlx-protect
                                          'function))
        (equal * '(sb-c::protected &body sb-c::cleanup))
        (eq * :ordinary)))
  ;; Check that DOCUMENT doesn't fail when the function lambda list is
  ;; really a macro lambda list.
  #+sbcl
  (check-document #'sb-c::ir1-convert-nlx-protect
                  "<a id=\"SB-C:IR1-CONVERT-NLX-PROTECT%20FUNCTION\"></a>

- [function] **SB-C::IR1-CONVERT-NLX-PROTECT** *PROTECTED &BODY CLEANUP*
"))


(deftest test-generic-function ()
  ;; Referring to a GENERIC-FUNCTION as FUNCTION
  (check-head (xref 'test-gf 'function)
              "<a id=\"MGL-PAX-TEST:TEST-GF%20GENERIC-FUNCTION\"></a>")
  (check-pred (dref 'test-gf 'generic-function)
              "`TEST-GF` is not a link."))


(deftest test-method-combination ()
  (with-failure-expected ((alexandria:featurep '(:or :abcl :allegro)))
    (check-document (dref 'my-comb 'method-combination)
                    "<a id=\"MGL-PAX-TEST:MY-COMB%20METHOD-COMBINATION\"></a>

- [method-combination] **MY-COMB**

    This is `MY-COMB`.
")))


(deftest test-method ()
  (test-method/arglist)
  (signals-not (error)
    (document (dref 'test-gf '(method () ((eql #.(find-package :cl)))))
              :stream nil))
  (is (equal (pax::urldecode "MGL-PAX-TEST:TEST-GF%20%28METHOD%20NIL%20%28%28EQL%20%23%3CPACKAGE%20%22COMMON-LISP%22%3E%29%29%29")
             "MGL-PAX-TEST:TEST-GF (METHOD NIL ((EQL #<PACKAGE \"COMMON-LISP\">)))"))
  (check-document (dref 'test-gf '(method () (number)))
                  "<a id=\"MGL-PAX-TEST:TEST-GF%20%28METHOD%20NIL%20%28NUMBER%29%29\"></a>

- [method] **TEST-GF** *(X NUMBER)*

    `TEST-GF` is not a link. `X` is not a link.
"))

(deftest test-method/arglist ()
  (check-pred (dref 'test-gf '(method () ((eql :bar))))
              "- [method] **TEST-GF** *(X (EQL :BAR))*"))

(deftest test-accessor ()
  (check-head (list "FOO-A `(accessor foo)`"
                    (dref 'foo-a '(accessor foo))
                    (dref 'foo-a 'variable))
              "[`FOO-A`][dbec] `(accessor foo)`"))

(deftest test-reader ()
  (check-head (list "FOO-R `(reader foo)`"
                    (dref 'foo-r '(reader foo))
                    (dref 'foo-r 'variable))
              "[`FOO-R`][618a] `(reader foo)`")
  (check-document (dref 'foo-r '(reader foo))
                  "<a id=\"MGL-PAX-TEST:FOO-R%20%28MGL-PAX:READER%20MGL-PAX-TEST::FOO%29\"></a>

- [reader] **FOO-R** *FOO*
"))

(deftest test-writer ()
  (check-head (list "FOO-W `(writer foo)`"
                    (dref 'foo-w '(writer foo))
                    (dref 'foo-w 'variable))
              "[`FOO-W`][2b65] `(writer foo)`"))

(deftest test-structure-accessor ()
  (check-pred (dref 'baz-aaa '(structure-accessor baz))
              "- [structure-accessor] **BAZ-AAA** *BAZ*
"))


(deftest test-declaration ()
  (check-head "SAFETY" "[`SAFETY`][f384]")
  (check-head "SAFETY declaration" "[`SAFETY`][f384] declaration")
  (check-head "[safety][declaration]" "[safety][f384]"))


(deftest test-condition ()
  (check-document
   (list (dref 'transcription-values-consistency-error
                'condition)
         (dref 'transcription-consistency-error
                'condition))
   "<a id=\"MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR%20CONDITION\"></a>

- [condition] **TRANSCRIPTION-VALUES-CONSISTENCY-ERROR** *[TRANSCRIPTION-CONSISTENCY-ERROR][a249]*

    Signalled (with [`CERROR`][4317]) by `TRANSCRIBE` when invoked
    with `:CHECK-CONSISTENCY` and the values of a form are inconsistent
    with their parsed representation.

<a id=\"MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR%20CONDITION\"></a>

- [condition] **TRANSCRIPTION-CONSISTENCY-ERROR** *TRANSCRIPTION-ERROR*

    A common superclass for
    `TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR` and
    [`TRANSCRIPTION-VALUES-CONSISTENCY-ERROR`][238c].

  [238c]: #MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR%20CONDITION \"MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR CONDITION\"
  [4317]: CLHS/Body/f_cerror.htm \"CERROR (MGL-PAX:CLHS FUNCTION)\"
  [a249]: #MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR%20CONDITION \"MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR CONDITION\"
"))


(deftest test-restart ()
  (check-head "ABORT restart" "[`ABORT`][ae44] restart")
  (check-document (dref 'use-value 'restart)
                  "<a id=\"USE-VALUE%20RESTART\"></a>

- [restart] **USE-VALUE** *VALUE*

    This is the name of the [`RESTART`][38e4] to which [`USE-VALUE`][5406]
    transfers control.

  [38e4]: CLHS/Body/t_rst.htm \"RESTART (MGL-PAX:CLHS CLASS)\"
  [5406]: CLHS/Body/f_abortc.htm \"USE-VALUE (MGL-PAX:CLHS FUNCTION)\"
"))


(deftest test-asdf-system ()
  (with-test ("name is a symbol accessible in the current package")
    (is (find-symbol (string '#:mgl-pax/full) '#:mgl-pax-test))
    (check-head (list "MGL-PAX/FULL"
                      (dref :mgl-pax/full 'asdf:system))
                "`MGL-PAX/FULL`")
    (check-head (list "MGL-PAX/FULL asdf:system"
                      (dref 'mgl-pax/full 'asdf:system))
                "[`MGL-PAX/FULL`][d761] asdf:system")
    (check-head (list "[MGL-PAX/FULL][asdf:system]"
                      (dref 'mgl-pax/full 'asdf:system))
                "[`MGL-PAX/FULL`][d761]"))
  (with-test ("name is not a symbol accessible in the current package")
    (is (null (find-symbol (string '#:mgl-pax-test) '#:mgl-pax-test)))
    (check-head (list "MGL-PAX-TEST"
                      (dref "mgl-pax-test" 'asdf:system))
                "MGL-PAX-TEST")
    (check-head (list "MGL-PAX-TEST asdf:system"
                      (dref "mgl-pax-test" 'asdf:system))
                "MGL-PAX-TEST asdf:system")
    (check-head (list "`MGL-PAX-TEST` asdf:system"
                      (dref "mgl-pax-test" 'asdf:system))
                "[`MGL-PAX-TEST`][cad4] asdf:system")
    (check-head (list "[MGL-PAX-TEST][asdf:system]"
                      (dref "mgl-pax-test" 'asdf:system))
                "[MGL-PAX-TEST][cad4]")))


(defpackage interned-pkg-name)
(defpackage #:non-interned-pkg-name)

(deftest test-package ()
  (check-head (list "INTERNED-PKG-NAME"
                    (dref 'interned-pkg-name 'package))
              "`INTERNED-PKG-NAME`")
  (check-head (list "INTERNED-PKG-NAME package"
                    (dref 'interned-pkg-name 'package))
              "[`INTERNED-PKG-NAME`][0651] package")
  (check-head (list "[INTERNED-PKG-NAME][package]"
                    (dref 'interned-pkg-name 'package))
              "[`INTERNED-PKG-NAME`][0651]")
  (let ((*package* (find-package :mgl-pax-test)))
    (is (not (internedp '#:non-interned-pkg-name))))
  (check-head (list "NON-INTERNED-PKG-NAME"
                    (dref '#:non-interned-pkg-name 'package))
              "NON-INTERNED-PKG-NAME")
  (check-head (list "NON-INTERNED-PKG-NAME package"
                    (dref '#:non-interned-pkg-name 'package))
              "NON-INTERNED-PKG-NAME package")
  (check-head (list "[NON-INTERNED-PKG-NAME][package]"
                    (dref '#:non-interned-pkg-name 'package))
              "[NON-INTERNED-PKG-NAME][5a00]"))


(deftest test-readtable ()
  (with-failure-expected ((alexandria:featurep :abcl))
    (check-document (named-readtables:find-readtable 'xxx-rt)
                    "<a id=\"MGL-PAX-TEST:XXX-RT%20READTABLE\"></a>

- [readtable] **XXX-RT**

    ddd
"))
  (check-head (list "[XXX-RT][readtable]"
                    (named-readtables:find-readtable 'xxx-rt))
              "[`XXX-RT`][ec74]"))


;;;; PAX::@PAX-LOCATIVES

(deftest test-locative ()
  (check-document (dref 'pax::funny-loc 'locative)
                  "<a id=\"MGL-PAX:FUNNY-LOC%20MGL-PAX:LOCATIVE\"></a>

- [locative] **MGL-PAX::FUNNY-LOC** *SOME-ARG*

    This is `SOME-ARG`.
" :package (find-package :cl)))

(define-glossary-term @external-link (:title "See X"
                                      :url "http://example.com/x")
                      "docstring")

(deftest test-glossary-term ()
  (with-test ("external links")
    (check-document "@EXTERNAL-LINK" "[See X][ffc6]

  [ffc6]: http://example.com/x \"See X\"
")
    (check-document "[xxx][@external-link]" "[xxx][ffc6]

  [ffc6]: http://example.com/x \"See X\"
")
    (check-document
     @external-link
     "<a id=\"MGL-PAX-TEST:@EXTERNAL-LINK%20MGL-PAX:GLOSSARY-TERM\"></a>

- [glossary-term] **See X**

    External link to [http://example.com/x](http://example.com/x).

    docstring
")))

(deftest test-go ()
  (with-test ("canonicalize GO target")
    (check-ref (dref 'xxx '(go (stream type)))
               'xxx '(go (stream class))))
  (check-head "[XXX][(go (3.4.1 clhs))]" "[`XXX`][4336]")
  (check-head "[&KEY][(go (3.4.1 clhs))]" "[`&KEY`][4336]")
  (check-head "&KEY" "[`&KEY`][4336]")
  (check-head "XXX `(go (3.4.1 clhs))`" "[`XXX`][4336] `(go (3.4.1 clhs))`")
  (check-head "&KEY `(go (1 clhs))`" "[`&KEY`][81be] `(go (1 clhs))`")
  (with-test ("accidental GO with no arguments")
    (check-head "PRINT go" "[`PRINT`][d451] go")))

(deftest test-docstring ()
  (check-head "[BAR CONSTANT][docstring]" "`BAR` is not a link.")
  (check-head (list "[BAR CONSTANT][docstring]"
                    (dref 'bar 'constant))
              "[`BAR`][f3f4] is not a link."))

(deftest test-hyperspec ()
  (check-head "FIND-IF" "[`FIND-IF`][5884]")
  (check-head "LIST" "`LIST`([`0`][79d8] [`1`][6d9f])")
  (check-head "[LIST][type]" "[`LIST`][79d8]")
  (check-head "T" "`T`")
  (check-head "NIL" "`NIL`")
  (check-head "[T][]" "`T`([`0`][9172] [`1`][fe21])")
  (check-head "[T][constant]" "[`T`][fe21]")
  (check-pred #'print (lambda (output)
                        (search "- [function] **PRINT**" output))))

(deftest test-clhs-definitions ()
  (check-ref (dref 'function '(clhs class) nil)
             'function '(clhs class))
  (check-ref (dref 'function '(clhs macro) nil)
             'function '(clhs macro))
  (is (null (dref 'function '(clhs xxx) nil)))
  (is (null (dref 'xxx '(clhs function) nil)))
  (with-test ("disambiguation paged preferred to section and glossary entry")
    (check-ref (dref 'function 'clhs nil)
               'function 'clhs))
  (check-head "[function][(clhs class)]" "[function][119e]")
  (check-head "[function][(clhs macro)]" "[function][81f7]")
  (with-test ("disambiguation page")
    (check-head "[function][clhs]" "[function][aeb6]"))
  (check-head "[PRINT][clhs]" "[`PRINT`][d451]")
  (with-test ("clhs entry does not clutter disambiguations")
    (check-document (list "[PRINT][clhs]" "PRINT")
                    "[`PRINT`][d451]

[`PRINT`][d451]

  [d451]: CLHS/Body/f_wr_pr.htm \"PRINT (MGL-PAX:CLHS FUNCTION)\"
")
    (check-document (list "PRINT" "[PRINT][clhs]")
                    "[`PRINT`][d451]

[`PRINT`][d451]

  [d451]: CLHS/Body/f_wr_pr.htm \"PRINT (MGL-PAX:CLHS FUNCTION)\"
"))
  (with-test ("prefer live definition to CLHS")
    (with-failure-expected ((alexandria:featurep '(:or :abcl :allegro :ecl)))
      (check-head (list "[PRINT][function]" #'print) "[`PRINT`][fdd1]"))
    (with-failure-expected ((alexandria:featurep
                             '(:or :abcl :allegro :ccl :clisp :cmucl)))
      (check-head (list "[DOCUMENTATION][generic-function]" #'documentation)
                  "[`DOCUMENTATION`][68f1]")))
  (when (null (dref 'otherwise 'macro nil))
    (with-test ("if no live definition, then link to CLHS")
      (check-head "[otherwise][macro]" "[otherwise][c9ce]")))
  (with-test ("explicit definition link always works")
    (check-head "[PRINT][pax:clhs]" "[`PRINT`][d451]")))

(deftest test-clhs-section ()
  (let ((*document-link-to-hyperspec* t))
    (test-clhs-section-1))
  (with-failure-expected ()
    (let ((*document-link-to-hyperspec* nil))
      (test-clhs-section-1))))

(defun test-clhs-section-1 ()
  ;; "A.1" and "3.4" are section ids in the CLHS.
  (check-ref (dref "A.1" '(clhs section) nil)
             "A.1" '(clhs section))
  (is (null (dref "a.1" '(clhs section) nil)))
  (check-ref (dref "lambda lists" '(clhs section) nil)
             "3.4" '(clhs section))
  (check-ref (dref "Lambda Lists" '(clhs section) nil)
             "3.4" '(clhs section))
  (check-head "A.1" "A.1")
  (check-head "`A.1`" "`A.1`")
  (check-head "CLHS A.1" "`CLHS` A.1")
  (check-head "CLHS 3.4" "`CLHS` 3.4")
  (check-head "CLHS `3.4`" "`CLHS` [`3.4`][e442]")
  (check-head "`3.4` CLHS" "[`3.4`][e442] `CLHS`")
  (check-head "[3.4][]" "3.4" :warnings 1)
  (check-head "[`3.4`][]" "`3.4`" :warnings 1)
  (check-head "[3.4][CLHS]" "[3.4][e442]")
  (check-head "[Lambda Lists][clhs]" "[Lambda Lists][e442]")
  (check-head "[03_d][clhs]" "[03\\_d][e442]")
  (check-head "[ Lambda  Lists ][clhs]" "[ Lambda  Lists ][e442]"))

(deftest test-clhs-glossary-entries ()
  (check-head "[readably][(clhs glossary-term)]" "[readably][278a]")
  (check-document "[non-local exit][clhs]" "[non-local exit][b815]

  [b815]: CLHS/Body/26_glo_n.htm#non-local_exit '\"non-local exit\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
")
  (check-head (format nil "[ non-local~%exit ][(clhs glossary-term)]")
              (format nil "[ non-local~%exit ][b815]")
              :n-lines 2))

(deftest test-clhs-issue ()
  (let ((*document-link-to-hyperspec* t))
    (test-clhs-issue-1))
  (let ((*document-link-to-hyperspec* nil))
    (test-clhs-issue-1)))

(defun test-clhs-issue-1 ()
  (check-ref (dref "ISSUE:AREF-1D" 'clhs nil)
             '"ISSUE:AREF-1D" '(clhs section))
  (check-ref (dref "iss009" 'clhs nil)
             '"SUMMARY:AREF-1D" '(clhs section))
  (check-head "ISSUE:AREF-1D" "ISSUE:AREF-1D")
  (check-head "`ISSUE:AREF-1D`" "`ISSUE:AREF-1D`")
  (check-head "CLHS ISSUE:AREF-1D" "`CLHS` ISSUE:AREF-1D")
  (check-head "ISSUE:AREF-1D CLHS" "ISSUE:AREF-1D `CLHS`")
  (check-head "CLHS `ISSUE:AREF-1D`" "`CLHS` [`ISSUE:AREF-1D`][63ef]")
  (check-head "`ISSUE:AREF-1D` CLHS" "[`ISSUE:AREF-1D`][63ef] `CLHS`")
  (check-head "[ISSUE:AREF-1D][]" "ISSUE:AREF-1D" :warnings 1)
  (check-head "[`ISSUE:AREF-1D`][]" "`ISSUE:AREF-1D`" :warnings 1)
  (check-head "[ISSUE:AREF-1D][CLHS]" "[ISSUE:AREF-1D][63ef]")
  (check-head "[iss009][clhs]" "[iss009][e357]"))

(deftest test-argument ()
  (check-head "[PRINT][argument]" "`PRINT`"))

(define-locative-alias %%%defun function)

(deftest test-define-locative-alias ()
  (check-head (list "FOO %%%defun"
                    (dref 'foo 'function)
                    (dref 'foo 'compiler-macro))
              "[`FOO`][bc64] %%%defun"))


(deftest test-cl-transcript ()
  (let ((*document-hyperspec-root* "CLHS/")
        (input "```cl-transcript
(print :hello)
..
.. :HELLO 
=> :HELLO
```")
        (expected "```common-lisp
(print :hello)
..
.. :HELLO 
=> :HELLO
```

"))
    (check-document input expected)
    (signals (transcription-consistency-error)
      (document "```cl-transcript
(print :hello)
..
.. :WORLD
=> :HELLO
```"))))


(defvar *testing-bad-transcript* nil)

(defsection @must-have ()
  (foo-with-bad-transcript function))
(defun foo-with-bad-transcript ()
  "```cl-transcript (:dynenv check-transcript-only-if-testing)
  (1+ 2)
  => 7
  ```"
  nil)

(defun check-transcript-only-if-testing (fn)
  ;; Silence the warning if someone does an apropos on this.
  (let ((*transcribe-check-consistency* *testing-bad-transcript*))
    (funcall fn)))

(deftest test-document/open ()
  (with-test ("no link duplication for objects being documented")
    (check-head (list "PAX:LOCATIVE"
                      (dref 'pax:locative 'pax:locative))
                "[`PAX:LOCATIVE`][0b3a]"
                :w3m t))
  (with-failure-expected ((alexandria:featurep :clisp))
    (with-test ("simplified ambiguous links")
      (check-head "AMBI" "[`AMBI`](pax:MGL-PAX-TEST:AMBI)" :w3m t))
    (is (find 'package (definitions 'cl) :key #'xref-locative-type))
    (with-test ("escaping of non-ambiguous")
      (check-head "`foo<>&`"
                  "<a href=\"pax:MGL-PAX-TEST:FOO%3C%3E%26%20FUNCTION\" title=\"MGL-PAX-TEST:FOO&lt;&gt;&amp; FUNCTION\"><strong><code>foo&lt;&gt;&amp;</code></strong></a>"
                  :w3m t :format :html))
    (with-test ("escaping of ambiguous")
      (check-head "`ambi<>&`"
                  "<a href=\"pax:MGL-PAX-TEST:AMBI%3C%3E%26\" ><strong><code>ambi&lt;&gt;&amp;</code></strong></a>"
                  :w3m t :format :html)))
  (let ((*error-output* (make-broadcast-stream))
        (*testing-bad-transcript* t))
    (check-head (dref 'foo-with-bad-transcript 'function)
                "<a id=\"MGL-PAX-TEST:FOO-WITH-BAD-TRANSCRIPT%20FUNCTION\"></a>

- [function] **FOO-WITH-BAD-TRANSCRIPT**

    ```common-lisp
    (1+ 2)
    => 7
    ```"
                :n-lines 8 :warnings 1 :w3m t))
  (with-test ("[in package]")
    (let ((*package* (find-package '#:mgl-pax-test)))
      (check-pred @test-examples (lambda (output)
                                   (not (search "in package" output)))
                  :w3m t)))
  (test-documentables-of)
  (test-document/open/live-vs-static)
  (test-document/open/object)
  (test-document/open/clhs)
  (test-document/open/undefined))

(deftest test-documentables-of ()
  ;; This test relies on what is and what is not available through
  ;; SWANK-BACKEND:FIND-DEFINITIONS in a given implementation.
  #+sbcl
  (is (endp (different-elements
             (dref::sort-references (pax::documentables-of nil))
             (list (dref "NIL" '(clhs glossary-term))
                   (dref :common-lisp 'readtable)
                   (dref 'nil '(clhs constant))
                   (dref 'nil '(clhs type))
                   (dref 'nil 'clhs)
                   (dref 'nil 'constant))
             :pred (lambda (r1 r2)
                     (and (typep r1 'xref)
                          (typep r2 'xref)
                          (xref= r1 r2)))))))

(deftest test-document/open/live-vs-static ()
  (with-test ("prefer live definition to CLHS")
    (with-failure-expected
        ((alexandria:featurep '(:or :abcl :allegro :clisp :ecl)))
      (check-head "[PRINT][function]" "[`PRINT`][fdd1]" :w3m t))
    (with-failure-expected ((alexandria:featurep
                             '(:or :abcl :allegro :ccl :clisp :cmucl)))
      (check-head "[DOCUMENTATION][generic-function]" "[`DOCUMENTATION`][68f1]"
                  :w3m t)))
  (when (null (dref 'otherwise 'macro nil))
    (with-test ("if no live definition, then link to CLHS")
      (check-head "[otherwise][macro]" "[otherwise][c9ce]" :w3m t))))

(deftest test-document/open/object ()
  (check-head "[PAX][package] [MGL-PAX][package] [`mgl-pax`][asdf:system]"
              "[PAX][97b3] [MGL-PAX][97b3] [`mgl-pax`][6fdb]"
              :w3m t))

(deftest test-document/open/clhs ()
  (let ((*document-hyperspec-root* "CLHS/"))
    (loop for (object locative) in '((print function)
                                     (single-float type))
          do (let ((reference (dref object `(clhs ,locative))))
               (signals-not (error :msg ("REFERENCE=~S" reference))
                 (let ((url (let ((*standard-output* (make-broadcast-stream)))
                              (pax::document-for-emacs/reference reference
                                                                 nil))))
                   (is (stringp url))
                   (is (alexandria:starts-with-subseq "CLHS/" url)))))))
  (with-test ("ambiguous link")
    ;; This used to fail because CLHS aliases are XREFs and not DREFs.
    (let ((pax::*document-open-linking* t))
      (pax:document "DOUBLE-FLOAT" :stream nil :format :html))))

(defsection @bad-section (:export nil)
  (undefined function))

(deftest test-document/open/undefined ()
  (check-head @bad-section
              "<a id=\"MGL-PAX-TEST:@BAD-SECTION%20MGL-PAX:SECTION\"></a>"
              :warnings 1 :w3m t))

(defun ambi ())
(defclass ambi () ())
(defun foo<>& ())
(defun ambi<>& ())
(defclass ambi<>& () ())


(deftest test-map-documentable ()
  (is (equal (with-output-to-string (*standard-output*)
               (mgl-pax::map-documentable 'princ 1))
             "1"))
  (is (equal (with-output-to-string (*standard-output*)
               (mgl-pax::map-documentable 'princ nil))
             ""))
  (is (equal (with-output-to-string (*standard-output*)
               (mgl-pax::map-documentable 'princ '(nil)))
             "NIL"))
  (is (equal (with-output-to-string (*standard-output*)
               (mgl-pax::map-documentable 'princ '(1 2 3)))
             "123"))
  (is (equal (with-output-to-string (*standard-output*)
               (mgl-pax::map-documentable 'princ '((progv '(*print-base*) '(2))
                                                   1 2 3)))
             "11011"))
  (with-failure-expected ((alexandria:featurep :ecl))
    (is (equal (with-output-to-string (*standard-output*)
                 (mgl-pax::map-documentable 'princ
                                            '(2
                                              ((progv '(*print-base*) '(2))
                                               3)
                                              4)))
               "2114"))))


(deftest test-table-of-contents ()
  (check-document (list @parent-section-without-title
                        @test-examples)
                  "- [`@PARENT-SECTION-WITHOUT-TITLE`][74ce]
- [`@TEST-EXAMPLES`][bb1c]

<a id=\"MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION\"></a>

# `@PARENT-SECTION-WITHOUT-TITLE`

## Table of Contents

- [1 `@SECTION-WITHOUT-TITLE`][eeac]

###### \\[in package MGL-PAX-TEST\\]
<a id=\"MGL-PAX-TEST:@SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION\"></a>

## 1 `@SECTION-WITHOUT-TITLE`


<a id=\"MGL-PAX-TEST:@TEST-EXAMPLES%20MGL-PAX:SECTION\"></a>

# `@TEST-EXAMPLES`

###### \\[in package MGL-PAX-TEST\\]
example section

  [74ce]: #MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION \"`MGL-PAX-TEST::@PARENT-SECTION-WITHOUT-TITLE`\"
  [bb1c]: #MGL-PAX-TEST:@TEST-EXAMPLES%20MGL-PAX:SECTION \"`MGL-PAX-TEST::@TEST-EXAMPLES`\"
  [eeac]: #MGL-PAX-TEST:@SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION \"`MGL-PAX-TEST::@SECTION-WITHOUT-TITLE`\"
")
  (test-table-of-contents-reapated-section-depth))

(deftest test-table-of-contents-reapated-section-depth ()
  ;; When the same section is documented twice: first as a subsection
  ;; of another section, second directly as itself, then determining
  ;; its heading depth is trickier.
  (check-document (list @parent-section-without-title @section-without-title)
                  "- [`@PARENT-SECTION-WITHOUT-TITLE`][74ce]
- [`@SECTION-WITHOUT-TITLE`][eeac]

<a id=\"MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION\"></a>

# `@PARENT-SECTION-WITHOUT-TITLE`

## Table of Contents

- [1 `@SECTION-WITHOUT-TITLE`][eeac]

###### \\[in package MGL-PAX-TEST\\]
<a id=\"MGL-PAX-TEST:@SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION\"></a>

## 1 `@SECTION-WITHOUT-TITLE`


<a id=\"MGL-PAX-TEST:@SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION\"></a>

## `@SECTION-WITHOUT-TITLE`

###### \\[in package MGL-PAX-TEST\\]

  [74ce]: #MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION \"`MGL-PAX-TEST::@PARENT-SECTION-WITHOUT-TITLE`\"
  [eeac]: #MGL-PAX-TEST:@SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION \"`MGL-PAX-TEST::@SECTION-WITHOUT-TITLE`\"
"))
