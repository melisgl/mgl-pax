(in-package :mgl-pax-test)

;;; Make Allegro record lambda lists, from which we can extract
;;; default values of arguments.
#+allegro
(eval-when (:compile-toplevel)
  (declaim (optimize (debug 3))))

(defun check-document (input expected)
  (let ((output (let ((*package* (find-package :mgl-pax-test))
                      (*document-hyperspec-root* "CLHS/"))
                  (first (document input)))))
    (is (null (mismatch% output expected))
        :ctx ("Input: ~S" input))))

(defun check-head (input expected &key (format :markdown) msg (n-lines 1))
  (let* ((*package* (find-package :mgl-pax-test))
         (*document-hyperspec-root* "CLHS/")
         (full-output (first (document input :format format)))
         (got (first-n-lines full-output n-lines))
         (expected (format nil expected)))
    (is (equal got expected)
        :ctx ("Input: ~S~%Full output:~%~S" input full-output))))

(defun first-n-lines (string n)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for i below n do
        (if (< i (1- n))
            (write-line (read-line in nil nil) out)
            (write-string (read-line in nil nil) out))))))

(defun internedp (name)
  (find-symbol (string name) :mgl-pax-test))


(defsection @stealing-from-other-package (:package (find-package :mgl-pax))
  (method locative))

(defsection @test-examples (:export nil)
  "example section")

(defsection @test-other (:export nil :title "test other title")
  "backlink @TEST")

(defsection @test-section-with-link-to-other-page-in-title
    (:title "Link to @TEST-OTHER"
            :link-title-to (@test-other section))
  "Same link in docstring to @TEST-OTHER.")

(defsection @test-section-with-link-to-same-page-in-title
    (:title "Link to @TEST" :link-title-to (@test section))
  "Same link in docstring to @TEST.")

(defsection @test-tricky-title
    (:export nil :title "`CODE` *italic* _italic2_ *bold* [link][sdf] <thing>")
  "backlink @TEST")

(define-locative-type my-loc ()
  "This is MY-LOC.")

(defun foo (ook x)
  "FOO has args OOK and X.

  This function FOO is related to compiler-macro FOO.

  Or [foo][compiler-macro], if you prefer.

  Now, [foo][] should link to [foo][compiler-macro] and [foo][class]
  but not to [foo][function]."
  (declare (ignore ook x))
  nil)
(define-compiler-macro foo ()
  "Docstring of a compiler macro."
  nil)
(defclass foo (unexported-class)
  ((a :accessor foo-a)
   (r :reader foo-r)
   (w :writer foo-w)))
(defclass unexported-class () ())
(defvar foo-a)
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
  (:documentation "This is MY-ERROR."))
(defun my-error ())

(defmacro bar (x y &key (z 7))
  "BAR has args X, Y and Z."
  (declare (ignore x y z))
  nil)
(deftype bar (x &rest r)
  "BAR has args X and R."
  (declare (ignore x r))
  'null)
(defconstant bar 2
  "BAR is not a link.")

(defgeneric baz ())
;; KLUDGE: CMUCL clobbers the DEFVAR's source location with that of
;; the DEFSTRUCT if they have the same name.
(defvar bazz)
(defstruct baz
  aaa)

(defgeneric test-gf (x)
  (:documentation "TEST-GF is not a link."))
(defmethod test-gf ((x number))
  "TEST-GF is not a link. X is not a link."
  nil)
(defmethod test-gf ((x (eql 7))))

(define-glossary-term some-term ()
  "SOME-TERM is a link.")

(defun ->max ())

(defmacro define-declaration (decl-name (decl-spec env) &body body)
  #+sbcl
  `(sb-cltl2:define-declaration ,decl-name (,decl-spec ,env)
     ,@body)
  #-sbcl
  (declare (ignore decl-name decl-spec env body)))

(define-declaration test-declaration (decl-spec env)
  (declare (ignore env))
  (values :declare decl-spec))

(unless (named-readtables:find-readtable 'xxx-rt)
  (named-readtables:defreadtable xxx-rt
    ;; KLUDGE: ABCL bundles an older named-readtables version that
    ;; does not support docstrings.
    #-abcl
    "ddd"))

(defparameter *navigation-test-cases*
  '(;; @MGL-PAX-VARIABLELIKE-LOCATIVES
    (foo-a variable (defvar foo-a))
    (foo-r variable (defvar foo-r))
    (foo-w variable (defvar foo-w))
    (bar constant (defconstant bar))
    ;; @MGL-PAX-MACROLIKE-LOCATIVES
    (bar macro (defmacro bar))
    (my-smac symbol-macro (define-symbol-macro my-smac))
    (foo compiler-macro (define-compiler-macro foo))
    ;; @MGL-PAX-FUNCTIONLIKE-LOCATIVES
    (foo function (defun foo))
    (test-gf generic-function (defgeneric test-gf))
    (test-gf (method () (number)) (defmethod test-gf))
    (my-comb method-combination (define-method-combination my-comb))
    (foo-a (accessor foo) (defclass foo) (a :accessor foo-a))
    (foo-r (reader foo) (defclass foo) (r :reader foo-r))
    (foo-w (writer foo) (defclass foo) (w :writer foo-w))
    (baz-aaa structure-accessor (defstruct baz))
    ;; @MGL-PAX-TYPELIKE-LOCATIVES
    (bar type (deftype bar))
    (foo type (defclass foo))
    (my-error type (define-condition my-error))
    (foo class (defclass foo))
    (test-declaration declaration (define-declaration test-declaration))
    ;; @MGL-PAX-CONDITION-SYSTEM-LOCATIVES
    (my-error condition (define-condition my-error))
    (some-restart restart (define-restart some-restart))
    ;; @MGL-PAX-PACKAGELIKE-LOCATIVES
    (mgl-pax asdf:system ())
    (mgl-pax package
     (eval-when (:compile-toplevel :load-toplevel :execute))
     (cl:defpackage))
    (xxx-rt readtable (defreadtable xxx-rt))
    ;; @MGL-PAX-PAX-LOCATIVES
    (@mgl-pax-manual section (defsection @mgl-pax-manual))
    (some-term glossary-term (define-glossary-term some-term))
    (my-loc locative (define-locative-type my-loc))))

(defun working-locative-p (locative)
  (let ((type (locative-type locative)))
    (cond ((and (alexandria:featurep :abcl)
                (member type '(variable constant method type restart
                               section locative glossary-term)))
           nil)
          ((alexandria:featurep :clisp)
           nil)
          ((eq type 'symbol-macro)
           (alexandria:featurep '(:not :ccl)))
          ((eq type 'declaration)
           (alexandria:featurep :sbcl))
          ((eq type 'readtable)
           nil)
          ((eq type 'generic-function)
           ;; AllegroCL is off by one form.
           (alexandria:featurep '(:not :allegro)))
          ((eq type 'method-combination)
           (alexandria:featurep '(:not (:or :abcl :cmucl :ecl))))
          ((member type '(reader writer accessor))
           (alexandria:featurep '(:not (:or :abcl :cmucl :ecl))))
          ((eq type 'structure-accessor)
           (alexandria:featurep '(:not (:or :abcl :ecl))))
          ((eq type 'type)
           (alexandria:featurep '(:not :ecl)))
          ((eq type 'package)
           (alexandria:featurep '(:not (:or :abcl :allegro :clisp :cmucl
                                        :ecl))))
          (t
           t))))

(deftest test-navigation ()
  (dolist (test-case *navigation-test-cases*)
    (destructuring-bind
        (symbol locative prefix &optional alternative-prefix) test-case
      (let* ((ref (make-reference symbol locative))
             (located (resolve ref)))
        ;; Test FIND-SOURCE with a REFERENCE and a resolved object if
        ;; there is one.
        (dolist (target (if (and (typep located 'reference)
                                 (mgl-pax::reference= located ref))
                            (list ref)
                            (list ref located)))
          (with-test ((format nil "navigate to ~S" target))
            (with-failure-expected ((not (working-locative-p locative)))
              (let ((location (ignore-errors (find-source target))))
                (when (is (and location (not (eq :error (first location))))
                          :msg `("Find source location for (~S ~S)."
                                 ,symbol ,locative))
                  (multiple-value-bind (file position function-name)
                      (extract-source-location location)
                    (is (or position function-name))
                    (when position
                      (let ((form
                              (let ((*package* (find-package :mgl-pax-test)))
                                (read-form-from-file-position file position))))
                        (is (and (listp form)
                                 (or (alexandria:starts-with-subseq
                                      prefix form :test #'equal)
                                     (and alternative-prefix
                                          (alexandria:starts-with-subseq
                                           alternative-prefix form
                                           :test #'equal))))
                            :msg `("Find prefix ~S~@[ or ~S~] ~
                                    at source location~%~S~% ~
                                    for reference (~S ~S).~%~
                                    Form found was:~%~S."
                                   ,prefix ,alternative-prefix
                                   ,location ,symbol ,locative
                                   ,form))))))))))))))

(defun extract-source-location (location)
  (let ((file-entry (find :file (rest location) :key #'first))
        (position-entry (find :position (rest location) :key #'first))
        (offset-entry (find :offset (rest location) :key #'first))
        (function-name-entry (find :function-name (rest location)
                                   :key #'first)))
    (values (second file-entry)
            (cond (position-entry
                   (1- (second position-entry)))
                  (offset-entry
                   (1- (third offset-entry))))
            (second function-name-entry))))

(defun read-form-from-file-position (filename position)
  (with-open-file (stream filename :direction :input)
    (file-position stream position)
    (read stream)))

(deftest test-read-locative-from-string ()
  (let ((*package* (find-package :mgl-pax-test)))
    (unintern (read-from-string "non-interned"))
    (unintern (read-from-string "yyy"))
    (is (null (mgl-pax::read-locative-from-string "non-interned")))
    (is (null (find-symbol (string '#:non-interned))))
    (is (null (mgl-pax::read-locative-from-string "find")))
    (is (eq (mgl-pax::read-locative-from-string "function") 'function))
    (is (eq (mgl-pax::read-locative-from-string " function") 'function))
    (is (eq (mgl-pax::read-locative-from-string "function ") 'function))
    (is (null (mgl-pax::read-locative-from-string "function junk")))
    (let ((locative (mgl-pax::read-locative-from-string "(function yyy)")))
      (is (eq (first locative) 'function))
      (is (string= (symbol-name (second locative)) (string '#:yyy)))
      (is (eq (symbol-package (second locative)) *package*)))))

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
          (and (string= (symbol-name *) (string '#:yyy))
               (eq (symbol-package *) *package*))
          (eq * 'function)
          (eq * t)))
    (is (match-values (mgl-pax::read-reference-from-string " yyy  function ")
          (and (string= (symbol-name *) (string '#:yyy))
               (eq (symbol-package *) *package*))
          (eq * 'function)
          (eq * t)))
    (is (null (mgl-pax::read-reference-from-string "yyy function junk")))))

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

(deftest test-macro-arg-names ()
  (is (equal '(x a b c)
             (mgl-pax::macro-arg-names '((&key (x y)) (a b) &key (c d))))))


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
      (check-head "DO" "[`DO`][be20]")
      (check-head "\\DO" "DO")
      (check-head "COS" "[`COS`][90ce]")
      (check-head "\\COS" "COS")))
  ;; FIXME
  (with-test ("reflink")
    (with-test ("no refs")
      (check-head "[U]" "[U][]")
      (check-head "[FORMAT][dislocated]" "`FORMAT`"))))

(defun q ())
(defun qq ())
(defun qqq ())
(defvar *q*)


(deftest test-names ()
  (with-test ("Uppercase name with uppercase plural.")
    (check-head "CARS" "[`CAR`][86ef]s")
    (check-head "CARS." "[`CAR`][86ef]s.")
    (check-head "CLASSES" "[`CLASS`][46f7]es")
    (check-head "CLASSES." "[`CLASS`][46f7]es."))
  (with-test ("Uppercase name with lowercase plural.")
    (check-head "CARs" "[`CAR`][86ef]s")
    (check-head "CARs." "[`CAR`][86ef]s.")
    (check-head "CLASSes" "[`CLASS`][46f7]es")
    (check-head "CLASSes." "[`CLASS`][46f7]es."))
  (with-test ("Uppercase code + lowercase plural.")
    (check-head "`CAR`s" "[`CAR`][86ef]s")
    (check-head "`CAR`s." "[`CAR`][86ef]s.")
    (check-head "`CLASS`es" "[`CLASS`][46f7]es")
    (check-head "`CLASS`es." "[`CLASS`][46f7]es."))
  (with-test ("Lowercase code + lowercase plural.")
    (check-head "`car`s" "[`car`][86ef]s")
    (check-head "`car`s." "[`car`][86ef]s.")
    (check-head "`class`es" "[`class`][46f7]es")
    (check-head "`class`es." "[`class`][46f7]es."))
  (with-test ("Lowercase code with lowercase plural.")
    (check-head "`cars`" "[`cars`][86ef]")
    (check-head "`cars.`" "`cars.`")
    (check-head "`classes`" "[`classes`][46f7]")
    (check-head "`classes.`" "`classes.`"))
  (with-test ("Uppercase name with uppercase plural in reflink.")
    (check-head "[CARS][]" "[`CAR`s][86ef]")
    (check-head "[CARS.][]" "[`CAR`s.][]")
    (check-head "[CLASSES][]" "[`CLASS`es][46f7]")
    (check-head "[CLASSES.][]" "[`CLASS`es.][]"))
  (with-test ("Uppercase name with lowercase plural in reflink.")
    (check-head "[CARs][]" "[`CAR`s][86ef]")
    (check-head "[CARs.][]" "[`CAR`s.][]")
    (check-head "[CLASSes][]" "[`CLASS`es][46f7]")
    (check-head "[CLASSes.][]" "[`CLASS`es.][]"))
  (with-test ("Uppercase code + lowercase plural in reflink.")
    (check-head "[`CAR`s][]" "[`CAR`s][86ef]")
    (check-head "[`CAR`s.][]" "[`CAR`s.][]")
    (check-head "[`CLASS`es][]" "[`CLASS`es][46f7]")
    (check-head "[`CLASS`es.][]" "[`CLASS`es.][]"))
  (with-test ("Trimming")
    (check-head "`#<CLASS>`" "`#<CLASS>`")
    (check-head "#\\<CLASS>" "#<[`CLASS`][46f7]>")))


(deftest test-downcasing ()
  (test-downcasing-in-docstrings)
  (test-downcasing-of-section-names))

(defsection @section-without-title ())

(deftest test-downcasing-in-docstrings ()
  (with-test ("unadorned")
    (check-downcasing "NOT-INTERNED" "NOT-INTERNED")
    ;; has no refs
    (check-downcasing "TEST" "`test`")
    ;; has refs
    (check-downcasing "CLASS" "[`class`][46f7]")
    ;; has no refs
    (check-downcasing "*FORMAT*" "`*format*`")
    ;; has refs
    (check-downcasing "*PACKAGE*" "[`*package*`][1063]")
    ;; section with refs
    (check-downcasing (list "@SECTION-WITHOUT-TITLE" @section-without-title)
                      "[`@section-without-title`][9a4b]"))
  (with-test ("escaped unadorned")
    (check-downcasing "\\NOT-INTERNED" "NOT-INTERNED")
    (check-downcasing "\\TEST" "TEST")
    (check-downcasing "\\CLASS" "CLASS")
    (check-downcasing "*\\FORMAT*" "*FORMAT*")
    (check-downcasing "*\\PACKAGE*" "*PACKAGE*")
    (check-downcasing (list "\\@SECTION-WITHOUT-TITLE" @section-without-title)
                      "@SECTION-WITHOUT-TITLE"))
  (with-test ("code")
    (check-downcasing "`NOT-INTERNED`" "`not-interned`")
    (check-downcasing "`TEST`" "`test`")
    (check-downcasing "`CLASS`" "[`class`][46f7]")
    (check-downcasing "`*FORMAT*`" "`*format*`")
    (check-downcasing "`*PACKAGE*`" "[`*package*`][1063]")
    (check-downcasing (list "`@SECTION-WITHOUT-TITLE`" @section-without-title)
                      "[`@section-without-title`][9a4b]"))
  (with-test ("escaped code")
    (check-downcasing "`\\NOT-INTERNED`" "`NOT-INTERNED`")
    (check-downcasing "`\\TEST`" "`TEST`")
    (check-downcasing "`\\CLASS`" "`CLASS`")
    (check-downcasing "`\\*FORMAT*`" "`*FORMAT*`")
    (check-downcasing "`\\*PACKAGE*`" "`*PACKAGE*`")
    (check-downcasing (list "`\\@SECTION-WITHOUT-TITLE`"
                            @section-without-title)
                      "`@SECTION-WITHOUT-TITLE`"))
  (with-test ("reflink unadorned")
    (check-downcasing "[NOT-INTERNED][]" "[NOT-INTERNED][]")
    (check-downcasing "[TEST][]" "[`test`][]")
    (check-downcasing "[CLASS][]" "[`class`][46f7]")
    (check-downcasing "[*FORMAT*][]" "[`*format*`][]")
    (check-downcasing "[*PACKAGE*][]" "[`*package*`][1063]")
    (check-downcasing (list "[@SECTION-WITHOUT-TITLE][]"
                            @section-without-title)
                      "[`@section-without-title`][9a4b]"))
  (with-test ("reflink code")
    (check-downcasing "[`NOT-INTERNED`][]" "[`not-interned`][]")
    (check-downcasing "[`TEST`][]" "[`test`][]")
    (check-downcasing "[`CLASS`][]" "[`class`][46f7]")
    (check-downcasing "[`*FORMAT*`][]" "[`*format*`][]")
    (check-downcasing "[`*PACKAGE*`][]" "[`*package*`][1063]")
    (check-downcasing (list "[`@SECTION-WITHOUT-TITLE`][]"
                            @section-without-title)
                      "[`@section-without-title`][9a4b]"))
  (with-test ("multiple symbols")
    (check-downcasing "`(LIST :XXX 'PRINT)`" "`(list :xxx 'print)`")
    (with-failure-expected (t)
      (check-downcasing "`(PRINT \"hello\")`" "`(print \"hello\")`")))
  (with-test ("no-uppercase-is-code")
    (let ((*document-uppercase-is-code* nil))
      (check-downcasing "XXX" "XXX")
      (check-downcasing "`XXX`" "`xxx`")
      (check-downcasing "`(PRINT \"hello\")`" "`(print \"hello\")`"))))

(defsection @parent-section-without-title ()
  (@section-without-title section))

(deftest test-downcasing-of-section-names ()
  (let ((*document-downcase-uppercase-code* t))
    (check-document @parent-section-without-title
                    "<a id='x-28MGL-PAX-TEST-3A-40PARENT-SECTION-WITHOUT-TITLE-20MGL-PAX-3ASECTION-29'></a>

# @parent-section-without-title

## Table of Contents

- [1 @section-without-title][9a4b]

###### \\[in package MGL-PAX-TEST\\]
<a id='x-28MGL-PAX-TEST-3A-40SECTION-WITHOUT-TITLE-20MGL-PAX-3ASECTION-29'></a>

## 1 @section-without-title


  [9a4b]: #x-28MGL-PAX-TEST-3A-40SECTION-WITHOUT-TITLE-20MGL-PAX-3ASECTION-29 \"mgl-pax-test:@section-without-title\"
")))

(defun check-downcasing (docstring expected)
  (let ((*document-downcase-uppercase-code* t))
    (check-head docstring expected)))


(deftest test-link ()
  (test-autolink)
  (test-resolve-reflink)
  (test-explicit-label)
  (test-suppressed-links))


(deftest test-autolink ()
  (with-test ("object with multiple refs")
    (check-head (list "macro BAR function"
                      (make-reference 'bar 'type)
                      (make-reference 'bar 'macro))
                ;; "9119" is the id of the macro.
                "macro [`BAR`][9119] function"
                :msg "locative before, irrelavant locative after")
    (check-head (list "function BAR macro"
                      (make-reference 'bar 'type)
                      (make-reference 'bar 'macro))
                "function [`BAR`][9119] macro"
                :msg "locative after, irrelavant locative before")
    (check-head (list "macro BAR type"
                      (make-reference 'bar 'type)
                      (make-reference 'bar 'macro)
                      (make-reference 'bar 'constant))
                ;; "cece" is the the id of the type.
                "macro `BAR`([`0`][9119] [`1`][cece]) type"
                :msg "ambiguous locative"))
  (with-test ("locative in code")
    (check-head (list "`TEST-GF` `(method t (number))`"
                      (make-reference 'test-gf '(method () (number))))
                "[`TEST-GF`][ba01] `(method t (number))`")
    (check-head (list "`(method t (number))` `TEST-GF`"
                      (make-reference 'test-gf '(method () (number))))
                "`(method t (number))` [`TEST-GF`][ba01]")))

(deftest test-resolve-reflink ()
  (with-test ("label is a single name")
    (check-head "[*package*][]" "[*package*][]")
    (check-head "[*emphasized*][normaldef]" "[*emphasized*][normaldef]")
    (check-head "[*format*][]" "[*format*][]"))
  (with-test ("definition is a reference")
    (check-head "[see this][car function]" "[see this][86ef]")
    (check-head "[`see` *this*][car function]" "[`see` *this*][86ef]")
    (check-head "[see this][foo2]" "[see this][foo2]")))


(defsection @section-with-title (:title "My Title"))
(define-glossary-term @gt-with-title (:title "My Title") "")

(deftest test-explicit-label ()
  (with-test ("section")
    (check-downcasing (list "@SECTION-WITH-TITLE" @section-with-title)
                      "[My Title][224f]")
    (check-downcasing (list "`@SECTION-WITH-TITLE`" @section-with-title)
                      "[My Title][224f]")
    (check-downcasing (list "[@SECTION-WITH-TITLE][]" @section-with-title)
                      "[My Title][224f]")
    (check-downcasing (list "[`@SECTION-WITH-TITLE`][]" @section-with-title)
                      "[My Title][224f]"))
  (with-test ("glossary-term")
    (check-downcasing (list "@GT-WITH-TITLE" @gt-with-title)
                      "[My Title][cf05]")
    (check-downcasing (list "`@GT-WITH-TITLE`" @gt-with-title)
                      "[My Title][cf05]")
    (check-downcasing (list "[@GT-WITH-TITLE][]" @gt-with-title)
                      "[My Title][cf05]")
    (check-downcasing (list "[`@GT-WITH-TITLE`][]" @gt-with-title)
                      "[My Title][cf05]")))


(deftest test-suppressed-links ()
  (test-t-and-nil-links)
  (test-repeated-links)
  (test-self-referencing-links))

(deftest test-t-and-nil-links ()
  (check-head "T" "`T`")
  (check-head "NIL" "`NIL`"))

(deftest test-repeated-links ()
  (check-head "PRINT PRINT" "[`PRINT`][3acc] `PRINT`")
  (check-head (list "PRINT" "PRINT") "[`PRINT`][3acc]~%[`PRINT`][3acc]"
              :n-lines 2)
  (check-head "[STRING][function] STRING" "[`STRING`][8d40] `STRING`")
  (check-head "[STRING][dislocated] STRING" "`STRING` `STRING`")
  (check-head "[STRING][function] STRING function"
              "[`STRING`][8d40] [`STRING`][8d40] function"))

(defun self-referencing ()
  "This is SELF-REFERENCING."
  ())

(define-glossary-term @self-referencing-term (:title "Self-referencing Term")
  "This is @SELF-REFERENCING-TERM.")

(defsection @self-referencing (:title "Self-referencing")
  "This is @SELF-REFERENCING.")

(deftest test-self-referencing-links ()
  (check-document #'self-referencing
                  "<a id='x-28MGL-PAX-TEST-3A-3ASELF-REFERENCING-20FUNCTION-29'></a>

- [function] **SELF-REFERENCING** 

    This is `SELF-REFERENCING`.
")
  (check-document @self-referencing-term
                  "<a id='x-28MGL-PAX-TEST-3A-3A-40SELF-REFERENCING-TERM-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **Self-referencing Term**

    This is [Self-referencing Term][97fa].

  [97fa]: #x-28MGL-PAX-TEST-3A-3A-40SELF-REFERENCING-TERM-20MGL-PAX-3AGLOSSARY-TERM-29 \"(MGL-PAX-TEST::@SELF-REFERENCING-TERM MGL-PAX:GLOSSARY-TERM)\"
")
  (let ((*document-max-table-of-contents-level* 0))
    (check-document @self-referencing
                    "<a id='x-28MGL-PAX-TEST-3A-40SELF-REFERENCING-20MGL-PAX-3ASECTION-29'></a>

# Self-referencing

###### \\[in package MGL-PAX-TEST\\]
This is [Self-referencing][ca46].

  [ca46]: #x-28MGL-PAX-TEST-3A-40SELF-REFERENCING-20MGL-PAX-3ASECTION-29 \"Self-referencing\"
")))


(deftest test-macro ()
  (test-macro/canonical-reference)
  (test-macro/arglist))

(setf (macro-function 'setfed-macro)
      (lambda (whole env)
        (declare (ignore whole env))))

(deftest test-macro/canonical-reference ()
  (let ((ref (make-reference 'setfed-macro 'macro)))
    (is (mgl-pax::reference= (canonical-reference ref) ref))))

(defmacro macro-with-fancy-args (x &optional (o 1) &key (k 2 kp))
  (declare #+sbcl (sb-ext:muffle-conditions style-warning)
           (ignore x o k kp))
  ())

(deftest test-macro/arglist ()
  (with-failure-expected ((alexandria:featurep '(:or :allegro)))
    (is (or (equal (% (mgl-pax::arglist 'macro-with-fancy-args))
                   '(x &optional (o 1) &key (k 2 kp)))
            (equal (mgl-pax::arglist 'macro-with-fancy-args)
                   '(x &optional (o 1) &key (k 2)))))))


(defsection @test-symbol-macro ()
  (my-smac symbol-macro))

(define-symbol-macro my-smac 42)
(setf (documentation 'my-smac 'symbol-macro)
      "This is MY-SMAC.")

(deftest test-symbol-macro ()
  (check-document (make-reference 'my-smac 'symbol-macro)
                  "<a id='x-28MGL-PAX-TEST-3AMY-SMAC-20MGL-PAX-3ASYMBOL-MACRO-29'></a>

- [symbol-macro] **MY-SMAC**

    This is `MY-SMAC`.
"))


(deftest test-function ()
  (test-function/canonical-reference)
  (test-function/arglist)
  (test-function/encapsulated))

(setf (symbol-function 'setfed-function)
      (lambda ()))

(deftest test-function/canonical-reference ()
  (let ((ref (make-reference 'setfed-function 'function)))
    (is (mgl-pax::reference= (canonical-reference ref) ref))))

(defun function-with-fancy-args (x &optional (o 1) &key (k 2 kp))
  (declare #+sbcl (sb-ext:muffle-conditions style-warning)
           (ignore x o k kp))
  nil)

(deftest test-function/arglist ()
  (with-failure-expected ((alexandria:featurep :ccl))
    (is (equal (mgl-pax::arglist 'function-with-fancy-args)
               '(x &optional (o 1) &key (k 2 kp))))))

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
  (let ((expected "<a id='x-28MGL-PAX-TEST-3A-3AENCAPSULATED-FUNCTION-20FUNCTION-29'></a>

- [function] **ENCAPSULATED-FUNCTION** *X &REST ARGS*

    This may be encapsulated.
"))
    (with-failure-expected ((alexandria:featurep '(:or :abcl :clisp :ecl)))
      (check-document (make-reference 'encapsulated-function 'function)
                      expected)
      (check-document #'encapsulated-function expected)))
  (let ((expected "<a id='x-28MGL-PAX-TEST-3A-3AENCAPSULATED-GENERIC-FUNCTION-20GENERIC-FUNCTION-29'></a>

- [generic-function] **ENCAPSULATED-GENERIC-FUNCTION** *X*

    This may be encapsulated.
"))
    (with-failure-expected ((alexandria:featurep '(:or :abcl :clisp)))
      (check-document (make-reference 'encapsulated-generic-function
                                      'generic-function)
                      expected))
    (with-failure-expected ((alexandria:featurep
                             '(:or :abcl :clisp :cmucl :ecl)))
      (check-document #'encapsulated-generic-function expected))))


(setf (symbol-function 'setfed-generic-function)
      (lambda ()))

(deftest test-generic-function ()
  (let ((ref (make-reference 'setfed-generic-function 'function)))
    (is (mgl-pax::reference= (canonical-reference ref) ref))))


(deftest test-accessor ()
  (check-head (list "FOO-A `(accessor foo)`"
                    (make-reference 'foo-a '(accessor foo))
                    (make-reference 'foo-a 'variable))
              "[`FOO-A`][6483] `(accessor foo)`"))

(deftest test-reader ()
  (check-head (list "FOO-R `(reader foo)`"
                    (make-reference 'foo-r '(reader foo))
                    (make-reference 'foo-r 'variable))
              "[`FOO-R`][0aab] `(reader foo)`"))

(deftest test-writer ()
  (check-head (list "FOO-W `(writer foo)`"
                    (make-reference 'foo-w '(writer foo))
                    (make-reference 'foo-w 'variable))
              "[`FOO-W`][f90e] `(writer foo)`"))


(defsection @test-method-combination ()
  (my-comb method-combination))

(define-method-combination my-comb :identity-with-one-argument t
  :documentation "This is MY-COMB.")

(deftest test-method-combination ()
  (with-failure-expected ((alexandria:featurep '(:or :abcl :allegro)))
    (check-document (make-reference 'my-comb 'method-combination)
                    "<a id='x-28MGL-PAX-TEST-3AMY-COMB-20METHOD-COMBINATION-29'></a>

- [method-combination] **MY-COMB**

    This is `MY-COMB`.
")))


(deftest test-hyperspec ()
  (check-head "FIND-IF" "[`FIND-IF`][badc]")
  (check-head "LIST" "`LIST`([`0`][df43] [`1`][7def])")
  (check-head "[LIST][type]" "[`LIST`][7def]")
  (check-head "T" "`T`")
  (check-head "NIL" "`NIL`")
  (check-head "[T][]" "`T`([`0`][b743] [`1`][cb19])")
  (check-head "[T][constant]" "[`T`][b743]"))


(deftest test-clhs-section ()
  ;; "A.1" and "3.4" are section names in the CLHS.
  (check-head "A.1" "A.1")
  (check-head "`A.1`" "`A.1`")
  (check-head "CLHS A.1" "`CLHS` A.1")
  (check-head "CLHS 3.4" "`CLHS` 3.4")
  (check-head "CLHS `3.4`" "`CLHS` [`3.4`][7647]")
  (check-head "`3.4` CLHS" "[`3.4`][7647] `CLHS`")
  (check-head "[3.4][]" "[3.4][7647]")
  (check-head "[`3.4`][]" "[`3.4`][7647]")
  (check-head "[3.4][CLHS]" "[3.4][7647]")
  (check-head "[Lambda Lists][clhs]" "[Lambda Lists][7647]")
  (check-head "[03_d][clhs]" "[03\\_d][7647]"))


(deftest test-clhs-issue ()
  (check-head "ISSUE:AREF-1D" "ISSUE:AREF-1D")
  (check-head "`ISSUE:AREF-1D`" "`ISSUE:AREF-1D`")
  (check-head "CLHS ISSUE:AREF-1D" "`CLHS` ISSUE:AREF-1D")
  (check-head "ISSUE:AREF-1D CLHS" "ISSUE:AREF-1D `CLHS`")
  (check-head "CLHS `ISSUE:AREF-1D`" "`CLHS` [`ISSUE:AREF-1D`][3e36]")
  (check-head "`ISSUE:AREF-1D` CLHS" "[`ISSUE:AREF-1D`][3e36] `CLHS`")
  (check-head "[ISSUE:AREF-1D][]" "[ISSUE:AREF-1D][3e36]")
  (check-head "[`ISSUE:AREF-1D`][]" "[`ISSUE:AREF-1D`][3e36]")
  (check-head "[ISSUE:AREF-1D][CLHS]" "[ISSUE:AREF-1D][3e36]")
  (check-head "[iss009][clhs]" "[iss009][eed0]"))


(deftest test-argument ()
  (check-head "[PRINT][argument]" "`PRINT`"))


(deftest test-declaration ()
  (check-head "SAFETY" "[`SAFETY`][9f0e]")
  (check-head "SAFETY declaration" "[`SAFETY`][9f0e] declaration")
  (check-head "[safety][declaration]" "[safety][9f0e]"))


(deftest test-readtable ()
  (with-failure-expected ((alexandria:featurep :abcl))
    (check-document (named-readtables:find-readtable 'xxx-rt)
                    "<a id='x-28MGL-PAX-TEST-3A-3AXXX-RT-20READTABLE-29'></a>

- [readtable] **XXX-RT**

    ddd
"))
  (check-head (list "[XXX-RT][readtable]"
                    (named-readtables:find-readtable 'xxx-rt))
              "[`XXX-RT`][9ac2]"))


(defpackage interned-pkg-name)
(defpackage #:non-interned-pkg-name)

(deftest test-package ()
  (check-head (list "INTERNED-PKG-NAME"
                    (make-reference 'interned-pkg-name 'package))
              "[`INTERNED-PKG-NAME`][2509]")
  (check-head (list "NON-INTERNED-PKG-NAME"
                    (make-reference '#:non-interned-pkg-name 'package))
              "[`NON-INTERNED-PKG-NAME`][11d0]"))


(deftest test-asdf-system ()
  (is (find-symbol (string '#:mgl-pax/full) '#:mgl-pax-test))
  (is (null (find-symbol (string '#:mgl-pax/test) '#:mgl-pax-test)))
  (check-head (list "MGL-PAX/FULL"
                    (make-reference 'mgl-pax/full 'asdf:system))
              "[`MGL-PAX/FULL`][0785]")
  (check-head (list "MGL-PAX/TEST"
                    (make-reference "mgl-pax/test" 'asdf:system))
              "[`MGL-PAX/TEST`][4b83]"))


(mgl-pax:define-locative-alias instance class)

(deftest test-define-locative-alias ()
  (check-head (list "SECTION instance"
                    (make-reference 'section 'class)
                    (make-reference 'section 'locative))
              "[`SECTION`][aee8] instance"))


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



(deftest test-all ()
  (test-navigation)
  (test-read-locative-from-string)
  (test-read-reference-from-string)
  (test-transform-tree)
  (test-macro-arg-names)
  (test-codify)
  (test-names)
  (test-downcasing)
  (test-link)
  (test-function)
  (test-generic-function)
  (test-accessor)
  (test-reader)
  (test-writer)
  (test-macro)
  (test-symbol-macro)
  (test-method-combination)
  (test-hyperspec)
  (test-clhs-section)
  (test-clhs-issue)
  (test-argument)
  (test-declaration)
  (test-readtable)
  (test-package)
  (test-asdf-system)
  (test-define-locative-alias)
  (test-transcribe)
  (test-cl-transcript))

(defun test (&key (debug nil) (print 't) (describe *describe*))
  ;; Bind *PACKAGE* so that names of tests printed have package names,
  ;; and M-. works on them in Slime.
  (let ((*package* (find-package :common-lisp))
        (*print-duration* nil)
        (*print-compactly* t)
        (*defer-describe* t))
    (warn-on-tests-not-run ((find-package :mgl-pax-test))
      (print (try 'test-all :debug debug :print print :describe describe)))))

#+nil
(test)

#+nil
(test-all)
