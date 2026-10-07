(in-package :mgl-pax-test)

(deftest test-markdown ()
  (test-md-code)
  (test-md-emph)
  (test-md-strong)
  (test-add-to-forest)
  (test-possible-md-start-position)
  (test-parse-md-after-tree)
  (test-parse-markdown))

(deftest test-md-code ()
  (is (equal (pax::md-code "") ""))
  (is (equal (pax::md-code "x") "`x`"))
  (is (equal (pax::md-code "`") "`` ` ``"))
  (is (equal (pax::md-code "x`") "``x` ``"))
  (is (equal (pax::md-code "`y") "`` `y``"))
  (is (equal (pax::md-code "x`y") "``x`y``"))
  (is (equal (pax::md-code "``") "``` `` ```")))

(deftest test-md-emph ()
  (is (equal (pax::md-emph "") ""))
  (is (equal (pax::md-emph "x") "*x*"))
  (is (equal (pax::md-emph "*x*") "*\\*x\\**"))
  (is (equal (pax::md-emph "x*y") "*x\\*y*"))
  (is (equal (pax::md-emph "x\\*y") "*x\\\\*y*"))
  (is (equal (pax::md-emph "x\\*y" nil) "*x\\*y*")))

(deftest test-md-strong ()
  (is (equal (pax::md-strong "") ""))
  (is (equal (pax::md-strong "x") "**x**"))
  (is (equal (pax::md-strong "*x*") "**\\*x\\***"))
  (is (equal (pax::md-strong "x*y") "**x\\*y**"))
  (is (equal (pax::md-strong "x\\*y") "**x\\\\*y**"))
  (is (equal (pax::md-strong "x\\*y" nil) "**x\\*y**")))

(deftest test-add-to-forest ()
  (is (equal (mgl-pax::add-to-forest
              (copy-tree '((:bullet-list (:list-item (:plain "1")))))
              (copy-tree '((:bullet-list (:list-item (:plain "2")))))
              t 0)
             '((:bullet-list
                (:list-item (:plain "1"))
                (:list-item (:plain "2")))))))

(deftest test-possible-md-start-position ()
  (is (eql 0 (pax::possible-md-start-position "    x")))
  (is (eql 0 (pax::possible-md-start-position "*x*")))
  (is (eql 4 (pax::possible-md-start-position "x y `z`")))
  (is (eql 4 (pax::possible-md-start-position "x y a_z_")))
  (is (eql 7 (pax::possible-md-start-position "x y - a")))
  (is (eql 7 (pax::possible-md-start-position "x y # a")))
  (is (eql 2 (pax::possible-md-start-position "x y #\\@")))
  (is (eql 7 (pax::possible-md-start-position (format nil "x y~%- a"))))
  (is (eql 3 (pax::possible-md-start-position (format nil "a~%~%b"))))
  (is (eql 7 (pax::possible-md-start-position
              (format nil "a b c~%~%```~%d~%```")))))

(deftest test-parse-md-after-tree ()
  (is (equal (pax::parse-md-after-tree
              (copy-tree '((:plain "x"))) "y" :paragraphp nil)
             '((:plain "xy"))))
  (is (equal (pax::parse-md-after-tree
              (copy-tree '((:plain "x"))) "y" :paragraphp t)
             '((:paragraph "x") (:plain "y"))))
  (is (equal (pax::parse-md-after-tree
              (copy-tree '((:bullet-list (:list-item (:plain "x")))))
              "y" :paragraphp nil)
             '((:bullet-list (:list-item (:plain "xy"))))))
  (is (equal (pax::parse-md-after-tree
              (copy-tree '((:bullet-list (:list-item (:plain "x")))))
              "    y" :paragraphp t)
             '((:bullet-list (:list-item (:paragraph "x") (:paragraph "y"))))))
  (is (equal (pax::parse-md-after-tree
              (copy-tree '((:bullet-list (:list-item (:plain "x")))))
              (format nil "y~%~%z") :paragraphp nil)
             '((:bullet-list (:list-item (:plain "xy")))
               (:plain "z"))))
  (is (equal (pax::parse-md-after-tree
              (copy-tree '((:bullet-list (:list-item (:plain "x")))))
              (format nil "    y~%~%z") :paragraphp t)
             '((:bullet-list (:list-item (:paragraph "x") (:paragraph "y")))
               (:plain "z")))))

(deftest test-parse-markdown ()
  (is (equal (pax::parse-markdown "+ x")
             '((:bullet-list (:list-item (:plain "x"))))))
  (is (equal (pax::parse-markdown "a >b #_x_")
             '((:plain "a" " " ">b" " " "#" (:emph "x")))))
  (is (equal (pax::parse-markdown "a
===
")
             '((:heading :level 1 :contents ("a")))))
  (is (equal (pax::parse-markdown "a
---
")
             '((:heading :level 2 :contents ("a"))))))
