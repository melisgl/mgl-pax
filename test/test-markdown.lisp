(in-package :mgl-pax-test)

(deftest test-markdown ()
  (test-md-code))

(deftest test-md-code ()
  (is (equal (pax::md-code "") ""))
  (is (equal (pax::md-code "x") "`x`"))
  (is (equal (pax::md-code "`") "`` ` ``"))
  (is (equal (pax::md-code "x`") "``x` ``"))
  (is (equal (pax::md-code "`y") "`` `y``"))
  (is (equal (pax::md-code "x`y") "``x`y``"))
  (is (equal (pax::md-code "``") "``` `` ```")))
