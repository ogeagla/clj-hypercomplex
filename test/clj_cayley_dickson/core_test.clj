(ns clj-cayley-dickson.core-test
  (:require [clojure.test :refer :all]
            [clj-cayley-dickson.core :refer :all]
            [clojure.pprint :refer :all]))


(deftest cayley-dickson-constructions-test

  (testing "Scale"
    (is (= (complex {:a 0.1 :b 0.1})
           (scale (complex {:a 2 :b 2}) 0.05)))
    (is (= (quaternion {:a 1.5 :b 1.5 :c 1.5 :d 1.5})
           (scale (quaternion {:a 1 :b 1 :c 1 :d 1}) 1.5))))

  (testing "Mag"
    (is (= (Math/sqrt 2.0)
           (mag (complex {:a 1 :b 1}))))
    (is (= 2.0
           (mag (quaternion {:a 1 :b 1 :c 1 :d 1})))))

  (testing "Norm"
    (is (= 2
           (norm (complex {:a 1 :b 1}))))
    (is (= 4
           (norm (quaternion {:a 1 :b 1 :c 1 :d 1})))))

  (testing "Inverse"
    (is (= (complex {:a 1.0 :b 0.0})
           (inv (complex {:a 1 :b 0}))))
    (is (= (complex {:a 0.1 :b 0.0})
           (inv (complex {:a 10 :b 0}))))
    (is (= (complex {:a 0.5 :b -0.5})
           (inv (complex {:a 1 :b 1}))))
    (is (= (quaternion {:a 0.25 :b -0.25 :c -0.25 :d -0.25})
           (inv (quaternion {:a 1 :b 1 :c 1 :d 1})))))

  (testing "Commutivity holds for order <= 2"
    (is (= (times (complex {:a 1 :b 2})
                  (complex {:a 3 :b 4}))
           (times (complex {:a 3 :b 4})
                  (complex {:a 1 :b 2}))))
    (is (not
          (= (times (quaternion {:a 1 :b 2 :c 3 :d 4})
                    (quaternion {:a 8 :b 7 :c 6 :d 5}))
             (times (quaternion {:a 8 :b 7 :c 6 :d 5})
                    (quaternion {:a 1 :b 2 :c 3 :d 4}))))))

  (testing "Associativity holds for order <= 4"
    (is (= (times
             (complex {:a 5 :b 6})
             (times (complex {:a 1 :b 2})
                    (complex {:a 3 :b 4})))
           (times
             (times
               (complex {:a 5 :b 6})
               (complex {:a 1 :b 2}))
             (complex {:a 3 :b 4}))))
    (is (= (times
             (quaternion {:a 1 :b 2 :c 3 :d 4})
             (times (quaternion {:a 8 :b 7 :c 6 :d 5})
                    (quaternion {:a 9 :b 10 :c 11 :d 12})))
           (times
             (times
               (quaternion {:a 1 :b 2 :c 3 :d 4})
               (quaternion {:a 8 :b 7 :c 6 :d 5}))
             (quaternion {:a 9 :b 10 :c 11 :d 12}))))
    (is (not
          (= (times
               (octonion {:a 0 :b 2 :c 3 :d 4 :e 4 :f 3 :g 2 :h 1})
               (times (octonion {:a 8 :b 7 :c 6 :d 5 :e 5 :f 6 :g 7 :h 8})
                      (octonion {:a 9 :b 10 :c 11 :d 13 :e 12 :f 11 :g 10 :h 9})))
             (times
               (times
                 (octonion {:a 0 :b 2 :c 3 :d 4 :e 4 :f 3 :g 2 :h 1})
                 (octonion {:a 8 :b 7 :c 6 :d 5 :e 5 :f 6 :g 7 :h 8}))
               (octonion {:a 9 :b 10 :c 11 :d 13 :e 12 :f 11 :g 10 :h 9})))))))
