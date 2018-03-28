(ns clj-cayley-dickson.core-test
  (:require [clojure.test :refer :all]
            [clj-cayley-dickson.core :refer :all]))


(deftest cayley-dickson-constructions-test
  (testing "Commutivity holds for order <= 2"
    (is
      (= (times (complex2 1 2)
                (complex2 3 4))
         (times (complex2 3 4)
                (complex2 1 2))))
    (is
      (not
        (= (times (complex4 1 2 3 4)
                  (complex4 8 7 6 5))
           (times (complex4 8 7 6 5)
                  (complex4 1 2 3 4))))))
  (testing "Associativity holds for order <= 4"
    (is
      (= (times
             (complex2 5 6)
             (times (complex2 1 2)
                    (complex2 3 4)))
           (times
             (times
               (complex2 5 6)
               (complex2 1 2))
             (complex2 3 4))))
    (is
      (= (times
           (complex4 1 2 3 4)
           (times (complex4 8 7 6 5)
                  (complex4 9 10 11 12)))
         (times
           (times
             (complex4 1 2 3 4)
             (complex4 8 7 6 5))
           (complex4 9 10 11 12))))
    (is
      (not
        (= (times
             (complex8 0 2 3 4 4 3 2 1)
             (times (complex8 8 7 6 5 5 6 7 8)
                    (complex8 9 10 11 13 12 11 10 9)))
           (times
             (times
               (complex8 0 2 3 4 4 3 2 1)
               (complex8 8 7 6 5 5 6 7 8))
             (complex8 9 10 11 13 12 11 10 9)))))))
