(ns clj-cayley-dickson.core-test
  (:require [clojure.test :refer :all]
            [clj-cayley-dickson.core :refer :all]
            [clojure.pprint :refer :all]))


(println "order 2:" (complex2 1 1))

(println
  "times order 2:")

(pprint
  (times (complex2 1 1)
         (complex2 1 1)))

(println
  "order 4:"
  (complex4 1 1 1 1))

(println
  "times order 4:")

(pprint
  (times (complex4 1 1 1 1)
         (complex4 1 1 1 1)))

(println "order 8: " (complex8 1 1 1 1 1 1 1 1))

(println
  "times order 8:")
(pprint
  (times (complex8 1 1 1 1 1 1 1 1)
         (complex8 1 1 1 1 1 1 1 1)))

(println "inv: " (inv (complex4 1 1 1 1)))

(println "norm: " (norm (complex4 1 1 1 1)))

(println "mag: " (mag (complex4 1 1 1 1)))

(println "scale: " (scale (complex4 1 1 1 1) 0.5))

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
