(ns clj-cayley-dickson.core-test
  (:require [clojure.test :refer :all]
            [clj-cayley-dickson.core :refer :all]
            [clojure.pprint :refer :all]))


(deftest cayley-dickson-constructions-test
  (testing "Commutivity holds for order <= 2"
    (is
      (= (times (complex {:a 1 :b 2})
                (complex {:a 3 :b 4}))
         (times (complex {:a 3 :b 4})
                (complex {:a 1 :b 2}))))
    (is
      (not
        (= (times (quaternion {:a 1 :b 2 :c 3 :d 4})
                  (quaternion {:a 8 :b 7 :c 6 :d 5}))
           (times (quaternion {:a 8 :b 7 :c 6 :d 5})
                  (quaternion {:a 1 :b 2 :c 3 :d 4}))))))
  (testing "Associativity holds for order <= 4"
    (is
      (= (times
           (complex {:a 5 :b 6})
           (times (complex {:a 1 :b 2})
                  (complex {:a 3 :b 4})))
         (times
           (times
             (complex {:a 5 :b 6})
             (complex {:a 1 :b 2}))
           (complex {:a 3 :b 4}))))
    (is
      (= (times
           (quaternion {:a 1 :b 2 :c 3 :d 4})
           (times (quaternion {:a 8 :b 7 :c 6 :d 5})
                  (quaternion {:a 9 :b 10 :c 11 :d 12})))
         (times
           (times
             (quaternion {:a 1 :b 2 :c 3 :d 4})
             (quaternion {:a 8 :b 7 :c 6 :d 5}))
           (quaternion {:a 9 :b 10 :c 11 :d 12}))))
    (is
      (not
        (= (times
             (octonion {:a 0 :b 2 :c 3 :d 4 :e 4 :f 3 :g 2 :h 1})
             (times (octonion {:a 8 :b 7 :c 6 :d 5 :e 5 :f 6 :g 7 :h 8})
                    (octonion {:a 9 :b 10 :c 11 :d 13 :e 12 :f 11 :g 10 :h 9})))
           (times
             (times
               (octonion {:a 0 :b 2 :c 3 :d 4 :e 4 :f 3 :g 2 :h 1})
               (octonion {:a 8 :b 7 :c 6 :d 5 :e 5 :f 6 :g 7 :h 8}))
             (octonion {:a 9 :b 10 :c 11 :d 13 :e 12 :f 11 :g 10 :h 9})))))))



;(println "order 2:" (complex2 1 1))
;
;(println
;  "times order 2:")
;
;(pprint
;  (times (complex2 1 1)
;         (complex2 1 1)))
;
;(println
;  "order 4:"
;  (complex4 1 1 1 1))
;
;(println
;  "times order 4:")
;
;(pprint
;  (times (complex4 1 1 1 1)
;         (complex4 1 1 1 1)))
;
;(println "order 8: " (complex8 1 1 1 1 1 1 1 1))
;
;(println
;  "times order 8:")
;(pprint
;  (times (complex8 1 1 1 1 1 1 1 1)
;         (complex8 1 1 1 1 1 1 1 1)))
;
;(println "inv: " (inv (complex4 1 1 1 1)))
;
;(println "norm: " (norm (complex4 1 1 1 1)))
;
;(println "mag: " (mag (complex4 1 1 1 1)))
;
;(println "scale: " (scale (complex4 1 1 1 1) 0.5))