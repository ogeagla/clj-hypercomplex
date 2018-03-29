(ns og.clj-cayley-dickson.core-test
  (:require [clojure.test :refer :all]
            [og.clj-cayley-dickson.core :refer :all]
            [clojure.pprint :refer :all]))

(defn fuzzy= [tolerance x y]
  (let [diff (Math/abs (- x y))]
    (< diff tolerance)))

(defn quatfuzzy= [tolerance q1 q2]
  (and (fuzzy= tolerance
               (:a (:a q1))
               (:a (:a q2)))
       (fuzzy= tolerance
               (:a (:b q1))
               (:a (:b q2)))
       (fuzzy= tolerance
               (:b (:a q1))
               (:b (:a q2)))
       (fuzzy= tolerance
               (:b (:b q1))
               (:b (:b q2)))))

(deftest cayley-dickson-constructions-test

  (testing "Rotation"
    (is (= (quaternion {:a 1.5 :b 1.5 :c 1.5 :d 1.5})
           (rot (quaternion {:a 1.5 :b 1.5 :c 1.5 :d 1.5})
                (quaternion {:a 2.5 :b 1.5 :c 1.5 :d 1.5}))))
    (is (= (quaternion {:a 0.0 :b 1.5 :c 1.5 :d 1.5})
           (rot (quaternion {:a 0.0 :b 1.5 :c 1.5 :d 1.5})
                (quaternion {:a 2.5 :b 1.5 :c 1.5 :d 1.5}))))
    (is (quatfuzzy=
          0.00000001
          (quaternion {:a 0.0
                       :b (/
                            (+ 10.0
                               (* 4.0
                                  (Math/sqrt 3.0)))
                            8.0)
                       :c (/
                            (+ 1.0
                               (* 2.0
                                  (Math/sqrt 3.0)))
                            8.0)
                       :d (/
                            (- 14.0
                               (* 3.0
                                  (Math/sqrt 3.0)))
                            8.0)})
          (rot (quaternion {:a 0.0
                            :b 1.0
                            :c -1.0
                            :d 2.0})
               (quaternion {:a (/ (Math/sqrt 3.0)
                                  2.0)
                            :b 0.0
                            :c 0.25
                            :d (/ (Math/sqrt 3.0)
                                  4.0)})))))

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
           (norm (quaternion {:a 1 :b 1 :c 1 :d 1}))))
    (is (= 32
           (norm (trigdunion {:a 1 :b 1 :c 1 :d 1 :e 1 :f 1 :g 1 :h 1
                              :i 1 :j 1 :k 1 :l 1 :m 1 :n 1 :o 1 :p 1
                              :q 1 :r 1 :s 1 :t 1 :u 1 :v 1 :w 1 :x 1
                              :y 1 :z 1 :aa 1 :bb 1 :cc 1 :dd 1 :ee 1 :ff 1})))))

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
