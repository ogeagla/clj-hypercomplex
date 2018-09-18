(ns og.cayley-dickson.load-test
  (:require [clojure.test :refer :all]
            [og.cayley-dickson.core :refer :all]
            [og.cayley-dickson.construction :refer
             [complex quaternion octonion sedenion pathion]])
  (:import (java.util Random)))

(defn rand-w-seed [seed]
  (let [^Random r (Random.)]
    (.setSeed r seed)
    r))

(def ^Random RANDOM (rand-w-seed 7845639217))

(defn RANDOM-STRING [len]
  (->>
    #(char
       (+
         (.nextInt RANDOM 26)
         65))
    (repeatedly)
    (take len)
    (apply str)))

(defn RANDOM-FLOAT
  "Random float between -5000.0 and 5000.0"
  []
  (-> (.nextFloat RANDOM)
      (- 0.5)
      (* 10000.0)))

(defn do-once [impl]

  (rot (quaternion {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :c (RANDOM-FLOAT) :d (RANDOM-FLOAT) :impl impl})
       (quaternion {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :c (RANDOM-FLOAT) :d (RANDOM-FLOAT) :impl impl}))

  (rot (quaternion {:a    (RANDOM-FLOAT)
                    :b    (RANDOM-FLOAT)
                    :c    (RANDOM-FLOAT)
                    :d    (RANDOM-FLOAT)
                    :impl impl})
       (quaternion {:a    (/ (RANDOM-FLOAT)
                             2.0)
                    :b    (RANDOM-FLOAT)
                    :c    (RANDOM-FLOAT)
                    :d    (/ (RANDOM-FLOAT)
                             4.0)
                    :impl impl}))

  (complex {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :impl impl})

  (scale (quaternion {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :c (RANDOM-FLOAT) :d (RANDOM-FLOAT) :impl impl}) 1.5)

  (mag (complex {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :impl impl}))

  (mag (quaternion {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :c (RANDOM-FLOAT) :d (RANDOM-FLOAT) :impl impl}))

  (norm (complex {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :impl impl}))

  (norm (quaternion {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :c (RANDOM-FLOAT) :d (RANDOM-FLOAT) :impl impl}))

  (norm (pathion {:a    (RANDOM-FLOAT) :b (RANDOM-FLOAT) :c (RANDOM-FLOAT) :d (RANDOM-FLOAT) :e (RANDOM-FLOAT) :f (RANDOM-FLOAT) :g (RANDOM-FLOAT) :h (RANDOM-FLOAT)
                  :i    (RANDOM-FLOAT) :j (RANDOM-FLOAT) :k (RANDOM-FLOAT) :l (RANDOM-FLOAT) :m (RANDOM-FLOAT) :n (RANDOM-FLOAT) :o (RANDOM-FLOAT) :p (RANDOM-FLOAT)
                  :q    (RANDOM-FLOAT) :r (RANDOM-FLOAT) :s (RANDOM-FLOAT) :t (RANDOM-FLOAT) :u (RANDOM-FLOAT) :v (RANDOM-FLOAT) :w (RANDOM-FLOAT) :x (RANDOM-FLOAT)
                  :y    (RANDOM-FLOAT) :z (RANDOM-FLOAT) :aa (RANDOM-FLOAT) :bb (RANDOM-FLOAT) :cc (RANDOM-FLOAT) :dd (RANDOM-FLOAT) :ee (RANDOM-FLOAT) :ff (RANDOM-FLOAT)
                  :impl impl}))

  (inv (complex {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :impl impl}))
  (inv (quaternion {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :c (RANDOM-FLOAT) :d (RANDOM-FLOAT) :impl impl}))

  (times
    (complex {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :impl impl})
    (times (complex {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :impl impl})
           (complex {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :impl impl})))

  (times
    (times
      (quaternion {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :c (RANDOM-FLOAT) :d (RANDOM-FLOAT) :impl impl})
      (quaternion {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :c (RANDOM-FLOAT) :d (RANDOM-FLOAT) :impl impl}))
    (quaternion {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :c (RANDOM-FLOAT) :d (RANDOM-FLOAT) :impl impl}))

  (times
    (times
      (octonion {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :c (RANDOM-FLOAT) :d (RANDOM-FLOAT) :e (RANDOM-FLOAT) :f (RANDOM-FLOAT) :g (RANDOM-FLOAT) :h (RANDOM-FLOAT) :impl impl})
      (octonion {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :c (RANDOM-FLOAT) :d (RANDOM-FLOAT) :e (RANDOM-FLOAT) :f (RANDOM-FLOAT) :g (RANDOM-FLOAT) :h (RANDOM-FLOAT) :impl impl}))
    (octonion {:a (RANDOM-FLOAT) :b (RANDOM-FLOAT) :c (RANDOM-FLOAT) :d (RANDOM-FLOAT) :e (RANDOM-FLOAT) :f (RANDOM-FLOAT) :g (RANDOM-FLOAT) :h (RANDOM-FLOAT) :impl impl})))

(deftest load-test
  (testing "Load test plain impl"
    (println "Running load test using :plain impl:")
    (time
      (dotimes [i 100000]
        (do-once :plain)))
    )
  (testing "Load test apache impl"
    (println "Running load test using :apache impl:")
    (time
      (dotimes [i 100000]
        (do-once :apache)))
    ))