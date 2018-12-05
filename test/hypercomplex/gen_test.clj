(ns hypercomplex.gen-test
  (:require
    [clojure.test :refer :all]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as stest]
    [clojure.test.check.clojure-test :as ct]
    [clojure.test.check.properties :as prop]
    [clojure.spec.gen.alpha :as gen]
    [clojure.test.check.generators :as g]
    [hypercomplex.core :refer :all]
    [hypercomplex.cayley-dickson-construction :refer
     [complex quaternion octonion sedenion pathion n-hypercomplex power-of?]])
  (:import (hypercomplex.core Complex2Apache Complex2 Construction)))

(def MAX-TRIES 10000)
(def SEED 12345678987654321)

(defn regular-number? [n]
  (not
    (or
      (= n ##-Inf)
      (= n ##Inf)
      (= n ##NaN))))

(s/def ::coeff
  (s/with-gen
    #(and
       (number? %)
       (regular-number? %))
    (fn []
      (gen/fmap
        (fn [c]
          c)
        (gen/such-that
          (fn [c]
            (regular-number? c))
          (gen/double)
          MAX-TRIES)))))

(s/def ::complex2-apache
  (s/with-gen
    #(and (instance? Complex2Apache %)
          (satisfies? Nion %)
          (satisfies? NionOps %))
    (fn []
      (gen/fmap
        (fn [[a b]]
          (init-complex2 a b :apache))
        (gen/tuple
          (s/gen ::coeff)
          (s/gen ::coeff))))))

(s/def ::complex2-plain
  (s/with-gen
    #(and (instance? Complex2 %)
          (satisfies? Nion %)
          (satisfies? NionOps %))
    (fn []
      (gen/fmap
        (fn [[a b]]
          (init-complex2 a b :plain))
        (gen/tuple
          (s/gen ::coeff)
          (s/gen ::coeff))))))

(s/def ::hypercomplex-apache
  (s/with-gen
    #(and (instance? Construction %)
          (satisfies? Nion %)
          (satisfies? NionOps %))
    (fn []
      (gen/fmap
        (fn [[coeffs]]
          (n-hypercomplex coeffs :apache))
        (gen/such-that
          (fn [[coeffs]]
            (and
              (< 1 (count coeffs))
              (power-of? (count coeffs) 2)))
          (gen/tuple
            (gen/list (s/gen ::coeff)))
          MAX-TRIES)))))

(s/def ::hypercomplex-plain
  (s/with-gen
    #(and (instance? Construction %)
          (satisfies? Nion %)
          (satisfies? NionOps %))
    (fn []
      (gen/fmap
        (fn [[coeffs]]
          (n-hypercomplex coeffs :plain))
        (gen/such-that
          (fn [[coeffs]]
            (and
              (< 1 (count coeffs))
              (power-of? (count coeffs) 2)))
          (gen/tuple
            (gen/list (s/gen ::coeff)))
          MAX-TRIES)))))

(s/def ::construction-apache
  (s/with-gen
    #(and (instance? Construction %)
          (satisfies? Nion %)
          (satisfies? NionOps %))
    (fn []
      (gen/fmap
        (fn [[a b]]
          (init-construction a b))
        (gen/such-that
          (fn [[a b]]
            (eq-order? a b))
          (gen/tuple
            (s/gen ::hypercomplex-apache)
            (s/gen ::hypercomplex-apache))
          MAX-TRIES)))))

(s/def ::construction-plain
  (s/with-gen
    #(and (instance? Construction %)
          (satisfies? Nion %)
          (satisfies? NionOps %))
    (fn []
      (gen/fmap
        (fn [[a b]]
          (init-construction a b))
        (gen/such-that
          (fn [[a b]]
            (eq-order? a b))
          (gen/tuple
            (s/gen ::hypercomplex-plain)
            (s/gen ::hypercomplex-plain))
          MAX-TRIES)))))

(s/def ::2d-domain
  (s/with-gen
    (fn [d]
      (and (= 2 (count d))
           (let [[a b] d]
             (and
               (<= 0 a 200)
               (<= 0 b 200)))))

    (fn []
      (gen/fmap
        (fn [[a b]]
          [a b])
        (gen/such-that
          (fn [[a b]]
            (and
              (<= 0 a 200)
              (<= 0 b 200)))
          (gen/tuple
            (s/gen ::coeff)
            (s/gen ::coeff))
          MAX-TRIES)))))

(defn compute-iters [p q impl max-iterations]
  (let [c (complex {:a p :b q :impl impl})]
    (loop [z          c
           iterations 0]
      ;(println "hc Mandelbrot z: " z)
      (if (or (> (mag z) 2.0)
              (> iterations max-iterations))
        (if (= 0 iterations)
          0
          (dec iterations))
        (recur
          (plus
            c
            (times z z))
          (inc iterations))))))


(s/def ::intensity-apache
  (s/with-gen
    (fn [d]
      (and (= 3 (count d))
           (let [[a b insy] d]
             (and
               (<= 0 insy 200)))))

    (fn []
      (gen/fmap
        (fn [[a b]]
          [a b
            (compute-iters a b :plain 200)])
        (s/gen ::2d-domain)))))

;interesting in the sense that it's not 0 and not max-iters,
;so some value that would render something interesting
(s/def ::interesting-intensity-apache
  (s/with-gen
    (fn [i]
      (and (< 0 (nth i 2))
           (> 200 (nth i 2))))

    (fn []
      (gen/fmap
        (fn [[i]]
          i)
        (gen/such-that
          (fn [[i]]
            (and (< 0 (nth i 2))
                 (> 200 (nth i 2))))
          (gen/tuple
            (s/gen ::intensity-apache))
          MAX-TRIES)))))

(ct/defspec
  creates-intensity-apache
  {:num-tests 100 :seed SEED}
  (prop/for-all [i (s/gen ::interesting-intensity-apache)]
                (println "intensity: " i)
                (is (s/valid? ::interesting-intensity-apache i))))

(ct/defspec
  creates-apache2
  {:num-tests 10 :seed SEED}
  (prop/for-all [c2a (s/gen ::complex2-apache)]
                (is (s/valid? ::complex2-apache c2a))))

(ct/defspec
  creates-plain2
  {:num-tests 10 :seed SEED}
  (prop/for-all [c2p (s/gen ::complex2-plain)]
                (is (s/valid? ::complex2-plain c2p))))

(ct/defspec
  creates-apache-hypercomplex
  {:num-tests 10 :seed SEED}
  (prop/for-all [ha (s/gen ::hypercomplex-apache)]
                (is (s/valid? ::hypercomplex-apache ha))))
(ct/defspec
  creates-plain-hypercomplex
  {:num-tests 10 :seed SEED}
  (prop/for-all [ha (s/gen ::hypercomplex-plain)]
                (is (s/valid? ::hypercomplex-plain ha))))

(ct/defspec
  creates-apache-hypercomplex-construction
  {:num-tests 10 :seed SEED}
  (prop/for-all [ca (s/gen ::construction-apache)]
                ;(println ca)
                (is (s/valid? ::construction-apache ca))))

(ct/defspec
  creates-plain-hypercomplex-construction
  {:num-tests 10 :seed SEED}
  (prop/for-all [ca (s/gen ::construction-plain)]
                ;(println ca)
                (is (s/valid? ::construction-plain ca))))


;(run-tests 'hypercomplex.gen-test)
