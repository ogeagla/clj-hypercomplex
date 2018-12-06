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
     [complex quaternion octonion sedenion
      pathion n-hypercomplex power-of?]]
    [mikera.image.core :as imgz]
    [mikera.image.colours :as imgz-color])
  (:import (hypercomplex.core Complex2Apache Complex2 Construction)
           (java.awt.image BufferedImage WritableRaster)
           (java.awt Graphics)))

(def MAX-TRIES 10000)
(def SEED 12345678987654321)


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
        (fn [coeffs]
          (n-hypercomplex coeffs :apache))
        (gen/such-that
          (fn [coeffs]
            (and
              (power-of? (count coeffs) 2)))
          (gen/vector-distinct
            (s/gen ::coeff)
            {:min-elements 2
             :max-elements 256})
          MAX-TRIES)))))

(s/def ::hypercomplex-plain
  (s/with-gen
    #(and (instance? Construction %)
          (satisfies? Nion %)
          (satisfies? NionOps %))
    (fn []
      (gen/fmap
        (fn [coeffs]
          (n-hypercomplex coeffs :plain))
        (gen/such-that
          (fn [coeffs]
            (and
              (power-of? (count coeffs) 2)))
          (gen/vector-distinct
            (s/gen ::coeff)
            {:min-elements 2
             :max-elements 256})
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



;;;; Generate fractals:

(def XY-RANGE [-2.0 0.5])
(def X-RANGE [-0.4 0.2])
(def Y-RANGE [-1.1 -0.5])
(def MAX-CONV-ITERS 64)


(defn regular-number? [n domain]
  (and
    (<= (first domain) n (second domain))
    (not
      (or
        (= n ##-Inf)
        (= n ##Inf)
        (= n ##NaN)))))

(s/def ::ycoeff
  (s/with-gen
    #(and
       (number? %)
       (regular-number? % Y-RANGE))
    (fn []
      (gen/double*
        {:infinite? false
         :NaN?      false
         :min       (first Y-RANGE)
         :max       (second Y-RANGE)}))))

(s/def ::xcoeff
  (s/with-gen
    #(and
       (number? %)
       (regular-number? % X-RANGE))
    (fn []
      (gen/double*
        {:infinite? false
         :NaN?      false
         :min       (first X-RANGE)
         :max       (second X-RANGE)}))))

(s/def ::coeff
  (s/with-gen
    #(and
       (number? %)
       (regular-number? % XY-RANGE))
    (fn []
      (gen/double*
        {:infinite? false
         :NaN?      false
         :min       (first XY-RANGE)
         :max       (second XY-RANGE)}))))

(s/def ::2d-domain
  (s/with-gen
    (fn [d]
      (= 2 (count d)))
    (fn []
      (gen/tuple
        (s/gen ::xcoeff)
        (s/gen ::ycoeff)))))

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


(s/def ::intensity-plain
  (s/with-gen
    (fn [d]
      (and (= 3 (count d))
           (let [[a b insy] d]
             (and
               (<= 0 insy MAX-CONV-ITERS)))))
    (fn []
      (gen/fmap
        (fn [[a b]]
          [a b
           (compute-iters a b :plain MAX-CONV-ITERS)])
        (s/gen ::2d-domain)))))

;interesting in the sense that it's not 0 and not max-iters,
;so some value that would render something interesting
(s/def ::interesting-intensity-plain
  (s/with-gen
    (fn [i]
      (and (< 0 (nth i 2))
           (> MAX-CONV-ITERS (nth i 2))))
    (fn []
      (gen/such-that
        (fn [i]
          (and (< 0 (nth i 2))
               (> MAX-CONV-ITERS (nth i 2))))
        (s/gen ::intensity-plain)
        MAX-TRIES))))

(s/def ::interesting-intensities-plain
  (s/with-gen
    (fn [i]
      (coll? i))
    (fn []
      (gen/vector-distinct
        (s/gen ::interesting-intensity-plain)
        {:num-elements 100}))))


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

(ct/defspec
  creates-plain-intensity
  {:num-tests 10 :seed SEED}
  (prop/for-all [i (s/gen ::interesting-intensity-plain)]
                (is (s/valid? ::interesting-intensity-plain i))))

(ct/defspec
  creates-plain-intensities
  {:num-tests 10 :seed SEED}
  (prop/for-all
    [its (s/gen ::interesting-intensities-plain)]
    ;(println "Intensities count: " (count its))
    (is (s/valid? ::interesting-intensities-plain its))))


;(run-tests 'hypercomplex.gen-test)
