(ns hypercomplex.alpha.fractal-spec
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [hypercomplex.alpha.fractal-iters :as f]
            [hypercomplex.core :refer :all]
            [hypercomplex.cayley-dickson-construction :refer
             [complex quaternion octonion sedenion
              pathion n-hypercomplex power-of?]]))


;;TODO major perf issues doing derefs millions of times by different threads...

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(def MAX-TRIES 10000)
(def SCALE 1000000)

(def XY-RANGE* (atom [-2.0 0.5]))
(def X-RANGE* (atom [-0.4 0.2]))
(def Y-RANGE* (atom [-1.1 -0.5]))
(def MAX-CONV-ITERS* (atom 64))
(def JULIA-COEFF-PLAIN* (atom (complex {:a -0.1 :b 0.7 :impl :plain})))


(defn- regular-number? [n domain]
  (and
    (<= (first domain) n (second domain))
    (not
      (or
        (= n ##-Inf)
        (= n ##Inf)
        (= n ##NaN)))))


(defn i->x [i imax range]
  (let [dr (apply - (reverse range))
        ri (/ i imax)]
    (+
      (first range)
      (* ri dr))))

;This doesn't sample uniformly at all
(s/def ::int-in
  (s/int-in 0 (inc SCALE)))


(s/def ::ycoeff
  (s/with-gen
    #(and
       (number? %)
       (regular-number? % @Y-RANGE*))
    (fn []
      (gen/fmap
        (fn [i]
          (i->x (rand-int (inc SCALE)) SCALE @Y-RANGE*))
        (gen/int)))))

(s/def ::xcoeff
  (s/with-gen
    #(and
       (number? %)
       (regular-number? % @X-RANGE*))
    (fn []
      (gen/fmap
        (fn [i]
          (i->x (rand-int (inc SCALE)) SCALE @X-RANGE*))
        (gen/int)))))

(s/def ::coeff
  (s/with-gen
    #(and
       (number? %)
       (regular-number? % @XY-RANGE*))
    (fn []
      (gen/fmap
        (fn [i]
          (i->x (rand-int SCALE) (inc SCALE) @XY-RANGE*))
        (gen/int)))))

(s/def ::2d-domain
  (s/with-gen
    (fn [d]
      (= 2 (count d)))
    (fn []
      (gen/tuple
        (s/gen ::xcoeff)
        (s/gen ::ycoeff)))))

(s/def ::intensity-plain-julia
  (s/with-gen
    (fn [d]
      (and (= 3 (count d))
           (let [[a b insy] d]
             (and
               (<= 0 insy @MAX-CONV-ITERS*)))))
    (fn []
      (gen/fmap
        (fn [[a b]]
          [a b
           (f/compute-iters-julia a b :plain @MAX-CONV-ITERS* @JULIA-COEFF-PLAIN*)])
        (s/gen ::2d-domain)))))

;interesting in the sense that it's not 0 and not max-iters,
;so some value that would render something interesting
(s/def ::interesting-intensity-plain-julia
  (s/with-gen
    (fn [i]
      (and (< 0 (nth i 2))
           #_(> @MAX-CONV-ITERS* (nth i 2))))
    (fn []
      (gen/such-that
        (fn [i]
          (and (< 0 (nth i 2))
               #_(> @MAX-CONV-ITERS* (nth i 2))))
        (s/gen ::intensity-plain-julia)
        MAX-TRIES))))

(s/def ::interesting-intensities-plain-julia
  (s/with-gen
    (fn [i]
      (coll? i))
    (fn []
      (gen/vector-distinct
        (s/gen ::interesting-intensity-plain-julia)
        {:num-elements 100}))))

(s/def ::intensity-plain-mandel
  (s/with-gen
    (fn [d]
      (and (= 3 (count d))
           (let [[a b insy] d]
             (and
               (<= 0 insy @MAX-CONV-ITERS*)))))
    (fn []
      (gen/fmap
        (fn [[a b]]
          [a b
           (f/compute-iters-mandel a b :plain @MAX-CONV-ITERS*)])
        (s/gen ::2d-domain)))))

;interesting in the sense that it's not 0 and not max-iters,
;so some value that would render something interesting
(s/def ::interesting-intensity-plain-mandel
  (s/with-gen
    (fn [i]
      (and (< 0 (nth i 2))
           #_(> @MAX-CONV-ITERS* (nth i 2))))
    (fn []
      (gen/such-that
        (fn [i]
          (and (< 0 (nth i 2))
               #_(> @MAX-CONV-ITERS* (nth i 2))))
        (s/gen ::intensity-plain-mandel)
        MAX-TRIES))))

(s/def ::interesting-intensities-plain-mandel
  (s/with-gen
    (fn [i]
      (coll? i))
    (fn []
      (gen/vector-distinct
        (s/gen ::interesting-intensity-plain-mandel)
        {:num-elements 100}))))
