(ns hypercomplex.alpha.core-spec
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [hypercomplex.core :refer :all]
            [hypercomplex.alpha.fractal-spec :as fs]
            [hypercomplex.cayley-dickson-construction :refer
             [complex quaternion octonion sedenion
              pathion n-hypercomplex power-of?]])
  (:import (hypercomplex.core Complex2Apache Complex2 Construction)))

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(def MAX-TRIES 10000)

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
          (s/gen ::fs/coeff)
          (s/gen ::fs/coeff))))))

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
          (s/gen ::fs/coeff)
          (s/gen ::fs/coeff))))))

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
            (s/gen ::fs/coeff)
            {:min-elements 2
             :max-elements 257})
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
            (s/gen ::fs/coeff)
            {:min-elements 2
             :max-elements 257})
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
