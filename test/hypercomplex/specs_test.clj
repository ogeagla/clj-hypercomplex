(ns hypercomplex.specs-test
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

(def MAX-TRIES 1000)
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
        (gen/such-that
          (fn [[a b]]
            true)
          (gen/tuple
            (gen/double)
            (gen/double))
          MAX-TRIES)))))

(s/def ::complex2-plain
  (s/with-gen
    #(and (instance? Complex2 %)
          (satisfies? Nion %)
          (satisfies? NionOps %))
    (fn []
      (gen/fmap
        (fn [[a b]]
          (init-complex2 a b :plain))
        (gen/such-that
          (fn [[a b]]
            true)
          (gen/tuple
            (gen/double)
            (gen/double))
          MAX-TRIES)))))

(s/def ::hypercomplex-apache
  (s/with-gen
    #(and (instance? Construction %)
          (satisfies? Nion %)
          (satisfies? NionOps %))
    (fn []
      (gen/fmap
        (fn [[coeffs ]]
          (n-hypercomplex coeffs :apache))
        (gen/such-that
          (fn [[coeffs ]]
            (and
              (not (zero? (count coeffs)))
              (power-of? (count coeffs) 2)))
          (gen/tuple
            (gen/list (gen/double)))
          MAX-TRIES)))))

(s/def ::hypercomplex-plain
  (s/with-gen
    #(and (instance? Construction %)
          (satisfies? Nion %)
          (satisfies? NionOps %))
    (fn []
      (gen/fmap
        (fn [[coeffs ]]
          (n-hypercomplex coeffs :plain))
        (gen/such-that
          (fn [[coeffs ]]
            (and
              (not (zero? (count coeffs)))
              (power-of? (count coeffs) 2)))
          (gen/tuple
            (gen/list (gen/double)))
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
            (println "ca order: " (:order a) (:order b))
            (eq-order? a b))
          (gen/tuple
            (s/gen ::hypercomplex-apache)
            (s/gen ::hypercomplex-apache))
          MAX-TRIES)))) )

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
            (println "ca order: " (:order a) (:order b))
            (eq-order? a b))
          (gen/tuple
            (s/gen ::hypercomplex-plain)
            (s/gen ::hypercomplex-plain))
          MAX-TRIES)))) )


(ct/defspec
  creates-apache2
  {:num-tests 10 :seed SEED}
  (prop/for-all [c2a (s/gen ::complex2-apache)]
                (println c2a)
                (is (s/valid? ::complex2-apache c2a))))

(ct/defspec
  creates-plain2
  {:num-tests 10 :seed SEED}
  (prop/for-all [c2p (s/gen ::complex2-plain)]
                (println c2p)
                (is (s/valid? ::complex2-plain c2p))))

(ct/defspec
  creates-apache-hypercomplex
  {:num-tests 10 :seed SEED}
  (prop/for-all [ha (s/gen ::hypercomplex-apache)]
                (println ha)
                (is (s/valid? ::hypercomplex-apache ha))))
(ct/defspec
  creates-plain-hypercomplex
  {:num-tests 10 :seed SEED}
  (prop/for-all [ha (s/gen ::hypercomplex-plain)]
                (println ha)
                (is (s/valid? ::hypercomplex-plain ha))))

(ct/defspec
  creates-apache-hypercomplex-construction
  {:num-tests 10 :seed SEED}
  (prop/for-all [ca (s/gen ::construction-apache)]
                (println ca)
                (is (s/valid? ::construction-apache ca))))

(ct/defspec
  creates-plain-hypercomplex-construction
  {:num-tests 10 :seed SEED}
  (prop/for-all [ca (s/gen ::construction-plain)]
                (println ca)
                (is (s/valid? ::construction-plain ca))))


(run-tests 'hypercomplex.specs-test)
