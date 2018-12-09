(ns hypercomplex.gen-img-test
  (:require [clojure.test :refer :all]
            [hypercomplex.fractal-generate :as fg]))

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(deftest gen-img-test
  (testing "Generates fractal image"
    (println "Generating frac")
    #_(fg/run)))

;(run-tests 'hypercomplex.gen-img-test)