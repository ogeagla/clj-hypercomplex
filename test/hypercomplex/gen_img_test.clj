(ns hypercomplex.gen-img-test
  (:require [clojure.test :refer :all]
            [hypercomplex.alpha.fractal-generate :as fg]))

(deftest gen-img-test
  (testing "Generates fractal image"
    (println "Generating frac")
    #_(fg/run)))
