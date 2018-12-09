(ns hypercomplex.gen-test
  (:require
    [clojure.test :refer :all]
    [clojure.spec.alpha :as s]
    [clojure.test.check.clojure-test :as ct]
    [clojure.test.check.properties :as prop]
    [hypercomplex.core :refer :all]
    [hypercomplex.core-spec :as cs]
    [hypercomplex.fractal-spec :as fs]
    [hypercomplex.cayley-dickson-construction :refer
     [complex quaternion octonion sedenion
      pathion n-hypercomplex power-of?]]))


(def SEED 12345678987654321)

(ct/defspec
  creates-apache2
  {:num-tests 10 :seed SEED}
  (prop/for-all [c2a (s/gen ::cs/complex2-apache)]
                (is (s/valid? ::cs/complex2-apache c2a))))

(ct/defspec
  creates-plain2
  {:num-tests 10 :seed SEED}
  (prop/for-all [c2p (s/gen ::cs/complex2-plain)]
                (is (s/valid? ::cs/complex2-plain c2p))))

(ct/defspec
  creates-apache-hypercomplex
  {:num-tests 10 :seed SEED}
  (prop/for-all [ha (s/gen ::cs/hypercomplex-apache)]
                (is (s/valid? ::cs/hypercomplex-apache ha))))
(ct/defspec
  creates-plain-hypercomplex
  {:num-tests 10 :seed SEED}
  (prop/for-all [ha (s/gen ::cs/hypercomplex-plain)]
                (is (s/valid? ::cs/hypercomplex-plain ha))))

(ct/defspec
  creates-apache-hypercomplex-construction
  {:num-tests 10 :seed SEED}
  (prop/for-all [ca (s/gen ::cs/construction-apache)]
                ;(println ca)
                (is (s/valid? ::cs/construction-apache ca))))

(ct/defspec
  creates-plain-hypercomplex-construction
  {:num-tests 10 :seed SEED}
  (prop/for-all [ca (s/gen ::cs/construction-plain)]
                ;(println ca)
                (is (s/valid? ::cs/construction-plain ca))))

(ct/defspec
  creates-plain-intensity-julia
  {:num-tests 10 :seed SEED}
  (prop/for-all [i (s/gen ::fs/interesting-intensity-plain-julia)]
                (is (s/valid? ::fs/interesting-intensity-plain-julia i))))

(ct/defspec
  creates-plain-intensities-julia
  {:num-tests 10 :seed SEED}
  (prop/for-all
    [its (s/gen ::fs/interesting-intensities-plain-julia)]
    ;(println "Intensities count: " (count its))
    (is (s/valid? ::fs/interesting-intensities-plain-julia its))))

(ct/defspec
  creates-plain-intensity-julia
  {:num-tests 10 :seed SEED}
  (prop/for-all [i (s/gen ::fs/interesting-intensity-plain-julia)]
                (is (s/valid? ::fs/interesting-intensity-plain-julia i))))

(ct/defspec
  creates-plain-intensities-julia
  {:num-tests 10 :seed SEED}
  (prop/for-all
    [its (s/gen ::fs/interesting-intensities-plain-julia)]
    ;(println "Intensities count: " (count its))
    (is (s/valid? ::fs/interesting-intensities-plain-julia its))))


;(run-tests 'hypercomplex.gen-test)
