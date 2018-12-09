(ns hypercomplex.fractal-iters
  (:require [hypercomplex.core :refer :all]
            [hypercomplex.cayley-dickson-construction :refer
             [complex quaternion octonion sedenion
              pathion n-hypercomplex power-of?]]))

(def JULIA-COEFF-PLAIN* (atom (complex {:a -0.1 :b 0.7 :impl :plain})))

(defn compute-iters-mandel [p q impl max-iterations]
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

(defn compute-iters-julia [p q impl max-iterations]
  (let [c           (complex {:a p :b q :impl impl})
        julia-coeff @JULIA-COEFF-PLAIN*]
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
            julia-coeff
            (times z z))
          (inc iterations))))))
