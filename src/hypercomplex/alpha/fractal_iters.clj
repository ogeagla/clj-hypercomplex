(ns hypercomplex.alpha.fractal-iters
  (:require [hypercomplex.core :refer :all]
            [hypercomplex.cayley-dickson-construction :refer
             [complex quaternion octonion sedenion
              pathion n-hypercomplex power-of?]]))

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(defn compute-iters-mandel
  "Computer iterations required for equation:
  z_n+1 = z_n^2 + c, where z_0 = c, and
  c = p + q*i.
  Iterations are complete when |z| > 2.
  Impl is :apache or :plain."
  [p q impl max-iterations]
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

(defn compute-iters-julia
  "Computer iterations of equation:
  z_n+1 = z_n^2 + d, where z_0 = p + q*i,
  and d is the provided julia coefficient.
  Iterations are complete when |z| > 2.
  Impl is :apache or :plain."
  [p q impl max-iterations julia-coeff]
  (let [c           (complex {:a p :b q :impl impl})
        julia-coeff julia-coeff]
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

(defn compute-iters-quat-julia
  [p q impl max-iterations julia-coeff]
  (let [c           (quaternion {:a p :b q :c 0.0 :d 0.0 :impl impl})
        julia-coeff julia-coeff]
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
