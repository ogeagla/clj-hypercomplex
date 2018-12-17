(ns hypercomplex.alpha.fractal-iters
  (:require [hypercomplex.core :refer :all]
            [hypercomplex.cayley-dickson-construction :refer
             [complex quaternion octonion sedenion
              pathion n-hypercomplex power-of?]]
            [taoensso.nippy :as nippy]
            [me.raynes.fs :as fs]
            [clojure.set :as set])
  (:import (java.util Date)))



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

(defn- compute-iters-julia-f
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

(def compute-iters-julia (memoize compute-iters-julia-f))


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



(defn ppmap
  "Partitioned pmap, for grouping map ops together to make parallel
  overhead worthwhile"
  [name grain-size f & colls]
  (let [c*    (atom 0)
        parts (int (Math/ceil (/ (count (apply concat colls)) grain-size)))]
    (apply concat
           (apply pmap
                  (fn [& pgroups]
                    (println name " doing part: "
                             (swap! c* inc) " / "
                             parts " " (Date.))
                    (doall (apply map f pgroups)))
                  (map (partial partition-all grain-size) colls)))))

(defn iter-key->iters
  [{:keys [itype p q imax coeff impl x y z] :as iter-key}]
  (let [iter-val (case itype
                   :julia-2 (compute-iters-julia-f p q impl imax coeff))]
    [iter-key iter-val]))

(defn cached-compute-iters [iter-keys cfile]
  (println "Loading cache " cfile)
  (let [cache              (if (fs/exists? cfile)
                             (nippy/thaw-from-file cfile)
                             {})
        cache-keys-and-old (into
                             {}
                             (ppmap
                               "Splitting coords from cache keys"
                               200000
                               (fn [ik]
                                 [(dissoc ik :x :y :z) [(:x ik) (:y ik) (:z ik)]])
                               iter-keys))
        cache-keys         (keys cache-keys-and-old)
        cache-hits         (select-keys cache cache-keys)
        _                  (println "Cache hits: " (count cache-hits) " / " (count cache-keys)
                                    " , cache size: " (count cache))
        found-keys         (keys cache-hits)
        to-xompute-keys    (set/difference (set cache-keys) (set found-keys))
        _                  (println "Computing: " (count to-xompute-keys))
        computed           (into {}
                                 (ppmap
                                   "Computing iters "
                                   200000
                                   iter-key->iters
                                   to-xompute-keys))]

    (when-not (empty? to-xompute-keys)
      (println "Persisting cache")
      (nippy/freeze-to-file cfile (merge cache computed)))
    (let [wo-xyz (merge cache-hits computed)
          w-xyz  (into
                   {}
                   (ppmap
                     "Adding back voxel xyz to cached iter data "
                     200000
                     (fn [[kk vv]]
                       (let [wxyz (get cache-keys-and-old kk)]
                         [wxyz
                          vv]))
                     wo-xyz))]
      w-xyz))
  )
