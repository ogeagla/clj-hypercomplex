(ns hypercomplex.alpha.fractal-voxels
  (:require
    [hypercomplex.alpha.fractal-iters :as fi]
    [thi.ng.geom.core :as g]
    [thi.ng.geom.core.vector :refer [vec3]]
    [thi.ng.geom.voxel.svo :as svo]
    [thi.ng.geom.voxel.isosurface :as iso]
    [thi.ng.math.simplexnoise :as n]
    [thi.ng.geom.mesh.io :as mio]
    [thi.ng.math.core :as m]
    [clojure.java.io :as io]
    [hypercomplex.cayley-dickson-construction :as c]))

(defn- i->x [i [imin imax] [xmin xmax]]
  (let [s (/ (- xmax xmin) (- imax imin))
        x (+ xmin (* s (- i imin)))]
    x))

(defn iter-voxels
  [{:keys [iso-val delta-i irng
           xrng yrng zrng iter-thresh max-iters
           julia-a julia-b]}]

  (println "Computing cells")

  (let [r                 (range (first irng) (second irng) delta-i)

        cartesian-domain3 (doall
                            (for [x r y r z r]
                              [x y z]))

        iters             (time
                            (into
                              {}
                              (pmap
                                (fn [[x y z]]
                                  (let [xx (i->x x irng xrng)
                                        yy (i->x y irng yrng)
                                        zz (i->x z irng zrng)
                                        ja (or julia-a zz)
                                        jb (or julia-b zz)]
                                    [[x y z] (fi/compute-iters-julia
                                               xx yy :plain max-iters
                                               (c/complex
                                                 {:a ja :b jb :impl :plain}))]))
                                cartesian-domain3)))

        v                 (time
                            (->>
                              (for [x r y r z r
                                    :when (< iter-thresh (get iters [x y z]))]
                                (vec3 x y z))
                              (svo/apply-voxels
                                svo/set-at (svo/voxeltree 32 delta-i))))]

    (println "Writing")

    (time
      (with-open [o (io/output-stream "gen-voxels-test/frac.stl")]
        (mio/write-stl
          (mio/wrapped-output-stream o)
          (g/tessellate (iso/surface-mesh v 10 iso-val)))))
    (println "Done")))

(def conf-1
  {:iso-val     0.5
   :delta-i     (double 1/4)
   :irng        [1 31]
   :xrng        [-1.5 1.5]
   :yrng        [-1.5 1.5]
   :zrng        [0.0 -1.5]
   :julia-b     0.7
   :max-iters   11
   :iter-thresh 10})

(iter-voxels conf-1)