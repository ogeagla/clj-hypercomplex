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
    [hypercomplex.cayley-dickson-construction :as c])
  (:import (java.util Date)))

(defn- i->x [i imin xmin s]
  (+ xmin (* s (- i imin))))

(defn ppmap
  "Partitioned pmap, for grouping map ops together to make parallel
  overhead worthwhile"
  [grain-size f & colls]
  (let [c*    (atom 0)
        parts (int (Math/ceil (/ (count (apply concat colls)) grain-size)))]
    (apply concat
           (apply pmap
                  (fn [& pgroups]
                    (println "Doing part: "
                             (swap! c* inc) " / "
                             parts " " (Date.))
                    (doall (apply map f pgroups)))
                  (map (partial partition-all grain-size) colls)))))

(defn iters-parallel-part
  [cartesian-domain3 imin xmin ymin zmin sx sy sz julia-a julia-b max-iters]
  (doall
    (ppmap
      200000
      (fn [[x y z]]
        (let [xx (i->x x imin xmin sx)
              yy (i->x y imin ymin sy)
              zz (i->x z imin zmin sz)
              ja (or julia-a zz)
              jb (or julia-b zz)]
          [[x y z] (fi/compute-iters-julia
                     xx yy :plain max-iters
                     (c/complex
                       {:a ja :b jb :impl :plain}))]))
      cartesian-domain3)))

(defn iter-voxels
  [{:keys [iso-val delta-i irng
           xrng yrng zrng iter-thresh max-iters
           julia-a julia-b fname]}]

  (println "Computing cells " fname)

  (let [r                 (range (first irng) (second irng) delta-i)
        cartesian-domain3 (time
                            (doall
                              (for [x r y r z r]
                                [x y z])))
        [imin imax] irng
        [xmin xmax] xrng
        [ymin ymax] yrng
        [zmin zmax] zrng
        sx                (/ (- xmax xmin) (- imax imin))
        sy                (/ (- ymax ymin) (- imax imin))
        sz                (/ (- zmax zmin) (- imax imin))
        _                 (println "Computing iters")
        iters             (time
                            (into
                              {}
                              (iters-parallel-part
                                cartesian-domain3 imin xmin ymin zmin
                                sx sy sz julia-a julia-b max-iters)))
        _                 (println "Gathering voxels")
        v                 (time
                            (->>
                              (for [[x y z] cartesian-domain3
                                    :when (< iter-thresh (get iters [x y z]))]
                                (vec3 x y z))
                              (svo/apply-voxels
                                svo/set-at (svo/voxeltree 32 delta-i))))]

    (println "Writing " fname)

    (time
      (with-open [o (io/output-stream fname)]
        (mio/write-stl
          (mio/wrapped-output-stream o)
          (g/tessellate (iso/surface-mesh v 10 iso-val)))))
    (println "Done")))

(def conf-1
  {:fname       "gen-voxels-test/fractal-1.stl"
   :iso-val     0.5
   :delta-i     (double 1/4)
   :irng        [1 31]
   :xrng        [-1.5 1.5]
   :yrng        [-1.5 1.5]
   :zrng        [0.0 -1.5]
   :julia-b     0.7
   :max-iters   11
   :iter-thresh 10})

(def conf-2
  {:fname       "gen-voxels-test/fractal-2.stl"
   :iso-val     0.5
   :delta-i     (double 1/4)
   :irng        [1 31]
   :xrng        [-2.0 2.0]
   :yrng        [-2.0 2.0]
   :zrng        [-2.0 2.0]
   :julia-a     0.065
   :max-iters   11
   :iter-thresh 10})

(def conf-3
  {:fname       "gen-voxels-test/fractal-3.stl"
   :iso-val     0.5
   :delta-i     (double 1/4)
   :irng        [1 31]
   :xrng        [-3.5 3.5]
   :yrng        [-3.5 3.5]
   :zrng        [-4.5 4.5]
   :julia-a     0.065
   :max-iters   3
   :iter-thresh 2})

(def conf-4
  {:fname       "gen-voxels-test/fractal-4.stl"
   :iso-val     0.5
   :delta-i     (double 1/4)
   :irng        [1 31]
   :xrng        [-3.5 3.5]
   :yrng        [-3.5 3.5]
   :zrng        [-4.5 4.5]
   :julia-a     0.65
   :max-iters   3
   :iter-thresh 2})

(def conf-5
  {:fname       "gen-voxels-test/fractal-5.stl"
   :iso-val     0.5
   :delta-i     (double 1/4)
   :irng        [1 31]
   :xrng        [0.6 1.8]
   :yrng        [0.6 1.8]
   :zrng        [0.0 4.0]
   :julia-b     -0.65
   :max-iters   5
   :iter-thresh 4})

(def conf-6
  {:fname       "gen-voxels-test/fractal-6.stl"
   :iso-val     0.5
   :delta-i     (double 1/8)
   :irng        [1 31]
   :xrng        [-0.1 1.8]
   :yrng        [-0.1 1.8]
   :zrng        [-0.3 0.3]
   :julia-b     -0.85
   :max-iters   15
   :iter-thresh 14})

(def conf-7
  {:fname       "gen-voxels-test/fractal-7.stl"
   :iso-val     0.5
   :delta-i     (double 1/8)
   :irng        [1 31]
   :xrng        [-0.4 0.8]
   :yrng        [-0.4 0.8]
   :zrng        [-0.2 0.2]
   :julia-b     -0.85
   :max-iters   15
   :iter-thresh 14})

(def conf-8
  {:fname       "gen-voxels-test/fractal-8.stl"
   :iso-val     0.5
   :delta-i     (double 1/8)
   :irng        [1 31]
   :xrng        [0.0 0.4]
   :yrng        [0.4 0.8]
   :zrng        [-0.2 0.2]
   :julia-b     -0.85
   :max-iters   20
   :iter-thresh 19})

(iter-voxels conf-8)