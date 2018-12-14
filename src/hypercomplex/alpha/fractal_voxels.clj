(ns hypercomplex.alpha.fractal-voxels
  (:require [thi.ng.geom.core :as g]
            [thi.ng.geom.core.vector :refer [vec3]]
            [thi.ng.geom.voxel.svo :as svo]
            [thi.ng.geom.voxel.isosurface :as iso]
            [thi.ng.math.simplexnoise :as n]
            [thi.ng.geom.mesh.io :as mio]
            [thi.ng.math.core :as m]
            [clojure.java.io :as io])
  )


;(def res (double 1/8))
;(def wall 0.25)
;
;(defn gyroid ^double [s t p]
;  "Evaluates gyroid function at scaled point `p`."
;  (let [[x y z] (g/* p s)]
;    (- (m/abs
;         (+ (* (Math/cos x) (Math/sin z))
;            (* (Math/cos y) (Math/sin x))
;            (* (Math/cos z) (Math/sin y))))
;       t)))
;
;(defn voxel-box
;  ([tree op flt r]
;   (voxel-box tree op flt r r r))
;  ([tree op flt rx ry rz]
;   (->> (for [x rx y ry, z rz] (vec3 x y z))
;        (filter flt)
;        (svo/apply-voxels op tree))))
;
;(time
;  (def v
;    (reduce
;      (fn [tree [op r]] (voxel-box tree op identity r))
;      (svo/voxeltree 32 res)
;      [[svo/set-at (range 10 20 res)] [svo/set-at (range 15 25 res)]])))
;
;(time
;  (def v2
;    (reduce
;      (fn [tree [op r]] (voxel-box tree op identity r))
;      v [[svo/delete-at (range (+ 10 wall) (- 20 wall) res)]
;         [svo/delete-at (range (+ 15 wall) (- 25 wall) res)]])))
;
;(time
;  (def v3
;    (reduce
;      (fn [tree [op r]]
;        (voxel-box tree op #(m/in-range? 0.0 10.0 (gyroid 1.0 1.2 %)) r))
;      v2 [[svo/set-at (range (+ 10 wall) (- 20 wall) res)]
;          [svo/set-at (range (+ 15 wall) (- 25 wall) res)]])))
;
;(time
;  (def v4
;    (reduce
;      (fn [tree [op rx ry rz]] (voxel-box tree op identity rx ry rz))
;      v3 [[svo/delete-at (range 9 26 res) (range 9 26 res) (range 18 26 res)]])))
;
;(time
;  (with-open [o (io/output-stream "gen-voxels-test/voxel.stl")]
;    (mio/write-stl
;      (mio/wrapped-output-stream o)
;      (g/tessellate (iso/surface-mesh v4 11 0.5)))))
;
;
;;TODO these STL files can be loaded into Bullet3D
;
;
;(def res (double 1/2))
;(def num-holes 30)
;
;(defn voxel-sphere
;  ([tree op o r res]
;   (let [rg (range (- r) (+ r res) res)]
;     (->> (for [x rg y rg, z rg
;                :let [v (vec3 x y z)]
;                :when (<= (g/mag v) r)] (g/+ o v))
;          (svo/apply-voxels op tree)))))
;
;(time
;  (def v
;    (reduce
;      (fn [tree [op o r]] (voxel-sphere tree op o r res))
;      (svo/voxeltree 32 res)
;      (concat
;        [[svo/set-at (vec3 15 15 15) 14]]
;        (repeatedly
;          num-holes
;          #(vector
;             svo/delete-at
;             (vec3 (m/random 32) (m/random 32) (m/random 32))
;             (m/random 4 8)))))))
;
;(time
;  (with-open [o (io/output-stream "gen-voxels-test/sphere.stl")]
;    (mio/write-stl
;      (mio/wrapped-output-stream o)
;      (g/tessellate (iso/surface-mesh v 10 0.5)))))
;
;;;
;
;
;(def res (double 1/2))
;(def n-scale 0.1)
;(def iso-val 0.5)
;
;(def v
;  (let [r (range 1 31 res)]
;    (->> (for [x r y r z r
;               :when (<= iso-val (m/abs (n/noise3 (* x n-scale) (* y n-scale) (* z n-scale))))]
;           (vec3 x y z))
;         (svo/apply-voxels svo/set-at (svo/voxeltree 32 res)))))
;
;(time
;  (with-open [o (io/output-stream "gen-voxels-test/noise.stl")]
;    (mio/write-stl
;      (mio/wrapped-output-stream o)
;      (g/tessellate (iso/surface-mesh v 10 iso-val)))))

;;TODO takes in [x y iters] and makes voxelz

(defn iters-voxels [iters]

  (println "Computing cells")

  (let [res     (double 1/2)
        n-scale 0.1
        iso-val 0.5
        r       (range 1 31 res)
        v       (time
                  (->>
                    (for [x r y r z r
                          :when (or
                                  (<= 0.3
                                      (Math/sqrt
                                        (+ (* (- x 1.5) (- x 1.5)
                                              n-scale n-scale)
                                           (* (- y 1.5) (- y 1.5)
                                              n-scale n-scale)
                                           (* (- z 1.5) (- z 1.5)
                                              n-scale n-scale)))
                                      1.0)
                                  (<= iso-val
                                      (m/abs (n/noise3
                                               (* x n-scale)
                                               (* y n-scale)
                                               (* z n-scale)))))]
                      (vec3 x y z))
                    (svo/apply-voxels
                      svo/set-at (svo/voxeltree 32 res))))]

    (println "Writing")

    (time
      (with-open [o (io/output-stream "gen-voxels-test/frac.stl")]
        (mio/write-stl
          (mio/wrapped-output-stream o)
          (g/tessellate (iso/surface-mesh v 10 iso-val)))))
    (println "Done"))

  )

(iters-voxels nil)