(ns hypercomplex.gen-img-test
  (:require [clojure.test :refer :all]
            [mikera.image.core :as imgz]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [hypercomplex.gen-test :as gt])
  (:import (java.awt.image BufferedImage WritableRaster)))


(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(defn- calc-pixel-color-argb
  [iterations max-iterations]
  (if (or #_(< iterations 10)
        (= iterations max-iterations))
    (int-array [220 230 22 255])
    (let [gray (int (/ (* iterations 255) max-iterations))
          r    (min (int (+ 20 gray)) 255)
          g    (min (int (- 255 gray)) 255)
          b    (min (int (Math/abs (- gray 80))) 255)]
      (int-array [r g b 255]))))

(defn intensity->img-coord
  [intensity-value img-dim-size intensity-domain]
  (->>
    intensity-domain
    (reverse)
    (apply -)
    (/ img-dim-size)
    (* (- intensity-value (first intensity-domain)))
    (int)
    (max 0)
    (min (dec img-dim-size))))

(defn img-intensities [intensities surface-width surface-height]
  (pmap
    (fn [[x y iters]]
      (let [i       (intensity->img-coord
                      x surface-width @gt/X-RANGE*)
            j       (intensity->img-coord
                      y surface-height @gt/Y-RANGE*)
            ^ints c (calc-pixel-color-argb
                      iters @gt/MAX-CONV-ITERS*)]
        [i j c]))
    intensities))

(defn- draw-intensities-w-raster
  [intensities image surface-width surface-height]
  ; Maybe look at this if performance is not improving with this technique:
  ; https://stackoverflow.com/questions/25178810/create-a-writableraster-based-on-int-array
  (let [^WritableRaster raster (.getRaster image)
        img-vals               (img-intensities
                                 intensities surface-width surface-height)]
    (doseq [[^int i ^int j ^ints argb] img-vals]
      (.setPixel raster i j argb))))


(defn pgen-spec [generator quant]
  (pmap
    (fn [i]
      (when (= 0 (mod i (int (/ quant 20))))
        (println "Generating intensity " i " / " quant " , "
                 (int (* 100 (/ i quant))) "% complete, t="
                 (System/currentTimeMillis)))
      (gen/generate generator))
    (range quant)))

(defn width->iters [w]
  (cond
    (> w 0.5) 32
    (> w 0.4) 36
    (> w 0.3) 40
    (> w 0.1) 45
    (> w 0.05) 50
    (> w 0.04) 55
    (> w 0.03) 60
    (> w 0.01) 85
    (> w 0.008) 90
    (> w 0.006) 95
    (> w 0.005) 100
    (> w 0.003) 105
    (> w 0.002) 110
    (> w 0.001) 125
    (> w 0.00075) 150
    (> w 0.0005) 160
    (> w 0.0001) 180
    (> w 0.00005) 210
    (> w 0.00001) 230
    (> w 0.00005) 250
    (> w 0.000001) 270
    :else 300))

(defn fractal-config [c]
  (case c
    :c1 {:x-center  -0.01
         :y-center  -0.80
         :type      :edge-spiral-arms
         :x-width0  0.4
         :x-widthf  0.004
         :y-height0 0.4
         :y-heightf 0.004
         }))

(defn render-config [mode]
  (case mode
    :fast {:test-size    100000
           :views        50
           :img-w        300
           :img-h        300
           :img-scale-fn #(imgz/scale % 4)}
    :medium {:test-size    500000
             :views        10
             :img-w        400
             :img-h        400
             :img-scale-fn #(imgz/scale % 3)}
    :slow {:test-size    2000000
           :views        5
           :img-w        1200
           :img-h        1200
           :img-scale-fn identity}
    :render {:test-size    50000000
             :views        5
             :img-w        1200
             :img-h        1200
             :img-scale-fn identity})
  )

(deftest gen-img-test
  (testing "Generates fractal image"
    (println "Generating frac")
    (let [{:keys
           [x-center y-center x-width0
            x-widthf y-height0 y-heightf]} (fractal-config :c1)
          {:keys
           [test-size views img-w
            img-h img-scale-fn]} (render-config :fast)
          tx     (- x-width0 x-widthf)
          ty     (- y-height0 y-heightf)
          domans (map
                   (fn [i]
                     (let [pct (/ i views)
                           dx  (+ (/ x-widthf 2) (* pct (/ tx 2)))
                           dy  (+ (/ y-heightf 2) (* pct (/ ty 2)))]
                       [[(- x-center dx)
                         (+ x-center dx)]
                        [(- y-center dy)
                         (+ y-center dy)]
                        (width->iters (* 2 dx))]))
                   (range views 0 -1))]
      (doseq [[xrange yrange iters] domans]
        (println "Domain, iters, size : " xrange yrange iters test-size)
        (reset! gt/X-RANGE* xrange)
        (reset! gt/Y-RANGE* yrange)
        (reset! gt/MAX-CONV-ITERS* iters)
        (let [image (BufferedImage.
                      img-w
                      img-h
                      BufferedImage/TYPE_INT_ARGB)
              its   (pgen-spec
                      (s/gen
                        ::gt/interesting-intensity-plain)
                      test-size)
              imgf  (str "gen-img-test/gen-img-test-i" iters "-"
                         (System/currentTimeMillis) "-" test-size
                         "-x-" (first xrange) "-" (second xrange)
                         "-y-" (first yrange) "-" (second yrange) ".png")]
          (draw-intensities-w-raster its image w h)
          ;(imgz/show image)
          (imgz/save (img-scale-fn image) imgf)
          (println "Wrote: " imgf))))))

(run-tests 'hypercomplex.gen-img-test)