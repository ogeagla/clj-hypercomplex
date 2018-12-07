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
  "Returns an int array representing an ARGB color
  derived from iterations, with alpha = 1.0."
  [iterations max-iterations]
  (if (or (< iterations 10)
          (= iterations max-iterations))
    (int-array [1.0 0 0 0])
    (let [gray (int (/ (* iterations 255) max-iterations))
          r    (min (int (+ 90 (/ (* 5 (* gray gray)) 255))) 255)
          g    (min (int (/ (* 3 (* gray gray)) 255)) 255)
          b    gray]
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
      (when (= 0 (mod i (int (/ quant 10))))
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

(deftest gen-img-test
  (testing "Generates fractal image"
    (println "Generating frac")
    (doseq [test-size [
                       20000
                       ;400000
                       ;800000
                       ;;this one takes ~80 seconds
                       ;1600000
                       ;;this one might take 10 mins:
                       ;12800000
                       ;;this last one might take 80 minutes or so:
                       ;102400000
                       ]]
      (let [x-center  -0.01
            x-width0  0.7
            x-widthf  0.000001
            y-center  -0.8
            y-height0 0.7
            y-heightf 0.000001
            tx        (- x-width0 x-widthf)
            ty        (- y-height0 y-heightf)
            views     50
            domans    (map
                        (fn [i]
                          (let [pctr (/ i views)
                                pct (cond
                                      ;for fast initial zoom
                                      (>= pctr 0.95) (- 1.0 (* 10 (- 1.0 pctr)))
                                      (>= pctr 0.5) (- pctr 0.45)
                                      :else (/ pctr 10.0))
                                dx  (+ (/ x-widthf 2) (* pct (/ tx 2)))
                                dy  (+ (/ y-heightf 2) (* pct (/ ty 2)))]
                            (println "---- Percent mapping: " pctr "->" pct)
                            [[(- x-center dx)
                              (+ x-center dx)]
                             [(- y-center dy)
                              (+ y-center dy)]
                             (width->iters (* 2 dx))]))
                        (range views 0 -1))]
        (doseq [[xrange yrange iters] domans]
          (println "Domain, iters : " xrange yrange iters)
          (reset! gt/X-RANGE* xrange)
          (reset! gt/Y-RANGE* yrange)
          (reset! gt/MAX-CONV-ITERS* iters)
          (let [w     80
                h     80
                image (BufferedImage.
                        w
                        h
                        BufferedImage/TYPE_INT_ARGB)
                its   (pgen-spec
                        (s/gen
                          ::gt/interesting-intensity-plain)
                        test-size)
                imgf  (str "gen-img-test/gen-img-test-i" iters "-"
                           (System/currentTimeMillis) "-" test-size
                           "-x-" (first xrange) "-" (second xrange)
                           "-y-" (first yrange) "-" (second yrange) ".png")]
            (println "Drawing size: "
                     test-size (count its) (count (set its)))
            (draw-intensities-w-raster its image w h)
            (println "Show!")
            ;(imgz/show image)
            (imgz/save (imgz/scale image 4) imgf)
            (println "Wrote: " imgf)))))))

(run-tests 'hypercomplex.gen-img-test)