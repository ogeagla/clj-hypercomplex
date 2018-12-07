(ns hypercomplex.gen-img-test
  (:require [clojure.test :refer :all]
            [mikera.image.core :as imgz]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [hypercomplex.gen-test :as gt])
  (:import (java.awt.image BufferedImage WritableRaster)))

(defn- calc-pixel-color-argb
  "Returns an int array representing an ARGB color
  derived from iterations, with alpha = 1.0."
  [iterations max-iterations]
  (if (or (< iterations 10)
          (= iterations max-iterations))
    (int-array [1.0 0 0 0])
    (let [gray (int (/ (* iterations 255) max-iterations))
          r    (min (int (+ 60 (/ (* 5 (* gray gray)) 255))) 255)
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

(deftest gen-img-test
  (testing "Generates fractal image"
    (println "Generating frac")
    (doseq [test-size [
                       6400000
                       ;400000
                       ;800000
                       ;;this one takes ~80 seconds
                       ;1600000
                       ;;this one might take 10 mins:
                       ;12800000
                       ;;this last one might take 80 minutes or so:
                       ;102400000
                       ]]
      (let [x-center  0.0026
            x-width0  0.001
            x-widthf  0.00000001
            y-center  -0.822
            y-height0 0.001
            y-heightf 0.00000001
            tx        (- x-width0 x-widthf)
            ty        (- y-height0 y-heightf)
            views     5
            domans    (map
                        (fn [i]
                          (let [pct (/ i views)
                                dx  (+ (/ x-widthf 2) (* pct (/ tx 2)))
                                dy  (+ (/ y-heightf 2) (* pct (/ ty 2)))]
                            [[(- x-center dx)
                              (+ x-center dx)]
                             [(- y-center dy)
                              (+ y-center dy)]]))
                        (range (dec views) -1 -1))]
        (doseq [[xrange yrange] domans]
          (println "Domain: " xrange yrange)
          (reset! gt/X-RANGE* xrange)
          (reset! gt/Y-RANGE* yrange)
          (let [w     1600
                h     1600
                image (BufferedImage.
                        w
                        h
                        BufferedImage/TYPE_INT_ARGB)
                its   (pgen-spec
                        (s/gen
                          ::gt/interesting-intensity-plain)
                        test-size)
                imgf  (str "gen-img-test/gen-img-test-"
                           (System/currentTimeMillis) "-" test-size
                           "-x-" (first xrange) "-" (second xrange)
                           "-y-" (first yrange) "-" (second yrange) ".png")]
            (println "Drawing size: "
                     test-size (count its) (count (set its)))
            (draw-intensities-w-raster its image w h)
            (println "Show!")
            ;(imgz/show image)
            (imgz/save image imgf)
            (println "Wrote: " imgf)))))))

(run-tests 'hypercomplex.gen-img-test)