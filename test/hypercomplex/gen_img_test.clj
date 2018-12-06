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
          r    (min (int (+ 40 (/ (* 5 (* gray gray)) 255))) 255)
          g    (min (int (/ (* 5 (* gray gray)) 255)) 255)
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
      (when (= 0 (mod i 50000))
        (println "Generating intensity " i " / " quant " , "
                 (int (* 100 (/ i quant))) "% complete"))
      (gen/generate generator))
    (range quant)))

(deftest gen-img-test
  (testing "Generates fractal image"
    (println "Generating frac")
    (let [c* (atom 1)]
      (doseq [test-size [
                         1000000
                         ;400000
                         ;800000
                         ;;this one takes ~80 seconds
                         ;1600000
                         ;3200000
                         ;6400000
                         ;;this one might take 10 mins:
                         ;12800000
                         ;25600000
                         ;51200000
                         ;;this last one might take 80 minutes or so:
                         ;102400000
                         ]]

        (doseq [[xrange yrange] [
                                 [[-0.3 0.2] [-1.0 -0.5]]
                                 [[-0.28 0.18] [-0.98 -0.52]]
                                 [[-0.26 0.16] [-0.96 -0.54]]
                                 [[-0.24 0.14] [-0.94 -0.56]]
                                 [[-0.22 0.12] [-0.92 -0.58]]
                                 [[-0.21 0.11] [-0.91 -0.59]]
                                 [[-0.20 0.10] [-0.90 -0.60]]
                                 [[-0.19 0.09] [-0.89 -0.61]]
                                 [[-0.185 0.085] [-0.885 -0.615]]
                                 [[-0.18 0.08] [-0.88 -0.62]]
                                 [[-0.175 0.075] [-0.875 -0.625]]
                                 [[-0.17 0.07] [-0.87 -0.63]]

                                 ]]
          (reset! gt/X-RANGE* xrange)
          (reset! gt/Y-RANGE* yrange)
          (let [
                ;test-size 200000
                w     2000
                h     1200
                image (BufferedImage.
                        w
                        h
                        BufferedImage/TYPE_INT_ARGB)
                its   (pgen-spec
                        (s/gen
                          ::gt/interesting-intensity-plain)
                        test-size)
                imgf  (str "gen-img-test/gen-img-test-"
                           (System/currentTimeMillis) "-"
                           test-size "-" xrange "-" yrange ".png")]
            (swap! c* inc)
            (println "Drawing size: "
                     test-size (count its) (count (set its)))
            (draw-intensities-w-raster its image w h)
            (println "Show!")
            ;(imgz/show image)
            (imgz/save image imgf)
            (println "Wrote: " imgf)))))))

(run-tests 'hypercomplex.gen-img-test)