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
          r    gray
          g    (min (int (/ (* 5 (* gray gray)) 255)) 255)
          b    (min (int (+ 40 (/ (* 5 (* gray gray)) 255))) 255)]
      (int-array [1.0 r g b]))))


(defn img-intensities [intensities surface-width surface-height]
  (pmap
    (fn [[x y iters]]
      (let [i       (min
                      (dec surface-width)
                      (max
                        0
                        (int
                          (*
                            (- x (first gt/xy-range))
                            (/ surface-width
                               (apply
                                 - (reverse gt/xy-range)))))))
            j       (min
                      (dec surface-height)
                      (max
                        0
                        (int
                          (*
                            (- y (first gt/xy-range))
                            (/ surface-height
                               (apply
                                 - (reverse gt/xy-range)))))))
            ^ints c (calc-pixel-color-argb
                      iters gt/max-f-iters)]
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
      (when (= 0 (mod i 500))
        (println "Doing " i " / " quant))
      (gen/generate generator))
    (range quant)))

(deftest a-test
  (testing "  "
    (println "Generating frac")
    (let [w     500
          h     500
          image (BufferedImage.
                  w
                  h
                  BufferedImage/TYPE_INT_ARGB)
          its   (pgen-spec
                  (s/gen
                    ::gt/interesting-intensity-apache)
                  500000)]
      (println "Drawing size: " (count its) (count (set its)))
      (draw-intensities-w-raster its image w h)
      (println "Show!")
      (imgz/show (imgz/scale image 4)))))

(run-tests 'hypercomplex.gen-img-test)