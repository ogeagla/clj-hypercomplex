(ns hypercomplex.alpha.fractal-pixels
  (:require [hypercomplex.alpha.fractal-spec :as fspec]
            [hypercomplex.alpha.fractal-colors :as fc])
  (:import (java.awt Color Graphics)
           (java.awt.image WritableRaster BufferedImage)))


(defn- calc-pixel-color-argb
  [iterations max-iterations palette]
  (case palette
    :green-red (fc/palette-green-to-red iterations max-iterations)
    :rainbow (fc/palette-rainbox iterations max-iterations)
    :rainbow2 (fc/palette-rainbox2 iterations max-iterations)))

(defn intensity-coord->img-coord
  [intensity-coord img-dim-size intensity-domain flip?]
  (let [c (->>
            intensity-domain
            (reverse)
            (apply -)
            (/ img-dim-size)
            (* (- intensity-coord (first intensity-domain)))
            (int)
            (max 0)
            (min (dec img-dim-size)))]
    (if flip?
      (- (dec img-dim-size) c)
      c)))

(defn img-intensities
  [intensities surface-width surface-height palette]
  (pmap
    (fn [[x y iters]]
      (let [i       (intensity-coord->img-coord
                      x surface-width @fspec/X-RANGE* false)
            j       (intensity-coord->img-coord
                      y surface-height @fspec/Y-RANGE* true)
            ^ints c (calc-pixel-color-argb
                      iters @fspec/MAX-CONV-ITERS* palette)]
        [i j c]))
    intensities))



(defn draw-intensities-w-raster
  [intensities ^BufferedImage image surface-width surface-height palette]
  (let [^WritableRaster raster (.getRaster image)
        ^Graphics g            (.getGraphics image)
        img-vals               (set
                                 (img-intensities
                                   intensities surface-width
                                   surface-height palette))
        x-zero                 (intensity-coord->img-coord
                                 0 surface-width @fspec/X-RANGE* false)
        y-zero                 (intensity-coord->img-coord
                                 0 surface-height @fspec/Y-RANGE* true)]
    (doseq [[^int i ^int j ^ints argb] img-vals]
      (.setPixel raster i j argb))
    (.setColor g Color/BLACK)
    (.drawLine g
               0 y-zero
               surface-width y-zero)
    (.drawLine g
               x-zero 0
               x-zero surface-height)))