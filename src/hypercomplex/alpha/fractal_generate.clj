(ns hypercomplex.alpha.fractal-generate
  (:require [me.raynes.fs :as fs]
            [clojure.spec.alpha :as s]
            [clojure.java.shell :refer [sh]]
            [mikera.image.core :as imgz]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [hypercomplex.cayley-dickson-construction :as c]
            [hypercomplex.alpha.fractal-spec :as fspec]
            [hypercomplex.alpha.fractal-colors :as fc]
            [hypercomplex.core :refer :all]
            [me.raynes.fs :as fs]
            [hypercomplex.alpha.fractal-iters :as f])
  (:import (java.awt.image BufferedImage WritableRaster)
           (java.awt Color Graphics)
           (java.util Date)))


(set! *unchecked-math* true)
(set! *warn-on-reflection* true)


(defn animate-results-gif [glob output]
  (println "Creating animated gif from files, to file: " glob " -> " output)
  (try
    (sh "convert" " -dispose" "2" "-delay" "7" "-loop" "0" glob output)
    (catch Throwable t
      (println "Could not create animated gif:" t))))

(defn animate-results-ffmpeg [imgdir output]
  (println "Creating animated mp4 from files, to file: " imgdir " -> " output)
  (try
    (sh "ffmpeg" "-framerate" "6" "-i"
        (str imgdir "/img-%04d.png") "-c:v" "libx264" "-r" "30"
        output)))

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

(defn- draw-intensities-w-raster
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


(defn intensities-gen-spec [generator quant]
  (pmap
    (fn [i]
      (when (= 0 (rem i (int (/ quant 10))))
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

(defn complex-between-pct [c1 c2 pct]
  (c/complex
    {:a    (+ (:a c1) (* pct (- (:a c2) (:a c1))))
     :b    (+ (:b c1) (* pct (- (:b c2) (:b c1))))
     :impl :plain}))

(defn domains
  [{:keys [views type wf hf w0 h0 x0 y0 xf yf julia-coeff0 julia-coefff iter-scale]
    :or   {iter-scale 1.0}
    :as   d}]
  (->>
    (range views -1 -1)
    (map
      (fn [i]
        (let [width-range  (- w0 wf)
              height-range (- h0 hf)
              pct-desc     (/ i views)
              pct-asc      (- 1.0 pct-desc)
              dx           (* pct-asc (- xf x0))
              dy           (* pct-asc (- yf y0))
              dw           (+ (/ wf 2) (* pct-desc (/ width-range 2)))
              dh           (+ (/ hf 2) (* pct-desc (/ height-range 2)))]
          [[(+ (- x0 dw) dx)
            (+ x0 dw dx)]
           [(+ (- y0 dh) dy)
            (+ y0 dh dy)]
           (int (* iter-scale (width->iters (* 2 dw))))
           (when (= :julia type)
             (complex-between-pct julia-coeff0 julia-coefff pct-asc))])))))

(defn combine-domains [ds views]
  (->>
    ds
    (map #(assoc % :views views))
    (map domains)
    (mapcat identity)))

(defn multidom-with
  "Ops:
   :pan [new-x-coord new-y]
   :zoom [new-w new-h]
   :iter-var new-scale
   :frac-var new-frac-var
  "
  [doms & ops]
  (let [{:keys [xf yf wf hf type
                julia-coefff iter-scale]
         :or   {iter-scale 1.0}} (last doms)
        {:keys [pan zoom fractal-iters-change fractal-coeff-change]
         :or   {fractal-iters-change iter-scale}} ops
        my-type         type
        my-julia-coeff0 julia-coefff
        my-julia-coefff (or fractal-coeff-change julia-coefff)
        my-x0           xf
        my-y0           yf
        my-h0           hf
        my-w0           wf
        my-xf           (if pan (first pan) xf)
        my-yf           (if pan (second pan) yf)
        my-wf           (if zoom (first zoom) wf)
        my-hf           (if zoom (second zoom) hf)]
    (conj doms
          {:iter-scale   fractal-iters-change
           :x0           my-x0
           :y0           my-y0
           :xf           my-xf
           :yf           my-yf
           :w0           my-w0
           :wf           my-wf
           :h0           my-h0
           :hf           my-hf
           :type         my-type
           :julia-coeff0 my-julia-coeff0
           :julia-coefff my-julia-coefff})))

(def fractal-config
  {
   ;Moves, zooms, on Mandelbrot
   :c2  {:x0   -1.4
         :y0   0.0
         :xf   -1.4
         :yf   0.0
         :w0   1.0
         :wf   0.004
         :h0   1.0
         :hf   0.004
         :type :mandel}
   ;Moves, zooms, on constant Julia coeff
   :c3  {:x0           -1.0
         :y0           0.5397
         :xf           -1.0
         :yf           0.5397
         :w0           4.0
         :wf           0.004
         :h0           4.0
         :hf           0.004
         :type         :julia
         :julia-coeff0 (c/complex {:a -0.1 :b 0.7 :impl :plain})
         :julia-coefff (c/complex {:a -0.1 :b 0.7 :impl :plain})}
   ;Moves, zooms, and varies Julia coeff (all variations)
   :c4  {:x0           -0.5
         :y0           0.5397
         :xf           0.5
         :yf           -0.5
         :w0           2.0
         :wf           0.1
         :h0           2.0
         :hf           0.1
         :type         :julia
         :julia-coeff0 (c/complex {:a -0.2 :b 0.9 :impl :plain})
         :julia-coefff (c/complex {:a -0.1 :b 0.7 :impl :plain})}

   ;Multidomain
   :c9  {:type :julia
         :multidom
               [{:iter-scale   1.0
                 :x0           -1.0
                 :y0           0.0
                 :xf           1.0
                 :yf           0.0
                 :w0           4.0
                 :wf           4.0
                 :h0           4.0
                 :hf           2.0
                 :type         :julia
                 :julia-coeff0 (c/complex {:a -0.1 :b 1.0 :impl :plain})
                 :julia-coefff (c/complex {:a -0.1 :b 0.6 :impl :plain})}
                {:iter-scale   1.0
                 :x0           1.0
                 :y0           0.0
                 :xf           -1.0
                 :yf           0.0
                 :w0           4.0
                 :wf           4.0
                 :h0           2.0
                 :hf           4.0
                 :type         :julia
                 :julia-coeff0 (c/complex {:a -0.1 :b 0.6 :impl :plain})
                 :julia-coefff (c/complex {:a -0.1 :b 1.0 :impl :plain})}]}

   ;Good points
   :c11 {:type :julia
         :multidom
               [{:iter-scale   2.0
                 :x0           0.0
                 :y0           0.0
                 :xf           0.0
                 :yf           0.0
                 :w0           4.0
                 :wf           4.0
                 :h0           4.0
                 :hf           4.0
                 :type         :julia
                 :julia-coeff0 (c/complex {:a -0.1 :b -0.7 :impl :plain})
                 :julia-coefff (c/complex {:a 0.1 :b -0.7 :impl :plain})}
                {:iter-scale   2.0
                 :x0           0.0
                 :y0           0.0
                 :xf           0.0
                 :yf           0.0
                 :w0           4.0
                 :wf           4.0
                 :h0           4.0
                 :hf           4.0
                 :type         :julia
                 :julia-coeff0 (c/complex {:a 0.1 :b -0.7 :impl :plain})
                 :julia-coefff (c/complex {:a -0.1 :b -0.7 :impl :plain})}]}
   :c12 {:type     :julia
         :palette  :rainbow
         :multidom (->
                     [{:iter-scale   2.0
                       :x0           0.0
                       :y0           0.0
                       :xf           0.0
                       :yf           0.0
                       :w0           3.5
                       :wf           3.5
                       :h0           3.5
                       :hf           3.5
                       :type         :julia
                       :julia-coeff0 (c/complex
                                       {:a 0.0 :b -0.7 :impl :plain})
                       :julia-coefff (c/complex
                                       {:a -0.1 :b -0.7 :impl :plain})}]
                     (multidom-with
                       :fractal-coeff-change
                       (c/complex {:a -0.1 :b 0.7 :impl :plain}))
                     (multidom-with
                       :fractal-coeff-change
                       (c/complex {:a 0.0 :b 0.7 :impl :plain}))
                     (multidom-with
                       :fractal-coeff-change
                       (c/complex {:a -0.1 :b 0.7 :impl :plain}))
                     (multidom-with
                       :fractal-coeff-change
                       (c/complex {:a -0.1 :b -0.7 :impl :plain}))
                     (multidom-with
                       :fractal-coeff-change
                       (c/complex {:a 0.0 :b -0.7 :impl :plain})))}
   :c13 {:type     :julia
         :palette  :rainbow2
         :multidom (->
                     [{:iter-scale   15.0
                       :x0           0.0
                       :y0           0.0
                       :xf           0.0
                       :yf           0.0
                       :w0           3.5
                       :wf           3.5
                       :h0           3.5
                       :hf           3.5
                       :type         :julia
                       :julia-coeff0 (c/complex
                                       {:a -0.08 :b -0.65 :impl :plain})
                       :julia-coefff (c/complex
                                       {:a -0.01 :b -0.72 :impl :plain})}]
                     (multidom-with
                       :fractal-coeff-change
                       (c/complex {:a -0.05 :b -0.66 :impl :plain}))
                     (multidom-with
                       :fractal-coeff-change
                       (c/complex {:a -0.11 :b -0.71 :impl :plain}))
                     )}})


(def render-config
  {:fast   {:test-size    20000
            :views        10
            :img-w        100
            :img-h        100
            :img-scale-fn #(imgz/scale % 12)}
   :medium {:test-size    500000
            :views        10
            :img-w        400
            :img-h        400
            :img-scale-fn #(imgz/scale % 3)}
   :slow   {:test-size    5000000
            :views        20
            :img-w        1200
            :img-h        1200
            :img-scale-fn identity}
   :render {:test-size    20000000
            :views        20
            :img-w        1600
            :img-h        1600
            :img-scale-fn identity}
   :insane {:test-size    100000000
            :views        3
            :img-w        2400
            :img-h        2400
            :img-scale-fn identity}})


(defn intensities-random-sample [xrange yrange iters julia-coeff render-size test-size]
  (->>
    (range test-size)
    (pmap
      (fn [i]
        [i
         (fspec/i->x (rand-int render-size) render-size xrange)
         (fspec/i->x (rand-int render-size) render-size yrange)]))
    (filter
      (fn [[i x y]]
        (> 2.0 (mag (c/complex {:a x :b y})))))
    (pmap
      (fn [[i x y]]
        (when (= 0 (rem i (int (/ test-size 20))))
          (println (Date.) " Computing " (int (* 100 (/ i test-size)))
                   "% complete "))
        [x y
         (f/compute-iters-julia
           x
           y
           :plain
           iters
           julia-coeff)]))
    (filter #(not (zero? (nth % 2))))))

(defn run []
  (let [gen-config   :rand-sample
        fconfig      :c13
        rconfig      :insane
        dir          (str
                       "gen-img-test/test-" (name fconfig) "-"
                       (name rconfig) "-" (System/currentTimeMillis))
        {:keys [type multidom palette]
         :or   {palette :green-red}
         :as   fconfig-data} (fractal-config fconfig)
        {:keys
         [test-size views img-w
          img-h img-scale-fn]} (render-config rconfig)
        fconfig-data (assoc fconfig-data :views views)
        domans       (if multidom
                       (combine-domains multidom views)
                       (domains fconfig-data))
        frac-spec    (case type
                       :julia ::fspec/interesting-intensity-plain-julia
                       ;TODO mandel specs
                       ;:mandel :TODO
                       )
        imgc*        (atom 1)]
    (if-not (fs/exists? dir)
      (fs/mkdir dir))
    (doseq [[xrange yrange iters julia-coeff] domans]
      (println "Domain, iters, size : " xrange yrange iters test-size)
      (when julia-coeff
        (reset! fspec/JULIA-COEFF-PLAIN* julia-coeff))
      (reset! fspec/X-RANGE* xrange)
      (reset! fspec/Y-RANGE* yrange)
      (reset! fspec/MAX-CONV-ITERS* iters)
      (let [image       (BufferedImage.
                          img-w
                          img-h
                          BufferedImage/TYPE_INT_ARGB)
            intensities (case gen-config
                          :spec-sample (intensities-gen-spec
                                         (s/gen frac-spec)
                                         test-size)
                          :rand-sample (intensities-random-sample
                                         xrange yrange iters
                                         julia-coeff 1000000 test-size))

            imgf        (str dir (format "/img-%04d" @imgc*) ".png")]
        (draw-intensities-w-raster intensities image img-w img-h palette)
        (imgz/save (img-scale-fn image) imgf)
        (println "Wrote: " imgf)
        (when (or (= :insane rconfig)
                  (= :render rconfig)
                  (= :slow rconfig))
          (animate-results-ffmpeg dir (str dir "/fractal-part-" @imgc* ".mp4")))
        (swap! imgc* inc)))
    (animate-results-ffmpeg dir (str dir "/fractal-all.mp4"))))

(comment
  (run))