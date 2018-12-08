(ns hypercomplex.gen-img-test
  (:require [clojure.test :refer :all]
            [clojure.java.shell :refer [sh]]
            [mikera.image.core :as imgz]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [hypercomplex.gen-test :as gt]
            [hypercomplex.cayley-dickson-construction :as c]
            [me.raynes.fs :as fs])
  (:import (java.awt.image BufferedImage WritableRaster)
           (java.awt Graphics Color)))


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

(defn img-intensities [intensities surface-width surface-height]
  (pmap
    (fn [[x y iters]]
      (let [i       (intensity-coord->img-coord
                      x surface-width @gt/X-RANGE* false)
            j       (intensity-coord->img-coord
                      y surface-height @gt/Y-RANGE* true)
            ^ints c (calc-pixel-color-argb
                      iters @gt/MAX-CONV-ITERS*)]
        [i j c]))
    intensities))

(defn- draw-intensities-w-raster
  [intensities ^BufferedImage image surface-width surface-height]
  ; Maybe look at this if performance is not improving with this technique:
  ; https://stackoverflow.com/questions/25178810/create-a-writableraster-based-on-int-array
  (let [^WritableRaster raster (.getRaster image)
        ^Graphics g            (.getGraphics image)
        img-vals               (img-intensities
                                 intensities surface-width surface-height)
        x-zero                 (intensity-coord->img-coord 0 surface-width @gt/X-RANGE* false)
        y-zero                 (intensity-coord->img-coord 0 surface-height @gt/Y-RANGE* true)]
    (doseq [[^int i ^int j ^ints argb] img-vals]
      (.setPixel raster i j argb))
    (.setColor g Color/BLACK)
    (.drawLine g
               0 y-zero
               surface-width y-zero)
    (.drawLine g
               x-zero 0
               x-zero surface-height)))


(defn pgen-spec [generator quant]
  (pmap
    (fn [i]
      (when (= 0 (mod i (int (/ quant 5))))
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
   ;Varies Julia fractal params only.
   :c5  {
         :x0           0.0
         :y0           0.0
         :xf           0.0
         :yf           -0.0
         :w0           2.0
         :wf           2.0
         :h0           2.0
         :hf           2.0
         :type         :julia
         :julia-coeff0 (c/complex {:a -0.2 :b 0.9 :impl :plain})
         :julia-coefff (c/complex {:a -0.1 :b 0.7 :impl :plain})}

   ;Varies Julia fractal params in `a` only
   :c6  {
         :x0           0.0
         :y0           0.0
         :xf           0.0
         :yf           -0.0
         :w0           2.0
         :wf           2.0
         :h0           2.0
         :hf           2.0
         :type         :julia
         :julia-coeff0 (c/complex {:a -0.2 :b 0.7 :impl :plain})
         :julia-coefff (c/complex {:a 0.2 :b 0.7 :impl :plain})}


   ;Varies Julia fractal params in `b` only
   :c7  {
         :x0           0.0
         :y0           0.0
         :xf           0.0
         :yf           -0.0
         :w0           2.0
         :wf           2.0
         :h0           2.0
         :hf           2.0
         :type         :julia
         :julia-coeff0 (c/complex {:a -0.1 :b 0.7 :impl :plain})
         :julia-coefff (c/complex {:a -0.1 :b -0.7 :impl :plain})}

   ;All variations Julia
   :c8  {:iter-scale   2.0
         :x0           0.0
         :y0           0.0
         :xf           0.0
         :yf           0.0
         :w0           4.0
         :wf           4.0
         :h0           4.0
         :hf           4.0
         :type         :julia
         :julia-coeff0 (c/complex {:a -0.1 :b 1.0 :impl :plain})
         :julia-coefff (c/complex {:a -0.1 :b 0.6 :impl :plain})}

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

   :c10 {:type :julia
         :multidom
               [{:iter-scale   1.0
                 :x0           0.0
                 :y0           0.0
                 :xf           0.0
                 :yf           0.0
                 :w0           4.0
                 :wf           4.0
                 :h0           4.0
                 :hf           4.0
                 :type         :julia
                 :julia-coeff0 (c/complex {:a -0.1 :b -1.0 :impl :plain})
                 :julia-coefff (c/complex {:a -0.1 :b 1.0 :impl :plain})}
                {:iter-scale   1.0
                 :x0           0.0
                 :y0           0.0
                 :xf           0.0
                 :yf           0.0
                 :w0           4.0
                 :wf           4.0
                 :h0           4.0
                 :hf           4.0
                 :type         :julia
                 :julia-coeff0 (c/complex {:a -0.1 :b 1.0 :impl :plain})
                 :julia-coefff (c/complex {:a -0.1 :b -1.0 :impl :plain})}]}

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
   :c12 {:type :julia
         :multidom
               [{:iter-scale   1.0
                 :x0           0.0
                 :y0           0.0
                 :xf           0.0
                 :yf           0.0
                 :w0           3.5
                 :wf           3.5
                 :h0           3.5
                 :hf           3.5
                 :type         :julia
                 :julia-coeff0 (c/complex {:a 0.0 :b -0.7 :impl :plain})
                 :julia-coefff (c/complex {:a -0.1 :b -0.7 :impl :plain})}
                {:iter-scale   1.0
                 :x0           0.0
                 :y0           0.0
                 :xf           0.0
                 :yf           0.0
                 :w0           3.5
                 :wf           3.5
                 :h0           3.5
                 :hf           3.5
                 :type         :julia
                 :julia-coeff0 (c/complex {:a -0.1 :b -0.7 :impl :plain})
                 :julia-coefff (c/complex {:a -0.1 :b 0.7 :impl :plain})}
                {:iter-scale   1.0
                 :x0           0.0
                 :y0           0.0
                 :xf           0.0
                 :yf           0.0
                 :w0           3.5
                 :wf           3.5
                 :h0           3.5
                 :hf           3.5
                 :type         :julia
                 :julia-coeff0 (c/complex {:a -0.1 :b 0.7 :impl :plain})
                 :julia-coefff (c/complex {:a 0.0 :b 0.7 :impl :plain})}
                {:iter-scale   1.0
                 :x0           0.0
                 :y0           0.0
                 :xf           0.0
                 :yf           0.0
                 :w0           3.5
                 :wf           3.5
                 :h0           3.5
                 :hf           3.5
                 :type         :julia
                 :julia-coeff0 (c/complex {:a 0.0 :b 0.7 :impl :plain})
                 :julia-coefff (c/complex {:a -0.1 :b 0.7 :impl :plain})}
                {:iter-scale   1.0
                 :x0           0.0
                 :y0           0.0
                 :xf           0.0
                 :yf           0.0
                 :w0           3.5
                 :wf           3.5
                 :h0           3.5
                 :hf           3.5
                 :type         :julia
                 :julia-coeff0 (c/complex {:a -0.1 :b 0.7 :impl :plain})
                 :julia-coefff (c/complex {:a -0.1 :b -0.7 :impl :plain})}
                {:iter-scale   1.0
                 :x0           0.0
                 :y0           0.0
                 :xf           0.0
                 :yf           0.0
                 :w0           3.5
                 :wf           3.5
                 :h0           3.5
                 :hf           3.5
                 :type         :julia
                 :julia-coeff0 (c/complex {:a -0.1 :b -0.7 :impl :plain})
                 :julia-coefff (c/complex {:a 0.0 :b -0.7 :impl :plain})}]}})

(def render-config
  {:fast   {:test-size    10000
            :views        20
            :img-w        200
            :img-h        200
            :img-scale-fn #(imgz/scale % 6)}
   :medium {:test-size    500000
            :views        20
            :img-w        400
            :img-h        400
            :img-scale-fn #(imgz/scale % 3)}
   :slow   {:test-size    1000000
            :views        20
            :img-w        1200
            :img-h        1200
            :img-scale-fn identity}
   :render {:test-size    10000000
            :views        20
            :img-w        1600
            :img-h        1600
            :img-scale-fn identity}})

(defn run []
  (let [dir          (str "gen-img-test/test-" (System/currentTimeMillis))
        fconfig      :c12
        rconfig      :render

        {:keys [type multidom]
         :as   fconfig-data} (fractal-config fconfig)
        {:keys
         [test-size views img-w
          img-h img-scale-fn]} (render-config rconfig)

        fconfig-data (assoc fconfig-data :views views)
        domans       (if multidom
                       (combine-domains multidom views)
                       (domains fconfig-data))
        f-spec       (case type
                       :julia ::gt/interesting-intensity-plain-julia
                       ;;
                       ;;
                       ;TODO mandel specs
                       ;;
                       ;;
                       :mandel :TODO)
        imgc*        (atom 1)]
    (if-not (fs/exists? dir)
      (fs/mkdir dir))
    (doseq [[xrange yrange iters julia-coeff] domans]
      (println "Domain, iters, size : " xrange yrange iters test-size)
      (when julia-coeff
        (reset! gt/JULIA-COEFF-PLAIN* julia-coeff))
      (reset! gt/X-RANGE* xrange)
      (reset! gt/Y-RANGE* yrange)
      (reset! gt/MAX-CONV-ITERS* iters)
      (let [image (BufferedImage.
                    img-w
                    img-h
                    BufferedImage/TYPE_INT_ARGB)
            its   (pgen-spec
                    (s/gen
                      f-spec)
                    test-size)
            imgf  (str dir (format "/img-%04d" @imgc*) ".png")]
        (draw-intensities-w-raster its image img-w img-h)
        ;(imgz/show image)
        (imgz/save (img-scale-fn image) imgf)
        (println "Wrote: " imgf)
        (when (or (= :render rconfig)
                  (= :slow rconfig))
          #_(animate-results-gif (str dir "/*.png")
                               (str dir "/fractal-part-" @imgc* ".gif"))
          (animate-results-ffmpeg dir
                                  (str dir "/fractal-part-" @imgc* ".mp4")))
        (swap! imgc* inc)))
    #_(animate-results-gif (str dir "/*.png")
                         (str dir "/fractal-all.gif"))
    (animate-results-ffmpeg dir
                            (str dir "/fractal-all.mp4"))))

(deftest gen-img-test
  (testing "Generates fractal image"
    (println "Generating frac")
    (run)))

(run-tests 'hypercomplex.gen-img-test)