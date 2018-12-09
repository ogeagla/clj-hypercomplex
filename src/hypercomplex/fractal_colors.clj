(ns hypercomplex.fractal-colors)

(def MAX-COLOR-YELLOW (int-array [220 230 22 255]))

(defn palette-green-to-red [iters max-iters]
  (if (or #_(< iterations 10)
        (= iters max-iters))
    MAX-COLOR-YELLOW
    (let [total-intensity (int (/ (* iters 255) max-iters))
          r               (min
                            (int (+ 20 total-intensity)) 255)
          g               (min
                            (int (- 255 total-intensity)) 255)
          b               (min
                            (int (Math/abs (- total-intensity 80))) 255)]
      (int-array [r g b 255]))))

(defn palette-rainbox [iters max-iters]
  (let [total-intensity (/ iters max-iters)
        [r g b imax] (cond
                       (<= 0.0 total-intensity 0.2) [3 146 207 0.2]
                       (<= 0.2 total-intensity 0.4) [123 192 67 0.4]
                       (<= 0.4 total-intensity 0.6) [253 244 152 0.6]
                       (<= 0.6 total-intensity 0.8) [243 119 54 0.8]
                       (<= 0.8 total-intensity 1.0) [238 64 53 1.0])
        [r g b] (->>
                  [r g b]
                  (map
                    #(* % (/ total-intensity imax)))
                  (map int))]
    (int-array [r g b 255])))

(defn palette-rainbox2 [iters max-iters]
  (let [pct-intensity (/ iters max-iters)
        [r g b] (cond
                  (<= 0.0 pct-intensity 0.04) [3 146 207]

                  (<= 0.04 pct-intensity 0.08) [10 156 160]
                  (<= 0.08 pct-intensity 0.12) [20 166 120]
                  (<= 0.12 pct-intensity 0.16) [40 172 90]
                  (<= 0.16 pct-intensity 0.2) [80 182 75]

                  (<= 0.2 pct-intensity 0.24) [123 192 67]

                  (<= 0.24 pct-intensity 0.28) [143 200 87]
                  (<= 0.28 pct-intensity 0.32) [163 210 117]
                  (<= 0.32 pct-intensity 0.36) [193 224 122]
                  (<= 0.36 pct-intensity 0.4) [223 234 152]

                  (<= 0.4 pct-intensity 0.44) [253 244 152]

                  (<= 0.44 pct-intensity 0.48) [250 204 122]
                  (<= 0.48 pct-intensity 0.52) [248 164 90]
                  (<= 0.52 pct-intensity 0.56) [246 149 74]
                  (<= 0.56 pct-intensity 0.6) [243 129 64]

                  (<= 0.6 pct-intensity 0.64) [243 119 54]

                  (<= 0.64 pct-intensity 0.68) [242 90 54]
                  (<= 0.68 pct-intensity 0.72) [241 80 54]
                  (<= 0.72 pct-intensity 0.76) [240 75 53]
                  (<= 0.76 pct-intensity 0.8) [239 70 53]

                  (<= 0.8 pct-intensity 0.84) [238 64 53]

                  (<= 0.84 pct-intensity 0.88) [242 64 53]
                  (<= 0.88 pct-intensity 0.92) [246 63 52]
                  (<= 0.92 pct-intensity 0.96) [248 62 51]
                  (<= 0.96 pct-intensity 1.00) [252 61 50]
                  )]
    (int-array [r g b 255])))


