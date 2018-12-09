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
                  (zero? pct-intensity) [0 0 255]
                  (<= 0.0 pct-intensity 0.01) [3 146 207]
                  (<= 0.01 pct-intensity 0.02) [10 149 200]
                  (<= 0.02 pct-intensity 0.03) [15 152 190]
                  (<= 0.03 pct-intensity 0.04) [20 155 180]
                  (<= 0.04 pct-intensity 0.05) [25 160 175]
                  (<= 0.05 pct-intensity 0.06) [30 165 171]
                  (<= 0.06 pct-intensity 0.07) [35 168 168]
                  (<= 0.07 pct-intensity 0.08) [40 171 164]
                  (<= 0.08 pct-intensity 0.09) [45 175 160]
                  (<= 0.09 pct-intensity 0.1) [50 180 154]
                  (<= 0.1 pct-intensity 0.11) [55 182 144]
                  (<= 0.11 pct-intensity 0.12) [60 184 134]
                  (<= 0.12 pct-intensity 0.13) [70 186 124]
                  (<= 0.13 pct-intensity 0.14) [80 188 114]
                  (<= 0.14 pct-intensity 0.15) [90 192 110]
                  (<= 0.15 pct-intensity 0.16) [100 194 105]
                  (<= 0.16 pct-intensity 0.17) [110 196 100]
                  (<= 0.17 pct-intensity 0.18) [120 197 95]
                  (<= 0.18 pct-intensity 0.19) [130 198 90]
                  (<= 0.19 pct-intensity 0.2) [143 200 87]

                  (<= 0.19 pct-intensity 0.2) [148 202 90]
                  (<= 0.20 pct-intensity 0.21) [153 204 94]
                  (<= 0.21 pct-intensity 0.22) [158 206 97]
                  (<= 0.22 pct-intensity 0.23) [162 208 102]
                  (<= 0.23 pct-intensity 0.24) [166 210 106]
                  (<= 0.24 pct-intensity 0.25) [171 212 110]
                  (<= 0.25 pct-intensity 0.26) [175 214 114]
                  (<= 0.26 pct-intensity 0.27) [180 216 117]
                  (<= 0.27 pct-intensity 0.28) [187 219 120]
                  (<= 0.28 pct-intensity 0.29) [191 222 123]
                  (<= 0.29 pct-intensity 0.3) [195 225 126]
                  (<= 0.3 pct-intensity 0.31) [199 228 129]
                  (<= 0.31 pct-intensity 0.32) [202 231 133]
                  (<= 0.32 pct-intensity 0.33) [208 232 136]
                  (<= 0.33 pct-intensity 0.34) [215 235 139]
                  (<= 0.34 pct-intensity 0.35) [219 237 143]
                  (<= 0.35 pct-intensity 0.36) [223 238 144]
                  (<= 0.36 pct-intensity 0.37) [228 239 146]
                  (<= 0.37 pct-intensity 0.38) [235 240 148]
                  (<= 0.38 pct-intensity 0.39) [241 241 150]
                  (<= 0.39 pct-intensity 0.4) [245 242 151]


                  (<= 0.4 pct-intensity 0.41) [253 244 152]

                  (<= 0.41 pct-intensity 0.42) [252 238 148]
                  (<= 0.42 pct-intensity 0.43) [251 233 143]
                  (<= 0.43 pct-intensity 0.44) [250 228 138]
                  (<= 0.44 pct-intensity 0.45) [249 222 133]
                  (<= 0.45 pct-intensity 0.46) [248 218 128]
                  (<= 0.46 pct-intensity 0.47) [247 210 122]
                  (<= 0.47 pct-intensity 0.48) [246 204 118]
                  (<= 0.48 pct-intensity 0.49) [245 195 113]
                  (<= 0.49 pct-intensity 0.5) [244 190 108]
                  (<= 0.5 pct-intensity 0.51) [244 180 103]
                  (<= 0.51 pct-intensity 0.52) [244 175 98]
                  (<= 0.52 pct-intensity 0.53) [244 170 93]
                  (<= 0.53 pct-intensity 0.54) [244 160 88]
                  (<= 0.54 pct-intensity 0.55) [244 155 83]
                  (<= 0.55 pct-intensity 0.56) [244 150 78]
                  (<= 0.56 pct-intensity 0.57) [244 145 72]
                  (<= 0.57 pct-intensity 0.58) [244 140 65]
                  (<= 0.58 pct-intensity 0.59) [244 135 62]
                  (<= 0.59 pct-intensity 0.6) [244 125 59]


                  (<= 0.6 pct-intensity 0.61) [243 119 54]

                  (<= 0.61 pct-intensity 0.62) [242 117 54]
                  (<= 0.62 pct-intensity 0.63) [241 115 54]
                  (<= 0.63 pct-intensity 0.64) [240 113 54]
                  (<= 0.64 pct-intensity 0.65) [239 110 54]
                  (<= 0.65 pct-intensity 0.66) [238 107 54]
                  (<= 0.66 pct-intensity 0.67) [238 104 54]
                  (<= 0.67 pct-intensity 0.68) [238 100 54]
                  (<= 0.68 pct-intensity 0.69) [238 98 54]
                  (<= 0.69 pct-intensity 0.7) [238 94 54]
                  (<= 0.7 pct-intensity 0.71) [238 90 54]
                  (<= 0.71 pct-intensity 0.72) [238 85 54]
                  (<= 0.72 pct-intensity 0.73) [238 82 54]
                  (<= 0.73 pct-intensity 0.74) [238 80 54]
                  (<= 0.74 pct-intensity 0.75) [238 78 54]
                  (<= 0.75 pct-intensity 0.76) [238 74 54]
                  (<= 0.76 pct-intensity 0.77) [238 72 54]
                  (<= 0.77 pct-intensity 0.78) [238 70 54]
                  (<= 0.78 pct-intensity 0.79) [238 68 54]
                  (<= 0.79 pct-intensity 0.8) [238 66 54]

                  (<= 0.8 pct-intensity 0.81) [239 64 53]
                  (<= 0.81 pct-intensity 0.82) [240 62 53]
                  (<= 0.82 pct-intensity 0.83) [241 60 53]
                  (<= 0.83 pct-intensity 0.84) [242 58 53]
                  (<= 0.84 pct-intensity 0.85) [243 55 52]
                  (<= 0.85 pct-intensity 0.86) [244 53 52]
                  (<= 0.86 pct-intensity 0.87) [245 50 52]
                  (<= 0.87 pct-intensity 0.88) [246 48 52]
                  (<= 0.88 pct-intensity 0.89) [247 46 52]
                  (<= 0.89 pct-intensity 0.9) [248 42 52]
                  (<= 0.9 pct-intensity 0.91) [249 38 52]
                  (<= 0.91 pct-intensity 0.92) [250 33 51]
                  (<= 0.92 pct-intensity 0.93) [251 28 51]
                  (<= 0.93 pct-intensity 0.94) [253 22 50]
                  (<= 0.94 pct-intensity 0.95) [254 18 49]
                  (<= 0.95 pct-intensity 0.96) [254 15 48]
                  (<= 0.96 pct-intensity 0.97) [250 12 47]
                  (<= 0.97 pct-intensity 0.98) [245 9 46]
                  (<= 0.98 pct-intensity 0.99) [240 6 45]

                  (<= 0.99 pct-intensity 1.00) [235 3 44]
                  )]
    (int-array [r g b 255])))


