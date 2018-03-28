(ns clj-cayley-dickson.core
  (:gen-class))

; inspired by https://github.com/hamiltron/py-cayleydickson/blob/master/cayleydickson.py

(defn -main
  [& args]
  (println "Hello, World!"))

(defprotocol Nion
  (init [this])
  (c [this])
  (times [this other])
  (neg [this])
  (plus [this other])
  (minus [this other])
  (valid-idx? [this idx])
  (get-idx [this idx])
  (set-idx [this idx new-val]))

(defprotocol NionOps
  (mag [this])
  (scale [this s])
  (norm [this])
  (inv [this])
  (rot [this other]))

(defrecord Complex2 [a b]
  Nion
  (init [this]
    (assoc this :order 2))
  (c [this]
    (assoc this :b (* -1
                      (:b this))))
  (neg [this]
    (assoc this :a (* -1
                      (:a this))
                :b (* -1
                      (:b this))))

  (times [this other]
    (assoc this :a (- (* (:a this) (:a other))
                      (* (:b other) (:b this)))
                :b (+ (* (:a this) (:b other))
                      (* (:a other) (:b this)))))
  (plus [this other]
    (assoc this :a (+ (:a this)
                      (:a other))
                :b (+ (:b this)
                      (:b other))))
  (minus [this other]
    (plus this (neg other)))

  (valid-idx? [this idx]
    (if-not
      (and
        (contains? #{0 1} idx)
        (:order this))
      (do (println "C2 Index must be int 0 or 1: " idx)
          false)
      true))

  (get-idx [this idx]
    (when (valid-idx? this idx)
      (if (< idx
             (/ (:order this)
                2))
        (:a this)
        (:b this))))
  (set-idx [this idx new-val]
    (when (valid-idx? this idx)
      (if (< idx
             (/ (:order this)
                2))
        (assoc this :a new-val)
        (assoc this :b new-val)))))

(defn eq-order? [a b]
  (and
    (:order a)
    (:order b)
    (= (:order a) (:order b))))

(defrecord Construction [a b]
  Nion
  (init [this]
    (if-not (eq-order? a b)
      (println "Orders don't match or missing" a b)
      (assoc this
        :order (* 2 (:order a)))))
  (c [this]
    (assoc this :a (c (:a this))
                :b (neg (:b this))))
  (times [this other]
    (if-not (eq-order? a b)
      (println "Orders don't match or missing" a b)
      (assoc this :a (minus
                       (times (:a this) (:a other))
                       (times (c (:b other)) (:b this)))
                  :b (plus
                       (times (:b other) (:a this))
                       (times (:b this) (c (:a other)))))))
  (neg [this]
    (assoc this :a (neg (:a this))
                :b (neg (:b this))))
  (plus [this other]
    (if-not (eq-order? a b)
      (println "Orders don't match or missing" a b)
      (assoc this :a (plus (:a this)
                           (:a other))
                  :b (plus (:b this)
                           (:b other)))))
  (minus [this other]
    (plus this (neg other)))

  (valid-idx? [this idx]
    (if-not (and
              (:order this)
              (= idx
                 (int idx))
              (>= idx 0)
              (<= idx (:order this)))
      (do (println "Construction Index must be int less than order: " idx (:order this))
          false)
      true))
  (get-idx [this idx]
    (when (valid-idx? this idx)
      (let [new-idx (mod idx
                         (/ (:order this)
                            2))
            retval  (if (< idx
                           (/ (:order this)
                              2))
                      (get-idx (:a this) new-idx)
                      (get-idx (:b this) new-idx))]
        retval)))

  (set-idx [this idx new-val]
    (when (valid-idx? this idx)
      (let [new-idx (mod idx (/ (:order this) 2))]
        (if (< idx
               (/ (:order this)
                  2))
          (assoc this :a (set-idx (:a this) new-idx new-val))
          (assoc this :b (set-idx (:b this) new-idx new-val))))))

  NionOps
  (mag [this]
    (->
      this
      norm
      Math/sqrt))
  (scale [this s]
    (loop [new-this this
           idx      (dec (:order this))]
      (let [new-new-this (set-idx
                           new-this
                           idx
                           (*
                             s
                             (get-idx new-this idx)))]
        (if (pos? idx)
          (recur new-new-this (dec idx))
          new-new-this))))
  (norm [this]
    (loop [sum 0
           idx (dec (:order this))]
      (let [new-sum (+
                      sum
                      (* (get-idx this idx)
                         (get-idx this idx)))]
        (if (pos? idx)
          (recur new-sum (dec idx))
          new-sum))))
  (inv [this]
    (scale (c this)
           (/ 1.0
              (norm this))))
  (rot [this other]
    (times
      (times other
             this)
      (inv other))))

(defn- init-complex2 [a b]
  (init
    (->Complex2 a b)))

(defn- init-construction [a b]
  (init
    (->Construction a b)))

(defn complex [{:keys
                [a b]
                :as
                params}]
  (init-complex2 a b))

(defn quaternion [{:keys
                   [a b c d]
                   :as
                   params}]
  (init-construction
    (complex
      {:a a :b b})
    (complex
      {:a c :b d})))

(defn octonion [{:keys
                 [a b c d
                  e f g h]
                 :as
                 params}]
  (init-construction
    (quaternion
      {:a a :b b :c c :d d})
    (quaternion
      {:a e :b f :c g :d h})))

(defn sedonion [{:keys
                 [a b c d
                  e f g h
                  i j k l
                  m n o p]
                 :as
                 params}]
  (init-construction
    (octonion
      {:a a :b b :c c :d d :e e :f f :g g :h h})
    (octonion
      {:a i :b j :c k :d l :e m :f n :g o :h p})))

