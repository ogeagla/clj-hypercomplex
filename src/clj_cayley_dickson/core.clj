(ns clj-cayley-dickson.core
  (:require [clojure.pprint :refer :all])
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
    ;new.a = self.a * other.a - other.b * self.b
    ;new.b = self.a * other.b + other.a * self.b
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
    ;new.a = self.a * other.a - other.b.c() * self.b
    ;new.b = other.b * self.a + self.b * other.a.c()
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
    ;(println "*GETIDX " idx this)
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
    ;(println "*SETIDX " idx this new-val)
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
    ;(println "*SCALE " this s)
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
    ;(println "*NORM" this)
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
    ;(println "* INV" this)
    (scale (c this)
           (/ 1.0
              (norm this))))
  (rot [this other]
    (times
      (times other
             this)
      (inv other))))

(defn complex2 [a b]
  (init (->Complex2 a b)))

(defn complex4 [a b c d]
  (init (->Construction
          (complex2 a b)
          (complex2 c d))))

(defn complex8 [a b c d e f g h]
  (init (->Construction
          (complex4 a b c d)
          (complex4 e f g h))))

(defn complex16 [a b c d e f g h
                 i j k l m n o p]
  (init (->Construction
          (complex8 a b c d e f g h)
          (complex8 i j k l m n o p))))

(println "order 2:" (complex2 1 1))

(println
  "times order 2:")

(pprint
  (times (complex2 1 1)
         (complex2 1 1)))

(println
  "order 4:"
  (complex4 1 1 1 1))

(println
  "times order 4:")

(pprint
  (times (complex4 1 1 1 1)
         (complex4 1 1 1 1)))

(println "order 8: " (complex8 1 1 1 1 1 1 1 1))

(println
  "times order 8:")
(pprint
  (times (complex8 1 1 1 1 1 1 1 1)
         (complex8 1 1 1 1 1 1 1 1)))

(println "inv: " (inv (complex4 1 1 1 1)))

(println "norm: " (norm (complex4 1 1 1 1)))

(println "mag: " (mag (complex4 1 1 1 1)))

(println "scale: " (scale (complex4 1 1 1 1) 0.5))
