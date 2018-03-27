(ns clj-cayley-dickson.core
  (:gen-class))

(defn -main
  [& args]
  (println "Hello, World!"))

(defprotocol Nion
  (init [this])
  (c [this])
  (times [this other])
  (neg [this])
  (plus [this other])
  (minus [this other]))

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
    (plus this (neg other))))

(defrecord Construction [a b]
  Nion
  (init [this]
    (if-not (and
              (:order a)
              (:order b)
              (= (:order a) (:order b)))
      (println "Orders don't match or missing" a b)
      (assoc this
        :order (* 2 (:order a)))))
  (c [this]
    ;new.a = self.a.c()
    ;new.b = -(self.b)
    (assoc this :a (c (:a this))
                :b (neg (:b this)))
    )
  (times [this other]
    ;new.a = self.a * other.a - other.b.c() * self.b
    ;new.b = other.b * self.a + self.b * other.a.c()

    (assoc this :a (minus
                     (times (:a this) (:a other))
                     (times (c (:b other)) (:b this)))
                :b (plus
                     (times (:b other) (:a this))
                     (times (:b this) (c (:a other)))))
    )
  (neg [this]
    (assoc this :a (neg (:a this))
                :b (neg (:b this))))
  (plus [this other]
    (assoc this :a (plus (:a this)
                         (:a other))
                :b (plus (:b this)
                         (:b other))))
  (minus [this other]
    (plus this (neg other))))

(println
  "order 2:"
  (times (c (init (->Complex2 2.1 3.2)))
         (init (->Complex2 2.1 3.200000000000000001))))

(println
  "order 4:"
  (init (->Construction (c (init (->Complex2 2.1 3.2)))
                        (neg (init (->Complex2 2.1 3.2))))))

(println
  "times: "
  (times
    (init (->Construction (init (->Construction (c (init (->Complex2 2.1 3.2)))
                                                (neg (init (->Complex2 2.1 3.2)))))
                          (init (->Construction (c (init (->Complex2 2.1 3.2)))
                                                (neg (init (->Complex2 2.1 3.2)))))))
    (init (->Construction (init (->Construction (c (init (->Complex2 2.1 3.2)))
                                                (neg (init (->Complex2 2.1 3.2)))))
                          (init (->Construction (c (init (->Complex2 2.1 3.2)))
                                                (neg (init (->Complex2 2.1 3.2)))))))))
(clojure.pprint/pprint
  (times (init
           (->Construction
             (init
               (->Construction
                 (init
                   (->Construction
                     (c (init
                          (->Complex2 2.1 3.2)))
                     (neg (init
                            (->Complex2 2.1 3.2)))))
                 (init
                   (->Construction
                     (c (init
                          (->Complex2 2.1 3.2)))
                     (neg (init (->Complex2 2.1 3.2)))))))
             (init
               (->Construction
                 (init
                   (->Construction
                     (c (init
                          (->Complex2 2.1 3.2)))
                     (neg (init
                            (->Complex2 2.1 3.2)))))
                 (init
                   (->Construction
                     (init
                       (->Complex2 2.1 3.2))
                     (neg (init
                            (->Complex2 2.1 3.2)))))))))
         (init
           (->Construction
             (init
               (->Construction
                 (init
                   (->Construction
                     (c (init
                          (->Complex2 2.1 3.2)))
                     (neg (init
                            (->Complex2 2.1 3.2)))))
                 (init
                   (->Construction
                     (c (init
                          (->Complex2 2.1 3.2)))
                     (init (->Complex2 2.1 3.2))))))
             (init
               (->Construction
                 (init
                   (->Construction
                     (c (init
                          (->Complex2 2.1 3.2)))
                     (neg (init
                            (->Complex2 2.1 3.2)))))
                 (init
                   (->Construction
                     (c (init
                          (->Complex2 2.1 3.2)))
                     (neg (init
                            (->Complex2 2.1 3.2)))))))))))
