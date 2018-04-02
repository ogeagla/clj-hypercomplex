(ns og.clj-cayley-dickson.construction
  (:require [og.clj-cayley-dickson.core :refer :all]))


; :impl can be in #{:plain :apache}

(defn complex [{:keys
                    [a b
                     impl]
                :as params
                :or {impl :plain}}]
  (init-complex2 a b impl))

(defn quaternion [{:keys
                       [a b c d
                        impl]
                   :as params
                   :or {impl :plain}}]
  (init-construction
    (complex
      {:a a :b b :impl impl})
    (complex
      {:a c :b d :impl impl})))

(defn octonion [{:keys
                     [a b c d
                      e f g h impl]
                 :as params
                 :or {impl :plain}}]
  (init-construction
    (quaternion
      {:a a :b b :c c :d d :impl impl})
    (quaternion
      {:a e :b f :c g :d h :impl impl})))

(defn sedenion [{:keys
                     [a b c d
                      e f g h
                      i j k l
                      m n o p
                      impl]
                 :as params
                 :or {impl :plain}}]
  (init-construction
    (octonion
      {:a a :b b :c c :d d :e e :f f :g g :h h :impl impl})
    (octonion
      {:a i :b j :c k :d l :e m :f n :g o :h p :impl impl})))

(defn pathion [{:keys
                    [a b c d e f g h
                     i j k l m n o p
                     q r s t u v w x
                     y z aa bb cc dd ee ff
                     impl]
                :as params
                :or {impl :plain}}]
  (init-construction
    (sedenion
      {:a    a :b b :c c :d d :e e :f f :g g :h h
       :i    i :j j :k k :l l :m m :n n :o o :p p
       :impl impl})
    (sedenion
      {:a    q :b r :c s :d t :e u :f v :g w :h x
       :i    y :j z :k aa :l bb :m cc :n dd :o ee :p ff
       :impl impl})))

