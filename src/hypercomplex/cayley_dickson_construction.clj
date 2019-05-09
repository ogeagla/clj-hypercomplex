(ns hypercomplex.cayley-dickson-construction
  (:require [hypercomplex.core :refer :all]))

(defn complex
  [{:keys
        [a b
         impl]
    :as params
    :or {impl :plain}}]
  (init-complex2 a b impl))

(defn quaternion
  [{:keys
        [a b c d
         impl]
    :as params
    :or {impl :plain}}]
  (init-construction
    (complex
      {:a a :b b :impl impl})
    (complex
      {:a c :b d :impl impl})))

(defn octonion
  [{:keys
        [a b c d
         e f g h impl]
    :as params
    :or {impl :plain}}]
  (init-construction
    (quaternion
      {:a a :b b :c c :d d :impl impl})
    (quaternion
      {:a e :b f :c g :d h :impl impl})))

(defn sedenion
  [{:keys
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

(defn pathion
  [{:keys
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

(defn power-of? [number base]
  (or
    (= 1 number)
    (= 1.0 number)
    (let [log-base-of-number (/
                               (Math/log number)
                               (Math/log base))]
      (zero?
        (- log-base-of-number
           (int log-base-of-number))))))

(defn n-hypercomplex
  "Provide a power-of-2-sized vector of coefficients
  from which we generate a hypercomplex number. If the
  coefficient vector does not contain a power-of-2
  amount of elements, an Exception is thrown."
  [coeffs impl]
  (if-not
    (and
      (power-of? (count coeffs) 2)
      (< 1 (count coeffs)))
    (let [err-str (str
                    "Fn n-hypercomplex requires coefficients count
                    to be a power of 2, and GT 1. Provided: "
                    (vec coeffs))]
      (println
        err-str)
      (throw
        (Exception.
          err-str)))
    (if (= 2 (count coeffs))
      (complex {:a (nth coeffs 0) :b (nth coeffs 1) :impl impl})
      (let [[first-half-coeffs
             second-half-coeffs] (split-at
                                   (int (/ (count coeffs) 2))
                                   coeffs)]
        (init-construction
          (n-hypercomplex first-half-coeffs impl)
          (n-hypercomplex second-half-coeffs impl))))))