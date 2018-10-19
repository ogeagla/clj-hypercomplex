# hypercomplex

[Changelog](CHANGELOG.md)

[![Build Status](https://travis-ci.com/ogeagla/clj-hypercomplex.svg?branch=master)](https://travis-ci.com/ogeagla/clj-hypercomplex)

[![Clojars Project](https://img.shields.io/clojars/v/hypercomplex.svg)](https://clojars.org/hypercomplex)

```clojure
[hypercomplex "0.0.2"]
```

A hypercomplex number library written in Clojure.

Includes Cayley-Dickson construction for generating and working with hypercomplex algebras.

https://en.wikipedia.org/wiki/Cayley%E2%80%93Dickson_construction

![The Hypercomplex Numbers](papers/Figure1.JPG)

## Usage

 - `hypercomplex.core` for operators on or between hypercomplex numbers: `times`, `plus`, `minus`, `neg`, `c` (conjugate), `scale`, `norm`, `inv`, and `mag`.
 - `hypercomplex.cayley-dickson-construction` for constructing hypercomplex numbers: `complex`, `quaternion`, `octonion`, `sedenion`, `pathion`, and `n-hypercomplex` for constructing higher-order algebras of arbitrary order, provided it is a power of 2.
 - All constructions can take an `:impl`, which can be either `:plain`, or `:apache`, which represent either pure Clojure or `org.apache.commons.math3.complex.Complex` implementations to be used under the hood for constructing the hypercomplex numbers.  


An example of creating `quaternion`, performing multiplication, and checking equality as a result of associativity:
```clojure
(:require [hypercomplex.core :refer :all]
          [hypercomplex.cayley-dickson-construction :refer
            [complex quaternion octonion sedenion pathion]])

(is
  (= (times
       (quaternion {:a 1 :b 2 :c 3 :d 4})
       (times (quaternion {:a 8 :b 7 :c 6 :d 5})
              (quaternion {:a 9 :b 10 :c 11 :d 12})))
     (times
       (times
         (quaternion {:a 1 :b 2 :c 3 :d 4})
         (quaternion {:a 8 :b 7 :c 6 :d 5}))
       (quaternion {:a 9 :b 10 :c 11 :d 12}))))
```
An example of several other operators and constructions:
```clojure
(is 
  (= (quaternion {:a 1.5 :b 1.5 :c 1.5 :d 1.5})
     (scale 
       (quaternion {:a 1 :b 1 :c 1 :d 1})
       1.5)))
       
(is (= 32
       (norm (pathion {:a 1 :b 1 :c 1 :d 1 :e 1 :f 1 :g 1 :h 1
                       :i 1 :j 1 :k 1 :l 1 :m 1 :n 1 :o 1 :p 1
                       :q 1 :r 1 :s 1 :t 1 :u 1 :v 1 :w 1 :x 1
                       :y 1 :z 1 :aa 1 :bb 1 :cc 1 :dd 1 :ee 1 :ff 1}))))       
```
An example of creating much higher order algebras using pure Clojure implementation, `:plain`:
```clojure
(n-hypercomplex (range 4096) :plain)
```


## Tests
```bash
lein test
```

## License

Credits to:
  - https://github.com/hamiltron/py-cayleydickson (MIT License)
  - https://nakkaya.com/2009/09/29/fractals-in-clojure-mandelbrot-fractal/
  - https://github.com/clojure-numerics/image-matrix/blob/master/src/main/clojure/mikera/image_matrix/colours.clj (EPL License)

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
