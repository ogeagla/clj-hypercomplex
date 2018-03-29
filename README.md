# clj-cayley-dickson

Cayley-Dickson construction for generating hypercomplex algebras in Clojure.
https://en.wikipedia.org/wiki/Cayley%E2%80%93Dickson_construction

Credit to https://github.com/hamiltron/py-cayleydickson for some guidance (MIT License).

## Usage

Here's an example of using `clj-cayley-dickson` to demonstrate that the quaternions are associative for some fixed values:

```clojure
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
In addition to `times` there is also `plus`, `minus`, `neg`, and `c` (conjugate).

Or, you can use operations on the algebras like `scale`, `norm`, `inv`, or `mag` like so:
```clojure
(is 
  (= (quaternion {:a 1.5 :b 1.5 :c 1.5 :d 1.5})
     (scale 
       (quaternion {:a 1 :b 1 :c 1 :d 1})
       1.5)))
```

In addition to `quaternion`, this library also provides built-in support to construct `complex`, `octonion`, and `sedenion` algebras, with easy extensibility for higher order algebras.

## Tests

```bash
lein test
```

## License

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
