(ns hypercomplex.construction-test
  (:require [clojure.test :refer :all]
            [hypercomplex.cayley-dickson-construction :as c]))

;TODO this might be the ugliest clojure test ever seen

(deftest power-of-test
  (testing "If powers of 2 work"
    (let [po-1? (c/power-of? 1 1)
          po-2? (c/power-of? 1.0 1.0)
          po-3? (c/power-of? 2 2)
          po-4? (c/power-of? 8 2)
          po-5? (c/power-of? 8.0 2.0)
          po-6? (c/power-of? 9 2)
          po-7? (c/power-of? 9.0 2.0)]
      (is (=
            po-1?
            true))
      (is (=
            po-2?
            true))
      (is (=
            po-3?
            true))
      (is (=
            po-4?
            true))
      (is (=
            po-5?
            true))
      (is (=
            po-6?
            false))
      (is (=
            po-7?
            false)))))

(deftest n-degree-test
  (testing "Cannot create n-hypercomplex unless coeffs count is power of 2, and > 1"
    (let [coeffs-1 [0]
          coeffs-2 [0 1]
          coeffs-3 [0 1 2]
          coeffs-4 [0 1 2 3 4 5 6 7]
          coeffs-5 (range 32)]
      (is
        (thrown? Exception (c/n-hypercomplex coeffs-1 :plain)))
      (is (=
            (c/n-hypercomplex coeffs-2 :plain)
            #hypercomplex.core.Complex2{:a 0 :b 1 :order 2}))
      (is
        (thrown? Exception (c/n-hypercomplex coeffs-3 :plain)))
      (is
        (=
          (c/n-hypercomplex coeffs-4 :plain)
          #hypercomplex.core.Construction{:a
                                                 #hypercomplex.core.Construction{:a
                                                                                        #hypercomplex.core.Complex2{:a     0
                                                                                                                    :b     1
                                                                                                                    :order 2}
                                                                                 :b
                                                                                        #hypercomplex.core.Complex2{:a     2
                                                                                                                    :b     3
                                                                                                                    :order 2}
                                                                                 :order 4}
                                          :b
                                                 #hypercomplex.core.Construction{:a
                                                                                        #hypercomplex.core.Complex2{:a     4
                                                                                                                    :b     5
                                                                                                                    :order 2}
                                                                                 :b     #hypercomplex.core.Complex2{:a     6
                                                                                                                    :b     7
                                                                                                                    :order 2}
                                                                                 :order 4}
                                          :order 8}))
      (is
        (=
          (c/n-hypercomplex coeffs-5 :plain)
          #hypercomplex.core.Construction{:a #hypercomplex.core.Construction{:a #hypercomplex.core.Construction{:a #hypercomplex.core.Construction{:a #hypercomplex.core.Complex2{:a 0, :b 1, :order 2}, :b #hypercomplex.core.Complex2{:a 2, :b 3, :order 2}, :order 4}, :b #hypercomplex.core.Construction{:a #hypercomplex.core.Complex2{:a 4, :b 5, :order 2}, :b #hypercomplex.core.Complex2{:a 6, :b 7, :order 2}, :order 4}, :order 8}, :b #hypercomplex.core.Construction{:a #hypercomplex.core.Construction{:a #hypercomplex.core.Complex2{:a 8, :b 9, :order 2}, :b #hypercomplex.core.Complex2{:a 10, :b 11, :order 2}, :order 4}, :b #hypercomplex.core.Construction{:a #hypercomplex.core.Complex2{:a 12, :b 13, :order 2}, :b #hypercomplex.core.Complex2{:a 14, :b 15, :order 2}, :order 4}, :order 8}, :order 16}, :b #hypercomplex.core.Construction{:a #hypercomplex.core.Construction{:a #hypercomplex.core.Construction{:a #hypercomplex.core.Complex2{:a 16, :b 17, :order 2}, :b #hypercomplex.core.Complex2{:a 18, :b 19, :order 2}, :order 4}, :b #hypercomplex.core.Construction{:a #hypercomplex.core.Complex2{:a 20, :b 21, :order 2}, :b #hypercomplex.core.Complex2{:a 22, :b 23, :order 2}, :order 4}, :order 8}, :b #hypercomplex.core.Construction{:a #hypercomplex.core.Construction{:a #hypercomplex.core.Complex2{:a 24, :b 25, :order 2}, :b #hypercomplex.core.Complex2{:a 26, :b 27, :order 2}, :order 4}, :b #hypercomplex.core.Construction{:a #hypercomplex.core.Complex2{:a 28, :b 29, :order 2}, :b #hypercomplex.core.Complex2{:a 30, :b 31, :order 2}, :order 4}, :order 8}, :order 16}, :order 32}))
      #_(is
          (=
            (c/n-hypercomplex (range 4096) :plain)
            {:too :many}))
      )))