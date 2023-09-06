(defproject hypercomplex "0.0.4"
  :description "Hypercomplex algebras in Clojure."
  :url "https://github.com/ogeagla/clj-hypercomplex"
  :signing {:gpg-key "ogeagla@gmail.com"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins      [[lein-ancient "0.7.0"]
                 [jonase/eastwood "1.4.0"]
                 [lein-kibit "0.1.8"]
                 [lein-bikeshed "0.5.2"]
                 [venantius/yagni "0.1.7"]]
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/test.check "1.1.1"]
                 [org.apache.commons/commons-math3 "3.6.1"]]
  :main ^:skip-aot hypercomplex.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
