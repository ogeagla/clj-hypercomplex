(defproject hypercomplex "0.0.2"
  :description "Hypercomplex algebras in Clojure."
  :url "https://github.com/ogeagla/clj-hypercomplex"
  :signing {:gpg-key "ogeagla@gmail.com"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins      [[lein-ancient "0.6.14"]
                 [jonase/eastwood "0.2.5"]
                 [lein-kibit "0.1.5"]
                 [lein-bikeshed "0.5.0"]]
  :dependencies [[org.clojure/clojure "1.10.0-beta4"]
                 [org.apache.commons/commons-math3 "3.6.1"]]
  :main ^:skip-aot hypercomplex.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
