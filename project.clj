(defproject clj-cayley-dickson "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins      [[lein-ancient "0.6.14"]
                 [jonase/eastwood "0.2.5"]
                 [lein-kibit "0.1.5"]
                 [lein-bikeshed "0.5.0"]]
  :dependencies [[org.clojure/clojure "1.10.0-alpha7"]
                 [org.apache.commons/commons-math3 "3.6.1"]]
  :main ^:skip-aot og.cayley-dickson.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
