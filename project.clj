(defproject hypercomplex "0.0.3"
  :description "Hypercomplex algebras in Clojure."
  :url "https://github.com/ogeagla/clj-hypercomplex"
  :signing {:gpg-key "ogeagla@gmail.com"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins      [[lein-ancient "0.6.14"]
                 [jonase/eastwood "0.2.5"]
                 [lein-kibit "0.1.5"]
                 [lein-bikeshed "0.5.0"]]
  :dependencies [[org.clojure/clojure "1.10.0-RC5"]
                 [org.clojure/test.check "0.10.0-alpha3"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [me.raynes/fs "1.4.6"]
                 [net.mikera/imagez "0.12.0"]
                 [thi.ng/geom "0.0.908"]
                 [thi.ng/geom-core "0.0.908"]
                 [thi.ng/geom-types "0.0.908"]
                 [thi.ng/geom-meshops "0.0.908"]
                 [thi.ng/geom-physics "0.0.908"]
                 [thi.ng/geom-svg "0.0.908"]
                 [thi.ng/geom-viz "0.0.908"]
                 [thi.ng/geom-voxel "0.0.908"]
                 [thi.ng/geom-webgl "0.0.908"]

                 ]
  :main ^:skip-aot hypercomplex.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
