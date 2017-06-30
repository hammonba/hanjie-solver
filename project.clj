(defproject hanjie3 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/tools.trace "0.7.9"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/tools.logging "0.4.0"]
                 [quil "2.6.0"]]
  :main ^:skip-aot hanjie3.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
