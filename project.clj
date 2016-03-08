(defproject project-euler "0.1.0-SNAPSHOT"
  :description "My attempt at the project euler problems"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :main ^:skip-aot project-euler.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
