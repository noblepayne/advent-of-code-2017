(defproject advent-of-code-2017 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/test.check "0.10.0-alpha2"]
                 [ubergraph "0.4.0"]
                 [criterium "0.4.4"]
                 [org.clojure/data.finger-tree "0.0.2"]
                 [org.clojure/core.async "0.3.465"]
                 [net.mikera/core.matrix "0.61.0"]]

  :main ^:skip-aot advent-of-code-2017.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :resource-paths ["inputs"])
