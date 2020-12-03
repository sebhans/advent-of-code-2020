(defproject advent-of-code-2020 "0.1.0-SNAPSHOT"
  :description "Solutions for the advent of code 2020 puzzles"
  :url "https://github.com/sebhans/advent-of-code-2020"
  :license {:name "public domain"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main ^:skip-aot advent-of-code-2020.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
