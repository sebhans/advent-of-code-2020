(defproject advent-of-code-2020 "0.1.0-SNAPSHOT"
  :description "Solutions for the advent of code 2020 puzzles"
  :url "https://github.com/sebhans/advent-of-code-2020"
  :license {:name "public domain"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [com.koloboke/koloboke-api-jdk8 "1.0.0"]
                 [com.koloboke/koloboke-impl-jdk8 "1.0.0"]]
  :main ^:skip-aot advent-of-code-2020.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :dev {:dependencies [[com.clojure-goes-fast/clj-async-profiler "0.4.1"]]
                   :jvm-opts ["-Djdk.attach.allowAttachSelf", "-XX:+UnlockDiagnosticVMOptions", "-XX:+DebugNonSafepoints"]}})
