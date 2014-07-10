(defproject terp "0.1.0-SNAPSHOT"
  :description "Tiny ClojureScript interpreter in ClojureScript"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2268"]]
  :plugins [[lein-cljsbuild "1.0.3"]]
  :cljsbuild {:builds [{:source-paths ["src"]
                        :compiler {:output-to "target/tests.js"
                                   :optimizations :whitespace}}]})
