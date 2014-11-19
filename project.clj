(defproject mkremins/stilts "0.0-SNAPSHOT"
  :description "Tiny interpreted Lisp in ClojureScript"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2268"]
                 [medley "0.5.0"]]
  :plugins [[lein-cljsbuild "1.0.3"]]
  :cljsbuild {:builds [{:source-paths ["src"]
                        :compiler {:output-to "target/stilts.js"
                                   :optimizations :simple
                                   :target :nodejs}}]})
