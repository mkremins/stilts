(ns terp.repl
  (:require [cljs.reader :as rdr]
            [terp.core :as terp]))

(enable-console-print!)

(def rl (js/require "readline"))

(def ^:dynamic *repl-env* terp/default-env)

(defn eval-print! [interface line]
  (let [form (rdr/read-string line)
        [res env] (terp/eval form *repl-env*)]
    (set! *repl-env* env)
    (prn res)
    (.prompt interface)))

(defn -main []
  (let [opts #js {:input (.-stdin js/process) :output (.-stdout js/process)}
        interface (.createInterface rl opts)]
    (doto interface
      (.setPrompt "user=> ")
      (.prompt)
      (.on "line" (partial eval-print! interface)))))

(set! *main-cli-fn* -main)
