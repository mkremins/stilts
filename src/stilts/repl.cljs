(ns stilts.repl
  (:require [cljs.reader :as rdr]
            [stilts.core :as stilts]))

(enable-console-print!)

(def rl (js/require "readline"))

(def ^:dynamic *repl-env* stilts/default-env)

(defn eval-print! [interface line]
  (try (let [form (rdr/read-string line)
             [res env] (stilts/eval form *repl-env*)]
         (set! *repl-env* env)
         (prn res))
       (catch js/Error e
         (println (str "Error: " (.-message e))))
       (finally
         (.prompt interface))))

(defn -main []
  (let [opts #js {:input (.-stdin js/process) :output (.-stdout js/process)}
        interface (.createInterface rl opts)]
    (doto interface
      (.setPrompt "user=> ")
      (.prompt)
      (.on "line" (partial eval-print! interface)))))

(set! *main-cli-fn* -main)
