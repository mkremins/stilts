(ns stilts.repl
  (:require [cljs.reader :as rdr]
            [stilts.core :as stilts]))

(enable-console-print!)

(def rl (js/require "readline"))

(def ^:dynamic *repl-env* stilts/default-env)

(def code-buffer (atom []))

(defn eval-print! [interface line]
  (try (let [form (rdr/read-string (apply str (conj @code-buffer line)))
             [res env] (stilts/eval form *repl-env*)]
         (.setPrompt interface "user=> ")
         (reset! code-buffer [])
         (set! *repl-env* env)
         (prn res))
       (catch js/Error e
         (if (= (.-message e) "EOF while reading")
           (do (.setPrompt interface "  #_=> ")
               (swap! code-buffer conj (str line " ")))
           (println (str "Error: " (.-message e)))))
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
