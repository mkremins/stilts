(ns stilts.repl
  (:require [cljs.reader :as rdr]
            [stilts.core :as stilts]))

(enable-console-print!)

(def rl (js/require "readline"))

(def ^:dynamic *repl-env* stilts/default-env)

(def code-buffer (atom []))

(defn update-prompt! [interface]
  (let [ns-name (name (:ns *repl-env*))
        prompt (if (empty? @code-buffer)
                 (str ns-name "=> ")
                 (str (apply str (repeat (- (count ns-name) 2) " ")) "#_=> "))]
    (.setPrompt interface prompt)))

(defn eval-print! [interface line]
  (try (let [form (rdr/read-string (apply str (conj @code-buffer line)))
             [res env] (stilts/eval form *repl-env*)]
         (reset! code-buffer [])
         (set! *repl-env* env)
         (update-prompt! interface)
         (prn res))
       (catch js/Error e
         (if (= (.-message e) "EOF while reading")
           (do (swap! code-buffer conj (str line " "))
               (update-prompt! interface))
           (do (reset! code-buffer [])
               (update-prompt! interface)
               (println (str "Error: " (.-message e))))))
       (finally
         (.prompt interface))))

(defn -main []
  (let [opts #js {:input (.-stdin js/process) :output (.-stdout js/process)}
        interface (.createInterface rl opts)]
    (doto interface
      (update-prompt!)
      (.prompt)
      (.on "line" (partial eval-print! interface)))))

(set! *main-cli-fn* -main)
