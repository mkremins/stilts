(ns stilts.cli
  (:require [cljs.reader :as rdr]
            [stilts.core :as stilts]
            [stilts.repl.node :as repl]))

(enable-console-print!)

(def fs (js/require "fs"))

(defn- read-forms [string]
  (let [pbr (rdr/push-back-reader string)
        eof (js/Object.)]
    (loop [forms []]
      (let [form (rdr/read pbr false eof false)]
        (if (= form eof)
          forms
          (recur (conj forms form)))))))

(defn -main
  ([] (repl/start!))
  ([fpath]
    (stilts/eval-all (read-forms (.readFileSync fs fpath)))))

(set! *main-cli-fn* -main)
