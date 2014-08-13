(ns stilts.repl
  (:require [cljs.reader :as rdr]
            [clojure.string :as str]
            [stilts.core :as stilts]))

(def init-state
  {:buffer [] :env stilts/default-env})

(defn push-line [{:keys [buffer env]} line]
  (try (let [form (rdr/read-string (str/join " " (conj buffer line)))
             [res env'] (stilts/eval form env)]
         {:buffer [] :env env' :result res})
       (catch js/Error e
         (if (= (.-message e) "EOF while reading")
           {:buffer (conj buffer line) :env env}
           {:buffer [] :env env :error e}))))

(defn prompt [{:keys [buffer env]}]
  (let [ns-name (name (:ns env))]
    (if (empty? buffer)
      (str ns-name "=> ")
      (str (str/join (repeat (- (count ns-name) 2) " ")) "#_=> "))))
