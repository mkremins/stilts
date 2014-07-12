(ns terp.macros)

(defn -defn [name args & body]
  `(def ~name (fn* ~args (do ~@body))))

(defn map-vals [f m]
  (apply hash-map (interleave (keys m) (map f (vals m)))))

(def core-macros
  (->> {'defn -defn}
    (map-vals #(with-meta % {:macro true}))))
