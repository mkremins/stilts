(ns stilts.macros)

(defn -defn [name args & body]
  `(def ~name (fn* ~args (do ~@body))))

(defn -defmacro [name args & body]
  `(~'defn ~(with-meta name {:macro true}) ~args ~@body))

(defn -and
  ([] true)
  ([x] x)
  ([x & xs] `(if ~x (~'and ~@xs) ~x)))

(defn -or
  ([] nil)
  ([x] x)
  ([x & xs] `(if ~x ~x (~'or ~@xs))))

(defn -when [test & body]
  `(if ~test ~(cons 'do body) nil))

;; destructuring binding

(defprotocol IBindingForm
  (-bindings [this expr]))

(defn- map-bindings [m expr]
  (let [as-sym (or (:as m) (gensym))
        or-map (or (:or m) {})
        keys (for [s (:keys m)] [(symbol (name s)) (keyword (namespace s) (name s))])
        strs (for [s (:strs m)] [(symbol s) (str s)])
        syms (for [s (:syms m)] [(symbol (name s)) s])
        bpairs (concat (dissoc m :as :or :keys :strs :syms) keys strs syms)]
    (concat (-bindings as-sym expr)
            (apply concat
              (for [[k v] bpairs] (-bindings k (list 'get as-sym v))))
            (for [[k v] or-map] [k (list 'or k v)]))))

(defn- vec-bindings [v expr]
  (let [[nths specials] (split-with (complement #{'& :as}) v)
        specials (apply hash-map specials)
        as-sym (or (:as specials) (gensym))
        rest-form ('& specials)]
    (concat (-bindings as-sym expr)
            (->> nths
              (map-indexed (fn [n bform] (-bindings bform (list 'nth as-sym n))))
              (apply concat))
            (when rest-form
              (-bindings rest-form (list 'nthnext as-sym (count nths)))))))

(extend-protocol IBindingForm
  Symbol
  (-bindings [this expr]
    [[this expr]])

  PersistentArrayMap
  (-bindings [this expr]
    (map-bindings this expr))

  PersistentHashMap
  (-bindings [this expr]
    (map-bindings this expr))

  PersistentVector
  (-bindings [this expr]
    (vec-bindings this expr)))

(defn -let [bvec & body]
  (let [bpairs (mapcat #(apply -bindings %) (partition 2 bvec))]
    `(let* [~@(apply concat bpairs)]
       (do ~@body))))

;; tying it all together

(defn map-vals [f m]
  (apply hash-map (interleave (keys m) (map f (vals m)))))

(def core-macros
  (->> {'and -and, 'defmacro -defmacro, 'defn -defn, 'let -let, 'or -or, 'when -when}
    (map-vals #(with-meta % {:macro true}))))
