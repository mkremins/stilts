(ns stilts.stdlib
  "The Stilts standard library, containing functions (aliased from `cljs.core`)
  and macros (reimplemented as ordinary ClojureScript functions) borrowed from
  Clojure's own stdlib.")

;; functions

(def core-functions
  (map (fn [[sym val]] {:name sym :ns 'core :value val})
       {'+ + '- - '* * '/ / '< < '<= <= '> > '>= >= '= = '== == 'aget aget 'apply apply 'aset aset
        'assoc assoc 'assoc-in assoc-in 'comp comp 'complement complement 'concat concat 'conj conj
        'cons cons 'constantly constantly 'contains? contains? 'count count 'cycle cycle 'dec dec
        'disj disj 'dissoc dissoc 'distinct distinct 'distinct? distinct? 'drop drop
        'drop-last drop-last 'drop-while drop-while 'empty empty 'empty? empty? 'every? every?
        'filter filter 'filterv filterv 'find find 'first first 'ffirst ffirst 'fnil fnil 'get get
        'get-in get-in 'group-by group-by 'hash hash 'hash-map hash-map 'hash-set hash-set
        'identity identity 'inc inc 'interleave interleave 'interpose interpose 'into into
        'iterate iterate 'juxt juxt 'key key 'keys keys 'keyword keyword 'last last 'list list
        'list* list* 'map map 'map? map? 'mapcat mapcat 'mapv mapv 'map-indexed map-indexed
        'max max 'merge merge 'merge-with merge-with 'min min 'mod mod 'name name
        'namespace namespace 'next next 'nil? nil? 'not not 'not= not= 'not-any? not-any? 'nth nth
        'nthnext nthnext 'partial partial 'partition partition 'partition-all partition-all
        'partition-by partition-by 'peek peek 'pop pop 'quot quot 'range range 'reduce reduce
        'reduce-kv reduce-kv 'rem rem 'remove remove 'repeat repeat 'replace replace 'rest rest
        'reverse reverse 'rseq rseq 'second second 'seq seq 'seq? seq? 'set set 'set? set?
        'select-keys select-keys 'some some 'some? some? 'sort sort 'subvec subvec 'symbol symbol
        'symbol? symbol? 'take take 'take-last take-last 'take-while take-while 'type type
        'update-in update-in 'val val 'vals vals 'vector vector 'vector? vector? 'vec vec
        'zipmap zipmap}))

;; macros

(defn -case [expr & body]
  (loop [expanded (if (odd? (count body))
                    (last body) `(throw ~(js/Error. "No matching clause")))
         clauses (reverse (partition 2 body))]
    (if-let [[test then] (first clauses)]
      (let [test (if (list? test)
                   `(~'contains? ~(set test) ~expr)
                   `(~'= ~expr ~test))]
        (recur `(if ~test ~then ~expanded) (rest clauses)))
      expanded)))

(defn -cond [& body]
  (loop [expanded (if (odd? (count body))
                    (last body) `(throw ~(js/Error. "No matching clause")))
         clauses (reverse (partition 2 body))]
    (if-let [[test then] (first clauses)]
      (recur `(if ~test ~then ~expanded) (rest clauses))
      expanded)))

(defn -defn [name & clauses]
  `(def ~name (~'fn ~name ~@clauses)))

(defn -defmacro [name & clauses]
  `(~'defn ~(with-meta name {:macro true}) ~@clauses))

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
  (-bindings [this expr] [[this expr]])
  PersistentArrayMap
  (-bindings [this expr] (map-bindings this expr))
  PersistentHashMap
  (-bindings [this expr] (map-bindings this expr))
  PersistentVector
  (-bindings [this expr] (vec-bindings this expr)))

(defn -fn [& clauses]
  (let [[name clauses] (if (symbol? (first clauses))
                         [(first clauses) (rest clauses)]
                         [(gensym "fn_") clauses])
        clauses (if (vector? (first clauses)) (list clauses) clauses)]
    `(fn* ~name
       ~@(for [[arglist & body] clauses
               :let [gensyms (repeatedly gensym)
                     genargs (zipmap (remove '#{&} arglist) gensyms)]]
           `(~(mapv #(get genargs % %) arglist) ; replace bforms with gensyms in arglist
             (~'let [~@(interleave (keys genargs) (vals genargs))] ~@body))))))

(defn -let [bvec & body]
  (let [bpairs (mapcat #(apply -bindings %) (partition 2 bvec))]
    `(let* [~@(apply concat bpairs)]
       (do ~@body))))

(defn -loop [bvec & body]
  (let [bpairs (partition 2 bvec)
        gensyms (repeatedly gensym)]
    `(loop* [~@(interleave gensyms (map second bpairs))]
       (~'let [~@(interleave (map first bpairs) gensyms)]
         ~@body))))

(defn -if-let [[bform bval] then else]
  (let [bsym (gensym)]
    `(let* [~bsym ~bval]
       (~'if ~bsym (~'let [~bform ~bsym] ~then) ~else))))

(defn -when-let [[bform bval] & body]
  `(~'if-let [~bform ~bval] (do ~@body) nil))

;; tying it all together

(def core-macros
  (map (fn [[sym val]] {:name sym :ns 'core :value val :meta {:macro true}})
       {'and -and 'case -case 'cond -cond 'defmacro -defmacro 'defn -defn 'fn -fn 'if-let -if-let
        'let -let 'loop -loop 'or -or 'when -when 'when-let -when-let}))

(def core-defs (concat core-functions core-macros))
