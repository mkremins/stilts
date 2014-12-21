(ns stilts.core
  "The core Stilts interpreter. Exports `eval` and `eval-all` functions as
   entry points for code evaluation, defines `default-env` (the default initial
   evaluation environment), and provides several other utilities for working
   with environment maps."
  (:refer-clojure :exclude [eval intern macroexpand macroexpand-1 resolve])
  (:require [clojure.walk :as walk]
            [medley.core :refer [update]]
            [stilts.stdlib :as stdlib]))

;; symbol resolution

(def undefined
  "Sentinel value returned by `resolve` in the event that the symbol to be
   resolved is unbound in the evaluation environment."
  (js/Object.))

(defn resolve-ns
  "Looks up the namespace named by the symbol `ns-sym` in the environment map
   `env` and returns the namespace's full name, or nil if not found."
  [ns-sym env]
  (or (get-in env [:namespaces (:ns env) :aliases ns-sym])
      (get-in env [:namespaces ns-sym :ns])))

(defn canonicalize
  "Looks up the symbol `sym` in the environment map `env` and returns its fully
   canonicalized form, or nil if `env` contains no definition of `sym`."
  [sym env]
  (if-let [ns-name (namespace sym)]
    (when-let [full-ns (resolve-ns (symbol ns-name) env)]
      (when (get-in env [:namespaces full-ns :defs (symbol (name sym))])
        (symbol (name full-ns) (name sym))))
    (let [{:keys [defs referrals]} (get-in env [:namespaces (:ns env)])]
      (condp contains? sym
        defs (canonicalize (symbol (name (:ns env)) (name sym)) env)
        referrals (canonicalize (symbol (name (referrals sym)) (name sym)) env)
        nil))))

(defn resolve-var [sym env]
  (when-let [sym' (canonicalize sym env)]
    (get-in env [:namespaces (symbol (namespace sym')) :defs (symbol (name sym'))])))

(defn resolve
  "Looks up the symbol `sym` in the environment map `env` and returns the bound
   value, or `undefined` if `env` contains no binding for `sym`. If `sym` is
   namespaced, attempts to resolve the namespace using `resolve-ns`."
  ([sym env] (resolve sym env {}))
  ([sym env locals]
    (if (contains? locals sym)
      (locals sym)
      (:value (resolve-var sym env) undefined))))

(defn create-ns [name]
  {:aliases {} :defs {} :ns name :referrals {}})

(defn refer-core [ns]
  (reduce #(assoc-in %1 [:referrals (:name %2)] 'core) ns stdlib/core-defs))

(defn intern [ns var]
  (assoc-in ns [:defs (:name var)] var))

(defn define-ns
  "Returns a copy of the environment map `env` in which the namespace named by
   `ns-sym` exists and is populated with the standard core referrals."
  [ns-sym env]
  (assoc-in env [:namespaces ns-sym] (-> (create-ns ns-sym) refer-core)))

(defn define
  "Returns a copy of the environment map `env` in which `var` is defined."
  [var env]
  (update-in env [:namespaces (:ns var)] intern var))

;; evaluation utils

(declare eval*)

(defn- valid-binding-form? [x]
  (and (symbol? x) (not (namespace x))))

(deftype RecurThunk [args]) ; represents a `(recur ...)` special form

;; effectful function application

(deftype StiltsFn [name clauses max-fixed-arity locals _meta]
  IMeta
  (-meta [this] _meta)
  IWithMeta
  (-with-meta [this new-meta]
    (StiltsFn. name clauses max-fixed-arity locals new-meta)))

(defn- arity [arglist]
  (if (= (last (butlast arglist)) '&)
    :variadic
    (count arglist)))

(defn- bind-args [arglist args]
  (if (= (arity arglist) :variadic)
    (let [[fixed-args rest-args] (split-at (- (count arglist) 2) args)]
      (-> (zipmap arglist fixed-args) (assoc (last arglist) rest-args)))
    (zipmap arglist args)))

(defn- apply-stilts-fn [f args env ctx]
  (let [argc (count args)
        arity (if (> argc (.-max-fixed-arity f)) :variadic argc)
        [arglist body] (get (.-clauses f) arity)
        _ (assert arglist (str "no matching clause for arity " arity))
        argsyms (remove '#{&} arglist)
        ctx' (-> (update ctx :locals merge (.-locals f) {(.-name f) f})
                 (assoc :recur-arity (count argsyms)))]
    (loop [env env locals (bind-args arglist args)]
      (let [[ret env'] (eval* body env (update ctx' :locals merge locals))]
        (if (instance? RecurThunk ret)
          (recur env' (zipmap argsyms (.-args ret)))
          [ret env'])))))

(defn- eval-invoke [f args env ctx]
  (if (instance? StiltsFn f)
    (apply-stilts-fn f args env ctx)
    [(apply f args) env]))

;; macroexpansion

(defn macroexpand-1 [form env]
  (if (and (seq? form) (symbol? (first form)))
    (let [var (resolve-var (first form) env)]
      (if (:macro (:meta var))
        (first (eval-invoke (:value var) (rest form) env {}))
        form))
    form))

(defn macroexpand [form env]
  (let [expanded (macroexpand-1 form env)]
    (if (= expanded form)
      form
      (recur expanded env))))

(defn macroexpand-all [form env]
  (walk/prewalk #(macroexpand % env) form))

;; special forms

(defmulti ^:private eval-special (fn [exp _ _] (first exp)))

(defmethod eval-special 'def [[_ sym arg] env ctx]
  (assert (valid-binding-form? sym) "first argument to def must be a non-namespaced symbol")
  (let [shadowed (get-in env [:namespaces (:ns env) :referrals sym])
        _ (assert (not shadowed) (str "def shadows existing referral " shadowed))
        [val env'] (eval* arg env (dissoc ctx :recur-arity))
        var {:name sym :ns (:ns env) :value val :meta (meta sym)}]
    [val (define var env')]))

(defmethod eval-special 'do [[_ & statements] env ctx]
  (let [return (last statements)
        ctx' (dissoc ctx :recur-arity)]
    (loop [statements (butlast statements) env env]
      (if-let [statement (first statements)]
        (let [[_ env'] (eval* statement env ctx')]
          (recur (rest statements) env'))
        (eval* return env ctx)))))

(defmethod eval-special 'if [[_ test then else] env ctx]
  (let [[test-v env'] (eval* test env (dissoc ctx :recur-arity))]
    (eval* (if test-v then else) env' ctx)))

(defmethod eval-special 'in-ns [[_ ns-sym] env _]
  (assert (valid-binding-form? ns-sym) "namespace name must be a non-namespaced symbol")
  (let [env' (if (resolve-ns ns-sym env) env (define-ns ns-sym env))]
    [nil (assoc env' :ns ns-sym)]))

(defmethod eval-special 'fn* [[_ name & clauses] env {:keys [locals]}]
  (let [_ (assert (valid-binding-form? name)
                  "local fn name must be a non-namespaced symbol")
        arglists (map first clauses)
        _ (assert (every? #(every? valid-binding-form? %) arglists)
                  "fn params must be non-namespaced symbols")
        arities (map arity arglists)
        _ (assert (<= (count (filter #{:variadic} arities)) 1)
                  "only one variadic clause allowed per function")
        max-fixed-arity (or (apply max (remove #{:variadic} arities)) -1)]
    [(StiltsFn. name (zipmap arities clauses) max-fixed-arity locals {}) env]))

(defmethod eval-special 'let* [[_ bvec body] env {:keys [recur-arity] :as ctx}]
  (let [bpairs (partition 2 bvec)]
    (assert (every? valid-binding-form? (map first bpairs))
            "let binding names must be non-namespaced symbols")
    (loop [bpairs bpairs env env
           ctx' (dissoc ctx :recur-arity)]
      (if-let [[bsym bform] (first bpairs)]
        (let [[v env'] (eval* bform env ctx')]
          (recur (rest bpairs) env' (assoc-in ctx' [:locals bsym] v)))
        (eval* body env (assoc ctx' :recur-arity recur-arity))))))

(defmethod eval-special 'loop* [[_ bvec body] env ctx]
  (let [bpairs (partition 2 bvec)
        bsyms (map first bpairs)
        recur-arity (count bpairs)]
    (assert (every? valid-binding-form? bsyms)
            "loop binding names must be non-namespaced symbols")
    (loop [bpairs bpairs env env
           ctx' (dissoc ctx :recur-arity)]
      (if-let [[bsym bform] (first bpairs)]
        (let [[v env'] (eval* bform env ctx')]
          (recur (rest bpairs) env' (assoc-in ctx' [:locals bsym] v)))
        (let [[v env'] (eval* body env (assoc ctx' :recur-arity recur-arity))]
          (if (instance? RecurThunk v)
            (recur (map vector bsyms (.-args v)) env' ctx')
            [v env']))))))

(defmethod eval-special 'quote [[_ arg] env _]
  [arg env])

(defmethod eval-special 'recur [[_ & args] env ctx]
  (let [arity (:recur-arity ctx)
        argc (count args)]
    (assert arity "can only recur from tail position within fn*/loop* body")
    (assert (= arity argc) (str "expected " arity " args to recur, but got " argc)))
  [(RecurThunk. (map #(first (eval* % env (dissoc ctx :recur-arity))) args)) env])

;; generic evaluation

(defn- invoke? [exp]
  (and (seq? exp) (seq exp)))

(defn- special? [exp]
  (and (seq? exp) (get-method eval-special (first exp))))

(defn- eval*
  ([exp env] (eval* exp env {}))
  ([exp env ctx]
    (let [ctx' (dissoc ctx :recur-arity)
          eval*' #(first (eval* % env ctx'))]
      (condp apply [exp]
        special? (eval-special exp env ctx) ; use original ctx to pass on recur arity
        invoke?  (let [[f & args] (map eval*' exp)] (eval-invoke f args env ctx'))
        map?     [(->> (flatten1 exp) (map eval*') (apply hash-map)) env]
        coll?    [(into (empty exp) (map eval*' exp)) env]
        symbol?  (let [v (resolve exp env (:locals ctx))]
                   (assert (not= v undefined) (str "var " exp " is not defined"))
                   [v env])
                 [exp env]))))

(def default-env
  "The default environment map for `eval` and `eval-all`, used as a fallback in
   the event that the caller doesn't provide an environment."
  {:namespaces
   {'core (reduce intern (create-ns 'core) stdlib/core-defs)
    'user (-> (create-ns 'user) refer-core)}
   :ns 'user})

(defn eval
  "Evaluates a Clojure `form` within the context of an optional environment map
   `env`. Returns a vector `[res env']` in which `res` is the value of the
   evaluated form and `env'` is a potentially updated copy of `env`."
  ([form]
    (eval form default-env))
  ([form env]
    (-> form (macroexpand-all env) (eval* env))))

(defn eval-all
  "Evaluates a seq of Clojure `forms` in order within the context of an
   optional environment map `env`. Like `eval`, returns a vector `[res env']`
   in which `res` is the value of the last evaluated form and `env'` is a
   potentially updated copy of `env`."
  ([forms]
    (eval-all forms default-env))
  ([forms env]
    (eval (cons 'do (seq forms)) env)))
