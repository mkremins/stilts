(ns stilts.core
  (:refer-clojure :exclude [eval macroexpand macroexpand-1 resolve])
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

(defn resolve
  "Looks up the symbol `sym` in the environment map `env` and returns the bound
   value, or `undefined` if `env` contains no binding for `sym`. If `sym` is
   namespaced, attempts to resolve the namespace using `resolve-ns`."
  [sym env]
  (if-let [ns-name (namespace sym)]
    (if-let [full-ns (resolve-ns (symbol ns-name) env)]
      (get-in env [:namespaces full-ns :mappings (symbol (name sym))] undefined)
      undefined)
    (let [{:keys [locals namespaces ns]} env
          {:keys [mappings referrals]} (namespaces ns)]
      (condp contains? sym
        locals (locals sym)
        mappings (mappings sym)
        referrals (resolve (referrals sym) env)
        undefined))))

(def core-mappings
  (merge stdlib/core-functions stdlib/core-macros))

(def core-referrals
  (reduce (fn [referrals sym]
            (assoc referrals sym (symbol "core" (name sym))))
          {} (keys core-mappings)))

(defn define-ns
  "Returns a copy of the environment map `env` in which the namespace named by
   `ns-sym` exists and is populated with the standard core referrals."
  [ns-sym env]
  (assoc-in env [:namespaces ns-sym]
            {:aliases {} :mappings {} :ns ns-sym :referrals core-referrals}))

(defn define
  "Returns a copy of the environment map `env` in which the symbol `sym` is
   bound to the value `val` within the environment's current working namespace
   `(:ns env)`."
  [sym val env]
  (assoc-in env [:namespaces (:ns env) :mappings sym] val))

;; evaluation utils

(declare eval-exp)

(defn- valid-binding-form? [x]
  (and (symbol? x) (not (namespace x))))

(deftype RecurThunk [args]) ; represents a `(recur ...)` special form

;; effectful function application

(deftype StiltsFn [clauses max-fixed-arity _meta]
  IMeta
  (-meta [this] _meta)
  IWithMeta
  (-with-meta [this new-meta] (StiltsFn. clauses max-fixed-arity new-meta)))

(defn- arity [arglist]
  (if (= (last (butlast arglist)) '&)
    :variadic
    (count arglist)))

(defn- bind-args [arglist args]
  (if (= (arity arglist) :variadic)
    (let [[fixed-args rest-args] (split-at (- (count arglist) 2) args)]
      (-> (zipmap arglist fixed-args) (assoc (last arglist) rest-args)))
    (zipmap arglist args)))

(defn- apply-stilts-fn [f args env]
  (let [argc (count args)
        variadic? (> argc (.-max-fixed-arity f))
        [arglist body] (get (.-clauses f) (if variadic? :variadic argc))
        _ (assert arglist "no matching clause for arity")
        argsyms (remove '#{&} arglist)
        benv (assoc env :recur-arity (count argsyms) :variadic-recur? variadic?)]
    (loop [locals (bind-args arglist args)]
      (let [[ret env'] (eval-exp body (update benv :locals merge locals))]
        (if (instance? RecurThunk ret)
          (recur (zipmap argsyms (.-args ret)))
          [ret (merge env' (select-keys env [:locals :recur-arity :variadic-recur?]))])))))

(defn- eval-invoke [f args env]
  (if (instance? StiltsFn f)
    (apply-stilts-fn f args env)
    [(apply f args) env]))

;; macroexpansion

(defn macroexpand-1 [form env]
  (if (and (seq? form) (symbol? (first form)))
    (let [f (resolve (first form) env)]
      (if (:macro (meta f))
        (first (eval-invoke f (rest form) env))
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

(defmulti ^:private eval-special (fn [exp _] (first exp)))

(defmethod eval-special 'def [[_ sym arg] env]
  (assert (valid-binding-form? sym) "first argument to def must be a non-namespaced symbol")
  (let [[v env'] (eval-exp arg (dissoc env :recur-arity))
        v (if (satisfies? IMeta v) (with-meta v (meta sym)) v)]
    [v (define sym v env')]))

(defmethod eval-special 'do [[_ & statements] env]
  (let [return (last statements)]
    (loop [statements (butlast statements)
           st-env (dissoc env :recur-arity)]
      (if-let [statement (first statements)]
        (let [[_ env'] (eval-exp statement st-env)]
          (recur (rest statements) env'))
        (eval-exp return (assoc st-env :recur-arity (:recur-arity env)))))))

(defmethod eval-special 'if [[_ test then else] env]
  (let [[test-v env'] (eval-exp test (dissoc env :recur-arity))]
    (eval-exp (if test-v then else) (assoc env' :recur-arity (:recur-arity env)))))

(defmethod eval-special 'in-ns [[_ ns-sym] env]
  (assert (valid-binding-form? ns-sym) "namespace name must be a non-namespaced symbol")
  (let [env' (if (resolve-ns ns-sym env) env (define-ns ns-sym env))]
    [nil (assoc env' :ns ns-sym)]))

(defmethod eval-special 'fn* [[_ & clauses] env]
  (let [arglists (map first clauses)
        _ (assert (every? #(every? valid-binding-form? %) arglists)
                  "fn params must be non-namespaced symbols")
        arities (map arity arglists)
        _ (assert (<= (count (filter #{:variadic} arities)) 1)
                  "only one variadic clause allowed per function")
        max-fixed-arity (or (apply max (remove #{:variadic} arities)) -1)]
    [(StiltsFn. (zipmap arities clauses) max-fixed-arity {}) env]))

(defmethod eval-special 'let* [[_ bvec body] env]
  (let [bpairs (partition 2 bvec)]
    (assert (every? valid-binding-form? (map first bpairs))
            "let binding names must be non-namespaced symbols")
    (loop [bpairs bpairs
           benv (dissoc env :recur-arity)]
      (if-let [[bsym bform] (first bpairs)]
        (let [[v benv'] (eval-exp bform benv)]
          (recur (rest bpairs) (assoc-in benv' [:locals bsym] v)))
        (let [[v benv'] (eval-exp body (assoc benv :recur-arity (:recur-arity env)))]
          [v (dissoc benv' :locals)])))))

(defmethod eval-special 'loop* [[_ bvec body] env]
  (let [bpairs (partition 2 bvec)
        bsyms (map first bpairs)
        recur-arity (count bpairs)]
    (assert (every? valid-binding-form? bsyms)
            "loop binding names must be non-namespaced symbols")
    (loop [bpairs bpairs
           benv (dissoc env :recur-arity)]
      (if-let [[bsym bform] (first bpairs)]
        (let [[v benv'] (eval-exp bform benv)]
          (recur (rest bpairs) (assoc-in benv' [:locals bsym] v)))
        (let [[v benv'] (eval-exp body (assoc benv :recur-arity recur-arity))]
          (if (instance? RecurThunk v)
            (recur (map vector bsyms (.-args v)) env)
            [v (dissoc benv' :locals)]))))))

(defmethod eval-special 'quote [[_ arg] env]
  [arg (dissoc env :recur-arity)])

(defmethod eval-special 'recur [[_ & args] env]
  (let [arity (:recur-arity env)
        argc (count args)]
    (assert arity "can only recur from tail position within fn*/loop* body")
    (assert (= arity argc) (str "expected " arity " args to recur, but got " argc))
    (when (:variadic-recur? env)
      (assert (rest (last args)) "last arg to recur within variadic fn must be seqable or nil")))
  [(RecurThunk. (map #(first (eval-exp % (dissoc env :recur-arity))) args)) env])

(defmethod eval-special 'throw [[_ arg] env]
  (let [[thrown _] (eval-exp arg env)]
    (if-let [[_ local body] (:catch env)]
      (let [[v env'] (eval-exp body (-> env (assoc-in [:locals local] thrown) (dissoc :catch)))]
        [v (dissoc env' :locals)])
      (throw (js/Error. "evaluated code threw an uncaught exception")))))

(defmethod eval-special 'try [[_ body [_ local :as catch]] env]
  (assert (valid-binding-form? local) "caught exception name must be a non-namespaced symbol")
  (eval-exp body (-> env (assoc :catch catch) (dissoc :recur-arity))))

;; generic evaluation

(defn- invoke? [exp]
  (and (seq? exp) (seq exp)))

(defn- special? [exp]
  (and (seq? exp) (get-method eval-special (first exp))))

(defn- eval-exp [exp env]
  (let [env' (dissoc env :recur-arity)
        eval-subexp #(first (eval-exp % env'))]
    (condp apply [exp]
      special? (eval-special exp env) ; use original env to pass on recur arity
      invoke? (let [[f & args] (map eval-subexp exp)]
                (eval-invoke f args env'))
      map? [(->> (interleave (keys exp) (vals exp))
              (map eval-subexp)
              (apply hash-map)) env']
      coll? [(into (empty exp) (map eval-subexp exp)) env']
      symbol? (let [v (resolve exp env)]
                (assert (not= v undefined) (str "var " exp " is not defined"))
                [v env'])
      [exp env'])))

(def default-env
  "The default environment map for `eval` and `eval-all`, used as a fallback in
   the event that the caller doesn't provide an environment."
  {:namespaces
   {'core {:aliases {} :mappings core-mappings :ns 'core :referrals {}}
    'user {:aliases {} :mappings {} :ns 'user :referrals core-referrals}}
   :ns 'user})

(defn eval
  "Evaluates a Clojure `form` within the context of an optional environment map
   `env`. Returns a vector `[res env']` in which `res` is the value of the
   evaluated form and `env'` is a potentially updated copy of `env`."
  ([form]
    (eval form default-env))
  ([form env]
    (-> form (macroexpand-all env) (eval-exp env))))

(defn eval-all
  "Evaluates a seq of Clojure `forms` in order within the context of an
   optional environment map `env`. Like `eval`, returns a vector `[res env']`
   in which `res` is the value of the last evaluated form and `env'` is a
   potentially updated copy of `env`."
  ([forms]
    (eval-all forms default-env))
  ([forms env]
    (eval (cons 'do (seq forms)) env)))
