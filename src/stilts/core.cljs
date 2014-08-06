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
    (if (contains? (:locals env) sym)
      (get-in env [:locals sym])
      (get-in env [:namespaces (:ns env) :mappings sym] undefined))))

(def core-mappings
  (merge stdlib/core-functions stdlib/core-macros))

;; macroexpansion

(defn macroexpand-1 [form env]
  (if (and (seq? form) (symbol? (first form))
           (-> (first form) (resolve env) meta :macro))
    (apply (resolve (first form) env) (rest form))
    form))

(defn macroexpand [form env]
  (let [expanded (macroexpand-1 form env)]
    (if (= expanded form)
      form
      (recur expanded env))))

(defn macroexpand-all [form env]
  (walk/prewalk #(macroexpand % env) form))

;; special forms

(declare eval-exp)

(deftype RecurThunk [args]) ; represents a `(recur ...)` special form

(defmulti ^:private eval-special (fn [exp _] (first exp)))

(defmethod eval-special 'def [[_ sym arg] env]
  (assert (symbol? sym) "first argument to def must be a symbol")
  (assert (not (namespace sym)) "can't def a namespace-qualified symbol")
  (let [[v env'] (eval-exp arg (dissoc env :recur-arity))
        v (if (satisfies? IMeta v) (with-meta v (meta sym)) v)]
    [v (assoc-in env' [:namespaces (:ns env') :mappings sym] v)]))

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
  [nil (-> env
         (update-in [:namespaces ns-sym :mappings] #(or % core-mappings))
         (update-in [:namespaces ns-sym :ns] #(or % ns-sym))
         (assoc :ns ns-sym))])

(defn- arity [arglist]
  (if (= (last (butlast arglist)) '&)
    :variadic
    (count arglist)))

(defn- bind-args [arglist args]
  (if (= (arity arglist) :variadic)
    (let [[fixed-args rest-args] (split-at (- (count arglist) 2) args)]
      (-> (zipmap arglist fixed-args) (assoc (last arglist) rest-args)))
    (zipmap arglist args)))

(defmethod eval-special 'fn* [[_ & clauses] env]
  (let [arities (map (comp arity first) clauses)
        clause-for-arity (zipmap arities clauses)
        max-fixed-arity (or (apply max (remove #{:variadic} arities)) -1)]
    [(fn [& args]
       (let [argc (count args)]
         (if-let [[arglist body] (clause-for-arity (if (> argc max-fixed-arity) :variadic argc))]
           (let [argsyms (remove '#{&} arglist)
                 benv (assoc env :recur-arity (count argsyms) :variadic-recur? true)]
             (loop [locals (bind-args arglist args)]
               (let [[v _] (eval-exp body (update benv :locals merge locals))]
                 (if (instance? RecurThunk v)
                   (recur (zipmap argsyms (.-args v)))
                   v))))
           (throw (js/Error. "no matching clause for arity"))))) env]))

(defmethod eval-special 'let* [[_ bvec body] env]
  (loop [bpairs (partition 2 bvec)
         benv (dissoc env :recur-arity)]
    (if-let [[bsym bform] (first bpairs)]
      (let [[v benv'] (eval-exp bform benv)]
        (recur (rest bpairs) (assoc-in benv' [:locals bsym] v)))
      (let [[v benv'] (eval-exp body (assoc benv :recur-arity (:recur-arity env)))]
        [v (dissoc benv' :locals)]))))

(defmethod eval-special 'loop* [[_ bvec body] env]
  (let [bpairs (partition 2 bvec)
        bsyms (map first bpairs)
        recur-arity (count bpairs)]
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

(defmethod eval-special 'try [[_ body catch] env]
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
                [(apply f args) env'])
      map? [(->> (interleave (keys exp) (vals exp))
              (map eval-subexp)
              (apply hash-map)) env']
      set? [(set (map eval-subexp exp)) env']
      symbol? (do (assert (not= (resolve exp env) undefined) (str "var " exp " is not defined"))
                  [(resolve exp env) env'])
      vector? [(mapv eval-subexp exp) env']
      [exp env'])))

(def default-env
  "The default environment map for `eval` and `eval-all`, used as a fallback in
   the event that the caller doesn't provide an environment."
  {:namespaces {'user {:mappings core-mappings :ns 'user}}
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
