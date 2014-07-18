(ns stilts.core
  (:refer-clojure :exclude [eval macroexpand macroexpand-1 resolve])
  (:require [clojure.walk :as walk]
            [medley.core :refer [update]]
            [stilts.macros :as macros]))

;; symbol resolution

(def undefined
  "Sentinel value returned by `resolve` in the event that the symbol to be
   resolved is unbound in the evaluation environment."
  (js/Object.))

(defn resolve
  "Looks up the symbol `sym` in the environment map `env` and returns the bound
   value, or `undefined` if `env` contains no binding for `sym`."
  [sym env]
  (let [local (get-in env [:locals sym] undefined)]
    (if (= local undefined)
      (get-in env [:globals sym] undefined)
      local)))

;; macroexpansion

(defn macroexpand-1 [form env]
  (if (and (seq? form) (-> (first form) (resolve env) meta :macro))
    (apply (resolve (first form) env) (rest form))
    form))

(defn macroexpand [form env]
  (let [expanded (macroexpand-1 form env)]
    (if (= expanded form)
      form
      (recur expanded env))))

(defn macroexpand-all [form env]
  (walk/prewalk #(macroexpand % env) form))

;; interop normalization

(defn normalize-interop [form]
  (if (and (seq? form)
           (symbol? (first form))
           (= (first (str (first form))) \.))
    (let [sym-str (str (first form))
          target (second form)]
      (if (= (second sym-str) \-)
        (list 'aget target (subs sym-str 2))
        (cons (list 'aget target (subs sym-str 1)) (drop 2 form))))
    form))

(defn normalize-all-interop [form]
  (walk/prewalk normalize-interop form))

;; special forms + function application

(declare eval-exp)

(deftype RecurThunk [args]) ; represents a `(recur ...)` special form

(defmulti ^:private eval-seq (fn [exp _] (first exp)))

(defmethod eval-seq :default [exp env]
  (let [vs (map #(first (eval-exp % env)) exp)]
    [(apply (first vs) (rest vs)) env]))

(defmethod eval-seq nil [_ env]
  [() env])

(defmethod eval-seq 'def [[_ sym arg] env]
  (assert (symbol? sym) "first argument to def must be a symbol")
  (let [[v env'] (eval-exp arg env)
        v (if (satisfies? IMeta v) (with-meta v (meta sym)) v)]
    [v (assoc-in env' [:globals sym] v)]))

(defmethod eval-seq 'do [[_ & statements] env]
  (let [return (last statements)]
    (loop [statements (butlast statements)
           st-env (dissoc env :recur-arity)]
      (if-let [statement (first statements)]
        (let [[_ env'] (eval-exp statement st-env)]
          (recur (rest statements) env'))
        (eval-exp return (if-let [recur-arity (:recur-arity env)]
                           (assoc st-env :recur-arity recur-arity)
                           st-env))))))

(defmethod eval-seq 'if [[_ test then else] env]
  (let [[test-v env'] (eval-exp test env)]
    (eval-exp (if test-v then else) env')))

(defn- arity [arglist]
  (if (= (last (butlast arglist)) '&)
    :variadic
    (count arglist)))

(defn- bind-args [arglist args]
  (if (= (arity arglist) :variadic)
    (let [[fixed-args rest-args] (split-at (- (count arglist) 2) args)]
      (-> (zipmap arglist fixed-args) (assoc (last arglist) rest-args)))
    (zipmap arglist args)))

(defn- bind-recur-args [arglist args]
  (zipmap (if (= (arity arglist) :variadic)
            (concat (drop-last 2 arglist) [(last arglist)])
            arglist)
          args))

(defmethod eval-seq 'fn* [[_ & clauses] env]
  (let [arities (map (comp arity first) clauses)
        clause-for-arity (zipmap arities clauses)
        max-fixed-arity (apply max (remove #{:variadic} arities))]
    [(fn [& args]
       (let [argc (count args)
             invoke-arity (if (> argc max-fixed-arity) :variadic argc)
             recur-arity (if (= invoke-arity :variadic) (inc max-fixed-arity) argc)]
         (if-let [[arglist body] (clause-for-arity invoke-arity)]
           (loop [benv (update env :locals merge (bind-args arglist args))]
             (let [[v _] (eval-exp body (assoc benv :recur-arity recur-arity))]
               (if (instance? RecurThunk v)
                 (recur (update env :locals merge (bind-recur-args arglist (.-args v))))
                 v)))
           (throw (js/Error. "no matching clause for arity"))))) env]))

(defmethod eval-seq 'let* [[_ bvec body] env]
  (loop [bpairs (partition 2 bvec) benv env]
    (if-let [[bsym bform] (first bpairs)]
      (let [[v benv'] (eval-exp bform benv)]
        (recur (rest bpairs) (assoc-in benv' [:locals bsym] v)))
      (let [[v benv'] (eval-exp body benv)]
        [v (dissoc benv' :locals)]))))

(defmethod eval-seq 'loop* [[_ bvec body] env]
  (let [bpairs (partition 2 bvec)
        bsyms (map first bpairs)
        recur-arity (count bpairs)]
    (loop [bpairs bpairs benv env]
      (if-let [[bsym bform] (first bpairs)]
        (let [[v benv'] (eval-exp bform benv)]
          (recur (rest bpairs) (assoc-in benv' [:locals bsym] v)))
        (let [[v benv'] (eval-exp body (assoc benv :recur-arity recur-arity))]
          (if (instance? RecurThunk v)
            (recur (map vector bsyms (.-args v)) env)
            [v (dissoc benv' :locals :recur-arity)]))))))

(defmethod eval-seq 'quote [[_ arg] env]
  [arg env])

(defmethod eval-seq 'recur [[_ & args] env]
  (let [arity (:recur-arity env)
        argc (count args)]
    (assert arity "can only recur from tail position within fn*/loop* body")
    (assert (= arity argc) (str "expected " arity " args to recur, but got " argc)))
  [(RecurThunk. (map #(first (eval-exp % (dissoc env :recur-arity))) args)) env])

(defmethod eval-seq 'throw [[_ arg] env]
  (let [[thrown _] (eval-exp arg env)]
    (if-let [[_ local body] (:catch env)]
      (let [[v env'] (eval-exp body (-> env (assoc-in [:locals local] thrown) (dissoc :catch)))]
        [v (dissoc env' :locals)])
      (throw (js/Error. "evaluated code threw an uncaught exception")))))

(defmethod eval-seq 'try [[_ body catch] env]
  (eval-exp body (assoc env :catch catch)))

;; generic evaluation

(defn- eval-exp [exp env]
  (let [eval-subexp #(first (eval-exp % env))]
    (condp apply [exp]
      map? [(->> (interleave (keys exp) (vals exp))
              (map eval-subexp)
              (apply hash-map)) env]
      seq? (eval-seq exp env)
      set? [(set (map eval-subexp exp)) env]
      symbol? (do (assert (not= (resolve exp env) undefined) (str "var " exp " is not defined"))
                  [(resolve exp env) env])
      vector? [(mapv eval-subexp exp) env]
      [exp env])))

(def default-env
  "The default environment map for `eval` and `eval-all`, used as a fallback in
   the event that the caller doesn't provide an environment."
  {:globals (-> {'+ +, '- -, '* *, '/ /, '< <, '<= <=, '> >, '>= >=, '= =,
                 'aget aget, 'get get, 'list list, 'nth nth, 'nthnext nthnext}
              (merge macros/core-macros))})

(defn eval
  "Evaluates a Clojure `form` within the context of an optional environment map
   `env`. Returns a vector `[res env']` in which `res` is the value of the
   evaluated form and `env'` is a potentially updated copy of `env`."
  ([form]
    (eval form default-env))
  ([form env]
    (-> form (macroexpand-all env) normalize-all-interop (eval-exp env))))

(defn eval-all
  "Evaluates a seq of Clojure `forms` in order within the context of an
   optional environment map `env`. Like `eval`, returns a vector `[res env']`
   in which `res` is the value of the last evaluated form and `env'` is a
   potentially updated copy of `env`."
  ([forms]
    (eval-all forms default-env))
  ([forms env]
    (eval (cons 'do (seq forms)) env)))
