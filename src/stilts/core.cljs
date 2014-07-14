(ns stilts.core
  (:refer-clojure :exclude [eval macroexpand macroexpand-1 resolve])
  (:require [clojure.walk :as walk]
            [stilts.macros :as macros]))

(def default-env
  {:globals (-> {'+ +, '- -, '* *, '/ /, '< <, '<= <=, '> >, '>= >=, '= =,
                 'aget aget, 'get get, 'nth nth, 'nthnext nthnext}
              (merge macros/core-macros))})

(defn resolve [sym env]
  (or (get-in env [:locals sym]) (get-in env [:globals sym])))

(deftype RecurThunk [args]) ; represents a `(recur ...)` special form

;; macroexpansion

(defn macro? [f]
  (:macro (meta f)))

(defn macroexpand-1 [form env]
  (if (and (seq? form) (macro? (resolve (first form) env)))
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

(defmulti eval-seq (fn [exp _] (first exp)))

(defmethod eval-seq :default [exp env]
  (let [vs (map #(first (eval-exp % env)) exp)]
    [(apply (first vs) (rest vs)) env]))

(defmethod eval-seq nil [_ env]
  [() env])

(defmethod eval-seq 'def [[_ sym arg] env]
  (let [[v env'] (eval-exp arg env)]
    [v (assoc-in env' [:globals sym] v)]))

(defmethod eval-seq 'do [[_ & statements] env]
  (let [return (last statements)]
    (loop [statements (butlast statements)
           st-env (dissoc env :allow-recur?)]
      (if-let [statement (first statements)]
        (let [[_ env'] (eval-exp statement st-env)]
          (recur (rest statements) env'))
        (eval-exp return (if (:allow-recur? env)
                           (assoc st-env :allow-recur? true)
                           st-env))))))

(defmethod eval-seq 'if [[_ test then else] env]
  (let [[test-v env'] (eval-exp test env)]
    (eval-exp (if test-v then else) env')))

(defmethod eval-seq 'fn* [[_ & clauses] env]
  (let [clauses (if (vector? (first clauses))
                  [[(first clauses) (second clauses)]]
                  clauses)
        clause-for-argc (zipmap (map (comp count first) clauses) clauses)]
    [(fn [& args]
       (if-let [[arg-names body] (clause-for-argc (count args))]
         (loop [benv (update-in env [:locals] merge (zipmap arg-names args))]
           (let [[v _] (eval-exp body (assoc benv :allow-recur? true))]
             (if (instance? RecurThunk v)
               (recur (update-in env [:locals] merge (zipmap arg-names (.-args v))))
               v)))
         (throw (js/Error. "no matching clause for arg count")))) env]))

(defmethod eval-seq 'let* [[_ bvec body] env]
  (loop [bpairs (partition 2 bvec) benv env]
    (if-let [[bsym bform] (first bpairs)]
      (let [[v benv'] (eval-exp bform benv)]
        (recur (rest bpairs) (assoc-in benv' [:locals bsym] v)))
      (let [[v benv'] (eval-exp body benv)]
        [v (dissoc benv' :locals)]))))

(defmethod eval-seq 'loop* [[_ bvec body] env]
  (let [bpairs (partition 2 bvec)
        bsyms (map first bpairs)]
    (loop [bpairs bpairs benv env]
      (if-let [[bsym bform] (first bpairs)]
        (let [[v benv'] (eval-exp bform benv)]
          (recur (rest bpairs) (assoc-in benv' [:locals bsym] v)))
        (let [[v benv'] (eval-exp body (assoc benv :allow-recur? true))]
          (if (instance? RecurThunk v)
            (recur (map vector bsyms (.-args v)) env)
            [v (dissoc benv' :allow-recur? :locals)]))))))

(defmethod eval-seq 'quote [[_ arg] env]
  [arg env])

(defmethod eval-seq 'recur [[_ & args] env]
  (assert (:allow-recur? env) "can only recur from tail position within fn*/loop* body")
  [(RecurThunk. (map #(first (eval-exp % env)) args)) env])

(defmethod eval-seq 'throw [[_ arg] env]
  (let [[thrown _] (eval-exp arg env)]
    (if-let [[_ local body] (:catch env)]
      (let [[v env'] (eval-exp body (-> env (assoc-in [:locals local] thrown) (dissoc :catch)))]
        [v (dissoc env' :locals)])
      (throw (js/Error. "evaluated code threw an uncaught exception")))))

(defmethod eval-seq 'try [[_ body catch] env]
  (eval-exp body (assoc env :catch catch)))

;; generic evaluation

(defn eval-exp [exp env]
  (let [eval-subexp #(first (eval-exp % env))]
    (condp apply [exp]
      map? [(->> (interleave (keys exp) (vals exp))
              (map eval-subexp)
              (apply hash-map)) env]
      seq? (eval-seq exp env)
      set? [(set (map eval-subexp exp)) env]
      symbol? (do (assert (resolve exp env) (str "var " exp " is not defined"))
                  [(resolve exp env) env])
      vector? [(mapv eval-subexp exp) env]
      [exp env])))

(defn eval
  ([form]
    (eval form default-env))
  ([form env]
    (-> form (macroexpand-all env) normalize-all-interop (eval-exp env))))

(defn eval-all
  ([forms]
    (eval-all forms default-env))
  ([forms env]
    (eval (cons 'do (seq forms)) env)))
