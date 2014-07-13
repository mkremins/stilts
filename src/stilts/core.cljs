(ns stilts.core
  (:refer-clojure :exclude [eval macroexpand macroexpand-1])
  (:require [clojure.walk :as walk]
            [stilts.macros :as macros]))

(def default-env
  (-> {'+ +, '- -, '* *, '/ /, '< <, '<= <=, '> >, '>= >=, '= =,
       'aget aget, 'get get, 'nth nth, 'nthnext nthnext}
    (merge macros/core-macros)))

(deftype RecurThunk [args]) ; represents a `(recur ...)` special form

;; macroexpansion

(defn macro? [f]
  (:macro (meta f)))

(defn macroexpand-1 [form env]
  (if (and (seq? form) (macro? (env (first form))))
    (apply (env (first form)) (rest form))
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
    [v (assoc env' sym v)]))

(defmethod eval-seq 'do [[_ & statements] env]
  (let [return (last statements)]
    (loop [statements (butlast statements)
           st-env (vary-meta env dissoc :allow-recur?)]
      (if-let [statement (first statements)]
        (let [[_ env'] (eval-exp statement st-env)]
          (recur (rest statements) env'))
        (eval-exp return (if (:allow-recur? (meta env))
                           (vary-meta st-env assoc :allow-recur? true)
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
         (loop [benv (merge env (zipmap arg-names args))]
           (let [[v _] (eval-exp body (with-meta benv {:allow-recur? true}))]
             (if (instance? RecurThunk v)
               (recur (merge env (zipmap arg-names (.-args v))))
               v)))
         (throw (js/Error. "no matching clause for arg count")))) env]))

(defmethod eval-seq 'let* [[_ bvec body] env]
  (loop [bpairs (partition 2 bvec) benv env]
    (if-let [[bsym bform] (first bpairs)]
      (let [[v benv'] (eval-exp bform benv)]
        (recur (rest bpairs) (assoc benv' bsym v)))
      (eval-exp body benv))))

(defmethod eval-seq 'loop* [[_ bvec body] env]
  (let [bpairs (partition 2 bvec)
        bsyms (map first bpairs)]
    (loop [bpairs bpairs benv env]
      (if-let [[bsym bform] (first bpairs)]
        (let [[v benv'] (eval-exp bform benv)]
          (recur (rest bpairs) (assoc benv' bsym v)))
        (let [[v benv'] (eval-exp body (with-meta benv {:allow-recur? true}))]
          (if (instance? RecurThunk v)
            (recur (map vector bsyms (.-args v)) env)
            [v (vary-meta benv' dissoc :allow-recur?)]))))))

(defmethod eval-seq 'quote [[_ arg] env]
  [arg env])

(defmethod eval-seq 'recur [[_ & args] env]
  (assert (:allow-recur? (meta env)) "can only recur from tail position within fn*/loop* body")
  [(RecurThunk. (map #(first (eval-exp % env)) args)) env])

;; generic evaluation

(defn eval-exp [exp env]
  (let [eval-subexp #(first (eval-exp % env))]
    (condp apply [exp]
      map? [(->> (interleave (keys exp) (vals exp))
              (map eval-subexp)
              (apply hash-map)) env]
      seq? (eval-seq exp env)
      set? [(set (map eval-subexp exp)) env]
      symbol? (do (assert (env exp) (str "var " exp " is not defined"))
                  [(env exp) env])
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
