(ns dolan.eval
  (:refer-clojure :exclude [eval])
  (:require [dolan.types :as t]
            [dolan.monad :as m]
            [dolan.lambda :refer [lambda?]])
  (:import [dolan.types LNumber LString LSymbol LCons LBoolean]
           [dolan.lambda Lambda]))

(defprotocol Eval
  (-eval [this]))

(declare invoke)

(extend-protocol Eval
  LNumber
  (-eval [this]
    (fn [scope] [this scope]))

  LString
  (-eval [this]
    (fn [scope] [this scope]))

  LBoolean
  (-eval [this]
    (fn [scope] [this scope]))

  LSymbol
  (-eval [this]
    (fn [scope]
      (if-let [val (scope (:value this))]
        [val scope]
        (throw (ex-info (str "unbound symbol \"" this "\"")
                        {:value this :scope scope})))))

  LCons
  (-eval [this]
    (fn [scope]
      (let [[func & args] (seq this)
            [func scope] ((-eval func) scope)]
        (cond
         (fn? func) ((func args) scope)
         (lambda? func) ((invoke func args) scope)
         :else (throw (ex-info (str "invoking non-function " func)
                               {:value this :scope scope}))))))

  nil
  (-eval [this]
    (fn [scope] [nil scope])))

(defn eval [value]
  (-eval value))

(defn map-eval [scope values]
  (m/map-state scope (map eval values)))

(defn- invoke-clause [{:keys [args body]} arg-vals scope]
  (let [[value state] ((eval body)
                       (merge scope (zipmap (map :value args) arg-vals)))]
    value))

(defn invoke [func args]
  (fn [outer-scope]
    (let [{:keys [clauses arity scope]} func
          [args outer-scope] (map-eval outer-scope args)]
      (cond
       (not= arity (count args))
       (throw (ex-info (str "function of arity " arity " invoked with "
                            (count args) " arguments " args)
                       {:args args :func func}))

       :else
       [(invoke-clause (first clauses) args scope) outer-scope]))))
