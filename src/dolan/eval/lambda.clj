(ns dolan.eval.lambda
  (:require [dolan.eval.core :as eval]
            [dolan.lambda :refer [curry lambda?]])
  (:import [dolan.types LCons]))

(defn- invoke-clause [{:keys [args body]} arg-vals scope]
  (let [[value state] ((eval/eval body)
                       (merge scope (zipmap (map :value args) arg-vals)))]
    value))

(defn invoke [func args]
  (fn [outer-scope]
    (let [{:keys [clauses arity scope curried-args]} func
          [args outer-scope] (eval/map-eval outer-scope args)
          args (concat curried-args args)
          call-arity (count args)]
      (cond
       (> call-arity arity)
       (throw (ex-info (str "function of arity " arity " invoked with "
                            call-arity " arguments " args)
                       {:args args :func func}))

       (< call-arity arity)
       [(curry func args) outer-scope]

       :else
       [(invoke-clause (first clauses) args scope) outer-scope]))))

(extend-type LCons
  eval/Eval
  (eval/-eval [this]
    (fn [scope]
      (let [[func & args] (seq this)
            [func scope] ((eval/-eval func) scope)]
        (cond
         (fn? func) ((func args) scope)

         (lambda? func)
         (let [[args scope] (eval/map-eval scope args)]
           ((invoke func args) scope))

         :else (throw (ex-info (str "invoking non-function " func)
                               {:value this :scope scope})))))))
