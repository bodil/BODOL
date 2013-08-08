(ns bodol.eval.lambda
  (:require [bodol.eval.core :as eval]
            [bodol.lambda :as l]
            [bodol.types :as t]
            [bodol.error :as err])
  (:import [bodol.types LCons]
           [bodol.lambda Lambda]))



(defn invoke [func args]
  (fn [outer-scope]
    (let [arity (l/-arity func)
          scope (l/-scope func)
          scope (if-let [name (l/-name func)]
                  (assoc scope name func)
                  scope)
          curried-args (l/-curried-args func)
          [args outer-scope] (eval/map-eval outer-scope args)
          args (concat curried-args args)
          call-arity (count args)]
      (cond
       (> call-arity arity)
       (err/raise scope func :arity-mismatch
                  (str "function of arity " arity " invoked with "
                       call-arity " arguments " args))

       (< call-arity arity)
       [(l/curry func args) outer-scope]

       :else
       (if-let [match (l/pattern-match func args)]
         (let [[clause scope-mv] match
               scope (-> (scope-mv scope)
                         second
                         (merge {:function func}))
               [result final-scope]
               ((eval/eval (:body clause)) scope)]
           [result outer-scope])
         (err/raise outer-scope func :pattern-match-failure
                    (str "function call did not match any defined patterns "
                         "(" (t/pr-value func) " "
                         (clojure.string/join " " (map t/pr-value args))
                         ")")))))))

(extend-protocol eval/Eval
  Lambda
  (-eval [this]
    (fn [scope]
      (if-let [name (l/-name this)]
        (let [this (l/bind this scope)
              scope (assoc scope name this)]
          [this scope])
        (let [this (l/bind this scope)]
          [this scope]))))

  LCons
  (eval/-eval [this]
    (fn [scope]
      (let [[func & args] (seq this)
            [func scope] ((eval/-eval func) scope)]
        (cond
         (fn? func) ((func args) scope)

         (l/lambda? func)
         (let [[result scope] ((invoke func args) scope)]
           [result scope])

         :else (err/raise scope this :noncallable-invocation
                          (str "invoking non-function " func)))))))
