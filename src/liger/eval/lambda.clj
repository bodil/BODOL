(ns liger.eval.lambda
  (:require [liger.eval.core :as eval]
            [liger.lambda :refer [curry lambda?]]
            [liger.types :as t]
            [liger.monad :as m :refer [state-id]])
  (:import [liger.types LCons LSymbol]))

(defn- match-arg [matcher arg]
  (cond
   (= (LSymbol. "_") matcher) state-id
   (t/lsymbol? matcher) (fn [scope] [nil (assoc scope (:value matcher) arg)])
   :else (when (= arg matcher) state-id)))

(defn- match-clause [args clause]
  (let [{matchers :args} clause
        scope-mvs
        (reduce (fn [curr next] (when (and curr next) (conj curr next))) []
                (map (partial apply match-arg)
                     (partition 2 (interleave matchers args))))]
    (when scope-mvs [clause scope-mvs])))

(defn- find-match [args clauses]
  (some (partial match-clause args) clauses))

(defn invoke [func args]
  (fn [outer-scope]
    (let [{:keys [clauses arity scope curried-args]} func
          [args outer-scope] (eval/map-eval outer-scope args)
          args (concat curried-args args)
          call-arity (count args)]
      (cond
       (> call-arity arity)
       (throw (ex-info (str "function of arity " arity " invoked with "
                            call-arity " arguments " (t/pr-value args))
                       {:args args :func func}))

       (< call-arity arity)
       [(curry func args) outer-scope]

       :else
       (if-let [match (find-match args clauses)]
         (do
           (let [[clause scope-mvs] match
                 scope (-> (m/reduce-state scope scope-mvs)
                             second
                             (merge {:function func}))
                 [result final-scope]
                 ((eval/eval (:body clause)) scope)]
             [result outer-scope]))
         (throw (ex-info
                 (str "function call did not match any defined patterns "
                      "(" (t/pr-value func) " "
                      (clojure.string/join " " (map t/pr-value args)) ")")
                 {:args args :scope outer-scope :function func})))))))

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
