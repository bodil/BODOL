(ns bodol.repl
  (:refer-clojure :exclude [eval])
  (:require [bodol.eval :as eval]
            [bodol.parser :as parser]
            [bodol.scope :as scope]
            [bodol.monad :as m]
            [bodol.types :as t]))

(defn eval
  ([scope string]
     (->> (parser/parse string)
          (map eval/eval)
          (m/reduce-state scope)
          first
          t/pr-value))
  ([string]
     (eval (scope/scope) string)))

(defn eval-forms [forms]
  (-> (scope/scope)
      (m/reduce-state (map (comp eval/eval t/clj->ltype) forms))
      first))

(defmacro eval-form [& forms]
  `(let [value# (eval-forms (list ~@(for [form forms] `(quote ~form))))]
     (t/pr-value value#)))
