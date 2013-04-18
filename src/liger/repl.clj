(ns liger.repl
  (:refer-clojure :exclude [eval])
  (:require [liger.eval :as eval]
            [liger.parser :as parser]
            [liger.scope :as scope]
            [liger.monad :as m]
            [liger.types :as t]))

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
