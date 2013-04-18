(ns dolan.repl
  (:refer-clojure :exclude [eval])
  (:require [dolan.eval :as eval]
            [dolan.parser :as parser]
            [dolan.scope :as scope]
            [dolan.monad :as m]
            [dolan.types :as t]))

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
  `(t/pr-value (eval-forms (list ~@(for [form forms] `(quote ~form))))))
