(ns bodol.repl
  (:refer-clojure :exclude [eval])
  (:require [bodol.eval :as eval]
            [bodol.parser :as parser]
            [bodol.scope :as scope]
            [bodol.monad :as m]
            [bodol.types :as t]
            [bodol.error :as err]))

(defn eval-ast [scope ast]
  (->> ast
       (map eval/eval)
       (m/reduce-state scope)
       first
       t/pr-value))

(defn eval
  ([scope string] (eval-ast scope (parser/parse string)))
  ([string] (eval (scope/scope) string)))

(defn eval-file
  ([scope filename] (eval-ast scope (parser/parse-file filename)))
  ([filename] (eval-file (scope/scope) filename)))

(defn eval-forms [forms]
  (-> (scope/scope)
      (m/reduce-state (map (comp eval/eval t/clj->ltype) forms))
      first))

(defmacro eval-form [& forms]
  `(let [value# (eval-forms (list ~@(for [form forms] `(quote ~form))))]
     (t/pr-value value#)))

(defn -main [& args]
  (println "BODOL version 0.0.0")
  (println "You are in a maze of twisty little passages, all alike.")
  (loop [scope (scope/scope) input nil]
    (print (if input "   " "→→ "))
    (flush)
    (let [input (str (or input "") (read-line))]
      (when-not input
        (println)
        (System/exit 0))
      (if (parser/incomplete? input)
        (recur scope input)
        (let [[result r-scope]
              (try
                (->> input
                     (parser/parse)
                     (map eval/eval)
                     (m/reduce-state scope))
                (catch clojure.lang.ExceptionInfo e
                  (print (err/report e))))]
          (if (nil? result)
            (recur scope nil)
            (do
              (println (if (string? result) result (t/pr-value result)))
              (recur r-scope nil))))))))
