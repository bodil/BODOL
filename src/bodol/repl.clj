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

(defn -main [& args]
  (println "BODOL version 0.0.0")
  (println "You are in a maze of twisty little passages, all alike.")
  (loop [scope (scope/scope)]
    (print "→→ ")
    (flush)
    (let [input (read-line)]
      (when-not input
        (println)
        (System/exit 0))
      (let [[result scope]
            (try
              (->> input
                   (parser/parse)
                   (map eval/eval)
                   (m/reduce-state scope))
              (catch clojure.lang.ExceptionInfo e
                (let [data (ex-data e)]
                  [(str "Error: " (.getMessage e) "\n"
                        (clojure.string/join
                         "\n"
                         (map (fn [[k v]] (str k ": " (t/pr-value v)))
                              (dissoc data :scope))))
                   scope])))]
        (println (if (string? result) result (t/pr-value result)))
        (recur scope)))))
