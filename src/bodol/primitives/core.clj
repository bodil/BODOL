(ns bodol.primitives.core
  (:refer-clojure :exclude [eval])
  (:require [bodol.eval :as eval]
            [bodol.eval.lambda :as lambda]
            [bodol.types :refer [llist car cdr]]
            [bodol.types :as t]
            [bodol.monad :as m]
            [bodol.error :as err]))

(defn l-quote [[value]]
  (fn [scope]
    [value scope]))

(defn l-define [[name value]]
  (fn [scope]
    (let [[value scope] ((eval/eval value) scope)]
      (if (t/lsymbol? name)
        [value (assoc scope (t/-value name) value)]
        (err/raise scope name :non-symbol-define
                   "define called with non-symbol")))))

;; TODO: get rid of cond, use pattern matching
(defn l-cond [clauses]
  (if-not (zero? (rem (count clauses) 2))
    (err/raise nil (first clauses) :uneven-cond
               (str "cond takes even number of clause pairs, "
                    (count clauses) " given"))

    (let [clauses (partition 2 clauses)]
      (fn [scope]
        (loop [clauses clauses scope scope]
          (if-not (seq clauses)
            [nil scope]

            (let [[test then] (first clauses)
                  [result scope] ((eval/eval test) scope)]
              (if-not (or (nil? result) (= (t/lboolean false) result))
                ((eval/eval then) scope)
                (recur (rest clauses) scope)))))))))

(defn l-recur [args]
  (fn [scope]
    (if-let [func (:function scope)]
      ((lambda/invoke func args) scope)
      (err/raise scope (first args) :recur-outside-lambda
                 "recur called outside lambda"))))

(defn l-asserts [[name & asserts]]
  (fn [scope]
    (loop [scope scope asserts asserts]
      (if-not (seq asserts)
        [(t/lboolean true) scope]

        (let [form (first asserts)
              [result scope] ((eval/eval form) scope)]
          (when-not (= (t/lboolean true) result)
            (err/raise scope form :assert-failure
                       (str "Assert in " (t/-value name) " failed:\n"
                            "    " form
                            " -> " result "\n")))
          (recur scope (rest asserts)))))))

(defn l-assert [[name & body]]
  (fn [scope]
    (let [[result scope] (m/reduce-state scope (map eval/eval body))]
      (when-not (= (t/lboolean true) result)
        (err/raise scope (last body) :assert-failure
                   (str "Assert in " (t/-value name) " failed:\n"
                        "    " (last body)
                        " -> " result "\n"))))
    [(t/lboolean true) scope]))



(defmacro defprim [name bindings & body]
  `(defn ~name [args#]
     (fn [scope#]
       (let [[~bindings scope#] (eval/map-eval scope# args#)]
         (try
           [(do ~@body) scope#]
           (catch clojure.lang.ExceptionInfo e#
             (throw (ex-info (.getMessage e#)
                             (assoc (ex-data e#)
                               {:sexp (first args#)
                                :pos (t/-pos (first args#))
                                :scope scope#})))))))))

(defprim l-cons [item list]
  (if (t/cons-list? list)
    (t/lcons item list)
    (throw (ex-info "cons called with non-list" {}))))

(defprim l-list values
  (llist values))



(def primitives
  {"quote" l-quote
   "define" l-define
   "recur" l-recur
   "cond" l-cond
   "asserts" l-asserts
   "assert" l-assert
   "cons" l-cons
   "list" l-list})
