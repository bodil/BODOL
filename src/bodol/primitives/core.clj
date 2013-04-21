(ns bodol.primitives.core
  (:refer-clojure :exclude [eval])
  (:require [bodol.eval :as eval]
            [bodol.eval.lambda :as lambda]
            [bodol.types :refer [cons-list car cdr]]
            [bodol.types :as t]))

(defn l-quote [[value]]
  (fn [scope]
    [value scope]))

(defn l-define [[name value]]
  (fn [scope]
    (let [[value scope] ((eval/eval value) scope)]
      (if (t/lsymbol? name)
        [value (assoc scope (t/-value name) value)]
        (throw (ex-info "define called with non-symbol"
                        {:args [name value] :scope scope}))))))

;; TODO: get rid of cond, use pattern matching
(defn l-cond [clauses]
  (if-not (zero? (rem (count clauses) 2))
    (throw (ex-info (str "cond takes even number of clause pairs, "
                         (count clauses) " given")
                    {:args clauses}))

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
      (throw (ex-info "recur called outside lambda"
                      {:args args :scope scope})))))



(defmacro defprim [name bindings & body]
  `(defn ~name [args#]
     (fn [scope#]
       (let [[~bindings scope#] (eval/map-eval scope# args#)]
         (try
           [(do ~@body) scope#]
           (catch clojure.lang.ExceptionInfo e#
             (throw (ex-info (.getMessage e#)
                             (assoc (ex-data e#)
                               {:args args# :scope scope#})))))))))

(defprim l-cons [item list]
  (if (t/cons-list? list)
    (t/lcons item list)
    (throw (ex-info "cons called with non-list" {}))))

(defprim l-car [list]
  (if (t/cons-list? list)
    (when list (car list))
    (throw (ex-info "car called with non-list" {}))))

(defprim l-cdr [list]
  (if (t/cons-list? list)
    (when list (cdr list))
    (throw (ex-info "cdr called with non-list" {}))))

(defprim l-atom? [value]
  (t/lboolean (not (t/cons-list? value))))

(defprim l-eq [v1 v2]
  (t/lboolean (= v1 v2)))

(defprim l-list values
  (apply cons-list values))



(def primitives
  {"quote" l-quote
   "define" l-define
   "recur" l-recur
   "cond" l-cond
   "cons" l-cons
   "car" l-car
   "cdr" l-cdr
   "atom?" l-atom?
   "=" l-eq
   "list" l-list})
