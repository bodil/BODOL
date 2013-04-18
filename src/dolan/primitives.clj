(ns dolan.primitives
  (:refer-clojure :exclude [eval])
  (:require [dolan.eval :as eval]
            [dolan.types :refer [cons-list]]
            [dolan.monad :as m]
            [dolan.types :as t]
            [dolan.lambda :refer [lambda]])
  (:import [dolan.types LCons LBoolean]))

(defn l-quote [[value]]
  (fn [scope]
    [value scope]))

(defn l-define [[name value]]
  (fn [scope]
    (let [[value scope] ((eval/eval value) scope)]
      (if (t/lsymbol? name)
        [value (assoc scope (:value name) value)]
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
              (if-not (or (nil? result) (= (LBoolean. false) result))
                ((eval/eval then) scope)
                (recur (rest clauses) scope)))))))))



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
    (LCons. item list)
    (throw (ex-info "cons called with non-list" {}))))

(defprim l-car [list]
  (if (t/cons-list? list)
    (first list)
    (throw (ex-info "car called with non-list" {}))))

(defprim l-cdr [list]
  (if (t/cons-list? list)
    (apply cons-list (rest list))
    (throw (ex-info "cdr called with non-list" {}))))

(defprim l-atom? [value]
  (LBoolean. (not (t/cons-list? value))))

(defprim l-eq [v1 v2]
  (LBoolean. (= v1 v2)))


(defn primitives []
  {"quote" l-quote
   "define" l-define
   "cond" l-cond
   "cons" l-cons
   "car" l-car
   "cdr" l-cdr
   "atom?" l-atom?
   "=" l-eq
   "lambda" lambda
   "fn" lambda
   "λ" lambda})
