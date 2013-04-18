(ns dolan.primitives
  (:refer-clojure :exclude [eval])
  (:require [dolan.eval :as eval]
            [dolan.types :refer [cons-list]]
            [dolan.monad :as m]
            [dolan.types :as t]
            [dolan.lambda :refer [lambda]])
  (:import [dolan.types LCons LBoolean]))

(defn quote [[value]]
  (fn [scope]
    [value scope]))

(defn define [[name value]]
  (fn [scope]
    (let [[value scope] ((eval/eval value) scope)]
      (if (t/lsymbol? name)
        [value (assoc scope (:value name) value)]
        (throw (ex-info "define called with non-symbol"
                        {:args [name value] :scope scope}))))))


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

(defprim cons [item list]
  (if (t/cons-list? list)
    (LCons. item list)
    (throw (ex-info "cons called with non-list" {}))))

(defprim car [list]
  (if (t/cons-list? list)
    (first list)
    (throw (ex-info "car called with non-list" {}))))

(defprim cdr [list]
  (if (t/cons-list? list)
    (apply cons-list (rest list))
    (throw (ex-info "cdr called with non-list" {}))))

(defprim atom? [value]
  (LBoolean. (not (t/cons-list? value))))

(defprim eq [v1 v2]
  (LBoolean. (= v1 v2)))


(defn primitives []
  {"quote" quote
   "define" define
   "cons" cons
   "car" car
   "cdr" cdr
   "atom?" atom?
   "=" eq
   "lambda" lambda
   "fn" lambda
   "λ" lambda})
