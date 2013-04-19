(ns bodol.eval.primtypes
  (:require [bodol.eval.core :as eval]
            [bodol.types :as t])
  (:import [bodol.types LNumber LString LSymbol LBoolean]))

(extend-protocol eval/Eval
  LNumber
  (-eval [this]
    (fn [scope] [this scope]))

  LString
  (-eval [this]
    (fn [scope] [this scope]))

  LBoolean
  (-eval [this]
    (fn [scope] [this scope]))

  LSymbol
  (-eval [this]
    (fn [scope]
      (if-let [val (scope (:value this))]
        [val scope]
        (throw (ex-info (str "unbound symbol \"" this "\"")
                        {:value this :scope scope})))))

  nil
  (-eval [this]
    (fn [scope] [nil scope])))
