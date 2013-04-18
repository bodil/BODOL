(ns dolan.eval.primtypes
  (:require [dolan.eval.core :as eval]
            [dolan.types :as t])
  (:import [dolan.types LNumber LString LSymbol LBoolean]))

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
