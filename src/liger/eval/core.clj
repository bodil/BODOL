(ns liger.eval.core
  (:refer-clojure :exclude [eval])
  (:require [liger.monad :as m]))

(defprotocol Eval
  (-eval [this]))

(defn eval [value]
  (-eval value))

(defn map-eval [scope values]
  (m/map-state scope (map eval values)))
