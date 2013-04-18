(ns liger.eval
  (:refer-clojure :exclude [eval])
  (:require [liger.eval.core :as eval]
            [liger.eval.primtypes]
            [liger.eval.lambda]))

(def eval eval/eval)
(def map-eval eval/map-eval)
