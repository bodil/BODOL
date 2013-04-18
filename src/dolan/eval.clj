(ns dolan.eval
  (:refer-clojure :exclude [eval])
  (:require [dolan.eval.core :as eval]
            [dolan.eval.primtypes]
            [dolan.eval.lambda]))

(def eval eval/eval)
(def map-eval eval/map-eval)
