(ns bodol.eval
  (:refer-clojure :exclude [eval])
  (:require [bodol.eval.core :as eval]
            [bodol.eval.primtypes]
            [bodol.eval.lambda]))

(def eval eval/eval)
(def map-eval eval/map-eval)
