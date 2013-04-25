(ns bodol.scope
  (:require [bodol.primitives :refer [primitives]]
            [bodol.eval.core :as eval]
            [bodol.prelude :refer [prelude]]
            [bodol.types :as t]
            [bodol.monad :as m]))

(defn scope []
  (-> (primitives)
      (m/reduce-state (map eval/eval prelude))
      second))
