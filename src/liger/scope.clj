(ns liger.scope
  (:require [liger.primitives :refer [primitives]]
            [liger.eval.core :as eval]
            [liger.prelude :refer [prelude]]
            [liger.types :as t]
            [liger.monad :as m]))

(defn scope []
  (-> (primitives)
      (m/reduce-state (map (comp eval/eval t/clj->ltype) prelude))
      second))
