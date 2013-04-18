(ns dolan.scope
  (:require [dolan.primitives :refer [primitives]]
            [dolan.eval :as eval]
            [dolan.prelude :refer [prelude]]
            [dolan.types :as t]
            [dolan.monad :as m]))

(defn scope []
  (-> (primitives)
      (m/reduce-state (map (comp eval/eval t/clj->ltype) prelude))
      second))
