(ns liger.primitives
  (:require [liger.lambda :refer [lambda]]
            [liger.primitives.core :as core]
            [liger.primitives.numbers :as numbers]))

(defn primitives []
  (merge core/primitives
         numbers/primitives
         {"λ" lambda
          "fn" lambda
          "lambda" lambda}))
