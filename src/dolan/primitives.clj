(ns dolan.primitives
  (:require [dolan.lambda :refer [lambda]]
            [dolan.primitives.core :as core]
            [dolan.primitives.numbers :as numbers]))

(defn primitives []
  (merge core/primitives
         numbers/primitives
         {"λ" lambda
          "fn" lambda
          "lambda" lambda}))
