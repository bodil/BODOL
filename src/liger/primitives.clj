(ns liger.primitives
  (:require [liger.lambda :refer [lambda defun]]
            [liger.primitives.core :as core]
            [liger.primitives.numbers :as numbers]))

(defn primitives []
  (merge core/primitives
         numbers/primitives
         {"λ" lambda
          "fn" lambda
          "lambda" lambda
          "ƒ" defun
          "defn" defun
          "defun" defun}))
