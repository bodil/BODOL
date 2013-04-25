(ns bodol.primitives
  (:require [bodol.lambda :refer [lambda defun]]
            [bodol.primitives.core :as core]
            [bodol.primitives.numbers :as numbers]))

(defn primitives []
  (merge core/primitives
         numbers/primitives))
