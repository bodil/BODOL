(ns bodol.prelude
  (:require [bodol.parser :refer [parse]]))

(def prelude
  (parse (slurp "src/bodol/prelude.bodol")))
