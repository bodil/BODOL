(ns bodol.prelude
  (:require [bodol.parser :refer [parse]]))

(def prelude
  (parse (slurp (clojure.java.io/resource "BODOL/prelude.bodol"))))
