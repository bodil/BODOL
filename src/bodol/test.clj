(ns bodol.test
  (:refer-clojure :exclude [test])
  (:require [bodol.repl :as repl]
            [bodol.types :as t]
            [bodol.parser :as parser]))

(defn run []
  (repl/eval (slurp "src/bodol/test.bodol")))
