(ns bodol.primitives.numbers
  (:require [bodol.eval :as eval]
            [bodol.types :as t]
            [bodol.primitives.core :refer [defprim]]))

(defmacro defnumf [name args & body]
  `(defprim ~name ~args
     (let ~(apply vector (interleave args (map (fn [x#] `(t/-value ~x#)) args)))
       (t/lnumber (do ~@body)))))

(defmacro defboolf [name args & body]
  `(defprim ~name ~args
     (let ~(apply vector (interleave args (map (fn [x#] `(t/-value ~x#)) args)))
       (t/lboolean (do ~@body)))))

(defnumf plus [n1 n2] (+ n1 n2))
(defnumf minus [n1 n2] (- n1 n2))
(defnumf multiply [n1 n2] (* n1 n2))
(defnumf divide [n1 n2] (/ n1 n2))
(defnumf remainder [n1 n2] (rem n1 n2))

(defboolf less-than [n1 n2] (< n1 n2))
(defboolf greater-than [n1 n2] (> n1 n2))
(defboolf less-than-or-equal [n1 n2] (<= n1 n2))
(defboolf greater-than-or-equal [n1 n2] (>= n1 n2))

(def primitives
  {"+" plus
   "-" minus
   "*" multiply
   "/" divide
   "%" remainder
   "rem" remainder
   "<" less-than
   ">" greater-than
   "<=" less-than-or-equal
   ">=" greater-than-or-equal})
