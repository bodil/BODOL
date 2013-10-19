(ns bodol.primitives.numbers
  (:require [bodol.eval :as eval]
            [bodol.types :as t]
            [bodol.primitives.core :refer [defprim]]
            [bodol.type.vars :refer [Num Str Bool Sym function pair]]))

(defmacro defnumf [sig name args & body]
  `(defprim ~sig ~name ~args
     (let ~(apply vector (interleave args (map (fn [x#] `(t/-value ~x#)) args)))
       (t/lnumber (do ~@body)))))

(defmacro defboolf [sig name args & body]
  `(defprim ~sig ~name ~args
     (let ~(apply vector (interleave args (map (fn [x#] `(t/-value ~x#)) args)))
       (t/lboolean (do ~@body)))))

(defnumf (function Num Num Num) plus [n1 n2] (+ n1 n2))
(defnumf (function Num Num Num) minus [n1 n2] (- n1 n2))
(defnumf (function Num Num Num) multiply [n1 n2] (* n1 n2))
(defnumf (function Num Num Num) divide [n1 n2] (/ n1 n2))
(defnumf (function Num Num Num) remainder [n1 n2] (rem n1 n2))

(defboolf (function Num Num Bool) less-than [n1 n2] (< n1 n2))
(defboolf (function Num Num Bool) greater-than [n1 n2] (> n1 n2))
(defboolf (function Num Num Bool) less-than-or-equal [n1 n2] (<= n1 n2))
(defboolf (function Num Num Bool) greater-than-or-equal [n1 n2] (>= n1 n2))

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
   "⋜" less-than-or-equal
   ">=" greater-than-or-equal
   "⋝" greater-than-or-equal})
