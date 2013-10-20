(ns bodol.type.vars
  (:refer-clojure :exclude [list]))

(defrecord Variable [id])

(defrecord Oper [name args])

(def Num (Oper. "Num" []))
(def Str (Oper. "Str" []))
(def Bool (Oper. "Bool" []))
(def Sym (Oper. "Sym" []))

(defn function [& args]
  (if (<= (count args) 2)
    (Oper. "→" (vec args))
    (Oper. "→" [(first args) (apply function (rest args))])))

(defn pair [& args]
  (if (<= (count args) 2)
    (Oper. "×" (vec args))
    (Oper. "×" [(first args) (apply pair (rest args))])))

(defn list [v1]
  (Oper. "list" [v1]))
