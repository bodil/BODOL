(ns dolan.types
  (:require [clojure.string :as string]
            [clojure.walk :as walk]))

(defn pr-value [value]
  (.toString value))

(defrecord LBoolean [value]
  Object
  (toString [_]
    (if value "#t" "#f")))

(defrecord LNumber [value]
  Object
  (toString [_]
    (pr-str value)))

(defn lnumber? [value]
  (instance? LNumber value))

(defrecord LString [value]
  Object
  (toString [_]
    (pr-str value)))

(defn lstring? [value]
  (instance? LString value))

(defrecord LSymbol [value]
  Object
  (toString [_] value))

(defn lsymbol? [value]
  (instance? LSymbol value))

(declare cons-list?)
(declare seq-cons-list)

(defprotocol ConsCell
  (car [this])
  [cdr [this]])

(deftype LCons [head tail]
  Object
  (toString [this]
    (if (cons-list? this)
      (str "(" (string/join " " (map pr-value this)) ")")
      (str "(" head " . " tail ")")))
  (equals [this other]
    (and (instance? LCons other)
         (= head (car other))
         (= tail (cdr other))))

  ConsCell
  (car [this] head)
  (cdr [this] tail)

  clojure.lang.Seqable
  (seq [this] (seq-cons-list this)))

(defn cons-list? [cell]
  (or (nil? cell)
      (and (instance? LCons cell)
           (cons-list? (cdr cell)))))

(defn cons-list [& l]
  (if-not (seq l)
    nil
    (LCons. (first l) (apply cons-list (rest l)))))

(defn seq-cons-list [cell]
  (lazy-seq
   (when-not (nil? cell)
     (cons (car cell) (cdr cell)))))

(deftype LVector [values]
  Object
  (toString [this]
    (if (cons-list? this)
      (str "(" (string/join " " (map pr-value this)) ")")
      (str "(" car " . " cdr ")")))

  clojure.lang.Seqable
  (seq [this] (seq values)))



(defn clj->ltype [form]
  (cond
   (list? form) (apply cons-list (map clj->ltype form))
   (number? form) (LNumber. form)
   (string? form) (LString. form)
   (or (true? form) (false? form)) (LBoolean. form)
   (symbol? form) (LSymbol. (name form))
   :else (throw (ex-info (str "cljvalue " (pr-str form)
                              " has no corresponding type")
                         {:form form}))))
