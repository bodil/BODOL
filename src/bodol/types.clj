(ns bodol.types
  (:require [clojure.string :as string]
            [clojure.walk :as walk]))

(declare cons-list)

(defn pr-value [value]
  (if (nil? value) "nil"
      (.toString value)))

(defprotocol LType
  (-value [this]))

(defmacro defltype [tname to-string]
  (let [lname (clojure.string/lower-case (name tname))
        lname? (symbol (str lname "?"))
        lname (symbol lname)
        cname (symbol (str (name tname) "."))]
    `(do
       (deftype ~tname [value#]
         Object
         (toString [_] (let [~'value value#] ~to-string))
         (equals [_ o#]
           (and (instance? ~tname o#) (= value# (-value o#))))
         LType
         (-value [_] value#))

       (defn ~lname? [v#]
         (instance? ~tname v#))

       (defn ~lname [v#]
         (~cname v#)))))

(defltype LBoolean (if value "#t" "#f"))
(defltype LNumber (pr-str value))
(defltype LString (pr-str value))
(defltype LSymbol value)

(defn atom? [value]
  (some #(% value) [lboolean? lnumber? lstring? lsymbol?]))

(declare cons-list?)
(declare seq-cons-list)

(defprotocol ConsCell
  (car [this])
  [cdr [this]])

(deftype LCons [a d]
  Object
  (toString [this]
    (if (cons-list? this)
      (str "(" (string/join " " (map pr-value this)) ")")
      (str "(" a " . " d ")")))
  (equals [this other]
    (and (instance? LCons other)
         (= a (car other))
         (= d (cdr other))))

  LType
  (-value [this] (seq this))

  ConsCell
  (car [this] a)
  (cdr [this] d)

  clojure.lang.Seqable
  (seq [this] (seq-cons-list this)))

(defn lcons? [v] (instance? LCons v))
(defn lcons [a d] (LCons. a d))

(defn cons-list? [cell]
  (or (nil? cell)
      (and (lcons? cell)
           (cons-list? (cdr cell)))))

(defn cons-list [& l]
  (if-not (seq l)
    nil
    (LCons. (first l) (apply cons-list (rest l)))))

(defn seq-cons-list [cell]
  (lazy-seq
   (when-not (nil? cell)
     (cons (car cell) (cdr cell)))))



(defn clj->ltype
  "Convert a Clojure form into a BODOL form.

This is NOT a generic function for converting Clojure values.
Because of the special handling of BODOL cons cell syntax, it
should ONLY be used for turning BODOL code written in Clojure
into a BODOL data structure suitable for evaling."
  [form]
  (cond
   (list? form)
   (if (and (= 3 (count form))
            (= '. (second form)))
     (LCons. (clj->ltype (first form)) (clj->ltype (nth form 2)))
     (apply cons-list (map clj->ltype form)))

   (number? form) (LNumber. form)
   (string? form) (LString. form)
   (or (true? form) (false? form)) (LBoolean. form)
   (symbol? form) (LSymbol. (name form))
   :else (throw (ex-info (str "cljvalue " (pr-str form)
                              " has no corresponding type")
                         {:form form}))))
