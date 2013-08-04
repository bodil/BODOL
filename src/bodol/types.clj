(ns bodol.types
  (:require [clojure.string :as string]
            [clojure.walk :as walk]))

(defn pr-value [value]
  (if (nil? value) "nil"
      (.toString value)))

(defprotocol LType
  (-value [this]))

(defprotocol IPositioned
  (-pos [this]))

(defmacro defltype [tname to-string]
  (let [lname (clojure.string/lower-case (name tname))
        lname? (symbol (str lname "?"))
        lname (symbol lname)
        cname (symbol (str (name tname) "."))]
    `(do
       (deftype ~tname [value# pos#]
         Object
         (toString [_] (let [~'value value#] ~to-string))
         (equals [_ o#]
           (and (instance? ~tname o#) (= value# (-value o#))))
         LType
         (-value [_] value#)

         IPositioned
         (-pos [_] pos#))

       (defn ~lname? [v#]
         (instance? ~tname v#))

       (defn ~lname
         ([v#] (~cname v# nil))
         ([v# pos#] (~cname v# pos#))))))

(defltype LBoolean (if value "#t" "#f"))
(defltype LNumber (pr-str value))
(defltype LString (pr-str value))
(defltype LSymbol (str value))

(defn atom? [value]
  (some #(% value) [lboolean? lnumber? lstring? lsymbol?]))

(declare cons-list?)
(declare seq-cons-list)

(defprotocol ConsCell
  (car [this])
  [cdr [this]])

(deftype LCons [a d pos]
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

  IPositioned
  (-pos [this] pos)

  ConsCell
  (car [this] a)
  (cdr [this] d)

  clojure.lang.Seqable
  (seq [this] (seq-cons-list this)))

(defn lcons? [v] (instance? LCons v))
(defn lcons
  ([pos a d] (LCons. a d pos))
  ([a d] (LCons. a d nil)))

(defn cons-list? [cell]
  (or (nil? cell)
      (and (lcons? cell)
           (cons-list? (cdr cell)))))

(defn llist
  ([l pos]
     (when (seq l)
       (LCons. (first l) (llist (rest l) pos) pos)))
  ([l] (llist l nil)))

;; (defn cons-list [& l]
;;   (if-not (seq l)
;;     nil
;;     (LCons. (first l) (apply cons-list (rest l)) nil)))

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
   (seq? form)
   (if (and (= 3 (count form))
            (= '. (second form)))
     (LCons. (clj->ltype (first form)) (clj->ltype (nth form 2)) nil)
     (llist (map clj->ltype form)))

   (number? form) (lnumber form)
   (string? form) (lstring form)
   (or (true? form) (false? form)) (lboolean form)
   (symbol? form) (lsymbol (name form))
   :else (throw (ex-info (str "cljvalue " (pr-str form)
                              " has no corresponding type")
                         {:form form}))))
