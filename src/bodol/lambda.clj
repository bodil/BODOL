(ns bodol.lambda
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as string]
            [bodol.types :as t]
            [bodol.match :refer [find-match]]))

(def ^:private alphabet "abcdefghijklmnopqrstuvwxyz")

(defn gen-args [num]
  (map str (take num alphabet)))

(defprotocol ILambda
  (-name [this])
  (-arity [this])
  (-scope [this])
  (-curried-args [this])
  (curry [this args])
  (bind [this scope])
  (pattern-match [this args]))

(deftype Lambda [pos name clauses arity scope curried-args]
  Object
  (toString [this]
    (let [vars (gen-args (- (inc arity) (count curried-args)))]
      (str "("
           (if name (str "ƒ " name) "λ")
           " "
           (if (zero? arity) (str "-> " (first vars))
               (string/join " -> " vars)) ")")))

  (equals [this o]
    (identical? this o))

  t/IPositioned
  (-pos [this] pos)

  ILambda
  (curry [this args]
    (Lambda. pos name clauses arity scope args))

  (bind [this new-scope]
    (Lambda. pos name clauses arity new-scope curried-args))

  (pattern-match [this args]
    (find-match args clauses))

  (-name [_] name)
  (-arity [_] arity)
  (-scope [_] scope)
  (-curried-args [_] curried-args))

(defmethod print-method Lambda [l ^java.io.Writer writer]
  (.write writer (.toString l)))

(defn lambda? [val]
  (instance? Lambda val))

(defn- parse-def [clauses]
  (let [cs (map (fn [[args body]] {:args args :body body}) clauses)]
    (cond
     (not= 1 (count (set (map (comp count :args) cs))))
     (throw (ex-info (str "function declarations have different arities " clauses)
                     {:args clauses}))

     :else [cs ((comp count :args) (first cs))])))

(defn lambda [pos & clauses]
  (let [[clauses arity] (parse-def clauses)]
    (Lambda. pos nil clauses arity nil [])))

(defn defun [pos name & clauses]
  (let [[clauses arity] (parse-def clauses)]
    (Lambda. pos (t/-value name) clauses arity nil [])))
