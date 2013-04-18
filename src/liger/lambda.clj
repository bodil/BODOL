(ns liger.lambda
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as string]
            [liger.parser :refer [parse]]
            [liger.types :as t]))

(def prog (first (parse "(a b -> (cons a b))")))

(def ^:private alphabet "abcdefghijklmnopqrstuvwxyz")

(defn gen-args [num]
  (map str (take num alphabet)))

(defprotocol Currying
  (curry [this args]))

(defrecord Lambda [clauses arity scope curried-args]
  Object
  (toString [this]
    (let [vars (gen-args (- (inc arity) (count curried-args)))]
      (str "(λ "
           (if (zero? arity) (str "-> " (first vars))
               (string/join " -> " vars)) ")")))

  Currying
  (curry [this args]
    (Lambda. clauses arity scope args)))

(defn lambda? [val]
  (instance? Lambda val))

(defn- arrow? [val]
  (and (t/lsymbol? val) (= "->" (:value val))))

(defn- arg? [val]
  (not (arrow? val)))

(defn parse-def [l]
  (let [[state leftovers cs]
        (reduce
         (fn [state next]
           (match [state next]
             [[:arrow args acc] body]
             [:args [] (conj acc {:args args :body body})]

             [[:args args acc] (next :guard arrow?)]
             [:arrow args acc]

             [[:args args acc] (next :guard arg?)]
             [:args (conj args next) acc]))
         [:args [] []]
         l)]
    (cond
     (or (nil? cs) (zero? (count cs)) (not-empty leftovers) (not= :args state))
     (throw (ex-info (str "malformed function declaration " l)
                      {:args l}))

     (not= 1 (count (set (map (comp count :args) cs))))
     (throw (ex-info (str "function declarations have different arities " l)
                     {:args l}))

     :else [cs ((comp count :args) (first cs))])))

(defn lambda [args]
  (fn [scope]
    (let [[clauses arity] (parse-def args)
          f (Lambda. clauses arity scope [])]
      [f scope])))

(defn defun [[name & args]]
  (fn [scope]
    (let [[clauses arity] (parse-def args)
          name (:value name)
          fn-scope (assoc scope name (scope "recur")
                          :name name)
          f (Lambda. clauses arity fn-scope [])
          def-scope (assoc scope name f)]
      [f def-scope])))
