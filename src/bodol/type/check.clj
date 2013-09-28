(ns bodol.type.check
  (:require [bodol.types :as t]
            [bodol.lambda :as l]
            [bodol.monad :as m]
            [bodol.parser :as parser]
            [clojure.string :as str])
  (:import [bodol.types LCons LBoolean LNumber LString LSymbol]
           [bodol.lambda Lambda]))

(defprotocol IState
  (fresh-var [this])
  (fresh-lvar [this t])
  (generic? [this t])
  (lookup-env [this name])
  (bind-env [this name value])
  (set-nongen [this t])
  (bind-lvar [this t lvar])
  (unscope [this other])
  (prune [this t])
  (unify [this t1 t2])
  (-unified [this])
  (-env [this])
  (-nongen [this])
  (-lvars [this])
  (-next [this]))

(defprotocol SyntaxNode
  (analyse [this]))

(defprotocol TypeValue
  (to-string [this state]))

(defrecord Variable [id]
  TypeValue
  (to-string [this state]
    (let [pruned (prune state this)]
      (if (= pruned this)
        id
        (to-string pruned state)))))

(defrecord Oper [name args]
  TypeValue
  (to-string [_ state]
    (cond
     (zero? (count args)) name
     (= 2 (count args)) (str "(" (to-string (first args) state) " " name
                             " " (to-string (second args) state) ")")
     :else (str name " " (str/join " " (map #(to-string % state) args))))))



(def Num (Oper. "Num" []))
(def Str (Oper. "Str" []))
(def Bool (Oper. "Bool" []))
(def Sym (Oper. "Sym" []))

(defn function [& args]
  (if (<= (count args) 2)
    (Oper. "→" (vec args))
    (Oper. "→" [(first args) (apply function (rest args))])))



(declare occurs-in? occurs-in-type?)

(deftype State [unified env nongen lvars next]
  IState
  (fresh-var [_]
    (let [inced (-> next int inc char)]
      [(Variable. (str next)) (State. unified env nongen lvars inced)]))

  (fresh-lvar [this t]
    (if-let [lvar (lvars t)]
      [lvar this]
      (let [[lvar state] (fresh-var this)]
        [lvar (bind-lvar state t lvar)])))

  (bind-lvar [this t lvar]
    (State. unified env nongen (assoc lvars t lvar) next))

  (generic? [this t]
    (not (occurs-in? this nongen t)))

  (lookup-env [_ name]
    (env name))

  (bind-env [_ name value]
    (State. unified (assoc env name value) nongen lvars next))

  (set-nongen [_ t]
    (State. unified env (conj nongen t) lvars next))

  (unscope [_ other]
    (State. (-unified other) env nongen (-lvars other) (-next other)))

  (prune [this t]
    (cond
     (and (instance? Variable t)
          (generic? this t)
          (lvars t))
     (prune this (lvars t))

     (and (instance? Variable t)
          (contains? unified t))
      (prune this (unified t))

      :else t))

  (unify [this t1 t2]
    (let [t1 (prune this t1)
          t2 (prune this t2)]
      (cond

       (and (instance? Variable t1) (not= t1 t2))
       (if (occurs-in-type? this t1 t2)
         (throw (Error. (str "recursive unification between " t1 " and " t2)))
         (State. (assoc unified t1 t2) env nongen lvars next))

       (and (instance? Oper t1) (instance? Variable t2))
       (unify this t2 t1)

       (and (instance? Oper t1) (instance? Oper t2))
       (if (or (not= (:name t1) (:name t2))
               (not= (count (:args t1)) (count (:args t2))))
         (throw (Error. (str "Type mismatch: " (to-string t1 this)
                             " ≠ " (to-string t2 this))))
         (loop [state this a (:args t1) b (:args t2)]
           (if (and (seq a) (seq b))
             (recur (unify state (first a) (first b)) (rest a) (rest b))
             state)))

       :else (throw (Error. (str "Cannot unify args " t1 " and " t2))))))

  (-unified [_] unified)
  (-env [_] env)
  (-nongen [_] nongen)
  (-lvars [_] lvars)
  (-next [_] next)

  Object
  (toString [_]
    (let [env (dissoc env "pair" "true" "cond" "zero" "pred" "times")]
      (str unified " :: " env " :: " nongen
           " :: " lvars " :: " next))))

(defn occurs-in? [^State state ^Variable t l]
  (some #(occurs-in-type? state t %) l))

(defn occurs-in-type? [^State state ^Variable v type2]
  (let [v (prune state v)]
    (cond
     (= v type2) true
     (instance? Oper type2) (occurs-in? state v (:args type2))
     :else false)))

(defn fresh [t]
  (fn [^State state]
    (let [t (prune state t)]
      (cond
       (instance? Variable t)
       (if-not (generic? state t)
         [t state]
         (fresh-lvar state t))

       (instance? Oper t)
       (let [{:keys [name args]} t]
         (let [[args state]
               (m/map-state state (map fresh args))]
           [(Oper. name args) state]))

       :else
       (throw (Error. (str "Don't know how to fresh " t)))))))

(defn resolve [t state]
  (let [pruned (prune state t)]
    (if (= pruned t) t pruned)))

(defn bind-arg [arg]
  (fn [^State state]
    (let [[argtype state] (fresh-var state)
          state (-> state
                    (bind-env (t/-value arg) argtype)
                    (set-nongen argtype))]
      [argtype state])))

(defn bind-args [state args]
  (m/map-state state (map bind-arg args)))



(extend-protocol SyntaxNode
  LNumber
  (analyse [this]
    (fn [^State state]
      [Num state]))

  LString
  (analyse [this]
    (fn [^State state]
      [Str state]))

  LBoolean
  (analyse [this]
    (fn [^State state]
      [Bool state]))

  LSymbol
  (analyse [this]
    (fn [^State state]
      (let [name (t/-value this)
            sym (lookup-env state name)]
        (if sym
          ((fresh sym) state)
          (throw (Error. (str "Undefined symbol " name " ::: " state)))))))

  LCons
  (analyse [this]
    (let [func (t/car this)
          args (t/cdr this)]

      (fn [^State state]
        (let [[functype state] ((analyse func) state)]
          (loop [args args
                 state state
                 functype functype]
            (if-let [arg (when (t/lcons? args) (t/car args))]
              (let [[argtype state] ((analyse arg) state)
                    [resulttype state] (fresh-var state)
                    state (unify state (function argtype resulttype) functype)]
                (recur (t/cdr args) state resulttype))
              [functype state]))))))

  Lambda
  (analyse [this]
    (fn [^State state]
      (let [{:keys [args body]} (first (l/-clauses this))
            [argtypes new-state] (bind-args state args)
            [resulttype new-state] ((analyse body) new-state)]
        [(apply function (concat argtypes (list resulttype)))
         (unscope state new-state)]))))



(defn fresh-state []
  (let [state (State. {} {} #{} {} \a)
        [var1 state] (fresh-var state)
        [var2 state] (fresh-var state)
        [var3 state] (fresh-var state)
        pairtype (Oper. "×" [var1 var2])]
    (-> state
        (bind-env "+" (function Num Num Num)))))

(defn- check-sexp-m [sexp]
  (fn [^State state]
    ((analyse sexp) state)))

(defn type-check
  ([prog state]
     (m/map-state state (map check-sexp-m prog)))
  ([prog] (type-check prog (fresh-state))))

(defn print-result [[types state]]
  (doseq [type types]
    (println (to-string type state))))



#_(-> (parser/parse "(ƒ double-add m n → (+ (+ m m) (+ n n)))")
      type-check
      print-result)
