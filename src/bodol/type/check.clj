(ns bodol.type.check
  (:refer-clojure :exclude [resolve])
  (:require [bodol.types :as t]
            [bodol.lambda :as l]
            [bodol.monad :as m]
            [bodol.parser :as parser]
            [bodol.type.vars :as v :refer [function Num Str Bool Sym]]
            [clojure.string :as str])
  (:import [bodol.types LCons LBoolean LNumber LString LSymbol]
           [bodol.lambda Lambda]
           [bodol.type.vars Variable Oper]))

(defprotocol SyntaxNode
  (analyse [this]))

(defprotocol TypeValue
  (to-string [this state]))



(declare occurs-in-type? prune)

(defn occurs-in? [state ^Variable t l]
  (some #(occurs-in-type? state t %) l))

(defn occurs-in-type? [state ^Variable v type2]
  (let [v (prune state v)]
    (cond
     (= v type2) true
     (instance? Oper type2) (occurs-in? state v (:args type2))
     :else false)))

(defn generic? [state t]
  (not (occurs-in? state (:nongen state) t)))

(defn prune [state t]
  (cond
   (and (instance? Variable t)
        (generic? state t)
        ((:lvars state) t))
   (prune state ((:lvars state) t))

   (and (instance? Variable t)
        (contains? (:unified state) t))
   (prune state ((:unified state) t))

   :else t))



(extend-type Variable
  TypeValue
  (to-string [this state]
    (let [pruned (prune state this)]
      (if (= pruned this)
        (:id this)
        (to-string pruned state)))))

(extend-type Oper
  TypeValue
  (to-string [this state]
    (let [args (:args this)
          name (:name this)]
      (cond
       (zero? (count args))
       name

       (= 2 (count args))
       (str "(" (to-string (first args) state) " " name
            " " (to-string (second args) state) ")")

       :else (str (str/join " " (map #(to-string % state) args)) " " name)))))



(defn fresh-var [state]
  (assoc state :next (-> (:next state) int inc char)))

(defn bind-lvar [state t lvar]
  (assoc-in state [:lvars t] lvar))

(defn fresh-lvar [state t]
  (if-let [lvar ((:lvars state) t)]
    [lvar state]
    (let [[lvar state] (fresh-var state)]
      [lvar (bind-lvar state t lvar)])))

(defn fresh-var [state]
  (let [next (:next state)
        inced (-> next int inc char)]
    [(Variable. (str next)) (assoc state :next inced)]))

(defn lookup-env [state name]
  (when-let [val ((:env state) name)]
    (if-let [m (:signature (meta val))] m val)))

(defn bind-env [state name value]
  (assoc-in state [:env name] value))

(defn set-nongen [state t]
  (assoc state :nongen (conj (:nongen state) t)))

(defn unscope [state other]
  (assoc other
    :env (:env state)
    :nongen (:nongen state)))

(defn unify [state t1 t2]
  (let [t1 (prune state t1)
        t2 (prune state t2)]
    (cond

     (and (instance? Variable t1) (not= t1 t2))
     (if (occurs-in-type? state t1 t2)
       (throw (Error. (str "recursive unification between " t1 " and " t2)))
       (assoc state :unified (assoc (:unified state) t1 t2)))

       (and (instance? Oper t1) (instance? Variable t2))
       (unify state t2 t1)

       (and (instance? Oper t1) (instance? Oper t2))
       (if (or (not= (:name t1) (:name t2))
               (not= (count (:args t1)) (count (:args t2))))
         (throw (Error. (str "Type mismatch: " (to-string t1 state)
                             " ≠ " (to-string t2 state))))
         (loop [state state a (:args t1) b (:args t2)]
           (if (and (seq a) (seq b))
             (recur (unify state (first a) (first b)) (rest a) (rest b))
             state)))

       :else (throw (Error. (str "Cannot unify args " t1 " and " t2))))))

(defn fresh [t]
  (fn [state]
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
  (fn [state]
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
    (fn [state]
      [Num state]))

  LString
  (analyse [this]
    (fn [state]
      [Str state]))

  LBoolean
  (analyse [this]
    (fn [state]
      [Bool state]))

  LSymbol
  (analyse [this]
    (fn [state]
      (let [name (t/-value this)
            sym (lookup-env state name)]
        (if sym
          ((fresh sym) state)
          (throw (Error. (str "Undefined symbol " name)))))))

  LCons
  (analyse [this]
    (let [func (t/car this)
          args (t/cdr this)]

      (fn [state]
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
    (fn [state]
      (let [{:keys [args body]} (first (l/-clauses this))
            [argtypes new-state] (bind-args state args)
            [resulttype new-state] ((analyse body) new-state)]
        [(apply function (concat argtypes (list resulttype)))
         (unscope state new-state)]))))



(defn fresh-state [scope]
  {:unified {} :env scope :nongen #{} :lvars {} :next \a})

(defn- check-sexp-m [sexp]
  (fn [state]
    ((analyse sexp) state)))

(defn type-check
  ([prog state]
     (m/map-state state (map check-sexp-m prog))))

(defn print-result [[types state]]
  (doseq [type types]
    (println (to-string type state))))




#_(-> (parser/parse "1337")
      (type-check (fresh-state (bodol.scope/scope)))
      print-result)

#_(-> (parser/parse "rainbow-dash")
      (type-check (fresh-state (bodol.scope/scope)))
      print-result)

#_(-> (parser/parse "(ƒ add a b → (+ a b))")
      (type-check (fresh-state (bodol.scope/scope)))
      print-result)

#_(-> (parser/parse "(ƒ double-mult a b → (* (+ a a) (+ b b)))")
      (type-check (fresh-state (bodol.scope/scope)))
      print-result)

#_(-> (parser/parse "(ƒ eq a a → #t a b → #f)")
      (type-check (fresh-state (bodol.scope/scope)))
      print-result)
