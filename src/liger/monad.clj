(ns liger.monad)

(defn reduce-state
  "Evaluate a list of state monad values in sequence, using the
   provided initial state."
  [scope mvs]
  (reduce (fn [[value state] next]
            (next state))
          [nil scope]
          mvs))

(defn map-state
  "Evaluate a list of state monad values in sequence, producing
   a list of the results of each evaluation."
  [scope mvs]
  (reduce (fn [[values state] next]
            (let [[value state] (next state)]
              [(conj values value) state]))
          [[] scope]
          mvs))

(defn state-id [scope]
  [nil scope])
