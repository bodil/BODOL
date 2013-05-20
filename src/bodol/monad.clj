(ns bodol.monad)

(defn reduce-state
  "Evaluate a list of state monad values in sequence, using the
   provided initial state."
  [state mvs]
  (reduce (fn [[value state] next]
            (next state))
          [nil state]
          mvs))

(defn map-state
  "Evaluate a list of state monad values in sequence, producing
   a list of the results of each evaluation."
  [state mvs]
  (reduce (fn [[values state] next]
            (let [[value state] (next state)]
              [(conj values value) state]))
          [[] state]
          mvs))

(defn state-id [state]
  [nil state])
