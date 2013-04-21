(ns bodol.prelude)

(def prelude
  '[(ƒ atom?
       () -> false
       (_ . _) -> false
       _ -> true)

    (ƒ not
       true -> false
       false -> true)

    (ƒ =
       a a -> true
       _ _ -> false)

    (ƒ car
       () -> ()
       (a . _) -> a)

    (ƒ cdr
       () -> ()
       (_ . d) -> d)

    (ƒ caar l -> (car (car l)))
    (ƒ cadr l -> (car (cdr l)))
    (ƒ cddr l -> (cdr (cdr l)))])
