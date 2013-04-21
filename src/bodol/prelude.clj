(ns bodol.prelude)

(def prelude
  '[(ƒ not
       true -> false
       false -> true)

    (ƒ car
       () -> ()
       (a . _) -> a)

    (ƒ cdr
       () -> ()
       (_ . d) -> d)

    (ƒ caar l -> (car (car l)))
    (ƒ cadr l -> (car (cdr l)))
    (ƒ cddr l -> (cdr (cdr l)))])

(bodol.repl/eval-form (cons 5 '(1 2 3)))
