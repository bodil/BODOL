(ns dolan.test
  (:require [clojure.test :refer [deftest is]]
            [dolan.repl :as repl])
  (:import [dolan.types LBoolean]))

(defmacro test [name & forms]
  `(deftest ~name
     (is (= (LBoolean. true)
            (repl/eval-forms (list ~@(for [form forms] `(quote ~form))))))))

(defmacro tests [name & forms]
  `(deftest ~name
     ~@(for [form forms]
        `(is (= (LBoolean. true)
                (repl/eval-forms (list (quote ~form))))))))


(tests equality
  (= 1337 1337)
  (= "this is dog fort" "this is dog fort")
  (= '(1 3 3 7) '(1 3 3 7))
  (= 'symbolo 'symbolo)
  (= true true)
  (= false false))
