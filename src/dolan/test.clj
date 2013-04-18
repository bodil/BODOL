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

;;; Tests go here

(tests equality
  (= 1337 1337)
  (= "this is dog fort" "this is dog fort")
  (= '(1 3 3 7) '(1 3 3 7))
  (= 'symbolo 'symbolo)
  (= true true)
  (= false false))

(tests cond
  (cond (= "foo" "bar") false
        true true
        true false))

(tests lists
  (= '(1 2 3) (cons 1 '(2 3)))
  (= 1 (car '(1 2 3)))
  (= '(2 3) (cdr '(1 2 3)))
  (= () (car ()))
  (= () (cdr ()))
  (= '(1) (cons 1 ())))

(tests atomicity
  (atom? 1337)
  (atom? 0)
  (atom? "foo")
  (atom? true)
  (atom? false)
  (not (atom? ()))
  (not (atom? '(1 2 3))))

(test basic-lambda
  (= '("omg" "lol") ((λ a -> (cons a '("lol"))) "omg")))

(test define-in-scope
  (define foo "foobar")
  (= "foobar" foo))

(test lexical-scope
  (define constantly
    (λ a ->
      (λ -> a)))
  (= "foo" ((constantly "foo"))))

(test define-stays-in-local-scope
  (define foo "foobar")
  (define bar
    (λ -> (define foo "barfoo")))
  (bar)
  (= foo "foobar"))
