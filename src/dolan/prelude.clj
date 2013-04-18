(ns dolan.prelude)

(def prelude
  '[(define caar (lambda l -> (car (car l))))
    (define cadr (lambda l -> (car (cdr l))))
    (define cddr (lambda l -> (cdr (cdr l))))])
