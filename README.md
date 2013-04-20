BODOL
=====

This is my language experiment.

Preliminary goals which may or may not be possible to combine:

* Pure functional language.
* Homoiconic in the Lisp tradition.
* Built on the traditional Lisp primitives.
* Hindley-Milner flavoured type system.
* Currying function calls.
* Pattern matching.

This is a work in progress. Don't expect it to even run its own test
suite.

A Taste of BODOL
----------------

Functional hello world:

```lisp
(ƒ fib
  0 -> 0
  1 -> 1
  n -> (+ (fib (- n 1)) (fib (- n 2))))
```

More advanced pattern matching:

```lisp
(ƒ map
   f () -> ()
   f (head . tail) -> (cons (f head) (map f tail)))

(= '(2 3 4) (map (λ a -> (+ a 1)) '(1 2 3))))
```

Currying:

```lisp
(ƒ triplet a b c -> (list a b c))
(define one-and (triplet 1))
(define one-and-two-and (one-and 2))
(= '(1 2 3) (one-and-two-and 3))
```
