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

Test Drive
----------

The BODOL prototype interpreter is written in Clojure, so you'll need
to install [Leiningen](http://leiningen.org/) in order to run it.
Optionally, you may also want to install
[rlwrap](http://utopia.knoware.nl/~hlub/rlwrap/#rlwrap) for a better
line editing experience. Once that's done, check out the BODOL repo,
cd over there and type:

```
$ ./repl
BODOL version 0.0.0
You are in a maze of twisty little passages, all alike.
→→
```

If you're discouraged by the curious absence of `λ` and `ƒ` on your
keyboard, you can substitute Clojure's `fn` and `defn` or Common
Lisp's `lambda` and `defun` respectively.

If you're using Emacs, I find this helps:

```elisp
;; Keybindings for λ and ƒ
(global-set-key (kbd "M-l") (lambda () (interactive) (insert "\u03bb"))) ;lambda
(global-set-key (kbd "M-f") (lambda () (interactive) (insert "\u0192"))) ;function

;; Launch the BODOL REPL in an inferior-lisp buffer
(defun bodol-repl ()
  (interactive)
  (run-lisp "<path to your BODOL repo>/repl"))
```