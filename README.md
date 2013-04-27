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

# Inspiration

Clojure is great, but sometimes I miss static typing. Haskell is
great, but sometimes I miss Lisp's elegantly simple syntax. Shen is a
splendid compromise, but I found myself wishing it came with Haskell's
uncompromising purity and Standard ML's straightforward type system.
BODOL is my attempt at crossbreeding the three, with Shen obviously
the major influence.

# A Taste of BODOL

Functional hello world:

```lisp
(ƒ factorial
  0 -> 1
  n -> (* n (factorial (- n 1))))

(ƒ fibonacci
  0 -> 0
  1 -> 1
  n -> (+ (fibonacci (- n 1)) (fibonacci (- n 2))))
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

Take a look at the theoretically comprehensive
[test suite](src/bodol/test.bodol) for examples of all the currently
defined language features.

# Test Drive

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

## What's with those non-ASCII characters?

If you're discouraged by the curious absence of `λ` and `ƒ` on your
keyboard, you can substitute Clojure's `fn` and `defn` or Common
Lisp's `lambda` and `defun` respectively.

### Emacs bindings

If you're using Emacs, I find this helps:

```lisp
;; Keybindings for λ and ƒ
(global-set-key (kbd "M-l") (lambda () (interactive) (insert "\u03bb"))) ;lambda
(global-set-key (kbd "M-f") (lambda () (interactive) (insert "\u0192"))) ;function

;; Launch the BODOL REPL in an inferior-lisp buffer
(defun bodol-repl ()
  (interactive)
  (run-lisp "<path to your BODOL repo>/repl"))
```

### Vim bindings

Add this snippet to your `.vimrc` to configure some keybindings for `ƒ` and `λ`:

```vim
" Keybindings for λ and ƒ
:inoremap <A-l> <C-v>u3bb<Space>
:inoremap <A-f> <C-v>u192<Space>
```

# License

Copyright 2013 Bodil Stokke

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this file except in compliance with the License. You may
obtain a copy of the License at
[http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0).

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing
permissions and limitations under the License.
