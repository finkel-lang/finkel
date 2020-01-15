# Finkel

[![Build status][build-status]][travis]

Finkel is a statically typed, purely functional, non-strict-by-default
dialect of the [Lisp][lisp] programming language.

Or in other words, [Haskell][haskell] in S-expression.

## Sample code

```lisp
;;;; File: fib.fnk

(:doc "Simple example module for fib executable.

This file contains a function named `main', which will be invoked when
this module were compiled as an executable. The compiled executable
taks an integer argument from command line input and print the
fibonacci number of the argument.")

(defmodule Main
  (import
   (System.Environment (getArgs))))

(defn (:: main (IO ()))
  "The main entry point function."
  (>>= getArgs (. print fib read head)))

(defn (:: fib (-> Int Int))
  "Naive fibonacci function."
  [0] 0
  [1] 1
  [n] (+ (fib (- n 1))
         (fib (- n 2))))
```

## Compiling and running:

    $ finkel make -o fib fib.fnk
    [1 of 1] Compiling Main             ( fib.fnk, fib.o )
    Linking fib
    $ ./fib 10
    55

## Further resources

See the [documentation][doc] for more details.

[travis]: https://travis-ci.org/finkel-lang/finkel
[build-status]: https://travis-ci.org/finkel-lang/finkel.svg?branch=master
[lisp]: https://en.wikipedia.org/wiki/Lisp_(programming_language)
[haskell]: https://haskell.org
[doc]: https://finkel.readthedocs.io/en/latest/
