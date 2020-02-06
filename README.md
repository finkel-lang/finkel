# Finkel

[![codecov][codecov-status]][codecov]
[![Build status][build-status]][travis]
[![Documentation Status][doc-status]][doc]

Finkel is a statically typed, purely functional, non-strict-by-default
dialect of the [Lisp][lisp] programming language.

Or in other words, [Haskell][haskell] in S-expression.


## Sample code

```clojure
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

## Compiling an executable

```
$ finkel make -o fib fib.fnk
[1 of 1] Compiling Main             ( fib.fnk, fib.o )
Linking fib
$ ./fib 10
55
```

## Running REPL

```
$ finkel repl
Hit `Ctrl-d' or type ,q to quit, type ,? for help.
> (+ 1 2 3 4 5)
15
> ,load fib.fnk
[1 of 1] Compiling Main             ( fib.fnk, interpreted )
; loaded fib.fnk
> ,info fib
fib :: Int -> Int       -- Defined at fib.fnk:18:11
> (map fib [1 .. 10])
[1,1,2,3,5,8,13,21,34,55]
> (System.Environment.withArgs ["10"] main)
55
> ,q
```

## Further resources

See the [documentation][doc] for more details.

[codecov-status]: https://codecov.io/gh/finkel-lang/finkel/branch/master/graph/badge.svg
[codecov]: https://codecov.io/gh/finkel-lang/finkel
[build-status]: https://travis-ci.org/finkel-lang/finkel.svg?branch=master
[travis]: https://travis-ci.org/finkel-lang/finkel
[doc-status]: http://readthedocs.org/projects/finkel/badge/?version=latest
[doc]: https://finkel.readthedocs.io/en/latest/
[lisp]: https://en.wikipedia.org/wiki/Lisp_(programming_language)
[haskell]: https://haskell.org
