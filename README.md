# Finkel

[![CI status][ci-badge]][ci]
[![Documentation][doc-badge]][doc]
[![Codecov][codecov-badge]][codecov]

Finkel is a statically typed, purely functional, and non-strict-by-default
[LISP][lisp] flavored programming language.

Or in other words, **[Haskell][haskell] in S-expression**.


## Features

- Integration with existing Haskell modules.
- Building Haskell-compatible [Cabal][cabal] packages.
- Documentation generation with [Haddock][haddock].
- Lisp style macro system.
- Tool executable, including interactive REPL.

## Example

### Sample code

```clojure
;;;; File: fib.hs

(:doc "Simple example module to show fibonacci number.

The compiled executable takes an integer argument from command line
input and print the fibonacci number of the argument.")

(defmodule Main
  (import
   (System.Environment [getArgs])))

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

### Compiling an executable

```console
$ finkel make -o fib fib.hs
[1 of 1] Compiling Main             ( fib.hs, fib.o )
Linking fib
$ ./fib 10
55
```

### Running REPL

```console
$ finkel repl
Hit `Ctrl-d' or type ,q to quit, type ,? for help.
> ,load fib.hs
[1 of 1] Compiling Main             ( fib.hs, interpreted )
; loaded fib.hs
> ,info fib
fib :: Int -> Int       -- Defined at fib.hs:16:11
> (map fib [1 .. 10])
[1,1,2,3,5,8,13,21,34,55]
> (System.Environment.withArgs ["10"] main)
55
> ,q
```

## Further resources

See the [documentation][doc] for more details.


## Contributing

Contributions are welcome. Please see the [CONTRIBUTING.md][contrib].

[ci-badge]: https://img.shields.io/github/actions/workflow/status/finkel-lang/finkel/ci.yml?logo=github&label=ci
[ci]: https://github.com/finkel-lang/finkel/actions/workflows/ci.yml
[doc-badge]: http://readthedocs.org/projects/finkel/badge/?version=latest
[doc]: https://finkel.readthedocs.io/en/latest/
[codecov-badge]: https://codecov.io/gh/finkel-lang/finkel/branch/master/graph/badge.svg
[codecov]: https://codecov.io/gh/finkel-lang/finkel

[cabal]: https://www.haskell.org/cabal/
[contrib]: https://github.com/finkel-lang/finkel/blob/master/CONTRIBUTING.md
[haddock]: https://www.haskell.org/haddock/
[haskell]: https://haskell.org
[lisp]: https://en.wikipedia.org/wiki/Lisp_(programming_language)
