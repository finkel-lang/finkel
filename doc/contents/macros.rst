Macros In Finkel
================

This section shows how to write and use macros. Macros in Finkel are
similar to macros in Common Lisp and Clojure. Macros in Finkel are
implemented as a function taking codes and returning a code.


Understanding Compilation Phases
--------------------------------

During compilation, the compiler executable parses the contents of the
source code. If the parsed code was a list, and the first element of
the list was a symbol of known macro name, the rest of the elements in
the list will be passed to the macro. Resulting forms will be replaced
with the original list form of the macro. This replacement of the code
with macro function is often called *macro expansion*. The expanded
result will again get expanded until it cannot be expanded anymore.
During macro expansion, the compiler can use predefined functions in
the executable. To add functions to use during macro expansion, one
needs to explicitly tell so.


Defining Macro With ``eval-when`` And ``defmacro``
--------------------------------------------------

One way to tell a new macro to the compiler is to define a macro
inside ``eval-when (compile)``. The ``eval-when`` is a macro that
specifies the phase of declaration in its body form. The phase
``compile`` will evaluate the contents while compiling the parsed
source code.

Open a new file and save following contents to a file named
``eval-when.fnk``:

.. literalinclude:: ../code/macro/eval-when.fnk
   :language: finkel

In the above example, ``(require (Finkel.Prelude))`` is added in the
``defmodule`` to introduce functions and data types for writing
macros.

The ``eval-when`` macro can take multiple forms. Two forms are passed
to ``eval-when`` in the above example, one to define a macro named
``say-hello`` , and another to define a macro named ``say-bye``.

The ``say-hello`` macro takes no argument, and the body of the macro
simply returns a quoted form with a single quote (i.e. ``'``).
Similarly, the ``say-bye`` macro takes no argument and returns a form
to prints out a message.

The ``main`` function contains the ``say-hello`` and ``say-bye``
macros. Unlike functions, macros taking no arguments need to be
surrounded by parentheses.

One can run the compiler with the ``-ddump-parsed`` option to observe
the parsed Haskell representation:

.. code-block:: console

   $ finkel make -fno-code -ddump-parsed eval-when.fnk

   ==================== Parser ====================
   module Main where
   main :: IO ()
   main
     = do putStrLn ";;; eval-when ;;;"
          putStrLn "Hello macro!"
          putStrLn "Goodbye."


   [1 of 1] Compiling Main             ( eval-when.fnk, nothing )


Defining Macro With ``macrolet``
--------------------------------

One can add a temporary macro with the ``macrolet`` macro. Following
``macrolet.fnk`` example do similar work done in the previous example,
but using ``macrolet`` instead of ``eval-when`` and ``defmacro``.

.. literalinclude:: ../code/macro/macrolet.fnk
   :language: finkel

Note that single ``macrolet`` form can define multiple temporary
macros.

.. code-block:: console

   $ finkel make -fno-code -ddump-parsed macrolet.fnk

   ==================== Parser ====================
   module Main where
   main :: IO ()
   main
     = do putStrLn ";;; macrolet ;;;"
          putStrLn "Hello macro!"
          putStrLn "Goodbye."


   [1 of 1] Compiling Main             ( macrolet.fnk, nothing )


Loading Macros With ``require``
-------------------------------

Another way to add macros to the current module is to ``require`` a
module containing macros. Open a file named ``RequireMe.fnk`` and save
the following code:

.. literalinclude:: ../code/macro/RequireMe.fnk
   :language: finkel

Note that the ``RequireMe`` module has the ``import`` of
``Finkel.Prelude`` inside ``defmodule``. This is because the macros
defined in ``RequireMe`` are not for itself, but other modules.

Next, open and edit another file named ``require.fnk`` to require the
``RequireMe`` module:

.. literalinclude:: ../code/macro/require.fnk
   :language: finkel

Compilation output:

.. code-block:: console

   $ finkel make -dynamic-too RequireMe.fnk require.fnk

   [1 of 2] Compiling RequireMe        ( RequireMe.fnk, RequireMe.o )

   ==================== Parser ====================
   module Main where
   main :: IO ()
   main
     = do putStrLn ";;; require ;;;"
          putStrLn "Hello macro!"
          putStrLn "Goodbye."


   [2 of 2] Compiling Main             ( require.fnk, require.o )
   Linking doc/code/macro/require ...

Unlike the previous two examples, one needs to generate an object code
of the ``RequireMe`` module so that the macro functions defined in
``RequireMe`` could be used in the file ``require.fnk``.


.. note::

   As of finkel-kernel version 0.28.0.0, one may need to add
   ``-dynamic-too`` option to the ``finkel`` executable when compiling
   a source code file containing ``require``.


Quasiquote, Unquote, And Unquote-Splice
---------------------------------------

Macro can *unquote* and *unquote-splice* a form inside
*quasiquote*.

Open a new file named ``unquote.fnk`` and save the following contents:

.. literalinclude:: ../code/macro/unquote.fnk
   :language: finkel

The example defines two macros: ``uq1`` and ``uq2``. Both macros use
````` (back-tick) instead of ``'`` in body expression.

In ``uq1``, the macro argument ``arg`` is unquoted with ``,``, and the
unquoted form is passed as the second argument of ``++`` function.  In
``uq2`` the expression ``(++ "uq2: arg = " (show arg))`` is unquoted
with ``,``.

Observing parsed result with ``-ddump-parsed``:

.. code-block:: console

   $ finkel make -fno-code -ddump-parsed unquote.fnk

   ==================== Parser ====================
   module Main where
   main :: IO ()
   main
     = do putStrLn ("uq1: arg = " ++ show "foo")
          putStrLn "uq2: arg = \"bar\""


   [1 of 1] Compiling Main             ( unquote.fnk, nothing )

Parsed Haskell representation shows ``++`` in the expanded form of
``uq1`` macro. Expanded result of ``uq2`` evaluates ``++`` at the time
of macro expansion, so the resulting form of ``uq2`` is a literal
``String``.

Inside the quasi-quoted form, ``,@`` is used to unquote-splice a list
form. The ``,@`` can unquote-splice a quoted list and a Haskell list.

.. literalinclude:: ../code/macro/unquote-splice.fnk
   :language: finkel

Observing parsed Haskell code:

.. code-block:: console

   $ finkel make -fno-code -ddump-parsed unquote-splice.fnk

   ==================== Parser ====================
   module Main where
   main :: IO ()
   main
     = do putStrLn (concat ["foo", "bar", "buzz"])
          putStrLn (concat ["foo", "bar", "buzz"])


   [1 of 1] Compiling Main             ( unquote-splice.fnk, nothing )


Getting Macro Arguments As A List
---------------------------------

Macro can take its entire argument as a list form. Below example codes
show a macro which takes entire arguments passed to it as a list named
``args``:

.. literalinclude:: ../code/macro/arglist.fnk
   :language: finkel

Parsed Haskell code:

.. code-block:: console

   $ finkel make -fno-code -ddump-parsed arglist.fnk

   ==================== Parser ====================
   module Main where
   main :: IO ()
   main = putStrLn (unwords ["foo", "bar", "buzz"])


   [1 of 1] Compiling Main             ( arglist.fnk, nothing )


Getting Values From Macro Arguments
-----------------------------------

One can obtain Haskell values from arguments passed to macro:

.. literalinclude:: ../code/macro/fib-macro.fnk
   :language: finkel

The above example applies the ``fromCode`` function to the macro
argument to get an ``Int`` value from the code object. To return the
code object, the ``fib-macro`` applies ``toCode`` to the ``Int`` value
evaluated by the ``fib`` function. Note that the ``fib`` function
needs to be defined inside ``eval-when`` so that ``fib-macro`` can use
the function during macro expansion.

Sample compilation output:

.. code-block:: console

   $ finkel make -fno-code -ddump-parsed fib-macro.fnk

   ==================== Parser ====================
   module Main where
   main :: IO ()
   main = print 55


   [1 of 1] Compiling Main             ( fib-macro.fnk, nothing )


Special forms
-------------

The Finkel core keywords are implemented as macros made from Finkel
kernel.  Details of Finkel core keywords are described in the `haddock
API documentation <https://hackage.haskell.org>`_ of the
``finkel-lang`` package.

This section explains built-in macros in the Finkel kernel
language. These built-in macros are sometimes called *special
forms*. All special forms start with ``:``, followed by lower case
alphabetic character, to avoid name conflict with existing Haskell
functions.


:begin
^^^^^^

The ``:begin`` special form is basically for writing a macro returning
multiple top-level declarations. Following code shows an example use
of ``:begin``, to return type synonym declarations from the
``nat-types`` macro:

.. literalinclude:: ../code/macro/begin.fnk
   :language: finkel

Observing parsed Haskell code:

.. code-block:: console

   $ finkel make -fno-code -ddump-parsed begin.fnk

   ==================== Parser ====================
   module Main where
   import Data.Proxy
   data Nat = Zero | Succ Nat
   type N0 = 'Zero
   type N1 = ('Succ 'Zero)
   type N2 = ('Succ ('Succ 'Zero))
   type N3 = ('Succ ('Succ ('Succ 'Zero)))
   type N4 = ('Succ ('Succ ('Succ ('Succ 'Zero))))
   type N5 = ('Succ ('Succ ('Succ ('Succ ('Succ 'Zero)))))
   type N6 = ('Succ ('Succ ('Succ ('Succ ('Succ ('Succ 'Zero))))))
   main :: IO ()
   main = print (Proxy :: (Proxy N6))


   [1 of 1] Compiling Main             ( begin.fnk, nothing )


:eval-when-compile
^^^^^^^^^^^^^^^^^^

The ``:eval-when-compile`` special form is used to implement
``eval-when`` macro in the core language. Basically,
``(:eval-when-compile BODY1 BODY2 ...)`` is the same as ``(eval-when
(compile) BODY1 BODY2 ...)``.

The following code shows sample use of ``:eval-when-compile``. The
function ``wrap-actions`` is defined inside ``:eval-when-compile``, so
that later the compiler can use the function in the ``doactions``
macro.

.. literalinclude:: ../code/macro/eval-when-compile.fnk
   :language: finkel

Parsed Haskell code:

.. code-block:: console

   $ finkel make -fno-code -ddump-parsed eval-when-compile.fnk

   ==================== Parser ====================
   module Main where
   foo :: Int -> (IO ())
   foo n
     = do putStrLn "from foo"
          print (n + 1)
   bar :: Int -> Int -> (IO ())
   bar a b
     = do putStrLn "from bar"
          print (a + (b * 2))
   main :: IO ()
   main
     = do foo 41
          bar 10 16


   [1 of 1] Compiling Main             ( eval-when-compile.fnk, nothing )


:quote
^^^^^^

The ``:quote`` special form is used for quoting the given value as a
code object. The ``'`` is syntax sugar of this special
form. Internally, quoted values are passed to functions exported from
the ``finkel-kernel`` package.

Following code shows how underlying Finkel kernel functions are
applied to literal values in source code:

.. literalinclude:: ../code/macro/quote.fnk
   :language: finkel

Parsed Haskell source:

.. code-block:: console

   $ finkel make -fno-code -ddump-parsed quote.fnk

   ==================== Parser ====================
   module Main where
   import Finkel.Prelude
   main :: IO ()
   main
     = do putStrLn ";;; quote ;;;"
          print (qSymbol "foo")
          print (qSymbol "foo")
          print (qInteger 42)
          print (qInteger 42)
          print (qString "string")
          print (qString "string")


   [1 of 1] Compiling Main             ( quote.fnk, nothing )


:quasiquote
^^^^^^^^^^^

The ``:quasiquote`` is the underlying special form for the `````
syntax sugar. Inside a quasi-quoted form, ``:unquote`` and
``:unquote-splice`` could be used for getting the value from the
code. Indeed, ``,`` is a syntax sugar of ``:unquote``, and ``,@`` is a
syntax sugar of ``:unquote-splice``.

.. literalinclude:: ../code/macro/quasiquote.fnk
   :language: finkel

Above example prints ``True``:

.. code-block:: console

   $ finkel make -o a.out quasiquote.fnk
   [1 of 1] Compiling Main             ( quasiquote.fnk, quasiquote.o )
   Linking a.out ...
   $ ./a.out
   True


:require
^^^^^^^^

The ``:require`` is for adding a module to the compiler during macro
expansion. It also adds macros defined in the required module to the
current compiler environment. This special form is used by the
``defmodule`` macro.

.. literalinclude:: ../code/macro/raw-require.fnk
   :language: finkel

Parsed Haskell code:

.. code-block:: console

   $ finkel make -fno-code -ddump-parsed raw-reruire.fnk

   ==================== Parser ====================
   module Main where
   main :: IO ()
   main
     = do putStrLn ";;; raw-require.fnk ;;;"
          putStrLn "Hello macro!"
          putStrLn "Goodbye."


   [1 of 1] Compiling Main             ( raw-require.fnk, nothing )


:with-macro
^^^^^^^^^^^

The ``:with-macro`` is the underlying special form for ``macrolet``
macro. This special form is perhaps not useful unless one wants to
write an alternative implementation of the ``macrolet`` macro. See the
source code of ``Finkel.Core`` module for usage.
