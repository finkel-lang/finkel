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

.. literalinclude:: ../include/macros/eval-when.fnk
   :language: finkel

In the above example, ``(import-when [:compile] (Finkel.Prelude))`` is
added in the ``defmodule`` to import functions and data types for
writing while compiling the ``Main`` module.

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

.. literalinclude:: ../include/macros/eval-when.console
   :language: console


Defining Macro With ``macrolet``
--------------------------------

One can add a temporary macro with the ``macrolet`` macro. Following
``macrolet.fnk`` example do similar work done in the previous example,
but using ``macrolet`` instead of ``eval-when`` and ``defmacro``.

.. literalinclude:: ../include/macros/macrolet.fnk
   :language: finkel

Note that single ``macrolet`` form can define multiple temporary
macros.

.. literalinclude:: ../include/macros/macrolet.console
   :language: console


Loading Macros With ``require``
-------------------------------

Another way to add macros to the current module is to ``require`` a
module containing macros. Open a file named ``RequireMe.fnk`` and save
the following code:

.. literalinclude:: ../include/macros/RequireMe.fnk
   :language: finkel

Note that the ``RequireMe`` module has the ``import`` of
``Finkel.Prelude`` inside ``defmodule``. This is because the macros
defined in ``RequireMe`` are not for itself, but other modules.

Next, open and edit another file named ``require.fnk`` to require the
``RequireMe`` module:

.. literalinclude:: ../include/macros/require.fnk
   :language: finkel

Compilation output:

.. literalinclude:: ../include/macros/require.console
   :language: console

Unlike the previous two examples, one needs to generate an object code
of the ``RequireMe`` module so that the macro functions defined in
``RequireMe`` could be used in the file ``require.fnk``.

.. note::

   As of finkel version 0.1, one may need to add ``-dynamic-too`` option to the
   ``finkel`` executable when compiling a source code file containing
   ``require``.


Quasiquote, Unquote, And Unquote-Splice
---------------------------------------

Macro can *unquote* and *unquote-splice* a form inside
*quasiquote*.

Open a new file named ``unquote.fnk`` and save the following contents:

.. literalinclude:: ../include/macros/unquote.fnk
   :language: finkel

The example defines two macros: ``uq1`` and ``uq2``. Both macros use
````` (back-tick) instead of ``'`` (single quote) in body expression.

In ``uq1``, the macro argument ``arg`` is unquoted with ``,``, and the
unquoted form is passed as the second argument of ``++`` function.  In
``uq2`` the expression ``(++ "uq2: arg = " (show arg))`` is unquoted
with ``,``.

Observing parsed result with ``-ddump-parsed``:

.. literalinclude:: ../include/macros/unquote.console
   :language: console

Parsed Haskell representation shows ``++`` in the expanded form of
``uq1`` macro. Expanded result of ``uq2`` evaluates ``++`` at the time
of macro expansion, so the resulting form of ``uq2`` is a literal
``String``.

Inside the quasi-quoted form, ``,@`` is used to unquote-splice a list
form. The ``,@`` can unquote-splice a quoted list and a Haskell list.

.. literalinclude:: ../include/macros/unquote-splice.fnk
   :language: finkel

Observing parsed Haskell code:

.. literalinclude:: ../include/macros/unquote-splice.console
   :language: console


Getting Macro Arguments As A List
---------------------------------

Macro can take its entire argument as a list form. Below example codes
show a macro which takes entire arguments passed to it as a list named
``args``:

.. literalinclude:: ../include/macros/arglist.fnk
   :language: finkel

Parsed Haskell code:

.. literalinclude:: ../include/macros/arglist.console
   :language: console


Getting Values From Macro Arguments
-----------------------------------

One can obtain Haskell values from arguments passed to macro:

.. literalinclude:: ../include/macros/fib-macro.fnk
   :language: finkel

The above example applies the ``fromCode`` function to the macro
argument to get an ``Int`` value from the code object. To return the
code object, the ``fib-macro`` applies ``toCode`` to the ``Int`` value
evaluated by the ``fib`` function. Note that the ``fib`` function
needs to be defined inside ``eval-when`` so that ``fib-macro`` can use
the function during macro expansion.

Sample compilation output:

.. literalinclude:: ../include/macros/fib-macro.console
   :language: console


Special forms
-------------

The Finkel core keywords are implemented as macros made from Finkel
kernel.  Details of Finkel core keywords are described in the `haddock
API documentation <https://hackage.haskell.org>`_ of the
``finkel-core`` package.

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

.. literalinclude:: ../include/macros/begin.fnk
   :language: finkel

Observing parsed Haskell code:

.. literalinclude:: ../include/macros/begin.console
   :language: console


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

.. literalinclude:: ../include/macros/eval-when-compile.fnk
   :language: finkel

Parsed Haskell code:

.. literalinclude:: ../include/macros/eval-when-compile.console
   :language: console


:quote
^^^^^^

The ``:quote`` special form is used for quoting the given value as a
code object. The ``'`` is syntax sugar of this special
form. Internally, quoted values are passed to functions exported from
the ``finkel-kernel`` package.

Following code shows how underlying Finkel kernel functions are
applied to literal values in source code:

.. literalinclude:: ../include/macros/quote.fnk
   :language: finkel

Parsed Haskell source:

.. literalinclude:: ../include/macros/quote.console
   :language: console


:quasiquote
^^^^^^^^^^^

The ``:quasiquote`` is the underlying special form for the `````
syntax sugar. Inside a quasi-quoted form, ``:unquote`` and
``:unquote-splice`` could be used for getting the value from the
code. Indeed, ``,`` is a syntax sugar of ``:unquote``, and ``,@`` is a
syntax sugar of ``:unquote-splice``.

.. literalinclude:: ../include/macros/quasiquote.fnk
   :language: finkel

Above example prints ``True``:

.. literalinclude:: ../include/macros/quasiquote.console
   :language: console


:require
^^^^^^^^

The ``:require`` is for adding a module to the compiler during macro
expansion. It also adds macros defined in the required module to the
current compiler environment. This special form is used by the
``defmodule`` macro.

.. literalinclude:: ../include/macros/raw-require.fnk
   :language: finkel

Parsed Haskell code:

.. literalinclude:: ../include/macros/raw-require.console
   :language: console


:with-macro
^^^^^^^^^^^

The ``:with-macro`` is the underlying special form for ``macrolet``
macro. This special form is perhaps not useful unless one wants to
write an alternative implementation of the ``macrolet`` macro. See the
source code of ``Finkel.Core`` module for usage.
