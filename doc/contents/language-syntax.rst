Language Syntax
===============

The Finkel language is made from Finkel kernel keywords and Finkel
core keywords.

The Finkel kernel keywords are designed to be compatible with `Haskell
2010 <https://www.haskell.org/onlinereport/haskell2010/>`_, with few
exceptions.  The syntax for literal values and function applications
are also defined in the Finkel kernel.  The rest of this section will
go through the Finkel kernel language syntax with small example
codes. Each Finkel code is compared to an equivalent Haskell code.

The Finkel core keywords are implemented as macros.  Details of the
Finkel core keywords are described in the `haddock API documentation
<https://hackage.haskell.org>`_ of the ``finkel-lang`` package.


Literals
--------

Comments
^^^^^^^^

Line contents after ``;`` are treated as comments.

.. literalinclude:: ../include/language-syntax/expr/line-comment.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/line-comment.hs
   :language: haskell

Block style comment is supported with ``#;`` and ``;#``.

.. literalinclude:: ../include/language-syntax/expr/block-comment.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/block-comment.hs
   :language: haskell


Variable identifier
^^^^^^^^^^^^^^^^^^^

Finkel accepts valid variable identifiers defined in Haskell 2010, and
variable identifiers containing hyphens which starting with a
non-operator character. Hyphens in variable identifiers are internally
converted to underscores. For instance, ``foo-bar-buzz`` will be
converted to ``foo_bar_buzz``:

.. literalinclude:: ../include/language-syntax/expr/varid.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/varid.hs
   :language: haskell

The hyphen conversion will be triggered only when the first letter of a
variable identifier was a non-operator character. Operators like
``-:-``, ``*+-``, ``$-$``, etc are kept as-is.


Numeric
^^^^^^^

As described in the `Numeric Literals
<https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-190002.5>`_
section of the Haskell 2010 report, decimal, octal, hexadecimal
integers and float with exponent are supported.

.. literalinclude:: ../include/language-syntax/expr/numeric.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/numeric.hs
   :language: haskell


Character And String
^^^^^^^^^^^^^^^^^^^^

A character literal in Finkel starts with ``#'`` instead of
surrounding with single quotes. Other than that, Finkel mostly follows
the `Characters and String Literals
<https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6>`_
section in the Haskell 2010 report.

Following code prints single lower case character ``a``:

.. literalinclude:: ../include/language-syntax/expr/char-a.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/char-a.hs
   :language: haskell

Finkel uses a backslash to escape special characters. Following code
prints backslash and single quote:

.. literalinclude:: ../include/language-syntax/expr/char-escape.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/char-escape.hs
   :language: haskell

Some characters like newline, space, NUL, etc. are expressed with
escape character and specific character sequences.

.. literalinclude:: ../include/language-syntax/expr/char-special.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/char-special.hs
   :language: haskell

Characters could be expressed with their numeric code in decimal, octal,
and hexadecimal:

.. literalinclude:: ../include/language-syntax/expr/char-ncode.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/char-ncode.hs
   :language: haskell

String literals are written between double-quotes. Special characters are
escaped with ``\``. Finkel also supports the **gap** feature, to ignore the
string contents between two backslashes.

.. literalinclude:: ../include/language-syntax/expr/string.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/string.hs
   :language: haskell


Expressions
-----------

Function Applications
^^^^^^^^^^^^^^^^^^^^^

Function application in Finkel is done with parentheses:

.. literalinclude:: ../include/language-syntax/expr/funapp.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/funapp.hs
   :language: haskell

Unlike some other lisps, extra parentheses are ignored. For instance:

.. literalinclude:: ../include/language-syntax/expr/funapp-pars.fnk
   :language: finkel

is simplified to:

.. literalinclude:: ../include/language-syntax/expr/funapp-pars.hs
   :language: haskell


Operator Applications
^^^^^^^^^^^^^^^^^^^^^

Finkel does not have native support for infix operator
applications. However, a form applying operator function will be
expanded to a form taking all of its arguments, with two operands for
each. For example, adding numbers from 1 to 5 could be written as:

.. literalinclude:: ../include/language-syntax/expr/opexp-add.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/opexp-add.hs
   :language: haskell

Operator expansion understands right and left associativity. Operator
precedence in Finkel is explicitly specified with parentheses.

.. literalinclude:: ../include/language-syntax/expr/opexp-app.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/opexp-app.hs
   :language: haskell

The compiler treats the above expression as:

.. code-block:: haskell

  ((pure foldr <*> Just (+)) <*> pure 0) <*> pure [] -- Haskell

because the ``<*>`` operator is left-associative.

When a single argument has been passed to operator function, the resulting
expression is partial application:

.. literalinclude:: ../include/language-syntax/expr/map-mul2.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/map-mul2.hs
   :language: haskell

To apply more than two arguments to an operator function, one needs to
explicitly surround the operator with parenthesis. Suppose that there
is an operator function ``*+`` which takes three arguments:

.. literalinclude:: ../include/language-syntax/expr/muladd.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/muladd.hs
   :language: haskell


Unary Operator Application
^^^^^^^^^^^^^^^^^^^^^^^^^^

The operator ``-`` is always treated as a binary operator in
Finkel. In below Finkel example, ``(- 1)`` is a partially applied
function:

.. literalinclude:: ../include/language-syntax/expr/map-unary.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/map-unary.hs
   :language: haskell


Lambda
^^^^^^

Lambda expression starts with ``\``. At least one space after ``\`` is
mandatory. The last form in the lambda expression the body expression
of entire lambda abstraction, others forms are argument patterns:

.. literalinclude:: ../include/language-syntax/expr/lambda.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/lambda.hs
   :language: haskell


Conditionals
^^^^^^^^^^^^

An ``if`` expression does not take ``then`` and ``else``:

.. literalinclude:: ../include/language-syntax/expr/if.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/if.hs
   :language: haskell

A guard starts with ``|``, and supports pattern, local declaration,
and boolean:

.. literalinclude:: ../include/language-syntax/expr/guard.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/guard.hs
   :language: haskell

See also `cond <https://hackage.haskell.org>`_ in ``finkel-lang``.


Tuples
^^^^^^

Tuple constructor expression uses single comma. At least one space
after the comma is required:

.. literalinclude:: ../include/language-syntax/expr/tup2.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/tup2.hs
   :language: haskell

Single comma works for tuples with more than two elements:

.. literalinclude:: ../include/language-syntax/expr/tup5.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/tup5.hs
   :language: haskell

To express tuple data and type constructor, use consecutive commas
without spaces:

.. literalinclude:: ../include/language-syntax/expr/tupfn.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/tupfn.hs
   :language: haskell


Unit
^^^^

Unit is expressed with empty parentheses:

.. literalinclude:: ../include/language-syntax/expr/unit.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/unit.hs
   :language: haskell

See also `nil <https://hackage.haskell.org>`_ to express an empty form.


Lists
^^^^^

List expression does not take commas:

.. literalinclude:: ../include/language-syntax/expr/list-const.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/list-const.hs
   :language: haskell

Arithmetic sequences use ``..``. Space on each side of ``..`` are
required:

.. literalinclude:: ../include/language-syntax/expr/list-range.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/list-range.hs
   :language: haskell

List comprehensions use ``|`` to separate the resulting expression.
Space between ``|`` and the result is required.

.. literalinclude:: ../include/language-syntax/expr/list-comp.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/list-comp.hs
   :language: haskell


Let
^^^

A let expression is expressed with ``let`` without ``in``:

.. literalinclude:: ../include/language-syntax/expr/let.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/let.hs
   :language: haskell


Case
^^^^

A case expression is expressed with ``case`` without ``of`` and ``->``:

.. literalinclude:: ../include/language-syntax/expr/case.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/case.hs
   :language: haskell


Do
^^^

Do expression is expressed with ``do``, and bindings inside do-expressions are
expressed with ``<-``:

.. literalinclude:: ../include/language-syntax/expr/do.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/do.hs
   :language: haskell


Datatypes with field labels
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Field labels are enclosed with ``{`` and ``}``. Does not use ``=``:

.. literalinclude:: ../include/language-syntax/expr/fieldlabels.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/fieldlabels.hs
   :language: haskell


Expression Type-Signatures
^^^^^^^^^^^^^^^^^^^^^^^^^^

Type signature uses ``::``:

.. literalinclude:: ../include/language-syntax/expr/sige.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/sige.hs
   :language: haskell


Pattern Matching
^^^^^^^^^^^^^^^^

A non-variable pattern requires parentheses, as in ``Just`` shown
below:

.. literalinclude:: ../include/language-syntax/expr/pat-maybe.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/pat-maybe.hs
   :language: haskell


As pattern
""""""""""

As pattern uses ``@``:

.. literalinclude:: ../include/language-syntax/expr/pat-as.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/pat-as.hs
   :language: haskell


Irrefutable pattern
"""""""""""""""""""

Irrefutable patterns are expressed with ``~``:

.. literalinclude:: ../include/language-syntax/expr/pat-irf.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/pat-irf.hs
   :language: haskell


Operator expansion
""""""""""""""""""

The Operator expansion rule applies to patterns. For instance, the
``:`` constructor for a list is expanded with its pattern arguments:

.. literalinclude:: ../include/language-syntax/expr/pat-opexp.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/expr/pat-opexp.hs
   :language: haskell


Declarations And Bindings
-------------------------

Algebraic Datatype
^^^^^^^^^^^^^^^^^^

Algebraic datatype declaration uses ``data``. It does not use ``=``
and ``|``. Optional ``deriving`` form may appear at the last element
of the ``data`` form:

.. literalinclude:: ../include/language-syntax/decl/data-d1.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/decl/data-d1.hs
   :language: haskell

Constructor with labeled fields are supported with ``{`` and ``}``:

.. literalinclude:: ../include/language-syntax/decl/data-d2.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/decl/data-d2.hs
   :language: haskell


Type Synonym
^^^^^^^^^^^^

Type synonym declaration uses ``type``. It does not use ``=``:

.. literalinclude:: ../include/language-syntax/decl/tysym.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/decl/tysym.hs
   :language: haskell


Datatype Renamings
^^^^^^^^^^^^^^^^^^

Newtype declaration uses ``newtype``:

.. literalinclude:: ../include/language-syntax/decl/newtype.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/decl/newtype.hs
   :language: haskell


Class
^^^^^

Type class declaration uses ``class``:

.. literalinclude:: ../include/language-syntax/decl/class.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/decl/class.hs
   :language: haskell

Class instance declaration uses ``instance``:

.. literalinclude:: ../include/language-syntax/decl/instance.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/decl/instance.hs
   :language: haskell


Defaults for Overloaded Numeric Operations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Default declaration is done with ``default``:

.. literalinclude:: ../include/language-syntax/decl/default.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/decl/default.hs
   :language: haskell


Type Signatures
^^^^^^^^^^^^^^^

Type signature uses ``::``:

.. literalinclude:: ../include/language-syntax/decl/tysig-one.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/decl/tysig-one.hs
   :language: haskell

Single type signature could be used for multiple variables:

.. literalinclude:: ../include/language-syntax/decl/tysig-many.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/decl/tysig-many.hs
   :language: haskell

Constraints in type signature are expressed with ``=>``. The last
element of the form ``=>`` should be a type:

.. literalinclude:: ../include/language-syntax/decl/tysig-constraints.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/decl/tysig-constraints.hs
   :language: haskell


Fixity
^^^^^^

It is possible to declare fixity and precedence with ``infix``, ``infixl``, and
``infixr``. Assuming ``$+$`` as a binary operator:

.. literalinclude:: ../include/language-syntax/decl/fixity.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/decl/fixity.hs
   :language: haskell

Note that Finkel syntax is affected by the left and right
associativity of operators, but not by the precedence of operators.


Bindings
^^^^^^^^

Function binding declaration uses ``=``. The form after ``=`` is the
function name, the last form is the expression body. Rest of the forms
are argument patterns:

.. literalinclude:: ../include/language-syntax/decl/bind-simpl.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/decl/bind-simpl.hs
   :language: haskell

Keyword ``where`` can appear in the right-hand side:

.. literalinclude:: ../include/language-syntax/decl/bind-where.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/decl/bind-where.hs
   :language: haskell

Pattern bindings are similarly done with ``=``:

.. literalinclude:: ../include/language-syntax/decl/bind-pat.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/decl/bind-pat.hs
   :language: haskell


Modules
-------

Top-level module definition does not use ``where``:

.. literalinclude:: ../include/language-syntax/module/simpl.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/module/simpl.hs
   :language: haskell

See also `defmodule <https://hackage.haskell.org>`_ in
``finkel-lang``.


Export Lists
^^^^^^^^^^^^

Module definition can contain an explicit export list. Entities in the
export list can contain bindings, type and data constructors, type
classes, and modules:

.. literalinclude:: ../include/language-syntax/module/export-list.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/module/export-list.hs
   :language: haskell


Import Declarations
^^^^^^^^^^^^^^^^^^^

Module import declarations use ``import``:

.. literalinclude:: ../include/language-syntax/import/simpl.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/import/simpl.hs
   :language: haskell

Qualified import declarations use ``qualified`` and optional ``as``:

.. literalinclude:: ../include/language-syntax/import/qualified-as.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/import/qualified-as.hs
   :language: haskell

Entity lists use list:

.. literalinclude:: ../include/language-syntax/import/entity-list.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/import/entity-list.hs
   :language: haskell

Hiding specified entities with ``hiding``. Form after ``hiding`` is a
list of entity names to hide:

.. literalinclude:: ../include/language-syntax/import/hiding.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/import/hiding.hs
   :language: haskell

Altogether:

.. literalinclude:: ../include/language-syntax/import/altogether.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/import/altogether.hs
   :language: haskell


Foreign Function Interfaces
---------------------------

Foreign Import
^^^^^^^^^^^^^^

Foreign import declarations start with ``foreign`` ``import``:

.. literalinclude:: ../include/language-syntax/ffi/import.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/ffi/import.hs
   :language: haskell


Foreign Export
^^^^^^^^^^^^^^

Foreign export declarations start with ``foreign`` ``export``:

.. literalinclude:: ../include/language-syntax/ffi/export.fnk
   :language: finkel

.. literalinclude:: ../include/language-syntax/ffi/export.hs
   :language: haskell


Compiler Pragmas
----------------

All pragmas use ``%p(..)`` form.


Inlining
^^^^^^^^

Pragmas to control inlining of codes use ``INLINE`` and ``NOINLINE``:

.. code-block:: finkel

   %p(INLINE foo) ; Finkel

.. code-block:: haskell

   {-# INLINE foo #-} -- Haskell

GHC specific phase controls are also supported:

.. code-block:: finkel

   %p(INLINE [1] bar) ; Finkel
   %p(NOINLINE [~2] buzz)

.. code-block:: haskell

   {-# INLINE [1] bar #-} -- Haskell
   {-# NOINLINE [~2] buzz #-}


Specialization
^^^^^^^^^^^^^^

Pragmas to control specialization of overloaded function use
``SPECIALIZE``:

.. code-block:: finkel

   %p(SPECIALIZE (:: factorial (-> Int Int))) ; Finkel

.. code-block:: haskell

   {-# SPECIALIZE factorial :: Int -> Int #-} -- Haskell


Language extensions
^^^^^^^^^^^^^^^^^^^

Pragma for language extensions use ``LANGUAGE``:

.. code-block:: finkel

   %p(LANGUAGE GADTs OverloadedStrings) ; Finkel

.. code-block:: haskell

   {-# LANGUAGE GADTs, OverloadedStrings #-} -- Haskell

..
   Overlaps
   ^^^^^^^^

   This is GHC specific ...

..
   .. rubric:: Footnotes

   .. [#f1] With few exceptions. Perhaps the most notable exception is the
            lack of native infix function support, but has operator
            expansion instead.
