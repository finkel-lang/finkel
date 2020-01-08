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
<https://hackage.haskell.org>`_ of the ``finkel-core`` package.


Literals
--------

Comments
^^^^^^^^

Line contents after ``;`` are treated as comments.

.. code-block:: finkel

   (putStrLn "foo") ; single-line comment in Finkel

.. code-block:: haskell

    putStrLn "foo" -- single-line comment in Haskell

Block style comment is supported with ``#;`` and ``;#``.

.. code-block:: finkel

   (putStrLn #;Finkel block comment;# "bar")

.. code-block:: haskell

    putStrLn {- Haskell block comment -} "bar"

Variable identifier
^^^^^^^^^^^^^^^^^^^

Finkel accepts valid variable identifiers defined in Haskell 2010, and
variable identifiers containing hyphens which starting with a
non-operator character. Hyphens in variable identifiers are internally
converted to underscores. For instance, ``foo-bar-buzz`` will be
converted to ``foo_bar_buzz``:

.. code-block:: finkel

   (foo-bar-buzz quux) ; Finkel

.. code-block:: haskell

   foo_bar_buzz quux -- Haskell

The hyphen conversion will be triggered only when the first letter of a
variable identifier was a non-operator character. Operators like
``-:-``, ``*+-``, ``$-$``, etc are kept as-is.


Numeric
^^^^^^^

As described in the `Numeric Literals
<https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-190002.5>`_
section of the Haskell 2010 report, decimal, octal, hexadecimal
integers and float with exponent are supported.

.. code-block:: finkel

   1    ; decimal integer
   0o77 ; octal integer
   0xff ; hexadecimal integer
   2.34 ; float
   1e-2 ; float with exponent

.. code-block:: haskell

   1    -- decimal integer
   0o77 -- octal integer
   0xff -- hexadecimal integer
   2.34 -- float
   1e-2 -- float with exponent


Character And String
^^^^^^^^^^^^^^^^^^^^

A character literal in Finkel starts with ``#'`` instead of
surrounding with single quotes. Other than that, Finkel mostly follows
the `Characters and String Literals
<https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6>`_
section in the Haskell 2010 report.

Following code prints single lower case character ``a``:

.. code-block:: finkel

   (putChar #'a) ; Finkel

.. code-block:: haskell

   putChar 'a'  -- Haskell

Finkel uses a backslash to escape special characters. Following code
prints backslash and single quote:

.. code-block:: finkel

   (print [#'\ #'']) ; Finkel

.. code-block:: haskell

   print ['\\', '\''] -- Haskell

Some characters like newline, space, NUL, etc. are expressed with
escape character and specific character sequences.

.. code-block:: finkel

   (print [#'\n #'  #'\NUL #'\^L]) ; Finkel

.. code-block:: haskell

   print ['\n', ' ', '\NUL', '\^L'] -- Haskell

Characters could be expressed with their numeric code in decimal, octal,
and hexadecimal:

.. code-block:: finkel

   (print [#'\97 #'\o141 #'\x61]) ; Finkel, prints "aaa"

.. code-block:: haskell

   print ['\97', '\o141', '\x61'] -- Haskell, prints "aaa"

String literals are written between double-quotes. Special characters are
escaped with ``\``.

.. code-block:: finkel

   "foo\nbar\nbuzz. numeric escape \97 and control char \^X" ; Finkel

.. code-block:: haskell

   "foo\nbar\nbuzz. numeric escape \97 and control char \^X" -- Haskell

At the time of writing, Finkel does not support the **gap** feature,
which is available in Haskell 2010 to ignore the string contents
between two backslashes.


Expressions
-----------

Function Applications
^^^^^^^^^^^^^^^^^^^^^

Function application in Finkel is done with parentheses:

.. code-block:: finkel

   (putStrLn "hello") ; Finkel

.. code-block:: haskell

   putStrLn "hello" -- Haskell

Unlike some other lisps, extra parentheses are ignored:

.. code-block:: finkel

   (((((putStrLn)) "hello"))) ; Finkel

.. code-block:: haskell

   ((((putStrLn)) "hello")) -- Haskell


Operator Applications
^^^^^^^^^^^^^^^^^^^^^

Finkel does not have native support for infix operator
applications. However, a form applying operator function will be
expanded to a form taking all of its arguments, with two operands for
each. For example, adding numbers from 1 to 5 could be written as:

.. code-block:: finkel

   (+ 1 2 3 4 5) ; Finkel

.. code-block:: haskell

   1 + 2 + 3 + 4 + 5 -- Haskell

Operator expansion understands right and left associativity. Operator
precedence in Finkel is explicitly specified with parentheses.

.. code-block:: finkel

  (<*> (pure foldr) (Just +) (pure 1) (pure [2 3])) ; Finkel

.. code-block:: haskell

  pure foldr <*> Just (+) <*> pure 1 <*> pure [2, 3] -- Haskell


The compiler treats the above expression as:

.. code-block:: haskell

  ((pure foldr <*> Just (+)) <*> pure 0) <*> pure [] -- Haskell

because the ``<*>`` operator is left-associative.

When a single argument has been passed to operator function, the resulting
expression is partial application:

.. code-block:: finkel

   (map (* 2) [1 2 3]) ; Finkel

.. code-block:: haskell

   map ((*) 2) [1, 2, 3] -- Haskell

To apply more than two arguments to an operator function, one needs to
explicitly surround the operator with parenthesis. Suppose that there
is an operator function ``*+`` which takes three arguments:

.. code-block:: finkel

   ((*+) 2 3 4) ; Finkel

.. code-block:: haskell

   (*+) 2 3 4 -- Haskell


Unary Operator Application
^^^^^^^^^^^^^^^^^^^^^^^^^^

The operator ``-`` is always treated as a binary operator in
Finkel. In below Finkel example, ``(- 1)`` is a partially applied
function:

.. code-block:: finkel

   (map (- 1) [1 2 3]) ; Finkel

.. code-block:: haskell

   map ((-) 1) [1, 2, 3] -- Haskell


Lambda
^^^^^^

Lambda expression starts with ``\``. At least one space after ``\`` is
mandatory. The last form in the lambda expression the body expression
of entire lambda abstraction, others forms are argument patterns:

.. code-block:: finkel

   (zipWith (\ x y (* x (+ y 1))) [1 2 3] [4 5 6]) ; Finkel

.. code-block:: haskell

   zipWith (\x y -> x * (y + 1)) [1, 2, 3] [4, 5, 6] -- Haskell


Conditionals
^^^^^^^^^^^^

An ``if`` expression does not take ``then`` and ``else``:

.. code-block:: finkel

  (if test true-expr false-expr) ; Finkel

.. code-block:: haskell

  if test then true_expr else false_expr -- Haskell

A guard starts with ``|``, and supports pattern, local declaration,
and boolean:

.. code-block:: finkel

   (case expr ; Finkel
     (Just y) (| ((even y) r1)
                 ((odd y) (< y 10) r2)
                 ((<- (Just z) (lookup y kvs))
                  (let ((= z' (* z 2))))
                  (r3 z'))
                 (otherwise r4)))

.. code-block:: haskell

   case expr of -- Haskell
     Just y | even y -> r1
            | odd y, y < 10 -> r2
            | Just z <- lookup y kvs
            , let z' = z * 2
            -> r3 z'
            | otherwise -> r4

See also `cond <https://hackage.haskell.org>`_ in ``finkel-core``.


Tuples
^^^^^^

Tuple constructor expression uses single comma. At least one space
after the comma is required:

.. code-block:: finkel

   (print (, True #'x)) ; Finkel

.. code-block:: haskell

   print (True, 'x') -- Haskell

Single comma works for tuples with more than two elements:

.. code-block:: finkel

   (print (, True #'x 42 1.23 "foo")) ; Finkel

.. code-block:: haskell

   print (True, 'x', 42, 1.23, "foo") -- Haskell

To express tuple data and type constructor, use consecutive commas
without spaces:

.. code-block:: finkel

   (<*> (pure (,,,)) (Just 1) (Just 2) (Just 3) (Just 4)) ; Finkel

.. code-block:: haskell

   pure (,,,) <*> Just 1 <*> Just 2 <*> Just 3 <*> Just 4 -- Haskell


Unit
^^^^

Unit is expressed with empty parentheses:

.. code-block:: finkel

   (return ()) ; Finkel

.. code-block:: haskell

   return () -- Haskell

See also `nil <https://hackage.haskell.org>`_ to express an empty form.


Lists
^^^^^

List expression does not take commas:

.. code-block:: finkel

   (print [1 2 3]) ; Finkel

.. code-block:: haskell

   print [1, 2, 3] -- Haskell

Arithmetic sequences use ``..``. Space on each side of ``..`` are
required:

.. code-block:: finkel

   (print [1 3 .. 9]) ; Finkel

.. code-block:: haskell

   print [1, 3 .. 9] -- Haskell

List comprehensions use ``|`` to separate the resulting expression.
Space between ``|`` and the result is required.

.. code-block:: finkel

   [x | (<- x [1 .. 10]) (even x)] ; Finkel

.. code-block:: haskell

   [x | x <- [1 .. 10], even x] -- Haskell


Let
^^^

A let expression is expressed with ``let`` without ``in``:

.. code-block:: finkel

   (let ((:: a Int)) ; Finkel
         (:: (b c) Int))
         (= a 10)
         (= b 4)
         (= c 2))
     (+ (* a b) 2))

.. code-block:: haskell

   let a :: Int -- Haskell
       b, c :: Int
       a = 10
       b = 4
       c = 2
   in  a * b + 2

Case
^^^^

A case expression is expressed with ``case`` without ``of`` and ``->``:

.. code-block:: finkel

   (case n ; Finkel
     0 "zero"
     1 "one"
     _ "many")

.. code-block:: haskell

   case n of -- Haskell
     0 -> "zero"
     1 -> "one"
     _ -> "many"

Do
^^^

Do expression is expressed with ``do``, and bindings inside
do-expressions are expressed with ``<-``:

.. code-block:: finkel

   (do (putStr "x: ") ; Finkel
       (<- l getLine)
       (return (words l)))

.. code-block:: haskell

   do putStr "x: " -- Haskell
      l <- getLine
      return (words l)


Datatypes with field labels
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Field labels are enclosed with ``{`` and ``}``. Does not use ``=``:

.. code-block:: finkel

   (print (C {f1 1 f2 True f3 "abc"})) ; Finkel

.. code-block:: haskell

   print (C {f1=1, f2=True, f3="abc"}) -- Haskell


Expression Type-Signatures
^^^^^^^^^^^^^^^^^^^^^^^^^^

Type signature uses ``::``:

.. code-block:: finkel

    (:: 42 Int) ; Finkel

.. code-block:: haskell

   42 :: Int -- Haskell


Pattern Matching
^^^^^^^^^^^^^^^^

A non-variable pattern requires parentheses, as in ``Just`` shown
below:

.. code-block:: finkel

   (case expr ; Finkel
     (Just x) (+ x 1)
     Nothing  0)

.. code-block:: haskell

   case expr of -- Haskell
     Just x -> x + 1
     Nothing -> 0


As pattern
""""""""""

As pattern uses ``@``:

.. code-block:: finkel

  (let ((= (@ x (Just n)) expr)) ; Finkel
    (+ n 1))

.. code-block:: haskell

  let x@(Just n) = expr -- Haskell
  in  n + 1


Irrefutable pattern
"""""""""""""""""""

Irrefutable patterns are expressed with ``~``:

.. code-block:: finkel

   (let ((= ~(, a ~(, b c)) expr)) ; Finkel
     (+ a (* b c)))

.. code-block:: haskell

   let ~(a, ~(b, c)) = expr -- Haskell
   in  a + b * c


Operator expansion
""""""""""""""""""

The Operator expansion rule applies to patterns. For instance, the
``:`` constructor for a list is expanded with its pattern arguments:

.. code-block:: finkel

   (case expr ; Finkel
     (: a1 a2 _) (+ a1 a2)
     _ 0)

.. code-block:: haskell

   case expr of -- Haskell
     a1 : a2 : _ -> a1 + a2
     _ -> 0


Declarations And Bindings
-------------------------

Algebraic Datatype
^^^^^^^^^^^^^^^^^^

Algebraic datatype declaration uses ``data``. It does not use ``=``
and ``|``. Optional ``deriving`` form may appear at the last element
of the ``data`` form:

.. code-block:: finkel

   (data (D1 a b) ; Finkel
     C1
     (C2 a)
     (C3 b)
     (deriving (Eq Show)))

.. code-block:: haskell

   data D1 a b
     = C1
     | C2 a
     | C3 b
     deriving (Eq, Show)

Constructor with labeled fields are supported with ``{`` and ``}``:

.. code-block:: finkel

   (data (D2 a b) ; Finkel
    (D2 {f1 a
         f2 b
         f3 Int}))

.. code-block:: haskell

   data D2 a b -- Haskell
    = D2 { f1 :: a
         , f2 :: b
         , f3 :: Int }


Type Synonym
^^^^^^^^^^^^

Type synonym declaration uses ``type``. It does not use ``=``:

.. code-block:: finkel

   (type (T1 a) (Maybe (, a Bool String))) ; Finkel

.. code-block:: haskell

   type T1 a = Maybe (a, Bool, String) -- Haskell


Datatype Renamings
^^^^^^^^^^^^^^^^^^

Newtype declaration uses ``newtype``:

.. code-block:: finkel

   (newtype N (N {unN Int})) ; Finkel

.. code-block:: haskell

   newtype N = N { unN :: Int } -- Haskell


Class
^^^^^

Type class declaration uses ``class``:

.. code-block:: finkel

   (class (=> (Ord a) (C1 a)) ; Finkel
     (:: m1 (-> a Int))
     (= m1 _ 0)

.. code-block:: haskell

   class Ord a => C1 a where -- Haskell
     m1 :: a -> Int
     m1 _ = 0

Class instance declaration uses ``instance``:

.. code-block:: finkel

   (instance (C1 Int) ; Finkel
     (= m1 n (+ n 1)))

.. code-block:: haskell

    instance C1 Int where -- Haskell
      m1 n = n + 1


Defaults for Overloaded Numeric Operations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Default declaration is done with ``default``:

.. code-block:: finkel

   (default Int Double) ; Finkel

.. code-block:: haskell

   default (Int, Double) -- Haskell


Type Signatures
^^^^^^^^^^^^^^^

Type signature uses ``::``:

.. code-block:: finkel

   (:: f (-> Int Int Int)) ; Finkel

.. code-block:: haskell

   f :: Int -> Int -> Int -- Haskell

Single type signature could be used for multiple variables:

.. code-block:: finkel

   (:: (f g h) (-> Int Int)) ; Finkel


.. code-block:: haskell

   f, g, h :: Int -> Int -- Haskell

Constraints in type signature are expressed with ``=>``. The last
element of the form ``=>`` should be a type:

.. code-block:: finkel

   (:: f (=> (Eq a) (Ord a) (-> a a))) ; Finkel

.. code-block:: haskell

   f :: Eq a, Ord a => a -> a -- Haskell


Fixity
^^^^^^

It is possible to declare fixity and precedence with ``infix``,
``infixl``, and ``infixr``:

.. code-block:: finkel

   (= $+$ a b (+ a (f b))) ; Finkel
   (infixr 6 $+$)

.. code-block:: haskell

   ($+$) a b = a + f b -- Haskell
   infixr 6 $+$


Note that Finkel syntax is affected by the left and right
associativity of operators, but not by the precedence of operators.


Bindings
^^^^^^^^

Function binding declaration uses ``=``. The form after ``=`` is the
function name, the last form is the expression body. Rest of the forms
are argument patterns:

.. code-block:: finkel

   (= f1 x y z (+ x (* y z))) ; Finkel

.. code-block:: haskell

   f1 x y z = x + (y * z) -- Haskell

Keyword ``where`` can appear in the right-hand side:

.. code-block:: finkel

   (= f2 n ; Finkel
     (where body
       (= body (+ n 1))))

.. code-block:: haskell

   f2 n = body -- Haskell
     where
       body = n + 1

Pattern bindings are similarly done with ``=``:

.. code-block:: finkel

    (= (Just x) (lookup k vs)) ; Finkel

.. code-block:: haskell

    Just x = lookup k vs -- Haskell


Modules
-------

Top-level module definition does not use ``where``:

.. code-block:: finkel

   (module M1) ; Finkel
   (= x 1)
   (= y 2)

.. code-block:: haskell

   module M1 where -- Haskell
   x = 1
   y = 2

See also `defmodule <https://hackage.haskell.org>`_ in
``finkel-core``.


Export Lists
^^^^^^^^^^^^

Module definition can contain an explicit export list. Entities in the
export list can contain bindings, type and data constructors, type
classes, and modules:

.. code-block:: finkel

  (module M2 ; Finkel
    f1           ; Value, field name, or class method
    (T1)         ; Type constructor only
    (T2 ..)      ; Type constructor and all of its data constructors
    (T3 T3a T3b) ; Type constructor and specified data constructors
    (T4 t4f1)    ; Type constructor and field label

    (module Data.Char)      ; Module reexport
    (Mb.Maybe Just Nothing) ; Reexport with a qualified name
    )

.. code-block:: haskell

   module M2 -- Haskell
     ( f1           -- Value, field name, or class method
     , T1           -- Type constructor only
     , T2(..)       -- Type constructor and all of its data constructors
     , T3(T3a, T3b) -- Type constructor and specified data constructors
     , T4(t4f1)     -- Type constructor and field label

     , module Data.Char        -- Module reexport
     , Mb.Maybe(Just, Nothing) -- Reexport with a qualified name
     ) where


Import Declarations
^^^^^^^^^^^^^^^^^^^

Module import declarations use ``import``:

.. code-block:: finkel

   (import Data.Maybe) ; Finkel

.. code-block:: haskell

   import Data.Maybe -- Haskell

Qualified import declarations use ``qualified`` and optional ``as``:

.. code-block:: finkel

   (import qualified Data.Maybe as Mb) ; Finkel

.. code-block:: haskell

   import qualified Data.Maybe as Mb -- Haskell

Entity lists use list:

.. code-block:: finkel

   (import Data.Maybe (catMaybes fromMaybe)) ; Finkel

.. code-block:: haskell

   import Data.Maybe (catMaybes, fromMaybe) -- Haskell

Hiding specified entities with ``hiding``. Form after ``hiding`` is a
list of entity names to hide:

.. code-block:: finkel

   (import Data.Maybe hiding (fromJust fromMaybe)) ; Finkel

.. code-block:: haskell

   import Data.Maybe hiding (fromJust, fromMaybe) -- Haskell

Altogether:

.. code-block:: finkel

   (import qualified Data.Maybe as Mb hiding (fromJust)) ; Finkel

.. code-block:: haskell

   import qualified Data.Maybe as Mb hiding (fromJust) -- Haskell


Foreign Function Interfaces
---------------------------

Foreign Import
^^^^^^^^^^^^^^

Foreign import declarations start with ``foreign`` ``import``:

.. code-block:: finkel

   (foreign import ccall safe "string.h strlen" ; Finkel
     (:: cstrlen (-> (Ptr CChar) (IO CSize))))

.. code-block:: haskell

   foreign import ccall safe "string.h strlen" -- Haskell
     cstrlen :: Ptr CChar -> IO CSize

Foreign Export
^^^^^^^^^^^^^^

Foreign export declarations start with ``foreign`` ``export``:

.. code-block:: finkel

   (foreign export ccall "addInt"
     (:: + (-> Int Int Int)))

.. code-block:: haskell

   foreign export ccall "addInt"
     (+) :: Int -> Int -> Int


Compiler Pragmas
----------------

All pragmas use ``#p(..)`` form.

Inlining
^^^^^^^^

Pragmas to control inlining of codes use ``INLINE`` and ``NOINLINE``:

.. code-block:: finkel

   #p(INLINE foo) ; Finkel

.. code-block:: haskell

   {-# INLINE foo #-} -- Haskell

GHC specific phase controls are also supported:

.. code-block:: finkel

   #p(INLINE [1] bar) ; Finkel
   #p(NOINLINE [~2] buzz)

.. code-block:: haskell

   {-# INLINE [1] bar #-} -- Haskell
   {-# NOINLINE [~2] buzz #-}

Specialization
^^^^^^^^^^^^^^

Pragmas to control specialization of overloaded function use
``SPECIALIZE``:

.. code-block:: finkel

   #p(SPECIALIZE (:: factorial (-> Int Int))) ; Finkel

.. code-block:: haskell

   {-# SPECIALIZE factorial :: Int -> Int #-} -- Haskell

Language extensions
^^^^^^^^^^^^^^^^^^^

Pragma for language extensions use ``LANGUAGE``:

.. code-block:: finkel

   #p(LANGUAGE GADTs OverloadedStrings) ; Finkel

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
