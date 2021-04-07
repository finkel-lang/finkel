The Finkel Documentation
========================

Introduction
------------

Finkel is a statically typed, purely functional, and
non-strict-by-default dialect of the `Lisp
<https://en.wikipedia.org/wiki/Lisp_(programming_language)>`_
programming language. Or in other words, `Haskell
<https://haskell.org>`_ **in S-expression**. [#f1]_

Finkel has the following features:

* Integration with existing Haskell modules.

* Building Haskell-compatible `Cabal
  <https://www.haskell.org/cabal/>`_ packages.

* Documentation generation with `Haddock
  <https://www.haskell.org/haddock/>`_.

* Lisp style macro system.

* Tool executable, including interactive REPL.

..
   And following anti-features:

   * CPP language extension

The capital lettered term Finkel is used to refer to the programming
language itself, and the quoted ``finkel`` is used to refer an
executable program to work with the language. This documentation
briefly introduces the ``finkel`` executable and the language, just
enough to get started.  Readers of this documentation are assumed to
have some basic knowledge of the Unix-like environment and have some
programming experiences with Haskell.


.. toctree::
   :maxdepth: 2
   :caption: Contents:

   contents/install.rst
   contents/finkel-executable.rst
   contents/building-package.rst
   contents/macros.rst
   contents/language-syntax.rst

..  ==================
    Indices and tables
    ==================
    * :ref:`genindex`
    * :ref:`search`


.. rubric:: Footnotes

.. [#f1] More precisely, `GHC <https://www.haskell.org/ghc/>`_ in
   S-expression.
