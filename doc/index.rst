Getting Started With Sk
=======================

Introduction
------------

Welcome to the getting started document of the *sk programming
language*. Sk is a statically typed, purely functional,
non-strict-by-default dialect of the `Lisp
<https://en.wikipedia.org/wiki/Lisp_(programming_language)>`_
programming language. Or in other words, **Haskell in
S-expression**. [#f1]_

Sk has the following features:

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

The term sk is used to refer to the programming language itself, and
the quoted ``sk`` is used to refer an executable program to work with
the language. This documentation briefly introduces the ``sk``
executable and the language, just enough to get started.  Readers of
this documentation are assumed to have some basic knowledge of the
Unix-like environment and have some programming experiences with
Haskell.


.. toctree::
   :maxdepth: 2
   :caption: Contents:

   contents/install.rst
   contents/sk-executable.rst
   contents/building-package.rst
   contents/macros.rst
   contents/language-syntax.rst

..
      contents/sk-specific-syntax.rst
      contents/further.rst
      contents/appendix.rst

..  ==================
    Indices and tables
    ==================
    * :ref:`genindex`
    * :ref:`search`


.. rubric:: Footnotes

.. [#f1] More precisely, `GHC <https://www.haskell.org/ghc/>`_ in
         S-expression.
