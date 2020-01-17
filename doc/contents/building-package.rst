Building Package
================

To build a library package with Finkel, make a cabal configuration
file as in the Haskell cabal package, but with custom setup script and
some build tool and package dependencies.

.. note::

   This documentation assumes the readers are using the `stack
   <https://docs.haskellstack.org/en/stable/README/>`_ build tool for
   building cabal packages. Those who prefer other tools such as
   `cabal-install <http://hackage.haskell.org/package/cabal-install>`_
   may translate the invoked commands and modify the file contents as
   necessary.

Building My First Package
-------------------------

Make a directory named ``my-first-package``, and create a file named
``package.yaml`` under the directory with following
contents:

.. literalinclude:: ../code/my-first-package/package.yaml
   :language: yaml

And a custom ``Setup.hs`` script:

.. literalinclude:: ../code/my-first-package/Setup.hs
   :language: haskell

And a Finkel source code ``src/MyFirstPackage.fnk`` for exposed
module:

.. literalinclude:: ../code/my-first-package/src/MyFirstPackage.fnk
   :language: finkel

At this point the files under the ``my-first-project`` directory
should look like below:

::

   my-first-package
   ├── package.yaml
   ├── Setup.hs
   └── src
      └── MyFirstPackage.fnk


Now one can build the ``my-first-package`` package with ``stack``:

.. code-block:: console

   $ stack build my-first-package
   ... Output messages omitted ...
   [1 of 2] Compiling Main
   [2 of 2] Compiling StacksetupShim
   ... More output messages ...
   [1 of 1] Compiling MyFirstPackage

.. note:: While building packages with ``stack``, one may see a warning
   message saying "Unable to find a known candidate for the Cabal
   entry". Although the message says it cannot find the candidate, the
   compilation process does work. This is a known issue, and hoping to
   fix in not so far future.


Mixing Finkel And Haskell Source Codes
--------------------------------------

One can mix Finkel source codes and Haskell source codes in a package.
This time, making a package ``my-second-package`` from Finkel specific
template:

.. code-block:: console

   $ stack new my-second-package https://raw.githubusercontent.com/finkel-lang/finkel/master/tool/finkel.hsfiles

The above command will make a directory named ``my-second-package``
with a cabal configuration file, ``Setup.hs`` script, and a stub
Finkel source code file. Directory contents of ``my-second-package``
should look like below:

::

   my-second-package
   ├── app
   │  └── Main.hs
   ├── LICENSE
   ├── my-second-package.cabal
   ├── README.md
   ├── Setup.hs
   ├── src
   │  └── Lib.fnk
   └── test
      └── Spec.hs


.. note::

   As of cabal version 3.0.0, the file extension of an executable in a
   cabal package needs to end with ``.hs`` or ``.c`` file
   extension. From this restriction, one needs to make a wrapper file
   to run an executable written in Finkel. This is why the executable
   and test stanzas in cabal configuration file generated from
   template contains dummy ``Main.hs`` and ``Spec.hs`` files instead
   of ``*.fnk`` files.

Add a new file named ``my-second-package/src/FnkCodes.fnk``, with
Finkel source codes:

.. literalinclude:: ../code/my-second-package/src/FnkCodes.fnk
   :language: finkel

And another new file named ``my-second-package/src/HsCodes.hs``, with
Haskell source codes:

.. literalinclude:: ../code/my-second-package/src/HsCodes.hs
   :language: haskell

Modify the ``library`` stanza of the file ``my-second-package.cabal``
and add ``HsCodes`` and ``FnkCodes`` modules as shown below:

.. literalinclude:: ../code/my-second-package/my-second-package.cabal
   :lines: 22-29

The functions exported from ``HsCodes`` module could be used from
``Lib`` module, as in compilation of cabal package without Finkel
codes. Modify the file ``my-second-package/src/Lib.fnk`` to import
``hsfactorial`` and ``fnkfactorial`` functions from ``HsCodes``:

.. literalinclude:: ../code/my-second-package/src/Lib.fnk
  :language: finkel

One can build the ``my-second-package`` with ``stack build`` command, as
before:

.. code-block:: console

   $ stack build my-second-package

.. note::

   It is also possible to use a library package containing Finkel code
   from other Haskell packages as a build dependency, since the
   resulting object codes are compiled by compatible ``ghc`` version.


Test, Coverage, And Haddock
---------------------------

To run tests, invoke ``stack test`` or ``stack build --test``:

.. code-block:: console

   $ stack build --test my-second-package

To generate code coverage report, add ``--coverage`` option when running
test:

.. code-block:: console

   $ stack build --test --coverage my-second-package

And, to generate haddock documentation of the package, add ``--haddock``
option to ``stack build`` command:

.. code-block:: console

   $ stack build --haddock my-second-package
