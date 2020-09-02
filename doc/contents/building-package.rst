Building Cabal Package
======================

To build a cabal package with Finkel, make a cabal configuration file
as in the Haskell cabal package, but with custom setup script and some
build tool and package dependencies.

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

.. literalinclude:: ../include/building-package/my-first-package/package.yaml
   :language: yaml

And a custom ``Setup.hs`` script:

.. literalinclude:: ../include/building-package/my-first-package/Setup.hs
   :language: haskell

And a Finkel source code ``src/MyFirstPackage.fnk`` for exposed
module:

.. literalinclude:: ../include/building-package/my-first-package/src/MyFirstPackage.fnk
   :language: finkel

And a ``stack.yaml``:

.. literalinclude:: ../include/building-package/my-first-package/stack.template.yaml
   :language: yaml

At this point the files under the ``my-first-project`` directory
should look like below:

::

   my-first-package
   ├── package.yaml
   ├── Setup.hs
   ├── src
   │   └── MyFirstPackage.fnk
   └── stack.yaml

Now one can build the ``my-first-package`` package with ``stack``:

.. code-block:: console

   $ stack build my-first-package
   ... Output messages omitted ...
   [1 of 2] Compiling Main
   [2 of 2] Compiling StacksetupShim
   ... More output messages ...
   [1 of 1] Compiling MyFirstPackage

.. note::

   While building packages with ``stack``, one may see a warning
   message saying "Unable to find a known candidate for the Cabal
   entry". Although the message says it cannot find the candidate, the
   compilation process does work. This is a known issue, and hoping to
   fix in not so far future.

.. tip::

   To build a package containing Finkel source codes with the latest
   ``finkel`` built from source, one can specify the packages from
   `finkel git repository <https://github.com/finkel-lang/finkel>`_ as
   extra dependencies.

   For example, the following ``stack.yaml`` is set to build a package
   in the current directory with ``finkel`` from the git repository:

   .. literalinclude:: ../include/building-package/my-first-package/stack.git.yaml
      :language: yaml

   See the `stack documentation
   <https://docs.haskellstack.org/en/stable/yaml_configuration/#extra-deps>`_
   and the `Cabal User Guide
   <https://cabal.readthedocs.io/en/3.4/cabal-project.html#specifying-packages-from-remote-version-control-locations>`_
   for more information about using remote git repository for extra
   dependencies.


Mixing Finkel And Haskell Source Codes
--------------------------------------

One can mix Finkel source codes and Haskell source codes in a package.
This time, making a package ``my-second-package`` from Finkel specific
template:

.. code-block:: console

   $ stack new my-second-package https://raw.githubusercontent.com/finkel-lang/finkel/master/tool/finkel.hsfiles

.. warning::

   At the time of writing, one may encounter messages similar to the
   following when running ``stack new`` with the above template:

   .. code-block:: console

      Selecting the best among 17 snapshots...

      * Partially matches lts-15.7
          finkel-setup not found
              - my-second-pkg requires -any

      * Partially matches nightly-2020-03-04
          finkel-setup not found
              - my-second-pkg requires -any

      ...

      Selected resolver: lts-15.7
      Resolver 'lts-15.7' does not have all the packages to match your requirements.
          finkel-setup not found
              - my-second-pkg requires -any

      This may be resolved by:
          - Using '--omit-packages' to exclude mismatching package(s).
          - Using '--resolver' to specify a matching snapshot/resolver

   This is because the packages for ``finkel`` is not yet uploaded to
   `stackage <https://stackage.org>`_.

   As the message indicates, one can pass ``--omit-packages`` option
   or ``--resolver`` option to ``stack new`` until the ``finkel``
   dependency packages are uploaded to the upstream, and add the git
   repository to ``stack.yaml``.

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
   of ``.fnk`` files.

Add a new file named ``my-second-package/src/FnkCodes.fnk``, with
Finkel source codes:

.. literalinclude:: ../include/building-package/my-second-package/src/FnkCodes.fnk
   :language: finkel

And another new file named ``my-second-package/src/HsCodes.hs``, with
Haskell source codes:

.. literalinclude:: ../include/building-package/my-second-package/src/HsCodes.hs
   :language: haskell

Modify the ``library`` stanza of the file ``my-second-package.cabal``
and add ``HsCodes`` and ``FnkCodes`` modules as shown below:

.. literalinclude:: ../include/building-package/my-second-package/my-second-package.cabal
   :lines: 22-29

The functions exported from ``HsCodes`` module could be used from
``Lib`` module, as in compilation of cabal package without Finkel
codes. Modify the file ``my-second-package/src/Lib.fnk`` to import
``hsfactorial`` and ``fnkfactorial`` functions from ``HsCodes``:

.. literalinclude:: ../include/building-package/my-second-package/src/Lib.fnk
  :language: finkel

One can build the ``my-second-package`` with ``stack build`` command, as
before:

.. code-block:: console

   $ stack build my-second-package

.. note::

   It is also possible to use a library package containing Finkel code
   from other Haskell packages as a build dependency since the
   resulting object codes are compiled by compatible ``ghc`` version.


Executable, Test, Coverage, And Haddock
---------------------------------------

The ``my-second-package`` cabal package contains an executable named
``my-second-package``. The executable simply invokes the
``Lib.someFunc`` function. To compile and run the executable:

.. code-block:: console

   $ stack run my-second-package:my-second-package
   From `Lib.someFunc':
     hsfactorial 10  : 3628800
     fnkfactorial 10 : 3628800

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
