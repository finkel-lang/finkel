Building And Installing
=======================

.. note::

   At the time of writing, Finkel related packages are not yet uploaded to
   `hackage <https://hackage.haskell.org>`_ and `stackage
   <https://stackage.org>`_.


Container Image
---------------

There is a container image with the ``finkel`` executable built from the latest
source code with accompanying Haskell libraries and some development tools. The
main use case of the container image is to play with ``finkel`` without setting
up a working Haskell environment. For instance, one can run ``finkel`` with
``docker`` by followings:

.. code-block:: console

   $ docker pull ghcr.io/finkel-lang/finkel:latest
   $ docker run --rm -it ghcr.io/finkel-lang/finkel:latest
   / # finkel eval '(putStrLn "Hello, Finkel!")'
   Hello, Finkel!

See the `GitHub package page
<https://github.com/orgs/finkel-lang/packages/container/package/finkel>`_ for
more info.


Building From Source
--------------------

Getting The Latest Source
^^^^^^^^^^^^^^^^^^^^^^^^^

Clone the Finkel repository with ``git``:

.. code-block:: console

   $ git clone https://github.com/finkel-lang/finkel.git


Building With ``stack``
^^^^^^^^^^^^^^^^^^^^^^^

One can use ``stack`` to build the packages. To build and test with
``stack``:

.. code-block:: console

   $ cd finkel
   $ stack build --test

And to install the ``finkel`` executable:

.. code-block:: console

   $ stack build --copy-bins finkel


Building With ``cabal-install``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. note::

   As of ``cabal-install`` version 3.4.0.0, installing with ``cabal
   v2-install`` does not work. This is `a known issue
   <https://github.com/haskell/cabal/issues/6124>`_ related to the
   file extension used by Finkel. To install with ``cabal-install``,
   use the ``cabal v1-install`` command or the ``setup`` executable
   built under the ``dist-newstyle`` directory.

To build and test with ``cabal-install``:

.. code-block:: console

   $ cd finkel
   $ cabal v2-build all
   $ cabal v2-test all


Building With ``nix``
^^^^^^^^^^^^^^^^^^^^^

The git repository contains ``default.nix`` file. Building and testing with `nix
<https://nixos.org/>`_ could be done with:

.. code-block:: console

   $ nix-build
