Building And Installing
========================

At the time of writing, Finkel related packages are not yet uploaded
to hackage and stackage, so one need to build from source to install.


Getting the source
------------------

Clone the Finkel repository with ``git``:

.. code-block:: console

   $ git clone https://github.com/finkel-lang/finkel.git


Building with ``stack``
-----------------------

One can use ``stack`` to build the packages. To build and test with
``stack``:

.. code-block:: console

   $ cd finkel
   $ stack build --test

And to install the ``finkel`` executable:

.. code-block:: console

   $ stack build --copy-bins finkel-tool


Building with ``cabal-install``
-------------------------------

.. note::

   As of ``cabal-install`` version 3.0.0.0, installing with ``cabal
   v2-install`` does not work. This is a known issue related to the
   file extension used by Finkel. See `this issue
   <https://github.com/haskell/cabal/issues/6124>`_ for details.  To
   install with ``cabal-install``, use the ``cabal v1-install``
   command or the ``setup`` executable built under the
   ``dist-newstyle`` directory.

Building and testing with ``cabal-install`` is supported with
``ghc-8.8.x`` series:

.. code-block:: console

   $ cd finkel
   $ cabal v2-build all
   $ cabal v2-test all

..
   Using docker
   ------------

   TODO ...?

   Using nix
   ---------

   TODO ...?
