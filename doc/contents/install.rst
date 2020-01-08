Building And Installing
========================

At the time of writing, Finkel related packages are not yet uploaded
to hackage and stackage, so one need to build from source to install.


Building from source
--------------------

Clone the Finkel repository with ``git``:

.. code-block:: console

   $ git clone https://github.com/finkel-lang/finkel.git

One can either use ``stack`` or ``cabal-install`` to build the
packages. To build with ``stack``:

.. code-block:: console

   $ cd finkel
   $ stack build --copy-bins finkel-tool

To build with ``cabal-install``, use ``v2`` style commands:

.. code-block:: console

   $ cd finkel
   $ cabal new-install finkel-tool

..
   Using docker
   ------------

   TODO ...?

   Using nix
   ---------

   TODO ...?
