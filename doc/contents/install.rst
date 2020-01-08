Building And Installing
========================

At the time of writing, sk-related packages are not yet uploaded to
hackage and stackage, so one need to build from source to install.


Building from source
--------------------

Clone the sk repository with ``git``:

.. code-block:: console

   $ git clone https://url/of/the/git/repository/of/sk.git

One can either use ``stack`` or ``cabal-install`` to build the
packages. To build with ``stack``:

.. code-block:: console

   $ cd sk
   $ stack build --copy-bins sk-tool

To build with ``cabal-install``, use ``v2`` style commands:

.. code-block:: console

   $ cd sk
   $ cabal new-install sk-tool

..
   Using docker
   ------------

   TODO ...?

   Using nix
   ---------

   TODO ...?
