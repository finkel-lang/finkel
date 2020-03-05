Using The Finkel Executable
===========================

The ``finkel`` executable from the ``finkel`` package contains
sub-commands to work with Finkel source codes.


Compiling with Finkel Make
--------------------------

To compile a Finkel source code file, one can use the ``make``
sub-command. Open a file named ``hello.fnk`` with your favorite editor
and save the file with the following contents:

.. literalinclude:: ../code/hello.fnk
   :language: finkel


Then invoke ``finkel make`` to compile the file. The command shown in
the following line will compile the file as an executable named ``hello``:

.. code-block:: console

   $ finkel make -o hello hello.fnk
   $ ./hello
   Hello, World!

The ``make`` sub-command understands most of the options for the
``ghc`` executable ``--make`` mode:

.. code-block:: console

   $ finkel make -o hello -fforce-recomp -prof -fprof-auto hello.fnk
   $ ./hello +RTS -s -p
   Hello, World!
             56,992 bytes allocated in the heap
              4,864 bytes copied during GC
             46,040 bytes maximum residency (1 sample(s))
             23,592 bytes maximum slop
                  0 MB total memory in use (0 MB lost due to fragmentation)

                                        Tot time (elapsed)  Avg pause  Max pause
     Gen  0         0 colls,     0 par    0.000s   0.000s     0.0000s    0.0000s
     Gen  1         1 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

     INIT    time    0.000s  (  0.000s elapsed)
     MUT     time    0.000s  (  0.000s elapsed)
     GC      time    0.000s  (  0.000s elapsed)
     RP      time    0.000s  (  0.000s elapsed)
     PROF    time    0.000s  (  0.000s elapsed)
     EXIT    time    0.000s  (  0.000s elapsed)
     Total   time    0.000s  (  0.001s elapsed)

     %GC     time       0.0%  (0.0% elapsed)

     Alloc rate    0 bytes per MUT second

     Productivity 100.0% of total user, 22.7% of total elapsed


Running REPL
------------

From shell
^^^^^^^^^^

The ``finkel`` executable has ``repl`` sub-command to run an interactive
*read-eval-print-loop* (a.k.a. REPL). To start a REPL from a shell,
invoke ``finkel repl``:

.. code-block:: console

   $ finkel repl
   Hit `Ctrl-d' or type ,q to quit, type ,? for help.
   > (+ 41 1)
   42
   > ,type foldr
   foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
   > ,info Rational
   type Rational = GHC.Real.Ratio Integer  -- Defined in ‘GHC.Real’
   > ,load hello.fnk
   [1 of 1] Compiling Main             ( hello.fnk, interpreted )
   ; loaded hello.fnk
   > main
   Hello, World!
   > ,q
   $


From Emacs
^^^^^^^^^^

There is a major mode named ``finkel-mode`` for the `Emacs
<https://www.gnu.org/software/emacs/>`_ editor, with functionality to
run an interactive REPL session from Emacs. As of the time of writing
this documentation, the REPL interaction feature in the ``finkel-mode``
contains simple functionalities only, similar to the ``inferior-lisp``
mode for other LISP languages.

To use the ``finkel-mode``, first clone the source repository:

.. code-block:: console

   $ git clone https://repository/of/finkel-mode.git


Then place the ``finkel-mode.el`` to somewhere reachable from Emacs,
byte-compile and load the ``finkel-mode.el``. After successful
compilation and load, open a file with ``.fnk`` extension in Emacs,
then hitting ``Ctrl-z`` will show a prompt to start the Finkel REPL
executable.


Getting More Help
-----------------

The ``finkel`` executable contains a ``help`` sub-command to show
brief usages of available commands:

.. code-block:: console

   $ finkel help make
   USAGE: finkel make [command-line-options-and-files]

   OPTIONS:

       --fnk-debug      Show debug messages.
       --fnk-dump-hs    Dump Haskell source code.
       --fnk-help       Show this help.
       --fnk-hsdir=DIR  Save Haskell source code to DIR.
       --fnk-version    Dump Finkel version and exit.

     Other options are passed to ghc.
