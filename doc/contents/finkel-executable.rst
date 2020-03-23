Using The Finkel Executable
===========================

The ``finkel`` executable from the ``finkel`` package contains
sub-commands to work with Finkel source codes.


Compiling with Finkel Make
--------------------------

To compile a Finkel source code file, one can use the ``make``
sub-command. Open a file named ``hello.fnk`` with your favorite editor
and save the file with the following contents:

.. literalinclude:: ../include/finkel-executable/hello.fnk
   :language: finkel

Then invoke ``finkel make`` to compile the file. The command shown in
the following line will compile the file as an executable named ``hello``:

.. literalinclude:: ../include/finkel-executable/hello.console
   :language: console

The ``make`` sub-command understands most of the options for the
``ghc`` executable ``--make`` mode:

.. literalinclude:: ../include/finkel-executable/hello-prof.console
   :language: console

The compiled executable understands RTS options:

.. code-block:: console

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


From Emacs
^^^^^^^^^^

There is a major mode named ``finkel-mode`` for the `Emacs
<https://www.gnu.org/software/emacs/>`_ editor, with functionality to
run an interactive REPL session from Emacs.  See the README file in
the `finkel-mode repository
<https://github.com/finkel-lang/finkel-mode/#finkel-mode>`_ for more
details.


Getting More Help
-----------------

The ``finkel`` executable contains a ``help`` sub-command to show
brief usages of available commands:

.. literalinclude:: ../include/finkel-executable/finkel-help-make.console
   :language: console
