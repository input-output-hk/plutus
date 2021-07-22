.. _what_is_plutus_foundation:

What is Plutus Foundation?
==========================

In order for an application to run its :ref:`trusted kernel<what_is_the_plutus_platform>` of logic as a script on a :ref:`ledger<what_is_a_ledger>`, the ledger needs a way of specifying and executing scripts.
Scripts are simply programs, so this means we need a *programming language*.

Plutus Core
-----------

In the Plutus Platform, this language is *Plutus Core*.
Plutus Core is a variant of the lambda calculus, a well-studied formalism for computing.

.. note::
    Plutus Core is our "assembly language".
    Trust me, you don't want to see any!
    Dealing with that is the compiler's job.

Plutus Core is designed for simplicity, determinism, and to allow careful cost control of program execution.
Using the lambda calculus makes it an easy compilation target for functional programming languages, and allows us to have a simple, formally verified evaluator.

Plutus Tx
---------

Writing Plutus Core by hand is not a job for a human!
It is designed to be written by a compiler, and the Platform provides a compiler from a subset of Haskell to Plutus Core.
This subset is known as Plutus Tx.
This allows you to seamlessly write applications in Haskell, while compiling part of the code to on-chain Plutus Core, and part into an off-chain application.

Supporting "mixed" code in this way enables libraries written with the Plutus Haskell SDK to share logic and datatypes across both parts of the application, reducing the risk of errors significantly.

Further reading
---------------

The formal details of Plutus Core are in its specification :cite:p:`plutus-core-spec`.
The design is discussed in :cite:t:`plutus-report`.

For more about Plutus Tx, see the :ref:`tutorial<plutus_tx_tutorial>`.
