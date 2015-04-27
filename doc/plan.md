Plan
====

This is my rough plan for Icicle.
I'm writing it down because there are probably better ways to do this.

Core language
-------------

First, I plan on implementing the core language (see core.md).
The idea is that this should be expressive enough to write virtual features in, but not particularly pleasant for non-programmers.

We need the datatypes, typechecker (with no inference) and evaluator, but not necessarily a parser for this stage.

Once these are implemented, we can hand code some features to check that it is expressive enough.


Avalanche language
---------------
It's unsafe, it's fast, and you certainly don't want to write in it.

This should be an abstraction over an imperative machine, like C or LLVM.
We need an evaluator, but it should also be easy to pretty print into a few gotos or some loops.
We should then be able to have some quickcheck properties so say that the translation from Core to Avalanche preserves semantics.


Icicle language
---------------
We probably need things like type inference, some sort of polymorphism so "count" over arrays and streams looks similar, and nice syntax for people.

It should be possible to write an evaluator and show that translating to Core preserves semantics.

I'm pretty fuzzy on the details of this for now.

