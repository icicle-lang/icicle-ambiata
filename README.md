icicle
======

[![Build Status](https://api.travis-ci.org/ambiata/icicle.svg?branch=master)](https://travis-ci.org/ambiata/icicle)

> Icicle == I(vory) SQL.

A streaming query language.

purpose
-------

Icicle is a small language for expressing (and statically verifying)
a certain set of streaming computations.

The driver for this language is the [ivory](https://github.com/ambiata/ivory)
data-store, and the rest of this documents things in terms of ivory,
however the problems being addressed are not unique to ivory, and it
is plausible that this could be adapted to different contexts. For more
context on ivory see:

 - https://github.com/ambiata/ivory

 - https://speakerdeck.com/ambiata/ivory-concepts

 - https://speakerdeck.com/ambiata/ivory-a-data-store-for-data-science

 - https://speakerdeck.com/ambiata/ivory-data-modelling


facts & values
--------------

Facts are (typed) values, keyed along three dimensions:

 - Entity, this would be typically thought to represent the primary key of
   a row in a traditional data base.

 - Attribute, this would be typically thought to represent the name of
   a column in a traditional data base.

 - Time, this represents when a facts if valid at. Different types of
   facts may interpret this in different ways (for example for a state
   like value, this would indicate a fact is valid from time (t) until
   the next fact with the same entity / attribute and a more resent
   time dimension. There is no analog in traditional data bases, but
   this is more common in immutable or append-only data stores.

Values themselves are structured, and may be primitives, structs,
or lists of values.


data processing
---------------

Data processing in ivory (and similar data stores) is heavily
parallelized. This places restrictions on how data is processed
and how expressions can relate to each other - in most cases
these restrictions are simplifying to the desigin of icicle.

The basic invariants are:

 - Data is processed in "batches", where a batch has a set of uniform
   properties:

 - All facts in a batch are for the same entity.

 - All facts in a batch are for the same attribute.

 - Facts in a batch are processed in chronological order.

 - A batch is _guaranteed_ to have _all_ facts for a given
   entity / attribute.


expressions
-----------

### types of expressions

Ivory contains notes on some required expressions:

 - https://github.com/ambiata/ivory/blob/master/doc/design/feature-gen.md

Supporting this will be the existing corpus of ivory dictionaries.


### outputs

One complication is that there are two distinct outputs from each
expression:

 - The decision of which facts are interesting to a given snapshot.

 - The output of the actual computation.


specification
-------------

A prototype implementation is being built before we finalize the
complete specification of the icicle.


prototype
---------

The goals of the v0 prototype implementation are:

 - identify language primitives.

 - identify a reasonable heuristic for cost, to help elliminate skew in
   the case of distributed computation.

 - determine an appropriate algorithm for arranging and fusing
   computation/expressions.

 - determine scope and plausibility of static verification.

 - propotype and iterate on surface syntax.

 - best strategy for code-gen given primitives (shallow embedding,
   with supporting libraries vs bit twiddling).


simulation
----------

The simulation engine for the prototype shall use ivory EAVT text format
as an ingestion. From this text format it shall:

 - Parse each row into raw facts.

 - Join in a dictionary to convert raw facts into typed facts.

 - Sort by entity and attribute, then group into like entity and attribute,
   then perform a secondary sort on time.

 - Take a compiled expression and run over each group. There should be two
   outputs produced:
     1. A feed of all the facts that were used to compute values.
     2. A feed of all computed values, EAV format.

optimisation
------------

To be completed after prototype. A discussion of optimisation techniques
useful to icicle expressions.
