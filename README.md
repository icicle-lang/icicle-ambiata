icicle
======

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


specification
-------------

A prototype implementation is being built before we finalize the
complete specification.


prototype
---------

### goals

 - identify language primitives.
 - identify a reasonable heuristic for cost, to help elliminate skew in
   the case of distributed computation.
 - determine an appropriate algorithm for arranging and fusing
   computation/expressions.

MTH: Complete. Example expressions, limitations, simplifications, metadata/dictionary.


simulation
----------

MTH: Spec out.


optimisation
------------

To be completed after prototype. A discussion of optimisation techniques
useful to icicle expressions.
