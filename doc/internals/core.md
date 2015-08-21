Core
========

The Core language is at roughly the same level of abstraction as a query execution plan.

Stages
------

Computation is based on a few discrete stages, where each stage cannot depend on later stages.

- Pre-computations: constants, more or less
- Worker functions: can use pre-computation values and any arguments, but cannot refer directly to streams or reductions
- Streams: on-line stream computations such as filtering, mapping and windowing
- Reductions on streams: folds over streams, such as sum, latest N entries, and so on.
- Post-computations: cannot touch streams, but only the result of reductions and pre-computations


Expressions
-----------

Expressions are simple pure computations.
We have lambdas and lets, but no fixpoint or recursion.

```
Exp ::= Exp Int    +  Exp Int                       : Exp Int
      | Exp Int    -  Exp Int                       : Exp Int
      | Exp Double +. Exp Double                    : Exp Double
      | Exp Double -. Exp Double                    : Exp Double
      | ...
      | (\b : a. Exp b)                             : Exp (a -> b)
      | let b = Exp a in Exp b                      : Exp b
      | Var                                         : Exp a
```


Precomputation
--------------
Precomputation stage is just setting constants.

```
Pre ::= Name = Exp a
```


Worker functions
----------------
Worker functions are just expressions.
They can access precomputation values and their arguments.
They cannot access or perform stream computations or reductions.

Worker functions also cannot return functions or take functions as arguments.


Streams
-------

Stream transformers must be online: they require no buffering, use no random access, and so on.
All expressions here are worker functions, only able to access the precomputations and their arguments.
The content types of streams must be concrete values, not functions or streams.

```
Stream  ::= Source                              : Stream (concrete feature type)
          | Windowed (Days, Days)    (Stream a) : Stream a
          | Filter (Exp (a -> Bool)) (Stream a) : Stream a
          | Map    (Exp (a -> b))    (Stream a) : Stream b
```

These are not strictly necessary from an expressiveness standpoint, as a fold on each of these could be rewritten as a fold on the original data.
However, making these explicit makes composition a lot easier than with folds.

The important part for Bubblegum is that these are stateless, operating only on the current index and the current value.

Reductions
----------

Reductions take a single stream and fold over it.
There can be multiple reductions, but these reductions cannot rely on each other.
This means that they can all be computed in a single pass.
Again, the expressions here can only refer to precomputations and their arguments.

```
Reduction   ::= Reduce (Exp b) (Exp (b -> a -> b)) (Stream a) : Reduction b
              | Latest (Exp Int)                   (Stream a) : Reduction (Array a)
```

Latest should use a circular buffer to take the last N elements from an array.
This could really be implemented with Reduce, but it seems so fundamental that it's worth including.
Latest also has very different Bubblegum semantics from a latest implemented with actual Reduce.
The important part is that latest does not actually inspect all the *values* of the stream, because they will be stored and then thrown away.


Postcomputations
----------------

Finally, the result of the reductions might need to be modified somehow.
Postcomputations can be any expression, referring to precomputations, the result of reductions and previous postcomputations.
Postcomputations cannot refer to streams.

Postcomputations are also the only place that the current or snapshot date can be read and used.
By disallowing reductions and stream transformers from using the current date, we ensure determinism:
the stream transformers have the same output each time they are run on the same data.
This is very important for computing lookbehind and so on, since we know that only same values that are needed today will be needed tomorrow.


Types
-----

These are the types of expression values.
Note that there is no "Stream" type, as streams cannot be mentioned in expressions - only reduce and streams.

```
Type    ::= Int
          | Double
          | String
          | Date
          | Bool
          | Unit
          | Option Type
          | Array Type
          | Map Type Type
          | List  Type
          | Struct [(Field,Type)]
```

There is no kind of polymorphism in Core.
All primitives are explicitly monomorphically typed, and lambdas are monomorphic too.


Bubblegum: Computing lookbehind
--------------------------------
Along with the "obvious" value semantics, each program needs to keep track of which stream values were used to compute the output.

It is important that the Stream computations cannot depend on previous stream values, which means that if a stream value contributes to the output, that stream value depends on no previous stream values.

Each value passed to a fold reduction is assumed to be used, but latests only use their last N values, which is why latests aren't implemented as folds.

Postcomputations cannot access stream values, but we can assume that all reductions are used.

What about "numflips" or a sum over all time?
Since it's simply a reduce over all previous data, and the previous data isn't going to change, we should be able to just use the previous reduce value etc.

There are three types of reductions: 

- latest N: the used facts are stored
- windowed: the used facts are stored
- fold over all time: the current accumulator value is stored as lookbehind

Note that the fold reduction's value will not necessarily be the output value of the virtual feature, as a postcomputation could modify that.


Code generation
---------------
Generating loops for Core programs turns out to be rather easy, because of the lack of "join" or "zip" combinators that take multiple streams.
This means each stream has (at most) one parent, so we can simply nest each stream's statements under its sole parent's statements.
This seems to be a very simple version of "flow fusion".


Fusion / tupling
----------------
It is also quite easy to merge two virtual features based on the same concrete feature into a single computation.
There can be no data dependencies between the two, so we simply give all the bindings fresh names and append all the pres together, all the streams together, and so on.
Afterwards, a simple common subexpression elimination is performed, which is perfectly safe (unlike in Haskell) because there is no laziness.


Updating old data
-----------------
I assume that if we update historical data, we will need to go back to the snapshot for the earliest update and recompute all afterwards..

I also assume that if we change the virtual features, we will need to recompute using all data.


