Thoughts
========

Stages
------

Computation based on a few discrete stages, where each stage cannot depend on later stages.

- Pre-computations: constants, setting up arrays, reading lookup tables
- Worker functions: can use pre-computation values, but cannot directly load lookup tables from disk
- Streams: on-line stream computations such as filtering, mapping, taking the latest N of input
- Reductions on streams: each stream can be folded, where worker functions and initial values cannot depend on streams or other reductions
- Post-computations: cannot touch streams, but only the result of reductions and pre-computations


Expressions
-----------
Worker functions must be expressions, while pre-computations could be side-effecting statements like "read this table from disk".

We/I have two semi-conflicting goals for the expressions themselves:

- Expressions should be pure: I don't think anybody needs to be convinced of this, but it's just so important for actually writing this stuff, having it composable, fusion, and so on.
- We want to be able to keep track of allocations in worker functions. If each iteration of the input were to keep allocating more data, we would die very quickly.

But there is a conflict here, because a pure array update requires allocating a new array.

It would be *nice* if we could ensure no-allocation in expressions, but I fear that using linear types or effect tracking to do this is too complicated in both implementation and usage.
For now, I suggest we leave this burden on the user.

```
Exp ::= Exp Int    +  Exp Int
      | Exp Int    -  Exp Int
      | Exp Double +. Exp Double
      | Exp Double -. Exp Double
      | ...
      | update (Exp Int) (Exp a) (Exp (Array a))
      | ...
      | Var
```


Precomputation
--------------
Precomputation stage is just setting constants, allocating initial arrays, and loading lookup tables from disk.

```
Pre ::= Load File
      | Allocate (Exp Int)
      | Exp t
```
(I'm not convinced that "allocate" needs to be here, instead of just in Exp.)


Worker functions
----------------
Worker functions are just expressions.
They can access precomputation values and their arguments.
They cannot access or perform stream computations or reductions.


Streams
-------

Stream transformers must be online: they require no buffering, use no random access, and so on.
All expressions here are worker functions, only able to access the precomputations and their arguments.

```
Stream  ::= Latest (Exp Int)                    : Stream (concrete feature type)
          | Filter (Exp (a -> Bool)) (Stream a) : Stream a
          | Map    (Exp (a -> b))    (Stream a) : Stream b
          | Scan   (Exp (a -> a -> a)) (Exp a)
                                     (Stream a) : Stream a
```

These are not strictly necessary from an expressiveness standpoint, as a fold on each of these could be rewritten as a fold on the original data.
However, making these explicit makes composition a lot easier than with folds.

I'm not sure whether Scan (running sum) is necessary.

Reductions
----------

Reductions take a single stream and fold over it.
There can be multiple reductions, but these reductions cannot rely on each other.
This means that they can all be computed in a single pass.
Again, the expressions here can only refer to precomputations and their arguments.

```
Reduction   ::= Reduce (Exp b) (Exp (b -> a -> b)) (Stream a) : Reduction b
              | HeadOrDefault (Exp b)                         : Reduction b
```


Postcomputations
----------------

Finally, the result of the reductions might need to be modified somehow.
Postcomputations can be any expression, referring to precomputations, the result of reductions and previous postcomputations.
Postcomputations cannot refer to streams.


Types
-----

These are the types of expression values.
Note that there is no "Stream" type, as streams cannot be mentioned in expressions - only reduce and streams.

```
Type    ::= Int
          | Double
          | String
          | Date
          | Array Type
          | Map Type Type
          | List  Type
          | Struct [(Field,Type)]
```

I think we can get away without having real parametric polymorphism.
In the concrete language, we could just recognise "+" and compile it to either "+" or "+." depending on type. 


Computing lookbehind
-----------------------
We need to know upfront, without executing the program, how much lookbehind or historical data is necessary.
It looks like this is just computing the maximum of all the "Latest" streams.
At the moment this is an Exp which could rely on lookup data, but if that's a problem we can just require it to be a constant int.

Code generation
---------------
I'm pretty confident that with these restrictions we'll be able to compile into a single loop over the source stream.
Precomputations and postcomputations are easy, they just go before and after the loop.
We also initialise all the reductions to their initial values before the loop.
Since the only stream combinators take only one input, it should fall out pretty easily too.


Fusion / tupling
----------------
It should also be "quite easy" to merge two virtual features based on the same concrete feature into a single computation.
There can be no data dependencies between the two, so we should be able to just give all the bindings fresh names and append all the pres together, all the streams together, and so on.

The lookbehind of the fused "superfeature" should be the maximum of the two, which fits the definition above.

