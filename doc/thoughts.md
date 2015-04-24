Thoughts
========

Stages
------

Computation based on a few discrete stages, where each stage cannot depend on later stages.

- Pre-computations: constants, setting up arrays, reading lookup tables
- Worker functions: can use pre-computation values, but cannot directly load lookup tables from disk
- Streams: on-line stream computations such as filtering, mapping, taking the first N of input
- Reductions on streams: each stream can be folded, where worker functions and initial values cannot depend on streams or other reductions. Latest N is actually a reduction.
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
Exp ::= Exp Int    +  Exp Int                       : Exp Int
      | Exp Int    -  Exp Int                       : Exp Int
      | Exp Double +. Exp Double                    : Exp Double
      | Exp Double -. Exp Double                    : Exp Double
      | ...
      | update (Exp Int) (Exp a) (Exp (Array a))    : Exp (Array a)
      | generate (Exp Int) (Exp (Int -> a))         : Exp (Array a)
      | index  (Exp Int) (Exp (Array a))            : Exp a
      | ...
      | Var                                         : Exp a
```


Precomputation
--------------
Precomputation stage is just setting constants, and loading lookup tables from disk.
Presumably initial arrays and maps would be allocated here, too.

```
Pre ::= Load File
      | Exp t
```


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
Stream  ::= Source                              : Stream (concrete feature type)
          | Take   (Exp Int) (Stream a)         : Stream a
          | Filter (Exp (a -> Bool)) (Stream a) : Stream a
          | Map    (Exp (a -> b))    (Stream a) : Stream b
```

These are not strictly necessary from an expressiveness standpoint, as a fold on each of these could be rewritten as a fold on the original data.
However, making these explicit makes composition a lot easier than with folds.

I had Scan here before, but a) I'm not convinced it's necessary, and b) it complicates lookbehind computation.

Similarly, Drop could be added, but it also complicates lookbehind.

The important part for lookbehind is that these are stateless, operating only on the current index and the current value.

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
Latest also has very different lookbehind semantics from a latest implemented with lookbehind.
The important part is that latest does not actually allow inspection of all the *values* of the stream, because they will be stored and then thrown away.


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
Along with the "obvious" value semantics, each program needs to keep track of which stream values were used to compute the output.

It is important that the Stream computations cannot depend on previous stream values, which means that if a stream value contributes to the output, that stream value depends on no previous stream values.

Each value passed to a fold reduction is assumed to be used, but latests only use their last N values, which is why latests aren't implemented as folds.

Postcomputations cannot access stream values, but we can assume that all reductions are used.

What about "numflips" or a sum over all time?
Since it's simply a reduce over all previous data, and the previous data isn't going to change, we should be able to just use the previous reduce value etc.

So in summary, each fold reduction should output its current value as the lookbehind, and each latest reduction should output the values it used.
Note that the fold reduction's value will not necessarily be the output value of the virtual feature, as a postcomputation could modify that.


Code generation
---------------
I'm pretty confident that with these restrictions we'll be able to compile into a single loop over the source stream, potentially followed by folds over latest arrays and so on.
Precomputations and postcomputations are easy, they just go before and after the loop.
We also initialise all the fold reductions to their initial values before the loop.
Since the only stream combinators take only one input, it should fall out pretty easily too.


Fusion / tupling
----------------
It should also be "quite easy" to merge two virtual features based on the same concrete feature into a single computation.
There can be no data dependencies between the two, so we should be able to just give all the bindings fresh names and append all the pres together, all the streams together, and so on.

The lookbehind of the fused "superfeature" should be the union of the two.


Updating old data
-----------------
I assume that if we update historical data, we will need to go back to the snapshot for the earliest update and recompute all afterwards..

I also assume that if we change the virtual features, we will need to recompute using all data.


Concrete language
-----------------
The above has been describing the core language, not the concrete language; we will want to provide a nicer layer for people to write in.

Things like "+" should work for any kind of number, but for code generation it's better to know exactly which plus we're talking about.

There is a distinction between Streams and Arrays, with unintuitive or messy details like "Latest" takes a Stream and returns an Array, but "Take" takes a Stream and returns a Stream.
This distinction should simplify code generation and lookbehind computation a lot, but makes the user's life more difficult.
The concrete language should allow treating Streams and Arrays the same in most cases, eg "Sum(Source)" and "Sum(Latest(10,Source))" should both work, but would be translated to a stream reduce and a latest followed by array reduce, respectively.

```
Sum(Source)
==>
PRE=========

STREAMS=====
s = Source

REDUCTIONS==
r = Fold (+) 0 s

POST========
return r
```


```
Sum( Latest(10, Source) )
==>
PRE=========

STREAMS=====
s = Source

REDUCTIONS==
l = Latest(10, s)

POST========
r = ArrayFold (+) 0 s

return r
```



