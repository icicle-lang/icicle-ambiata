Icicle (source language)
========================

We now have a reasonable target language - Core - and have enough of the compilation pipeline to convince ourselves we can implement it efficiently.

We want a nice source language that we can translate to Core.

Vague high-level goals
======================

* Restrict to single pass over input
    - We get this almost "by definition" by the conversion to Core

* Type inference
    - This should be relatively simple: most lambdas are worker functions

* Polymorphism over arithmetic primitives
    - (+) works for Int, Double
    - Less than works for Int, Double, dates
    - Perhaps we won't need to perform generalisation, if we don't have function bindings

* Filter, map, fold and so on should work for streams and arrays. Either
    - Polymorphic over Stream and Array,
    - Or later pass that extracts some streams to Arrays.
    

Questions
=========

Do we need function definitions?
------

I don't *think* we would need function definitions in programs themselves, but having a Prelude of some kind would be real useful.
For example, being able to define Count in terms of fold and map:

```
Sum     :: Streamy s, Num n => s n -> n
Sum xs   = Fold (+) 0 xs

Count :: Streamy s, Num n => s a -> n
Count xs = Sum (Map (\_ -> 1) xs)
```

Maybe we can require these to be explicitly typed to simplify typechecking, maybe that won't be necessary.


Implicit casts between number types?
------
I would really rather not.


Examples
========

Let's try writing a few programs in some made-up syntax, then imagining how we'd typecheck and convert to Core.


Count
-----

Start simple.
Ideally we'd have a prelude with count = sum.map or whatever.

```
Count(Source)
```

unfolding the Prelude, we get

```
Fold (+) 0 (Map (\_ -> 1) Source) 
```

Since the `Map` is working directly over the Source, it must be a stream computation (as opposed to array).
The Fold is working over a stream computation, so it must be a reduction.

So we end up with a Core program like
```
Streams
    ones = Map (\_ -> 1) Source
Reductions
    sum  = Fold (+) 0 ones
Return
    sum
```


We could also have a windowed count, like

```
Count( Windowed(50) )
Windowed :: Int -> Stream f

Count( Windowed(Source, 50) )
Windowed :: Stream f -> Int -> Stream f
```
Note that Windowed is not polymorphic and returns a Stream rather than a Streamy which could be an Array.

Which one? Does it matter?
If the only Stream computations are built on Filter, Map and Source then we can certainly move window in:
```
Windowed(Filter(p,xs), n) = Filter(p, Windowed(xs, n))
```

CountBy
-------

```
CountBy :: Streamy s, Ord k => s k -> Map k Int
CountBy xs
 = Fold (\map v -> Map.updateOrInsert (+1) 1 v map)
     Map.empty
     xs
```

Do we need Ord?

This should work on arrays or streams. If it's applied to a stream, eg Source, the Fold is a stream reduction, otherwise it's an array fold.

What makes me anxious here is that there's nothing in the type that says the result can't be used in stream computations.
In fact - maybe it could if it were doing a countby over a lookup table.

```
CountBy(Source)
==> stream reduction

CountBy(Windowed(Source,50))
==> stream reduction

CountBy(Latest(Source,10))
==> latest reduction, followed by array fold
```


Counterexamples
===============
It's probably worth looking at what needs to be outlawed.

Array normalise
---------------

We can normalise a latest, fine:

```
let l = Latest(Source, 10)          :: Array Int
    s = Sum(l)                      ::       Int
in  Map((/s), l)                    :: Array Int

==>

Stream
    Source
Reductions
    l = Latest(Source, 10)
Post
    s = Array.Sum(l)
    l'= Array.Map((/s), l)
```

or even if the sum is over all time, not just the latest

```
let l = Latest(Source, 10)          :: Array Int
    s = Sum(Source)                 ::       Int
in  Map((/s), l)                    :: Array Int

==>

Stream
    Source
Reductions
    l = Latest(Source, 10)
    s = Fold (+) Source
Post
    l'= Array.Map((/s), l)
```

However, a normalise over stream - windowed or unwindowed is not allowed.

```
let l = Windowed(Source, 10)        :: Stream Int
    s = Sum(Source)                 ::        Int
in  Map((/s), l)                    :: Stream Int
       -- ^ this stream transformer mentions a stream fold result in its worker function

=/=>

Stream
    l   = Windowed Source 10
    l'  = Map (/s) l
             -- ^ s is not bound yet!
Reductions
    s = Fold (+) Source
```


Effect typing
==============

So, can we taint stream reductions so they cannot be used in other stream transforms? Sounds like (co)effect typing or a monad to me.

Suppose we have effects for describing where a computation can be run.

```
data Where
 = Pre
 | Strm
 | Post
```

Below, `S` stands for "suspended computation". It describes a computation that requires some context to execute.
```
S :: Where -> * -> *
```

We can also cast from pre-computations to postcomputations, but not the other way.
We probably could go from Pre to Strm, but I don't see the point. 
We cannot cast from Strm to Post: post-computations are only available after all the streams have been handled, but casting Strm to Post would make streams available.

Note that a cast from Post to Pre would allow multiple passes over the data, and so must be disallowed.

```
-- Useful:
castPrePost :: S Pre a -> S Post a
-- Valid but useless:
castPreStrm :: S Pre a -> S Strm a 

-- Invalid and useless:
castStrmPost :: S Strm a -> S Post a
-- Invalid but useful:
castPostPre  :: S Post a -> S Pre  a
```

We could also imagine that Pre is identity/pure, which would make `castPostPre` like `unsafePerformIO`.

Also, unanointed values can be injected into a value and computations strung along like you'd expect.
```
return :: a -> S w a
bind   :: S w a -> (a -> S w b) -> S w b
```

Folding over the stream means the result can only be read in postcomputation.
However, the inputs and so on - the initial value, the input stream, need to be in Pre and Strm.

```
FoldStream  :: (a -> b -> S Pre a) -> S Pre a -> S Strm (Stream b) -> S Post a
Latest      :: S Pre Int -> S Strm (Stream a) -> S Post (Array a)
```

Stream transformers require worker functions to be in pre, and streams themselves in Strm.

```
MapStream   :: (a -> S Pre b)           -> S Strm (Stream a) -> S Strm (Stream b)
FilterStream:: (a -> S Pre Bool)        -> S Strm (Stream a) -> S Strm (Stream a)
```

The only way to introduce a Stream is in Strm, and the only way to move out of Strm is with FoldStream or Latest, which hides the Stream.
Stream and Strm are very much intertwined.
```
Source :: S Strm (Stream InputFact)
```

Most things don't really care - an array fold can happen wherever, as long as its arguments and return all agree.
```
FoldArray   :: (a -> b -> S w a) -> S w a -> S w (Array b) -> S w a
```
So this could run in Pre, Strm or Post, but there's not much point running in Strm since we can't create a Stream, and can't go from Strm to anything else.

If one of the arguments referred to the result of a stream reduction, its type would be (S Post), so the result of the FoldArray would be in Post too.


Relationship to Core
--------------------
This is all very similar to how Core works, with the separation into four parts.
We should be able to perform "relatively simple" type inference, as well as elaboration by adding returns, binds and casts wherever necessary. 

After elaboration, it should be easy enough to decide which part of core to convert to.


Elaboration
-----------
When should elaboration run?

We want as many things as possible to run on streams or arrays,
and we probably want things in the prelude like "Count".

But fold doesn't really have a general type we can give, that tells the whole story:

```
Sum :: Streamy s => s Int -> Int
Sum = Fold (+) 0

==>

SumStream   :: S Strm (Stream Int) -> S Post Int
SumStream = FoldStream (+) 0
SumArray    :: S w    (Array  Int) -> S w    Int
SumArray  = FoldArray  (+) 0
```

So we don't want to run elaboration until *after* the prelude definitions have all been inlined in.
Because for the prelude definitions, we don't know what they're working on until after they're inlined.

This means in the prelude we might have a function like `AddSum`
```
AddSum :: Streamy s => s Int -> s Int
AddSum xs = Map (\x -> x + Sum(xs)) xs
```
but this only works for arrays, not streams, so its type should be specialised `Array`, but we don't know until after elaboration.
Maybe elaboration with the staged coeffect junk is the wrong way to do this.

This type for AddSum is a lie, so it makes me feel bad. Is it really a problem though?
It'll get inlined and elaborated and then checked, so we still won't allow bad programs, but it still feels wrong.
There must be a simpler way.




