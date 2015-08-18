Expression fragments
====================

We (will) have these three languages; Icicle, Core and Avalanche.
There are a lot of differences between these, but they all need some way to compute expressions.
I figure that we might as well lift out the Core.Expression type to a general type that can cover all of the cases.
This gives us some nice things like useful and uniform functions for extraction and transform without having to write them three times.

However, the expressions are a bit different:
- different primitives
- different types
- different invariants
So we need to parameterise over these.

Primitives
----------

Core will have no stream computations, as they are described in the top-level program, but Core does have Map primitives like fold, Array filter and so on.

The earliest stage of Avalanche will probably have the same primitives as Core, but real Avalanche will have all combinators gone down to imperative loops.
Avalanche primitives will probably be things like allocating arrays.

For Icicle, I'm imagining that the streamish primitives like "Filter" will work on both streams and arrays, then we'll split them out on the way to Core.


Types
-----

Core has function types because fold primitives need worker functions.

Avalanche does not have function types, except that primitives take value arguments.

Neither Avalanche nor Core have a Stream type, since streams are already lifted up into the top-level program structure, and it would be an error for an expression to refer directly to a stream.


Invariants
----------

In Core and Avalanche, all primitives must be fully applied.
I don't imagine this is the case in Icicle, but it will be easy to eta expand on the way to Core.

Some Core expressions allow functions at the top level: worker functions for stream filters and so on.
But other than that, precomputations and postcomputations must be values, so functions aren't allowed there.
By the time we get to Avalanche, all expressions should be values, not functions.

It'd be super simple to disallow name shadowing for all stages, but I fear this might be burdensome for Icicle.

Disallowing lets or requiring something like A-normal form would be other useful invariants.




Implementation
--------------

```
data Type f
 = Base (BaseType f)
 | Fun  (Type f) (Type f)


class Fragment f where
 type Prim      f
 type BaseType  f
 type FragmentError f

 invariants :: Invariants f
 typeOfPrim :: Prim f -> Type f 

data Invariants f
 = Invariants

 -- Do all names need to be unique?
 { uniqueNames      :: Bool

 -- Can primitives be partially applied
 , partialPrims     :: Bool

 -- Require right hand side of applications to be bound variables
 , appsOnlyVars     :: Bool

 -- Check if this type is allowed at the top-level
 , checkTopType     :: Type f -> Either (FragmentError f) ()

 -- Core only allows base types in lambdas
 -- Lambdas and lets could easily be outright banned too.
 , checkLamType     :: Type f -> Either (FragmentError f) ()
 , checkLetType     :: Type f -> Either (FragmentError f) ()
 }
```
