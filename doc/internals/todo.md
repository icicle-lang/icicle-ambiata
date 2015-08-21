Todo for Source
===============

Case expressions and ifs
------------------------

We probably need slightly clunkier syntax than Haskell, unless we want to implement full-blown whitespace sensitivity.
I think something like this would be fine.
```
option d v
 = match v
   | None   -> d
   | Some a -> a
   end
```

Also need completeness checking, however since there are only a few types to pattern match on, this should be simple enough.


Folds over groups
-----------------

Groups must end in folds, but there is currently no way to fold over all the values in the group.

Suppose we want to group by days, then find the day with the maximum number of entries:
```
feature feat
~> group fold (key,value) = (group date ~> count)
~> maximum value
```

We could probably simplify the syntax a bit; there's no point of a "group fold" over something that isn't a group.
Keeping them separate might be useful for function reuse though.


Get current time (now)
----------------------
Add primitive ``now`` as Aggregate Date.
It needs to be Aggregate because filters, and folds cannot actually access the current date; it would make them non-deterministic.


Boolean connective primitives
-----------------------------
Define ``(&&)``, ``(||)`` and so on as operators.
Define ``not`` as primitive.
Once we have case expressions we could probably define these in the prelude, however we don't have any way of defining infix operators in the prelude at the moment - and I'm not convinced it's particularly necessary.


Set operations
--------------

I think "CountBySecondary" requires set operations.
```
setOf v
 = let fold
    set = Set.empty
        : Set.insert v set
 ~> set
```

Array operations
----------------
Since the result of ``latest`` can be an array, it would be very useful to have indexing, filters and whatnots.
Currently filters are allowed, but only if the end result is an aggregate:
```
latest 5 ~> filter pred ~> count
```
is allowed, whereas this is not, as the result is an element.
It should return an array of values, filtered.
```
latest 5 ~> filter pred ~> value
```


