Existing virtual features
=========================

If we can express all these, we're doing OK.
This is just using a made-up core syntax, and windowed expressions which aren't implemented.
The plan is to add constant size windows and make it so precomputations, streams and reductions can only use the current/snapshot/chord date if they are windowed.

I'm assuming that we can add array, map and set primitives fairly easily.
I'm also ignoring tombstones for now, but I think we can add an option/maybe-ish type for that.


CountBy
--------------

```
Program CountBy where
    Source  : Stream key
Precomputations
    init    = Map.empty
Reductions
    agg     = Fold (\agg' val' -> Map.updateOrInsert (+1) 0 val' agg')
                    init
                    Source
Return
    agg     : Map key Int
```

Nothing too complicated here. Because this is unwindowed, we could just store the map as the reduction result inside the snapshot.

We should be able to prefix stream maps and stream filters here with no problem.
Changing it to windowed would mean we'd have to store the list of used values instead of the map result - because some inputs would drop off the start of the windowed stream.

Note that I'm assuming this exists:
```
Map.updateOrInsert : (v -> v) -> v -> k -> Map k v -> Map k v
```


CountBySecondary
-----------------------

```
Program CountBySecondary where
    Source  : Stream (key,value) 
Precomputations
    init    = Map.empty
Reductions
    agg     = Fold (\agg' val' ->
                        Map.updateOrInsert
                            (Set.insert (snd val'))
                            (Set.singleton (snd val'))
                            (fst val')
                            agg')
                    init
                    Source
Return
    agg     : Map key (Set value)
```

This really isn't very different from CountBy except for a more complicated worker function.


Count
-----

```
Program Count where
    Source  : Stream (Fact n)
Streams
    vals    = Filter    (\v -> not (tombstone v)) Source
    ones    = Map       (\_ -> 1)                 vals
Reductions
    count   = Fold (+) 0 ones
Return
    count   : Int
```

Again, I see no reason that this can't be windowed.


CountDays
---------

```
Program CountDays where
    Source  : Stream (Fact v)
Streams
    vals    = Filter (not.tombstone) Source
    dates   = Map     dateOf         vals
Reductions
    set     = Fold Set.insert Set.empty dates
Return
    Set.size set : Int
```

This one is interesting because it's so trivial.
We aren't looking at the current date, so this doesn't need to be windowed.
We would need to store the entire set, but because the dates are monotonically increasing (right?) we could cheat by folding with the last seen date and count unique.

```
Program CountDays where
    Source  : Stream (Fact v)
Streams
    vals    = Filter (not.tombstone) Source
    dates   = Map     dateOf         vals
Reductions
    uniques = Fold
                (\ds v ->
                    -- If last seen date is same as this date, don't update
                    if   Just v == fst ds
                    then ds
                    -- Otherwise update last seen date and increment
                    else (Just v, snd ds + 1) )

                -- Start with no last seen date
                (Nothing, 0) 
                dates
Return
    snd uniques : Int
```



CountUnique
-----------

Fold with `Set.insert`, and then postcomputation of `Set.size`.
For non-windowed, the entire set would be stored in snapshot for tomorrow's computation.


DaysSinceEarliest
-----------------

```
Program DaysSinceEarliest where
    Source  : Stream (Fact v)
Reductions
    first   = Fold (\a v -> if a == Nothing then Just v else a)
                    Nothing
                    Source
Postcomputation with now : Date
    since   = map (\f -> daysOf (now - f)) first
Returning
    since   : Maybe Int
```

So this doesn't need windowing because it's only the postcomputation that depends on the snapshot time.

The snapshot would just store the result of the fold - the first Maybe Date.

We could rewrite this with a "take first" too, and the snapshot would just store the first fact.

```
Program DaysSinceEarliest where
    Source  : Stream (Fact v)
Streams
    first   = Take 1 Source
Reductions
    first'  = Fold (\a v -> Just v) Nothing first
Postcomputation with now : Date
    since   = daysOf (now - first')
Returning
    since   : Int
```


DaysSinceEarliestBy
-----------------
This is really similar to above, except we're building up a `Map v Date`.
We start with an empty set and only insert if there's no existing value - we don't update or overwrite.
Then at the end we would go over the Map and calculate days since.


DaysSinceLatest
---------------
These are really similar too, and only the postcomputation needs the current date.


DaysSince
---------
Again, this uses the latest value and only uses the current date in the postcomputation.


Filter
------
Primitive.


Gradient
--------
```
Program Gradient where
    Source  : Stream (Date, Double)
Streams
    nums    = Map (\(d,i) -> (numOfDate d, i)) Source
    sqs     = Map (\(x,y) -> (x, y, x*x, x*y, 1)) nums
Reduction
    sums    = Fold (+) (0,0,0,0,0) sqs
Postcomputations
    (x, y, xx, yy, count)
            = sums
    z       = (xx * count) - (x * x)
    fixed   = if   z == 0
              then 0
              else ((xy * count) - (x * y)) / z
Returning
    fixed   : Double
```

Looks like this will work fine with windowed or not.


Interval
--------

This isn't using the current date, only the difference between two fact dates.
Looks like just a fold on (DateTime, value).
TODO.


Inverse
-------
Just adding (1/) to the return expression.


LatestBy
--------
Looks fine, just inserting and overwriting into map.

LatestN
-------
Primitive.

Latest
------
Same as `LatestN` or `Fold (\a v -> Just v) Nothing`

Max
---
Just a fold.


MaximumInDays
-------------
We should be able to implement this with just a fold, with no date set. Same for mean etc.



Skip a few...
-------
...


NumFlips
--------
This one is my favourite but it's just an ugly fold.
```
Program NumFlips where
    Source  : value
Reductions
    red     = Fold
                (\a v -> if   fst a == Just v
                         then a
                         else (Just v, snd a + 1))
                (Nothing, 0)
                Source
Returning
    snd red
```


Quantile
--------

It looks like quantile is the only one that actually requires random access to the date set - the others can be implemented as folds on top of it.
todo...
