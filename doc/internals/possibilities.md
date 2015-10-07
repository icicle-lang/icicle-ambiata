Possibilities
=============

Source has two Possibility modes: Possibly and Definitely.
Possibly means that a thing can fail, while Definitely implies definite success.

Tracking this in the type system has a few benefits:
- one can tell from the type whether a given query may return an error;
- definite queries or parts of queries are assured to have no error handling overhead.

These possibilities only show up in the very first stage of Source.
After that, we need a transform to convert any possibility to explicit error handling.
This should be able to be a Source to Source transformation.

Folds
-----

The source-to-source ReifyPossibility transform converts fold1s into folds.
Core conversion should not need to worry but possibility of folds, or fold1s at all.


### Fold: sum of tombstone

Sum of tombstone field: ``input :: Element Possibly Int``

```
fold sum
    = 0
    : input + sum
~> sum
```

Here is the same program with type annotations.
(Note that usually temporality and possibility would be omitted for Pure and Definitely)

```
fold sum
    = (0     :: Pure    Definitely Int)
    : (input :: Element   Possibly Int) + (sum :: Element Possibly Int) :: Element Possibly Int
~>      (sum :: Aggregate Possibly Int)
```

So in general, a ``Possibly t`` becomes a data type ``Error t`` (or something like ``Either ErrorInfo t``), while ``Definitely t`` stays as a raw ``t``.
Anywhere that a ``Definitely t`` is cast to a ``Possibly t``, we wrap it inside a ``Right`` constructor.
Wherever primitives are applied to possiblies, we need to unwrap them, apply the primitive, and rewrap.

```
fold sum
    = Right 0
    : case input
      |  Left err
      -> Left err
      |  Right input'
      -> case sum
         |  Left err
         -> Left err
         | Right sum'
         -> Right (input' + sum')
         end
      end
~> sum
```

Conversion to Core for this is "trivial":
(assume that simple case expressions are allowed in Core for now)
```
Program
Reduction
 sum = Fold
        (\sum input
          -> case input
             | Left err -> Left err
             | Right input' -> case sum ...
             end
        (Right 0)
        Source
```

### Fold1: newest of tombstone

Newest of tombstone field: ``input :: Element Possibly Int``

```
fold1 newest
    = input
    : input
~> newest
```

with types

```
fold1 newest
    = (input :: Element   Possibly Int)
    : (input :: Element   Possibly Int)
~>   (newest :: Aggregate Possibly Int)
```

We could actually convert this to a fold instead of a fold1.

```
fold  newest
    = Left ErrorEmptyFold1
    : case newest
      -- Initial case
      -- Because input is already Possibly, no need to inject Right
      | Left ErrorEmptyFold1
      -> input 
      -- Rest case
      | _
      -> input
      end
~> newest
```

Now the ToCore conversion becomes rather simple.


### Fold1 over definites

The date always exists (cannot be tombstone) so ``date :: Element Definitely Date``.
If we want the earliest date, we could:

```
fold1 earliest
    = date
    : earliest
~> earliest
```

Again, with types.

```
fold1 earliest
    = date      :: Element Definitely Date
    : earliest  :: Element Definitely Date
~> earliest     :: Aggregate Possibly Date
```

So to turn this into a normal fold we just need to cast the Definitelys to Possiblys with ``Right``.
However, the catch is that because the initial and rest are Definitely, we actually need to unwrap the error first.
```
fold  earliest
    = Left ErrorEmptyFold1
    : case earliest
      -- Initial case
      | Left ErrorEmptyFold1
      -> Right date

      -- Rest case. Unwrap error
      | Right earliest'
      -> Right earliest'

      -- Impossible error! Since initial and rest are Definitely, this should not happen.
      | Left err
      -> Left err
      end
~> newest
```


Window
------
Windows can be left alone. (Right?)

Latest
------
Latest returns a Possibly, because there might not be enough rows to fill the whole buffer.
ReifyPossibility needn't worry about this, but ToCore should be modified:
```
latest 5 ~> value
```
must become something like
```
Program
Streams
 values  = Map (\v -> value v) Source
Reductions
 latests = Latest 5 values
Postcomputations
 post    = if   length latests == 5
           then Right latests
           else Left  ErrorLatestLacksData
```

There are other options - Core Latest could actually return an (Either Error (Array a)), but this would require changing Core and Avalanche.FromCore at the same time.

Note that Latest will also need to change quite a bit in order to allow Latests inside groups; see [issue 107](https://github.com/ambiata/icicle/issues/107).


Group
-----
ToCore needs to do fancy things for Groups.
Assume key and value are both possibly.
```
group key ~> sum value
```
becomes
```
Program
Reductions
 groups = Fold
            (\a v
            -> case a
               | Left err -> Left err
               | Right map
               -> case key v
                  | Left err -> Left (GroupKeyBad err)
                  | Right k
                  -> case Map.lookup k map
                     | None
                     -> case Right 0
                        | Left err -> Left (GroupValueInitialBad err)
                        | Right i  -> Right (Map.insert k i m)
                        end
                     | Some current_value
                     -> case value v
                        | Left err -> Left (GroupValueUpdateBad err)
                        | Right i  -> Right (Map.insert k (current_value + i) m)
                        end
                     end
                  end
               end
            )
            (Right Map.empty) Source
```

Gosh, that's quite an earful.
Note that the ``case Right 0`` should get case-of-constructored out pretty easily.
Funny thing about this is that we cannot use an ``insertOrUpdate`` primitive, but we could probably have lookup return the update index or something too.

GroupFold
---------

So, I don't think anything much special is required here; it just ends up being a fold over the map at the end, so just unwrapping the map before folding.
```
group fold (k,v) = (group key ~> sum value)
~> min value
```

Distinct
--------
Might be better off just converting this to an equivalent groupfold/group in the source-to-source transform.

Filter
------
Filter has very interesting semantics, because it's supposed to be history-agnostic: so if you filter one element, it shouldn't depend on whether any earlier elements were filtered.
So what happens if you are filtering over a field and get tombstone?
I think in this case, the element should be returned through the filter as Error, but subsequent elements must act normally.

So filter actually needs to be converted to something like
```
filter tomb ~> rest
```
becomes two maps and a filter (oh my)
```
x1 = Map (\v -> case tomb v
                | Left e -> (True, Left e)
                | Right p -> (p, Right v))
x2 = Filter (\(p,v) -> p) x1
x3 = Map (\(p,v) -> v) x2
...
rest
...
```

Actually, maybe this could be done in Source-to-Source, but I don't know whether it matters any more.

Let
---
I think let is "fairly simple".

Other stuff
-------

Ok so for now I think, the ToCore conversion will have to deal with some of it: for groups, latests, fold1s etc.
However having this pass beforehand deal with function application and wrapping should simplify that a lot.


