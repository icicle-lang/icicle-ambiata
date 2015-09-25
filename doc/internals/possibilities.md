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

Fold examples
------------------


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

or if the ToCore conversion handled the ErrorEmptyFoldl1, as it currently does, then we actually don't need to modify this one at all:

```
fold1 newest
    = input
    : input
~> newest
```


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

But we can also leave it as a fold1 with
```
fold1 earliest
    = Right date
    : earliest
~> earliest
```


Other stuff
-------

Ok so for now I think, the ToCore conversion will have to deal with some of it: for groups, latests, fold1s etc.
However having this pass beforehand deal with function application and wrapping should simplify that a lot.

