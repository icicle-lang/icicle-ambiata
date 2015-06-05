Typing rules
============

Program
-------

Simple queries
```
P := feature V ~> Q

Q := C ~> Q
   | E

C := latest Int
   | filter E
   | group  E

E := sum Q
   | max Q
   | count
   | E + E
```


Contexts
--------

Typechecking contexts that are threaded through with the current environment - the list of available bindings and their types.
I'm not certain that splitting these into aggregates and scalars is necessary, when we have computations split into different universes anyway.

- Γ: aΓΓregates: bindings like ``let x = sum y``
    * These bindings are not available to value computations

- Σ: Σcalars: available things in the current feature and scalar bindings
    * The idea is that these exist for each record, but have no meaning in aggregate.

- Φ: Φeatures, a scalar environment for each concrete feature.
    * A scalar feature like salary would just have ``value : Int``
    * A struct might have ``plan : PlanId, usage : Double`` or something.
    * I guess a pair (a,b) would have ``fst : a, snd : b``
    * This is only used at the top level, to look up the feature


Universes?
----------
Universes describe the kind of computation, or where the computation occurs.
The idea is pretty similar to the Pre/Stream/Reduce/Post stratification in Core, but here all the computations are mixed up.
So we need to infer the universes before we can separate them out.

- Agg τ: some aggregate. aggregate computations can depend on values/scalars and groupings.
- Val τ: some scalar value. scalar computations cannot use aggregations!

Actually, it looks like another one is needed:
- Group τ τ': the result of a grouping, where τ is the key and τ' is the value.
This is necessary because we can do aggregates over groups - but we can't do aggregates over aggregates.
They are sort of half-way between aggregates and values, then.

In the rules below, the variable ``u`` can be assumed to be bound by a forall over universes.

Types
-----
The concrete types don't matter so much here.
- Time
- Int
- Enum?
- Map τ τ'
- Bool
- and so on

Judgments
---------

(Toplevel)
Look up a concrete feature and use it. There can only be one of these in a program.
```
Φ ⊢ n : φ    φ, date : Time; ∅ ⊢ Q : Agg τ
------------------------------------------
        Φ ⊢ feature n ~> Q : Agg τ
```


(LatestAgg)
Getting the aggregate of the latest is itself an aggregate.
```
      Σ; Γ ⊢ Q : Agg τ
----------------------------
Σ; Γ ⊢ latest i ~> Q : Agg τ
```

(LatestVal)
The latest of a value is allowed, and the result is an aggregate array of the values
```
          Σ; Γ ⊢ Q : Val τ
------------------------------------
Σ; Γ ⊢ latest i ~> Q : Agg (Array τ)
```

(Filter)
The predicate of a filter cannot mention aggregates - it must be in Val.
The output is in Agg.
```
Σ; ∅ ⊢ e : Val Bool        Σ; Γ ⊢ Q : Agg τ
-------------------------------------------
       Σ; Γ ⊢ filter e ~> Q : Agg τ
```


(Group)
The expression we're grouping over must be enum-ish.
```
Σ; ∅ ⊢ e : Val τ    Enum τ    Σ; Γ ⊢ Q : Agg τ'
-----------------------------------------------
       Σ; Γ ⊢ group e ~> Q : Group τ τ'
```

(AggOfGroup)
A group computation can be converted to an aggregate by constructing a map of all the values.
```
  Σ; Γ ⊢ Q : Group τ τ'
-------------------------
Σ; Γ ⊢ Q : Agg (Map τ τ')
```



(LetVal)
If the definition of a let is a value computation, we add it to just the scalar environment.
```
Σ; Γ ⊢ e : Val τ      Σ, n : τ; Γ ⊢ Q : u τ'
---------------------------------------------------
           Σ; Γ ⊢ let n = e ~> Q : u τ'
```

(LetAgg)
If the definition of a let is an aggregate, it is only be available from aggregate computations.
```
Σ; Γ ⊢ e : Agg τ      Σ; Γ, n : τ ⊢ Q : Agg τ'
----------------------------------------------
        Σ; Γ ⊢ let n = e ~> Q : Agg τ'
```


(SumVal)
The expression to sum on must be a value computation, and the output is an aggregate.
```
  Σ; ∅ ⊢ e : Val Int
----------------------
Σ; Γ ⊢ sum e : Agg Int
```

(SumGroup)
We can also find the sum of a group.
Note that we still clear the aggregate environment, as we don't want groups depending on previous aggregates over the whole thing. Is that right?
```
Σ; ∅ ⊢ e : Group τ Int
----------------------
Σ; Γ ⊢ sum e : Agg Int
```


(MaxVal)
The same as SumVal.
```
  Σ; ∅ ⊢ e : Val Int
----------------------
Σ; Γ ⊢ max e : Agg Int
```

(MaxGroup)
The same as SumGroup.
```
Σ; ∅ ⊢ e : Group τ Int
----------------------
Σ; Γ ⊢ max e : Agg Int
```



(Count)
Count is always available.
```
----------------------
Σ; Γ ⊢ count : Agg Int
```


(Plus)
Plus can work for aggregates or values. It can't really work for groups.
```
Σ; Γ ⊢ e : u Int        Σ; Γ ⊢ e' : u Int      u ∈ Agg  ∨  u ∈ Val
------------------------------------------------------------------
                     Σ; Γ ⊢ e + e' : U Int
```



(ValVar)
Variables are pretty simple; they just look in the environment
```
   v : τ ∈ Σ
-----------------
Σ; Γ ⊢ v : Val τ
```

(AggVar)
```
   v : τ ∈ Γ
-----------------
Σ; Γ ⊢ v : Agg τ
```


Example derivations
-------------------
### Simple sum
```
           value : Int  ∈ {value : Int}
        ------------------------------------  (ValVar)
        value : Int; ∅ ⊢     value : Val Int
        ------------------------------------- (SumVal)
        value : Int; ∅ ⊢ sum value : Agg Int
   ------------------------------------------ (TopLevel)
   Φ ⊢ feature salary ~> sum value : Agg Int
```

### Group by date, count
```
   ---------------------  (ValVar)                 ---------------------- (Count)
   ... ⊢ date : Val Time                           ... ⊢ count : Agg Int
   --------------------------------------------------------------------- (Group)
   value : Int, date : Time; ∅ ⊢ group date ~> count : Group    Time Int
   ---------------------------------------------------------------------- (AggOfGroup)
   value : Int, date : Time; ∅ ⊢ group date ~> count : Agg (Map Time Int)
   ---------------------------------------------------------------------- (TopLevel)
           Φ ⊢ feature salary ~> group date ~> count : Agg (Map Time Int)
```

### Nested group: maximum by days
```
   ---------------------  (ValVar)                 ---------------------- (Count)
   ... ⊢ date : Val Time                           ... ⊢ count : Agg Int
   --------------------------------------------------------------------- (Group)
   value : Int, date : Time; ∅ ⊢ group date ~> count : Group    Time Int
   ---------------------------------------------------------------------- (MaxGroup)
        value : Int, date : Time; ∅ ⊢ max (group date ~> count) : Agg Int
        ----------------------------------------------------------------- (TopLevel)
                Φ ⊢ feature salary ~> max (group date ~> count) : Agg Int
```



Outlaws
-------
### Promotion is outlawed

It is tempting to think that "promotion" from values to aggregates can be done implicitly, but this is not true.
This is not actually desirable because it could return very large arrays.
```
feature salary
~> value
: Agg (Array Int)
```

Auto-promotion could also allow something like
```
feature salary
~> value / sum value
: Agg (Array Int)
```
which requires two passes and a very big array.


Shortcomings / todo
-------------------

### Latest
Latest is very interesting, because by the time we have the latest it's stored in an array.
That means we can do as many passes as we want over it. Joy!
This seems roughly equivalent to ``sum e`` being ``Val Int`` instead of ``Agg Int`` - because then the stage restriction wouldn't kick in.
Maybe we could add a new parameter to the typechecking rules, which is the universe that aggregates are created in.
We'd start that with ``Agg``, and for any latests it would change it to ``Val``.

This isn't quite right: we need to know whether the end result is an aggregate or a value, in order to know whether latest returns an array or not.

Array:
```
feature salary
~> latest 3
~> value / sum value

: Agg (Array Int)
```

Aggregate:
```
feature salary
~> latest 3
~> sum value

: Agg Int
```

### Environments and universes
Separating the environments is troublesome now that we have the Group universe. They should be one environment with each entry having a universe tag.



Questions
---------

- What about nested groups - is there any reason to disallow them?
