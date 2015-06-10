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
   | count
   | E + E
```

Types
-----
The concrete types don't matter so much here.

- Time
- Int
- Enum?
- Map τ τ'
- Bool
- and so on

Universes
----------
Universes describe different kinds of computation, or where the computation occurs.
The idea is pretty similar to the Pre/Stream/Reduce/Post stratification in Core, but here all the computations are mixed up.
So we need to infer the universes before we can separate them out.

(See "notes on staging" for references and how this compares.)

- Val τ: some scalar value. scalar computations cannot use aggregations or groups!
- Group τ τ': the result of a grouping, where τ is the key and τ' is the value.
- Agg τ: some aggregate. aggregate computations can depend on values/scalars and groupings.

Group is necessary because we can do aggregates over groups - but we can't do aggregates over aggregates.
They are sort of half-way between aggregates and values, then.

In the rules below, the variable ``u`` can be assumed to be bound by a forall over universes, or ``u τ`` where ``u`` is ``Val`` or ``Agg`` or whatever.

Contexts
--------

- Γ: bindings of names and their universes and types.

- Φ: Φeatures, a scalar environment for each concrete feature.
    * A scalar feature like salary would just have ``value : Int``
    * A struct might have ``plan : PlanId, usage : Double`` or something.
    * I guess a pair (a,b) would have ``fst : a, snd : b``
    * This is only used at the top level, to look up the feature



Judgments
---------

(Toplevel)
Look up a concrete feature and use it. There can only be one of these in a program.
The result has to be an aggregate or a group - if it were a value it could return an unboundedly large result.
```
Φ ⊢ n : φ    {Val t | t in φ} date : Val Time ⊢ Q : u τ   u /= Val
----------------------------------------------------------------
             Φ ⊢ feature n ~> Q : u τ
```


(LatestNotVal)
Getting the aggregate or grouping the latest is fine, and remains in the same universe.
```
Γ ⊢ Q : u τ     u /= Val
------------------------
Γ ⊢ latest i ~> Q : u τ
```

(LatestVal)
The latest of a value is allowed, and the result is an aggregate array of the values
```
            Γ ⊢ Q : Val τ
---------------------------------
Γ ⊢ latest i ~> Q : Agg (Array τ)
```

(Filter)
The predicate of a filter cannot mention aggregates - it must be in Val.
Conversely, the output / rest of query cannot be in Val - it must be an aggregate or a group.

* I'm not certain that this restriction is necessary; ``sum (filter p ~> x) == filter p ~> sum x`` is reasonable.
```
Γ ⊢ e : Val Bool    Γ ⊢ Q : u τ   u /= Val
------------------------------------------
        Γ ⊢ filter e ~> Q : u τ
```


(Group)
The expression we're grouping over must be enum-ish.

* Disallowing nested groups
* What about ``group date ~> date``? Perhaps the group by expression should be available as an aggregate in Q.

```
Γ ⊢ e : Val τ    Enum τ    Γ ⊢ Q : Agg τ'
-----------------------------------------
    Γ ⊢ group e ~> Q : Group τ τ'
```



(Let)
If the definition of a let is a value computation, we add it to just the scalar environment.
```
Γ ⊢ e : u τ      n : u τ, Γ ⊢ Q : u' τ'
---------------------------------------
      Γ ⊢ let n = e ~> Q : u' τ'
```

(Sum)
The expression to sum on must be a value or group computation, and the output is an aggregate.
```
Γ ⊢ e : u Int     u /= Agg
--------------------------
    Γ ⊢ sum e : u Int
```


(Count)
Count is always available.
```
-------------------
Γ ⊢ count : Agg Int
```


(Plus)
Plus can work for aggregates or values. It can't really work for groups.
```
Γ ⊢ e : u Int        Γ ⊢ e' : u Int      u /= Group
---------------------------------------------------
                 Γ ⊢ e + e' : u Int
```



(Var)
Variables are pretty simple; they just look in the environment
```
   v : u τ ∈ Γ
-----------------
Γ ⊢ v : u τ
```


Example derivations
-------------------
### Simple sum
```
           value : Val Int  ∈ {value : Val Int}
        ------------------------------------  (Var)
        value : Val Int ⊢     value : Val Int
        ------------------------------------- (Sum)
        value : Val Int ⊢ sum value : Agg Int
   ------------------------------------------ (TopLevel)
   Φ ⊢ feature salary  ~> sum value : Agg Int
```

### Group by date, count
```
   ---------------------  (Var)                    ---------------------- (Count)
   ... ⊢ date : Val Time                           ... ⊢ count : Agg Int
   ---------------------------------------------------------------------  (Group)
   value : Int, date : Time    ⊢ group date ~> count : Group    Time Int
   ---------------------------------------------------------------------- (TopLevel)
           Φ ⊢ feature salary ~> group date ~> count : Group    Time Int
```

### Nested group: maximum by days
```
   ---------------------  (Var)                    ---------------------- (Count)
   ... ⊢ date : Val Time                           ... ⊢ count : Agg Int
   ---------------------------------------------------------------------  (Group)
   value : Int, date : Time    ⊢ group date ~> count : Group    Time Int
   ---------------------------------------------------------------------- (Max)
        value : Int, date : Time    ⊢ max (group date ~> count) : Agg Int
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

Questions
---------

- What about nested groups - is there any reason to disallow them?


Notes on staging
----------------

This is probably closest to "lambda circle" in the MetaML paper.

I believe the fundamental difference between our problem (values can't depend on aggregates)
and the staging problem (compile time values can't depend on runtime values)
is that in the staging problem, the same operations are available at every level.
In our case, there are a lot of aggregate operations that are only available on the aggregate level.

If we ignore the group universe for now, "aggregate" corresponds to the "runtime" stage, while "values" correspond to compile time.
In lambda circle, this would mean that a type of "aggregate A" becomes "circle A".
In that case, "sum" and so on are like "prev", having type "A -> circle A".
Since there is no way to evaluate circles (as they correspond to open expressions) this means the worker for sum cannot evaluate aggregates.

References:
- "Staged Computation with Staged Lexical Scope" by Morten Rhiger, 2012.
- "Staged Computation with Names and Necessity" by Aleksander Nanevski and Frank Pfenning, 1996; 
- "MetaML and multi-stage programming with explicit annotations" by Walid Taha and Tim Sheard, 1997;
