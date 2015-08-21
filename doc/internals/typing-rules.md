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
   | 0 | 1 | 2...
```

Types
-----
### Base types
The concrete types don't matter so much here, but they exist.

```
τ := Time
   | Int
   | Map τ τ
   | Bool
   | ...
```

### Universes / modalities / effects
Universes describe different kinds of computation, or where the computation occurs.
The idea is pretty similar to the Pre/Stream/Reduce/Post stratification in Core, but here all the computations are mixed up.
So we need to infer the universes before we can separate them out.

```
U :=         τ
   | Elem    τ
   | Group τ τ
   | Agg     τ
```
(See "notes on staging" for references and how this compares.)

- τ: the universe of pure computations
- Elem τ: some scalar element of a stream. scalar computations cannot use aggregations or groups!
- Group τ τ': the result of a grouping, where τ is the key and τ' is the value.
- Agg τ: some aggregate. aggregate computations can depend on elements and groupings.

Group is necessary because we can do aggregates over groups - but we can't do aggregates over aggregates.
They are sort of half-way between aggregates and elements, then.

In the rules below, we use ``C`` to stand for any effectful computation - ``C τ`` wraps or unwraps a type in an effectful computation, whereas ``τ`` alone is a pure computation, and ``u`` is a pure or effectful computation.

### Functions
Functions can only be right-nested - really that means they're not higher order.
Note also that the elements cannot contain functions.
We cannot have an "Elem (a -> b)" or "Map k (a -> b)".
```
F := U
   | U -> F
```

Contexts
--------

- Γ: bindings of names and their universe types. This is just ``U``. There can be no functions in the context.

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
Φ ⊢ n : φ    {Elem t | t in φ}, date : Elem Time ⊢ Q : u τ    u /= Elem
-----------------------------------------------------------------------
                  Φ ⊢ feature n ~> Q : u τ
```

(LatestNotElem)
Getting the aggregate or grouping the latest is fine, and remains in the same universe.
```
Γ ⊢ Q : u τ     u /= Elem
------------------------
Γ ⊢ latest i ~> Q : u τ
```

(LatestElem)
The latest of a value is allowed, and the result is an aggregate array of the values
```
            Γ ⊢ Q : Elem τ
---------------------------------
Γ ⊢ latest i ~> Q : Agg (Array τ)
```

(Filter)
The predicate of a filter cannot mention aggregates - it must be in Elem.
Conversely, the output / rest of query cannot be in Elem - it must be an aggregate or a group.

* I'm not certain that this restriction is necessary; ``sum (filter p ~> x) == filter p ~> sum x`` is reasonable.
```
Γ ⊢ e : Elem Bool    Γ ⊢ Q : u τ   u /= Elem
------------------------------------------
        Γ ⊢ filter e ~> Q : u τ
```


(Group)
The expression we're grouping over must be enum-ish.

* Disallowing nested groups
* What about ``group date ~> date``? Perhaps the group by expression should be available as an aggregate in Q.

```
Γ ⊢ e : Elem τ    Enum τ    Γ ⊢ Q : Agg τ'
-----------------------------------------
    Γ ⊢ group e ~> Q : Group τ τ'
```



(Let)
Lets can have any type and any universe, except for function types.
```
Γ ⊢ e : u        n : u, Γ ⊢ Q : u'
----------------------------------
      Γ ⊢ let n = e ~> Q : u'
```

(Sum)
The expression to sum on must be a value or group computation, and the output is an aggregate.
```
Γ ⊢ e : u Int     u /= Agg
--------------------------
    Γ ⊢ sum e : Agg Int
```


(Count)
Count is always available.
```
-------------------
Γ ⊢ count : Agg Int
```


(Var)
Variables are pretty simple; they just look in the environment
```
    v : u ∈ Γ
-------------
Γ ⊢ v : u
```


(Plus)
Plus works for anything
```
---------------------------
Γ ⊢ (+) : Int -> Int -> Int
```




(App)
Some primitive application with exactly the right type.
If the types don't match, implicit boxing might apply on either side.
```
Γ ⊢ f   : u -> u'     Γ ⊢ e : u
--------------------------------
Γ ⊢ f e :      u'
```


(BoxI)
We can box a pure computation into any effect.
This is equivalent to inserting a ``box`` in lambda box.
```
Γ ⊢ e : τ    wrap C τ => u'
---------------------------
        Γ ⊢ e : u'
```

Implicitly wrapping a type in a computation only succeeds if there is no existing computation.
For function types, this means none of the leaves can have computations.
```
--------------- (WrapT)
wrap C τ => C τ

wrap C a => a'      wrap C b => b'
---------------------------------- (WrapF)
wrap C (a -> b) => a' -> b'
```




Example derivations
-------------------
### Simple sum
```
           value : Elem Int  ∈ {value : Elem Int}
        ------------------------------------  (Var)
        value : Elem Int ⊢     value : Elem Int
        ------------------------------------- (Sum)
        value : Elem Int ⊢ sum value : Agg Int
   ------------------------------------------ (TopLevel)
   Φ ⊢ feature salary  ~> sum value : Agg Int
```

### Group by date, count
```
   ---------------------  (Var)                    ---------------------- (Count)
   ... ⊢ date : Elem Time                           ... ⊢ count : Agg Int
   ---------------------------------------------------------------------  (Group)
   value : Int, date : Time    ⊢ group date ~> count : Group    Time Int
   ---------------------------------------------------------------------- (TopLevel)
           Φ ⊢ feature salary ~> group date ~> count : Group    Time Int
```

### Nested group: maximum by days
```
   ---------------------  (Var)                    ---------------------- (Count)
   ... ⊢ date : Elem Time                           ... ⊢ count : Agg Int
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
This seems roughly equivalent to ``sum e`` being ``Elem Int`` instead of ``Agg Int`` - because then the stage restriction wouldn't kick in.
Maybe we could add a new parameter to the typechecking rules, which is the universe that aggregates are created in.
We'd start that with ``Agg``, and for any latests it would change it to ``Elem``.

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
