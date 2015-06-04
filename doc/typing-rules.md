Typing rules
============

Contexts
--------

- Γ: aΓΓregates: bindings like ``let x = sum y``
- Σ: Σcalars: available things in the current feature and scalar bindings
- Φ: Φeatures, like the types of concrete features

Universes?
----------

- Agg τ: some aggregate. aggregate computations can depend on values/scalars.
- Val τ: some scalar value. scalar computations cannot use aggregations!

Types
-----
- Time
- Int
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


(AggOfVal)
Any "Value" computation can be promoted to an "Aggregate" computation, but not vice versa.
```
Σ; Γ ⊢ e : Val τ
----------------
Σ; Γ ⊢ e : Agg τ
```

(Latest)
Getting the latest forces the computation to be in Agg.
```
      Σ; Γ ⊢ Q : Agg τ
----------------------------
Σ; Γ ⊢ latest i ~> Q : Agg τ
```

(Filter)
The predicate of a filter cannot mention aggregates - it must be in Val.
The output is in Agg.
```
Σ; ∅ ⊢ e : Val Bool        Σ; Γ ⊢ Q : Agg τ
-------------------------------------------
       Σ; Γ ⊢ filter e ~> Q : Agg τ
```




(LetVal)
If the definition of a let is a value computation, we add it to both environments.
```
Σ; Γ ⊢ e : Val τ      Σ, n : τ; Γ, n : τ ⊢ Q : u τ'
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


(Sum)
The expression to sum on must be a value computation, and the output is an aggregate.
```
  Σ; ∅ ⊢ e : Val Int
----------------------
Σ; Γ ⊢ sum e : Agg Int
```

