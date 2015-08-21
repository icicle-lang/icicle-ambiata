Stages of the Pipeline
====================

This document describes the journey a query must go through before it can be executed.

Icicle contains three distinct languages, at different levels of abstraction;

- The user writes programs in the Source language (Icicle) where initial typechecking and inference is performed;

- Source programs are converted to Core, which is similar to a query execution plan;

- Next, Core programs are converted to Avalanche, an imperative language with just loops;

- Avalanche can be pretty-printed to Java, C, or whatnot.


Source (Icicle)
---------------
Source is the first language, and the only one that should be visible to the user.
It aims to offer relatively high abstraction, full type inference, and type safety.
No recursion or loops of any kind are offered, so this is non-Turing complete, total language.
This is probably a _good thing_, because queries are expected to terminate.

An example Source query:
```
feature salary
 ~> filter value > 5
 ~> sum value
```

The main concept of Source is of piping data through transformers into aggregates.
The ``~>`` operator, pronounced "flows into", is used for directing data.
Data can be split and directed into different pipes, but cannot be joined except as scalar aggregate operations.

Source also allows simple, first-order, non-recursive functions to be defined.
All function definitions are simply inlined before conversion to Core.

An example function definition:
```
sum values
 = let fold
    sum = 0
        : sum + values
 ~> sum
```


The stages are:

- Parsing;

- Typechecking and inference;

- Inlining functions;

- Typechecking again and attaching annotations;

- Conversion to Core.

Note that typechecking must be performed again after inlining, as the conversion to Core uses concrete types on the attached type annotations.




Core
-------------

Core lines up roughly with query execution plans in traditional DBMSes.
(In contrast with DBMSes, the conversion from Source to Core does not make use statistical knowledge of data.)

Core programs are split into four discrete stage, each performed strictly after the next:
- Precomputations;
- Stream transformers;
- Reductions over streams;
- Postcomputations and outputs.

This strict separation of stages ensures that all computations require only a single pass over the data.

For simplicity, sanity, and convenience of transforms, Core programs require explicit type annotations.

An example Core program with type annotations removed
```
Program (source : Stream Int)
Precomputations:

Stream transformers:
    input    = source
    filtered = sfilter (\inp : (Int, DateTime). fst inp > 5)
                input
Reductions:
    sum      = rfold (\sum : Int. \inp : (Int,DateTime). sum + fst inp)
                0
                filtered
Postcomputations:

Returning:
    sum
```

The stages of Core are:
- Typechecking for sanity;
- Merging Core programs on same concrete feature together (Fusion);
- Removing duplicate stream transformers and reductions (Condense);
- Partial evaluation and small optimisations (Simplifier);
- Conversion to Avalanche.

Avalanche language
---------------

Avalanche is the lowest level of abstraction, offering loops, mutable variables, and so on.
It might line up somewhat with the VMs of traditional DBMSes, but I am not certain.

An example flattened Avalanche program with type annotations removed:
```
snapshot_date = DATE
{
  init sum_acc = 0 : Int (Mutable);
  load_resumable sum_acc;
  for_facts (fact_value : Int, fact_date : Date) in new {
    if (gt# fact_value (5 : Int)) {
      read sum = sum_acc;
      write sum_acc = fact_value + sum;
    }
  }

  save_resumable sum_acc;
  read sum = sum_acc;
  return sum;
}
```

The stages are:
- Typechecking for sanity;
- Converting folds, cases to loops and top-level ifs (Flatten);
- Ripping out arrays of pairs to pairs of arrays, pairs of variables to multiple variables (Melt);
- Projections of constructors, eg "fst (pair a b)" more or less case of constructor (Constructor);
- Pretty printing to Java or C.

