Fact Modes
==========

Fact modes only affect input. Compute semantics are the same regardless of the fact mode for the current attribute.

The following fact modes are supported:

1. Event
2. Sparse States
3. Dense States

Event
-----

This is the default fact mode. Each fact is ingested as a single value.

Sparse States
-------------

Facts are treated as state updates. A running state is kept and updated with each new fact.

### Notes

1. Number of facts: one extra fact with the current fact state is inserted with the snapshot/chord date.
2. Start of window: at the start of a window, the fact state is the same as the last update before the window.
3. End of window: at the end of the window, if there is no state update (no fact) in the window, the fact state still has the value it did at the start of the window.

### Small Data Mode

Fact state starts from the beginning of time.

#### Examples

With fact:

```
[fact.pokemon]
  mode = "sparse_state"
  encoding="(age:int*,location:string*)"
```

Input:

```
bulbasaur|pokemon|{"location":"pallet"}|2014-01-01
charmander|pokemon|{"age":1,"location":"pallet"}|2015-01-01
charmander|pokemon|{"age":2}|2016-01-01
charmander|pokemon|{"location":"indigo"}|2016-07-01
```

Snapshot time: `2016-07-14`

#### Latest value is the latest state

```
feature pokemon ~> newest fields

bulbasaur|foo|{"location":"pallet"}
charmander|foo|{"location":"indigo","age":2}
```

#### State persists even if there is no fact for that entity in the window

```
feature pokemon ~> windowed 4 weeks ~> newest fields

bulbasaur|bar|{"location":"pallet"}
charmander|bar|{"location":"indigo","age":2}
```

### Big Data Mode

Here be dragons.

Dense States
------------

As far as Icicle is concerned, this mode behaves the same as `event`. No extra state is kept and facts are treated as single values.
