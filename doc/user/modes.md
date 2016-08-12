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

1. Number of facts: one extra fact is inserted, with the current fact state and the snapshot/chord date.
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

[fact.pokemon_event]
  mode = "event"
  encoding="(age:int*,location:string*)"

```

Input:

```
bulbasaur |pokemon|{"location":"pallet"}|2014-01-01
charmander|pokemon|{"age":1,"location":"pallet"}|2015-01-01
charmander|pokemon|{"age":2}|2016-01-01
charmander|pokemon|{"location":"indigo"}|2016-07-01
```

..and the same but for `pokemon_event`.

Snapshot time: `2016-07-14`.

#### Latest value is the latest state

```
feature pokemon ~> newest fields

bulbasaur|state_newest|{"location":"pallet"}
charmander|state_newest|{"location":"indigo","age":2}

feature pokemon_event ~> newest fields

bulbasaur|event_newest|{"location":"pallet"}
charmander|event_newest|{"location":"indigo"}
```

#### State persists even if there is no fact for that entity in the window

```
feature pokemon ~> windowed 4 weeks ~> newest fields

bulbasaur|state_newest_windowed|{"location":"pallet"}
charmander|state_newest_windowed|{"location":"indigo","age":2}

feature pokemon_event ~> windowed 4 weeks ~> newest fields

charmander|event_newest_windowed|{"location":"indigo"}
```

#### Number of facts

Note that sparse state facts have an extra fact added at the end:

```
feature pokemon ~> count fields

bulbasaur|state_count|2
charmander|state_count|4

feature pokemon_event ~> count fields

bulbasaur|event_count|1
charmander|event_count|3
```

#### Latest fact time

```
feature pokemon ~> newest time

bulbasaur|state_newest_time|2016-07-14T00:00:00Z
charmander|state_newest_time|2016-07-14T00:00:00Z

feature pokemon_event ~> newest time

bulbasaur|event_newest_time|2014-01-01T00:00:00Z
charmander|event_newest_time|2016-07-01T00:00:00Z
```

### Big Data Mode

Here be dragons.

Dense States
------------

As far as Icicle is concerned, this mode behaves the same as `event`. No extra state is kept and facts are treated as single values.
