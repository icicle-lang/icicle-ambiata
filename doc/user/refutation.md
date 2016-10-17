Refutation of Facts
===================

This document describes how Icicle resovles fact conflicts, e.g. from duplicate facts.

Keys
----

Each fact may be defined with a refutation expression, which acts as a `nubBy` key on the facts stream.

The key values are assumed to be monotonic.

Priorities and Keys
-------------------

Facts are nubbed as first-write-wins, since we assume:

  * Facts with higher priorities will be ingested first.
  * No fact can have the same priority, key, and value(s).

Examples
--------

### No refutation

This is the default. Facts are consumed as-is. Duplicate ingestion will result in duplicate facts.


### Time

```
[fact.keyed_by_time]
  key = "time"
  encoding="int"
  
bulbasaur|keyed_by_time|1|2014-01-01
bulbasaur|keyed_by_time|3|2014-01-01
bulbasaur|keyed_by_time|5|2016-01-01
charmander|keyed_by_time|0|2014-01-01
charmander|keyed_by_time|2|2014-01-01
```

```
> feature keyed_by_time ~> count value
[bulbasaur, 2,charmander, 1]

> feature keyed_by_time ~> newest value
[bulbasaur, 5,charmander, 0]

> feature keyed_by_time ~> group (year_of time) ~> newest value
[bulbasaur, [(2014,1),(2016,5)],charmander, [(2014,0)]]
```

### Expression: `year_of time`

```
[fact.keyed_by_year_of_time]
  key = "year_of time"
  encoding="string"

blatoise|keyed_by_year_of_time|foo14|2014-01-01
blatoise|keyed_by_year_of_time|foo15|2015-01-01
blatoise|keyed_by_year_of_time|foo16|2016-01-01
blatoise|keyed_by_year_of_time|foo160302|2016-03-02
blatoise|keyed_by_year_of_time|foo160307|2016-03-07
```

```
> feature keyed_by_year_of_time ~> count value
[blatoise, 3]

> feature keyed_by_year_of_time ~> newest value
[blatoise, "foo16"]

> feature keyed_by_year_of_time ~> group (year_of time) ~> newest value
[blatoise, [(2014,"foo14"),(2015,"foo15"),(2016,"foo16")]]
```

### Struct Field

```
[fact.keyed_by_field]
  key = "transaction_id"
  encoding="(transaction_id:int,dollarydoos:int)"

zubat|keyed_by_field|{"transaction_id":0,"dollarydoos":2}|2015-01-01
zubat|keyed_by_field|{"transaction_id":3,"dollarydoos":1}|2016-01-01
zubat|keyed_by_field|{"transaction_id":3,"dollarydoos":9}|2016-01-05
```

```
> feature keyed_by_field ~> count dollarydoos
[zubat, 2]

> feature keyed_by_field ~> newest dollarydoos
[zubat, 1]

> feature keyed_by_field ~> group (year_of time) ~> count dollarydoos
[zubat, [(2015,1),(2016,1)]]

> feature keyed_by_field ~> group (year_of time) ~> newest dollarydoos
[zubat, [(2015,2),(2016,1)]]
```

### Struct Field + Time

```
[fact.keyed_by_field_time]
  key = "(transaction_id, time)"
  encoding="(transaction_id:int,dollarydoos:int)"

zubat|keyed_by_field_time|{"transaction_id":5,"dollarydoos":2}|2015-01-01
zubat|keyed_by_field_time|{"transaction_id":5,"dollarydoos":5}|2015-01-01
zubat|keyed_by_field_time|{"transaction_id":5,"dollarydoos":7}|2015-01-03
```

```
> feature keyed_by_field_time ~> count dollarydoos
[zubat, 2]

> feature keyed_by_field_time ~> newest dollarydoos
[zubat, 7]

> feature keyed_by_field_time ~> group (year_of time) ~> newest dollarydoos
[zubat, [(2015,7)]]

> feature keyed_by_field_time ~> group (day_of time) ~> newest dollarydoos
[zubat, [(1,2),(3,7)]]
```
