Plan for Ice V1.
---------------

This is what I would *really* like to get done before my internship officially ends on Friday 20th.
That would be an ice that supports a decent chunk of Icicle, executing fast enough to be useful via the C backend.
It will only work on the whole data set at once, with no ingests.

Assumptions/limitations
-----------

We can ignore a whole bunch of things:

1. No resumables or bubblegum used (as all data is available)
2. All stream input for given customer processed in single run
3. All stream input for given customer fits in memory
4. Nested arrays not supported: if a feature is an array, you cannot do "feature arr ~> latest 5 ~> value :: Array (Array Int)".


Todo
----

## High

1. Melt for arrays (Tran)
2. Melt maps into pair of arrays
    - (should be simple after melt arrays because majority is done in flatten)
    - Converting maps to/from C values
3. Array of strings
    - Converting to/from C values

## Medium

1. C implementation of parsing

## Low

1. topic/window-frames https://github.com/ambiata/icicle/pull/133
2. String generation functions eg concatenation, stringOfDouble?

## Done
1. ~~Stupid linear implementation of maps: perhaps this can be done during flatten~~
2. ~~topic/ivory-dates and implementations of date primitives https://github.com/ambiata/icicle/tree/topic/ivory-dates~~
3. ~~Name shadowing bug https://github.com/ambiata/icicle/issues/166~~
4. ~~Assembly output in repl, C output strip out preamble boilerplate~~
6. ~~seaOfXValue needs to allocate and fill arrays, buffers, maps.~~ (not particularly well tested)

