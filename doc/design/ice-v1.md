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


Todo
----

## High

1. Stupid linear implementation of maps: perhaps this can be done during flatten
2. Melt for arrays (Tran)
3. topic/ivory-dates and implementations of date primitives https://github.com/ambiata/icicle/tree/topic/ivory-dates
4. ~~Name shadowing bug https://github.com/ambiata/icicle/issues/166~~

## Medium

1. C implementation of parsing?
2. ~~Assembly output in repl, C output strip out preamble boilerplate~~

## Low

1. topic/window-frames https://github.com/ambiata/icicle/pull/133
2. String generation functions eg concatenation, stringOfDouble?

