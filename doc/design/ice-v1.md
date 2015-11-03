Plan for Ice V1.
---------------

Assumptions
-----------

1. No resumables or bubblegum used
2. All stream input for given customer processed in single run
3. All stream input for given customer fits in memory
4. No runtime string generation functions (stringOfDouble#)


Todo
----

## High

1. Stupid linear implementation of maps: perhaps this can be done during flatten
2. Melt for arrays (Tran)
3. topic/ivory-dates and implementations of date primitives https://github.com/ambiata/icicle/tree/topic/ivory-dates
4. Name shadowing bug https://github.com/ambiata/icicle/issues/166

## Medium

1. C implementation of parsing?
2. Assembly output in repl, C output strip out preamble boilerplate 

## Low

1. topic/window-frames https://github.com/ambiata/icicle/pull/133


