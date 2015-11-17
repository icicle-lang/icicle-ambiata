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
3. All stream input for given customer fits in memory (actually we do this correctly now)
4. Nested arrays not supported: if a feature is an array, you cannot do "feature arr ~> latest 5 ~> value :: Array (Array Int)".
5. Chords are not supported, but we're fast so why not run 100 snapshots


Todo
----

## High
1. PSV output for:
    - Maps, arrays, pairs?
2. PSV input for arrays of primitive types
    - Strings, Ints?
    - Can we have arrays inside structs, or structs inside arrays?
3. PSV missing output for last item

## Medium

1. Support dense PSV output
2. Hide chord command line options in Ice, & remove from docs so people don't think it's supported

## Low

1. Nicer error messages, particularly constraint stuff
    - I think we could modify typechecker to keep context/path of where current expression is, as well as each constraint keeping the expression it was required by, and print some of that when error?
2. It could still be faster
3. 

## Super, super low
1. topic/window-frames https://github.com/ambiata/icicle/pull/133
2. String generation functions eg concatenation, stringOfDouble?


## Done
1. ~~Stupid linear implementation of maps: perhaps this can be done during flatten~~
2. ~~topic/ivory-dates and implementations of date primitives https://github.com/ambiata/icicle/tree/topic/ivory-dates~~
3. ~~Name shadowing bug https://github.com/ambiata/icicle/issues/166~~
4. ~~Assembly output in repl, C output strip out preamble boilerplate~~
6. ~~seaOfXValue needs to allocate and fill arrays, buffers, maps.~~ (not particularly well tested)
7. ~~Melt for arrays~~
8. ~~Melt maps into pair of arrays~~
9. ~~Array of strings to/from C repl eval (not PSV)~~
10. ~~C implementation of parsing~~

