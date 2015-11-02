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

1. Conversion of strings to/from C
2. Special SaveResumable even in windowed
3. "Garbage collection" or freeing of result bubblegum and inputs
4. Stupid linear implementation of maps
...
999. C implementation of parsing



Garbage collection of Maps
=======

Maps don't have to appear in output:

```haskell
group fold (k,v) = (group key ~> minimum)
~> maximum v
```
this would end up as outputting a single integer, but the bubblegum resumable would be the minimum map.

Can just garbage collect over resumables.

Suggested Invariant: anything that is dynamically allocated must end up as a resumable

This doesn't necessarily work for dynamically allocated strings, because:
```
let x = location ++ show blah
~> newest x
```
this would allocate a new string for each fact, but only the last one is attached to the bubblegum.
It works for maps though.



Not all things become resumables; if it is a windowed computation, they will not be saved as resumables.
However, we can just mark them as special resumables that are saved but not loaded.
This isn't what I want to do in the long term, but in the short term it's an incredibly simple hack that works.

```
windowed 5 days
~> group key ~> minimum value
```
Because the group is inside the window, the generated Map does not become a resumable, so it would not end up in the state.

