Icicle: Source language
========================

Rough high-level design of the Source language:

* Restrict to single pass over input
    - We get this almost "by definition" by the conversion to Core

* Type safe
    - If we start running a query, we don't want it to fail twelve hours later

* Type inference
    - No annotations necessary

* Polymorphism
    - (+) works for Int, Double
    - Less than works for Int, Double, dates
    - ``newest`` works for anything

* Track history (Bubblegum)
    - Keep track of the data necessary to continue this computation
    - For example the facts used to compute a windowed sum
    - Or the running sum, for an unwindowed sum
    - Ideally the "smallest feasible" amount of data


