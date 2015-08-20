Ambling through the Ice
=========================

This document aims to give a brief introduction to the concepts of Icicle, and how to load data and perform simple queries interactively.

Repl
--------

Let us start by opening the Icicle interactive repl, loading a dictionary and some sample data.
In the repl, lines starting with ``--`` are simply comments, while lines starting with ``:`` are commands to the interpreter.
Lines with neither are interpreted as Icicle expressions, as we will see soon.

```
$ dist/build/icicle-repl/icicle-repl
welcome to iREPL

> -- Load the dictionary and some helper functions
> :dictionary data/example/DictionaryTrial.toml
ok, loaded dictionary with 7 features and 6 functions

> -- Load some example data

> :load data/example/demographics.psv
ok, loaded data/example/demographics.psv, 18 rows
```

To find out more information about the newly loaded dictionary, we can use the ``:set`` command.
The first few lines of internal details have been omitted.

```
> :set
...
now:        1970-1-1
data:       18 rows
dictionary: Dictionary
  Functions
    count                = let fold c = 0 : c + 1 ~> c
    mean v               = sum (double v) / count
    newest v             = let fold1 s = v : v ~> s
    oldest v             = let fold1 s = v : s ~> s
    sd v                 = let v_ = double v
                           ~> let sv = sum v_
                           ~> ((sum (v_ * v_) * count - sv * sv) / (count * (count - 1))) ^ 0.5
    sum v                = let fold s = 0 : v + s ~> s
  Features
    salary               : Int
    injury               : Struct [location : String
                                  ,severity : Int
                                  ,optional action : String]
    age                  : Int
    gender               : String
    mean_salary          = feature salary
                           ~> mean value
    salary_standard_deviation
                         = feature salary
                           ~> sd value
    gender               = feature gender
                           ~> newest value
```

First we see the that the date is set to 1970.
This is the date by which windows and whatnots are calculated, but the default date is not particularly useful.

The dictionary includes functions, such as ``count``, which are available to use in queries.

The features in the dictionary are the most important part.
The concrete features are denoted with a ``:``, with their encoding to the right of the colon.
We see that ``salary`` is an integer,  while ``injury`` is a structure.
In Icicle, each query requires a single concrete feature.

Next are the virtual features, along with their definitions.
We see that ``mean_salary`` is defined by ``feature salary ~> mean value``.

Mean queries
------------

We can test the mean_salary feature by copying its definition text into the repl, with no line breaks.

```
> feature salary ~> mean value
- Result:
[homer, 74420.0,marge, 0.0]
```
We see that homer's mean salary is around 75k, while marge's is zero.

If we tried the same query on the next feature, ``injury``, what will happen?

```
> feature injury ~> mean value
                         λλλλ
REPL Error:
Check error:
  Unknown variable value at (line 1, column 24)
  Extra information:
    The available bindings are:  action : Element Definitely (Option String)
                                 ...
```

The nasty error here is because ``injury`` is a structure with fields, rather than just an integer.
Inside queries on simple types such as integers, doubles and strings, we use ``value`` to refer to the sole value.
However, inside structures, we refer to each field by its name (``action``, ``location`` and ``severity`` in this case) or ``fields`` to refer to the entire structure.

Now, if we try to find the mean of severity, we would get:
```
> feature injury ~> mean severity
- Result:
[homer, 2.2]
```

However, finding the mean of location is not so simple: location is a string, but mean only works on numbers such as integers or doubles.
```
> feature injury ~> mean location
                    λλλλ
REPL Error:
Check error:
  Cannot discharge constraints at  (line 1, column 19)
  Constraints: ((line 1, column 19),Not a number: String)
```

The error is not particularly pretty right now, but the important part is on the last line: ``Not a number: String``.


Newest and oldest
-----------------
Other functions don't have the same restrictions on numbers.
Consider ``newest`` and ``oldest`` which simply return the first and last entries.
We can join the result of two aggregates with a comma.
```
> feature injury ~> newest location, oldest severity
- Result:
[homer, ("torso",2)]
```


Filters and contexts
--------------------
Queries can be thought of as piping data through some number of transformers such as filters, then finally collecting the data in an aggregate at the end.
The data must be aggregated at the end, as otherwise we might end up with an unbounded amount of output data.

The source of this data is a single concrete feature.
We can imagine that ``feature salary`` starts reading all entries from the disk.
The data is then passed through with the operator ``~>``, pronounced "flows into".
For example, in the simple query
```
feature salary ~> mean value
```
Here, all data is being read, and passed into the ``mean`` aggregate.
We can introduce new "contexts" between the source and the aggregate, for example if we wanted to find the mean of only positive values:
```
> feature salary ~> filter value > 0 ~> mean value
- Result:
[homer, 74420.0,marge, NaN]
```

Here, the data is filtered before it reaches the aggregate.
We can even split the data feed, by applying different contexts at different places, as long as the different pipelines are joined by aggregates.
For example, if we wished to compute the ratio of positive to negative numbers, we would introduce a context ``filter value > 0`` and ``filter value < 0`` like so:

```
> feature salary ~> (filter value > 0 ~> count), (filter value < 0 ~> count)
- Result:
[homer, (5,0),marge, (0,0)]
```
Of course there are no negative salaries, so a division would cause division by zero, however it is still valid to do so.

```
> feature salary ~> (filter value > 0 ~> count) / (filter value < 0 ~> count)
- Result:
[homer, Infinity,marge, NaN]
```

This gets a bit messy however, and we may wish to introduce intermediate bindings with ``let``.
Here I have split the query across multiple lines for exposition, however for now the repl only handles input queries on a single line.
```
> feature salary
  ~> let pos = (filter value > 0 ~> count)
  ~> let neg = (filter value < 0 ~> count)
  ~> pos / neg

- Result:
[homer, Infinity,marge, NaN]
```


Single-pass restriction
----------------------

What if we wanted to count the number of elements larger than the mean?
In order to find the mean, we need to see all elements in the stream.
To find elements in the stream larger than the mean, we would again need see all elements in the stream.
This would require multiple passes over the data, requiring untold amounts of memory or time.

Instead we introduce a staging restriction on the language, splitting into two stages:
- Element computations, based on a single stream value
- Aggregate computations, which are available after all stream elements have been read.
The restriction is that Element computations cannot refer to Aggregate computations, as Aggregate computations are only available at the end of the stream.

```
> feature salary ~> filter double value > mean value ~> count
                                        λλλλ
REPL Error:
Check error:
  Cannot discharge constraints at  (line 1, column 39)
  Constraints: ((line 1, column 39),Cannot unify Aggregate = Element)
```
Again, the last line is the most important, as well as the position of the error.
This means that the two arguments to ``>`` are at different stages, the left being Element and the right Aggregate.

The ``double value`` here is required because ``mean value`` returns a Double, while ``value`` itself is an Int.
In order to compare the two, they must be explicitly cast to the same type.
It would also be fine to convert the other argument to an Int, as in ``filter value > int (mean value)``.


The rest is TODO
================


CountBy
--------------

```
> feature injury ~> group location ~> mean severity
- Result:
[homer, [("arm",4.0),("head",1.5),("torso",2.0)]]
```



CountDays
---------

```
> feature salary ~> distinct date ~> count
- Result:
[homer, 5,marge, 1]
```


CountUnique
-----------

```
> feature injury ~> distinct location ~> count
- Result:
[homer, 3]
```

