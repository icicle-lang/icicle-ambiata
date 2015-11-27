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
The first few lines, omitted here, list the internal REPL flags, which can be enabled/disabled with `:set +flag` and `:set -flag`.

```
> :set
...
now:        1970-1-1
data:       18 rows
dictionary: Dictionary
  Functions
    count                : (Num a) => Aggregate a
    mean                 : (Num b) => Element a b -> Aggregate a Double
    newest               : Element a b -> Aggregate a b
    oldest               : Element a b -> Aggregate a b
    sd                   : (Num b) => Element a b -> Aggregate a Double
    sum                  : (Num b) => Element a b -> Aggregate a b
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

If we wanted the newest of both location and severity, we could use parentheses to nest the comma inside the argument to ``newest`` like so:

```
> feature injury ~> newest (location, severity)
- Result:
[homer, ("torso",1)]
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


Windows and dates
-----------------

It is often useful to look at just the last few months of data - this is called windowing.
Before looking at windowing proper, we should look at the dates in the sample data.
Each fact, along with its ``value`` or struct fields, has a ``date`` attached to it.
We can view the first and last dates quite easily:

```
> feature salary ~> oldest date, newest date
- Result:
[homer, (1989-12-17,2010-1-1),marge, (1989-12-17,1989-12-17)]
```

Now that we know the most recent entry was 2010, we can set the current snapshot date.

```
> :set date 2010 1 1
ok, date set to 2010-1-1
```

If we wish to see the number of salary changes between late 2009 and 2010, we could do this:

```
> feature salary ~> windowed 30 days ~> count, sum value
- Result:
[homer, (1,90100),marge, (0,0)]
```

We can also look at changes over the last year, but ignoring any made within the current month:

```
> feature salary ~> windowed between 1 months and 12 months ~> count, sum value
- Result:
[homer, (0,0),marge, (0,0)]
```

It is not possible, however, to look at the changes strictly older than some time: we cannot say "windowed before 3 months".
This omission is not entirely fundamental, but would require significant work to implement efficiently.
It may be added in later versions of Icicle if it is deemed useful or necessary.


Groups and distinction
----------------------

A common task is to bucket facts into different categories.
Suppose we wanted to look at where the majority of injuries occur, and which tend to be the most severe.
We would group by the ``location`` of the injury, then for each ``location`` find the ``count`` and mean with ``mean severity``.

```
> feature injury ~> group location ~> mean severity, count
- Result:
[homer, [("arm",(4.0,1)),("head",(1.5,2)),("torso",(2.0,2))]]
```
We see that perhaps arms are the most painful injuries, but since there is only one arm injury it is quite possible that it is an outlier.

A similar concept is ``distinct``.
It is interesting to compare how many entries there are, versus how many different locations, as well as only those locations with particularly severe injuries.

```
> feature injury
  ~> count
   , (distinct location ~> count)
   , (filter severity > 3 ~> distinct location ~> count)

- Result:
[homer, ((5,3),2)]
```


Custom folds
------------

Sums, counts and means are timeless reductions, but they fall short of particularly interesting analyses.
It is possible to create custom traversals or folds over the stream, which is indeed how sum and count themselves are implemented.

As a simple example, let us reimplement sum as a fold.
(Again, to input this into the repl, you will need to condense it onto one line)

```
> feature salary
  ~> fold
       my_sum = 0
              : my_sum + value
  ~> my_sum

- Result:
[homer, 372100,marge, 1]
```

Here we use the ``let`` syntax as before to give a definition a name, but instead of a simple definition, this is a fold definition.
The name of the fold is ``my_sum`` and its initial value is ``0`` - this is what the sum of an empty stream would produce.
Then we have a colon, ``:``, pronounced "followed by".
After the initial value, each fact in the stream computes a new ``my_sum``, based on the previous ``my_sum`` plus the current fact's ``value``.

The initial value of the fold cannot refer to any element, as it is used even if the stream is empty.
There is another type of fold that only works if the stream is not empty, though, called ``fold1`` - fold on at least one element.
With this, we can implement an exponentially rolling average:

```
> feature salary
  ~> fold1
       roll = double value
            : double value * 0.25 + roll * 0.75
  ~> roll

- Result:
[homer, 75083.59375,marge, 0.0]
```

Here, the initial value of the fold is the value of the first element of the stream.
For any subsequent facts in the stream, a quarter of the value is mixed with three quarters of the previous fold value.

Garnish with a lime twist.

