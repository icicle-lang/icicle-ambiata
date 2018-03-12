Ambling through the Ice
=========================

This document aims to give a brief introduction to the concepts of Icicle, and how to load data and perform simple queries interactively.

Repl
--------

Let us start by opening the Icicle interactive repl, loading a dictionary and some sample data.
In the repl, lines starting with ``--`` are simply comments, while lines starting with ``:`` are commands to the interpreter.
Lines with neither are interpreted as Icicle expressions, as we will see soon.

```
$ dist/build/icicle/icicle repl

  ██▓ ▄████▄   ██▓ ▄████▄   ██▓    ▓█████
 ▓██▒▒██▀ ▀█  ▓██▒▒██▀ ▀█  ▓██▒    ▓█   ▀
 ▒██▒▒▓█    ▄ ▒██▒▒▓█    ▄ ▒██░    ▒███
 ░██░▒▓▓▄ ▄██▒░██░▒▓▓▄ ▄██▒▒██░    ▒▓█  ▄
 ░██░▒ ▓███▀ ░░██░▒ ▓███▀ ░░██████▒░▒████▒
 ░▓  ░ ░▒ ▒  ░░▓  ░ ░▒ ▒  ░░ ▒░▓  ░░░ ▒░ ░
  ▒ ░  ░  ▒    ▒ ░  ░  ▒   ░ ░ ▒  ░ ░ ░  ░
  ▒ ░░         ▒ ░░          ░ ░ REPL ░
  ░  ░ ░       ░  ░ ░          ░  ░   ░  ░
     ░            ░
                  ░     :help for help

λ -- Load the dictionary and some helper functions
λ :load data/example/DictionaryTrial.toml
Loaded dictionary with 4 inputs, 4 outputs, 41 functions.

λ -- Load some example data
λ :load data/example/demographics.psv
Selected psv file as input: data/example/demographics.psv
```

To find out more information about the newly loaded dictionary,
we can use the ``:dictionary`` command. 
```
λ :set
Dictionary
----------

## Functions

  log : Double -> Possibly Double
  exp : Double -> Possibly Double
  sqrt : Double -> Possibly Double
  abs : Num a => a -> a
  ...

## Inputs

  demographics:age : Int
  demographics:gender : String
  demographics:salary : Int
  injuries:injury :
    { required location : String
    , required severity : Int
    , optional action : String
    , optional admitted : Time
    }

## Outputs

  demographics:gender =
    feature "gender"  ~> newest value

  demographics:is_head =
    feature "injury" ~> newest (location == "head")

  demographics:mean_salary =
    feature "salary" ~> mean value

  demographics:salary_standard_deviation =
    feature "salary" ~> sd value
```

The dictionary includes functions, such as `log` and `count`,
which are available to use in queries.

The facts and features in the dictionary are the most
important part. The fact definitions (or inputs), display
their encoding to the right of the colon. We see that `salary`
is an integer,  while `injury` is a structure. In Icicle,
each query operates on a single stream of facts.

Next are the features, along with their definitions. We see
that `mean_salary` is defined by `feature salary ~> mean value`.

To find out about the environment of the icicle repl, we can use
the `:set` command. Which will tell us which optimisations are
on, and which evaluation strategies will be used. When getting
to know icicle, using commands like `:set +annotated`, to turn
on the printing of a type annotated query, and `:set +core` to
see the core for each query, can be very handy.

Mean queries
------------

We can test the mean_salary feature by copying its definition text into the repl, with no line breaks.

```
λ feature salary ~> mean value
C evaluation
------------

homer|62016.83333333333
marge|0.0
```
We see that homer's mean salary is around 62k, while marge's is zero.

If we tried the same query on the next feature, ``injury``, what will happen?

```
λ feature injury ~> mean value
                         ^
Error
-----

## Check error

  Unknown variable value at 1:24
```

The nasty error here is because `injury` is a structure with fields,
rather than just an integer. Inside queries on simple types such as
integers, doubles and strings, we use `value` to refer to the sole
value. However, inside structures, we refer to each field by its name
(`action`, `location` and `severity` in this case) or `fields`
to refer to the entire structure.

Now, if we try to find the mean of severity, we would get:

```
λ feature injury ~> mean severity
C evaluation
------------

homer|2.2
```

However, finding the mean of location is not so simple: location is a
string, but mean only works on numbers such as integers or doubles.

```
λ feature injury ~> mean location
                    ^
Error
-----

## Check error

  Cannot discharge constraints at 1:19

    1:19  Not a number: String
          Chances are you tried to apply some numerical computation like (+) or sum to the wrong field.
```

The important part us on the last line: `Not a number: String`.


Newest and oldest
-----------------
Other functions don't have the same restrictions on numbers.
Consider ``newest`` and ``oldest`` which simply return the first and last entries.
We can join the result of two aggregates with a comma.

```
λ feature injury ~> newest location, oldest severity
C evaluation
------------

homer|("torso", 2)
```

If we wanted the newest of both location and severity, we
could use parentheses to nest the comma inside the argument
to `newest` like so:

```
λ feature injury ~> newest (location, severity)
C evaluation
------------

homer|("torso", 1)
```


Filters and contexts
--------------------

Queries can be thought of as piping data through some number of
transformers such as filters, then finally collecting the data
in an aggregate at the end. The data must be aggregated at the
end, as otherwise we might end up with an unbounded amount of
output data.

The source of this data is a single concrete feature. We can
imagine that `feature salary` starts reading all entries from
the disk. The data is then passed through with the operator `~>`,
pronounced "flows into". For example, in the simple query

```
feature salary ~> mean value
```
Here, all data is being read, and passed into the `mean` aggregation.
We can introduce new "contexts" between the source and the aggregate,
for example if we wanted to find the mean of only positive values:

```
λ feature salary ~> filter value > 0 ~> mean value
C evaluation
------------

homer|62016.83333333333
marge|tombstone
```

Here, the data is filtered before it reaches the aggregation. We can
even split the data feed, by applying different contexts at different
places, as long as the different pipelines are joined by aggregates.
For example, if we wished to compute the ratio of positive to negative
numbers, we would introduce a context `filter value > 0` and
`filter value < 0` like so:

```
λ feature salary ~> (filter value > 0 ~> count value), (filter value < 0 ~> count value)
C evaluation
------------

homer|(6, 0)
marge|(0, 0)
```
Of course there are no negative salaries, so a division would cause
division by zero, however it is still valid to do so.

```
λ feature salary ~> (filter value > 0 ~> count) / (filter value < 0 ~> count)
C evaluation
------------

homer|tombstone
marge|tombstone
```

This gets a bit messy however, and we may wish to introduce intermediate
bindings with `let`. Here I have split the query across multiple lines
for exposition, however for now the repl only handles input queries on
a single line.

```
λ feature salary
  ~> let pos = (filter value > 0 ~> count value)
  ~> let neg = (filter value < 0 ~> count value)
  ~> pos / neg
```

For complex queries, `let` binding become invaluable, as they allow
us to document the code and simplify our expressions.

Single-pass restriction
----------------------

What if we wanted to count the number of elements larger than the mean?
In order to find the mean, we need to see all elements in the stream.
To find elements in the stream larger than the mean, we would again
need see all elements in the stream. This would require multiple passes
over the data, requiring untold amounts of memory or time.

Instead we introduce a staging restriction on the language, splitting
into two stages:
- Element computations, based on a single stream value
- Aggregate computations, which are available after all stream elements have been read.

The restriction is that Element computations cannot refer to Aggregate
computations, as Aggregate computations are only available at the end
of the stream.

```
λ feature salary ~> filter double value > mean value ~> count value
                                        ^
Error
-----

## Check error

  Cannot discharge constraints at 1:39

    1:39  Cannot join temporalities.
          Aggregate with Element
```

Again, the last lines are the most important, as well as the position
of the error. This means that the two arguments to `>` are at different
stages, the left being Element and the right Aggregate.

Windows and dates
-----------------

It is often useful to look at just the last few months of data - this
is called windowing. Before looking at windowing proper, we should
look at the dates in the sample data. Each fact, along with its `value`
or struct fields, has a `time` attached to it. We can view the first
and last dates quite easily:

```
λ feature salary ~> oldest time, newest time
C evaluation
------------

homer|(1989-12-17, 2010-01-01)
marge|(1989-12-17, 1989-12-17)
```

Now that we know the most recent entry was 2010, we can set the current snapshot date.

```
λ :set snapshot 2010-01-01
Snapshot mode activated with a snapshot date of 2010-01-01.
```

If we wish to see the number of salary changes between late 2009 and 2010, we could do this:

```
λ feature salary ~> windowed 30 days ~> count, sum value
C evaluation
------------

homer|(1, 90100)
marge|(0, 0)
```

Groups and distinction
----------------------

A common task is to bucket facts into different categories. Suppose we
wanted to look at where the majority of injuries occur, and which tend
to be the most severe. We would group by the `location` of the injury,
then for each `location` find the `count` and mean with `mean severity`.

```
λ feature injury ~> group location ~> mean severity, count
C evaluation
------------

homer|[ ("arm", (4.0, 1))
      , ("head", (1.5, 2))
      , ("torso", (2.0, 2))
      ]
```

We see that perhaps arms are the most painful injuries, but since there
is only one arm injury it is quite possible that it is an outlier.

A similar concept is `distinct`. It is interesting to compare how many
entries there are, versus how many different locations, as well as only
those locations with particularly severe injuries.

```
λ feature injury ~>
    count location ,
    (distinct location ~> count location) ,
    (filter severity > 3 ~> distinct location ~> count location)

C evaluation
------------

homer|((5, 3), 1)
```

It is worth understanding that distinct permits only one value through
a fold, so operating on multiple fields of a structure within a `distict`
is unadvised.

Group Folds
-----------

Finally, we permit folding over the results of a group, so if we wanted
to know which `location` has the highest rate of injury, we can do so
with

```
λ feature injury ~> group fold (loc,sev) = (group location ~> mean severity) ~> max_by sev loc
C evaluation
------------

homer|"arm"
```

Custom folds
------------

Sums, counts and means are timeless reductions, but they fall short of
particularly interesting analyses. It is possible to create custom
traversals or folds over the stream, which is indeed how sum and count
themselves are implemented. As a simple example, let us reïmplement sum
as a fold. (Again, to input this into the repl, you will need to condense
it onto one line)

```
> feature salary
  ~> fold
       my_sum = 0
              : my_sum + value
  ~> my_sum

- Result:
[homer, 372100,marge, 1]
```

Here we use the `let` syntax as before to give a definition a name, but
instead of a simple definition, this is a fold definition. The name of
the fold is `my_sum` and its initial value is `0` - this is what the
sum of an empty stream would produce. Then we have a colon, `:`,
pronounced "followed by".

After the initial value, each fact in the stream computes a new `my_sum`,
based on the previous `my_sum` plus the current fact's `value`.

The initial value of the fold cannot refer to any element, as it is used
even if the stream is empty. There is another type of fold that only works
if the stream is not empty, though, called `fold1` - fold on at least one
element. With this, we can implement an exponentially rolling average:

```
> feature salary
  ~> fold1
       roll = double value
            : double value * 0.25 + roll * 0.75
  ~> roll

- Result:
[homer, 75083.59375,marge, 0.0]
```

Here, the initial value of the fold is the value of the first element of
the stream. For any subsequent facts in the stream, a quarter of the
value is mixed with three quarters of the previous fold value.

Garnish with a lime twist.

