Icicle concrete syntax
======================

Sum
---

Core:
```
Program
Stream
    feat = Source
Reduction
    sum  = Fold (+) 0 feat
Return
    sum
```


Haskell:
(Whatever - this would be defined in Prelude anyway.)
```
sum = fold (+) 0

sum feat
```

SQL:
```
SELECT SUM(value) FROM feat
```

R:
```
sum(feat)
```

LINQ:
```
int Sum(this IEnumerable<int> inp) {
    return inp.Reduce((x,y) => x+y, 0);
}

feat.Sum()
```

Lucid:
```
sum
 where
  sum = 0 fby sum + feat
 end
```


RDO:
```
&RDB& for f in feat
&RDB& get
&RDB&   sum_total = sum_total + f.value;
&RBD& end_get
```

C:
```
int sum = 0;
for (V* cur = feat; ix != feat_end; ++ix) {
    sum += ix->value;
}
```


Count
-----

Core:
```
Program
Stream
    feat = Source
    ones = Map (\x -> 1) feat
Reduction
    count= Fold (+) 0 ones
Return
    count
```

Haskell
```
count = sum . map (const 1)

count feat
```

SQL:
```
select count(*) from feat
```

R:
```
length(feat)

Reduce(function(x,y) { x + 1 }, feat, 0)
```

LINQ:
```
int Count(this IEnumerable<int> inp) {
    return inp.Reduce((x,y) => x+1, 0);
}

feat.Count()
```


Lucid - I have no idea actually.
```
count
 where
  incs  = 0 fby incs + 1
 end
```

RDO:
```
&RDB& for f in feat
&RDB& get
&RDB&   sum_total = sum_total + 1;
&RBD& end_get
```

C:
```
int count = 0;
for (V* cur = feat; ix != feat_end; ++ix) {
    count += 1;
}
```



Windowed count
--------------

Core:
```
Program
Stream
    feat = SourceWindowed 30
    ones = Map (\x -> 1) feat
Reduction
    count= Fold (+) 0 ones
Return
    count
```

Haskell
```
window i
 = filter (\e -> diff unsafeDateNow (dateOf e) <= i)

count $ window 30 feat
```

SQL:
```
select count(*) from feat
where (now() - date <= 30)
```

R
```
count( filter ... ) ?
```

LINQ:
```
feat.Where(x => x.date - now <= 30)
    .Count();
```

C:
```
int count = 0;
for (V* cur = feat; ix != feat_end; ++ix) {
    if (cur->days <= 30) {
        count += 1;
    }
}
```

CountBy
-------

Core:
```
Program
Stream
    feat    = Source
Reduction
    counts  = Fold (\map v -> Map.insertWith (+1) 1 v map) Map.empty feat
Return
    counts
```

Haskell
```
length . group
```

SQL
```
select cat, count(*) from feat group by cat

OR???

select
    cats.cat,
    (select count(*) from feat f where f.cat = cats.cat) as count
from
    (select distinct cat from feat) cats
 
```

LINQ
```
feat.GroupBy(x => x.cat)
    .Select(c => c.Count())
```

C
```
// assuming cat is a small int
int cats[CAT_SIZE];
for (int i = 0; i != CAT_SIZE; ++i) {
    cats[i] = 0;
}

for (V* v = feat; v != feat_end; ++v) {
    cats[v->cat]++;
}
```

CountDays
--------

Core:
```
Program
Stream
    feat    = Source
    dates   = Map dateOf feat
Reduction
    uniques = Fold
                (\ds v ->
                    -- If last seen date is same as this date, don't update
                    if   Just v == fst ds
                    then ds
                    -- Otherwise update last seen date and increment
                    else (Just v, snd ds + 1) )

                -- Start with no last seen date
                (Nothing, 0) 
                dates
Return
    snd uniques
```


Haskell
```
count $ groupBy ((==) `on` dateOf) feat
```

SQL
```
SELECT COUNT(day) AS c FROM feat;
```

LINQ
```
feat.GroupBy(x => x.date)
    .Count()
```

C:
```
int   seen = 0;
date* last = NULL;

for (V* v = feat; v != feat_end; ++v) {
    if (last == NULL || *last != v->date) {
        seen++;
    }
    last = &(v->date);
}

return seen;
```

MaxDays
--------

Core: (just pretend we have triples)
```
Program
Stream
    feat    = Source
    dates   = Map dateOf feat
Reduction
    uniques = Fold
                (\(date, today, maxum) v ->
                    -- If last seen date is same as this date, increment num on this day
                    if   Just v == date
                    then (date, today + 1, max maxum (today + 1))
                    -- Otherwise we're on a new day, so reset today to 0 but keep maxum
                    else (Just v, 0, maxum) )

                -- Start with no last seen date and no max
                (Nothing, 0, 0) 
                dates
Return
    fst uniques
```


Haskell
```
maximum $ groupBy ((==) `on` dateOf) feat
```

SQL
```
SELECT MAX(z.c) FROM (SELECT COUNT(*) AS c FROM feat GROUP BY day) z;
```

LINQ
```
feat.GroupBy(x => x.date)
    .Max()
```

C:
```
int   seen = 0;
int   maxs = 0;
date* last = NULL;

for (V* v = feat; v != feat_end; ++v) {
    if (last == NULL || *last != v->date) {
        seen++;
        if (seen > maxs) {
            maxs = seen;
        }
    }
    last = &(v->date);
}

return seen;
```

DaysSinceEarliest
-----------------

```
Program
Reduction
    first   = Fold (\a v -> if a == Nothing then Just v else a)
                    Nothing
                    Source
Postcomputation with now : Date
    since   = map (\f -> daysOf (now - f)) first
Returning
    since
```


Haskell
```
safeHead $ map (diff unsafeDateNow . dateOf) feat
```

SQL
```
SELECT TOP 1
    now() - day
FROM
    day
ORDERED BY day ASC
```

LINQ
```
feat.Select(x => now() - x.date)
    .Head()
```

C
```
int days = 666;
if (feat != feat_end) {
    days = now() - feat->day;
}
return days;
```


FilteredOverTotal
----------------

Core:
```
Program
Stream
    feat    = Source
    filt    = Filter (>0) feat
Reduction
    cfeat   = Fold (+) 0 feat
    cfilt   = Fold (+) 0 filt
Return
    cfeat / cfilt
```


Haskell
```
count (filter (>0) feat) `div` count feat
```

SQL
```
SELECT
  filt.c / total.c
FROM
  (SELECT COUNT(*) as c FROM source WHERE value > 0) filt,
  (SELECT COUNT(*) as c FROM source) total;

or faster (maybe)

SELECT
    COUNT(case when val <= 0 then null else val end),
    COUNT(*), 
    COUNT(case when val <= 0 then null else val end) / COUNT(*)
FROM
    source;
```

LINQ
```
feat.Filter(x => x > 0).Count() / feat.Count()
```

C
```
int filtd;
int count;
for (V* v = feat; v != feat_end; ++v) {
    count++;
    if (v->value > 0) {
        filtd++;
    }
}
return filtd / count;
```

More
----

More examples to do, from Aaron:

 - Value from month before
 - Average from 3 months before
 - Average of all months
 - standard deviation over all months
 - relative standard deviation (S.d. divided by mean)
 - Is last month's usage 1/2 SD outside persons typical usage, above/below?
 - Usage zero in last month indicator
 - Usage zero in last 3 months indicator
 - If usage is zero in last month, but not in month before that.
 - Irregularity - Fraction of months where this person has zero usage in this feature.
 - Exponentially smoothed value

Name
-------


Haskell
```
```
SQL
```
```
R
```
```
LINQ
```
```
Lucid
```
```
RDO
```
```
C:
```
```
