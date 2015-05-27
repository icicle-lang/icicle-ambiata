Icicle concrete syntax
======================

Sum
---

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

Haskell
```
countBy
 = fold (\map v -> Map.insertWith (+1) 1 v map) Map.empty
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
