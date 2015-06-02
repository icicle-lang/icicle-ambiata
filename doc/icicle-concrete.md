Icicle concrete syntax
======================

Sum
---

Icicle:
```
sum feat

feature feat
~> sum value
```

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

Datomic/datalog
```
[:find (sum ?f)
 :where [?f :feat]]
```


Count
-----

Icicle:
```
count feat

feature feat
~> count
```

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


Datomic/datalog
```
[:find (count ?f)
 :where [?f :feat]]
```



Windowed count
--------------

Icicle:
```
count (windowed feat (< 30 days))

feature feat
~> windowed newer than 30 days
~> count
```

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

Datomic/datalog
```
[:find (count ?f)
 :where [?f :feat]
        [?f :feat/date ?d]
        [(daysAgo ?d) ?ago]
        [(< ?ago 30)]]
```


CountBy
-------

Icicle:
```
[ count ms | ms <- group feat ]

groupWith count feat

feature feat
~> group by value
~> count
```

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
map length $ group feat
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

Datomic/datalog (not sure if the :with is correct)
```
; query
[:find ?cat (count ?cat) .
 :with ?cat
 :in [[?cat :feat]]] 
 
;; rather than this
[:find ?cat (count ?cat)
 :with ?cat
 :where [?cat :feat]]
 
;; http://docs.datomic.com/query.html#outline-container-5-17-1
```


CountDays
--------

Icicle:
```
count (groupDays feat)

feature feat
~> distinct date
~> count
```


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

Datomic/datalog
```
[:find (count-distinct ?d)
 :where [?f :feat]
        [?f :feat/date ?d]]
```


MaxDays
--------
Icicle:
```
max [ count d | d <- groupDays feat ]

feature feat
~> max (group by date ~> count)
```

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

Datomic/datalog
```
[:find (max ?d)
 :where [_ :feat/date ?d]]
```


DaysSinceEarliest
-----------------
Icicle:
```
now - dateOf (oldest feat)

???

[ now - dateOf f | f <- oldest feat ]

feature feat
~> now - oldest date
```

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
Icicle?
```
count (filter (>0) feat) / count feat

count [ f | f <- feat, f > 0] / count feat

count ( feat where (>0) ) / count feat

feature feat
~> let v = (filter value > 0 ~> count)
~> let y = count
~> v / y
```

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


Standard deviation
-------
Icicle
```
let sqs  = avg (feat^2)
    mean = avg  feat
in  sqrt (sqs - mean * mean)


sqrt (
    ( sum (feat*feat) - sum feat * sum feat )
    / count feat
)


sqrt (avg (feat*feat) - avg feat * avg feat)


stddev feat


feature feat
~> let sqr = average (value * value)
~> let mnn = average  value
~> sqrt (sqr - mnn * mnn)

feature feat
~> stddev value
```

Core:
```
Program
Stream
    feat    = Source
    sqs_s   = Map (\v -> v*v) feat
Reduction
    mean    = Fold (+) 0 feat
    sqs     = Fold (+) 0 sqs_s
Return
    sqrt ((sqs - mean * mean) / len)
```



Haskell
```
let mean = sum feat / length feat
    devs = map (\v -> (v - mean) ^ 2) feat
in  sqrt (sum devs / length feat)

==>

let sqs  = sum $ map (^2) feat
    mean = sum            feat
in  sqrt ((sqs - mean * mean) / length feat)

==>

let (mean,sqs) 
         = fold ((+) *** (+))   (0,0)
         $ map  (\v -> (v, v*v)) feat
in  sqrt ((sqs - mean * mean) / length feat)
```

SQL
```
SELECT
    sqrt(
        avg(value * value) - (avg(value) * avg(value))
    )
FROM
    feat

OR (if you're lucky)

SELECT stddev(value) FROM feat
```

C:
```
double mean = 0;
double sqs  = 0;
int    len  = 0;
for (V* cur = feat; cur != feat_end; ++cur) {
    mean += cur->value;
    sqs  += cur->value * cur->value;
    len  += 1;
}
return sqrt((sqs - mean * mean) / len);
```

Datomic/datalog
```
[:find (stddev ?f)
 :where [?f :feat]]
```


Relative standard deviation (S.d. divided by mean)
-------------------
Core:
```
Program
Stream
    feat    = Source
    sqs_s   = Map (\v -> v*v) feat
Reduction
    mean    = Fold (+) 0 feat
    sqs     = Fold (+) 0 sqs_s
Return
    sqrt ((sqs - mean * mean) / len) / (mean / len)
```


Most recent value from 30-60 days ago
----------------------
Icicle:
```
newest (windowed feat (> 30days) (< 60days))

feature feat
~> windowed between 30 and 60 days
~> newest value
```

Core:
actually this one is a problem right now: only postcomputations can and "newer than" windows can access the current date.
Two options: add "between window" primitive, or allow filters to access current date if windowed.
```
Program
Stream
    wind   = WindowBetween 60 30
Reduction
    recent = LatestN 1 wind
Returns
    listToMaybe recent
```


Haskell
```
safeLast
 $ filter (\f -> let ago = diff unsafeNow $ dateOf f
                  in ago > 30 && ago < 60)
   feat
```

SQL
```
SELECT TOP 1
 *
FROM
 feat
WHERE
 days - now() between 30 and 60
ORDER BY
 days DESC
```

C:
```
V* recent = null;
for (V* cur = feat; cur != feat_end; ++cur) {
    if (cur->days > 30 && cur->days < 60) {
        recent = cur;
    }
}
return recent;
```

Average of last calendar month
----------------------
Icicle:
```
avg (windowed feat (=1month))

feature feat
~> windowed between 1 and 2 months
~> average value
```

Core: this is hard to express because the reduction can't use the current date.
Map of averages by month:
```
Program
Stream
    feat = Source
Reduction
    avg = Fold (\m f ->
                    let v     = valueOf f
                        month = monthOf $ dateOf f
                    in  Map.insertWith ((+v) *** (+1)) (v,1) m month)
          Map.empty feat
Post
    maybe_sum_count = avg.lookup (Now - 1 month)
    mean            = case maybe_sum_count of Nothing -> 0; Just (s,c) -> s / c
Return
    mean
```
But we can actually window the input in this case:
```
Program
Stream
    feat = SourceWindowed 70
Reduction
    avg = Fold (\m f ->
                    let v     = valueOf f
                        month = monthOf $ dateOf f
                    in  Map.insertWith ((+v) *** (+1)) (v,1) m month)
          Map.empty feat
Post
    maybe_sum_count = avg.lookup (Now - 1 month)
    mean            = case maybe_sum_count of Nothing -> 0; Just (s,c) -> s / c
Return
    mean
```
Having the current date in the filter would make it much simpler.
```
Program
Stream
    feat = SourceWindowed 70
    last = Filter (\f -> dateOf f `sameMonthAs` unsafeDateNow - 1 month) feat
Reduction
    sum   = Fold (+) 0 last
    count = Fold (const (+1)) 0 last
Return
    sum / count
```
But a window by calendar months actually seems even simpler.
```
Program
Stream
    feat = WindowBetween (1 month) (2 months)
Reduction
    sum   = Fold (+) 0 feat
    count = Fold (const (+1)) 0 feat
Return
    sum / count
```


Haskell
```
let sameMonthAs a b = a{day=1} == b{day=1}
    lastMonth   = unsafeDateNow - 1 month
    fs = filter (\f -> dateOf f `sameMonthAs` unsafeDateNow) feat
in  avg fs
```

SQL
```
SELECT  avg(value)
FROM    feat
WHERE   month date = month (now() - 1 month)
    AND year  date = year  (now() - 1 month)
```

C
```
double sum   = 0;
double count = 0;
int dt = minus_month(now(), 1);
for (...) {
    if (same_month(dt, v->date)) {
        sum += v->value;
        count += 1;
    }
}
return sum / count;
```

Datomic/datalog
```
[:find (avg ?f)
 :where [?f :feat]
        [?f :feat/date ?d]
        [(monthOf ?d) ?md]
        [(now) ?now]
        [(monthOf ?now) ?mn]
        [= ?md ?mn]]
```

Average of last three months
------------------------
Icicle:
```
avg (windowed feat (< 3 months))

feature feat
~> windowed newer than 3 months
~> average value
```

Core
```
Program
Stream
    feat = SourceWindowed (3 months)
Reduction
    sum   = Fold (+) 0 feat
    count = Fold (const (+1)) 0 feat
Return
    sum / count
```


Haskell
```
avg
 $ filter (\v -> ... ) feat
```

SQL
```
SELECT
    avg(value)
FROM
    feat
WHERE
    month date >= month now() - 3
```

Number of zeroes in last 3 entries
---------------
Icicle?
```
count (latest 3 feat == 0)

feature feat
~> latest 3 entries
~> filter value == 0
~> count
```

Haskell
```
length
 $ filter (==0)
 $ latest 3 feat
```

SQL
```
SELECT
 count(*)
FROM
 (SELECT TOP 3 * FROM feat ORDER BY date DESC) z
WHERE
 z.value = 0
```

Core
```
Stream
    feat = Source
Reduction
    last = Latest 3
Postcomputation
    zeros = Array.filter (==0) last
Return
    count zeros
```

If zero in last entry, but not in entry before.
-----------------------------------------------
Icicle?
```
[ a == 0 && b /= 0 | [a,b] <- latest 2 feat ]

feature feat
~> latest 2 entries
~> let a = newest value
~> let b = oldest value
~> a == 0 and b /= 0
```

Core
```
Stream
    feat = Source
Reduction
    last = Latest 2
Return
    case (index last 0, index last 1) of
     (Some a, Some b)
      -> a == 0 && b /= 0
     _
      -> False
```


Haskell
```
case latest 2 feat of
 [a,b]
  -> a == 0 && b /= 0
 _
  -> False
```

SQL
```
SELECT
    CASE WHEN a.value = 0 AND b.value <> 0
         THEN 1
         ELSE 0
    END
FROM
    (SELECT TOP 1   FROM feat ORDER BY date DESC) a,
    (SELECT TOP 1 FROM (SELECT TOP 2 FROM feat ORDER BY date DESC) ORDER BY date asc) b

OR

SELECT
    CASE WHEN a.value = 0 AND b.value <> 0
         THEN 1
         ELSE 0
    END
FROM
    (SELECT FROM feat ORDER BY date DESC OFFSET 0 ROWS FETCH FIRST 1 ROWS ONLY) a,
    (SELECT FROM feat ORDER BY date DESC OFFSET 1 ROWS FETCH FIRST 1 ROWS ONLY) b,
```

C:
```
int a = 0;
int b = 0;
for (V* cur = feat; cur != ...) {
    b = a;
    a = cur->value;
}
return (a == 0 && b != 0);
```

Fraction of zeroes
------------------
Icicle??

If we have "auto-promotion" of eg (*) to (zipWith (*)) and (*5) to (map (*5)) or whatever,
then it's tempting to imagine that (==0) should mean (filter (==0)).

But this is wrong, because why should it be filter and not (map (==0))? and what does (==) applied to two vectors do - return the pairs that satisfy?
That's stupid.
So I propose that if we had autopromotion sort of thing this would always return 1:
```
count (feat==0) / count feat

feature feat
~> let c = (filter value == 0 ~> count)
~> let t = count
~> c / t
```

Core
```
Stream
    feat = Source
    zeroes = Filter (==0) feat
Reduction
    countA = Fold (const (+1)) 0 feat
    countZ = Fold (const (+1)) 0 zeroes
Return
    countZ / countA
```

See FilteredOverTotal.


Exponentially smoothed value
----------------------------
Icicle?
```
fold1 (\a v -> a * 0.5 + v * 0.5) feat

feature feat
~> let fold smooth = value : value * 0.5 + smooth * 0.5 
~> smooth
```

Core
```
Stream
    feat = Source
Reduction
    recent_avg
        = Fold
            (\a v -> case a of
                      Nothing -> Just v
                      Just a' -> Just $ a' * 0.5 + v * 0.5)
            Nothing
            feat
Return
    recent_avg
```



Haskell
```
foldl1_safe k
 = foldl
    (\a v -> case a of
              Nothing -> Just v
              Just a' -> Just $ k a' v)
    Nothing
        

foldl1_safe (\a v -> a * 0.5 + v * 0.5) feat
```

SQL
```
SELECT sum(value / pow(2,ROWNUM))
FROM   feat
ORDER BY date DESC
```

Lucid
```
moving
 where
  moving = value fby moving * 0.5 + value * 0.5
 end
```

C
```
int v = 0;
int v_set = 0;
for (V* cur = feat; cur != feat_end; ++cur) {
    if (v_set) {
        v = v * 0.5 + cur->value * 0.5;
    } else {
        v = cur->value;
    }
}
return v;
```

