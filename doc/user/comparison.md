Icicle versus SQL
=================

First, a disclaimer that this comparison must not be taken too seriously.
While Icicle and SQL might both be query languages, they fulfil very different needs.
SQL is quite a bit more expressive than Icicle, but at the expense of certain performance constraints.

All these examples assume a feature called "feat" of type int.
The corresponding table would be something like
```
CREATE TABLE feat
 ( value  int
 , date   int
 , entity id )
```
with an implicit group-by on the entity.


Sum
---

Icicle:
```
feature feat
~> sum value
```

SQL:
```
select  sum(value)
from    feat
```

Count
-----

Icicle:
```
feature feat
~> count
```

SQL:
```
select  count(*)
from    feat
```

Windowed count
--------------

Icicle:
```
feature feat
~> windowed 30 days
~> count
```

SQL:
```
select  count(*)
from    feat
where  (now() - date <= 30)
```

Filtered count
--------------

Icicle:
```
feature feat
~> filter value > 50
~> count
```

SQL:
```
select count(*) from feat
where value > 50
```


CountBy
-------

Icicle:
```
feature feat
~> group value
~> count
```

SQL
```
select      value, count(*)
from        feat
group by    value
```

CountDays
--------

Icicle:
```
feature feat
~> distinct date
~> count
```


SQL
```
select  count(date) as c
from    feat;
```

MaxDays
--------

TODO: Note that the "regroup" primitive is not currently implemented in Icicle, nor is maximum.
Neither is the "group of monotonic key" optimisation which would allow this to not allocate at all.

Icicle:
```
feature feat
~> group fold (day|day_count) = (group date ~> count)
~> maximum day_count
```

SQL
```
select  max(z.c)
from   (select      count(*) as c
        from        feat
        group by    date) z;
```


DaysSinceEarliest
-----------------

TODO: Sadly, "now" is not actually implemented yet either.

Icicle:
```
feature feat
~> now - oldest date
```

SQL
```
select  top 1
        now() - date
from    feat
ordered by date asc
```


FilteredOverTotal
----------------
Icicle
```
feature feat
~> let v = (filter value > 0 ~> count)
~> let y = count
~> v / y
```

SQL
```
select  filt.c / total.c
from   (select  count(*) as c
        from    source
        where   value > 0) filt,
       (select  count(*) as c
        from    source) total;
```

Standard deviation
-------
Icicle
```
feature feat
~> let sqr = mean (value * value)
~> let mnn = mean  value
~> sqrt (sqr - mnn * mnn)
```

SQL
```
select
        sqrt(
            avg(value * value) - (avg(value) * avg(value))
        )
from    feat
```

Most recent value from 30-60 days ago
----------------------
Icicle:
```
feature feat
~> windowed between 30 and 60 days
~> newest value
```

SQL
```
select      top 1
            value
from        feat
where       now() - date between 30 and 60
order by    days desc
```

Average of last three months
------------------------
Icicle:
```
feature feat
~> windowed 3 months
~> average value
```

SQL
```
select  avg(value)
from    feat
where   now() - date <= 90
```

Number of zeroes in last 3 entries
---------------

Icicle
```
feature feat
~> latest 3
~> filter value == 0
~> count
```

SQL
```
select  count(*)
from   (select  top 3 *
        from    feat
        order by date desc) z
where   z.value = 0
```

If zero in last entry, but not in entry before.
-----------------------------------------------
TODO (&&) needs to be implemented (trivial)

Icicle
```
feature feat
~> latest 2
~> let a = newest value
~> let b = oldest value
~> a == 0 && b /= 0
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
```



Exponentially smoothed value
----------------------------
Icicle
```
feature feat
~> let fold1 smooth = value : value * 0.5 + smooth * 0.5
~> smooth
```

SQL
```
select      sum(value / pow(2,rownum))
from        feat
order by    date desc
```

