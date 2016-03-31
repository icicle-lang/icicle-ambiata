# Weird queries in the wild

I think it's worthwhile writing down a bunch of interesting queries here.
Particularly queries that are hard to express, or not obvious.

There are a few reasons for this: firstly, we can keep track of exactly what we are missing in terms of expressiveness.
I think with higher-order functions we'd be able to add prelude functions for everything we want, but this is just a hunch.
These are also good examples that make me feel good about how useful it is at its core: these examples might not be the prettiest, but it makes me happy we can express them at all.
Finally, they might be useful to someone trying to write their own queries, as advanced examples.

## Number of days where sum is greater than zero

```
feature salary
 ~> group fold (date',sum') = (group date ~> sum value)
 ~> let has_any = case sum' > 0 | True -> 1 | False -> 0 end
 ~> sum has_any
```

An extra function in the prelude could clean this up a bit.


## Check if maximum of date is after now

```
feature injury
 ~> windowed 2 months
 ~> let last_injury = max injury_date
 ~> last_injury > now
```

Currently, "now" is an Aggregate because, originally for resumable queries (big data), the fold could not mention the current date.
This means you can't do the obvious thing and `filter injury_date > now ~> count` but this restriction will probably be lifted later.

## Find location whose recent injury is most severe, then find its mean

```
feature injury
 ~> group fold (loc', newmean')
     = (group location
    ~> (newest severity, mean severity))
 ~> let new' = fst newmean'
 ~> maxby new' (loc', newmean')
```

The order of this query is potentially confusing: while you are grouping over the location and trying to find the newest severity, you need to also find the means of all locations, just in case.
This is because of the single-pass restriction.
After finding the location with max severity, you have already seen all the data, so you cannot look at it again.
Instead, you need to move the mean calculation to inside the grouping.
