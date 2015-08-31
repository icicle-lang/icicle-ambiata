On Tombstones
=============
Machine learning processes need to be able to support sparse data, indicating a lack of knowledge or genuinely nonsense and unset states.
It is also required that one can reset this state, so that one's current (or latest) state is the same as if it were never set.

As an example, I may know the country in which Ames lives, and use it as a feature. Let's say I `ingest` this into icicle when they
moved to their new home.
```
         '11       '12       '13
     =====|=========|=========|======>>>   Time
Ames               AUS -------------->>>

```

If I had a label for Ames in 2011, then I can't use the fact that she lived in AUS at the time, as this could be label leakage. So the
only appropriate value for this feature is `Missing`. For both 2012 and 2013, I can say that she lived in AUS, and use it as a feature.

Moving forward, Ames finishes her job and moves away in 2014, but we don't know which country she moved to. We need to say that she isn't
in AUS, and also can't set any new value --- so we put down a tombstone to indicate that the AUS value is no longer valid beyond 2014.
```
         '11       '12       '13       '14
     =====|=========|=========|=========|======>>>   Time
Ames               AUS -----------------☠

```
Now, after 2014, for either learning or scoring purposes, the value is again `Missing`. This does not mean however, that if one were to
attempt to learn from Ames with a label of 2013, that we should not use AUS, as that is still the correct value for that time.

Expressions
-----------
This has been simple for the `latest` or current value expression, however, for Icicle expressions things can get more complicated quickly.

Using a bank balance as an example, a simple expression may be that I would like to know the average amount of money in it for the past month,
If we say that each day (or at semi-frequent intervals) I receive a data feed, then this is simply the integral of the balance over time.

If however, there is a data corruption event, or the account has just opened or closed, then for a portion of the month there is no data
(either by an explicit placement of a tombstone, or just that the data only started halfway through).
```
     =====|=========|=========|=========|======>>>   Time
                             ####
                             ####
                            /####
          _______          / ####
      ___/       \________/  ####\        ___
     /                       #### \      /
                             ####  \____/
                             ####
```

Now there's a few things one could do here, it's quite reasonable to just say, I don't know, and return a `Missing` value for ths feature,
one could also calculate the integral for all of the area they do know about, or, one could assume the data was steady underneath while the
value was unknown. None of these is `wrong` per se.

This motivates Icicle to be both explicit and flexible when it comes to Tombstones and Missing values.

Proposal
--------
Any expression which `uses` a tombstone during its evaluation returns a missing value, however, a primitive will be provided to either
filter or map tombstones, such that they can be handled explicitly.

```
feature x ~> filter (not tombstone) ~> count
```
will filter out all tombstone values and allow count to run.
```
feature x ~> let xx = if (tombstone) 0 else value ~> mean xx
```
will map any tombstone value into 0 before taking the mean.

The first expression means that unless another feature is also requesting these facts, they won't even be put into an incremental snapshot.
The second expression means that the tombstone will in fact have bubblegum attached to it, so go into the snapshot, but the expression will
yield a valid answer. The benefit of this approach is that we have only one new value to implement, `tombstone`, which is either True or False,
depending on the value.
