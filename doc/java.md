Outputting Java Bytecode
========================

We want to convert flattened Avalanche to jbc.
It looks like Jacob Stanley has the majority of a decent library for doing this: [https://github.com/jystic/tonic].

There are other libraries which may be better baked, for example hs-java, but I just dislike its imperative way of constructing bytecode.
I think I will start with Jacob's library, and if I run into too many shortcomings, switch to hs-java.


Examples
========

CountAboveTen
-------------

Let's start with a very simple one.
Given the input query:
```
feature salary ~> filter value > 10 ~> count
```

We get the following flattened Avalanche.
```
gen$date = DATE
{
  init acc$conv$5 = 0 : Int (Resumable);
  for_facts (gen$fact : Int) in new {
    let anf$3 = fst# [Int] [DateTime] gen$fact;
    if (gt# [Int] anf$3 (10 : Int)) {
      read acc$conv$5 = acc$conv$5;
      write acc$conv$5 = add# acc$conv$5 (1 : Int);
    }
  }
  read conv$5 = acc$conv$5;
  return conv$5;
}
```

I'm imagining the Java would be something like this.
```
void Compute(IcicleMagic icicle)
{
    // gen$date = DATE
    DateTime gen_date = icicle.SnapshotDate();

    // init acc$conv$5 = 0 : Int (Resumable);
    Integer acc_conv_5_load = icicle.LoadResumable<Integer>("CountAboveTen", "acc$conv$5");
    int acc_conv_5 = 0;
    if (acc_conv_5_load != null) {
        acc_conv_5 = acc_conv_5_load.value;
    }

    // for_facts (gen$fact : Int) in new
    icicle.StartNew();
    while (icicle.HasField()) {
        // let anf$3 = fst# [Int] [DateTime] gen$fact;
        int anf_3 = icicle.CurrentRow<Integer>().value;

        // if (gt# [Int] anf$3 (10 : Int))
        if (anf_3 > 10) {
            // read acc$conv$5 = acc$conv$5;
            int read_acc_conv_5 = acc_conv_5;
            // write acc$conv$5 = add# acc$conv$5 (1 : Int);
            acc_conv_5 = read_acc_conv_5 + 1;
        }
    }

    // init acc$conv$5 = 0 : Int (Resumable);
    icicle.SaveResumable<Integer>("CountAboveTen", "acc$conv$5", new Integer(acc_conv_5));

    // read conv$5 = acc$conv$5;
    int conv_5 = acc_conv_5
    // return conv$5;
    icicle.Output<Integer>("CountAboveTen", new Integer(conv_5));
}
```

And the very very rough bytecode.
This is not quite right syntax but the idea is there.
It could also be improved a bit like not storing variables before using them just once.
```
// void Compute(IcicleMagic icicle)
    aload       ICICLE
    invokestatic    #IcicleMagic.SnapShotDate
    astore      GEN_DATE                // gen_date = icicle.SnapShotDate()


    aload       ICICLE
    ldc             "CountAboveTen"
    ldc             "acc$conv$5"
    invokestatic    #IcicleMagic.LoadResumable
    astore      ACC_CONV_5_LOAD         // acc_conv_5_load = icicle.LoadResumable(...)


    iconst      0
    istore      ACC_CONV_5              // acc_conv_5 = 0


    aload       ACC_CONV_5_LOAD         // acc_conv_5_load
    aconstnull                          // null
    if_acmpne   BR_LOAD_ACC_CONV_5      // if (acc_conv_5_load != null)
    goto        BR_SKIP_ACC_CONV_5

BR_LOAD_ACC_CONV_5:

    aload       ACC_CONV_5_LOAD
    invokestatic    #Integer.value
    istore      ACC_CONV_5              // acc_conv_5 = acc_conv_5_load.value()

BR_SKIP_ACC_CONV_5:

    aload       ICICLE
    invokevirtual   #IcicleMagic.StartNew

BR_WHILE_START:

    aload       ICICLE
    invokevirtual   #IcicleMagic.HasField
    ifeq        BR_WHILE_END        // if (icicle.HasField() == 0) goto end


    aload       ICICLE
    invokevirtual   #IcicleMagic.CurrentRow
    checkcast       #Integer
    invokevirtual   #Integer.value
    istore      ANF_3               // anf_3 = icicle.CurrentRow<Integer>().value;

    iload       ANF_3
    iconst      10
    if_icmpgt   BR_ANF_3_THEN       // if (anf_3 > 10)
    goto        BR_ANF_3_ELSE

BR_ANF_3_THEN:
    iload       ACC_CONV_5
    istore      READ_ACC_CONV_5     // read_acc_conv_5 = acc_conv_5;

    iload       READ_ACC_CONV_5
    iconst      1
    iadd
    istore      ACC_CONV_5          // acc_conv_5 = read_acc_conv_5 + 1

BR_ANF_3_ELSE:
    goto BR_WHILE_START


BR_WHILE_END:

    aload       ICICLE
    ldc         "CountAboveTen"
    ldc         "acc$conv$5"

    new         #Integer
    dup
    iload       ACC_CONV_5
    invokespecial #Integer.<init>

    invokestatic #IcicleMagic.SaveResumable // icicle.SaveResumable<Integer>(...);

    iload       ACC_CONV_5
    istore      CONV_5

    aload       ICICLE
    ldc         "CountAboveTen"

    new         #Integer
    dup
    iload       ACC_CONV_5
    invokespecial #Integer.<init>

    invokestatic #IcicleMagic.Output    // icicle.Output<Integer>("CountAboveTen", new Integer(conv_5));
}
```


CountLastWeek
-------------

How many things happened last week?
Input query:
```
feature salary ~> windowed between 7 days and 14 days ~> count
```

Flattened Avalanche.
```
gen$date = DATE
{
  init acc$conv$4 = 0 : Int (Windowed);
  for_facts (gen$fact : Int) in history {
    let anf$4 = snd# [Int] [DateTime] gen$fact;
    let anf$5 = DateTime_daysDifference# anf$4 gen$date;
    let anf$6 = le# [Int] anf$5 (14 : Int);
    let anf$8 = ge# [Int] anf$5 (7 : Int);
    if (and# anf$6 anf$8) {
      read acc$conv$4 = acc$conv$4;
      write acc$conv$4 = add# acc$conv$4 (1 : Int);
    } else {
      if (lt# [Int] anf$5 (7 : Int)) {
        keep_fact_in_history;
      }

    }

  }
  for_facts (gen$fact : Int) in new {
    let anf$16 = snd# [Int] [DateTime] gen$fact;
    let anf$17 = DateTime_daysDifference# anf$16 gen$date;
    let anf$18 = le# [Int] anf$17 (14 : Int);
    let anf$20 = ge# [Int] anf$17 (7 : Int);
    if (and# anf$18 anf$20) {
      read acc$conv$4 = acc$conv$4;
      write acc$conv$4 = add# acc$conv$4 (1 : Int);
    } else {
      if (lt# [Int] anf$17 (7 : Int)) {
        keep_fact_in_history;
      }

    }

  }
  read conv$4 = acc$conv$4;
  return conv$4;
}
```

Java:
```
void Compute(IcicleMagic icicle)
{
    // gen$date = DATE
    DateTime gen_date = icicle.SnapshotDate();

    // init acc$conv$4 = 0 : Int (Windowed);
    int acc_conv_4 = 0;

    // for_facts (gen$fact : Int) in history
    icicle.StartHistory();
    while (icicle.HasField()) {
        // let anf$4 = snd# [Int] [DateTime] gen$fact;
        DateTime anf_4 = icicle.CurrentTime();
        // let anf$5 = DateTime_daysDifference# anf$4 gen$date;
        int anf_5 = DateTime.daysDifference(anf_4, gen_date);

        // let anf$6 = le# [Int] anf$5 (14 : Int);
        bool anf_6 = anf_5 <= 14;
        // let anf$8 = ge# [Int] anf$5 (7 : Int);
        bool anf_8 = anf_8 >= 7;

        // if (and# anf$6 anf$8)
        if (anf_6 && anf_8) {
            // read acc$conv$4 = acc$conv$4;
            int acc_conv_4_read = acc_conv_4;
            // write acc$conv$4 = add# acc$conv$4 (1 : Int);
            acc_conv_4 = acc_conv_4_read + 1;
        } else {
            // if (lt# [Int] anf$5 (7 : Int))
            if (anf_5 < 7) {
                // keep_fact_in_history;
                icicle.KeepFactInHistory();
            }
        }
    }

    // for_facts (gen$fact : Int) in new
    icicle.StartNew();
    while (icicle.HasField()) {
        // let anf$16 = snd# [Int] [DateTime] gen$fact;
        DateTime anf_16 = icicle.CurrentTime();
        // let anf$17 = DateTime_daysDifference# anf$16 gen$date;
        int anf_17 = DateTime.daysDifference(anf_16, gen_date);

        // let anf$18 = le# [Int] anf$17 (14 : Int);
        bool anf_18 = anf_17 <= 14;
        // let anf$20 = ge# [Int] anf$17 (7 : Int);
        bool anf_20 = anf_17 >= 7;

        // if (and# anf$18 anf$20)
        if (anf_18 && anf_20) {
            // read acc$conv$4 = acc$conv$4;
            int acc_conv_4_read = acc_conv_4;
            // write acc$conv$4 = add# acc$conv$4 (1 : Int);
            acc_conv_4 = acc_conv_4_read + 1;
        } else {
            // if (lt# [Int] anf$17 (7 : Int))
            if (anf_17 < 7) {
                icicle.KeepFactInHistory();
            }
        }
    }

    // read conv$4 = acc$conv$4;
    int conv_4 = acc_conv_4
    // return conv$4;
    icicle.Output<Integer>("CountLastWeek", new Integer(conv_4));
}
```

