// :dictionary data/example/asx-dictionary.psv
// feature stuff ~> windowed 60 days ~> filter (high - low > 100) ~> group date ~> sum (open - close)
//
package examples;

import java.util.*;

public class Asx
{
  public void compute(IcicleState<IcicleStruct> icicle)
  {
    int gen$date = icicle.snapshotDate();

    HashMap<Integer, Integer> ACCUMULATOR$acc$conv$4 = IcicleMap.empty();
    icicle.startHistory();
    while (icicle.nextRow())
    {
      IcicleStruct elem$gen$fact = icicle.currentRow();
      int elem$gen$date = icicle.currentRowDate();
      int anf$1 = icicle.daysDifference(elem$gen$date, gen$date);
      if (anf$1 <= 60)
      {
        int anf$8 = (elem$gen$fact.<Integer>getField("high")).intValue();
        int anf$10 = (elem$gen$fact.<Integer>getField("low")).intValue();
        int anf$11 = anf$8 - anf$10;
        if (anf$11 > 100)
        {
          HashMap<Integer, Integer> acc$conv$4 = ACCUMULATOR$acc$conv$4;
          int anf$20 = (elem$gen$fact.<Integer>getField("open")).intValue();
          int anf$22 = (elem$gen$fact.<Integer>getField("close")).intValue();
          int anf$23 = anf$20 - anf$22;
          int anf$24 = 0 + anf$23;
          Integer flat$0 = acc$conv$4.get(Integer.valueOf(elem$gen$date));
          if (flat$0 != null)
          {
            int flat$1 = (flat$0).intValue();
            int simp$11 = flat$1 + anf$23;
            HashMap<Integer, Integer> flat$2 = IcicleMap.put(acc$conv$4, Integer.valueOf(elem$gen$date), Integer.valueOf(simp$11));
            ACCUMULATOR$acc$conv$4 = flat$2;
          }
          else
          {
            HashMap<Integer, Integer> flat$3 = IcicleMap.put(acc$conv$4, Integer.valueOf(elem$gen$date), Integer.valueOf(anf$24));
            ACCUMULATOR$acc$conv$4 = flat$3;
          }
          icicle.keepFactInHistory();
        }
      }
    }
    icicle.startNew();
    while (icicle.nextRow())
    {
      IcicleStruct elem$gen$fact = icicle.currentRow();
      int elem$gen$date = icicle.currentRowDate();
      int anf$27 = icicle.daysDifference(elem$gen$date, gen$date);
      if (anf$27 <= 60)
      {
        int anf$34 = (elem$gen$fact.<Integer>getField("high")).intValue();
        int anf$36 = (elem$gen$fact.<Integer>getField("low")).intValue();
        int anf$37 = anf$34 - anf$36;
        if (anf$37 > 100)
        {
          HashMap<Integer, Integer> acc$conv$4 = ACCUMULATOR$acc$conv$4;
          int anf$46 = (elem$gen$fact.<Integer>getField("open")).intValue();
          int anf$48 = (elem$gen$fact.<Integer>getField("close")).intValue();
          int anf$49 = anf$46 - anf$48;
          int anf$50 = 0 + anf$49;
          Integer flat$4 = acc$conv$4.get(Integer.valueOf(elem$gen$date));
          if (flat$4 != null)
          {
            int flat$5 = (flat$4).intValue();
            int simp$23 = flat$5 + anf$49;
            HashMap<Integer, Integer> flat$6 = IcicleMap.put(acc$conv$4, Integer.valueOf(elem$gen$date), Integer.valueOf(simp$23));
            ACCUMULATOR$acc$conv$4 = flat$6;
          }
          else
          {
            HashMap<Integer, Integer> flat$7 = IcicleMap.put(acc$conv$4, Integer.valueOf(elem$gen$date), Integer.valueOf(anf$50));
            ACCUMULATOR$acc$conv$4 = flat$7;
          }
          icicle.keepFactInHistory();
        }
      }
    }
    HashMap<Integer, Integer> conv$4 = ACCUMULATOR$acc$conv$4;
    HashMap<Integer, Integer> ACCUMULATOR$flat$8 = IcicleMap.empty();
    for (int flat$9 = 0; flat$9 < conv$4.size(); flat$9++)
    {
      HashMap<Integer, Integer> flat$8 = ACCUMULATOR$flat$8;
      Pair<Integer, Integer> flat$10 = IcicleMap.getByIndex(conv$4, flat$9);
      int flat$11 = (flat$10.fst()).intValue();
      int flat$12 = (flat$10.snd()).intValue();
      HashMap<Integer, Integer> flat$13 = IcicleMap.put(flat$8, Integer.valueOf(flat$11), Integer.valueOf(flat$12));
      ACCUMULATOR$flat$8 = flat$13;
    }
    HashMap<Integer, Integer> flat$8 = ACCUMULATOR$flat$8;
    icicle.output(flat$8);
  }
}
