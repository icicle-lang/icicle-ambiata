// feature salary ~> (group date ~> count), (group value ~> count)
package examples;

import icicle.*;
import java.util.*;
public class GroupDate
{
  public void compute(IcicleState<Integer> icicle)
  {
    int gen$date = icicle.snapshotDate();

    HashMap<Integer, Integer> ACCUMULATOR$acc$conv$1 = IcicleMap.empty();
    HashMap<Integer, Integer> ACCUMULATOR$acc$conv$7 = IcicleMap.empty();
    HashMap<Integer, Integer> LOAD$acc$conv$1 = icicle.<HashMap<Integer, Integer>>loadResumable("feature", "acc$conv$1");
    if (LOAD$acc$conv$1 != null)
    {
      ACCUMULATOR$acc$conv$1 = LOAD$acc$conv$1;
    }
    HashMap<Integer, Integer> LOAD$acc$conv$7 = icicle.<HashMap<Integer, Integer>>loadResumable("feature", "acc$conv$7");
    if (LOAD$acc$conv$7 != null)
    {
      ACCUMULATOR$acc$conv$7 = LOAD$acc$conv$7;
    }
    icicle.startNew();
    while (icicle.nextRow())
    {
      int elem$gen$fact = (icicle.currentRow()).intValue();
      int elem$gen$date = icicle.currentRowDate();
      HashMap<Integer, Integer> acc$conv$1 = ACCUMULATOR$acc$conv$1;
      int anf$2 = 0 + 1;
      Integer flat$0 = acc$conv$1.get(Integer.valueOf(elem$gen$date));
      if (flat$0 != null)
      {
        int flat$1 = (flat$0).intValue();
        int simp$1 = flat$1 + 1;
        HashMap<Integer, Integer> flat$2 = IcicleMap.put(acc$conv$1, Integer.valueOf(elem$gen$date), Integer.valueOf(simp$1));
        ACCUMULATOR$acc$conv$1 = flat$2;
      }
      else
      {
        HashMap<Integer, Integer> flat$3 = IcicleMap.put(acc$conv$1, Integer.valueOf(elem$gen$date), Integer.valueOf(anf$2));
        ACCUMULATOR$acc$conv$1 = flat$3;
      }
      HashMap<Integer, Integer> acc$conv$7 = ACCUMULATOR$acc$conv$7;
      Integer flat$4 = acc$conv$7.get(Integer.valueOf(elem$gen$fact));
      if (flat$4 != null)
      {
        int flat$5 = (flat$4).intValue();
        int simp$3 = flat$5 + 1;
        HashMap<Integer, Integer> flat$6 = IcicleMap.put(acc$conv$7, Integer.valueOf(elem$gen$fact), Integer.valueOf(simp$3));
        ACCUMULATOR$acc$conv$7 = flat$6;
      }
      else
      {
        HashMap<Integer, Integer> flat$7 = IcicleMap.put(acc$conv$7, Integer.valueOf(elem$gen$fact), Integer.valueOf(anf$2));
        ACCUMULATOR$acc$conv$7 = flat$7;
      }
    }
    icicle.saveResumable("feature", "acc$conv$1", ACCUMULATOR$acc$conv$1);
    icicle.saveResumable("feature", "acc$conv$7", ACCUMULATOR$acc$conv$7);
    HashMap<Integer, Integer> conv$1 = ACCUMULATOR$acc$conv$1;
    HashMap<Integer, Integer> conv$7 = ACCUMULATOR$acc$conv$7;
    HashMap<Integer, Integer> ACCUMULATOR$flat$8 = IcicleMap.empty();
    for (int flat$15 = 0; flat$15 < conv$1.size(); flat$15++)
    {
      HashMap<Integer, Integer> flat$8 = ACCUMULATOR$flat$8;
      Pair<Integer, Integer> flat$16 = IcicleMap.getByIndex(conv$1, flat$15);
      int flat$17 = (flat$16.fst()).intValue();
      int flat$18 = (flat$16.snd()).intValue();
      HashMap<Integer, Integer> flat$19 = IcicleMap.put(flat$8, Integer.valueOf(flat$17), Integer.valueOf(flat$18));
      ACCUMULATOR$flat$8 = flat$19;
    }
    HashMap<Integer, Integer> flat$8 = ACCUMULATOR$flat$8;
    HashMap<Integer, Integer> ACCUMULATOR$flat$9 = IcicleMap.empty();
    for (int flat$10 = 0; flat$10 < conv$7.size(); flat$10++)
    {
      HashMap<Integer, Integer> flat$9 = ACCUMULATOR$flat$9;
      Pair<Integer, Integer> flat$11 = IcicleMap.getByIndex(conv$7, flat$10);
      int flat$12 = (flat$11.fst()).intValue();
      int flat$13 = (flat$11.snd()).intValue();
      HashMap<Integer, Integer> flat$14 = IcicleMap.put(flat$9, Integer.valueOf(flat$12), Integer.valueOf(flat$13));
      ACCUMULATOR$flat$9 = flat$14;
    }
    HashMap<Integer, Integer> flat$9 = ACCUMULATOR$flat$9;
    Pair<HashMap<Integer, Integer>, HashMap<Integer, Integer>> conv$13 = Pair.create(flat$8, flat$9);
    icicle.output(conv$13);
  }
}
