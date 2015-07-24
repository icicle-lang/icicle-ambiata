// feature salary ~> group date ~> count
package examples;

import icicle.*;
import java.util.*;
public class GroupDate
{
  public void compute(IcicleState<Integer> icicle)
  {
    int gen$date = icicle.snapshotDate();

    HashMap<Integer, Integer> ACCUMULATOR$acc$conv$1 = IcicleMap.empty();
    HashMap<Integer, Integer> LOAD$acc$conv$1 = icicle.<HashMap<Integer, Integer>>loadResumable("feature", "acc$conv$1");
    if (LOAD$acc$conv$1 != null)
    {
      ACCUMULATOR$acc$conv$1 = LOAD$acc$conv$1;
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
    }
    icicle.saveResumable("feature", "acc$conv$1", ACCUMULATOR$acc$conv$1);
    HashMap<Integer, Integer> conv$1 = ACCUMULATOR$acc$conv$1;
    HashMap<Integer, Integer> ACCUMULATOR$flat$4 = IcicleMap.empty();
    for (int flat$5 = 0; flat$5 < conv$1.size(); flat$5++)
    {
      HashMap<Integer, Integer> flat$4 = ACCUMULATOR$flat$4;
      Pair<Integer, Integer> flat$6 = IcicleMap.getByIndex(conv$1, flat$5);
      int flat$7 = (flat$6.fst()).intValue();
      int flat$8 = (flat$6.snd()).intValue();
      HashMap<Integer, Integer> flat$9 = IcicleMap.put(flat$4, Integer.valueOf(flat$7), Integer.valueOf(flat$8));
      ACCUMULATOR$flat$4 = flat$9;
    }
    HashMap<Integer, Integer> flat$4 = ACCUMULATOR$flat$4;
    icicle.output(flat$4);
  }
}
