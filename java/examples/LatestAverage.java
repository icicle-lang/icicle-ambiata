// feature salary ~> latest 3 ~> sum value / count
package examples;

import icicle.*;
import java.util.*;
public class LatestAverage
{
  public void compute(IcicleState<Integer> icicle)
  {
    int gen$date = icicle.snapshotDate();

    Latest<Integer> ACCUMULATOR$acc$conv$1$simp$8 = icicle.makeLatest(3);
    icicle.startHistory();
    while (icicle.nextRow())
    {
      int elem$gen$fact = (icicle.currentRow()).intValue();
      int elem$gen$date = icicle.currentRowDate();
      icicle.pushLatest(ACCUMULATOR$acc$conv$1$simp$8, Integer.valueOf(elem$gen$fact));
    }
    icicle.startNew();
    while (icicle.nextRow())
    {
      int elem$gen$fact = (icicle.currentRow()).intValue();
      int elem$gen$date = icicle.currentRowDate();
      icicle.pushLatest(ACCUMULATOR$acc$conv$1$simp$8, Integer.valueOf(elem$gen$fact));
    }
    ArrayList<Integer> conv$1$simp$10 = icicle.readLatest(ACCUMULATOR$acc$conv$1$simp$8);
    int ACCUMULATOR$flat$0$simp$12 = 0;
    int ACCUMULATOR$flat$0$simp$13$simp$14 = 0;
    for (int flat$1 = 0; flat$1 < conv$1$simp$10.size(); flat$1++)
    {
      int flat$0$simp$16 = ACCUMULATOR$flat$0$simp$12;
      int flat$0$simp$17$simp$18 = ACCUMULATOR$flat$0$simp$13$simp$14;
      int simp$4 = (conv$1$simp$10.get(flat$1)).intValue();
      int simp$5 = flat$0$simp$16 + simp$4;
      int simp$6 = flat$0$simp$17$simp$18 + 1;
      ACCUMULATOR$flat$0$simp$12 = simp$5;
      ACCUMULATOR$flat$0$simp$13$simp$14 = simp$6;
    }
    int flat$0$simp$20 = ACCUMULATOR$flat$0$simp$12;
    int flat$0$simp$21$simp$22 = ACCUMULATOR$flat$0$simp$13$simp$14;
    int conv$3 = flat$0$simp$20 / flat$0$simp$21$simp$22;
    icicle.output(conv$3);
  }
}
