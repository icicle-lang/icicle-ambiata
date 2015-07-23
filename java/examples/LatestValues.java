// feature salary ~> latest 10 ~> value

import java.util.*;
class LatestValues
{
  public void compute(IcicleState<Integer> icicle)
  {
    int gen$date = icicle.snapshotDate();

    Latest<Integer> ACCUMULATOR$acc$conv$1 = icicle.makeLatest(10);
    icicle.startHistory();
    while (icicle.nextRow())
    {
      int elem$gen$fact = (icicle.currentRow()).intValue();
      int elem$gen$date = icicle.currentRowDate();
      icicle.pushLatest(ACCUMULATOR$acc$conv$1, Integer.valueOf(elem$gen$fact));
    }
    icicle.startNew();
    while (icicle.nextRow())
    {
      int elem$gen$fact = (icicle.currentRow()).intValue();
      int elem$gen$date = icicle.currentRowDate();
      icicle.pushLatest(ACCUMULATOR$acc$conv$1, Integer.valueOf(elem$gen$fact));
    }
    ArrayList<Integer> conv$1 = icicle.readLatest(ACCUMULATOR$acc$conv$1);
    icicle.output(conv$1);
  }
}
