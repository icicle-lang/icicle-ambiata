// feature salary ~> latest 3 ~> value
package examples;

import icicle.*;
import java.util.*;

public class LatestValues implements IcicleComputer<Integer>
{
  public void compute(IcicleState<Integer> icicle)
  {
    int gen$date = icicle.snapshotDate();

    Latest<Integer> ACCUMULATOR$acc$conv$1 = icicle.makeLatest(3);
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
