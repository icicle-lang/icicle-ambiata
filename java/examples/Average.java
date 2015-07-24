// feature salary ~> sum value / count
package examples;

import icicle.*;
import java.util.*;

public class Average
{
  public void compute(IcicleState<Integer> icicle)
  {
    int gen$date = icicle.snapshotDate();

    int ACCUMULATOR$acc$conv$3 = 0;
    int ACCUMULATOR$acc$conv$7 = 0;
    Integer LOAD$acc$conv$3 = icicle.<Integer>loadResumable("feature", "acc$conv$3");
    if (LOAD$acc$conv$3 != null)
    {
      ACCUMULATOR$acc$conv$3 = (LOAD$acc$conv$3).intValue();
    }
    Integer LOAD$acc$conv$7 = icicle.<Integer>loadResumable("feature", "acc$conv$7");
    if (LOAD$acc$conv$7 != null)
    {
      ACCUMULATOR$acc$conv$7 = (LOAD$acc$conv$7).intValue();
    }
    icicle.startNew();
    while (icicle.nextRow())
    {
      int elem$gen$fact = (icicle.currentRow()).intValue();
      int elem$gen$date = icicle.currentRowDate();
      int acc$conv$3 = ACCUMULATOR$acc$conv$3;
      ACCUMULATOR$acc$conv$3 = acc$conv$3 + elem$gen$fact;
      int acc$conv$7 = ACCUMULATOR$acc$conv$7;
      ACCUMULATOR$acc$conv$7 = acc$conv$7 + 1;
    }
    icicle.saveResumable("feature", "acc$conv$3", Integer.valueOf(ACCUMULATOR$acc$conv$3));
    icicle.saveResumable("feature", "acc$conv$7", Integer.valueOf(ACCUMULATOR$acc$conv$7));
    int conv$3 = ACCUMULATOR$acc$conv$3;
    int conv$7 = ACCUMULATOR$acc$conv$7;
    int conv$9 = conv$3 / conv$7;
    icicle.output(conv$9);
  }
}
