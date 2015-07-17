import java.util.*;

interface IcicleState<T>
{
    Day snapshotDate();

    <U>
    U       loadResumable(String virtualFeature, String accumulatorName);

    <U>
    void    saveResumable(String virtualFeature, String accumulatorName, U value);

    <U>
    void    output(String virtualFeature, U value);

    void    startHistory();
    void    startNew();

    boolean nextRow();

    void    keepFactInHistory();

    Pair<T,Day> currentRow();

    <U>
    Latest<U> makeLatest(int size);

    <U>
    void      pushLatest(Latest<U> latest, U value);

    <U>
    ArrayList<U>       getLatest(Latest<U> latest);

}
