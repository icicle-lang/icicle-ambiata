import java.util.*;

interface IcicleState<T>
{
    int snapshotDate();
    int daysDifference(int from, int to);

    <U>
    U       loadResumable(String virtualFeature, String accumulatorName);

    <U>
    void    saveResumable(String virtualFeature, String accumulatorName, U value);

    <U>
    void    output(U value);

    void    startHistory();
    void    startNew();

    boolean nextRow();

    void    keepFactInHistory();

    T currentRow();
    int currentRowDate();

    <U>
    Latest<U> makeLatest(int size);

    <U>
    void      pushLatest(Latest<U> latest, U value);

    <U>
    ArrayList<U>       readLatest(Latest<U> latest);

}
