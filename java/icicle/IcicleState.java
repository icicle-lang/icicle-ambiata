import java.util.*;
import java.time.LocalDate;

interface IcicleState<T>
{
    LocalDate snapshotDate();

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

    T       currentRow();
    LocalDate currentRowDate();

    <U>
    Latest<U> makeLatest(int size);

    <U>
    void      pushLatest(Latest<U> latest, U value);

    <U>
    ArrayList<U>       getLatest(Latest<U> latest);

}
