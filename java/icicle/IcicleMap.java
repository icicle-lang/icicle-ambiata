import java.util.*;

class IcicleMap
{
    public static <K,V>
    HashMap<K,V> empty()
    {
        return new HashMap<K,V>();
    }

    public static <K,V>
    HashMap<K,V> put(HashMap<K,V> m, K k, V v)
    {
        // Gosh!
        m.put(k,v);
        return m;
    }

    public static <K,V>
    Pair<K,V> getByIndex(HashMap<K,V> m, int ix)
    {
        // !
        return null;
    }
}


