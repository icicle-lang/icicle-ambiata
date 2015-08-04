package icicle;

import java.util.*;

public class IcicleMap
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
        // Mutation!
        m.put(k,v);
        return m;
    }

    public static <K,V>
    Pair<K,V> getByIndex(HashMap<K,V> m, int ix)
    {
        int num = 0;
        // This is quite bad.
        // TODO
        for (Map.Entry<K,V> elem : m.entrySet()) {
            if (ix == num) {
                return Pair.create(elem.getKey(), elem.getValue());
            }
            num++;
        }
        return null;
    }
}


