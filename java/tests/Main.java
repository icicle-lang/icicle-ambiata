package tests;

import examples.*;
import icicle.*;

import java.util.*;

class Main
{
    public static void main(String[] args)
    {
        MemoryState<Integer> ms = new MemoryState<Integer>();
        ms.setDate(100);

        ArrayList<Pair<Integer,Integer>> arr = new ArrayList<>();
        arr.add(Pair.create(Integer.valueOf(5), Integer.valueOf(2)));
        arr.add(Pair.create(Integer.valueOf(10), Integer.valueOf(20)));
        arr.add(Pair.create(Integer.valueOf(30), Integer.valueOf(30)));
        arr.add(Pair.create(Integer.valueOf(40), Integer.valueOf(40)));
        arr.add(Pair.create(Integer.valueOf(40), Integer.valueOf(41)));
        arr.add(Pair.create(Integer.valueOf(40), Integer.valueOf(41)));
        arr.add(Pair.create(Integer.valueOf(40), Integer.valueOf(41)));
        arr.add(Pair.create(Integer.valueOf(40), Integer.valueOf(42)));
        arr.add(Pair.create(Integer.valueOf(40), Integer.valueOf(43)));
        ms.ingest(arr);

        GroupDate l = new GroupDate();
        l.compute(ms);

        System.out.println(ms.getOutputs().toString());
    }
}
