package tests;

import examples.*;
import icicle.*;

import java.util.*;

class Main
{

    static Pair<Integer,Integer>
     pc(int i, int j) {
         return Pair.create(Integer.valueOf(i), Integer.valueOf(j));
     }

    public static void doit(IcicleComputer<Integer> inp)
    {
        MemoryState<Integer> ms = new MemoryState<Integer>();
        ms.setDate(10);

        ArrayList<Pair<Integer,Integer>> arr = new ArrayList<>();
        arr.add(pc(5,2));
        arr.add(pc(10,3));
        arr.add(pc(20,3));
        arr.add(pc(30,4));
        arr.add(pc(25,5));
        ms.ingest(arr);

        inp.compute(ms);

        System.out.println("initial data [5@2 10@3 20@3 30@4 25@5]");
        System.out.println(ms.getOutputs().toString());


        arr.clear();
        arr.add(pc(30,6));
        ms.ingest(arr);
        inp.compute(ms);

        System.out.println("ingest [30@6]");
        System.out.println(ms.getOutputs().toString());

        arr.clear();
        arr.add(pc(35,6));
        arr.add(pc(35,7));
        ms.ingest(arr);
        inp.compute(ms);

        System.out.println("ingest [35@6 35@7]");
        System.out.println(ms.getOutputs().toString());


        arr.clear();
        arr.add(pc(40,8));
        arr.add(pc(40,9));
        arr.add(pc(30,10));
        arr.add(pc(20,11));
        ms.ingest(arr);
        inp.compute(ms);

        System.out.println("ingest [40@8 40@9 30@10 20@11]");
        System.out.println(ms.getOutputs().toString());
    }

    public static void main(String[] args)
    {
        System.out.println("\n--- GroupDate");
        doit(new GroupDate());
        System.out.println("\n--- Average");
        doit(new Average());
        System.out.println("\n--- LatestAverage");
        doit(new LatestAverage());
        System.out.println("\n--- LatestValues");
        doit(new LatestValues());
    }
}
