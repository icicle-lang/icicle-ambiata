package icicle;

public class Pair<A,B>
{
    private A _fst;
    private B _snd;
    public A fst() { return _fst; }
    public B snd() { return _snd; }

    public Pair(A a, B b)
    {
        _fst = a;
        _snd = b;
    }

    public static<A,B> Pair<A,B> create(A a, B b)
    {
        return new Pair<A,B>(a, b);
    }

    public String toString()
    {
        return "(" + _fst.toString() + ", " + _snd.toString() + ")";
    }
}
