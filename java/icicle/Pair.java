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

    @SuppressWarnings("unchecked")
    public boolean equals(Object o)
    {
        if (o instanceof Pair) // <A,B> ???
            return this.equals((Pair<A,B>)o);
        else
            return false;
    }
    public boolean equals(Pair<A,B> o)
    {
        return this._fst.equals(o._fst)
            && this._snd.equals(o._snd);
    }

    public int hashCode()
    {
        return this._fst.hashCode() * this._snd.hashCode();
    }
}
