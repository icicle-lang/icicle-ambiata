import java.util.*;

class MemoryState<T> implements IcicleState<T>
{
    private Day _snapshotDate     = Day.now();
    private ArrayList<Pair<T,Day>> _facts_history = new ArrayList<>();
    private ArrayList<Pair<T,Day>> _facts_new     = new ArrayList<>();

    private HashMap<Pair<String,String>, Object>
                        _resumables     = new HashMap<>();

    private HashMap<String, Object>
                        _outputs        = new HashMap<>();

    private IdentityHashMap<Object, ArrayList<Pair<Integer, Object>>>
                        _latests        = new IdentityHashMap<>();

    private HashSet<Integer>
                        _kept_in_history= new HashSet<>();

    private HashSet<Integer>
                        _read_from_history= null;

    private boolean _in_history = true;
    private int _history_ix = -1;
    private int _new_ix     = -1;

    public void setDate(Day d)
    {
        _snapshotDate = d;
    }

    public void ingest(ArrayList<Pair<T,Day>> newest)
    {
        _facts_history.addAll(_facts_new);
        _facts_new.clear();
        _facts_new.addAll(newest);

        for (ArrayList<Pair<Integer,Object>> ixs : _latests.values()) {
            for (Pair<Integer,Object> kv : ixs) {
                _kept_in_history.add(kv.fst());
            }
        }

        _latests.clear();
        _outputs.clear();

        _read_from_history = (HashSet<Integer>)_kept_in_history.clone();
        _kept_in_history.clear();
    }

    public HashMap<String,Object> getOutputs()
    {
        return (HashMap<String,Object>)_outputs.clone();
    }

    public Day snapshotDate()
    {
        return _snapshotDate;
    }

    public <U> U loadResumable(String virtualFeature, String accumulatorName)
    {
        return (U)_resumables.get(Pair.create(virtualFeature, accumulatorName));
    }

    public <U> void    saveResumable(String virtualFeature, String accumulatorName, U value)
    {
        _resumables.put(Pair.create(virtualFeature, accumulatorName), value);
    }

    public <U> void    output(String virtualFeature, U value)
    {
        _outputs.put(virtualFeature, value);
    }

    public void    startHistory()
    {
        _in_history = true;
        _history_ix = -1;
    }

    public void    startNew()
    {
        _in_history = false;
        _new_ix     = -1;
    }


    public boolean nextRow()
    {
        if (_in_history) {
            while (true) {
                _history_ix++;
                if (_history_ix >= _facts_history.size()) {
                    return false;
                }
                if (readThisIx(_history_ix)) {
                    return true;
                }
            }
        } else {
            while (true) {
                _new_ix++;
                if (_new_ix >= _facts_new.size()) {
                    return false;
                }
                if (readThisIx(idOfNewIx(_new_ix))) {
                    return true;
                }
            }
        }
    }

    private boolean readThisIx(int val)
    {
        return (_read_from_history == null || _read_from_history.contains(Integer.valueOf(val)));
    }
    private int idOfNewIx(int new_ix)
    {
        return new_ix + _facts_history.size();
    }

    private int currentId()
    {
        if (_in_history) {
            return _history_ix;
        } else {
            return idOfNewIx(_new_ix);
        }
    }

    public void keepFactInHistory()
    {
        _kept_in_history.add(currentId());
    }

    public Pair<T,Day> currentRow()
    {
        if (_in_history) {
            return _facts_history.get(_history_ix);
        } else {
            return _facts_new.get(_new_ix);
        }
    }


    public <U> Latest<U> makeLatest(int size)
    {
        Latest<U> l = new Latest<U>();
        l.numberOfThings = size;

        _latests.put(l, new ArrayList<>());

        return l;
    }

    public <U> void pushLatest(Latest<U> latest, U value)
    {
        ArrayList<Pair<Integer,Object>> objs = _latests.get(latest);
        objs.add(Pair.create(Integer.valueOf(currentId()), value));

        if (objs.size() > latest.numberOfThings) {
            objs.remove(0);
        }
    }

    public <U> ArrayList<U> getLatest(Latest<U> latest)
    {
        ArrayList<U> out = new ArrayList<U>();
        ArrayList<Pair<Integer,Object>> objs = _latests.get(latest);
        for (int i = 0; i != objs.size(); ++i) {
            out.add((U)objs.get(i).snd());
        }

        return out;
    }

}
