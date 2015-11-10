#include "31-buffer.h"

template <typename K, typename V>
struct imap_t
{
    iint_t count;
    K* keys;
    V* vals;

    imap_t()
    {
        this->count = 0;
        this->keys  = 0;
        this->vals  = 0;
    }


    iint_t length() const
    {
        return this->count;
    }

    imap_t<K,V>& put(imempool_t* pool, const K& key, const V& val)
    {
        for (iint_t ix = 0; ix != count; ++ix)
        {
            if (keys[ix] == key) {
                vals[ix] = val;
                return *this;
            }
        }

        K* keys_ = imempool_alloc_of<K>(pool, count+1);
        V* vals_ = imempool_alloc_of<V>(pool, count+1);

        for (iint_t ix = 0; ix != count; ++ix)
        {
            keys_[ix] = keys[ix];
            vals_[ix] = vals[ix];
        }
        keys_[count] = key;
        vals_[count] = val;

        count = count + 1;
        keys = keys_;
        vals = vals_;

        return *this;
    }

    ioption_t<V> lookup(const K& key) const
    {
        for (iint_t ix = 0; ix != count; ++ix)
        {
            if (keys[ix] == key) {
                return ioption_some(vals[ix]);
            }
        }
        return ioption_none<V>();
    }

    ipair_t<K,V> index(iint_t ix) const
    {
        return ipair_pair(keys[ix], vals[ix]);
    }


    // TODO equality etc.
    // equality would be much easier with an ordered map
};

