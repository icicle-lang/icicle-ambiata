#include "30-array.h"

template <typename T>
struct ibuf_t
{
    iint_t max_size;
    iint_t cur_size;
    iint_t head;

    T* payload;

    ibuf_t()
    {
        max_size = cur_size = head = 0;
        payload  = 0;
    }

    ibuf_t(imempool_t* pool, iint_t sz)
    {
        size_t bytes = sizeof(T) * sz;
        payload      = (T*)imempool_alloc(pool, bytes);

        max_size     = sz;
        cur_size     = 0;
        head         = 0;
    }

    ibuf_t& push(const T& val)
    {
        iint_t head_new = (cur_size < max_size)
                        ? head
                        : (head + 1) % max_size;
        iint_t size_new = cur_size < max_size
                        ? cur_size + 1
                        : max_size;

        iint_t update   = (head + cur_size) % max_size;

        payload[update] = val;

        head            = head_new;
        cur_size        = size_new;

        return *this;

    }

    iarray_t<T> read(imempool_t* pool) const
    {
        iarray_t<T> out(pool, cur_size);
        for (iint_t ix = 0; ix != cur_size; ++ix) {
            iint_t in = (head + ix) % max_size;
            out.put(ix, payload[in]);
        }
        return out;
    }
};


