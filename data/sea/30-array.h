#include "23-compounds.h"

template<typename T>
struct iarray_t
{
    iint_t count;
    T* payload;

    iint_t length() const
    {
        return this->count;
    }

    ibool_t operator==(const iarray_t<T>& other) const
    {
        if (other.count != count) return ifalse;
        for (iint_t ix = 0; ix != other.count; ++ix) {
            if (payload[ix] != other.payload[ix]) {
                return ifalse;
            }
        }
        return itrue;
    }

    ibool_t operator<(const iarray_t<T>& other) const
    {
        iint_t min = (count < other.count) ? count : other.count;
        for (iint_t ix = 0; ix != min; ++ix) {
            if (!(payload[ix] < other.payload[ix])) {
                return ifalse;
            }
        }
        if (count < other.count) return itrue;
        else return ifalse;
    }

    ibool_t operator!=(const iarray_t<T>& other) const
    { return !(*this == other); }
    ibool_t operator<=(const iarray_t<T>& other) const
    { return (*this == other) || (*this < other); }
    ibool_t operator>=(const iarray_t<T>& other) const
    { return !(*this < other); }
    ibool_t operator>(const iarray_t<T>& other) const
    { return !(*this <= other); }

    const T& index(iint_t ix) const
    { return this->payload[ix]; }

    iarray_t<T>& put(iint_t ix, const T& v)
    {
        this->payload[ix] = v;
        return *this;
    }

    iarray_t()
    {
        this->count   = 0;
        this->payload = 0;
    }

    iarray_t(imempool_t* pool, iint_t count)
    {
        this->count   = count;
        this->payload = imempool_alloc_of<T>(pool, count);
    }
};

