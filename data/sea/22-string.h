#include "21-date.h"

class istring_t
{

    bool operator==(const istring_t& y) const
    { return cmp(y) == 0; }
    bool operator!=(const istring_t& y) const
    { return cmp(y) != 0; }
    bool operator>(const istring_t& y) const
    { return cmp(y) > 0; }
    bool operator>=(const istring_t& y) const
    { return cmp(y) >= 0; }
    bool operator<(const istring_t& y) const
    { return cmp(y) < 0; }
    bool operator<=(const istring_t& y) const
    { return cmp(y) <= 0; }

    iint_t cmp(const istring_t& y) const
    {
        return strcmp(this->payload, y.payload);
    }

    const char* const payload;

    istring_t(const char* const payload)
        : payload(payload)
    { }

    istring_t(const istring_t& other)
        : payload(other.payload)
    { }
};

