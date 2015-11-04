#include "03-simple.h"

/* Number of days since 1600-03-01 (see Ivory DateTime). */
static iint_t INLINE idate_to_epoch (idate_t x)
{
    int64_t y = (x >> 48) - 1600;
    int64_t m = (x >> 40) & 0xff;
    int64_t d = (x >> 32) & 0xff;

    m = (m + 9) % 12;
    y = y - m/10;

    return 365*y + y/4 - y/100 + y/400 + (m*306 + 5)/10 + (d - 1);
}

static iint_t INLINE idate_from_epoch (iint_t g)
{
    int64_t y = ((10000*g + 14780)/3652425);
    int64_t ddd = g - (365*y + y/4 - y/100 + y/400);

    if (ddd < 0) {
        y = y - 1;
        ddd = g - (365*y + y/4 - y/100 + y/400);
    }

    int64_t mi = (100*ddd + 52)/3060;
    int64_t mm = (mi + 2)%12 + 1;

    y = y + (mi + 2)/12;

    int64_t dd = ddd - (mi*306 + 5)/10 + 1;

    return (y + 1600) << 48 | mm << 40 | dd << 32;
}

static iint_t INLINE idate_days_diff (idate_t x, idate_t y)
{
    return idate_to_epoch(y) - idate_to_epoch(x);
}

static iint_t INLINE idate_minus_days (idate_t x, iint_t y)
{
    return idate_from_epoch(idate_to_epoch(x) - y);
}

static ibool_t INLINE is_leap_year (int64_t y)
{
    return (y % 4 == 0 && y % 100 != 0) || y % 400 == 0;
}

const int64_t month_lengths[] = {31,28,31,30,31,30,31,31,30,31,30,31};

static iint_t INLINE idate_minus_months (idate_t x, iint_t offset)
{
    int64_t y = (x >> 48) - 1600;
    int64_t m = (x >> 40) & 0xff;
    int64_t d = (x >> 32) & 0xff;

    ibool_t prev_year = offset > 0 && offset >= m;
    ibool_t succ_year = offset < 0 && -offset > 12 - m;

    if (prev_year) {
        y = y - ((12 + offset - m) / 12);
        m = 12 + ((m - offset) % 12);
    } else if (succ_year) {
        y = y + ((m - offset - 1) / 12);
        m = ((m - offset) % 12);
        if (m == 0) m = 12;
    } else {
        m = m - offset;
    }

    if (m == 2 && d > 28 && is_leap_year (y)) {
        d = 29;
    } else {
        int64_t max_month = month_lengths[m-1];
        if (d > max_month) {
            d = max_month;
        }
    }

    return (y + 1600) << 48 | m << 40 | d << 32;
}
