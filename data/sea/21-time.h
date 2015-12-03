#include "20-simple.h"

static void INLINE itime_to_gregorian (itime_t x, iint_t *y, iint_t *M, iint_t *d, iint_t *h, iint_t *m, iint_t *s)
{
    *y = (x >> 48);
    *M = (x >> 40) & 0xff;
    *d = (x >> 32) & 0xff;

    int64_t i = x & 0xffffffff;
    *h = i / 3600;
    *m = i % 3600 / 60;
    *s = i % 60;
}

static itime_t INLINE itime_from_gregorian (iint_t y, iint_t M, iint_t d, iint_t h, iint_t m, iint_t s)
{
    return y << 48 | M << 40 | d << 32 | (3600 * h + 60 * m + s);
}

/* Number of days since 1600-03-01 (see Ivory DateTime). */
static iint_t INLINE itime_to_epoch (itime_t x)
{
    int64_t y, m, d, _0, _1, _2;
    itime_to_gregorian (x, &y, &m, &d, &_0, &_1, &_2);
    y = y - 1600;

    m = (m + 9) % 12;
    y = y - m/10;

    return 365*y + y/4 - y/100 + y/400 + (m*306 + 5)/10 + (d - 1);
}

static itime_t INLINE itime_from_epoch (iint_t g)
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

    return itime_from_gregorian (y + 1600, mm, dd, 0, 0, 0);
}

static iint_t INLINE itime_days_diff (itime_t x, itime_t y)
{
    return itime_to_epoch (y) - itime_to_epoch (x);
}

static itime_t INLINE itime_minus_days (itime_t x, iint_t y)
{
    return itime_from_epoch (itime_to_epoch(x) - y);
}

static ibool_t INLINE is_leap_year (int64_t y)
{
    return (y % 4 == 0 && y % 100 != 0) || y % 400 == 0;
}

const int64_t month_lengths[] = {31,28,31,30,31,30,31,31,30,31,30,31};

static itime_t INLINE itime_minus_months (itime_t x, iint_t offset)
{
    int64_t y, M, d, h, m, s;
    itime_to_gregorian (x, &y, &M, &d, &h, &m, &s);
    y = y - 1600;

    ibool_t prev_year = offset > 0 && offset >= M;
    ibool_t succ_year = offset < 0 && -offset > 12 - M;

    if (prev_year) {
        y = y - ((12 + offset - M) / 12);
        M = 12 + ((M - offset) % 12);
    } else if (succ_year) {
        y = y + ((M - offset - 1) / 12);
        M = ((M - offset) % 12);
        if (M == 0) M = 12;
    } else {
        M = M - offset;
    }

    if (M == 2 && d > 28 && is_leap_year (y)) {
        d = 29;
    } else {
        int64_t max_month = month_lengths[M-1];
        if (d > max_month) {
            d = max_month;
        }
    }

    return itime_from_gregorian (y + 1600, M, d, h, m, s);
}
