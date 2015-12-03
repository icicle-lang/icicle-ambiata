#include "40-grisu2-powers.h"

#if !ICICLE_NO_PSV

/*
Copyright (c) 2009 Florian Loitsch

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
*/

typedef struct diyfp_t {
    uint64_t f;
    int      e;
} diyfp_t;

static diyfp_t INLINE diyfp_sub (diyfp_t x, diyfp_t y)
{
    assert(x.e == y.e);
    assert(x.f >= y.f);
    diyfp_t r = {.f = x.f - y.f, .e = x.e};
    return r;
}

static diyfp_t INLINE diyfp_mul (diyfp_t x, diyfp_t y)
{
    uint64_t a,b,c,d,ac,bc,ad,bd,tmp,h;
    diyfp_t r; uint64_t M32 = 0xFFFFFFFF;
    a = x.f >> 32; b = x.f & M32;
    c = y.f >> 32; d = y.f & M32;
    ac = a*c; bc = b*c; ad = a*d; bd = b*d;
    tmp = (bd>>32) + (ad&M32) + (bc&M32);
    tmp += 1U << 31; /// mult_round
    r.f = ac+(ad>>32)+(bc>>32)+(tmp >>32);
    r.e = x.e + y.e + 64;
    return r;
}

#define DIY_SIGNIFICAND_SIZE 64

static diyfp_t diyfp_cached_power (int k) {
    diyfp_t res;
    int index = 343 + k;
    res.f = powers_ten[index];
    res.e = powers_ten_e[index];
    return res;
}

typedef union {
    double d;
    uint64_t n;
} converter_t;

static uint64_t INLINE double_to_uint64 (double d)
{
    converter_t tmp;
    tmp.d = d;
    return tmp.n;
}

static double INLINE uint64_to_double (uint64_t d64)
{
    converter_t tmp;
    tmp.n = d64;
    return tmp.d;
}

#define DP_SIGNIFICAND_SIZE (52)
#define DP_EXPONENT_BIAS    (0x3FF + DP_SIGNIFICAND_SIZE)
#define DP_MIN_EXPONENT     (-DP_EXPONENT_BIAS)
#define DP_EXPONENT_MASK    (UINT64_C(0x7FF0000000000000))
#define DP_SIGNIFICAND_MASK (UINT64_C(0x000FFFFFFFFFFFFF))
#define DP_HIDDEN_BIT       (UINT64_C(0x0010000000000000))

static diyfp_t INLINE diyfp_normalize (diyfp_t in)
{
    diyfp_t res = in;
    /* Normalize now */
    /* the original number could have been a denormal. */
    while (! (res.f & DP_HIDDEN_BIT))
    {
        res.f <<= 1;
        res.e--;
    }
    /* do the final shifts in one go. Don't forget the hidden bit (the '-1') */
    res.f <<= (DIY_SIGNIFICAND_SIZE - DP_SIGNIFICAND_SIZE - 1);
    res.e = res.e - (DIY_SIGNIFICAND_SIZE - DP_SIGNIFICAND_SIZE - 1);
    return res;
}

static diyfp_t INLINE double_to_difyfp (double d)
{
    uint64_t d64 = double_to_uint64 (d);
    int biased_e = (d64 & DP_EXPONENT_MASK) >> DP_SIGNIFICAND_SIZE;
    uint64_t significand = (d64 & DP_SIGNIFICAND_MASK);
    diyfp_t res;
    if (biased_e != 0)
    {
        res.f = significand + DP_HIDDEN_BIT;
        res.e = biased_e - DP_EXPONENT_BIAS;
    } else {
        res.f = significand;
        res.e = DP_MIN_EXPONENT + 1;
    }
    return res;
}

static diyfp_t INLINE diyfp_normalize_boundary (diyfp_t in)
{
    diyfp_t res = in;
    /* Normalize now */
    /* the original number could have been a denormal. */
    while (! (res.f & (DP_HIDDEN_BIT << 1)))
    {
        res.f <<= 1;
        res.e--;
    }
    /* do the final shifts in one go. Don't forget the hidden bit (the '-1') */
    res.f <<= (DIY_SIGNIFICAND_SIZE - DP_SIGNIFICAND_SIZE - 2);
    res.e = res.e - (DIY_SIGNIFICAND_SIZE - DP_SIGNIFICAND_SIZE - 2);
    return res;
}

static void INLINE diyfp_normalized_boundaries (double d, diyfp_t* out_m_minus, diyfp_t* out_m_plus)
{
    diyfp_t v = double_to_difyfp(d);
    diyfp_t pl, mi;
    bool significand_is_zero = v.f == DP_HIDDEN_BIT;
    pl.f  = (v.f << 1) + 1; pl.e  = v.e - 1;
    pl = diyfp_normalize_boundary(pl);
    if (significand_is_zero)
    {
        mi.f = (v.f << 2) - 1;
        mi.e = v.e - 2;
    } else {
        mi.f = (v.f << 1) - 1;
        mi.e = v.e - 1;
    }
    mi.f <<= mi.e - pl.e;
    mi.e = pl.e;
    *out_m_plus = pl;
    *out_m_minus = mi;
}

static void INLINE grisu2_fill_exponent (int K, char* buffer)
{
    int i = 0;
    if (K < 0) {
        buffer[i++] = '-';
        K = -K;
    }
    if (K >= 100) {
        buffer[i++] = '0' + K / 100; K %= 100;
        buffer[i++] = '0' + K / 10; K %= 10;
        buffer[i++] = '0' + K;
    } else if (K >= 10) {
        buffer[i++] = '0' + K / 10; K %= 10;
        buffer[i++] = '0' + K;
    } else {
        buffer[i++] = '0' + K;
    }
    buffer[i] = '\0';
}

#define TEN9 1000000000

static void INLINE grisu_round (char* buffer, int len, uint64_t delta, uint64_t rest, uint64_t ten_kappa, uint64_t wp_w)
{
    while (rest < wp_w &&
            delta - rest >= ten_kappa &&
            (rest + ten_kappa < wp_w || /// closer
             wp_w - rest > rest+ten_kappa - wp_w))
    {
        buffer[len-1]--; rest += ten_kappa;
    }
}

#define D_1_LOG2_10 0.30102999566398114 //  1 / lg(10)

static int INLINE grisu2_k_comp (int e, int alpha, int gamma)
{
    return ceil ((alpha-e+63) * D_1_LOG2_10);
}

static void INLINE grisu2_digit_gen (diyfp_t W, diyfp_t Mp, diyfp_t delta, char* buffer, int* len, int* K)
{
    uint32_t div; int d,kappa; diyfp_t one, wp_w;
    wp_w = diyfp_sub (Mp, W);
    one.f = ((uint64_t) 1) << -Mp.e; one.e = Mp.e;
    uint32_t p1 = Mp.f >> -one.e; /// Mp_cut
    uint64_t p2 = Mp.f & (one.f - 1);
    *len = 0; kappa = 10; div = TEN9;
    while (kappa > 0) {
        d = p1 / div;
        if (d || *len) buffer[(*len)++] = '0' + d; /// Mp_inv1
        p1 %= div; kappa--;
        uint64_t tmp = (((uint64_t)p1)<<-one.e)+p2;
        if (tmp <= delta.f) { /// Mp_delta
            *K += kappa;
            grisu_round (buffer, *len, delta.f, tmp, ((uint64_t)div) << -one.e, wp_w.f);
            return;
        }
        div /= 10;
    }
    uint64_t unit = 1;
    while (1) {
        p2 *= 10; delta.f *= 10; unit *= 10;
        d = p2 >> -one.e;
        if (d || *len) buffer[(*len)++] = '0' + d;
        p2 &= one.f - 1; kappa--;
        if (p2 < delta.f) {
            *K += kappa;
            grisu_round (buffer, *len, delta.f, p2, one.f, wp_w.f*unit);
            return;
        }
    }
}

static void INLINE grisu2 (double v, char* buffer, int* length, int* K)
{
    diyfp_t w_m, w_p;
    int q = 64, alpha = -59, gamma = -56; int pos;
    diyfp_normalized_boundaries (v, &w_m, &w_p);
    diyfp_t w = diyfp_normalize (double_to_difyfp (v));
    int mk = grisu2_k_comp (w_p.e + q, alpha, gamma);
    diyfp_t c_mk = diyfp_cached_power (mk);
    diyfp_t W  = diyfp_mul (w,   c_mk);
    diyfp_t Wp = diyfp_mul (w_p, c_mk);
    diyfp_t Wm = diyfp_mul (w_m, c_mk);
    Wm.f++; Wp.f--;
    diyfp_t delta = diyfp_sub (Wp, Wm);
    *K = -mk;
    grisu2_digit_gen (W, Wp, delta, buffer, length, K);
}

static int INLINE grisu2_prettify (char* buffer, int from_pos, int end_pos, int k)
{
    int nb_digits = end_pos - from_pos;
    int i, offset;
    /* v = buffer * 10^k
       kk is such that 10^(kk-1) <= v < 10^kk
       this way kk gives the position of the comma.
       */
    int kk = nb_digits + k;

    buffer[end_pos] = '\0';
    if (nb_digits <= kk && kk <= 21) {
        /* the first digits are already in. Add some 0s and call it a day. */
        /* the 21 is a personal choice. Only 16 digits could possibly be relevant.
         * Basically we want to print 12340000000 rather than 1234.0e7 or 1.234e10 */
        for (i = nb_digits; i < kk; i++)
            buffer[from_pos + i] = '0';
        buffer[kk] = '.';
        buffer[kk+1] = '0';
        buffer[kk+2] = '\0';
        return kk+2;
    } else if (0 < kk && kk <= 21) {
        /* comma number. Just insert a '.' at the correct location. */
        memmove (&buffer[from_pos + kk + 1], &buffer[from_pos + kk], nb_digits - kk);
        buffer[from_pos + kk] = '.';
        buffer[from_pos + nb_digits + 1] = '\0';
        return from_pos + nb_digits + 1;
    } else if (-6 < kk && kk <= 0) {
        /* something like 0.000abcde.
         * add '0.' and some '0's */
        offset = 2-kk;
        memmove (&buffer[from_pos + offset], &buffer[from_pos], nb_digits);
        buffer[from_pos] = '0';
        buffer[from_pos + 1] = '.';
        for (i = from_pos + 2; i < from_pos + offset; i++)
            buffer[i] = '0';
        buffer[from_pos + nb_digits + offset] = '\0';
        return from_pos + nb_digits + offset;
    } else if (nb_digits == 1) {
        /* just add 'e...' */
        buffer[from_pos + 1] = 'e';
        /* fill_positive_fixnum will terminate the string */
        grisu2_fill_exponent (kk - 1, &buffer[from_pos + 2]);
        return strlen (buffer);
    } else {
        /* leave the first digit. then add a '.' and at the end 'e...' */
        memmove (&buffer[from_pos + 2], &buffer[from_pos + 1], nb_digits-1);
        buffer[from_pos + 1] = '.';
        buffer[from_pos + nb_digits + 1] = 'e';
        /* fill_fixnum will terminate the string */
        grisu2_fill_exponent (kk - 1, &buffer[from_pos + nb_digits + 2]);
        return strlen (buffer);
    }
}

static int NOINLINE grisu2_dtostr (double v, char* buffer)
{
    if (isnan (v)) {
        buffer[0] = 'n';
        buffer[1] = 'a';
        buffer[2] = 'n';
        buffer[3] = '\0';
        return 3;
    }

    if (v == INFINITY) {
        buffer[0] = 'i';
        buffer[1] = 'n';
        buffer[2] = 'f';
        buffer[3] = '\0';
        return 3;
    }

    if (v == -INFINITY) {
        buffer[0] = '-';
        buffer[1] = 'i';
        buffer[2] = 'n';
        buffer[3] = 'f';
        buffer[4] = '\0';
        return 4;
    }

    if (v == 0) {
        buffer[0] = '0';
        buffer[1] = '.';
        buffer[2] = '0';
        buffer[3] = '\0';
        return 3;
    }

    int sign_length = 0;

    if (v < 0) {
        sign_length = 1;
        *buffer++ = '-';
        v = -v;
    }

    int grisu2_length, K;
    grisu2 (v, buffer, &grisu2_length, &K);
    int final_length = grisu2_prettify (buffer, 0, grisu2_length, K);

    return final_length + sign_length;
}

#endif
