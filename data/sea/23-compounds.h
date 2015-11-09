#include "22-string.h"

// TODO equality etc

template <typename T>
class ioption_t
{
    const ibool_t is_some;
    const T       value;

    ibool_t isSome() const
    {
        return this->is_some;
    }

    const T& unsafeGet() const
    {
        return this->value;
    }

    ioption_t(ibool_t s, const T& t)
     : is_some(s), value(t)
     {};

    ioption_t(const ioption_t<T>& other)
     : is_some(other.is_some), value(other.value)
     {};

    ioption_t()
     : is_some(ifalse), value()
     {};
};

template <typename T>
ioption_t<T> ioption_none()
{ return ioption_t<T>(); }

template <typename T>
ioption_t<T> ioption_some(const T& t)
{ return ioption_t<T>(itrue, t); }


template <typename T, typename U>
class ipair_t
{
    const T t;
    const U u;

    T& fst() const
    {
        return this->t;
    }

    U& snd() const
    {
        return this->snd;
    }

    ipair_t(const T& t, const U& u)
     : t(t), u(u)
     {};

    ipair_t(const ipair_t<T,U>& other)
     : t(other.t), u(other.u)
     {};
};

template <typename T, typename U>
ipair_t<T,U> ipair_pair(const T& t, const U& u)
{ return ipair_t<T,U>(t, u); }


template <typename L, typename R>
class isum_t
{
    const ibool_t   is_right;
    const L         l;
    const R         r;

    ibool_t isRight() const
    {
        return this->is_right;
    }

    const L& unsafeLeft() const
    {
        return this->l;
    }

    const R& unsafeRight() const
    {
        return this->r;
    }

    isum_t(ibool_t is_right, const L& l, const R& r)
     : is_right(is_right), l(l), r(r)
     {};

    isum_t(const isum_t<L,R>& other)
     : is_right(other.is_right), l(other.l), r(other.r)
     {};
};

template <typename L, typename R>
isum_t<L,R> isum_left(const L& l)
{ return isum_t<L,R>(l, R()); }
template <typename L, typename R>
isum_t<L,R> isum_right(const R& r)
{ return isum_t<L,R>(L(), r); }

