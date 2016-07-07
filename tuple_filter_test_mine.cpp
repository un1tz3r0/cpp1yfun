#include <iostream>
#include <tuple>
#include <typeinfo>
#include <type_traits>

template <class... Args>
struct sequence
{
    typedef std::tuple<Args...> tuple_type;
};

template <class U, class V>
struct sequence_cat;

template <class... U, class... V>
struct sequence_cat<sequence<U...>, sequence<V...>>
{
    typedef sequence<U..., V...> type;
};

template <class... V>
struct sequence_cat<void, sequence<V...>>
{
    typedef sequence<V...> type;
};

template <class... U>
struct sequence_cat<sequence<U...>, void>
{
    typedef sequence<U...> type;
};

template <>
struct sequence_cat<void, void>
{
    typedef void type;
};

template <class T>
struct undecorate
{
    typedef typename std::remove_reference<T>::type noref_type;
    typedef typename std::remove_pointer<noref_type>::type noptr_type;
    typedef typename std::remove_cv<noptr_type>::type type;
};

template <class T>
struct deduce_storage_type
{
    typedef T type;
};

template <class T>
struct deduce_storage_type<T&>
{
    typedef T* type;
};

template <class T>
struct deduce_storage_type<const T&>
{
    typedef T type;
};

template <class T>
struct deduce_storage_type<T&&>
{
    typedef T type;
};

// helper predicate metafunction template (template template) useful
// for partially binding the first argument of a binary type predicate
// meta
template<typename T>
template<typename U>
using is_same_as = std::is_same<T, U>;

template <template<typename> class TPred, class... Args>
struct filter_type;

template <template<typename> class TPred, class Arg, class... Args>
struct filter_type<TPred, Arg, Args...>
{
    static constexpr bool pred = 
    TPred<typename undecorate<Arg>::type>::value;

    typedef typename deduce_storage_type<Arg>::type storage_type;

    typedef typename
    std::conditional<
        pred,
        typename sequence_cat<
            sequence<storage_type>,
            typename filter_type<TPred, Args...>::type
        >::type,
        typename filter_type<TPred, Args...>::type
    >::type type;       
};

template <template<typename> class TPred, class Arg>
struct filter_type<TPred, Arg>
{
    static constexpr bool pred =
    TPred<typename undecorate<Arg>::type>::value;

    typedef typename deduce_storage_type<Arg>::type storage_type;

    typedef typename
    std::conditional<pred, sequence<storage_type>, void>::type
    type;
};

/*
template <class... Args>
struct deduce_sequence_type
{
    typedef typename filter_type<is_same_as<char>, Args...>::type char_sequence;
    typedef typename filter_type<is_same_as<int>, Args...>::type int_sequence;
    typedef typename filter_type<is_same_as<float>, Args...>::type float_sequence;

    typedef typename
    sequence_cat<
        char_sequence,
        typename sequence_cat<
            int_sequence,
            float_sequence
        >::type
    >::type type;
};
*/

template <class T>
struct get_storage_type
{
    static T apply(T t) { return t; }
};

template <class T>
struct get_storage_type<T&>
{
    static T* apply(T& t) { return &t; }
};

template <class T>
struct get_storage_type<const T&>
{
    static T apply(const T& t) { return t; }
};

template <class T>
struct get_storage_type<T&&>
{
    static T&& apply(T&& t) { return std::move(t); }
};

/* some metafunction templates for transforming type_traits-like predicates. predicate_undecorate<template<typename> class TT> is 
 * a template <typename T> whose static constexpr bool member 'value' is equal to TT<typename undecorate<T>::type>::value, or if
 * you prefer, "a predicate type trait template, template-templated on another predicate type trait template, which applies the 
 * other predicate to the undecorated type of the template's type argument.
 */

template <class T>
template <class Arg>
struct is_same_as_undecorated_type
{
    static constexpr bool value =
    std::is_same<typename undecorate<Arg>::type, T>::value;
};

template <template<typename> class TPred>
template <class Arg>
struct predicate_undecorate
{
    static constexpr bool value =
    TPred<typename undecorate<Arg>::type>::value;
};

template <template<typename, typename> class TPred, typename T>
template <class Arg>
struct predicate_bind_2nd
{
    static constexpr bool value =
    TPred<Arg, T>::value;
};

static_assert(predicate_undecorate<predicate_bind_2nd<std::is_same, int>><const int&>::value);

/* 
 * build a perfect-forwarded function call discarding some parameters
*/

template <bool Pred, bool IsNextVoid, class T, class... Args>
struct filter_parameter_impl;

template <class T, class Arg1, class Arg2, class... Args>
struct filter_parameter_impl<false, false, T, Arg1, Arg2, Args...>
{
    typedef typename filter_type<T, Arg2, Args...>::type sequence_type;
    typedef typename sequence_type::tuple_type tuple_type;

    static constexpr bool pred = equals_undecorated_type<T, Arg2>::value;

    static constexpr bool is_next_next_void =
    std::is_same<typename filter_type<T, Args...>::type, void>::value;

    static tuple_type apply(Arg1&&, Arg2&& arg2, Args&&... args)
    {
        return filter_parameter_impl<
            pred, is_next_next_void, T, Arg2, Args...
        >::apply(
            std::forward<Arg2>(arg2),
            std::forward<Args>(args)...
        );
    }
};

template <class T, class Arg1, class Arg2, class... Args>
struct filter_parameter_impl<false, true, T, Arg1, Arg2, Args...>
{
    static void apply(Arg1&&, Arg2&&, Args&&...) {}
};

template <class T, class Arg1, class Arg2, class... Args>
struct filter_parameter_impl<true, false, T, Arg1, Arg2, Args...>
{
    typedef typename
    filter_type<T, Arg1, Arg2, Args...>::type
    sequence_type;

    typedef typename sequence_type::tuple_type tuple_type;

    static constexpr bool pred = equals_undecorated_type<T, Arg2>::value;

    static constexpr bool is_next_next_void =
    std::is_same<typename filter_type<T, Args...>::type, void>::value;

    static tuple_type apply(Arg1&& arg1, Arg2&& arg2, Args&&... args)
    {
        return std::tuple_cat(
            std::make_tuple(get_storage_type<Arg1>::apply(arg1)),
            filter_parameter_impl<
                pred, is_next_next_void, T, Arg2, Args...
            >::apply(
                std::forward<Arg2>(arg2),
                std::forward<Args>(args)...
            )
        );
    }
};

template <class T, class Arg1, class Arg2, class... Args>
struct filter_parameter_impl<true, true, T, Arg1, Arg2, Args...>
{
    typedef typename filter_type<T, Arg1>::type sequence_type;
    typedef typename sequence_type::tuple_type tuple_type;

    static tuple_type apply(Arg1&& arg1, Arg2&&, Args&&...)
    {
        return std::make_tuple(get_storage_type<Arg1>::apply(
            std::forward<Arg1>(arg1)
        ));
    }
};

template <class T, class Arg1, class Arg2>
struct filter_parameter_impl<false, false, T, Arg1, Arg2>
{
    typedef typename filter_type<T, Arg2>::type sequence_type;
    typedef typename sequence_type::tuple_type tuple_type;

    static tuple_type apply(Arg1&&, Arg2&& arg2)
    {
        return std::make_tuple(get_storage_type<Arg2>::apply(
            std::forward<Arg2>(arg2)
        ));
    }
};

template <class T, class Arg1, class Arg2>
struct filter_parameter_impl<false, true, T, Arg1, Arg2>
{
    static void apply(Arg1&&, Arg2&&) {}
};

template <class T, class Arg1, class Arg2>
struct filter_parameter_impl<true, false, T, Arg1, Arg2>
{
    typedef typename filter_type<T, Arg1>::type sequence_type;
    typedef typename sequence_type::tuple_type tuple_type;

    static tuple_type apply(Arg1&& arg1, Arg2&& arg2)
    {
        return std::make_tuple(
            get_storage_type<Arg1>::apply(std::forward<Arg1>(arg1)),
            get_storage_type<Arg2>::apply(std::forward<Arg2>(arg2))
        );
    }
};

template <class T, class Arg1, class Arg2>
struct filter_parameter_impl<true, true, T, Arg1, Arg2>
{
    typedef typename filter_type<T, Arg1, Arg2>::type sequence_type;
    typedef typename sequence_type::tuple_type tuple_type;

    static tuple_type apply(Arg1&& arg1, Arg2&&)
    {
        return std::make_tuple(
            get_storage_type<Arg1>::apply(std::forward<Arg1>(arg1))
        );
    }
};

template <class T, class... Args>
struct filter_parameter;

template <class T, class Arg, class... Args>
struct filter_parameter<T, Arg, Args...>
{
    typedef typename filter_type<T, Arg, Args...>::type sequence_type;

    typedef typename std::conditional<
        std::is_same<sequence_type, void>::value,
        void,
        typename sequence_type::tuple_type
    >::type tuple_type;

    static constexpr bool pred = equals_undecorated_type<T, Arg>::value;

    static constexpr bool is_next_void =
    std::is_same<typename filter_type<T, Args...>::type, void>::value;

    static tuple_type apply(Arg&& arg, Args&&... args)
    {
        return filter_parameter_impl<
            pred, is_next_void, T, Arg, Args...
        >::apply(std::forward<Arg>(arg), std::forward<Args>(args)...);
    }
};

template <bool Is1Void, bool Is2Void, bool Is3Void, class... Args>
struct get_tuple_impl;

template <class... Args>
struct get_tuple_impl<false, false, false, Args...>
{
    typedef typename deduce_sequence_type<Args...>::type sequence_type;
    typedef typename sequence_type::tuple_type tuple_type;

    static tuple_type apply(Args&&... args)
    {
        return std::tuple_cat(
            filter_parameter<char, Args...>::apply(
                std::forward<Args>(args)...
            ),
            filter_parameter<int, Args...>::apply(
                std::forward<Args>(args)...
            ),
            filter_parameter<float, Args...>::apply(
                std::forward<Args>(args)...
            )
        );
    }
};

template <class... Args>
struct get_tuple
{
    typedef typename deduce_sequence_type<Args...>::type sequence_type;

    typedef typename std::conditional<
        std::is_same<sequence_type, void>::value,
        void,
        typename sequence_type::tuple_type
    >::type tuple_type;

    static constexpr bool is1void =
    std::is_same<typename filter_type<char, Args...>::type, void>::value;
    static constexpr bool is2void =
    std::is_same<typename filter_type<int, Args...>::type, void>::value;
    static constexpr bool is3void =
    std::is_same<typename filter_type<float, Args...>::type, void>::value;

    static tuple_type apply(Args&&... args)
    {
        return get_tuple_impl<is1void, is2void, is3void, Args...>::
            apply(std::forward<Args>(args)...);
    }
};

template <class... Args>
struct foo
{
    typedef typename deduce_sequence_type<Args...>::type sequence_type;
    typedef typename sequence_type::tuple_type tuple_type;

    tuple_type t_;

    foo(Args&&... args) : t_{} {}
};

int main()
{
    char a = 5;
    const int b = 6;
    float c = 7;
    int d = 8;
    float e = 9;
    char f = 10;

    auto x = get_tuple<char&, const int&, float&, int&, float&&, char&>::
        apply(a, b, c, d, std::move(e), f);
    //std::tuple<char*, char*, int, int*, float*, float> x{&a, &f, b, &d, &c, std::move(f)};

    std::cout << typeid(x).name() << std::endl;

    return 0;
}