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
struct is_same_as {
    template<typename U>
    using apply = std::is_same<typename undecorate<T>::type, typename undecorate<U>::type>;
};

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

template <template<typename> class TPred, class... Args>
struct deduce_sequence_type
{
    typedef typename filter_type<TPred, Args...>::type sequence_type;
    
    typedef sequence_type type;
};

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
struct is_same_as_undecorated
{
    template <class Arg>
    using apply = std::is_same<typename undecorate<Arg>::type, T>;
};

template <template<typename> class TPred>
struct undecorate_predicate
{
    template <class Arg>
    using apply = TPred<typename undecorate<Arg>::type>;
};

template <template<typename, typename> class TPred, typename T>
struct bind_2nd_predicate
{
    template <class Arg>
    using apply = TPred<Arg, T>;
};

static_assert(undecorate_predicate<bind_2nd_predicate<std::is_same, int>::apply>::apply<const int&>::value, "should be same type");

/* 
 * build a perfect-forwarded function call discarding some parameters
*/

template <bool Pred, bool IsNextVoid, template<typename> class TPred, class... Args>
struct filter_parameter_impl;

template <template<typename> class TPred, class Arg1, class Arg2, class... Args>
struct filter_parameter_impl<false, false, TPred, Arg1, Arg2, Args...>
{
    typedef typename filter_type<TPred, Arg2, Args...>::type sequence_type;
    typedef typename sequence_type::tuple_type tuple_type;

    static constexpr bool pred = TPred<typename undecorate<Arg2>::type>::value;

    static constexpr bool is_next_next_void =
    std::is_same<typename filter_type<TPred, Args...>::type, void>::value;

    static tuple_type apply(Arg1&&, Arg2&& arg2, Args&&... args)
    {
        return filter_parameter_impl<
            pred, is_next_next_void, TPred, Arg2, Args...
        >::apply(
            std::forward<Arg2>(arg2),
            std::forward<Args>(args)...
        );
    }
};

template <template<typename> class TPred, class Arg1, class Arg2, class... Args>
struct filter_parameter_impl<false, true, TPred, Arg1, Arg2, Args...>
{
    static void apply(Arg1&&, Arg2&&, Args&&...) {}
};

template <template<typename> class TPred, class Arg1, class Arg2, class... Args>
struct filter_parameter_impl<true, false, TPred, Arg1, Arg2, Args...>
{
    typedef typename
    filter_type<TPred, Arg1, Arg2, Args...>::type
    sequence_type;

    typedef typename sequence_type::tuple_type tuple_type;

    static constexpr bool pred = TPred<typename undecorate<Arg2>::type>::value;

    static constexpr bool is_next_next_void =
    std::is_same<typename filter_type<TPred, Args...>::type, void>::value;

    static tuple_type apply(Arg1&& arg1, Arg2&& arg2, Args&&... args)
    {
        return std::tuple_cat(
            std::make_tuple(get_storage_type<Arg1>::apply(arg1)),
            filter_parameter_impl<
                pred, is_next_next_void, TPred, Arg2, Args...
            >::apply(
                std::forward<Arg2>(arg2),
                std::forward<Args>(args)...
            )
        );
    }
};

template <template<typename> class TPred, class Arg1, class Arg2, class... Args>
struct filter_parameter_impl<true, true, TPred, Arg1, Arg2, Args...>
{
    typedef typename filter_type<TPred, Arg1>::type sequence_type;
    typedef typename sequence_type::tuple_type tuple_type;

    static tuple_type apply(Arg1&& arg1, Arg2&&, Args&&...)
    {
        return std::make_tuple(get_storage_type<Arg1>::apply(
            std::forward<Arg1>(arg1)
        ));
    }
};

template <template<typename> class TPred, class Arg1, class Arg2>
struct filter_parameter_impl<false, false, TPred, Arg1, Arg2>
{
    typedef typename filter_type<TPred, Arg2>::type sequence_type;
    typedef typename sequence_type::tuple_type tuple_type;

    static tuple_type apply(Arg1&&, Arg2&& arg2)
    {
        return std::make_tuple(get_storage_type<Arg2>::apply(
            std::forward<Arg2>(arg2)
        ));
    }
};

template <template<typename> class TPred, class Arg1, class Arg2>
struct filter_parameter_impl<false, true, TPred, Arg1, Arg2>
{
    static void apply(Arg1&&, Arg2&&) {}
};

template <template<typename> class TPred, class Arg1, class Arg2>
struct filter_parameter_impl<true, false, TPred, Arg1, Arg2>
{
    typedef typename filter_type<TPred, Arg1, Arg2>::type sequence_type;
    typedef typename sequence_type::tuple_type tuple_type;

    static tuple_type apply(Arg1&& arg1, Arg2&& arg2)
    {
        return std::make_tuple(
            get_storage_type<Arg1>::apply(std::forward<Arg1>(arg1)),
            get_storage_type<Arg2>::apply(std::forward<Arg2>(arg2))
        );
    }
};

template <template<typename> class TPred, class Arg1, class Arg2>
struct filter_parameter_impl<true, true, TPred, Arg1, Arg2>
{
    typedef typename filter_type<TPred, Arg1, Arg2>::type sequence_type;
    typedef typename sequence_type::tuple_type tuple_type;

    static tuple_type apply(Arg1&& arg1, Arg2&&)
    {
        return std::make_tuple(
            get_storage_type<Arg1>::apply(std::forward<Arg1>(arg1))
        );
    }
};

template <template<typename> class TPred, class... Args>
struct filter_parameter;

template <template<typename> class TPred, class Arg, class... Args>
struct filter_parameter<TPred, Arg, Args...>
{
    typedef typename filter_type<TPred, Arg, Args...>::type sequence_type;

    typedef typename std::conditional<
        std::is_same<sequence_type, void>::value,
        void,
        typename sequence_type::tuple_type
    >::type tuple_type;

    static constexpr bool pred = TPred<typename undecorate<Arg>::type>::value;

    static constexpr bool is_next_void =
    std::is_same<typename filter_type<TPred, Args...>::type, void>::value;

    static tuple_type apply(Arg&& arg, Args&&... args)
    {
        return filter_parameter_impl<
            pred, is_next_void, TPred, Arg, Args...
        >::apply(std::forward<Arg>(arg), std::forward<Args>(args)...);
    }
};

template <bool Is1Void, template<typename> class TPred, class... Args>
struct get_tuple_impl;

template <template<typename> class TPred, class... Args>
struct get_tuple_impl<false, TPred, Args...>
{
    typedef typename deduce_sequence_type<TPred, Args...>::type sequence_type;
    typedef typename sequence_type::tuple_type tuple_type;

    static tuple_type apply(Args&&... args)
    {
        return /*std::tuple_cat(*/
            filter_parameter<TPred, Args...>::apply(
                std::forward<Args>(args)...
            )/*,
            filter_parameter<intPred, Args...>::apply(
                std::forward<Args>(args)...
            ),
            filter_parameter<floatPred, Args...>::apply(
                std::forward<Args>(args)...
            )
        )*/;
    }
};

template <template<typename> class TPred, class... Args>
struct get_tuple_impl<true, TPred, Args...>
{
    typedef void sequence_type;
    typedef void tuple_type;

    static void apply(Args&&... args)
    {
        /*std::tuple_cat(*/
            filter_parameter<TPred, Args...>::apply(
                std::forward<Args>(args)...
            )/*,
            filter_parameter<intPred, Args...>::apply(
                std::forward<Args>(args)...
            ),
            filter_parameter<floatPred, Args...>::apply(
                std::forward<Args>(args)...
            )
        )*/;
    }
};


template <template<typename> class TPred, class... Args>
struct get_tuple
{
    typedef typename deduce_sequence_type<TPred, Args...>::type sequence_type;

    typedef typename std::conditional<
        std::is_same<sequence_type, void>::value,
        void,
        typename sequence_type::tuple_type
    >::type tuple_type;

    static constexpr bool is1void =
    std::is_same<typename filter_type<TPred, Args...>::type, void>::value;

    static tuple_type apply(Args&&... args)
    {
        return get_tuple_impl<is1void, TPred, Args...>::
            apply(std::forward<Args>(args)...);
    }
};

// ------------------------------------------------------------------------------------------
// my extension for passing in a tuple type instead of its types as args



/** ---------------------------------------------------------------------------------------- */

/*
template<typename F, typename Tuple, std::size_t ... I>
auto apply_impl(F&& f, Tuple&& t, std::index_sequence<I...>) {
    return std::forward<F>(f)(std::get<I>(std::forward<Tuple>(t))...);
}
template<typename F, typename Tuple>
auto apply(F&& f, Tuple&& t) {
    using Indices = std::make_index_sequence<std::tuple_size<std::decay_t<Tuple>>::value>;
    return apply_impl(std::forward<F>(f), std::forward<Tuple>(t), Indices());
}
*/

template <typename Func, typename Args, typename IndexList>
struct apply_helper;

template <typename Func, typename Tuple, size_t... I>
struct apply_helper<Func, Tuple, std::index_sequence<I...>> {
    static auto apply(Func&& func, Tuple args) {
        return func(std::get<I>(std::forward<Tuple>(args))...);
    }
};

template <typename Func, typename... T>
inline
auto apply(Func&& func, std::tuple<T...>&& args) {
    using helper = apply_helper<Func, std::tuple<T...>&&, std::index_sequence_for<T...>>;
    return helper::apply(std::forward<Func>(func), std::move(args));
}

template <typename Func, typename... T>
inline
auto apply(Func&& func, std::tuple<T...>& args) {
    using helper = apply_helper<Func, std::tuple<T...>&, std::index_sequence_for<T...>>;
    return helper::apply(std::forward<Func>(func), args);
}

template <typename Func, typename... T>
inline
auto apply(Func&& func, const std::tuple<T...>& args) {
    using helper = apply_helper<Func, const std::tuple<T...>&, std::index_sequence_for<T...>>;
    return helper::apply(std::forward<Func>(func), args);
}

/** ---------------------------------------------------------------------------------------- */
// apply_filtered<type_predicate>(func, tuple)
//
// expands tuple into arguments of get_tuple<type_predicate>::apply and then expands the resulting
// tuple into arguments of func


template<typename F, typename Tuple, std::size_t ... I>
auto apply_filtered_impl2(F&& f, Tuple&& t, std::index_sequence<I...>) {
    return std::forward<F>(f)(std::get<I>(std::forward<Tuple>(t))...);
}

template<template<typename> typename TPred, typename F, typename Tuple, std::size_t ... I>
auto apply_filtered_impl1(F&& f, Tuple&& t, std::index_sequence<I...>) {
    using FilteredTuple = decltype(get_tuple<TPred, decltype(std::get<I>(std::forward<Tuple>(t)))...>::apply(std::get<I>(std::forward<Tuple>(t))...));
    using Indices = std::make_index_sequence<std::tuple_size<std::decay_t<FilteredTuple>>::value>;
    return apply_filtered_impl2(std::forward<F>(f), std::forward<FilteredTuple>(get_tuple<TPred, decltype(std::get<I>(std::forward<Tuple>(t)))...>::apply(std::get<I>(std::forward<Tuple>(t))...)), Indices());
}

template<template<typename> typename TPred, typename F, typename Tuple>
auto apply_filtered(F&& f, Tuple&& t) {
    using Indices = std::make_index_sequence<std::tuple_size<std::decay_t<Tuple>>::value>;
    return apply_filtered_impl1<TPred>(std::forward<F>(f), std::forward<Tuple>(t), Indices());
}

/** ----------------------------------------------------------------------------------------- */

struct nonesuch {};

template<bool is_void, typename F, typename Arg>
struct result_of_or_nonesuch_impl;

template<typename F, typename Arg>
struct result_of_or_nonesuch_impl<false, F, Arg> {
	static auto apply(F&& f, Arg&& arg)
	{
		return std::forward<F>(f)(std::forward<Arg>(arg));
	}
};

template<typename F, typename Arg>
struct result_of_or_nonesuch_impl<true, F, Arg> {
	static auto apply(F&& f, Arg&& arg)
	{
		std::forward<F>(f)(std::forward<Arg>(arg));
		return nonesuch{};
	}
};

template<typename F, typename Arg>
auto result_of_or_nonesuch(F&& f, Arg&& arg)
{
	return result_of_or_nonesuch_impl<std::is_same< decltype(std::forward<F>(f)(std::forward<Arg>(arg))), void >::value, F, Arg>::apply(
		std::forward<F>(f), std::forward<Arg>(arg) );
}

template<typename T>
struct is_not_nonesuch : std::integral_constant<bool, !std::is_same<T, nonesuch>::value> {};

template<typename T>
struct is_nonesuch : std::integral_constant<bool, std::is_same<T, nonesuch>::value> {};


template<typename... Ts>
struct is_every_nonesuch;

template<>
struct is_every_nonesuch<> : std::true_type {};

template<typename T, typename... Ts>
struct is_every_nonesuch<T, Ts...> : std::integral_constant<bool, std::is_same<T, nonesuch>::value && is_every_nonesuch<Ts...>::value> {};

template<typename Tuple, std::size_t ... I>
bool is_tuple_of_nonesuch_impl(std::index_sequence<I...>) {
	return is_every_nonesuch< decltype(std::get<I>(std::declval<Tuple>()))... >::value;
}

template<typename Tuple>
bool is_tuple_of_nonesuch()
{
	return is_tuple_of_nonesuch_impl<Tuple>(std::make_index_sequence<std::tuple_size<std::decay_t<Tuple>>::value>());
}

template<typename F, typename Tuple, std::size_t ... I>
auto apply_each_filtered_impl2(F&& f, Tuple&& t, std::index_sequence<I...>) {
    return std::make_tuple<decltype(result_of_or_nonesuch(std::forward<F>(f), std::get<I>(std::forward<Tuple>(t))))... >( result_of_or_nonesuch(std::forward<F>(f), std::get<I>(std::forward<Tuple>(t)))... );
}

template<template<typename> typename TPred, typename F, typename Tuple, std::size_t ... I>
auto apply_each_filtered_impl1(F&& f, Tuple&& t, std::index_sequence<I...>) {
    using FilteredTuple = decltype(get_tuple<TPred, decltype(std::get<I>(std::forward<Tuple>(t)))...>::apply(std::get<I>(std::forward<Tuple>(t))...));
    using Indices = std::make_index_sequence<std::tuple_size<std::decay_t<FilteredTuple>>::value>;
    return apply_each_filtered_impl2(std::forward<F>(f), std::forward<FilteredTuple>(get_tuple<TPred, decltype(std::get<I>(std::forward<Tuple>(t)))...>::apply(std::get<I>(std::forward<Tuple>(t))...)), Indices());
}

template<template<typename> typename TPred, typename F, typename Tuple>
auto apply_each_filtered(F&& f, Tuple&& t) {
    using Indices = std::make_index_sequence<std::tuple_size<std::decay_t<Tuple>>::value>;
    return apply_each_filtered_impl1<TPred>(std::forward<F>(f), std::forward<Tuple>(t), Indices());
}

struct void_if_nonesuch
{
	auto apply(auto&& result) {
		return std::forward<std::decay<decltype(result)>::type>(result);
	}

	void apply(nonesuch&& result) {
		volatile auto x = std::forward<std::decay<decltype(result)>::type>(result);
	}

};

/** ------------------------------------------------------------------------------------------ */

struct foo {
    foo() {}

    template <typename... Args>
    void operator()(Args&&... args) {
        std::cout << __PRETTY_FUNCTION__ << "\n";
    }
};

struct baz {

};

int main()
{
    char a = 5;
    const int b = 6;
    float c = 7;
    int d = 8;
    float e = 9;
    char f = 10;
    baz g;
    baz h;

    auto t = std::make_tuple(a, b, c, d, std::move(e), f, 3.2, 5, 'c', g, h);
    //auto x = get_tuple<is_same_as_undecorated<char>::apply>::apply(a, b, c, d, std::move(e), f);
    //std::tuple<char*, char*, intPred, int*, float*, float> x{&a, &f, b, &d, &c, std::move(f)};

    apply_filtered<is_same_as_undecorated<int>::apply>(foo(), t);
    apply_filtered<is_same_as_undecorated<float>::apply>(foo(), t);

    auto u = apply_each_filtered<is_same_as_undecorated<char>::apply>(foo(), (const decltype(t))t);
    auto v = apply_each_filtered<is_same_as_undecorated<baz>::apply>(foo(), t);


    apply(foo(), u);
    apply(foo(), v);

    return 0;
}