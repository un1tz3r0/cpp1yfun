#pragma once

#include <initializer_list>
#include <vector>
#include <functional>
#include <type_traits>
#include <utility>
#include <memory>

//#include <experimental/variant>

// ==============================================================================
// oof... here is an is_callable<T, Sig> that can be used with enable_if to detect
// whether T is a functor (free function or object with operator()) which can be 
// called with signature Sig. NEAT!
// ==============================================================================

namespace // implementation detail
{
    // build R (*)(Args...) from R (Args...)
    // compile error if signature is not a valid function signature
    template <typename, typename>
    struct build_free_function;

    template <typename F, typename R, typename ... Args>
    struct build_free_function<F, R (Args...)>
    { using type = R (*)(Args...); };

    // build R (C::*)(Args...) from R (Args...)
    //       R (C::*)(Args...) const from R (Args...) const
    //       R (C::*)(Args...) volatile from R (Args...) volatile
    // compile error if signature is not a valid member function signature
    template <typename, typename>
    struct build_class_function;

    template <typename C, typename R, typename ... Args>
    struct build_class_function<C, R (Args...)>
    { using type = R (C::*)(Args...); };

    template <typename C, typename R, typename ... Args>
    struct build_class_function<C, R (Args...) const>
    { using type = R (C::*)(Args...) const; };

    template <typename C, typename R, typename ... Args>
    struct build_class_function<C, R (Args...) volatile>
    { using type = R (C::*)(Args...) volatile; };

    // determine whether a class C has an operator() with signature S
    template <typename C, typename S>
    struct is_functor_with_signature
    {
        typedef char (& yes)[1];
        typedef char (& no)[2];

        // helper struct to determine that C::operator() does indeed have
        // the desired signature; &C::operator() is only of type 
        // R (C::*)(Args...) if this is true
        template <typename T, T> struct check;

        // T is needed to enable SFINAE
        template <typename T> static yes deduce(check<
            typename build_class_function<C, S>::type, &T::operator()> *);
        // fallback if check helper could not be built
        template <typename> static no deduce(...);

        static bool constexpr value = sizeof(deduce<C>(0)) == sizeof(yes);
    };

    // determine whether a free function pointer F has signature S
    template <typename F, typename S>
    struct is_function_with_signature
    {
        // check whether F and the function pointer of S are of the same
        // type
        static bool constexpr value = std::is_same<
            F, typename build_free_function<F, S>::type
        >::value;
    };

    // C is a class, delegate to is_functor_with_signature
    template <typename C, typename S, bool>
    struct is_callable_impl
        : std::integral_constant<
            bool, is_functor_with_signature<C, S>::value
          >
    {};

    // F is not a class, delegate to is_function_with_signature
    template <typename F, typename S>
    struct is_callable_impl<F, S, false>
        : std::integral_constant<
            bool, is_function_with_signature<F, S>::value
          >
    {};
}

// Determine whether type Callable is callable with signature Signature.
// Compliant with functors, i.e. classes that declare operator(); and free
// function pointers: R (*)(Args...), but not R (Args...)!
template <typename Callable, typename Signature>
struct is_callable
    : is_callable_impl<
        Callable, Signature,
        std::is_class<Callable>::value
      >
{};
