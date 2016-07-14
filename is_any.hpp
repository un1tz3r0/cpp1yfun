#pragma once

#include <type_traits>
#include <integral_constant>


// ------------------------------------------------------------------------------
// stuff that should be in <type_traits> but isn't...


template<template<typename> class Predicate, typename... Rest>
struct is_any : std::false_type {};

template<template<typename> class Predicate, typename First>
struct is_any<Predicate, First> : std::integral_constant<bool, Predicate<First>::value> {};

template<template<typename> class Predicate, typename First, typename... Rest>
struct is_any<Predicate, First, Rest...> : std::integral_constant<bool, (
		Predicate<First>::value || is_any<Predicate, Rest...>::value
	)> {};

// ---------------------------------------------------------------------------------
// variadic version of std::is_same

template<typename T, typename... Rest>
struct is_any_same : std::false_type {};

template<typename T, typename First>
struct is_any_same<T, First> : std::is_same<T, First> {};

template<typename T, typename First, typename... Rest>
struct is_any_same<T, First, Rest...>
    : std::integral_constant<bool, std::is_same<T, First>::value || is_any_same<T, Rest...>::value>
{};
