#include <utility>
#include <type_traits>
#include <tuple>
#include <array>
#include <iostream>

#define HAVE_EXPERIMENTAL_TYPE_TRAITS 1

#ifdef HAVE_EXPERIMENTAL_TYPE_TRAITS
#include <experimental/type_traits>
namespace std
{
	using std::experimental::fundamentals_v1::is_same_v;
};
#else
namespace std
{
	template<typename _Tp, typename _Up>
	constexpr bool is_same_v = is_same<_Tp, _Up>::value;
};
#endif

/* -------------------------------------------------------------
 * @brief char_sequence<char...> wraps an arbitrary sequence of
 * character non-type template arguments in a templated type.
 *
 * The resulting type, an alias for 
 * `std::integer_sequence<char, ...>` is suitable for use as a
 * type template argument, or as an unnamed parameter to a 
 * function template from which a parameter pack of chars in the
 * template arguments can be deduced.
 */

template<size_t i>
using ic = std::integral_constant<size_t, i>;

template <char... chars>
struct char_sequence : public std::integer_sequence<char, chars...>
{
	/**
	 * @brief element access by index using get(index, "char sequence"_chars)
	 */
	
	template <size_t n, char ch, char... cs>
	struct at_helper	
	{
	  static const char value = at_helper<n-1, cs...>::value;
	};

	template <char ch, char... cs>
	struct at_helper<0, ch, cs...>
	{
	  static const char value = ch;
	};

	template<size_t n>
	constexpr char at(std::integral_constant<size_t, n> = {})
	{
		return at_helper<n, chars...>::value;
	}

	template<size_t n>
	constexpr char operator[](std::integral_constant<size_t, n>)
	{
		return at(std::integral_constant<size_t, n>());
	}

	/*
	constexpr char operator[](int n)
	{
		return at(std::integral_constant<size_t, n>());
	}
	*/
};

/*
 * @brief a user-defined string literal operator which returns
 * a constexpr char_sequence
 *
 * The resulting constexpr char_sequence can be converted to a 
 * type using decltype() or passed to an unnamed parameter of a
 * templated function, which can then deduce a parameter pack of
 * chars in it's template arguments from it.
 */

template <typename T, T... chars>
constexpr char_sequence<chars...> operator"" _chars()
{ return {}; }

/**
 * @brief combine two char_sequence types into a char_sequence of the 
 * characters from the first followed by the characters from the second.
 */

template <char... chars1, char... chars2>
constexpr auto concat(char_sequence<chars1...>, char_sequence<chars2...>)
  -> char_sequence<chars1..., chars2...>
{ return {}; };

/**
 * @brief some static assertions and basic functionality examples
 */

int main()
{
	// test user defined string literals
	static_assert(std::is_same_v<decltype("abc"_chars), char_sequence<'a', 'b', 'c'>>, "should be same type");

	// test `at` metafunction, extracts the Nth character in a sequence type
	static_assert(std::is_same_v<
			decltype(
				concat(
						"uvw"_chars, 
						"xyz"_chars
				)
			),
			decltype("uvwxyz"_chars)
	>, "should be true");

	static_assert(
		"pqrst"_chars.at(std::integral_constant<size_t, 2>()) == 'r'
		, "should be true");

	static_assert(
		"pqrst"_chars[ic<2>()] == 'r'
		, "should be true");
}
