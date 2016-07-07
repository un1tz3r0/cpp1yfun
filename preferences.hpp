#ifndef NO_PREFERENCES_HPP

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

// ------------------------------------------------------------------------------
// stuff that should be in <type_traits> but isn't...

// variadic version of std::is_same

template<typename T, typename... Rest>
struct is_any : std::false_type {};

template<typename T, typename First>
struct is_any<T, First> : std::is_same<T, First> {};

template<typename T, typename First, typename... Rest>
struct is_any<T, First, Rest...>
    : std::integral_constant<bool, std::is_same<T, First>::value || is_any<T, Rest...>::value>
{};

// shorthand integer constant type

template<int N>
using int_c_t = std::integral_constant<int, N>;

// some metaprogramming sugar

template<template<class...>class Z, class T> struct is_template_instance: std::false_type {};
template<template<class...>class Z, class... Ts> struct is_template_instance<Z,Z<Ts...>>: std::true_type {};

// ------------------------------------------------------------------------------
// platform glue for string handling, uses STL strings except on Arduino, where
// we use the lighter-weight Arduino built-in String class

#ifdef ARDUINO

#include <EEPROM.h>

using string = String;

template<typename T>
string format(const T& val)
{
	return (string(val));
}

template<typename T>
T parse(const string& s);

template<>
bool parse<bool>(const string& s)
{
	return !!(s.toInt());
}

template<>
int parse<int>(const string& s)
{
	return s.toInt();
}

template<>
float parse<float>(const string& s)
{
	return s.toFloat();
}

// ---------------------------------------------------------------------
// countwords and nthword are used by the prefs_cli class to parse very
// simple command line syntax used to communicated over serial link with
// a user interface.
// ---------------------------------------------------------------------

int countwords(const String& line, char delim)
{
	if(line.length() <= 0)
		return 0;
	int pos = -1, i = 0;
	do { i++; pos = line.indexOf(delim, pos + 1); } while(pos >= 0);
	return i;
}

String nthword(const String& line, char delim, int n)
{
	int pos = 0, newpos = -1;
	for(int i = 0; i <= n; i++)
	{
		pos = newpos + 1;
		newpos = line.indexOf(delim, pos);
		if(newpos < 0)
		{
			newpos = line.length();
			break;
		}	
	}
	return line.substring(pos, newpos);
}

// -------------------------------------------------------------------------------
// linebuffer is a support class used by the prefs_cli class below. it implements
// a simple, line buffered wrapper around an arduino stream device, such as the
// USART hardware and USB-HID CDC/ACM devices
// -------------------------------------------------------------------------------

template<class IOS>
class linebuffer
{
	IOS &stream;
	string line;
	bool oflowing, oflowed, ready;
	const int maxlen;
public:
	explicit linebuffer(IOS &stream_, int maxlen_): 
		stream(stream_), line(), oflowed(false), oflowing(false), ready(false), maxlen(maxlen_)
	{
		line.reserve(maxlen);
	}

	~linebuffer()
	{
	}

	// not copyable or assignable
	
	linebuffer(const linebuffer&) = delete;

	linebuffer& operator=(const linebuffer&) = delete;

	// check if the last line returned by getline was longer than the buffer

	bool overflow() {
		return oflowed;
	}

	// buffer chars until one or more end-of-line chars are seen, returning
	// true with the buffered line placed in result reference parameter

	bool getline(string &result)
	{
		bool ret = false;
		if(ready)
		{
			result = line;
			line = "";
			ret = true;
			ready = false;
			oflowed = oflowing;
			oflowing = false;
		}
		while(stream.available())
		{
			if(!ready)
			{
				int ch = stream.read();
				if(ch == '\n' || ch == '\r')
					ready = true;
				else if(line.length() < maxlen)
					line.append((char)ch);
				else
					oflowing = true;
			}
			else
			{
				int ch = stream.peek();
				if(ch == '\n' || ch == '\r')
				{
					ch = stream.read();
					continue;
				}
				break;
			}
		}
		return ret;
	}

};


// ------------------------------------------------------------------
#else
// ------------------------------------------------------------------
// non-Arduino string handling glue - uses STL strings and iostreams

#include <iostream>
#include <string>
#include <sstream>

using std::string;

template<typename T>
string format(const T& val)
{
	std::ostringstream oss;
	oss << val;
	return oss.str();
}

template<typename T>
T parse(const string& s, T s)
{
	std::istringstream iss(s);
	T temp;
	iss >> temp;
	return temp;
}

template<>
bool parse<bool>(const string& s)
{
	if(s=="true" || s=="t")
		return true;
	if(s=="false" || s=="f")
		return false;
	return !!parse<int>(s);
}

// --------------------------------------------------------------
#endif

// --------------------------------------------------------------
// type-description helpers used by preference and type-erasing 
// internal polymorphic implementation template classes
// --------------------------------------------------------------

namespace detail {
	template<typename T> string type_to_str(); // { return "?"; }
	template<> string type_to_str<bool>() { return ("bool"); }
	template<> string type_to_str<int>() { return ("int"); }
	//template<> string type_to_str<signed int>() { return "sint"; }
	//template<> string type_to_str<unsigned int>() { return "uint"; }
	template<> string type_to_str<float>() { return ("float"); }
	template<> string type_to_str<double>() { return ("double"); }
	/*template<> string type_to_str<signed char>() { return "schar"; }
	template<> string type_to_str<unsigned char>() { return "uchar"; }
	template<> string type_to_str<signed short>() { return "short"; }
	template<> string type_to_str<signed long long>() { return "long"; }
	template<> string type_to_str<unsigned short>() { return "ushort"; }
	template<> string type_to_str<unsigned long long>() { return "ullong"; }*/
};

template<typename T, typename = decltype(&(detail::type_to_str<T>))> string type_to_str()
{
	return (detail::type_to_str<T>());
}

// ==============================================================
// preferences class
// 
// a clean, user-friendly, non-verbose and fairly versatile C++
// arduino compatible library for exposing global parameters via
// two-way serial connection.
// 
// the meat of this unweildy and questionably crafted sandwich
// ---------------------------------------------------------------

class preference
{
public:

	class impl_base
	{
		string _name;
	protected:
		static int _next_saved_index;
	public:
		impl_base(const string& name_): _name(name_) {}
		string const & name() const { return _name; }
		virtual string kind() const = 0;		// returns a string describing the underlying data type
		virtual bool get(string&) const = 0;	// returns a string representation of the value
		virtual bool set(const string&) = 0;	// parses the given string and sets the value to the result
		virtual string minimum() const = 0;		// returns "none" or a string representation of the lower bound
		virtual string maximum() const = 0;		// same as above, but for the upper bound
		virtual bool bounded() const = 0;		// returns true if minimum() and maximum() are not "none"
		virtual string initial() const = 0;		// returns a string representation of the default value
		virtual void reset() = 0;				// initializes the underlying storage to the default value
		virtual bool persisted() const = 0;		// returns true if save() and load() are not empty
		virtual void save() const = 0;			// optionally writes the underlying value to persistent storage
		virtual void load() = 0;				// optionally restores the previously save()d value
		//virtual impl_base* clone() const = 0;	// used to implement type-erased copy constructor
	};

	template<typename T>
	class impl_trivial : public impl_base
	{
		T & _ref;
		const T _def;
		const int _index;
	protected:
		impl_trivial(const impl_trivial& other):
			impl_base(other.name()), _ref(other._ref),  _def(other._def),
			_index(other._index) {}
		virtual bool _set(const T& val) { _ref = val; return true; }
		virtual T _get() const { return _ref; }
	public:
		impl_trivial(const string& name_, T& ref_, const T& def_, bool saved_):
			impl_base(name_), _ref(ref_),  _def(def_),
			_index(saved_ ? (_next_saved_index += sizeof(T)) - sizeof(T) : -1)
		{}
		//impl_trivial& operator=(const impl_trivial&) = delete;
		~impl_trivial() {};
		//virtual impl_base* clone() const { return new impl_trivial(*this); }
		virtual string kind() const { return (type_to_str<T>()); }
		virtual bool get(string& ret) const { ret = format(_get()); return true; }
		virtual bool set(const string& val) { _set(parse<T>(val)); return true; }
		virtual string minimum() const { return ("none"); }
		virtual string maximum() const { return ("none"); }
		virtual bool bounded() const { return false; }
		virtual string initial() const { return (format(_def)); }
		virtual void reset() { _set(_def); }
		virtual bool persisted() const { return _index >= 0; }
		virtual void save() const { if(_index >= 0){ T temp = _get(); EEPROM.put(_index, temp); }  }
		virtual void load() { if(_index >= 0){ T temp; EEPROM.get(_index, temp); _set(temp); } }
	};

	template<typename T, typename G, typename S>
	class impl_functional : public impl_base
	{
		std::function<T(void)> _getter;
		std::function<void(T)> _setter;
		const T _def;
		const int _index;
	protected:
/*		impl_functional(const impl_functional& other):
			impl_base(other.name()), _getter(other._getter), _setter(other._setter),
			 _def(other._def), _index(other._index) {}*/
		virtual bool _set(const T& val) { _setter(val); return true; }
		virtual T _get() const { return _getter(); }
	public:
		impl_functional(const string& name_, G g_, S s_, const T& def_, bool saved_):
			impl_base(name_), _getter(g_), _setter(s_), _def(def_),
			_index(saved_ ? (_next_saved_index += sizeof(T)) - sizeof(T) : -1)
		{}
		//impl_functional& operator=(const impl_functional&) = delete;
		~impl_functional() {};
		//virtual impl_base* clone() const { return new impl_functional(*this); }
		virtual string kind() const { return (type_to_str<T>()); }
		virtual bool get(string& ret) const { ret = format(_get()); return true; }
		virtual bool set(const string& val) { _set(parse<T>(val)); return true; }
		virtual string minimum() const { return ("none"); }
		virtual string maximum() const { return ("none"); }
		virtual bool bounded() const { return false; }
		virtual string initial() const { return (format(_def)); }
		virtual void reset() { _set(_def); }
		virtual bool persisted() const { return _index >= 0; }
		virtual void save() const { if(_index >= 0){ T temp = _get(); EEPROM.put(_index, temp); }  }
		virtual void load() { if(_index >= 0){ T temp; EEPROM.get(_index, temp); _set(temp); } }
	};



	template<typename T>
	class impl_bounded_trivial : public impl_trivial<T>
	{
		const T _min, _max;
	protected:
/*		impl_bounded_trivial(const impl_bounded_trivial& other):
			impl_trivial<T>(other),  _min(other._min), _max(other._max)
		{}*/
		virtual bool _set(const T& val) { return impl_trivial<T>::_set( (val < _min) ? (_min) : ((val > _max) ? (_max) : (val)) ); }
	public:
		impl_bounded_trivial(const string& name_, T& ref_, const T& def_, const T& min_, const T& max_, bool saved_):
			impl_trivial<T>(name_, ref_, def_, saved_), _min(min_), _max(max_)
		{}
		~impl_bounded_trivial() {};
		//virtual impl_base* clone() const { return new impl_bounded_trivial(*this); }
		virtual bool set(const string& val) { return _set(parse<T>(val)); }
		virtual string minimum() const { return (format(_min)); }
		virtual string maximum() const { return (format(_max)); }
		virtual bool bounded() const { return true; }
	};

	template<typename T, typename G, typename S>
	class impl_bounded_functional : public impl_functional<T, G, S>
	{
		const T _min, _max;
	protected:
/*		impl_bounded_functional(const impl_bounded_functional& other):
			impl_functional<T>(other),  _min(other._min), _max(other._max)
		{}*/
		virtual bool _set(const T& val) { return impl_functional<T, G, S>::_set( (val < _min) ? (_min) : ((val > _max) ? (_max) : (val)) ); }
	public:
		impl_bounded_functional(const string& name_, G g_, S s_, const T& def_, const T& min_, const T& max_, bool saved_):
			impl_functional<T, G, S>(name_, g_, s_, def_, saved_), _min(min_), _max(max_) {}
		~impl_bounded_functional() {};
		//virtual impl_base* clone() const { return new impl_bounded_functional(*this); }
		virtual bool set(const string& val) { return _set(parse<T>(val)); }
		virtual string minimum() const { return (format(_min)); }
		virtual string maximum() const { return (format(_max)); }
		virtual bool bounded() const { return true; }
	};

private:

	std::shared_ptr<impl_base> pimpl;

public:

	preference():
		pimpl() {}
	
	preference(const preference& other):
		pimpl(other.pimpl) {}

	preference& operator=(const preference& other)
	{
		pimpl = other.pimpl;
		return *this;
	}

	/*preference(const string& name_, float& value_, float def_, float min_ float max_, bool saved_): 
		pimpl(new impl_bounded<float>(name_, value_, def_, min_, max_, saved_)) {}
	
	preference(const string& name_, int& value_, int def_, int min_ int max_, bool saved_): 
		pimpl(new impl_bounded<int>(name_, value_, def_, min_, max_, saved_)) {}*/

	template<typename T, typename = typename std::enable_if< is_any<T, int, float, bool>::value >::type >
	preference(const string& name_, T& value_, T def_, bool saved_): 
		pimpl(std::make_shared<impl_trivial<T>>(name_, value_, def_, saved_)) {}
	
	template<typename T, typename = typename std::enable_if< is_any<T, int, float, bool>::value >::type >
	preference(const string& name_, T& value_, T def_, T min_, T max_, bool saved_): 
		pimpl(std::make_shared<impl_bounded_trivial<T>>(name_, value_, def_, min_, max_, saved_)) {}

	template<typename T, typename G, typename S, typename = typename std::enable_if<is_callable<G, T()>::value && is_callable<S, void(T)>::value && is_any<T, int, float, bool>::value>::type>
	preference(const string& name_, G g_, S s_, T def_, bool saved_): 
		pimpl(std::make_shared<impl_functional<T, G, S>>(name_, g_, s_, def_, saved_)) {}
	
	template<typename T, typename G, typename S, typename = typename std::enable_if<is_callable<G, T()>::value && is_callable<S, void(T)>::value && is_any<T, int, float, bool>::value>::type>
	preference(const string& name_, G g_, S s_, T def_, T min_, T max_, bool saved_): 
		pimpl(std::make_shared<impl_bounded_functional<T, G, S>>(name_, g_, s_, def_, min_, max_, saved_)) {}

	/*preference(const string& name_, bool& value_, bool def_, bool saved_): 
		pimpl(new impl_trivial<bool>(name_, value_, def_, saved_)) {} */

	string name() const { if(!pimpl) return ""; else return pimpl->name(); }
	bool get(string& ret) const { if(!pimpl) return false; else return pimpl->get(ret); }
	bool set(const string& val) { if(!pimpl) return false; else return pimpl->set(val); }
	string kind() const { if(!pimpl) return ("empty"); else return (pimpl->kind()); }
	string minimum() const { if(!pimpl) return ("none"); else return (pimpl->minimum()); }
	string maximum() const { if(!pimpl) return ("none"); else return (pimpl->maximum()); }
	bool bounded() const { if(!pimpl) return false; else return pimpl->bounded(); }
	void reset() { if(!!pimpl) pimpl->reset(); }
	string initial() const { if(!pimpl) return ("none"); else return (pimpl->initial()); }
	bool persisted() const { if(!pimpl) return false; else return pimpl->persisted(); }
	void load() { if(!!pimpl) pimpl->load(); }
	void save() { if(!!pimpl) pimpl->save(); }
};

int preference::impl_base::_next_saved_index = sizeof(unsigned long);

// ---------------------------------------------------------------------------------

#ifdef __MK20DX256__

void reboot_soft()
{
	Serial.println("!!! HALTING !!!");
	Serial2.println("!!! HALTING !!!");
	delay(1000);
	*((volatile uint32_t*)0xE000ED0Cul) = 0x05FA0001ul;
	for(;;) {}
}



namespace std {
	void __throw_bad_function_call()
	{
		Serial.println("!!! __throw_bad_function_call() invoked !!!");
		Serial2.println("!!! __throw_bad_function_call() invoked !!!");
		delay(1000);
		reboot_soft();
	}

	void __throw_bad_alloc() 
	{
		Serial.println("!!! __throw_bad_alloc() invoked !!!");
		Serial2.println("!!! __throw_bad_alloc() invoked !!!");
		delay(1000);
		reboot_soft();
	}
}

#endif

// ---------------------------------------------------------------------------------

// ------------------------------------------------------------------
// CLI for Arduino stream devices
// 
// Fully implements a line-buffered simple command line interface for
// listing, querying and changing a set of preferences
// ------------------------------------------------------------------


template<class IOS>
class prefs_cli
{
	std::vector<preference> preferences;
	IOS &ios;
	linebuffer<IOS> linebuf;
	unsigned long version;
	template<class> friend class prefs_cli;
public:
	template<typename Q>
	prefs_cli(IOS& ios_, const prefs_cli<Q>& other): preferences(other.preferences), version(other.version), ios(ios_), linebuf(ios_, 100) {}
	prefs_cli(IOS& ios_, unsigned long ver, const std::initializer_list<preference>& prefs_): preferences(prefs_), version(ver), ios(ios_), linebuf(ios_, 100) {}

	void print_pref(const preference& pref)
	{
			string s;
			ios.print(pref.kind());
			ios.print(" ");
			ios.print(pref.name());
			if(pref.bounded())
			{
				ios.print(" ");
				ios.print(pref.minimum());
				ios.print(" ");
				ios.print(pref.maximum());
			}
			else
			{
				ios.print(" none none");
			}
			ios.print(" ");
			ios.print(pref.initial());
			ios.print(" ");
			pref.get(s);
			ios.print(s);
			ios.println();
		
	}

	void list_prefs()
	{
		ios.println("begin");
		for(auto pref : preferences)
		{
			print_pref(pref);
		}
		ios.println("end");
	}

	bool load_prefs()
	{
		unsigned long cur_version;
		EEPROM.get(0, cur_version);
		if(cur_version != version)
			return false;
		for(auto pref : preferences)
		{
			pref.load();
		}
		return true;
	}

	void save_prefs()
	{
		EEPROM.put(0, version);
		for(auto pref : preferences)
		{
			pref.save();
		}
	}

	void reset_prefs()
	{
		for(auto pref : preferences)
		{
			pref.reset();
		}
	}

	bool has_pref(const string& name) const
	{
		for(auto pref : preferences)
		{
			if(pref.name() == name)
				return true;
		}
		return false;	
	}

	preference get_pref(const string& name) const
	{
		for(auto pref : preferences)
		{
			if(pref.name() == name)
				return pref;
		}/*
		if((name.length() > 0) && (name[0] >= '0') && (name[0] <= '9'))
		{
			int i = -1;
			i = parse<int>(name);
			if((i >= 0) && (i < preferences.size()))
				return preferences[i];
		}*/
		return preference();
	}

	bool set_pref(const string& name, const string& value)
	{
		return get_pref(name).set(value);
	}

	void loop()
	{
		string line;
		while(linebuf.getline(line))
		{
			if(linebuf.overflow())
			{
				ios.println("!error: line too long");
				continue;
			}
			
			if(line.length() <= 0)
				continue;

			int nwords = countwords(line, ' ');
			string cmd = nthword(line, ' ', 0);
			string p1 = nthword(line, ' ', 1);
			string p2 = nthword(line, ' ', 2);

			if((cmd == "list") && (nwords == 1))
			{
				list_prefs();
			}
			else if((cmd == "get") && (nwords == 2)) 
			{
				print_pref(get_pref(p1));
			}
			else if((cmd == "set") && (nwords == 3))
			{
				preference pref = get_pref(p1);
				if(pref.name() == "")
				{
					ios.println("!error: set bad name '" + p1 + "'");
				}
				else if(!pref.set(p2))
				{
					ios.println("!error: set bad value '" + p2 + "'");
				}
				else
				{
					print_pref(pref);
				}
			}
			else if((cmd == "save") && (nwords == 1))
			{
				save_prefs();
			}
			else if((cmd == "load") && (nwords == 1))
			{
				load_prefs();
			}
			else if((cmd == "reset") && (nwords == 1))
			{
				reset_prefs();
			}
			else
			{
				ios.println("!error: '" + line + "' invalid command");
			}
		}
	}

};

// ============================================================================================
#endif