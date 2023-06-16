#pragma once
#include <type_traits>
#include <typeinfo>
#include <concepts>
#include <cstddef>
#include <array>
#include <utility>
#include <string_view>
#include <algorithm>

namespace kaixo {
    constexpr std::size_t npos = static_cast<std::size_t>(-1);

    template<std::size_t N, class CharType = char>
    struct string_literal {
        constexpr static std::size_t npos = std::basic_string_view<CharType>::npos;

        using view_type = std::basic_string_view<CharType>;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;
        using value_type = CharType;
        using reference = CharType&;
        using const_reference = const CharType&;
        using pointer = CharType*;
        using const_pointer = const CharType*;

        class const_iterator {
        public:
            using iterator_category = std::random_access_iterator_tag;
            using size_type = std::size_t;
            using difference_type = std::ptrdiff_t;
            using value_type = const CharType;
            using reference = const CharType&;

            constexpr const_iterator(const const_iterator&) = default;
            constexpr const_iterator(const_iterator&&) = default;
            constexpr const_iterator& operator=(const const_iterator&) = default;
            constexpr const_iterator& operator=(const_iterator&&) = default;
            constexpr const_iterator() : m_Ptr(nullptr) {}
            constexpr const_iterator(const CharType* ptr) : m_Ptr(ptr) {}

            constexpr reference operator*() const { return *m_Ptr; }
            constexpr const_iterator& operator+=(difference_type d) { m_Ptr += d; return *this; }
            constexpr const_iterator& operator-=(difference_type d) { m_Ptr -= d; return *this; }
            constexpr const_iterator& operator++() { ++m_Ptr; return *this; }
            constexpr const_iterator& operator--() { --m_Ptr; return *this; }
            constexpr const_iterator operator++(int) { auto _c = *this; ++m_Ptr; return _c; }
            constexpr const_iterator operator--(int) { auto _c = *this; --m_Ptr; return _c; }

            constexpr reference operator[](difference_type d) const { return m_Ptr[d]; }

            constexpr auto operator<=>(const const_iterator& other) const = default;

            friend constexpr const_iterator operator+(difference_type a, const const_iterator& b) { return { a + b.m_Ptr }; }
            friend constexpr const_iterator operator+(const const_iterator& a, difference_type b) { return { a.m_Ptr + b }; }
            friend constexpr const_iterator operator-(difference_type a, const const_iterator& b) { return { a - b.m_Ptr }; }
            friend constexpr const_iterator operator-(const const_iterator& a, difference_type b) { return { a.m_Ptr - b }; }
            friend constexpr difference_type operator-(const const_iterator& a, const const_iterator& b) { return a.m_Ptr - b.m_Ptr; }
        protected:
            const CharType* m_Ptr;
        };

        class iterator : public const_iterator {
        public:
            using iterator_category = std::random_access_iterator_tag;
            using size_type = std::size_t;
            using difference_type = std::ptrdiff_t;
            using value_type = CharType;
            using reference = CharType&;

            constexpr iterator(const iterator&) = default;
            constexpr iterator(iterator&&) = default;
            constexpr iterator& operator=(const iterator&) = default;
            constexpr iterator& operator=(iterator&&) = default;
            constexpr iterator() : const_iterator(nullptr) {}
            constexpr iterator(CharType* ptr) : const_iterator(ptr) {}

            constexpr reference operator*() const { return *const_cast<CharType*>(this->m_Ptr); }
            constexpr iterator& operator+=(difference_type d) { this->m_Ptr += d; return *this; }
            constexpr iterator& operator-=(difference_type d) { this->m_Ptr -= d; return *this; }
            constexpr iterator& operator++() { ++this->m_Ptr; return *this; }
            constexpr iterator& operator--() { --this->m_Ptr; return *this; }
            constexpr iterator operator++(int) { auto _c = *this; ++this->m_Ptr; return _c; }
            constexpr iterator operator--(int) { auto _c = *this; --this->m_Ptr; return _c; }

            constexpr reference operator[](difference_type d) const { return const_cast<CharType*>(this->m_Ptr)[d]; }

            constexpr auto operator<=>(const iterator& other) const = default;

            friend constexpr iterator operator+(difference_type a, const iterator& b) { return { a + b.m_Ptr }; }
            friend constexpr iterator operator+(const iterator& a, difference_type b) { return { a.m_Ptr + b }; }
            friend constexpr iterator operator-(difference_type a, const iterator& b) { return { a - b.m_Ptr }; }
            friend constexpr iterator operator-(const iterator& a, difference_type b) { return { a.m_Ptr - b }; }
            friend constexpr difference_type operator-(const iterator& a, const iterator& b) { return a.m_Ptr - b.m_Ptr; }
        };

        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

        constexpr ~string_literal() = default;
        constexpr string_literal() = default;
        constexpr string_literal(const CharType(&data)[N]) {
            std::copy_n(data, N, m_Data);
        }

        constexpr string_literal(std::string_view data) {
            std::copy_n(data.data(), N, m_Data);
            m_Data[N - 1] = '\0';
        }

        constexpr string_literal(string_literal&&) = default;
        constexpr string_literal(const string_literal&) = default;
        constexpr string_literal& operator=(string_literal&&) = default;
        constexpr string_literal& operator=(const string_literal&) = default;

        template<std::size_t I> requires (I < N)
            constexpr string_literal& operator=(const CharType(&data)[I]) {
            std::copy_n(data, I, m_Data);
        }

        constexpr iterator begin() { return { m_Data }; }
        constexpr iterator end() { return { m_Data + size() }; }
        constexpr const_iterator begin() const { return { m_Data }; }
        constexpr const_iterator end() const { return { m_Data + size() }; }
        constexpr const_iterator cbegin() const { return begin(); }
        constexpr const_iterator cend() const { return end(); }
        constexpr reverse_iterator rbegin() { return end(); }
        constexpr reverse_iterator rend() { return begin(); }
        constexpr const_reverse_iterator rbegin() const { return end(); }
        constexpr const_reverse_iterator rend() const { return begin(); }
        constexpr const_reverse_iterator crbegin() const { return end(); }
        constexpr const_reverse_iterator crend() const { return begin(); }

        constexpr reference at(size_type d) { return m_Data[d]; }
        constexpr const_reference at(size_type d) const { return m_Data[d]; }
        constexpr reference operator[](size_type d) { return m_Data[d]; }
        constexpr const_reference operator[](size_type d) const { return m_Data[d]; }
        constexpr reference front() { return m_Data[0]; }
        constexpr const_reference front() const { return m_Data[0]; }
        constexpr reference back() { return m_Data[size() - 1]; }
        constexpr const_reference back() const { return m_Data[size() - 1]; }

        constexpr pointer data() { return m_Data; }
        constexpr const_pointer data() const { return m_Data; }
        constexpr const_pointer c_str() const { return m_Data; }
        constexpr size_type size() const { return N - 1; }
        constexpr size_type length() const { return size(); }
        constexpr size_type max_size() const { return size(); }
        constexpr bool empty() const { return false; }
        constexpr void swap(string_literal& other) { std::swap(data(), other.data()); }

        constexpr view_type view() const { return { data(), size() }; }
        constexpr operator view_type() const { return { data(), size() }; }

        template<std::size_t I>
        constexpr auto operator==(const string_literal<I, CharType>& other) const {
            if constexpr (I != N) return false;
            else return view() == other.view();
        };
        template<std::size_t I>
        constexpr auto operator<=>(const string_literal<I, CharType>& other) const { return view() <=> other.view(); }
        template<class Ty>
        constexpr auto operator==(Ty&& val) const { return view() == std::forward<Ty>(val); }
        template<class Ty>
        constexpr auto operator<=>(Ty&& val) const { return view() <=> std::forward<Ty>(val); }

        constexpr auto starts_with(view_type t) const { return view().starts_with(t); }
        constexpr auto ends_with(view_type t) const { return view().ends_with(t); }
        constexpr auto substr(size_type pos = 0, size_type count = npos) const { return view().substr(pos, count); }
        constexpr auto find(std::string_view t, size_type pos = 0) const { return view().find(t, pos); }
        constexpr auto rfind(view_type t, size_type pos = 0) const { return view().rfind(t, pos); }
        constexpr auto find_first_of(view_type t, size_type pos = 0) const { return view().find_first_of(t, pos); }
        constexpr auto find_first_not_of(view_type t, size_type pos = 0) const { return view().find_first_not_of(t, pos); }
        constexpr auto find_last_of(view_type t, size_type pos = 0) const { return view().find_last_of(t, pos); }
        constexpr auto find_last_not_of(view_type t, size_type pos = 0) const { return view().find_last_not_of(t, pos); }

        CharType m_Data[N]{};
    };

    template<class ...Tys> struct info;
    template<class ...Args> struct template_pack;

    /**
     * Find the closes larger power of 2.
     * @param v value
     * @return closes larger power of 2 from value
     */
    template <std::unsigned_integral Ty>
    constexpr Ty closest_larger_power2(Ty v) {
        return v > 1ull ? 1ull << (sizeof(Ty) * CHAR_BIT - std::countl_zero(v - 1ull)) : v;
    }
    static_assert(closest_larger_power2(2ull) == 2);
    static_assert(closest_larger_power2(3ull) == 4);
    static_assert(closest_larger_power2(33ull) == 64);
    static_assert(closest_larger_power2(128ull) == 128);

    /**
     * Find the next multiple of a number.
     * @param num number
     * @param multiple multiple
     * @return next multiple of num
     */
    template<class Ty>
    constexpr Ty next_multiple(Ty num, Ty multiple) {
        if (multiple == 0) return num;

        const auto remainder = num % multiple;
        if (remainder == 0) return num;

        return num + multiple - remainder;
    }
    static_assert(next_multiple(2, 0) == 2);
    static_assert(next_multiple(2, 5) == 5);
    static_assert(next_multiple(5, 5) == 5);
    static_assert(next_multiple(13, 12) == 24);

    /**
     * Change a type to Ty, useful in fold expressions.
     * @tparam Ty type to change to
     */
    template<class, class Ty> using change = Ty;

    /**
     * Templated for, calls lambda with index sequence in pack,
     * requires S < E
     * @tparam S start value, or size when E is left at npos
     * @tparam E end value, or nothing when npos
     */
    template<std::integral auto S, std::integral auto E = npos> constexpr auto sequence = [](auto lambda) {
        if constexpr (E == npos)
            return[&] <std::size_t ...Is>(std::integer_sequence<decltype(S), Is...>) {
            return lambda.operator() < (Is)... > ();
        }(std::make_integer_sequence<decltype(S), S>{});
        else return[&] <auto ...Is>(std::integer_sequence<decltype(E - S), Is...>) {
            return lambda.operator() < (Is + S)... > ();
        }(std::make_integer_sequence<decltype(E - S), E - S>{});
    };

    /**
     * Templated for, calls lambda with index sequence in reverse in pack,
     * requires S < E
     * @tparam S start value, or size when E is left at npos
     * @tparam E end value, or nothing when npos
     */
    template<std::integral auto S, std::integral auto E = npos> constexpr auto reverse_sequence = [](auto lambda) {
        if constexpr (E == npos)
            return[&] <std::size_t ...Is>(std::integer_sequence<decltype(S), Is...>) {
            return lambda.operator() < (S - Is - 1)... > ();
        }(std::make_integer_sequence<decltype(S), S>{});
        else return[&] <auto ...Is>(std::integer_sequence<decltype(E - S), Is...>) {
            return lambda.operator() < ((E + S) - (Is + S) - 1)... > ();
        }(std::make_integer_sequence<decltype(E - S), E - S>{});
    };

    /**
     * Templated for, calls lambda with all indices separately,
     * requires S < E
     * @tparam S start value, or size when E is left at npos
     * @tparam E end value, or nothing when npos
     */
    template<std::integral auto S, std::integral auto E = npos> constexpr auto indexed_for =
        []<class Ty>(Ty && lambda) {
        if constexpr (E == npos)
            [&] <auto ...Is>(std::integer_sequence<decltype(S), Is...>) {
            (lambda.operator() < Is > (), ...);
        }(std::make_integer_sequence<decltype(S), S>{});
        else[&] <auto ...Is>(std::integer_sequence<decltype(E - S), Is...>) {
            (lambda.operator() < Is + S > (), ...);
        }(std::make_integer_sequence<decltype(E - S), E - S>{});
    };

    /**
     * Class that's convertible to every type.
     */
    struct convertible_to_everything {
        template<class Ty> constexpr operator Ty& ();
        template<class Ty> constexpr operator Ty && ();
    };
    static_assert(std::convertible_to<convertible_to_everything, int&&>);
    static_assert(std::convertible_to<convertible_to_everything, int&>);
    static_assert(std::convertible_to<convertible_to_everything, int>);
    static_assert(std::convertible_to<convertible_to_everything, const volatile int&&>);
    static_assert(std::convertible_to<convertible_to_everything, const volatile int&>);
    static_assert(std::convertible_to<convertible_to_everything, const volatile int>);

    /**
     * Class that's only convertible to Tys...
     */
    template<class ...Tys>
    struct only_convertible_to {
        template<class M> requires ((std::same_as<std::decay_t<M>, Tys>) || ...)
            constexpr operator M && ();
        template<class M> requires ((std::same_as<std::decay_t<M>, Tys>) || ...)
            constexpr operator M& ();
    };
    static_assert(std::convertible_to<only_convertible_to<int>, int&&>);
    static_assert(std::convertible_to<only_convertible_to<int>, int&>);
    static_assert(std::convertible_to<only_convertible_to<int>, int>);
    static_assert(std::convertible_to<only_convertible_to<int>, const volatile int&&>);
    static_assert(std::convertible_to<only_convertible_to<int>, const volatile int&>);
    static_assert(std::convertible_to<only_convertible_to<int>, const volatile int>);
    static_assert(!std::convertible_to<only_convertible_to<int>, float&&>);
    static_assert(!std::convertible_to<only_convertible_to<int>, float&>);
    static_assert(!std::convertible_to<only_convertible_to<int>, float>);
    static_assert(!std::convertible_to<only_convertible_to<int>, const volatile float&&>);
    static_assert(!std::convertible_to<only_convertible_to<int>, const volatile float&>);
    static_assert(!std::convertible_to<only_convertible_to<int>, const volatile float>);

    /**
     * Class that's not convertible to Tys...
     */
    template<class ...Tys>
    struct not_convertible_to {
        template<class M> requires (((!std::same_as<std::decay_t<M>, Tys>) && ...)
            && ((!std::is_base_of_v<std::decay_t<M>, Tys>) && ...))
            constexpr operator M& ();
        template<class M> requires (((!std::same_as<std::decay_t<M>, Tys>) && ...)
            && ((!std::is_base_of_v<std::decay_t<M>, Tys>) && ...))
            constexpr operator M && ();
    };
    static_assert(!std::convertible_to<not_convertible_to<int>, int&&>);
    static_assert(!std::convertible_to<not_convertible_to<int>, int&>);
    static_assert(!std::convertible_to<not_convertible_to<int>, int>);
    static_assert(!std::convertible_to<not_convertible_to<int>, const volatile int&&>);
    static_assert(!std::convertible_to<not_convertible_to<int>, const volatile int&>);
    static_assert(!std::convertible_to<not_convertible_to<int>, const volatile int>);
    static_assert(std::convertible_to<not_convertible_to<int>, float&&>);
    static_assert(std::convertible_to<not_convertible_to<int>, float&>);
    static_assert(std::convertible_to<not_convertible_to<int>, float>);
    static_assert(std::convertible_to<not_convertible_to<int>, const volatile float&&>);
    static_assert(std::convertible_to<not_convertible_to<int>, const volatile float&>);
    static_assert(std::convertible_to<not_convertible_to<int>, const volatile float>);

    /**
     * Call lambda with array values as template arguments, like
     * Lambda.operator()<Array[Is]...>();
     */
    template<auto Array>
    constexpr auto iterate = [](auto Lambda) {
        return[Lambda]<std::size_t ...Is>(std::index_sequence<Is...>) {
            return Lambda.operator() < Array[Is]... > ();
        }(std::make_index_sequence<Array.size()>{});
    };

    template<auto Array, template<std::size_t ...> class Ty> 
    struct array_to_pack {
        template<class> struct helper;
        template<std::size_t ...Is>
        struct helper<std::index_sequence<Is...>> {
            using type = Ty<Array[Is]...>;
        };

        using type = typename helper<std::make_index_sequence<Array.size()>>::type;
    };

    /**
     * Convert an array of indices to a template pack.
     * @tparam Array array of std::size_t
     * @tparam Ty templated type that takes std::size_t's
     */
    template<auto Array, template<std::size_t ...> class Ty>
    using array_to_pack_t = typename array_to_pack<Array, Ty>::type;

    /**
     * Partially specialize T by setting the first 
     * template parameters to ...As
     * @tparam T templated type
     * @tparam ...As types to 
     */
    template<template<class...> class T, class ...As>
    struct partial_first {
        template<class ...Bs>
        using type = T<As..., Bs...>;
    };
    template<template<class...> class T, class ...As>
    using partial = partial_first<T, As...>;

    template<class ...Tys> 
    struct tparams { 
        using type = info<typename tparams<Tys>::type...>;
    };
    template<class Ty> struct tparams<Ty> { using type = Ty; };
    template<template<class...> class T, class ...Tys>
    struct tparams<T<Tys...>> {
        using type = info<Tys...>;
    };

    /**
     * Get the template parameters from a templated type.
     * @tparam Ty templated type
     */
    template<class ...Tys>
    using tparams_t = typename tparams<Tys...>::type;

    /**
     * Partially specialize T by setting the first 
     * template parameters to ...As
     * @tparam T templated type
     * @tparam ...As types to 
     */
    template<template<class...> class T, class ...As>
    struct partial_last {
        template<class ...Bs>
        using type = T<Bs..., As...>;
    };

    /**
     * Type for a template value.
     * @tparam V template value
     */
    template<auto V>
    struct value_t {
        constexpr static auto value = V;
    };

    /**
     * Wrapper for a templated type.
     * @tparam T templated type
     */
    template<template<class...> class T>
    struct templated_t {
        template<class ...Args>
        using type = T<Args...>;
    };

    template<class Ty> struct uninstantiate { using type = Ty; };
    template<template<class...> class T, class ...Tys> 
    struct uninstantiate<T<Tys...>> {
        using type = templated_t<T>; 
    };

    /**
     * Remove template parameters from templated type.
     * @tparam Ty templated type
     */
    template<class Ty> 
    using uninstantiate_t = typename uninstantiate<Ty>::type;
    
    template<template<class...> class T, class ...Tys> 
    struct instantiate {
        using type = T<Tys...>; 
    };
    template<template<class...> class T, class ...Tys> 
    struct instantiate<T, info<Tys...>> {
        using type = T<Tys...>; 
    };

    /**
     * Specialize a templated type.
     * @tparam Ty templated type
     */
    template<template<class...> class T, class ...Tys>
    using instantiate_t = typename instantiate<T, Tys...>::type;
    
    template<class T, class ...Tys> struct reinstantiate;
    template<template<class...> class T, class ...Args, class ...Tys> 
    struct reinstantiate<T<Args...>, Tys...> {
        using type = T<Tys...>; 
    };
    template<template<class...> class T, class ...Args, class ...Tys> 
    struct reinstantiate<T<Args...>, info<Tys...>> {
        using type = T<Tys...>; 
    };

    /**
     * Specialize a templated type with new parameters.
     * @tparam Ty templated type
     */
    template<class T, class ...Tys>
    using reinstantiate_t = typename reinstantiate<T, Tys...>::type;

    /**
     * Overloaded Functor.
     * @tparam Functors... functor types
     */
    template<class ...Functors>
    struct overloaded : Functors... {
        using Functors::operator()...;
    };

    /**
     * Dud type, used in places as a placeholder, or
     * when nothing else should match.
     */
    struct dud {
        constexpr bool operator==(const dud&) const { return true; }
    };

    /**
     * Extract enum name from function signature, used in
     * enum_name_impl.
     * @param name string containing enum name
     * @return extracted enum name
     */
    consteval std::string_view _enum_pretty_name(std::string_view name) noexcept {
        // Starting at end of string_view, only keep valid enum name characters
        for (std::size_t i = name.size(); i > 0; --i) {
            auto& c = name[i - 1];
            // Valid characters are [0-9a-zA-Z_]
            if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z')
                || (c >= 'A' && c <= 'Z') || (c == '_'))) {
                // Remove prefix once we've hit invalid character
                name.remove_prefix(i);
                break;
            }
        }

        // Make sure first character is valid as well.
        if (name.size() > 0 && ((name.front() >= 'a' && name.front() <= 'z') ||
            (name.front() >= 'A' && name.front() <= 'Z') || (name.front() == '_')))
            return name;

        return {}; // Invalid name.
    }

    /**
     * Get enum name using function signature macro.
     * @tparam Ty enum type
     * @tparam Value enum value
     * @return extracted enum name
     */
    template<class Ty, Ty Value>
    consteval std::string_view enum_name_impl() noexcept {
#if defined(__clang__) || defined(__GNUC__)
        return _enum_pretty_name({ __PRETTY_FUNCTION__, sizeof(__PRETTY_FUNCTION__) - 2 });
#elif defined(_MSC_VER)
        return _enum_pretty_name({ __FUNCSIG__, sizeof(__FUNCSIG__) - 17 });
#else
        return string_view{};
#endif
    }

    /**
     * Get enum name.
     * @tparam Ty enum type
     * @tparam V value that's convertible to Ty
     */
    template<class Ty, auto V>
    constexpr auto enum_name = [] {
        constexpr auto name = enum_name_impl<Ty, static_cast<Ty>(V)>();
        if constexpr (name.data() == nullptr) return string_literal<1>{ "\0" };
        else return string_literal<name.size() + 1>{ name };
    }();

    /**
     * Extract value name from function signature.
     * @tparam Value template value
     * @return extracted value name
     */
    template<auto Value>
    consteval auto value_name_impl() noexcept {
#if defined(__clang__) || defined(__GNUC__)
        constexpr auto name = std::string_view{ __PRETTY_FUNCTION__, sizeof(__PRETTY_FUNCTION__) - 2 };
        // Remove prefix, template argument starts at first '<'
        return name.substr(name.find_first_of('<') + 1);
#elif defined(_MSC_VER)
        constexpr auto name = std::string_view{ __FUNCSIG__, sizeof(__FUNCSIG__) - 17 };
        // Remove prefix, template argument starts at first '<'
        return name.substr(name.find_first_of('<') + 1);
#else
        return string_view{};
#endif
    }

    /**
     * Get value as string.
     * @tparam V template value
     */
    template<auto V>
    constexpr auto value_name = [] {
        constexpr auto name = value_name_impl<V>();
        if constexpr (name.data() == nullptr) return string_literal<1>{ "\0" };
        else return string_literal<name.size() + 1>{ name };
    }();

    /**
     * Get pretty function name from string_view containing
     * the function name.
     * @param name string containing function name
     * @return extracted function name
     */
    constexpr std::string_view _function_pretty_name(std::string_view name) noexcept {
        if (name.size() == 0) return {};

        // remove call '( ... )' part
        std::size_t count = 0;
        std::size_t suffix = 0;
        for (std::size_t i = name.size(); i > 0; --i) {
            auto& c = name[i - 1];
            if (c == ')') count++;
            if (c == '(') count--;
            suffix++;
            if (count == 0) {
                name.remove_suffix(suffix);
                break;
            }
        }

        if (name.size() == 0) return {};

        // Remove template part if it exists '< ... >'
        if (name[name.size() - 1] == '>') {
            count = 0;
            suffix = 0;
            for (std::size_t i = name.size(); i > 0; --i) {
                auto& c = name[i - 1];
                if (c == '>') count++;
                if (c == '<') count--;

                suffix++;
                if (count == 0) {
                    name.remove_suffix(suffix);
                    break;
                }
            }
        }

        if (name.size() == 0) return {};

        // Collect valid identifier characters
        for (std::size_t i = name.size(); i > 0; --i) {
            if (auto& c = name[i - 1]; !((c >= '0' && c <= '9') ||
                (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_'))) {
                name.remove_prefix(i); break;
            }
        }

        // Make sure it starts with a valid character
        if (name.size() > 0 && ((name.front() >= 'a' && name.front() <= 'z') ||
            (name.front() >= 'A' && name.front() <= 'Z') || (name.front() == '_')))
            return name;

        return {}; // Invalid name.
    }

    /**
     * Get function name from function signature.
     * @tparam Value function pointer
     * @return function name
     */
    template<auto Value>
    consteval auto function_name_impl() noexcept {
#if defined(__clang__) || defined(__GNUC__)
        constexpr auto name = std::string_view{ __PRETTY_FUNCTION__, sizeof(__PRETTY_FUNCTION__) - 2 };
        return _function_pretty_name(name.substr(name.find_first_of('<') + 1));
#elif defined(_MSC_VER)
        constexpr auto name = std::string_view{ __FUNCSIG__, sizeof(__FUNCSIG__) - 17 };
        return _function_pretty_name(name.substr(name.find_first_of('<') + 1));
#else
        return string_view{};
#endif
    }

    /**
     * Get function name.
     * @tparam Value (member) function pointer
     */
    template<auto V>
    constexpr auto function_name = [] {
        constexpr auto name = function_name_impl<V>();
        if constexpr (name.data() == nullptr) return string_literal<1>{ "\0" };
        else return string_literal<name.size() + 1>{ name };
    }();

    /**
     * Get type name from function signature.
     * @tparam Value type
     * @return type name
     */
    template<class Value>
    consteval auto type_name_impl() noexcept {
#if defined(__clang__) || defined(__GNUC__)
        auto name = std::string_view{ __PRETTY_FUNCTION__, sizeof(__PRETTY_FUNCTION__) - 2 };
        name = name.substr(name.find_first_of('<') + 1);
        if (name.starts_with("struct ")) name = name.substr(7);
        return name;
#elif defined(_MSC_VER)
        auto name = std::string_view{ __FUNCSIG__, sizeof(__FUNCSIG__) - 17 };
        name = name.substr(name.find_first_of('<') + 1);
        if (name.starts_with("struct ")) name = name.substr(7);
        return name;
#else
        return string_view{};
#endif
    }
    /**
     * Get type name.
     * @tparam V type
     */
    template<class V>
    constexpr auto type_name = [] {
        constexpr auto name = type_name_impl<V>();
        if constexpr (name.data() == nullptr) return string_literal<1>{ "\0" };
        else return string_literal<name.size() + 1>{ name };
    }();

    /**
     * Basically sizeof(Ty), but special case for
     * void and functions, as they normally give errors.
     */
    template<class Ty>
    constexpr std::size_t sizeof_v = [] {
        if constexpr (std::is_void_v<Ty>) return 0;
        else if constexpr (std::is_function_v<Ty>) return 0;
        else if constexpr (std::is_array_v<Ty> && std::extent_v<Ty> == 0) return 0;
        else return sizeof(Ty);
    }();

    /**
     * Basically alignof(Ty), but special case for
     * void and functions, as they normally give errors.
     */
    template<class Ty>
    constexpr std::size_t alignof_v = [] {
        if constexpr (std::is_void_v<Ty>) return 0;
        else if constexpr (std::is_function_v<Ty>) return 0;
        else return std::alignment_of_v<Ty>;
    }();


    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     *                                           type trait helpers.                                           *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    template<template<class Ty, class ...Args> class Trait, class Ty, class ...Args>
    struct pack_trait_helper : Trait<Ty, Args...> {};
    template<template<class Ty, class ...Args> class Trait, class Ty, class ...Args>
    struct pack_trait_helper<Trait, Ty, info<Args...>> : Trait<Ty, Args...> {};

    inline namespace type_concepts {
        template<class Ty> concept void_type = std::is_void_v<Ty>;
        template<class Ty> concept null_pointer = std::is_null_pointer_v<Ty>;
        template<class Ty> concept boolean = std::is_same_v<Ty, bool>;
        template<class Ty> concept integral = std::is_integral_v<Ty>;
        template<class Ty> concept floating_point = std::is_floating_point_v<Ty>;
        template<class Ty> concept array = std::is_array_v<Ty>;
        template<class Ty> concept enum_type = std::is_enum_v<Ty>;
        template<class Ty> concept union_type = std::is_union_v<Ty>;
        template<class Ty> concept class_type = std::is_class_v<Ty>;
        template<class Ty> concept function = std::is_function_v<Ty>;
        template<class Ty> concept pointer = std::is_pointer_v<Ty>;
        template<class Ty> concept lvalue_reference = std::is_lvalue_reference_v<Ty>;
        template<class Ty> concept rvalue_reference = std::is_rvalue_reference_v<Ty>;
        template<class Ty> concept member_object_pointer = std::is_member_object_pointer_v<Ty>;
        template<class Ty> concept member_function_pointer = std::is_member_function_pointer_v<Ty>;
        template<class Ty> concept fundamental = std::is_fundamental_v<Ty>;
        template<class Ty> concept arithmetic = std::is_arithmetic_v<Ty>;
        template<class Ty> concept scalar = std::is_scalar_v<Ty>;
        template<class Ty> concept object = std::is_object_v<Ty>;
        template<class Ty> concept compound = std::is_compound_v<Ty>;
        template<class Ty> concept reference = std::is_reference_v<Ty>;
        template<class Ty> concept member_pointer = std::is_member_pointer_v<Ty>;
        template<class Ty> concept const_type = std::is_const_v<std::remove_reference_t<Ty>>;
        template<class Ty> concept volatile_type = std::is_volatile_v<std::remove_reference_t<Ty>>;
        template<class Ty> concept trivial = std::is_trivial_v<Ty>;
        template<class Ty> concept trivially_copyable = std::is_trivially_copyable_v<Ty>;
        template<class Ty> concept standard_layout = std::is_standard_layout_v<Ty>;
        template<class Ty> concept empty = std::is_empty_v<Ty>;
        template<class Ty> concept polymorphic = std::is_polymorphic_v<Ty>;
        template<class Ty> concept abstract = std::is_abstract_v<Ty>;
        template<class Ty> concept final = std::is_final_v<Ty>;
        template<class Ty> concept aggregate = std::is_aggregate_v<Ty>;
        template<class Ty> concept signed_integral = std::is_signed_v<Ty>;
        template<class Ty> concept unsigned_integral = std::is_unsigned_v<Ty>;
        template<class Ty> concept bounded_array = std::is_bounded_array_v<Ty>;
        template<class Ty> concept unbounded_array = std::is_unbounded_array_v<Ty>;
        template<class Ty> concept default_constructible = std::is_default_constructible_v<Ty>;
        template<class Ty> concept trivially_default_constructible = std::is_trivially_default_constructible_v<Ty>;
        template<class Ty> concept nothrow_default_constructible = std::is_nothrow_default_constructible_v<Ty>;
        template<class Ty> concept copy_constructible = std::is_copy_constructible_v<Ty>;
        template<class Ty> concept trivially_copy_constructible = std::is_trivially_copy_constructible_v<Ty>;
        template<class Ty> concept nothrow_copy_constructible = std::is_nothrow_copy_constructible_v<Ty>;
        template<class Ty> concept move_constructible = std::is_move_constructible_v<Ty>;
        template<class Ty> concept trivially_move_constructible = std::is_trivially_move_constructible_v<Ty>;
        template<class Ty> concept nothrow_move_constructible = std::is_nothrow_move_constructible_v<Ty>;
        template<class Ty> concept copy_assignable = std::is_copy_assignable_v<Ty>;
        template<class Ty> concept trivially_copy_assignable = std::is_trivially_copy_assignable_v<Ty>;
        template<class Ty> concept nothrow_copy_assignable = std::is_nothrow_copy_assignable_v<Ty>;
        template<class Ty> concept move_assignable = std::is_move_assignable_v<Ty>;
        template<class Ty> concept trivially_move_assignable = std::is_trivially_move_assignable_v<Ty>;
        template<class Ty> concept nothrow_move_assignable = std::is_nothrow_move_assignable_v<Ty>;
        template<class Ty> concept destructible = std::is_destructible_v<Ty>;
        template<class Ty> concept trivially_destructible = std::is_trivially_destructible_v<Ty>;
        template<class Ty> concept nothrow_destructible = std::is_nothrow_destructible_v<Ty>;
        template<class Ty> concept swappable = std::is_swappable_v<Ty>;
        template<class Ty> concept nothrow_swappable = std::is_nothrow_swappable_v<Ty>;

        template<class Ty> concept unique_object_representations = std::has_unique_object_representations_v<Ty>;
        template<class Ty> concept virtual_destructor = std::has_virtual_destructor_v<Ty>;

        template<class Ty, class Other> concept assignable = std::is_assignable_v<Ty, Other>;
        template<class Ty, class Other> concept trivially_assignable = std::is_trivially_assignable_v<Ty, Other>;
        template<class Ty, class Other> concept nothrow_assignable = std::is_nothrow_assignable_v<Ty, Other>;
        template<class Ty, class Other> concept assignable_to = std::is_assignable_v<Other, Ty>;
        template<class Ty, class Other> concept trivially_assignable_to = std::is_trivially_assignable_v<Other, Ty>;
        template<class Ty, class Other> concept nothrow_assignable_to = std::is_nothrow_assignable_v<Other, Ty>;
        template<class Ty, class Other> concept swappable_with = std::is_swappable_with_v<Ty, Other>;
        template<class Ty, class Other> concept nothrow_swappable_with = std::is_nothrow_swappable_with_v<Ty, Other>;

        template<class Ty, class Other> concept same_as = std::is_same_v<Ty, Other>;
        template<class Ty, class Other> concept base_of = std::is_base_of_v<Ty, Other>;
        template<class Ty, class Other> concept convertible_to = std::is_convertible_v<Ty, Other>;
        template<class Ty, class Other> concept nothrow_convertible_to = std::is_nothrow_convertible_v<Ty, Other>;
        template<class Ty, class Other> concept convertible_from = std::is_convertible_v<Other, Ty>;
        template<class Ty, class Other> concept nothrow_convertible_from = std::is_nothrow_convertible_v<Other, Ty>;

        template<class Ty, class ...Args> concept constructible = pack_trait_helper<std::is_constructible, Ty, Args...>::value;
        template<class Ty, class ...Args> concept trivially_constructible = pack_trait_helper<std::is_trivially_constructible, Ty, Args...>::value;
        template<class Ty, class ...Args> concept nothrow_constructible = pack_trait_helper<std::is_nothrow_constructible, Ty, Args...>::value;
        template<class Ty, class ...Args> concept invocable = pack_trait_helper<std::is_invocable, Ty, Args...>::value;
        template<class Ty, class ...Args> concept nothrow_invocable = pack_trait_helper<std::is_nothrow_invocable, Ty, Args...>::value;

        template<class, template<class...> class>
        struct specialization_impl : std::false_type {};
        template<template<class...> class Ref, class... Args>
        struct specialization_impl<Ref<Args...>, Ref> : std::true_type {};

        template<class Test, template<class...> class Ref>
        concept specialization = specialization_impl<std::decay_t<Test>, Ref>::value;
    }
    /**
     * All standard type traits wrapped in an object, allows for
     * simple boolean operations and partial application.
     * Even allows for complex concept constraints like:
     * template<require<is_integral || is_floating_point> Ty>
     */

    inline namespace type_traits {
        /**
         * Boolean and on 2 type_trait classes.
         * @tparam A type trait 1
         * @tparam B type trait 2
         */
        template<template<class ...> class A, template<class ...> class B>
        struct type_trait_and {
            template<class ...Args> struct type {
                constexpr static bool value = A<Args...>::value && B<Args...>::value;
            };
        };

        /**
         * Boolean or on 2 type_trait classes.
         * @tparam A type trait 1
         * @tparam B type trait 2
         */
        template<template<class ...> class A, template<class ...> class B>
        struct type_trait_or {
            template<class ...Args> struct type {
                constexpr static bool value = A<Args...>::value || B<Args...>::value;
            };
        };

        /**
         * Boolean not on a type_trait class.
         * @tparam A type trait
         */
        template<template<class ...> class A>
        struct type_trait_not {
            template<class ...Args> struct type {
                constexpr static bool value = !A<Args...>::value;
            };
        };

        /**
         * Partially applied type trait, where last types
         * are provided.
         * @tparam A type trait
         * @tparam Tys... provided arguments
         */
        template<template<class ...> class A, class ...Tys>
        struct type_trait_partial_last {
            template<class ...Args> struct type {
                constexpr static bool value = A<Args..., Tys...>::value;
            };
        };

        /**
         * Partially applied type trait, where last types
         * are provided in a pack.
         * @tparam A type trait
         * @tparam Tys... provided arguments
         */
        template<template<class ...> class A, class ...Tys>
        struct type_trait_partial_last<A, info<Tys...>> {
            template<class ...Args> struct type {
                constexpr static bool value = A<Args..., Tys...>::value;
            };
        };

        /**
         * Partially applied type trait, where first types
         * are provided.
         * @tparam A type trait
         * @tparam Tys... provided arguments
         */
        template<template<class ...> class A, class ...Tys>
        struct type_trait_partial_first {
            template<class ...Args> struct type {
                constexpr static bool value = A<Tys..., Args...>::value;
            };
        };

        /**
         * Partially applied type trait, where first types
         * are provided in a pack.
         * @tparam A type trait
         * @tparam Tys... provided arguments
         */
        template<template<class ...> class A, class ...Tys>
        struct type_trait_partial_first<A, info<Tys...>> {
            template<class ...Args> struct type {
                constexpr static bool value = A<Tys..., Args...>::value;
            };
        };

        /**
         * Unevaluated type trait wrapper.
         * @tparam Trait type trait
         */
        template<template<class ...> class Trait>
        struct type_trait {
            template<class ...Tys>
            constexpr static bool value = Trait<Tys...>::value;
        };

        template<class> struct is_type_trait_impl : std::false_type{};
        template<template<class ...> class T>
        struct is_type_trait_impl<type_trait<T>> : std::true_type {};

        /**
         * Check if Ty is a type trait object.
         * @tparam Ty type
         */
        template<class Ty>
        concept is_type_trait = is_type_trait_impl<Ty>::value;

        /**
         * Concept to match a type_trait.
         * @tparam Ty type
         * @tparam V type_trait value
         */
        template<class Ty, is_type_trait auto V>
        concept require = V.template value<Ty>;

        /**
         * Boolean and on 2 type traits
         * @tparam A type trait 1
         * @tparam B type trait 2
         * @return type trait that matches if both match
         */
        template<template<class ...> class A, template<class ...> class B>
        consteval auto operator and(type_trait<A>, type_trait<B>) {
            return type_trait<typename type_trait_and<A, B>::type>{};
        }

        /**
         * Boolean or on 2 type traits
         * @tparam A type trait 1
         * @tparam B type trait 2
         * @return type trait that matches if either matches
         */
        template<template<class ...> class A, template<class ...> class B>
        consteval auto operator or(type_trait<A>, type_trait<B>) {
            return type_trait<typename type_trait_or<A, B>::type>{};
        }

        /**
         * Boolean not on a type trait
         * @tparam A type trait
         * @return type trait that matches if A doesn't match
         */
        template<template<class ...> class A>
        consteval auto operator not(type_trait<A>) {
            return type_trait<typename type_trait_not<A>::type>{};
        }

        constexpr auto is_void = type_trait<std::is_void>{};
        constexpr auto is_null_pointer = type_trait<std::is_null_pointer>{};
        constexpr auto is_integral = type_trait<std::is_integral>{};
        constexpr auto is_floating_point = type_trait<std::is_floating_point>{};
        constexpr auto is_array = type_trait<std::is_array>{};
        constexpr auto is_enum = type_trait<std::is_enum>{};
        constexpr auto is_union = type_trait<std::is_union>{};
        constexpr auto is_class = type_trait<std::is_class>{};
        constexpr auto is_function = type_trait<std::is_function>{};
        constexpr auto is_pointer = type_trait<std::is_pointer>{};
        constexpr auto is_lvalue_reference = type_trait<std::is_lvalue_reference>{};
        constexpr auto is_rvalue_reference = type_trait<std::is_rvalue_reference>{};
        constexpr auto is_member_object_pointer = type_trait<std::is_member_object_pointer>{};
        constexpr auto is_member_function_pointer = type_trait<std::is_member_function_pointer>{};
        constexpr auto is_fundamental = type_trait<std::is_fundamental>{};
        constexpr auto is_arithmetic = type_trait<std::is_arithmetic>{};
        constexpr auto is_scalar = type_trait<std::is_scalar>{};
        constexpr auto is_object = type_trait<std::is_object>{};
        constexpr auto is_compound = type_trait<std::is_compound>{};
        constexpr auto is_reference = type_trait<std::is_reference>{};
        constexpr auto is_member_pointer = type_trait<std::is_member_pointer>{};
        constexpr auto is_const = type_trait<std::is_const>{};
        constexpr auto is_volatile = type_trait<std::is_volatile>{};
        constexpr auto is_trivial = type_trait<std::is_trivial>{};
        constexpr auto is_trivially_copyable = type_trait<std::is_trivially_copyable>{};
        constexpr auto is_standard_layout = type_trait<std::is_standard_layout>{};
        constexpr auto is_empty = type_trait<std::is_empty>{};
        constexpr auto is_polymorphic = type_trait<std::is_polymorphic>{};
        constexpr auto is_abstract = type_trait<std::is_abstract>{};
        constexpr auto is_final = type_trait<std::is_final>{};
        constexpr auto is_aggregate = type_trait<std::is_aggregate>{};
        constexpr auto is_signed = type_trait<std::is_signed>{};
        constexpr auto is_unsigned = type_trait<std::is_unsigned>{};
        constexpr auto is_bounded_array = type_trait<std::is_bounded_array>{};
        constexpr auto is_unbounded_array = type_trait<std::is_unbounded_array>{};
        constexpr auto is_default_constructible = type_trait<std::is_default_constructible>{};
        constexpr auto is_trivially_default_constructible = type_trait<std::is_trivially_default_constructible>{};
        constexpr auto is_nothrow_default_constructible = type_trait<std::is_nothrow_default_constructible>{};
        constexpr auto is_copy_constructible = type_trait<std::is_copy_constructible>{};
        constexpr auto is_trivially_copy_constructible = type_trait<std::is_trivially_copy_constructible>{};
        constexpr auto is_nothrow_copy_constructible = type_trait<std::is_nothrow_copy_constructible>{};
        constexpr auto is_move_constructible = type_trait<std::is_move_constructible>{};
        constexpr auto is_trivially_move_constructible = type_trait<std::is_trivially_move_constructible>{};
        constexpr auto is_nothrow_move_constructible = type_trait<std::is_nothrow_move_constructible>{};
        constexpr auto is_copy_assignable = type_trait<std::is_copy_assignable>{};
        constexpr auto is_trivially_copy_assignable = type_trait<std::is_trivially_copy_assignable>{};
        constexpr auto is_nothrow_copy_assignable = type_trait<std::is_nothrow_copy_assignable>{};
        constexpr auto is_move_assignable = type_trait<std::is_move_assignable>{};
        constexpr auto is_trivially_move_assignable = type_trait<std::is_trivially_move_assignable>{};
        constexpr auto is_nothrow_move_assignable = type_trait<std::is_nothrow_move_assignable>{};
        constexpr auto is_destructible = type_trait<std::is_destructible>{};
        constexpr auto is_trivially_destructible = type_trait<std::is_trivially_destructible>{};
        constexpr auto is_nothrow_destructible = type_trait<std::is_nothrow_destructible>{};
        constexpr auto is_swappable = type_trait<std::is_swappable>{};
        constexpr auto is_nothrow_swappable = type_trait<std::is_nothrow_swappable>{};

        constexpr auto has_unique_object_representations = type_trait<std::has_unique_object_representations>{};
        constexpr auto has_virtual_destructor = type_trait<std::has_virtual_destructor>{};

        template<class Other> constexpr auto is_assignable = type_trait<typename type_trait_partial_last<std::is_assignable, Other>::type>{};
        template<class Other> constexpr auto is_trivially_assignable = type_trait<typename type_trait_partial_last<std::is_trivially_assignable, Other>::type>{};
        template<class Other> constexpr auto is_nothrow_assignable = type_trait<typename type_trait_partial_last<std::is_nothrow_assignable, Other>::type>{};
        template<class Other> constexpr auto is_assignable_to = type_trait<typename type_trait_partial_first<std::is_assignable, Other>::type>{};
        template<class Other> constexpr auto is_trivially_assignable_to = type_trait<typename type_trait_partial_first<std::is_trivially_assignable, Other>::type>{};
        template<class Other> constexpr auto is_nothrow_assignable_to = type_trait<typename type_trait_partial_first<std::is_nothrow_assignable, Other>::type>{};
        template<class Other> constexpr auto is_swappable_with = type_trait<typename type_trait_partial_last<std::is_swappable_with, Other>::type>{};
        template<class Other> constexpr auto is_nothrow_swappable_with = type_trait<typename type_trait_partial_last<std::is_nothrow_swappable_with, Other>::type>{};

        template<class Other> constexpr auto is_same = type_trait<typename type_trait_partial_last<std::is_same, Other>::type>{};
        template<class Other> constexpr auto is_base_of = type_trait<typename type_trait_partial_last<std::is_base_of, Other>::type>{};
        template<class Other> constexpr auto is_derived_of = type_trait<typename type_trait_partial_first<std::is_base_of, Other>::type>{};
        template<class Other> constexpr auto is_convertible_to = type_trait<typename type_trait_partial_last<std::is_convertible, Other>::type>{};
        template<class Other> constexpr auto is_nothrow_convertible_to = type_trait<typename type_trait_partial_last<std::is_nothrow_convertible, Other>::type>{};
        template<class Other> constexpr auto is_convertible_from = type_trait<typename type_trait_partial_first<std::is_convertible, Other>::type>{};
        template<class Other> constexpr auto is_nothrow_convertible_from = type_trait<typename type_trait_partial_first<std::is_nothrow_convertible, Other>::type>{};

        template<class ...Args> constexpr auto is_constructible = type_trait<typename type_trait_partial_last<std::is_constructible, Args...>::type>{};
        template<class ...Args> constexpr auto is_trivially_constructible = type_trait<typename type_trait_partial_last<std::is_trivially_constructible, Args...>::type>{};
        template<class ...Args> constexpr auto is_nothrow_constructible = type_trait<typename type_trait_partial_last<std::is_nothrow_constructible, Args...>::type>{};
        template<class ...Args> constexpr auto is_invocable = type_trait<typename type_trait_partial_last<std::is_invocable, Args...>::type>{};
        template<class ...Args> constexpr auto is_nothrow_invocable = type_trait<typename type_trait_partial_last<std::is_nothrow_invocable, Args...>::type>{};

        template<template<class ...> class Ty>
        struct is_specialization_impl {
            template<class T> struct type {
                constexpr static bool value = specialization<T, Ty>;
            };
        };

        template<template<class ...> class Ty> constexpr auto is_specialization = type_trait<typename is_specialization_impl<Ty>::type>{};
    }
    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     *                                              pack helpers.                                              *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    template<class T, template<class...> class Ty>
    struct move_tparams { using type = Ty<T>; };
    template<template<class...> class T, class ...Args, template<class...> class Ty>
    struct move_tparams<T<Args...>, Ty> { using type = Ty<Args...>; };
    template<class ...Args, template<class...> class Ty>
    struct move_tparams<template_pack<Args...>, Ty> { using type = Ty<Args&&...>; };

    /**
     * Move the template parameters from T to Ty. If T is
     * not templated, it will itself be used as the template parameter.
     * @tparam T type or type with template parameters
     * @tparam Ty templated type
     */
    template<class T, template<class...> class Ty>
    using move_tparams_t = typename move_tparams<T, Ty>::type;

    /**
     * Type linked to an index.
     * @tparam I index
     * @tparam Ty type
     */
    template<std::size_t I, class Ty> struct indexed {
        constexpr static std::size_t index = I;
        using type = Ty;
    };

    template<class, class...> struct indexer_impl;
    template<std::size_t ...Is, class ...Args>
    struct indexer_impl<std::index_sequence<Is...>, Args...> : indexed<Is, Args>... {};

    /**
     * Maps indices to all types in ...Args.
     * @tparam ...Args types
     */
    template<class ...Args>
    using indexer = indexer_impl<std::index_sequence_for<Args...>, Args...>;

    template<class, class...> struct reverse_indexer_impl;
    template<std::size_t ...Is, class ...Args>
    struct reverse_indexer_impl<std::index_sequence<Is...>, Args...>
        : indexed<sizeof...(Args) - Is - 1, Args>... {};

    /**
     * Maps indices in decending order to all types in ...Args.
     * @tparam ...Args types
     */
    template<class ...Args>
    using reverse_indexer = reverse_indexer_impl<
        std::index_sequence_for<Args...>, Args...>;

    template<std::size_t I, class Ty> // Overload resolution to get element at
    consteval indexed<I, Ty> element_impl(indexed<I, Ty>) {}; // index I

    template<std::size_t I, class ...Args>
    struct element {
        using type = typename decltype(element_impl<I>(indexer<Args...>{}))::type;
    };

    template<std::size_t I, class ...Args>
    struct element<I, info<Args...>> {
        using type = typename decltype(element_impl<I>(indexer<Args...>{}))::type;
    };

    /**
     * Get the I'th type in ...Args.
     * @tparam I index
     * @tparam ...Args types
     */
    template<std::size_t I, class ...Args>
    using element_t = typename element<I, Args...>::type;

    template<class Ty, class ...Args>
    struct index {
        constexpr static std::size_t _get() {
            std::size_t index = 0;
            ((++index, std::is_same_v<Ty, Args>) || ...);
            return index - 1;
        }

        constexpr static std::size_t value = _get();
    };

    template<class Ty, class ...Args>
    struct index<Ty, info<Args...>> : index<Ty, Args...> {};

    /**
     * Get the first index of Ty in ...Args.
     * @tparam Ty type to find index of
     * @tparam ...Args types
     */
    template<class Ty, class ...Args>
    constexpr std::size_t index_v = index<Ty, Args...>::value;

    template<class Ty, class ...Args>
    struct last_index {
        constexpr static std::size_t _get() {
            std::size_t _fromEnd = 0; // increment, but reset on match
            ((std::is_same_v<Ty, Args> ? _fromEnd = 0 : ++_fromEnd), ...);
            std::size_t _index = sizeof...(Args) - _fromEnd - 1;
            return _fromEnd == sizeof...(Args) ? npos : _index;
        }

        constexpr static std::size_t value = _get();
    };

    template<class Ty, class ...Args>
    struct last_index<Ty, info<Args...>> : last_index<Ty, Args...> {};

    /**
     * Get the last index of Ty in ...Args.
     * @tparam Ty type to find index of
     * @tparam ...Args types
     */
    template<class Ty, class ...Args>
    constexpr std::size_t last_index_v = last_index<Ty, Args...>::value;

    template<class Ty, class ...Args>
    struct count : std::integral_constant<std::size_t, ((std::is_same_v<Ty, Args>) + ... + 0)> {};

    template<class Ty, class ...Args>
    struct count<Ty, info<Args...>> : count<Ty, Args...> {};

    /**
     * Count the number of occurences of Ty in ...Args.
     * @tparam Ty type to count
     * @tparam ...Args types
     */
    template<class Ty, class ...Args>
    constexpr std::size_t count_v = count<Ty, Args...>::value;

    template<class Ty, class ...Args>
    struct occurs : std::bool_constant<(count_v<Ty, Args...> > 0)> {};

    template<class Ty, class ...Args>
    struct occurs<Ty, info<Args...>> : occurs<Ty, Args...> {};

    /**
     * Check if Ty occurs in ...Args.
     * @tparam Ty type to check
     * @tparam ...Args types
     */
    template<class Ty, class ...Args>
    constexpr bool occurs_v = occurs<Ty, Args...>::value;

    template<std::size_t From, std::size_t To, std::size_t ...Except>
    struct generate_indices {
        constexpr static std::array<std::size_t, To - From - sizeof...(Except)> value = [] {
            std::array<std::size_t, To - From - sizeof...(Except)> _indices{};
            std::size_t _index = 0;
            std::size_t _match = 0;
            for (std::size_t _index = From; _index < To; ++_index)
                if (((_index != Except) && ...)) _indices[_match++] = _index;
            return _indices;
        }();
    };

    /**
     * Generate an array of indices from From to To,
     * leaving out all in indices ...Except.
     * @tparam From start of range
     * @tparam To end of range
     * @tparam ...Except values to skip
     */
    template<std::size_t From, std::size_t To, std::size_t ...Except>
    constexpr std::array<std::size_t, To - From - sizeof...(Except)>
        generate_indices_v = generate_indices<From, To, Except...>::value;

    template<class Ty, class ...Args>
    struct indices {
        constexpr static std::array<std::size_t, count_v<Ty, Args...>> value = [] {
            std::array<std::size_t, count_v<Ty, Args...>> _result{};
            std::size_t _index = 0, _match = 0;
            ((std::is_same_v<Ty, Args> ?
                _result[_match++] = _index++ : ++_index), ...);
            return _result;
        }();
    };

    template<class ...As, class ...Args>
    struct indices<info<As...>, Args...> {
        constexpr static auto _count = (count_v<As, Args...> +... + 0);
        constexpr static std::array<std::size_t, _count> value = [] {
            std::array<std::size_t, _count> _result{};
            std::size_t _index = 0, _match = 0;
            ((occurs_v<Args, As...> ?
                _result[_match++] = _index++ : ++_index), ...);
            return _result;
        }();
    };

    template<class A, class ...Args>
    struct indices<A, info<Args...>> : indices<A, Args...> {};

    template<class ...As, class ...Args>
    struct indices<info<As...>, info<Args...>> : indices<info<As...>, Args...> {};

    /**
     * Indices of Ty in ...Args.
     * @tparam Ty type, or info<Types...> for multiple
     * @tparam ...Args types
     */
    template<class Ty, class ...Args>
    constexpr auto indices_v = indices<Ty, Args...>::value;

    template<class Ty, class ...Args>
    struct indices_except {
        constexpr static std::array<std::size_t, sizeof...(Args) - count_v<Ty, Args...>> value = [] {
            std::array<std::size_t, sizeof...(Args) - count_v<Ty, Args...>> _result{};
            std::size_t _index = 0, _match = 0;
            ((std::is_same_v<Ty, Args> ?
                ++_index : _result[_match++] = _index++), ...);
            return _result;
        }();
    };

    template<class ...As, class ...Args>
    struct indices_except<info<As...>, Args...> {
        constexpr static auto _count = sizeof...(Args) - (count_v<As, Args...> +... + 0);
        constexpr static std::array<std::size_t, _count> value = [] {
            std::array<std::size_t, _count> _result{};
            std::size_t _index = 0, _match = 0;
            ((occurs_v<Args, As...> ?
                ++_index : _result[_match++] = _index++), ...);
            return _result;
        }();
    };

    template<class A, class ...Args>
    struct indices_except<A, info<Args...>> : indices_except<A, Args...> {};

    template<class ...As, class ...Args>
    struct indices_except<info<As...>, info<Args...>> : indices_except<info<As...>, Args...> {};

    /**
     * Indices of Ty in ...Args.
     * @tparam Ty type, or info<Types...> for multiple
     * @tparam ...Args types
     */
    template<class Ty, class ...Args>
    constexpr auto indices_except_v = indices_except<Ty, Args...>::value;

    template<class> struct reverse;
    template<template<class...> class T, class ...As>
    struct reverse<T<As...>> {
        template<class> struct helper;
        template<std::size_t... Is>
        struct helper<std::index_sequence<Is...>> {
            using type = T<typename decltype(
                element_impl<Is>(reverse_indexer<As...>{}))::type... > ;
        };
        using type = typename helper<std::index_sequence_for<As...>>::type;
    };

    /**
     * Reverse the template parameters of Ty.
     * @tparam Ty templated type
     */
    template<class Ty>
    using reverse_t = typename reverse<Ty>::type;

    template<class ...Args>
    struct unique_count {
        constexpr static std::size_t _get() {
            std::size_t _index = 0, _match = 0;
            ((_match += index_v<Args, Args...> == _index++), ...);
            return _match;
        }

        constexpr static std::size_t value = _get();
    };

    template<class ...Args>
    struct unique_count<info<Args...>> : unique_count<Args...> {};

    /**
     * Amount of unique types in ...Args.
     * @tparam ...Args types
     */
    template<class ...Args>
    constexpr std::size_t unique_count_v = unique_count<Args...>::value;

    template<class ...Args>
    struct first_indices {
        constexpr static std::array<std::size_t, unique_count_v<Args...>> value = [] {
            std::array<std::size_t, unique_count_v<Args...>> _result{};
            std::size_t _index = 0, _match = 0;
            (((index_v<Args, Args...> == _index) ?
                _result[_match++] = _index++ : ++_index), ...);
            return _result;
        }();
    };

    template<class ...Args>
    struct first_indices<info<Args...>> : first_indices<Args...> {};

    /**
     * Get the index of the first occurence of each type in ...Args.
     * @tparam ...Args types
     */
    template<class ...Args>
    constexpr std::array<std::size_t, unique_count_v<Args...>>
        first_indices_v = first_indices<Args...>::value;

    template<std::size_t, class> struct take;
    template<std::size_t N, template<class...> class T, class ...As>
    struct take<N, T<As...>> {
        template<class> struct helper;
        template<std::size_t ...Is>
        struct helper<std::index_sequence<Is...>> {
            using type = T<element_t<Is, As...>...>;
        };
        using type = typename helper<std::make_index_sequence<N>>::type;
    };

    /**
     * Take N types from the templated type Ty.
     * @tparam N amount of types to take
     * @tparam Ty templated type
     */
    template<std::size_t N, class Ty>
    using take_t = typename take<N, Ty>::type;

    template<std::size_t, class> struct drop;
    template<std::size_t N, template<class...> class T, class ...As>
    struct drop<N, T<As...>> {
        template<class> struct helper;
        template<std::size_t ...Is>
        struct helper<std::index_sequence<Is...>> {
            using type = T<element_t<Is + N, As...>...>;
        };
        using type = typename helper<std::make_index_sequence<sizeof...(As) - N>>::type;
    };

    /**
     * Drop N types from the templated type Ty.
     * @tparam N amount of types to drop
     * @tparam Ty templated type
     */
    template<std::size_t N, class Ty>
    using drop_t = typename drop<N, Ty>::type;
    
    template<std::size_t, class> struct drop_last;
    template<std::size_t N, template<class...> class T, class ...As>
    struct drop_last<N, T<As...>> {
        template<class> struct helper;
        template<std::size_t ...Is>
        struct helper<std::index_sequence<Is...>> {
            using type = T<element_t<Is, As...>...>;
        };
        using type = typename helper<std::make_index_sequence<sizeof...(As) - N>>::type;
    };

    /**
     * Drop last N types from the templated type Ty.
     * @tparam N amount of types to drop
     * @tparam Ty templated type
     */
    template<std::size_t N, class Ty>
    using drop_last_t = typename drop_last<N, Ty>::type;

    template<auto, class> struct keep_indices;
    template<auto Array, template<class...> class T, class ...As>
    struct keep_indices<Array, T<As...>> {
        template<std::size_t ...Is> struct helper {
            using type = T<element_t<Is, As...>...>;
        };
        using type = typename array_to_pack_t<Array, helper>::type;
    };

    /**
     * Only keep the types at the indices in Array.
     * @tparam Array std::array of indices
     * @tparam T templated type
     */
    template<auto Array, class T>
    using keep_indices_t = typename keep_indices<Array, T>::type;

    template<auto, class> struct remove_indices;
    template<auto Array, template<class...> class T, class ...As>
    struct remove_indices<Array, T<As...>> {
        template<std::size_t ...Is> struct helper {
            using type = keep_indices_t<generate_indices_v<0, sizeof...(As), Is...>, T<As...>>;
        };
        using type = typename array_to_pack_t<Array, helper>::type;
    };

    /**
     * Remove the types at the indices in Array.
     * @tparam Array std::array of indices
     * @tparam T templated type
     */
    template<auto Array, class T>
    using remove_indices_t = typename remove_indices<Array, T>::type;

    template<class, class> struct remove;
    template<class Ty, template<class...> class T, class ...As>
    struct remove<Ty, T<As...>> {
        using type = typename keep_indices<
            indices_except_v<Ty, As...>, T<As...>>::type;
    };

    /**
     * Remove the type T from the template parameters of Ty.
     * @tparam T type, or info<Types...> for multiple
     * @tparam Ty templated type
     */
    template<class T, class Ty>
    using remove_t = typename remove<T, Ty>::type;

    template<class, class> struct keep;
    template<class Ty, template<class...> class T, class ...As>
    struct keep<Ty, T<As...>> {
        using type = typename keep_indices<
            indices_v<Ty, As...>, T<As...>>::type;
    };

    /**
     * Only keep the type T in the template parameters of Ty.
     * @tparam T type, or info<Types...> for multiple
     * @tparam Ty templated type
     */
    template<class T, class Ty>
    using keep_t = typename keep<T, Ty>::type;

    template<std::size_t, class> struct erase;
    template<std::size_t I, template<class...> class T, class ...As>
    struct erase<I, T<As...>> {
        using type = keep_indices_t<
            generate_indices_v<0, sizeof...(As), I>, T<As... >>;
    };

    /**
     * Erase the type at index I from the template parameters of Ty.
     * @tparam I index
     * @tparam Ty templated type
     */
    template<std::size_t I, class Ty>
    using erase_t = typename erase<I, Ty>::type;

    template<class, class> struct append;
    template<class Ty, template<class...> class T, class ...As>
    struct append<Ty, T<As...>> {
        using type = T<As..., Ty>;
    };
    template<class ...Bs, template<class...> class T, class ...As>
    struct append<info<Bs...>, T<As...>> {
        using type = T<As..., Bs...>;
    };

    /**
     * Append T to the template parameters of Ty.
     * @tparam T type or info<Types...> for multiple
     * @tparam Ty templated type
     */
    template<class T, class Ty>
    using append_t = typename append<T, Ty>::type;

    template<class, class> struct prepend;
    template<class Ty, template<class...> class T, class ...As>
    struct prepend<Ty, T<As...>> {
        using type = T<Ty, As...>;
    };
    template<class ...Bs, template<class...> class T, class ...As>
    struct prepend<info<Bs...>, T<As...>> {
        using type = T<Bs..., As...>;
    };

    /**
     * Prepend T to the template parameters of Ty.
     * @tparam T type or info<Types...> for multiple
     * @tparam Ty templated type
     */
    template<class T, class Ty>
    using prepend_t = typename prepend<T, Ty>::type;

    template<std::size_t I, class T, class Ty>
    struct insert {
        using _as_info = move_tparams_t<Ty, info>;
        using _result = append_t<drop_t<I, _as_info>, append_t<T, take_t<I, _as_info>>>;

        template<class, class> struct helper;
        template<class Info, template<class...> class Ty, class ...As>
        struct helper<Info, Ty<As...>> {
            using type = move_tparams_t<Info, Ty>;
        };

        using type = typename helper<_result, Ty>::type;
    };

    /**
     * Insert T at I in the template parameters of Ty.
     * @tparam I index
     * @tparam T type or info<Types...> for multiple
     * @tparam Ty templated type
     */
    template<std::size_t I, class T, class Ty>
    using insert_t = typename insert<I, T, Ty>::type;

    template<class> struct unique;
    template<template<class...> class T, class ...As>
    struct unique<T<As...>> {
        template<class> struct helper;
        template<std::size_t ...Is>
        struct helper<std::index_sequence<Is...>> {
            using type = T<element_t<
                first_indices_v<As...>[Is], As...>...>;
        };
        using type = typename helper<
            std::make_index_sequence<unique_count_v<As...>>>::type;
    };

    /**
     * Only keep unique types in the template parameters of Ty.
     * @tparam Ty templated type
     */
    template<class Ty>
    using unique_t = typename unique<Ty>::type;

    template<std::size_t S, std::size_t E, class Ty>
    using sub_t = take_t<(E - S), drop_t<S, Ty>>;

    /**
     * Only keep the types from index S to E in the
     * template parameters of Ty.
     * @tparam Ty templated type
     */
    template<std::size_t S, std::size_t E, class Ty>
    struct sub {
        using type = sub_t<S, E, Ty>;
    };

    template<class L>
    struct filter_object_wrapper {
        consteval filter_object_wrapper(L value) : value(value) {}
        L value;
    };

    template<class L, std::size_t I, class Ty>
    concept _call_type0 = requires (L l) { // Only type -> bool
        { l.template operator() < Ty > () } -> convertible_to<bool>;
    };
    
    template<class L, std::size_t I, class Ty>
    concept _call_type1 = requires (L l) { // Index, type -> bool
        { l.template operator() < I, Ty > () } -> convertible_to<bool>;
    };
    
    template<class L, std::size_t I, class Ty>
    concept _call_type2 = requires (L l) { // Only index -> bool
        { l.template operator() < I > () } -> convertible_to<bool>;
    };
    
    template<class L, std::size_t I, class Ty> 
    concept _call_type3 = requires (L l) { // Type -> void (type constraint)
        { l.template operator() < Ty > () } -> same_as<void>;
    };
    
    template<class L, std::size_t I, class Ty> // Type trait
    concept _call_type4 = requires (L) { L::template value<Ty>; };

    template<class L, std::size_t I, class Ty>// Value
    concept _call_type5 = requires(L l) { { l == Ty::value } -> convertible_to<bool>; };
    
    template<class L, std::size_t I, class Ty>// Value
    concept _call_type6 = std::same_as<L, bool>;

    template<class L>
    struct wrap_filter_object { using type = filter_object_wrapper<L>; };
    template<class L> requires is_type_trait<L>
    struct wrap_filter_object<L> { using type = L; };


    /**
     * Filter object with operator overloads for several cases
     * for the type filter.
     * @tparam L filter type
     */
    template<class L>
    struct filter_object : wrap_filter_object<L>::type {
        template<std::size_t I, class Ty> consteval bool call() {
            if constexpr (_call_type6<L, I, Ty>) return this->value;
            else if constexpr (_call_type0<L, I, Ty>) return this->value.template operator() < Ty > ();
            else if constexpr (_call_type1<L, I, Ty>) return this->value.template operator() < I, Ty > ();
            else if constexpr (_call_type2<L, I, Ty>) return this->value.template operator() < I > ();
            else if constexpr (_call_type3<L, I, Ty>) return true;
            else if constexpr (_call_type4<L, I, Ty>) return L::template value<Ty>;
            else if constexpr (_call_type5<L, I, Ty>) return this->value == Ty::value;
            else return false; // Otherwise always return false
        }
    };

    template<class T> filter_object(T)->filter_object<T>;

    template<auto Filter, class ...Args>
    consteval std::size_t count_filter_impl(info<Args...>) {
        return ((static_cast<std::size_t>(filter_object{ Filter }.
            template call<Args::template element<0>::value,
            typename Args::template element<1>::type>())) + ... + 0);
    }

    template<auto Filter, class ...Args>
    struct count_filter {
        template<class> struct helper;
        template<std::size_t ...Is>
        struct helper<std::index_sequence<Is...>> {
            consteval static std::size_t value() {
                return ((static_cast<std::size_t>(filter_object{ Filter }.template call<Is, Args>())) + ... + 0);
            }
        };

        constexpr static std::size_t value = helper<std::index_sequence_for<Args...>>::value();
    };

    /**
     * Amount of types in ...Args that match Filter.
     * @tparam Filter lambda or type trait object
     * @tparam ...Args types
     */
    template<auto Filter, class ...Args>
    constexpr std::size_t count_filter_v = count_filter<Filter, Args...>::value;

    template<auto Filter, class ...Args>
    struct indices_filter {
        constexpr static std::array<std::size_t,
            count_filter_v<Filter, Args...>> value = [] {
            std::array<std::size_t, count_filter_v<Filter, Args...>> _indices{};
            std::size_t _index = 0, _match = 0;
            [&]<std::size_t ...Is>(std::index_sequence<Is...>) {
                ((filter_object{ Filter }.template call<Is, Args>() ?
                    _indices[_match++] = _index++ : ++_index), ...);
            }(std::index_sequence_for<Args...>{});
            return _indices;
        }();
    };

    template<auto Filter, class ...Args>
    struct indices_filter<Filter, info<Args...>> : indices_filter<Filter, Args...> {};

    /**
     * Get indices of types in ...Args that match Filter
     * given in the form of a Lambda, or type trait object.
     * @tparam Filter lambda or type trait object
     * @tparam ...Args types
     */
    template<auto Filter, class ...Args>
    constexpr std::array<std::size_t, count_filter_v<Filter, Args...>>
        indices_filter_v = indices_filter<Filter, Args...>::value;

    template<auto, class> struct filter;
    template<auto Filter, template<class...> class T, class ...Args>
    struct filter<Filter, T<Args...>> {
        template<class> struct helper;
        template<std::size_t ...Is>
        struct helper<std::index_sequence<Is...>> {
            using type = T<element_t<indices_filter_v<Filter, Args...>[Is], Args...>...>;
        };
        using type = typename helper<
            std::make_index_sequence<count_filter_v<Filter, Args...>>>::type;
    };

    /**
     * Filter the template parameters of Ty using Filter
     * @tparam Filter lambda or type trait object
     * @tparam Ty templated type
     */
    template<auto Filter, class Ty>
    using filter_t = typename filter<Filter, Ty>::type;

    template<auto Sorter, class A, class B>
    constexpr bool type_sorter_result = Sorter.operator() < A, B > ();

    template<auto, class, class> struct type_merge_sort_merge;
    template<auto Sorter, class A, class ...As, class B, class ...Bs>
        requires type_sorter_result<Sorter, A, B>
    struct type_merge_sort_merge<Sorter, info<A, As...>, info<B, Bs...>> {
        using _recurse = typename type_merge_sort_merge<Sorter, info<As...>, info<B, Bs...>>::type;
        using type = append_t<_recurse, info<A>>;
    };

    template<auto Sorter, class A, class ...As, class B, class ...Bs>
        requires (!type_sorter_result<Sorter, A, B>)
    struct type_merge_sort_merge<Sorter, info<A, As...>, info<B, Bs...>> {
        using _recurse = typename type_merge_sort_merge<Sorter, info<A, As...>, info<Bs...>>::type;
        using type = append_t<_recurse, info<B>>;
    };

    template<auto Sorter, class ...As>
    struct type_merge_sort_merge<Sorter, info<As...>, info<>> { using type = info<As...>; };

    template<auto Sorter, class ...Bs>
    struct type_merge_sort_merge<Sorter, info<>, info<Bs...>> { using type = info<Bs...>; };

    template<auto Sorter, class Ty> struct type_merge_sort
        : type_merge_sort<Sorter, move_tparams<Ty, info>> {};
    template<auto Sorter, class ...Tys>
    struct type_merge_sort<Sorter, info<Tys...>> {
        constexpr static std::size_t _mid = sizeof...(Tys) / 2.;
        using _left = typename type_merge_sort<Sorter, typename info<Tys...>::template take<_mid>>::type;
        using _right = typename type_merge_sort<Sorter, typename info<Tys...>::template drop<_mid>>::type;
        using type = typename type_merge_sort_merge<Sorter, _left, _right>::type;
    };

    template<auto Sorter, class Ty>
    struct type_merge_sort<Sorter, info<Ty>> {
        using type = info<Ty>;
    };

    /**
     * Merge sort algorithm for the template parameters of Ty.
     * @tparam Sorter lambda that takes 2 template parameters and returns a bool
     * @tparam Ty templated type
     */
    template<auto Sorter, class Ty>
    using type_merge_sort_t = typename type_merge_sort<Sorter, Ty>::type;

    template<auto Sorter, class Ty>
    struct sort_types {
        using type = type_merge_sort_t<Sorter, Ty>;
    };

    /**
     * Sorting algorithm for the template parameters of Ty.
     * @tparam Sorter lambda that takes 2 template parameters and returns a bool
     * @tparam Ty templated type
     */
    template<auto Sorter, class Ty>
    using sort_types_t = typename type_merge_sort<Sorter, Ty>::type;

    /**
     * Some often used sorting methods for types.
     */
    namespace type_sorters {
        constexpr auto size = []<class A, class B>{ return sizeof_v<A> < sizeof_v<B>; };
        constexpr auto rsize = []<class A, class B>{ return sizeof_v<A> > sizeof_v<B>; };
        constexpr auto alignment = []<class A, class B>{ return alignof_v<A> < alignof_v<B>; };
        constexpr auto ralignment = []<class A, class B>{ return alignof_v<A> > alignof_v<B>; };
    }

    template<class...>struct concat { using type = info<>; };
    template<template<class...> class A, class ...As>
    struct concat<A<As...>> { using type = A<As...>; };
    template<template<class...> class A, template<class...> class B, class ...As, class ...Bs, class ...Rest>
    struct concat<A<As...>, B<Bs...>, Rest...> { using type = typename concat<A<As..., Bs...>, Rest...>::type; };

    /**
     * Concat all template parameters of all templated
     * types in ...Tys.
     * @tparam ...Tys templated types
     */
    template<class ...Tys>
    using concat_t = typename concat<Tys...>::type;

    template<class... As> struct zip {
        using _first = info<typename info<As...>::template type<0>>;
        template<class A, std::size_t I> using a_a_i = typename info<A>::tparams::template element<I>::type;
        template<std::size_t I> using at_index = typename _first::template reinstantiate<a_a_i<As, I>...>::type;
        template<std::size_t ...Is> struct helper {
            using type = typename _first::template reinstantiate<at_index<Is>...>::type;
        };
        using type = array_to_pack_t<generate_indices_v<0, std::min({ info<As>::tparams::size... })>, helper>::type;
    };

    template<class A> struct zip<A> { using type = A; };
    template<> struct zip<> { using type = info<>; };

    /**
     * Zip all types in the info's ...As
     * @tparam ...As info types
     */
    template<class ...As> using zip_t = typename zip<As...>::type;

    template<class...> struct cartesian_helper;
    template<template<class...> class T, class ...As>
    struct cartesian_helper<T<As...> > {
        using type = T<As...>;
    };

    template<template<class...> class T, class...As>
    struct cartesian_helper<T<T<>>, As... > {
        using type = T<>;
    };

    template<template<class...> class T, class...As, class ...Cs>
    struct cartesian_helper<T<As...>, T<>, Cs... > {
        using type = T<>;
    };

    template<template<class...> class T, class ...As, class B, class ...Cs>
    struct cartesian_helper<T<As...>, T<B>, Cs...> {
        using type1 = T<concat_t<As, T<B>>...>;
        using type = typename cartesian_helper<type1, Cs...>::type;
    };

    template<template<class...> class T, class ...As, class B, class ...Bs, class ...Cs>
    struct cartesian_helper<T<As...>, T<B, Bs...>, Cs...> {
        using type1 = T<concat_t<As, T<B>>...>;
        using type2 = typename cartesian_helper<T<As...>, T<Bs...> >::type;
        using type3 = concat_t<type1, type2>;
        using type = typename cartesian_helper<type3, Cs...>::type;
    };

    template<class...> struct cartesian;
    template<template<class...> class T, class...As, class... Tys>
    struct cartesian<T<As...>, Tys...> {
        using type = typename cartesian_helper<T<T<As>...>, Tys...>::type;
    };

    /**
     * Get the cartesian product of the template types
     * of all templated types ...Tys.
     * @tparam ...Tys templated types
     */
    template<class ...Tys>
    using cartesian_t = typename cartesian<Tys...>::type;

    template<template<class...> class T, class Ty>
    struct transform { using type = T<Ty>; };

    template<template<class...> class T, class ...As>
    struct transform<T, info<As...>> { using type = info<T<As>...>; };

    template<template<class...> class T, class Ty>
    using transform_t = typename transform<T, Ty>::type;
    
    template<auto Filter, template<class...> class T, class Ty>
    struct conditional_transform { using type = Ty; };
    
    template<auto Filter, template<class...> class T, class Ty>
        requires (filter_object{ Filter }.template call<0, Ty>())
    struct conditional_transform<Filter, T, Ty> { using type = T<Ty>; };

    template<auto Filter, template<class...> class T, class ...As>
    struct conditional_transform<Filter, T, info<As...>> {
        using type = info<typename conditional_transform<Filter, T, As>::type...>;
    };

    /**
     * Conditionally transform Ty using T if matched Filter
     * @tparam Filter filter
     * @tparam T transform
     * @tparam Ty type
     */
    template<auto Filter, template<class...> class T, class Ty>
    using conditional_transform_t = typename conditional_transform<Filter, T, Ty>::type;

    template<class A, class B, class ...Args> struct replace {
        using type = typename conditional_transform<is_same<A>, typename partial_last<change, B>::type, info<Args...>>::type;
    };
    template<class ...As, class B, class ...Args> 
    struct replace<info<As...>, B, Args...> {
        using type = typename conditional_transform<(is_same<As> || ...), typename partial_last<change, B>::type, info<Args...>>::type;
    };
    template<class A, class B, class ...Args>
    struct replace<A, B, info<Args...>> {
        using type = typename replace<A, B, Args...>::type;
    };

    /**
     * Replace A with B in ...Args
     * @tparam A type to replace, or info containing multiple types
     * @tparam B type to replace with
     * @tparam ...Args pack or single info to replace in
     */
    template<class A, class B, class ...Args>
    using replace_t = typename replace<A, B, Args...>::type;

    /**
     * Allows for a transform inside a filter. Used like:
     *   with<transform>(filter)
     */
    template<template<class...> class Ty> struct with_impl {
        template<class Trait> struct trait {
            template<class...Tys> struct type {
                constexpr static bool value = filter_object{ Trait{} }.template call<0, Ty<Tys...>>();
            };
        };

        template<class Ty> constexpr auto operator()(Ty) const 
            -> kaixo::type_trait<typename trait<Ty>::type> { return {}; }
    };

    template<template<class...> class Ty>
    constexpr auto with = with_impl<Ty>{};

    // Test if has certain static member
    namespace has {
        template<class Ty> concept type_v = requires (Ty) { typename Ty::type; };
        template<class Ty> concept value_v = requires (Ty) { Ty::value; };
        template<class Ty> concept size_v = requires (Ty) { Ty::size; };
        template<class Ty> concept off_type_v = requires (Ty) { typename Ty::off_type; };
        template<class Ty> concept state_type_v = requires (Ty) { typename Ty::state_type; };
        template<class Ty> concept int_type_v = requires (Ty) { typename Ty::int_type; };
        template<class Ty> concept pos_type_v = requires (Ty) { typename Ty::pos_type; };
        template<class Ty> concept char_type_v = requires (Ty) { typename Ty::char_type; };
        template<class Ty> concept comparison_category_v = requires (Ty) { typename Ty::comparison_category; };
        template<class Ty> concept traits_type_v = requires (Ty) { typename Ty::traits_type; };
        template<class Ty> concept string_type_v = requires (Ty) { typename Ty::string_type; };
        template<class Ty> concept format_v = requires (Ty) { typename Ty::format; };
        template<class Ty> concept iterator_category_v = requires (Ty) { typename Ty::iterator_category; };
        template<class Ty> concept iterator_concept_v = requires (Ty) { typename Ty::iterator_concept; };
        template<class Ty> concept key_type_v = requires (Ty) { typename Ty::key_type; };
        template<class Ty> concept mapped_type_v = requires (Ty) { typename Ty::mapped_type; };
        template<class Ty> concept key_compare_v = requires (Ty) { typename Ty::key_compare; };
        template<class Ty> concept value_compare_v = requires (Ty) { typename Ty::value_compare; };
        template<class Ty> concept node_type_v = requires (Ty) { typename Ty::node_type; };
        template<class Ty> concept insert_return_type_v = requires (Ty) { typename Ty::insert_return_type; };
        template<class Ty> concept value_type_v = requires (Ty) { typename Ty::value_type; };
        template<class Ty> concept allocator_type_v = requires (Ty) { typename Ty::allocator_type; };
        template<class Ty> concept size_type_v = requires (Ty) { typename Ty::size_type; };
        template<class Ty> concept difference_type_v = requires (Ty) { typename Ty::difference_type; };
        template<class Ty> concept reference_v = requires (Ty) { typename Ty::reference; };
        template<class Ty> concept const_reference_v = requires (Ty) { typename Ty::const_reference; };
        template<class Ty> concept pointer_v = requires (Ty) { typename Ty::pointer; };
        template<class Ty> concept const_pointer_v = requires (Ty) { typename Ty::const_pointer; };
        template<class Ty> concept iterator_v = requires (Ty) { typename Ty::iterator; };
        template<class Ty> concept const_iterator_v = requires (Ty) { typename Ty::const_iterator; };
        template<class Ty> concept reverse_iterator_v = requires (Ty) { typename Ty::reverse_iterator; };
        template<class Ty> concept const_reverse_iterator_v = requires (Ty) { typename Ty::const_reverse_iterator; };

        template<class Ty> struct type_impl : std::bool_constant<type_v<Ty>> {};
        template<class Ty> struct value_impl : std::bool_constant<value_v<Ty>> {};
        template<class Ty> struct size_impl : std::bool_constant<size_v<Ty>> {};
        template<class Ty> struct off_type_impl : std::bool_constant<off_type_v<Ty>> {};
        template<class Ty> struct state_type_impl : std::bool_constant<state_type_v<Ty>> {};
        template<class Ty> struct int_type_impl : std::bool_constant<int_type_v<Ty>> {};
        template<class Ty> struct pos_type_impl : std::bool_constant<pos_type_v<Ty>> {};
        template<class Ty> struct char_type_impl : std::bool_constant<char_type_v<Ty>> {};
        template<class Ty> struct comparison_category_impl : std::bool_constant<comparison_category_v<Ty>> {};
        template<class Ty> struct traits_type_impl : std::bool_constant<traits_type_v<Ty>> {};
        template<class Ty> struct string_type_impl : std::bool_constant<string_type_v<Ty>> {};
        template<class Ty> struct format_impl : std::bool_constant<format_v<Ty>> {};
        template<class Ty> struct iterator_category_impl : std::bool_constant<iterator_category_v<Ty>> {};
        template<class Ty> struct iterator_concept_impl : std::bool_constant<iterator_concept_v<Ty>> {};
        template<class Ty> struct key_type_impl : std::bool_constant<key_type_v<Ty>> {};
        template<class Ty> struct mapped_type_impl : std::bool_constant<mapped_type_v<Ty>> {};
        template<class Ty> struct key_compare_impl : std::bool_constant<key_compare_v<Ty>> {};
        template<class Ty> struct value_compare_impl : std::bool_constant<value_compare_v<Ty>> {};
        template<class Ty> struct node_type_impl : std::bool_constant<node_type_v<Ty>> {};
        template<class Ty> struct insert_return_type_impl : std::bool_constant<insert_return_type_v<Ty>> {};
        template<class Ty> struct value_type_impl : std::bool_constant<value_type_v<Ty>> {};
        template<class Ty> struct allocator_type_impl : std::bool_constant<allocator_type_v<Ty>> {};
        template<class Ty> struct size_type_impl : std::bool_constant<size_type_v<Ty>> {};
        template<class Ty> struct difference_type_impl : std::bool_constant<difference_type_v<Ty>> {};
        template<class Ty> struct reference_impl : std::bool_constant<reference_v<Ty>> {};
        template<class Ty> struct const_reference_impl : std::bool_constant<const_reference_v<Ty>> {};
        template<class Ty> struct pointer_impl : std::bool_constant<pointer_v<Ty>> {};
        template<class Ty> struct const_pointer_impl : std::bool_constant<const_pointer_v<Ty>> {};
        template<class Ty> struct iterator_impl : std::bool_constant<iterator_v<Ty>> {};
        template<class Ty> struct const_iterator_impl : std::bool_constant<const_iterator_v<Ty>> {};
        template<class Ty> struct reverse_iterator_impl : std::bool_constant<reverse_iterator_v<Ty>> {};
        template<class Ty> struct const_reverse_iterator_impl : std::bool_constant<const_reverse_iterator_v<Ty>> {};

        constexpr auto type = type_trait<type_impl>{};
        constexpr auto value = type_trait<value_impl>{};
        constexpr auto size = type_trait<size_impl>{};
        constexpr auto off_type = type_trait<off_type_impl>{};
        constexpr auto state_type = type_trait<state_type_impl>{};
        constexpr auto int_type = type_trait<int_type_impl>{};
        constexpr auto pos_type = type_trait<pos_type_impl>{};
        constexpr auto char_type = type_trait<char_type_impl>{};
        constexpr auto comparison_category = type_trait<comparison_category_impl>{};
        constexpr auto traits_type = type_trait<traits_type_impl>{};
        constexpr auto string_type = type_trait<string_type_impl>{};
        constexpr auto format = type_trait<format_impl>{};
        constexpr auto iterator_category = type_trait<iterator_category_impl>{};
        constexpr auto iterator_concept = type_trait<iterator_concept_impl>{};
        constexpr auto key_type = type_trait<key_type_impl>{};
        constexpr auto mapped_type = type_trait<mapped_type_impl>{};
        constexpr auto key_compare = type_trait<key_compare_impl>{};
        constexpr auto value_compare = type_trait<value_compare_impl>{};
        constexpr auto node_type = type_trait<node_type_impl>{};
        constexpr auto insert_return_type = type_trait<insert_return_type_impl>{};
        constexpr auto value_type = type_trait<value_type_impl>{};
        constexpr auto allocator_type = type_trait<allocator_type_impl>{};
        constexpr auto size_type = type_trait<size_type_impl>{};
        constexpr auto difference_type = type_trait<difference_type_impl>{};
        constexpr auto reference = type_trait<reference_impl>{};
        constexpr auto const_reference = type_trait<const_reference_impl>{};
        constexpr auto pointer = type_trait<pointer_impl>{};
        constexpr auto const_pointer = type_trait<const_pointer_impl>{};
        constexpr auto iterator = type_trait<iterator_impl>{};
        constexpr auto const_iterator = type_trait<const_iterator_impl>{};
        constexpr auto reverse_iterator = type_trait<reverse_iterator_impl>{};
        constexpr auto const_reverse_iterator = type_trait<const_reverse_iterator_impl>{};
    }

    /**
     * Grab a certain member in a transform operation.
     * Most member types in the standard are here.
     */
    namespace grab {
        template<class Ty> struct type_impl { using type = Ty; }; 
        template<has::type_v Ty> struct type_impl<Ty> { using type = typename Ty::type; };
        template<class Ty> struct value_impl { using type = Ty; }; 
        template<has::value_v Ty> struct value_impl<Ty> { using type = value_t<Ty::value>; };
        template<class Ty> struct size_impl { using type = Ty; }; 
        template<has::size_v Ty> struct size_impl<Ty> { using type = value_t<Ty::size>; };
        template<class Ty> struct off_type_impl { using type = Ty; }; 
        template<has::off_type_v Ty> struct off_type_impl<Ty> { using type = typename Ty::off_type; };
        template<class Ty> struct state_type_impl { using type = Ty; }; 
        template<has::state_type_v Ty> struct state_type_impl<Ty> { using type = typename Ty::state_type; };
        template<class Ty> struct int_type_impl { using type = Ty; }; 
        template<has::int_type_v Ty> struct int_type_impl<Ty> { using type = typename Ty::int_type; };
        template<class Ty> struct pos_type_impl { using type = Ty; }; 
        template<has::pos_type_v Ty> struct pos_type_impl<Ty> { using type = typename Ty::pos_type; };
        template<class Ty> struct char_type_impl { using type = Ty; }; 
        template<has::char_type_v Ty> struct char_type_impl<Ty> { using type = typename Ty::char_type; };
        template<class Ty> struct comparison_category_impl { using type = Ty; }; 
        template<has::comparison_category_v Ty> struct comparison_category_impl<Ty> { using type = typename Ty::comparison_category; };
        template<class Ty> struct traits_type_impl { using type = Ty; }; 
        template<has::traits_type_v Ty> struct traits_type_impl<Ty> { using type = typename Ty::traits_type; };
        template<class Ty> struct string_type_impl { using type = Ty; }; 
        template<has::string_type_v Ty> struct string_type_impl<Ty> { using type = typename Ty::string_type; };
        template<class Ty> struct format_impl { using type = Ty; }; 
        template<has::format_v Ty> struct format_impl<Ty> { using type = typename Ty::format; };
        template<class Ty> struct iterator_category_impl { using type = Ty; }; 
        template<has::iterator_category_v Ty> struct iterator_category_impl<Ty> { using type = typename Ty::iterator_category; };
        template<class Ty> struct iterator_concept_impl { using type = Ty; }; 
        template<has::iterator_concept_v Ty> struct iterator_concept_impl<Ty> { using type = typename Ty::iterator_concept; };
        template<class Ty> struct key_type_impl { using type = Ty; }; 
        template<has::key_type_v Ty> struct key_type_impl<Ty> { using type = typename Ty::key_type; };
        template<class Ty> struct mapped_type_impl { using type = Ty; }; 
        template<has::mapped_type_v Ty> struct mapped_type_impl<Ty> { using type = typename Ty::mapped_type; };
        template<class Ty> struct key_compare_impl { using type = Ty; }; 
        template<has::key_compare_v Ty> struct key_compare_impl<Ty> { using type = typename Ty::key_compare; };
        template<class Ty> struct value_compare_impl { using type = Ty; }; 
        template<has::value_compare_v Ty> struct value_compare_impl<Ty> { using type = typename Ty::value_compare; };
        template<class Ty> struct node_type_impl { using type = Ty; }; 
        template<has::node_type_v Ty> struct node_type_impl<Ty> { using type = typename Ty::node_type; };
        template<class Ty> struct insert_return_type_impl { using type = Ty; }; 
        template<has::insert_return_type_v Ty> struct insert_return_type_impl<Ty> { using type = typename Ty::insert_return_type; };
        template<class Ty> struct value_type_impl { using type = Ty; }; 
        template<has::value_type_v Ty> struct value_type_impl<Ty> { using type = typename Ty::value_type; };
        template<class Ty> struct allocator_type_impl { using type = Ty; }; 
        template<has::allocator_type_v Ty> struct allocator_type_impl<Ty> { using type = typename Ty::allocator_type; };
        template<class Ty> struct size_type_impl { using type = Ty; }; 
        template<has::size_type_v Ty> struct size_type_impl<Ty> { using type = typename Ty::size_type; };
        template<class Ty> struct difference_type_impl { using type = Ty; }; 
        template<has::difference_type_v Ty> struct difference_type_impl<Ty> { using type = typename Ty::difference_type; };
        template<class Ty> struct reference_impl { using type = Ty; }; 
        template<has::reference_v Ty> struct reference_impl<Ty> { using type = typename Ty::reference; };
        template<class Ty> struct const_reference_impl { using type = Ty; }; 
        template<has::const_reference_v Ty> struct const_reference_impl<Ty> { using type = typename Ty::const_reference; };
        template<class Ty> struct pointer_impl { using type = Ty; }; 
        template<has::pointer_v Ty> struct pointer_impl<Ty> { using type = typename Ty::pointer; };
        template<class Ty> struct const_pointer_impl { using type = Ty; }; 
        template<has::const_pointer_v Ty> struct const_pointer_impl<Ty> { using type = typename Ty::const_pointer; };
        template<class Ty> struct iterator_impl { using type = Ty; }; 
        template<has::iterator_v Ty> struct iterator_impl<Ty> { using type = typename Ty::iterator; };
        template<class Ty> struct const_iterator_impl { using type = Ty; }; 
        template<has::const_iterator_v Ty> struct const_iterator_impl<Ty> { using type = typename Ty::const_iterator; };
        template<class Ty> struct reverse_iterator_impl { using type = Ty; }; 
        template<has::reverse_iterator_v Ty> struct reverse_iterator_impl<Ty> { using type = typename Ty::reverse_iterator; };
        template<class Ty> struct const_reverse_iterator_impl { using type = Ty; }; 
        template<has::const_reverse_iterator_v Ty> struct const_reverse_iterator_impl<Ty> { using type = typename Ty::const_reverse_iterator; };

        template<class Ty> using type = type_impl<Ty>::type;
        template<class Ty> using value = value_impl<Ty>::type;
        template<class Ty> using size = size_impl<Ty>::type;
        template<class Ty> using off_type = off_type_impl<Ty>::type;
        template<class Ty> using state_type = state_type_impl<Ty>::type;
        template<class Ty> using int_type = int_type_impl<Ty>::type;
        template<class Ty> using pos_type = pos_type_impl<Ty>::type;
        template<class Ty> using char_type = char_type_impl<Ty>::type;
        template<class Ty> using comparison_category = comparison_category_impl<Ty>::type;
        template<class Ty> using traits_type = traits_type_impl<Ty>::type;
        template<class Ty> using string_type = string_type_impl<Ty>::type;
        template<class Ty> using format = format_impl<Ty>::type;
        template<class Ty> using iterator_category = iterator_category_impl<Ty>::type;
        template<class Ty> using iterator_concept = iterator_concept_impl<Ty>::type;
        template<class Ty> using key_type = key_type_impl<Ty>::type;
        template<class Ty> using mapped_type = mapped_type_impl<Ty>::type;
        template<class Ty> using key_compare = key_compare_impl<Ty>::type;
        template<class Ty> using value_compare = value_compare_impl<Ty>::type;
        template<class Ty> using node_type = node_type_impl<Ty>::type;
        template<class Ty> using insert_return_type = insert_return_type_impl<Ty>::type;
        template<class Ty> using value_type = value_type_impl<Ty>::type;
        template<class Ty> using allocator_type = allocator_type_impl<Ty>::type;
        template<class Ty> using size_type = size_type_impl<Ty>::type;
        template<class Ty> using difference_type = difference_type_impl<Ty>::type;
        template<class Ty> using reference = reference_impl<Ty>::type;
        template<class Ty> using const_reference = const_reference_impl<Ty>::type;
        template<class Ty> using pointer = pointer_impl<Ty>::type;
        template<class Ty> using const_pointer = const_pointer_impl<Ty>::type;
        template<class Ty> using iterator = iterator_impl<Ty>::type;
        template<class Ty> using const_iterator = const_iterator_impl<Ty>::type;
        template<class Ty> using reverse_iterator = reverse_iterator_impl<Ty>::type;
        template<class Ty> using const_reverse_iterator = const_reverse_iterator_impl<Ty>::type;
    }

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     *                                            function helpers.                                            *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    constexpr auto unit = []<class Ty>(Ty && i) -> Ty&& { return std::forward<Ty>(i); };
    template<class To>
    constexpr auto cast = []<class Ty>(Ty && i) -> To { return static_cast<To>(std::forward<Ty>(i)); };

#define NO_ARG
#define KAIXO_MEMBER_CALL_C(MAC, V, REF, NOEXCEPT) \
MAC(NO_ARG, V, REF, NOEXCEPT)                       \
MAC(const , V, REF, NOEXCEPT)

#define KAIXO_MEMBER_CALL_V(MAC, REF, NOEXCEPT)   \
KAIXO_MEMBER_CALL_C(MAC, NO_ARG  , REF, NOEXCEPT) \
KAIXO_MEMBER_CALL_C(MAC, volatile, REF, NOEXCEPT)

#define KAIXO_MEMBER_CALL_NOEXCEPT(MAC, NOEXCEPT) \
KAIXO_MEMBER_CALL_V(MAC, NO_ARG, NOEXCEPT)            \
KAIXO_MEMBER_CALL_V(MAC,      &, NOEXCEPT)            \
KAIXO_MEMBER_CALL_V(MAC,     &&, NOEXCEPT)

#define KAIXO_MEMBER_CALL(MAC)            \
KAIXO_MEMBER_CALL_NOEXCEPT(MAC, NO_ARG  ) \
KAIXO_MEMBER_CALL_NOEXCEPT(MAC, noexcept) 

    template<class Ty> concept is_functor = requires(decltype(&Ty::operator()) a) { a; };

    template<class> struct function_info_impl;
    template<is_functor Ty> struct function_info_impl<Ty>
    : function_info_impl<decltype(&Ty::operator())> {};

#define KAIXO_MEMBER_FUNCTION_INFO_MOD(CONST, VOLATILE, REF, NOEXCEPT)                   \
template<class Ty, class R, class ...Args>                                               \
struct function_info_impl<R(Ty::*)(Args...) CONST VOLATILE REF NOEXCEPT> {               \
    using pointer = R(*)(Args...) NOEXCEPT;                                              \
    using signature = R(Args...) CONST VOLATILE REF NOEXCEPT;                            \
    using object = CONST VOLATILE Ty REF;                                                \
    using result = R;                                                                    \
    using arguments = info<Args...>;                                                     \
    constexpr static bool is_fun_const = std::same_as<const int, CONST int>;             \
    constexpr static bool is_fun_mutable = !is_fun_const;                                \
    constexpr static bool is_fun_volatile = std::same_as<volatile int, VOLATILE int>;    \
    constexpr static bool is_fun_lvalue_reference = std::same_as<int&, int REF>;         \
    constexpr static bool is_fun_rvalue_reference = std::same_as<int&&, int REF>;        \
    constexpr static bool is_fun_reference = std::is_reference_v<int REF>;               \
    constexpr static bool is_noexcept = std::same_as<void() noexcept, void() NOEXCEPT>;  \
    using add_fun_const = R(Ty::*)(Args...) const VOLATILE REF NOEXCEPT;                 \
    using remove_fun_const = R(Ty::*)(Args...) VOLATILE REF NOEXCEPT;                    \
    using add_fun_volatile = R(Ty::*)(Args...) CONST volatile REF NOEXCEPT;              \
    using remove_fun_volatile = R(Ty::*)(Args...) CONST REF NOEXCEPT;                    \
    using add_fun_cv = R(Ty::*)(Args...) const volatile REF NOEXCEPT;                    \
    using remove_fun_cv = R(Ty::*)(Args...) REF NOEXCEPT;                                \
    using add_fun_lvalue_reference = R(Ty::*)(Args...) CONST VOLATILE & NOEXCEPT;        \
    using add_fun_rvalue_reference = R(Ty::*)(Args...) CONST VOLATILE && NOEXCEPT;       \
    using remove_fun_reference = R(Ty::*)(Args...) CONST VOLATILE NOEXCEPT;              \
    using remove_fun_cvref = R(Ty::*)(Args...) NOEXCEPT;                                 \
    using add_noexcept = R(Ty::*)(Args...) CONST VOLATILE REF noexcept;                  \
    using remove_noexcept = R(Ty::*)(Args...) CONST VOLATILE REF;                        \
};

    KAIXO_MEMBER_CALL(KAIXO_MEMBER_FUNCTION_INFO_MOD);
#undef KAIXO_MEMBER_FUNCTION_INFO_MOD

#define KAIXO_FUNCTION_INFO_MOD(CONST, VOLATILE, REF, NOEXCEPT)                         \
template<class R, class ...Args>                                                        \
struct function_info_impl<R(Args...) CONST VOLATILE REF NOEXCEPT> {                     \
    using pointer = R(*)(Args...) NOEXCEPT;                                             \
    using signature = R(Args...) CONST VOLATILE REF NOEXCEPT;                           \
    using result = R;                                                                   \
    using arguments = info<Args...>;                                                    \
    constexpr static bool is_fun_const = std::same_as<const int, CONST int>;            \
    constexpr static bool is_fun_mutable = !is_fun_const;                               \
    constexpr static bool is_fun_volatile = std::same_as<volatile int, VOLATILE int>;   \
    constexpr static bool is_fun_lvalue_reference = std::same_as<int&, int REF>;        \
    constexpr static bool is_fun_rvalue_reference = std::same_as<int&&, int REF>;       \
    constexpr static bool is_fun_reference = std::is_reference_v<int REF>;              \
    constexpr static bool is_noexcept = std::same_as<void() noexcept, void() NOEXCEPT>; \
    using add_fun_const = R(Args...) const VOLATILE REF NOEXCEPT;                       \
    using remove_fun_const = R(Args...) VOLATILE REF NOEXCEPT;                          \
    using add_fun_volatile = R(Args...) CONST volatile REF NOEXCEPT;                    \
    using remove_fun_volatile = R(Args...) CONST REF NOEXCEPT;                          \
    using add_fun_cv = R(Args...) const volatile REF NOEXCEPT;                          \
    using remove_fun_cv = R(Args...) REF NOEXCEPT;                                      \
    using add_fun_lvalue_reference = R(Args...) CONST VOLATILE & NOEXCEPT;              \
    using add_fun_rvalue_reference = R(Args...) CONST VOLATILE && NOEXCEPT;             \
    using remove_fun_reference = R(Args...) CONST VOLATILE NOEXCEPT;                    \
    using remove_fun_cvref = R(Args...) NOEXCEPT;                                       \
    using add_noexcept = R(Args...) CONST VOLATILE REF noexcept;                        \
    using remove_noexcept = R(Args...) CONST VOLATILE REF;                              \
};

    KAIXO_MEMBER_CALL(KAIXO_FUNCTION_INFO_MOD);
#undef KAIXO_FUNCTION_INFO_MOD

#define KAIXO_FUNCTION_PTR_INFO_MOD(NOEXCEPT)                                               \
template<class R, class ...Args>                                                            \
struct function_info_impl<R(*)(Args...) NOEXCEPT> {                                         \
    using pointer = R(*)(Args...) NOEXCEPT;                                                 \
    using signature = R(Args...) NOEXCEPT;                                                  \
    using result = R;                                                                       \
    using arguments = info<Args...>;                                                        \
    constexpr static bool is_noexcept = std::same_as<void() noexcept, void() NOEXCEPT>;     \
    using add_noexcept = R(Args...) noexcept;                                               \
    using remove_noexcept = R(Args...);                                                     \
};

    KAIXO_FUNCTION_PTR_INFO_MOD(NO_ARG);
    KAIXO_FUNCTION_PTR_INFO_MOD(noexcept);
#undef KAIXO_FUNCTION_PTR_INFO_MOD

    template<class Ty> using function_info = function_info_impl<std::remove_cv_t<std::remove_reference_t<Ty>>>;
    template<auto Ty> using function_info_v = function_info_impl<std::remove_cv_t<std::remove_reference_t<decltype(Ty)>>>;
    template<class Ty> concept callable_type = requires(Ty) { typename kaixo::function_info<Ty>::result; };

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     *                                            struct helpers.                                              *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    template<class Ty, class ...Args>
    concept struct_constructible_with = requires (Args...args) {
        { Ty{ args... } };
    };


     /**
      * Find amount of members in a struct.
      * @tparam Ty struct
      */
    template<class Ty>
    struct struct_size {
        /**
         * Finds struct size by trying to construct the struct using
         * a type that's convertible to anything but the struct itself
         * (to prevent copy or move constructor from being called and
         *  making it result in 1 when it should be 0), starting at
         * sizeof(Ty) parameters, and trying 1 less each time until
         * it is constructible.
         */
        constexpr static std::size_t value = reverse_sequence<0, sizeof_v<Ty> +1>([]<std::size_t ...Ns>{
            using convertible_type = not_convertible_to<Ty>;

            std::size_t res = 0;
            constexpr auto try_one = []<std::size_t ...Is> {
                return struct_constructible_with<Ty, change<value_t<Is>, convertible_type>...>;
            };

            ((sequence<Ns>(try_one) ? (res = Ns, true) : false) || ...);
            return res;
        });
    };

    template<class Ty, std::size_t N>
    struct struct_size<std::array<Ty, N>> {
        constexpr static std::size_t value = N;
    };

    /**
     * Find amount of members in a struct.
     * @tparam Ty struct
     */
    template<aggregate Ty>
    constexpr std::size_t struct_size_v = struct_size<Ty>::value;

    /**
     * Find member types of a struct, uses a macro to define
     * overloads up to 99 members using structured bindings.
     */
    template<aggregate Ty, std::size_t N>
    struct struct_members {
        using type = info<Ty>;
    };

    template<aggregate Ty>
    struct struct_members<Ty, 0> {
        using type = info<Ty>;
    };

#define KAIXO_STRUCT_MEMBERS_M(c, ...)                                         \
        template<aggregate Ty>                                                 \
            requires (struct_size_v<Ty> == c)                                  \
        struct struct_members<Ty, c> {                                         \
            using type = typename decltype([](Ty& ty) {                        \
                auto& [__VA_ARGS__] = ty;                                      \
                using tuple_t = decltype(std::tuple{ __VA_ARGS__ });           \
                return move_tparams_t<tuple_t, info>{};                        \
            }(std::declval<Ty&>()));                                           \
        };

#define KAIXO_MERGE(a, b) KAIXO_MERGE1(a, b)
#define KAIXO_MERGE1(a, b) a##b
#define KAIXO_LABEL(x) KAIXO_MERGE(val, x)
#define KAIXO_UNIQUE_NAME KAIXO_LABEL(__COUNTER__)
#define KAIXO_SIZE(...) (std::tuple_size_v<decltype(std::make_tuple(__VA_ARGS__))>)
#define    KAIXO_UNIQUE(m, c, ...) m(c, __VA_ARGS__)
#define  KAIXO_UNIQUE_1(m, c, ...)    KAIXO_UNIQUE(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__)
#define  KAIXO_UNIQUE_2(m, c, ...)  KAIXO_UNIQUE_1(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c -  1), __VA_ARGS__)
#define  KAIXO_UNIQUE_3(m, c, ...)  KAIXO_UNIQUE_2(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c -  2), __VA_ARGS__)
#define  KAIXO_UNIQUE_4(m, c, ...)  KAIXO_UNIQUE_3(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c -  3), __VA_ARGS__)
#define  KAIXO_UNIQUE_5(m, c, ...)  KAIXO_UNIQUE_4(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c -  4), __VA_ARGS__)
#define  KAIXO_UNIQUE_6(m, c, ...)  KAIXO_UNIQUE_5(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c -  5), __VA_ARGS__)
#define  KAIXO_UNIQUE_7(m, c, ...)  KAIXO_UNIQUE_6(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c -  6), __VA_ARGS__)
#define  KAIXO_UNIQUE_8(m, c, ...)  KAIXO_UNIQUE_7(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c -  7), __VA_ARGS__)
#define  KAIXO_UNIQUE_9(m, c, ...)  KAIXO_UNIQUE_8(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c -  8), __VA_ARGS__)
#define KAIXO_UNIQUE_10(m, c, ...)  KAIXO_UNIQUE_9(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c -  9), __VA_ARGS__)
#define KAIXO_UNIQUE_11(m, c, ...) KAIXO_UNIQUE_10(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 10), __VA_ARGS__)
#define KAIXO_UNIQUE_12(m, c, ...) KAIXO_UNIQUE_11(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 11), __VA_ARGS__)
#define KAIXO_UNIQUE_13(m, c, ...) KAIXO_UNIQUE_12(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 12), __VA_ARGS__)
#define KAIXO_UNIQUE_14(m, c, ...) KAIXO_UNIQUE_13(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 13), __VA_ARGS__)
#define KAIXO_UNIQUE_15(m, c, ...) KAIXO_UNIQUE_14(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 14), __VA_ARGS__)
#define KAIXO_UNIQUE_16(m, c, ...) KAIXO_UNIQUE_15(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 15), __VA_ARGS__)
#define KAIXO_UNIQUE_17(m, c, ...) KAIXO_UNIQUE_16(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 16), __VA_ARGS__)
#define KAIXO_UNIQUE_18(m, c, ...) KAIXO_UNIQUE_17(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 17), __VA_ARGS__)
#define KAIXO_UNIQUE_19(m, c, ...) KAIXO_UNIQUE_18(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 18), __VA_ARGS__)
#define KAIXO_UNIQUE_20(m, c, ...) KAIXO_UNIQUE_19(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 19), __VA_ARGS__)
#define KAIXO_UNIQUE_21(m, c, ...) KAIXO_UNIQUE_20(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 20), __VA_ARGS__)
#define KAIXO_UNIQUE_22(m, c, ...) KAIXO_UNIQUE_21(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 21), __VA_ARGS__)
#define KAIXO_UNIQUE_23(m, c, ...) KAIXO_UNIQUE_22(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 22), __VA_ARGS__)
#define KAIXO_UNIQUE_24(m, c, ...) KAIXO_UNIQUE_23(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 23), __VA_ARGS__)
#define KAIXO_UNIQUE_25(m, c, ...) KAIXO_UNIQUE_24(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 24), __VA_ARGS__)
#define KAIXO_UNIQUE_26(m, c, ...) KAIXO_UNIQUE_25(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 25), __VA_ARGS__)
#define KAIXO_UNIQUE_27(m, c, ...) KAIXO_UNIQUE_26(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 26), __VA_ARGS__)
#define KAIXO_UNIQUE_28(m, c, ...) KAIXO_UNIQUE_27(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 27), __VA_ARGS__)
#define KAIXO_UNIQUE_29(m, c, ...) KAIXO_UNIQUE_28(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 28), __VA_ARGS__)
#define KAIXO_UNIQUE_30(m, c, ...) KAIXO_UNIQUE_29(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 29), __VA_ARGS__)
#define KAIXO_UNIQUE_31(m, c, ...) KAIXO_UNIQUE_30(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 30), __VA_ARGS__)
#define KAIXO_UNIQUE_32(m, c, ...) KAIXO_UNIQUE_31(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 31), __VA_ARGS__)
#define KAIXO_UNIQUE_33(m, c, ...) KAIXO_UNIQUE_32(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 32), __VA_ARGS__)
#define KAIXO_UNIQUE_34(m, c, ...) KAIXO_UNIQUE_33(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 33), __VA_ARGS__)
#define KAIXO_UNIQUE_35(m, c, ...) KAIXO_UNIQUE_34(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 34), __VA_ARGS__)
#define KAIXO_UNIQUE_36(m, c, ...) KAIXO_UNIQUE_35(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 35), __VA_ARGS__)
#define KAIXO_UNIQUE_37(m, c, ...) KAIXO_UNIQUE_36(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 36), __VA_ARGS__)
#define KAIXO_UNIQUE_38(m, c, ...) KAIXO_UNIQUE_37(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 37), __VA_ARGS__)
#define KAIXO_UNIQUE_39(m, c, ...) KAIXO_UNIQUE_38(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 38), __VA_ARGS__)
#define KAIXO_UNIQUE_40(m, c, ...) KAIXO_UNIQUE_39(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 39), __VA_ARGS__)
#define KAIXO_UNIQUE_41(m, c, ...) KAIXO_UNIQUE_40(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 40), __VA_ARGS__)
#define KAIXO_UNIQUE_42(m, c, ...) KAIXO_UNIQUE_41(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 41), __VA_ARGS__)
#define KAIXO_UNIQUE_43(m, c, ...) KAIXO_UNIQUE_42(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 42), __VA_ARGS__)
#define KAIXO_UNIQUE_44(m, c, ...) KAIXO_UNIQUE_43(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 43), __VA_ARGS__)
#define KAIXO_UNIQUE_45(m, c, ...) KAIXO_UNIQUE_44(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 44), __VA_ARGS__)
#define KAIXO_UNIQUE_46(m, c, ...) KAIXO_UNIQUE_45(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 45), __VA_ARGS__)
#define KAIXO_UNIQUE_47(m, c, ...) KAIXO_UNIQUE_46(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 46), __VA_ARGS__)
#define KAIXO_UNIQUE_48(m, c, ...) KAIXO_UNIQUE_47(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 47), __VA_ARGS__)
#define KAIXO_UNIQUE_49(m, c, ...) KAIXO_UNIQUE_48(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 48), __VA_ARGS__)
#define KAIXO_UNIQUE_50(m, c, ...) KAIXO_UNIQUE_49(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 49), __VA_ARGS__)
#define KAIXO_UNIQUE_51(m, c, ...) KAIXO_UNIQUE_50(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 50), __VA_ARGS__)
#define KAIXO_UNIQUE_52(m, c, ...) KAIXO_UNIQUE_51(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 51), __VA_ARGS__)
#define KAIXO_UNIQUE_53(m, c, ...) KAIXO_UNIQUE_52(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 52), __VA_ARGS__)
#define KAIXO_UNIQUE_54(m, c, ...) KAIXO_UNIQUE_53(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 53), __VA_ARGS__)
#define KAIXO_UNIQUE_55(m, c, ...) KAIXO_UNIQUE_54(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 54), __VA_ARGS__)
#define KAIXO_UNIQUE_56(m, c, ...) KAIXO_UNIQUE_55(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 55), __VA_ARGS__)
#define KAIXO_UNIQUE_57(m, c, ...) KAIXO_UNIQUE_56(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 56), __VA_ARGS__)
#define KAIXO_UNIQUE_58(m, c, ...) KAIXO_UNIQUE_57(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 57), __VA_ARGS__)
#define KAIXO_UNIQUE_59(m, c, ...) KAIXO_UNIQUE_58(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 58), __VA_ARGS__)
#define KAIXO_UNIQUE_60(m, c, ...) KAIXO_UNIQUE_59(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 59), __VA_ARGS__)
#define KAIXO_UNIQUE_61(m, c, ...) KAIXO_UNIQUE_60(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 60), __VA_ARGS__)
#define KAIXO_UNIQUE_62(m, c, ...) KAIXO_UNIQUE_61(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 61), __VA_ARGS__)
#define KAIXO_UNIQUE_63(m, c, ...) KAIXO_UNIQUE_62(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 62), __VA_ARGS__)
#define KAIXO_UNIQUE_64(m, c, ...) KAIXO_UNIQUE_63(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 63), __VA_ARGS__)
#define KAIXO_UNIQUE_65(m, c, ...) KAIXO_UNIQUE_64(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 64), __VA_ARGS__)
#define KAIXO_UNIQUE_66(m, c, ...) KAIXO_UNIQUE_65(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 65), __VA_ARGS__)
#define KAIXO_UNIQUE_67(m, c, ...) KAIXO_UNIQUE_66(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 66), __VA_ARGS__)
#define KAIXO_UNIQUE_68(m, c, ...) KAIXO_UNIQUE_67(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 67), __VA_ARGS__)
#define KAIXO_UNIQUE_69(m, c, ...) KAIXO_UNIQUE_68(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 68), __VA_ARGS__)
#define KAIXO_UNIQUE_70(m, c, ...) KAIXO_UNIQUE_69(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 69), __VA_ARGS__)
#define KAIXO_UNIQUE_71(m, c, ...) KAIXO_UNIQUE_70(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 70), __VA_ARGS__)
#define KAIXO_UNIQUE_72(m, c, ...) KAIXO_UNIQUE_71(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 71), __VA_ARGS__)
#define KAIXO_UNIQUE_73(m, c, ...) KAIXO_UNIQUE_72(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 72), __VA_ARGS__)
#define KAIXO_UNIQUE_74(m, c, ...) KAIXO_UNIQUE_73(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 73), __VA_ARGS__)
#define KAIXO_UNIQUE_75(m, c, ...) KAIXO_UNIQUE_74(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 74), __VA_ARGS__)
#define KAIXO_UNIQUE_76(m, c, ...) KAIXO_UNIQUE_75(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 75), __VA_ARGS__)
#define KAIXO_UNIQUE_77(m, c, ...) KAIXO_UNIQUE_76(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 76), __VA_ARGS__)
#define KAIXO_UNIQUE_78(m, c, ...) KAIXO_UNIQUE_77(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 77), __VA_ARGS__)
#define KAIXO_UNIQUE_79(m, c, ...) KAIXO_UNIQUE_78(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 78), __VA_ARGS__)
#define KAIXO_UNIQUE_80(m, c, ...) KAIXO_UNIQUE_79(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 79), __VA_ARGS__)
#define KAIXO_UNIQUE_81(m, c, ...) KAIXO_UNIQUE_80(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 80), __VA_ARGS__)
#define KAIXO_UNIQUE_82(m, c, ...) KAIXO_UNIQUE_81(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 81), __VA_ARGS__)
#define KAIXO_UNIQUE_83(m, c, ...) KAIXO_UNIQUE_82(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 82), __VA_ARGS__)
#define KAIXO_UNIQUE_84(m, c, ...) KAIXO_UNIQUE_83(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 83), __VA_ARGS__)
#define KAIXO_UNIQUE_85(m, c, ...) KAIXO_UNIQUE_84(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 84), __VA_ARGS__)
#define KAIXO_UNIQUE_86(m, c, ...) KAIXO_UNIQUE_85(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 85), __VA_ARGS__)
#define KAIXO_UNIQUE_87(m, c, ...) KAIXO_UNIQUE_86(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 86), __VA_ARGS__)
#define KAIXO_UNIQUE_88(m, c, ...) KAIXO_UNIQUE_87(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 87), __VA_ARGS__)
#define KAIXO_UNIQUE_89(m, c, ...) KAIXO_UNIQUE_88(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 88), __VA_ARGS__)
#define KAIXO_UNIQUE_90(m, c, ...) KAIXO_UNIQUE_89(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 89), __VA_ARGS__)
#define KAIXO_UNIQUE_91(m, c, ...) KAIXO_UNIQUE_90(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 90), __VA_ARGS__)
#define KAIXO_UNIQUE_92(m, c, ...) KAIXO_UNIQUE_91(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 91), __VA_ARGS__)
#define KAIXO_UNIQUE_93(m, c, ...) KAIXO_UNIQUE_92(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 92), __VA_ARGS__)
#define KAIXO_UNIQUE_94(m, c, ...) KAIXO_UNIQUE_93(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 93), __VA_ARGS__)
#define KAIXO_UNIQUE_95(m, c, ...) KAIXO_UNIQUE_94(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 94), __VA_ARGS__)
#define KAIXO_UNIQUE_96(m, c, ...) KAIXO_UNIQUE_95(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 95), __VA_ARGS__)
#define KAIXO_UNIQUE_97(m, c, ...) KAIXO_UNIQUE_96(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 96), __VA_ARGS__)
#define KAIXO_UNIQUE_98(m, c, ...) KAIXO_UNIQUE_97(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 97), __VA_ARGS__)
#define KAIXO_UNIQUE_99(m, c, ...) KAIXO_UNIQUE_98(m, c, KAIXO_UNIQUE_NAME, __VA_ARGS__) KAIXO_UNIQUE_1(m, (c - 98), __VA_ARGS__)

    KAIXO_UNIQUE_99(KAIXO_STRUCT_MEMBERS_M, 99);

    /**
     * Find the member types of a struct.
     * @tparam Ty struct
     */
    template<aggregate Ty>
    using struct_members_t = typename struct_members<Ty, struct_size_v<Ty>>::type;

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     *                                             cvref helpers.                                              *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    template<class To, class From>
    struct add_ref {
        using _lvalue = std::conditional_t<std::is_lvalue_reference_v<From>, std::add_lvalue_reference_t<To>, To>;
        using _rvalue = std::conditional_t<std::is_rvalue_reference_v<From>, std::add_rvalue_reference_t<_lvalue>, _lvalue>;
        using type = typename _rvalue;
    };

    template<class From, class ...Tys>
    struct add_ref<info<Tys...>, From> {
        using type = info<typename add_ref<Tys, From>::type...>;
    };

    template<class To, class From>
    struct copy_ref {
        using type = add_ref<std::decay_t<To>, From>::type;
    };

    template<class From, class ...Tys>
    struct copy_ref<info<Tys...>, From> {
        using type = info<typename copy_ref<Tys, From>::type...>;
    };

    template<class To, class From = const int>
    struct add_const {
        using _unrefFrom = std::remove_reference_t<From>;
        using _unrefTo = std::remove_reference_t<To>;
        using _const = std::conditional_t<std::is_const_v<_unrefFrom>, typename add_ref<std::add_const_t<_unrefTo>, To>::type, To>;
        using type = _const;
    };

    template<class From, class ...Tys>
    struct add_const<info<Tys...>, From> {
        using type = info<typename add_const<Tys, From>::type...>;
    };

    template<class To, class From = volatile int>
    struct add_volatile {
        using _unrefFrom = std::remove_reference_t<From>;
        using _unrefTo = std::remove_reference_t<To>;
        using _volatile = std::conditional_t<std::is_volatile_v<_unrefFrom>, typename add_ref<std::add_volatile_t<To>, To>::type, To>;
        using type = _volatile;
    };

    template<class From, class ...Tys>
    struct add_volatile<info<Tys...>, From> {
        using type = info<typename add_volatile<Tys, From>::type...>;
    };

    template<class To, class From = const volatile int>
    struct add_cv {
        using type = typename add_volatile<typename add_const<To, From>::type, From>::type;
    };

    template<class From, class ...Tys>
    struct add_cv<info<Tys...>, From> {
        using type = info<typename add_cv<Tys, From>::type...>;
    };

    template<class To, class From>
    struct copy_const {
        using type = add_ref<typename add_const<std::remove_const_t<std::remove_reference_t<To>>, From>::type, To>::type;
    };

    template<class From, class ...Tys>
    struct copy_const<info<Tys...>, From> {
        using type = info<typename copy_const<Tys, From>::type...>;
    };

    template<class To, class From>
    struct copy_volatile {
        using type = add_ref<typename add_volatile<std::remove_volatile_t<std::remove_reference_t<To>>, From>::type, To>::type;
    };

    template<class From, class ...Tys>
    struct copy_volatile<info<Tys...>, From> {
        using type = info<typename copy_volatile<Tys, From>::type...>;
    };

    template<class To, class From>
    struct copy_cv {
        using type = add_ref<typename add_cv<std::remove_cv_t<std::remove_reference_t<To>>, From>::type, To>::type;
    };

    template<class From, class ...Tys>
    struct copy_cv<info<Tys...>, From> {
        using type = info<typename copy_cv<Tys, From>::type...>;
    };

    template<class To, class From>
    struct add_cvref {
        using _cv = typename add_cv<To, From>::type;
        using _lvalue = std::conditional_t<std::is_lvalue_reference_v<From>, std::add_lvalue_reference_t<_cv>, _cv>;
        using _rvalue = std::conditional_t<std::is_rvalue_reference_v<From>, std::add_rvalue_reference_t<_lvalue>, _lvalue>;
        using type = typename _rvalue;
    };

    template<class From, class ...Tys>
    struct add_cvref<info<Tys...>, From> {
        using type = info<typename add_cvref<Tys, From>::type...>;
    };

    template<class To, class From>
    struct copy_cvref {
        using type = add_cvref<std::decay_t<To>, From>::type;
    };

    template<class From, class ...Tys>
    struct copy_cvref<info<Tys...>, From> {
        using type = info<typename copy_cvref<Tys, From>::type...>;
    };

    template<class To, class From>
    using copy_const_t = typename copy_const<To, From>::type;
    template<class To, class From>
    using copy_volatile_t = typename copy_volatile<To, From>::type;
    template<class To, class From>
    using copy_cv_t = typename copy_cv<To, From>::type;
    template<class To, class From>
    using copy_ref_t = typename copy_ref<To, From>::type;
    template<class To, class From>
    using copy_cvref_t = typename copy_cvref<To, From>::type;
    template<class To, class From = const int>
    using add_const_t = typename add_const<To, From>::type;
    template<class To, class From = volatile int>
    using add_volatile_t = typename add_volatile<To, From>::type;
    template<class To, class From = const volatile int>
    using add_cv_t = typename add_cv<To, From>::type;
    template<class To, class From>
    using add_ref_t = typename add_ref<To, From>::type;
    template<class To, class From>
    using add_cvref_t = typename add_cvref<To, From>::type;

    template<class To>
    struct remove_const {
        using type = typename copy_const<To, int>::type;
    };

    template<class To>
    using remove_const_t = typename remove_const<To>::type;

    template<class To>
    struct remove_volatile {
        using type = typename copy_volatile<To, int>::type;
    };

    template<class To>
    using remove_volatile_t = typename remove_volatile<To>::type;

    template<class To>
    struct remove_cv {
        using type = typename copy_cv<To, int>::type;
    };

    template<class To>
    using remove_cv_t = typename remove_cv<To>::type;

    template<class Ty>
    struct decay {
        using type = std::decay_t<Ty>;
    };

    template<class ...Tys>
    struct decay<info<Tys...>> {
        using type = info<typename decay<Tys>::type...>;
    };

    template<class Ty>
    using decay_t = typename decay<Ty>::type;

    template<class Ty>
    struct remove_reference {
        using type = std::remove_reference_t<Ty>;
    };

    template<class ...Tys>
    struct remove_reference<info<Tys...>> {
        using type = info<typename remove_reference<Tys>::type...>;
    };

    template<class Ty>
    using remove_reference_t = typename remove_reference<Ty>::type;

    template<class Ty>
    struct remove_cvref {
        using type = std::remove_cvref_t<Ty>;
    };

    template<class ...Tys>
    struct remove_cvref<info<Tys...>> {
        using type = info<typename remove_cvref<Tys>::type...>;
    };

    template<class Ty>
    using remove_cvref_t = typename remove_cvref<Ty>::type;

    template<class Ty>
    struct add_lvalue_reference {
        using type = std::add_lvalue_reference_t<Ty>;
    };

    template<class ...Tys>
    struct add_lvalue_reference<info<Tys...>> {
        using type = info<typename add_lvalue_reference<Tys>::type...>;
    };

    template<class Ty>
    using add_lvalue_reference_t = typename add_lvalue_reference<Ty>::type;

    template<class Ty>
    struct add_rvalue_reference {
        using type = std::add_rvalue_reference_t<Ty>;
    };

    template<class ...Tys>
    struct add_rvalue_reference<info<Tys...>> {
        using type = info<typename add_rvalue_reference<Tys>::type...>;
    };

    template<class Ty>
    using add_rvalue_reference_t = typename add_rvalue_reference<Ty>::type;

    template<class Ty>
    struct remove_pointer {
        using type = std::remove_pointer_t<Ty>;
    };

    template<class ...Tys>
    struct remove_pointer<info<Tys...>> {
        using type = info<typename remove_pointer<Tys>::type...>;
    };

    template<class Ty>
    using remove_pointer_t = typename remove_pointer<Ty>::type;

    template<class Ty>
    struct add_pointer {
        using type = std::add_pointer_t<Ty>;
    };

    template<class ...Tys>
    struct add_pointer<info<Tys...>> {
        using type = info<typename add_pointer<Tys>::type...>;
    };

    template<class Ty>
    using add_pointer_t = typename add_pointer<Ty>::type;

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     *                                           type info object.                                             *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    struct _s_none {};
    struct _s_fun {};
    struct _s_fun_ptr {};
    struct _s_integral {};
    struct _s_floating {};
    struct _s_enum {};
    struct _s_array {};
    struct _s_aggregate {};
    struct _s_memfun {};
    struct _s_value {};
    struct _s_templated {};

    template<class ...Tys>
    struct specialized_info {
        using _selected_specialization = _s_none;
    };

    /**
     * Specialization for callable types, to also
     * contain function info.
     */
    template<class ...Tys>
    struct specialized_info_fun0;

    template<class ...Tys> requires (sizeof...(Tys) > 1)
        struct specialized_info_fun0<Tys...> {
        using arguments = info<typename function_info<Tys>::arguments...>;
    };

    template<class Ty>
    struct specialized_info_fun0<Ty> {
        using arguments = typename function_info<Ty>::arguments;
    };

    template<class ...Tys>
    struct specialized_info_fun1 : specialized_info_fun0<Tys...> {
        using pointer = info<typename function_info<Tys>::pointer...>;
        using signature = info<typename function_info<Tys>::signature...>;
        using result = info<typename function_info<Tys>::result...>;

        using is_noexcept = info<value_t<function_info<Tys>::is_noexcept>...>;

        using add_noexcept = info<typename function_info<Tys>::add_noexcept...>;
        using remove_noexcept = info<typename function_info<Tys>::remove_noexcept...>;
    };

    template<class ...Tys>
    struct specialized_info_fun2 : specialized_info_fun1<Tys...> {
        using is_fun_const = info<value_t<function_info<Tys>::is_fun_const>...>;
        using is_fun_mutable = info<value_t<function_info<Tys>::is_fun_mutable>...>;
        using is_fun_volatile = info<value_t<function_info<Tys>::is_fun_volatile>...>;
        using is_fun_lvalue_reference = info<value_t<function_info<Tys>::is_fun_lvalue_reference>...>;
        using is_fun_rvalue_reference = info<value_t<function_info<Tys>::is_fun_rvalue_reference>...>;
        using is_fun_reference = info<value_t<function_info<Tys>::is_fun_reference>...>;

        using add_fun_const = info<typename function_info<Tys>::add_fun_const...>;
        using remove_fun_const = info<typename function_info<Tys>::remove_fun_const...>;
        using add_fun_volatile = info<typename function_info<Tys>::add_fun_volatile...>;
        using remove_fun_volatile = info<typename function_info<Tys>::remove_fun_volatile...>;
        using add_fun_cv = info<typename function_info<Tys>::add_fun_cv...>;
        using remove_fun_cv = info<typename function_info<Tys>::remove_fun_cv...>;
        using add_fun_lvalue_reference = info<typename function_info<Tys>::add_fun_lvalue_reference...>;
        using add_fun_rvalue_reference = info<typename function_info<Tys>::add_fun_rvalue_reference...>;
        using remove_fun_reference = info<typename function_info<Tys>::remove_fun_reference...>;
        using remove_fun_cvref = info<typename function_info<Tys>::remove_fun_cvref...>;
    };

    template<class ...Tys>
        requires ((callable_type<Tys> && ...) && (pointer<Tys> || ...))
    struct specialized_info<Tys...> : specialized_info_fun1<Tys...> {
        using _selected_specialization = _s_fun_ptr;
    };

    template<class ...Tys>
        requires ((callable_type<Tys> && ...) && (!pointer<Tys> && ...))
    struct specialized_info<Tys...> : specialized_info_fun2<Tys...> {
        using _selected_specialization = _s_fun;
    };

    /**
     * Specialization for integral types, to
     * add the make_signed/make_unsigned type traits.
     * Also inherits from std::numeric_limits<Ty>
     */
    template<class ...Tys>
        requires ((integral<Tys> && !boolean<Tys>) && ...)
    struct specialized_info<Tys...> {
        using _selected_specialization = _s_integral;

        using make_signed = info<std::make_signed<Tys>...>;
        using make_unsigned = info<std::make_unsigned<Tys>...>;
    };

    /**
     * Specialization for floating pointer, inherits
     * from std::numeric_limits<Ty>
     */
    template<class Ty, class ...Tys>
        requires (floating_point<Ty> && (same_as<Ty, Tys> && ...))
    struct specialized_info<Ty, Tys...> : std::numeric_limits<Ty> {
        using _selected_specialization = _s_floating;
    };

    /**
     * Specialization for enum types, contains underlying
     * type, and enum names.
     */
    template<class ...Tys>
        requires (enum_type<Tys> && ...)
    struct specialized_info<Tys...> {
        using _selected_specialization = _s_enum;

        using underlying = info<std::underlying_type_t<Tys>...>;

        template<auto Value>
        using name = info<value_t<enum_name<Tys, Value>>...>;

        template<auto Value>
        using defined = info<value_t<enum_name<Tys, Value>.size() != 0>...>;
    };

    /**
     * Specialization for member object pointer,
     * contains the 'object' and 'value_type'
     */
    template<class ...Tys, class ...Objs>
        requires member_object_pointer<Tys Objs::*...>
    struct specialized_info<Tys Objs::*...> {
        using _selected_specialization = _s_memfun;

        using object = info<Objs...>;
        using value_type = info<Tys...>;
    };

    /**
     * Specialization for Arrays,
     * contains rank/extent traits.
     */
    template<class ...Tys>
        requires (array<Tys> && ...)
    struct specialized_info<Tys...> {
        using _selected_specialization = _s_array;

        using rank = info<value_t<std::rank_v<Tys>>...>;

        template<std::size_t Dim = 0>
        using extent = info<value_t<std::extent_v<Tys, Dim>>...>;

        using remove_extent = info<std::remove_extent_t<Tys>...>;
        using remove_all_extents = info<std::remove_all_extents_t<Tys>...>;
    };

    /**
     * Specialization for structs, contains information on
     * members, like offset, types, and if 'register' macro was
     * used, also member names and pointers. Also contains
     * non-constexpr member pointers, made using std::bit_cast
     * and the calculated offset of each member.
     */
    template<class ...Tys>
        requires ((aggregate<Tys> && ...) && (!array<Tys> && ...))
    struct specialized_info<Tys...> {
        using _selected_specialization = _s_aggregate;

        using members = info<struct_members_t<Tys>...>;
    };

    template<auto V>
    struct specialized_info<value_t<V>> : info<decltype(V)> {
        using _selected_specialization = _s_value;

        constexpr static auto value = V;
    };

    template<auto ...Vs> requires (sizeof...(Vs) > 1)
        struct specialized_info<value_t<Vs>...> : info<decltype(Vs)...> {
        using _selected_specialization = _s_value;

        template<std::size_t I> constexpr static auto value = element_t<I, value_t<Vs>...>::value;
    };
    
    template<template<class...> class ...Tys>
    struct specialized_info<templated_t<Tys>...> {
        using _selected_specialization = _s_templated;

        template<class ...Args>
        using instantiate = info<instantiate_t<Tys, Args...>...>;
    };

    template<class ...T>
    struct info_base;

    /**
     * Base for a pack of types.
     * @tparam ...Tys types
     */
    template<class ...Tys> requires (sizeof...(Tys) > 1)
        struct info_base<Tys...> : specialized_info<Tys...> {
        template<std::size_t I>
        using type = element_t<I, Tys...>;

        using alignment = info<value_t<alignof_v<Tys>>...>;
    };

    template<class Ty>
    struct info_base<Ty> : specialized_info<Ty> {
        using type = Ty;

        using alignment = info<value_t<alignof_v<Ty>>>;
    };

    template<> struct info_base<> {};

    template<class ...Tys>
    struct info : info_base<Tys...> {
        constexpr static auto size = sizeof...(Tys);
        constexpr static auto unique_size = unique_count_v<Tys...>;

        using bytes = info<value_t<sizeof_v<Tys>>...>;

        template<class T> constexpr static auto index = index_v<T, Tys...>;
        template<class T> constexpr static auto last_index = last_index_v<T, Tys...>;
        template<class T> constexpr static auto count = count_v<T, Tys...>;
        template<class T> constexpr static auto occurs = occurs_v<T, Tys...>;
        template<class T> constexpr static auto indices = indices_v<T, Tys...>;
        template<class T> constexpr static auto indices_except = indices_except_v<T, Tys...>;

        template<auto Filter> constexpr static auto count_filter = count_filter_v<Filter, Tys...>;
        template<auto Filter> constexpr static auto indices_filter = indices_filter_v<Filter, Tys...>;

        template<class Ty> struct _element_is_info { using type = info<Ty>; };
        template<class ...Tys> struct _element_is_info<info<Tys...>> { using type = info<Tys...>; };

        template<std::size_t I> using element = _element_is_info<element_t<I, Tys...>>::type;
        template<std::size_t I> using take = take_t<I, info>;
        template<std::size_t I> using last = drop_t<size - I, info>;
        template<std::size_t I> using drop = drop_t<I, info>;
        template<std::size_t I> using drop_last = drop_last_t<I, info>;
        template<std::size_t I> using erase = erase_t<I, info>;

        template<std::size_t I, class T> using insert = insert_t<I, T, info>;
        template<std::size_t I, class T> using swap = erase<I>::template insert<I, T>;

        template<std::size_t A, std::size_t B> using sub = sub_t<A, B, info>;

        template<auto Array> using remove_indices = remove_indices_t<Array, info>;
        template<auto Array> using keep_indices = keep_indices_t<Array, info>;

        template<class T> using remove = remove_t<T, info>;
        template<class T> using keep = keep_t<T, info>;

        template<class T> using append = append_t<T, info>;
        template<class T> using prepend = prepend_t<T, info>;

        template<class A, class B> using replace = replace_t<A, B, info>;

        using unique = unique_t<info>;
        using reverse = reverse_t<info>;
        using uninstantiate = info<uninstantiate_t<Tys>...>;
        using tparams = tparams_t<Tys...>;

        template<class ...Args> using reinstantiate = info<reinstantiate_t<Tys, Args...>...>;

        template<template<class...> class T> using transform = transform_t<T, info>;
        template<template<class...> class T> using as = T<Tys...>;
        template<auto Filter> using filter = filter_t<Filter, info>;
        template<auto Sorter> using sort = sort_types_t<Sorter, info>;
        constexpr static auto for_each = []<class Ty>(Ty && lambda) {
            return lambda.operator()<Tys...>();
        };

        template<auto Filter> struct when {
            template<template<class...> class T> 
            using transform = conditional_transform_t<Filter, T, info>;
        };

        using is_void = info<value_t<std::is_void_v<Tys>>...>;
        using is_null_pointer = info<value_t<std::is_null_pointer_v<Tys>>...>;
        using is_integral = info<value_t<std::is_integral_v<Tys>>...>;
        using is_floating_point = info<value_t<std::is_floating_point_v<Tys>>...>;
        using is_array = info<value_t<std::is_array_v<Tys>>...>;
        using is_enum = info<value_t<std::is_enum_v<Tys>>...>;
        using is_union = info<value_t<std::is_union_v<Tys>>...>;
        using is_class = info<value_t<std::is_class_v<Tys>>...>;
        using is_function = info<value_t<std::is_function_v<Tys>>...>;
        using is_pointer = info<value_t<std::is_pointer_v<Tys>>...>;
        using is_lvalue_reference = info<value_t<std::is_lvalue_reference_v<Tys>>...>;
        using is_rvalue_reference = info<value_t<std::is_rvalue_reference_v<Tys>>...>;
        using is_member_object_pointer = info<value_t<std::is_member_object_pointer_v<Tys>>...>;
        using is_member_function_pointer = info<value_t<std::is_member_function_pointer_v<Tys>>...>;
        using is_fundamental = info<value_t<std::is_fundamental_v<Tys>>...>;
        using is_arithmetic = info<value_t<std::is_arithmetic_v<Tys>>...>;
        using is_scalar = info<value_t<std::is_scalar_v<Tys>>...>;
        using is_object = info<value_t<std::is_object_v<Tys>>...>;
        using is_compound = info<value_t<std::is_compound_v<Tys>>...>;
        using is_reference = info<value_t<std::is_reference_v<Tys>>...>;
        using is_member_pointer = info<value_t<std::is_member_pointer_v<Tys>>...>;
        using is_const = info<value_t<std::is_const_v<Tys>>...>;
        using is_volatile = info<value_t<std::is_volatile_v<Tys>>...>;
        using is_trivial = info<value_t<std::is_trivial_v<Tys>>...>;
        using is_trivially_copyable = info<value_t<std::is_trivially_copyable_v<Tys>>...>;
        using is_standard_layout = info<value_t<std::is_standard_layout_v<Tys>>...>;
        using is_empty = info<value_t<std::is_empty_v<Tys>>...>;
        using is_polymorphic = info<value_t<std::is_polymorphic_v<Tys>>...>;
        using is_abstract = info<value_t<std::is_abstract_v<Tys>>...>;
        using is_final = info<value_t<std::is_final_v<Tys>>...>;
        using is_aggregate = info<value_t<std::is_aggregate_v<Tys>>...>;
        using is_signed_integral = info<value_t<std::is_signed_v<Tys>>...>;
        using is_unsigned_integral = info<value_t<std::is_unsigned_v<Tys>>...>;
        using is_bounded_array = info<value_t<std::is_bounded_array_v<Tys>>...>;
        using is_unbounded_array = info<value_t<std::is_unbounded_array_v<Tys>>...>;
        using is_default_constructible = info<value_t<std::is_default_constructible_v<Tys>>...>;
        using is_trivially_default_constructible = info<value_t<std::is_trivially_default_constructible_v<Tys>>...>;
        using is_nothrow_default_constructible = info<value_t<std::is_nothrow_default_constructible_v<Tys>>...>;
        using is_copy_constructible = info<value_t<std::is_copy_constructible_v<Tys>>...>;
        using is_trivially_copy_constructible = info<value_t<std::is_trivially_copy_constructible_v<Tys>>...>;
        using is_nothrow_copy_constructible = info<value_t<std::is_nothrow_copy_constructible_v<Tys>>...>;
        using is_move_constructible = info<value_t<std::is_move_constructible_v<Tys>>...>;
        using is_trivially_move_constructible = info<value_t<std::is_trivially_move_constructible_v<Tys>>...>;
        using is_nothrow_move_constructible = info<value_t<std::is_nothrow_move_constructible_v<Tys>>...>;
        using is_copy_assignable = info<value_t<std::is_copy_assignable_v<Tys>>...>;
        using is_trivially_copy_assignable = info<value_t<std::is_trivially_copy_assignable_v<Tys>>...>;
        using is_nothrow_copy_assignable = info<value_t<std::is_nothrow_copy_assignable_v<Tys>>...>;
        using is_move_assignable = info<value_t<std::is_move_assignable_v<Tys>>...>;
        using is_trivially_move_assignable = info<value_t<std::is_trivially_move_assignable_v<Tys>>...>;
        using is_nothrow_move_assignable = info<value_t<std::is_nothrow_move_assignable_v<Tys>>...>;
        using is_destructible = info<value_t<std::is_destructible_v<Tys>>...>;
        using is_trivially_destructible = info<value_t<std::is_trivially_destructible_v<Tys>>...>;
        using is_nothrow_destructible = info<value_t<std::is_nothrow_destructible_v<Tys>>...>;
        using is_swappable = info<value_t<std::is_swappable_v<Tys>>...>;
        using is_nothrow_swappable = info<value_t<std::is_nothrow_swappable_v<Tys>>...>;

        using has_unique_object_representations = info<value_t<std::has_unique_object_representations_v<Tys>>...>;
        using has_virtual_destructors = info<value_t<std::has_virtual_destructor_v<Tys>>...>;

        template<class Other> using is_assignable = info<value_t<std::is_assignable_v<Tys, Other>>...>;
        template<class Other> using is_trivially_assignable = info<value_t<std::is_trivially_assignable_v<Tys, Other>>...>;
        template<class Other> using is_nothrow_assignable = info<value_t<std::is_nothrow_assignable_v<Tys, Other>>...>;
        template<class Other> using is_swappable_with = info<value_t<std::is_swappable_with_v<Tys, Other>>...>;
        template<class Other> using is_nothrow_swappable_with = info<value_t<std::is_nothrow_swappable_with_v<Tys, Other>>...>;

        template<class Other> using is_same = info<value_t<std::is_same_v<Tys, Other>>...>;
        template<class Other> using is_base_of = info<value_t<std::is_base_of_v<Tys, Other>>...>;
        template<class Other> using is_convertible_to = info<value_t<std::is_convertible_v<Tys, Other>>...>;
        template<class Other> using is_nothrow_convertible_to = info<value_t<std::is_nothrow_convertible_v<Tys, Other>>...>;

        template<class ...Args> using is_constructible = info<value_t<std::is_constructible_v<Tys, Args...>>...>;
        template<class ...Args> using is_trivially_constructible = info<value_t<std::is_trivially_constructible_v<Tys, Args...>>...>;
        template<class ...Args> using is_nothrow_constructible = info<value_t<std::is_nothrow_constructible_v<Tys, Args...>>...>;
        template<class ...Args> using is_invocable = info<value_t<std::is_invocable_v<Tys, Args...>>...>;
        template<class ...Args> using is_nothrow_invocable = info<value_t<std::is_nothrow_invocable_v<Tys, Args...>>...>;

        template<class Ty> using can_construct = info<value_t<std::is_constructible_v<Ty, Tys...>>>;
        template<class Ty> using can_trivially_construct = info<value_t<std::is_trivially_constructible_v<Ty, Tys...>>>;
        template<class Ty> using can_nothrow_construct = info<value_t<std::is_nothrow_constructible_v<Ty, Tys...>>>;
        template<class Ty> using can_invoke = info<value_t<std::is_invocable_v<Ty, Tys...>>>;
        template<class Ty> using can_nothrow_invoke = info<value_t<std::is_nothrow_invocable_v<Ty, Tys...>>>;

        // Copy modifiers from From to ...Tys
        template<class From>
        using copy_const_from = info<kaixo::copy_const_t<Tys, From>...>;
        template<class From>
        using copy_volatile_from = info<kaixo::copy_volatile_t<Tys, From>...>;
        template<class From>
        using copy_cv_from = info<kaixo::copy_cv_t<Tys, From>...>;
        template<class From>
        using copy_ref_from = info<kaixo::copy_ref_t<Tys, From>...>;
        template<class From>
        using copy_cvref_from = info<kaixo::copy_cvref_t<Tys, From>...>;

        // Add modifiers of From to ...Tys
        template<class From>
        using add_const_from = info<kaixo::add_const_t<Tys, From>...>;
        template<class From>
        using add_volatile_from = info<kaixo::add_volatile_t<Tys, From>...>;
        template<class From>
        using add_cv_from = info<kaixo::add_cv_t<Tys, From>...>;
        template<class From>
        using add_ref_from = info<kaixo::add_ref_t<Tys, From>...>;
        template<class From>
        using add_cvref_from = info<kaixo::add_cvref_t<Tys, From>...>;

        // Copy modifiers from ...Tys to To
        template<class To>
        using copy_const_to = info<kaixo::copy_const_t<To, Tys>...>;
        template<class To>
        using copy_volatile_to = info<kaixo::copy_volatile_t<To, Tys>...>;
        template<class To>
        using copy_cv_to = info<kaixo::copy_cv_t<To, Tys>...>;
        template<class To>
        using copy_ref_to = info<kaixo::copy_ref_t<To, Tys>...>;
        template<class To>
        using copy_cvref_to = info<kaixo::copy_cvref_t<To, Tys>...>;

        // Add modifiers of ...Tys to To
        template<class To>
        using add_const_to = info<kaixo::add_const_t<To, Tys>...>;
        template<class To>
        using add_volatile_to = info<kaixo::add_volatile_t<To, Tys>...>;
        template<class To>
        using add_cv_to = info<kaixo::add_cv_t<To, Tys>...>;
        template<class To>
        using add_ref_to = info<kaixo::add_ref_t<To, Tys>...>;
        template<class To>
        using add_cvref_to = info<kaixo::add_cvref_t<To, Tys>...>;

        using decay = info<kaixo::decay_t<Tys>...>;
        using remove_cv = info<kaixo::remove_cv_t<Tys>...>;
        using remove_const = info<kaixo::remove_const_t<Tys>...>;
        using remove_volatile = info<kaixo::remove_volatile_t<Tys>...>;
        using add_cv = info<kaixo::add_cv_t<Tys>...>;
        using add_const = info<kaixo::add_const_t<Tys>...>;
        using add_volatile = info<kaixo::add_volatile_t<Tys>...>;
        using remove_reference = info<kaixo::remove_reference_t<Tys>...>;
        using remove_cvref = info<kaixo::remove_cvref_t<Tys>...>;
        using add_lvalue_reference = info<kaixo::add_lvalue_reference_t<Tys>...>;
        using add_rvalue_reference = info<kaixo::add_rvalue_reference_t<Tys>...>;
        using remove_pointer = info<kaixo::remove_pointer_t<Tys>...>;
        using add_pointer = info<kaixo::add_pointer_t<Tys>...>;

        template<class Ty> using to_function_args = info<Ty(Tys...)>;
        template<class Ty> using to_member_pointer = info<kaixo::remove_reference_t<Tys> Ty::* ...>;
    };

    /**
     * Info object of value_t's.
     * @tparam ...Vs non-type template parameters
     */
    template<auto ...Vs>
    using info_v = info<value_t<Vs>...>;
    
    /**
     * Info object of templated types.
     * @tparam ...Tys templated types
     */
    template<template<class...> class ...Tys>
    using info_t = info<templated_t<Tys>...>;

    /**
     * Template parameters of Ty to info.
     * @tparam Ty templated type
     */
    template<class Ty>
    using as_info = move_tparams_t<decay_t<Ty>, info>;

    template<std::size_t ...Is>
    constexpr std::array<std::size_t, sizeof...(Is)> as_array{ Is... };

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     *                                             tuple helpers.                                              *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    template<class Ty, class Tuple>
    concept is_tuple_modifier = std::invocable<decay_t<Ty>, Tuple>;

    template<std::size_t I> struct get_v_impl {
        template<class ...Tys> 
        constexpr decltype(auto) operator()(const std::tuple<Tys...>& tuple) const {
            using forward_types = info<Tys...>::
                template iff<not is_reference>::template transform<add_lvalue_reference_t>::
                template iff<is_rvalue_reference>::template transform<remove_reference_t>;
            using type = std::conditional_t<reference<typename info<Tys...>::template element<I>::type>,
                typename forward_types::template element<I>::type,
                const typename info<Tys...>::template element<I>::type&>;
            return std::forward<type>(std::get<I>(tuple));
        }
    };

    template<std::size_t I> constexpr auto get_v = get_v_impl<I>{};

    template<class T, is_tuple_modifier<T> Ty>
    constexpr decltype(auto) operator|(T&& tuple, Ty&& val) {
        return std::forward<Ty>(val)(std::forward<T>(tuple));
    }

    template<std::size_t I> struct take_v_impl {
        template<class ...Tys, class Type
            = typename info<Tys...>::template take<I>::template as<std::tuple>>
        constexpr auto operator()(const std::tuple<Tys...>& tuple) const -> Type {
            return sequence<I>([&]<std::size_t ...Is>{
                return Type{ (get_v<Is>(tuple))... };
            });
        }
    };
    
    template<std::size_t I> struct drop_v_impl {
        template<class ...Tys, class Type
            = typename info<Tys...>::template drop<I>::template as<std::tuple>>
        constexpr auto operator()(const std::tuple<Tys...>& tuple) const -> Type {
            return sequence<I, info<Tys...>::size>([&]<std::size_t ...Is>{
                return Type{ (get_v<Is>(tuple))... };
            });
        }
    };

    template<std::size_t I> constexpr auto take_v = take_v_impl<I>{};
    template<std::size_t I> constexpr auto drop_v = drop_v_impl<I>{};

    template<std::size_t I> struct last_v_impl {
        template<class ...Tys, class Type
            = typename info<Tys...>::template drop<info<Tys...>::size - I>::template as<std::tuple>>
        constexpr auto operator()(const std::tuple<Tys...>& tuple) const -> Type {
            return drop_v<info<Tys...>::size - I>(tuple);
        }
    };

    template<std::size_t I> struct drop_last_v_impl {
        template<class ...Tys, class Type
            = typename info<Tys...>::template drop_last<I>::template as<std::tuple>>
        constexpr auto operator()(const std::tuple<Tys...>& tuple) const -> Type {
            return take_v<info<Tys...>::size - I>(tuple);
        }
    };

    template<std::size_t I> struct erase_v_impl {
        template<class ...Tys, class Type
            = typename info<Tys...>::template erase<I>::template as<std::tuple>>
        constexpr auto operator()(const std::tuple<Tys...>& tuple) const -> Type {
            return iterate<generate_indices_v<0, info<Tys...>::size, I>>([&]<std::size_t ...Is>{
                return Type{ (get_v<Is>(tuple))... };
            });
        }
    };

    template<std::size_t I> struct insert_v_impl {
        template<class ...Args> struct result {
            std::tuple<Args&&...> _data{};

            template<class ...Tys, class Type
                = typename info<Tys...>::template insert<I, info<decay_t<Args>...>>::template as<std::tuple>>
            constexpr Type operator()(const std::tuple<Tys...>& tuple) const {
                return[&]<std::size_t ...Is, std::size_t ...Ns, std::size_t ...Qs>
                    (std::index_sequence<Is...>, std::index_sequence<Ns...>, std::index_sequence<Qs...>) {
                    return Type{ (get_v<Is>(tuple))..., (get_v<Qs>(_data))..., (get_v<I + Ns>(tuple))...};
                }(std::make_index_sequence<I>{}, std::make_index_sequence<info<Tys...>::size - I>{},
                    std::index_sequence_for<Args...>{});
            }
        };

        template<class ...Tys>
        constexpr result<Tys...> operator()(Tys&&...args) const {
            return result<Tys...>{._data{ std::forward<Tys>(args)... } };
        }
    };

    template<std::size_t I> constexpr auto erase_v = erase_v_impl<I>{};
    template<std::size_t I> constexpr auto insert_v = insert_v_impl<I>{};
    
    template<std::size_t I> struct swap_v_impl {
        template<class Ty> struct result {
            Ty&& _data;
            template<class ...Tys, class Type
                = typename info<Tys...>::template swap<I, decay_t<Ty>>::template as<std::tuple>>
            constexpr Type operator()(const std::tuple<Tys...>& tuple) const {
                return erase_v<I>(tuple) | insert_v<I>(std::forward<Ty>(_data));
            }
        };

        template<class Ty>
        constexpr result<Ty> operator()(Ty&& arg) const {
            return result<Ty>{._data = std::forward<Ty>(arg) };
        }
    };

    template<std::size_t A, std::size_t B> struct sub_v_impl {
        template<class ...Tys, class Type
            = typename info<Tys...>::template sub<A, B>::template as<std::tuple>>
        constexpr auto operator()(const std::tuple<Tys...>& tuple) const -> Type {
            return iterate<generate_indices_v<A, B>>([&]<std::size_t ...Is>{
                return Type{ (get_v<Is>(tuple))... };
            });
        }
    };

    template<class ...Args> struct remove_v_impl {
        template<class ...Tys, auto Indices = info<Tys...>::decay::template indices_except<info<Args...>>,
            class Type = typename keep_indices_t<Indices, info<Tys...>>::template as<std::tuple>>
        constexpr auto operator()(const std::tuple<Tys...>& tuple) const -> Type {
            return iterate<Indices>([&]<std::size_t ...Is>{
                return Type{ (get_v<Is>(tuple))... };
            });
        }
    };

    //template<std::size_t ...Is> struct remove_indices_v_impl {
    //    template<class T,
    //        class Type = typename remove_indices_t<as_array<Is...>, as_info<T>>::template as<std::tuple>>
    //    constexpr auto operator()(T&& tuple) const -> Type {
    //        return iterate<as_array<Is...>>([&]<std::size_t ...Ns>{
    //            return Type{ std::get<Ns>(tuple)... };
    //        });
    //    }
    //};

    ///**
    // * Only keep at Indices.
    // * @tparam R type to remove, or info<Types...> for multiple
    // */
    //template<std::size_t ...Is> constexpr auto remove_indices_v = remove_indices_v_impl<Is...>{};

    template<class ...Args> struct remove_raw_v_impl {
        template<class ...Tys, auto Indices = info<Tys...>::decay::template indices_except<info<Args...>>,
            class Type = typename keep_indices_t<Indices, info<Tys...>>::template as<std::tuple>>
            constexpr auto operator()(const std::tuple<Tys...>& tuple) const -> Type {
            return iterate<Indices>([&]<std::size_t ...Is>{
                return Type{ (get_v<Is>(tuple))... };
            });
        }
    };

    template<class ...Args> struct keep_v_impl {
        template<class ...Tys, auto Indices = info<Tys...>::decay::template indices<info<Args...>>,
            class Type = typename keep_indices_t<Indices, info<Tys...>>::template as<std::tuple>>
        constexpr auto operator()(const std::tuple<Tys...>& tuple) const -> Type {
            return iterate<Indices>([&]<std::size_t ...Is>{
                return Type{ (get_v<Is>(tuple))... };
            });
        }
    };

    template<std::size_t ...Is> struct keep_indices_v_impl {
        template<class ...Tys, class Type = std::tuple<typename info<Tys...>
            ::template element<Is>::type...>>
        constexpr auto operator()(const std::tuple<Tys...>& tuple) const -> Type {
            return Type{ (get_v<Is>(tuple))... };
        }
    };

    template<class ...Args> struct keep_raw_v_impl {
        template<class ...Tys, auto Indices = info<Tys...>::template indices<info<Args...>>,
            class Type = typename keep_indices_t<Indices, info<Tys...>>::template as<std::tuple>>
        constexpr auto operator()(const std::tuple<Tys...>& tuple) const -> Type {
            return iterate<Indices>([&]<std::size_t ...Is>{
                return Type{ (get_v<Is>(tuple))... };
            });
        }
    };

    struct append_v_impl {
        template<class ...Args> struct result {
            std::tuple<Args&&...> _data{};

            template<class ...Tys, class Type = typename info<Tys...>
                ::template append<info<decay_t<Args>...>>::template as<std::tuple>>
            constexpr Type operator()(const std::tuple<Tys...>& tuple) const {
                return[&]<std::size_t ...Is, std::size_t ...Ns>
                    (std::index_sequence<Is...>, std::index_sequence<Ns...>) {
                    return Type{ (get_v<Is>(tuple))..., (get_v<Ns>(_data))... };
                }(std::make_index_sequence<info<Tys...>::size>{},
                    std::index_sequence_for<Args...>{});
            }
        };

        template<class ...Tys>
        constexpr result<Tys...> operator()(Tys&&...args) const {
            return result<Tys...>{._data{ std::forward<Tys>(args)... } };
        }
    };

    struct prepend_v_impl {
        template<class ...Args> struct result {
            std::tuple<Args&&...> _data{};

            template<class ...Tys, class Type = typename info<Tys...>
                ::template prepend<info<decay_t<Args>...>>::template as<std::tuple>>
                constexpr Type operator()(const std::tuple<Tys...>& tuple) const {
                return[&]<std::size_t ...Is, std::size_t ...Ns>
                    (std::index_sequence<Is...>, std::index_sequence<Ns...>) {
                    return Type{ (get_v<Ns>(_data))..., (get_v<Is>(tuple))... };
                }(std::make_index_sequence<info<Tys...>::size>{},
                    std::index_sequence_for<Args...>{});
            }
        };

        template<class ...Tys>
        constexpr result<Tys...> operator()(Tys&&...args) const {
            return result<Tys...>{._data{ std::forward<Tys>(args)... } };
        }
    };

    struct unique_v_impl {
        template<class ...Tys, class Type = typename keep_indices_t<
            first_indices_v<typename info<Tys...>::decay>, info<Tys...>>::template as<std::tuple>>
        constexpr auto operator()(const std::tuple<Tys...>& tuple) const -> Type {
            return iterate<first_indices_v<typename info<Tys...>::decay>>([&]<std::size_t ...Is>{
                return Type{ (get_v<Is>(tuple))... };
            });
        }
    };

    struct reverse_v_impl {
        template<class ...Tys, class Type = typename info<Tys...>::reverse::template as<std::tuple>>
        constexpr auto operator()(const std::tuple<Tys...>& tuple) const -> Type {
            return sequence<0, info<Tys...>::size>([&]<std::size_t ...Is>{
                constexpr std::size_t size = info<Tys...>::size;
                return Type{ (get_v<size - Is - 1>(tuple))... };
            });
        }
    };

    template<auto Filter> struct filter_v_impl {
        template<class ...Tys>
        constexpr auto operator()(const std::tuple<Tys...>& tuple) const {
            return iterate<info<Tys...>::decay::template indices_filter<Filter>>([&]<std::size_t ...Is>{
                return std::tuple{ (get_v<Is>(tuple))... };
            });
        }
    };

    struct call_v_impl {
        template<class Functor> struct result {
            Functor&& _functor;
            template<class ...Tys>
            constexpr decltype(auto) operator()(const std::tuple<Tys...>& tuple) const {
                return sequence<0, info<Tys...>::size>([&]<std::size_t ...Is>() -> decltype(auto) {
                    return std::forward<Functor>(_functor)((get_v<Is>(tuple))...);
                });
            }
        };

        template<class Functor>
        constexpr result<Functor> operator()(Functor&& functor) const {
            return result<Functor>{ std::forward<Functor>(functor) };
        }
    };

    template<std::size_t I> constexpr auto drop_last_v = drop_last_v_impl<I>{};
    template<std::size_t I> constexpr auto last_v = last_v_impl<I>{};
    template<std::size_t I> constexpr auto swap_v = swap_v_impl<I>{};
    template<std::size_t A, std::size_t B> constexpr auto sub_v = sub_v_impl<A, B>{};
    template<class ...Tys> constexpr auto remove_v = remove_v_impl<Tys...>{};
    template<class ...Tys> constexpr auto remove_raw_v = remove_raw_v_impl<Tys...>{};
    template<class ...Tys> constexpr auto keep_v = keep_v_impl<Tys...>{};
    template<class ...Tys> constexpr auto keep_raw_v = keep_raw_v_impl<Tys...>{};
    template<std::size_t ...Is> constexpr auto keep_indices_v = keep_indices_v_impl<Is...>{};
    constexpr auto append_v = append_v_impl{};
    constexpr auto prepend_v = prepend_v_impl{};
    constexpr auto unique_v = unique_v_impl{};
    constexpr auto reverse_v = reverse_v_impl{};
    template<auto Filter> constexpr auto filter_v = filter_v_impl<Filter>{};
    constexpr auto call_v = call_v_impl{};

    /**
     * Helper for dealing with the actual values in a template pack.
     * Used like this:
     *
     * template<class ...Tys>
     * void my_fun(Tys&&...tys) {
     *     template_pack<Tys...> vals{ tys... };
     *     vals.get<0>(); // etc...
     *
     * @tparam ...Args types
     */
    template<class ...Args>
    struct template_pack : std::tuple<Args&&...> {

        constexpr template_pack(Args&...args) : std::tuple<Args&&...>{ std::forward<Args>(args)... } {}

        /**
         * Get the template pack as a tuple.
         */
        template<class Self>
        constexpr std::tuple<Args&&...> as_tuple(this Self&& self) { return std::forward<Self>(self); }
    };

    constexpr auto zip_v = []<class ...Tys>(Tys&&... tuples) {
        constexpr std::size_t min_size = std::min({ as_info<Tys>::size... });
        using zipped = as_info<zip_t<decay_t<Tys>...>>;
        auto _one = [&]<std::size_t I>(value_t<I>)
            -> typename zipped::template type<I> {
            return { (tuples | get_v<I>)... };
        };
        return sequence<min_size>([&]<std::size_t ...Is> {
            return std::tuple{ _one(value_t<Is>{})... };
        });
    };
    
    template<std::size_t, class> struct concat_v_helper;
    template<std::size_t I, std::size_t ...Is> 
    struct concat_v_helper<I, std::index_sequence<Is...>> {
        using type = info<info<value_t<I>, value_t<Is>>...>;
    };

    constexpr auto concat_v = []<class ...Tys>(Tys&&... tuples) -> concat_t<decay_t<Tys>...> {
        template_pack<Tys...> _tuples{ tuples... };
        return sequence<sizeof...(Tys)>([&]<std::size_t ...Is>() -> concat_t<decay_t<Tys>...> {
            using types = info<as_info<Tys>...>;
            using indices = concat_t<typename concat_v_helper<Is, 
                std::make_index_sequence<types::template element<Is>::size>>::type...>;
            return indices::for_each([&]<class ...Index>{ return concat_t<decay_t<Tys>...>{ 
                (_tuples | get_v<Index::template value<0>> | get_v<Index::template value<1>>)... 
            }; });
        });
    };

    constexpr auto cartesian_v = []<class ...Tys>(Tys&&... tpls) {
        template_pack<Tys...> _tuples{ tpls... };
        auto eval_at = [&_tuples]<std::size_t I>(value_t<I>) {
            return sequence<sizeof...(Tys)>([&_tuples]<std::size_t ...Is>{
                constexpr auto indices_at_index = [](std::size_t pos) {
                    return sequence<sizeof...(Tys)>([&]<std::size_t ...Ns>() {
                        constexpr std::array sizes{ as_info<Tys>::size... };
                        std::size_t _t_pos = 0;
                        return std::array{ (_t_pos = pos % sizes[Ns], pos /= sizes[Ns], _t_pos)... };
                    });
                };                
                constexpr auto _indices = indices_at_index(I);
                using infos = info<Tys...>;
                return std::tuple<typename as_info<typename infos::
                    template element<Is>::type>::template element<_indices[Is]>::type...>{
                    (_tuples | get_v<Is> | get_v<_indices[Is]>)...
                };
            });
        };

        return sequence<(as_info<Tys>::size * ... * 1)>([&]<std::size_t ...Is>{
            return std::tuple{ eval_at(value_t<Is>{})... };
        });
    };
    

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     *                                                switch.                                                  *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#define   KAIXO_SC1(sc, a)         sc (    0ull + a) 
#define   KAIXO_SC2(sc, a)         sc (    0ull + a)         sc (      1ull + a)
#define   KAIXO_SC4(sc, a)   KAIXO_SC2(sc, 0ull + a)   KAIXO_SC2(sc,   2ull + a)
#define   KAIXO_SC8(sc, a)   KAIXO_SC4(sc, 0ull + a)   KAIXO_SC4(sc,   4ull + a)
#define  KAIXO_SC16(sc, a)   KAIXO_SC8(sc, 0ull + a)   KAIXO_SC8(sc,   8ull + a)
#define  KAIXO_SC32(sc, a)  KAIXO_SC16(sc, 0ull + a)  KAIXO_SC16(sc,  16ull + a)
#define  KAIXO_SC64(sc, a)  KAIXO_SC32(sc, 0ull + a)  KAIXO_SC32(sc,  32ull + a)
#define KAIXO_SC128(sc, a)  KAIXO_SC64(sc, 0ull + a)  KAIXO_SC64(sc,  64ull + a)
#define KAIXO_SC256(sc, a) KAIXO_SC128(sc, 0ull + a) KAIXO_SC128(sc, 128ull + a)
#define KAIXO_SC512(sc, a) KAIXO_SC256(sc, 0ull + a) KAIXO_SC256(sc, 256ull + a)

#define KAIXO_SWITCH_IMPL(TYPE, CASE) \
TYPE(  1ull,   KAIXO_SC1(CASE, 0ull)) \
TYPE(  2ull,   KAIXO_SC2(CASE, 0ull)) \
TYPE(  4ull,   KAIXO_SC4(CASE, 0ull)) \
TYPE(  8ull,   KAIXO_SC8(CASE, 0ull)) \
TYPE( 16ull,  KAIXO_SC16(CASE, 0ull)) \
TYPE( 32ull,  KAIXO_SC32(CASE, 0ull)) \
TYPE( 64ull,  KAIXO_SC64(CASE, 0ull)) \
TYPE(128ull, KAIXO_SC128(CASE, 0ull)) \
TYPE(256ull, KAIXO_SC256(CASE, 0ull)) \
TYPE(512ull, KAIXO_SC512(CASE, 0ull))

    /**
     * Cases switch, has a unique lambda for all
     * the cases.
     */
    template<std::size_t I>
    struct cases_switch_impl;

#define KAIXO_CASES_SWITCH_C(i) case transform(i):      \
if constexpr (i < sizeof...(Args)) {                    \
    if constexpr (std::invocable<                       \
        decltype(std::get<i>(cases)), decltype(index)>) \
        return std::get<i>(cases)(index);               \
    else return std::get<i>(cases)();                   \
} else break; 

#define KAIXO_CASES_SWITCH_S(n, cs)                              \
template<>                                                       \
struct cases_switch_impl<n> {                                    \
template<auto transform, class ...Args>                          \
constexpr static auto handle(Args&& ...cases) {                  \
    return [cases = std::tuple(                                  \
                    std::forward<Args>(cases)...)](auto index) { \
        switch (index) { cs }                                    \
    };                                                           \
}                                                                \
};

    KAIXO_SWITCH_IMPL(KAIXO_CASES_SWITCH_S, KAIXO_CASES_SWITCH_C)
#undef KAIXO_CASES_SWITCH_S
#undef KAIXO_CASES_SWITCH_C

    /**
     * Generate a switch statement with lambdas as cases.
     * @tparam transform transform case index to any other value
     * @param cases... functors, either invocable with case value, or nothing
     * @return generated switch
     */
    template<auto transform = unit>
    constexpr auto generate_switch = []<class ...Functors>(Functors&& ...cases) {
        constexpr auto p2 = closest_larger_power2(sizeof...(Functors));
        return cases_switch_impl<p2>::template handle<transform>(std::forward<Functors>(cases)...);
    };

    /**
     * Templated switch statement, where the
     * argument will be converted into a template parameter value.
     */
    template<std::size_t I>
    struct template_switch_impl;

#define KAIXO_TEMPLATE_SWITCH_C(i) case transform(i):  \
if constexpr (i < cases) {                             \
    return functor.operator()<transform(i)>();         \
} else break; 

#define KAIXO_TEMPLATE_SWITCH_S(n, cs)                           \
template<>                                                       \
struct template_switch_impl<n> {                                 \
template<auto cases, auto transform, class Arg>                  \
constexpr static auto handle(Arg&& functor) {                    \
    return [functor = std::forward<Arg>(functor)](auto index) {  \
        switch (index) { cs }                                    \
    };                                                           \
}                                                                \
};

    KAIXO_SWITCH_IMPL(KAIXO_TEMPLATE_SWITCH_S, KAIXO_TEMPLATE_SWITCH_C)
#undef KAIXO_TEMPLATE_SWITCH_S
#undef KAIXO_TEMPLATE_SWITCH_C

    /**
     * Generate a template switch statement, takes a single
     * functor which has a template argument value.
     * @tparam cases how many cases to generate
     * @tparam transform transform the case index
     * @return generated template switch
     */
    template<std::unsigned_integral auto cases, auto transform = unit>
    constexpr auto generate_template_switch = []<class Arg>(Arg && functor) {
        constexpr auto p2 = closest_larger_power2(cases);
        return template_switch_impl<p2>::template handle<cases, transform>(std::forward<Arg>(functor));
    };

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     *                                                 tests.                                                  *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    namespace _tests {
        using _big_info = info<int, double, float, char, long, unsigned>;
        using _big_info_d = info<int, double, float, int, double, float>;
        using _small_info = info<int, double, float>;
        static_assert(_big_info::size == 6);
        static_assert(_big_info_d::unique_size == 3);
        static_assert(_small_info::bytes::value<0> == sizeof(int));
        static_assert(_big_info_d::index<double> == 1);
        static_assert(_big_info_d::last_index<double> == 4);
        static_assert(_big_info_d::count<float> == 2);
        static_assert(_big_info_d::occurs<float>);
        static_assert(!_big_info_d::occurs<long>);
        static_assert(_big_info_d::indices<int>.size() == 2);
        static_assert(_big_info_d::indices<int>[0] == 0 && _big_info_d::indices<int>[1] == 3);
        static_assert(_big_info_d::indices_except<int>.size() == 4);
        static_assert(_big_info_d::indices_except<int>[0] == 1);
        static_assert(_big_info_d::indices_filter<is_integral>.size() == 2);
        static_assert(_big_info_d::indices_filter<is_integral>[0] == 0);
        static_assert(same_as<_big_info::element<3>::type, char>);

        static_assert(_big_info::take<_big_info::size>::size == 6);
        static_assert(_big_info::take<0>::size == 0);
        static_assert(_big_info::take<2>::size == 2);
        static_assert(same_as<_big_info::take<2>, info<int, double>>);

        static_assert(_big_info::drop<_big_info::size>::size == 0);
        static_assert(_big_info::drop<0>::size == 6);
        static_assert(_big_info::drop<4>::size == 2);
        static_assert(same_as<_big_info::drop<4>, info<long, unsigned>>);

        static_assert(_big_info::erase<0>::size == 5);
        static_assert(_big_info::erase<5>::size == 5);
        static_assert(same_as<_big_info::erase<5>::last<1>::type, long>);

        static_assert(_big_info::insert<3, int>::size == 7);
        static_assert(_big_info::insert<6, int>::size == 7);
        static_assert(same_as<_big_info::insert<6, int>::element<6>::type, int>);
        static_assert(same_as<_big_info::insert<4, int>::element<6>::type, unsigned>);

        static_assert(_big_info::swap<3, int>::size == 6);
        static_assert(_big_info::swap<5, int>::size == 6);
        static_assert(same_as<_big_info::swap<5, int>::element<5>::type, int>);
        static_assert(same_as<_big_info::swap<4, int>::element<5>::type, unsigned>);

        static_assert(_big_info::sub<2, 4>::size == 2);
        static_assert(same_as<_big_info::sub<2, 4>::element<0>, _big_info::element<2>>);
        static_assert(same_as<_big_info::sub<2, 6>::element<3>, _big_info::element<5>>);

        static_assert(_big_info_d::remove<int>::size == 4);
        static_assert(_big_info_d::keep<int>::size == 2);

        static_assert(_big_info_d::append<int>::size == 7);
        static_assert(same_as<_big_info_d::append<int>::element<6>::type, int>);
        static_assert(_big_info_d::prepend<int>::size == 7);
        static_assert(same_as<_big_info_d::prepend<int>::element<0>::type, int>);

        static_assert(_big_info_d::unique::size == 3);
        static_assert(_big_info_d::reverse::size == 6);
        static_assert(same_as<_big_info::reverse::element<0>, _big_info::element<5>>);
        static_assert(same_as<_big_info::reverse::element<1>, _big_info::element<4>>);
        static_assert(same_as<_big_info::reverse::element<2>, _big_info::element<3>>);

        static_assert(_big_info::filter<is_integral>::size == 4);
        static_assert(_big_info_d::filter<is_integral>::size == 2);
    }
}