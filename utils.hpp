#pragma once
#include <type_traits>
#include <typeinfo>
#include <concepts>
#include <cstddef>
#include <array>
#include <utility>
#include <string_view>
#include <algorithm>
#include <source_location>

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                           *
 *                       Kaixo Utils                         *
 *                                                           *
 * General Utils:                                            *
 *   general stuff.                                          *
 * RTTI Table:                                               *
 *   RTTI function table with stuff like move/copy           *
 *   assign/construct functions, erase type by converting    *
 *   to RTTI_Ftable<void>, which keeps the function pointers *
 * String Literal:                                           *
 *   template compatible wrapper for a string literal.       *
 *   acts like a proper string, with iterator, index         *
 *   operator, even types like value_type etc.               *
 * Pack Utils:                                               *
 *   defines a pack<Args...> to help deal with template      *
 *   packs adds stuff like element<I>, specialization for    *
 *   template values, filter<Lambda> to filter types using   *
 *   concepts or other things, sort<Lambda> to sort the      *
 *   types however you want.                                 *
 * Function Utils:                                           *
 *   has a function_info<Callable> that contains information *
 *   on argument types, return type. And if it's a member    *
 *   function pointer it also contains information on the    *
 *   object it belongs to, cv/ref/noexcept modifiers etc.    *
 * Type Utils:                                               *
 *   defines an info<Ty> to help deal with type_traits.      *
 *   contains specialization for callables to include        *
 *   all the function_info as well. Also has a namespace     *
 *   called type_concepts that contains all std type_traits  *
 *   as concepts.                                            *
 * Sequence Utils:                                           *
 *   helpers for sequences, like index sequence and looping  *
 *   over the types in a tuple.                              *
 * Switch Utils:                                             *
 *   macro to define a custom switch generator, faster than  *
 *   a sequence of if-statements.                            *
 * Multi Initializer List:                                   *
 *   Initializer list but for multiple types.                *
 *                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

namespace kaixo {

    inline namespace type_utils_n {
        template<class Ty> struct info_base;
        template<class Ty> struct info : info_base<Ty> {};
    }

    inline namespace pack_utils_n {
        template<class ...Args> struct pack_base;
        template<class ...Args> struct pack;
    }

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                           *
     *                      General Utils                        *
     *                                                           *
     *                   General useful stuff                    *
     *                                                           *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    inline namespace general_utils_n {

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
         * Class that's convertible to every type.
         */
        struct convertible_to_everything {
            template<class Ty> constexpr operator Ty&();
            template<class Ty> constexpr operator Ty&&();
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
            constexpr operator M&&();
            template<class M> requires ((std::same_as<std::decay_t<M>, Tys>) || ...)
            constexpr operator M&();
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
            constexpr operator M&();
            template<class M> requires (((!std::same_as<std::decay_t<M>, Tys>) && ...)
                && ((!std::is_base_of_v<std::decay_t<M>, Tys>) && ...))
            constexpr operator M&&();
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

        constexpr std::size_t npos = static_cast<std::size_t>(-1);

        /**
         * Type for a template value.
         * @tparam V template value
         */
        template<auto V>
        struct value_t { 
            constexpr static auto value = V; 
        };

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
        struct dud {};

        /**
         * Little integer type helper with signed and size
         * @tparam Size size of integral type in bits
         * @tparam Unsigned is integral type unsigned
         */
        template<std::size_t Size, bool Unsigned> struct integer_t;
        template<> struct integer_t<8, false> { using type = std::int8_t; };
        template<> struct integer_t<16, false> { using type = std::int16_t; };
        template<> struct integer_t<32, false> { using type = std::int32_t; };
        template<> struct integer_t<64, false> { using type = std::int64_t; };
        template<> struct integer_t<8, true> { using type = std::uint8_t; };
        template<> struct integer_t<16, true> { using type = std::uint16_t; };
        template<> struct integer_t<32, true> { using type = std::uint32_t; };
        template<> struct integer_t<64, true> { using type = std::uint64_t; };
        
        /**
         * Integer type
         * @tparam Size size of integral type in bits
         * @tparam Unsigned is integral type unsigned
         */
        template<std::size_t Size, bool Unsigned = false>
            requires (Size == 8 || Size == 16 || Size == 32 || Size == 64)
        using int_t = typename integer_t<Size, Unsigned>::type;
        
        /**
         * Unsigned integer type
         * @tparam Size size of integral type in bits
         */
        template<std::size_t Size>
            requires (Size == 8 || Size == 16 || Size == 32 || Size == 64)
        using uint_t = typename integer_t<Size, true>::type;

        /**
         * Basically sizeof(Ty), but special case for 
         * void and functions, as they normally give errors.
         */
        template<class Ty>
        constexpr std::size_t sizeof_v = [] {
            if constexpr (std::is_void_v<Ty>) return 0;
            else if constexpr (std::is_function_v<Ty>) return 0;
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

        template<class, template<class...> class>
        struct is_specialization : std::false_type {};
        template<template<class...> class Ref, class... Args>
        struct is_specialization<Ref<Args...>, Ref> : std::true_type {};

        /**
         * Concept, Test is specialization of Ref
         * @tparam Test type to test
         * @tparam Ref templated type
         */
        template<class Test, template<class...> class Ref>
        concept specialization = is_specialization<std::decay_t<Test>, Ref>::value;

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
        constexpr auto enum_name = enum_name_impl<Ty, static_cast<Ty>(V)>();

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
        constexpr auto value_name = value_name_impl<V>();

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
        constexpr auto function_name = function_name_impl<V>();

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
        constexpr auto type_name = type_name_impl<V>();
    }

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                           *
     *                        RTTI Table                         *
     *                                                           *
     *           Table of all the possibly useful RTTI           *
     *       functions like copy/move, Ty = void for type        *
     *                         erasure                           *
     *                                                           *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    inline namespace rtti_table_n {

        /**
         * RTTI function table for basic operations 
         * like delete, move, and copy.
         */
        template<class Ty>
        struct RTTI_Ftable {
            constexpr static void deleter_in_place(void* ptr) {
                if constexpr (std::is_destructible_v<Ty>)
                    static_cast<Ty*>(ptr)->~Ty();
            }

            constexpr static void deleter(void* ptr) {
                if constexpr (std::is_destructible_v<Ty>)
                    delete static_cast<Ty*>(ptr);
            }

            constexpr static void default_construct_in_place(void* to) {
                if constexpr (std::is_default_constructible_v<Ty>)
                    new (to) Ty{};
            }

            constexpr static void* default_construct(void* to) {
                if constexpr (std::is_default_constructible_v<Ty>)
                    return new Ty{};
                return nullptr;
            }

            constexpr static void move_construct_in_place(void* from, void* to) {
                if constexpr (std::is_move_constructible_v<Ty>)
                    new (to) Ty{ std::move(*static_cast<Ty*>(from)) };
            }

            constexpr static void* move_construct(void* from) {
                if constexpr (std::is_move_constructible_v<Ty>)
                    return new Ty{ std::move(*static_cast<Ty*>(from)) };
                return nullptr;
            }

            constexpr static void copy_construct_in_place(const void* from, void* to) {
                if constexpr (std::is_copy_constructible_v<Ty>)
                    new (to) Ty{ *static_cast<const Ty*>(from) };
            }

            constexpr static void* copy_construct(const void* from) {
                if constexpr (std::is_copy_constructible_v<Ty>)
                    return new Ty{ *static_cast<const Ty*>(from) };
                return nullptr;
            }

            constexpr static void move_assign(void* from, void* to) {
                if constexpr (std::is_move_assignable_v<Ty>)
                    *static_cast<Ty*>(to) = std::move(*static_cast<Ty*>(from));
            }

            constexpr static void copy_assign(const void* from, void* to) {
                if constexpr (std::is_copy_assignable_v<Ty>)
                    *static_cast<Ty*>(to) = *static_cast<const Ty*>(from);
            }

            constexpr operator RTTI_Ftable<void>();
        };

        template<>
        struct RTTI_Ftable<void> {
            void(*deleter_in_place)(void*);
            void(*deleter)(void*);
            void(*default_construct_in_place)(void*);
            void* (*default_construct)(void*);
            void(*move_construct_in_place)(void*, void*);
            void* (*move_construct)(void*);
            void(*copy_construct_in_place)(const void*, void*);
            void* (*copy_construct)(const void*);
            void(*move_assign)(void*, void*);
            void(*copy_assign)(const void*, void*);
        };

        template<class Ty>
        constexpr RTTI_Ftable<Ty>::operator RTTI_Ftable<void>() {
            return RTTI_Ftable<void>{
                &deleter_in_place,
                    & deleter,
                    & default_construct_in_place,
                    & default_construct,
                    & move_construct_in_place,
                    & move_construct,
                    & copy_construct_in_place,
                    & copy_construct,
                    & move_assign,
                    & copy_assign,
            };
        }
    }

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                           *
     *                      String Literal                       *
     *                                                           *
     *           Basically a string_view for constexpr           *
     *         strings, can be used as template argument         *
     *                                                           *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    inline namespace string_literal_n {

        /**
         * String literal, compatible with template parameter value. 
         * Basically acts like a string_view, although to be compatible
         * with template parameter values it has to copy the string into
         * a local array.
         * @tparam N literal length
         * @tparam CharType character type
         */
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

            using iterator = const_iterator;
            using reverse_iterator = std::reverse_iterator<iterator>;
            using const_reverse_iterator = std::reverse_iterator<const_iterator>;

            constexpr ~string_literal() = default;

            consteval string_literal(const CharType(&data)[N]) {
                std::copy_n(data, N, m_Data);
            }

            constexpr string_literal(std::string_view data) {
                std::copy_n(data.data(), N, m_Data);
                m_Data[N - 1] = '\n';
            }

            constexpr string_literal(string_literal&&) = default;
            constexpr string_literal(const string_literal&) = default;
            constexpr string_literal& operator=(string_literal&&) = default;
            constexpr string_literal& operator=(const string_literal&) = default;

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
    }

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                           *
     *                        Pack Utils                         *
     *                                                           *
     *             Bunch of helper templates to make             *
     *                dealing with template packs                *
     *                    a little bit easier                    *
     *                                                           *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    inline namespace pack_utils_n {

        namespace detail {
            // Single, non-templated type -> Ty<T>
            template<class T, template<class...> class Ty>
            struct move_types_impl { using type = Ty<T>; };
            // Convert T<Args...> to Ty<Args...>
            template<template<class...> class T,
                class ...Args, template<class...> class Ty>
            struct move_types_impl<T<Args...>, Ty> { using type = Ty<Args...>; };
        }

        // Move template types from one class to another, if first class 
        // isn't templated, it will itself be used as template argument
        template<class T, template<class...> class Ty>
        using move_types = typename detail::move_types_impl<T, Ty>::type;

        // Change type to Ty, used in fold expressions
        template<class, class Ty> using change = Ty;

        namespace detail {

            template<class ...Tys>
            struct pack_or_as_pack_impl {
                using type = pack<Tys...>;
            };

            template<class ...Tys>
            struct pack_or_as_pack_impl<pack<Tys...>> {
                using type = pack<Tys...>;
            };

            template<class ...Tys> // Either is a pack, or put in a pack
            using pack_or_as_pack = typename pack_or_as_pack_impl<Tys...>::type;

            // Indexed type
            template<std::size_t I, class Ty>
            struct indexed {
                constexpr static std::size_t index = I;
                using type = Ty;
            };

            // Indexer implementation inherits all indexed args
            template<class, class...> struct indexer_impl;
            template<std::size_t ...Is, class ...Args>
            struct indexer_impl<std::index_sequence<Is...>, Args...>
                : indexed<Is, Args>... {};

            // Create indexer for pack
            template<class ...Args>
            using indexer = indexer_impl<std::index_sequence_for<Args...>, Args...>;

            // Indexer implementation inherits all reversely indexed args
            template<class, class...> struct reverse_indexer_impl;
            template<std::size_t ...Is, class ...Args>
            struct reverse_indexer_impl<std::index_sequence<Is...>, Args...>
                : indexed<sizeof...(Args) - Is - 1, Args>... {};

            // Create reverse indexer for pack
            template<class ...Args>
            using reverse_indexer = reverse_indexer_impl<
                std::index_sequence_for<Args...>, Args...>;

            // Get type from index using the indexer and overload resolution
            template<std::size_t I, class Ty>
            consteval indexed<I, Ty> element_impl(indexed<I, Ty>) {};
            template<std::size_t I, class ...Args>
            using element = typename decltype(
                element_impl<I>(indexer<Args...>{}))::type;

            // Find index of first occurence of Ty in Args
            template<class Ty, class ...Args>
            consteval std::size_t index_impl() {
                std::size_t _index = 0; // use short-circuit to increment until
                ((std::same_as<Ty, Args> ? true : (++_index, false)) || ...); // first occurence
                // - 1 because we included the type itself
                return _index == sizeof...(Args) ? npos : _index;
            }
            template<class Ty, class ...Args>
            constexpr std::size_t index = index_impl<Ty, Args...>();

            // Find index of last occurence of Ty in Args
            template<class Ty, class ...Args>
            consteval std::size_t last_index_impl() {
                std::size_t _fromEnd = 0; // increment, but reset on match
                ((std::same_as<Ty, Args> ? _fromEnd = 0 : ++_fromEnd), ...);
                // This counted distance to end, so calculate index from that
                std::size_t _index = sizeof...(Args) - _fromEnd - 1;
                return _fromEnd == sizeof...(Args) ? npos : _index;
            }
            template<class Ty, class ...Args>
            constexpr std::size_t last_index = last_index_impl<Ty, Args...>();

            // Count the number of occurences of Ty in Args
            template<class Ty, class ...Args>
            constexpr std::size_t count = ((std::same_as<Ty, Args>) + ... + 0);

            // Check if Ty occurs in Args
            template<class Ty, class ...Args>
            constexpr bool occurs = count<Ty, Args...> > 0;

            // Find indices of all occurence of Ty in Args
            template<class Ty, class ...Args>
            consteval std::array<std::size_t,
                count<Ty, Args...>> indices_impl() {
                std::array<std::size_t, count<Ty, Args...>> _result{};
                std::size_t _index = 0;
                std::size_t _match = 0;
                ((std::same_as<Ty, Args> ?
                    _result[_match++] = _index++ : ++_index), ...);
                return _result;
            }
            template<class Ty, class ...Args>
            constexpr std::array<std::size_t, count<Ty, Args...>>
                indices = indices_impl<Ty, Args...>();

            // Find indices of all occurence of Ty in Args
            template<class ...Args, template<class...> class Ty, class ...Tys>
            consteval std::array<std::size_t,
                (count<Tys, Args...> +...)> indices_all_impl(
                    std::type_identity<Ty<Tys...>>) {
                std::array<std::size_t, (count<Tys, Args...> +...)> _result{};
                std::size_t _index = 0;
                std::size_t _match = 0;
                ((count<Args, Tys...> > 0 ? _result[_match++] = _index++ : ++_index), ...);
                return _result;
            }

            template<class Ty, class ...Args>
            constexpr auto indices_all =
                indices_all_impl<Args...>(std::type_identity<Ty>{});

            // Find indices of all types except Ty in Args
            template<class Ty, class ...Args>
            consteval std::array<std::size_t, sizeof...(Args)
                - count<Ty, Args...>> indices_except_impl() {
                std::array<std::size_t, sizeof...(Args) -
                    count<Ty, Args...>> _result{};
                std::size_t _index = 0;
                std::size_t _match = 0;
                ((std::same_as<Ty, Args>
                    ? ++_index : _result[_match++] = _index++), ...);
                return _result;
            }
            template<class Ty, class ...Args>
            constexpr std::array<std::size_t, sizeof...(Args) - count<Ty, Args...>>
                indices_except = indices_except_impl<Ty, Args...>();

            // Find indices of all types except Ty in Args
            template<class ...Args, template<class...> class Ty, class ...Tys>
            consteval std::array<std::size_t, sizeof...(Args)
                - (count<Tys, Args...> +...)> indices_except_all_impl(
                    std::type_identity<Ty<Tys...>>) {
                std::array<std::size_t, sizeof...(Args) -
                    (count<Tys, Args...> +...)> _result{};
                std::size_t _index = 0;
                std::size_t _match = 0;
                ((count<Args, Tys...> > 0 ? ++_index : _result[_match++] = _index++), ...);
                return _result;
            }
            template<class Ty, class ...Args>
            constexpr auto indices_except_all =
                indices_except_all_impl<Args...>(std::type_identity<Ty>{});

            // Reverse Args
            template<class> struct reverse_impl;
            template<template<class...> class Tuple, class ...Args>
            struct reverse_impl<Tuple<Args...>> {
                template<class> struct helper;
                template<std::size_t... Is>
                struct helper<std::index_sequence<Is...>> {
                    using type = Tuple<typename decltype(
                        element_impl<Is>(reverse_indexer<Args...>{}))::type... > ;
                };
                using type = typename helper<std::index_sequence_for<Args...>>::type;
            };
            template<class Ty>
            using reverse = typename reverse_impl<Ty>::type;

            // Count unique types in Args
            template<class ...Args>
            consteval std::size_t count_unique_impl() {
                std::size_t _index = 0;
                std::size_t _match = 0;
                ((_match += index<Args, Args...> == _index++), ...);
                return _match;
            }
            template<class ...Args>
            constexpr std::size_t unique_count = count_unique_impl<Args...>();

            // Find indices of all first occurrences of types in Args
            template<class ...Args>
            consteval std::array<std::size_t,
                unique_count<Args...>> first_indices_impl() {
                std::array<std::size_t, unique_count<Args...>> _result{};
                std::size_t _index = 0;
                std::size_t _match = 0;
                (((index<Args, Args...> == _index) ?
                    _result[_match++] = _index++ : ++_index), ...);
                return _result;
            }

            // Keep the first N Args in Tuple
            template<std::size_t, class> struct take_impl;
            template<std::size_t N, template<class...> class Tuple, class ...Args>
            struct take_impl<N, Tuple<Args...>> {
                template<class> struct helper;
                template<std::size_t ...Is>
                struct helper<std::index_sequence<Is...>> {
                    using type = Tuple<element<Is, Args...>...>;
                };
                using type = typename helper<std::make_index_sequence<N>>::type;
            };
            template<std::size_t N, class Ty>
            using take = typename take_impl<N, Ty>::type;

            // Drop the first N Args in Tuple
            template<std::size_t, class> struct drop_impl;
            template<std::size_t N, template<class...> class Tuple, class ...Args>
            struct drop_impl<N, Tuple<Args...>> {
                template<class> struct helper;
                template<std::size_t ...Is>
                struct helper<std::index_sequence<Is...>> {
                    using type = Tuple<element<Is + N, Args...>...>;
                };
                using type = typename helper<
                    std::make_index_sequence<sizeof...(Args) - N>>::type;
            };
            template<std::size_t N, class Ty>
            using drop = typename drop_impl<N, Ty>::type;

            // Helper to only keep types at indices in Array
            template<auto, class> struct keep_at_indices;
            template<auto Array, template<class...> class Tuple, class ...Args>
            struct keep_at_indices<Array, Tuple<Args...>> {
                template<class> struct helper;
                template<std::size_t ...Is>
                struct helper<std::index_sequence<Is...>> {
                    using type = Tuple<element<Array[Is], Args...>...>;
                };
                using type = typename helper<std::make_index_sequence<Array.size()>>::type;
            };

            // Remove Ty from Args
            template<class, class> struct remove_impl;
            template<class Ty, template<class...> class Tuple, class ...Args>
            struct remove_impl<Ty, Tuple<Args...>> {
                using type = typename keep_at_indices<
                    indices_except<Ty, Args...>, Tuple<Args...>>::type;
            };
            template<class T, class Ty>
            using remove = typename remove_impl<T, Ty>::type;

            // Remove Tys from Args
            template<class, class> struct remove_all_impl;
            template<template<class...> class Ty, class ...Tys,
                template<class...> class Tuple, class ...Args>
            struct remove_all_impl<Ty<Tys...>, Tuple<Args...>> {
                using type = typename keep_at_indices<
                    indices_except_all<kaixo::pack<Tys...>, Args...>, Tuple<Args...>>::type;
            };
            template<class T, class Ty>
            using remove_all = typename remove_all_impl<T, Ty>::type;

            // Keeps Ty in Args
            template<class, class> struct keep_impl;
            template<class Ty, template<class...> class Tuple, class ...Args>
            struct keep_impl<Ty, Tuple<Args...>> {
                using type = typename keep_at_indices<
                    indices<Ty, Args...>, Tuple<Args...>>::type;
            };
            template<class T, class Ty>
            using keep = typename keep_impl<T, Ty>::type;

            // Keeps Tys in Args
            template<class, class> struct keep_all_impl;
            template<template<class...> class Ty, class ...Tys,
                template<class...> class Tuple, class ...Args>
            struct keep_all_impl<Ty<Tys...>, Tuple<Args...>> {
                using type = typename keep_at_indices<
                    indices_all<Ty<Tys...>, Args...>, Tuple<Args... >>::type;
            };
            template<class T, class Ty>
            using keep_all = typename keep_all_impl<T, Ty>::type;

            // Generates indices from 0 to I, excluding all in C
            template<std::size_t I, std::size_t ...C>
            constexpr auto generate_indices = []() {
                std::array<std::size_t, I - sizeof...(C)> _indices{};
                std::size_t _index = 0;
                std::size_t _match = 0;
                for (std::size_t _index = 0; _index < I; ++_index)
                    if (((_index != C) && ...)) _indices[_match++] = _index;
                return _indices;
            }();

            // Remove index I from Args
            template<std::size_t, class> struct erase_impl;
            template<std::size_t I, template<class...> class Tuple, class ...Args>
            struct erase_impl<I, Tuple<Args...>> {
                using type = typename keep_at_indices<
                    generate_indices<sizeof...(Args), I>, Tuple<Args... >>::type;
            };
            template<std::size_t I, class Ty>
            using erase = typename erase_impl<I, Ty>::type;

            // Erase all indices I from Args
            template<class, std::size_t...> struct erase_all_impl;
            template<std::size_t ...I, template<class...> class Tuple, class ...Args>
            struct erase_all_impl<Tuple<Args...>, I...> {
                using type = typename keep_at_indices<
                    generate_indices<sizeof...(Args), I...>, Tuple<Args... >>::type;
            };
            template<class Ty, std::size_t ...Is>
            using erase_all = typename erase_all_impl<Ty, Is...>::type;

            // Append Ty to Args
            template<class, class> struct append_impl;
            template<class Ty, template<class...> class Tuple, class ...Args>
            struct append_impl<Ty, Tuple<Args...>> {
                using type = Tuple<Args..., Ty>;
            };
            template<class T, class Ty>
            using append = typename append_impl<T, Ty>::type;

            // Append Tys to Args
            template<class, class> struct append_all_impl;
            template<template<class...> class Ty, class ...Tys,
                template<class...> class Tuple, class ...Args>
            struct append_all_impl<Ty<Tys...>, Tuple<Args...>> {
                using type = Tuple<Args..., Tys...>;
            };
            template<class T, class Ty>
            using append_all = typename append_all_impl<T, Ty>::type;

            // Prepend Ty to Args
            template<class, class> struct prepend_impl;
            template<class Ty, template<class...> class Tuple, class ...Args>
            struct prepend_impl<Ty, Tuple<Args...>> {
                using type = Tuple<Ty, Args...>;
            };
            template<class T, class Ty>
            using prepend = typename prepend_impl<T, Ty>::type;

            // Prepend Tys to Args
            template<class, class> struct prepend_all_impl;
            template<template<class...> class Ty, class ...Tys,
                template<class...> class Tuple, class ...Args>
            struct prepend_all_impl<Ty<Tys...>, Tuple<Args...>> {
                using type = Tuple<Tys..., Args...>;
            };
            template<class T, class Ty>
            using prepend_all = typename prepend_all_impl<T, Ty>::type;

            // Insert T at position I in the template parameters of Ty
            template<std::size_t I, class T, class Ty>
            using insert = append_all<drop<I, Ty>, append<T, take<I, Ty>>>;

            // Insert T at position I in the template parameters of Ty
            template<std::size_t I, class T, class Ty>
            using insert_all = append_all<drop<I, Ty>, append_all<T, take<I, Ty>>>;

            // Get indices of first occurrences of all types in Args
            template<class ...Args>
            constexpr auto first_indices = first_indices_impl<Args...>();

            // Erase all indices I from Args
            template<class> struct unique_impl;
            template<template<class...> class Tuple, class ...Args>
            struct unique_impl<Tuple<Args...>> {
                template<class> struct helper;
                template<std::size_t ...Is>
                struct helper<std::index_sequence<Is...>> {
                    using type = Tuple<element<
                        first_indices<Args...>[Is], Args...>...>;
                };
                using type = typename helper<
                    std::make_index_sequence<unique_count<Args...>>>::type;
            };
            template<class Ty>
            using unique = typename unique_impl<Ty>::type;

            // Keeps all template parameters of Ty from index S to E 
            template<std::size_t S, std::size_t E, class Ty>
            using sub = take<(E - S), drop<S, Ty>>;

            // Fake lambda used for filtering
            template<class L>
            struct fake_lambda : L {
                template<class Ty> requires requires (L l) {
                    { l.template operator() < Ty::get() > () } -> std::same_as<bool>;
                }
                consteval bool operator()() { return L::template operator() < Ty::get() > (); }
                // Operator can't be instantiated with Ty
                template<class Ty> requires (!requires (L l) {
                    { l.template operator() < Ty > () };
                } && (!requires (L l) {
                    { l.template operator() < Ty::get() > () } -> std::same_as<bool>;
                }))
                    consteval bool operator()() { return false; }
                // Operator can be instantiated and returns a bool
                template<class Ty> requires (requires (L l) {
                    { l.template operator() < Ty > () } -> std::same_as<bool>;
                } && (!requires (L l) {
                    { l.template operator() < Ty::get() > () } -> std::same_as<bool>;
                }))
                    consteval bool operator()() { // Call operator
                    return L::template operator() < Ty > ();
                }
                // Operator can be instantiated but returns void
                // This means constraint on template parameter.
                template<class Ty> requires requires (L l) {
                    { l.template operator() < Ty > () } -> std::same_as<void>;
                }
                consteval bool operator()() { return true; }
            };

            // Count amount of types in Args after filtering
            template<auto Lambda, class ...Args>
            consteval std::size_t count_filter_impl() {
                return ((static_cast<std::size_t>(fake_lambda{ Lambda }.operator() < Args > ())) + ...);
            }
            template<auto Lambda, class ...Args>
            constexpr std::size_t count_filter = count_filter_impl<Lambda, Args...>();

            // Get all indices that match the filter
            template<auto Lambda, class ...Args>
            consteval std::array<std::size_t,
                count_filter<Lambda, Args...>> filter_indices_impl() {
                std::array<std::size_t, count_filter<Lambda, Args...>> _indices{};
                std::size_t _index = 0;
                std::size_t _match = 0;
                ((fake_lambda{ Lambda }.operator() < Args > () ?
                    _indices[_match++] = _index++ : ++_index), ...);
                return _indices;
            }
            template<auto Lambda, class ...Args>
            constexpr std::array<std::size_t, count_filter<Lambda, Args...>>
                filter_indices = filter_indices_impl<Lambda, Args...>();

            // Apply filter
            template<class, auto> struct filter_impl;
            template<auto Lambda, template<class...> class Tuple, class ...Args>
            struct filter_impl<Tuple<Args...>, Lambda> {
                template<class> struct helper;
                template<std::size_t ...Is>
                struct helper<std::index_sequence<Is...>> {
                    using type = Tuple<element<filter_indices<Lambda, Args...>[Is], Args...>...>;
                };
                using type = typename helper<
                    std::make_index_sequence<count_filter<Lambda, Args...>>>::type;
            };

            // Filter the template parameters of Ty using a templated lambda
            template<class Ty, auto Lambda>
            using filter = typename detail::filter_impl<Ty, Lambda>::type;

            // Merge sort for types
            template<auto Lambda, class ...As>
            constexpr auto merge_sort_merge(kaixo::pack<As...>, kaixo::pack<>) {
                return kaixo::pack<As...>{};
            }
            template<auto Lambda, class ...Bs>
            constexpr auto merge_sort_merge(kaixo::pack<>, kaixo::pack<Bs...>) {
                return kaixo::pack<Bs...>{};
            }
            template<auto Lambda, class A, class B>
            constexpr auto merge_sort_lambda(
                std::type_identity<A>, std::type_identity<B>) {
                return Lambda.template operator() < A, B > ();
            }
            template<auto Lambda, class A, class ...As, class B, class ...Bs>
            constexpr auto merge_sort_merge(kaixo::pack<A, As...>, kaixo::pack<B, Bs...>) {
                if constexpr (merge_sort_lambda<Lambda>(
                    std::type_identity<A>{}, std::type_identity<B>{}))
                    return append_all<
                    decltype(merge_sort_merge<Lambda>(
                        kaixo::pack<As...>{},
                        kaixo::pack<B, Bs...>{})),
                    kaixo::pack<A >> {};
                else
                    return append_all<
                    decltype(merge_sort_merge<Lambda>(
                        kaixo::pack<A, As...>{},
                        kaixo::pack<Bs...>{})),
                    kaixo::pack<B >> {};
            }
            template<auto Lambda, class ...Args>
            constexpr auto merge_sort(kaixo::pack<Args...>) {
                if constexpr (kaixo::pack<Args...>::size > 1) {
                    constexpr std::size_t mid = kaixo::pack<Args...>::size / 2;
                    constexpr auto left = merge_sort<Lambda>(typename kaixo::pack<Args...>::template take<mid>{});
                    constexpr auto right = merge_sort<Lambda>(typename kaixo::pack<Args...>::template drop<mid>{});

                    return merge_sort_merge<Lambda>(left, right);
                }
                else return kaixo::pack<Args...>{};
            }

            template<auto Lambda, class Ty>
            using sort_impl = decltype(merge_sort<Lambda>(Ty{}));

            template<class...>struct concat_impl;
            template<template<class...> class A, class ...As>
            struct concat_impl<A<As...>> { using type = A<As...>; };
            template<template<class...> class A, template<class...> class B, class ...As, class ...Bs, class ...Rest>
            struct concat_impl<A<As...>, B<Bs...>, Rest...> { using type = typename concat_impl<B<As..., Bs...>, Rest...>::type; };
        }

        template<class Ty> concept is_pack = specialization<Ty, pack>;

        // Pack utils for a pack of types
        template<class ...Args>
        struct pack_base {
            constexpr static std::size_t size = sizeof...(Args);
            constexpr static std::size_t unique_count = detail::unique_count<Args...>;
            constexpr static std::size_t bytes = (sizeof_v<Args> +... + 0);

            template<class Ty> // First index of Ty in Args
            constexpr static std::size_t index = detail::index<Ty, Args...>;

            template<class Ty> // Last index of Ty in Args
            constexpr static std::size_t last_index = detail::last_index<Ty, Args...>;

            template<class Ty> // Amount of occurrences of Ty in Args
            constexpr static std::size_t count = detail::count<Ty, Args...>;

            template<class Ty> // Check if Ty occurs in Args
            constexpr static bool occurs = detail::occurs<Ty, Args...>;

            template<class ...Pack> // All indices of all in Pack in Args
            constexpr static auto indices = detail::indices_all<detail::pack_or_as_pack<Pack...>, Args...>;

            template<class ...Pack> // All indices of all types in Args except those in in Pack
            constexpr static auto indices_except = detail::indices_except_all<detail::pack_or_as_pack<Pack...>, Args...>;

            template<auto Lambda> // All indices of all types that match the filter
            constexpr static auto indices_filter = detail::filter_indices<Lambda, Args...>;

            template<class ...Pack> // Remove all in Pack from Args
            using remove = detail::remove_all<detail::pack_or_as_pack<Pack...>, pack<Args...>>;

            template<class ...Pack> // Only keep all in Pack in Args
            using keep = detail::keep_all<detail::pack_or_as_pack<Pack...>, pack<Args...>>;

            template<std::size_t I> // Get element at index I
            using element = detail::element<I, Args...>;

            template<std::size_t I> // Get info of type at index I
            using element_info = kaixo::info<detail::element<I, Args...>>;

            using pack_info = kaixo::info<pack<Args...>>;

            template<std::size_t N> // Get first N elements in Args
            using take = detail::take<N, pack<Args...>>;

            template<std::size_t N> // Remove first N elements from Args
            using drop = detail::drop<N, pack<Args...>>;

            // Remove first element from Args
            using tail = drop<1>;

            // First element in Args
            using head = element<0>;

            // Remove last element from Args
            using init = take<size - 1>;

            // Last element in Args
            using last = element<size - 1>;

            // Move the template parameters Args to the templated type Ty
            template<template<class...> class Ty>
            using as = kaixo::move_types<pack<Args...>, Ty>;

            template<class ...Pack> // Append the types in Pack to Args
            using append = typename detail::concat_impl<pack<Args...>, detail::pack_or_as_pack<Pack...>>::type;

            template<class ...Pack> // Prepend the types in Pack to Args
            using prepend = typename detail::concat_impl<detail::pack_or_as_pack<Pack...>, pack<Args...>>::type;

            template<std::size_t I, class ...Pack> // Insert the types Tys in Args at index I
            using insert = detail::insert_all<I, detail::pack_or_as_pack<Pack...>, pack<Args...>>;

            template<std::size_t ...Is> // Erase indices Is from Args
            using erase = detail::erase_all<pack<Args...>, Is...>;

            template<std::size_t I, class Ty>
            using swap = erase<I>::template insert<I, Ty>;

            // Create a sub pack from index S to index E of Args
            template<std::size_t S, std::size_t E>
            using sub = detail::sub<S, E, pack<Args...>>;

            // Reverse Args
            using reverse = detail::reverse<pack<Args...>>;

            // Remove duplicates from Args
            using unique = detail::unique<pack<Args...>>;

            template<auto Lambda> // Filter using templated lambda
            using filter = detail::filter<pack<Args...>, Lambda>;

            template<auto Lambda> // Sort using templated lambda
            using sort = detail::sort_impl<Lambda, pack<Args...>>;

            // Calls lambda with all types as template arguments, like
            // Lambda.operator()<Args...>();
            constexpr static auto for_each = [](auto Lambda) {
                return Lambda.operator() < Args... > ();
            };
        };

        // Pack utils for a pack of values
        template<auto ...Args>
        struct pack_base<value_t<Args>...> {
            constexpr static std::size_t size = sizeof...(Args);
            constexpr static std::size_t unique_count = detail::unique_count<value_t<Args>...>;
            constexpr static std::size_t bytes = (sizeof_v<decltype(Args)> +... + 0);

            template<auto Ty>
            constexpr static std::size_t index = detail::index<value_t<Ty>, value_t<Args>...>;

            template<auto Ty>
            constexpr static std::size_t last_index = detail::last_index<value_t<Ty>, value_t<Args>...>;

            template<auto Ty>
            constexpr static std::size_t count = detail::count<value_t<Ty>, value_t<Args>...>;

            template<auto Ty> // Check if Ty occurs in Args
            constexpr static bool occurs = detail::occurs<value_t<Ty>, value_t<Args>...>;

            template<class ...Pack> // All indices of all Tys in Args
            constexpr static auto indices = detail::indices_all<detail::pack_or_as_pack<Pack...>, value_t<Args>...>;

            template<class ...Pack> // All indices of all values in Args except those in Tys
            constexpr static auto indices_except = detail::indices_except_all<detail::pack_or_as_pack<Pack...>, value_t<Args>...>;

            template<auto Lambda> // All indices of all types that match the filter
            constexpr static auto indices_filter = detail::filter_indices<Lambda, value_t<Args>...>;

            template<class ...Pack> // Remove all Tys from Args
            using remove = detail::remove_all<detail::pack_or_as_pack<Pack...>, pack<value_t<Args>...>>;

            template<class ...Pack> // Only keep all Tys in Args
            using keep = detail::keep_all<detail::pack_or_as_pack<Pack...>, pack<value_t<Args>...>>;

            template<std::size_t I> // Get element at index I
            constexpr static auto element = detail::element<I, value_t<Args>...>::value;

            template<std::size_t I> // Info of type of value at index I
            using element_info = kaixo::info<value_t<element<I>>>;

            using pack_info = kaixo::info<pack<value_t<Args>...>>;

            template<std::size_t N> // Get first N elements in Args
            using take = detail::take<N, pack<value_t<Args>...>>;

            template<std::size_t N> // Remove first N elements from Args
            using drop = detail::drop<N, pack<value_t<Args>...>>;

            // Remove first element from Args
            using tail = drop<1>;

            // First element in Args
            constexpr static auto head = element<0>;

            // Remove last element from Args
            using init = take<size - 1>;

            // Last element in Args
            constexpr static auto last = element<size - 1>;

            // Move the template parameters Args to the templated type Ty
            template<template<class...> class Ty>
            using as = kaixo::move_types<pack<value_t<Args>...>, Ty>;

            template<class ...Pack> // Append the types Tys to Args
            using append = typename detail::concat_impl<pack<value_t<Args>...>, detail::pack_or_as_pack<Pack...>>::type;

            template<class ...Pack> // Prepend the types Tys to Args
            using prepend = typename detail::concat_impl<detail::pack_or_as_pack<Pack...>, pack<value_t<Args>...>>::type;

            template<std::size_t I, class ...Pack> // Insert the types Tys in Args at index I
            using insert = detail::insert_all<I, detail::pack_or_as_pack<Pack...>, pack<value_t<Args>...>>;

            template<std::size_t ...Is> // Erase indices Is from Args
            using erase = detail::erase_all<pack<value_t<Args>...>, Is...>;

            template<std::size_t I, class Ty>
            using swap = erase<I>::template insert<I, Ty>;

            // Create a sub pack from index S to index E of Args
            template<std::size_t S, std::size_t E>
            using sub = detail::sub<S, E, pack<value_t<Args>...>>;

            // Reverse Args
            using reverse = detail::reverse<pack<value_t<Args>...>>;

            // Remove duplicates from Args
            using unique = detail::unique<pack<value_t<Args>...>>;

            template<auto Lambda> // Filter using a lambda
            using filter = detail::filter<pack<value_t<Args>...>, Lambda>;

            template<auto Lambda> // Sort using a Lambda
            using sort = detail::sort_impl<Lambda, pack<value_t<Args>...>>;

            // Calls lambda with all values as template arguments, like
            // Lambda.operator()<Args...>();
            constexpr static auto for_each = [](auto Lambda) {
                return Lambda.operator() < Args... > ();
            };

            constexpr static auto fold = [](auto Lambda) {
                return Lambda.operator() < Args... > ();
            };
        };

        template<> // Special case for empty pack
        struct pack_base<> {
            constexpr static std::size_t size = 0;
            constexpr static std::size_t unique_count = 0;
            constexpr static std::size_t bytes = 0;

            template<class Ty> // First index of Ty in Args
            constexpr static std::size_t index = detail::index<Ty>;

            template<class Ty> // Last index of Ty in Args
            constexpr static std::size_t last_index = detail::last_index<Ty>;

            template<class Ty> // Amount of occurrences of Ty in Args
            constexpr static std::size_t count = 0;

            template<class Ty> // Check if Ty occurs in Args
            constexpr static bool occurs = false;

            template<class ...Pack> // All indices of all Tys in Args
            constexpr static auto indices = std::array<std::size_t, 0>{};

            template<class ...Pack> // All indices of all types in Args except those in Tys
            constexpr static auto indices_except = std::array<std::size_t, 0>{};

            template<auto Lambda> // All indices of all types that match the filter
            constexpr static auto indices_filter = std::array<std::size_t, 0>{};

            template<class ...Pack> // Remove all Tys from Args
            using remove = kaixo::pack<>;

            template<class ...Pack> // Only keep all Tys in Args
            using keep = kaixo::pack<>;

            template<std::size_t I> // Get element at index I
            using element = detail::element<I>;

            template<std::size_t I> // Get element at index I
            using info = kaixo::info<detail::element<I>>;

            using pack_info = kaixo::info<pack<>>;

            template<std::size_t N> // Get first N elements in Args
            using take = detail::take<N, pack<>>;

            template<std::size_t N> // Remove first N elements from Args
            using drop = detail::drop<N, pack<>>;

            // Move the template parameters Args to the templated type Ty
            template<template<class...> class Ty>
            using as = kaixo::move_types<pack<>, Ty>;

            template<class ...Pack> // Append the types Tys to Args
            using append = detail::pack_or_as_pack<Pack...>;

            template<class ...Pack> // Prepend the types Tys to Args
            using prepend = detail::pack_or_as_pack<Pack...>;

            template<std::size_t I, class ...Pack> // Insert the types Tys in Args at index I
            using insert = detail::pack_or_as_pack<Pack...>;

            template<std::size_t ...Is> // Erase indices Is from Args
            using erase = detail::erase_all<pack<>, Is...>;

            template<std::size_t I, class Ty>
            using swap = erase<I>::template insert<I, Ty>;

            // Create a sub pack from index S to index E of Args
            template<std::size_t S, std::size_t E>
            using sub = detail::sub<S, E, pack<>>;

            // Reverse Args
            using reverse = kaixo::pack<>;

            // Remove duplicates from Args
            using unique = kaixo::pack<>;

            template<auto Lambda> // Filter using templated lambda
            using filter = kaixo::pack<>;

            template<auto Lambda> // Sort using templated lambda
            using sort = kaixo::pack<>;

            // Pack is empty so just calls the lambda with no template arguments
            constexpr static auto for_each = [](auto Lambda) { return Lambda(); };
        };

        template<class ...Args>
        struct pack : pack_base<Args...>, info<pack_base<Args...>> {};

        template<> struct pack<> : pack_base<> {};

        template<class Ty, std::size_t N = 1> // Fill a pack with N * Ty
        using fill_pack = decltype([]<std::size_t ...Is, class Ty>
            (std::index_sequence<Is...>, std::type_identity<Ty>) {
            return pack<change<value_t<Is>, Ty>...>{};
        }(std::make_index_sequence<N>{}, std::type_identity<Ty>{}));

        // Concat template arguments of templated types Args...
        // requires all templated types to be the same
        template<class ...Args>
        using concat = typename detail::concat_impl<Args...>::type;

        template<class Ty> // Convert templated type to pack
        using as_pack = move_types<Ty, kaixo::pack>;

        template<auto ...Values> // Template pack of values to pack
        using to_pack = kaixo::pack<value_t<Values>...>;

        // Call lambda with array values as template arguments, like
        // Lambda.operator()<Array[Is]...>();
        template<auto Array>
        constexpr auto iterate = [](auto Lambda) {
            return[Lambda]<std::size_t ...Is>(std::index_sequence<Is...>) {
                return Lambda.operator() < Array[Is]... > ();
            }(std::make_index_sequence<Array.size()>{});
        };

        template<auto Array>
        using array_to_pack = decltype(iterate<Array>([]<auto ...Is>{ return to_pack<Is...>{}; }));

        /**
         * Create fold expression lambda.
         * for example: sequence<5, 10>(Args + ___);
         * is short for: sequence<5, 10>([]<auto ...Args>{ return (Args + ...); });
         */
        namespace fold {
            struct fold_t {
                consteval auto operator=(fold_t) { 
                    return []<auto...Args> {
                        if constexpr (sizeof...(Args) > 0) return (Args = ...);
                        else return;
                    }; 
                }
            };
#define FOLD_OP(op) consteval auto operator op(fold_t, fold_t) {             \
                return [] <auto...Args> {                                    \
                    if constexpr (sizeof...(Args) > 0) return (Args op ...); \
                    else return; }; }

            FOLD_OP(+) FOLD_OP(-) FOLD_OP(*) FOLD_OP(/ ) FOLD_OP(%) FOLD_OP(^) FOLD_OP(&) FOLD_OP(| );
            FOLD_OP(< ) FOLD_OP(> ) FOLD_OP(<< ) FOLD_OP(>> ) FOLD_OP(+= ) FOLD_OP(-= ) FOLD_OP(*= );
            FOLD_OP(/= ) FOLD_OP(%= ) FOLD_OP(^= ) FOLD_OP(&= ) FOLD_OP(|= ) FOLD_OP(<<= ) FOLD_OP(>>= );
            FOLD_OP(== ) FOLD_OP(!= ) FOLD_OP(<= ) FOLD_OP(>= ) FOLD_OP(->*);
            consteval auto operator,(fold_t, fold_t) { return[]<auto...Args>{ return (Args, ...); }; }
            consteval auto operator&&(fold_t, fold_t) { return[]<auto...Args>{ return (Args && ...); }; }
            consteval auto operator||(fold_t, fold_t) { return[]<auto...Args>{ return (Args || ...); }; }
            constexpr fold_t Args{};
            constexpr fold_t ___{};
        }
#undef FOLD_OP
    }

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                           *
     *                      Function Utils                       *
     *                                                           *
     *            Helper templates to make dealing with          *
     *               functions and member functions              *
     *                    a little bit easier                    *
     *                                                           *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
    
    inline namespace function_utils_n {

        namespace functional {
            constexpr auto unit = []<class Ty>(Ty && i) -> Ty&& { return std::forward<Ty>(i); };
            template<class To>
            constexpr auto cast = []<class Ty>(Ty && i) -> To { return static_cast<To>(std::forward<Ty>(i)); };
        }

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
    using pointer = info<R(*)(Args...) NOEXCEPT>;                                        \
    using signature = info<R(Args...) CONST VOLATILE REF NOEXCEPT>;                      \
    using object = info<CONST VOLATILE Ty REF>;                                          \
    using result = info<R>;                                                              \
    using arguments = pack<Args...>;                                                     \
    constexpr static bool is_fun_const = std::same_as<const int, CONST int>;             \
    constexpr static bool is_fun_mutable = !is_fun_const;                                \
    constexpr static bool is_fun_volatile = std::same_as<volatile int, VOLATILE int>;    \
    constexpr static bool is_fun_lvalue_reference = std::same_as<int&, int REF>;         \
    constexpr static bool is_fun_rvalue_reference = std::same_as<int&&, int REF>;        \
    constexpr static bool is_fun_reference = std::is_reference_v<int REF>;               \
    constexpr static bool is_noexcept = std::same_as<void() noexcept, void() NOEXCEPT>;  \
    using add_fun_const = info<R(Ty::*)(Args...) const VOLATILE REF NOEXCEPT>;           \
    using remove_fun_const = info<R(Ty::*)(Args...) VOLATILE REF NOEXCEPT>;              \
    using add_fun_volatile = info<R(Ty::*)(Args...) CONST volatile REF NOEXCEPT>;        \
    using remove_fun_volatile = info<R(Ty::*)(Args...) CONST REF NOEXCEPT>;              \
    using add_fun_cv = info<R(Ty::*)(Args...) const volatile REF NOEXCEPT>;              \
    using remove_fun_cv = info<R(Ty::*)(Args...) REF NOEXCEPT>;                          \
    using add_fun_lvalue_reference = info<R(Ty::*)(Args...) CONST VOLATILE & NOEXCEPT>;  \
    using add_fun_rvalue_reference = info<R(Ty::*)(Args...) CONST VOLATILE && NOEXCEPT>; \
    using remove_fun_reference = info<R(Ty::*)(Args...) CONST VOLATILE NOEXCEPT>;        \
    using remove_fun_cvref = info<R(Ty::*)(Args...) NOEXCEPT>;                           \
    using add_noexcept = info<R(Ty::*)(Args...) CONST VOLATILE REF noexcept>;            \
    using remove_noexcept = info<R(Ty::*)(Args...) CONST VOLATILE REF>;                  \
};

        KAIXO_MEMBER_CALL(KAIXO_MEMBER_FUNCTION_INFO_MOD);
#undef KAIXO_MEMBER_FUNCTION_INFO_MOD

#define KAIXO_FUNCTION_INFO_MOD(CONST, VOLATILE, REF, NOEXCEPT)                         \
template<class R, class ...Args>                                                        \
struct function_info_impl<R(Args...) CONST VOLATILE REF NOEXCEPT> {                     \
    using pointer = info<R(*)(Args...) NOEXCEPT>;                                       \
    using signature = info<R(Args...) CONST VOLATILE REF NOEXCEPT>;                     \
    using result = info<R>;                                                             \
    using arguments = pack<Args...>;                                                    \
    constexpr static bool is_fun_const = std::same_as<const int, CONST int>;            \
    constexpr static bool is_fun_mutable = !is_fun_const;                               \
    constexpr static bool is_fun_volatile = std::same_as<volatile int, VOLATILE int>;   \
    constexpr static bool is_fun_lvalue_reference = std::same_as<int&, int REF>;        \
    constexpr static bool is_fun_rvalue_reference = std::same_as<int&&, int REF>;       \
    constexpr static bool is_fun_reference = std::is_reference_v<int REF>;              \
    constexpr static bool is_noexcept = std::same_as<void() noexcept, void() NOEXCEPT>; \
    using add_fun_const = info<R(Args...) const VOLATILE REF NOEXCEPT>;                 \
    using remove_fun_const = info<R(Args...) VOLATILE REF NOEXCEPT>;                    \
    using add_fun_volatile = info<R(Args...) CONST volatile REF NOEXCEPT>;              \
    using remove_fun_volatile = info<R(Args...) CONST REF NOEXCEPT>;                    \
    using add_fun_cv = info<R(Args...) const volatile REF NOEXCEPT>;                    \
    using remove_fun_cv = info<R(Args...) REF NOEXCEPT>;                                \
    using add_fun_lvalue_reference = info<R(Args...) CONST VOLATILE & NOEXCEPT>;        \
    using add_fun_rvalue_reference = info<R(Args...) CONST VOLATILE && NOEXCEPT>;       \
    using remove_fun_reference = info<R(Args...) CONST VOLATILE NOEXCEPT>;              \
    using remove_fun_cvref = info<R(Args...) NOEXCEPT>;                                 \
    using add_noexcept = info<R(Args...) CONST VOLATILE REF noexcept>;                  \
    using remove_noexcept = info<R(Args...) CONST VOLATILE REF>;                        \
};

        KAIXO_MEMBER_CALL(KAIXO_FUNCTION_INFO_MOD);
#undef KAIXO_FUNCTION_INFO_MOD

#define KAIXO_FUNCTION_PTR_INFO_MOD(NOEXCEPT)                                               \
template<class R, class ...Args>                                                            \
struct function_info_impl<R(*)(Args...) NOEXCEPT> {                                         \
    using pointer = info<R(*)(Args...) NOEXCEPT>;                                           \
    using signature = info<R(Args...) NOEXCEPT>;                                            \
    using result = info<R>;                                                                 \
    using arguments = pack<Args...>;                                                        \
    constexpr static bool is_noexcept = std::same_as<void() noexcept, void() NOEXCEPT>;     \
    using add_noexcept = info<R(Args...) noexcept>;                                         \
    using remove_noexcept = info<R(Args...)>;                                               \
};

        KAIXO_FUNCTION_PTR_INFO_MOD(NO_ARG);
        KAIXO_FUNCTION_PTR_INFO_MOD(noexcept);
#undef KAIXO_FUNCTION_PTR_INFO_MOD

        template<class Ty> using function_info = function_info_impl<std::remove_cv_t<std::remove_reference_t<Ty>>>;
        template<auto Ty> using function_info_v = function_info_impl<std::remove_cv_t<std::remove_reference_t<decltype(Ty)>>>;
        template<class Ty> concept callable_type = requires() { typename kaixo::function_info<Ty>::result; };
    }

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                           *
     *                       Sequence Utils                      *
     *                                                           *
     *           Template helpers for integer sequences          *
     *                                                           *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    inline namespace sequence_utils_n {

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
        template<std::integral auto S, std::integral auto E = npos> constexpr auto indexed_for = [](auto lambda) {
            if constexpr (E == npos)
                [&] <auto ...Is>(std::integer_sequence<decltype(S), Is...>) {
                (lambda.operator() < Is > (), ...);
            }(std::make_integer_sequence<decltype(S), S>{});
            else[&] <auto ...Is>(std::integer_sequence<decltype(E - S), Is...>) {
                (lambda.operator() < Is + S > (), ...);
            }(std::make_integer_sequence<decltype(E - S), E - S>{});
        };

        // Test if a functor is invocable without implicit conversions
        template<class Functor, class ...Args>
        constexpr bool invocable_no_conversions = [] {
            // If not callable type, it is a templated functor, so just check invocability
            if constexpr (!is_functor<Functor>)
                return std::invocable<Functor, Args...>;
            else { // Otherwise check matching arguments
                using tys = pack<Args...>;
                using args = info<Functor>::arguments;
                if constexpr (args::size != tys::size) return false;
                else {
                    const auto check_one = []<std::size_t I>(value_t<I>) {
                        using ty1 = tys::template element_info<I>;
                        using ty2 = args::template element_info<I>;

                        return std::same_as<typename ty1::decay::type, typename ty2::decay::type>
                            && std::convertible_to<typename ty1::type, typename ty2::type>;
                    };
                    return sequence<args::size>([&]<std::size_t ...Is>{
                        return (check_one(value_t<Is>{}) && ...);
                    });
                }
            }
        }();

        // templated for, for tuple, supports concept constraints
        constexpr auto tuple_for = []<class Tuple>(Tuple && tuple, auto... lambdas) {
            auto loop_no_conversions = [&]<std::size_t I>(auto & lambda, value_t<I>) {
                using l_type = std::decay_t<decltype(lambda)>;
                if constexpr (invocable_no_conversions<l_type,
                    decltype(std::get<I>(tuple))>) {
                    lambda(std::get<I>(tuple));
                    return true;
                }
                return false;
            };

            auto loop = [&]<std::size_t I>(auto & lambda, value_t<I>) {
                using l_type = std::decay_t<decltype(lambda)>;
                if constexpr (std::invocable<l_type,
                    decltype(std::get<I>(tuple))>) {
                    lambda(std::get<I>(tuple));
                    return true;
                }
                return false;
            };

            indexed_for<std::tuple_size_v<std::decay_t<Tuple>>>([&]<std::size_t I> {
                // 2 different checks, first check invocable without implicit conversions
                if (!(loop_no_conversions(lambdas, value_t<I>{}) || ...))
                    // If that fails, try with implicit conversions
                    (loop(lambdas, value_t<I>{}) || ...);
            });
        };
    }

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                           *
     *                        Type Utils                         *
     *                                                           *
     *            Template helpers to query type traits          *
     *                    a little bit easier                    *
     *                                                           *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    inline namespace type_utils_n {

        // All standard type traits as concepts
        namespace type_concepts {
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
            template<class Ty> concept unique_object_representations = std::has_unique_object_representations_v<Ty>;
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
            template<class Ty, class From> concept assignable = std::is_assignable_v<Ty, From>;
            template<class Ty, class From> concept trivially_assignable = std::is_trivially_assignable_v<Ty, From>;
            template<class Ty, class From> concept nothrow_assignable = std::is_nothrow_assignable_v<Ty, From>;
            template<class Ty> concept copy_assignable = std::is_copy_assignable_v<Ty>;
            template<class Ty> concept trivially_copy_assignable = std::is_trivially_copy_assignable_v<Ty>;
            template<class Ty> concept nothrow_copy_assignable = std::is_nothrow_copy_assignable_v<Ty>;
            template<class Ty> concept move_assignable = std::is_move_assignable_v<Ty>;
            template<class Ty> concept trivially_move_assignable = std::is_trivially_move_assignable_v<Ty>;
            template<class Ty> concept nothrow_move_assignable = std::is_nothrow_move_assignable_v<Ty>;
            template<class Ty> concept destructible = std::is_destructible_v<Ty>;
            template<class Ty> concept trivially_destructible = std::is_trivially_destructible_v<Ty>;
            template<class Ty> concept nothrow_destructible = std::is_nothrow_destructible_v<Ty>;
            template<class Ty> concept virtual_destructor = std::has_virtual_destructor_v<Ty>;
            template<class Ty, class Other> concept swappable_with = std::is_swappable_with_v<Ty, Other>;
            template<class Ty> concept swappable = std::is_swappable_v<Ty>;
            template<class Ty, class Other> concept nothrow_swappable_with = std::is_nothrow_swappable_with_v<Ty, Other>;
            template<class Ty> concept nothrow_swappable = std::is_nothrow_swappable_v<Ty>;
        
            template<class Ty, class Other> concept same_as = std::same_as<Ty, Other>;
            template<class Ty, class Other> concept base_of = std::is_base_of_v<Ty, Other>;
            template<class Ty, class Other> concept convertible_to = std::is_convertible_v<Ty, Other>;
            template<class Ty, class Other> concept nothrow_convertible_to = std::is_nothrow_convertible_v<Ty, Other>;

            template<template<class Ty, class ...Args> class Trait, class Ty, class ...Args>
            struct pack_trait_helper : Trait<Ty, Args...> {};
            template<template<class Ty, class ...Args> class Trait, class Ty, class ...Args>
            struct pack_trait_helper<Trait, Ty, pack<Args...>> : Trait<Ty, Args...> {};

            template<class Ty, class ...Args> concept constructible = pack_trait_helper<std::is_constructible, Ty, Args...>::value;
            template<class Ty, class ...Args> concept trivially_constructible = pack_trait_helper<std::is_trivially_constructible, Ty, Args...>::value;
            template<class Ty, class ...Args> concept nothrow_constructible = pack_trait_helper<std::is_nothrow_constructible, Ty, Args...>::value;
            template<class Ty, class ...Args> concept invocable = pack_trait_helper<std::is_invocable, Ty, Args...>::value;
            template<class Ty, class ...Args> concept nothrow_invocable = pack_trait_helper<std::is_nothrow_invocable, Ty, Args...>::value;
        }

        /**
         * Find amount of members in a struct.
         * @tparam Ty struct
         */
        template<class Ty>
        struct struct_size_impl {
            /**
             * Finds struct size by trying to construct the struct using 
             * a type that's convertible to anything but the struct itself
             * (to prevent copy or move constructor from being called and
             *  making it result in 1 when it should be 0), starting at
             * sizeof(Ty) parameters, and trying 1 less each time until
             * it is constructible.
             */
            constexpr static std::size_t value = reverse_sequence<0, sizeof_v<Ty> + 1>([]<std::size_t ...Ns>{
                using convertible_type = not_convertible_to<Ty>;

                std::size_t res = 0;
                constexpr auto try_one = []<std::size_t ...Is> {
                    return type_concepts::constructible<Ty, change<value_t<Is>, convertible_type>...>;
                };

                ((sequence<Ns>(try_one) ? (res = Ns, true) : false) || ...);
                return res;
            });
        };

        /**
         * Find amount of members in a struct.
         * @tparam Ty struct
         */
        template<type_concepts::aggregate Ty>
        constexpr std::size_t struct_size = struct_size_impl<Ty>::value;

        /**
         * Find member types of a struct, uses a macro to define 
         * overloads up to 99 members using structured bindings.
         */
        template<type_concepts::aggregate Ty, std::size_t N>
        struct struct_members_impl {
            using types = pack<>;
        };

        template<type_concepts::aggregate Ty>
        struct struct_members_impl<Ty, 0> {
            using types = pack<>;
        };

#define STRUCT_MEMBERS_M(c, ...)                                               \
        template<type_concepts::aggregate Ty>                                  \
            requires (struct_size<Ty> == c)                                    \
        struct struct_members_impl<Ty, c> {                                    \
            using types = decltype([](Ty& ty) {                                \
                auto& [__VA_ARGS__] = ty;                                      \
                using tuple_t = decltype(std::tuple{ __VA_ARGS__ });           \
                return as_pack<tuple_t>{};                                     \
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

        KAIXO_UNIQUE_99(STRUCT_MEMBERS_M, 99)

        /**
         * Find the member types of a struct.
         * @tparam Ty struct
         */
        template<type_concepts::aggregate Ty>
        using struct_members = typename struct_members_impl<Ty, struct_size<Ty>>::types;

        /**
         * Some helpers to add/copy cvref qualifiers from one
         * type to another. Leaves other qualifiers intact, so when
         * changing just 'const', it won't change any other qualifiers.
         * Add will simply add on top of the existing qualifiers of 'To', and
         * copy will remove the qualifiers from 'To' first, and then add the ones
         * that 'From' has back.
         */

        template<class From, class To>
        struct add_ref_impl {
            using _lvalue = std::conditional_t<std::is_lvalue_reference_v<From>, std::add_lvalue_reference_t<To>, To>;
            using _rvalue = std::conditional_t<std::is_rvalue_reference_v<From>, std::add_rvalue_reference_t<_lvalue>, _lvalue>;
            using type = typename _rvalue;
        };

        template<class From, class To>
        using copy_ref_impl = add_ref_impl<From, std::decay_t<To>>;

        template<class From, class To>
        struct add_const_impl {
            using _unrefFrom = std::remove_reference_t<From>;
            using _unrefTo = std::remove_reference_t<To>;
            using _const = std::conditional_t<std::is_const_v<_unrefFrom>, typename add_ref_impl<To, std::add_const_t<_unrefTo>>::type, To>;
            using type = _const;
        };

        template<class From, class To>
        struct add_volatile_impl {
            using _unrefFrom = std::remove_reference_t<From>;
            using _unrefTo = std::remove_reference_t<To>;
            using _volatile = std::conditional_t<std::is_volatile_v<_unrefFrom>, typename add_ref_impl<To, std::add_volatile_t<To>>::type, To>;
            using type = _volatile;
        };

        template<class From, class To>
        struct add_cv_impl {
            using type = typename add_volatile_impl<From, typename add_const_impl<From, To>::type>::type;
        };

        template<class From, class To>
        using copy_const_impl = add_ref_impl<To, typename add_const_impl<From, std::remove_const_t<std::remove_reference_t<To>>>::type>;
        template<class From, class To>
        using copy_volatile_impl = add_ref_impl<To, typename add_volatile_impl<From, std::remove_volatile_t<std::remove_reference_t<To>>>::type>;
        template<class From, class To>
        using copy_cv_impl = add_ref_impl<To, typename add_cv_impl<From, std::remove_cv_t<std::remove_reference_t<To>>>::type>;

        template<class From, class To>
        struct add_cvref_impl {
            using _cv = typename add_cv_impl<From, To>::type;
            using _lvalue = std::conditional_t<std::is_lvalue_reference_v<From>, std::add_lvalue_reference_t<_cv>, _cv>;
            using _rvalue = std::conditional_t<std::is_rvalue_reference_v<From>, std::add_rvalue_reference_t<_lvalue>, _lvalue>;
            using type = typename _rvalue;
        };

        template<class From, class To>
        using copy_cvref_impl = add_cvref_impl<From, std::decay_t<To>>;

        template<class To, class From>
        using copy_const = typename copy_const_impl<From, To>::type;
        template<class To, class From>
        using copy_volatile = typename copy_volatile_impl<From, To>::type;
        template<class To, class From>
        using copy_cv = typename copy_cv_impl<From, To>::type;
        template<class To, class From>
        using copy_ref = typename copy_ref_impl<From, To>::type;
        template<class To, class From>
        using copy_cvref = typename copy_cvref_impl<From, To>::type;
        template<class To, class From = const int>
        using add_const = typename add_const_impl<From, To>::type;
        template<class To, class From = volatile int>
        using add_volatile = typename add_volatile_impl<From, To>::type;
        template<class To, class From = const volatile int>
        using add_cv = typename add_cv_impl<From, To>::type;
        template<class To, class From>
        using add_ref = typename add_ref_impl<From, To>::type;
        template<class To, class From>
        using add_cvref = typename add_cvref_impl<From, To>::type;

        template<class To>
        using remove_const = typename copy_const_impl<int, To>::type;
        template<class To>
        using remove_volatile = typename copy_volatile_impl<int, To>::type;
        template<class To>
        using remove_cv = typename copy_cv_impl<int, To>::type;

        /**
         * Type traits info base. Contains all standard type traits.
         * @tparam Ty type
         */
        template<class Ty> struct info_base : RTTI_Ftable<Ty> {
            constexpr static bool is_void = std::is_void_v<Ty>;
            constexpr static bool is_null_pointer = std::is_null_pointer_v<Ty>;
            constexpr static bool is_integral = std::is_integral_v<Ty>;
            constexpr static bool is_floating_point = std::is_floating_point_v<Ty>;
            constexpr static bool is_array = std::is_array_v<Ty>;
            constexpr static bool is_enum = std::is_enum_v<Ty>;
            constexpr static bool is_union = std::is_union_v<Ty>;
            constexpr static bool is_class = std::is_class_v<Ty>;
            constexpr static bool is_function = std::is_function_v<Ty>;
            constexpr static bool is_pointer = std::is_pointer_v<Ty>;
            constexpr static bool is_lvalue_reference = std::is_lvalue_reference_v<Ty>;
            constexpr static bool is_rvalue_reference = std::is_rvalue_reference_v<Ty>;
            constexpr static bool is_member_object_pointer = std::is_member_object_pointer_v<Ty>;
            constexpr static bool is_member_function_pointer = std::is_member_function_pointer_v<Ty>;
            constexpr static bool is_fundamental = std::is_fundamental_v<Ty>;
            constexpr static bool is_arithmetic = std::is_arithmetic_v<Ty>;
            constexpr static bool is_scalar = std::is_scalar_v<Ty>;
            constexpr static bool is_object = std::is_object_v<Ty>;
            constexpr static bool is_compound = std::is_compound_v<Ty>;
            constexpr static bool is_reference = std::is_reference_v<Ty>;
            constexpr static bool is_member_pointer = std::is_member_pointer_v<Ty>;
            constexpr static bool is_const = std::is_const_v<Ty>;
            constexpr static bool is_volatile = std::is_volatile_v<Ty>;
            constexpr static bool is_trivial = std::is_trivial_v<Ty>;
            constexpr static bool is_trivially_copyable = std::is_trivially_copyable_v<Ty>;
            constexpr static bool is_standard_layout = std::is_standard_layout_v<Ty>;
            constexpr static bool has_unique_object_representations = std::has_unique_object_representations_v<Ty>;
            constexpr static bool is_empty = std::is_empty_v<Ty>;
            constexpr static bool is_polymorphic = std::is_polymorphic_v<Ty>;
            constexpr static bool is_abstract = std::is_abstract_v<Ty>;
            constexpr static bool is_final = std::is_final_v<Ty>;
            constexpr static bool is_aggregate = std::is_aggregate_v<Ty>;
            constexpr static bool is_signed = std::is_signed_v<Ty>;
            constexpr static bool is_unsigned = std::is_unsigned_v<Ty>;
            constexpr static bool is_bounded_array = std::is_bounded_array_v<Ty>;
            constexpr static bool is_unbounded_array = std::is_unbounded_array_v<Ty>;
            template<class ...Args> constexpr static bool is_constructible = std::constructible_from<Ty, Args...>;
            template<class ...Args> constexpr static bool is_trivially_constructible = std::is_trivially_constructible_v<Ty, Args...>;
            template<class ...Args> constexpr static bool is_nothrow_constructible = std::is_nothrow_constructible_v<Ty, Args...>;
            constexpr static bool is_default_constructible = std::is_default_constructible_v<Ty>;
            constexpr static bool is_trivially_default_constructible = std::is_trivially_default_constructible_v<Ty>;
            constexpr static bool is_nothrow_default_constructible = std::is_nothrow_default_constructible_v<Ty>;
            constexpr static bool is_copy_constructible = std::is_copy_constructible_v<Ty>;
            constexpr static bool is_trivially_copy_constructible = std::is_trivially_copy_constructible_v<Ty>;
            constexpr static bool is_nothrow_copy_constructible = std::is_nothrow_copy_constructible_v<Ty>;
            constexpr static bool is_move_constructible = std::is_move_constructible_v<Ty>;
            constexpr static bool is_trivially_move_constructible = std::is_trivially_move_constructible_v<Ty>;
            constexpr static bool is_nothrow_move_constructible = std::is_nothrow_move_constructible_v<Ty>;
            template<class From> constexpr static bool is_assignable = std::is_assignable_v<Ty, From>;
            template<class From> constexpr static bool is_trivially_assignable = std::is_trivially_assignable_v<Ty, From>;
            template<class From> constexpr static bool is_nothrow_assignable = std::is_nothrow_assignable_v<Ty, From>;
            constexpr static bool is_copy_assignable = std::is_copy_assignable_v<Ty>;
            constexpr static bool is_trivially_copy_assignable = std::is_trivially_copy_assignable_v<Ty>;
            constexpr static bool is_nothrow_copy_assignable = std::is_nothrow_copy_assignable_v<Ty>;
            constexpr static bool is_move_assignable = std::is_move_assignable_v<Ty>;
            constexpr static bool is_trivially_move_assignable = std::is_trivially_move_assignable_v<Ty>;
            constexpr static bool is_nothrow_move_assignable = std::is_nothrow_move_assignable_v<Ty>;
            constexpr static bool is_destructible = std::is_destructible_v<Ty>;
            constexpr static bool is_trivially_destructible = std::is_trivially_destructible_v<Ty>;
            constexpr static bool is_nothrow_destructible = std::is_nothrow_destructible_v<Ty>;
            constexpr static bool has_virtual_destructor = std::has_virtual_destructor_v<Ty>;
            template<class Other> constexpr static bool is_swappable_with = std::is_swappable_with_v<Ty, Other>;
            constexpr static bool is_swappable = std::is_swappable_v<Ty>;
            template<class Other> constexpr static bool is_nothrow_swappable_with = std::is_nothrow_swappable_with_v<Ty, Other>;
            constexpr static bool is_nothrow_swappable = std::is_nothrow_swappable_v<Ty>;

            template<class Other> constexpr static bool is_same_as = std::same_as<Ty, Other>;
            template<class Other> constexpr static bool is_base_of = std::is_base_of_v<Ty, Other>;
            template<class Other> constexpr static bool is_convertible_to = std::is_convertible_v<Ty, Other>;
            template<class Other> constexpr static bool is_nothrow_convertible_to = std::is_nothrow_convertible_v<Ty, Other>;
            template<class ...Args> constexpr static bool is_invocable = std::invocable<Ty, Args...>;
            template<class ...Args> constexpr static bool is_nothrow_invocable = std::is_nothrow_invocable_v<Ty, Args...>;
            template<class Other> constexpr static bool can_construct = std::constructible_from<Ty, Other>;
            template<class Other> constexpr static bool can_trivially_construct = std::is_trivially_constructible_v<Ty, Other>;
            template<class Other> constexpr static bool can_nothrow_construct = std::is_nothrow_constructible_v<Ty, Other>;
            template<class Other> constexpr static bool can_invoke = std::invocable<Ty, Other>;
            template<class Other> constexpr static bool can_nothrow_invoke = std::is_nothrow_invocable_v<Ty, Other>;

            constexpr static std::size_t bytes = sizeof_v<Ty>;
            constexpr static std::size_t alignment = alignof_v<Ty>;

            template<class To>
            using copy_const_to = info<kaixo::copy_const<To, Ty>>;
            template<class To>
            using copy_volatile_to = info<kaixo::copy_volatile<To, Ty>>;
            template<class To>
            using copy_cv_to = info<kaixo::copy_cv<To, Ty>>;
            template<class To>
            using copy_ref_to = info<kaixo::copy_ref<To, Ty>>;
            template<class To>
            using copy_cvref_to = info<kaixo::copy_cvref<To, Ty>>;
            template<class To>
            using add_const_to = info<kaixo::add_const<To, Ty>>;
            template<class To>
            using add_volatile_to = info<kaixo::add_volatile<To, Ty>>;
            template<class To>
            using add_cv_to = info<kaixo::add_cv<To, Ty>>;
            template<class To>
            using add_ref_to = info<kaixo::add_ref<To, Ty>>;
            template<class To>
            using add_cvref_to = info<kaixo::add_cvref<To, Ty>>;

            template<class From>
            using copy_const_from = info<kaixo::copy_const<Ty, From>>;
            template<class From>
            using copy_volatile_from = info<kaixo::copy_volatile<Ty, From>>;
            template<class From>
            using copy_cv_from = info<kaixo::copy_cv<Ty, From>>;
            template<class From>
            using copy_ref_from = info<kaixo::copy_ref<Ty, From>>;
            template<class From>
            using copy_cvref_from = info<kaixo::copy_cvref<Ty, From>>;
            template<class From>
            using add_const_from = info<kaixo::add_const<Ty, From>>;
            template<class From>
            using add_volatile_from = info<kaixo::add_volatile<Ty, From>>;
            template<class From>
            using add_cv_from = info<kaixo::add_cv<Ty, From>>;
            template<class From>
            using add_ref_from = info<kaixo::add_ref<Ty, From>>;
            template<class From>
            using add_cvref_from = info<kaixo::add_cvref<Ty, From>>;

            using decay = info<std::decay_t<Ty>>;
            using remove_cv = info<kaixo::remove_cv<Ty>>;
            using remove_const = info<kaixo::remove_const<Ty>>;
            using remove_volatile = info<kaixo::remove_volatile<Ty>>;
            using add_cv = info<kaixo::add_cv<Ty>>;
            using add_const = info<kaixo::add_const<Ty>>;
            using add_volatile = info<kaixo::add_volatile<Ty>>;
            using remove_reference = info<std::remove_reference_t<Ty>>;
            using remove_cvref = info<std::remove_cvref_t<Ty>>;
            using add_lvalue_reference = info<std::add_lvalue_reference_t<Ty>>;
            using add_rvalue_reference = info<std::add_rvalue_reference_t<Ty>>;
            using remove_pointer = info<std::remove_pointer_t<Ty>>;
            using add_pointer = info<std::add_pointer_t<Ty>>;
            using type = Ty;

            constexpr static auto type_info = &typeid(Ty);
            constexpr static auto type_name = kaixo::type_name<Ty>;

            // Convert to function, with return type Other
            template<class Other> using to_function = info<Other(Ty)>;

            // Convert to member pointer, with object type Other
            template<class Other>
            using to_member_pointer = info<std::remove_reference_t<Ty> Other::*>;
        };

        /**
         * Special case for pack_base, contains traits for all types.
         * @tparam Tys... pack types
         */
        template<class ...Tys> requires (sizeof...(Tys) > 0) struct info_base<pack_base<Tys...>> {
            constexpr static bool are_void = (std::is_void_v<Tys> && ...);
            constexpr static bool are_null_pointer = (std::is_null_pointer_v<Tys> && ...);
            constexpr static bool are_integral = (std::is_integral_v<Tys> && ...);
            constexpr static bool are_floating_point = (std::is_floating_point_v<Tys> && ...);
            constexpr static bool are_array = (std::is_array_v<Tys> && ...);
            constexpr static bool are_enum = (std::is_enum_v<Tys> && ...);
            constexpr static bool are_union = (std::is_union_v<Tys> && ...);
            constexpr static bool are_class = (std::is_class_v<Tys> && ...);
            constexpr static bool are_function = (std::is_function_v<Tys> && ...);
            constexpr static bool are_pointer = (std::is_pointer_v<Tys> && ...);
            constexpr static bool are_lvalue_reference = (std::is_lvalue_reference_v<Tys> && ...);
            constexpr static bool are_rvalue_reference = (std::is_rvalue_reference_v<Tys> && ...);
            constexpr static bool are_member_object_pointer = (std::is_member_object_pointer_v<Tys> && ...);
            constexpr static bool are_member_function_pointer = (std::is_member_function_pointer_v<Tys> && ...);
            constexpr static bool are_fundamental = (std::is_fundamental_v<Tys> && ...);
            constexpr static bool are_arithmetic = (std::is_arithmetic_v<Tys> && ...);
            constexpr static bool are_scalar = (std::is_scalar_v<Tys> && ...);
            constexpr static bool are_object = (std::is_object_v<Tys> && ...);
            constexpr static bool are_compound = (std::is_compound_v<Tys> && ...);
            constexpr static bool are_reference = (std::is_reference_v<Tys> && ...);
            constexpr static bool are_member_pointer = (std::is_member_pointer_v<Tys> && ...);
            constexpr static bool are_const = (std::is_const_v<Tys> && ...);
            constexpr static bool are_volatile = (std::is_volatile_v<Tys> && ...);
            constexpr static bool are_trivial = (std::is_trivial_v<Tys> && ...);
            constexpr static bool are_trivially_copyable = (std::is_trivially_copyable_v<Tys> && ...);
            constexpr static bool are_standard_layout = (std::is_standard_layout_v<Tys> && ...);
            constexpr static bool have_unique_object_representations = (std::has_unique_object_representations_v<Tys> && ...);
            constexpr static bool are_empty = (std::is_empty_v<Tys> && ...);
            constexpr static bool are_polymorphic = (std::is_polymorphic_v<Tys> && ...);
            constexpr static bool are_abstract = (std::is_abstract_v<Tys> && ...);
            constexpr static bool are_final = (std::is_final_v<Tys> && ...);
            constexpr static bool are_aggregate = (std::is_aggregate_v<Tys> && ...);
            constexpr static bool are_signed_integral = (std::is_signed_v<Tys> && ...);
            constexpr static bool are_unsigned_integral = (std::is_unsigned_v<Tys> && ...);
            constexpr static bool are_bounded_array = (std::is_bounded_array_v<Tys> && ...);
            constexpr static bool are_unbounded_array = (std::is_unbounded_array_v<Tys> && ...);
            template<class ...Args> constexpr static bool are_constructible = (std::constructible_from<Tys, Args...> && ...);
            template<class ...Args> constexpr static bool are_trivially_constructible = (std::is_trivially_constructible_v<Tys, Args...> && ...);
            template<class ...Args> constexpr static bool are_nothrow_constructible = (std::is_nothrow_constructible_v<Tys, Args...> && ...);
            constexpr static bool are_default_constructible = (std::is_default_constructible_v<Tys> && ...);
            constexpr static bool are_trivially_default_constructible = (std::is_trivially_default_constructible_v<Tys> && ...);
            constexpr static bool are_nothrow_default_constructible = (std::is_nothrow_default_constructible_v<Tys> && ...);
            constexpr static bool are_copy_constructible = (std::is_copy_constructible_v<Tys> && ...);
            constexpr static bool are_trivially_copy_constructible = (std::is_trivially_copy_constructible_v<Tys> && ...);
            constexpr static bool are_nothrow_copy_constructible = (std::is_nothrow_copy_constructible_v<Tys> && ...);
            constexpr static bool are_move_constructible = (std::is_move_constructible_v<Tys> && ...);
            constexpr static bool are_trivially_move_constructible = (std::is_trivially_move_constructible_v<Tys> && ...);
            constexpr static bool are_nothrow_move_constructible = (std::is_nothrow_move_constructible_v<Tys> && ...);
            template<class From> constexpr static bool are_assignable = (std::is_assignable_v<Tys, From> && ...);
            template<class From> constexpr static bool are_trivially_assignable = (std::is_trivially_assignable_v<Tys, From> && ...);
            template<class From> constexpr static bool are_nothrow_assignable = (std::is_nothrow_assignable_v<Tys, From> && ...);
            constexpr static bool are_copy_assignable = (std::is_copy_assignable_v<Tys> && ...);
            constexpr static bool are_trivially_copy_assignable = (std::is_trivially_copy_assignable_v<Tys> && ...);
            constexpr static bool are_nothrow_copy_assignable = (std::is_nothrow_copy_assignable_v<Tys> && ...);
            constexpr static bool are_move_assignable = (std::is_move_assignable_v<Tys> && ...);
            constexpr static bool are_trivially_move_assignable = (std::is_trivially_move_assignable_v<Tys> && ...);
            constexpr static bool are_nothrow_move_assignable = (std::is_nothrow_move_assignable_v<Tys> && ...);
            constexpr static bool are_destructible = (std::is_destructible_v<Tys> && ...);
            constexpr static bool are_trivially_destructible = (std::is_trivially_destructible_v<Tys> && ...);
            constexpr static bool are_nothrow_destructible = (std::is_nothrow_destructible_v<Tys> && ...);
            constexpr static bool have_virtual_destructors = (std::has_virtual_destructor_v<Tys> && ...);
            template<class Other> constexpr static bool are_swappable_with = (std::is_swappable_with_v<Tys, Other> && ...);
            constexpr static bool are_swappable = (std::is_swappable_v<Tys> && ...);
            template<class Other> constexpr static bool are_nothrow_swappable_with = (std::is_nothrow_swappable_with_v<Tys, Other> && ...);
            constexpr static bool are_nothrow_swappable = (std::is_nothrow_swappable_v<Tys> && ...);

            template<class Other> constexpr static bool are_same_as = (std::same_as<Tys, Other> && ...);
            template<class Other> constexpr static bool are_base_of = (std::is_base_of_v<Tys, Other> && ...);
            template<class Other> constexpr static bool are_convertible_to = (std::is_convertible_v<Tys, Other> && ...);
            template<class Other> constexpr static bool are_nothrow_convertible_to = (std::is_nothrow_convertible_v<Tys, Other> && ...);
            template<class ...Args> constexpr static bool are_invocable = (std::invocable<Tys, Args...> && ...);
            template<class ...Args> constexpr static bool are_nothrow_invocable = (std::is_nothrow_invocable_v<Tys, Args...> && ...);
            template<class Ty> constexpr static bool can_construct = std::constructible_from<Ty, Tys...>;
            template<class Ty> constexpr static bool can_trivially_construct = std::is_trivially_constructible_v<Ty, Tys...>;
            template<class Ty> constexpr static bool can_nothrow_construct = std::is_nothrow_constructible_v<Ty, Tys...>;
            template<class Ty> constexpr static bool can_invoke = std::invocable<Ty, Tys...>;
            template<class Ty> constexpr static bool can_nothrow_invoke = std::is_nothrow_invocable_v<Ty, Tys...>;

            constexpr static std::size_t bytes = (sizeof_v<Tys> +...);
            constexpr static std::size_t alignment = std::max({ alignof_v<Tys>... });

            template<class From>
            using copy_const_from = kaixo::pack<kaixo::copy_const<Tys, From>...>;
            template<class From>
            using copy_volatile_from = kaixo::pack<kaixo::copy_volatile<Tys, From>...>;
            template<class From>
            using copy_cv_from = kaixo::pack<kaixo::copy_cv<Tys, From>...>;
            template<class From>
            using copy_ref_from = kaixo::pack<kaixo::copy_ref<Tys, From>...>;
            template<class From>
            using copy_cvref_from = kaixo::pack<kaixo::copy_cvref<Tys, From>...>;
            template<class From>
            using add_const_from = kaixo::pack<kaixo::add_const<Tys, From>...>;
            template<class From>
            using add_volatile_from = kaixo::pack<kaixo::add_volatile<Tys, From>...>;
            template<class From>
            using add_cv_from = kaixo::pack<kaixo::add_cv<Tys, From>...>;
            template<class From>
            using add_ref_from = kaixo::pack<kaixo::add_ref<Tys, From>...>;
            template<class From>
            using add_cvref_from = kaixo::pack<kaixo::add_cvref<Tys, From>...>;

            using decay = kaixo::pack<std::decay_t<Tys>...>;
            using remove_cv = kaixo::pack<kaixo::remove_cv<Tys>...>;
            using remove_const = kaixo::pack<kaixo::remove_const<Tys>...>;
            using remove_volatile = kaixo::pack<kaixo::remove_volatile<Tys>...>;
            using add_cv = kaixo::pack<kaixo::add_cv<Tys>...>;
            using add_const = kaixo::pack<kaixo::add_const<Tys>...>;
            using add_volatile = kaixo::pack<kaixo::add_volatile<Tys>...>;
            using remove_reference = kaixo::pack<std::remove_reference_t<Tys>...>;
            using remove_cvref = kaixo::pack<std::remove_cvref_t<Tys>...>;
            using add_lvalue_reference = kaixo::pack<std::add_lvalue_reference_t<Tys>...>;
            using add_rvalue_reference = kaixo::pack<std::add_rvalue_reference_t<Tys>...>;
            using remove_pointer = kaixo::pack<std::remove_pointer_t<Tys>...>;
            using add_pointer = kaixo::pack<std::add_pointer_t<Tys>...>;
            using type = kaixo::pack<Tys...>;

            constexpr static auto type_info = std::array{ &typeid(Tys)... };
            constexpr static auto type_name = std::array{ kaixo::type_name<Tys>... };

            // Convert pack to function, with return type Ty
            template<class Ty> using to_function = info<Ty(Tys...)>;
            
            // Convert to a pack of member pointers, with object type Ty
            template<class Ty>
            using to_member_pointer = pack<std::remove_reference_t<Tys> Ty::* ...>;
        };

        /**
         * Specialization for callable types, to also
         * contain function info.
         */
        template<callable_type Ty>
        struct info<Ty> : info_base<Ty>, function_info<Ty> {};

        /**
         * Specialization for integral types, to
         * add the make_signed/make_unsigned type traits.
         * Also inherits from std::numeric_limits<Ty>
         */
        template<class Ty>
            requires (type_concepts::integral<Ty> && !type_concepts::boolean<Ty>)
        struct info<Ty> : info_base<Ty>, std::numeric_limits<Ty> {
            using make_signed = info<std::make_signed<Ty>>;
            using make_unsigned = info<std::make_unsigned<Ty>>;
        };

        /**
         * Specialization for floating pointer, inherits
         * from std::numeric_limits<Ty>
         */
        template<class Ty>
            requires (type_concepts::floating_point<Ty>)
        struct info<Ty> : info_base<Ty>, std::numeric_limits<Ty> {};

        /**
         * Specialization for enum types, contains underlying
         * type, and enum names.
         */
        template<class Ty>
            requires (type_concepts::enum_type<Ty>)
        struct info<Ty> : info_base<Ty> {
            using underlying = info<std::underlying_type_t<Ty>>;

            template<auto Value>
            constexpr static auto name = enum_name<Ty, Value>;

            template<auto Value>
            constexpr static auto defined = name<Value>.data() != nullptr;
        };

        /**
         * Specialization for member object pointer,
         * contains the 'object' and 'value_type'
         */
        template<class Ty, class Obj>
            requires type_concepts::member_object_pointer<Ty Obj::*>
        struct info<Ty Obj::*> : info_base<Ty Obj::*> {
            using object = info<Obj>;
            using value_type = info<Ty>;
        };

        /**
         * Specialization for Arrays, 
         * contains rank/extent traits.
         */
        template<class Ty>
            requires (type_concepts::array<Ty>)
        struct info<Ty> : info_base<Ty> {
            constexpr static std::size_t rank = std::rank_v<Ty>;

            template<std::size_t Dim = 0>
            constexpr static std::size_t extent = std::extent_v<Ty, Dim>;

            using remove_extent = info<std::remove_extent_t<Ty>>;
            using remove_all_extents = info<std::remove_all_extents_t<Ty>>;
        };

        /**
         * Special case for value template arguments.
         * actually uses decltype(Ty), but also contains
         * the value name.
         */
        template<auto Ty>
        struct info<value_t<Ty>> : info<decltype(Ty)> {
            constexpr static auto name = kaixo::value_name<Ty>;
        };

        /**
         * Special case for (member) function pointers,
         * also uses decltype(Ty), and contains value name
         * and also function name.
         */
        template<auto Ty>
            requires callable_type<decltype(Ty)>
        struct info<value_t<Ty>> : info<decltype(Ty)> {
            constexpr static auto name = kaixo::value_name<Ty>;
            constexpr static auto function_name = kaixo::function_name<Ty>;
        };

        /**
         * Alias for template values.
         * @tparam V value
         */
        template<auto V>
        using info_v = info<value_t<V>>;

        /**
         * Member pointer info, used in the 'register' macro.
         * Stores the name and pointer, and the info_v<V> type.
         * @tparam V member pointer
         * @tparam Name name
         */
        template<auto V, string_literal Name>
        struct member_info_t {
            using info = info_v<V>;
            constexpr static std::string_view name = Name.view();
            constexpr static auto pointer = V;
        };

        /**
         * Member info. Specializations are made with
         * the 'register' macro.
         * @tparam Ty struct type
         * @tparam Index member index in Ty
         */
        template<class Ty, std::size_t Index>
        constexpr auto member_info = member_info_t<0, "">{};
        
        /**
         * Member info, but specialized on the member pointer.
         * Allows looking for name using the member pointer.
         * @tparam MemPtr member pointer
         */
        template<auto MemPtr>
        constexpr auto member_info_ptr = member_info_t<0, "">{};

        template<class Ty, std::size_t S>
        constexpr std::size_t _member_start = S;

#define MEMBR_IMPL(x, y, c) /* x: Struct, y: Member name, c: __COUNTER__ value */         \
template<> /* This keeps track of first __COUNTER__ val per struct, so we know offset */  \
constexpr std::size_t kaixo::_member_start<x, c> = kaixo::_member_start<x, c - 1>;        \
template<> /* Member info on index, uses _member_start to know offset. */                 \
constexpr auto kaixo::member_info<x, c - kaixo::_member_start<x, c> - 1> =                \
         kaixo::member_info_t<&x::y, #y>{};                                               \
template<> /* Member info on member pointer */                                            \
constexpr auto kaixo::member_info_ptr<&x::y> = kaixo::member_info_t<&x::y, #y>{} 
#define register(x, y) MEMBR_IMPL(x, y, __COUNTER__)

        /**
         * Special case for member object pointer value.
         * Contains the member_info, so if 'register' macro 
         * was used, it contains the member name.
         */
        template<auto Ty>
            requires type_concepts::member_object_pointer<decltype(Ty)>
        struct info<value_t<Ty>> : info<decltype(Ty)>, decltype(kaixo::member_info_ptr<Ty>) {};
        
        /**
         * Specialization for structs, contains information on
         * members, like offset, types, and if 'register' macro was
         * used, also member names and pointers. Also contains 
         * non-constexpr member pointers, made using std::bit_cast
         * and the calculated offset of each member.
         */
        template<class Ty>
            requires (type_concepts::aggregate<Ty>)
        struct info<Ty> : info_base<Ty> {
            using members = struct_members<Ty>;

            template<std::size_t I>
            constexpr static std::size_t offset = [] {
                if constexpr (I == 0) return 0;
                else if constexpr (I != 0) {
                    // Wrapped in lambda because somehow it gets infinite
                    // recursion without, even though there's an if-constexpr
                    return[]<std::size_t N>(value_t<N>) {
                        using prev = typename members::template element<N - 1>;
                        using curr = typename members::template element<N>;

                        constexpr std::size_t off = offset<N - 1> +sizeof_v<prev>;
                        constexpr std::size_t align = alignof_v<curr>;

                        return next_multiple(off, align);
                    }(value_t<I>{});
                }
            }();

            template<std::size_t I> // Only works when you 'register' a member
            using member_info = decltype(kaixo::member_info<Ty, I>);

            // Only works when you 'register' members
            using member_ptrs = decltype(sequence<struct_size<Ty>>([]<std::size_t ...Is> {
                return to_pack<member_info<Is>::pointer...>{};
            }));

            template<std::size_t I>
            constexpr static auto member_name = member_info<Ty, I>.name;

            template<std::size_t I>
            const inline static auto member_ptr = _member_impl<I>();

            template<std::size_t I>
            constexpr static auto _member_impl() {
                return std::bit_cast<typename members::template element<I> Ty::*>(
                    static_cast<std::uint32_t>(offset<I>));
            }
        };
    }

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                           *
     *                        Switch Utils                       *
     *                                                           *
     *            Macro to help define custom length             *
     *         switch statements with custom case bodies         *
     *                                                           *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    inline namespace switch_utils_n {
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

#define KAIXO_CASES_SWITCH_C(i) case transform(i):          \
    if constexpr (i < sizeof...(Args)) {                    \
        if constexpr (std::invocable<                       \
            decltype(std::get<i>(cases)), decltype(index)>) \
            return std::get<i>(cases)(index);               \
        else return std::get<i>(cases)();                   \
    } else break; 

#define KAIXO_CASES_SWITCH_S(n, cs)                                  \
template<>                                                           \
struct cases_switch_impl<n> {                                        \
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
        template<auto transform = functional::unit>
        constexpr auto generate_switch = []<class ...Functors>(Functors&& ...cases) {
            constexpr auto p2 = closest_larger_power2(sizeof...(Functors));
            return tuple_switch_impl<p2>::template handle<transform>(std::forward<Functors>(cases)...);
        };

        /**
         * Templated switch statement, where the
         * argument will be converted into a template parameter value.
         */
        template<std::size_t I>
        struct template_switch_impl;

#define KAIXO_TEMPLATE_SWITCH_C(i) case transform(i):  \
    if constexpr (i < cases) {                         \
        return functor.operator()<transform(i)>();     \
    } else break; 

#define KAIXO_TEMPLATE_SWITCH_S(n, cs)                               \
template<>                                                           \
struct template_switch_impl<n> {                                     \
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
        template<std::unsigned_integral auto cases, auto transform = functional::unit>
        constexpr auto generate_template_switch = []<class Arg>(Arg && functor) {
            constexpr auto p2 = closest_larger_power2(cases);
            return template_switch_impl<p2>::template handle<cases, transform>(std::forward<Arg>(functor));
        };

        /**
         * Convert enum value to string, either requires
         * that the enum contains a 'Size' member, or you
         * manually choose a size. Generates a switch statement
         * with cases from 0 to Size.
         * @tparam Ty enum type
         * @tparam Size amount of cases
         * @param val enum value
         * @return enum value as string view to a string literal
         */ 
        template<type_concepts::enum_type Ty,
            std::size_t Size = static_cast<std::size_t>(Ty::Size)>
        constexpr auto enum_to_string(Ty val) {
            // Generates a template switch to convert the runtime
            // argument into a compiletime template value
            // which then gets the name.
            constexpr auto _str = generate_template_switch<
                Size, functional::cast<Ty>>([]<Ty V> { 
                return info<Ty>::template name<V>; 
            });
            return _str(val);
        }
    }

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                           *
     *                   Multi Initializer List                  *
     *                                                           *
     *               Initializer list that supports              *
     *                       multiple types                      *
     *                                                           *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    inline namespace multi_initializer_list_n {
        /**
         * Basically a wrapper for a dynamic raw array, can't 
         * use unique_ptr because it aint constexpr yet...
         */
        template<class Ty>
        struct initializer_storage {
            Ty* _storage = nullptr;
            std::size_t _size = 0;

            constexpr initializer_storage() {}
            constexpr initializer_storage(Ty* storage, std::size_t size)
                : _storage(storage), _size(size) {}
            constexpr initializer_storage(const initializer_storage&) = delete;
            constexpr initializer_storage(initializer_storage&& other) noexcept
                : _storage(other._storage), _size(other._size) {
                other._storage = nullptr, other._size = 0;
            }

            constexpr ~initializer_storage() { delete[] _storage; }

            constexpr const Ty* begin() const { return _storage; }
            constexpr const Ty* end() const { return _storage + _size; }
        };

        /**
         * An initializer list that supports multiple types.
         * Get all values of a single type by calling get<Type>()
         */
        template<class ...Tys>
            requires std::same_as<kaixo::pack<Tys...>,
            typename kaixo::pack<Tys...>::unique> // Make sure only unique types
        struct multi_initializer_list {
            using types = kaixo::pack<Tys...>;
            std::tuple<initializer_storage<Tys>...> _lists;

            /**
             * Get all values of some type.
             * @tparam Ty type in Tys...
             */
            template<class Ty, class Self>
                requires kaixo::pack<Tys...>::template occurs<Ty>
            constexpr const initializer_storage<Ty>& get(this Self&& self) {
                return std::get<types::template index<Ty>>(self._lists);
            }

            template<class ...Args>
            constexpr multi_initializer_list(Args&&...args)
                : multi_initializer_list{ std::forward_as_tuple(std::forward<Args>(args)...) } {}

        private:
            template<class ...Args>
            constexpr multi_initializer_list(std::tuple<Args&&...>&& args) : _lists{
                [&]() -> initializer_storage<Tys> { // Pack expansion over Tys
                    using args_pack = kaixo::pack<Args...>;
                    using value_type = Tys;

                    constexpr auto indices = args_pack::decay::template indices<value_type>;

                    const auto l = [&]<std::size_t I>(kaixo::value_t<I>) {
                        using type = args_pack::template element<I>;
                        return std::forward<type>(std::get<I>(args));
                    };

                    if constexpr (indices.size() == 0) return {};
                    else return kaixo::iterate<indices>([&]<std::size_t... Is>() {
                        constexpr std::size_t size = sizeof...(Is);
                        return initializer_storage<value_type>{
                            new value_type[size]{ l(kaixo::value_t<Is>{})... }, size
                        };
                    });
                }()... } {}
        };
    }
}