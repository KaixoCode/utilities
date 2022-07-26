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

    template<class, template<class...> class>
    struct specialization_impl : std::false_type {};
    template<template<class...> class Ref, class... Args>
    struct specialization_impl<Ref<Args...>, Ref> : std::true_type {};

    /**
     * Concept, Test is specialization of Ref
     * @tparam Test type to test
     * @tparam Ref templated type
     */
    template<class Test, template<class...> class Ref>
    concept specialization = specialization_impl<std::decay_t<Test>, Ref>::value;

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

    template<class ...Tys> struct info;

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

    /**
     * All standard type traits as concepts.
     */
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

    template<class Ty, class ...Args> concept constructible = pack_trait_helper<std::is_constructible, Ty, Args...>::value;
    template<class Ty, class ...Args> concept trivially_constructible = pack_trait_helper<std::is_trivially_constructible, Ty, Args...>::value;
    template<class Ty, class ...Args> concept nothrow_constructible = pack_trait_helper<std::is_nothrow_constructible, Ty, Args...>::value;
    template<class Ty, class ...Args> concept invocable = pack_trait_helper<std::is_invocable, Ty, Args...>::value;
    template<class Ty, class ...Args> concept nothrow_invocable = pack_trait_helper<std::is_nothrow_invocable, Ty, Args...>::value;

    /**
     * All standard type traits wrapped in an object, allows for
     * simple boolean operations and partial application.
     * Even allows for complex concept constraints like:
     * template<require<is_integral || is_floating_point> Ty>
     */

     /**
      * Concept to match a type_trait.
      * @tparam Ty type
      * @tparam V type_trait value
      */
    template<class Ty, auto V>
    concept require = V.template value<Ty>;

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

    /**
     * Check if Ty is a type trait object.
     * @tparam Ty type
     */
    template<class Ty>
    concept is_type_trait = std::convertible_to<decltype(Ty::template value<int>), bool>;

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
    constexpr auto is_unique_object_representations = type_trait<std::has_unique_object_representations>{};
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
    template<class From> constexpr auto is_assignable = type_trait<typename type_trait_partial_last<std::is_assignable, From>::type>{};
    template<class From> constexpr auto is_trivially_assignable = type_trait<typename type_trait_partial_last<std::is_trivially_assignable, From>::type>{};
    template<class From> constexpr auto is_nothrow_assignable = type_trait<typename type_trait_partial_last<std::is_nothrow_assignable, From>::type>{};
    template<class To> constexpr auto is_assignable_to = type_trait<typename type_trait_partial_first<std::is_assignable, To>::type>{};
    template<class To> constexpr auto is_trivially_assignable_to = type_trait<typename type_trait_partial_first<std::is_trivially_assignable, To>::type>{};
    template<class To> constexpr auto is_nothrow_assignable_to = type_trait<typename type_trait_partial_first<std::is_nothrow_assignable, To>::type>{};
    constexpr auto is_copy_assignable = type_trait<std::is_copy_assignable>{};
    constexpr auto is_trivially_copy_assignable = type_trait<std::is_trivially_copy_assignable>{};
    constexpr auto is_nothrow_copy_assignable = type_trait<std::is_nothrow_copy_assignable>{};
    constexpr auto is_move_assignable = type_trait<std::is_move_assignable>{};
    constexpr auto is_trivially_move_assignable = type_trait<std::is_trivially_move_assignable>{};
    constexpr auto is_nothrow_move_assignable = type_trait<std::is_nothrow_move_assignable>{};
    constexpr auto is_destructible = type_trait<std::is_destructible>{};
    constexpr auto is_trivially_destructible = type_trait<std::is_trivially_destructible>{};
    constexpr auto is_nothrow_destructible = type_trait<std::is_nothrow_destructible>{};
    constexpr auto has_virtual_destructor = type_trait<std::has_virtual_destructor>{};
    template<class Other> constexpr auto is_swappable_with = type_trait<typename type_trait_partial_last<std::is_swappable_with, Other>::type>{};
    constexpr auto is_swappable = type_trait<std::is_swappable>{};
    template<class Other> constexpr auto is_nothrow_swappable_with = type_trait<typename type_trait_partial_last<std::is_nothrow_swappable_with, Other>::type>{};
    constexpr auto is_nothrow_swappable = type_trait<std::is_nothrow_swappable>{};

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
    struct element<I, info<Args...>> : element<I, Args...> {};

    /**
     * Get the I'th type in ...Args.
     * @tparam I index
     * @tparam ...Args types
     */
    template<std::size_t I, class ...Args>
    using element_t = typename element<I, Args...>::type;

    template<class Ty, class ...Args>
    struct index : std::integral_constant < std::size_t, [] {
        std::size_t index = 0;
        ((++index, std::is_same_v<Ty, Args>) || ...);
        return index - 1;
    }() > {};

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
    struct last_index : std::integral_constant < std::size_t, [] {
        std::size_t _fromEnd = 0; // increment, but reset on match
        ((std::is_same_v<Ty, Args> ? _fromEnd = 0 : ++_fromEnd), ...);
        std::size_t _index = sizeof...(Args) - _fromEnd - 1;
        return _fromEnd == sizeof...(Args) ? npos : _index;
    }() > {};

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
    struct unique_count : std::integral_constant < std::size_t, [] {
        std::size_t _index = 0, _match = 0;
        ((_match += index_v<Args, Args...> == _index++), ...);
        return _match;
    }() > {};

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

    template<auto, class> struct keep_at_indices;
    template<auto Array, template<class...> class T, class ...As>
    struct keep_at_indices<Array, T<As...>> {
        template<class> struct helper;
        template<std::size_t ...Is>
        struct helper<std::index_sequence<Is...>> {
            using type = T<element_t<Array[Is], As...>...>;
        };
        using type = typename helper<std::make_index_sequence<Array.size()>>::type;
    };

    /**
     * Only keep the types at the indices in Array.
     * @tparam Array std::array of indices
     * @tparam T templated type
     */
    template<auto Array, class T>
    using keep_at_indices_t = typename keep_at_indices<Array, T>::type;

    template<class, class> struct remove;
    template<class Ty, template<class...> class T, class ...As>
    struct remove<Ty, T<As...>> {
        using type = typename keep_at_indices<
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
        using type = typename keep_at_indices<
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
        using type = keep_at_indices_t<
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

    /**
     * Filter object with operator overloads for several cases
     * for the type filter.
     * @tparam L filter type
     */
    template<class L>
    struct filter_object : L {
        // Operator can't be instantiated with Ty
        template<class Ty> requires (!requires (L l) {
            { l.template operator() < Ty > () };
        }) consteval bool operator()() {
            // if it's a type trait object instead of a lambda
            if constexpr (requires () { L::template value<Ty>; }) return L::template value<Ty>;
            else return false; // Otherwise always return false
        }
        // Operator can be instantiated and returns a bool
        template<class Ty> requires (requires (L l) {
            { l.template operator() < Ty > () } -> std::same_as<bool>;
        }) consteval bool operator()() { // Call operator
            return L::template operator() < Ty > ();
        }
        // Operator can be instantiated but returns void
        // This means concept constraint on template parameter.
        // if it matches, return true
        template<class Ty> requires requires (L l) {
            { l.template operator() < Ty > () } -> std::same_as<void>;
        } consteval bool operator()() { return true; }
    };

    template<auto Filter, class ...Args>
    struct count_filter : std::integral_constant < std::size_t, [] {
        return ((static_cast<std::size_t>(filter_object{ Filter }.operator() < Args > ())) + ...);
    }() > {};

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
            ((filter_object{ Filter }.operator() < Args > () ?
                _indices[_match++] = _index++ : ++_index), ...);
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
            using type = T<element<indices_filter_v<Filter, Args...>[Is], Args...>...>;
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
    using pointer = info<R(*)(Args...) NOEXCEPT>;                                        \
    using signature = info<R(Args...) CONST VOLATILE REF NOEXCEPT>;                      \
    using object = info<CONST VOLATILE Ty REF>;                                          \
    using result = info<R>;                                                              \
    using arguments = info<Args...>;                                                     \
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
    using arguments = info<Args...>;                                                    \
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
    using arguments = info<Args...>;                                                        \
    constexpr static bool is_noexcept = std::same_as<void() noexcept, void() NOEXCEPT>;     \
    using add_noexcept = info<R(Args...) noexcept>;                                         \
    using remove_noexcept = info<R(Args...)>;                                               \
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
                return constructible<Ty, change<value_t<Is>, convertible_type>...>;
            };

            ((sequence<Ns>(try_one) ? (res = Ns, true) : false) || ...);
            return res;
        });
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
        using type = info<>;
    };

    template<aggregate Ty>
    struct struct_members<Ty, 0> {
        using type = info<>;
    };

#define KAIXO_STRUCT_MEMBERS_M(c, ...)                                         \
        template<aggregate Ty>                                                 \
            requires (struct_size_v<Ty> == c)                                  \
        struct struct_members<Ty, c> {                                         \
            using types = decltype([](Ty& ty) {                                \
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

    template<class From, class To>
    struct add_ref {
        using _lvalue = std::conditional_t<std::is_lvalue_reference_v<From>, std::add_lvalue_reference_t<To>, To>;
        using _rvalue = std::conditional_t<std::is_rvalue_reference_v<From>, std::add_rvalue_reference_t<_lvalue>, _lvalue>;
        using type = typename _rvalue;
    };

    template<class From, class To>
    using copy_ref = add_ref<From, std::decay_t<To>>;

    template<class From, class To>
    struct add_const {
        using _unrefFrom = std::remove_reference_t<From>;
        using _unrefTo = std::remove_reference_t<To>;
        using _const = std::conditional_t<std::is_const_v<_unrefFrom>, typename add_ref<To, std::add_const_t<_unrefTo>>::type, To>;
        using type = _const;
    };

    template<class From, class To>
    struct add_volatile {
        using _unrefFrom = std::remove_reference_t<From>;
        using _unrefTo = std::remove_reference_t<To>;
        using _volatile = std::conditional_t<std::is_volatile_v<_unrefFrom>, typename add_ref<To, std::add_volatile_t<To>>::type, To>;
        using type = _volatile;
    };

    template<class From, class To>
    struct add_cv {
        using type = typename add_volatile<From, typename add_const<From, To>::type>::type;
    };

    template<class From, class To>
    using copy_const = add_ref<To, typename add_const<From, std::remove_const_t<std::remove_reference_t<To>>>::type>;
    template<class From, class To>
    using copy_volatile = add_ref<To, typename add_volatile<From, std::remove_volatile_t<std::remove_reference_t<To>>>::type>;
    template<class From, class To>
    using copy_cv = add_ref<To, typename add_cv<From, std::remove_cv_t<std::remove_reference_t<To>>>::type>;

    template<class From, class To>
    struct add_cvref {
        using _cv = typename add_cv<From, To>::type;
        using _lvalue = std::conditional_t<std::is_lvalue_reference_v<From>, std::add_lvalue_reference_t<_cv>, _cv>;
        using _rvalue = std::conditional_t<std::is_rvalue_reference_v<From>, std::add_rvalue_reference_t<_lvalue>, _lvalue>;
        using type = typename _rvalue;
    };

    template<class From, class To>
    using copy_cvref = add_cvref<From, std::decay_t<To>>;

    template<class To, class From>
    using copy_const_t = typename copy_const<From, To>::type;
    template<class To, class From>
    using copy_volatile_t = typename copy_volatile<From, To>::type;
    template<class To, class From>
    using copy_cv_t = typename copy_cv<From, To>::type;
    template<class To, class From>
    using copy_ref_t = typename copy_ref<From, To>::type;
    template<class To, class From>
    using copy_cvref_t = typename copy_cvref<From, To>::type;
    template<class To, class From = const int>
    using add_const_t = typename add_const<From, To>::type;
    template<class To, class From = volatile int>
    using add_volatile_t = typename add_volatile<From, To>::type;
    template<class To, class From = const volatile int>
    using add_cv_t = typename add_cv<From, To>::type;
    template<class To, class From>
    using add_ref_t = typename add_ref<From, To>::type;
    template<class To, class From>
    using add_cvref_t = typename add_cvref<From, To>::type;

    template<class To>
    using remove_const_t = typename copy_const<int, To>::type;
    template<class To>
    using remove_volatile_t = typename copy_volatile<int, To>::type;
    template<class To>
    using remove_cv_t = typename copy_cv<int, To>::type;

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     *                                           type info object.                                             *
     *                                                                                                         *
     *                                                                                                         *
     *                                                                                                         *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    template<class Ty>
    struct specialized_info {};

    /**
     * Specialization for callable types, to also
     * contain function info.
     */
    template<callable_type Ty>
    struct specialized_info<Ty> : function_info<Ty> {};

    /**
     * Specialization for integral types, to
     * add the make_signed/make_unsigned type traits.
     * Also inherits from std::numeric_limits<Ty>
     */
    template<class Ty>
        requires (integral<Ty> && !boolean<Ty>)
    struct specialized_info<Ty> : std::numeric_limits<Ty> {
        using make_signed = info<std::make_signed<Ty>>;
        using make_unsigned = info<std::make_unsigned<Ty>>;
    };

    /**
     * Specialization for floating pointer, inherits
     * from std::numeric_limits<Ty>
     */
    template<class Ty>
        requires (floating_point<Ty>)
    struct specialized_info<Ty> : std::numeric_limits<Ty> {};

    /**
     * Specialization for enum types, contains underlying
     * type, and enum names.
     */
    template<class Ty>
        requires (enum_type<Ty>)
    struct specialized_info<Ty> {
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
        requires member_object_pointer<Ty Obj::*>
    struct specialized_info<Ty Obj::*> {
        using object = info<Obj>;
        using value_type = info<Ty>;
    };

    /**
     * Specialization for Arrays,
     * contains rank/extent traits.
     */
    template<class Ty>
        requires (array<Ty>)
    struct specialized_info<Ty> {
        constexpr static std::size_t rank = std::rank_v<Ty>;

        template<std::size_t Dim = 0>
        constexpr static std::size_t extent = std::extent_v<Ty, Dim>;

        using remove_extent = info<std::remove_extent_t<Ty>>;
        using remove_all_extents = info<std::remove_all_extents_t<Ty>>;
    };

    /**
     * Specialization for structs, contains information on
     * members, like offset, types, and if 'register' macro was
     * used, also member names and pointers. Also contains
     * non-constexpr member pointers, made using std::bit_cast
     * and the calculated offset of each member.
     */
    template<class Ty>
        requires (aggregate<Ty>)
    struct specialized_info<Ty> {
        using members = struct_members_t<Ty>;

        template<std::size_t I>
        constexpr static std::size_t offset = [] {
            if constexpr (I == 0) return 0;
            else if constexpr (I != 0) {
                // Wrapped in lambda because somehow it gets infinite
                // recursion without, even though there's an if-constexpr
                return[]<std::size_t N>(value_t<N>) {
                    using prev = typename members::template element<N - 1>::type;
                    using curr = typename members::template element<N>::type;

                    constexpr std::size_t off = offset<N - 1> +sizeof_v<prev>;
                    constexpr std::size_t align = alignof_v<curr>;

                    return next_multiple(off, align);
                }(value_t<I>{});
            }
        }();

        template<std::size_t I>
        const inline static auto member_ptr = _member_impl<I>();

        template<std::size_t I>
        constexpr static auto _member_impl() {
            return std::bit_cast<typename members::template element<I> Ty::*>(
                static_cast<std::uint32_t>(offset<I>));
        }
    };

    /**
     * Base for a pack of types.
     * @tparam ...Tys types
     */
    template<class ...Tys>
    struct info_base {
        using type = info<Tys...>;
    };

    /**
     * Base for a single type.
     * @tparam Ty type
     */
    template<class Ty>
    struct info_base<Ty> : specialized_info<Ty> {
        using type = Ty;

        constexpr static std::size_t alignment = alignof_v<Ty>;
    };

    template<class ...Tys>
    struct info : info_base<Tys...> {
        constexpr static auto size = sizeof...(Tys);
        constexpr static auto unique_size = unique_count_v<Tys...>;
        constexpr static auto bytes = (sizeof_v<Tys> +... + 0);

        template<class T> constexpr static auto index = index_v<T, Tys...>;
        template<class T> constexpr static auto last_index = last_index_v<T, Tys...>;
        template<class T> constexpr static auto count = count_v<T, Tys...>;
        template<class T> constexpr static auto occurs = occurs_v<T, Tys...>;
        template<class T> constexpr static auto indices = indices_v<T, Tys...>;
        template<class T> constexpr static auto indices_except = indices_except_v<T, Tys...>;

        template<auto Filter> constexpr static auto indices_filter = indices_filter_v<Filter, Tys...>;

        template<std::size_t I> using element = info<element_t<I, Tys...>>;
        template<std::size_t I> using take = take_t<I, info>;
        template<std::size_t I> using last = drop_t<size - I, info>;
        template<std::size_t I> using drop = drop_t<I, info>;
        template<std::size_t I> using erase = erase_t<I, info>;

        template<std::size_t I, class T> using insert = insert_t<I, T, info>;
        template<std::size_t I, class T> using swap = erase<I>::template insert<I, T>;

        template<std::size_t A, std::size_t B> using sub = sub_t<A, B, info>;

        template<class T> using remove = remove_t<T, info>;
        template<class T> using keep = keep_t<T, info>;

        template<class T> using append = append_t<T, info>;
        template<class T> using prepend = prepend_t<T, info>;

        using unique = unique_t<info>;
        using reverse = reverse_t<info>;

        template<auto Filter> using filter = filter_t<Filter, info>;

        constexpr static bool is_void = (std::is_void_v<Tys> && ...);
        constexpr static bool is_null_pointer = (std::is_null_pointer_v<Tys> && ...);
        constexpr static bool is_integral = (std::is_integral_v<Tys> && ...);
        constexpr static bool is_floating_point = (std::is_floating_point_v<Tys> && ...);
        constexpr static bool is_array = (std::is_array_v<Tys> && ...);
        constexpr static bool is_enum = (std::is_enum_v<Tys> && ...);
        constexpr static bool is_union = (std::is_union_v<Tys> && ...);
        constexpr static bool is_class = (std::is_class_v<Tys> && ...);
        constexpr static bool is_function = (std::is_function_v<Tys> && ...);
        constexpr static bool is_pointer = (std::is_pointer_v<Tys> && ...);
        constexpr static bool is_lvalue_reference = (std::is_lvalue_reference_v<Tys> && ...);
        constexpr static bool is_rvalue_reference = (std::is_rvalue_reference_v<Tys> && ...);
        constexpr static bool is_member_object_pointer = (std::is_member_object_pointer_v<Tys> && ...);
        constexpr static bool is_member_function_pointer = (std::is_member_function_pointer_v<Tys> && ...);
        constexpr static bool is_fundamental = (std::is_fundamental_v<Tys> && ...);
        constexpr static bool is_arithmetic = (std::is_arithmetic_v<Tys> && ...);
        constexpr static bool is_scalar = (std::is_scalar_v<Tys> && ...);
        constexpr static bool is_object = (std::is_object_v<Tys> && ...);
        constexpr static bool is_compound = (std::is_compound_v<Tys> && ...);
        constexpr static bool is_reference = (std::is_reference_v<Tys> && ...);
        constexpr static bool is_member_pointer = (std::is_member_pointer_v<Tys> && ...);
        constexpr static bool is_const = (std::is_const_v<Tys> && ...);
        constexpr static bool is_volatile = (std::is_volatile_v<Tys> && ...);
        constexpr static bool is_trivial = (std::is_trivial_v<Tys> && ...);
        constexpr static bool is_trivially_copyable = (std::is_trivially_copyable_v<Tys> && ...);
        constexpr static bool is_standard_layout = (std::is_standard_layout_v<Tys> && ...);
        constexpr static bool is_empty = (std::is_empty_v<Tys> && ...);
        constexpr static bool is_polymorphic = (std::is_polymorphic_v<Tys> && ...);
        constexpr static bool is_abstract = (std::is_abstract_v<Tys> && ...);
        constexpr static bool is_final = (std::is_final_v<Tys> && ...);
        constexpr static bool is_aggregate = (std::is_aggregate_v<Tys> && ...);
        constexpr static bool is_signed_integral = (std::is_signed_v<Tys> && ...);
        constexpr static bool is_unsigned_integral = (std::is_unsigned_v<Tys> && ...);
        constexpr static bool is_bounded_array = (std::is_bounded_array_v<Tys> && ...);
        constexpr static bool is_unbounded_array = (std::is_unbounded_array_v<Tys> && ...);
        constexpr static bool is_default_constructible = (std::is_default_constructible_v<Tys> && ...);
        constexpr static bool is_trivially_default_constructible = (std::is_trivially_default_constructible_v<Tys> && ...);
        constexpr static bool is_nothrow_default_constructible = (std::is_nothrow_default_constructible_v<Tys> && ...);
        constexpr static bool is_copy_constructible = (std::is_copy_constructible_v<Tys> && ...);
        constexpr static bool is_trivially_copy_constructible = (std::is_trivially_copy_constructible_v<Tys> && ...);
        constexpr static bool is_nothrow_copy_constructible = (std::is_nothrow_copy_constructible_v<Tys> && ...);
        constexpr static bool is_move_constructible = (std::is_move_constructible_v<Tys> && ...);
        constexpr static bool is_trivially_move_constructible = (std::is_trivially_move_constructible_v<Tys> && ...);
        constexpr static bool is_nothrow_move_constructible = (std::is_nothrow_move_constructible_v<Tys> && ...);
        constexpr static bool is_copy_assignable = (std::is_copy_assignable_v<Tys> && ...);
        constexpr static bool is_trivially_copy_assignable = (std::is_trivially_copy_assignable_v<Tys> && ...);
        constexpr static bool is_nothrow_copy_assignable = (std::is_nothrow_copy_assignable_v<Tys> && ...);
        constexpr static bool is_move_assignable = (std::is_move_assignable_v<Tys> && ...);
        constexpr static bool is_trivially_move_assignable = (std::is_trivially_move_assignable_v<Tys> && ...);
        constexpr static bool is_nothrow_move_assignable = (std::is_nothrow_move_assignable_v<Tys> && ...);
        constexpr static bool is_destructible = (std::is_destructible_v<Tys> && ...);
        constexpr static bool is_trivially_destructible = (std::is_trivially_destructible_v<Tys> && ...);
        constexpr static bool is_nothrow_destructible = (std::is_nothrow_destructible_v<Tys> && ...);
        constexpr static bool is_swappable = (std::is_swappable_v<Tys> && ...);
        constexpr static bool is_nothrow_swappable = (std::is_nothrow_swappable_v<Tys> && ...);

        constexpr static bool has_unique_object_representations = (std::has_unique_object_representations_v<Tys> && ...);
        constexpr static bool has_virtual_destructors = (std::has_virtual_destructor_v<Tys> && ...);

        template<class Other> constexpr static bool is_assignable = (std::is_assignable_v<Tys, Other> && ...);
        template<class Other> constexpr static bool is_trivially_assignable = (std::is_trivially_assignable_v<Tys, Other> && ...);
        template<class Other> constexpr static bool is_nothrow_assignable = (std::is_nothrow_assignable_v<Tys, Other> && ...);
        template<class Other> constexpr static bool is_swappable_with = (std::is_swappable_with_v<Tys, Other> && ...);
        template<class Other> constexpr static bool is_nothrow_swappable_with = (std::is_nothrow_swappable_with_v<Tys, Other> && ...);

        template<class Other> constexpr static bool is_same = (std::is_same_v<Tys, Other> && ...);
        template<class Other> constexpr static bool is_base_of = (std::is_base_of_v<Tys, Other> && ...);
        template<class Other> constexpr static bool is_convertible_to = (std::is_convertible_v<Tys, Other> && ...);
        template<class Other> constexpr static bool is_nothrow_convertible_to = (std::is_nothrow_convertible_v<Tys, Other> && ...);

        template<class ...Args> constexpr static bool is_constructible = (std::is_constructible_v<Tys, Args...> && ...);
        template<class ...Args> constexpr static bool is_trivially_constructible = (std::is_trivially_constructible_v<Tys, Args...> && ...);
        template<class ...Args> constexpr static bool is_nothrow_constructible = (std::is_nothrow_constructible_v<Tys, Args...> && ...);
        template<class ...Args> constexpr static bool is_invocable = (std::is_invocable_v<Tys, Args...> && ...);
        template<class ...Args> constexpr static bool is_nothrow_invocable = (std::is_nothrow_invocable_v<Tys, Args...> && ...);

        template<class Ty> constexpr static bool can_construct = std::is_constructible_v<Ty, Tys...>;
        template<class Ty> constexpr static bool can_trivially_construct = std::is_trivially_constructible_v<Ty, Tys...>;
        template<class Ty> constexpr static bool can_nothrow_construct = std::is_nothrow_constructible_v<Ty, Tys...>;
        template<class Ty> constexpr static bool can_invoke = std::is_invocable_v<Ty, Tys...>;
        template<class Ty> constexpr static bool can_nothrow_invoke = std::is_nothrow_invocable_v<Ty, Tys...>;

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

        using decay = info<std::decay_t<Tys>...>;
        using remove_cv = info<kaixo::remove_cv_t<Tys>...>;
        using remove_const = info<kaixo::remove_const_t<Tys>...>;
        using remove_volatile = info<kaixo::remove_volatile_t<Tys>...>;
        using add_cv = info<kaixo::add_cv_t<Tys>...>;
        using add_const = info<kaixo::add_const_t<Tys>...>;
        using add_volatile = info<kaixo::add_volatile_t<Tys>...>;
        using remove_reference = info<std::remove_reference_t<Tys>...>;
        using remove_cvref = info<std::remove_cvref_t<Tys>...>;
        using add_lvalue_reference = info<std::add_lvalue_reference_t<Tys>...>;
        using add_rvalue_reference = info<std::add_rvalue_reference_t<Tys>...>;
        using remove_pointer = info<std::remove_pointer_t<Tys>...>;
        using add_pointer = info<std::add_pointer_t<Tys>...>;

        template<class Ty> using to_function = info<Ty(Tys...)>;
        template<class Ty> using to_member_pointer = info<std::remove_reference_t<Tys> Ty::* ...>;
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
        static_assert(_small_info::bytes == (sizeof(int) + sizeof(double) + sizeof(float)));
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