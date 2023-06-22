#pragma once
#include <any>
#include <variant>
#include "kaixo/type_utils.hpp" // Contains helpers for getting lambda argument types

/**
 * example:
 * 
 * std::any v = 2;
 * l{ 1, v } >> match {
 *     l{ 2, 3 } = 4,                       // value
 *     l{ 1, 4 } = [] { ... },              // lambda body
 *     l{ 1, 4 } = [](int, int) { ... },    // lambda with arguments
 *     otherwise = [] { ... }               // otherwise, always matches
 * };
 * 
 * The result type of each match case has to be the same, so the value type
 * and the return type of the lambdas of all the match cases have to be 
 * the same type.
 * 
 * In the case of value or lambda with no arguments, if there's no
 * equality operator defined between the inputs and the match case types, 
 * it will try casting using 'general_cast' (see down below), but not 
 * before making sure the stored type is the same using overloaded 
 * 'type' function (also down below).
 * 
 * In the case of the lambda with arguments, it will always try casting
 * the inputs to the provided argument types in the lambda, and then
 * try to equality compare those casted values to the match case values.
 */

namespace kaixo {

    // Get actual stored type, overloaded on things like std::any and 
    // std::variant to get internally stored type at runtime.
    template<class Ty>
    const std::type_info& type(Ty&&) { return typeid(Ty); }
    const std::type_info& type(std::any& v) { return v.type(); }
    const std::type_info& type(const std::any& v) { return v.type(); }
    template<class ...Args>
    const std::type_info& type(std::variant<Args...>& v) {
        constinit static const std::type_info* _infos[sizeof...(Args)]{ &typeid(Args)... };
        return *_infos[v.index()];
    }
    template<class ...Args>
    const std::type_info& type(const std::variant<Args...>& v) {
        constinit static const std::type_info* _infos[sizeof...(Args)]{ &typeid(Args)... };
        return *_infos[v.index()];
    }

    // General cast, also overloaded on things like std::any and std::variant 
    // so there's a general way to cast those kinds of types to another type
    template<class Ty, class Arg> constexpr Ty general_cast(Arg&& arg) { return static_cast<Ty>(std::forward<Arg>(arg)); }
    template<class Ty> Ty general_cast(std::any& v) { return std::any_cast<Ty>(v); }
    template<class Ty> Ty general_cast(const std::any& v) { return std::any_cast<const Ty>(v); }
    template<class Ty, class ...As> Ty general_cast(std::variant<As...>& v) { return std::get<Ty>(v); }
    template<class Ty, class ...As> Ty general_cast(const std::variant<As...>& v) { return std::get<const Ty>(v); }

    template<class A, class B> // Simple test if equality operator exists
    concept comparable = requires(A a, B b) { { a == b } -> std::convertible_to<bool>; };

    // Compare 2 tuples, if not directly comparable, it tries the general cast, 
    // it tries casting the types in 'a' to the types in 'b'.
    template<class ...As, class ...Bs>
    constexpr bool compare_tuples(std::tuple<As...>& a, std::tuple<Bs...>& b) {

        // If not same size, can't be equal
        if constexpr (sizeof...(Bs) != sizeof...(As)) return false;

        // Otherwise go over all arguments recursively
        else return[&]<std::size_t I>(this auto && self) {
            if constexpr (I == sizeof...(As)) return true; // base case
            else { // Get both types
                using A = decltype(std::get<I>(a));
                using B = decltype(std::get<I>(b));

                if constexpr (comparable<A, B>) return // Just compare and recurse
                    std::get<I>(a) == std::get<I>(b) && self.operator()<I + 1>();

                else { // Otherwise try casting
                    // First check if stored type in As match up with Bs, 
                    // to make sure we can actually cast to Bs
                    if (type(std::get<I>(a)) != typeid(B)) return false;
                    return general_cast<B>(std::get<I>(a)) == std::get<I>(b)
                        && self.operator()<I + 1>(); // And recurse
                }
            }
        }.operator()<0>();
    }

    template<class Body, class ...As> struct match_case;
    namespace match_types {
        struct wildcard { // Wildcard is a simple equality operator that always returns true
            template<class Arg> constexpr bool operator==(Arg&&) { return true; }
        } _;

        template<class...As> struct l : std::tuple<As&&...> {
            using std::tuple<As&&...>::tuple;

            template<class Body> constexpr match_case<Body&&, As...>
            operator=(this l&& self, Body&& b) { return { std::move(self), std::forward<Body>(b) }; }

            constexpr std::tuple<As&&...>& tuple() { return *this; }
        };
        template<class ...Args> l(Args&&...)->l<Args...>;

        struct default_t {};
        template<> struct l<default_t> {
            template<class Body> constexpr match_case<Body&&, default_t>
            operator=(this l self, Body&& b) { return { self, std::forward<Body>(b) }; }

            constexpr wildcard tuple() { return {}; }
        };
        constexpr l<default_t> otherwise;
    }

    template<class ...As> // Special case for wildcard, return true, used for 'otherwise'
    constexpr bool compare_tuples(std::tuple<As...>& a, match_types::wildcard b) { return true; }

    // Match result for non-references, uses union
    // to optionally store value, boolean flag for empty.
    template<class Ty> struct match_result {
        constexpr match_result() : empty(nullptr), has_val(false) {}
        constexpr match_result(Ty&& v) : value(std::move(v)), has_val(true) {}

        union { void* empty; Ty value; };

        bool has_val = false;
        constexpr operator bool() { return has_val; }
        constexpr Ty get() { return std::move(value); }
    };

    // Match result for references, uses pointer
    // nullptr when empty
    template<class Ty> requires std::is_reference_v<Ty> struct match_result<Ty> {
        constexpr match_result() : value(nullptr) {}
        constexpr match_result(Ty v) : value(&v) {}

        std::decay_t<Ty>* value;
        constexpr operator bool() { return value != nullptr; }
        constexpr std::decay_t<Ty>& get() { return *value; }
    };

    // Match result for void, has special int constructor
    // to distinguish between empty and non-empty.
    template<> struct match_result<void> {
        constexpr match_result() : has_val(false) {}
        constexpr match_result(int) : has_val(true) {}

        bool has_val;
        constexpr operator bool() { return has_val; }
        constexpr void get() { }
    };

    template<class Body, class ...As>
    struct match_case {
        using body_t = std::decay_t<Body>;
        enum class body_type { Value, NoArgs, Args };
        constexpr static body_type TYPE =
            std::invocable<body_t> ? body_type::NoArgs // If invocable with no arguments: NoArgs
            : is_functor<body_t> ? body_type::Args // If it does have a function operator: Args
            : body_type::Value; // Otherwise it's just a value

        match_types::l<As...> args;
        Body body;

        template<class ...Tys>
        constexpr auto check(match_types::l<Tys...>& c) {

            // Simple value as body, just check equality of tuples, and return the body 
            if constexpr (TYPE == body_type::Value) return check_value(c);

            // Lambda that takes no arguments, check equality of tuples, return result 
            // of call to body. If return type is void we need to differentiate, so 
            // call constructor with '0' (specialized match_result for type == void)
            else if constexpr (TYPE == body_type::NoArgs) return check_noarg(c);

            // Lambda that takes arguments, bit more complicated with edge cases.
            else if constexpr (TYPE == body_type::Args) return check_arg(c);
        }

        template<class ...Tys>
        constexpr auto check_value(match_types::l<Tys...>& c) {
            using result = match_result<Body>;
            if (compare_tuples(c.tuple(), args.tuple())) return result{ std::move(body) };
            else return result{};
        }

        template<class ...Tys>
        constexpr auto check_noarg(match_types::l<Tys...>& c) {
            using res_type = decltype(body());
            using result = match_result<res_type>;
            if (compare_tuples(c.tuple(), args.tuple()))
                if constexpr (std::same_as<res_type, void>) return (body(), result{ 0 });
                else return result{ body() };
            else return result{};
        }

        template<class ...Tys>
        constexpr auto check_arg(match_types::l<Tys...>& c) {
            using fargs = info<body_t>::arguments::template as<std::tuple>;
            using res_type = decltype(std::apply(body, std::declval<fargs>()));
            using result = match_result<res_type>;

            // Step 1: make sure amount of arguments of lambda matches amount of inputs provided
            if constexpr (std::tuple_size_v<std::decay_t<decltype(c.tuple())>> != std::tuple_size_v<fargs>) return result{};

            // Step 2: check if function is callable with inputs
            else return[&]<std::size_t ...Is>(std::index_sequence<Is...>) {
                if constexpr (!(std::convertible_to<   // If the input type is not convertible to
                    decltype(std::get<Is>(c.tuple())), // the provided lambda arguments, we'll have
                    std::tuple_element_t<Is, fargs>>   // to check if we can cast them.
                    && ...)) {
                    // We use the overloaded 'type' function to get the actual stored type in the input
                    // and we do a runtime check using std::type_info to check if they equal.
                    if (((type(std::get<Is>(c.tuple())) != typeid(std::tuple_element_t<Is, fargs>)) || ...))
                        return result{}; // If no equal, there's nothing we can do, can't compare them...
                }

                // After checking if they're convertible or castible, collect a casted tuple and compare 
                // that new tuple to the match case. If successful, call body with casted inputs.
                std::tuple _casted{ general_cast<std::tuple_element_t<Is, fargs>>(std::get<Is>(c.tuple()))... };
                if (compare_tuples(_casted, args.tuple())) return result{ std::apply(body, _casted) };
                else return result{};
            }(std::make_index_sequence<std::tuple_size_v<fargs>>{});
        }
    };

    template<class ...Cases> struct match : std::tuple<Cases&&...> {
        using std::tuple<Cases&&...>::tuple;
    };
    template<class ...Args> match(Args&&...)->match<Args...>;

    template<class ...As, class ...Cases>
    constexpr auto operator>>(match_types::l<As...>&& arg, match<Cases...>&& m) {
        // Go through all match cases and check if it's a match
        return[&]<std::size_t I>(this auto && self) {
            auto res = std::get<I>(m).check(arg);
            using result = decltype(res.get());
            if (res) return res.get();
            // If no match (prevent infinite recursive type)
            else if constexpr (I == sizeof...(Cases) - 1) {
                if constexpr (std::is_default_constructible_v<result>) return result{};
            }
            else return self.operator()<I + 1>(); // Otherwise recurse.
        }.operator()<0>();
    }
}