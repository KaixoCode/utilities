
// ------------------------------------------------

#include <iostream>
#include <string>
#include <source_location>
#include <vector>
#include <map>
#include <ranges>
#include <algorithm>
#include <utility>
#include <functional>
#include <cassert>
#include <expected>
#include <thread>
#include <any>
#include <mutex>
#include <type_traits>
#include <concepts>
#include <cstddef>
#include <utility>
#include <algorithm>
// ------------------------------------------------

#include "pack_utils/pack_utils.hpp"
#include "string_literal.hpp"

// ------------------------------------------------

namespace kaixo {

    // ------------------------------------------------
    
    namespace detail {
        template<class Ty> concept has_depend = requires() { typename Ty::depend; };
        template<class Ty> concept has_define = requires() { typename Ty::define; };

        template<class Ty> struct depend_impl { using type = pack<>; };
        template<class Ty> struct define_impl { using type = pack<>; };

        template<has_depend Ty> struct depend_impl<Ty> { using type = Ty::depend; };
        template<has_define Ty> struct define_impl<Ty> { using type = Ty::define; };

        template<class ...Tys> struct depend_impl<std::tuple<Tys...>> {
            using type = pack_unique_t<pack_concat_t<typename depend_impl<Tys>::type...>>;
        };

        template<class ...Tys> struct define_impl<std::tuple<Tys...>> {
            using type = pack_unique_t<pack_concat_t<typename define_impl<Tys>::type...>>;
        };
    }

    template<class Ty> using depends = detail::depend_impl<Ty>::type;
    template<class Ty> using defines = detail::define_impl<Ty>::type;

    // ------------------------------------------------
    //                   Concepts
    // ------------------------------------------------

    namespace detail {
        template<class, template<class...> class>
        struct specialization_of_impl : std::false_type {};

        template<template<class...> class A, class ...Args>
        struct specialization_of_impl<A<Args...>, A> : std::true_type {};
    }

    template<class A, template<class...> class B>
    concept specialization_of = detail::specialization_of_impl<std::decay_t<A>, B>::value;

    // ------------------------------------------------

    template<class Ty> concept is_var = requires() { typename std::decay_t<Ty>::_is_var; };
    template<class Ty> concept is_operator = requires() { typename std::decay_t<Ty>::_is_operator; };
    template<class Ty> concept is_unevaluated = (pack_size_v<pack_remove_all_t< // Remove all defines from depends to check
                                                 pack_concat_t<defines<Ty>>,    // whether any depends that aren't defined.
                                                 pack_concat_t<depends<Ty>>>> != 0);

    // ------------------------------------------------
    
    template<class, is_var> 
    struct named_value;

    namespace detail {
        template<class>
        struct is_named_tuple_impl : std::false_type {};

        template<specialization_of<named_value> ...Tys>
        struct is_named_tuple_impl<std::tuple<Tys...>> : std::true_type {};
    }

    template<class Ty>
    concept is_named_tuple = specialization_of<std::decay_t<Ty>, std::tuple>
        && detail::is_named_tuple_impl<std::decay_t<Ty>>::value;

    // ------------------------------------------------

    template<class Ty, is_var Var>
    constexpr static bool defines_contains = pack_contains_v<Var, defines<Ty>>;

    // ------------------------------------------------
    //                 Named Classes
    // ------------------------------------------------

    template<class Ty, is_var Var> 
    struct named_value {

        // ------------------------------------------------

        using define = pack<Var>;
        using depend = depends<Ty>;

        // ------------------------------------------------
        
        [[no_unique_address]] Ty value;

        // ------------------------------------------------

    };

    // ------------------------------------------------

    template<class Ty, is_var ...Vars>
        requires (std::ranges::range<Ty> || is_unevaluated<Ty>)
    struct named_range : Ty {

        // ------------------------------------------------

        using range_type = Ty;
        using define = pack<Vars...>; // TODO: check whether nof vars matches value_type
        using depend = depends<Ty>;

        // ------------------------------------------------
        
    };

    // ------------------------------------------------

    template<string_literal>
    struct var {

        // ------------------------------------------------

        using _is_var = int;
        using depend = pack<var>;

        // ------------------------------------------------
        
        template<class Ty>
        constexpr named_value<std::decay_t<Ty>, var> operator=(Ty&& value) const {
            return { std::forward<Ty>(value) };
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------
    
    template<class Result, class ...Parts>
    struct list_comprehension;

    template<class ...As> 
    concept are_valid_expression = (is_unevaluated<std::decay_t<As>> || ...) 
        && (!specialization_of<std::decay_t<As>, named_range> && ...)
        && (!specialization_of<std::decay_t<As>, list_comprehension> && ...);

    template<is_operator Op, class ...Args>
        requires are_valid_expression<Args...>
    struct unevaluated_expression : std::tuple<Args...> {

        // ------------------------------------------------
        
        constexpr static std::size_t size = sizeof...(Args);

        // ------------------------------------------------

        using operation = Op;
        using depend = pack_unique_t<pack_concat_t<depends<Args>...>>;

        // ------------------------------------------------

    };

    // ------------------------------------------------
    //             Get From Named Tuple
    // ------------------------------------------------

    template<is_var Var, is_named_tuple Tuple>
        requires defines_contains<std::decay_t<Tuple>, Var>
    constexpr decltype(auto) named_get(Tuple&& tuple) {
        constexpr std::size_t index = pack_index_of_v<Var, defines<std::decay_t<Tuple>>>;
        return std::get<index>(std::forward<Tuple>(tuple)).value;
    }

    // ------------------------------------------------
    //           Evaluate With Named Tuple
    // ------------------------------------------------

    template<class Ty, is_named_tuple Tuple>
    constexpr Ty&& evaluate(Ty&& val, Tuple&&) {
        return std::forward<Ty>(val);
    }

    template<is_var Var, is_named_tuple Tuple>
    constexpr decltype(auto) evaluate(Var&&, Tuple&& tuple) {
        if constexpr (defines_contains<std::decay_t<Tuple>, Var>) 
            return kaixo::named_get<Var>(tuple);
        else return Var{};
    }
    
    template<is_var Var, is_named_tuple Tuple>
    constexpr decltype(auto) evaluate(const Var&, Tuple&& tuple) {
        return kaixo::evaluate(Var{});
    }

    template<specialization_of<unevaluated_expression> Ty, is_named_tuple Tuple>
    constexpr decltype(auto) evaluate(Ty&& value, Tuple&& tuple) {
        return [&]<std::size_t ...Is>(std::index_sequence<Is...>) {
            return Ty::operation::evaluate(
                kaixo::evaluate(std::get<Is>(std::forward<Ty>(value)), tuple)...);
        }(std::make_index_sequence<std::decay_t<Ty>::size>{});
    }
    
    template<specialization_of<named_range> Ty, is_named_tuple Tuple>
    constexpr decltype(auto) evaluate(Ty&& value, Tuple&& tuple) {
        if constexpr (is_unevaluated<Ty>) {
            using range_type = typename std::decay_t<Ty>::range_type;
            // TODO: handle unevaluated ranges
            return 0;
        } else {
            return std::forward<Ty>(value);
        }
    }

    template<class ...As, is_named_tuple Tuple>
        requires are_valid_expression<As...>
    constexpr decltype(auto) evaluate(std::tuple<As...>&& value, Tuple&& tuple) {
        auto res = [&]<std::size_t ...Is>(std::index_sequence<Is...>) {
            return std::tuple(kaixo::evaluate(std::get<Is>(std::move(value)), tuple)...);
        }(std::index_sequence_for<As...>{});
        
        // TODO: check if still unevaluated
        return res;
    }

    // ------------------------------------------------

    enum return_code : std::size_t {
        none = 0, skip = 1
    };

    template<class Ty, is_named_tuple Tuple>
    constexpr decltype(auto) execute(Ty&& value, std::size_t& code, Tuple&& tuple) {
        if constexpr (std::convertible_to<decltype(kaixo::evaluate(std::forward<Ty>(value), tuple)), bool>) {
            code = bool(kaixo::evaluate(std::forward<Ty>(value), tuple)) ? return_code::none : return_code::skip;
            return tuple;
        }
    }
    // ------------------------------------------------

    template<class Ty, is_named_tuple>
    struct evaluated_type : std::type_identity<Ty> {};

    template<is_unevaluated Ty, is_named_tuple Tuple>
    struct evaluated_type<Ty, Tuple> : evaluated_type<
        decltype(kaixo::evaluate(std::declval<Ty>(), std::declval<Tuple>())), Tuple> {};

    template<class Ty, is_named_tuple Tuple> // Resulting type after evaluate with Tuple
    using evaluated_type_t = evaluated_type<Ty, Tuple>::type;

    // ------------------------------------------------

    template<class R, is_named_tuple T>
    struct defined_values : std::type_identity<std::decay_t<decltype(
        kaixo::execute(std::declval<R&>(), std::declval<std::size_t&>(), std::declval<T&>()))>> {};

    template<std::ranges::range R, is_named_tuple T>
    struct defined_values<R, T> : std::type_identity<pack_prepend_t<pack<T>, typename R::reference>> {};

    template<is_unevaluated R, is_named_tuple T>
    struct defined_values<R, T> : defined_values<evaluated_type_t<R, T>, T> {};

    template<class R, is_named_tuple T>
    using defined_values_t = typename defined_values<R, T>::type;

    // ------------------------------------------------

    template<is_named_tuple Tuple, class Parts>
    struct named_tuple_type;

    template<is_named_tuple Tuple, class Part, class ...Parts>
    struct named_tuple_type<Tuple, pack<Part, Parts...>>
        : named_tuple_type<defined_values_t<Part, Tuple>, pack<Parts...>> {};

    template<is_named_tuple Tuple, class Part>
    struct named_tuple_type<Tuple, pack<Part>> {
        using type = defined_values_t<Part, Tuple>;
    };

    template<class ...Parts>
    using named_tuple_type_t = named_tuple_type<std::tuple<>, pack<Parts...>>::type;

    // ------------------------------------------------
    
    template<class Result, class ...Parts>
    struct list_comprehension {

        // ------------------------------------------------

        using depend = pack_unique_t<pack_concat_t<depends<Result>, depends<Parts>...>>;
        using define = pack_unique_t<pack_concat_t<defines<Result>, defines<Parts>...>>;

        // ------------------------------------------------

        [[no_unique_address]] Result result;
        std::tuple<Parts...> parts;

        // ------------------------------------------------

    };
    
    // ------------------------------------------------
        
    namespace operators {

        // ------------------------------------------------
        //           Tuple Expression Operators
        // ------------------------------------------------

        template<class A, class B>
            requires are_valid_expression<A, B>
        constexpr std::tuple<std::decay_t<A>, std::decay_t<B>> operator,(A&& a, B&& b) {
            return { std::forward<A>(a), std::forward<B>(b) };
        }

        template<class A, class ...Bs>
            requires are_valid_expression<Bs...>
        constexpr std::tuple<std::decay_t<A>, Bs...> operator,(A&& a, std::tuple<Bs...>&& b) {
            return std::tuple_cat(std::forward_as_tuple(std::forward<A>(a)), std::move(b));
        }

        template<class A, class ...Bs>
            requires are_valid_expression<Bs...>
        constexpr std::tuple<Bs..., std::decay_t<A>> operator,(std::tuple<Bs...>&& b, A&& a) {
            return std::tuple_cat(std::move(b), std::forward_as_tuple(std::forward<A>(a)));
        }

        // ------------------------------------------------
        //        Unevaluated Expression Operators
        // ------------------------------------------------

#define KAIXO_UNARY_OPERATOR(name, op)                        \
        struct name {                                         \
            using _is_operator = int;                         \
            template<class A>                                 \
            constexpr static decltype(auto) evaluate(A&& a) { \
                return op std::forward<A>(a);                 \
            }                                                 \
        };                                                                           \
                                                                                     \
        template<is_unevaluated A>                                                   \
        constexpr unevaluated_expression<name, std::decay_t<A>> operator op(A&& a) { \
            return { { std::forward<A>(a) } };                                       \
        }

#define KAIXO_BINARY_OPERATOR(name, op)                              \
        struct name {                                                \
            using _is_operator = int;                                \
            template<class A, class B>                               \
            constexpr static decltype(auto) evaluate(A&& a, B&& b) { \
                return std::forward<A>(a) op std::forward<B>(b);     \
            }                                                        \
        };                                                                                                   \
                                                                                                             \
        template<class A, class B>                                                                           \
            requires are_valid_expression<A, B>                                                              \
        constexpr unevaluated_expression<name, std::decay_t<A>, std::decay_t<B>> operator op(A&& a, B&& b) { \
            return { { std::forward<A>(a), std::forward<B>(b) } };                                           \
        }

        KAIXO_UNARY_OPERATOR(increment, ++);
        KAIXO_UNARY_OPERATOR(decrement, --);
        KAIXO_UNARY_OPERATOR(boolean_not, !);
        KAIXO_UNARY_OPERATOR(bitwise_not, ~);

        KAIXO_BINARY_OPERATOR(add, +);
        KAIXO_BINARY_OPERATOR(subtract, -);
        KAIXO_BINARY_OPERATOR(multiply, *);
        KAIXO_BINARY_OPERATOR(divide, / );
        KAIXO_BINARY_OPERATOR(modulo, %);
        KAIXO_BINARY_OPERATOR(less_than, < );
        KAIXO_BINARY_OPERATOR(less_or_equal, <= );
        KAIXO_BINARY_OPERATOR(greater_than, > );
        KAIXO_BINARY_OPERATOR(greater_or_equal, >= );
        KAIXO_BINARY_OPERATOR(equal, == );
        KAIXO_BINARY_OPERATOR(not_equal, != );
        KAIXO_BINARY_OPERATOR(left_shift, << );
        KAIXO_BINARY_OPERATOR(right_shift, >> );
        KAIXO_BINARY_OPERATOR(boolean_and, &&);
        KAIXO_BINARY_OPERATOR(boolean_or, || );
        KAIXO_BINARY_OPERATOR(bitwise_and, &);
        KAIXO_BINARY_OPERATOR(bitwise_or, | );
        KAIXO_BINARY_OPERATOR(bitwise_xor, ^);
        KAIXO_BINARY_OPERATOR(spaceship, <=> );
        KAIXO_BINARY_OPERATOR(add_assign, +=);
        KAIXO_BINARY_OPERATOR(subtract_assign, -=);
        KAIXO_BINARY_OPERATOR(multiply_assign, *=);
        KAIXO_BINARY_OPERATOR(divide_assign, /=);
        KAIXO_BINARY_OPERATOR(modulo_assign, %=);
        KAIXO_BINARY_OPERATOR(left_shift_assign, <<=);
        KAIXO_BINARY_OPERATOR(right_shift_assign, >>=);
        KAIXO_BINARY_OPERATOR(and_assign, &=);
        KAIXO_BINARY_OPERATOR(or_assign, |=);
        KAIXO_BINARY_OPERATOR(xor_assign, ^=);    

        // ------------------------------------------------
        //            Named Range Operators
        // ------------------------------------------------

        template<std::ranges::range Range>
        constexpr decltype(auto) operator-(Range&& r) { return std::views::all(std::forward<Range>(r)); }

        template<is_unevaluated Range>
        constexpr decltype(auto) operator-(Range&& r) { return std::forward<Range>(r); }

        template<class Range, is_var Var>
            requires (std::ranges::range<Range> || is_unevaluated<Range>)
        constexpr decltype(auto) operator<(const Var& v, Range&& r) {
            return named_range<std::decay_t<Range>, Var>{ std::forward<Range>(r) };
        }
        
        template<class Range, is_var Var>
            requires (std::ranges::range<Range> || is_unevaluated<Range>)
        constexpr decltype(auto) operator<(Var&& v, Range&& r) {
            return named_range<std::decay_t<Range>, Var>{ std::forward<Range>(r) };
        }
        
        template<class Range, is_var ...Vars>
            requires (std::ranges::range<Range> || is_unevaluated<Range>)
        constexpr decltype(auto) operator<(std::tuple<Vars...>&& v, Range&& r) {
            return named_range<std::decay_t<Range>, Vars...>{ std::forward<Range>(r) };
        }

        // ------------------------------------------------
        //            List Comprehension Operators
        // ------------------------------------------------

        template<specialization_of<list_comprehension> Ty, class Part>
        constexpr auto operator,(Ty&& lc, Part&& part) {
            return list_comprehension{ std::forward<Ty>(lc).result,
                std::tuple_cat(std::forward<Ty>(lc).parts, std::tuple(std::forward<Part>(part)))
            };
        };

        template<is_unevaluated A, class B>
        constexpr list_comprehension<std::decay_t<A>, std::decay_t<B>> operator|(A&& a, B&& b) {
            return { std::forward<A>(a), std::tuple{ std::forward<B>(b) } };
        }

        // ------------------------------------------------

    }

    // ------------------------------------------------



}

// ------------------------------------------------

int main() {

    using namespace kaixo;
    using namespace kaixo::operators;

    constexpr var<"a"> a{};
    constexpr var<"b"> b{};
    constexpr var<"c"> c{};

    constexpr auto aioen = is_var<var<"a">>;
    constexpr auto efae = is_var<int>;

    constexpr auto faefaefae = (a, b);

    constexpr auto faea = is_named_tuple<decltype(std::tuple{ a = 1, b = 2 })>;

    constexpr auto aeoinfae = (a, b, c);
    constexpr auto aoin = evaluate((a + 1, b + 1, 1), std::tuple{ a = 1, b = 2 });

    using ggmgmo = evaluated_type_t<decltype((a, b)), decltype(std::tuple{ a = 1, b = 2 }) > ;

    constexpr ggmgmo a{ 1, 2 };

    constexpr auto aeoiun = ((a, b) | (a <- std::array<int, 3>{ 1, 2, 3 }), a != 1);

    return 1;
}

// ------------------------------------------------
