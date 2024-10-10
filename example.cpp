
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

namespace kaixo {

    // ------------------------------------------------

    template<std::size_t N>
    struct template_name {
        char m_Name[N];
        constexpr template_name(const char(&n)[N]) {
            std::copy_n(n, N, m_Name); 
        }
    };

    // ------------------------------------------------

    namespace detail {
        template<class Ty>
        concept has_depend = requires() {
            typename Ty::depend;
        };

        template<class Ty> 
        struct depends_impl : std::type_identity<std::tuple<>> {};

        template<has_depend Ty>
        struct depends_impl<Ty> : std::type_identity<typename Ty::depend> {};

        template<class Ty>
        concept has_define = requires() {
            typename Ty::define;
        };

        template<class Ty> 
        struct defines_impl : std::type_identity<std::tuple<>> {};

        template<has_define Ty>
        struct defines_impl<Ty> : std::type_identity<typename Ty::define> {};
    }

    template<class Ty>
    using depends = detail::depends_impl<std::decay_t<Ty>>::type;
    
    template<class Ty>
    using defines = detail::defines_impl<std::decay_t<Ty>>::type;

    // ------------------------------------------------

    template<class Ty>
    concept var = requires() {
        typename Ty::_is_variable;
    };
    
    template<class Ty>
    concept is_named_range = requires() {
        typename Ty::_is_named_range;
    };

    template<class Ty>
    concept has_dependencies = std::tuple_size_v<depends<Ty>> != 0;
    
    template<class Ty>
    concept has_definitions = std::tuple_size_v<defines<Ty>> != 0;

    using std::ranges::range;

    // ------------------------------------------------

    template<var Var, class Ty>
    struct named_variable {
        using define = std::tuple<Var>;

        Ty value;
    };

    // ------------------------------------------------

    template<template_name>
    struct variable {
        using _is_variable = int;

        using depends = std::tuple<variable>;

        template<class Ty>
        constexpr named_variable<variable, Ty> operator=(Ty&& value) const {
            return { std::forward<Ty>(value) };
        }
    };

    // ------------------------------------------------

    template<range Range, var... Vars>
    struct named_range : std::views::all_t<Range> {

        using _is_named_range = int;

        using depend = depends<Range>;
        using define = std::tuple<Vars...>;

    };

    // ------------------------------------------------

    template<range Range>
    constexpr std::views::all_t<Range> operator-(Range&& range) {
        return std::views::all(std::forward<Range>(range));
    }

    template<range Range, var Var>
    constexpr named_range<Range, Var> operator<(Var, Range&& range) {
        return { std::forward<Range>(range) };
    }
    
    template<range Range, var ...Vars>
    constexpr named_range<Range, Vars...> operator<(std::tuple<Vars...>, Range&& range) {
        return { std::forward<Range>(range) };
    }

    // ------------------------------------------------

    template<var A, var B>
    constexpr std::tuple<A, B> operator,(A, B) { return {}; }
    template<var A, var ...Bs>
    constexpr std::tuple<A, Bs...> operator,(A, std::tuple<Bs...>) { return {}; }
    template<var ...As, var B>
    constexpr std::tuple<As..., B> operator,(std::tuple<As...>, B) { return {}; }
    template<var ...As, var ...Bs>
    constexpr std::tuple<As..., Bs...> operator,(std::tuple<As...>, std::tuple<Bs...>) { return {}; }

    // ------------------------------------------------

    template<class Expr, class ...Parts>
    struct list_comprehension {
        Expr expression;
        std::tuple<Parts...> parts;
    };

    template<class Expr, has_definitions Part>
    constexpr list_comprehension<std::decay_t<Expr>, std::decay_t<Part>> operator|(Expr&& expr, Part&& part) {
        return { std::forward<Expr>(expr), std::make_tuple(std::forward<Part>(part)) };
    }

    template<class Expr, class ...Parts, class Part>
    constexpr list_comprehension<Expr, Parts..., std::decay_t<Part>> operator,(list_comprehension<Expr, Parts...>&& lc, Part&& part) {
        return { 
            std::move(lc.expression), 
            std::tuple_cat(std::move(lc.parts), std::forward_as_tuple(std::forward<Part>(part))) 
        };
    }

    // ------------------------------------------------

}

// ------------------------------------------------

int main() {
    using namespace kaixo;

    return 1;
}

// ------------------------------------------------
