
// ------------------------------------------------

#include <iostream>
#include <fstream>
#include <string>
#include <source_location>
#include <vector>
#include <map>
#include <ranges>
#include <algorithm>
#include <utility>
#include <functional>
#include <cassert>
#include <set>
#include <expected>
#include <thread>
#include <any>
#include <mutex>
#include <type_traits>
#include <concepts>
#include <cstddef>
#include <string_view>
#include <unordered_set>
#include <regex>
#include <complex>
#include <print>

// ------------------------------------------------

namespace kaixo {

    // ------------------------------------------------
    
    constexpr std::size_t npos = static_cast<std::size_t>(-1);

    // ------------------------------------------------
    
    template<class Ty>
    constexpr std::tuple<Ty&> flatten_tuple(Ty& t) { return { t }; }
    template<class A, class B>
    constexpr std::tuple<A&, B&> flatten_tuple(std::pair<A, B>& t) { return { t.first, t.second }; }
    template<class A, class B>
    constexpr std::tuple<const A&, const B&> flatten_tuple(const std::pair<A, B>& t) { return { t.first, t.second }; }
    
    template<class ...Tys>
    constexpr auto flatten_tuple(std::tuple<Tys...>& t) {
        return std::apply([]<class ...As>(As&& ...v) { return std::tuple_cat(flatten_tuple(v)...);  }, t);
    }
    
    template<class ...Tys>
    constexpr auto flatten_tuple(const std::tuple<Tys...>& t) {
        return std::apply([]<class ...As>(As&& ...v) { return std::tuple_cat(flatten_tuple(v)...); }, t);
    }

    // ------------------------------------------------

    template<std::size_t I, class Tuple>
    constexpr decltype(auto) recursive_get(Tuple&& tuple) { return std::get<I>(flatten_tuple(tuple)); }

    // ------------------------------------------------

    template<class...> 
    struct var {};

    template<class, class, std::size_t = 0> 
    struct find_var : std::integral_constant<std::size_t, npos> {};

    template<class Find, class Var, class ...Vars, std::size_t N> 
    struct find_var<Find, var<Var, Vars...>, N> : find_var<Find, var<Vars...>, N + 1> {};

    template<class Find, class ...Vars, std::size_t N>
    struct find_var<Find, var<Find, Vars...>, N> : std::integral_constant<std::size_t, N> {};

    template<class, class> 
    struct contains_all : std::false_type {};

    template<class ...Contains, class Vars> 
    struct contains_all<var<Contains...>, Vars> : std::bool_constant<((find_var<Contains, Vars>::value != npos) && ...)> {};

    // ------------------------------------------------
    
    template<class> 
    struct depends : std::type_identity<var<>> {};

    template<class ...Vars>
    struct depends<var<Vars...>> : std::type_identity<var<Vars...>> {};

    template<class> 
    struct defines : std::type_identity<var<>> {};

    // ------------------------------------------------

    template<class Vars, class Tuple>
    struct named_tuple : Tuple {
        template<class Find> 
        constexpr decltype(auto) get() const { 
            return recursive_get<find_var<Find, Vars>::value>(static_cast<const Tuple&>(*this));
        }
    };

    // ------------------------------------------------

    template<class ...As, class Vars, class Tuple>
    constexpr decltype(auto) evaluate_expression(var<As...>, const named_tuple<Vars, Tuple>& v) {
        if constexpr (sizeof...(As) == 1) return (v.template get<As>(), ...);
        else return std::forward_as_tuple(v.template get<As>()...);
    }

    // ------------------------------------------------
    
    template<class Vars, std::ranges::range Range, class Expression = int>
    struct named_range_storage {
        Range range{};
        Expression expression{};
    };

    template<class, std::ranges::range, class = int> 
    struct named_range;

    template<class Vars, std::ranges::range Range, class Expression>
        requires (!contains_all<typename depends<Expression>::type, Vars>::value)
    struct named_range<Vars, Range, Expression> : named_range_storage<Vars, Range, Expression> {};

    template<class Vars, std::ranges::range Range, class Expression> 
        requires (contains_all<typename depends<Expression>::type, Vars>::value)
    struct named_range<Vars, Range, Expression> : named_range_storage<Vars, Range, Expression> {
        using base_iterator = std::ranges::iterator_t<Range>;
        using base_reference_t = std::ranges::range_reference_t<Range>;

        struct iterator : base_iterator {
            const named_range<Vars, Range, Expression>* self = nullptr;

            constexpr decltype(auto) operator*() {
                return evaluate_expression(self->expression, named_tuple<Vars, base_reference_t>{ base_iterator::operator*() });
            }

            constexpr decltype(auto) operator[](std::size_t i) requires std::ranges::random_access_range<Range> {
                return evaluate_expression(self->expression, named_tuple<Vars, base_reference_t>{ base_iterator::operator[](i) });
            }
        };

        constexpr iterator begin() const { return { std::ranges::begin(this->range), this }; }
        constexpr iterator end() const { return { std::ranges::end(this->range), this }; }
        constexpr iterator cbegin() const { return { std::ranges::cbegin(this->range), this }; }
        constexpr iterator cend() const { return { std::ranges::cend(this->range), this }; }

        constexpr decltype(auto) operator[](std::size_t i) requires std::ranges::random_access_range<Range> {
            return evaluate_expression(this->expression, named_tuple<Vars, base_iterator>{ this->range[i] });
        }
    };

    template<class Vars, std::ranges::range Range, class Expression>
    struct defines<named_range<Vars, Range, Expression>> : std::type_identity<Vars> {};

    // ------------------------------------------------

    template<class> 
    struct range_result_size : std::integral_constant<std::size_t, 1> {};

    template<class ...Tys>
    struct range_result_size<std::tuple<Tys...>> : std::integral_constant<std::size_t, sizeof...(Tys)> {};

    template<class A, class B>
    struct range_result_size<std::pair<A, B>> : std::integral_constant<std::size_t, 2> {};

    // ------------------------------------------------

    template<class ...As, class ...Bs>
    constexpr var<As..., Bs...> operator,(var<As...>, var<Bs...>) { return {}; }

    template<std::ranges::range Range>
    constexpr std::views::all_t<Range> operator-(Range&& r) { return std::views::all(std::forward<Range>(r)); }

    template<class ...Vs, std::ranges::range Range>
        requires (range_result_size<std::ranges::range_value_t<Range>>::value == sizeof...(Vs))
    constexpr named_range<var<Vs...>, Range> operator<(var<Vs...>, Range&& r) { return { std::forward<Range>(r) }; }

    template<std::ranges::range A, std::ranges::range B>
    constexpr auto operator,(A&& a, B&& b) 
        -> std::ranges::zip_view<std::views::all_t<A>, std::views::all_t<B>> {
        return std::ranges::zip_view{ std::forward<A>(a), std::forward<B>(b) };
    }

    template<class ...As, class Expression, std::ranges::range A, class ...Bs, std::ranges::range B>
    constexpr auto operator,(named_range<var<As...>, A, Expression>&& a, named_range<var<Bs...>, B>&& b)
        -> named_range<var<As..., Bs...>, std::ranges::cartesian_product_view<A, B>, Expression> {
        return { std::ranges::cartesian_product_view<A, B>(std::move(a.range), std::move(b.range)), std::move(a.expression) };
    }

    template<class ...Vs, class ...As, std::ranges::range A>
    constexpr auto operator|(var<Vs...>, named_range<var<As...>, A>&& a) 
        -> named_range<var<As...>, A, var<Vs...>> { return { a.range }; }

    // ------------------------------------------------

}

int main() {
    using namespace kaixo;

    constexpr var<struct A> a;
    constexpr var<struct B> b;
    constexpr var<struct C> c;
    constexpr var<struct D> d;
    constexpr var<struct E> e;

    const std::vector<int> as{ 1, 2, 3 };
    std::vector<char> bs{ 1, 2, 3 };
    std::vector<long long> cs{ 1, 2, 3 };
    std::map<float, double> map{ { 1, 2 }, { 2, 3 } };

    auto lc = ((a, b, c, d, e) | (a, b) <- (as, bs), c <- cs, (d, e) <- map);

    for (auto [a, b, c, d, e] : lc) {
        std::println("({}, {}, {}, {}, {})", a, b, c, d, e);
    }

    return 0;
}

// ------------------------------------------------
