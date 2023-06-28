#include <iostream>
#include <set>
#include <deque>
#include <vector>
#include <map>
#include <ranges>

#include "kaixo/type_utils.hpp"


struct MyType {
    MyType() { 
        std::cout << "Construct\n"; 
    }

    MyType(MyType&& v) noexcept
        : a(v.a), b(v.b), c(v.c) { 
        std::cout << "Move\n"; 
    }

    MyType(const MyType& v)
        : a(v.a), b(v.b), c(v.c) {
        std::cout << "Copy\n";
    }
    
    MyType& operator=(MyType&& v) noexcept {
        a = v.a, b = v.b, c = v.c;
        std::cout << "Move Assign\n"; 
        return *this;
    }

    MyType& operator=(const MyType& v) {
        a = v.a, b = v.b, c = v.c;
        std::cout << "Copy Assign\n";
        return *this;
    }

    ~MyType() {
        std::cout << "Destruct\n";
    }

    int a = 1;
    float b = 2;
    std::string c = "Woof";

    template<std::size_t I>
    constexpr auto& get() & {
        if constexpr (I == 0) return a;
        else if constexpr (I == 1) return b;
        else if constexpr (I == 2) return c;
    }

    template<std::size_t I>
    constexpr auto&& get() && {
        if constexpr (I == 0) return a;
        else if constexpr (I == 1) return b;
        else if constexpr (I == 2) return c;
    }
    template<std::size_t I>
    constexpr auto& get() const & {
        if constexpr (I == 0) return a;
        else if constexpr (I == 1) return b;
        else if constexpr (I == 2) return c;
    }

    template<std::size_t I>
    constexpr auto&& get() const && {
        if constexpr (I == 0) return a;
        else if constexpr (I == 1) return b;
        else if constexpr (I == 2) return c;
    }
};

namespace std {
    template<> struct tuple_size<MyType> : std::integral_constant<std::size_t, 3> {};
    template<> struct tuple_element<0, MyType> : std::type_identity<int> {};
    template<> struct tuple_element<1, MyType> : std::type_identity<float> {};
    template<> struct tuple_element<2, MyType> : std::type_identity<std::string> {};
}


int main() {
    using namespace std::string_literals;
    using namespace kaixo;
    using kaixo::tuples::take;
    using kaixo::tuples::get;
    using kaixo::tuples::drop;
    using kaixo::tuples::drop_last;
    using kaixo::tuples::last;
    using kaixo::tuples::erase;
    using kaixo::tuples::insert;
    using kaixo::tuples::sub;
    using kaixo::tuples::swap;
    using kaixo::tuples::remove;
    using kaixo::tuples::keep;
    using kaixo::tuples::keep_raw;
    using kaixo::tuples::keep_indices;
    using kaixo::tuples::append;
    using kaixo::tuples::prepend;
    using kaixo::tuples::unique;
    using kaixo::tuples::filter;
    using kaixo::tuples::call;
    using kaixo::tuples::head;
    using kaixo::tuples::transform;
    using kaixo::tuples::elements;

    std::vector<int> ion;

    std::ranges::filter_view ae{ ion, [](int val) { return val == 0; } };


    auto sorin = [&](auto& val) { return val + 1; };

    info<int&, double&>::filter<!can_invoke<decltype(sorin)>>::size;

    info<int, double, float>::transform<partial<std::invoke_result_t, decltype(sorin)>::type>::size;

    constexpr std::tuple tpl{ 1, 1., 1.f, 1, 1., 1.f };

    decltype(take<2>)::tuple_pipe;

    constexpr auto soinr = invocable<decltype(take<2>), std::tuple<int, int>&>;

    constexpr auto isonr = tuples::pipe<decltype(take<2>), std::tuple<int, int>&>;


    auto sorin = tpl | take<5> | drop_last<1> | last<3> | erase<1>;

    int af = 1;

    constexpr auto soinsr = std::tuple{ 1, 1., 1.f, 1, 1., 1.f }
        | drop<2> | last<4> | erase<0> | swap<0>(1.) | keep_indices<0, 1, 2> 
        | sub<0, 1> | append(1, 1., 1.f) | prepend(1) | unique | filter<is_integral || is_floating_point>;

    constexpr auto soinrs = soinsr | transform([]<class Arg>(Arg&& arg) { return arg + 1; });

    constexpr auto sroinfg = call(soinsr, [](auto& ...args) { return (args + ...); });

    decltype(soinsr)::types::size;

    constexpr auto v1 = soinrs.get<0>();
    constexpr auto v2 = soinrs.get<1>();
    constexpr auto v3 = soinrs.get<2>();

    constexpr auto sorignsr = std::tuple{
        std::tuple{ 1, 2., 3.f },
        std::tuple{ 1., 2.f, 3 },
        std::tuple{ 1.f, 2, 3. },
    } | elements<0>;

    constexpr auto q1 = sorignsr.get<0>();
    constexpr auto q2 = sorignsr.get<1>();
    constexpr auto q3 = sorignsr.get<2>();

    //auto oisnr = tuples::all(soinsr);

    //auto oignr = insert<0>(sorin, 1, 2, 3);

    //using oinsr = tuples::all_t<decltype(sorin)>;

    //auto rsoni = sorin | insert<0>(1, 2, 3);

    //decltype(rsoni)::types::size;
    
    //rsoni.get<0>();
    //rsoni.get<1>();

    //auto rsgr1 = std::get<0>(std::move(resr));
    //auto rsgr2 = std::get<1>(resr);

    auto re = kaixo::tuples::all(std::move(tpl));

    auto fe = kaixo::tuples::all(re);

    //std::tuple res = tpl
    //    | append("Hello World"s, 3.)
    //    | prepend(0.)
    //    | reverse
    //    | remove_indices<2, 3>
    //    | take<3>
    //    | drop<1>;
    //
    //std::tuple res2 = zip(res, tpl);

    return 0;
}