
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

// ------------------------------------------------

#include "pack_utils/pack_utils.hpp"

// ------------------------------------------------

template<std::size_t I, class ...Args>
constexpr decltype(auto) forward_ith(Args&& ...args) {
    kaixo::template_pack<Args...> _args{ args... };
    return _args | kaixo::views::forward<I>;
}

struct MyStruct {

};

namespace std {

    template<>
    struct tuple_size<MyStruct> : std::integral_constant<std::size_t, 1> {};

}

using namespace kaixo;
using namespace tuples;
using namespace views;

double someFunction(tuple_view<int, double, int> value) {
    return value | fold<std::plus<>>;
}

void myEpicCode() {
    std::tuple<int, int, int> values{ 1, 2, 3 };

    auto pi = someFunction(values | take<2> | insert<1>(0.14159));


    std::cout << pi;
}

int main() {
    using namespace kaixo;
    using namespace tuples;
    using namespace views;
    myEpicCode();
    {
        std::tuple<float, int, int, float, double, int> tuple{ 0.f, 1, 2, 3.f, 4.0, 5 };
        float& result = std::get<0>(tuple | take<5> | drop<1> | drop_while<std::is_integral>);
        assert(result == 3.f);
    }
    {
        std::tuple<float, int, int, float, double, int> tuple{ 0.f, 1, 2, 3.f, 4.0, 5 };
        double& result = std::get<0>(tuple | unique | drop<2>);
        assert(result == 4.0);
    }    
    {
        std::tuple<float, int, int, float, double, int> tuple{ 0.f, 1, 2, 3.f, 4.0, 5 };
        double& result = std::get<0>(tuple | views::remove<int> | drop<2>);
        assert(result == 4.0);
    }
    {
        std::tuple<float, int, int, float, double, int> tuple{ 0.f, 1, 2, 3.f, 4.0, 5 };
        double& result = std::get<0>(tuple | erase_all<std::index_sequence<1, 2>> | drop<2>);
        assert(result == 4.0);
    }
    {
        std::tuple<float, int, int, float, double, int> tuple{ 0.f, 1, 2, 3.f, 4.0, 5 };
        double& result = std::get<0>(tuple | reverse | drop<1>);
        assert(result == 4.0);
    }
    {
        std::tuple<float, const long> tuple{ 0.f, 1 };

        int value = 193;
        const float num = 25;

        auto values = tuple | insert<1>(value, num, 3.) | reverse;

        float&       res0 = values | forward<4>;
        int&         res1 = values | forward<3>;
        const float& res2 = values | forward<2>;
        double       res3 = values | forward<1>;
        const long&  res4 = values | forward<0>;

        assert(res0 == 0.f);
        assert(res1 == value);
        assert(res2 == num);
        assert(res3 == 3.);
        assert(res4 == 1);
    }

    {
        std::tuple<int, double> a{ 1, 2.0 };
        std::tuple<float, long> b{ 3.f, 4 };

        auto values = concat(a, b);

        int&    res0 = values | forward<0>;
        double& res1 = values | forward<1>;
        float&  res2 = values | forward<2>;
        long&   res3 = values | forward<3>;
    }
    {

        constexpr std::tuple<int, double, float, long, char, long long> values{ 1, 3, 4, 5, 6, 7 };

        constexpr auto oinf = values | fold<std::plus<>>;

        constexpr auto efae = values | call([](auto ...args) { return (args + ...); });

        constexpr auto res = values | sub<0, 5> | append(8, 9, 10) | drop_while<std::is_integral>
                                    | filter<std::is_integral> | take<4> | reverse | last_unique 
                                    | fold<std::multiplies<>>;

        

    }

    constexpr std::tuple<int, double, float, int> feaefa{ 1, 2.0, 3.f, 4 };
    constexpr auto aoine = (std::move(feaefa) | last_unique | take_last<2> | forward<1>) == 4;

    std::tuple<int, double, int, float> vals{ 1, 1.3, 3, 4.f };

    constexpr auto oiane = view<empty_view>;

    auto res = vals | nth_unique<1>;

    std::tuple<int, double, float> values{ 1, 1.2, 3.f };

    using paeres = decltype(values | forward<0>);
    using oieine = decltype(values | drop_while<std::is_integral>);
    using efaefa = decltype(values | take_while<std::is_integral>);
    using efgrew = decltype(values | take_until<std::is_floating_point>);

    std::get<0>(values | all);
    std::get<0>(values | take<2>);

}