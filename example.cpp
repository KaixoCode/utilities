#include <concepts>

#include <bit>

#include "utils.hpp"

#include <iostream>








#include <variant>

template<class ...Args, class ...Functors>
constexpr void visit(std::variant<Args...>& v, Functors&& ...functors) {
    const kaixo::overloaded _overloaded{ std::forward<Functors>(functors)... };
    using types = kaixo::pack<Args...>;
    kaixo::generate_template_switch<sizeof...(Args)>(
        [&]<std::size_t I> { 
            using type = typename types::template element<I>;
            _overloaded(std::get<type>(v));
        }
    )(v.index());
}



void myfun(int, double, float, char, long) {}

namespace kaixo {

}

struct Struct {
    int f = 3;
    double a = 1;
    char c = 6;
    double e = 2;
    int b = 4;
    long d = 5;
};

int main() {
    using namespace kaixo;

    static_assert(info<Struct>::offset<0> == offsetof(Struct, f));
    static_assert(info<Struct>::offset<1> == offsetof(Struct, a));
    static_assert(info<Struct>::offset<2> == offsetof(Struct, c));
    static_assert(info<Struct>::offset<3> == offsetof(Struct, e));
    static_assert(info<Struct>::offset<4> == offsetof(Struct, b));
    static_assert(info<Struct>::offset<5> == offsetof(Struct, d));

    Struct val;
    auto r1 = val.*info<Struct>::member<0>();
    auto r2 = val.*info<Struct>::member<1>();
    auto r3 = val.*info<Struct>::member<2>();
    auto r4 = val.*info<Struct>::member<3>();
    auto r5 = val.*info<Struct>::member<4>();
    auto r6 = val.*info<Struct>::member<5>();

    enum Fruit {
        Apple, 
        Banana, 
        Pear, 
        Orange,
        Size
    };

    auto v1 = kaixo::enum_to_string(Fruit::Apple);
    auto v2 = kaixo::enum_to_string(Fruit::Banana);
    auto v3 = kaixo::enum_to_string(Fruit::Pear);
    auto v4 = kaixo::enum_to_string(Fruit::Orange);

    std::tuple<int, double, float, long> tuple;
    


    constexpr auto aion = kaixo::invocable_no_conversions<decltype([](double) {}), double&>;

    

    kaixo::tuple_for(tuple, 
        [](int v) { std::cout << "int\n"; },
        [](double v) { std::cout << "double\n"; },
        [](std::integral auto v) { std::cout << "integral\n"; },
        [](std::floating_point auto v) { std::cout << "floating\n"; }
    );

    return 0;
}