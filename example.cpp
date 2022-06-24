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
    double a;
    double e;
    int f;
    int b;
    long d;
    char c;
};

int main() {
    using namespace kaixo;
    using struct_members = info<Struct>::members;

    using ce = not_convertible_to<Struct, const Struct&, Struct&&>;
    constexpr auto ginrg = std::convertible_to<ce, Struct>;

    constexpr auto aoine = std::constructible_from<Struct, ce, ce, ce, ce, ce, ce>;


    constexpr auto bs = info<Struct>::bytes;
    constexpr auto ms = info<Struct>::struct_size;
    constexpr auto ii = info<Struct>::indices<int>;
    constexpr auto ci = info<Struct>::count<int>;
    constexpr auto cc = info<Struct>::count<char>;
    constexpr auto hs = info<Struct>::has<short>;


    pack<void, void>::swap<1, int>::element<0>;

    fill_pack<void, 10>::swap<1, int>::element<0>;

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