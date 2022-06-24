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


int main() {



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
    

    kaixo::pack<int, double>::append<int>::unique_count;

    constexpr auto aion = kaixo::invocable_no_conversions<decltype([](double) {}), double&>;

    

    kaixo::tuple_for(tuple, 
        [](int v) { std::cout << "int\n"; },
        [](double v) { std::cout << "double\n"; },
        [](std::integral auto v) { std::cout << "integral\n"; },
        [](std::floating_point auto v) { std::cout << "floating\n"; }
    );

    return 0;
}