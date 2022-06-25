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


int func(double&, int, long&&);

template<kaixo::type_concepts::aggregate Ty> // require aggregate type
    requires (kaixo::info<Ty>::members::size > 1 // With at least 2 members
           && kaixo::info<Ty>::members::are_arithmetic) // And all members are arithmetic
constexpr auto sum_struct(const Ty& val) {
    using type_info = kaixo::info<Ty>; // Sum all members
    return kaixo::sequence<type_info::members::size>([&]<std::size_t ...Is>{
        return ((val.*type_info::template member<Is>) + ...);
    });
}


int main() {
    using namespace kaixo;

    Struct value;
    auto sum = sum_struct(value);


    using func_sig = info_v<&func>::signature;

    auto n = func_sig::type_name;
    auto r = func_sig::result::type_name;
    auto a1 = func_sig::arguments::element_info<0>::type_name;
    auto a2 = func_sig::arguments::element_info<1>::type_name;
    auto a3 = func_sig::arguments::element_info<2>::type_name;

    info<Struct>::members::indices<int>;

    static_assert(info<Struct>::offset<0> == offsetof(Struct, f));
    static_assert(info<Struct>::offset<1> == offsetof(Struct, a));
    static_assert(info<Struct>::offset<2> == offsetof(Struct, c));
    static_assert(info<Struct>::offset<3> == offsetof(Struct, e));
    static_assert(info<Struct>::offset<4> == offsetof(Struct, b));
    static_assert(info<Struct>::offset<5> == offsetof(Struct, d));



    Struct val;
    auto r1 = val.*info<Struct>::member<0>;
    auto r2 = val.*info<Struct>::member<1>;
    auto r3 = val.*info<Struct>::member<2>;
    auto r4 = val.*info<Struct>::member<3>;
    auto r5 = val.*info<Struct>::member<4>;
    auto r6 = val.*info<Struct>::member<5>;

    enum Fruit {
        Apple, 
        Banana, 
        Pear, 
        Orange,
        Size
    };


    Fruit my_fruit = Fruit::Apple;

    auto name = kaixo::enum_to_string(my_fruit);

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