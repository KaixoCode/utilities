#include <concepts>



#include "utils.hpp"

#include <iostream>




class Test {

};

namespace kaixo {
    template<class ...Packs>
    using zip = decltype(kaixo::sequence<std::min({ Packs::size... })>([]<std::size_t ...Is>{

        return kaixo::pack<Packs::template element<Is>...>{};
    }));

}



int main() {

    std::tuple<int, double, float, long> tuple;
    
    constexpr auto aion = kaixo::invocable_no_conversions<decltype([](int) {}), double&>;

    kaixo::tuple_for(tuple, 
        [](int v) { std::cout << "int\n"; },
        [](double v) { std::cout << "double\n"; },
        [](std::integral auto v) { std::cout << "integral\n"; },
        [](std::floating_point auto v) { std::cout << "floating\n"; }
    );

    return 0;
}