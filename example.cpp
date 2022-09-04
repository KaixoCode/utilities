#include <iostream>
#include <string_view>
#include <array>
#include <vector>
#include <fstream>
#include <string>
#include <ranges>
#include <algorithm>

#include <functional>

#include "type_utils.hpp"

template<class Tpl, class Lmb>
constexpr auto visit(std::size_t index, Tpl&& tuple, Lmb&& lambda) {
    using namespace kaixo;
    return generate_template_switch<as_info<decay_t<Tpl>>::size>([&]<std::size_t I>{
        return std::forward<Lmb>(lambda)(std::get<I>(std::forward<Tpl>(tuple)));
    })(index);
}


int main() {

    std::tuple<int, double, char> vals{ 1, 2, 3 };

    while (true) {
        int index = 0;
        std::cin >> index;

        visit(index, vals, [](auto v) {
            std::cout << "type: " << typeid(v).name() << "\n";
        });
    }

    return 0;
}