#include <string_view>
#include <utility>
#include <algorithm>
#include <charconv>
#include <stdexcept>

#include "callable.hpp"

#include <iostream>


int main() {
    using namespace kaixo;

    callable clbl = [](auto v) { std::cout << "called 1 with type: " << typeid(v).name() << '\n'; };

    struct my_struct {};

    clbl(my_struct{});

    callable clbl2 = [](auto v, auto a) { std::cout << "called 2 with type: " << typeid(v).name() << '\n'; };

    clbl2(my_struct{}, int{});
    clbl(my_struct{});
    clbl(int{});

    return 0;
}