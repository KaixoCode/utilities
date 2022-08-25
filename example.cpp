#include <iostream>
#include <string_view>
#include <array>
#include <vector>
#include <fstream>
#include <string>
#include <ranges>
#include <algorithm>

#include <functional>

#include "list_comprehension.hpp"

using namespace kaixo;

int main() {

    std::vector<int> a{ 1, 2, 3, 4, 5, 6 };
    std::vector<int> b{ 1, 2, 3, 4, 5, 6 };
    std::vector<int> c{ 1, 2, 3, 4, 5, 6 };

    constexpr var<"x"> x;
    constexpr var<"y"> y;
    constexpr var<"z"> z;

    auto v = lc[(x, y, z) | x <- a, x % 2 == 0, y <- b, x > y, z <- c, z == x + y];

    auto aeoinf = lc[(x, y) | x <- a, y <- b, brk <<= y == 4];

    auto nae = *aeoinf.begin();

    for (auto [a, b] : aeoinf) {
        std::cout << a << ", " << b << "\n";
    }

    for (auto [a, b, c] : v) {
        std::cout << a << ", " << b << ", " << c << "\n";
    }

    return 0;
}