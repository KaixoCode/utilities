#include "list_comprehension.hpp"


int main() {

    using namespace kaixo;
    using namespace kaixo::views;

    constexpr std::array a{ 1, 2, 3, 4 };
    constexpr std::array b{ 1, 2, 3, 4 };

    constexpr auto x = var<"x">;
    constexpr auto y = var<"y">;

    constexpr auto res = lc[(x, y) | x <- range(0, inf), y <- range(0, 10), x % 2 == 0, y % 2 == 0];

    for (auto i : res) {
        std::cout << std::get<0>(i) << ", " << std::get<1>(i) << '\n';
    }

    //for (auto i : res)
    //{
    //    std::cout << std::get<0>(i) << ", " << std::get<1>(i) << ", " << std::get<2>(i) << ", " << std::get<3>(i) << '\n';
    //}

    //[](auto a, auto b, auto c, auto d) { return a + b + c + d; };

    return 0;
}

