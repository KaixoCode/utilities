#include "list_comprehension.hpp"
#include <array>

#include <map>

int main() {

    using namespace kaixo;
    using namespace kaixo::lc_operators;

    constexpr auto a = var<"a">;
    constexpr auto b = var<"b">;
    constexpr auto c = var<"c">;

    constexpr auto res = lc[(a, b, c) | c <- range(0, inf), b <- range(1, 10), a <- range(1, 11), a * a + b * b == c * c];
    constexpr auto v1 = res[0];

    constexpr auto rs2 = lc[(a, b, c) | (a, (b, c)) <- (range(0, 10), range(0, 10), range(0, 10))];
    constexpr auto v2 = rs2[6];

    std::map<int, int> data{ { 1, 2 }, { 3, 4 }, { 5, 6 } };

    auto rs3 = lc[(a, b) | (a, b) <- data];

    for (auto i : rs3) {
        std::cout << std::get<0>(i) << ", " << std::get<1>(i) << '\n';
    }

    //for (auto i : res) {
    //    std::cout << std::get<0>(i) << ", " << std::get<1>(i) << ", " << std::get<2>(i) << '\n';
    //}


    //for (auto i : res)
    //{
    //    std::cout << std::get<0>(i) << ", " << std::get<1>(i) << ", " << std::get<2>(i) << ", " << std::get<3>(i) << '\n';
    //}

    //[](auto a, auto b, auto c, auto d) { return a + b + c + d; };

    return 0;
}

