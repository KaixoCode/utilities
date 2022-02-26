#include "list_comprehension.hpp"
#include <array>

int main() {

    using namespace kaixo;
    using namespace kaixo::lc_operators;

    
    constexpr auto a = var<"a">;
    constexpr auto b = var<"b">;
    constexpr auto c = var<"c">;

    constexpr auto res = lc[(a, b, c) | c <- range(1, 11), b <- range(1, 11), a <- range(1, 11), a*a + b*b == c*c];
    constexpr auto v1 = res[2];

    constexpr auto rs2 = lc[(a, b, c) | (a, (b, c)) <- (range(0, 10), range(0, 10), range(0, 10))];
    constexpr auto v2 = rs2[6];

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

