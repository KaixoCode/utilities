#include "list_comprehension.hpp"
#include <array>

#include <map>
#include <list>
#include <vector>


template<template<class ...> class Fun>
struct aaaaa {};

int main() {

    using namespace kaixo;
    using namespace kaixo::lc_operators;

    constexpr auto a = var<"a">;
    constexpr auto b = var<"b">;
    constexpr auto c = var<"c">;

    constexpr auto res = lc[(a, b, c) | c <- range(0, inf), b <- range(1, 10), a <- range(1, 11), a * a + b * b == c * c];
    constexpr auto v1 = res[0];

    constexpr auto rs2 = lc[(a + b + c) | (a, b, c) <- (range(0, 10), range(0, 10), range(0, 10))];
    constexpr auto v2 = rs2[6];

    constexpr auto rs4 = lc[(a, b, c) | (a, b) <- lc[(a, b) | a <- range(0, inf), b <- range(0, 10)], c <- range(0, 10)];
    constexpr auto v4 = rs4[106];

    constexpr auto rs5 = lc[c | a <- range(0ll, inf), b <<= a * a, c <<= b * b, a * 100 < c, c != 100];
    constexpr auto v5 = rs5[3099];

    using af = tuple_cat_t<std::tuple<int, int>, std::tuple<int>>;



    //constexpr auto v5 = rs5[4];
    //using aaaea = decltype(std::tuple_cat(
    //    std::declval<typename decltype(rs5)::container_type::names::names>(), 
    //    std::declval<typename var_alias_names<decltype(rs5)::var_aliases>::type>()));
    //

    //tuple_with_names<tags<"a", "b", "c">, std::tuple<int, int, int>> apple;
    //apple.assign(tuple_with_names<tags<"a", "b", "c">, std::tuple<int, int, int>>{ std::tuple{ 0, 0, 0 } });

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

