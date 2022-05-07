﻿#include <string_view>
#include <utility>
#include <algorithm>
#include <charconv>
#include <stdexcept>

#include "pack_utils.hpp"

#include <vector>

template<class Ty> requires (std::same_as<Ty, int>)
struct Test {};;





int main() {
    std::tuple<void, int, double, void, float>;

    using pack = kaixo::pack<int, float, double, float, long, unsigned>;
    using filtered = pack::filter<[]<std::integral Ty>{}>;
    filtered::size;
    pack::take<0>::size;

    // Tuple with lots of types, unordered
    using my_tuple = std::tuple<std::string, double, short, int, char>;
    // Sorter to sort types by their size
    constexpr auto my_sorter = []<class A, class B>{ return sizeof(A) < sizeof(B); };
    // Sort the tuple using the custom sorter
    using sorted_tuple = kaixo::as_pack<my_tuple>::sort<my_sorter>::as<std::tuple>;
    static_assert(std::same_as<sorted_tuple, std::tuple<char, short, int, double, std::string>>);


    static_assert(std::same_as<pack::element<3>, float>);
    static_assert(std::same_as<pack::head, int>);
    static_assert(std::same_as<pack::last, unsigned>);
    static_assert(pack::index<float> == 1);
    static_assert(pack::last_index<float> == 3);
    static_assert(pack::count<float> == 2);
    static_assert(!pack::occurs<char>);
    static_assert(pack::indices<float>[0] == 1);
    static_assert(pack::indices<float>[1] == 3);
    static_assert(std::same_as<pack::remove<float>::remove<double>, kaixo::pack<int, long, unsigned>>);
    static_assert(pack::last_index<std::string> == kaixo::npos);
    static_assert(std::same_as<pack::sub<2, 3>, kaixo::pack<double>>);

    using my_pack = kaixo::pack<double, double, double>;
    my_pack::insert<2, int>::size;

}