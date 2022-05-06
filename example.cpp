#include <string_view>
#include <utility>
#include <algorithm>
#include <charconv>
#include <stdexcept>

#include "pack_utils.hpp"

int main() {
    std::tuple<void, int, double, void, float>;

    using pack = kaixo::pack<int, float, double, float, long, unsigned>;

    static_assert(std::same_as<pack::element<3>, float>);
    static_assert(std::same_as<pack::head, int>);
    static_assert(std::same_as<pack::last, unsigned>);
    static_assert(pack::index<float> == 1);
    static_assert(pack::last_index<float> == 3);
    static_assert(pack::count<float> == 2);
    static_assert(!pack::occurs<char>);
    static_assert(pack::indices<float>[0] == 1);
    static_assert(pack::indices<float>[1] == 3);
    static_assert(std::same_as<pack::remove<float>::remove<double>, 
        kaixo::pack<int, long, unsigned>>);

    using my_complicated_tuple = std::tuple<int, float, double, float, long, unsigned>;

    kaixo::pack<my_complicated_tuple>::indices<float>;


}