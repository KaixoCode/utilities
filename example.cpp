#include <string_view>
#include <utility>
#include <algorithm>
#include <charconv>
#include <stdexcept>

#include "pack_utils.hpp"

#include <vector>

int main() {
    std::tuple<void, int, double, void, float>;

    using pack = kaixo::pack<int, float, double, float, long, unsigned>;
    using filtered = pack::filter<[]<class Ty>{ return sizeof(Ty) <= 4; }>;
    filtered::size;

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