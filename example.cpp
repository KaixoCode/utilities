#include <iostream>
#include <set>
#include <deque>
#include <map>

#include "kaixo/type_utils.hpp"
#include "kaixo/expression.hpp"
#include "kaixo/overloads.hpp"
#include "kaixo/list_comprehension.hpp"
#include "kaixo/range.hpp"
#include "kaixo/zipped_range.hpp"

using namespace kaixo;
using namespace kaixo::operators;
using namespace kaixo::overloads;
using namespace kaixo::default_variables;


struct aaa {
    int a;
    int b;
    int c;
    int d;
};

int main() {

    constexpr var<"key"> key;

    std::map<int, aaa> vals{ 
        { 1, { 1, 2, 3, 4 } }, 
        { 2, { 3, 4, 5, 6 } }, 
        { 3, { 5, 6, 7, 8 } } 
    };

    auto lc = ((key, a, b) | (key, (a, b, _, _)) <- vals);

    for (auto [key, a, b] : lc) {
        std::cout << key << ": [" << a << ", " << b << "]\n";
    }

   // constexpr auto aoinf = struct_get_member<aaa, struct_size_v<aaa>>::get<0>(val);

    return 0;
}