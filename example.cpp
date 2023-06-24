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
#include "kaixo/range_inserter.hpp"

using namespace kaixo;
using namespace kaixo::operators;
using namespace kaixo::overloads;
using namespace kaixo::default_variables;


struct aaa {
    struct {
        int a = 1;
        int b = 2;

        struct {
            int a = 3;
            int b = 4;
        } c;
    } a;
    int b = 5;
    int c = 6;
    int d = 7;
};


int main() {
    using namespace std::string_literals;


    auto oaine = range(1, 10);
    auto lc = (b | a <- range(1, 10), b <- views::stride(range(1, 10), a));


    for (auto a : lc) {
        std::cout << a << '\n';
    }

    return 0;
}