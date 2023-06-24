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

    std::map<int, std::string> names{
        { 3, "fizz" },
        { 5, "buzz "},
    };

    auto fizzbuzz = ((x, ranges::fold_left((b | (a, b) <- names, x % a == 0), ""s, std::plus{})) | x <- range(1, inf));
    
    for (auto [x, fb] : fizzbuzz) {
        std::cout << x << ": " << fb << "\n";
    }

    return 0;
}