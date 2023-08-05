#include <iostream>
#include <set>
#include <deque>
#include <vector>
#include <map>
#include <ranges>
#include <complex>
#include <list>
#include <tuple>
#include <fstream>



#include "kaixo/list_comprehension.hpp"
#include "kaixo/overloads.hpp"
#include "kaixo/range.hpp"
#include "kaixo/break.hpp"
#include "kaixo/zipped_range.hpp"




int main() {
    using namespace kaixo;
    using namespace kaixo::operators;
    using namespace kaixo::default_variables;

    auto r1 = ((a, b, c) | c <- range(1, 11), b <- range(1, c), a <- range(1, b), a * a + b * b == c * c);

    for (auto [a, b, c] : r1) {
        std::cout << "[" << a << ", " << b << ", " << c << "]\n";
    }

}