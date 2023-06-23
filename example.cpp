#include <iostream>
#include <set>
#include <deque>
#include <map>

#include "kaixo/type_utils.hpp"
#include "kaixo/expression.hpp"
#include "kaixo/overloads.hpp"
#include "kaixo/list_comprehension.hpp"
#include "kaixo/range.hpp"

using namespace kaixo;
using namespace kaixo::operators;
using namespace kaixo::overloads;
using namespace kaixo::default_variables;


int main() {

    static_assert(
        same_as<
            info<
                std::tuple<int, double>,
                std::vector<double>,
                std::unordered_map<std::string, int>,
                std::list<long long>,
                std::set<float>,
                std::vector<std::string_view>,
                std::map<int, double>,
                std::deque<char>,
                std::array<std::string, 4>
            >

            ::filter<has::value_type && with<grab::value_type>(is_arithmetic)>

            ::when<with<grab::value_type>(is_integral)>
                ::transform<reinstantiate<int>::type>

            ::when<with<grab::value_type>(is_floating_point)>
                ::transform<reinstantiate<double>::type>
            
            ::sort<type_sorters::size>
        
            ::as<std::tuple>,

            std::tuple<
                std::set<double>,
                std::list<int>,
                std::vector<double>,
                std::deque<int>
            >
        >
    );

    return 0;
}