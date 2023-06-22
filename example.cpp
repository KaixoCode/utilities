#include <iostream>



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


    info<int, double, int>
        ::transform<std::vector>
        ::tparams
        ::transform<drop_last<1>::type>
        ::transform<grab::type>
        ::bytes::size;

    return 0;
}