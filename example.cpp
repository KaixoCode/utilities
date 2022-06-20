#include <concepts>



#include "utils.hpp"

#include <iostream>


struct O {
    int V(double const) {}
    int Q(this O&, double const) {}
};

enum class Fruit { 
    Apple, 
    Pear,
    Banana,
    Mango,
    Grape,
    Orange,
    Count
};

template<kaixo::string_literal Str>
struct my_str {

};

int main() {

    my_str<"helloworld"> a;

    using namespace kaixo;


    info<int&&>::add_lvalue_reference;

    using oina = info<int(*)(int)>::add_const::type;

    static_assert(std::same_as<oina, int(* const)(int)>);

    using my_tuple = std::tuple<int, double, char>;
    using new_tuple = as_pack<my_tuple>::pack_info::copy_cvref_from<const int&&>::as<std::tuple>;

    new_tuple;

    info<int(O::* const volatile&&)(int)>::remove_reference::is_member_function_pointer;

    pack_info<int(), void(), double()>::bytes;

    return 0;
}