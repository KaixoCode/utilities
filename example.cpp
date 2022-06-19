#include <concepts>



#include "function_utils.hpp"


struct MyClass {
    int test(double&, float) volatile noexcept {}
}; 

int main() {
    using namespace kaixo;

    using info = function_info<decltype(&MyClass::test)>;
    
    static_assert(std::same_as<info::pointer, int(*)(double&, float) noexcept>);
    static_assert(std::same_as<info::minimal_pointer, int(*)(double&, float)>);
    static_assert(std::same_as<info::signature, int(double&, float) volatile noexcept>);
    static_assert(std::same_as<info::minimal_signature, int(double&, float)>);
    static_assert(std::same_as<info::object, volatile MyClass>);
    static_assert(std::same_as<info::minimal_object, MyClass>);
    static_assert(std::same_as<info::return_type, int>);
    static_assert(std::same_as<info::argument_types::element<0>, double&>);
    static_assert(std::same_as<info::argument_types::element<1>, float>);
    static_assert(info::is_volatile);
    static_assert(info::is_noexcept);


    //function_info<funt>::is_const;
    //function_info<funt>::is_volatile;
    //function_info<funt>::is_lvalue;
    //function_info<funt>::is_rvalue;
    //function_info<funt>::is_reference;
    //function_info<funt>::is_noexcept;
    //function_info<funt>::pointer;

    return 0;
}