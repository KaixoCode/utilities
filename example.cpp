#include <concepts>



#include "utils.hpp"




struct MyClass {
    int test(double&, float) const noexcept {}
};


int main() {
    using namespace kaixo;

    using my_info = info<decltype(MyClass::test)>;

    my_info::argument_types::sort<[]<class A, class B>{ return sizeof(A) < sizeof(B); }>;


    static_assert(std::same_as<my_info::pointer, int(*)(double&, float) noexcept>);
    static_assert(std::same_as<my_info::minimal_pointer, int(*)(double&, float)>);
    static_assert(std::same_as<my_info::signature, int(double&, float) const noexcept>);
    static_assert(std::same_as<my_info::minimal_signature, int(double&, float)>);
    static_assert(std::same_as<my_info::object, const MyClass>);
    static_assert(std::same_as<my_info::minimal_object, MyClass>);
    static_assert(std::same_as<my_info::return_type, int>);
    static_assert(std::same_as<my_info::argument_types::element<0>, double&>);
    static_assert(std::same_as<my_info::argument_types::element<1>, float>);
    static_assert(my_info::is_const);
    static_assert(my_info::is_noexcept);


    //function_info<funt>::is_const;
    //function_info<funt>::is_volatile;
    //function_info<funt>::is_lvalue;
    //function_info<funt>::is_rvalue;
    //function_info<funt>::is_reference;
    //function_info<funt>::is_noexcept;
    //function_info<funt>::pointer;

    return 0;
}