#include <concepts>



#include "utils.hpp"


struct MyClass {
    double a;
    double b;
    int test(double&, float) const noexcept {}
};


int main() {
    using namespace kaixo;
    using namespace kaixo::type_concepts;

    using my_info = info<decltype(&MyClass::test)>;

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

    

    using lambda = decltype([](const int, volatile double&, float&&) {});
    using args = info<lambda>::argument_types;
    using decayed_args = args::decay;
    using filtered = decayed_args::filter<[]<floating_point>{}>;

    static_assert(std::same_as<filtered::element<0>, double>);
    static_assert(std::same_as<filtered::element<1>, float>);


    //function_info<funt>::is_const;
    //function_info<funt>::is_volatile;
    //function_info<funt>::is_lvalue;
    //function_info<funt>::is_rvalue;
    //function_info<funt>::is_reference;
    //function_info<funt>::is_noexcept;
    //function_info<funt>::pointer;

    return 0;
}