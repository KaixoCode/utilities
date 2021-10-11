#include "smart_tuple.hpp"
#include "pa_function.hpp"
#include "lambda.hpp"
#include "struct_tuple.hpp"
#include "type_linker.hpp"
#include "constexpr_counter.hpp"

using namespace kaixo;

int Add(int a, int b) { return a + b; }
struct Object { int Add(int a, int b) { return a + b; } };

struct Functor
{
    int someValue;
    int operator()(int a, int b) { return a + b + someValue; }
};

void pa_function_example()
{
    // Functor
    pa_function<int(int, int)> functor = Functor{ 5 };
    pa_function<int(int)> functor2 = functor(1);
    int functorResult = functor2(3);
    assert(functorResult == 9);

    // Lambda
    pa_function<int(int, int)> lambda = [](int a, int b) { return a + b; };
    pa_function<int(int)> lambda2 = lambda(1);
    int lambdaResult = lambda2(3);
    assert(lambdaResult == 4);

    // Function Pointer
    pa_function<int(int, int)> funPtr = Add;
    pa_function<int(int)> funPtr2 = funPtr(1);
    int funPtrResult = funPtr2(3);
    assert(funPtrResult == 4);

    // Member function
    Object obj;
    pa_function<int(int, int)> memberFun = { &Object::Add, obj };
    pa_function<int(int)> memberFun2 = memberFun(1);
    int memberFunResult = memberFun2(3);
    assert(memberFunResult == 4);
}

void smart_tuple_example()
{
    smart_tuple<int, float, double> thing{ 1, 0.5f, 4. };

    // By value
    int val1 = thing[0];
    float val2 = thing[1];
    double val3 = thing[2];
    assert(val1 == 1);
    assert(val2 == 0.5f);
    assert(val3 == 4.);

    // By reference
    int& val4 = thing[0];
    val4++;
    int val5 = thing[0];
    assert(val4 == val5);
}

void lambda_example()
{
    //int a = 0;
    //double b = 1;

    //lambda t = [=](int c) -> int { return a + b + c; };

    //std::cout << typeid(decltype(t)).name() << std::endl;

    //t.get<0>() += 10;
    //t.get<1>() += 10;

    //std::cout << t(1) << std::endl;
}

class Thing
{
public:
    Thing(int a, short b, long long c, double d, float e) {}
};

struct Apple {
    short q = 1;
    long long d = 2;
    int a = 3;
    double b = 4;
    float c = 5;
};

void struct_tuple_example()
{
    //Apple apple;
    //
    //constexpr size_t fields = struct_info<Apple>::fields;
    //using tuple = struct_info<Apple>::field_types;
    //std::cout << fields << std::endl;
    //std::cout << typeid(tuple).name() << std::endl;
    //
    //tuple _tuple = as_tuple(apple);
    //std::cout << typeid(decltype(_tuple)).name() << std::endl;
    //std::cout << std::get<0>(_tuple) << std::endl;
    //std::cout << std::get<1>(_tuple) << std::endl;
    //std::cout << std::get<2>(_tuple) << std::endl;
    //std::cout << std::get<3>(_tuple) << std::endl;
    //std::cout << std::get<4>(_tuple) << std::endl;
    //
    //constexpr size_t args = constructor_info<Thing>::args;
    //std::cout << args << std::endl;
    //std::cout << typeid(constructor_info<Thing>::arg_types).name() << std::endl;
}


void type_linker_example()
{
    //link_types<type_group<double, double>, type_group<int, short>>;
    //link_types<type_group<short, long>, type_group<char, float>>;
    //
    //using type1 = linked_types<int, short>;
    //using type2 = linked_types<char, float>;
    //
    //std::cout << typeid(type1).name() << std::endl;
    //std::cout << typeid(type2).name() << std::endl;
}

// Instantiate auto return friend functions using overload index:
//   magic_friend(member<Type, Index, Overload>)
// and increment overload index after each find, and check each
// iteration to make sure there's at least 1 different argument
// relative to each previous overload. And if there isn't, 
// stop incrementing overload index and finalize.





template<size_t>
struct number {};

template <bool b1, bool b2>
constexpr static bool and_val = false;

// template specialization
template <bool b2>
constexpr static bool and_val<true, b2> = b2;

template<class Type, class ToCheck, size_t Overload, size_t Index>
struct not_same {
    constexpr static inline bool value = not_same<Type, ToCheck, Overload - 1, Index>::value 
        && !std::is_same_v<linked_types<Type, number<Overload>, number<Index>>, ToCheck>;
};

template<class Type, class ToCheck, size_t Index>
struct not_same<Type, ToCheck, 0, Index> {
    constexpr static inline bool value = true;
};

template<class Type, class Converted, size_t Overload, size_t Index>
struct actual_linker {
    constexpr static inline bool value = (link_types<type_group<Converted>, type_group<Type, number<Overload>, number<Index>>>, true);
};

template<class Type, size_t Overload, size_t Index>
struct converter {
    template<class Converted> requires (std::conditional_t<not_same<Type, Converted, Overload - 1, Index>::value, 
        actual_linker<Type, Converted, Overload, Index>, std::bool_constant<false>>::value)
    operator Converted();
};

template<class Type, size_t Overload, size_t ...Is>
concept find_overload = requires() {
    new Type(converter<Type, Overload, Is>{}...);
};

struct Woof {
    Woof(double, double, short, char) {};
    Woof(int, int, int) {};
    Woof(std::string, float, float) {};
};


int main()
{
    number<incr::get()> nmr1;
    number<incr::get()> nmr2;
    number<incr::get()> nmr3;
    number<incr::get()> nmr4;
    number<incr::get()> nmr5;

    std::cout << typeid(nmr1).name() << std::endl;    
    std::cout << typeid(nmr2).name() << std::endl;   
    std::cout << typeid(nmr3).name() << std::endl;   
    std::cout << typeid(nmr4).name() << std::endl;
    std::cout << typeid(nmr5).name() << std::endl;



    //new Woof{ converter<Woof, 1, 0>{}, converter<Woof, 1, 1>{} };
    //constexpr bool v3 = find_overload<Woof, 1, 0, 1, 2, 3>;
    //constexpr bool v2 = find_overload<Woof, 2, 0, 1, 2>;
    //constexpr bool v1 = find_overload<Woof, 3, 0, 1, 2>;
   //// link_types<type_group<int>, type_group<Woof, number<1>, number<0>>>;
    ////link_types<type_group<int>, type_group<Woof, number<1>, number<1>>>;
    //std::cout << typeid(linked_types<Woof, number<1>, number<0>>).name() << std::endl;
    //std::cout << typeid(linked_types<Woof, number<1>, number<1>>).name() << std::endl;
    //std::cout << typeid(linked_types<Woof, number<1>, number<2>>).name() << std::endl;
    //std::cout << typeid(linked_types<Woof, number<1>, number<3>>).name() << std::endl;
    //std::cout << std::endl;
    //std::cout << typeid(linked_types<Woof, number<2>, number<0>>).name() << std::endl;
    //std::cout << typeid(linked_types<Woof, number<2>, number<1>>).name() << std::endl;
    //std::cout << typeid(linked_types<Woof, number<2>, number<2>>).name() << std::endl;
    //std::cout << std::endl;
    //std::cout << typeid(linked_types<Woof, number<3>, number<0>>).name() << std::endl;
    //std::cout << typeid(linked_types<Woof, number<3>, number<1>>).name() << std::endl;
    //std::cout << typeid(linked_types<Woof, number<3>, number<2>>).name() << std::endl;

    //std::cout << v1 << std::endl;

    //constexpr bool a1 = find_overload<Woof, 0, 0>;
    //constexpr bool a2 = find_overload<Woof, 1, 0>;

    //using type = decltype(fun_magic(argument<Woof, 0, 0>{}));

    //std::cout << typeid(arguments_at_overload<Woof, 1>).name() << std::endl;
    //std::cout << typeid(decltype(fun_magic(argument<Woof, 1, 0>{}))::type).name() << std::endl;

    type_linker_example();
    pa_function_example();
    smart_tuple_example();
    lambda_example();
    struct_tuple_example();
}