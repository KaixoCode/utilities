#include <iostream>
#include <set>
#include <deque>
#include <vector>
#include <map>
#include <ranges>

#include "kaixo/type_utils.hpp"


struct MyType {
    MyType() { 
        std::cout << "Construct\n"; 
    }

    MyType(MyType&& v) noexcept
        : a(v.a), b(v.b), c(v.c) { 
        std::cout << "Move\n"; 
    }

    MyType(const MyType& v)
        : a(v.a), b(v.b), c(v.c) {
        std::cout << "Copy\n";
    }
    
    MyType& operator=(MyType&& v) noexcept {
        a = v.a, b = v.b, c = v.c;
        std::cout << "Move Assign\n"; 
        return *this;
    }

    MyType& operator=(const MyType& v) {
        a = v.a, b = v.b, c = v.c;
        std::cout << "Copy Assign\n";
        return *this;
    }

    ~MyType() {
        std::cout << "Destruct\n";
    }

    int a = 1;
    float b = 2;
    std::string c = "Woof";

    template<std::size_t I>
    constexpr auto& get() & {
        if constexpr (I == 0) return a;
        else if constexpr (I == 1) return b;
        else if constexpr (I == 2) return c;
    }

    template<std::size_t I>
    constexpr auto&& get() && {
        if constexpr (I == 0) return a;
        else if constexpr (I == 1) return b;
        else if constexpr (I == 2) return c;
    }
    template<std::size_t I>
    constexpr auto& get() const & {
        if constexpr (I == 0) return a;
        else if constexpr (I == 1) return b;
        else if constexpr (I == 2) return c;
    }

    template<std::size_t I>
    constexpr auto&& get() const && {
        if constexpr (I == 0) return a;
        else if constexpr (I == 1) return b;
        else if constexpr (I == 2) return c;
    }
};

namespace std {
    template<> struct tuple_size<MyType> : std::integral_constant<std::size_t, 3> {};
    template<> struct tuple_element<0, MyType> : std::type_identity<int> {};
    template<> struct tuple_element<1, MyType> : std::type_identity<float> {};
    template<> struct tuple_element<2, MyType> : std::type_identity<std::string> {};
}

template<kaixo::is_type_trait T, class Arg>
struct test_trait {
    Arg arg;

    template<class ...Args>
    constexpr bool value() const {
        return T::template value<Args...> == arg;
    }
};

template<kaixo::is_type_trait T, class Arg>
constexpr auto operator==(T, Arg&& val) {
    return test_trait<T, Arg>{ std::forward<Arg>(val) };
}

using namespace kaixo;

int main() {

    constexpr auto sionr = (sizeof_v<> == 4).template evaluate<uint32_t>();

    using namespace kaixo::pack;

    //using namespace kaixo::pack;

    //kaixo::info<kaixo::info<char, int>, kaixo::info<float, double>>::filter<contains_all_v<char, int>>::size;
    //
    //contains_v<int>.value<std::tuple<int, double>>;
    //
    //contains_all<kaixo::info<int, double>, kaixo::info<int, double, float>>::value;
    //
    //contains_all<kaixo::info<int>>::value;
    //
    //kaixo::info<kaixo::info<char, int>, kaixo::info<float, double>>::filter<count_v<int> == 1>::size;
    //
    //
    //kaixo::info<int, double, char, long>::filter<(contains_v<int> > 2)>::size;
    //
    //kaixo::sizeof_v<> == 2;
    //
    //kaixo::sizeof_v<>;
    //element_t<0>;

    return 0;
}