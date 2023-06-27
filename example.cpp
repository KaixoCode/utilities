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

using namespace kaixo;
template<class ...Args>
constexpr decltype(auto) fun(Args&& ...args) {
    using kaixo::tuple::take;
    using kaixo::tuple::reverse;
    using kaixo::tuple::unique;

    template_pack<Args...> pack{ args... };

    return pack | unique | take<3> | reverse;
}

int main() {
    using namespace std::string_literals;
    using namespace kaixo;
    using kaixo::tuple::take;
    using kaixo::tuple::drop;
    using kaixo::tuple::remove_indices;
    using kaixo::tuple::get;
    using kaixo::tuple::last;
    using kaixo::tuple::insert;
    using kaixo::tuple::append;
    using kaixo::tuple::prepend;
    using kaixo::tuple::reverse;
    using kaixo::tuple::zip;
    using kaixo::tuple::unique;


    std::tuple tpl{ 1, 2., 4.f };

    std::tuple res = tpl
        | append("Hello World"s, 3.)
        | prepend(0.)
        | reverse
        | remove_indices<2, 3>
        | take<3>
        | drop<1>;

    std::tuple res2 = zip(res, tpl);

    return 0;
}