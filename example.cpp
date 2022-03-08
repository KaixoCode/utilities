

#include <vector>
#include <iostream>
#include "utils.hpp"


struct X {

    void foo(int i) && {

    }

};


constexpr auto test = [](auto a, int b, double c) { return a * b * c; };

struct everything { template<class Ty> constexpr operator Ty(); };

template<class, class> struct argument_types;
template<class Ty, class ...Tys, std::invocable<Tys...> Lambda> struct argument_types<Lambda, std::tuple<Ty, Tys...>> { 
    using type = decltype(std::declval<Lambda>()(std::declval<Tys>()...))(Tys...);
};
template<class ...Tys, class Lambda> struct argument_types<Lambda, std::tuple<Tys...>> 
    : argument_types<Lambda, std::tuple<Tys..., everything>>{};
template<class Lambda> using argument_types_t = typename argument_types<Lambda, std::tuple<>>::type;

int main() {
    using aetype = argument_types_t<decltype(test)>;

    using namespace kaixo;
    constexpr auto i1 = tuple_index_v<int, std::tuple<double, short, float, int>>;
    constexpr auto i2 = in_tuple<int, std::tuple<double, short, float, int>>;

    using aieon = flatten_t<std::tuple<int, std::tuple<short, std::tuple<float, double>, int>, int>>;

    constexpr auto afeafa = detail::has_fun_op<decltype([]() {})>;

    using oaine = signature_t<decltype(&X::foo)>;

    aieon a;
    std::cout << a;

    return 0;
}

