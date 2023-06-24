#include <iostream>
#include <set>
#include <deque>
#include <map>

#include "kaixo/type_utils.hpp"
#include "kaixo/expression.hpp"
#include "kaixo/overloads.hpp"
#include "kaixo/list_comprehension.hpp"
#include "kaixo/range.hpp"
#include "kaixo/zipped_range.hpp"
#include "kaixo/range_inserter.hpp"

using namespace kaixo;
using namespace kaixo::operators;
using namespace kaixo::overloads;
using namespace kaixo::default_variables;


struct aaa {
    std::string b = "aaa[0]";

    struct {
        std::string a = "aaa[1][0]";
        std::string b = "aaa[1][1]";

        struct {
            std::string a = "aaa[1][2][0]";
            std::string b = "aaa[1][2][1]";
        } c;
    } a;

    std::string c = "aaa[2]";
    std::string d = "aaa[3]";
};

class bbb {
    std::string a = "bbb[0]";
    std::string b = "bbb[1]";

public:
    constexpr bbb() {}

    template <size_t I>
    auto& get()& {
        if constexpr (I == 0) return a;
        else if constexpr (I == 1) return b;
    }

    template <size_t I>
    auto const& get() const& {
        if constexpr (I == 0) return a;
        else if constexpr (I == 1) return b;
    }

    template <size_t I>
    auto&& get()&& {
        if constexpr (I == 0) return std::move(a);
        else if constexpr (I == 1) return std::move(b);
    }
};

namespace std {
    template<> struct tuple_size<bbb> : std::integral_constant<std::size_t, 2> {};
    template<> struct tuple_element<0, bbb> : std::type_identity<std::string> {};
    template<> struct tuple_element<1, bbb> : std::type_identity<std::string> {};
}

int main() {
    using namespace std::string_literals;

    std::vector<aaa> avals{ 1ull }; // aaa is an aggregate
    std::vector<bbb> bvals{ 1ull }; // bbb is a class with structured bindings defined.

    auto lc = ((a, b) | ((_, (a, _, _), _, _), (_, b)) <- (avals, bvals));

    for (auto [a, b] : lc) {
        std::cout << "[" << a << ", " << b << "]\n";
    }

    return 0;
}