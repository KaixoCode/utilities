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
    std::string a = "aaa[0]";

    struct B {
        std::string a = "aaa[1][0]";
        std::string b = "aaa[1][1]";

        struct C {
            std::string a = "aaa[1][2][0]";
            std::string b = "aaa[1][2][1]";
        } c;
    } b;

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

namespace kaixo {
    template<class C, class B, class E>
    struct iff {
        using depend = concat_t<depend<C>, depend<B>, depend<E>>;

        C condition;
        B body;
        E otherwise;

        template<class Self>
        constexpr auto evaluate(this Self&& self, is_named_tuple auto& tpl) {
            constexpr auto c1 = kaixo::depend<decay_t<decltype(kaixo::evaluate(std::forward<Self>(self).condition, tpl))>>::size == 0;
            constexpr auto c2 = kaixo::depend<decay_t<decltype(kaixo::evaluate(std::forward<Self>(self).body, tpl))>>::size == 0;
            constexpr auto c3 = kaixo::depend<decay_t<decltype(kaixo::evaluate(std::forward<Self>(self).otherwise, tpl))>>::size == 0;
            constexpr auto complete = c1 && c2 && c3;
            if constexpr (complete) {
                if (bool(kaixo::evaluate(std::forward<Self>(self).condition, tpl)))
                    return kaixo::evaluate(std::forward<Self>(self).body, tpl);
                else return kaixo::evaluate(std::forward<Self>(self).otherwise, tpl);
            }
            else {
                return kaixo::iff{
                    kaixo::evaluate(std::forward<Self>(self).condition, tpl),
                    kaixo::evaluate(std::forward<Self>(self).body, tpl),
                    kaixo::evaluate(std::forward<Self>(self).otherwise, tpl),
                };
            }
        }
    };
}


int main() {
    using namespace std::string_literals;

    std::vector<aaa> avals{ 1ull }; // aaa is an aggregate
    std::vector<bbb> bvals{ 1ull }; // bbb is a class with structured bindings defined.

    auto lc = ((a, b, c) | (x, y) <- (avals, bvals), (_, (a, b, _), _, _) = x, (_, c) = y);
    
    for (auto [a, b, c] : lc) {
        std::cout << "[" << a << ", " << b << ", " << c << "]\n";
    }

    std::vector<std::pair<int, std::string>> vals{
        { 3, "Fizz" },
        { 5, "Buzz" },
    };

    return 0;
}