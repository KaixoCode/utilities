
// ------------------------------------------------

#include <iostream>
#include <fstream>
#include <string>
#include <source_location>
#include <vector>
#include <map>
#include <ranges>
#include <algorithm>
#include <utility>
#include <functional>
#include <cassert>
#include <set>
#include <expected>
#include <thread>
#include <any>
#include <mutex>
#include <type_traits>
#include <concepts>
#include <cstddef>
#include <string_view>
#include <unordered_set>
#include <regex>
#include <complex>

// ------------------------------------------------


namespace detail {
    template<class Ty>
    struct tuple_view_tuple {
        virtual const Ty& tuple() const = 0;
    };

    template<std::size_t I, class T, class Ty = void>
    struct tuple_view_get : virtual tuple_view_get<I, T>,
        virtual tuple_view_tuple<Ty> {
        virtual const T& get() const override { return std::get<I>(this->tuple()); }
    };
    template<std::size_t I, class T>
    struct tuple_view_get<I, T, void> {
        virtual const T& get() const = 0;
    };

    template<class, class, class...> struct tuple_view_impl;
    template<std::size_t ...Is, class ...Ts>
    struct tuple_view_impl<void, std::index_sequence<Is...>, Ts...> : virtual tuple_view_get<Is, Ts>... {};
    template<class Ty, std::size_t ...Is, class ...Ts>
    struct tuple_view_impl<Ty, std::index_sequence<Is...>, Ts...>
         : tuple_view_impl<void, std::index_sequence<Is...>, Ts...>, virtual tuple_view_get<Is, Ts, Ty>... 
    {
        template<class Arg> tuple_view_impl(Arg&& tpl) : m_Tuple(std::forward<Arg>(tpl)) {}
        virtual const Ty& tuple() const override { return m_Tuple; }
        Ty m_Tuple;
    };
}

template<class ...Tys>
class tuple_view {
public:
    template<class Ty> tuple_view(Ty&& tuple)
        : m_Ptr(std::make_unique<detail::tuple_view_impl<Ty, std::index_sequence_for<Tys...>, Tys...>>(std::forward<Ty>(tuple))) {}
    template<class ...Args> tuple_view(Args&& ...args)
        : tuple_view(std::forward_as_tuple(std::forward<Args>(args)...)) {}

    template<std::size_t I> decltype(auto) get() const {
        using T = std::tuple_element_t<I, std::tuple<Tys...>>;
        return dynamic_cast<detail::tuple_view_get<I, T>*>(m_Ptr.get())->get();
    }

    std::unique_ptr<detail::tuple_view_impl<void, std::index_sequence_for<Tys...>, Tys...>> m_Ptr{};
};



int myFun(tuple_view<int, float> val) {
    return val.get<0>() + val.get<1>();
}

int main() {
    std::tuple<int, double> tuple{ 1, 2. };
    std::pair<float, char> pair{ 1.f, 'a' };
    std::array<int, 2> arr{ 1, 2 };

    auto res1 = myFun(tuple);
    auto res2 = myFun(pair);
    auto res3 = myFun(arr);
    auto res4 = myFun({ 1, 2.f });

    return 0;
}

// ------------------------------------------------
