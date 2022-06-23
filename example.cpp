#include <concepts>



#include "utils.hpp"

#include <iostream>



enum class Fruit { 
    Apple, 
    Pear,
    Banana,
    Mango,
    Grape,
    Orange,
    Count
};


template<class Ty>
struct Aeofina {
    int Vaefaefa(const std::string&, int&&);
};



struct member_access {
    void* ptr;

    template<class Ty>
    constexpr operator Ty&(){
        return *static_cast<Ty*>(ptr);
    }
};

template<class Obj>
member_access operator->*(Obj& obj, std::uint32_t offset) {
    // (sizeof(Ty) + offset <= sizeof(Obj))
    return { static_cast<void*>(&(obj.*std::bit_cast<uint8_t Obj::*>(offset))) };
}

template<std::size_t I>
struct meta {
    uint8_t _data[I];
};


template<auto V>
struct constant_v {
    constexpr static auto value = V;
};

template<char ...S>
consteval std::size_t to_index() {
    constexpr std::size_t size = sizeof...(S);
    using values = kaixo::to_pack<static_cast<int>(S - '0')...>;
    return [&]<std::size_t ...Is>(std::index_sequence<Is...>) {
        std::size_t m = 1;
        std::size_t res = 0;
        ((res += m * (values::template element<size - Is - 1>), m *= 10), ...);
        return res;
    }(std::make_index_sequence<size>{});
}

template<char ...S>
    requires ((S >= '0' && S <= '9') && ...)
consteval constant_v<to_index<S...>()> operator""_() {
    return {};
}

template<class ...Tys>
struct tuple : std::tuple<Tys...> {
    using std::tuple<Tys...>::tuple;
    using types = kaixo::pack<Tys...>;

    template<class Self, std::size_t I>
    constexpr auto operator[](this Self&& self, constant_v<I>)
        noexcept(noexcept(std::get<I>(std::forward<Self>(self))))
              -> decltype(std::get<I>(std::forward<Self>(self))) {
        return            std::get<I>(std::forward<Self>(self));
    }
};

struct CC {
    int v = 10;
    CC(int v) : v(v) {}
};

#include <vector>

#include "multi_initializer_list.hpp"

constexpr auto test(kaixo::multi_initializer_list<int, double, float> v) {
    auto& ints = v.get<int>();
    auto& doubles = v.get<double>();
    auto& floats = v.get<float>();

    int isum = 0;
    double dsum = 0;
    float fsum = 0;
    for (auto& i : ints) isum += i;
    for (auto& d : doubles) dsum += d;
    for (auto& f : floats) fsum += f;

    return std::tuple{ isum, dsum, fsum };
}

int main() {

    int afae = 1;
    constexpr auto res = test({ afae, 11., .4, 1, 3, 4, 3., 5 });
    


    using namespace kaixo;

    pack_info<const int&&, const volatile double&, volatile float>::remove_const;

    info<const int&&>::is_const;

    using type = info<const int&&>::copy_cv_from<int>::type;
    static_assert(std::same_as<int&&, type>);

    constexpr auto aei = to_index<'1', '2'>();

    constexpr tuple<int, double, float> tup{ 1, 2, 3 };

    auto v1 = tup[0_];
    auto v2 = tup[1_];
    auto v3 = tup[2_];

    CC a{ 6424 };
    CC b{ 1 };

    RTTI_Ftable<void> ftable = RTTI_Ftable<CC>{};
    ftable.copy_assign(&a, &b);
    ftable.deleter_in_place(&a);

    return 0;
}