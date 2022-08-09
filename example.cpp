#include <iostream>
#include <string_view>
#include <array>

consteval std::string_view _enum_pretty_name(std::string_view name) noexcept {
    for (std::size_t i = name.size(); i > 0; --i) if (!((name[i - 1] >= '0' && name[i - 1] <= '9') 
        || (name[i - 1] >= 'a' && name[i - 1] <= 'z') || (name[i - 1] >= 'A' && name[i - 1] <= 'Z') 
        || (name[i - 1] == '_'))) { name.remove_prefix(i); break; }
    if (name.size() > 0 && ((name.front() >= 'a' && name.front() <= 'z') ||
        (name.front() >= 'A' && name.front() <= 'Z') || (name.front() == '_')))
        return name;
    return {}; 
}

template<class Ty, Ty Value> consteval std::string_view enum_name_impl() noexcept {
#if defined(__clang__) || defined(__GNUC__)
    return _enum_pretty_name({ __PRETTY_FUNCTION__, sizeof(__PRETTY_FUNCTION__) - 2 });
#elif defined(_MSC_VER)
    return _enum_pretty_name({ __FUNCSIG__, sizeof(__FUNCSIG__) - 17 });
#else
    return string_view{};
#endif
}

template<class Ty, auto V> constexpr auto enum_name = enum_name_impl<Ty, static_cast<Ty>(V)>();

template<auto Value> consteval auto value_name_impl() noexcept {
#if defined(__clang__) || defined(__GNUC__)
    constexpr auto name = std::string_view{ __PRETTY_FUNCTION__, sizeof(__PRETTY_FUNCTION__) - 2 };
    // Remove prefix, template argument starts at first '<'
    return name.substr(name.find_first_of('<') + 1);
#elif defined(_MSC_VER)
    constexpr auto name = std::string_view{ __FUNCSIG__, sizeof(__FUNCSIG__) - 17 };
    // Remove prefix, template argument starts at first '<'
    return name.substr(name.find_first_of('<') + 1);
#else
    return string_view{};
#endif
}

template<auto V> constexpr auto value_name = value_name_impl<V>();

enum Definitions { Fizz = 3, Buzz = 5, End };

template<Definitions...>
consteval std::string_view fizzBuzzString() {
    auto _view = std::string_view{ __FUNCSIG__ }.substr(89);
    return _view.substr(0, _view.size() - 7);
}

constexpr std::size_t dec_to_hex(std::size_t i) {
    std::size_t _res = 0;
    std::size_t _rPow = 1;
    while (i != 0) {
        std::size_t _n = i - (i / 10) * 10;
        _res += _n * _rPow;
        i /= 10;
        _rPow *= 16;
    }
    return _res;
}

template<std::size_t N, std::size_t ...Is>
constexpr std::string_view fizzBuzzStringI() {
    if constexpr (sizeof...(Is) == 0)
        return value_name<dec_to_hex(N)>.substr(3);
    else return fizzBuzzString<static_cast<Definitions>(Is)...>();
}

template<std::size_t I, std::size_t N, std::size_t ...Is>
constexpr std::string_view fizzBuzzI() {
    if constexpr (enum_name<Definitions, N> == "End")
        return fizzBuzzStringI<I, Is...>();
    else if constexpr (enum_name<Definitions, N> != "") {
        if constexpr (I % N == 0)
            return fizzBuzzI<I, N + 1ull, Is..., N>();
        else return fizzBuzzI<I, N + 1ull, Is...>();
    } else return fizzBuzzI<I, N + 1ull, Is...>();
}

template<std::size_t N>
constexpr std::array<std::string_view, N> fizzBuzz() {
    return[]<std::size_t ...Is>(std::index_sequence<Is...>) {
        return std::array{ fizzBuzzI<Is + 1ull, 1ull>()...};
    }(std::make_index_sequence<N>{});
}

int main() {

    std::array<std::string_view, 100> res = fizzBuzz<100>();

    for (auto& view : res) {
        std::cout << view << "\n";
    }

    return 0;
}