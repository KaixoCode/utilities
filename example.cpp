

#include <vector>
#include <iostream>
#include "utils.hpp"

#include <variant>


// -- header file --
#include <iostream>
struct override;

template<class = override, class...>
inline auto logger = std::clog;

template<class... Tys, class T>
void log(T t) { logger<override, Tys...> << t; }

// -- cpp file --
#include <fstream>

template<> auto logger<override> = std::ofstream{ "log.txt" };

struct dud {};
template<auto Impl, class A = dud>
struct infix {
    A a;

    template<class Ty>
    constexpr friend auto operator<(Ty&& a, const infix& op) {
        return infix<Impl, Ty>{ std::forward<Ty>(a) };
    }

    template<class Ty> requires (std::invocable<decltype(Impl), A, Ty>)
    constexpr friend auto operator>(infix&& op, Ty&& b) 
        -> decltype(Impl(std::forward<A>(op.a), std::forward<Ty>(b))) {
        return Impl(std::forward<A>(op.a), std::forward<Ty>(b));
    }
};

#include <array>
#include <map>

constexpr infix<[](auto& a, auto& b){ return a.contains(b); }> contains;

constexpr infix<[]<class Ty>(Ty&& val, auto& container) {
    return std::find(std::begin(container), std::end(container), 
        std::forward<Ty>(val)) != std::end(container);
}> in;

constexpr infix<[]<class A, class B>(A&& a, B&& b){
    if constexpr (std::integral<A> && std::integral<B>)
        return a % b;
    else
        return std::fmod(std::forward<A>(a), std::forward<B>(b));
}> mod;


#define wrap(x) []<class ...Tys> requires requires(Tys&&...args) { x(std::forward<Tys>(args)...); } (Tys&&...args)\
    -> decltype(x(std::forward<Tys>(args)...)) { return x(std::forward<Tys>(args)...); }

#include "typed_any.hpp"

int main() {

    using namespace kaixo;

    struct Thing {
        int a = 10;
        double b = 3.5;
    };

    Any any1{};
    Any any2{};
    any1.set(Thing{});
    any2.set(0.0);
    auto val11 = any1.get();
    auto val12 = any2.get();
    any1.set(0);
    any2.set(Thing{});
    auto val21 = any1.get();
    auto val22 = any2.get();



    //kaixo::linked_types<kaixo::Thing::thing_dud>;


    std::map<std::string, int> map{ { 
        { "woof", 1 }, 
        { "carrot", 2 } 
    } };
    
    auto a3 = map <contains> "woof";

    constexpr auto res = 8 <mod> 3;

    constexpr std::array data{ 0, 2, 3, 6, 7 };
    constexpr bool isin = 2 <in> data;


    log("carrot soup");

    return 0;
}

