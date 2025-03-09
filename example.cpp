
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
#include <span>
#include <complex>
#include <print>
#include <coroutine>
#include <typeindex>

// ------------------------------------------------

template<class, class, bool, bool, bool, int>
struct fni;

template<class Ret, class Class, class ...Args, bool Const, bool Volatile, bool Noexcept, int R>
struct fni<Ret(Args...), Class, Const, Volatile, Noexcept, R> {

    using signature = Ret(Args...);

    template<std::size_t I>
    using argument_type = std::tuple_element_t<I, std::tuple<Args...>>;
    using return_type = Ret;
    using class_type = Class;

    template<template<class...> class T>
    using evaluate_with_arguments = T<Args...>;

    constexpr static std::size_t arguments = sizeof...(Args);

    constexpr static bool is_member_function = !std::same_as<Class, void>;
    constexpr static bool is_noexcept = Noexcept;
    constexpr static bool is_const = Const;
    constexpr static bool is_volatile = Volatile;
    constexpr static bool is_lvalue_reference = R == 1;
    constexpr static bool is_rvalue_reference = R == 2;

    constexpr fni() = default;
    constexpr fni(auto) {};
};

template<class> struct fnid;

template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...)                           > : fni<R(Ts...),    C, 0, 0, 0, 0> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...) const                     > : fni<R(Ts...),    C, 1, 0, 0, 0> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...)       volatile            > : fni<R(Ts...),    C, 0, 1, 0, 0> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...) const volatile            > : fni<R(Ts...),    C, 1, 1, 0, 0> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...)                &          > : fni<R(Ts...),    C, 0, 0, 0, 1> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...) const          &          > : fni<R(Ts...),    C, 1, 0, 0, 1> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...)       volatile &          > : fni<R(Ts...),    C, 0, 1, 0, 1> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...) const volatile &          > : fni<R(Ts...),    C, 1, 1, 0, 1> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...)                &&         > : fni<R(Ts...),    C, 0, 0, 0, 2> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...) const          &&         > : fni<R(Ts...),    C, 1, 0, 0, 2> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...)       volatile &&         > : fni<R(Ts...),    C, 0, 1, 0, 2> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...) const volatile &&         > : fni<R(Ts...),    C, 1, 1, 0, 2> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...)                   noexcept> : fni<R(Ts...),    C, 0, 0, 1, 0> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...) const             noexcept> : fni<R(Ts...),    C, 1, 0, 1, 0> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...)       volatile    noexcept> : fni<R(Ts...),    C, 0, 1, 1, 0> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...) const volatile    noexcept> : fni<R(Ts...),    C, 1, 1, 1, 0> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...)                &  noexcept> : fni<R(Ts...),    C, 0, 0, 1, 1> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...) const          &  noexcept> : fni<R(Ts...),    C, 1, 0, 1, 1> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...)       volatile &  noexcept> : fni<R(Ts...),    C, 0, 1, 1, 1> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...) const volatile &  noexcept> : fni<R(Ts...),    C, 1, 1, 1, 1> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...)                && noexcept> : fni<R(Ts...),    C, 0, 0, 1, 2> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...) const          && noexcept> : fni<R(Ts...),    C, 1, 0, 1, 2> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...)       volatile && noexcept> : fni<R(Ts...),    C, 0, 1, 1, 2> {};
template<class R, class C, class ...Ts> struct fnid<R(C::*)(Ts...) const volatile && noexcept> : fni<R(Ts...),    C, 1, 1, 1, 2> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...)                           > : fni<R(Ts...), void, 0, 0, 0, 0> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...) const                     > : fni<R(Ts...), void, 1, 0, 0, 0> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...)       volatile            > : fni<R(Ts...), void, 0, 1, 0, 0> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...) const volatile            > : fni<R(Ts...), void, 1, 1, 0, 0> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...)                &          > : fni<R(Ts...), void, 0, 0, 0, 1> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...) const          &          > : fni<R(Ts...), void, 1, 0, 0, 1> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...)       volatile &          > : fni<R(Ts...), void, 0, 1, 0, 1> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...) const volatile &          > : fni<R(Ts...), void, 1, 1, 0, 1> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...)                &&         > : fni<R(Ts...), void, 0, 0, 0, 2> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...) const          &&         > : fni<R(Ts...), void, 1, 0, 0, 2> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...)       volatile &&         > : fni<R(Ts...), void, 0, 1, 0, 2> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...) const volatile &&         > : fni<R(Ts...), void, 1, 1, 0, 2> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...)                   noexcept> : fni<R(Ts...), void, 0, 0, 1, 0> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...) const             noexcept> : fni<R(Ts...), void, 1, 0, 1, 0> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...)       volatile    noexcept> : fni<R(Ts...), void, 0, 1, 1, 0> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...) const volatile    noexcept> : fni<R(Ts...), void, 1, 1, 1, 0> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...)                &  noexcept> : fni<R(Ts...), void, 0, 0, 1, 1> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...) const          &  noexcept> : fni<R(Ts...), void, 1, 0, 1, 1> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...)       volatile &  noexcept> : fni<R(Ts...), void, 0, 1, 1, 1> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...) const volatile &  noexcept> : fni<R(Ts...), void, 1, 1, 1, 1> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...)                && noexcept> : fni<R(Ts...), void, 0, 0, 1, 2> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...) const          && noexcept> : fni<R(Ts...), void, 1, 0, 1, 2> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...)       volatile && noexcept> : fni<R(Ts...), void, 0, 1, 1, 2> {};
template<class R,          class ...Ts> struct fnid<R      (Ts...) const volatile && noexcept> : fni<R(Ts...), void, 1, 1, 1, 2> {};
template<class R,          class ...Ts> struct fnid<R   (*)(Ts...)                           > : fni<R(Ts...), void, 0, 0, 0, 0> {};
template<class R,          class ...Ts> struct fnid<R   (*)(Ts...)                   noexcept> : fni<R(Ts...), void, 0, 0, 1, 0> {};

template<class Ty>
struct function_info;

template<class Ty>
    requires requires { typename fnid<std::decay_t<Ty>>; }
struct function_info<Ty> : fnid<std::decay_t<Ty>> {};

template<class Ty>
    requires requires { { &std::decay_t<Ty>::operator() }; }
struct function_info<Ty> : function_info<decltype(&std::decay_t<Ty>::operator())> {};

// ------------------------------------------------

struct ScriptingLanguage {

    struct Function {
        struct Overload {
            std::vector<std::type_index> argument_types;
            std::function<std::any(std::span<std::any>)> function;

            std::size_t arity() const { return argument_types.size(); }

            bool can_call(std::span<std::any> args) const {
                if (args.size() != arity()) return false;
                for (std::size_t i = 0; i < arity(); ++i) {
                    if (args[i].type() != argument_types[i]) return false;
                }

                return true;
            }

            std::any operator()(std::span<std::any> args) { return function(args); }
        };

        std::vector<Overload> overloads;

        std::any operator()(std::span<std::any> args) {
            for (auto& overload : overloads) {
                if (overload.can_call(args)) {
                    return overload(args);
                }
            }

            return {};
        }
    };
    
    std::vector<std::pair<std::string, Function>> functions;

    Function& get(std::string_view name) {
        for (auto& fun : functions) {
            if (fun.first == name) {
                return fun.second;
            }
        }

        return functions.emplace_back(name, Function{}).second;
    }

    Function& operator[](std::string_view name) {
        return get(name);
    }

    void add(auto fun, std::string_view name) {
        using info = function_info<decltype(fun)>;
        auto& function = get(name);

        auto& overload = function.overloads.emplace_back();
        overload.argument_types.reserve(info::arguments);
        [&] <std::size_t ...Is>(std::index_sequence<Is...>) {
            (overload.argument_types.push_back(typeid(typename info::template argument_type<Is>)), ...);
        }(std::make_index_sequence<info::arguments>{});

        overload.function = [fun = std::move(fun)](std::span<std::any> arguments) -> std::any {
            return [&] <std::size_t ...Is>(std::index_sequence<Is...>) {
                if constexpr (std::same_as<typename info::return_type, void>) {
                    fun(std::any_cast<typename info::template argument_type<Is>>(arguments[Is])...);
                    return {};
                } else return fun(std::any_cast<typename info::template argument_type<Is>>(arguments[Is])...);
            }(std::make_index_sequence<info::arguments>{});
        };
    }

};

// ------------------------------------------------

struct MyClass {

    int fun1(int) { return 0; }
    int fun2(int) const { return 0; }
    int fun3(int) volatile { return 0; }
    int fun4(int) const volatile { return 0; }

    void operator()(int a, int b) const {}

};

// ------------------------------------------------

template<class A, class B>
constexpr auto add(A a, B b) { return a + b; }

// ------------------------------------------------

int main() {




    return 0;
}

// ------------------------------------------------
