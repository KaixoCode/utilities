#pragma once
#include <concepts>
#include <tuple>
#include <utility>
#include <typeinfo>
#include <stdexcept>

namespace kaixo {
    template<class Test, template<class...> class Ref>
    struct is_specialization : std::false_type {};

    template<template<class...> class Ref, class... Args>
    struct is_specialization<Ref<Args...>, Ref> : std::true_type {};

    template<class T, template<class...> class S>
    concept specialization_of = is_specialization<T, S>::value;

	template<class T, class ...Tys>
	concept one_of = (std::same_as<Tys, T> || ...);

    template<class>
    struct member_signature;
    template<class Return, class T, class... Args>
    struct member_signature<Return(T::*)(Args...) const> {
        using type = Return(Args...);
    };
    template<class Return, class T, class... Args>
    struct member_signature<Return(T::*)(Args...)> {
        using type = Return(Args...);
    };
    template<class T>
    using member_signature_t = typename member_signature<T>::type;

    template<class, class = void>
    struct lambda_signature;
    template<class _Fx>
    struct lambda_signature<_Fx, std::void_t<decltype(&_Fx::operator())>> {
        using type = member_signature<decltype(&_Fx::operator())>::type;
    };
    template<class T>
    using lambda_signature_t = typename lambda_signature<T>::type;

    template<class>
    struct funptr_to_type;
    template<class Return, class ...Args>
    struct funptr_to_type<Return(*)(Args...)> {
        using type = Return(Args...);
    };
    template<class T>
    using funptr_to_type_t = typename funptr_to_type<T>::type;

    template<class, std::size_t>
    struct last_n_args;
    template<class Return, class ...Args, std::size_t N>
    struct last_n_args<Return(Args...), N> {
        template<std::size_t... I>
        static inline Return(*last_n(std::index_sequence<I...>))(std::tuple_element_t<sizeof...(Args) - N + I, std::tuple<Args...>>...) {};
        using type = typename funptr_to_type<decltype(last_n(std::make_index_sequence<N>{})) > ::type;
    };
    template<class T, std::size_t N>
    using last_n_args_t = typename last_n_args<T, N>::type;

    template<class, std::size_t>
    struct first_n_args;
    template<class Return, typename ...Args, std::size_t N>
    struct first_n_args<Return(Args...), N> {
        template<std::size_t... I>
        static inline Return(*first_n(std::index_sequence<I...>))(std::tuple_element_t<I, std::tuple<Args...>>...) {};
        using type = typename funptr_to_type<decltype(first_n(std::make_index_sequence<N>{})) > ::type;
    };
    template<class T, std::size_t N>
    using first_n_args_t = typename first_n_args<T, N>::type;

    template<class Func, class ...Tys>
    concept are_first_n = requires(typename first_n_args<Func, sizeof...(Tys)>::type func, Tys&&...tys) {
        func(std::forward<Tys>(tys)...);
    };
}