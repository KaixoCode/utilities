#pragma once


struct Test {
    int a;
};

namespace kaixo {






    //namespace detail {
    //    template<class, template<class...> class>
    //    struct is_specialization : std::false_type {};
    //    template<template<class...> class Ref, class... Args>
    //    struct is_specialization<Ref<Args...>, Ref> : std::true_type {};
    //}
    //// is specialization of templated class
    //template<class Test, template<class...> class Ref>
    //concept specialization = detail::is_specialization<std::decay_t<Test>, Ref>::value;

    //// 
    //// Tuple helpers
    //// 

    //// is specialization of std::tuple
    //template<class Ty> concept is_tuple = specialization<Ty, std::tuple>;

    //namespace detail {
    //    template<std::size_t I, class Tuple, std::size_t... Is>
    //    constexpr auto element_as_tuple(Tuple tuple, std::index_sequence<Is...>) {
    //        if constexpr (!(std::is_same_v<std::tuple_element_t<I, Tuple>,
    //            std::tuple_element_t<Is, Tuple>> || ...))
    //            return std::tuple<std::tuple_element_t<I, Tuple>>(std::get<I>(tuple));
    //        else return std::make_tuple();
    //    }
    //    template<class Tuple, std::size_t... Is>
    //    constexpr auto make_tuple_unique(Tuple tuple, std::index_sequence<Is...>) {
    //        return std::tuple_cat(element_as_tuple<Is>(tuple, std::make_index_sequence<Is>())...);
    //    }
    //}

    //template<is_tuple Tuple> constexpr auto unique_tuple(Tuple tuple) {
    //    return detail::make_tuple_unique(tuple, std::make_index_sequence<std::tuple_size_v<tuple>>{});
    //}

    //template<is_tuple Ty> using unique_tuple_t = decltype(unique_tuple(std::declval<Ty>()));

    //namespace detail {
    //    template<class T, class E, std::size_t I = 0> struct tuple_index;
    //    template<class F, class ...R, class E, std::size_t I>
    //    struct tuple_index<std::tuple<F, R...>, E, I> : public std::conditional<std::is_same<E, F>::value,
    //        std::integral_constant<std::size_t, I>, tuple_index<std::tuple<R...>, E, I + 1>>::type{};
    //    template<class E, std::size_t I> struct tuple_index<std::tuple<>, E, I> {};
    //}
    //// index of type in tuple
    //template<class E, is_tuple Tuple>
    //constexpr static std::size_t tuple_index_v = detail::tuple_index<Tuple, E>::value;

    //namespace detail {
    //    template<class T, is_tuple Ty> struct is_in_tuple;
    //    template<class T, class ...Tys> struct is_in_tuple<T, std::tuple<Tys...>>
    //    : std::bool_constant<(std::same_as<T, Tys> || ...)> {};
    //}
    //// type is in tuple
    //template<class T, class Ty> concept in_tuple = is_tuple<Ty> && detail::is_in_tuple<T, Ty>::value;
    //// type is not in tuple
    //template<class T, class Ty> concept not_in_tuple = is_tuple<Ty> && !detail::is_in_tuple<T, Ty>::value;

    //// concat tuples
    //template<is_tuple ...Tys> using tuple_cat_t = decltype(std::tuple_cat(std::declval<Tys>()...));

    //namespace detail {
    //    template<class ...Tys> struct as_tuple { using type = std::tuple<Tys...>; };
    //    template<class Ty> struct as_tuple<Ty> { using type = std::tuple<Ty>; };
    //    template<class A, class B> struct as_tuple<std::pair<A, B>> { using type = std::tuple<A, B>; };
    //    template<class ...Tys, template<class...> class T> struct as_tuple<T<Tys...>> { using type = std::tuple<Tys...>; };
    //    template<class ...Tys> struct as_tuple<std::tuple<Tys...>> { using type = std::tuple<Tys...>; };
    //}
    //// get type as tuple (pair<a, b> -> tuple<a, b>, type -> tuple<type>, tuple<tys...> -> tuple<tys...>, 
    //// type<tys...> -> tuple<tys...>, tys... -> std::tuple<tys...>)
    //template<class ...Ty> using as_tuple_t = typename detail::as_tuple<Ty...>::type;

    //namespace detail {
    //    template<is_tuple Ty> struct flatten;
    //    template<class Ty> struct flatten<std::tuple<Ty>> { using type = std::tuple<Ty>; };
    //    template<class ...Tys> struct flatten<std::tuple<Tys...>> {
    //        using type = tuple_cat_t<typename flatten<as_tuple_t<Tys>>::type...>;
    //    };
    //}
    //// flatten nested tuples to a single tuple
    //template<is_tuple Ty> using flatten_t = typename detail::flatten<Ty>::type;

    //namespace detail {
    //    template<class T, std::size_t ... Is>
    //    constexpr void print_tuple(auto& a, T& v, std::index_sequence<Is...>) {
    //        a << "(";
    //        ((a << std::get<Is>(v) << ", "), ...);
    //        a << std::get<sizeof...(Is)>(v);
    //        a << ")";
    //    }
    //}
    //// simple tuple printing
    //template<class ...Ty>
    //constexpr auto& operator<<(auto& a, std::tuple<Ty...>& v) {
    //    if constexpr (sizeof...(Ty) == 0) a << "()";
    //    else detail::print_tuple(a, v, std::make_index_sequence<sizeof...(Ty) - 1>{});
    //    return a;
    //}

}