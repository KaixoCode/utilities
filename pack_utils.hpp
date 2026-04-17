
// ------------------------------------------------

#pragma once

// ------------------------------------------------

#include <cstddef>
#include <concepts>
#include <utility>
#include <tuple>
#include <memory>

// ------------------------------------------------

namespace kaixo {

    // ------------------------------------------------

    constexpr std::size_t npos = static_cast<std::size_t>(-1);

    // ------------------------------------------------

    // Combine Filters using logical and
    template<template<class> class ...Filter>
    struct filter_and {
        template<class Ty>
        struct type {
            constexpr static bool value = (static_cast<bool>(Filter<Ty>::value) && ...);
        };
    };

    // ------------------------------------------------

    // Combine Filters using logical or
    template<template<class> class ...Filter>
    struct filter_or {
        template<class Ty>
        struct type {
            constexpr static bool value = (static_cast<bool>(Filter<Ty>::value) || ...);
        };
    };

    // ------------------------------------------------

    // Invert Filter using logical not
    template<template<class> class Filter>
    struct filter_invert {
        template<class Ty>
        struct type {
            constexpr static bool value = !static_cast<bool>(Filter<Ty>::value);
        };
    };
    
    // ------------------------------------------------

    // Reverse the Sorter
    template<template<class, class> class Sorter>
    struct sorter_reverse {
        template<class A, class B>
        struct type {
            constexpr static bool value = !Sorter<A, B>::value;
        };
    };

    // ------------------------------------------------

    // Only apply Transform if Filter matches
    template<template<class> class Filter, template<class> class Transform>
    struct conditional_transform {
        template<class Ty>
        struct _impl;

        template<class Ty>
            requires (Filter<Ty>::value)
        struct _impl<Ty> {
            using type = Transform<Ty>;
        };

        template<class Ty>
            requires (!Filter<Ty>::value)
        struct _impl<Ty> {
            using type = Ty;
        };

        template<class Ty>
        using type = typename _impl<Ty>::type;
    };

    // ------------------------------------------------
    // ------------------------------------------------
    // ------------------------------------------------

    template<class ...Tys>
    struct pack;

    // ------------------------------------------------
    
    namespace detail {
        template<std::size_t I>
        struct index_as_type {
            constexpr static std::size_t value = I;
        };

        template<class>
        struct to_indices;

        template<std::size_t ...Is>
        struct to_indices<pack<index_as_type<Is>...>> {
            using type = std::index_sequence<Is...>;
        };

        template<class>
        struct indices_as_pack;

        template<std::size_t ...Ns>
        struct indices_as_pack<std::index_sequence<Ns...>> {
            using type = pack<index_as_type<Ns>...>;
        };
    }

    // ------------------------------------------------

    template<class Pack>
    struct pack_size;

    template<class ...Tys>
    struct pack_size<pack<Tys...>> {
        constexpr static std::size_t value = sizeof...(Tys);
    };

    // Size of Pack
    template<class Pack>
    constexpr std::size_t pack_size_v = pack_size<Pack>::value;

    // ------------------------------------------------

    template<std::size_t I, class Ty>
    struct pack_element;

    template<class Ty, class ...Tys>
    struct pack_element<0, pack<Ty, Tys...>> {
        using type = Ty;
    };

    template<std::size_t I, class Ty, class ...Tys>
    struct pack_element<I, pack<Ty, Tys...>> {
        using type = typename pack_element<I - 1, pack<Tys...>>::type;
    };

    // Ith element in Pack
    template<std::size_t I, class Pack>
    using pack_element_t = typename pack_element<I, Pack>::type;

    // ------------------------------------------------
    
    template<std::size_t N, class Indices>
    struct indices_element {
        constexpr static std::size_t value = pack_element<N, typename detail::indices_as_pack<Indices>::type>::type::value;
    };

    // Ith element in Indices
    template<std::size_t I, class Indices>
    constexpr std::size_t indices_element_v = indices_element<I, Indices>::value;

    // ------------------------------------------------

    template<class Type, class Pack>
    struct pack_contains {
        constexpr static bool value = false;
    };

    template<class Type, class ...Tys>
    struct pack_contains<Type, pack<Type, Tys...>> {
        constexpr static bool value = true;
    };

    template<class Type, class Ty, class ...Tys>
    struct pack_contains<Type, pack<Ty, Tys...>> {
        constexpr static bool value = pack_contains<Type, pack<Tys...>>::value;
    };

    // Pack contains Type
    template<class Type, class Pack>
    constexpr bool pack_contains_v = pack_contains<Type, Pack>::value;

    // ------------------------------------------------

    template<class Types, class Pack>
    struct pack_contains_all;

    template<class ...Types, class ...Tys>
    struct pack_contains_all<pack<Types...>, pack<Tys...>> {
        constexpr static bool value = (pack_contains<Types, pack<Tys...>>::value && ...);
    };

    // Pack contains all in Types
    template<class Types, class Pack>
    constexpr bool pack_contains_all_v = pack_contains_all<Types, Pack>::value;

    // ------------------------------------------------

    template<class Types, class Pack>
    struct pack_contains_any;

    template<class ...Types, class ...Tys>
    struct pack_contains_any<pack<Types...>, pack<Tys...>> {
        constexpr static bool value = (pack_contains<Types, pack<Tys...>>::value || ...);
    };

    // Pack contains any in Types
    template<class Types, class Pack>
    constexpr bool pack_contains_any_v = pack_contains_any<Types, Pack>::value;

    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_find;

    template<template<class> class Filter, class ...Tys>
    struct pack_find<Filter, pack<Tys...>> {
        constexpr static bool value = (Filter<Tys>::value || ...);
    };

    // Find match for Filter in Pack
    template<template<class> class Filter, class Pack>
    constexpr bool pack_find_v = pack_find<Filter, Pack>::value;

    // ------------------------------------------------

    template<class Type, class Pack>
    struct pack_count;

    template<class Type, class ...Tys>
    struct pack_count<Type, pack<Type, Tys...>> {
        constexpr static std::size_t value = 1 + pack_count<Type, pack<Tys...>>::value;
    };

    template<class Type, class Ty, class ...Tys>
    struct pack_count<Type, pack<Ty, Tys...>> {
        constexpr static std::size_t value = pack_count<Type, pack<Tys...>>::value;
    };

    template<class Type>
    struct pack_count<Type, pack<>> {
        constexpr static std::size_t value = 0;
    };

    // Number of occurences of Type in Pack
    template<class Type, class Pack>
    constexpr std::size_t pack_count_v = pack_count<Type, Pack>::value;

    // ------------------------------------------------

    template<class Types, class Pack>
    struct pack_count_all;

    template<class ...Types, class ...Tys>
    struct pack_count_all<pack<Types...>, pack<Tys...>> {
        constexpr static std::size_t value = (pack_count<Types, pack<Tys...>>::value + ... + 0);
    };

    // Sum of the number of occurences of all Types in Pack
    template<class Types, class Pack>
    constexpr std::size_t pack_count_all_v = pack_count_all<Types, Pack>::value;

    // ------------------------------------------------

    template<class Pack>
    struct pack_count_unique;

    template<class ...Tys>
    struct pack_count_unique<pack<Tys...>> {
        template<class T>
        constexpr static double _unq_cnt = 1.0 / (std::same_as<T, Tys> +...);

        constexpr static std::size_t value = static_cast<std::size_t>((_unq_cnt<Tys> +... + 0) + 0.5);
    };

    // Count unique elements in Pack
    template<class Pack>
    constexpr std::size_t pack_count_unique_v = pack_count_unique<Pack>::value;

    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_count_filter;

    template<template<class> class Filter, class ...Tys>
    struct pack_count_filter<Filter, pack<Tys...>> {
        constexpr static std::size_t value = (static_cast<bool>(Filter<Tys>::value) + ... + 0);
    };

    // Count number of matches for Filter in Pack
    template<template<class> class Filter, class Pack>
    constexpr std::size_t pack_count_filter_v = pack_count_filter<Filter, Pack>::value;

    // ------------------------------------------------

    template<class Type, std::size_t I, class Pack>
    struct pack_nth_index_of;

    template<class Type, std::size_t I, class Ty, class ...Tys>
    struct pack_nth_index_of<Type, I, pack<Ty, Tys...>> {
        constexpr static std::size_t _last = pack_nth_index_of<Type, I, pack<Tys...>>::value;
        constexpr static std::size_t value = _last == npos ? npos : _last + 1;
    };

    template<class Type, std::size_t I, class ...Tys>
    struct pack_nth_index_of<Type, I, pack<Type, Tys...>> {
        constexpr static std::size_t _last = pack_nth_index_of<Type, I - 1, pack<Tys...>>::value;
        constexpr static std::size_t value = _last == npos ? npos : _last + 1;
    };

    template<class Type, class ...Tys>
    struct pack_nth_index_of<Type, 0, pack<Type, Tys...>> {
        constexpr static std::size_t value = 0;
    };

    template<class Type, std::size_t N>
    struct pack_nth_index_of<Type, N, pack<>> {
        constexpr static bool _found = false;
        constexpr static std::size_t value = npos;
    };

    // Index of the Nth occurence of Type in Pack
    template<class Type, std::size_t N, class Pack>
    constexpr std::size_t pack_nth_index_of_v = pack_nth_index_of<Type, N, Pack>::value;

    // ------------------------------------------------

    template<class Types, std::size_t N, class Pack>
    struct pack_nth_index_of_any;

    template<class ...Types, std::size_t N, class Ty, class ...Tys>
        requires (!(std::same_as<Types, Ty> || ...))
    struct pack_nth_index_of_any<pack<Types...>, N, pack<Ty, Tys...>> {
        constexpr static std::size_t _last = pack_nth_index_of_any<pack<Types...>, N, pack<Tys...>>::value;
        constexpr static std::size_t value = _last == npos ? npos : _last + 1;
    };

    template<class ...Types, std::size_t N, class Ty, class ...Tys>
        requires (std::same_as<Types, Ty> || ...)
    struct pack_nth_index_of_any<pack<Types...>, N, pack<Ty, Tys...>> {
        constexpr static std::size_t _last = pack_nth_index_of_any<pack<Types...>, N - 1, pack<Tys...>>::value;
        constexpr static std::size_t value = _last == npos ? npos : _last + 1;
    };

    template<class ...Types, class Ty, class ...Tys>
        requires (std::same_as<Types, Ty> || ...)
    struct pack_nth_index_of_any<pack<Types...>, 0, pack<Ty, Tys...>> {
        constexpr static std::size_t value = 0;
    };

    template<class Types, std::size_t N>
    struct pack_nth_index_of_any<Types, N, pack<>> {
        constexpr static bool _found = false;
        constexpr static std::size_t value = npos;
    };

    // Index of the Nth occurence of any of Types in Pack
    template<class Types, std::size_t N, class Pack>
    constexpr std::size_t pack_nth_index_of_any_v = pack_nth_index_of_any<Types, N, Pack>::value;

    // ------------------------------------------------

    template<template<class> class Filter, std::size_t N, class Pack>
    struct pack_nth_index_filter;

    template<template<class> class Filter, std::size_t N, class Ty, class ...Tys>
        requires (!Filter<Ty>::value)
    struct pack_nth_index_filter<Filter, N, pack<Ty, Tys...>> {
        constexpr static std::size_t _last = pack_nth_index_filter<Filter, N, pack<Tys...>>::value;
        constexpr static std::size_t value = _last == npos ? npos : _last + 1;
    };

    template<template<class> class Filter, std::size_t N, class Ty, class ...Tys>
        requires (Filter<Ty>::value)
    struct pack_nth_index_filter<Filter, N, pack<Ty, Tys...>> {
        constexpr static std::size_t _last = pack_nth_index_filter<Filter, N - 1, pack<Tys...>>::value;
        constexpr static std::size_t value = _last == npos ? npos : _last + 1;
    };

    template<template<class> class Filter, class Ty, class ...Tys>
        requires (Filter<Ty>::value)
    struct pack_nth_index_filter<Filter, 0, pack<Ty, Tys...>> {
        constexpr static std::size_t value = 0;
    };

    template<template<class> class Filter, std::size_t N>
    struct pack_nth_index_filter<Filter, N, pack<>> {
        constexpr static bool _found = false;
        constexpr static std::size_t value = npos;
    };

    // Index of the Nth match of Filter in Pack
    template<template<class> class Filter, std::size_t N, class Pack>
    constexpr std::size_t pack_nth_index_filter_v = pack_nth_index_filter<Filter, N, Pack>::value;

    // ------------------------------------------------

    template<class Type, class Pack>
    struct pack_index_of {
        constexpr static std::size_t value = pack_nth_index_of<Type, 0, Pack>::value;
    };

    // Index of the first occurence of Type in Pack
    template<class Type, class Pack>
    constexpr std::size_t pack_index_of_v = pack_index_of<Type, Pack>::value;

    // ------------------------------------------------

    template<class Types, class Pack>
    struct pack_index_of_any {
        constexpr static std::size_t value = pack_nth_index_of_any<Types, 0, Pack>::value;
    };

    // Index of the first occurence of any of Types in Pack
    template<class Types, class Pack>
    constexpr std::size_t pack_index_of_any_v = pack_index_of_any<Types, Pack>::value;

    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_index_filter {
        constexpr static std::size_t value = pack_nth_index_filter<Filter, 0, Pack>::value;
    };

    // Index of the first match of Filter in Pack
    template<template<class> class Filter, class Pack>
    constexpr std::size_t pack_index_filter_v = pack_index_filter<Filter, Pack>::value;

    // ------------------------------------------------

    template<class Type, class Pack>
    struct pack_first_index_of {
        constexpr static std::size_t value = pack_nth_index_of<Type, 0, Pack>::value;
    };

    // Index of the first occurence of Type in Pack
    template<class Type, class Pack>
    constexpr std::size_t pack_first_index_of_v = pack_first_index_of<Type, Pack>::value;

    // ------------------------------------------------

    template<class Types, class Pack>
    struct pack_first_index_of_any {
        constexpr static std::size_t value = pack_nth_index_of_any<Types, 0, Pack>::value;
    };

    // Index of the first occurence of any of Types in Pack
    template<class Types, class Pack>
    constexpr std::size_t pack_first_index_of_any_v = pack_first_index_of_any<Types, Pack>::value;

    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_first_index_filter {
        constexpr static std::size_t value = pack_nth_index_filter<Filter, 0, Pack>::value;
    };

    // Index of the first match of Filter in Pack
    template<template<class> class Filter, class Pack>
    constexpr std::size_t pack_first_index_filter_v = pack_first_index_filter<Filter, Pack>::value;

    // ------------------------------------------------

    template<class Type, class Pack>
    struct pack_last_index_of {
        constexpr static std::size_t value = pack_nth_index_of<Type, pack_count<Type, Pack>::value - 1, Pack>::value;
    };

    // Index of the last occurence of Type in Pack
    template<class Type, class Pack>
    constexpr std::size_t pack_last_index_of_v = pack_last_index_of<Type, Pack>::value;

    // ------------------------------------------------

    template<class Types, class Pack>
    struct pack_last_index_of_any {
        constexpr static std::size_t value = pack_nth_index_of_any<Types, pack_count_all<Types, Pack>::value - 1, Pack>::value;
    };

    // Index of the last occurence of any of Types in Pack
    template<class Types, class Pack>
    constexpr std::size_t pack_last_index_of_any_v = pack_last_index_of_any<Types, Pack>::value;

    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_last_index_filter {
        constexpr static std::size_t value = pack_nth_index_filter<Filter, pack_count_filter<Filter, Pack>::value - 1, Pack>::value;
    };

    // Index of the last match of Filter in Pack
    template<template<class> class Filter, class Pack>
    constexpr std::size_t pack_last_index_filter_v = pack_last_index_filter<Filter, Pack>::value;

    // ------------------------------------------------

    template<class Type, class Pack>
    struct pack_indices_of {
        template<class>
        struct _impl;

        template<std::size_t ...Is>
        struct _impl<std::index_sequence<Is...>> {
            using type = std::index_sequence<pack_nth_index_of<Type, Is, Pack>::value...>;
        };

        using type = typename _impl<std::make_index_sequence<pack_count<Type, Pack>::value>>::type;
    };

    // All indices of Type in Pack
    template<class Type, class Pack>
    using pack_indices_of_t = pack_indices_of<Type, Pack>::type;
    
    // ------------------------------------------------

    template<class Types, class Pack>
    struct pack_indices_of_all {
        template<class>
        struct _impl;

        template<std::size_t ...Is>
        struct _impl<std::index_sequence<Is...>> {
            using type = std::index_sequence<pack_nth_index_of_any<Types, Is, Pack>::value...>;
        };

        using type = typename _impl<std::make_index_sequence<pack_count_all<Types, Pack>::value>>::type;
    };

    // All indices of all Types in Pack
    template<class Types, class Pack>
    using pack_indices_of_all_t = pack_indices_of_all<Types, Pack>::type;

    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_indices_filter {
        template<class>
        struct _impl;

        template<std::size_t ...Is>
        struct _impl<std::index_sequence<Is...>> {
            using type = std::index_sequence<pack_nth_index_filter<Filter, Is, Pack>::value...>;
        };

        using type = typename _impl<std::make_index_sequence<pack_count_filter<Filter, Pack>::value>>::type;
    };

    // All indices of all matches of Filter in Pack
    template<template<class> class Filter, class Pack>
    using pack_indices_filter_t = pack_indices_filter<Filter, Pack>::type;
    
    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_indices_not_filter {
        template<class>
        struct _impl;

        template<std::size_t ...Is>
        struct _impl<std::index_sequence<Is...>> {
            using type = std::index_sequence<pack_nth_index_filter<filter_invert<Filter>::type, Is, Pack>::value...>;
        };

        using type = typename _impl<std::make_index_sequence<pack_count_filter<filter_invert<Filter>::type, Pack>::value>>::type;
    };

    // All indices of all non-matches of Filter in Pack
    template<template<class> class Filter, class Pack>
    using pack_indices_not_filter_t = pack_indices_not_filter<Filter, Pack>::type;

    // ------------------------------------------------

    template<class Type, class Pack>
    struct pack_indices_not_of {
        template<class Other>
        struct _impl {
            constexpr static bool value = !std::same_as<Other, Type>;
        };

        using type = typename pack_indices_filter<_impl, Pack>::type;
    };

    // All indices not of Type in Pack
    template<class Type, class Pack>
    using pack_indices_not_of_t = pack_indices_not_of<Type, Pack>::type;
    
    // ------------------------------------------------

    template<class Types, class Pack>
    struct pack_indices_not_of_all {
        template<class Other>
        struct _impl {
            constexpr static bool value = !pack_contains<Other, Types>::value;
        };

        using type = typename pack_indices_filter<_impl, Pack>::type;
    };

    // All indices not of all Types in Pack
    template<class Types, class Pack>
    using pack_indices_not_of_all_t = pack_indices_not_of_all<Types, Pack>::type;

    // ------------------------------------------------

    namespace detail {
        template<class Nums, std::size_t Index = 0, std::size_t ...Result>
        struct remove_number_if_not_same_as_index;

        template<std::size_t Num, std::size_t ...Nums, std::size_t Index, std::size_t ...Result>
        struct remove_number_if_not_same_as_index<std::index_sequence<Num, Nums...>, Index, Result...> {
            using type = typename remove_number_if_not_same_as_index<
                std::index_sequence<Nums...>, Index + 1, Result...>::type;
        };

        template<std::size_t ...Nums, std::size_t Index, std::size_t ...Result>
        struct remove_number_if_not_same_as_index<std::index_sequence<Index, Nums...>, Index, Result...> {
            using type = typename remove_number_if_not_same_as_index<
                std::index_sequence<Nums...>, Index + 1, Result..., Index>::type;
        };

        template<std::size_t Index, std::size_t ...Result>
        struct remove_number_if_not_same_as_index<std::index_sequence<>, Index, Result...> {
            using type = std::index_sequence<Result...>;
        };
    }

    template<class Pack>
    struct pack_first_indices {
        template<class>
        struct _impl;

        template<std::size_t ...Is>
        struct _impl<std::index_sequence<Is...>> {
            using _first_indices = std::index_sequence<pack_first_index_of<typename pack_element<Is, Pack>::type, Pack>::value...>;
            using type = typename detail::remove_number_if_not_same_as_index<_first_indices>::type;
        };

        using type = typename _impl<std::make_index_sequence<pack_size<Pack>::value>>::type;
    };

    // First indices of all elements in Pack
    template<class Pack>
    using pack_first_indices_t = typename pack_first_indices<Pack>::type;

    // ------------------------------------------------

    template<class Pack>
    struct pack_last_indices {
        template<class>
        struct _impl;

        template<std::size_t ...Is>
        struct _impl<std::index_sequence<Is...>> {
            using _first_indices = std::index_sequence<pack_last_index_of<typename pack_element<Is, Pack>::type, Pack>::value...>;
            using type = typename detail::remove_number_if_not_same_as_index<_first_indices>::type;
        };

        using type = typename _impl<std::make_index_sequence<pack_size<Pack>::value>>::type;
    };

    // Last indices of all elements in Pack
    template<class Pack>
    using pack_last_indices_t = typename pack_last_indices<Pack>::type;
    
    // ------------------------------------------------

    template<std::size_t N, class Pack>
    struct pack_nth_indices {
        template<class>
        struct _impl;

        template<std::size_t ...Is>
        struct _impl<std::index_sequence<Is...>> {
            using _first_indices = std::index_sequence<pack_nth_index_of<typename pack_element<Is, Pack>::type, N, Pack>::value...>;
            using type = typename detail::remove_number_if_not_same_as_index<_first_indices>::type;
        };

        using type = typename _impl<std::make_index_sequence<pack_size<Pack>::value>>::type;
    };

    // Nth indices of all elements in Pack
    template<std::size_t N, class Pack>
    using pack_nth_indices_t = typename pack_nth_indices<N, Pack>::type;

    // ------------------------------------------------

    template<class Indices, class Pack>
    struct pack_at_indices;

    template<std::size_t ...Is, class Pack>
    struct pack_at_indices<std::index_sequence<Is...>, Pack> {
        using type = pack<typename pack_element<Is, Pack>::type...>;
    };

    // Create new pack by selecting Indices from Pack
    template<class Indices, class Pack>
    using pack_at_indices_t = typename pack_at_indices<Indices, Pack>::type;

    // ------------------------------------------------

    template<class Pack>
    struct pack_unique {
        using type = typename pack_at_indices<typename pack_first_indices<Pack>::type, Pack>::type;
    };

    // Only keep unique elements in Pack
    template<class Pack>
    using pack_unique_t = typename pack_unique<Pack>::type;

    // ------------------------------------------------

    template<std::size_t I, class Pack>
    struct pack_drop;

    template<std::size_t I, class Ty, class ...Tys>
    struct pack_drop<I, pack<Ty, Tys...>> {
        using type = typename pack_drop<I - 1, pack<Tys...>>::type;
    };

    template<class Pack>
    struct pack_drop<0, Pack> {
        using type = Pack;
    };

    // Drop the first I elements of Pack
    template<std::size_t I, class Pack>
    using pack_drop_t = typename pack_drop<I, Pack>::type;

    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_drop_while;

    template<template<class> class Filter, class Ty, class ...Tys>
        requires (Filter<Ty>::value)
    struct pack_drop_while<Filter, pack<Ty, Tys...>> {
        using type = typename pack_drop_while<Filter, pack<Tys...>>::type;
    };

    template<template<class> class Filter, class Ty, class ...Tys>
        requires (!Filter<Ty>::value)
    struct pack_drop_while<Filter, pack<Ty, Tys...>> {
        using type = pack<Ty, Tys...>;
    };

    template<template<class> class Filter>
    struct pack_drop_while<Filter, pack<>> {
        using type = pack<>;
    };

    // Drop elements from Pack while Filter matches
    template<template<class> class Filter, class Pack>
    using pack_drop_while_t = typename pack_drop_while<Filter, Pack>::type;

    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_drop_until;

    template<template<class> class Filter, class Ty, class ...Tys>
        requires (!Filter<Ty>::value)
    struct pack_drop_until<Filter, pack<Ty, Tys...>> {
        using type = typename pack_drop_until<Filter, pack<Tys...>>::type;
    };

    template<template<class> class Filter, class Ty, class ...Tys>
        requires (Filter<Ty>::value)
    struct pack_drop_until<Filter, pack<Ty, Tys...>> {
        using type = pack<Ty, Tys...>;
    };

    template<template<class> class Filter>
    struct pack_drop_until<Filter, pack<>> {
        using type = pack<>;
    };

    // Drop elements from Pack until Filter matches
    template<template<class> class Filter, class Pack>
    using pack_drop_until_t = typename pack_drop_until<Filter, Pack>::type;

    // ------------------------------------------------

    template<std::size_t I, class Pack>
    struct pack_take {
        template<class>
        struct _impl;

        template<std::size_t ...Is>
        struct _impl<std::index_sequence<Is...>> {
            using type = pack<typename pack_element<Is, Pack>::type...>;
        };

        using type = typename _impl<std::make_index_sequence<I>>::type;
    };

    // Take the first I elements of Pack
    template<std::size_t I, class Pack>
    using pack_take_t = typename pack_take<I, Pack>::type;

    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_take_while {
        template<std::size_t Take>
        struct _impl {
            using type = typename pack_take<Take, Pack>::type;
        };

        template<>
        struct _impl<npos> {
            using type = Pack;
        };

        constexpr static std::size_t _first_non_match = pack_first_index_filter<filter_invert<Filter>::type, Pack>::value;
        using type = typename _impl<_first_non_match>::type;
    };

    // Take elements from Pack while Filter matches
    template<template<class> class Filter, class Pack>
    using pack_take_while_t = typename pack_take_while<Filter, Pack>::type;

    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_take_until {
        template<std::size_t Take>
        struct _impl {
            using type = typename pack_take<Take, Pack>::type;
        };

        template<>
        struct _impl<npos> {
            using type = Pack;
        };

        constexpr static std::size_t _first_non_match = pack_first_index_filter<Filter, Pack>::value;
        using type = typename _impl<_first_non_match>::type;
    };

    // Take elements from Pack until Filter matches
    template<template<class> class Filter, class Pack>
    using pack_take_until_t = typename pack_take_until<Filter, Pack>::type;

    // ------------------------------------------------

    template<std::size_t I, class Pack>
    struct pack_drop_last {
        using type = typename pack_take<pack_size<Pack>::value - I, Pack>::type;
    };

    // Drop the last I elements of Pack
    template<std::size_t I, class Pack>
    using pack_drop_last_t = typename pack_drop_last<I, Pack>::type;
    
    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_drop_last_while {
        constexpr static std::size_t _last_match = pack_last_index_filter<filter_invert<Filter>::type, Pack>::value;
        using type = typename pack_take<_last_match + 1, Pack>::type;
    };

    // Drop elements from the end of Pack while Filter matches
    template<template<class> class Filter, class Pack>
    using pack_drop_last_while_t = typename pack_drop_last_while<Filter, Pack>::type;
    
    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_drop_last_until {
        constexpr static std::size_t _last_match = pack_last_index_filter<Filter, Pack>::value;
        using type = typename pack_take<_last_match + 1, Pack>::type;
    };

    // Drop elements from the end of Pack until Filter matches
    template<template<class> class Filter, class Pack>
    using pack_drop_last_until_t = typename pack_drop_last_until<Filter, Pack>::type;
    
    // ------------------------------------------------

    template<std::size_t I, class Pack>
    struct pack_take_last {
        using type = typename pack_drop<pack_size<Pack>::value - I, Pack>::type;
    };

    // Take the last I elements of Pack
    template<std::size_t I, class Pack>
    using pack_take_last_t = typename pack_take_last<I, Pack>::type;

    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_take_last_while {
        constexpr static std::size_t _last_match = pack_last_index_filter<filter_invert<Filter>::type, Pack>::value;
        using type = typename pack_drop<_last_match + 1, Pack>::type;
    };

    // Take elements from the end of Pack while Filter matches
    template<template<class> class Filter, class Pack>
    using pack_take_last_while_t = typename pack_take_last_while<Filter, Pack>::type;

    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_take_last_until {
        constexpr static std::size_t _last_match = pack_last_index_filter<Filter, Pack>::value;
        using type = typename pack_drop<_last_match + 1, Pack>::type;
    };

    // Take elements from the end of Pack until Filter matches
    template<template<class> class Filter, class Pack>
    using pack_take_last_until_t = typename pack_take_last_until<Filter, Pack>::type;

    // ------------------------------------------------

    template<class ...Packs>
    struct pack_concat;

    template<class ...As, class ...Bs, class ...Packs>
    struct pack_concat<pack<As...>, pack<Bs...>, Packs...> {
        using type = typename pack_concat<pack<As..., Bs...>, Packs...>::type;
    };

    template<class Pack>
    struct pack_concat<Pack> {
        using type = Pack;
    };

    template<>
    struct pack_concat<> {
        using type = pack<>;
    };

    // Concat all types in all Packs to single pack
    template<class ...Packs>
    using pack_concat_t = typename pack_concat<Packs...>::type;

    // ------------------------------------------------

    template<std::size_t I, class Pack>
    struct pack_erase {
        using type = typename pack_concat<typename pack_take<I, Pack>::type, typename pack_drop<I + 1, Pack>::type>::type;
    };

    // Erase index I from Pack
    template<std::size_t I, class Pack>
    using pack_erase_t = typename pack_erase<I, Pack>::type;

    // ------------------------------------------------

    namespace detail {
        template<class Indices, class Pack, class Result = pack<>, std::size_t Index = 0>
        struct pack_erase_all;

        template<std::size_t ...Is, class Ty, class ...Tys, class ...Result, std::size_t Index>
            requires ((Is != Index) && ...) // Index not in erase
        struct pack_erase_all<std::index_sequence<Is...>, pack<Ty, Tys...>, pack<Result...>, Index> {
            using type = typename pack_erase_all<std::index_sequence<Is...>, pack<Tys...>, pack<Result..., Ty>, Index + 1>::type;
        };

        template<std::size_t ...Is, class Ty, class ...Tys, class Result, std::size_t Index>
            requires ((Is == Index) || ...) // Index in erase
        struct pack_erase_all<std::index_sequence<Is...>, pack<Ty, Tys...>, Result, Index> {
            using type = typename pack_erase_all<std::index_sequence<Is...>, pack<Tys...>, Result, Index + 1>::type;
        };

        template<class Indices, class Result, std::size_t Index>
        struct pack_erase_all<Indices, pack<>, Result, Index> {
            using type = Result;
        };
    }

    template<class Indices, class Pack>
    struct pack_erase_all {
        using type = typename detail::pack_erase_all<Indices, Pack>::type;
    };

    // Erase all Indices from Pack
    template<class Indices, class Pack>
    using pack_erase_all_t = typename pack_erase_all<Indices, Pack>::type;

    // ------------------------------------------------

    template<class Type, std::size_t I, class Pack>
    struct pack_insert {
        using type = typename pack_concat<typename pack_take<I, Pack>::type, pack<Type>, typename pack_drop<I, Pack>::type>::type;
    };

    // Insert Type in Pack at index I
    template<class Type, std::size_t I, class Pack>
    using pack_insert_t = typename pack_insert<Type, I, Pack>::type;

    // ------------------------------------------------

    template<class Types, std::size_t I, class Pack>
    struct pack_insert_all {
        using type = typename pack_concat<typename pack_take<I, Pack>::type, Types, typename pack_drop<I, Pack>::type>::type;
    };

    // Insert all Types in Pack at index I
    template<class Types, std::size_t I, class Pack>
    using pack_insert_all_t = typename pack_insert_all<Types, I, Pack>::type;

    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_filter {
        using type = typename pack_at_indices<typename pack_indices_filter<Filter, Pack>::type, Pack>::type;
    };

    // Only keep matches of Filter in Pack
    template<template<class> class Filter, class Pack>
    using pack_filter_t = typename pack_filter<Filter, Pack>::type;

    // ------------------------------------------------

    template<template<class> class Filter, class Pack>
    struct pack_erase_filter {
        using type = typename pack_at_indices<typename pack_indices_filter<filter_invert<Filter>::type, Pack>::type, Pack>::type;
    };

    // Remove matches of Filter from Pack
    template<template<class> class Filter, class Pack>
    using pack_erase_filter_t = typename pack_erase_filter<Filter, Pack>::type;

    // ------------------------------------------------

    template<class Type, class Pack>
    struct pack_remove {
        using type = typename pack_erase_all<typename pack_indices_of<Type, Pack>::type, Pack>::type;
    };

    // Remove Type from Pack
    template<class Type, class Pack>
    using pack_remove_t = typename pack_remove<Type, Pack>::type;

    // ------------------------------------------------

    template<std::size_t I, class Indices>
    struct indices_remove {
        using type = typename detail::to_indices<typename pack_remove<detail::index_as_type<I>, typename detail::indices_as_pack<Indices>::type>::type>::type;
    };

    // Remove I from Indices
    template<std::size_t I, class Indices>
    using indices_remove_t = typename indices_remove<I, Indices>::type;

    // ------------------------------------------------

    template<class Types, class Pack>
    struct pack_remove_all {
        using type = typename pack_erase_all<typename pack_indices_of_all<Types, Pack>::type, Pack>::type;
    };

    // Remove all Types from Pack
    template<class Types, class Pack>
    using pack_remove_all_t = typename pack_remove_all<Types, Pack>::type;

    // ------------------------------------------------

    template<class Is, class Indices>
    struct indices_remove_all {
        using type = typename detail::to_indices<typename pack_remove_all<typename detail::indices_as_pack<Is>::type, typename detail::indices_as_pack<Indices>::type>::type>::type;
    };

    // Remove all Is from Indices
    template<class Is, class Indices>
    using indices_remove_all_t = typename indices_remove_all<Is, Indices>::type;

    // ------------------------------------------------

    namespace detail {
        template<class, class>
        struct pack_reverse;

        template<std::size_t ...Is, class Pack>
        struct pack_reverse<std::index_sequence<Is...>, Pack> {
            using type = pack<typename pack_element<pack_size<Pack>::value - Is - 1ull, Pack>::type...>;
        };
    }

    template<class Pack>
    struct pack_reverse;

    template<class ...Tys>
    struct pack_reverse<pack<Tys...>> {
        using type = typename detail::pack_reverse<std::index_sequence_for<Tys...>, pack<Tys...>>::type;
    };

    // Reverse order of elements in Pack
    template<class Pack>
    using pack_reverse_t = typename pack_reverse<Pack>::type;

    // ------------------------------------------------

    template<class Indices>
    struct indices_reverse {
        using type = typename detail::to_indices<typename pack_reverse<typename detail::indices_as_pack<Indices>::type>::type>::type;
    };

    // Reverse Indices
    template<class Indices>
    using indices_reverse_t = typename indices_reverse<Indices>::type;

    // ------------------------------------------------

    template<template<class> class Transform, class Pack>
    struct pack_transform;

    template<template<class> class Transform, class ...Tys>
    struct pack_transform<Transform, pack<Tys...>> {
        using type = pack<Transform<Tys>...>;
    };

    // Transform elements in Pack using Transform
    template<template<class> class Transform, class Pack>
    using pack_transform_t = typename pack_transform<Transform, Pack>::type;

    // ------------------------------------------------

    template<class Type, class Pack>
    struct pack_append;

    template<class Type, class ...Tys>
    struct pack_append<Type, pack<Tys...>> {
        using type = pack<Tys..., Type>;
    };

    // Append Type to Pack
    template<class Type, class Pack>
    using pack_append_t = typename pack_append<Type, Pack>::type;

    // ------------------------------------------------

    template<class Types, class Pack>
    struct pack_append_all;

    template<class ...Types, class ...Tys>
    struct pack_append_all<pack<Types...>, pack<Tys...>> {
        using type = pack<Tys..., Types...>;
    };

    // Append all Types to Pack
    template<class Types, class Pack>
    using pack_append_all_t = typename pack_append_all<Types, Pack>::type;

    // ------------------------------------------------

    template<class Type, class Pack>
    struct pack_prepend;

    template<class Type, class ...Tys>
    struct pack_prepend<Type, pack<Tys...>> {
        using type = pack<Type, Tys...>;
    };

    // Prepend Type to Pack
    template<class Type, class Pack>
    using pack_prepend_t = typename pack_prepend<Type, Pack>::type;

    // ------------------------------------------------

    template<class Types, class Pack>
    struct pack_prepend_all;

    template<class ...Types, class ...Tys>
    struct pack_prepend_all<pack<Types...>, pack<Tys...>> {
        using type = pack<Types..., Tys...>;
    };

    // Prepend all Types to Pack
    template<class Types, class Pack>
    using pack_prepend_all_t = typename pack_prepend_all<Types, Pack>::type;

    // ------------------------------------------------

    template<class Type, std::size_t I, class Pack>
    struct pack_swap {
        using type = typename pack_insert<Type, I, typename pack_erase<I, Pack>::type>::type;
    };

    // Swap element I of Pack with Type
    template<class Type, std::size_t I, class Pack>
    using pack_swap_t = typename pack_swap<Type, I, Pack>::type;

    // ------------------------------------------------

    template<class Type, class Indices, class Pack>
    struct pack_swap_all;

    template<class Type, std::size_t ...Is, class ...Tys>
    struct pack_swap_all<Type, std::index_sequence<Is...>, pack<Tys...>> {
        template<class>
        struct _impl;

        template<std::size_t N, class Ty>
        using _element = std::conditional_t<((Is == N) || ...), Type, Ty>;

        template<std::size_t ...Ns>
        struct _impl<std::index_sequence<Ns...>> {
            using type = pack<_element<Ns, Tys>...>;
        };

        using type = typename _impl<std::index_sequence_for<Tys...>>::type;
    };

    // Swap all elements at Indices in Pack with Type
    template<class Type, class Indices, class Pack>
    using pack_swap_all_t = typename pack_swap_all<Type, Indices, Pack>::type;

    // ------------------------------------------------

    template<class Type, class Replacement, class Pack>
    struct pack_replace {
        using type = typename pack_swap_all<Replacement, typename pack_indices_of<Type, Pack>::type, Pack>::type;
    };

    // Replace Type in Pack with Replacement
    template<class Type, class Replacement, class Pack>
    using pack_replace_t = typename pack_replace<Type, Replacement, Pack>::type;
    
    // ------------------------------------------------

    template<class Types, class Replacement, class Pack>
    struct pack_replace_all {
        using type = typename pack_swap_all<Replacement, typename pack_indices_of_all<Types, Pack>::type, Pack>::type;
    };

    // Replace all Types in Pack with Replacement
    template<class Types, class Replacement, class Pack>
    using pack_replace_all_t = typename pack_replace_all<Types, Replacement, Pack>::type;

    // ------------------------------------------------

    template<template<class> class Filter, class Replacement, class Pack>
    struct pack_replace_filter;

    template<template<class> class Filter, class Replacement, class ...Tys>
    struct pack_replace_filter<Filter, Replacement, pack<Tys...>> {
        using type = pack<std::conditional_t<Filter<Tys>::value, Replacement, Tys>...>;
    };

    // Replace all matches of Filter in Pack with Replacement
    template<template<class> class Filter, class Replacement, class Pack>
    using pack_replace_filter_t = typename pack_replace_filter<Filter, Replacement, Pack>::type;

    // ------------------------------------------------

    template<std::size_t Start, std::size_t End, class Pack>
    struct pack_sub {
        using type = typename pack_take<End - Start, typename pack_drop<Start, Pack>::type>::type;
    };

    // Keep indices from Start to End in Pack
    template<std::size_t Start, std::size_t End, class Pack>
    using pack_sub_t = typename pack_sub<Start, End, Pack>::type;

    // ------------------------------------------------

    namespace detail {
        template<template<class, class> class Sorter, class Left, class Right>
        struct merge_sort_merge;

        template<template<class, class> class Sorter, class A, class ...As, class B, class ...Bs>
            requires (Sorter<A, B>::value)
        struct merge_sort_merge<Sorter, pack<A, As...>, pack<B, Bs...>> {
            using _recurse = typename merge_sort_merge<Sorter, pack<As...>, pack<B, Bs...>>::type;
            using type = typename pack_prepend<A, _recurse>::type;
        };

        template<template<class, class> class Sorter, class A, class ...As, class B, class ...Bs>
            requires (!Sorter<A, B>::value)
        struct merge_sort_merge<Sorter, pack<A, As...>, pack<B, Bs...>> {
            using _recurse = typename merge_sort_merge<Sorter, pack<A, As...>, pack<Bs...>>::type;
            using type = typename pack_prepend<B, _recurse>::type;
        };

        template<template<class, class> class Sorter, class ...As>
        struct merge_sort_merge<Sorter, pack<As...>, pack<>> {
            using type = pack<As...>;
        };

        template<template<class, class> class Sorter, class ...Bs>
        struct merge_sort_merge<Sorter, pack<>, pack<Bs...>> {
            using type = pack<Bs...>;
        };

        template<template<class, class> class Sorter, class Pack>
        struct merge_sort {
            constexpr static std::size_t _mid = pack_size<Pack>::value / 2;
            using _left = typename merge_sort<Sorter, typename pack_take<_mid, Pack>::type>::type;
            using _right = typename merge_sort<Sorter, typename pack_drop<_mid, Pack>::type>::type;
            using type = typename merge_sort_merge<Sorter, _left, _right>::type;
        };

        template<template<class, class> class Sorter, class Ty>
        struct merge_sort<Sorter, pack<Ty>> {
            using type = pack<Ty>;
        };

        template<template<class, class> class Sorter>
        struct merge_sort<Sorter, pack<>> {
            using type = pack<>;
        };
    }

    template<template<class, class> class Sorter, class Pack>
    struct pack_sort {
        using type = typename detail::merge_sort<Sorter, Pack>::type;
    };

    // Sort elements in Pack using Sorter
    template<template<class, class> class Sorter, class Pack>
    using pack_sort_t = typename pack_sort<Sorter, Pack>::type;

    // ------------------------------------------------
    // ------------------------------------------------
    // ------------------------------------------------

    template<class ...Args>
    struct template_pack : std::tuple<Args&&...> {

        // ------------------------------------------------

        constexpr template_pack(Args&...args)
            : std::tuple<Args&&...>{ std::forward<Args>(args)... }
        {}

        // ------------------------------------------------

    };

    // ------------------------------------------------

    namespace tuples::views {}

    // ------------------------------------------------

    namespace views = tuples::views;

    // ------------------------------------------------

    namespace tuples {

        // ------------------------------------------------

        template<class View>
        struct view_interface {};

        // ------------------------------------------------

        template<class Pipe>
        struct pipe_interface {};

        // ------------------------------------------------

        template<class Ty>
        concept view = std::derived_from<std::decay_t<Ty>, view_interface<std::decay_t<Ty>>> && requires() {
            { std::decay_t<Ty>::size } -> std::convertible_to<std::size_t>;
        };

        // ------------------------------------------------

        namespace detail {
            template<class Ty, std::size_t I>
            concept _owner_view = view<Ty> && !Ty::template is_reference<I>;

            template<class Ty, std::size_t I>
            concept _const_view = view<Ty> && Ty::template is_const<I>;
        }

        // ------------------------------------------------

        template<class Ty>
        concept pipe = std::derived_from<std::decay_t<Ty>, pipe_interface<std::decay_t<Ty>>>;

        template<class Pipe, class Type>
        concept pipe_for = pipe<Pipe> && std::invocable<Pipe, Type&&>;

        // ------------------------------------------------

        template<class Ty>
        concept tuple_like = requires() {
            typename std::tuple_size<std::decay_t<Ty>>::type;
        };

        // ------------------------------------------------

        template<tuple_like T, pipe_for<T> Ty>
        constexpr decltype(auto) operator|(T&& tuple, Ty&& val) {
            return std::forward<Ty>(val)(std::forward<T>(tuple));
        }
        
        // ------------------------------------------------

        template<view Self, std::size_t I>
        struct get_type {
            using _element = typename std::decay_t<Self>::template element<I>;
            constexpr static bool _self_value = !std::is_reference_v<Self>;
            constexpr static bool _self_rvalue = std::is_rvalue_reference_v<Self>;
            constexpr static bool _self_const = std::is_const_v<std::remove_reference_t<Self>>;
            constexpr static bool _owner = detail::_owner_view<std::decay_t<Self>, I>;
            constexpr static bool _const = detail::_const_view<std::decay_t<Self>, I>;
            constexpr static bool _rvalue = _owner ? _self_value || _self_rvalue : false;
            constexpr static bool _add_const = _const || (_self_const && _owner);
            using _type = std::conditional_t<_add_const, const _element, _element>;
            using type = std::conditional_t<_rvalue, _type&&, _type&>;
        };

        // Return-type of get<I> for Self
        template<view Self, std::size_t I>
        using get_type_t = typename get_type<Self, I>::type;
        
        // ------------------------------------------------

        template<view Self, std::size_t I>
        struct forward_type {
            using _element = typename std::decay_t<Self>::template element<I>;
            constexpr static bool _el_rvalue = std::is_rvalue_reference_v<_element>;
            constexpr static bool _el_lvalue = std::is_lvalue_reference_v<_element>;
            constexpr static bool _self_const = std::is_const_v<std::remove_reference_t<Self>>;
            constexpr static bool _owner = detail::_owner_view<std::decay_t<Self>, I>;
            constexpr static bool _const = detail::_const_view<std::decay_t<Self>, I>;
            constexpr static bool _rvalue = _owner ? true : _el_rvalue;
            constexpr static bool _lvalue = _owner ? _el_lvalue : true;
            constexpr static bool _add_const = _owner ? _self_const : _const;
            using _type = std::conditional_t<_add_const, const _element, _element>;
            using type = std::conditional_t<_rvalue, _type&&, std::conditional_t<_lvalue, _type&, _type>>;
        };

        // Return-type of forward<I> for Self
        template<view Self, std::size_t I>
        using forward_type_t = typename forward_type<Self, I>::type;

        // ------------------------------------------------

        template<view View>
        struct as_pack {
            template<class>
            struct _impl;

            template<std::size_t ...Is>
            struct _impl<std::index_sequence<Is...>> {
                using type = pack<typename View::template element<Is>...>;
            };

            using type = typename _impl<std::make_index_sequence<View::size>>::type;
        };

        template<view View>
        using as_pack_t = typename as_pack<View>::type;

        // ------------------------------------------------

    }

    // ------------------------------------------------

}

// ------------------------------------------------

namespace std {

    // ------------------------------------------------

    template<kaixo::tuples::view Ty>
    struct tuple_size<Ty> : integral_constant<std::size_t, Ty::size> {};

    template<std::size_t I, kaixo::tuples::view Ty>
    struct tuple_element<I, Ty> : type_identity<typename Ty::template element<I>> {};
    
    // ------------------------------------------------

    template<std::size_t I, kaixo::tuples::view Ty>
        requires (I < decay_t<Ty>::size)
    constexpr decltype(auto) get(Ty&& view) {
        return std::forward<Ty>(view).template get<I>();
    }

    // ------------------------------------------------

    template<class ...Tys>
    struct tuple_size<kaixo::template_pack<Tys...>> : integral_constant<std::size_t, sizeof...(Tys)> {};

    template<std::size_t I, class ...Tys>
    struct tuple_element<I, kaixo::template_pack<Tys...>>
         : tuple_element<I, tuple<Tys&&...>> {};

    // ------------------------------------------------

}

// ------------------------------------------------

namespace kaixo {

    // ------------------------------------------------

    namespace tuples {

        // ------------------------------------------------

        template<tuple_like Tpl>
        struct ref_view : view_interface<ref_view<Tpl>> {

            // ------------------------------------------------

            constexpr ref_view(Tpl& tpl)
                : m_Tuple(&tpl)
            {}

            // ------------------------------------------------

            template<std::size_t>
            constexpr static bool is_const = std::is_const_v<Tpl>;

            template<std::size_t>
            constexpr static bool is_reference = true;

            constexpr static std::size_t size = std::tuple_size_v<Tpl>;

            // ------------------------------------------------

            template<std::size_t I>
                requires (I < size)
            using element = std::tuple_element_t<I, std::decay_t<Tpl>>;

            // ------------------------------------------------

            template<std::size_t I, class Self>
                requires (I < size)
            constexpr get_type_t<Self, I> get(this Self&& self) {
                return std::get<I>(*std::forward<Self>(self).m_Tuple);
            }

            // ------------------------------------------------

        private:
            Tpl* m_Tuple{};

            // ------------------------------------------------

        };

        // ------------------------------------------------

        template<tuple_like Tpl>
        struct owning_view : view_interface<owning_view<Tpl>> {

            // ------------------------------------------------

            constexpr owning_view(Tpl&& tpl)
                : m_Tuple(std::move(tpl))
            {}
            
            constexpr owning_view(const Tpl&& tpl)
                : m_Tuple(tpl)
            {}

            // ------------------------------------------------

            template<std::size_t>
            constexpr static bool is_const = std::is_const_v<Tpl>;

            template<std::size_t>
            constexpr static bool is_reference = false;

            constexpr static std::size_t size = std::tuple_size_v<Tpl>;

            // ------------------------------------------------

            template<std::size_t I>
                requires (I < size)
            using element = std::tuple_element_t<I, std::decay_t<Tpl>>;

            // ------------------------------------------------

            template<std::size_t I, class Self>
                requires (I < size)
            constexpr get_type_t<Self, I> get(this Self&& self) {
                return std::get<I>(std::forward<Self>(self).m_Tuple);
            }

            // ------------------------------------------------

        private:
            Tpl m_Tuple{};

            // ------------------------------------------------

        };

        // ------------------------------------------------

        struct empty_view : view_interface<empty_view> {

            // ------------------------------------------------

            template<std::size_t>
            constexpr static bool is_const = false;

            template<std::size_t>
            constexpr static bool is_reference = false;

            constexpr static std::size_t size = 0;

            // ------------------------------------------------

        };

        // ------------------------------------------------

        namespace views {

            // ------------------------------------------------

            struct _all_fun : pipe_interface<_all_fun> {

                template<tuple_like Tpl>
                constexpr auto operator()(Tpl&& val) const {
                    if constexpr (view<Tpl>) {
                        return std::forward<Tpl>(val);
                    } else if constexpr (std::tuple_size_v<std::decay_t<Tpl>> == 0) {
                        return empty_view{};
                    } else if constexpr (std::is_lvalue_reference_v<Tpl>) {
                        return ref_view{ std::forward<Tpl>(val) };
                    } else {
                        return owning_view{ std::forward<Tpl>(val) };
                    }
                }

            };

            constexpr _all_fun all{};

            template<class Ty>
            using all_t = decltype(all(std::declval<Ty>()));

            // ------------------------------------------------

        }

        // ------------------------------------------------

        namespace views {

            // ------------------------------------------------

            template<std::size_t I>
            struct _forward_fun : pipe_interface<_forward_fun<I>> {

                template<view View>
                    requires (I <= std::decay_t<View>::size)
                constexpr forward_type_t<View, I> operator()(View&& view) const {
                    return static_cast<forward_type_t<View, I>>(std::forward<View>(view).template get<I>());
                }
                
                template<tuple_like Tpl>
                    requires (I <= all_t<Tpl>::size && !view<Tpl>)
                constexpr forward_type_t<all_t<Tpl>, I> operator()(Tpl&& tuple) const {
                    return static_cast<forward_type_t<all_t<Tpl>, I>>(std::get<I>(std::forward<Tpl>(tuple)));
                }
                
            };

            // Perfect forward the Ith element
            template<std::size_t I>
            constexpr _forward_fun<I> forward{};

            // ------------------------------------------------

        }

        // ------------------------------------------------

        template<std::size_t A, std::size_t B, view View>
        struct sub_view : view_interface<sub_view<A, B, View>> {

            // ------------------------------------------------

            View view;

            // ------------------------------------------------

            template<std::size_t N>
            constexpr static bool is_const = View::template is_const<N + A>;

            template<std::size_t N>
            constexpr static bool is_reference = View::template is_reference<N + A>;

            constexpr static std::size_t size = B - A;

            // ------------------------------------------------

            template<std::size_t N>
                requires (N < size)
            using element = typename View::template element<N + A>;

            // ------------------------------------------------

            template<std::size_t N, class Self>
                requires (N < size)
            constexpr get_type_t<Self, N> get(this Self&& self) {
                return std::forward<Self>(self).view.template get<N + A>();
            }

            // ------------------------------------------------

        };

        // ------------------------------------------------

        namespace views {

            // ------------------------------------------------

            template<std::size_t I>
            struct _take_fun : pipe_interface<_take_fun<I>> {

                template<tuple_like Tpl>
                    requires (I <= all_t<Tpl>::size)
                constexpr auto operator()(Tpl&& tuple) const {
                    if constexpr (I == 0) {
                        return empty_view{};
                    } else {
                        return sub_view<0, I, all_t<Tpl>>{.view = all(std::forward<Tpl>(tuple)) };
                    }
                }

            };

            // Take first I elements
            template<std::size_t I>
            constexpr _take_fun<I> take{};

            // ------------------------------------------------

            template<std::size_t I>
            struct _take_last_fun : pipe_interface<_take_last_fun<I>> {

                template<tuple_like Tpl>
                    requires (I <= all_t<Tpl>::size)
                constexpr auto operator()(Tpl&& tuple) const {
                    if constexpr (I == 0) {
                        return empty_view{};
                    } else {
                        return sub_view<all_t<Tpl>::size - I, all_t<Tpl>::size, all_t<Tpl>>{.view = all(std::forward<Tpl>(tuple)) };
                    }
                }

            };

            // Take last I elements
            template<std::size_t I>
            constexpr _take_last_fun<I> take_last{};

            // ------------------------------------------------

            template<template<class> class Filter, template<template<class> class, class> class Take>
            struct _take_filter_fun : pipe_interface<_take_filter_fun<Filter, Take>> {

                template<tuple_like Tpl>
                constexpr auto operator()(Tpl&& tuple) const {
                    using _view = all_t<Tpl>;
                    using _pack = typename as_pack<_view>::type;
                    using _taken = typename Take<Filter, _pack>::type;
                    constexpr std::size_t _take = pack_size<_taken>::value;

                    if constexpr (_take == 0) {
                        return empty_view{};
                    } else {
                        return sub_view<0, _take, _view>{.view = all(std::forward<Tpl>(tuple)) };
                    }
                }

            };

            // Take while Filter matches
            template<template<class> class Filter>
            constexpr _take_filter_fun<Filter, pack_take_while> take_while{};

            // Take until Filter matches
            template<template<class> class Filter>
            constexpr _take_filter_fun<Filter, pack_take_until> take_until{};

            // ------------------------------------------------

            template<template<class> class Filter, template<template<class> class, class> class Take>
            struct _take_last_filter_fun : pipe_interface<_take_last_filter_fun<Filter, Take>> {

                template<tuple_like Tpl>
                constexpr auto operator()(Tpl&& tuple) const {
                    using _view = all_t<Tpl>;
                    using _pack = typename as_pack<_view>::type;
                    using _taken = typename Take<Filter, _pack>::type;
                    constexpr std::size_t _drop = _view::size - pack_size<_taken>::value;

                    if constexpr (pack_size<_taken>::value == 0) {
                        return empty_view{};
                    } else {
                        return sub_view<_drop, _view::size, _view>{.view = all(std::forward<Tpl>(tuple)) };
                    }
                }

            };

            // Take from end while Filter matches
            template<template<class> class Filter>
            constexpr _take_last_filter_fun<Filter, pack_take_last_while> take_last_while{};

            // Take from end until Filter matches
            template<template<class> class Filter>
            constexpr _take_last_filter_fun<Filter, pack_take_last_until> take_last_until{};

            // ------------------------------------------------

            template<std::size_t I>
            struct _drop_fun : pipe_interface<_drop_fun<I>> {

                template<tuple_like Tpl>
                    requires (I <= all_t<Tpl>::size)
                constexpr auto operator()(Tpl&& tuple) const {
                    if constexpr (I == all_t<Tpl>::size) {
                        return empty_view{};
                    } else {
                        return sub_view<I, all_t<Tpl>::size, all_t<Tpl>>{.view = all(std::forward<Tpl>(tuple)) };
                    }
                }

            };

            // Drop first I elements
            template<std::size_t I>
            constexpr _drop_fun<I> drop{};

            // ------------------------------------------------

            template<std::size_t I>
            struct _drop_last_fun : pipe_interface<_drop_last_fun<I>> {

                template<tuple_like Tpl>
                    requires (I <= all_t<Tpl>::size)
                constexpr auto operator()(Tpl&& tuple) const {
                    if constexpr (I == all_t<Tpl>::size) {
                        return empty_view{};
                    } else {
                        return sub_view<0, all_t<Tpl>::size - I, all_t<Tpl>>{.view = all(std::forward<Tpl>(tuple)) };
                    }
                }

            };

            // Drop last I elements
            template<std::size_t I>
            constexpr _drop_last_fun<I> drop_last{};

            // ------------------------------------------------

            template<template<class> class Filter, template<template<class> class, class> class Drop>
            struct _drop_filter_fun : pipe_interface<_drop_filter_fun<Filter, Drop>> {

                template<tuple_like Tpl>
                constexpr auto operator()(Tpl&& tuple) const {
                    using _view = all_t<Tpl>;
                    using _pack = typename as_pack<_view>::type;
                    using _dropped = typename Drop<Filter, _pack>::type;
                    constexpr std::size_t _drop = pack_size<_pack>::value - pack_size<_dropped>::value;

                    if constexpr (pack_size<_dropped>::value == 0) {
                        return empty_view{};
                    } else {
                        return sub_view<_drop, _view::size, _view>{.view = all(std::forward<Tpl>(tuple)) };
                    }
                }

            };

            // Drop while Filter matches
            template<template<class> class Filter>
            constexpr _drop_filter_fun<Filter, pack_drop_while> drop_while{};

            // Drop until Filter matches
            template<template<class> class Filter>
            constexpr _drop_filter_fun<Filter, pack_drop_until> drop_until{};

            // ------------------------------------------------

            template<template<class> class Filter, template<template<class> class, class> class Drop>
            struct _drop_last_filter_fun : pipe_interface<_drop_last_filter_fun<Filter, Drop>> {

                template<tuple_like Tpl>
                constexpr auto operator()(Tpl&& tuple) const {
                    using _view = all_t<Tpl>;
                    using _pack = typename as_pack<_view>::type;
                    using _dropped = typename Drop<Filter, _pack>::type;
                    constexpr std::size_t _take = pack_size<_dropped>::value;

                    if constexpr (pack_size<_dropped>::value == 0) {
                        return empty_view{};
                    } else {
                        return sub_view<0, _take, _view>{.view = all(std::forward<Tpl>(tuple)) };
                    }
                }

            };

            // Drop from end while Filter matches
            template<template<class> class Filter>
            constexpr _drop_last_filter_fun<Filter, pack_drop_last_while> drop_last_while{};

            // Drop from end until Filter matches
            template<template<class> class Filter>
            constexpr _drop_last_filter_fun<Filter, pack_drop_last_until> drop_last_until{};

            // ------------------------------------------------
            
            template<std::size_t A, std::size_t B>
                requires (A <= B)
            struct _sub_fun : pipe_interface<_sub_fun<A, B>> {

                template<tuple_like Tpl>
                    requires (B <= all_t<Tpl>::size)
                constexpr auto operator()(Tpl&& tuple) const {
                    if constexpr (A == B) {
                        return empty_view{};
                    } else {
                        return sub_view<A, B, all_t<Tpl>>{.view = all(std::forward<Tpl>(tuple)) };
                    }
                }

            };

            template<std::size_t A, std::size_t B>
            constexpr _sub_fun<A, B> sub{};

            // ------------------------------------------------

        }

        // ------------------------------------------------

        template<view View, template<class> class Indices>
        struct indices_view : view_interface<indices_view<View, Indices>> {

            // ------------------------------------------------

            using _pack = typename as_pack<View>::type;
            using _indices = typename Indices<_pack>::type;
            using _at_indices = typename pack_at_indices<_indices, _pack>::type;

            // ------------------------------------------------

            View view;

            // ------------------------------------------------

            template<std::size_t N>
            constexpr static bool is_const = View::template is_const<indices_element<N, _indices>::value>;

            template<std::size_t N>
            constexpr static bool is_reference = View::template is_reference<indices_element<N, _indices>::value>;

            constexpr static std::size_t size = pack_size<_at_indices>::value;

            // ------------------------------------------------

            template<std::size_t N>
                requires (N < size)
            using element = typename pack_element<N, _at_indices>::type;

            // ------------------------------------------------

            template<std::size_t N, class Self>
                requires (N < size)
            constexpr get_type_t<Self, N> get(this Self&& self) {
                constexpr std::size_t _index = indices_element<N, _indices>::value;
                return std::forward<Self>(self).view.template get<_index>();
            }

            // ------------------------------------------------

        };

        // ------------------------------------------------

        namespace views {

            // ------------------------------------------------

            template<template<class> class Indices>
            struct _indices_fun : pipe_interface<_indices_fun<Indices>> {

                template<tuple_like Tpl>
                constexpr auto operator()(Tpl&& tuple) const {
                    if constexpr (indices_view<all_t<Tpl>, Indices>::size == 0) {
                        return empty_view{};
                    } else {
                        return indices_view<all_t<Tpl>, Indices>{.view = all(std::forward<Tpl>(tuple)) };
                    }
                }

            };

            // First unique occurence of each type
            constexpr _indices_fun<pack_first_indices> unique{};

            // First unique occurence of each type
            constexpr _indices_fun<pack_first_indices> first_unique{};

            // Last unique occurence of each type
            constexpr _indices_fun<pack_last_indices> last_unique{};

            namespace detail {
                template<std::size_t N>
                struct _pack_nth_indices_last_unique {
                    template<class Pack>
                    using type = pack_nth_indices<N, Pack>;
                };
            }

            // Nth unique occurence of each type
            template<std::size_t N>
            constexpr _indices_fun<typename detail::_pack_nth_indices_last_unique<N>::type> nth_unique{};

            // ------------------------------------------------
            
            namespace detail {
                template<template<class, class> class Ty, class Type>
                struct partial_first_simple_pair {
                    template<class Pack>
                    using type = Ty<Type, Pack>;
                };
            }

            // Remove Type
            template<class Type>
            constexpr _indices_fun<typename detail::partial_first_simple_pair<pack_indices_not_of, Type>::type> remove{};
            
            // Remove all Types
            template<class Types>
            constexpr _indices_fun<typename detail::partial_first_simple_pair<pack_indices_not_of_all, Types>::type> remove_all{};

            // ------------------------------------------------

            namespace detail {
                template<template<template<class> class, class> class Ty, template<class> class Filter>
                struct partial_first_filter_pair {
                    template<class Pack>
                    using type = Ty<Filter, Pack>;
                };
            }

            // Only keep all that match Filter
            template<template<class> class Filter>
            constexpr _indices_fun<typename detail::partial_first_filter_pair<pack_indices_filter, Filter>::type> filter{};
            
            // Erase all that match Filter
            template<template<class> class Filter>
            constexpr _indices_fun<typename detail::partial_first_filter_pair<pack_indices_not_filter, Filter>::type> erase_filter{};

            // ------------------------------------------------
            
            namespace detail {
                template<class Indices>
                struct remove_indices {
                    template<class Pack>
                    using type = indices_remove_all<Indices, std::make_index_sequence<pack_size<Pack>::value>>;
                };
            }

            // Erase index I
            template<std::size_t I>
            constexpr _indices_fun<typename detail::remove_indices<std::index_sequence<I>>::type> erase{};
            
            // Erase all Indices
            template<class Indices>
            constexpr _indices_fun<typename detail::remove_indices<Indices>::type> erase_all{};

            // ------------------------------------------------

            namespace detail {
                template<class Pack>
                struct reverse_indices {
                    using type = typename indices_reverse<std::make_index_sequence<pack_size<Pack>::value>>::type;
                };
            }

            // Reverse
            constexpr _indices_fun<typename detail::reverse_indices> reverse{};

            // ------------------------------------------------

        }

        // ------------------------------------------------
        
        namespace views::detail {

            template<class Fun, class ...Args>
            struct _capture_closure : pipe_interface<_capture_closure<Fun, Args...>> {

                std::tuple<Args...> captures;

                template<tuple_like Tpl>
                    requires std::invocable<Fun, Tpl&&, Args&&...>
                constexpr auto operator()(Tpl&& tuple) const {
                    return std::apply(Fun{}, 
                        std::tuple_cat(std::forward_as_tuple(std::forward<Tpl>(tuple)), std::move(captures)));
                }

            };

        }

        // ------------------------------------------------
        
        template<view View, std::size_t I, class ...Args>
        struct insert_view : view_interface<insert_view<View, I, Args...>> {

            // ------------------------------------------------

            View view;
            std::tuple<Args...> captures;

            // ------------------------------------------------

            template<std::size_t N>
            constexpr static bool is_const = N < I                             ? View::template is_const<N>
                                           : N >= I && N < I + sizeof...(Args) ? false 
                                           :                                     View::template is_const<N - sizeof...(Args)>;
            
            template<std::size_t N>
            constexpr static bool is_reference = N < I                             ? View::template is_reference<N>
                                               : N >= I && N < I + sizeof...(Args) ? false 
                                               :                                     View::template is_reference<N - sizeof...(Args)>;

            constexpr static std::size_t size = View::size + sizeof...(Args);

            // ------------------------------------------------

            template<std::size_t N>
            struct _impl;
            
            template<std::size_t N>
                requires (N < I)
            struct _impl<N> {
                using type = typename View::template element<N>;
            };
            
            template<std::size_t N>
                requires (N >= I && N < I + sizeof...(Args))
            struct _impl<N> {
                using type = typename pack_element<N - I, pack<Args...>>::type;
            };

            template<std::size_t N>
                requires (N >= I + sizeof...(Args))
            struct _impl<N> {
                using type = typename View::template element<N - sizeof...(Args)>;
            };

            template<std::size_t N>
                requires (N < size)
            using element = typename _impl<N>::type;

            // ------------------------------------------------

            template<std::size_t N, class Self>
                requires (N < size)
            constexpr get_type_t<Self, N> get(this Self&& self) {
                if constexpr (N < I) {
                    return std::forward<Self>(self).view.template get<N>();
                } else if constexpr (N >= I && N < I + sizeof...(Args)) {
                    return static_cast<get_type_t<Self, N>>(std::get<N - I>(std::forward<Self>(self).captures));
                } else {
                    return std::forward<Self>(self).view.template get<N - sizeof...(Args)>();
                }
            }

            // ------------------------------------------------

        };

        namespace views {

            template<std::size_t I>
            struct _insert_fun : pipe_interface<_insert_fun<I>> {

                template<class ...Args>
                constexpr auto operator()(Args&& ...args) const {
                    return detail::_capture_closure<_insert_fun<I>, Args...>{
                        .captures = std::forward_as_tuple(std::forward<Args>(args)...) 
                    };
                }
                
                template<tuple_like Tpl, class ...Args>
                constexpr auto operator()(Tpl&& tuple, Args&& ...args) const {
                    constexpr std::size_t _index = I == npos ? all_t<Tpl>::size : I;
                    if constexpr (all_t<Tpl>::size == 0 && sizeof...(Args) == 0) {
                        return empty_view{};
                    } else if constexpr (sizeof...(Args) == 0) {
                        return all(std::forward<Tpl>(tuple));
                    } else if constexpr (_index <= all_t<Tpl>::size) {
                        return insert_view<all_t<Tpl>, _index, Args...>{
                            .view = all(std::forward<Tpl>(tuple)),
                            .captures = std::forward_as_tuple(std::forward<Args>(args)...),
                        };
                    }
                }

            };

            // Insert at index I
            template<std::size_t I>
            constexpr _insert_fun<I> insert{};

            // Append
            constexpr _insert_fun<npos> append{};
           
            // Prepend
            constexpr _insert_fun<0> prepend{};

        }

        // ------------------------------------------------
    
        template<view A, view B>
        struct concat_view : view_interface<concat_view<A, B>> {

            // ------------------------------------------------

            A a;
            B b;

            // ------------------------------------------------

            template<std::size_t I>
            constexpr static bool is_const = I < A::size 
                                           ? A::template is_const<I> 
                                           : B::template is_const<I - A::size>;
        
            template<std::size_t I>
            constexpr static bool is_reference = I < A::size 
                                               ? A::template is_reference<I> 
                                               : B::template is_reference<I - A::size>;

            constexpr static std::size_t size = A::size + B::size;

            // ------------------------------------------------

            template<std::size_t N>
            struct _impl;
        
            template<std::size_t N>
                requires (N < A::size)
            struct _impl<N> {
                using type = typename A::template element<N>;
            };
        
            template<std::size_t N>
                requires (N >= A::size)
            struct _impl<N> {
                using type = typename B::template element<N - A::size>;
            };

            template<std::size_t N>
                requires (N < size) 
            using element = typename _impl<N>::type;

            // ------------------------------------------------
        
            template<std::size_t N, class Self>
                requires (N < size)
            constexpr get_type_t<Self, N> get(this Self&& self) {
                if constexpr (N < A::size) {
                    return std::forward<Self>(self).a.template get<N>();
                } else {
                    return std::forward<Self>(self).b.template get<N - A::size>();
                }
            }

            // ------------------------------------------------

        };

        namespace views {

            struct _concat_fun : pipe_interface<_concat_fun> {

                template<tuple_like A, tuple_like B>
                constexpr auto operator()(A&& a, B&& b) const {
                    if constexpr (all_t<A>::size == 0 && all_t<B>::size == 0) {
                        return empty_view{};
                    } else if constexpr (all_t<A>::size == 0) {
                        return all(std::forward<B>(b));
                    } else if constexpr (all_t<B>::size == 0) {
                        return all(std::forward<A>(a));
                    } else {
                        return concat_view<all_t<A>, all_t<B>>{
                            .a = std::forward<A>(a),
                            .b = std::forward<B>(b),
                        };
                    }
                }
                
                template<tuple_like A>
                constexpr auto operator()(A&& a) const {
                    if constexpr (all_t<A>::size == 0) {
                        return empty_view{};
                    } else {
                        return detail::_capture_closure<_concat_fun, all_t<A>>{
                            .captures = std::forward_as_tuple(all(std::forward<A>(a))),
                        };
                    }
                }

            };

            // Concat
            constexpr _concat_fun concat{};

            // ------------------------------------------------

            struct _call_fun : pipe_interface<_call_fun> {

                template<tuple_like Tpl, class Lambda, std::size_t ...Is>
                constexpr auto _impl(Tpl&& tuple, Lambda&& lambda, std::index_sequence<Is...>) const {
                    return std::forward<Lambda>(lambda)(std::get<Is>(std::forward<Tpl>(tuple))...);
                }

                template<tuple_like Tpl, class Lambda>
                constexpr auto operator()(Tpl&& tuple, Lambda&& lambda) const {
                    return _impl(std::forward<Tpl>(tuple), std::forward<Lambda>(lambda), 
                        std::make_index_sequence<std::tuple_size_v<std::decay_t<Tpl>>>{});
                }
                
                template<class Lambda>
                constexpr auto operator()(Lambda&& lambda) const {
                    return detail::_capture_closure<_call_fun, Lambda>{
                        .captures = std::forward_as_tuple(std::forward<Lambda>(lambda))
                    };
                }

            };

            constexpr _call_fun call{};

            // ------------------------------------------------

            template<class Op>
            struct _fold_fun : pipe_interface<_fold_fun<Op>> {

                template<std::size_t I, tuple_like Tpl>
                constexpr auto _impl(Tpl&& tuple) const {
                    if constexpr (I == std::tuple_size_v<std::decay_t<Tpl>> - 1) {
                        return std::get<I>(std::forward<Tpl>(tuple));
                    } else {
                        return Op{}(std::get<I>(std::forward<Tpl>(tuple)), _impl<I + 1>(std::forward<Tpl>(tuple)));
                    }
                }

                template<tuple_like Tpl>
                constexpr auto operator()(Tpl&& tuple) const {
                    return _impl<0>(std::forward<Tpl>(tuple));
                }

            };

            template<class Op>
            constexpr _fold_fun<Op> fold{};

        }

        // ------------------------------------------------
        
        namespace detail {

            // ------------------------------------------------

            template<tuple_like Tpl>
            struct tuple_view_tuple { // Interface for retrieving tuple
                constexpr virtual Tpl& _tuple() const = 0;
            };

            template<class Ty, std::size_t, class = void>
            struct tuple_view_get { // Interface for element in tuple_view
                constexpr virtual const Ty& _get() const = 0;
            };

            template<class Ty, std::size_t I, tuple_like Tpl>
            struct tuple_view_get<Ty, I, Tpl> : virtual tuple_view_get<Ty, I>, // Base class for element I
                                                virtual tuple_view_tuple<Tpl>  // interface for getting tuple
            {   // Implementation for element I in Tpl, uses virtual inheritance
                constexpr const Ty& _get() const override { return std::get<I>(this->_tuple()); }
            };

            // ------------------------------------------------

            template<class Pack, class Ty = void, class Indices = std::make_index_sequence<pack_size<Pack>::value>>
            struct tuple_view_get_pack; // Inherits 'get' for all elements in Pack

            template<class ...Tys, class Ty, std::size_t ...Is>
            struct tuple_view_get_pack<pack<Tys...>, Ty, std::index_sequence<Is...>> 
                : virtual tuple_view_get<Tys, Is, Ty>... {};

            // ------------------------------------------------

            template<class Pack, class = void> // Base class, where no Tpl defined
            struct tuple_view_impl : tuple_view_get_pack<Pack> {};

            template<class Pack, tuple_like Tpl> // Implementation for base class
            struct tuple_view_impl<Pack, Tpl> : tuple_view_impl<Pack>,         // <<< Inherits base (for dynamic cast)
                                                tuple_view_get_pack<Pack, Tpl> // <<< implements base
            {   // Can't be constexpr because virtual base classes
                /*constexpr*/ tuple_view_impl(Tpl&& tpl)
                    : tuple(std::forward<Tpl>(tpl)) 
                {}

                Tpl&& tuple;

                constexpr Tpl& _tuple() const override { return tuple; } // <<< Implement virtual interface for tuple
            };

            // ------------------------------------------------
            
        }

        // ------------------------------------------------
        
        template<class ...Tys>
        struct tuple_view : view_interface<tuple_view<Tys...>> {

            // ------------------------------------------------

            using _pack = pack<Tys...>;

            // ------------------------------------------------

            template<std::size_t>
            constexpr static bool is_const = true;
            
            template<std::size_t>
            constexpr static bool is_reference = true;

            constexpr static std::size_t size = sizeof...(Tys);

            template<std::size_t I>
            using element = typename pack_element<I, _pack>::type;

            // ------------------------------------------------

            template<view Tpl, class Indices>
            struct _compatible {
                constexpr static bool value = false;
            };

            template<view Tpl, std::size_t ...Is>
                requires (Tpl::size == sizeof...(Is))
            struct _compatible<Tpl, std::index_sequence<Is...>> {
                constexpr static bool value = (std::same_as<const std::remove_reference_t<get_type_t<Tpl, Is>>, const element<Is>> && ...);
            };

            // ------------------------------------------------

            template<tuple_like Tpl>
                requires (_compatible<views::all_t<Tpl>, std::index_sequence_for<Tys...>>::value)
            constexpr tuple_view(Tpl&& tuple)
                : m_Ptr(std::make_shared<detail::tuple_view_impl<_pack, Tpl>>(std::forward<Tpl>(tuple)))
            {}

            template<class ...Args>
                requires (_compatible<views::all_t<std::tuple<Args...>>, std::index_sequence_for<Tys...>>::value)
            constexpr tuple_view(Args&& ...args)
                : tuple_view(std::forward_as_tuple(std::forward<Args>(args)...))
            {}

            // ------------------------------------------------

            template<std::size_t I>
            constexpr const element<I>& get() const {
                return dynamic_cast<detail::tuple_view_get<element<I>, I>*>(m_Ptr.get())->_get();
            }

            // ------------------------------------------------

        private:
            std::shared_ptr<detail::tuple_view_impl<_pack>> m_Ptr{};

            // ------------------------------------------------

        };

        // ------------------------------------------------

    }

    // ------------------------------------------------

}

// ------------------------------------------------