#include <iostream>
#include <string>
#include <source_location>
#include <vector>
#include <map>

/**
 * Type manipulator:
 * x transform<T<...>>                      Transform type to T<Ty>
 * - conditional_transform<Filter, T<...>>  Conditionally transform to T<Ty> if match Filter
 * - tparams                                Get the template parameters of Ty
 * - copy_tparams<T<...>>                   Copy tparams to T<Tys...>
 * - uninstantiate                          Remove the template parameters from Ty
 * - instantiate<T<...>>                    Add template parameters to T<Tys...>
 * - reinstantiate<T>                       Replace the template parameters from T like T<Tys...>
 */

/**
 * Pack info:
 * x element<I>                             Get I'th element
 * x contains<Ty>                           Contains Ty
 * x contains_all<Tys...>                   Contains all of Tys...
 * x contains_any<Tys...>                   Contains any of Tys...
 * x count<Ty>                              Number of occurences of Ty
 * x count_all<Tys...>                      Number of occurences of all Tys...
 * x count_filter<Filter>                   Number of occurences that match Filter
 * x count_unique                           Number of unique types
 * x index<Ty>                              First index of Ty
 * x index_filter<Filter>                   First index that matches Filter
 * x indices<Tys...>                        All indices of all Tys...
 * x indices_filter<Filter>                 All indices that match Filter
 * x first_index<Tys...>                    First index of any in Tys...
 * x first_indices<Tys...>                  First indices of all Tys...
 */

/**
 * Pack manipulators:
 * x reverse                                Reverse pack
 * x unique                                 Only keep first occurence of type
 * - join                                   Flatten pack of packs
 * - split<Tys...>                          Split pack at all Tys... (consumes)
 * - split_after<Tys...>                    Split pack after all Tys... (does not consume)
 * - split_before<Tys...>                   Split pack before all Tys... (does not consume)
 * - split_filter<Filter>                   Split pack at all Filter matches (consumes)
 * - split_after_filter<Filter>             Split pack after all Filter matches (does not consume)
 * - split_before_filter<Filter>            Split pack before all Filter matches (does not consume)
 * x sub<A, B>                              Only keep indices between A and B
 * x take<I>                                Take first I types
 * x take_while<Filter>                     Take while Filter matches
 * x take_until<Filter>                     Take until Filter matches
 * x drop<I>                                Drop first I types
 * x drop_while<Filter>                     Drop while Filter matches
 * x drop_until<Filter>                     Drop until Filter matches
 * - take_last<I>                           Take last I types
 * - take_last_while<Filter>                Take last while Filter matches
 * - drop_last<I>                           Drop last I types
 * - drop_last_while<Filter>                Drop last while Filter matches
 * x remove<Ty>                             Remove type Ty
 * x remove_all<Tys...>                     Remove types Tys...
 * x append<Ty>                             Append type Ty
 * x append_all<Tys...>                     Append types Tys...
 * x prepend<Ty>                            Prepend type Ty
 * x prepend_all<Tys...>                    Prepend types Tys...
 * x keep<I>                                Only keep index I
 * x keep_all<Is...>                        Only keep indices Is...
 * x erase<I>                               Remove index I
 * x erase_all<Is...>                       Remove all indices Is...
 * x insert<I, Ty>                          Insert Ty at index I
 * x insert_all<I, Tys...>                  Insert types Tys... at index I
 * x swap<I, B>                             Swap index I with B
 * x swap_all<Is..., B>                     Swap indices Is... with B
 * x replace<A, B...>                       Replace all B... with A
 * x replace_filter<A, Filter>              Replace Filter matches with A
 * x filter<Filter>                         Only keep types that match Filter
 * x erase_filter<Filter>                   Only keep types that do not match Filter
 * x sort<Sorter>                           Sort the types using the Sorter
 */

/**
 * Pack combiners:
 * x concat<Tys...>                         Concat all packs Tys...
 * - zip<Tys...>                            Zip all packs Tys...
 * - cartesian<Tys...>                      Cartesian product of all packs Tys...
 *
 */

 // ------------------------------------------------

namespace kaixo {

    // ------------------------------------------------

    constexpr std::size_t npos = static_cast<std::size_t>(-1);

    // ------------------------------------------------

    // Combine filters using logical and
    template<template<class> class ...Filter>
    struct filter_and {
        template<class Ty>
        struct type {
            constexpr static bool value = (static_cast<bool>(Filter<Ty>::value) && ...);
        };
    };

    // ------------------------------------------------

    // Combine filters using logical or
    template<template<class> class ...Filter>
    struct filter_or {
        template<class Ty>
        struct type {
            constexpr static bool value = (static_cast<bool>(Filter<Ty>::value) || ...);
        };
    };
    
    // ------------------------------------------------

    // Combine filters using logical or
    template<template<class> class Filter>
    struct filter_invert {
        template<class Ty>
        struct type {
            constexpr static bool value = !static_cast<bool>(Filter<Ty>::value);
        };
    };

    // ------------------------------------------------

    template<class ...Tys>
    struct pack;

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
    struct pack_element<I, pack<Ty, Tys...>> : pack_element<I - 1, pack<Tys...>> {};

    // Get the Ith element in Pack
    template<std::size_t I, class Pack>
    using pack_element_t = typename pack_element<I, Pack>::type;

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
    struct pack_contains<Type, pack<Ty, Tys...>> : pack_contains<Type, pack<Tys...>> {};

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

    // Pack contains all Types
    template<class Types, class Pack>
    constexpr bool pack_contains_all_v = pack_contains_all<Types, Pack>::value;

    // ------------------------------------------------

    template<class Types, class Pack>
    struct pack_contains_any;

    template<class ...Types, class ...Tys>
    struct pack_contains_any<pack<Types...>, pack<Tys...>> {
        constexpr static bool value = (pack_contains<Types, pack<Tys...>>::value || ...);
    };

    // Pack contains any in Find
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

    // Count number of matched for Filter in Pack
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

    template<class Type, std::size_t I>
    struct pack_nth_index_of<Type, I, pack<>> {
        constexpr static bool _found = false;
        constexpr static std::size_t value = npos;
    };

    // Index of the Ith occurance of Type in Pack
    template<class Type, std::size_t I, class Pack>
    constexpr std::size_t pack_nth_index_of_v = pack_nth_index_of<Type, I, Pack>::value;
    
    // ------------------------------------------------

    template<class Types, std::size_t I, class Pack>
    struct pack_nth_index_of_any;

    template<class ...Types, std::size_t I, class Ty, class ...Tys>
        requires (!(std::same_as<Types, Ty> || ...))
    struct pack_nth_index_of_any<pack<Types...>, I, pack<Ty, Tys...>> {
        constexpr static std::size_t _last = pack_nth_index_of_any<pack<Types...>, I, pack<Tys...>>::value;
        constexpr static std::size_t value = _last == npos ? npos : _last + 1;
    };

    template<class ...Types, std::size_t I, class Ty, class ...Tys>
        requires (std::same_as<Types, Ty> || ...)
    struct pack_nth_index_of_any<pack<Types...>, I, pack<Ty, Tys...>> {
        constexpr static std::size_t _last = pack_nth_index_of_any<pack<Types...>, I - 1, pack<Tys...>>::value;
        constexpr static std::size_t value = _last == npos ? npos : _last + 1;
    };

    template<class ...Types, class Ty, class ...Tys>
        requires (std::same_as<Types, Ty> || ...)
    struct pack_nth_index_of_any<pack<Types...>, 0, pack<Ty, Tys...>> {
        constexpr static std::size_t value = 0;
    };

    template<class Types, std::size_t I>
    struct pack_nth_index_of_any<Types, I, pack<>> {
        constexpr static bool _found = false;
        constexpr static std::size_t value = npos;
    };

    // Index of the Ith occurance of any of Types in Pack
    template<class Types, std::size_t I, class Pack>
    constexpr std::size_t pack_nth_index_of_any_v = pack_nth_index_of_any<Types, I, Pack>::value;
    
    // ------------------------------------------------

    template<template<class> class Filter, std::size_t I, class Pack>
    struct pack_nth_index_filter;

    template<template<class> class Filter, std::size_t I, class Ty, class ...Tys>
        requires (!Filter<Ty>::value)
    struct pack_nth_index_filter<Filter, I, pack<Ty, Tys...>> {
        constexpr static std::size_t _last = pack_nth_index_filter<Filter, I, pack<Tys...>>::value;
        constexpr static std::size_t value = _last == npos ? npos : _last + 1;
    };

    template<template<class> class Filter, std::size_t I, class Ty, class ...Tys>
        requires (Filter<Ty>::value)
    struct pack_nth_index_filter<Filter, I, pack<Ty, Tys...>> {
        constexpr static std::size_t _last = pack_nth_index_filter<Filter, I - 1, pack<Tys...>>::value;
        constexpr static std::size_t value = _last == npos ? npos : _last + 1;
    };

    template<template<class> class Filter, class Ty, class ...Tys>
        requires (Filter<Ty>::value)
    struct pack_nth_index_filter<Filter, 0, pack<Ty, Tys...>> {
        constexpr static std::size_t value = 0;
    };

    template<template<class> class Filter, std::size_t I>
    struct pack_nth_index_filter<Filter, I, pack<>> {
        constexpr static bool _found = false;
        constexpr static std::size_t value = npos;
    };

    // Index of the Ith match of Filter in Pack
    template<template<class> class Filter, std::size_t I, class Pack>
    constexpr std::size_t pack_nth_index_filter_v = pack_nth_index_filter<Filter, I, Pack>::value;

    // ------------------------------------------------

    template<class Type, class Pack>
    struct pack_index_of {
        constexpr static std::size_t value = pack_nth_index_of<Type, 0, Pack>::value;
    };

    // Index of the first occurance of Type in Pack
    template<class Type, class Pack>
    constexpr std::size_t pack_index_of_v = pack_index_of<Type, Pack>::value;
    
    // ------------------------------------------------

    template<class Types, class Pack>
    struct pack_index_of_any {
        constexpr static std::size_t value = pack_nth_index_of_any<Types, 0, Pack>::value;
    };

    // Index of the first occurance of any of Types in Pack
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

    // Index of the first occurance of Type in Pack
    template<class Type, class Pack>
    constexpr std::size_t pack_first_index_of_v = pack_first_index_of<Type, Pack>::value;
    
    // ------------------------------------------------

    template<class Types, class Pack>
    struct pack_first_index_of_any {
        constexpr static std::size_t value = pack_nth_index_of_any<Types, 0, Pack>::value;
    };

    // Index of the first occurance of any of Types in Pack
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

    // Index of the last occurance of Type in Pack
    template<class Type, class Pack>
    constexpr std::size_t pack_last_index_of_v = pack_last_index_of<Type, Pack>::value;
    
    // ------------------------------------------------

    template<class Types, class Pack>
    struct pack_last_index_of_any {
        constexpr static std::size_t value = pack_nth_index_of_any<Types, pack_count_all<Types, Pack>::value - 1, Pack>::value;
    };

    // Index of the last occurance of any of Types in Pack
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

    // All indices of Type in Pack
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

    // All indices of matches of Filter in Pack
    template<template<class> class Filter, class Pack>
    using pack_indices_filter_t = pack_indices_filter<Filter, Pack>::type;

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

    template<class Indices, class Pack>
    struct pack_at_indices;

    template<std::size_t ...Is, class Pack>
    struct pack_at_indices<std::index_sequence<Is...>, Pack> {
        using type = pack<typename pack_element<Is, Pack>::type...>;
    };

    // Only keep all Indices in Pack
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

    // Take the first I elements of Pack
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

    // Take the first I elements of Pack
    template<template<class> class Filter, class Pack>
    using pack_take_until_t = typename pack_take_until<Filter, Pack>::type;

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

    template<class Type, class Pack>
    using pack_remove_t = typename pack_remove<Type, Pack>::type;
    
    // ------------------------------------------------
    
    template<class Types, class Pack>
    struct pack_remove_all {
        using type = typename pack_erase_all<typename pack_indices_of_all<Types, Pack>::type, Pack>::type;
    };

    template<class Types, class Pack>
    using pack_remove_all_t = typename pack_remove_all<Types, Pack>::type;

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

    // Reverse elements in Pack
    template<class Pack>
    using pack_reverse_t = typename pack_reverse<Pack>::type;

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

    // Swap all elements at Indices of Pack with Type
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

    template<class Type, template<class> class Filter, class Pack>
    struct pack_replace_filter;

    template<class Type, template<class> class Filter, class ...Tys>
    struct pack_replace_filter<Type, Filter, pack<Tys...>> {
        using type = pack<std::conditional_t<Filter<Tys>::value, Type, Tys>...>;
    };

    // Replace all matches of Filter in Pack with Type
    template<class Type, template<class> class Filter, class Pack>
    using pack_replace_filter_t = typename pack_replace_filter<Type, Filter, Pack>::type;

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
            requires (!Sorter<A, B>::value)
        struct merge_sort_merge<Sorter, pack<A, As...>, pack<B, Bs...>> {
            using _recurse = typename merge_sort_merge<Sorter, pack<As...>, pack<B, Bs...>>::type;
            using type = typename pack_append<A, _recurse>::type;
        };

        template<template<class, class> class Sorter, class A, class ...As, class B, class ...Bs>
            requires (Sorter<A, B>::value)
        struct merge_sort_merge<Sorter, pack<A, As...>, pack<B, Bs...>> {
            using _recurse = typename merge_sort_merge<Sorter, pack<A, As...>, pack<Bs...>>::type;
            using type = typename pack_append<B, _recurse>::type;
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

}

// ------------------------------------------------

using namespace kaixo;

static_assert(pack_size_v<pack<double, int, float>> == 3);
static_assert(pack_size_v<pack<double, int, int, int>> == 4);
static_assert(pack_size_v<pack<>> == 0);

static_assert(std::same_as<pack_element_t<0, pack<double, int, int, char>>, double>);
static_assert(std::same_as<pack_element_t<1, pack<double, int, int, int>>, int>);
static_assert(std::same_as<pack_element_t<3, pack<double, int, int, char>>, char>);
static_assert(std::same_as<pack_element_t<0, pack<double>>, double>);

static_assert(pack_contains_v<int, pack<double, int, float>>);
static_assert(!pack_contains_v<char, pack<double, int, float>>);
static_assert(!pack_contains_v<char, pack<>>);

static_assert(pack_contains_all_v<pack<int, double>, pack<double, int, float>>);
static_assert(!pack_contains_all_v<pack<int, char>, pack<double, int, float>>);
static_assert(!pack_contains_all_v<pack<int, char>, pack<>>);
static_assert(pack_contains_all_v<pack<>, pack<>>);

static_assert(pack_contains_any_v<pack<int, char>, pack<double, int, float>>);
static_assert(!pack_contains_any_v<pack<long, char>, pack<double, int, float>>);
static_assert(!pack_contains_any_v<pack<long, char>, pack<>>);
static_assert(!pack_contains_any_v<pack<>, pack<>>);

static_assert(pack_find_v<std::is_integral, pack<long, char>>);
static_assert(pack_find_v<std::is_integral, pack<float, char>>);
static_assert(!pack_find_v<std::is_integral, pack<float, double>>);
static_assert(!pack_find_v<std::is_integral, pack<>>);

static_assert(pack_count_v<int, pack<double, int, float>> == 1);
static_assert(pack_count_v<char, pack<double, int, float>> == 0);
static_assert(pack_count_v<float, pack<float, int, float>> == 2);
static_assert(pack_count_v<float, pack<>> == 0);

static_assert(pack_count_all_v<pack<float>, pack<float, int, float>> == 2);
static_assert(pack_count_all_v<pack<float, int>, pack<float, int, float>> == 3);
static_assert(pack_count_all_v<pack<float, int>, pack<char, int, float>> == 2);
static_assert(pack_count_all_v<pack<float, int>, pack<>> == 0);

static_assert(pack_count_unique_v<pack<int, double>> == 2);
static_assert(pack_count_unique_v<pack<int, double, int>> == 2);
static_assert(pack_count_unique_v<pack<int, int, int>> == 1);
static_assert(pack_count_unique_v<pack<>> == 0);

static_assert(pack_count_filter_v<std::is_integral, pack<char, int, float>> == 2);
static_assert(pack_count_filter_v<std::is_integral, pack<double, float>> == 0);
static_assert(pack_count_filter_v<std::is_integral, pack<>> == 0);

static_assert(pack_nth_index_of_v<int, 0, pack<double, int, float, int, long>> == 1);
static_assert(pack_nth_index_of_v<int, 1, pack<double, int, float, int, long>> == 3);
static_assert(pack_nth_index_of_v<int, 2, pack<double, int, float, int, long>> == npos);
static_assert(pack_nth_index_of_v<int, 4, pack<int, int, int, int, int>> == 4);
static_assert(pack_nth_index_of_v<char, 0, pack<double, int, float>> == npos);
static_assert(pack_nth_index_of_v<char, 1, pack<double, int, float>> == npos);
static_assert(pack_nth_index_of_v<char, 1, pack<>> == npos);

static_assert(pack_nth_index_of_any_v<pack<int, float>, 0, pack<double, int, float, int, long>> == 1);
static_assert(pack_nth_index_of_any_v<pack<int, double>, 1, pack<double, int, float, int, long>> == 1);
static_assert(pack_nth_index_of_any_v<pack<int, float>, 2, pack<double, int, float, int, long>> == 3);
static_assert(pack_nth_index_of_any_v<pack<int, float>, 3, pack<double, int, float, int, long>> == npos);
static_assert(pack_nth_index_of_any_v<pack<int, float>, 4, pack<int, int, int, int, int>> == 4);
static_assert(pack_nth_index_of_any_v<pack<char, long>, 0, pack<double, int, float>> == npos);
static_assert(pack_nth_index_of_any_v<pack<char, long>, 1, pack<double, int, float>> == npos);
static_assert(pack_nth_index_of_any_v<pack<char, long>, 1, pack<>> == npos);
static_assert(pack_nth_index_of_any_v<pack<>, 0, pack<>> == npos);
static_assert(pack_nth_index_of_any_v<pack<>, 0, pack<int, long, float>> == npos);
static_assert(pack_nth_index_of_any_v<pack<>, 1, pack<int, long, float>> == npos);

static_assert(pack_nth_index_filter_v<std::is_integral, 0, pack<double, int, float, int, long>> == 1);
static_assert(pack_nth_index_filter_v<std::is_integral, 1, pack<double, int, float, int, long>> == 3);
static_assert(pack_nth_index_filter_v<std::is_integral, 2, pack<double, int, float, int, long>> == 4);
static_assert(pack_nth_index_filter_v<std::is_integral, 3, pack<double, int, float, int, long>> == npos);
static_assert(pack_nth_index_filter_v<std::is_integral, 4, pack<int, int, int, int, int>> == 4);
static_assert(pack_nth_index_filter_v<std::is_integral, 0, pack<double, int, float>> == 1);
static_assert(pack_nth_index_filter_v<std::is_integral, 1, pack<double, int, float>> == npos);
static_assert(pack_nth_index_filter_v<std::is_integral, 0, pack<>> == npos);
static_assert(pack_nth_index_filter_v<std::is_integral, 1, pack<>> == npos);

static_assert(pack_index_of_v<int, pack<double, int, float, int>> == 1);
static_assert(pack_index_of_v<char, pack<double, int, float>> == npos);
static_assert(pack_index_of_v<char, pack<>> == npos);

static_assert(pack_index_of_any_v<pack<int, float>, pack<double, int, float, int>> == 1);
static_assert(pack_index_of_any_v<pack<char, long>, pack<double, int, float>> == npos);
static_assert(pack_index_of_any_v<pack<char>, pack<>> == npos);
static_assert(pack_index_of_any_v<pack<>, pack<>> == npos);
static_assert(pack_index_of_any_v<pack<>, pack<int, long, char>> == npos);

static_assert(pack_index_filter_v<std::is_integral, pack<double, int, float, int>> == 1);
static_assert(pack_index_filter_v<std::is_integral, pack<double, float>> == npos);
static_assert(pack_index_filter_v<std::is_integral, pack<>> == npos);

static_assert(pack_first_index_of_v<int, pack<double, int, float, int>> == 1);
static_assert(pack_first_index_of_v<char, pack<double, int, float>> == npos);
static_assert(pack_first_index_of_v<char, pack<>> == npos);

static_assert(pack_first_index_of_any_v<pack<int, float>, pack<double, int, float, int>> == 1);
static_assert(pack_first_index_of_any_v<pack<char, long>, pack<double, int, float>> == npos);
static_assert(pack_first_index_of_any_v<pack<char>, pack<>> == npos);
static_assert(pack_first_index_of_any_v<pack<>, pack<>> == npos);
static_assert(pack_first_index_of_any_v<pack<>, pack<int, long, char>> == npos);

static_assert(pack_first_index_filter_v<std::is_integral, pack<double, int, float, int>> == 1);
static_assert(pack_first_index_filter_v<std::is_integral, pack<double, float>> == npos);
static_assert(pack_first_index_filter_v<std::is_integral, pack<>> == npos);

static_assert(pack_last_index_of_v<int, pack<double, int, float, int>> == 3);
static_assert(pack_last_index_of_v<int, pack<double, int, float, char>> == 1);
static_assert(pack_last_index_of_v<char, pack<double, int, float>> == npos);
static_assert(pack_last_index_of_v<char, pack<>> == npos);

static_assert(pack_last_index_of_any_v<pack<int, float>, pack<double, int, float, int>> == 3);
static_assert(pack_last_index_of_any_v<pack<int, float>, pack<double, int, int, char>> == 2);
static_assert(pack_last_index_of_any_v<pack<int, float>, pack<double, int, float, char>> == 2);
static_assert(pack_last_index_of_any_v<pack<char, long>, pack<double, int, float>> == npos);
static_assert(pack_last_index_of_any_v<pack<char>, pack<>> == npos);
static_assert(pack_last_index_of_any_v<pack<>, pack<>> == npos);
static_assert(pack_last_index_of_any_v<pack<>, pack<int, long, char>> == npos);

static_assert(pack_last_index_filter_v<std::is_integral, pack<double, int, float, int>> == 3);
static_assert(pack_last_index_filter_v<std::is_integral, pack<double, int, float, float>> == 1);
static_assert(pack_last_index_filter_v<std::is_integral, pack<double, float>> == npos);
static_assert(pack_last_index_filter_v<std::is_integral, pack<>> == npos);

static_assert(std::same_as<pack_indices_of_t<int, pack<double, float, int, long, int>>, std::index_sequence<2, 4>>);
static_assert(std::same_as<pack_indices_of_t<int, pack<int, long, int>>, std::index_sequence<0, 2>>);
static_assert(std::same_as<pack_indices_of_t<char, pack<int, long, int>>, std::index_sequence<>>);
static_assert(std::same_as<pack_indices_of_t<char, pack<>>, std::index_sequence<>>);

static_assert(std::same_as<pack_indices_of_all_t<pack<int, long>, pack<double, float, int, long, int>>, std::index_sequence<2, 3, 4>>);
static_assert(std::same_as<pack_indices_of_all_t<pack<int, long>, pack<int, long, int>>, std::index_sequence<0, 1, 2>>);
static_assert(std::same_as<pack_indices_of_all_t<pack<char, float>, pack<int, long, int>>, std::index_sequence<>>);
static_assert(std::same_as<pack_indices_of_all_t<pack<char, float>, pack<>>, std::index_sequence<>>);
static_assert(std::same_as<pack_indices_of_all_t<pack<>, pack<>>, std::index_sequence<>>);
static_assert(std::same_as<pack_indices_of_all_t<pack<>, pack<int>>, std::index_sequence<>>);
static_assert(std::same_as<pack_indices_of_all_t<pack<>, pack<int, double, float>>, std::index_sequence<>>);

static_assert(std::same_as<pack_indices_filter_t<std::is_integral, pack<double, float, int, long, int>>, std::index_sequence<2, 3, 4>>);
static_assert(std::same_as<pack_indices_filter_t<std::is_integral, pack<int>>, std::index_sequence<0>>);
static_assert(std::same_as<pack_indices_filter_t<std::is_integral, pack<int, double, float>>, std::index_sequence<0>>);
static_assert(std::same_as<pack_indices_filter_t<std::is_integral, pack<double, float, int>>, std::index_sequence<2>>);
static_assert(std::same_as<pack_indices_filter_t<std::is_integral, pack<int, long, int>>, std::index_sequence<0, 1, 2>>);
static_assert(std::same_as<pack_indices_filter_t<std::is_integral, pack<float, double>>, std::index_sequence<>>);
static_assert(std::same_as<pack_indices_filter_t<std::is_integral, pack<>>, std::index_sequence<>>);

static_assert(std::same_as<pack_first_indices_t<pack<int, double, int, float>>, std::index_sequence<0, 1, 3>>);
static_assert(std::same_as<pack_first_indices_t<pack<int, double, char, float>>, std::index_sequence<0, 1, 2, 3>>);
static_assert(std::same_as<pack_first_indices_t<pack<int, double, int, double>>, std::index_sequence<0, 1>>);
static_assert(std::same_as<pack_first_indices_t<pack<int, int, double, double>>, std::index_sequence<0, 2>>);
static_assert(std::same_as<pack_first_indices_t<pack<int, int, int, int>>, std::index_sequence<0>>);
static_assert(std::same_as<pack_first_indices_t<pack<>>, std::index_sequence<>>);

static_assert(std::same_as<pack_at_indices_t<std::index_sequence<0>, pack<int>>, pack<int>>);
static_assert(std::same_as<pack_at_indices_t<std::index_sequence<0>, pack<int, float>>, pack<int>>);
static_assert(std::same_as<pack_at_indices_t<std::index_sequence<1>, pack<int, float>>, pack<float>>);
static_assert(std::same_as<pack_at_indices_t<std::index_sequence<0, 1>, pack<int, float>>, pack<int, float>>);
static_assert(std::same_as<pack_at_indices_t<std::index_sequence<0, 3>, pack<int, float, char, long>>, pack<int, long>>);
static_assert(std::same_as<pack_at_indices_t<std::index_sequence<1, 3>, pack<int, float, char, long>>, pack<float, long>>);
static_assert(std::same_as<pack_at_indices_t<std::index_sequence<1, 3, 1>, pack<int, float, char, long>>, pack<float, long, float>>);
static_assert(std::same_as<pack_at_indices_t<std::index_sequence<>, pack<int, float, char, long>>, pack<>>);

static_assert(std::same_as<pack_unique_t<pack<int, float, char, long>>, pack<int, float, char, long>>);
static_assert(std::same_as<pack_unique_t<pack<int, float, int, long>>, pack<int, float, long>>);
static_assert(std::same_as<pack_unique_t<pack<int, int, int, long>>, pack<int, long>>);
static_assert(std::same_as<pack_unique_t<pack<long, int, int, long>>, pack<long, int>>);
static_assert(std::same_as<pack_unique_t<pack<int>>, pack<int>>);
static_assert(std::same_as<pack_unique_t<pack<>>, pack<>>);

static_assert(std::same_as<pack_drop_t<0, pack<int, double>>, pack<int, double>>);
static_assert(std::same_as<pack_drop_t<1, pack<int, double>>, pack<double>>);
static_assert(std::same_as<pack_drop_t<2, pack<int, double>>, pack<>>);
static_assert(std::same_as<pack_drop_t<1, pack<int>>, pack<>>);
static_assert(std::same_as<pack_drop_t<0, pack<>>, pack<>>);

static_assert(std::same_as<pack_drop_while_t<std::is_integral, pack<int, double>>, pack<double>>);
static_assert(std::same_as<pack_drop_while_t<std::is_integral, pack<double, int>>, pack<double, int>>);
static_assert(std::same_as<pack_drop_while_t<std::is_integral, pack<int>>, pack<>>);
static_assert(std::same_as<pack_drop_while_t<std::is_integral, pack<double>>, pack<double>>);
static_assert(std::same_as<pack_drop_while_t<std::is_integral, pack<>>, pack<>>);

static_assert(std::same_as<pack_drop_until_t<std::is_integral, pack<int, double>>, pack<int, double>>);
static_assert(std::same_as<pack_drop_until_t<std::is_integral, pack<double, int>>, pack<int>>);
static_assert(std::same_as<pack_drop_until_t<std::is_integral, pack<int>>, pack<int>>);
static_assert(std::same_as<pack_drop_until_t<std::is_integral, pack<double>>, pack<>>);
static_assert(std::same_as<pack_drop_until_t<std::is_integral, pack<>>, pack<>>);

static_assert(std::same_as<pack_take_t<0, pack<int, double>>, pack<>>);
static_assert(std::same_as<pack_take_t<1, pack<int, double>>, pack<int>>);
static_assert(std::same_as<pack_take_t<2, pack<int, double>>, pack<int, double>>);
static_assert(std::same_as<pack_take_t<1, pack<int>>, pack<int>>);
static_assert(std::same_as<pack_take_t<0, pack<>>, pack<>>);

static_assert(std::same_as<pack_take_while_t<std::is_integral, pack<int, double>>, pack<int>>);
static_assert(std::same_as<pack_take_while_t<std::is_integral, pack<double, int>>, pack<>>);
static_assert(std::same_as<pack_take_while_t<std::is_integral, pack<int>>, pack<int>>);
static_assert(std::same_as<pack_take_while_t<std::is_integral, pack<double>>, pack<>>);
static_assert(std::same_as<pack_take_while_t<std::is_integral, pack<>>, pack<>>);

static_assert(std::same_as<pack_take_until_t<std::is_integral, pack<int, double>>, pack<>>);
static_assert(std::same_as<pack_take_until_t<std::is_integral, pack<double, int>>, pack<double>>);
static_assert(std::same_as<pack_take_until_t<std::is_integral, pack<int>>, pack<>>);
static_assert(std::same_as<pack_take_until_t<std::is_integral, pack<double>>, pack<double>>);
static_assert(std::same_as<pack_take_until_t<std::is_integral, pack<>>, pack<>>);

static_assert(std::same_as<pack_concat_t<pack<>>, pack<>>);
static_assert(std::same_as<pack_concat_t<pack<>, pack<>>, pack<>>);
static_assert(std::same_as<pack_concat_t<pack<>, pack<>, pack<>>, pack<>>);
static_assert(std::same_as<pack_concat_t<pack<int>, pack<>, pack<>>, pack<int>>);
static_assert(std::same_as<pack_concat_t<pack<>, pack<int>, pack<>>, pack<int>>);
static_assert(std::same_as<pack_concat_t<pack<>, pack<>, pack<int>>, pack<int>>);
static_assert(std::same_as<pack_concat_t<pack<int>, pack<char>, pack<>>, pack<int, char>>);
static_assert(std::same_as<pack_concat_t<pack<int>, pack<>, pack<char>>, pack<int, char>>);
static_assert(std::same_as<pack_concat_t<pack<>, pack<int>, pack<char>>, pack<int, char>>);
static_assert(std::same_as<pack_concat_t<pack<int>, pack<long>, pack<char>>, pack<int, long, char>>);
static_assert(std::same_as<pack_concat_t<pack<int, long>, pack<char>>, pack<int, long, char>>);
static_assert(std::same_as<pack_concat_t<pack<int>, pack<long, char>>, pack<int, long, char>>);

static_assert(std::same_as<pack_erase_t<0, pack<int, long, int>>, pack<long, int>>);
static_assert(std::same_as<pack_erase_t<1, pack<int, long, int>>, pack<int, int>>);
static_assert(std::same_as<pack_erase_t<2, pack<int, long, int>>, pack<int, long>>);

static_assert(std::same_as<pack_erase_all_t<std::index_sequence<0>, pack<int, long, int>>, pack<long, int>>);
static_assert(std::same_as<pack_erase_all_t<std::index_sequence<1>, pack<int, long, int>>, pack<int, int>>);
static_assert(std::same_as<pack_erase_all_t<std::index_sequence<2>, pack<int, long, int>>, pack<int, long>>);
static_assert(std::same_as<pack_erase_all_t<std::index_sequence<0, 2>, pack<int, long, int>>, pack<long>>);
static_assert(std::same_as<pack_erase_all_t<std::index_sequence<0, 1>, pack<int, long, int>>, pack<int>>);
static_assert(std::same_as<pack_erase_all_t<std::index_sequence<1, 2>, pack<int, long, int>>, pack<int>>);
static_assert(std::same_as<pack_erase_all_t<std::index_sequence<1, 1>, pack<int, long, int>>, pack<int, int>>);
static_assert(std::same_as<pack_erase_all_t<std::index_sequence<>, pack<int, long, int>>, pack<int, long, int>>);
static_assert(std::same_as<pack_erase_all_t<std::index_sequence<>, pack<>>, pack<>>);

static_assert(std::same_as<pack_insert_t<int, 0, pack<float, long, double>>, pack<int, float, long, double>>);
static_assert(std::same_as<pack_insert_t<int, 1, pack<float, long, double>>, pack<float, int, long, double>>);
static_assert(std::same_as<pack_insert_t<int, 2, pack<float, long, double>>, pack<float, long, int, double>>);
static_assert(std::same_as<pack_insert_t<int, 3, pack<float, long, double>>, pack<float, long, double, int>>);
static_assert(std::same_as<pack_insert_t<int, 0, pack<>>, pack<int>>);

static_assert(std::same_as<pack_insert_all_t<pack<int, char>, 0, pack<float, long, double>>, pack<int, char, float, long, double>>);
static_assert(std::same_as<pack_insert_all_t<pack<int, char>, 1, pack<float, long, double>>, pack<float, int, char, long, double>>);
static_assert(std::same_as<pack_insert_all_t<pack<int, char>, 2, pack<float, long, double>>, pack<float, long, int, char, double>>);
static_assert(std::same_as<pack_insert_all_t<pack<int, char>, 3, pack<float, long, double>>, pack<float, long, double, int, char>>);
static_assert(std::same_as<pack_insert_all_t<pack<int, char>, 0, pack<>>, pack<int, char>>);
static_assert(std::same_as<pack_insert_all_t<pack<>, 0, pack<>>, pack<>>);
static_assert(std::same_as<pack_insert_all_t<pack<>, 0, pack<float, long, double>>, pack<float, long, double>>);
static_assert(std::same_as<pack_insert_all_t<pack<>, 1, pack<float, long, double>>, pack<float, long, double>>);
static_assert(std::same_as<pack_insert_all_t<pack<>, 2, pack<float, long, double>>, pack<float, long, double>>);
static_assert(std::same_as<pack_insert_all_t<pack<>, 3, pack<float, long, double>>, pack<float, long, double>>);

static_assert(std::same_as<pack_filter_t<std::is_integral, pack<float, long, double>>, pack<long>>);
static_assert(std::same_as<pack_filter_t<std::is_integral, pack<float, double>>, pack<>>);
static_assert(std::same_as<pack_filter_t<std::is_integral, pack<char, long>>, pack<char, long>>);
static_assert(std::same_as<pack_filter_t<std::is_integral, pack<>>, pack<>>);

static_assert(std::same_as<pack_erase_filter_t<std::is_integral, pack<float, long, double>>, pack<float, double>>);
static_assert(std::same_as<pack_erase_filter_t<std::is_integral, pack<float, double>>, pack<float, double>>);
static_assert(std::same_as<pack_erase_filter_t<std::is_integral, pack<char, long>>, pack<>>);
static_assert(std::same_as<pack_erase_filter_t<std::is_integral, pack<>>, pack<>>);

static_assert(std::same_as<pack_remove_t<int, pack<float, int, double, int>>, pack<float, double>>);
static_assert(std::same_as<pack_remove_t<int, pack<float, double>>, pack<float, double>>);
static_assert(std::same_as<pack_remove_t<int, pack<>>, pack<>>);

static_assert(std::same_as<pack_remove_all_t<pack<int>, pack<float, int, double, int>>, pack<float, double>>);
static_assert(std::same_as<pack_remove_all_t<pack<int>, pack<float, double>>, pack<float, double>>);
static_assert(std::same_as<pack_remove_all_t<pack<int>, pack<>>, pack<>>);
static_assert(std::same_as<pack_remove_all_t<pack<>, pack<>>, pack<>>);
static_assert(std::same_as<pack_remove_all_t<pack<>, pack<float, int>>, pack<float, int>>);

static_assert(std::same_as<pack_reverse_t<pack<float, int, char>>, pack<char, int, float>>);
static_assert(std::same_as<pack_reverse_t<pack<float, int>>, pack<int, float>>);
static_assert(std::same_as<pack_reverse_t<pack<int>>, pack<int>>);
static_assert(std::same_as<pack_reverse_t<pack<>>, pack<>>);

template<class Ty> using transform_to_int = int;
template<class Ty> using transform_to_self = Ty;

static_assert(std::same_as<pack_transform_t<transform_to_int, pack<>>, pack<>>);
static_assert(std::same_as<pack_transform_t<transform_to_int, pack<int>>, pack<int>>);
static_assert(std::same_as<pack_transform_t<transform_to_int, pack<float>>, pack<int>>);
static_assert(std::same_as<pack_transform_t<transform_to_int, pack<float, int>>, pack<int, int>>);
static_assert(std::same_as<pack_transform_t<transform_to_self, pack<float, int>>, pack<float, int>>);

static_assert(std::same_as<pack_append_t<char, pack<float, int>>, pack<float, int, char>>);
static_assert(std::same_as<pack_append_t<char, pack<>>, pack<char>>);

static_assert(std::same_as<pack_append_all_t<pack<char>, pack<float, int>>, pack<float, int, char>>);
static_assert(std::same_as<pack_append_all_t<pack<char>, pack<>>, pack<char>>);
static_assert(std::same_as<pack_append_all_t<pack<char, int>, pack<float, long>>, pack<float, long, char, int>>);
static_assert(std::same_as<pack_append_all_t<pack<char, int>, pack<>>, pack<char, int>>);

static_assert(std::same_as<pack_prepend_t<char, pack<float, int>>, pack<char, float, int>>);
static_assert(std::same_as<pack_prepend_t<char, pack<>>, pack<char>>);

static_assert(std::same_as<pack_prepend_all_t<pack<char>, pack<float, int>>, pack<char, float, int>>);
static_assert(std::same_as<pack_prepend_all_t<pack<char>, pack<>>, pack<char>>);
static_assert(std::same_as<pack_prepend_all_t<pack<char, int>, pack<float, long>>, pack<char, int, float, long>>);
static_assert(std::same_as<pack_prepend_all_t<pack<char, int>, pack<>>, pack<char, int>>);

static_assert(std::same_as<pack_swap_t<int, 0, pack<float, char, double>>, pack<int, char, double>>);
static_assert(std::same_as<pack_swap_t<int, 1, pack<float, char, double>>, pack<float, int, double>>);
static_assert(std::same_as<pack_swap_t<int, 2, pack<float, char, double>>, pack<float, char, int>>);
static_assert(std::same_as<pack_swap_t<int, 0, pack<char>>, pack<int>>);
static_assert(std::same_as<pack_swap_t<int, 0, pack<int>>, pack<int>>);

static_assert(std::same_as<pack_swap_all_t<int, std::index_sequence<0>, pack<float, char, double>>, pack<int, char, double>>);
static_assert(std::same_as<pack_swap_all_t<int, std::index_sequence<1>, pack<float, char, double>>, pack<float, int, double>>);
static_assert(std::same_as<pack_swap_all_t<int, std::index_sequence<2>, pack<float, char, double>>, pack<float, char, int>>);
static_assert(std::same_as<pack_swap_all_t<int, std::index_sequence<0>, pack<char>>, pack<int>>);
static_assert(std::same_as<pack_swap_all_t<int, std::index_sequence<0>, pack<int>>, pack<int>>);
static_assert(std::same_as<pack_swap_all_t<int, std::index_sequence<0, 2>, pack<float, char, double>>, pack<int, char, int>>);
static_assert(std::same_as<pack_swap_all_t<int, std::index_sequence<0, 1>, pack<float, char, double>>, pack<int, int, double>>);
static_assert(std::same_as<pack_swap_all_t<int, std::index_sequence<1, 2>, pack<float, char, double>>, pack<float, int, int>>);
static_assert(std::same_as<pack_swap_all_t<int, std::index_sequence<1, 1>, pack<float, char, double>>, pack<float, int, double>>);
static_assert(std::same_as<pack_swap_all_t<int, std::index_sequence<>, pack<float, char, double>>, pack<float, char, double>>);
static_assert(std::same_as<pack_swap_all_t<int, std::index_sequence<>, pack<>>, pack<>>);

static_assert(std::same_as<pack_replace_t<int, char, pack<float, char, double>>, pack<float, char, double>>);
static_assert(std::same_as<pack_replace_t<int, char, pack<float, int, double>>, pack<float, char, double>>);
static_assert(std::same_as<pack_replace_t<int, char, pack<int, int, double>>, pack<char, char, double>>);
static_assert(std::same_as<pack_replace_t<int, char, pack<int, int, int>>, pack<char, char, char>>);
static_assert(std::same_as<pack_replace_t<int, char, pack<>>, pack<>>);

static_assert(std::same_as<pack_replace_filter_t<char, std::is_integral, pack<float, float, double>>, pack<float, float, double>>);
static_assert(std::same_as<pack_replace_filter_t<char, std::is_integral, pack<float, char, double>>, pack<float, char, double>>);
static_assert(std::same_as<pack_replace_filter_t<char, std::is_integral, pack<float, int, double>>, pack<float, char, double>>);
static_assert(std::same_as<pack_replace_filter_t<char, std::is_integral, pack<int, int, double>>, pack<char, char, double>>);
static_assert(std::same_as<pack_replace_filter_t<char, std::is_integral, pack<int, int, int>>, pack<char, char, char>>);
static_assert(std::same_as<pack_replace_filter_t<char, std::is_integral, pack<>>, pack<>>);

static_assert(std::same_as<pack_sub_t<0, 1, pack<int, double, char, float, long>>, pack<int>>);
static_assert(std::same_as<pack_sub_t<0, 3, pack<int, double, char, float, long>>, pack<int, double, char>>);
static_assert(std::same_as<pack_sub_t<2, 3, pack<int, double, char, float, long>>, pack<char>>);
static_assert(std::same_as<pack_sub_t<4, 5, pack<int, double, char, float, long>>, pack<long>>);
static_assert(std::same_as<pack_sub_t<5, 5, pack<int, double, char, float, long>>, pack<>>);
static_assert(std::same_as<pack_sub_t<0, 5, pack<int, double, char, float, long>>, pack<int, double, char, float, long>>);
static_assert(std::same_as<pack_sub_t<0, 0, pack<>>, pack<>>);

template<class A, class B>
struct sort_on_size {
    constexpr static bool value = sizeof(A) < sizeof(B);
};

static_assert(std::same_as<pack_sort_t<sort_on_size, pack<double, char, int>>, pack<char, int, double>>);
static_assert(std::same_as<pack_sort_t<sort_on_size, pack<double, char, int>>, pack<char, int, double>>);
static_assert(std::same_as<pack_sort_t<sort_on_size, pack<int>>, pack<int>>);
static_assert(std::same_as<pack_sort_t<sort_on_size, pack<>>, pack<>>);

// ------------------------------------------------

int main() {
    using namespace kaixo;

    pack_last_index_of_any_v<pack<int, float>, pack<char, int, double, float, char, int, float, double>>;
    pack_first_index_of_any_v<pack<int, float>, pack<char, int, double, float, char, int, float>>;

    pack_indices_of_all_t<pack<int, float>, pack<int, double, float, char, int, float>>;

    pack_remove_t<int, pack<float, int, double>>;

    pack_append_t<int, pack<float>>;
    pack_prepend_t<int, pack<float>>;
    
    pack_append_all_t<pack<int>, pack<float>>;
    pack_prepend_all_t<pack<int>, pack<float>>;

    pack_first_index_filter_v<std::is_integral, pack<float, double, int>>;

    pack_nth_index_filter_v<std::is_integral, 1, pack<float, int, double, int>>;
    pack_nth_index_of_any_v<pack<int, float>, 1, pack<float, int, double, int>>;
    pack_nth_index_of_v<int, 0, pack<float, int, double, int>>;

    pack_take_until<std::is_integral, pack<float, double, int, long, float, int>>::type;
    pack_take_while<std::is_integral, pack<int, long, float, int>>::type;

    pack_drop_until<std::is_integral, pack<float, double, int, long, float, char>>::type;
    pack_drop_while<std::is_integral, pack<int, long, float, char>>::type;

    pack_filter<std::is_integral, pack<int, float, char, double, long>>::type;
    pack_erase_filter<std::is_integral, pack<int, float, char, double, long>>::type;

    pack_indices_filter<std::is_integral, pack<int, float, char, double, long>>::type;

    pack_count_filter<filter_and<std::is_integral, std::is_unsigned>::type, pack<unsigned int, double, float, char>>::value;

    pack_insert_all<pack<float, int>, 3, pack<int, double, char>>::type;

    pack_erase_all<std::index_sequence<2>, pack<int, float, double>>::type;

    pack_erase_all<std::index_sequence<3, 2, 1>, pack<int, float, double, int, char>>::type;

    pack_erase<4, pack<int, double, float, int, float>>::type;

    pack_take<3, pack<int, double, float, int>>::type;

    pack_reverse_t<pack<int, double>>;

    pack_unique_t<pack<int, double, int, float, double, char>>;

    pack_first_indices<pack<int, char, int, float, int, double>>::type;

    using myPack = pack<int, char, float, double, char, double, int>;

    pack_count_v<char, myPack>;

    pack_indices_of_t<char, myPack>;


    pack_index_of_v<char, pack<float, char, int, double, int, char, int, double>>;
    pack_last_index_of_v<char, pack<float, char, int, double, int, char, int, double>>;

    pack_nth_index_of_v<char, 2, pack<int, char, double, char, float, int, char>>;

    pack_contains_all<pack<>, pack<int, double, float>>::value;

    pack_count_all<pack<int, double>, pack<int, double, int, float>>::value;

    pack_count_unique<pack<int, double, int, float, int, float>>::value;
    pack_count_unique<pack<int, int, int, int>>::value;

}