#pragma once
#include <concepts>
#include <cstddef>
#include <array>
#include <utility>
#include <type_traits>

namespace kaixo {

    // Not found
    constexpr std::size_t npos = static_cast<std::size_t>(-1);

    template<class ...Args> struct pack;

    template<auto V> // Value wrapper
    struct value { constexpr static decltype(auto) get() { return V; }; };

    namespace detail {
        // Single, non-templated type -> Ty<T>
        template<class T, template<class...> class Ty>
        struct move_types_impl { using type = Ty<T>; };
        // Convert T<Args...> to Ty<Args...>
        template<template<class...> class T,
            class ...Args, template<class...> class Ty>
            struct move_types_impl<T<Args...>, Ty> { using type = Ty<Args...>; };
    }

    // Move template types from one class to another, if first class 
    // isn't templated, it will itself be used as template argument
    template<class T, template<class...> class Ty>
    using move_types = typename detail::move_types_impl<T, Ty>::type;

    namespace detail {
        // Change type to Ty, used in fold expressions
        template<class, class Ty>
        using change = Ty;

        // Indexed type
        template<std::size_t I, class Ty>
        struct indexed {
            constexpr static std::size_t index = I;
            using type = Ty;
        };

        // Indexer implementation inherits all indexed args
        template<class, class...> struct indexer_impl;
        template<std::size_t ...Is, class ...Args>
        struct indexer_impl<std::index_sequence<Is...>, Args...>
            : indexed<Is, Args>... {};

        // Create indexer for pack
        template<class ...Args>
        using indexer = indexer_impl<std::index_sequence_for<Args...>, Args...>;

        // Indexer implementation inherits all reversely indexed args
        template<class, class...> struct reverse_indexer_impl;
        template<std::size_t ...Is, class ...Args>
        struct reverse_indexer_impl<std::index_sequence<Is...>, Args...>
            : indexed<sizeof...(Args) - Is - 1, Args>... {};

        // Create reverse indexer for pack
        template<class ...Args>
        using reverse_indexer = reverse_indexer_impl<
            std::index_sequence_for<Args...>, Args...>;

        // Get type from index using the indexer and overload resolution
        template<std::size_t I, class Ty>
        consteval indexed<I, Ty> element_impl(indexed<I, Ty>) {};
        template<std::size_t I, class ...Args>
        using element = typename decltype(
            element_impl<I>(indexer<Args...>{}))::type;

        // Find index of first occurence of Ty in Args
        template<class Ty, class ...Args>
        consteval std::size_t index_impl() {
            std::size_t _index = 0; // use short-circuit to increment until
            ((std::same_as<Ty, Args> ? true : (++_index, false)) || ...); // first occurence
            // - 1 because we included the type itself
            return _index == sizeof...(Args) ? npos : _index;
        }
        template<class Ty, class ...Args>
        constexpr std::size_t index = index_impl<Ty, Args...>();

        // Find index of last occurence of Ty in Args
        template<class Ty, class ...Args>
        consteval std::size_t last_index_impl() {
            std::size_t _fromEnd = 0; // increment, but reset on match
            ((std::same_as<Ty, Args> ? _fromEnd = 0 : ++_fromEnd), ...);
            // This counted distance to end, so calculate index from that
            std::size_t _index = sizeof...(Args) - _fromEnd - 1;
            return _fromEnd == sizeof...(Args) ? npos : _index;
        }
        template<class Ty, class ...Args>
        constexpr std::size_t last_index = last_index_impl<Ty, Args...>();

        // Count the number of occurences of Ty in Args
        template<class Ty, class ...Args>
        constexpr std::size_t count = ((std::same_as<Ty, Args>) + ... + 0);

        // Check if Ty occurs in Args
        template<class Ty, class ...Args>
        constexpr bool occurs = count<Ty, Args...> > 0;

        // Find indices of all occurence of Ty in Args
        template<class Ty, class ...Args>
        consteval std::array<std::size_t,
            count<Ty, Args...>> indices_impl() {
            std::array<std::size_t, count<Ty, Args...>> _result{};
            std::size_t _index = 0;
            std::size_t _match = 0;
            ((std::same_as<Ty, Args> ?
                _result[_match++] = _index++ : ++_index), ...);
            return _result;
        }
        template<class Ty, class ...Args>
        constexpr std::array<std::size_t, count<Ty, Args...>>
            indices = indices_impl<Ty, Args...>();

        // Find indices of all occurence of Ty in Args
        template<class ...Args, template<class...> class Ty, class ...Tys>
        consteval std::array<std::size_t,
            (count<Tys, Args...> +...)> indices_all_impl(
                std::type_identity<Ty<Tys...>>) {
            std::array<std::size_t, (count<Tys, Args...> +...)> _result{};
            std::size_t _index = 0;
            std::size_t _match = 0;
            ((count<Args, Tys...> > 0 ? _result[_match++] = _index++ : ++_index), ...);
            return _result;
        }
        template<class Ty, class ...Args>
        constexpr auto indices_all =
            indices_all_impl<Args...>(std::type_identity<Ty>{});

        // Find indices of all types except Ty in Args
        template<class Ty, class ...Args>
        consteval std::array<std::size_t, sizeof...(Args)
            - count<Ty, Args...>> indices_except_impl() {
            std::array<std::size_t, sizeof...(Args) -
                count<Ty, Args...>> _result{};
            std::size_t _index = 0;
            std::size_t _match = 0;
            ((std::same_as<Ty, Args>
                ? ++_index : _result[_match++] = _index++), ...);
            return _result;
        }
        template<class Ty, class ...Args>
        constexpr std::array<std::size_t, sizeof...(Args) - count<Ty, Args...>>
            indices_except = indices_except_impl<Ty, Args...>();
        
        // Find indices of all types except Ty in Args
        template<class ...Args, template<class...> class Ty, class ...Tys>
        consteval std::array<std::size_t, sizeof...(Args)
            - (count<Tys, Args...> +...)> indices_except_all_impl(
                std::type_identity<Ty<Tys...>>) {
            std::array<std::size_t, sizeof...(Args) -
                (count<Tys, Args...> +...)> _result{};
            std::size_t _index = 0;
            std::size_t _match = 0;
            ((count<Args, Tys...> > 0 ? ++_index : _result[_match++] = _index++), ...);
            return _result;
        }
        template<class Ty, class ...Args>
        constexpr auto indices_except_all =
            indices_except_all_impl<Args...>(std::type_identity<Ty>{});

        // Reverse Args
        template<class> struct reverse_impl;
        template<template<class...> class Tuple, class ...Args>
        struct reverse_impl<Tuple<Args...>> {
            template<class> struct helper;
            template<std::size_t... Is>
            struct helper<std::index_sequence<Is...>> {
                using type = Tuple<typename decltype(
                    element_impl<Is>(reverse_indexer<Args...>{}))::type... > ;
            };
            using type = typename helper<std::index_sequence_for<Args...>>::type;
        };
        template<class Ty>
        using reverse = typename reverse_impl<Ty>::type;

        // Count unique types in Args
        template<class ...Args>
        consteval std::size_t count_unique_impl() {
            std::size_t _index = 0;
            std::size_t _match = 0;
            ((_match += index<Args, Args...> == _index++), ...);
            return _match;
        }
        template<class ...Args>
        constexpr std::size_t unique_count = count_unique_impl<Args...>();

        // Find indices of all first occurrences of types in Args
        template<class ...Args>
        consteval std::array<std::size_t,
            unique_count<Args...>> first_indices_impl() {
            std::array<std::size_t, unique_count<Args...>> _result{};
            std::size_t _index = 0;
            std::size_t _match = 0;
            (((index<Args, Args...> == _index) ?
                _result[_match++] = _index++ : ++_index), ...);
            return _result;
        }

        // Keep the first N Args in Tuple
        template<std::size_t, class> struct take_impl;
        template<std::size_t N, template<class...> class Tuple, class ...Args>
        struct take_impl<N, Tuple<Args...>> {
            template<class> struct helper;
            template<std::size_t ...Is>
            struct helper<std::index_sequence<Is...>> {
                using type = Tuple<element<Is, Args...>...>;
            };
            using type = typename helper<std::make_index_sequence<N>>::type;
        };
        template<std::size_t N, class Ty>
        using take = typename take_impl<N, Ty>::type;

        // Drop the first N Args in Tuple
        template<std::size_t, class> struct drop_impl;
        template<std::size_t N, template<class...> class Tuple, class ...Args>
        struct drop_impl<N, Tuple<Args...>> {
            template<class> struct helper;
            template<std::size_t ...Is>
            struct helper<std::index_sequence<Is...>> {
                using type = Tuple<element<Is + N, Args...>...>;
            };
            using type = typename helper<
                std::make_index_sequence<sizeof...(Args) - N>>::type;
        };
        template<std::size_t N, class Ty>
        using drop = typename drop_impl<N, Ty>::type;

        // Helper to only keep types at indices in Array
        template<auto, class> struct keep_at_indices;
        template<auto Array, template<class...> class Tuple, class ...Args>
        struct keep_at_indices<Array, Tuple<Args...>> {
            template<class> struct helper;
            template<std::size_t ...Is>
            struct helper<std::index_sequence<Is...>> {
                using type = Tuple<element<Array[Is], Args...>...>;
            };
            using type = typename helper<std::make_index_sequence<Array.size()>>::type;
        };

        // Remove Ty from Args
        template<class, class> struct remove_impl;
        template<class Ty, template<class...> class Tuple, class ...Args>
        struct remove_impl<Ty, Tuple<Args...>> {
            using type = typename keep_at_indices<
                indices_except<Ty, Args...>, Tuple<Args...>>::type;
        };
        template<class T, class Ty>
        using remove = typename remove_impl<T, Ty>::type;

        // Remove Tys from Args
        template<class, class> struct remove_all_impl;
        template<template<class...> class Ty, class ...Tys,
            template<class...> class Tuple, class ...Args>
        struct remove_all_impl<Ty<Tys...>, Tuple<Args...>> {
            using type = typename keep_at_indices<
                indices_except_all<kaixo::pack<Tys...>, Args...>, Tuple<Args...>>::type;
        };
        template<class T, class Ty>
        using remove_all = typename remove_all_impl<T, Ty>::type;

        // Keeps Ty in Args
        template<class, class> struct keep_impl;
        template<class Ty, template<class...> class Tuple, class ...Args>
        struct keep_impl<Ty, Tuple<Args...>> {
            using type = typename keep_at_indices<
                indices<Ty, Args...>, Tuple<Args...>>::type;
        };
        template<class T, class Ty>
        using keep = typename keep_impl<T, Ty>::type;

        // Keeps Tys in Args
        template<class, class> struct keep_all_impl;
        template<template<class...> class Ty, class ...Tys,
            template<class...> class Tuple, class ...Args>
        struct keep_all_impl<Ty<Tys...>, Tuple<Args...>> {
            using type = typename keep_at_indices<
                indices_all<Ty<Tys...>, Args...>, Tuple<Args... >>::type;
        };
        template<class T, class Ty>
        using keep_all = typename keep_all_impl<T, Ty>::type;

        // Generates indices from 0 to I, excluding all in C
        template<std::size_t I, std::size_t ...C>
        constexpr auto generate_indices = []() {
            std::array<std::size_t, I - sizeof...(C)> _indices{};
            std::size_t _index = 0;
            std::size_t _match = 0;
            for (std::size_t _index = 0; _index < I; ++_index)
                if (((_index != C) && ...)) _indices[_match++] = _index;
            return _indices;
        }();

        // Remove index I from Args
        template<std::size_t, class> struct erase_impl;
        template<std::size_t I, template<class...> class Tuple, class ...Args>
        struct erase_impl<I, Tuple<Args...>> {
            using type = typename keep_at_indices<
                generate_indices<sizeof...(Args), I>, Tuple<Args... >>::type;
        };
        template<std::size_t I, class Ty>
        using erase = typename erase_impl<I, Ty>::type;

        // Erase all indices I from Args
        template<class, std::size_t...> struct erase_all_impl;
        template<std::size_t ...I, template<class...> class Tuple, class ...Args>
        struct erase_all_impl<Tuple<Args...>, I...> {
            using type = typename keep_at_indices<
                generate_indices<sizeof...(Args), I...>, Tuple<Args... >>::type;
        };
        template<class Ty, std::size_t ...Is>
        using erase_all = typename erase_all_impl<Ty, Is...>::type;

        // Append Ty to Args
        template<class, class> struct append_impl;
        template<class Ty, template<class...> class Tuple, class ...Args>
        struct append_impl<Ty, Tuple<Args...>> {
            using type = Tuple<Args..., Ty>;
        };
        template<class T, class Ty>
        using append = typename append_impl<T, Ty>::type;

        // Append Tys to Args
        template<class, class> struct append_all_impl;
        template<template<class...> class Ty, class ...Tys,
            template<class...> class Tuple, class ...Args>
        struct append_all_impl<Ty<Tys...>, Tuple<Args...>> {
            using type = Tuple<Args..., Tys...>;
        };
        template<class T, class Ty>
        using append_all = typename append_all_impl<T, Ty>::type;

        // Prepend Ty to Args
        template<class, class> struct prepend_impl;
        template<class Ty, template<class...> class Tuple, class ...Args>
        struct prepend_impl<Ty, Tuple<Args...>> {
            using type = Tuple<Ty, Args...>;
        };
        template<class T, class Ty>
        using prepend = typename prepend_impl<T, Ty>::type;

        // Prepend Tys to Args
        template<class, class> struct prepend_all_impl;
        template<template<class...> class Ty, class ...Tys,
            template<class...> class Tuple, class ...Args>
        struct prepend_all_impl<Ty<Tys...>, Tuple<Args...>> {
            using type = Tuple<Tys..., Args...>;
        };
        template<class T, class Ty>
        using prepend_all = typename prepend_all_impl<T, Ty>::type;

        // Insert T at position I in the template parameters of Ty
        template<std::size_t I, class T, class Ty>
        using insert = append_all<drop<I, Ty>, append<T, take<I, Ty>>>;

        // Insert T at position I in the template parameters of Ty
        template<std::size_t I, class T, class Ty>
        using insert_all = append_all<drop<I, Ty>, append_all<T, take<I, Ty>>>;

        // Get indices of first occurrences of all types in Args
        template<class ...Args>
        constexpr auto first_indices = first_indices_impl<Args...>();

        // Erase all indices I from Args
        template<class> struct unique_impl;
        template<template<class...> class Tuple, class ...Args>
        struct unique_impl<Tuple<Args...>> {
            template<class> struct helper;
            template<std::size_t ...Is>
            struct helper<std::index_sequence<Is...>> {
                using type = Tuple<element<
                    first_indices<Args...>[Is], Args...>...>;
            };
            using type = typename helper<
                std::make_index_sequence<unique_count<Args...>>>::type;
        };
        template<class Ty>
        using unique = typename unique_impl<Ty>::type;

        // Keeps all template parameters of Ty from index S to E 
        template<std::size_t S, std::size_t E, class Ty>
        using sub = take<(E - S), drop<S, Ty>>;

        // Fake lambda used for filtering
        template<class L>
        struct fake_lambda : L {
            template<class Ty> requires requires (L l) {
                { l.template operator() < Ty::get() > () } -> std::same_as<bool>;
            }
            consteval bool operator()() { return L::template operator() < Ty::get() > (); }
            // Operator can't be instantiated with Ty
            template<class Ty> requires (!requires (L l) {
                { l.template operator() < Ty > () };
            } && (!requires (L l) {
                { l.template operator() < Ty::get() > () } -> std::same_as<bool>;
            }))
                consteval bool operator()() { return false; }
            // Operator can be instantiated and returns a bool
            template<class Ty> requires (requires (L l) {
                { l.template operator() < Ty > () } -> std::same_as<bool>;
            } && (!requires (L l) {
                { l.template operator() < Ty::get() > () } -> std::same_as<bool>;
            }))
                consteval bool operator()() { // Call operator
                return L::template operator() < Ty > ();
            }
            // Operator can be instantiated but returns void
            // This means constraint on template parameter.
            template<class Ty> requires requires (L l) {
                { l.template operator() < Ty > () } -> std::same_as<void>;
            }
            consteval bool operator()() { return true; }
        };

        // Count amount of types in Args after filtering
        template<auto Lambda, class ...Args>
        consteval std::size_t count_filter_impl() {
            return ((static_cast<std::size_t>(fake_lambda{ Lambda }.operator() < Args > ())) + ...);
        }
        template<auto Lambda, class ...Args>
        constexpr std::size_t count_filter = count_filter_impl<Lambda, Args...>();

        // Get all indices that match the filter
        template<auto Lambda, class ...Args>
        consteval std::array<std::size_t,
            count_filter<Lambda, Args...>> filter_indices_impl() {
            std::array<std::size_t, count_filter<Lambda, Args...>> _indices{};
            std::size_t _index = 0;
            std::size_t _match = 0;
            ((fake_lambda{ Lambda }.operator() < Args > () ?
                _indices[_match++] = _index++ : ++_index), ...);
            return _indices;
        }
        template<auto Lambda, class ...Args>
        constexpr std::array<std::size_t, count_filter<Lambda, Args...>>
            filter_indices = filter_indices_impl<Lambda, Args...>();

        // Apply filter
        template<class, auto> struct filter_impl;
        template<auto Lambda, template<class...> class Tuple, class ...Args>
        struct filter_impl<Tuple<Args...>, Lambda> {
            template<class> struct helper;
            template<std::size_t ...Is>
            struct helper<std::index_sequence<Is...>> {
                using type = Tuple<element<filter_indices<Lambda, Args...>[Is], Args...>...>;
            };
            using type = typename helper<
                std::make_index_sequence<count_filter<Lambda, Args...>>>::type;
        };

        // Filter the template parameters of Ty using a templated lambda
        template<class Ty, auto Lambda>
        using filter = typename detail::filter_impl<Ty, Lambda>::type;

        // Merge sort for types
        template<auto Lambda, class ...As>
        constexpr auto merge_sort_merge(kaixo::pack<As...>, kaixo::pack<>) {
            return kaixo::pack<As...>{};
        }
        template<auto Lambda, class ...Bs>
        constexpr auto merge_sort_merge(kaixo::pack<>, kaixo::pack<Bs...>) {
            return kaixo::pack<Bs...>{};
        }
        template<auto Lambda, class A, class B>
        constexpr auto merge_sort_lambda(
            std::type_identity<A>, std::type_identity<B>) {
            return Lambda.template operator() < A, B > ();
        }
        template<auto Lambda, class A, class ...As, class B, class ...Bs>
        constexpr auto merge_sort_merge(kaixo::pack<A, As...>, kaixo::pack<B, Bs...>) {
            if constexpr (merge_sort_lambda<Lambda>(
                std::type_identity<A>{}, std::type_identity<B>{}))
                return append_all<
                decltype(merge_sort_merge<Lambda>(
                    kaixo::pack<As...>{},
                    kaixo::pack<B, Bs...>{})),
                kaixo::pack<A >> {};
            else
                return append_all<
                decltype(merge_sort_merge<Lambda>(
                    kaixo::pack<A, As...>{},
                    kaixo::pack<Bs...>{})),
                kaixo::pack<B >> {};
        }
        template<auto Lambda, class ...Args>
        constexpr auto merge_sort(kaixo::pack<Args...>) {
            if constexpr (kaixo::pack<Args...>::size > 1) {
                constexpr std::size_t mid = kaixo::pack<Args...>::size / 2;
                constexpr auto left = merge_sort<Lambda>(typename kaixo::pack<Args...>::template take<mid>{});
                constexpr auto right = merge_sort<Lambda>(typename kaixo::pack<Args...>::template drop<mid>{});

                return merge_sort_merge<Lambda>(left, right);
            }
            else return kaixo::pack<Args...>{};
        }

        template<auto Lambda, class Ty>
        using sort_impl = decltype(merge_sort<Lambda>(Ty{}));

        template<auto Lambda, class ...Args>
        struct for_each_impl {
            using type = decltype(Lambda.template operator() < Args... > ());
        };

        template<auto Lambda, auto ...Vs>
        struct for_each_impl<Lambda, value<Vs>...> {
            using type = kaixo::value<(Lambda(Vs...))>;
        };
    }

    // Pack utils for a pack of types
    template<class ...Args>
    struct pack {
        constexpr static std::size_t size = sizeof...(Args);
        constexpr static std::size_t unique_count = detail::unique_count<Args...>;
        constexpr static std::size_t bytes = (sizeof(Args) + ... + 0);

        template<class Ty> // First index of Ty in Args
        constexpr static std::size_t index = detail::index<Ty, Args...>;

        template<class Ty> // Last index of Ty in Args
        constexpr static std::size_t last_index = detail::last_index<Ty, Args...>;

        template<class Ty> // Amount of occurrences of Ty in Args
        constexpr static std::size_t count = detail::count<Ty, Args...>;

        template<class Ty> // Check if Ty occurs in Args
        constexpr static bool occurs = detail::occurs<Ty, Args...>;

        template<class ...Tys> // All indices of all Tys in Args
        constexpr static auto indices = detail::indices_all<pack<Tys...>, Args...>;

        template<class ...Tys> // All indices of all types in Args except those in Tys
        constexpr static auto indices_except = detail::indices_except_all<pack<Tys...>, Args...>;

        template<auto Lambda> // All indices of all types that match the filter
        constexpr static auto indices_filter = detail::filter_indices<Lambda, Args...>;

        template<class ...Tys> // Remove all Tys from Args
        using remove = detail::remove_all<pack<Tys...>, pack>;

        template<class ...Tys> // Only keep all Tys in Args
        using keep = detail::keep_all<pack<Tys...>, pack>;

        template<std::size_t I> // Get element at index I
        using element = detail::element<I, Args...>;

        template<std::size_t N> // Get first N elements in Args
        using take = detail::take<N, pack>;

        template<std::size_t N> // Remove first N elements from Args
        using drop = detail::drop<N, pack>;

        // Remove first element from Args
        using tail = drop<1>;

        // First element in Args
        using head = element<0>;

        // Remove last element from Args
        using init = take<size - 1>;

        // Last element in Args
        using last = element<size - 1>;

        // Move the template parameters Args to the templated type Ty
        template<template<class...> class Ty>
        using as = kaixo::move_types<pack, Ty>;

        template<class ...Tys> // Append the types Tys to Args
        using append = pack<Args..., Tys...>;

        template<class ...Tys> // Prepend the types Tys to Args
        using prepend = pack<Tys..., Args...>;

        template<std::size_t I, class ...Tys> // Insert the types Tys in Args at index I
        using insert = detail::insert_all<I, pack<Tys...>, pack>;

        template<std::size_t ...Is> // Erase indices Is from Args
        using erase = detail::erase_all<pack, Is...>;

        // Create a sub pack from index S to index E of Args
        template<std::size_t S, std::size_t E>
        using sub = detail::sub<S, E, pack>;

        // Reverse Args
        using reverse = detail::reverse<pack>;

        // Remove duplicates from Args
        using unique = detail::unique<pack>;

        template<auto Lambda> // Filter using templated lambda
        using filter = detail::filter<pack, Lambda>;

        template<auto Lambda> // Sort using templated lambda
        using sort = detail::sort_impl<Lambda, pack>;

        //template<auto Lambda> // Iterate over each type in a templated Lambda
        constexpr static auto for_each = [](auto Lambda) {
            return Lambda.operator() < Args... > ();
        };
        //using for_each = typename detail::for_each_impl<Lambda, Args...>::type;
    };

    // Pack utils for a pack of values
    template<auto ...Args>
    struct pack<value<Args>...> {
        constexpr static std::size_t size = sizeof...(Args);
        constexpr static std::size_t unique_count = detail::unique_count<value<Args>...>;
        constexpr static std::size_t bytes = (sizeof(Args) + ... + 0);

        template<auto Ty>
        constexpr static std::size_t index = detail::index<value<Ty>, value<Args>...>;

        template<auto Ty>
        constexpr static std::size_t last_index = detail::last_index<value<Ty>, value<Args>...>;

        template<auto Ty>
        constexpr static std::size_t count = detail::count<value<Ty>, value<Args>...>;

        template<auto Ty> // Check if Ty occurs in Args
        constexpr static bool occurs = detail::occurs<value<Ty>, value<Args>...>;

        template<auto ...Tys> // All indices of all Tys in Args
        constexpr static auto indices = detail::indices_all<pack<value<Tys>...>, value<Args>...>;

        template<auto ...Tys> // All indices of all values in Args except those in Tys
        constexpr static auto indices_except = detail::indices_except_all<pack<value<Tys>...>, value<Args>...>;

        template<auto Lambda> // All indices of all types that match the filter
        constexpr static auto indices_filter = detail::filter_indices<Lambda, value<Args>...>;

        template<auto ...Tys> // Remove all Tys from Args
        using remove = detail::remove_all<pack<value<Tys>...>, pack>;

        template<auto ...Tys> // Only keep all Tys in Args
        using keep = detail::keep_all<pack<value<Tys>...>, pack>;

        template<std::size_t I> // Get element at index I
        constexpr static auto element = detail::element<I, value<Args>...>::get();

        template<std::size_t N> // Get first N elements in Args
        using take = detail::take<N, pack>;

        template<std::size_t N> // Remove first N elements from Args
        using drop = detail::drop<N, pack>;

        // Remove first element from Args
        using tail = drop<1>;

        // First element in Args
        constexpr static auto head = element<0>;

        // Remove last element from Args
        using init = take<size - 1>;

        // Last element in Args
        constexpr static auto last = element<size - 1>;

        // Move the template parameters Args to the templated type Ty
        template<template<class...> class Ty>
        using as = kaixo::move_types<pack, Ty>;

        template<auto ...Tys> // Append the values Tys to Args
        using append = pack<value<Args>..., value<Tys>...>;

        template<auto ...Tys> // Prepend the values Tys to Args
        using prepend = pack<value<Tys>..., value<Args>...>;

        template<std::size_t I, auto ...Tys> // Insert the values Tys in Args at index I
        using insert = detail::insert_all<I, pack<value<Tys>...>, pack>;

        template<std::size_t ...Is> // Erase indices Is from Args
        using erase = detail::erase_all<pack, Is...>;

        // Create a sub pack from index S to index E of Args
        template<std::size_t S, std::size_t E>
        using sub = detail::sub<S, E, pack>;

        // Reverse Args
        using reverse = detail::reverse<pack>;

        // Remove duplicates from Args
        using unique = detail::unique<pack>;

        template<auto Lambda> // Filter using a lambda
        using filter = detail::filter<pack, Lambda>;

        template<auto Lambda> // Sort using a Lambda
        using sort = detail::sort_impl<Lambda, pack>;

        //template<auto Lambda> // Iterate over all values using a Lambda
        constexpr static auto for_each = [](auto Lambda) {
            return Lambda.operator() < Args... > ();
        };
        //detail::for_each_impl<Lambda, value<Args>...>::type::get();
    };

    template<>
    struct pack<> {
        constexpr static std::size_t size = 0;
        constexpr static std::size_t unique_count = 0;
        constexpr static std::size_t bytes = 0;

        template<class Ty> // First index of Ty in Args
        constexpr static std::size_t index = detail::index<Ty>;

        template<class Ty> // Last index of Ty in Args
        constexpr static std::size_t last_index = detail::last_index<Ty>;

        template<class Ty> // Amount of occurrences of Ty in Args
        constexpr static std::size_t count = 0;

        template<class Ty> // Check if Ty occurs in Args
        constexpr static bool occurs = false;

        template<class ...Tys> // All indices of all Tys in Args
        constexpr static auto indices = std::array<std::size_t, 0>{};

        template<class ...Tys> // All indices of all types in Args except those in Tys
        constexpr static auto indices_except = std::array<std::size_t, 0>{};

        template<auto Lambda> // All indices of all types that match the filter
        constexpr static auto indices_filter = std::array<std::size_t, 0>{};

        template<class ...Tys> // Remove all Tys from Args
        using remove = kaixo::pack<>;

        template<class ...Tys> // Only keep all Tys in Args
        using keep = kaixo::pack<>;

        template<std::size_t I> // Get element at index I
        using element = detail::element<I>;

        template<std::size_t N> // Get first N elements in Args
        using take = detail::take<N, pack>;

        template<std::size_t N> // Remove first N elements from Args
        using drop = detail::drop<N, pack>;

        // Move the template parameters Args to the templated type Ty
        template<template<class...> class Ty>
        using as = kaixo::move_types<pack, Ty>;

        template<class ...Tys> // Append the types Tys to Args
        using append = pack<Tys...>;

        template<class ...Tys> // Prepend the types Tys to Args
        using prepend = pack<Tys...>;

        template<std::size_t I, class ...Tys> // Insert the types Tys in Args at index I
        using insert = pack<Tys...>;

        template<std::size_t ...Is> // Erase indices Is from Args
        using erase = detail::erase_all<pack, Is...>;

        // Create a sub pack from index S to index E of Args
        template<std::size_t S, std::size_t E>
        using sub = detail::sub<S, E, pack>;

        // Reverse Args
        using reverse = kaixo::pack<>;

        // Remove duplicates from Args
        using unique = kaixo::pack<>;

        template<auto Lambda> // Filter using templated lambda
        using filter = kaixo::pack<>;

        template<auto Lambda> // Sort using templated lambda
        using sort = kaixo::pack<>;
    };

    namespace detail {
        template<class> struct as_pack_impl;
        template<template<class ...> class Ty, class ...Args>
        struct as_pack_impl<Ty<Args...>> {
            using type = kaixo::pack<Args...>;
        };
    }

    template<class Ty> // Convert templated type to pack
    using as_pack = typename detail::as_pack_impl<Ty>::type;

    template<auto ...Values> // Template pack of values to pack
    using to_pack = kaixo::pack<value<Values>...>;

    template<auto Array>
    constexpr auto iterate = [](auto Lambda) {
        return[Lambda]<std::size_t ...Is>(std::index_sequence<Is...>) {
            return Lambda.operator() < Array[Is]... > ();
        }(std::make_index_sequence<Array.size()>{});
    };

    // Global utils
    template<class ...Args>
    constexpr static std::size_t size = sizeof...(Args);

    template<class ...Args> // Amount of unique types in Args
    constexpr static std::size_t unique_count = detail::unique_count<Args...>;

    template<class ...Args> // Sum of bytes of types in Args
    constexpr static std::size_t bytes = (sizeof(Args) + ... + 0);

    template<class Ty, class ...Args> // First index of Ty in Args
    constexpr static std::size_t index = detail::index<Ty, Args...>;

    template<class Ty, class ...Args> // Last index of Ty in Args
    constexpr static std::size_t last_index = detail::last_index<Ty, Args...>;

    template<class Ty, class ...Args> // Amount of occurrences of Ty in Args
    constexpr static std::size_t count = detail::count<Ty, Args...>;

    template<class Ty, class ...Args> // Check if Ty occurs in Args
    constexpr static bool occurs = detail::occurs<Ty, Args...>;

    template<class Ty, class ...Args> // All indices of Ty in Args
    constexpr static auto indices = detail::indices<Ty, Args...>;

    template<class Ty, class ...Args> // All indices of types in Args except Ty
    constexpr static auto indices_except = detail::indices_except<Ty, Args...>;

    template<auto Lambda, class ...Args> // All indices of all types that match the filter
    constexpr static auto indices_filter = detail::filter_indices<Lambda, Args...>;

    template<std::size_t I, class ...Args> // Get element at index I
    using element = detail::element<I, Args...>;

    template<class A, class B> // Remove all Tys from Args
    using remove = detail::remove_all<A, B>;

    template<class A, class B> // Only keep all Tys in Args
    using keep = detail::keep_all<A, B>;

    template<std::size_t N, class A> // Get first N elements in Args
    using take = detail::take<N, A>;

    template<std::size_t N, class A> // Remove first N elements from Args
    using drop = detail::drop<N, A>;

    template<class A> // Remove first element from Args
    using tail = drop<1, A>;

    template<class ...Args> // First element in Args
    using head = element<0, Args...>;

    template<class ...Args> // Last element in Args
    using last = element<size<Args...> -1, Args...>;

    template<class A, std::size_t I, class B> // Insert the types Tys in Args at index I
    using insert = detail::insert_all<I, A, B>;

    template<class A, std::size_t ...Is> // Erase indices Is from Args
    using erase = detail::erase_all<A, Is...>;

    // Create a sub pack from index S to index E of Args
    template<class A, std::size_t S, std::size_t E>
    using sub = detail::sub<S, E, A>;

    // Reverse Args
    template<class A>
    using reverse = detail::reverse<A>;

    // Remove duplicates from Args
    template<class A>
    using unique = detail::unique<A>;

    namespace detail {
        template<class...>struct concat_impl;
        template<template<class...> class A, class ...As>
        struct concat_impl<A<As...>> { using type = A<As...>; };
        template<template<class...> class A, class ...As, class ...Bs, class ...Rest>
        struct concat_impl<A<As...>, A<Bs...>, Rest...> { using type = typename concat_impl<A<As..., Bs...>, Rest...>::type; };
    }

    template<class ...Args>
    using concat = typename detail::concat_impl<Args...>::type;
}