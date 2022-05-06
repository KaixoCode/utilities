#include <concepts>
#include <cstddef>
#include <array>
#include <utility>
#include <type_traits>

namespace kaixo {

    // Check if Ty in Args
    template<class Ty, class ...Args>
    constexpr bool contains = (std::same_as<Ty, Args> || ...);

    // Not found
    constexpr std::size_t npos = static_cast<std::size_t>(-1);

    namespace detail {
        // Single, non-templated type -> Ty<T>
        template<class T, template<class...> class Ty>
        struct move_types_impl { using type = Ty<T>; };
        // Convert T<Args...> to Ty<Args...>
        template<template<class...> class T,
            class ...Args, template<class...> class Ty>
        struct move_types_impl<T<Args...>, Ty> { using type = Ty<Args...>; };
    }

    // Move template types from one class to another if first class 
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

        // Find index of first occurence of Ty in Args
        template<class Ty, class ...Args>
        consteval std::size_t index_impl() {
            std::size_t _index = 0; // use short-circuit to increment until
            ((++_index, std::same_as<Ty, Args>) || ...); // first occurence
            // - 1 because we included the type itself
            return _index == sizeof...(Args) ? npos : _index - 1;
        }

        // Find index of last occurence of Ty in Args
        template<class Ty, class ...Args>
        consteval std::size_t last_index_impl() {
            std::size_t _fromEnd = 0; // increment, but reset on match
            ((std::same_as<Ty, Args> ? _fromEnd = 0 : ++_fromEnd), ...);
            // This counted distance to end, so calculate index from that
            std::size_t _index = sizeof...(Args) - _fromEnd - 1;
            return _fromEnd == sizeof...(Args) ? npos : _index;
        }

        // Count the number of occurences of Ty in Args
        template<class Ty, class ...Args>
        constexpr std::size_t count_impl = ((std::same_as<Ty, Args>) + ... + 0);

        // Find indices of all occurence of Ty in Args
        template<class Ty, class ...Args>
        consteval std::array<std::size_t, 
            count_impl<Ty, Args...>> indices_impl() {
            std::array<std::size_t, count_impl<Ty, Args...>> _result{};
            std::size_t _index = 0;
            std::size_t _match = 0;
            ((std::same_as<Ty, Args> ? 
                _result[_match++] = _index++ : ++_index), ...);
            return _result;
        }
        
        // Find indices of all occurence of Ty in Args
        template<class ...Args, template<class...> class Ty, class ...Tys>
        consteval std::array<std::size_t, 
            (count_impl<Tys, Args...> +...)> indices_all_impl(
            std::type_identity<Ty<Tys...>>) {
            std::array<std::size_t, (count_impl<Tys, Args...> +...)> _result{};
            std::size_t _index = 0;
            std::size_t _match = 0;
            ((contains<Args, Tys...> ? _result[_match++] = _index++ : ++_index), ...);
            return _result;
        }

        // Find indices of all types except Ty in Args
        template<class Ty, class ...Args>
        consteval std::array<std::size_t, sizeof...(Args)
            - count_impl<Ty, Args...>> indices_except_impl() {
            std::array<std::size_t, sizeof...(Args) - 
                count_impl<Ty, Args...>> _result{};
            std::size_t _index = 0;
            std::size_t _match = 0;
            ((std::same_as<Ty, Args> 
                ? ++_index : _result[_match++] = _index++), ...);
            return _result;
        }

        // Find indices of all types except Ty in Args
        template<class ...Args, template<class...> class Ty, class ...Tys>
        consteval std::array<std::size_t, sizeof...(Args)
            - (count_impl<Tys, Args...> + ...)> indices_except_all_impl(
                std::type_identity<Ty<Tys...>>) {
            std::array<std::size_t, sizeof...(Args) - 
                (count_impl<Tys, Args...> +...)> _result{};
            std::size_t _index = 0;
            std::size_t _match = 0;
            ((contains<Args, Tys...> ? ++_index : _result[_match++] = _index++), ...);
            return _result;
        }

        // Reverse Args
        template<class> struct reverse_impl;
        template<template<class...> class Tuple, class ...Args> 
        struct reverse_impl<Tuple<Args...>> {
            template<class> struct helper;
            template<std::size_t... Is>
            struct helper<std::index_sequence<Is...>> {
                using type = Tuple<typename decltype(
                    detail::element_impl<Is>(
                        detail::reverse_indexer<Args...>{}))::type...>;
            };
            using type = typename helper<std::index_sequence_for<Args...>>::type;
        };
    }

    // Reverses the template parameters in Ty
    template<class Ty>
    using reverse = typename detail::reverse_impl<Ty>::type;

    // Get type at index I in pack Args
    template<std::size_t I, class ...Args>
    using element = typename decltype(
        detail::element_impl<I>(detail::indexer<Args...>{}))::type;

    // Get index of first occurence of Ty in Args
    template<class Ty, class ...Args>
    constexpr std::size_t index = detail::index_impl<Ty, Args...>();

    // Get index of last occurence of Ty in Args
    template<class Ty, class ...Args>
    constexpr std::size_t last_index = detail::last_index_impl<Ty, Args...>();

    // Get number of occurences of Ty in Args
    template<class Ty, class ...Args>
    constexpr std::size_t count = detail::count_impl<Ty, Args...>;

    // Check if Ty occurs in Args
    template<class Ty, class ...Args>
    constexpr bool occurs = count<Ty, Args...> > 0;

    // Indices of all occurences of Ty in Args
    template<class Ty, class ...Args>
    constexpr std::array<std::size_t, count<Ty, Args...>>
        indices = detail::indices_impl<Ty, Args...>();

    // Indices of all occurences of all template parameters of Ty in Args
    template<class Ty, class ...Args>
    constexpr auto indices_all = 
        detail::indices_all_impl<Args...>(std::type_identity<Ty>{});

    // Indices of all types except Ty in Args
    template<class Ty, class ...Args>
    constexpr std::array<std::size_t, sizeof...(Args) - count<Ty, Args...>>
        indices_except = detail::indices_except_impl<Ty, Args...>();
    
    // Indices of all types except the template parameters of Ty in Args
    template<class Ty, class ...Args>
    constexpr auto indices_except_all = 
        detail::indices_except_all_impl<Args...>(std::type_identity<Ty>{});

    namespace detail {
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

        // Remove Ty from Args
        template<class, class> struct remove_impl;
        template<class Ty, template<class...> class Tuple, class ...Args>
        struct remove_impl<Ty, Tuple<Args...>> {
            template<class> struct helper;
            template<std::size_t ...Is>
            struct helper<std::index_sequence<Is...>> {
                using type = Tuple<element<
                    kaixo::indices_except<Ty, Args...>[Is], Args...>...>;
            };
            using type = typename helper<std::make_index_sequence<
                sizeof...(Args) - count_impl<Ty, Args...>>>::type;
        };

        // Remove Tys from Args
        template<class, class> struct remove_all_impl;
        template<template<class...> class Ty, class ...Tys, 
            template<class...> class Tuple, class ...Args>
        struct remove_all_impl<Ty<Tys...>, Tuple<Args...>> {
            template<class> struct helper;
            template<std::size_t ...Is>
            struct helper<std::index_sequence<Is...>> {
                using type = Tuple<element<
                    kaixo::indices_except_all<Ty<Tys...>, Args...>[Is], Args...>...>;
            };
            using type = typename helper<std::make_index_sequence<
                sizeof...(Args) - (count_impl<Tys, Args...> +...)>>::type;
        };

        // Keeps Ty in Args
        template<class, class> struct keep_impl;
        template<class Ty, template<class...> class Tuple, class ...Args>
        struct keep_impl<Ty, Tuple<Args...>> {
            template<class> struct helper;
            template<std::size_t ...Is>
            struct helper<std::index_sequence<Is...>> {
                using type = Tuple<element<
                    kaixo::indices<Ty, Args...>[Is], Args...>...>;
            };
            using type = typename helper<std::make_index_sequence<
                count_impl<Ty, Args...>>>::type;
        };

        // Keeps Tys in Args
        template<class, class> struct keep_all_impl;
        template<template<class...> class Ty, class ...Tys, 
            template<class...> class Tuple, class ...Args>
        struct keep_all_impl<Ty<Tys...>, Tuple<Args...>> {
            template<class> struct helper;
            template<std::size_t ...Is>
            struct helper<std::index_sequence<Is...>> {
                using type = Tuple<element<
                    kaixo::indices_all<Ty<Tys...>, Args...>[Is], Args...>...>;
            };
            using type = typename helper<std::make_index_sequence<
                (count_impl<Tys, Args...> +...)>>::type;
        };

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
            template<class> struct helper;
            template<std::size_t ...Is>
            struct helper<std::index_sequence<Is...>> {
                using type = Tuple<element<
                    generate_indices<sizeof...(Args), I>[Is], Args...>...>;
            };
            using type = typename helper<
                std::make_index_sequence<sizeof...(Args) - 1>>::type;
        };

        // Erase all indices I from Args
        template<class, std::size_t...> struct erase_all_impl;
        template<std::size_t ...I, template<class...> class Tuple, class ...Args>
        struct erase_all_impl<Tuple<Args...>, I...> {
            template<class> struct helper;
            template<std::size_t ...Is>
            struct helper<std::index_sequence<Is...>> {
                using type = Tuple<element<
                    generate_indices<sizeof...(Args), I...>[Is], Args...>...>;
            };
            using type = typename helper<
                std::make_index_sequence<sizeof...(Args) - sizeof...(I)>>::type;
        };

        // Append Ty to Args
        template<class, class> struct append_impl;
        template<class Ty, template<class...> class Tuple, class ...Args>
        struct append_impl<Ty, Tuple<Args...>> {
            using type = Tuple<Args..., Ty>;
        };

        // Append Tys to Args
        template<class, class> struct append_all_impl;
        template<template<class...> class Ty, class ...Tys,
            template<class...> class Tuple, class ...Args>
        struct append_all_impl<Ty<Tys...>, Tuple<Args...>> {
            using type = Tuple<Args..., Tys...>;
        };

        // Prepend Ty to Args
        template<class, class> struct prepend_impl;
        template<class Ty, template<class...> class Tuple, class ...Args>
        struct prepend_impl<Ty, Tuple<Args...>> {
            using type = Tuple<Ty, Args...>;
        };

        // Prepend Tys to Args
        template<class, class> struct prepend_all_impl;
        template<template<class...> class Ty, class ...Tys, 
            template<class...> class Tuple, class ...Args>
        struct prepend_all_impl<Ty<Tys...>, Tuple<Args...>> {
            using type = Tuple<Tys..., Args...>;
        };
    }

    // Keep the first N template parameters of Ty
    template<std::size_t N, class Ty>
    using take = typename detail::take_impl<N, Ty>::type;

    // Drop the first N template parameters of Ty
    template<std::size_t N, class Ty>
    using drop = typename detail::drop_impl<N, Ty>::type;

    // Remove T from the template parameters of Ty
    template<class T, class Ty>
    using remove = typename detail::remove_impl<T, Ty>::type;

    // Remove the template parameters of T from the template parameters of Ty
    template<class T, class Ty>
    using remove_all = typename detail::remove_all_impl<T, Ty>::type;

    // Keeps T in the template parameters of Ty
    template<class T, class Ty>
    using keep = typename detail::keep_impl<T, Ty>::type;

    // Keeps the template parameters of T in the template parameters of Ty
    template<class T, class Ty>
    using keep_all = typename detail::keep_all_impl<T, Ty>::type;

    // Append T to the template parameters of Ty
    template<class T, class Ty>
    using append = typename detail::append_impl<T, Ty>::type;

    // Append the template parameters of T to the template parameters of Ty
    template<class T, class Ty>
    using append_all = typename detail::append_all_impl<T, Ty>::type;

    // Prepend T to the template parameters of Ty
    template<class T, class Ty>
    using prepend = typename detail::prepend_impl<T, Ty>::type;
    
    // Prepend the template parameters of T to the template parameters of Ty
    template<class T, class Ty>
    using prepend_all = typename detail::prepend_all_impl<T, Ty>::type;

    // Insert T at position I in the template parameters of Ty
    template<std::size_t I, class T, class Ty>
    using insert = kaixo::append_all<kaixo::drop<I, Ty>, 
        kaixo::append<T, kaixo::take<I, Ty>>>;

    // Insert T at position I in the template parameters of Ty
    template<std::size_t I, class T, class Ty>
    using insert_all = kaixo::append_all<kaixo::drop<I, Ty>, 
        kaixo::append_all<T, kaixo::take<I, Ty>>>;

    // Erase index I from the template parameters of Ty
    template<std::size_t I, class Ty>
    using erase = typename detail::erase_impl<I, Ty>::type;

    // Erase all indices Is from the template parameters of Ty
    template<class Ty, std::size_t ...Is>
    using erase_all = typename detail::erase_all_impl<Ty, Is...>::type;

    // Keeps all template parameters of Ty from index S to E 
    template<std::size_t S, std::size_t E, class Ty>
    using sub = kaixo::take<E - S, kaixo::drop<S, Ty>>;

    // Template pack helper stuff
    template<class ...Args>
    struct pack {
        // Amount of template arguments
        constexpr static std::size_t size = sizeof...(Args);

        // Sum of sizes of all template arguments
        constexpr static std::size_t bytes = (sizeof(Args) + ...);

        // First index of Ty in Args
        template<class Ty> 
        constexpr static std::size_t index = kaixo::index<Ty, Args...>;

        // Last index of Ty in Args
        template<class Ty> 
        constexpr static std::size_t last_index = kaixo::last_index<Ty, Args...>;

        // Amount of occurrences of Ty in Args
        template<class Ty> 
        constexpr static std::size_t count = kaixo::count<Ty, Args...>;

        // Check if Ty occurs in Args
        template<class Ty> 
        constexpr static bool occurs = kaixo::occurs<Ty, Args...>;

        // All indices of all Tys in Args
        template<class ...Tys>
        constexpr static auto indices = kaixo::indices_all<pack<Tys...>, Args...>;

        // All indices of all types in Args except those in Tys
        template<class ...Tys> 
        constexpr static auto indices_except = kaixo::indices_except_all<pack<Tys...>, Args...>;

        // Remove all Tys from Args
        template<class ...Tys> 
        using remove = kaixo::remove_all<pack<Tys...>, pack>;

        // Only keep all Tys in Args
        template<class ...Tys> 
        using keep = kaixo::keep_all<pack<Tys...>, pack>;

        // Get element at index I
        template<std::size_t I> 
        using element = kaixo::element<I, Args...>;

        // Get first N elements in Args
        template<std::size_t N> 
        using take = kaixo::take<N, pack>;

        // Remove first N elements from Args
        template<std::size_t N> 
        using drop = kaixo::drop<N, pack>;
        
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

        // Append the types Tys to Args
        template<class ...Tys> 
        using append = pack<Args..., Tys...>;

        // Prepend the types Tys to Args
        template<class ...Tys>
        using prepend = pack<Tys..., Args...>;

        // Insert the types Tys in Args at index I
        template<std::size_t I, class ...Tys> 
        using insert = kaixo::insert_all<I, pack<Tys...>, pack>;
        
        // Erase indices Is from Args
        template<std::size_t ...Is> 
        using erase = kaixo::erase_all<pack, Is...>;
        
        // Create a sub pack from index S to index E of Args
        template<std::size_t S, std::size_t E> 
        using sub = kaixo::sub<S, E, pack>;

        // Reverse Args
        using reverse = kaixo::reverse<pack>;
    };

    template<template<class...> class Ty, class ...Args>
    struct pack<Ty<Args...>> : pack<Args...> {};
}