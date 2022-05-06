#include <array>

namespace kaixo {
    namespace detail {
        // Single, non-templated type -> Ty<T>
        template<class T, template<class...> class Ty>
        struct move_types_impl { using type = Ty<T>; };
        // Convert T<Args...> to Ty<Args...>
        template<template<class...> class T, class ...Args, template<class...> class Ty>
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

        // Get type from index using the indexer and overload resolution
        template<std::size_t I, class Ty>
        consteval indexed<I, Ty> element_impl(indexed<I, Ty>) {};

        // Find index of first occurence of Ty in Args
        template<class Ty, class ...Args>
        consteval std::size_t index_impl() {
            std::size_t _index = 0; // use short-circuit to increment until
            ((++_index, std::same_as<Ty, Args>) || ...); // first occurence
            return _index - 1; // - 1 because we included the type itself
        }

        // Find index of last occurence of Ty in Args
        template<class Ty, class ...Args>
        consteval std::size_t last_index_impl() {
            std::size_t _fromEnd = 0; // increment, but reset on match
            ((std::same_as<Ty, Args> ? _fromEnd = 0 : ++_fromEnd), ...);
            // This counted distance to end, so calculate index from that
            return sizeof...(Args) - _fromEnd - 1;
        }

        // Count the number of occurences of Ty in Args
        template<class Ty, class ...Args>
        constexpr std::size_t count_impl = ((std::same_as<Ty, Args>) + ... + 0);

        // Find indices of all occurence of Ty in Args
        template<class Ty, class ...Args>
        consteval std::array<std::size_t, count_impl<Ty, Args...>> indices_impl() {
            std::array<std::size_t, count_impl<Ty, Args...>> _result{};
            std::size_t _index = 0;
            std::size_t _match = 0;
            ((std::same_as<Ty, Args> ? _result[_match++] = _index++ : ++_index), ...);
            return _result;
        }

        // Find indices of all types except Ty in Args
        template<class Ty, class ...Args>
        consteval std::array<std::size_t, sizeof...(Args)
            - count_impl<Ty, Args...>> indices_except_impl() {
            std::array<std::size_t, sizeof...(Args) - count_impl<Ty, Args...>> _result{};
            std::size_t _index = 0;
            std::size_t _match = 0;
            ((std::same_as<Ty, Args> ? ++_index : _result[_match++] = _index++), ...);
            return _result;
        }
    }

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

    // Indices of all types except Ty in Args
    template<class Ty, class ...Args>
    constexpr std::array<std::size_t, sizeof...(Args) - count<Ty, Args...>>
        indices_except = detail::indices_except_impl<Ty, Args...>();

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
            using type = typename helper<std::make_index_sequence<sizeof...(Args) - N>>::type;
        };

        // Remove Ty from Args
        template<class, class> struct remove_impl;
        template<class Ty, template<class...> class Tuple, class ...Args>
        struct remove_impl<Ty, Tuple<Args...>> {
            template<class> struct helper;
            template<std::size_t ...Is>
            struct helper<std::index_sequence<Is...>> {
                using type = Tuple<element<kaixo::indices_except<Ty, Args...>[Is], Args...>...>;
            };
            using type = typename helper<std::make_index_sequence<
                sizeof...(Args) - count_impl<Ty, Args...>>>::type;
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

    // Template pack helper stuff
    template<class ...Args>
    struct pack {
        constexpr static std::size_t size = sizeof...(Args);
        constexpr static std::size_t bytes = (sizeof(Args) + ...);
        template<class Ty> constexpr static std::size_t index = kaixo::index<Ty, Args...>;
        template<class Ty> constexpr static std::size_t last_index = kaixo::last_index<Ty, Args...>;
        template<class Ty> constexpr static std::size_t count = kaixo::count<Ty, Args...>;
        template<class Ty> constexpr static bool occurs = kaixo::occurs<Ty, Args...>;
        template<class Ty> constexpr static auto indices = kaixo::indices<Ty, Args...>;
        template<class Ty> constexpr static auto indices_except = kaixo::indices_except<Ty, Args...>;
        template<class Ty> using remove = kaixo::remove<Ty, pack>;
        template<std::size_t I> using element = kaixo::element<I, Args...>;
        template<std::size_t N> using take = kaixo::take<N, pack>;
        template<std::size_t N> using drop = kaixo::drop<N, pack>;
        using tail = drop<1>;
        using head = element<0>;
        using init = take<size - 1>;
        using last = element<size - 1>;
        template<template<class...> class Ty> using as = kaixo::move_types<pack, Ty>;
        template<class ...Tys> using append = pack<Args..., Tys...>;
        template<class ...Tys> using prepend = pack<Tys..., Args...>;
    };

    template<template<class...> class Ty, class ...Args>
    struct pack<Ty<Args...>> : pack<Args...> {};
}