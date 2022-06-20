#pragma once
#include <type_traits>
#include <concepts>
#include <cstddef>
#include <array>
#include <utility>
#include <string_view>
#include <algorithm>
#include <source_location>

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                           *
 *                       Kaixo Utils                         *
 *                                                           *
 * String Literal                                            *
 *   template compatible wrapper for a string literal.       *
 *   acts like a proper string, with iterator, index         *
 *   operator, even types like value_type etc.               *
 * Pack Utils:                                               *
 *   defines a pack<Args...> to help deal with template      *
 *   packs adds stuff like element<I>, specialization for    *
 *   template values, filter<Lambda> to filter types using   *
 *   concepts or other things, sort<Lambda> to sort the      *
 *   types however you want.                                 *
 * Function Utils:                                           *
 *   has a function_info<Callable> that contains information *
 *   on argument types, return type. And if it's a member    *
 *   function pointer it also contains information on the    *
 *   object it belongs to, cv/ref/noexcept modifiers etc.    *
 * Type Utils:                                               *
 *   defines an info<Ty> to help deal with type_traits.      *
 *   contains specialization for callables to include        *
 *   all the function_info as well. Also has a namespace     *
 *   called type_concepts that contains all std type_traits  *
 *   as concepts.                                            *
 *                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

namespace kaixo {

    constexpr std::size_t npos = static_cast<std::size_t>(-1);

    struct dud {};
    template<class Ty> struct info_base;
    template<class Ty> struct info : info_base<Ty> {};
    template<class ...Args> struct pack;
    template<class ...Tys> struct pack_info;

    template<class Ty> 
    constexpr static std::size_t sizeof_v = [] { 
        if constexpr (std::is_function_v<Ty>) return 0; 
        else return sizeof(Ty); 
    }();

    template<class Ty> 
    constexpr static std::size_t alignof_v = [] { 
        if constexpr (std::is_function_v<Ty>) return 0; 
        else return std::alignment_of_v<Ty>; 
    }();

    namespace detail {
        template<class, template<class...> class>
        struct is_specialization : std::false_type {};
        template<template<class...> class Ref, class... Args>
        struct is_specialization<Ref<Args...>, Ref> : std::true_type {};
    }
    // is specialization of templated class
    template<class Test, template<class...> class Ref>
    concept specialization = detail::is_specialization<std::decay_t<Test>, Ref>::value;

    // templated for, calls lambda with template argument std::size_t
    template<std::integral auto S, std::integral auto E = npos> constexpr auto sequence(auto lambda) {
        if constexpr (E == npos) 
        return[&] <std::size_t ...Is>(std::integer_sequence<decltype(S), Is...>) {
            return lambda.operator() < (Is)... > ();
        }(std::make_integer_sequence<decltype(S), S>{});
        else return[&] <auto ...Is>(std::integer_sequence<decltype(E - S), Is...>) {
            return lambda.operator() < (Is + S)... > ();
        }(std::make_integer_sequence<decltype(E - S), E - S>{});
    }

    // templated for, calls lambda with template argument std::size_t
    template<std::integral auto S, std::integral auto E = npos> constexpr void tfor(auto lambda) {
        if constexpr (E == npos) 
        [&] <std::size_t ...Is>(std::integer_sequence<decltype(S), Is...>) {
            (lambda.operator() < Is > (), ...);
        }(std::make_integer_sequence<decltype(S), S>{});
        else [&] <auto ...Is>(std::integer_sequence<decltype(E - S), Is...>) {
            (lambda.operator() < Is + S > (), ...);
        }(std::make_integer_sequence<decltype(E - S), E - S>{});
    }

    // templated for, for tuple, supports concept constraints
    template<class Tuple> constexpr void tfor(Tuple&& tuple, auto lambda) {
        tfor<std::tuple_size_v<std::decay_t<Tuple>>>([&]<std::size_t I> {
            if constexpr (std::is_invocable_v<decltype(lambda),
                decltype(std::get<I>(tuple))>) lambda(std::get<I>(tuple));
        });
    }

    // templated for, for tuple, supports concept constraints
    template<class Tuple> constexpr void tfor(Tuple&& tuple, auto... lambdas) {
        tfor<std::tuple_size_v<std::decay_t<Tuple>>>([&]<std::size_t I> {
            ([&](auto& lambda) {
                if constexpr (std::is_invocable_v<decltype(lambda),
                    decltype(std::get<I>(tuple))>) {
                    lambda(std::get<I>(tuple));
                    return true;
                }
                return false;
                }(lambdas) || ...);
        });
    }

    consteval std::string_view _enum_pretty_name(std::string_view name) noexcept {
        for (std::size_t i = name.size(); i > 0; --i) if (auto& c = name[i - 1];
            !((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_'))) {
            name.remove_prefix(i); break;
        }

        if (name.size() > 0 && ((name.front() >= 'a' && name.front() <= 'z') ||
            (name.front() >= 'A' && name.front() <= 'Z') || (name.front() == '_')))
            return name;

        return {}; // Invalid name.
    }

    template<class Ty, Ty Value>
    consteval auto enum_name_impl() noexcept {
#if defined(__clang__) || defined(__GNUC__)
        constexpr auto name = _enum_pretty_name({ __PRETTY_FUNCTION__, sizeof(__PRETTY_FUNCTION__) - 2 });
#elif defined(_MSC_VER)
        constexpr auto name = _enum_pretty_name({ __FUNCSIG__, sizeof(__FUNCSIG__) - 17 });
#else
        constexpr auto name = string_view{};
#endif
        return name;
    }

    template<class Ty, std::underlying_type_t<Ty> V>
    constexpr auto enum_name = enum_name_impl<Ty, static_cast<Ty>(V)>();

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                           *
     *                      String Literal                       *
     *                                                           *
     *           Basically a string_view for constexpr           *
     *         strings, can be used as template argument         *
     *                                                           *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    template<std::size_t N, class CharType = char>
    struct string_literal {
        constexpr static std::size_t npos = std::basic_string_view<CharType>::npos;

        using view_type = std::basic_string_view<CharType>;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;
        using value_type = CharType;
        using reference = CharType&;
        using const_reference = const CharType&;
        using pointer = CharType*;
        using const_pointer = const CharType*;

        class const_iterator {
        public:
            using iterator_category = std::random_access_iterator_tag;
            using size_type = std::size_t;
            using difference_type = std::ptrdiff_t;
            using value_type = const CharType;
            using reference = const CharType&;

            constexpr const_iterator(const const_iterator&) = default;
            constexpr const_iterator(const_iterator&&) = default;
            constexpr const_iterator& operator=(const const_iterator&) = default;
            constexpr const_iterator& operator=(const_iterator&&) = default;
            constexpr const_iterator() : m_Ptr(nullptr) {}
            constexpr const_iterator(const CharType* ptr) : m_Ptr(ptr) {}

            constexpr reference operator*() const { return *m_Ptr; }
            constexpr const_iterator& operator+=(difference_type d) { m_Ptr += d; return *this; }
            constexpr const_iterator& operator-=(difference_type d) { m_Ptr -= d; return *this; }
            constexpr const_iterator& operator++() { ++m_Ptr; return *this; }
            constexpr const_iterator& operator--() { --m_Ptr; return *this; }
            constexpr const_iterator operator++(int) { auto _c = *this; ++m_Ptr; return _c; }
            constexpr const_iterator operator--(int) { auto _c = *this; --m_Ptr; return _c; }

            constexpr reference operator[](difference_type d) const { return m_Ptr[d]; }

            constexpr auto operator<=>(const const_iterator& other) const = default;

            friend constexpr const_iterator operator+(difference_type a, const const_iterator& b) { return { a + b.m_Ptr }; }
            friend constexpr const_iterator operator+(const const_iterator& a, difference_type b) { return { a.m_Ptr + b }; }
            friend constexpr const_iterator operator-(difference_type a, const const_iterator& b) { return { a - b.m_Ptr }; }
            friend constexpr const_iterator operator-(const const_iterator& a, difference_type b) { return { a.m_Ptr - b }; }
            friend constexpr difference_type operator-(const const_iterator& a, const const_iterator& b) { return a.m_Ptr - b.m_Ptr; }
        protected:
            const CharType* m_Ptr;
        };

        using iterator = const_iterator;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

        constexpr ~string_literal() = default;
        
        consteval string_literal(const CharType(&data)[N]) {
            std::copy_n(data, N, m_Data);
        }

        constexpr string_literal(string_literal&&) = default;
        constexpr string_literal(const string_literal&) = default;
        constexpr string_literal& operator=(string_literal&&) = default;
        constexpr string_literal& operator=(const string_literal&) = default;

        constexpr iterator begin() { return { m_Data }; }
        constexpr iterator end() { return { m_Data + size() }; }
        constexpr const_iterator begin() const { return { m_Data }; }
        constexpr const_iterator end() const { return { m_Data + size() }; }
        constexpr const_iterator cbegin() const { return begin(); }
        constexpr const_iterator cend() const { return end(); }
        constexpr reverse_iterator rbegin() { return end(); }
        constexpr reverse_iterator rend() { return begin(); }
        constexpr const_reverse_iterator rbegin() const { return end(); }
        constexpr const_reverse_iterator rend() const { return begin(); }
        constexpr const_reverse_iterator crbegin() const { return end(); }
        constexpr const_reverse_iterator crend() const { return begin(); }

        constexpr reference at(size_type d) { return m_Data[d]; }
        constexpr const_reference at(size_type d) const { return m_Data[d]; }
        constexpr reference operator[](size_type d) { return m_Data[d]; }
        constexpr const_reference operator[](size_type d) const { return m_Data[d]; }
        constexpr reference front() { return m_Data[0]; }
        constexpr const_reference front() const { return m_Data[0]; }
        constexpr reference back() { return m_Data[size() - 1]; }
        constexpr const_reference back() const { return m_Data[size() - 1]; }

        constexpr pointer data() { return m_Data; }
        constexpr const_pointer data() const { return m_Data; }
        constexpr const_pointer c_str() const { return m_Data; }
        constexpr size_type size() const { return N - 1; }
        constexpr size_type length() const { return size(); }
        constexpr size_type max_size() const { return size(); }
        constexpr bool empty() const { return false; }
        constexpr void swap(string_literal& other) { std::swap(data(), other.data()); }

        constexpr view_type view() const { return { data(), size() }; }
        constexpr operator view_type() const { return { data(), size() }; }

        template<std::size_t I>
        constexpr auto operator==(const string_literal<I, CharType>& other) const {
            if constexpr (I != N) return false;
            else return view() == other.view();
        };
        template<std::size_t I>
        constexpr auto operator<=>(const string_literal<I, CharType>& other) const { return view() <=> other.view(); }

        constexpr auto starts_with(view_type t) const { return view().starts_with(t); }
        constexpr auto ends_with(view_type t) const { return view().ends_with(t); }
        constexpr auto substr(size_type pos = 0, size_type count = npos) const { return view().substr(pos, count); }
        constexpr auto find(std::string_view t, size_type pos = 0) const { return view().find(t, pos); }
        constexpr auto rfind(view_type t, size_type pos = 0) const { return view().rfind(t, pos); }
        constexpr auto find_first_of(view_type t, size_type pos = 0) const { return view().find_first_of(t, pos); }
        constexpr auto find_first_not_of(view_type t, size_type pos = 0) const { return view().find_first_not_of(t, pos); }
        constexpr auto find_last_of(view_type t, size_type pos = 0) const { return view().find_last_of(t, pos); }
        constexpr auto find_last_not_of(view_type t, size_type pos = 0) const { return view().find_last_not_of(t, pos); }

        CharType m_Data[N]{};
    };

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                           *
     *                        Pack Utils                         *
     *                                                           *
     *             Bunch of helper templates to make             *
     *                dealing with template packs                *
     *                    a little bit easier                    *
     *                                                           *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


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

        template<class ...Tys>
        struct pack_or_as_pack_impl {
            using type = pack<Tys...>;
        };

        template<class ...Tys>
        struct pack_or_as_pack_impl<pack<Tys...>> {
            using type = pack<Tys...>;
        };

        template<class ...Tys> // Either is a pack, or put in a pack
        using pack_or_as_pack = typename pack_or_as_pack_impl<Tys...>::type;

        // Change type to Ty, used in fold expressions
        template<class, class Ty> using change = Ty;

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

    namespace detail {
        template<class...>struct concat_impl;
        template<template<class...> class A, class ...As>
        struct concat_impl<A<As...>> { using type = A<As...>; };
        template<template<class...> class A, template<class...> class B, class ...As, class ...Bs, class ...Rest>
        struct concat_impl<A<As...>, B<Bs...>, Rest...> { using type = typename concat_impl<B<As..., Bs...>, Rest...>::type; };
    }

    template<class Ty> concept is_pack = specialization<Ty, pack>;

    // Pack utils for a pack of types
    template<class ...Args>
    struct pack {
        constexpr static std::size_t size = sizeof...(Args);
        constexpr static std::size_t unique_count = detail::unique_count<Args...>;
        constexpr static std::size_t bytes = (sizeof_v<Args> + ... + 0);

        template<class Ty> // First index of Ty in Args
        constexpr static std::size_t index = detail::index<Ty, Args...>;

        template<class Ty> // Last index of Ty in Args
        constexpr static std::size_t last_index = detail::last_index<Ty, Args...>;

        template<class Ty> // Amount of occurrences of Ty in Args
        constexpr static std::size_t count = detail::count<Ty, Args...>;

        template<class Ty> // Check if Ty occurs in Args
        constexpr static bool occurs = detail::occurs<Ty, Args...>;

        template<class ...Pack> // All indices of all in Pack in Args
        constexpr static auto indices = detail::indices_all<detail::pack_or_as_pack<Pack...>, Args...>;

        template<class ...Pack> // All indices of all types in Args except those in in Pack
        constexpr static auto indices_except = detail::indices_except_all<detail::pack_or_as_pack<Pack...>, Args...>;

        template<auto Lambda> // All indices of all types that match the filter
        constexpr static auto indices_filter = detail::filter_indices<Lambda, Args...>;

        template<class ...Pack> // Remove all in Pack from Args
        using remove = detail::remove_all<detail::pack_or_as_pack<Pack...>, pack>;

        template<class ...Pack> // Only keep all in Pack in Args
        using keep = detail::keep_all<detail::pack_or_as_pack<Pack...>, pack>;

        template<std::size_t I> // Get element at index I
        using element = detail::element<I, Args...>;
        
        template<std::size_t I> // Get info of type at index I
        using info = kaixo::info<detail::element<I, Args...>>;

        using pack_info = kaixo::info<pack>;

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

        template<class ...Pack> // Append the types in Pack to Args
        using append = typename detail::concat_impl<pack, detail::pack_or_as_pack<Pack...>>::type;

        template<class ...Pack> // Prepend the types in Pack to Args
        using prepend = typename detail::concat_impl<detail::pack_or_as_pack<Pack...>, pack>::type;

        template<std::size_t I, class ...Pack> // Insert the types Tys in Args at index I
        using insert = detail::insert_all<I, detail::pack_or_as_pack<Pack...>, pack>;

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

        // Calls lambda with all types as template arguments, like
        // Lambda.operator()<Args...>();
        constexpr static auto for_each = [](auto Lambda) {
            return Lambda.operator() < Args... > ();
        };
    };

    // Pack utils for a pack of values
    template<auto ...Args>
    struct pack<value<Args>...> {
        constexpr static std::size_t size = sizeof...(Args);
        constexpr static std::size_t unique_count = detail::unique_count<value<Args>...>;
        constexpr static std::size_t bytes = (sizeof_v<decltype(Args)> + ... + 0);

        template<auto Ty>
        constexpr static std::size_t index = detail::index<value<Ty>, value<Args>...>;

        template<auto Ty>
        constexpr static std::size_t last_index = detail::last_index<value<Ty>, value<Args>...>;

        template<auto Ty>
        constexpr static std::size_t count = detail::count<value<Ty>, value<Args>...>;

        template<auto Ty> // Check if Ty occurs in Args
        constexpr static bool occurs = detail::occurs<value<Ty>, value<Args>...>;

        template<class ...Pack> // All indices of all Tys in Args
        constexpr static auto indices = detail::indices_all<detail::pack_or_as_pack<Pack...>, value<Args>...>;

        template<class ...Pack> // All indices of all values in Args except those in Tys
        constexpr static auto indices_except = detail::indices_except_all<detail::pack_or_as_pack<Pack...>, value<Args>...>;

        template<auto Lambda> // All indices of all types that match the filter
        constexpr static auto indices_filter = detail::filter_indices<Lambda, value<Args>...>;

        template<class ...Pack> // Remove all Tys from Args
        using remove = detail::remove_all<detail::pack_or_as_pack<Pack...>, pack>;

        template<class ...Pack> // Only keep all Tys in Args
        using keep = detail::keep_all<detail::pack_or_as_pack<Pack...>, pack>;

        template<std::size_t I> // Get element at index I
        constexpr static auto element = detail::element<I, value<Args>...>::get();

        template<std::size_t I> // Info of type of value at index I
        using info = kaixo::info<decltype(element<I>)>;

        using pack_info = kaixo::info<pack<decltype(Args)...>>;

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

        template<class ...Pack> // Append the types Tys to Args
        using append = typename detail::concat_impl<pack, detail::pack_or_as_pack<Pack...>>::type;

        template<class ...Pack> // Prepend the types Tys to Args
        using prepend = typename detail::concat_impl<detail::pack_or_as_pack<Pack...>, pack>::type;

        template<std::size_t I, class ...Pack> // Insert the types Tys in Args at index I
        using insert = detail::insert_all<I, detail::pack_or_as_pack<Pack...>, pack>;

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

        // Calls lambda with all values as template arguments, like
        // Lambda.operator()<Args...>();
        constexpr static auto for_each = [](auto Lambda) {
            return Lambda.operator() < Args... > ();
        };
    };

    template<> // Special case for empty pack
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

        template<class ...Pack> // All indices of all Tys in Args
        constexpr static auto indices = std::array<std::size_t, 0>{};

        template<class ...Pack> // All indices of all types in Args except those in Tys
        constexpr static auto indices_except = std::array<std::size_t, 0>{};

        template<auto Lambda> // All indices of all types that match the filter
        constexpr static auto indices_filter = std::array<std::size_t, 0>{};

        template<class ...Pack> // Remove all Tys from Args
        using remove = kaixo::pack<>;

        template<class ...Pack> // Only keep all Tys in Args
        using keep = kaixo::pack<>;

        template<std::size_t I> // Get element at index I
        using element = detail::element<I>;
        
        template<std::size_t I> // Get element at index I
        using info = kaixo::info<detail::element<I>>;

        using pack_info = kaixo::info<pack<>>;

        template<std::size_t N> // Get first N elements in Args
        using take = detail::take<N, pack>;

        template<std::size_t N> // Remove first N elements from Args
        using drop = detail::drop<N, pack>;

        // Move the template parameters Args to the templated type Ty
        template<template<class...> class Ty>
        using as = kaixo::move_types<pack, Ty>;

        template<class ...Pack> // Append the types Tys to Args
        using append = detail::pack_or_as_pack<Pack...>;

        template<class ...Pack> // Prepend the types Tys to Args
        using prepend = detail::pack_or_as_pack<Pack...>;

        template<std::size_t I, class ...Pack> // Insert the types Tys in Args at index I
        using insert = detail::pack_or_as_pack<Pack...>;

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

        // Pack is empty so just calls the lambda with no template arguments
        constexpr static auto for_each = [](auto Lambda) { return Lambda(); };
    };

    // Concat template arguments of templated types Args...
    // requires all templated types to be the same
    template<class ...Args>
    using concat = typename detail::concat_impl<Args...>::type;

    namespace detail {
        template<class> struct as_pack_impl;
        template<template<class ...> class Ty, class ...Args>
        struct as_pack_impl<Ty<Args...>> {
            using type = kaixo::pack<Args...>;
        };
    }

    template<class Ty> // Convert templated type to pack
    using as_pack = move_types<Ty, kaixo::pack>;

    template<auto ...Values> // Template pack of values to pack
    using to_pack = kaixo::pack<value<Values>...>;

    // Call lambda with array values as template arguments, like
    // Lambda.operator()<Array[Is]...>();
    template<auto Array> 
    constexpr auto iterate = [](auto Lambda) {
        return[Lambda]<std::size_t ...Is>(std::index_sequence<Is...>) {
            return Lambda.operator() < Array[Is]... > ();
        }(std::make_index_sequence<Array.size()>{});
    };

    template<auto Array>
    using array_to_pack = decltype(iterate<Array>([]<auto ...Is>{ return to_pack<Is...>{}; }));

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                           *
     *                      Function Utils                       *
     *                                                           *
     *            Helper templates to make dealing with          *
     *               functions and member functions              *
     *                    a little bit easier                    *
     *                                                           *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#define KAIXO_MEMBER_CALL_C(MAC, V, REF, NOEXCEPT) \
MAC(     , V, REF, NOEXCEPT)                       \
MAC(const, V, REF, NOEXCEPT)

#define KAIXO_MEMBER_CALL_V(MAC, REF, NOEXCEPT)   \
KAIXO_MEMBER_CALL_C(MAC,         , REF, NOEXCEPT) \
KAIXO_MEMBER_CALL_C(MAC, volatile, REF, NOEXCEPT)

#define KAIXO_MEMBER_CALL_NOEXCEPT(MAC, NOEXCEPT) \
KAIXO_MEMBER_CALL_V(MAC,   , NOEXCEPT)            \
KAIXO_MEMBER_CALL_V(MAC,  &, NOEXCEPT)            \
KAIXO_MEMBER_CALL_V(MAC, &&, NOEXCEPT)

#define KAIXO_MEMBER_CALL(MAC)            \
KAIXO_MEMBER_CALL_NOEXCEPT(MAC,         ) \
KAIXO_MEMBER_CALL_NOEXCEPT(MAC, noexcept) 

    template<class Ty> concept is_functor = requires(decltype(&Ty::operator()) a) { a; };

    template<class> struct function_info_impl;
    template<is_functor Ty> struct function_info_impl<Ty>
        : function_info_impl<decltype(&Ty::operator())> {};

#define KAIXO_MEMBER_FUNCTION_INFO_MOD(CONST, VOLATILE, REF, NOEXCEPT)                   \
template<class Ty, class R, class ...Args>                                               \
struct function_info_impl<R(Ty::*)(Args...) CONST VOLATILE REF NOEXCEPT> {               \
    using pointer = info<R(*)(Args...) NOEXCEPT>;                                        \
    using signature = info<R(Args...) CONST VOLATILE REF NOEXCEPT>;                      \
    using object = info<CONST VOLATILE Ty REF>;                                          \
    using result = info<R>;                                                              \
    using arguments = pack_info<Args...>;                                                \
    constexpr static bool is_fun_const = std::same_as<const int, CONST int>;             \
    constexpr static bool is_fun_mutable = !is_fun_const;                                \
    constexpr static bool is_fun_volatile = std::same_as<volatile int, VOLATILE int>;    \
    constexpr static bool is_fun_lvalue_reference = std::same_as<int&, int REF>;         \
    constexpr static bool is_fun_rvalue_reference = std::same_as<int&&, int REF>;        \
    constexpr static bool is_fun_reference = std::is_reference_v<int REF>;               \
    constexpr static bool is_noexcept = std::same_as<void() noexcept, void() NOEXCEPT>;  \
    using add_fun_const = info<R(Ty::*)(Args...) const VOLATILE REF NOEXCEPT>;           \
    using remove_fun_const = info<R(Ty::*)(Args...) VOLATILE REF NOEXCEPT>;              \
    using add_fun_volatile = info<R(Ty::*)(Args...) CONST volatile REF NOEXCEPT>;        \
    using remove_fun_volatile = info<R(Ty::*)(Args...) CONST REF NOEXCEPT>;              \
    using add_fun_cv = info<R(Ty::*)(Args...) const volatile REF NOEXCEPT>;              \
    using remove_fun_cv = info<R(Ty::*)(Args...) REF NOEXCEPT>;                          \
    using add_fun_lvalue_reference = info<R(Ty::*)(Args...) CONST VOLATILE & NOEXCEPT>;  \
    using add_fun_rvalue_reference = info<R(Ty::*)(Args...) CONST VOLATILE && NOEXCEPT>; \
    using remove_fun_reference = info<R(Ty::*)(Args...) CONST VOLATILE NOEXCEPT>;        \
    using remove_fun_cvref = info<R(Ty::*)(Args...) NOEXCEPT>;                           \
    using add_noexcept = info<R(Ty::*)(Args...) CONST VOLATILE REF noexcept>;            \
    using remove_noexcept = info<R(Ty::*)(Args...) CONST VOLATILE REF>;                  \
};

        KAIXO_MEMBER_CALL(KAIXO_MEMBER_FUNCTION_INFO_MOD)
#undef KAIXO_MEMBER_FUNCTION_INFO_MOD

#define KAIXO_FUNCTION_INFO_MOD(CONST, VOLATILE, REF, NOEXCEPT)                         \
template<class R, class ...Args>                                                        \
struct function_info_impl<R(Args...) CONST VOLATILE REF NOEXCEPT> {                     \
    using pointer = info<R(*)(Args...) NOEXCEPT>;                                       \
    using signature = info<R(Args...) CONST VOLATILE REF NOEXCEPT>;                     \
    using result = info<R>;                                                             \
    using arguments = pack_info<Args...>;                                               \
    constexpr static bool is_fun_const = std::same_as<const int, CONST int>;            \
    constexpr static bool is_fun_mutable = !is_fun_const;                               \
    constexpr static bool is_fun_volatile = std::same_as<volatile int, VOLATILE int>;   \
    constexpr static bool is_fun_lvalue_reference = std::same_as<int&, int REF>;        \
    constexpr static bool is_fun_rvalue_reference = std::same_as<int&&, int REF>;       \
    constexpr static bool is_fun_reference = std::is_reference_v<int REF>;              \
    constexpr static bool is_noexcept = std::same_as<void() noexcept, void() NOEXCEPT>; \
    using add_fun_const = info<R(Args...) const VOLATILE REF NOEXCEPT>;                 \
    using remove_fun_const = info<R(Args...) VOLATILE REF NOEXCEPT>;                    \
    using add_fun_volatile = info<R(Args...) CONST volatile REF NOEXCEPT>;              \
    using remove_fun_volatile = info<R(Args...) CONST REF NOEXCEPT>;                    \
    using add_fun_cv = info<R(Args...) const volatile REF NOEXCEPT>;                    \
    using remove_fun_cv = info<R(Args...) REF NOEXCEPT>;                                \
    using add_fun_lvalue_reference = info<R(Args...) CONST VOLATILE & NOEXCEPT>;        \
    using add_fun_rvalue_reference = info<R(Args...) CONST VOLATILE && NOEXCEPT>;       \
    using remove_fun_reference = info<R(Args...) CONST VOLATILE NOEXCEPT>;              \
    using remove_fun_cvref = info<R(Args...) NOEXCEPT>;                                 \
    using add_noexcept = info<R(Args...) CONST VOLATILE REF noexcept>;                  \
    using remove_noexcept = info<R(Args...) CONST VOLATILE REF>;                        \
};

KAIXO_MEMBER_CALL(KAIXO_FUNCTION_INFO_MOD)
#undef KAIXO_FUNCTION_INFO_MOD

#define KAIXO_FUNCTION_PTR_INFO_MOD(NOEXCEPT)                                               \
template<class R, class ...Args>                                                            \
struct function_info_impl<R(*)(Args...) NOEXCEPT> {                                         \
    using pointer = info<R(*)(Args...) NOEXCEPT>;                                           \
    using signature = info<R(Args...) NOEXCEPT>;                                            \
    using result = info<R>;                                                                 \
    using arguments = pack_info<Args...>;                                                   \
    constexpr static bool is_noexcept = std::same_as<void() noexcept, void() NOEXCEPT>;     \
    using add_noexcept = info<R(Args...) noexcept>;                                         \
    using remove_noexcept = info<R(Args...)>;                                               \
};

KAIXO_FUNCTION_PTR_INFO_MOD( )
KAIXO_FUNCTION_PTR_INFO_MOD(noexcept)
#undef KAIXO_FUNCTION_PTR_INFO_MOD

    template<class Ty> using function_info = function_info_impl<std::remove_cv_t<std::remove_reference_t<Ty>>>;
    template<auto Ty> using function_info_v = function_info_impl<std::remove_cv_t<std::remove_reference_t<decltype(Ty)>>>;
    template<class Ty> concept callable_type = requires() { typename kaixo::function_info<Ty>::result; };

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *                                                           *
     *                        Type Utils                         *
     *                                                           *
     *            Template helpers to query type traits          *
     *                    a little bit easier                    *
     *                                                           *
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    // All type traits as concepts
    namespace type_concepts {
        template<class Ty> concept void_type = std::is_void_v<Ty>;
        template<class Ty> concept null_pointer = std::is_null_pointer_v<Ty>;
        template<class Ty> concept boolean = std::is_same_v<Ty, bool>;
        template<class Ty> concept integral = std::is_integral_v<Ty>;
        template<class Ty> concept floating_point = std::is_floating_point_v<Ty>;
        template<class Ty> concept array = std::is_array_v<Ty>;
        template<class Ty> concept enum_type = std::is_enum_v<Ty>;
        template<class Ty> concept union_type = std::is_union_v<Ty>;
        template<class Ty> concept class_type = std::is_class_v<Ty>;
        template<class Ty> concept function = std::is_function_v<Ty>;
        template<class Ty> concept pointer = std::is_pointer_v<Ty>;
        template<class Ty> concept lvalue_reference = std::is_lvalue_reference_v<Ty>;
        template<class Ty> concept rvalue_reference = std::is_rvalue_reference_v<Ty>;
        template<class Ty> concept member_object_pointer = std::is_member_object_pointer_v<Ty>;
        template<class Ty> concept member_function_pointer = std::is_member_function_pointer_v<Ty>;
        template<class Ty> concept fundamental = std::is_fundamental_v<Ty>;
        template<class Ty> concept arithmetic = std::is_arithmetic_v<Ty>;
        template<class Ty> concept scalar = std::is_scalar_v<Ty>;
        template<class Ty> concept object = std::is_object_v<Ty>;
        template<class Ty> concept compound = std::is_compound_v<Ty>;
        template<class Ty> concept reference = std::is_reference_v<Ty>;
        template<class Ty> concept member_pointer = std::is_member_pointer_v<Ty>;
        template<class Ty> concept const_type = std::is_const_v<Ty>;
        template<class Ty> concept volatile_type = std::is_volatile_v<Ty>;
        template<class Ty> concept trivial = std::is_trivial_v<Ty>;
        template<class Ty> concept trivially_copyable = std::is_trivially_copyable_v<Ty>;
        template<class Ty> concept standard_layout = std::is_standard_layout_v<Ty>;
        template<class Ty> concept pod = std::is_pod_v<Ty>;
        template<class Ty> concept unique_object_representations = std::has_unique_object_representations_v<Ty>;
        template<class Ty> concept empty = std::is_empty_v<Ty>;
        template<class Ty> concept polymorphic = std::is_polymorphic_v<Ty>;
        template<class Ty> concept abstract = std::is_abstract_v<Ty>;
        template<class Ty> concept final = std::is_final_v<Ty>;
        template<class Ty> concept aggregate = std::is_aggregate_v<Ty>;
        template<class Ty> concept signed_integral = std::is_signed_v<Ty>;
        template<class Ty> concept unsigned_integral = std::is_unsigned_v<Ty>;
        template<class Ty> concept bounded_array = std::is_bounded_array_v<Ty>;
        template<class Ty> concept unbounded_array = std::is_unbounded_array_v<Ty>;
        template<class Ty, class ...Args> concept constructible = std::is_constructible_v<Ty, Args...>;
        template<class Ty, class ...Args> concept trivially_constructible = std::is_trivially_constructible_v<Ty, Args...>;
        template<class Ty, class ...Args> concept nothrow_constructible = std::is_nothrow_constructible_v<Ty, Args...>;
        template<class Ty> concept default_constructible = std::is_default_constructible_v<Ty>;
        template<class Ty> concept trivially_default_constructible = std::is_trivially_default_constructible_v<Ty>;
        template<class Ty> concept nothrow_default_constructible = std::is_nothrow_default_constructible_v<Ty>;
        template<class Ty> concept copy_constructible = std::is_copy_constructible_v<Ty>;
        template<class Ty> concept trivially_copy_constructible = std::is_trivially_copy_constructible_v<Ty>;
        template<class Ty> concept nothrow_copy_constructible = std::is_nothrow_copy_constructible_v<Ty>;
        template<class Ty> concept move_constructible = std::is_move_constructible_v<Ty>;
        template<class Ty> concept trivially_move_constructible = std::is_trivially_move_constructible_v<Ty>;
        template<class Ty> concept nothrow_move_constructible = std::is_nothrow_move_constructible_v<Ty>;
        template<class Ty, class From> concept assignable = std::is_assignable_v<Ty, From>;
        template<class Ty, class From> concept trivially_assignable = std::is_trivially_assignable_v<Ty, From>;
        template<class Ty, class From> concept nothrow_assignable = std::is_nothrow_assignable_v<Ty, From>;
        template<class Ty> concept copy_assignable = std::is_copy_assignable_v<Ty>;
        template<class Ty> concept trivially_copy_assignable = std::is_trivially_copy_assignable_v<Ty>;
        template<class Ty> concept nothrow_copy_assignable = std::is_nothrow_copy_assignable_v<Ty>;
        template<class Ty> concept move_assignable = std::is_move_assignable_v<Ty>;
        template<class Ty> concept trivially_move_assignable = std::is_trivially_move_assignable_v<Ty>;
        template<class Ty> concept nothrow_move_assignable = std::is_nothrow_move_assignable_v<Ty>;
        template<class Ty> concept destructible = std::is_destructible_v<Ty>;
        template<class Ty> concept trivially_destructible = std::is_trivially_destructible_v<Ty>;
        template<class Ty> concept nothrow_destructible = std::is_nothrow_destructible_v<Ty>;
        template<class Ty> concept virtual_destructor = std::has_virtual_destructor_v<Ty>;
        template<class Ty, class Other> concept swappable_with = std::is_swappable_with_v<Ty, Other>;
        template<class Ty> concept swappable = std::is_swappable_v<Ty>;
        template<class Ty, class Other> concept nothrow_swappable_with = std::is_nothrow_swappable_with_v<Ty, Other>;
        template<class Ty> concept nothrow_swappable = std::is_nothrow_swappable_v<Ty>;
    }

    namespace detail {
        template<class From, class To>
        struct add_const_impl {
            using _unref = info<From>::remove_reference;
            using _to = info<To>;
            using _const = std::conditional_t<_unref::is_const, typename _to::add_const, _to>;
            using type = _const::type;
        };

        template<class From, class To>
        struct add_volatile_impl {
            using _unref = info<From>::remove_reference;
            using _to = info<To>;
            using _volatile = std::conditional_t<_unref::is_volatile, typename _to::add_volatile, _to>;
            using type = _volatile::type;
        };

        template<class From, class To>
        struct add_cv_impl {
            using type = add_volatile_impl<From, typename add_const_impl<From, To>::type>::type;
        };

        template<class From, class To>
        using copy_const_impl = add_const_impl<From, std::decay_t<To>>;
        template<class From, class To>
        using copy_volatile_impl = add_volatile_impl<From, std::decay_t<To>>;
        template<class From, class To>
        using copy_cv_impl = add_cv_impl<From, std::decay_t<To>>;

        template<class From, class To>
        struct add_ref_impl {
            using _info = info<From>;
            using _to = info<To>;
            using _lvalue = std::conditional_t<_info::is_lvalue_reference, typename _to::add_lvalue_reference, _to>;
            using _rvalue = std::conditional_t<_info::is_rvalue_reference, typename _lvalue::add_rvalue_reference, _lvalue>;
            using type = _rvalue::type;
        };

        template<class From, class To>
        using copy_ref_impl = add_ref_impl<From, std::decay_t<To>>;

        template<class From, class To>
        struct add_cvref_impl {
            using _cv = info<typename add_cv_impl<From, To>::type>;
            using _info = info<From>;
            using _lvalue = std::conditional_t<_info::is_lvalue_reference, typename _cv::add_lvalue_reference, _cv>;
            using _rvalue = std::conditional_t<_info::is_rvalue_reference, typename _lvalue::add_rvalue_reference, _lvalue>;
            using type = _rvalue::type;
        };

        template<class From, class To>
        using copy_cvref_impl = add_cvref_impl<From, std::decay_t<To>>;
    }

    template<class From, class To>
    using copy_const = typename detail::copy_const_impl<From, To>::type;
    template<class From, class To>
    using copy_volatile = typename detail::copy_volatile_impl<From, To>::type;
    template<class From, class To>
    using copy_cv = typename detail::copy_cv_impl<From, To>::type;
    template<class From, class To>
    using copy_ref = typename detail::copy_ref_impl<From, To>::type;
    template<class From, class To>
    using copy_cvref = typename detail::copy_cvref_impl<From, To>::type;
    template<class From, class To>
    using add_const = typename detail::add_const_impl<From, To>::type;
    template<class From, class To>
    using add_volatile = typename detail::add_volatile_impl<From, To>::type;
    template<class From, class To>
    using add_cv = typename detail::add_cv_impl<From, To>::type;
    template<class From, class To>
    using add_ref = typename detail::add_ref_impl<From, To>::type;
    template<class From, class To>
    using add_cvref = typename detail::add_cvref_impl<From, To>::type;

    template<class Ty> struct info_base {
        constexpr static bool is_void = type_concepts::void_type<Ty>;
        constexpr static bool is_null_pointer = type_concepts::null_pointer<Ty>;
        constexpr static bool is_integral = type_concepts::integral<Ty>;
        constexpr static bool is_floating_point = type_concepts::floating_point<Ty>;
        constexpr static bool is_array = type_concepts::array<Ty>;
        constexpr static bool is_enum = type_concepts::enum_type<Ty>;
        constexpr static bool is_union = type_concepts::union_type<Ty>;
        constexpr static bool is_class = type_concepts::class_type<Ty>;
        constexpr static bool is_function = type_concepts::function<Ty>;
        constexpr static bool is_pointer = type_concepts::pointer<Ty>;
        constexpr static bool is_lvalue_reference = type_concepts::lvalue_reference<Ty>;
        constexpr static bool is_rvalue_reference = type_concepts::rvalue_reference<Ty>;
        constexpr static bool is_member_object_pointer = type_concepts::member_object_pointer<Ty>;
        constexpr static bool is_member_function_pointer = type_concepts::member_function_pointer<Ty>;
        constexpr static bool is_fundamental = type_concepts::fundamental<Ty>;
        constexpr static bool is_arithmetic = type_concepts::arithmetic<Ty>;
        constexpr static bool is_scalar = type_concepts::scalar<Ty>;
        constexpr static bool is_object = type_concepts::object<Ty>;
        constexpr static bool is_compound = type_concepts::compound<Ty>;
        constexpr static bool is_reference = type_concepts::reference<Ty>;
        constexpr static bool is_member_pointer = type_concepts::member_pointer<Ty>;
        constexpr static bool is_const = type_concepts::const_type<Ty>;
        constexpr static bool is_volatile = type_concepts::volatile_type<Ty>;
        constexpr static bool is_trivial = type_concepts::trivial<Ty>;
        constexpr static bool is_trivially_copyable = type_concepts::trivially_copyable<Ty>;
        constexpr static bool is_standard_layout = type_concepts::standard_layout<Ty>;
        constexpr static bool is_pod = type_concepts::pod<Ty>;
        constexpr static bool has_unique_object_representations = type_concepts::unique_object_representations<Ty>;
        constexpr static bool is_empty = type_concepts::empty<Ty>;
        constexpr static bool is_polymorphic = type_concepts::polymorphic<Ty>;
        constexpr static bool is_abstract = type_concepts::abstract<Ty>;
        constexpr static bool is_final = type_concepts::final<Ty>;
        constexpr static bool is_aggregate = type_concepts::aggregate<Ty>;
        constexpr static bool is_signed_integral = type_concepts::signed_integral<Ty>;
        constexpr static bool is_unsigned_integral = type_concepts::unsigned_integral<Ty>;
        constexpr static bool is_bounded_array = type_concepts::bounded_array<Ty>;
        constexpr static bool is_unbounded_array = type_concepts::unbounded_array<Ty>;
        template<class ...Args> constexpr static bool is_constructible = type_concepts::constructible<Ty, Args...>;
        template<class ...Args> constexpr static bool is_trivially_constructible = type_concepts::trivially_constructible<Ty, Args...>;
        template<class ...Args> constexpr static bool is_nothrow_constructible = type_concepts::nothrow_constructible<Ty, Args...>;
        constexpr static bool is_default_constructible = type_concepts::default_constructible<Ty>;
        constexpr static bool is_trivially_default_constructible = type_concepts::trivially_default_constructible<Ty>;
        constexpr static bool is_nothrow_default_constructible = type_concepts::nothrow_default_constructible<Ty>;
        constexpr static bool is_copy_constructible = type_concepts::copy_constructible<Ty>;
        constexpr static bool is_trivially_copy_constructible = type_concepts::trivially_copy_constructible<Ty>;
        constexpr static bool is_nothrow_copy_constructible = type_concepts::nothrow_copy_constructible<Ty>;
        constexpr static bool is_move_constructible = type_concepts::move_constructible<Ty>;
        constexpr static bool is_trivially_move_constructible = type_concepts::trivially_move_constructible<Ty>;
        constexpr static bool is_nothrow_move_constructible = type_concepts::nothrow_move_constructible<Ty>;
        template<class From> constexpr static bool is_assignable = type_concepts::assignable<Ty, From>;
        template<class From> constexpr static bool is_trivially_assignable = type_concepts::trivially_assignable<Ty, From>;
        template<class From> constexpr static bool is_nothrow_assignable = type_concepts::nothrow_assignable<Ty, From>;
        constexpr static bool is_copy_assignable = type_concepts::copy_assignable<Ty>;
        constexpr static bool is_trivially_copy_assignable = type_concepts::trivially_copy_assignable<Ty>;
        constexpr static bool is_nothrow_copy_assignable = type_concepts::nothrow_copy_assignable<Ty>;
        constexpr static bool is_move_assignable = type_concepts::move_assignable<Ty>;
        constexpr static bool is_trivially_move_assignable = type_concepts::trivially_move_assignable<Ty>;
        constexpr static bool is_nothrow_move_assignable = type_concepts::nothrow_move_assignable<Ty>;
        constexpr static bool is_destructible = type_concepts::destructible<Ty>;
        constexpr static bool is_trivially_destructible = type_concepts::trivially_destructible<Ty>;
        constexpr static bool is_nothrow_destructible = type_concepts::nothrow_destructible<Ty>;
        constexpr static bool has_virtual_destructor = type_concepts::virtual_destructor<Ty>;
        template<class Other> constexpr static bool is_swappable_with = type_concepts::swappable_with<Ty, Other>;
        constexpr static bool is_swappable = type_concepts::swappable<Ty>;
        template<class Other> constexpr static bool is_nothrow_swappable_with = type_concepts::nothrow_swappable_with<Ty, Other>;
        constexpr static bool is_nothrow_swappable = type_concepts::nothrow_swappable<Ty>;

        template<class Other> constexpr static bool same_as = std::same_as<Ty, Other>;
        template<class Other> constexpr static bool base_of = std::is_base_of_v<Ty, Other>;
        template<class Other> constexpr static bool convertible_to = std::is_convertible_v<Ty, Other>;
        template<class Other> constexpr static bool nothrow_convertible_to = std::is_nothrow_convertible_v<Ty, Other>;
        template<class ...Args> constexpr static bool invocable = std::invocable<Ty, Args...>;
        template<class ...Args> constexpr static bool nothrow_invocable = std::is_nothrow_invocable_v<Ty, Args...>;

        constexpr static std::size_t bytes = sizeof_v<Ty>;
        constexpr static std::size_t alignment = alignof_v<Ty>;

        template<class To>
        using copy_const_to = info<kaixo::copy_const<Ty, To>>;
        template<class To>
        using copy_volatile_to = info<kaixo::copy_volatile<Ty, To>>;
        template<class To>
        using copy_cv_to = info<kaixo::copy_cv<Ty, To>>;
        template<class To>
        using copy_ref_to = info<kaixo::copy_ref<Ty, To>>;
        template<class To>
        using copy_cvref_to = info<kaixo::copy_cvref<Ty, To>>;
        template<class To>
        using add_const_to = info<kaixo::add_const<Ty, To>>;
        template<class To>
        using add_volatile_to = info<kaixo::add_volatile<Ty, To>>;
        template<class To>
        using add_cv_to = info<kaixo::add_cv<Ty, To>>;
        template<class To>
        using add_ref_to = info<kaixo::add_ref<Ty, To>>;
        template<class To>
        using add_cvref_to = info<kaixo::add_cvref<Ty, To>>;

        template<class From>
        using copy_const_from = info<kaixo::copy_const<From, Ty>>;
        template<class From>
        using copy_volatile_from = info<kaixo::copy_volatile<From, Ty>>;
        template<class From>
        using copy_cv_from = info<kaixo::copy_cv<From, Ty>>;
        template<class From>
        using copy_ref_from = info<kaixo::copy_ref<From, Ty>>;
        template<class From>
        using copy_cvref_from = info<kaixo::copy_cvref<From, Ty>>;
        template<class From>
        using add_const_from = info<kaixo::add_const<From, Ty>>;
        template<class From>
        using add_volatile_from = info<kaixo::add_volatile<From, Ty>>;
        template<class From>
        using add_cv_from = info<kaixo::add_cv<From, Ty>>;
        template<class From>
        using add_ref_from = info<kaixo::add_ref<From, Ty>>;
        template<class From>
        using add_cvref_from = info<kaixo::add_cvref<From, Ty>>;

        using decay = info<std::decay_t<Ty>>;
        using remove_cv = info<std::remove_cv_t<Ty>>;
        using remove_const = info<std::remove_const_t<Ty>>;
        using remove_volatile = info<std::remove_volatile_t<Ty>>;
        using add_cv = info<std::add_cv_t<Ty>>;
        using add_const = info<std::add_const_t<Ty>>;
        using add_volatile = info<std::add_volatile_t<Ty>>;
        using remove_reference = info<std::remove_reference_t<Ty>>;
        using remove_cvref = info<std::remove_cvref_t<Ty>>;
        using add_lvalue_reference = info<std::add_lvalue_reference_t<Ty>>;
        using add_rvalue_reference = info<std::add_rvalue_reference_t<Ty>>;
        using remove_pointer = info<std::remove_pointer_t<Ty>>;
        using add_pointer = info<std::add_pointer_t<Ty>>;
        using type = Ty;
    };

    template<class ...Tys> struct info_base<pack<Tys...>> : pack<Tys...> {
        constexpr static bool is_null_pointer = (type_concepts::null_pointer<Tys> && ...);
        constexpr static bool is_integral = (type_concepts::integral<Tys> && ...);
        constexpr static bool is_floating_point = (type_concepts::floating_point<Tys> && ...);
        constexpr static bool is_array = (type_concepts::array<Tys> && ...);
        constexpr static bool is_enum_type = (type_concepts::enum_type<Tys> && ...);
        constexpr static bool is_union_type = (type_concepts::union_type<Tys> && ...);
        constexpr static bool is_class_type = (type_concepts::class_type<Tys> && ...);
        constexpr static bool is_function = (type_concepts::function<Tys> && ...);
        constexpr static bool is_pointer = (type_concepts::pointer<Tys> && ...);
        constexpr static bool is_lvalue_reference = (type_concepts::lvalue_reference<Tys> && ...);
        constexpr static bool is_rvalue_reference = (type_concepts::rvalue_reference<Tys> && ...);
        constexpr static bool is_member_object_pointer = (type_concepts::member_object_pointer<Tys> && ...);
        constexpr static bool is_member_function_pointer = (type_concepts::member_function_pointer<Tys> && ...);
        constexpr static bool is_fundamental = (type_concepts::fundamental<Tys> && ...);
        constexpr static bool is_arithmetic = (type_concepts::arithmetic<Tys> && ...);
        constexpr static bool is_scalar = (type_concepts::scalar<Tys> && ...);
        constexpr static bool is_object = (type_concepts::object<Tys> && ...);
        constexpr static bool is_compound = (type_concepts::compound<Tys> && ...);
        constexpr static bool is_reference = (type_concepts::reference<Tys> && ...);
        constexpr static bool is_member_pointer = (type_concepts::member_pointer<Tys> && ...);
        constexpr static bool is_const_type = (type_concepts::const_type<Tys> && ...);
        constexpr static bool is_volatile_type = (type_concepts::volatile_type<Tys> && ...);
        constexpr static bool is_trivial = (type_concepts::trivial<Tys> && ...);
        constexpr static bool is_trivially_copyable = (type_concepts::trivially_copyable<Tys> && ...);
        constexpr static bool is_standard_layout = (type_concepts::standard_layout<Tys> && ...);
        constexpr static bool is_pod = (type_concepts::pod<Tys> && ...);
        constexpr static bool has_unique_object_representations = (type_concepts::unique_object_representations<Tys> && ...);
        constexpr static bool is_empty = (type_concepts::empty<Tys> && ...);
        constexpr static bool is_polymorphic = (type_concepts::polymorphic<Tys> && ...);
        constexpr static bool is_abstract = (type_concepts::abstract<Tys> && ...);
        constexpr static bool is_final = (type_concepts::final<Tys> && ...);
        constexpr static bool is_aggregate = (type_concepts::aggregate<Tys> && ...);
        constexpr static bool is_signed_integral = (type_concepts::signed_integral<Tys> && ...);
        constexpr static bool is_unsigned_integral = (type_concepts::unsigned_integral<Tys> && ...);
        constexpr static bool is_bounded_array = (type_concepts::bounded_array<Tys> && ...);
        constexpr static bool is_unbounded_array = (type_concepts::unbounded_array<Tys> && ...);
        template<class ...Args> constexpr static bool is_constructible = (type_concepts::constructible<Tys, Args...> && ...);
        template<class ...Args> constexpr static bool is_trivially_constructible = (type_concepts::trivially_constructible<Tys, Args...> && ...);
        template<class ...Args> constexpr static bool is_nothrow_constructible = (type_concepts::nothrow_constructible<Tys, Args...> && ...);
        constexpr static bool is_default_constructible = (type_concepts::default_constructible<Tys> && ...);
        constexpr static bool is_trivially_default_constructible = (type_concepts::trivially_default_constructible<Tys> && ...);
        constexpr static bool is_nothrow_default_constructible = (type_concepts::nothrow_default_constructible<Tys> && ...);
        constexpr static bool is_copy_constructible = (type_concepts::copy_constructible<Tys> && ...);
        constexpr static bool is_trivially_copy_constructible = (type_concepts::trivially_copy_constructible<Tys> && ...);
        constexpr static bool is_nothrow_copy_constructible = (type_concepts::nothrow_copy_constructible<Tys> && ...);
        constexpr static bool is_move_constructible = (type_concepts::move_constructible<Tys> && ...);
        constexpr static bool is_trivially_move_constructible = (type_concepts::trivially_move_constructible<Tys> && ...);
        constexpr static bool is_nothrow_move_constructible = (type_concepts::nothrow_move_constructible<Tys> && ...);
        template<class From> constexpr static bool is_assignable = (type_concepts::assignable<Tys, From> && ...);
        template<class From> constexpr static bool is_trivially_assignable = (type_concepts::trivially_assignable<Tys, From> && ...);
        template<class From> constexpr static bool is_nothrow_assignable = (type_concepts::nothrow_assignable<Tys, From> && ...);
        constexpr static bool is_copy_assignable = (type_concepts::copy_assignable<Tys> && ...);
        constexpr static bool is_trivially_copy_assignable = (type_concepts::trivially_copy_assignable<Tys> && ...);
        constexpr static bool is_nothrow_copy_assignable = (type_concepts::nothrow_copy_assignable<Tys> && ...);
        constexpr static bool is_move_assignable = (type_concepts::move_assignable<Tys> && ...);
        constexpr static bool is_trivially_move_assignable = (type_concepts::trivially_move_assignable<Tys> && ...);
        constexpr static bool is_nothrow_move_assignable = (type_concepts::nothrow_move_assignable<Tys> && ...);
        constexpr static bool is_destructible = (type_concepts::destructible<Tys> && ...);
        constexpr static bool is_trivially_destructible = (type_concepts::trivially_destructible<Tys> && ...);
        constexpr static bool is_nothrow_destructible = (type_concepts::nothrow_destructible<Tys> && ...);
        constexpr static bool has_virtual_destructor = (type_concepts::virtual_destructor<Tys> && ...);
        template<class Other> constexpr static bool is_swappable_with = (type_concepts::swappable_with<Tys, Other> && ...);
        constexpr static bool is_swappable = (type_concepts::swappable<Tys> && ...);
        template<class Other> constexpr static bool is_nothrow_swappable_with = (type_concepts::nothrow_swappable_with<Tys, Other> && ...);
        constexpr static bool is_nothrow_swappable = (type_concepts::nothrow_swappable<Tys> && ...);

        template<class Other> constexpr static bool same_as = (std::same_as<Tys, Other> && ...);
        template<class Other> constexpr static bool base_of = (std::is_base_of_v<Tys, Other> && ...);
        template<class Other> constexpr static bool convertible_to = (std::is_convertible_v<Tys, Other> && ...);
        template<class Other> constexpr static bool nothrow_convertible_to = (std::is_nothrow_convertible_v<Tys, Other> && ...);
        template<class ...Args> constexpr static bool invocable = (std::invocable<Tys, Args...> && ...);
        template<class ...Args> constexpr static bool nothrow_invocable = (std::is_nothrow_invocable_v<Tys, Args...> && ...);

        constexpr static std::size_t bytes = (sizeof_v<Tys> + ...);
        constexpr static std::size_t alignment = std::max({ alignof_v<Tys>... });

        template<class From>
        using copy_const_from = info<kaixo::pack<kaixo::copy_const<From, Tys>...>>;
        template<class From>
        using copy_volatile_from = info<kaixo::pack<kaixo::copy_volatile<From, Tys>...>>;
        template<class From>
        using copy_cv_from = info<kaixo::pack<kaixo::copy_cv<From, Tys>...>>;
        template<class From>
        using copy_ref_from = info<kaixo::pack<kaixo::copy_ref<From, Tys>...>>;
        template<class From>
        using copy_cvref_from = info<kaixo::pack<kaixo::copy_cvref<From, Tys>...>>;
        template<class From>
        using add_const_from = info<kaixo::pack<kaixo::add_const<From, Tys>...>>;
        template<class From>
        using add_volatile_from = info<kaixo::pack<kaixo::add_volatile<From, Tys>...>>;
        template<class From>
        using add_cv_from = info<kaixo::pack<kaixo::add_cv<From, Tys>...>>;
        template<class From>
        using add_ref_from = info<kaixo::pack<kaixo::add_ref<From, Tys>...>>;
        template<class From>
        using add_cvref_from = info<kaixo::pack<kaixo::add_cvref<From, Tys>...>>;

        using decay = info<kaixo::pack<std::decay_t<Tys>...>>;
        using remove_cv = info<kaixo::pack<std::remove_cv_t<Tys>...>>;
        using remove_const = info<kaixo::pack<std::remove_const_t<Tys>...>>;
        using remove_volatile = info<kaixo::pack<std::remove_volatile_t<Tys>...>>;
        using add_cv = info<kaixo::pack<std::add_cv_t<Tys>...>>;
        using add_const = info<kaixo::pack<std::add_const_t<Tys>...>>;
        using add_volatile = info<kaixo::pack<std::add_volatile_t<Tys>...>>;
        using remove_reference = info<kaixo::pack<std::remove_reference_t<Tys>...>>;
        using remove_cvref = info<kaixo::pack<std::remove_cvref_t<Tys>...>>;
        using add_lvalue_reference = info<kaixo::pack<std::add_lvalue_reference_t<Tys>...>>;
        using add_rvalue_reference = info<kaixo::pack<std::add_rvalue_reference_t<Tys>...>>;
        using remove_pointer = info<kaixo::pack<std::remove_pointer_t<Tys>...>>;
        using add_pointer = info<kaixo::pack<std::add_pointer_t<Tys>...>>;
        using type = kaixo::pack<Tys...>;
    };

    template<callable_type Ty>
    struct info<Ty> : info_base<Ty>, function_info<Ty> {
    };

    template<class Ty>
        requires (type_concepts::integral<Ty> && !type_concepts::boolean<Ty>)
    struct info<Ty> : info_base<Ty> {
        using make_signed = info<std::make_signed<Ty>>;
        using make_unsigned = info<std::make_unsigned<Ty>>;
    };

    template<class Ty>
        requires (type_concepts::enum_type<Ty>)
    struct info<Ty> : info_base<Ty> {
        using underlying = std::underlying_type_t<Ty>;

        template<underlying Value>
        constexpr static auto name = enum_name<Ty, Value>;

        template<underlying Value>
        constexpr static auto defined = name<Value>.data() != nullptr;
    };
    
    template<class Ty, class Obj>
        requires type_concepts::member_object_pointer<Ty Obj::*>
    struct info<Ty Obj::*> : info_base<Ty> {
        using object = info<Obj>;
    };

    template<class Ty>
        requires (type_concepts::array<Ty>)
    struct info<Ty> : info_base<Ty> {
        constexpr static std::size_t rank = std::rank_v<Ty>;

        template<std::size_t Dim = 0>
        constexpr static std::size_t extent = std::extent_v<Ty, Dim>;

        using remove_extent = info<std::remove_extent_t<Ty>>;
        using remove_all_extents = info<std::remove_all_extents_t<Ty>>;
    };

    template<class ...Tys>
    struct pack_info : info_base<pack<Tys...>> {};
}