﻿
// ------------------------------------------------

#include <iostream>
#include <string>
#include <source_location>
#include <vector>
#include <map>
#include <ranges>
#include <algorithm>
#include <utility>

// ------------------------------------------------

#include "pack_utils/pack_utils.hpp"

// ------------------------------------------------

namespace kaixo {

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
            { std::decay_t<Ty>::is_const } -> std::convertible_to<bool>;
            { std::decay_t<Ty>::is_reference } -> std::convertible_to<bool>;
            { std::decay_t<Ty>::size } -> std::convertible_to<std::size_t>;
        };
        
        // ------------------------------------------------
        
        template<class Ty>
        concept owner_view = view<Ty> && !Ty::is_reference;
        
        template<class Ty>
        concept const_view = view<Ty> && Ty::is_const;
        
        // ------------------------------------------------

        template<class Ty>
        concept pipe = std::derived_from<std::decay_t<Ty>, pipe_interface<std::decay_t<Ty>>>;
        
        template<class Pipe, class Type>
        concept pipe_for = pipe<Pipe> && std::invocable<Pipe, Type>;

        // ------------------------------------------------

        template<class Ty>
        concept tuple_like = requires() {
            std::tuple_size_v<std::decay_t<Ty>> == 0; // Either empty 
        } || requires(Ty val) {
            { std::tuple_size_v<std::decay_t<Ty>> };  // Or some size + std::get overload
            { std::get<0>(val) } -> std::convertible_to<std::tuple_element_t<0, std::decay_t<Ty>>>;
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
            using _const = std::conditional_t<const_view<Self>, const _element, _element>;
            using type = std::conditional_t<owner_view<Self>, _const&&, _const&>;
        };

        // Return-type of get<I> for Self
        template<view Self, std::size_t I>
        using get_type_t = typename get_type<Self, I>::type;

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
            
            constexpr static bool is_const = std::is_const_v<Tpl>;
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
            
            static_assert(!std::is_const_v<std::remove_reference_t<Tpl>>, 
                "Owning view may not own a const tuple_like");

            // ------------------------------------------------

            constexpr owning_view(Tpl&& tpl)
                : m_Tuple(std::move(tpl)) 
            {}
            
            // ------------------------------------------------

            constexpr static bool is_const = false;
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

            constexpr static bool is_const = false;
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

                template<tuple_like Tpl>
                    requires (I <= all_t<Tpl>::size)
                constexpr std::tuple_element_t<I, Tpl>&& operator()(Tpl& tuple) const {
                    return static_cast<std::tuple_element_t<I, Tpl>&&>(std::get<I>(tuple));
                }
                
                template<tuple_like Tpl>
                    requires (I <= all_t<Tpl>::size)
                constexpr std::tuple_element_t<I, Tpl>&& operator()(Tpl&& tuple) const {
                    return static_cast<std::tuple_element_t<I, Tpl>&&>(std::get<I>(std::forward<Tpl>(tuple)));
                }

            };

            // Perfect forward the Ith element
            template<std::size_t I>
            constexpr _forward_fun<I> forward{};

            // ------------------------------------------------

        }

        // ------------------------------------------------
        
        template<std::size_t I, view View>
        struct take_view : view_interface<take_view<I, View>> {

            // ------------------------------------------------

            View view;

            // ------------------------------------------------

            constexpr static bool is_const = View::is_const;
            constexpr static bool is_reference = View::is_reference;
            constexpr static std::size_t size = I;

            // ------------------------------------------------

            template<std::size_t I>
                requires (I < size)
            using element = typename View::template element<I>;

            // ------------------------------------------------

            template<std::size_t N, class Self>
                requires (N < size)
            constexpr get_type_t<Self, N> get(this Self&& self) {
                return std::forward<Self>(self).view.template get<N>();
            }

            // ------------------------------------------------

        };

        // ------------------------------------------------

        template<std::size_t I, view View>
        struct drop_view : view_interface<drop_view<I, View>> {

            // ------------------------------------------------

            View view;

            // ------------------------------------------------

            constexpr static bool is_const = View::is_const;
            constexpr static bool is_reference = View::is_reference;
            constexpr static std::size_t size = View::size - I;

            // ------------------------------------------------

            template<std::size_t N>
                requires (N < size)
            using element = typename View::template element<N + I>;

            // ------------------------------------------------

            template<std::size_t N, class Self>
                requires (N < size)
            constexpr get_type_t<Self, N> get(this Self&& self) {
                return std::forward<Self>(self).view.template get<N + I>();
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
                        return take_view<I, all_t<Tpl>>{ .view = all(std::forward<Tpl>(tuple)) };
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
                        return drop_view<all_t<Tpl>::size - I, all_t<Tpl>>{ .view = all(std::forward<Tpl>(tuple)) };
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
                        return take_view<_take, _view>{.view = all(std::forward<Tpl>(tuple)) };
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
                    constexpr std::size_t _drop = pack_size<_pack>::value - pack_size<_taken>::value;

                    if constexpr (pack_size<_taken>::value == 0) {
                        return empty_view{};
                    } else {
                        return drop_view<_drop, _view>{.view = all(std::forward<Tpl>(tuple)) };
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
                        return drop_view<I, all_t<Tpl>>{ .view = all(std::forward<Tpl>(tuple)) };
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
                        return take_view<all_t<Tpl>::size - I, all_t<Tpl>>{ .view = all(std::forward<Tpl>(tuple)) };
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
                        return drop_view<_drop, _view>{ .view = all(std::forward<Tpl>(tuple)) };
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
                        return take_view<_take, _view>{ .view = all(std::forward<Tpl>(tuple)) };
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

        }

        // ------------------------------------------------
        
        template<view View, template<class> class Indices>
        struct unique_view : view_interface<unique_view<View, Indices>> {

            // ------------------------------------------------
            
            using _pack = typename as_pack<View>::type;
            using _unique = typename pack_at_indices<typename Indices<_pack>::type, _pack>::type;

            // ------------------------------------------------

            View view;

            // ------------------------------------------------

            constexpr static bool is_const = View::is_const;
            constexpr static bool is_reference = View::is_reference;
            constexpr static std::size_t size = pack_size<_unique>::value;

            // ------------------------------------------------

            template<std::size_t N>
                requires (N < size)
            using element = typename pack_element<N, _unique>::type;

            // ------------------------------------------------

            template<std::size_t N, class Self>
                requires (N < size)
            constexpr get_type_t<Self, N> get(this Self&& self) {
                using _indices = typename Indices<_pack>::type;
                constexpr std::size_t _index = pack_indices_element<N, _indices>::value;
                return std::forward<Self>(self).view.template get<_index>();
            }

            // ------------------------------------------------

        };

        // ------------------------------------------------

        namespace views {

            // ------------------------------------------------

            template<template<class> class Indices>
            struct _unique_fun : pipe_interface<_unique_fun<Indices>> {

                template<tuple_like Tpl>
                constexpr auto operator()(Tpl&& tuple) const {
                    if constexpr (std::tuple_size_v<std::decay_t<Tpl>> == 0) {
                        return empty_view{};
                    } else {
                        return unique_view<all_t<Tpl>, Indices>{ .view = all(std::forward<Tpl>(tuple)) };
                    }
                }

            };

            // First unique occurence of each type
            constexpr _unique_fun<pack_first_indices> unique{};

            // First unique occurence of each type
            constexpr _unique_fun<pack_first_indices> first_unique{};

            // Last unique occurence of each type
            constexpr _unique_fun<pack_last_indices> last_unique{};

            namespace detail {
                template<std::size_t N>
                struct _pack_nth_indices_last_unique {
                    template<class Pack>
                    using type = pack_nth_indices<N, Pack>;
                };
            }

            // Nth unique occurence of each type
            template<std::size_t N>
            constexpr _unique_fun<typename detail::_pack_nth_indices_last_unique<N>::type> nth_unique{};

            // ------------------------------------------------

        }

        // ------------------------------------------------

    }

    // ------------------------------------------------

}

namespace kaixo::tuples::views {

    // Check that std::get gives same cv-ref qualifiers on view
    template<class Tuple>
    constexpr bool compare_get = std::same_as<
        decltype(std::get<0>(std::declval<all_t<Tuple>>() | take<1>)),
        decltype(std::get<0>(std::declval<Tuple>()))>;

    static_assert(compare_get<std::tuple<int>>);                decltype(std::get<0>(std::declval<all_t<std::tuple<int>>>() | take<1>));
    static_assert(compare_get<std::tuple<int&>>);               decltype(std::get<0>(std::declval<all_t<std::tuple<int&>>>() | take<1>));
    static_assert(compare_get<std::tuple<int&&>>);              decltype(std::get<0>(std::declval<all_t<std::tuple<int&&>>>() | take<1>));
    static_assert(compare_get<std::tuple<int>&>);               decltype(std::get<0>(std::declval<all_t<std::tuple<int>&>>() | take<1>));
    static_assert(compare_get<std::tuple<int&>&>);              decltype(std::get<0>(std::declval<all_t<std::tuple<int&>&>>() | take<1>));
    static_assert(compare_get<std::tuple<int&&>&>);             decltype(std::get<0>(std::declval<all_t<std::tuple<int&&>&>>() | take<1>));
    static_assert(compare_get<std::tuple<int>&&>);              decltype(std::get<0>(std::declval<all_t<std::tuple<int>&&>>() | take<1>));
    static_assert(compare_get<std::tuple<int&>&&>);             decltype(std::get<0>(std::declval<all_t<std::tuple<int&>&&>>() | take<1>));
    static_assert(compare_get<std::tuple<int&&>&&>);            decltype(std::get<0>(std::declval<all_t<std::tuple<int&&>&&>>() | take<1>));
    static_assert(compare_get<std::tuple<const int>>);          decltype(std::get<0>(std::declval<all_t<std::tuple<const int>>>() | take<1>));
    static_assert(compare_get<std::tuple<const int&>>);         decltype(std::get<0>(std::declval<all_t<std::tuple<const int&>>>() | take<1>));
    static_assert(compare_get<std::tuple<const int&&>>);        decltype(std::get<0>(std::declval<all_t<std::tuple<const int&&>>>() | take<1>));
    static_assert(compare_get<std::tuple<const int>&>);         decltype(std::get<0>(std::declval<all_t<std::tuple<const int>&>>() | take<1>));
    static_assert(compare_get<std::tuple<const int&>&>);        decltype(std::get<0>(std::declval<all_t<std::tuple<const int&>&>>() | take<1>));
    static_assert(compare_get<std::tuple<const int&&>&>);       decltype(std::get<0>(std::declval<all_t<std::tuple<const int&&>&>>() | take<1>));
    static_assert(compare_get<std::tuple<const int>&&>);        decltype(std::get<0>(std::declval<all_t<std::tuple<const int>&&>>() | take<1>));
    static_assert(compare_get<std::tuple<const int&>&&>);       decltype(std::get<0>(std::declval<all_t<std::tuple<const int&>&&>>() | take<1>));
    static_assert(compare_get<std::tuple<const int&&>&&>);      decltype(std::get<0>(std::declval<all_t<std::tuple<const int&&>&&>>() | take<1>));
    static_assert(compare_get<const std::tuple<int>&>);         decltype(std::get<0>(std::declval<all_t<const std::tuple<int>&>>() | take<1>));
    static_assert(compare_get<const std::tuple<int&>&>);        decltype(std::get<0>(std::declval<all_t<const std::tuple<int&>&>>() | take<1>));
    static_assert(compare_get<const std::tuple<int&&>&>);       decltype(std::get<0>(std::declval<all_t<const std::tuple<int&&>&>>() | take<1>));
    static_assert(compare_get<const std::tuple<const int>&>);   decltype(std::get<0>(std::declval<all_t<const std::tuple<const int>&>>() | take<1>));
    static_assert(compare_get<const std::tuple<const int&>&>);  decltype(std::get<0>(std::declval<all_t<const std::tuple<const int&>&>>() | take<1>));
    static_assert(compare_get<const std::tuple<const int&&>&>); decltype(std::get<0>(std::declval<all_t<const std::tuple<const int&&>&>>() | take<1>));

    template<class Tuple>
    using forwarded = decltype(std::declval<Tuple>() | forward<0>);

    static_assert(std::same_as<forwarded<std::tuple<int>>, int&&>);
    static_assert(std::same_as<forwarded<std::tuple<int>&>, int&&>);
    static_assert(std::same_as<forwarded<std::tuple<int>&&>, int&&>);
    static_assert(std::same_as<forwarded<std::tuple<int&>>, int&>);
    static_assert(std::same_as<forwarded<std::tuple<int&>&>, int&>);
    static_assert(std::same_as<forwarded<std::tuple<int&>&&>, int&>);
    static_assert(std::same_as<forwarded<std::tuple<int&&>>, int&&>);
    static_assert(std::same_as<forwarded<std::tuple<int&&>&>, int&&>);
    static_assert(std::same_as<forwarded<std::tuple<int&&>&&>, int&&>);
    static_assert(std::same_as<forwarded<std::tuple<const int>>, const int&&>);
    static_assert(std::same_as<forwarded<std::tuple<const int>&>, const int&&>);
    static_assert(std::same_as<forwarded<std::tuple<const int>&&>, const int&&>);
    static_assert(std::same_as<forwarded<std::tuple<const int&>>, const int&>);
    static_assert(std::same_as<forwarded<std::tuple<const int&>&>, const int&>);
    static_assert(std::same_as<forwarded<std::tuple<const int&>&&>, const int&>);
    static_assert(std::same_as<forwarded<std::tuple<const int&&>>, const int&&>);
    static_assert(std::same_as<forwarded<std::tuple<const int&&>&>, const int&&>);
    static_assert(std::same_as<forwarded<std::tuple<const int&&>&&>, const int&&>);
    static_assert(std::same_as<forwarded<const std::tuple<int>>, const int&&>);
    static_assert(std::same_as<forwarded<const std::tuple<int>&>, const int&&>);
    static_assert(std::same_as<forwarded<const std::tuple<int&>>, int&>);
    static_assert(std::same_as<forwarded<const std::tuple<int&>&>, int&>);
    static_assert(std::same_as<forwarded<const std::tuple<int&&>>, int&&>);
    static_assert(std::same_as<forwarded<const std::tuple<int&&>&>, int&&>);
    static_assert(std::same_as<forwarded<const std::tuple<const int>>, const int&&>);
    static_assert(std::same_as<forwarded<const std::tuple<const int>&>, const int&&>);
    static_assert(std::same_as<forwarded<const std::tuple<const int&>>, const int&>);
    static_assert(std::same_as<forwarded<const std::tuple<const int&>&>, const int&>);
    static_assert(std::same_as<forwarded<const std::tuple<const int&&>>, const int&&>);
    static_assert(std::same_as<forwarded<const std::tuple<const int&&>&>, const int&&>);

    constexpr std::tuple<int, double, float> value{};
    constexpr std::tuple<> empty{};

    static_assert(std::same_as<as_pack_t<decltype(value | drop<0>)>, pack<int, double, float>>);
    static_assert(std::same_as<as_pack_t<decltype(value | drop<1>)>, pack<double, float>>);
    static_assert(std::same_as<as_pack_t<decltype(value | drop<3>)>, pack<>>);
    static_assert(std::same_as<as_pack_t<decltype(empty | drop<0>)>, pack<>>);

    static_assert(std::same_as<as_pack_t<decltype(value | drop_while<std::is_integral>)>, pack<double, float>>);
    static_assert(std::same_as<as_pack_t<decltype(value | drop_while<std::is_floating_point>)>, pack<int, double, float>>);
    static_assert(std::same_as<as_pack_t<decltype(empty | drop_while<std::is_integral>)>, pack<>>);

    static_assert(std::same_as<as_pack_t<decltype(value | drop_until<std::is_floating_point>)>, pack<double, float>>);
    static_assert(std::same_as<as_pack_t<decltype(value | drop_until<std::is_integral>)>, pack<int, double, float>>);
    static_assert(std::same_as<as_pack_t<decltype(empty | drop_until<std::is_floating_point>)>, pack<>>);

    static_assert(std::same_as<as_pack_t<decltype(value | take<0>)>, pack<>>);
    static_assert(std::same_as<as_pack_t<decltype(value | take<1>)>, pack<int>>);
    static_assert(std::same_as<as_pack_t<decltype(value | take<3>)>, pack<int, double, float>>);
    static_assert(std::same_as<as_pack_t<decltype(empty | take<0>)>, pack<>>);

    static_assert(std::same_as<as_pack_t<decltype(value | take_while<std::is_integral>)>, pack<int>>);
    static_assert(std::same_as<as_pack_t<decltype(value | take_while<std::is_floating_point>)>, pack<>>);
    static_assert(std::same_as<as_pack_t<decltype(empty | take_while<std::is_integral>)>, pack<>>);

    static_assert(std::same_as<as_pack_t<decltype(value | take_until<std::is_floating_point>)>, pack<int>>);
    static_assert(std::same_as<as_pack_t<decltype(value | take_until<std::is_integral>)>, pack<>>);
    static_assert(std::same_as<as_pack_t<decltype(empty | take_until<std::is_floating_point>)>, pack<>>);

    static_assert(std::same_as<as_pack_t<decltype(value | drop_last<0>)>, pack<int, double, float>>);
    static_assert(std::same_as<as_pack_t<decltype(value | drop_last<1>)>, pack<int, double>>);
    static_assert(std::same_as<as_pack_t<decltype(value | drop_last<3>)>, pack<>>);
    static_assert(std::same_as<as_pack_t<decltype(empty | drop_last<0>)>, pack<>>);

    static_assert(std::same_as<as_pack_t<decltype(value | drop_last_while<std::is_floating_point>)>, pack<int>>);
    static_assert(std::same_as<as_pack_t<decltype(value | drop_last_while<std::is_integral>)>, pack<int, double, float>>);
    static_assert(std::same_as<as_pack_t<decltype(empty | drop_last_while<std::is_floating_point>)>, pack<>>);

    static_assert(std::same_as<as_pack_t<decltype(value | drop_last_until<std::is_integral>)>, pack<int>>);
    static_assert(std::same_as<as_pack_t<decltype(value | drop_last_until<std::is_floating_point>)>, pack<int, double, float>>);
    static_assert(std::same_as<as_pack_t<decltype(empty | drop_last_until<std::is_integral>)>, pack<>>);

    static_assert(std::same_as<as_pack_t<decltype(value | take_last<0>)>, pack<>>);
    static_assert(std::same_as<as_pack_t<decltype(value | take_last<1>)>, pack<float>>);
    static_assert(std::same_as<as_pack_t<decltype(value | take_last<3>)>, pack<int, double, float>>);
    static_assert(std::same_as<as_pack_t<decltype(empty | take_last<0>)>, pack<>>);

    static_assert(std::same_as<as_pack_t<decltype(value | take_last_while<std::is_floating_point>)>, pack<double, float>>);
    static_assert(std::same_as<as_pack_t<decltype(value | take_last_while<std::is_integral>)>, pack<>>);
    static_assert(std::same_as<as_pack_t<decltype(empty | take_last_while<std::is_floating_point>)>, pack<>>);

    static_assert(std::same_as<as_pack_t<decltype(value | take_last_until<std::is_integral>)>, pack<double, float>>);
    static_assert(std::same_as<as_pack_t<decltype(value | take_last_until<std::is_floating_point>)>, pack<>>);
    static_assert(std::same_as<as_pack_t<decltype(empty | take_last_until<std::is_integral>)>, pack<>>);
}

template<std::size_t I, class ...Args>
constexpr decltype(auto) forward_ith(Args&& ...args) {
    kaixo::template_pack<Args...> _args{ args... };
    return _args | kaixo::views::forward<I>;
}

int main() {
    using namespace kaixo;
    using namespace tuples;
    using namespace views;

    std::tuple<int, double, int, float> vals{ 1, 1.3, 3, 4.f };

    constexpr auto oiane = view<empty_view>;

    auto res = vals | nth_unique<1> | drop<1>;

    std::tuple<int, double, float> values{ 1, 1.2, 3.f };

    using paeres = decltype(values | forward<0>);
    using oieine = decltype(values | drop_while<std::is_integral>);
    using efaefa = decltype(values | take_while<std::is_integral>);
    using efgrew = decltype(values | take_until<std::is_floating_point>);

    get<0>(values | all);
    get<0>(values | take<2>);

}