
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
        concept view = std::derived_from<std::decay_t<Ty>, view_interface<std::decay_t<Ty>>>;
        
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
            using _type = std::conditional_t<std::is_lvalue_reference_v<_element>, Self&, Self>;
            using type = decltype(std::forward_like<_type>(std::declval<_element>()));
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
            
            constexpr static std::size_t size = std::tuple_size_v<Tpl>;

            // ------------------------------------------------

            template<std::size_t I>
                requires (I < size)
            using element = std::tuple_element_t<I, Tpl>&;

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

            constexpr static std::size_t size = std::tuple_size_v<Tpl>;

            // ------------------------------------------------

            template<std::size_t I>
                requires (I < size)
            using element = std::tuple_element_t<I, Tpl>;

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

        struct empty_view : view_interface<empty_view> {};

        // ------------------------------------------------
        
        namespace views {

            // ------------------------------------------------

            struct _all_fun : pipe_interface<_all_fun> {

                // ------------------------------------------------

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

                // ------------------------------------------------

            };

            // ------------------------------------------------

            constexpr _all_fun all{};

            // ------------------------------------------------

            template<class Ty>
            using all_t = decltype(all(std::declval<Ty>()));

            // ------------------------------------------------

        }

        // ------------------------------------------------

        namespace views {

            // ------------------------------------------------

            template<std::size_t I>
            struct _forward_fun : pipe_interface<_forward_fun<I>> {

                // ------------------------------------------------
                
                template<tuple_like Tpl>
                    requires (I <= std::tuple_size_v<std::decay_t<Tpl>>)
                constexpr std::tuple_element_t<I, Tpl>&& operator()(Tpl& tuple) const {
                    return static_cast<std::tuple_element_t<I, Tpl>&&>(std::get<I>(tuple));
                }
                
                template<tuple_like Tpl>
                    requires (I <= std::tuple_size_v<std::decay_t<Tpl>>)
                constexpr std::tuple_element_t<I, Tpl>&& operator()(Tpl&& tuple) const {
                    return static_cast<std::tuple_element_t<I, Tpl>&&>(std::get<I>(std::forward<Tpl>(tuple)));
                }

                // ------------------------------------------------

            };

            // ------------------------------------------------

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

        namespace views {

            // ------------------------------------------------

            template<std::size_t I>
            struct _take_fun : pipe_interface<_take_fun<I>> {

                // ------------------------------------------------

                template<tuple_like Tpl>
                    requires (I <= std::tuple_size_v<std::decay_t<Tpl>>)
                constexpr auto operator()(Tpl&& tuple) const {
                    if constexpr (I == 0) {
                        return empty_view{};
                    } else {
                        return take_view<I, all_t<Tpl>>{ .view = all(std::forward<Tpl>(tuple)) };
                    }
                }

                // ------------------------------------------------

            };

            // ------------------------------------------------

            template<std::size_t I>
            constexpr _take_fun<I> take{};

            // ------------------------------------------------

        }

        // ------------------------------------------------
        
        template<std::size_t I, view View>
        struct drop_view : view_interface<drop_view<I, View>> {

            // ------------------------------------------------

            View view;

            // ------------------------------------------------

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
            struct _drop_fun : pipe_interface<_drop_fun<I>> {

                // ------------------------------------------------

                template<tuple_like Tpl>
                    requires (I <= std::tuple_size_v<std::decay_t<Tpl>>)
                constexpr auto operator()(Tpl&& tuple) const {
                    if constexpr (I == std::tuple_size_v<std::decay_t<Tpl>>) {
                        return empty_view{};
                    } else {
                        return drop_view<I, all_t<Tpl>>{ .view = all(std::forward<Tpl>(tuple)) };
                    }
                }

                // ------------------------------------------------

            };

            // ------------------------------------------------

            template<std::size_t I>
            constexpr _drop_fun<I> drop{};

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

                // ------------------------------------------------

                template<tuple_like Tpl>
                constexpr auto operator()(Tpl&& tuple) const {
                    if constexpr (std::tuple_size_v<std::decay_t<Tpl>> == 0) {
                        return empty_view{};
                    } else {
                        return unique_view<all_t<Tpl>, Indices>{ .view = all(std::forward<Tpl>(tuple)) };
                    }
                }

                // ------------------------------------------------

            };

            // ------------------------------------------------

            // First unique occurence of each type
            constexpr _unique_fun<pack_first_indices> unique{};

            // First unique occurence of each type
            constexpr _unique_fun<pack_first_indices> first_unique{};

            // Last unique occurence of each type
            constexpr _unique_fun<pack_last_indices> last_unique{};

            template<std::size_t N>
            struct _pack_nth_indices_last_unique {
                template<class Pack>
                using type = pack_nth_indices<N, Pack>;
            };

            // Nth unique occurence of each type
            template<std::size_t N>
            constexpr _unique_fun<typename _pack_nth_indices_last_unique<N>::type> nth_unique{};

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

    static_assert(compare_get<std::tuple<int>>);
    static_assert(compare_get<std::tuple<int&>>);
    static_assert(compare_get<std::tuple<int&&>>);
    static_assert(compare_get<std::tuple<int>&>);
    static_assert(compare_get<std::tuple<int&>&>);
    static_assert(compare_get<std::tuple<int&&>&>);
    static_assert(compare_get<std::tuple<int>&&>);
    static_assert(compare_get<std::tuple<int&>&&>);
    static_assert(compare_get<std::tuple<int&&>&&>);
    static_assert(compare_get<std::tuple<const int>>);
    static_assert(compare_get<std::tuple<const int&>>);
    static_assert(compare_get<std::tuple<const int&&>>);
    static_assert(compare_get<std::tuple<const int>&>);
    static_assert(compare_get<std::tuple<const int&>&>);
    static_assert(compare_get<std::tuple<const int&&>&>);
    static_assert(compare_get<std::tuple<const int>&&>);
    static_assert(compare_get<std::tuple<const int&>&&>);
    static_assert(compare_get<std::tuple<const int&&>&&>);
    static_assert(compare_get<const std::tuple<int>&>);
    static_assert(compare_get<const std::tuple<int&>&>);
    static_assert(compare_get<const std::tuple<int&&>&>);
    static_assert(compare_get<const std::tuple<const int>&>);
    static_assert(compare_get<const std::tuple<const int&>&>);
    static_assert(compare_get<const std::tuple<const int&&>&>);

    static_assert(std::same_as<decltype(std::declval<std::tuple<int>&>() | forward<0>), int&&>);
    static_assert(std::same_as<decltype(std::declval<std::tuple<int>&&>() | forward<0>), int&&>);
    static_assert(std::same_as<decltype(std::declval<std::tuple<int&>&>() | forward<0>), int&>);
    static_assert(std::same_as<decltype(std::declval<std::tuple<int&>&&>() | forward<0>), int&>);
    static_assert(std::same_as<decltype(std::declval<std::tuple<int&&>&>() | forward<0>), int&&>);
    static_assert(std::same_as<decltype(std::declval<std::tuple<int&&>&&>() | forward<0>), int&&>);
    static_assert(std::same_as<decltype(std::declval<std::tuple<const int&>&>() | forward<0>), const int&>);
    static_assert(std::same_as<decltype(std::declval<std::tuple<const int&>&&>() | forward<0>), const int&>);
    static_assert(std::same_as<decltype(std::declval<const std::tuple<int>&>() | forward<0>), const int&&>);
    static_assert(std::same_as<decltype(std::declval<const std::tuple<int&>&>() | forward<0>), int&>);
    static_assert(std::same_as<decltype(std::declval<const std::tuple<int&&>&>() | forward<0>), int&&>);
    static_assert(std::same_as<decltype(std::declval<const std::tuple<const int&>&>() | forward<0>), const int&>);
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

    auto res = vals | nth_unique<1>;

    std::tuple<int, double, float> values{ 1, 1.2, 3.f };

    using paeres = decltype(values | forward<0>);

    get<0>(values | all);
    get<0>(values | take<2>);

}