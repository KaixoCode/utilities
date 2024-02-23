
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
        
        template<class Ty, class For>
        concept pipe_for = pipe<Ty> && std::invocable<Ty, For>;

        // ------------------------------------------------

        template<class Ty>
        concept tuple_like = requires() {
            std::tuple_size_v<std::decay_t<Ty>> == 0;
        } || requires(Ty val) {
            { std::tuple_size_v<std::decay_t<Ty>> };
            { std::get<0>(val) } -> std::convertible_to<std::tuple_element_t<0, std::decay_t<Ty>>>;
        };

        // ------------------------------------------------
        
        template<tuple_like T, pipe_for<T> Ty>
        constexpr decltype(auto) operator|(T&& tuple, Ty&& val) {
            return std::forward<Ty>(val)(std::forward<T>(tuple));
        }

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
    
    template<std::size_t I, class Ty>
        requires (I < decay_t<Ty>::size && kaixo::tuples::view<decay_t<Ty>>)
    constexpr decltype(auto) get(Ty&& view) {
        return std::forward<Ty>(view).template get<I>();
    }

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
            
            constexpr static std::size_t size = std::tuple_size_v<std::decay_t<Tpl>>;

            // ------------------------------------------------

            template<std::size_t I>
                requires (I < size)
            using element = std::tuple_element_t<I, std::decay_t<Tpl>>;

            // ------------------------------------------------

            template<std::size_t I, class Self>
                requires (I < size)
            constexpr decltype(auto) get(this Self&& self) {
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

            // ------------------------------------------------

            constexpr static std::size_t size = std::tuple_size_v<Tpl>;

            // ------------------------------------------------

            template<std::size_t I>
                requires (I < size)
            using element = std::tuple_element_t<I, Tpl>;

            // ------------------------------------------------

            template<std::size_t I, class Self>
                requires (I < size)
            constexpr decltype(auto) get(this Self&& self) {
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
            constexpr decltype(auto) get(this Self&& self) {
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
            constexpr decltype(auto) get(this Self&& self) {
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


    }


}

struct MyStruct {
    int a;
    double b;
    float c;
};

int main() {
    using namespace kaixo;

    constexpr auto aione2 = tuples::tuple_like<std::tuple<int>>;
    constexpr auto aione1 = tuples::tuple_like<std::tuple<>>;
    constexpr auto aione4 = tuples::tuple_like<std::pair<int, double>>;

    std::tuple<> vals2{};

    using namespace tuples;
    using namespace tuples::views;

    auto res = all(vals2);

    constexpr std::tuple<int, double> vals{ 1, 1.2 };
    constexpr auto naef = std::get<0>(vals | take<2> | drop<1>);

}