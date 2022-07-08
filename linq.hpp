#pragma once
#include "utils.hpp"

namespace kaixo {
#define just ]{ return (container_constraint{ std::tuple{}, [](auto&) { return true; } }
#define where ]{ return (
#define select(...) ).set_select(__VA_ARGS__); }();
#define in = value_t<__COUNTER__>{} %
#define from [

    template<std::size_t I, class Tuple, std::size_t... Is>
    constexpr auto element_as_tuple(Tuple& tuple, std::index_sequence<Is...>) {
        // Check if element is unique, by checking against all other tuple elements
        if constexpr (!(std::same_as<
            std::tuple_element_t<I, Tuple>,
            std::tuple_element_t<Is, Tuple>> || ...))
            // If unique, return element as a tuple
            return std::tuple<std::tuple_element_t<I, Tuple>>(std::get<I>(tuple));
        else return std::make_tuple(); // Otherwise empty element
    }

    template<class Tuple, std::size_t... Is>
    constexpr auto unique_tuple_impl(Tuple& tuple, std::index_sequence<Is...>) {
        return std::tuple_cat(element_as_tuple<Is>(tuple, std::make_index_sequence<Is>())...);
    }

    template<class Tuple> 
    constexpr auto unique_tuple(Tuple&& tuple) {
        constexpr auto size = std::tuple_size_v<std::decay_t<Tuple>>;
        using seq = std::make_index_sequence<size>;
        return unique_tuple_impl(tuple, seq{});
    }

    template<class A>
    concept is_container_wrapper = requires (A a) {
        typename A::value_type;
        typename A::container_type;
        A::id;
        { a.container } -> std::same_as<typename A::container_type&>;
    };

    template<class A>
    concept is_container_constraint = requires (A a) {
        { a.constraint };
        { a.containers };
    };

    template<class Ty, std::size_t ID>
    struct container_wrapper {
        using container_type = Ty;
        using value_type = std::decay_t<Ty>::value_type;
        using iterator = std::conditional_t<
            std::is_const_v<std::remove_reference_t<Ty>>,
            typename std::decay_t<Ty>::const_iterator,
            typename std::decay_t<Ty>::iterator>;

        constexpr static std::size_t id = ID;

        constexpr container_wrapper(Ty& container)
            : container(container) {}

        Ty& container;

        constexpr iterator begin() { return container.begin(); }
        constexpr iterator end() { return container.end(); }
    };

    template<class V, class Ty> requires
        requires (Ty ty) { V::value; { ty.begin() }; { ty.end() }; }
    constexpr auto operator%(V, Ty& container) {
        return container_wrapper<Ty, V::value>{ container };
    }

    template<class ...Tys>
    struct container_iterator_tuple {
        using containers = kaixo::pack<Tys...>;
        std::tuple<typename Tys::iterator...> its;

        template<std::size_t I>
        constexpr decltype(auto) get() const {
            return *[&]<std::size_t N, class Self>(this Self && self) {
                if constexpr (containers::template element<N>::id == I)
                    return std::get<N>(its);
                else return self.operator() < N + 1 > ();
            }.operator() < 0 > ();
        }
    };

    template<class ...Selects>
    struct container_select {
        std::tuple<Selects...> selects;

        constexpr auto to_value(auto& tpl) {
            return kaixo::sequence<sizeof...(Selects)>([&]<std::size_t ...Is>{
                return std::tuple<decltype(std::get<Is>(selects)(tpl))...>
                { std::get<Is>(selects)(tpl)... };
            });
        };
    };

    template<class Select, class Constraint, class Containers>
    struct cartesian_container {
        using containers_t = kaixo::as_pack<Containers>;

        Containers containers;
        Constraint constraint;
        Select selects;

        struct iterator {
            cartesian_container* self;
            containers_t::template as<container_iterator_tuple> iterators;
            bool at_end = false;

            constexpr iterator(bool begin, cartesian_container* self)
                : self(self) {
                kaixo::sequence<containers_t::size>([&]<std::size_t ...Is>{
                    if (begin) ((std::get<Is>(iterators.its) = std::get<Is>(self->containers).begin()), ...);
                    else at_end = true;
                });
                go_to_valid();
            }

            constexpr iterator& operator++() {
                incr_impl<0>();
                go_to_valid();
                return *this;
            }

            constexpr auto operator*() {
                return self->selects.to_value(iterators);
            }

            constexpr void go_to_valid() {
                while (!at_end && !self->constraint(iterators))
                    incr_impl<0>();
            }

            template<std::size_t I>
            constexpr void incr_impl() {
                if constexpr (I == containers_t::size) {
                    at_end = true;
                } else {
                    auto& it = std::get<I>(iterators.its);
                    auto& container = std::get<I>(self->containers);

                    if (++it == container.end()) {
                        it = container.begin();
                        incr_impl<I + 1>();
                    }
                }
            }

            constexpr bool operator==(const iterator& other) const {
                return at_end && other.at_end;
            }
        };

        constexpr iterator begin() { return { true, this, }; }
        constexpr iterator end() { return { false, this, }; }
    };

    template<class Constraint, class ...Tys>
    struct container_constraint {
        std::tuple<Tys...> containers;
        Constraint constraint;

        template<is_container_wrapper A>
        constexpr auto to_select(A& v) {
            return [](auto& tpl) -> decltype(auto) { return tpl.get<A::id>(); };
        }

        template<is_container_constraint A>
        constexpr auto to_select(A& v) {
            return v.constraint;
        };
        
        template<is_container_wrapper A>
        constexpr auto to_containers(A& v) {
            return std::tuple{ v };
        }

        template<is_container_constraint A>
        constexpr auto to_containers(A& v) {
            return v.containers;
        };

        template<class ...Args>
        constexpr auto set_select(Args&&...args) {
            return cartesian_container{
                unique_tuple(std::tuple_cat(containers, to_containers(args)...)),
                constraint,
                container_select{ std::tuple{ to_select(args)... } }
            };
        }
    };

#define KAIXO_C_OP_ARG(op)\
template<is_container_wrapper A, class B>                                                    \
constexpr auto operator op(const A& a, B&& b) {                                              \
    return container_constraint{ std::tuple{ a }, [b = std::forward<B>(b)]                   \
    (auto& tpl) -> decltype(auto) { return tpl.get<A::id>() op b; } }; }                     \
                                                                                             \
template<class A, is_container_wrapper B>                                                    \
constexpr auto operator op(A&& a, const B& b) {                                              \
    return container_constraint{ std::tuple{ b }, [a = std::forward<A>(a)]                   \
    (auto& tpl) -> decltype(auto) { return a op tpl.get<B::id>(); } }; }                     \
                                                                                             \
template<is_container_wrapper A, is_container_wrapper B>                                     \
constexpr auto operator op(const A& a, const B& b) {                                         \
    return container_constraint{ unique_tuple(std::tuple{ a, b }), []                        \
    (auto& tpl) -> decltype(auto) { return tpl.get<A::id>() op tpl.get<B::id>(); } }; }      \
                                                                                             \
template<is_container_constraint A, class B>                                                 \
constexpr auto operator op(A&& a, B&& b) {                                                   \
    return container_constraint{ a.containers, [a = a.constraint, b = std::forward<B>(b)]    \
    (auto& tpl) -> decltype(auto) { return a(tpl) op b; }}; }                                \
                                                                                             \
template<class A, is_container_constraint B>                                                 \
constexpr auto operator op(A&& a, B&& b) {                                                   \
    return container_constraint{ b.containers, [a = std::forward<A>(a), b = b.constraint]    \
    (auto& tpl) -> decltype(auto) { return a op b(tpl); }}; }                                \
                                                                                             \
template<is_container_constraint A, is_container_constraint B>                               \
constexpr auto operator op(A&& a, B&& b) {                                                   \
    return container_constraint{ unique_tuple(                                               \
        std::tuple_cat(a.containers, b.containers)), [a = a.constraint, b = b.constraint]    \
    (auto& tpl) -> decltype(auto) { return a(tpl) op b(tpl); }}; }                           \
                                                                                             \
template<is_container_constraint A, is_container_wrapper B>                                  \
constexpr auto operator op(A&& a, const B& b) {                                              \
    return container_constraint{ unique_tuple(                                               \
        std::tuple_cat(a.containers, std::tuple{ b })),[a = a.constraint]                    \
    (auto& tpl) -> decltype(auto) { return a(tpl) op tpl.get<B::id>(); }}; }                 \
                                                                                             \
template<class A, is_container_constraint B>                                                 \
constexpr auto operator op(const A& a, B&& b) {                                              \
    return container_constraint{ unique_tuple(                                               \
        std::tuple_cat(std::tuple{ a }, b.containers)),[b = b.constraint]                    \
    (auto& tpl) -> decltype(auto) { return tpl.get<A::id>() op b(tpl); }}; }

    KAIXO_C_OP_ARG(*); KAIXO_C_OP_ARG(/ ); KAIXO_C_OP_ARG(%); KAIXO_C_OP_ARG(+);
    KAIXO_C_OP_ARG(-); KAIXO_C_OP_ARG(<< ); KAIXO_C_OP_ARG(>> ); KAIXO_C_OP_ARG(<=> );
    KAIXO_C_OP_ARG(< ); KAIXO_C_OP_ARG(<= ); KAIXO_C_OP_ARG(> ); KAIXO_C_OP_ARG(>= );
    KAIXO_C_OP_ARG(== ); KAIXO_C_OP_ARG(!= ); KAIXO_C_OP_ARG(&); KAIXO_C_OP_ARG(^);
    KAIXO_C_OP_ARG(| ); KAIXO_C_OP_ARG(&&); KAIXO_C_OP_ARG(|| );
#undef KAIXO_C_OP_ARG;
}