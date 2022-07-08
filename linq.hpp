#pragma once
#include "utils.hpp"
#include <vector>
#include <functional>

namespace kaixo {
#define just ]{ return (container_constraint{ std::tuple{}, [](auto&) { return true; } }
#define where ]{ return (
#define select(...) ).set_select(__VA_ARGS__); }()
#define in = value_t<__COUNTER__>{} %
#define from [

    template<std::size_t I, class Tuple, std::size_t... Is>
    constexpr auto linq_element_as_tuple(Tuple& tuple, std::index_sequence<Is...>) {
        // Check if element is unique, by checking against all other tuple elements
        if constexpr (((
            std::tuple_element_t<I, Tuple>::id !=
            std::tuple_element_t<Is, Tuple>::id) && ...))
            // If unique, return element as a tuple
            return std::tuple<std::tuple_element_t<I, Tuple>>(std::get<I>(tuple));
        else return std::make_tuple(); // Otherwise empty element
    }

    template<class Tuple, std::size_t... Is>
    constexpr auto linq_unique_containers_impl(Tuple& tuple, std::index_sequence<Is...>) {
        return std::tuple_cat(linq_element_as_tuple<Is>(tuple, std::make_index_sequence<Is>())...);
    }

    template<class Tuple>
    constexpr auto linq_unique_containers(Tuple&& tuple) {
        constexpr auto size = std::tuple_size_v<std::decay_t<Tuple>>;
        using seq = std::make_index_sequence<size>;
        return linq_unique_containers_impl(tuple, seq{});
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

    template<class Ty, class V>
    concept value_type_is = std::same_as<typename std::decay_t<Ty>::value_type, V>;

#define linq_class(Type)                                     \
    template<class _access_type>                             \
    struct value_type_specialization<Type, _access_type>     \
        : value_type_specialization_base<Type, _access_type>
    
#define template_linq_class(tp, Type)                        \
    template<class _access_type, tp>                         \
    struct value_type_specialization<Type, _access_type>     \
        : value_type_specialization_base<Type, _access_type>


    template<class V, class Ty>
    struct value_type_specialization_base {
        const Ty* ptr;
    };

    template<class V, class Ty>
    struct value_type_specialization : value_type_specialization_base<V, Ty> {
    };

    template<class Ty, std::size_t ID>
    struct container_wrapper_base {
        using container_type = Ty;
        using value_type = std::decay_t<Ty>::value_type;
        using iterator = std::conditional_t<
            std::is_const_v<std::remove_reference_t<Ty>>,
            typename std::decay_t<Ty>::const_iterator,
            typename std::decay_t<Ty>::iterator>;

        constexpr static std::size_t id = ID;

        constexpr container_wrapper_base(Ty& container)
            : container(container) {}

        Ty& container;

        constexpr iterator cwb_begin() { return container.begin(); }
        constexpr iterator cwb_end() { return container.end(); }
        constexpr iterator cwb_begin() const { return container.begin(); }
        constexpr iterator cwb_end() const { return container.end(); }

        constexpr auto from_tpl() const { return [](auto& tpl) { return tpl.get<ID>(); }; }
    };

    template<class Ty, std::size_t ID>
    struct container_wrapper : container_wrapper_base<Ty, ID>,
        value_type_specialization<
            typename std::decay_t<Ty>::value_type,
            container_wrapper_base<Ty, ID>> {
        using value_parent = value_type_specialization<
            typename std::decay_t<Ty>::value_type,
            container_wrapper_base<Ty, ID>>;
        constexpr container_wrapper(Ty& container)
            : container_wrapper_base<Ty, ID>(container),
            value_parent{ this } {}
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

        constexpr auto to_value(auto& tpl) const {
            return kaixo::sequence<sizeof...(Selects)>([&]<std::size_t ...Is>{
                return std::tuple<decltype(std::get<Is>(selects)(tpl))...>
                { std::get<Is>(selects)(tpl)... };
            });
        };
    };

    template<class Select, class Constraint, class Containers>
    struct cartesian_container {
        using containers_t = kaixo::as_pack<Containers>;

        Containers containers{};
        Constraint constraint{};
        Select selects{};

        template<bool Const = false>
        struct iterator {
            using self_ptr = std::conditional_t<Const, const cartesian_container*, cartesian_container*>;
            self_ptr self = nullptr;
            containers_t::template as<container_iterator_tuple> iterators{};
            bool at_end = false;

            constexpr iterator(bool begin, self_ptr self)
                : self(self) {
                kaixo::sequence<containers_t::size>([&]<std::size_t ...Is>{
                    if (begin) ((std::get<Is>(iterators.its) = std::get<Is>(self->containers).cwb_begin()), ...);
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
                    
                    if (++it == container.cwb_end()) {
                        it = container.cwb_begin();
                        incr_impl<I + 1>();
                    }
                }
            }

            constexpr bool operator==(const iterator& other) const {
                return at_end && other.at_end;
            }
        };

        constexpr iterator<false> begin() { return { true, this, }; }
        constexpr iterator<false> end() { return { false, this, }; }
        constexpr iterator<true> begin() const { return { true, this, }; }
        constexpr iterator<true> end() const { return { false, this, }; }
    };

    template<class Ty>
    concept is_linq_fold = requires (Ty ty) {
        typename Ty::fold_op;
    };

    template<is_container_wrapper A>
    constexpr static auto to_select(A& v) {
        return [](auto& tpl) -> decltype(auto) { return tpl.get<A::id>(); };
    }

    template<is_container_constraint A>
    constexpr static auto to_select(A& v) {
        return v.constraint;
    };

    template<is_container_wrapper A>
    constexpr static auto to_containers(A& v) {
        return std::tuple{ v };
    }

    template<is_container_constraint A>
    constexpr static auto to_containers(A& v) {
        return v.containers;
    };

    template<class Constraint, class ...Tys>
    struct container_constraint :
        value_type_specialization<
        decltype(std::declval<Constraint>()(std::declval<container_iterator_tuple<Tys...>&>())),
        container_constraint<Constraint, Tys...>> {

        using value_parent = value_type_specialization<
            decltype(std::declval<Constraint>()(std::declval<container_iterator_tuple<Tys...>&>())),
            container_constraint<Constraint, Tys...>>;

        constexpr auto from_tpl() const { return [constraint = constraint](auto& tpl) { return constraint(tpl); }; }

        constexpr container_constraint(std::tuple<Tys...> containers, Constraint constraint)
            : containers(containers), constraint(constraint), value_parent{ this } {}

        std::tuple<Tys...> containers;
        Constraint constraint;

        template<class ...Args>
        constexpr auto set_select(Args&&...args) {
            using args_pack = kaixo::pack<std::decay_t<Args>...>;
            if constexpr ((is_linq_fold<Args> && ...)) {
                static_assert(sizeof...(Args) == 1);
                cartesian_container _container{
                    linq_unique_containers(std::tuple_cat(containers, args.constraint.containers...)),
                    constraint,
                    container_select{ std::tuple{ args.constraint.constraint... } }
                };

                using result = std::decay_t<std::tuple_element_t<0, decltype(*_container.begin())>>;
                result _res{};
                for (auto [x] : _container) {
                    _res = (args.op(_res, x), ...);
                }
                return _res;
            } else {
                return cartesian_container{
                    linq_unique_containers(std::tuple_cat(containers, to_containers(args)...)),
                    constraint,
                    container_select{ std::tuple{ to_select(args)... } }
                };
            }
        }
    };

#define KAIXO_C_OP_ARG(op)\
template<is_container_wrapper A, class B>                                                    \
    requires (!is_container_wrapper<B> && !is_container_constraint<B>)                       \
constexpr auto operator op(const A& a, B&& b) {                                              \
    return container_constraint{ std::tuple{ a }, [b = std::forward<B>(b)]                   \
    (auto& tpl) -> decltype(auto) { return tpl.get<A::id>() op b; } }; }                     \
                                                                                             \
template<class A, is_container_wrapper B>                                                    \
    requires (!is_container_wrapper<A> && !is_container_constraint<A>)                       \
constexpr auto operator op(A&& a, const B& b) {                                              \
    return container_constraint{ std::tuple{ b }, [a = std::forward<A>(a)]                   \
    (auto& tpl) -> decltype(auto) { return a op tpl.get<B::id>(); } }; }                     \
                                                                                             \
template<is_container_wrapper A, is_container_wrapper B>                                     \
constexpr auto operator op(const A& a, const B& b) {                                         \
    return container_constraint{ linq_unique_containers(std::tuple{ a, b }), []              \
    (auto& tpl) -> decltype(auto) { return tpl.get<A::id>() op tpl.get<B::id>(); } }; }      \
                                                                                             \
template<is_container_constraint A, class B>                                                 \
    requires (!is_container_wrapper<B> && !is_container_constraint<B>)                       \
constexpr auto operator op(A&& a, B&& b) {                                                   \
    return container_constraint{ a.containers, [a = a.constraint, b = std::forward<B>(b)]    \
    (auto& tpl) -> decltype(auto) { return a(tpl) op b; }}; }                                \
                                                                                             \
template<class A, is_container_constraint B>                                                 \
    requires (!is_container_wrapper<A> && !is_container_constraint<A>)                       \
constexpr auto operator op(A&& a, B&& b) {                                                   \
    return container_constraint{ b.containers, [a = std::forward<A>(a), b = b.constraint]    \
    (auto& tpl) -> decltype(auto) { return a op b(tpl); }}; }                                \
                                                                                             \
template<is_container_constraint A, is_container_constraint B>                               \
constexpr auto operator op(A&& a, B&& b) {                                                   \
    return container_constraint{ linq_unique_containers(                                     \
        std::tuple_cat(a.containers, b.containers)), [a = a.constraint, b = b.constraint]    \
    (auto& tpl) -> decltype(auto) { return a(tpl) op b(tpl); }}; }                           \
                                                                                             \
template<is_container_constraint A, is_container_wrapper B>                                  \
constexpr auto operator op(A&& a, const B& b) {                                              \
    return container_constraint{ linq_unique_containers(                                     \
        std::tuple_cat(a.containers, std::tuple{ b })),[a = a.constraint]                    \
    (auto& tpl) -> decltype(auto) { return a(tpl) op tpl.get<B::id>(); }}; }                 \
                                                                                             \
template<is_container_wrapper A, is_container_constraint B>                                  \
constexpr auto operator op(const A& a, B&& b) {                                              \
    return container_constraint{ linq_unique_containers(                                     \
        std::tuple_cat(std::tuple{ a }, b.containers)),[b = b.constraint]                    \
    (auto& tpl) -> decltype(auto) { return tpl.get<A::id>() op b(tpl); }}; }

    KAIXO_C_OP_ARG(*); KAIXO_C_OP_ARG(/ ); KAIXO_C_OP_ARG(%); KAIXO_C_OP_ARG(+);
    KAIXO_C_OP_ARG(-); KAIXO_C_OP_ARG(<< ); KAIXO_C_OP_ARG(>> ); KAIXO_C_OP_ARG(<=> );
    KAIXO_C_OP_ARG(< ); KAIXO_C_OP_ARG(<= ); KAIXO_C_OP_ARG(> ); KAIXO_C_OP_ARG(>= );
    KAIXO_C_OP_ARG(== ); KAIXO_C_OP_ARG(!= ); KAIXO_C_OP_ARG(&); KAIXO_C_OP_ARG(^);
    KAIXO_C_OP_ARG(| ); KAIXO_C_OP_ARG(&&); KAIXO_C_OP_ARG(|| );

#undef KAIXO_C_OP_ARG

    template<class Constraint, class Op>
    struct linq_fold_expression {
        using fold_op = Op;
        Constraint constraint;
        Op op;
    };

#define KAIXO_C_FOLD_ARG(op, cls)                                             \
    template<is_container_wrapper A>                                          \
    constexpr auto operator op(const A& a, const kaixo::fold::fold_t&) {      \
        return linq_fold_expression{                                          \
            container_constraint{                                             \
                std::tuple{ a },                                              \
                [](auto& tpl) -> decltype(auto) { return tpl.get<A::id>(); }  \
            }, std::cls<void>{}                                               \
        };                                                                    \
    }                                                                         \
                                                                              \
    template<is_container_constraint A>                                       \
    constexpr auto operator op(A&& a, const kaixo::fold::fold_t&) {           \
        return linq_fold_expression{                                          \
            a, std::cls<void>{}                                               \
        };                                                                    \
    }

    KAIXO_C_FOLD_ARG(*, multiplies); KAIXO_C_FOLD_ARG(/, divides); KAIXO_C_FOLD_ARG(%, modulus); 
    KAIXO_C_FOLD_ARG(+, plus); KAIXO_C_FOLD_ARG(-, minus); 
    KAIXO_C_FOLD_ARG(<, less); KAIXO_C_FOLD_ARG(<=, less_equal); KAIXO_C_FOLD_ARG(>, greater ); 
    KAIXO_C_FOLD_ARG(>=, greater_equal); KAIXO_C_FOLD_ARG(==, equal_to); KAIXO_C_FOLD_ARG(!=, not_equal_to); 
    KAIXO_C_FOLD_ARG(&, bit_and); KAIXO_C_FOLD_ARG(^, bit_xor); KAIXO_C_FOLD_ARG(|, bit_or);
    KAIXO_C_FOLD_ARG(&&, logical_and); KAIXO_C_FOLD_ARG(||, logical_or);

#undef KAIXO_C_FOLD_ARG

#define linq_member_function(name)                                             \
    template<class ...Args>                                                    \
    constexpr decltype(auto) name(Args&&...args) const {                       \
        auto access = this->ptr;                                               \
        return container_constraint{ to_containers(*access),                   \
            [access = access->from_tpl(), ...args = std::forward<Args>(args)]  \
            (auto& tpl) { return access(tpl).name(args...); }                  \
        };                                                                     \
    }

#define linq_member_object(name)                                               \
    constexpr static auto create_member_##name(const _access_type* access) {   \
    return container_constraint{  to_containers(*access),                      \
        [access = access->from_tpl()](auto& tpl) { return access(tpl).name; }  \
    };                                                                         \
    }                                                                          \
    decltype(create_member_##name(std::declval<const _access_type*>())) name = \
        create_member_##name(this->ptr);

    template<class Ty>
    concept is_container_type = requires(Ty ty) {
        { ty.begin() };
        { ty.end() };
    };

    template_linq_class(is_container_type Ty, Ty) {
        linq_member_function(operator=);
        linq_member_function(assign);
        linq_member_function(get_allocator);
        linq_member_function(at);
        linq_member_function(operator[]);
        linq_member_function(front);
        linq_member_function(back);
        linq_member_function(data);
        linq_member_function(c_str);
        linq_member_function(begin);
        linq_member_function(cbegin);
        linq_member_function(end);
        linq_member_function(cend);
        linq_member_function(rbegin);
        linq_member_function(crbegin);
        linq_member_function(rend);
        linq_member_function(crend);
        linq_member_function(empty);
        linq_member_function(size);
        linq_member_function(length);
        linq_member_function(max_size);
        linq_member_function(reserve);
        linq_member_function(capacity);
        linq_member_function(shrink_to_fit);
        linq_member_function(clear);
        linq_member_function(insert);
        linq_member_function(erase);
        linq_member_function(push_back);
        linq_member_function(pop_back);
        linq_member_function(append);
        linq_member_function(operator+=);
        linq_member_function(compare);
        linq_member_function(starts_with);
        linq_member_function(ends_with);
        linq_member_function(contains);
        linq_member_function(replace);
        linq_member_function(substr);
        linq_member_function(copy);
        linq_member_function(resize);
        linq_member_function(resize_and_overwrite);
        linq_member_function(swap);
        linq_member_function(find);
        linq_member_function(rfind);
        linq_member_function(find_first_of);
        linq_member_function(find_first_not_of);
        linq_member_function(find_last_of);
        linq_member_function(find_last_not_of);
    };
}