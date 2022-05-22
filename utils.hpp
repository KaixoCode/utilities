#pragma once
#include <utility>
#include <tuple>

namespace kaixo {
    template<class Ty> concept has_fun_op = requires(decltype(&Ty::operator()) a) { a; };
    namespace detail {
        template<class Ty> struct signature { static_assert(has_fun_op<Ty>, "Type has no function signature."); };
        template<has_fun_op Ty>
        struct signature<Ty> { using type = typename signature<decltype(&Ty::operator())>::type; };
        template<class R, class T, class ...Args>
        struct signature<R(T::*)(Args...)> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct signature<R(T::*)(Args...) const> { using type = R(Args...) const; };
        template<class R, class T, class ...Args>
        struct signature<R(T::*)(Args...) &&> { using type = R(Args...) &&; };
        template<class R, class T, class ...Args>
        struct signature<R(T::*)(Args...) &> { using type = R(Args...) &; };
        template<class R, class T, class ...Args>
        struct signature<R(T::*)(Args...) const&&> { using type = R(Args...) const&&; };
        template<class R, class T, class ...Args>
        struct signature<R(T::*)(Args...) const&> { using type = R(Args...) const&; };
        template<class R, class T, class ...Args>
        struct signature<R(T::*)(Args...) volatile> { using type = R(Args...) volatile; };
        template<class R, class T, class ...Args>
        struct signature<R(T::*)(Args...) volatile const> { using type = R(Args...) volatile const; };
        template<class R, class T, class ...Args>
        struct signature<R(T::*)(Args...) volatile &&> { using type = R(Args...) volatile &&; };
        template<class R, class T, class ...Args>
        struct signature<R(T::*)(Args...) volatile &> { using type = R(Args...) volatile &; };
        template<class R, class T, class ...Args>
        struct signature<R(T::*)(Args...) volatile const&&> { using type = R(Args...) volatile const&&; };
        template<class R, class T, class ...Args>
        struct signature<R(T::*)(Args...) volatile const&> { using type = R(Args...) volatile const&; };
        template<class R, class T, class ...Args>
        struct signature<R(T::* const)(Args...)> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct signature<R(T::* const)(Args...) const> { using type = R(Args...) const; };
        template<class R, class T, class ...Args>
        struct signature<R(T::* const)(Args...)&&> { using type = R(Args...)&&; };
        template<class R, class T, class ...Args>
        struct signature<R(T::* const)(Args...)&> { using type = R(Args...)&; };
        template<class R, class T, class ...Args>
        struct signature<R(T::* const)(Args...) const&&> { using type = R(Args...) const&&; };
        template<class R, class T, class ...Args>
        struct signature<R(T::* const)(Args...) const&> { using type = R(Args...) const&; };
        template<class R, class T, class ...Args>
        struct signature<R(T::* const)(Args...) volatile> { using type = R(Args...) volatile; };
        template<class R, class T, class ...Args>
        struct signature<R(T::* const)(Args...) volatile const> { using type = R(Args...) volatile const; };
        template<class R, class T, class ...Args>
        struct signature<R(T::* const)(Args...) volatile&&> { using type = R(Args...) volatile&&; };
        template<class R, class T, class ...Args>
        struct signature<R(T::* const)(Args...) volatile&> { using type = R(Args...) volatile&; };
        template<class R, class T, class ...Args>
        struct signature<R(T::* const)(Args...) volatile const&&> { using type = R(Args...) volatile const&&; };
        template<class R, class T, class ...Args>
        struct signature<R(T::* const)(Args...) volatile const&> { using type = R(Args...) volatile const&; };
        template<class R, class ...Args>
        struct signature<R(*)(Args...)> { using type = R(Args...); };        
        
        template<class Ty> struct minimal_signature { static_assert(has_fun_op<Ty>, "Type has no function signature."); };
        template<has_fun_op Ty>
        struct minimal_signature <Ty> { using type = typename minimal_signature<decltype(&Ty::operator())>::type; };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::*)(Args...)> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::*)(Args...) const> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::*)(Args...) &&> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::*)(Args...) &> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::*)(Args...) const&&> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::*)(Args...) const&> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::*)(Args...) volatile> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::*)(Args...) volatile const> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::*)(Args...) volatile &&> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::*)(Args...) volatile &> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::*)(Args...) volatile const&&> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::*)(Args...) volatile const&> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::* const)(Args...)> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::* const)(Args...) const> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::* const)(Args...)&&> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::* const)(Args...)&> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::* const)(Args...) const&&> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::* const)(Args...) const&> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::* const)(Args...) volatile> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::* const)(Args...) volatile const> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::* const)(Args...) volatile&&> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::* const)(Args...) volatile&> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::* const)(Args...) volatile const&&> { using type = R(Args...); };
        template<class R, class T, class ...Args>
        struct minimal_signature <R(T::* const)(Args...) volatile const&> { using type = R(Args...); };
        template<class R, class ...Args>
        struct minimal_signature <R(*)(Args...)> { using type = R(Args...); };

        template<class> struct member_function_type;
        template<class R, class T, class ...Args>
        struct member_function_type<R(T::*)(Args...)> { using type = T; };
        template<class R, class T, class ...Args>
        struct member_function_type<R(T::*)(Args...) const> { using type = T const; };
        template<class R, class T, class ...Args>
        struct member_function_type<R(T::*)(Args...)&&> { using type = T&&; };
        template<class R, class T, class ...Args>
        struct member_function_type<R(T::*)(Args...)&> { using type = T&; };
        template<class R, class T, class ...Args>
        struct member_function_type<R(T::*)(Args...) const&&> { using type = T const&&; };
        template<class R, class T, class ...Args>
        struct member_function_type<R(T::*)(Args...) const&> { using type = T const&; };
        template<class R, class T, class ...Args>
        struct member_function_type<R(T::*)(Args...) volatile> { using type = T volatile; };
        template<class R, class T, class ...Args>
        struct member_function_type<R(T::*)(Args...) volatile const> { using type = T volatile const; };
        template<class R, class T, class ...Args>
        struct member_function_type<R(T::*)(Args...) volatile&&> { using type = T volatile&&; };
        template<class R, class T, class ...Args>
        struct member_function_type<R(T::*)(Args...) volatile&> { using type = T volatile&; };
        template<class R, class T, class ...Args>
        struct member_function_type<R(T::*)(Args...) volatile const&&> { using type = T volatile const&&; };
        template<class R, class T, class ...Args>
        struct member_function_type<R(T::*)(Args...) volatile const&> { using type = T volatile const&; };

        template<class> struct function_args;
        template<class R, class ...Args>
        struct function_args<R(Args...)> { using type = std::tuple<Args...>; };
        template<class R, class ...Args>
        struct function_args<R(Args...) const> { using type = std::tuple<Args...>; };
        template<class R, class ...Args>
        struct function_args<R(Args...) volatile> { using type = std::tuple<Args...>; };
        template<class R, class ...Args>
        struct function_args<R(Args...) volatile const> { using type = std::tuple<Args...>; };
        template<class> struct function_return;
        template<class R, class ...Args>
        struct function_return<R(Args...)> { using type = R; };
        template<class R, class ...Args>
        struct function_return<R(Args...) const> { using type = R; };
        template<class R, class ...Args>
        struct function_return<R(Args...) volatile> { using type = R; };
        template<class R, class ...Args>
        struct function_return<R(Args...) volatile const> { using type = R; };

        template<class Ty, class Tuple> struct invocable_tuple;
        template<class Ty, class ...Args> 
        struct invocable_tuple<Ty, std::tuple<Args...>> 
            : std::bool_constant<std::invocable<Ty, Args...>> {};

        template<class> struct member_type;
        template<class Ty, class C> struct member_type<Ty C::*> { using type = Ty; };
        template<class> struct member_class;
        template<class Ty, class C> struct member_class<Ty C::*> { using type = C; };
        template<class Ty, class C> struct member_class<Ty C::* const> { using type = C; };
        template<class Ty, class C> struct member_class<Ty C::* volatile> { using type = C; };
        template<class Ty, class C> struct member_class<Ty C::* volatile const> { using type = C; };
        template<class Ty, class C> struct member_class<Ty C::* &> { using type = C; };
        template<class Ty, class C> struct member_class<Ty C::* const& > { using type = C; };
        template<class Ty, class C> struct member_class<Ty C::* volatile& > { using type = C; };
        template<class Ty, class C> struct member_class<Ty C::* volatile const&> { using type = C; };
        template<class Ty, class C> struct member_class<Ty C::*&&> { using type = C; };
        template<class Ty, class C> struct member_class<Ty C::* const&& > { using type = C; };
        template<class Ty, class C> struct member_class<Ty C::* volatile&& > { using type = C; };
        template<class Ty, class C> struct member_class<Ty C::* volatile const&&> { using type = C; };
    }
    // get the function signature of a (member) function or a lambda/functor
    template<class Ty> using signature_t = typename detail::signature<Ty>::type;
    template<class Ty> using minimal_signature_t = typename detail::minimal_signature<Ty>::type;
    template<class Ty> using member_function_type_t = typename detail::member_function_type<Ty>::type;
    template<class Ty> using function_args_t = typename detail::function_args<Ty>::type;
    template<class Ty> using function_return_t = typename detail::function_return<Ty>::type;
    template<class Ty, class Tuple> concept invocable_tuple_t = detail::invocable_tuple<Ty, Tuple>::value;
    template<class Ty> using member_type_t = typename detail::member_type<Ty>::type;
    template<class Ty> using member_class_t = typename detail::member_class<Ty>::type;

    namespace detail {
        template<class, template<class...> class>
        struct is_specialization : std::false_type {};
        template<template<class...> class Ref, class... Args>
        struct is_specialization<Ref<Args...>, Ref> : std::true_type {};
    }
    // is specialization of templated class
    template<class Test, template<class...> class Ref>
    concept specialization = detail::is_specialization<std::decay_t<Test>, Ref>::value;

    // 
    // Tuple helpers
    // 

    // is specialization of std::tuple
    template<class Ty> concept is_tuple = specialization<Ty, std::tuple>;

    namespace detail {
        template<std::size_t I, class Tuple, std::size_t... Is>
        constexpr auto element_as_tuple(Tuple tuple, std::index_sequence<Is...>) {
            if constexpr (!(std::is_same_v<std::tuple_element_t<I, Tuple>,
                std::tuple_element_t<Is, Tuple>> || ...))
                return std::tuple<std::tuple_element_t<I, Tuple>>(std::get<I>(tuple));
            else return std::make_tuple();
        }
        template<class Tuple, std::size_t... Is>
        constexpr auto make_tuple_unique(Tuple tuple, std::index_sequence<Is...>) {
            return std::tuple_cat(element_as_tuple<Is>(tuple, std::make_index_sequence<Is>())...);
        }
    }
    // remove duplicate types from tuple
    template<is_tuple Tuple> constexpr auto unique_tuple(Tuple tuple) {
        return detail::make_tuple_unique(tuple, std::make_index_sequence<std::tuple_size_v<tuple>>{});
    }
    // remove duplicate types from tuples
    template<is_tuple Ty> using unique_tuple_t = decltype(unique_tuple(std::declval<Ty>()));

    namespace detail {
        template<class T, class E, std::size_t I = 0> struct tuple_index;
        template<class F, class ...R, class E, std::size_t I>
        struct tuple_index<std::tuple<F, R...>, E, I> : public std::conditional<std::is_same<E, F>::value,
            std::integral_constant<std::size_t, I>, tuple_index<std::tuple<R...>, E, I + 1>>::type{};
        template<class E, std::size_t I> struct tuple_index<std::tuple<>, E, I> {};
    }
    // index of type in tuple
    template<class E, is_tuple Tuple>
    constexpr static std::size_t tuple_index_v = detail::tuple_index<Tuple, E>::value;

    namespace detail {
        template<class T, is_tuple Ty> struct is_in_tuple;
        template<class T, class ...Tys> struct is_in_tuple<T, std::tuple<Tys...>>
        : std::bool_constant<(std::same_as<T, Tys> || ...)> {};
    }
    // type is in tuple
    template<class T, class Ty> concept in_tuple = is_tuple<Ty> && detail::is_in_tuple<T, Ty>::value;
    // type is not in tuple
    template<class T, class Ty> concept not_in_tuple = is_tuple<Ty> && !detail::is_in_tuple<T, Ty>::value;

    // concat tuples
    template<is_tuple ...Tys> using tuple_cat_t = decltype(std::tuple_cat(std::declval<Tys>()...));

    namespace detail {
        template<class ...Tys> struct as_tuple { using type = std::tuple<Tys...>; };
        template<class Ty> struct as_tuple<Ty> { using type = std::tuple<Ty>; };
        template<class A, class B> struct as_tuple<std::pair<A, B>> { using type = std::tuple<A, B>; };
        template<class ...Tys, template<class...> class T> struct as_tuple<T<Tys...>> { using type = std::tuple<Tys...>; };
        template<class ...Tys> struct as_tuple<std::tuple<Tys...>> { using type = std::tuple<Tys...>; };
    }
    // get type as tuple (pair<a, b> -> tuple<a, b>, type -> tuple<type>, tuple<tys...> -> tuple<tys...>, 
    // type<tys...> -> tuple<tys...>, tys... -> std::tuple<tys...>)
    template<class ...Ty> using as_tuple_t = typename detail::as_tuple<Ty...>::type;

    namespace detail {
        template<is_tuple Ty> struct flatten;
        template<class Ty> struct flatten<std::tuple<Ty>> { using type = std::tuple<Ty>; };
        template<class ...Tys> struct flatten<std::tuple<Tys...>> {
            using type = tuple_cat_t<typename flatten<as_tuple_t<Tys>>::type...>;
        };
    }
    // flatten nested tuples to a single tuple
    template<is_tuple Ty> using flatten_t = typename detail::flatten<Ty>::type;

    namespace detail {
        template<class T, std::size_t ... Is>
        constexpr void print_tuple(auto& a, T& v, std::index_sequence<Is...>) {
            a << "(";
            ((a << std::get<Is>(v) << ", "), ...);
            a << std::get<sizeof...(Is)>(v);
            a << ")";
        }
    }
    // simple tuple printing
    template<class ...Ty>
    constexpr auto& operator<<(auto& a, std::tuple<Ty...>& v) {
        if constexpr (sizeof...(Ty) == 0) a << "()";
        else detail::print_tuple(a, v, std::make_index_sequence<sizeof...(Ty) - 1>{});
        return a;
    }

    // templated for, calls lambda with template argument std::size_t
    template<std::size_t N> constexpr auto tfor(auto lambda) {
        [&] <std::size_t ...Is>(std::index_sequence<Is...>) { 
            (lambda.operator() < Is > (), ...); }(std::make_index_sequence<N>{});
    }

    // templated for, for tuple, supports concept constraints
    template<class Tuple> constexpr auto tfor(Tuple&& tuple, auto lambda) {
        kaixo::tfor<std::tuple_size_v<std::decay_t<Tuple>>>([&]<std::size_t I> {
            if constexpr (std::is_invocable_v<decltype(lambda),
                decltype(std::get<I>(tuple))>) lambda(std::get<I>(tuple));
        });
    }

    // templated for, for tuple, supports concept constraints
    template<class Tuple> constexpr auto tfor(Tuple&& tuple, auto... lambdas) {
        kaixo::tfor<std::tuple_size_v<std::decay_t<Tuple>>>([&]<std::size_t I> {
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
}