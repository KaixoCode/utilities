#pragma once
#include <type_traits>
#include <concepts>
#include <cstddef>
#include <utility>
#include <algorithm>
#include "kaixo/type_utils.hpp"

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                      Unevaluated Expressions

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

namespace kaixo {

    /**
     * Forward as tuple, but only lvalue references are kept, rest is copied.
     * @param ...args arguments
     */
    template<class ...Args>
    constexpr auto ref_tuple(Args&& ...args) {
        using type = std::tuple<std::conditional_t<concepts::lvalue_reference<Args>, Args, decay_t<Args>>...>;
        return type{ std::forward<Args>(args)... };
    }

    /**
     * Construct a pair from a tuple of size 2.
     * @param t tuple
     */
    template<concepts::specialization<std::tuple> Tuple>
        requires (std::tuple_size_v<decay_t<Tuple>> == 2)
    constexpr auto tuple_to_pair(Tuple&& t) {
        using pair_type = move_tparams_t<decay_t<Tuple>, std::pair>;
        return pair_type(std::get<0>(t), std::get<1>(t));
    }

    namespace has {
        template<class Ty> concept depend_v = requires (Ty) { typename Ty::depend; };
        template<class Ty> concept define_v = requires (Ty) { typename Ty::define; };
        template<class Ty> concept var_v = requires (Ty) { typename Ty::var; };
        template<class Ty> concept range_v = requires (Ty) { typename Ty::range; };

        template<class Ty> struct depend_impl : std::bool_constant<depend_v<Ty>> {};
        template<class Ty> struct define_impl : std::bool_constant<define_v<Ty>> {};
        template<class Ty> struct var_impl : std::bool_constant<var_v<Ty>> {};
        template<class Ty> struct range_impl : std::bool_constant<range_v<Ty>> {};

        constexpr auto depend = type_trait<depend_impl>{};
        constexpr auto define = type_trait<define_impl>{};
        constexpr auto var = type_trait<var_impl>{};
        constexpr auto range = type_trait<range_impl>{};
    }

    namespace grab {
        template<class Ty> struct depend_impl { using type = info<>; };
        template<has::depend_v Ty> struct depend_impl<Ty> { using type = typename Ty::depend; };
        template<class Ty> struct define_impl { using type = info<>; };
        template<has::define_v Ty> struct define_impl<Ty> { using type = typename Ty::define; };
        template<class Ty> struct var_impl { using type = info<>; };
        template<has::var_v Ty> struct var_impl<Ty> { using type = typename Ty::var; };
        template<class Ty> struct range_impl { using type = info<>; };
        template<has::range_v Ty> struct range_impl<Ty> { using type = typename Ty::range; };

        template<class Ty> using depend = depend_impl<Ty>::type;
        template<class Ty> using define = define_impl<Ty>::type;
        template<class Ty> using var = var_impl<Ty>::type;
        template<class Ty> using range = range_impl<Ty>::type;
    }

    template<class Ty> using depend = grab::depend<decay_t<Ty>>;
    template<class Ty> using define = grab::define<decay_t<Ty>>;

    template<class Ty> concept explicit_range = requires() { typename Ty::is_range; };
    template<class Ty> concept is_range = std::ranges::range<Ty>;
    template<class Ty> concept is_partial = depend<Ty>::size != 0 || requires() { typename decay_t<Ty>::is_partial; };
    template<class Ty> concept is_partial_range = is_partial<Ty> && explicit_range<Ty>;
    template<class Ty> concept is_var = is_partial<Ty> && requires() { { decay_t<Ty>::name }; };
    template<class Ty> concept is_operator = requires() { typename Ty::is_operator; };
    template<class Ty> concept is_range_kind = is_range<Ty> || is_partial_range<Ty>;

    /**
     * Links value to a variable.
     * @tparam Ty value type
     * @tparam Var variable
     */
    template<class Ty, is_var Var>
    struct named_value {
        using value_type = Ty;
        using define = info<Var>;
        using depend = depend<Ty>;

        [[no_unique_address]] value_type value;

        constexpr decltype(auto) evaluate(auto& tuple) const;
        constexpr decltype(auto) execute(auto&, auto& tuple) const;
    };

    template<class Ty> concept is_named_value = specialization<Ty, named_value> && !is_partial<Ty>;

    /**
     * Variable.
     * @tparam Name variable name
     */
    template<string_literal Name>
    struct var {
        constexpr static string_literal name = Name;

        using depend = info<var>;

        template<class Ty>
        constexpr auto operator=(Ty&& value) const {
            return named_value<decay_t<Ty>, var>{ std::forward<Ty>(value) };
        }

        constexpr decltype(auto) evaluate(auto& tuple) const {
            if constexpr (tuple.contains<var>) return tuple.get<var>();
            else return var{};
        }
    };

    /**
     * Specialization for ignored var "_".
     */
    template<>
    struct var<string_literal<2, char>{ "_" }> {
        constexpr static string_literal name = "_";

        using is_partial = int;
        using depend = info<>;

        constexpr dud evaluate(auto&) const { return dud{}; }
    };

    /**
     * Wrap reference into reference wrapper, otherwise decay.
     * @tparam Ty type
     */
    template<class Ty>
    using wrap_reference = std::conditional_t<
        lvalue_reference<Ty>,
        std::reference_wrapper<remove_reference_t<Ty>>,
        decay_t<Ty>>;

    /**
     * Tuple of named values.
     * @tparam ...Args named values
     */
    template<is_named_value ...Args>
    struct named_tuple {
        // Wrap lvalue references in reference wrappers, this allows for overriding 
        // what reference is stored in the tuple, instead of just assigning to the 
        // reference that was stored on construction.
        using tuple_type = std::tuple<wrap_reference<typename Args::value_type>...>;
        using define = concat_t<kaixo::define<Args>...>::unique;

        template<is_var Var>
        constexpr static bool contains = define::template occurs<Var>;

        tuple_type value{};

        constexpr named_tuple(Args&&...args) : value(args.value...) {}
        constexpr named_tuple(tuple_type&& val) : value(std::move(val)) {}
        constexpr named_tuple(const tuple_type& val) : value(val) {}

        /**
         * Get value linked to variable.
         * @tparam Var variable
         */
        template<is_var Var, class Self>
        constexpr decltype(auto) get(this Self&& self) {
            static_assert(contains<decay_t<Var>>, "Tuple does not contain variable");
            constexpr std::size_t index = define::template index<Var>;
            using type = typename info<Args...>::template element<index>::type::value_type;
            if constexpr (lvalue_reference<type>) {
                return std::get<index>(std::forward<Self>(self).value).get();
            } else {
                return std::move(std::get<index>(std::forward<Self>(self).value));
            }
        }
        
        /**
         * Assign value to variable.
         * @tparam Var variable
         * @param value value
         */
        template<is_var Var, class Self, class Ty>
        constexpr decltype(auto) set(this Self&& self, Ty&& value) {
            static_assert(contains<decay_t<Var>>, "Variable is not part of tuple");
            constexpr std::size_t index = define::template index<Var>;
            std::get<index>(std::forward<Self>(self).value) = std::forward<Ty>(value);
        }

        /**
         * Assign another named tuple, either contains all variables of
         * this named tuple, or it contains none of them. Does not handle the
         * case where some variables are already in this tuple, and some are not.
         * @param val other named tuple
         */
        template<class Self, is_named_value ...Tys>
        constexpr decltype(auto) assign(this Self&& self, named_tuple<Tys...>&& val) {
            if constexpr ((contains<typename kaixo::define<Tys>::type>&& ...)) {
                ((std::forward<Self>(self).set<typename kaixo::define<Tys>::type>(
                    std::move(val).get<typename kaixo::define<Tys>::type>())), ...);
                return std::forward<Self>(self);
            } else {
                return named_tuple<Args..., Tys...>{
                    std::tuple_cat(std::forward<Self>(self).value, std::move(val).value)
                };
            }
        }
    };

    template<class Ty> concept is_named_tuple = specialization<Ty, named_tuple>;

    /**
     * Has an evaluate function that works for a certain tuple.
     */
    template<class A, class Tuple>
    concept has_evaluate_for = requires(A & a, Tuple & tuple) { { a.evaluate(tuple) }; };

    /**
     * Evaluate a variable in an expression.
     * @param var value
     * @param tuple named tuple
     */
    template<class A>
    constexpr decltype(auto) evaluate(A&& val, is_named_tuple auto& tuple) {
        if constexpr (has_evaluate_for<decay_t<A>, decay_t<decltype(tuple)>>)
            return val.evaluate(tuple);
        else return std::forward<A>(val);
    }

    template<class Ty, is_var Var>
    constexpr decltype(auto) named_value<Ty, Var>::execute(auto&, auto& tuple) const {
        return tuple.assign(named_tuple{ Var{} = evaluate(tuple) });
    }

    template<class Ty, is_var Var>
    constexpr decltype(auto) named_value<Ty, Var>::evaluate(auto& tuple) const {
        return kaixo::evaluate(value, tuple);
    }

    /**
     * Valid expression parts, one must be a partial value
     * and the rest may not be an operator or a range of any kind.
     */
    template<class ...As>
    concept are_valid_expression = (is_partial<decay_t<As>> || ...)
        && ((!is_range_kind<decay_t<As>> && !is_operator<decay_t<As>>) && ...);

    /**
     * Are valid arguments to an overloaded function for 
     * partial values. Means at least one of the values is partial.
     */
    template<class ...As>
    concept are_valid_arguments = (is_partial<decay_t<As>> || ...);

    /**
     * Binary operator expression.
     * @tparam A first type
     * @tparam B second type
     * @tparam Op operator
     */
    template<class A, class B, is_operator Op>
        requires are_valid_expression<A, B>
    struct binary_operation {
        using depend = concat_t<depend<A>, depend<B>>::unique;

        [[no_unique_address]] A a{};
        [[no_unique_address]] B b{};

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            return Op::evaluate(
                kaixo::evaluate(std::forward<Self>(self).a, tuple),
                kaixo::evaluate(std::forward<Self>(self).b, tuple));
        }
    };

    /**
     * Unary operator expression.
     * @tparam A first type
     * @tparam Op operator
     */
    template<is_partial A, is_operator Op>
    struct unary_operation {
        using depend = depend<A>;

        [[no_unique_address]] A a{};

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            return Op::evaluate(kaixo::evaluate(std::forward<Self>(self).a, tuple));
        }
    };

    /**
     * Tuple expression, when evaluated creates a tuple.
     * @tparam ...As values, expression, or variables.
     */
    template<class ...As>
        requires are_valid_expression<As...>
    struct tuple_operation {
        using depend = concat_t<depend<As>...>::unique;

        std::tuple<std::conditional_t<lvalue_reference<As>, As, decay_t<As>>...> parts;

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            auto res = [&]<std::size_t ...Is>(std::index_sequence<Is...>) {
                return ref_tuple(kaixo::evaluate(std::get<Is>(std::forward<Self>(self).parts), tuple)...);
            }(std::index_sequence_for<As...>{});
            // Check whether the resulting tuple still has partial values.
            if constexpr (as_info<decltype(res)>::template count_filter<[]<is_partial>{}> != 0)
                return move_tparams_t<decltype(res), tuple_operation>{ std::move(res) };
            else return res;
        }
    };

    /**
     * Tuple of vars, used in deconstruction of tuples, and 
     * in resulting expressions to yield a tuple.
     */
    template<is_var ...As> 
    struct var_tuple {
        using depend = concat_t<depend<As>...>::unique;

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            return tuple_operation<As...>{ std::tuple{ As{}... } }.evaluate(tuple);
        }
    };

    template<class Ty> concept is_var_tuple = specialization<Ty, var_tuple>;

    /**
     * All operators to construct the unevaluated expression objects.
     */
    namespace operators {
        // Variable pack operators
        template<is_var A, is_var B> constexpr auto operator,(const A&, const B&) { return var_tuple<A, B>{}; }
        template<is_var A, is_var ...Bs> constexpr auto operator,(var_tuple<Bs...>, const A&) { return var_tuple<Bs..., A>{}; }
        template<is_var A, is_var ...Bs> constexpr auto operator,(const A&, var_tuple<Bs...>) { return var_tuple<A, Bs...>{}; }

        // Tuple expression operators
        template<class A, class B>
            requires are_valid_expression<A, B>
        constexpr auto operator,(A&& a, B&& b) {
            return tuple_operation<decay_t<A>, decay_t<B>>{ std::tuple{ std::forward<A>(a), std::forward<B>(b) } };
        }

        template<class A, class ...Bs>
            requires are_valid_expression<Bs...>
        constexpr auto operator,(A&& a, tuple_operation<Bs...>&& b) {
            return tuple_operation<decay_t<A>, Bs...>{ std::tuple_cat(std::forward_as_tuple(std::forward<A>(a)), b.parts) };
        }

        template<class A, class ...Bs>
            requires are_valid_expression<Bs...>
        constexpr auto operator,(tuple_operation<Bs...>&& b, A&& a) {
            return tuple_operation<Bs..., decay_t<A>>{ std::tuple_cat(std::move(b).parts, std::forward_as_tuple(std::forward<A>(a))) };
        }

#define KAIXO_BINARY_OPERATOR(name, op)                              \
        struct name {                                                \
            using is_operator = int;                                 \
            template<class A, class B>                               \
            constexpr static decltype(auto) evaluate(A&& a, B&& b) { \
                return a op b;                                       \
            }                                                        \
        };                                                                                             \
                                                                                                       \
        template<class A, class B> requires are_valid_expression<A, B>                                 \
        constexpr binary_operation<decay_t<A>, decay_t<B>, name> operator op(A&& a, B&& b) {                 \
            return binary_operation<decay_t<A>, decay_t<B>, name>{ std::forward<A>(a), std::forward<B>(b) }; \
        }

#define KAIXO_UNARY_OPERATOR(name, op)                        \
        struct name {                                         \
            using is_operator = int;                          \
            template<class A>                                 \
            constexpr static decltype(auto) evaluate(A&& a) { \
                return op std::forward<A>(a);                 \
            }                                                 \
        };                                                                  \
                                                                            \
        template<is_partial A>                                              \
        constexpr unary_operation<decay_t<A>, name> operator op(A&& a) {    \
            return unary_operation<decay_t<A>, name>{ std::forward<A>(a) }; \
        }

        KAIXO_UNARY_OPERATOR(increment, ++);
        KAIXO_UNARY_OPERATOR(decrement, --);
        KAIXO_UNARY_OPERATOR(boolean_not, !);
        KAIXO_UNARY_OPERATOR(bitwise_not, ~);

        KAIXO_BINARY_OPERATOR(add, +);
        KAIXO_BINARY_OPERATOR(subtract, -);
        KAIXO_BINARY_OPERATOR(multiply, *);
        KAIXO_BINARY_OPERATOR(divide, / );
        KAIXO_BINARY_OPERATOR(modulo, %);
        KAIXO_BINARY_OPERATOR(less_than, < );
        KAIXO_BINARY_OPERATOR(less_or_equal, <= );
        KAIXO_BINARY_OPERATOR(greater_than, > );
        KAIXO_BINARY_OPERATOR(greater_or_equal, >= );
        KAIXO_BINARY_OPERATOR(equal, == );
        KAIXO_BINARY_OPERATOR(not_equal, != );
        KAIXO_BINARY_OPERATOR(left_shift, << );
        KAIXO_BINARY_OPERATOR(right_shift, >> );
        KAIXO_BINARY_OPERATOR(boolean_and, &&);
        KAIXO_BINARY_OPERATOR(boolean_or, || );
        KAIXO_BINARY_OPERATOR(bitwise_and, &);
        KAIXO_BINARY_OPERATOR(bitwise_or, | );
        KAIXO_BINARY_OPERATOR(bitwise_xor, ^);
        KAIXO_BINARY_OPERATOR(spaceship, <=> );
        KAIXO_BINARY_OPERATOR(add_assign, +=);
        KAIXO_BINARY_OPERATOR(subtract_assign, -=);
        KAIXO_BINARY_OPERATOR(multiply_assign, *=);
        KAIXO_BINARY_OPERATOR(divide_assign, /=);
        KAIXO_BINARY_OPERATOR(modulo_assign, %=);
        KAIXO_BINARY_OPERATOR(left_shift_assign, <<=);
        KAIXO_BINARY_OPERATOR(right_shift_assign, >>=);
        KAIXO_BINARY_OPERATOR(and_assign, &=);
        KAIXO_BINARY_OPERATOR(or_assign, |=);
        KAIXO_BINARY_OPERATOR(xor_assign, ^=);
    }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                        List Comprehension

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

namespace kaixo {

    template<class Range> struct range_wrapper;

    // Owning range
    template<is_range Range>
        requires (!lvalue_reference<Range>)
    struct range_wrapper<Range> {
        using range_type = decay_t<Range>;

        range_type value;

        using value_type = std::ranges::range_value_t<const range_type>;
        using reference = std::ranges::range_reference_t<const range_type>;
        using iterator = std::ranges::iterator_t<const range_type>;

        constexpr iterator begin() const { return value.begin(); }
        constexpr iterator end() const { return value.end(); }
    };
    
    // Reference to range
    template<is_range Range>
        requires lvalue_reference<Range>
    struct range_wrapper<Range> {
        using range_type = remove_reference_t<Range>;

        std::reference_wrapper<range_type> value;

        using value_type = std::ranges::range_value_t<range_type>;
        using reference = std::ranges::range_reference_t<range_type>;
        using iterator = std::ranges::iterator_t<range_type>;

        constexpr iterator begin() const { return value.get().begin(); }
        constexpr iterator end() const { return value.get().end(); }
    };

    template<class Ty> concept is_range_wrapper = specialization<Ty, range_wrapper>;

    template<is_range Range>
        requires (!is_range_wrapper<Range>)
    constexpr auto wrap_range(Range&& range) {
        return range_wrapper<Range&&>{ { std::forward<Range>(range) } };
    }
    
    template<class Range>
    constexpr decltype(auto) wrap_range(Range&& range) {
        return std::forward<Range>(range);
    }

    /**
     * Return code for executables in comprehension.
     */
    enum class return_code {
        none = 0, // Continue like normal
        stop = 1, // Stop iteration now, and set to end
        skip = 2, // Skip current values, and increment again
    };

    constexpr return_code choose_code(return_code a, return_code b) {
        return (return_code)std::max((int)a, (int)b);
    }

    /**
     * Tests if expression is executable with a certain named tuple.
     */
    template<class Ty, class Tuple>
    concept is_executable_with = requires(decay_t<Ty>&val, return_code & code, decay_t<Tuple>&tuple) {
        { val.execute(code, tuple) } -> is_named_tuple;
    };

    template<class E>
    constexpr decltype(auto) execute(E&& e, return_code& code, is_named_tuple auto& tuple) {
        if constexpr (is_executable_with<E, decltype(tuple)>) {
            return std::forward<E>(e).execute(code, tuple);
        } else if constexpr (std::convertible_to<decltype(evaluate(std::forward<E>(e), tuple)), bool>) {
            code = bool(evaluate(std::forward<E>(e), tuple)) ? return_code::none : return_code::skip;
            return tuple;
        }
    }

    /**
     * Get the complete type, given a named tuple.
     * @tparam R partial type
     * @tparam T named tuple
     */
    template<class R, is_named_tuple T>
    struct full_type : std::type_identity<R> {};

    template<is_partial R, is_named_tuple T> // Recurse on partial.
    struct full_type<R, T> : full_type<decltype(std::declval<R&>().evaluate(std::declval<T&>())), T> {};

    template<class R, is_named_tuple T>
    using full_type_t = full_type<R, T>::type;

    /**
     * Get the defined named values of type R.
     * @tparam R type to get defined named values of
     * @tparam T named tuple
     */
    template<class R, is_named_tuple T>
    struct defined_values : std::type_identity<T> {};

    template<class R, is_named_tuple T>
        requires is_executable_with<R, T>
    struct defined_values<R, T> : std::type_identity<decay_t<decltype(
        std::declval<R&>().execute(std::declval<return_code&>(), std::declval<T&>()))>> {};

    template<is_range R, is_named_tuple T>
    struct defined_values<R, T> : std::type_identity<prepend_t<as_info<T>, typename R::reference>> {};
    
    template<is_partial_range R, is_named_tuple T>
    struct defined_values<R, T> : defined_values<full_type_t<R, T>, T> {};

    template<class R, is_named_tuple T>
    using defined_values_t = typename defined_values<R, T>::type;

    /**
     * Get the type of the named tuple given all parts.
     * @tparam Tuple named tuple
     * @tparam Parts parts
     */
    template<is_named_tuple Tuple, class Parts>
    struct named_tuple_type;

    template<is_named_tuple Tuple, class Part, class ...Parts>
    struct named_tuple_type<Tuple, info<Part, Parts...>>
        : named_tuple_type<defined_values_t<Part, Tuple>, info<Parts...>> {};

    template<is_named_tuple Tuple, class Part>
    struct named_tuple_type<Tuple, info<Part>> {
        using type = defined_values_t<Part, Tuple>;
    };

    template<class ...Parts>
    using named_tuple_type_t = named_tuple_type<named_tuple<>, info<Parts...>>::type;

    /**
     * Get the intermediate value of type R. For every part this is a
     * reference to the original part, except for a partial range, where
     * for every iteration a new instance is generated, so it's stored by value.
     * @tparam R type to get intermediate value of
     * @tparam T named tuple
     */
    template<class R, is_named_tuple T>
    struct intermediate_value : std::type_identity<dud> {};

    template<is_partial_range R, is_named_tuple T>
    struct intermediate_value<R, T> : std::type_identity<std::optional<full_type_t<R, T>>> {};

    template<class R, is_named_tuple T>
    using intermediate_value_t = intermediate_value<R, T>::type;

    /**
     * Get the iterator type of a range, given a named tuple.
     * @tparam R (partial) range
     * @tparam T named tuple
     */
    template<class R, is_named_tuple T>
    struct iterator_data : std::type_identity<dud> {};

    template<is_range R, is_named_tuple T> // Normal range, just get its iterator type.
    struct iterator_data<R, T> : std::type_identity<std::ranges::iterator_t<R>> {};

    template<is_partial_range R, is_named_tuple T>
    struct iterator_data<R, T> : iterator_data<full_type_t<R, T>, T> {};

    template<class R, is_named_tuple T>
    using iterator_data_t = iterator_data<R, T>::type;

    /**
     * Flatten a tuple containing tuples.
     * @param arg value
     */
    template<class Ty>
    constexpr decltype(auto) flatten_tuple(Ty&& arg) { 
        return std::forward_as_tuple(std::forward<Ty>(arg)); 
    }

    template<specialization<std::pair> Tuple>
    constexpr decltype(auto) flatten_tuple(Tuple&& tuple) {
        return std::tuple_cat(
            flatten_tuple(std::forward<Tuple>(tuple).first),
            flatten_tuple(std::forward<Tuple>(tuple).second));
    }

    template<specialization<std::tuple> Tuple>
    constexpr decltype(auto) flatten_tuple(Tuple&& tuple) {
        return sequence<as_info<decay_t<Tuple>>::size>([&]<std::size_t ...Is>() {
            return std::tuple_cat(
                flatten_tuple(std::get<Is>(std::forward<Tuple>(tuple)))...);
        });
    }

    /**
     * Flatten a tuple type containing tuples.
     */
    template<class Ty>
    using flatten_tuple_type_t = decltype(flatten_tuple(std::declval<Ty>()));

    /**
     * Determine the reference type of a named range based on
     * provided variables and the range.
     * @tparam Range range
     * @tparam ...Vars defined variables
     */
    template<is_range Range, is_var ...Vars>
    struct determine_named_range_reference;

    template<is_range Range, is_var Var>
    struct determine_named_range_reference<Range, Var>
        : std::type_identity<named_tuple<named_value<std::ranges::range_reference_t<Range>, Var>>>{
        constexpr static bool flatten = false;
    };

    template<class, class, class>
    struct zip_as_named_tuple;
    template<template<class...> class R, class ...Tys, is_var ...Vars, class Q>
    struct zip_as_named_tuple<R<Tys...>, info<Vars...>, Q>
        : std::type_identity<named_tuple<named_value<add_cvref_t<Tys, Q>, Vars>...>> {};

    template<is_range Range, is_var ...Vars>
        requires (as_info<std::ranges::range_reference_t<Range>>::size == sizeof...(Vars))
    struct determine_named_range_reference<Range, Vars...>
        : zip_as_named_tuple<decay_t<std::ranges::range_reference_t<Range>>, info<Vars...>, std::ranges::range_reference_t<Range>> {
        constexpr static bool flatten = false;
    };
    
    template<is_range Range, is_var ...Vars>
        requires (as_info<std::ranges::range_reference_t<Range>>::size != sizeof...(Vars)
        && as_info<flatten_tuple_type_t<std::ranges::range_reference_t<Range>>>::size == sizeof...(Vars))
    struct determine_named_range_reference<Range, Vars...> : zip_as_named_tuple<
        flatten_tuple_type_t<std::ranges::range_reference_t<Range>>, info<Vars...>, std::ranges::range_reference_t<Range>> {
        constexpr static bool flatten = true;
    };

    template<class Range, is_var ...Vars> struct named_range;
    template<class T, class ...V> named_range(const T&, const V&...) -> named_range<T, V...>;
    template<class T, class ...V> named_range(T&&, const V&...) -> named_range<T, V...>;

    /**
     * Range linked to variables.
     * @tparam Range range
     * @tparam ...Vars variables
     */
    template<is_range Range, is_var ...Vars>
    struct named_range<Range, Vars...> {
        using define = info<Vars...>;

        using range_type = Range;
        using _range_info = determine_named_range_reference<Range, Vars...>;
        using reference = _range_info::type;
        using value_type = decay_t<reference>;

        [[no_unique_address]] Range rng;

        constexpr named_range(Range&& range, const Vars&...) : rng(std::move(range)) {}
        constexpr named_range(const Range& range, const Vars&...) : rng(range) {}

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using reference = reference;
            using value_type = value_type;

            std::ranges::iterator_t<const Range> it{};

            constexpr iterator& operator++() { ++it; return *this; }
            constexpr iterator operator++(int) { iterator b = *this; operator++(); return b; }

            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr bool operator==(const iterator& other) const { return it == other.it; }

            constexpr reference operator*() {
                if constexpr (_range_info::flatten) 
                    return reference{ flatten_tuple(*it) };
                else return reference{ *it };
            }
        };

        constexpr iterator begin() const { return iterator{ std::ranges::begin(rng) }; }
        constexpr iterator end() const { return iterator{ std::ranges::end(rng) }; }
    };

    /**
     * Partial range linked to variables, still depends on variables.
     * @tparam Range range
     * @tparam ...Vars variables
     */
    template<is_partial Range, is_var ...Vars>
    struct named_range<Range, Vars...> {
        using is_range = int;

        using define = info<Vars...>;
        using depend = depend<Range>;

        [[no_unique_address]] Range rng;

        constexpr named_range(Range&& range, const Vars&...) : rng(std::move(range)) {}
        constexpr named_range(const Range& range, const Vars&...) : rng(range) {}

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            return kaixo::named_range{
                wrap_range(kaixo::evaluate(std::forward<Self>(self).rng, tuple)), Vars{}... 
            };
        }
    };

    /**
     * List comprehension object.
     * @tparam R result expression
     * @tparam ...Parts comprehension parts
     */
    template<class R, class ...Parts>
    struct list_comprehension {
        using reference = std::conditional_t<is_partial<R>, 
            decltype(evaluate(std::declval<R&>(), std::declval<named_tuple_type_t<Parts...>&>())), R>;
        using value_type = decay_t<reference>;

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = value_type;
            using reference = reference;

            constexpr iterator() : at_end(true) {}
            constexpr iterator(const list_comprehension& self) : self(&self) { prepare(); }

            constexpr iterator& operator++() { increment<sizeof...(Parts) - 1>(); return *this; }
            constexpr iterator operator++(int) { iterator b = *this; operator++(); return b; }

            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr bool operator==(const iterator& other) const {
                return end() == true && other.end() == true // If end, iterators don't matter.
                    || other.end() == end() && other.iterators == iterators;
            }

            constexpr reference operator*() {
                if (end()) throw; // Can't access past end
                return kaixo::evaluate(self->result, values.value());
            }

        private:
            using named_tuple_type = named_tuple_type_t<Parts...>;
            using iterator_datas = std::tuple<iterator_data_t<Parts, named_tuple_type>...>;
            using intermediate_values = std::tuple<intermediate_value_t<Parts, named_tuple_type>...>;

            intermediate_values intermediate{};
            iterator_datas iterators{};
            std::optional<named_tuple_type> values{};
            const list_comprehension* self = nullptr;
            bool at_end = false;
          
            constexpr bool end() const { return at_end; }
            constexpr void set_end() { at_end = true; }

            template<std::size_t I>
            constexpr void increment() {
                using type = info<Parts...>::template element<I>::type;
                if constexpr (is_range_kind<type>) {
                    auto& part = std::get<I>(self->parts);
                    auto& intr = std::get<I>(intermediate);
                    auto& iter = std::get<I>(iterators);

                    auto at_end = [&] {
                        if constexpr (is_partial_range<type>)
                            return ++iter == std::ranges::end(intr.value());
                        else return ++iter == std::ranges::end(part);
                    };

                    auto to_begin = [&] {
                        if constexpr (is_partial_range<type>) {
                            intr = evaluate(part, values.value());
                            iter = std::ranges::begin(intr.value());
                        } else iter = std::ranges::begin(part);
                    };

                    do {
                        if (at_end()) {
                            if constexpr (I != 0) increment<I - 1>();
                            to_begin();
                            if constexpr (I == 0) { set_end(); return; }
                        }

                        values.value().assign(*iter);

                        if constexpr (I != sizeof...(Parts) - 1) {
                            return_code code = evaluate_i<I + 1>();
                            if (end() || code == return_code::stop) { set_end(); return; }
                            else if (code == return_code::skip) continue;
                        }
                        return;
                    } while (true);
                }
                else if constexpr (I == 0) { set_end(); return; }// We're at the end!
                else return increment<I - 1>();
            }

            template<std::size_t I>
            constexpr return_code evaluate_i() {
                using type = info<Parts...>::template element<I>::type;
                
                if constexpr (!is_range_kind<type>) {
                    auto& part = std::get<I>(self->parts);

                    return_code code = return_code::none;
                    
                    execute(part, code, values.value());

                    if (code != return_code::none) return code;
                    if constexpr (I == sizeof...(Parts) - 1) return code;
                    else return evaluate_i<I + 1>();
                }
                else return return_code::none;
            }

            template<std::int64_t I, class Tuple>
            constexpr return_code initialize(Tuple&& cur_values, return_code code) {
                if constexpr (I == sizeof...(Parts)) {
                    values.emplace(std::forward<Tuple>(cur_values).value);
                    return code;
                } else {
                    using type = info<Parts...>::template element<I>::type;
                    auto& part = std::get<I>(self->parts);
                    if constexpr (is_range_kind<type>) {
                        auto& intr = std::get<I>(intermediate);
                        auto& iter = std::get<I>(iterators);

                        auto at_end = [&] {
                            if constexpr (is_partial_range<type>)
                                return iter == std::ranges::end(intr.value());
                            else return iter == std::ranges::end(part);
                        };

                        if constexpr (is_partial_range<type>) {
                            intr = evaluate(part, cur_values);
                            iter = std::ranges::begin(intr.value());
                        } else iter = std::ranges::begin(part);

                        if (at_end()) {
                            set_end();
                            return return_code::stop;
                        }

                        return initialize<I + 1>(std::forward<Tuple>(cur_values).assign(*iter), code);
                    } else {
                        return_code new_code = return_code::none;
                        using returned_tuple = decltype(execute(part, new_code, cur_values));
                        // If returns same tuple (did not add values), don't evaluate
                        // when already determined this initial value is invalid.
                        if constexpr (kaixo::reference<returned_tuple>) {
                            if (code != return_code::none) {
                                return initialize<I + 1>(std::forward<Tuple>(cur_values), code);
                            }
                        }
                        decltype(auto) res = execute(part, new_code, cur_values);
                        return initialize<I + 1>(res, choose_code(new_code, code));
                    }
                }
            }

            constexpr inline void prepare() {
                return_code _code = initialize<0>(named_tuple<>{}, return_code::none); // Set iterators to begin
                if (_code == return_code::skip) operator++();
                if (_code == return_code::stop) set_end();
            }
        };

        using const_iterator = iterator;

        [[no_unique_address]] R result;
        std::tuple<Parts...> parts;

        constexpr iterator begin() const { return iterator(*this); }
        constexpr iterator end() const { return iterator(); }

        constexpr bool empty() const { return begin() == end(); }

        constexpr reference operator[](std::size_t index) const {
            auto _it = begin();
            while (index > 0) ++_it, --index;
            return *_it;
        }
    };

    template<class R, class ...As> list_comprehension(R&&, std::tuple<As...>&&) -> list_comprehension<R, As...>;

    /**
     * Partial list comprehension, still misses some variables.
     * @tparam R result expression
     * @tparam ...Parts comprehension parts
     */
    template<class R, class ...Parts>
        requires (concat_t<depend<R>, depend<Parts>...>::unique
    ::template remove<typename concat_t<define<Parts>...>::unique>::size != 0)
    struct list_comprehension<R, Parts...> {
        using is_range = int;

        using depend = concat_t<depend<R>, depend<Parts>...>::unique
            ::template remove<typename concat_t<define<Parts>...>::unique>;

        [[no_unique_address]] R result;
        std::tuple<Parts...> parts;

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            return sequence<sizeof...(Parts)>([&]<std::size_t ...Is>() {
                return kaixo::list_comprehension{ // No more dependencies
                    kaixo::evaluate(std::forward<Self>(self).result, tuple),
                    std::make_tuple(std::move(kaixo::evaluate(std::get<Is>(std::forward<Self>(self).parts), tuple))...)
                };
            });
        }

        template<class Self, is_named_value ...Tys>
        constexpr decltype(auto) operator()(this Self&& self, Tys&& ...vals) {
            named_tuple tpl{ std::forward<Tys>(vals)... };
            return std::forward<Self>(self).evaluate(tpl);
        }
    };

    template<class Ty> concept is_lc = specialization<Ty, list_comprehension>;
    template<class Ty> concept is_partial_lc = is_lc<Ty> && is_partial<Ty>;

    namespace operators {
        constexpr decltype(auto) operator-(is_range auto& r) { return wrap_range(r); }
        constexpr decltype(auto) operator-(is_range auto&& r) { return wrap_range(std::move(r)); }
        constexpr decltype(auto) operator-(is_partial auto& r) { return r; }
        constexpr decltype(auto) operator-(is_partial auto&& r) { return std::move(r); }
        
        constexpr decltype(auto) operator<(is_var auto v, is_range auto&& r) {
            return named_range{ std::move(r), v }; 
        }

        constexpr decltype(auto) operator<(is_var auto v, is_partial auto& r) {
            return named_range{ std::move(r), v };
        }
        
        constexpr decltype(auto) operator<(is_var auto v, is_partial auto&& r) {
            return named_range{ std::move(r), v };
        }
        
        template<is_var ...Vars>
        constexpr decltype(auto) operator<(var_tuple<Vars...>, is_range auto&& r) {
            return named_range{ std::move(r), Vars{}... };
        }

        template<is_var ...Vars>
        constexpr decltype(auto) operator<(var_tuple<Vars...>, is_partial auto& r) {
            return named_range{ std::move(r), Vars{}... };
        }
        
        template<is_var ...Vars>
        constexpr decltype(auto) operator<(var_tuple<Vars...>, is_partial auto&& r) {
            return named_range{ std::move(r), Vars{}... };
        }

        template<class A, class B>
        constexpr decltype(auto) construct_lc(A&& a, B&& b) {
            return list_comprehension<decay_t<A>, decay_t<B>>{
                std::forward<A>(a), std::tuple{ std::forward<B>(b) }
            };
        };

        template<class Ty, class Part>
            requires (is_lc<Ty> || is_partial_lc<Ty>)
        constexpr auto operator,(Ty&& lc, Part&& part) {
            return list_comprehension{ std::forward<Ty>(lc).result, 
                std::tuple_cat(std::forward<Ty>(lc).parts, std::tuple(std::forward<Part>(part))) 
            };
        };
        
        template<is_partial A, class B>
        constexpr auto operator|(A&& a, B&& b) {
            return construct_lc(std::forward<A>(a), std::forward<B>(b));
        }
        
        template<is_var ...As, class B>
        constexpr auto operator|(var_tuple<As...>, B&& b) {
            return construct_lc(tuple_operation<As...>{ std::tuple{ As{}... } }, std::forward<B>(b));
        }
    }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                   List Comprehension Extensions

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

namespace kaixo {

    /**
     * Type for conveying infinity.
     */
    constexpr struct inf_t {} inf{};

    /**
     * A break expression stops the list comprehension 
     * when its contained expression evaluates to true.
     */
    template<class A>
    struct break_expression {
        using depend = depend<A>;

        [[no_unique_address]] A expr;

        template<class Self>
        constexpr auto evaluate(this Self&& self, is_named_tuple auto& tuple) {
            using result = decay_t<decltype(kaixo::evaluate(std::forward<Self>(self).expr, tuple))>;
            return break_expression<result>{ kaixo::evaluate(std::forward<Self>(self).expr, tuple) };
        }

        template<class Self>
        constexpr decltype(auto) execute(this Self&& self, return_code& code, is_named_tuple auto& tuple) {
            if (kaixo::evaluate(std::forward<Self>(self).expr, tuple)) code = return_code::stop;
            return tuple;
        }
    };

    constexpr struct break_t {
        template<class A>
        constexpr auto operator=(A&& a) const {
            return break_expression{ std::forward<A>(a) };
        }
    } brk{};


    /**
     * Zipped range for parallel iteration.
     */
    template<class ...As> struct zipped_range;
    template<class ...As> zipped_range(std::tuple<As...>&&) -> zipped_range<As...>;

    template<is_range ...As>
    struct zipped_range<As...> {
        using define = concat_t<define<As>...>;
        using depend = concat_t<depend<As>...>;

        using reference = std::tuple<std::ranges::range_reference_t<As>...>;
        using value_type = std::tuple<std::ranges::range_value_t<As>...>;

        std::tuple<As...> ranges;

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using reference = reference;
            using value_type = value_type;

            using iterators = std::tuple<std::ranges::iterator_t<As>...>;

            iterators iters{};

            constexpr iterator& operator++() {
                sequence<sizeof...(As)>([&]<std::size_t ...Is>() {
                    ((++std::get<Is>(iters)), ...);
                });
                return *this;
            }

            constexpr iterator operator++(int) {
                iterator b = *this;
                operator++();
                return b;
            }

            constexpr reference operator*() {
                return sequence<sizeof...(As)>([&]<std::size_t ...Is>() {
                    return reference{ std::forward_as_tuple((*std::get<Is>(iters))...) };
                });
            }

            constexpr bool operator==(const iterator& o) const {
                return sequence<sizeof...(As)>([&]<std::size_t ...Is>() {
                    return ((std::get<Is>(iters) == std::get<Is>(o.iters)) || ...);
                });
            }
        };

        constexpr iterator begin() const {
            return sequence<sizeof...(As)>([&]<std::size_t ...Is>() {
                return iterator{ std::make_tuple(std::ranges::begin(std::get<Is>(ranges))...) };
            }); 
        }

        constexpr iterator end() const {
            return sequence<sizeof...(As)>([&]<std::size_t ...Is>() {
                return iterator{ std::make_tuple(std::ranges::end(std::get<Is>(ranges))...) };
            }); 
        }
    };

    template<class Ty>
    concept is_zipped_range = specialization<Ty, zipped_range>;

    template<class ...As>
        requires (((is_partial<As> || is_range<As>) && ...) && (is_partial<As> || ...))
    struct zipped_range<As...> {
        using is_range = int;
        using depend = concat_t<depend<As>...>;

        std::tuple<As...> ranges;

        template<class Self>
        constexpr auto evaluate(this Self&& self, is_named_tuple auto& tuple) {
            return sequence<sizeof...(As)>([&]<std::size_t ...Is>() {
                return kaixo::zipped_range{
                    std::make_tuple(
                        wrap_range(kaixo::evaluate(std::get<Is>(std::forward<Self>(self).ranges), tuple))...)
                };
            });
        }
    };

    template<class Ty>
    concept is_partial_zipped_range = is_zipped_range<Ty> && is_partial<Ty>;

    namespace operators {
        template<class A, class B>
            requires ((is_range_kind<A> || is_range_kind<B>) 
                && !is_lc<A> && !is_partial_lc<A> 
                && !is_zipped_range<A> && !is_partial_zipped_range<A>)
        constexpr auto operator,(A&& a, B&& b) {
            return zipped_range{
                std::make_tuple(wrap_range(std::forward<A>(a)), wrap_range(std::forward<B>(b)))
            };
        }        
        
        template<class A, is_range_kind B>
            requires (is_zipped_range<A> || is_partial_zipped_range<A>)
        constexpr auto operator,(A&& a, B&& b) {
            return zipped_range{ 
                std::tuple_cat(std::forward<A>(a).ranges, std::make_tuple(wrap_range(std::forward<B>(b)))) 
            };
        }
    }

    /**
     * Simple range object, allows for variable 
     * dependent, and infinite ranges.
     */
    template<class ...As> struct range;

    template<arithmetic A> range(A, A)->range<A>;
    template<arithmetic A> range(A, inf_t)->range<A, inf_t>;
    template<is_partial A> range(A, inf_t)->range<decay_t<A>, inf_t>;
    template<is_partial A, arithmetic B> range(A, B)->range<decay_t<A>, B>;
    template<arithmetic A, is_partial B> range(A, B)->range<A, decay_t<B>>;
    template<is_partial A, is_partial B> range(A, B)->range<decay_t<A>, decay_t<B>>;

    /**
     * Default range between 2 numbers.
     */
    template<arithmetic Ty>
    struct range<Ty> {
        Ty a{};
        Ty b{};
        constexpr range(Ty a, Ty b) : a(a), b(b) {}

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = Ty;

            Ty value{};

            constexpr iterator& operator++() { ++value; return *this; }
            constexpr iterator operator++(int) { iterator b = *this; ++value; return b; }
            constexpr Ty operator*() { return value; }
            constexpr bool operator==(const iterator& b) const { return value == b.value; }
        };

        constexpr iterator begin() const { return { a }; }
        constexpr iterator end() const { return { std::max(b + 1, a) }; }
    };
    
    /**
     * Range from value to infinity.
     */
    template<arithmetic Ty>
    struct range<Ty, inf_t> {
        Ty a{};

        constexpr range(Ty a, inf_t) : a(a) {}

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = Ty;

            std::optional<Ty> value{};

            constexpr iterator& operator++() { ++value.value(); return *this; }
            constexpr iterator operator++(int) { iterator b = *this; ++value.value(); return b; }
            constexpr Ty operator*() { return value.value(); }
            constexpr bool operator==(const iterator& b) const { return value == b.value; }
        };

        constexpr iterator begin() const { return { a }; }
        constexpr iterator end() const { return {}; }
    };

    /**
     * Range from dependent variable to value.
     */
    template<is_partial A, arithmetic Ty>
    struct range<A, Ty> {
        using is_range = int;
        using depend = depend<A>;

        A a{};
        Ty b{};

        template<class T>
        constexpr range(T&& a, Ty b) : a(std::forward<T>(a)), b(b) {}

        constexpr auto evaluate(is_named_tuple auto& tuple) const {
            return kaixo::range{ (Ty)kaixo::evaluate(a, tuple), b};
        }
    };
        
    /**
     * Range from dependent variable to infinity.
     */
    template<is_partial A>
    struct range<A, inf_t> {
        using is_range = int;
        using depend = depend<A>;

        A a;

        template<class T>
        constexpr range(T&& a, inf_t) : a(std::forward<T>(a)) {}

        constexpr auto evaluate(is_named_tuple auto& tuple) const {
            return kaixo::range{ kaixo::evaluate(a, tuple), inf };
        }
    };

    /**
     * Range from value to dependent variable.
     */
    template<arithmetic Ty, is_partial B>
    struct range<Ty, B> {
        using is_range = int;
        using depend = depend<B>;

        Ty a{};
        B b{};

        template<class T>
        constexpr range(Ty a, T&& b) : a(a), b(std::forward<T>(b)) {}

        constexpr auto evaluate(is_named_tuple auto& tuple) const {
            return kaixo::range{ a, (Ty)kaixo::evaluate(b, tuple) };
        }
    };

    /**
     * Range from dependent variable to dependent variable.
     */
    template<is_partial A, is_partial B>
    struct range<A, B> {
        using is_range = int;
        using depend = concat_t<depend<A>, depend<B>>;

        A a;
        B b;

        template<class T, class Q>
        constexpr range(T&& a, Q&& b) : a(std::forward<T>(a)), b(std::forward<Q>(b)) {}

        constexpr auto evaluate(is_named_tuple auto& tuple) const {
            return kaixo::range{ kaixo::evaluate(a, tuple), kaixo::evaluate(b, tuple) };
        }
    };

    /**
     * Some often used names in list comprehensions.
     */
    namespace default_variables {
        constexpr var<"a"> a{};
        constexpr var<"b"> b{};
        constexpr var<"c"> c{};
        constexpr var<"d"> d{};
        constexpr var<"e"> e{};
        constexpr var<"f"> f{};
        constexpr var<"g"> g{};
        constexpr var<"h"> h{};
        constexpr var<"i"> i{};
        constexpr var<"j"> j{};
        constexpr var<"k"> k{};
        constexpr var<"l"> l{};
        constexpr var<"m"> m{};
        constexpr var<"n"> n{};
        constexpr var<"o"> o{};
        constexpr var<"p"> p{};
        constexpr var<"q"> q{};
        constexpr var<"r"> r{};
        constexpr var<"s"> s{};
        constexpr var<"t"> t{};
        constexpr var<"u"> u{};
        constexpr var<"v"> v{};
        constexpr var<"w"> w{};
        constexpr var<"x"> x{};
        constexpr var<"y"> y{};
        constexpr var<"z"> z{};
        constexpr var<"_"> _{};
    }

    /**
     * Extension for a list comprehension to insert values
     * into a range directly.
     * @tparam Range range, must be non-const reference
     * @tparam Ty expression
     */
    template<is_range Range, class Ty>
    struct range_inserter {
        using depend = depend<Ty>;
        
        std::reference_wrapper<Range> value;
        Ty expr;

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, auto& tuple) {
            return range_inserter{ value.get(), std::forward<Self>(self).expr };
        }

        template<class Self>
        constexpr decltype(auto) execute(this Self&& self, auto& code, auto& tuple) {
            using value_type = decay_t<Range>::value_type;
            using result_type = decltype(kaixo::evaluate(std::forward<Self>(self).expr, tuple));
            auto& container = std::forward<Self>(self).value.get();
            // Need a special case for value type of pair, as there's no conversion
            // operator between a tuple with 2 elements and a pair.
            if constexpr (!specialization<result_type, std::pair> && specialization<value_type, std::pair>) {
                container.insert(container.end(), tuple_to_pair(kaixo::evaluate(std::forward<Self>(self).expr, tuple)));
            } else {
                container.insert(container.end(), kaixo::evaluate(std::forward<Self>(self).expr, tuple));
            }
            return tuple;
        }
    };

    namespace operators {
        template<is_range Range, is_partial Ty>
        constexpr auto operator<<(Range& range, Ty&& expr) {
            return range_inserter<Range, decay_t<Ty>>{ range, std::forward<Ty>(expr) };
        }
    }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                    Standard Function Overloads

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

namespace kaixo {
    /**
     * Expression for overloaded function.
     * @tparam Fun function type
     * @tparam ...Args provided argument types
     */
    template<class Fun, class ...Args>
    struct overload_expression {
        using depend = concat_t<depend<Args>...>;

        [[no_unique_address]] Fun fun{};
        std::tuple<Args...> args;

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            using remaining = depend::template remove<kaixo::define<decltype(tuple)>>;
            if constexpr (remaining::size == 0) {
                return sequence<sizeof...(Args)>([&]<std::size_t ...Is>() {
                    return std::forward<Self>(self).fun(kaixo::evaluate(std::get<Is>(std::forward<Self>(self).args), tuple)...);
                });
            } else {
                return sequence<sizeof...(Args)>([&]<std::size_t ...Is>() {
                    return overload_expression{
                        std::forward<Self>(self).fun, 
                        std::make_tuple(kaixo::evaluate(std::get<Is>(std::forward<Self>(self).args), tuple)...)
                    };
                });
            }
        }
    };

#define lc_std_fun(y, x)                                                                 \
    template<class ...Args>                                                              \
        requires are_valid_arguments<Args...>                                            \
    constexpr auto x(Args&&... args) {                                                   \
        constexpr auto fun = []<class ...Args>(Args&&...args) -> decltype(auto) {        \
            return ::y x(std::forward<Args>(args)...);                                   \
        };                                                                               \
        return overload_expression{ fun, std::make_tuple(std::forward<Args>(args)...) }; \
    }
}

#ifndef KAIXO_LC_FUNCTIONAL
#define KAIXO_LC_FUNCTIONAL 1
#endif
#if KAIXO_LC_FUNCTIONAL == 1
#include <functional>
namespace kaixo {
    namespace overloads {
        lc_std_fun(std::, bind_front);
        lc_std_fun(std::, bind);
        lc_std_fun(std::, ref);
        lc_std_fun(std::, cref);
        lc_std_fun(std::, invoke);
    }
}
#endif

#ifndef KAIXO_LC_ANY
#define KAIXO_LC_ANY 1
#endif
#if KAIXO_LC_ANY == 1
#include <any>
namespace kaixo {
    namespace overloads {
        lc_std_fun(std::, any_cast);
        lc_std_fun(std::, make_any);
    }
}
#endif

#ifndef KAIXO_LC_ALGORITHMS
#define KAIXO_LC_ALGORITHMS 1
#endif
#if KAIXO_LC_ALGORITHMS == 1
#include <algorithm>
namespace kaixo {
    namespace overloads {
        lc_std_fun(std::, adjacent_find);
        lc_std_fun(std::, binary_search);
        lc_std_fun(std::, bsearch);
        lc_std_fun(std::, clamp);
        lc_std_fun(std::, copy_backward);
        lc_std_fun(std::, copy_n);
        lc_std_fun(std::, count);
        lc_std_fun(std::, count_if);
        lc_std_fun(std::, equal);
        lc_std_fun(std::, equal_range);
        lc_std_fun(std::, fill);
        lc_std_fun(std::, fill_n);
        lc_std_fun(std::, find);
        lc_std_fun(std::, find_end);
        lc_std_fun(std::, find_first_of);
        lc_std_fun(std::, find_if);
        lc_std_fun(std::, find_if_not);
        lc_std_fun(std::, for_each);
        lc_std_fun(std::, for_each_n);
        lc_std_fun(std::, generate);
        lc_std_fun(std::, generate_n);
        lc_std_fun(std::, includes);
        lc_std_fun(std::, inplace_merge);
        lc_std_fun(std::, iter_swap);
        lc_std_fun(std::, lexicographical_compare);
        lc_std_fun(std::, lower_bound);
        lc_std_fun(std::, make_heap);
        lc_std_fun(std::, max);
        lc_std_fun(std::, max_element);
        lc_std_fun(std::, merge);
        lc_std_fun(std::, min);
        lc_std_fun(std::, min_element);
        lc_std_fun(std::, minmax);
        lc_std_fun(std::, minmax_element);
        lc_std_fun(std::, mismatch);
        lc_std_fun(std::, move);
        lc_std_fun(std::, move_backward);
        lc_std_fun(std::, next_permutation);
        lc_std_fun(std::, nth_element);
        lc_std_fun(std::, partial_sort);
        lc_std_fun(std::, partial_sort_copy);
        lc_std_fun(std::, partition);
        lc_std_fun(std::, partition_copy);
        lc_std_fun(std::, partition_point);
        lc_std_fun(std::, pop_heap);
        lc_std_fun(std::, prev_permutation);
        lc_std_fun(std::, push_heap);
        lc_std_fun(std::, qsort);
        lc_std_fun(std::, remove);
        lc_std_fun(std::, remove_copy);
        lc_std_fun(std::, replace);
        lc_std_fun(std::, replace_copy);
        lc_std_fun(std::, replace_copy_if);
        lc_std_fun(std::, reverse);
        lc_std_fun(std::, reverse_copy);
        lc_std_fun(std::, rotate);
        lc_std_fun(std::, rotate_copy);
        lc_std_fun(std::, sample);
        lc_std_fun(std::, search);
        lc_std_fun(std::, search_n);
        lc_std_fun(std::, shift_left);
        lc_std_fun(std::, shift_right);
        lc_std_fun(std::, set_difference);
        lc_std_fun(std::, set_intersection);
        lc_std_fun(std::, set_symmetric_difference);
        lc_std_fun(std::, set_union);
        lc_std_fun(std::, sort);
        lc_std_fun(std::, sort_heap);
        lc_std_fun(std::, stable_partition);
        lc_std_fun(std::, stable_sort);
        lc_std_fun(std::, swap);
        lc_std_fun(std::, swap_ranges);
        lc_std_fun(std::, transform);
        lc_std_fun(std::, unique);
        lc_std_fun(std::, unique_copy);
        lc_std_fun(std::, upper_bound);
    }
}
#endif

#ifndef KAIXO_LC_ITERATOR
#define KAIXO_LC_ITERATOR 1
#endif
#if KAIXO_LC_ITERATOR == 1
#include <iterator>
namespace kaixo {
    namespace overloads {
        lc_std_fun(std::, advance);
        lc_std_fun(std::, back_inserter);
        lc_std_fun(std::, begin);
        lc_std_fun(std::, data);
        lc_std_fun(std::, distance);
        lc_std_fun(std::, empty);
        lc_std_fun(std::, end);
        lc_std_fun(std::, front_inserter);
        lc_std_fun(std::, inserter);
        lc_std_fun(std::, make_move_iterator);
        lc_std_fun(std::, make_reverse_iterator);
        lc_std_fun(std::, next);
        lc_std_fun(std::, prev);
        lc_std_fun(std::, rbegin);
        lc_std_fun(std::, rend);
        lc_std_fun(std::, size);
    }
}
#endif

#ifndef KAIXO_LC_MEMORY
#define KAIXO_LC_MEMORY 1
#endif
#if KAIXO_LC_MEMORY == 1
#include <memory>
#include <memory_resource>
namespace kaixo {
    namespace overloads {
        lc_std_fun(std::, addressof);
        lc_std_fun(std::, align);
        lc_std_fun(std::, assume_aligned);
        lc_std_fun(std::, calloc);
        lc_std_fun(std::, free);
        lc_std_fun(std::, malloc);
        lc_std_fun(std::, realloc);
        lc_std_fun(std::, destroy);
        lc_std_fun(std::, destroy_at);
        lc_std_fun(std::, destroy_n);
        lc_std_fun(std::pmr::, get_default_resource);
        lc_std_fun(std::, make_obj_using_allocator);
        lc_std_fun(std::pmr::, new_delete_resource);
        lc_std_fun(std::pmr::, null_memory_resource);
        lc_std_fun(std::pmr::, pool_options);
        lc_std_fun(std::pmr::, set_default_resource);
        lc_std_fun(std::, to_address);
        lc_std_fun(std::, uninitialized_construct_using_allocator);
        lc_std_fun(std::, uninitialized_copy);
        lc_std_fun(std::, uninitialized_copy_n);
        lc_std_fun(std::, uninitialized_default_construct);
        lc_std_fun(std::, uninitialized_default_construct_n);
        lc_std_fun(std::, uninitialized_fill);
        lc_std_fun(std::, uninitialized_fill_n);
        lc_std_fun(std::, uninitialized_move);
        lc_std_fun(std::, uninitialized_move_n);
        lc_std_fun(std::, uninitialized_value_construct);
        lc_std_fun(std::, uninitialized_value_construct_n);
    }
}
#endif

#ifndef KAIXO_LC_NUMERIC
#define KAIXO_LC_NUMERIC 1
#endif
#if KAIXO_LC_NUMERIC == 1
#include <numeric>
namespace kaixo {
    namespace overloads {
        lc_std_fun(std::, accumulate);
        lc_std_fun(std::, adjacent_difference);
        lc_std_fun(std::, inclusive_scan);
        lc_std_fun(std::, inner_product);
        lc_std_fun(std::, iota);
        lc_std_fun(std::, reduce);
        lc_std_fun(std::, partial_sum);
        lc_std_fun(std::, transform_exclusive_scan);
        lc_std_fun(std::, transform_inclusive_scan);
        lc_std_fun(std::, transform_reduce);

        lc_std_fun(std::, bit_cast);
        lc_std_fun(std::, gcd);
        lc_std_fun(std::, lcm);
        lc_std_fun(std::, lerp);
        lc_std_fun(std::, abs);
        lc_std_fun(std::, acos);
        lc_std_fun(std::, acosh);
        lc_std_fun(std::, asin);
        lc_std_fun(std::, asinh);
        lc_std_fun(std::, atan);
        lc_std_fun(std::, atan2);
        lc_std_fun(std::, atanh);
        lc_std_fun(std::, cbrt);
        lc_std_fun(std::, ceil);
        lc_std_fun(std::, copysign);
        lc_std_fun(std::, cos);
        lc_std_fun(std::, cosh);
        lc_std_fun(std::, div);
        lc_std_fun(std::, erf);
        lc_std_fun(std::, erfc);
        lc_std_fun(std::, exp);
        lc_std_fun(std::, exp2);
        lc_std_fun(std::, expm1);
        lc_std_fun(std::, fabs);
        lc_std_fun(std::, fdim);
        lc_std_fun(std::, floor);
        lc_std_fun(std::, fma);
        lc_std_fun(std::, fmax);
        lc_std_fun(std::, fmin);
        lc_std_fun(std::, fmod);
        lc_std_fun(std::, fpclassify);
        lc_std_fun(std::, frexp);
        lc_std_fun(std::, hypot);
        lc_std_fun(std::, ilogb);
        lc_std_fun(std::, isfinite);
        lc_std_fun(std::, isgreater);
        lc_std_fun(std::, isgreaterequal);
        lc_std_fun(std::, isinf);
        lc_std_fun(std::, isless);
        lc_std_fun(std::, islessequal);
        lc_std_fun(std::, islessgreater);
        lc_std_fun(std::, isnan);
        lc_std_fun(std::, isnormal);
        lc_std_fun(std::, isunordered);
        lc_std_fun(std::, ldexp);
        lc_std_fun(std::, lgamma);
        lc_std_fun(std::, log);
        lc_std_fun(std::, log10);
        lc_std_fun(std::, log1p);
        lc_std_fun(std::, log2);
        lc_std_fun(std::, logb);
        lc_std_fun(std::, modf);
        lc_std_fun(std::, nan);
        lc_std_fun(std::, nearbyint);
        lc_std_fun(std::, nextafter);
        lc_std_fun(std::, pow);
        lc_std_fun(std::, remainder);
        lc_std_fun(std::, remquo);
        lc_std_fun(std::, rint);
        lc_std_fun(std::, round);
        lc_std_fun(std::, scalbn);
        lc_std_fun(std::, signbit);
        lc_std_fun(std::, sin);
        lc_std_fun(std::, sinh);
        lc_std_fun(std::, sqrt);
        lc_std_fun(std::, tan);
        lc_std_fun(std::, tanh);
        lc_std_fun(std::, tgamma);
        lc_std_fun(std::, trunc);
        lc_std_fun(std::, midpoint);
        lc_std_fun(std::, assoc_laguerre);
        lc_std_fun(std::, assoc_legendre);
        lc_std_fun(std::, beta);
        lc_std_fun(std::, comp_ellint_1);
        lc_std_fun(std::, comp_ellint_2);
        lc_std_fun(std::, comp_ellint_3);
        lc_std_fun(std::, cyl_bessel_i);
        lc_std_fun(std::, cyl_bessel_j);
        lc_std_fun(std::, cyl_bessel_k);
        lc_std_fun(std::, cyl_neumann);
        lc_std_fun(std::, ellint_1);
        lc_std_fun(std::, ellint_2);
        lc_std_fun(std::, ellint_3);
        lc_std_fun(std::, expint);
        lc_std_fun(std::, hermite);
        lc_std_fun(std::, laguerre);
        lc_std_fun(std::, legendre);
        lc_std_fun(std::, riemann_zeta);
        lc_std_fun(std::, sph_bessel);
        lc_std_fun(std::, sph_legendre);
        lc_std_fun(std::, sph_neumann);
    }
}
#endif

#ifndef KAIXO_LC_STRING
#define KAIXO_LC_STRING 1
#endif
#if KAIXO_LC_STRING == 1
#include <string>
#include <cstring>
#include <cwctype>
#include <cuchar>
namespace kaixo {
    namespace overloads {
        lc_std_fun(std::, atof);
        lc_std_fun(std::, atoi);
        lc_std_fun(std::, isalnum);
        lc_std_fun(std::, isalpha);
        lc_std_fun(std::, isblank);
        lc_std_fun(std::, iscntrl);
        lc_std_fun(std::, isdigit);
        lc_std_fun(std::, isgraph);
        lc_std_fun(std::, islower);
        lc_std_fun(std::, isprint);
        lc_std_fun(std::, ispunct);
        lc_std_fun(std::, isspace);
        lc_std_fun(std::, isupper);
        lc_std_fun(std::, isxdigit);
        lc_std_fun(std::, memchr);
        lc_std_fun(std::, memcmp);
        lc_std_fun(std::, memcpy);
        lc_std_fun(std::, memmove);
        lc_std_fun(std::, memset);
        lc_std_fun(std::, strcat);
        lc_std_fun(std::, strchr);
        lc_std_fun(std::, strcmp);
        lc_std_fun(std::, strcoll);
        lc_std_fun(std::, strcpy);
        lc_std_fun(std::, strcspn);
        lc_std_fun(std::, strerror);
        lc_std_fun(std::, strlen);
        lc_std_fun(std::, strncat);
        lc_std_fun(std::, strncmp);
        lc_std_fun(std::, strncpy);
        lc_std_fun(std::, strpbrk);
        lc_std_fun(std::, strrchr);
        lc_std_fun(std::, strspn);
        lc_std_fun(std::, strstr);
        lc_std_fun(std::, strtof);
        lc_std_fun(std::, strtok);
        lc_std_fun(std::, strtol);
        lc_std_fun(std::, strtoul);
        lc_std_fun(std::, strxfrm);
        lc_std_fun(std::, tolower);
        lc_std_fun(std::, toupper);
        lc_std_fun(std::, copy);
        lc_std_fun(std::, btowc);
        lc_std_fun(std::, c16rtomb);
        lc_std_fun(std::, c32rtomb);
        lc_std_fun(std::, mblen);
        lc_std_fun(std::, mbrlen);
        lc_std_fun(std::, mbrtoc16);
        lc_std_fun(std::, mbrtoc32);
        lc_std_fun(std::, mbrtowc);
        lc_std_fun(std::, mbsinit);
        lc_std_fun(std::, mbsrtowcs);
        lc_std_fun(std::, mbstowcs);
        lc_std_fun(std::, mbtowc);
        lc_std_fun(std::, wcrtomb);
        lc_std_fun(std::, wcsrtombs);
        lc_std_fun(std::, wcstombs);
        lc_std_fun(std::, wctob);
        lc_std_fun(std::, wctomb);
        lc_std_fun(std::, iswalnum);
        lc_std_fun(std::, iswalpha);
        lc_std_fun(std::, iswblank);
        lc_std_fun(std::, iswcntrl);
        lc_std_fun(std::, iswctype);
        lc_std_fun(std::, iswdigit);
        lc_std_fun(std::, iswgraph);
        lc_std_fun(std::, iswlower);
        lc_std_fun(std::, iswprint);
        lc_std_fun(std::, iswpunct);
        lc_std_fun(std::, iswspace);
        lc_std_fun(std::, iswupper);
        lc_std_fun(std::, iswxdigit);
        lc_std_fun(std::, towctrans);
        lc_std_fun(std::, towlower);
        lc_std_fun(std::, towupper);
        lc_std_fun(std::, wcscat);
        lc_std_fun(std::, wcschr);
        lc_std_fun(std::, wcscmp);
        lc_std_fun(std::, wcscoll);
        lc_std_fun(std::, wcscpy);
        lc_std_fun(std::, wcscspn);
        lc_std_fun(std::, wcslen);
        lc_std_fun(std::, wcsncat);
        lc_std_fun(std::, wcsncmp);
        lc_std_fun(std::, wcsncpy);
        lc_std_fun(std::, wcspbrk);
        lc_std_fun(std::, wcsrchr);
        lc_std_fun(std::, wcsspn);
        lc_std_fun(std::, wcsstr);
        lc_std_fun(std::, wcstof);
        lc_std_fun(std::, wcstok);
        lc_std_fun(std::, wcstol);
        lc_std_fun(std::, wcstoul);
        lc_std_fun(std::, wcsxfrm);
        lc_std_fun(std::, wctrans);
        lc_std_fun(std::, wctype);
        lc_std_fun(std::, wmemchr);
        lc_std_fun(std::, wmemcmp);
        lc_std_fun(std::, wmemcpy);
        lc_std_fun(std::, wmemmove);
        lc_std_fun(std::, wmemset);
    }
}
#endif

#undef lc_std_fun
#undef lc_mem_fun
