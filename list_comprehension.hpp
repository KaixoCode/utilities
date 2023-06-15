#pragma once
#include <type_traits>
#include <concepts>
#include <cstddef>
#include <utility>
#include <algorithm>
#include "type_utils.hpp"

namespace kaixo {

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                          Unevaluated Expressions

     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    namespace ranges = std::ranges;
    namespace views = std::views;

    template<class Ty> concept is_range = ranges::range<Ty>;

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

    template<class Ty>
    concept is_dependent = requires (Ty) { typename decay_t<Ty>::depend; typename decay_t<Ty>::define; };

    template<class Ty> using depend = grab::depend<decay_t<Ty>>;
    template<class Ty> using define = grab::define<decay_t<Ty>>;

    template<class ...> struct expression;
    template<class Ty> concept is_var = is_dependent<decay_t<Ty>> && requires() { { decay_t<Ty>::name }; };
    template<class Ty> concept is_expression = specialization<decay_t<Ty>, expression>;
    template<class Ty> concept is_operator = requires() { typename Ty::is_operator; };
    template<class Ty> concept is_partial_range = !is_range<Ty> && requires() { typename Ty::is_partial_range; };
    template<class Ty> concept is_range_kind = is_range<Ty> || is_partial_range<Ty>;
    template<class Ty> concept is_dependent_range = has::range_v<Ty> && is_dependent<Ty>;
    template<class Ty> concept is_varexpr = is_expression<Ty> || is_var<Ty>;

    /**
     * Links value to a variable.
     * @tparam Ty value type
     * @tparam Var variable
     */
    template<class Ty, is_var Var>
    struct named_value {
        using value_type = Ty;
        using var = Var;

        value_type value;
    };

    // Is type a named value.
    template<class Ty> concept is_named_value = specialization<Ty, named_value>;

    /**
     * Variable.
     * @tparam Name variable name
     */
    template<string_literal Name>
    struct var_t {
        using define = info<>;
        using depend = info<var_t>;

        constexpr static string_literal name = Name;

        template<class Ty>
        constexpr named_value<decay_t<Ty>, var_t> operator=(Ty&& value) const {
            return { std::forward<Ty>(value) };
        }
    };

    /**
     * Tuple of named values.
     * @tparam ...Args named values
     */
    template<is_named_value ...Args>
    struct named_tuple {
        using tuple = std::tuple<typename Args::value_type...>;
        using vars = info<typename Args::var...>;

        tuple value{};

        constexpr named_tuple(Args&&...args) : value(args.value...) {}

        /**
         * Get value linked to variable.
         * @tparam Var variable
         */
        template<is_var Var, class Self>
        constexpr decltype(auto) get(this Self&& self) {
            static_assert(vars::template occurs<decay_t<Var>>, "Variable is not part of tuple");
            constexpr std::size_t index = vars::template index<Var>;
            return std::get<index>(std::forward<Self>(self).value);
        }

        template<is_var Var>
        constexpr static bool contains = vars::template occurs<Var>;
    };

    // Is type a named value.
    template<class Ty> concept is_named_tuple = specialization<Ty, named_tuple>;

    /**
     * Evaluate a variable in an expression. When passed tuple contains
     * the variable, it's extracted from the tuple. Otherwise variable is returned.
     * @param var variable
     * @param tuple named tuple
     */
    template<is_var A, is_named_tuple Ty>
    constexpr decltype(auto) eval_in_expression(A&& var, Ty& tuple) {
        if constexpr (decay_t<Ty>::vars::template occurs<decay_t<A>>) return tuple.get<decay_t<A>>();
        else return (var);
    }

    /**
     * Evaluate an expression in an expression. Just recurses to the
     * expression's own 'eval' function.
     * @param value expression
     * @param tuple named tuple
     */
    template<is_expression A, is_named_tuple Ty>
    constexpr decltype(auto) eval_in_expression(A&& value, Ty& tuple) {
        return std::forward<A>(value).eval(tuple);
    }

    /**
     * Evaluate any other value in an expression, just returns value itself.
     * @param value value
     * @param tuple named tuple
     */
    template<class A, is_named_tuple Ty>
        requires (!is_varexpr<A>)
    constexpr A&& eval_in_expression(A&& value, Ty& tuple) {
        return std::forward<A>(value);
    }

    // Valid expression parts, one must be an expression or variable
    // and the rest may not be an operator or a range of any kind.
    template<class ...As>
    concept are_valid_expression = (is_varexpr<decay_t<As>> || ...)
        && ((!is_range_kind<decay_t<As>> && !is_operator<decay_t<As>>) && ...);

    /**
     * Binary operator expression.
     * @tparam A first type
     * @tparam B second type
     * @tparam Op operator
     */
    template<class A, class B, is_operator Op> requires are_valid_expression<A, B>
    struct expression<A, B, Op> {
        using define = concat_t<define<A>, define<B>>::unique;
        using depend = concat_t<depend<A>, depend<B>>::unique;

        [[no_unique_address]] A a{};
        [[no_unique_address]] B b{};

        template<class Self>
        constexpr decltype(auto) eval(this Self&& self, is_named_tuple auto& tuple) {
            return Op::eval(
                eval_in_expression(std::forward<Self>(self).a, tuple),
                eval_in_expression(std::forward<Self>(self).b, tuple));
        }
    };

    /**
     * Unary operator expression.
     * @tparam A first type
     * @tparam Op operator
     */
    template<is_varexpr A, is_operator Op>
    struct expression<A, Op> {
        using define = define<A>;
        using depend = depend<A>;

        [[no_unique_address]] A a{};

        template<class Self>
        constexpr decltype(auto) eval(this Self&& self, is_named_tuple auto& tuple) {
            return Op::eval(eval_in_expression(std::forward<Self>(self).a, tuple));
        }
    };

    /**
     * Partial range expression, still depends on additional variables.
     * @tparam A partial range
     */
    template<is_partial_range A>
    struct expression<A> {
        using is_partial_range = int;

        using define = define<A>;
        using depend = depend<A>;

        A value;

        constexpr expression(A&& a) : value(std::move(a)) {}
        constexpr expression(const A& a) : value(a) {}

        template<class Self>
        constexpr decltype(auto) eval(this Self&& self, is_named_tuple auto& tuple) {
            using result = decay_t<decltype(std::declval<A&>().eval(tuple))>;
            if constexpr (kaixo::is_partial_range<result>)
                return expression<result>{ std::forward<Self>(self).value.eval(tuple) };
            else return std::forward<Self>(self).value.eval(tuple);
        }
    };

    /**
     * Tuple expression, when evaluated creates a tuple.
     * @tparam ...As values, expression, or variables.
     */
    template<class ...As> requires are_valid_expression<As...>
    struct expression<As...> {
        using define = concat_t<define<As>...>::unique;
        using depend = concat_t<depend<As>...>::unique;

        std::tuple<As...> parts;

        template<class Self>
        constexpr decltype(auto) eval(this Self&& self, is_named_tuple auto& tuple) {
            auto res = [&]<std::size_t ...Is>(std::index_sequence<Is...>) {
                return std::tuple{ eval_in_expression(std::get<Is>(self.parts), tuple)... };
            }(std::index_sequence_for<As...>{});
            if constexpr (as_info<decltype(res)>::template count_filter < []<is_varexpr>{} > != 0)
                return move_tparams_t<decltype(res), expression>{ std::move(res) };
            else return res;
        }
    };

    /**
     * All operators to construct the unevaluated expression objects.
     */
    namespace operators {
        // Variable pack operators
        template<is_var A, is_var B> constexpr auto operator,(const A&, const B&) { return info<A, B>{}; }
        template<is_var A, is_var ...Bs> constexpr auto operator,(info<Bs...>, const A&) { return info<Bs..., A>{}; }
        template<is_var A, is_var ...Bs> constexpr auto operator,(const A&, info<Bs...>) { return info<A, Bs...>{}; }

        // Tuple expression operators
        template<class A, class B> requires are_valid_expression<A, B>
        constexpr auto operator,(A&& a, B&& b) {
            return expression<decay_t<A>, decay_t<B>>{ std::tuple{ std::forward<A>(a), std::forward<B>(b) } };
        }

        template<class A, class ...Bs> requires are_valid_expression<Bs...>
        constexpr auto operator,(A&& a, expression<Bs...>&& b) {
            return expression<decay_t<A>, Bs...>{ std::tuple_cat(std::forward_as_tuple(std::forward<A>(a)), b.parts) };
        }

        template<class A, class ...Bs> requires are_valid_expression<Bs...>
        constexpr auto operator,(expression<Bs...>&& b, A&& a) {
            return expression<Bs..., decay_t<A>>{ std::tuple_cat(std::move(b).parts, std::forward_as_tuple(std::forward<A>(a))) };
        }

#define KAIXO_BINARY_OPERATOR(name, op)                          \
        struct name {                                            \
            using is_operator = int;                             \
            template<class A, class B>                           \
            constexpr static decltype(auto) eval(A&& a, B&& b) { \
                return a op b;                                   \
            }                                                    \
        };                                                                                             \
                                                                                                       \
        template<class A, class B> requires are_valid_expression<A, B>                                 \
        constexpr expression<decay_t<A>, decay_t<B>, name> operator op(A&& a, B&& b) {                 \
            return expression<decay_t<A>, decay_t<B>, name>{ std::forward<A>(a), std::forward<B>(b) }; \
        }

#define KAIXO_UNARY_OPERATOR(name, op)                    \
        struct name {                                     \
            using is_operator = int;                      \
            template<class A>                             \
            constexpr static decltype(auto) eval(A&& a) { \
                return op(a);                             \
            }                                             \
        };                                                             \
                                                                       \
        template<is_varexpr A> requires (!is_partial_range<A>)         \
        constexpr expression<decay_t<A>, name> operator op(A&& a) {    \
            return expression<decay_t<A>, name>{ std::forward<A>(a) }; \
        }

        KAIXO_UNARY_OPERATOR(negate, -);
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


    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                           List Comprehension

     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    /**
     * Get the value type of a range, given a named tuple.
     * @tparam R (partial) range
     * @tparam T named tuple
     */
    template<is_range_kind R, is_named_tuple T>
    struct range_value;

    template<is_range R, is_named_tuple T> // Normal range, just get its value type.
    struct range_value<R, T> : std::type_identity<std::ranges::range_value_t<R>> {};

    template<is_partial_range R, is_named_tuple T> // Recurse on partial range.
    struct range_value<R, T> : range_value<decltype(std::declval<R&>().eval(std::declval<T&>())), T> {};

    template<is_range_kind R, is_named_tuple T>
    using range_value_t = range_value<R, T>::type;

    /**
     * Get the iterator type of a range, given a named tuple.
     * @tparam R (partial) range
     * @tparam T named tuple
     */
    template<is_range_kind R, is_named_tuple T>
    struct range_iterator;

    template<is_range R, is_named_tuple T> // Normal range, just get its iterator type.
    struct range_iterator<R, T> : std::type_identity<std::ranges::iterator_t<const R>> {};

    template<is_partial_range R, is_named_tuple T> // Recurse on partial range.
    struct range_iterator<R, T> : range_iterator<decltype(std::declval<R&>().eval(std::declval<T&>())), T> {};

    template<is_range_kind R, is_named_tuple T>
    using range_iterator_t = range_iterator<R, T>::type;

    /**
     * Get the complete range type, given a named tuple. Used to evaluate
     * the actual range type of partial ranges.
     * @tparam R (partial) range
     * @tparam T named tuple
     */
    template<is_range_kind R, is_named_tuple T>
    struct range_type;

    template<is_range R, is_named_tuple T> // Normal range, just take its type.
    struct range_type<R, T> : std::type_identity<R> {};

    template<is_partial_range R, is_named_tuple T> // Recurse on partial range.
    struct range_type<R, T> : range_type<decltype(std::declval<R&>().eval(std::declval<T&>())), T> {};

    template<is_range_kind R, is_named_tuple T>
    using range_type_t = range_type<R, T>::type;

    /**
     * Get the type of the iterator if it's a range, otherwise dud type.
     * @tparam R part
     * @tparam T named tuple
     */
    template<class R, is_named_tuple T>
    struct iterator_data : std::type_identity<dud> {};

    template<is_range_kind R, is_named_tuple T>
    struct iterator_data<R, T> : range_iterator<R, T> {};
    
    template<class R, is_named_tuple T>
    using iterator_data_t = iterator_data<R, T>::type;

    /**
     * Get the defined named values of type R.
     * @tparam R type to get defined named values of
     * @tparam T named tuple
     */
    template<class R, is_named_tuple T>
    struct defined_values : std::type_identity<info<>> {};

    template<is_dependent_range R, is_named_tuple T>
        requires (R::define::size == 1)
    struct defined_values<R, T> : std::type_identity<info<named_value<range_value_t<R, T>, typename R::define::type>>> {};

    template<is_dependent_range R, is_named_tuple T>
        requires (R::define::size > 1)
    struct defined_values<R, T> : std::type_identity<decltype(
        zip_t<typename as_info<range_value_t<R, T>>::remove_const, typename R::define>
        ::for_each([]<class ...Args>() { return info<typename Args::template as<named_value>...>{}; }))> {};

    template<is_partial_range R, is_named_tuple T> // Complete partial range
    struct defined_values<R, T> : defined_values<range_type_t<R, T>, T> {};
    
    template<class R, is_named_tuple T>
    using defined_values_t = defined_values<R, T>::type;

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
    struct intermediate_value<R, T> : std::type_identity<std::optional<range_type_t<R, T>>> {};

    template<class R, is_named_tuple T>
    using intermediate_value_t = intermediate_value<R, T>::type;

    /**
     * Get recursively dependent data using the provided named tuple. Accumulates the
     * defined values over all parts to evaluate incomplete parts.
     * @tparam Get trait to get from parts
     * @tparam Tuple named tuple to provide named values to the recursively dependent parts
     * @tparam Parts parts
     */
    template<template<class, class> class Get, is_named_tuple Tuple, class Parts>
    struct recursive_dependent_data;

    template<template<class, class> class Get, is_named_tuple Tuple, class Part, class ...Parts>
    struct recursive_dependent_data<Get, Tuple, info<Part, Parts...>> {
        using named_tuple_type = append_t<defined_values_t<Part, Tuple>, Tuple>;
        using type = recursive_dependent_data<Get, named_tuple_type, info<Parts...>>::type::template prepend<Get<Part, Tuple>>;
    };

    template<template<class, class> class Get, is_named_tuple Tuple, class Part>
    struct recursive_dependent_data<Get, Tuple, info<Part>> {
        using type = info<Get<Part, Tuple>>;
    };

    /**
     * Get iterator types of the parts.
     * @tparam ...Parts parts
     */
    template<class ...Parts>
    using iterator_datas_t = recursive_dependent_data<iterator_data_t, named_tuple<>, info<Parts...>>::type::template as<std::tuple>;

    /**
     * Get intermediate value types, this is for ranges which are dependent on a
     * variable, and for each iteration of the dependent variable need to create a new instance.
     * @tparam ...Parts parts
     */
    template<class ...Parts>
    using intermediate_values_t = recursive_dependent_data<intermediate_value_t, named_tuple<>,
        info<Parts...>>::type::template as<std::tuple>;

    /**
     * Get the type of the named tuple given all parts.
     * @tparam Tuple named tuple
     * @tparam Parts parts
     */
    template<is_named_tuple Tuple, class Parts>
    struct named_tuple_type;

    template<is_named_tuple Tuple, class Part, class ...Parts>
    struct named_tuple_type<Tuple, info<Part, Parts...>>
        : named_tuple_type<append_t<defined_values_t<Part, Tuple>, Tuple>, info<Parts...>> {};

    template<is_named_tuple Tuple, class Part>
    struct named_tuple_type<Tuple, info<Part>> {
        using type = append_t<defined_values_t<Part, Tuple>, Tuple>;
    };

    template<class ...Parts>
    using named_tuple_type_t = named_tuple_type<named_tuple<>, info<Parts...>>::type;

    /**
     * Value type of a list comprehension object.
     * @tparam R result expression
     * @tparam ...Parts parts
     */
    template<is_varexpr R, class ...Parts>
    using lc_value_type = decltype(eval_in_expression(std::declval<R&>(),
        std::declval<named_tuple_type_t<Parts...>&>()));

    /**
     * Return code for executables in comprehension.
     */
    enum class return_code {
        none = 0, // Continue like normal
        stop = 1, // Stop iteration now, and set to end
        again = 2  // Skip current values, and increment again
    };

    constexpr return_code choose_code(return_code a, return_code b) {
        return (return_code)std::max((int)a, (int)b);
    }

    /**
     * Execute overloads.
     */
    template<class E>
    constexpr return_code execute(E&& e, is_named_tuple auto& tuple) {
        return bool(eval_in_expression(std::forward<E>(e), tuple)) ? return_code::none : return_code::again;
    }

    /**
     * Range linked to variable.
     * @tparam Range range
     * @tparam Var variable
     */
    template<is_range Range, is_var ...Vars>
    struct named_range : Range {
        using define = info<Vars...>;
        using depend = depend<Range>;
        using range = Range;

        constexpr named_range(Range&& range, const Vars&...) : Range(std::move(range)) {}
        constexpr named_range(const Range& range, const Vars&...) : Range(range) {}
    };

    /**
     * Partial range linked to variable, still depends on variables.
     * @tparam Range range
     * @tparam Var variable
     */
    template<is_range_kind Range, is_var ...Vars>
    struct partial_named_range : Range {
        using is_partial_range = int;

        using define = info<Vars...>;
        using depend = depend<Range>;
        using range = Range;

        constexpr partial_named_range(Range&& range, const Vars&...) : Range(std::move(range)) {}
        constexpr partial_named_range(const Range& range, const Vars&...) : Range(range) {}

        template<class Self>
        constexpr decltype(auto) eval(this Self&& self, is_named_tuple auto& tuple) {
            using remaining = depend::template remove<typename decay_t<decltype(tuple)>::vars>;
            if constexpr (remaining::size == 0)
                return named_range{ std::forward<Self>(self).Range::eval(tuple), Vars{}... };
            else return partial_named_range{ std::forward<Self>(self).Range::eval(tuple), Vars{}... };
        }
    };

    /**
     * List comprehension object.
     * @tparam R result expression
     * @tparam ...Parts comprehension parts
     */
    template<class R, class ...Parts>
    struct list_comprehension {
        using define = concat_t<define<Parts>...>::unique;
        using depend = concat_t<depend<R>, depend<Parts>...>::unique::template remove<define>;

        using part_types = info<Parts...>;

        using value_type = lc_value_type<R, Parts...>;

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = value_type;

            constexpr iterator() : end(true) {}
            constexpr iterator(const list_comprehension& self) : self(&self) { prepare(); }

            constexpr iterator& operator++() { increment<sizeof...(Parts) - 1>(); return *this; }
            constexpr iterator operator++(int) { iterator b = *this; operator++(); return b; }

            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr bool operator==(const iterator& other) const {
                return end == true && other.end == true // If end, iterators don't matter.
                    || other.end == end && other.iterators == iterators;
            }

            constexpr value_type operator*() {
                if (end) throw; // Can't access past end
                return eval_in_expression(self->result, values.value());
            }

            using iterator_datas = iterator_datas_t<Parts...>;
            using named_tuple_type = named_tuple_type_t<Parts...>;
            using intermediate_values = intermediate_values_t<Parts...>;

            intermediate_values intermediate{};
            iterator_datas iterators{};
            std::optional<named_tuple_type> values{};
            const list_comprehension* self = nullptr;
            bool end = false;

            // How does this increment work?
            // E: Executable, C: Container
            // lc - C E C C E E C E E
            //                  |<<<< first increment 
            //            |<<<<<|     if == end: increment next one
            //          |<|           if == end: increment next one
            //          |             if != end: done
            //          |>|           execute till previous container
            //            |           set to begin
            //            |>>>>>|     execute till previous container   
            //                  |     set to begin
            //                  |>>>> execute till start
            // This way, only executables that rely on changed values will be executed.
            template<std::size_t I>
            constexpr void increment() {
                using type = info<Parts...>::template element<I>::type;

                auto& part = std::get<I>(self->parts);
                auto& intr = std::get<I>(intermediate);
                auto& iter = std::get<I>(iterators);
                if constexpr (is_range_kind<type>) {
                    using full = range_type_t<type, decltype(values.value())>;
                    do {
                        if constexpr (is_partial_range<type>) {
                            if (++iter == std::ranges::end(intr.value())) {
                                if constexpr (I != 0) increment<I - 1>();
                                intr = eval_in_expression(part, values.value());
                                iter = std::ranges::begin(intr.value());

                                if constexpr (I == 0) { end = true; return; }
                            }
                        }
                        else {
                            if (++iter == std::ranges::end(part)) {
                                if constexpr (I != 0) increment<I - 1>();
                                iter = std::ranges::begin(part);

                                if constexpr (I == 0) { end = true; return; }
                            }
                        }

                        if constexpr (full::define::size > 1) {
                            sequence<full::define::size>([&]<std::size_t ...Is>() {
                                ((values.value().get<decay_t<typename full::define::template element<Is>::type>>()
                                    = std::get<Is>(*iter)), ...);
                            });
                        } else {
                            values.value().get<decay_t<typename full::define::type>>() = *iter;
                        }

                        if constexpr (I != sizeof...(Parts) - 1) {
                            return_code code = evaluate<I + 1>();

                            if (end || code == return_code::stop) { end = true; return; }
                            else if (code == return_code::again) continue;
                        }
                        return;
                    } while (true);
                }
                else if constexpr (I == 0) { end = true; return; }// We're at the end!
                else return increment<I - 1>();
            }

            template<std::size_t I>
            constexpr return_code evaluate() {
                using type = info<Parts...>::template element<I>::type;
                if constexpr (!is_range_kind<type>) {
                    auto& part = std::get<I>(self->parts);

                    return_code code = execute(part, values.value());

                    if constexpr (I == sizeof...(Parts) - 1) return code;
                    else return choose_code(code, execute<I + 1>());
                }
                else return return_code::none;
            }

            template<std::int64_t I, class ...Args>
            constexpr return_code initialize(Args&& ...args) {
                if constexpr (I == sizeof...(Parts)) {
                    values = named_tuple{ std::forward<Args>(args)... };

                    return return_code::none;
                } else {
                    using type = info<Parts...>::template element<I>::type;

                    named_tuple cur_values{ std::forward<Args>(args)... };

                    auto& part = std::get<I>(self->parts);
                    auto& intr = std::get<I>(intermediate);
                    auto& iter = std::get<I>(iterators);
                    if constexpr (is_range_kind<type>) {
                        using full = range_type_t<type, decltype(cur_values)>;
                        if constexpr (is_partial_range<type>) {
                            intr = part.eval(cur_values);
                            iter = std::ranges::begin(intr.value());

                            if (iter == std::ranges::end(intr.value())) {
                                end = true;
                                return return_code::stop;
                            }
                        } else {
                            iter = std::ranges::begin(part);  

                            if (iter == std::ranges::end(part)) {
                                end = true;
                                return return_code::stop;
                            }
                        }

                        if constexpr (full::define::size > 1) {
                            return sequence<full::define::size>([&]<std::size_t ...Is>() {
                                return initialize<I + 1>(std::forward<Args>(args)..., 
                                    (decay_t<typename full::define::template element<Is>::type>{} = std::get<Is>(*iter))...);
                            });
                        } else {
                            return initialize<I + 1>(std::forward<Args>(args)...,
                                decay_t<typename full::define::type>{} = *iter);
                        }
                    } else {
                        return_code code = execute(part, cur_values);

                        return choose_code(code, initialize<I + 1>(std::forward<Args>(args)...));
                    }
                }
            }

            constexpr inline void prepare() {
                return_code _code = initialize<0>(); // Set iterators to begin
                if (_code == return_code::again) operator++();
            }
        };

        using const_iterator = iterator;

        R result;
        std::tuple<Parts...> parts;

        constexpr iterator begin() { return iterator(*this); }
        constexpr iterator end() { return iterator(); }
        constexpr const_iterator begin() const { return const_iterator(*this); }
        constexpr const_iterator end() const { return const_iterator(); }
    };

    /**
     * Partial list comprehension, still misses some variables.
     * @tparam R result expression
     * @tparam ...Parts comprehension parts
     */
    template<class R, class ...Parts>
    struct partial_list_comprehension {
        using is_partial_range = int;

        using define = concat_t<define<Parts>...>::unique;
        using depend = concat_t<depend<R>, depend<Parts>...>::unique::template remove<define>;

        using part_types = info<Parts...>;

        R result;
        std::tuple<Parts...> parts;

        template<class Self>
        constexpr decltype(auto) eval(this Self&& self, is_named_tuple auto& tuple) {
            // Find remaining dependencies.
            using remaining = depend::template remove<typename decay_t<decltype(tuple)>::vars>;
            return sequence<sizeof...(Parts)>([&]<std::size_t ...Is>() {
                if constexpr (remaining::size == 0) { // No more dependencies
                    return list_comprehension{
                        eval_in_expression(std::forward<Self>(self).result, tuple),
                        std::make_tuple(eval_in_expression(std::get<Is>(std::forward<Self>(self).parts), tuple)...)
                    };
                } else {
                    return partial_list_comprehension{
                        eval_in_expression(std::forward<Self>(self).result, tuple),
                        std::make_tuple(eval_in_expression(std::get<Is>(std::forward<Self>(self).parts), tuple)...)
                    };
                }
            });
        }
    };

    template<class Ty> concept is_lc = specialization<Ty, list_comprehension>;
    template<class Ty> concept is_partial_lc = is_expression<decay_t<Ty>> && is_partial_range<decay_t<Ty>> 
      && requires (decay_t<Ty> ty) { { ty.value } -> specialization<partial_list_comprehension>; };

    namespace operators {
        constexpr auto operator-(is_range auto& r) { return views::all(r); }
        constexpr auto operator-(is_range auto&& r) { return views::all(std::move(r)); }
        constexpr auto operator-(is_partial_range auto& r) { return r; }
        constexpr auto operator-(is_partial_range auto&& r) { return std::move(r); }
        
        constexpr auto operator<(is_var auto v, is_range auto&& r) { 
            return named_range{ std::move(r), v }; 
        }

        constexpr auto operator<(is_var auto v, is_partial_range auto&& r) {
            return expression<decltype(partial_named_range{ std::move(r), v }) > { partial_named_range{ std::move(r), v } };
        }
        
        template<is_var ...Vars>
        constexpr auto operator<(info<Vars...>, is_range auto&& r) { 
            return named_range{ std::move(r), Vars{}... };
        }

        template<is_var ...Vars>
        constexpr auto operator<(info<Vars...>, is_partial_range auto&& r) {
            return expression<decltype(partial_named_range{ std::move(r), Vars{}... }) > { partial_named_range{ std::move(r), Vars{}... } };
        }

        template<is_varexpr A, is_dependent B>
        constexpr decltype(auto) construct_lc(A&& a, B&& b) {
#define KAIXO_PARTIAL_CONSTRUCT(type) type<decay_t<A>, decay_t<B>>{  \
                std::forward<A>(a), std::tuple{ std::forward<B>(b) } \
            }
            // Determine whether the list comprehension is complete.
            using lc_t = decltype(KAIXO_PARTIAL_CONSTRUCT(partial_list_comprehension));
            if constexpr (lc_t::depend::size == 0) return KAIXO_PARTIAL_CONSTRUCT(list_comprehension);
            else return expression<lc_t>{ KAIXO_PARTIAL_CONSTRUCT(partial_list_comprehension) };
#undef KAIXO_PARTIAL_CONSTRUCT
        };

        template<is_lc Ty, is_dependent Part>
        constexpr auto operator,(Ty&& lc, Part&& part) {
#define KAIXO_PARTIAL_CONSTRUCT(type) type{ std::forward<Ty>(lc).result, std::tuple_cat(    \
                std::forward<Ty>(lc).parts, std::tuple(std::forward<Part>(part)) \
            ) }
            // Determine whether the list comprehension is complete.
            using lc_t = decltype(KAIXO_PARTIAL_CONSTRUCT(partial_list_comprehension));
            if constexpr (lc_t::depend::size == 0) return KAIXO_PARTIAL_CONSTRUCT(list_comprehension);
            else return expression<lc_t>{ KAIXO_PARTIAL_CONSTRUCT(partial_list_comprehension) };
#undef KAIXO_PARTIAL_CONSTRUCT
        };
        
        template<is_partial_lc Ty, is_dependent Part>
        constexpr auto operator,(Ty&& lc, Part&& part) {
#define KAIXO_PARTIAL_CONSTRUCT(type) type{ std::forward<Ty>(lc).value.result, std::tuple_cat(   \
                std::forward<Ty>(lc).value.parts, std::make_tuple(std::forward<Part>(part)) \
            ) }
            // Determine whether the list comprehension is complete.
            using lc_t = decltype(KAIXO_PARTIAL_CONSTRUCT(partial_list_comprehension));
            if constexpr (lc_t::depend::size == 0) return KAIXO_PARTIAL_CONSTRUCT(list_comprehension);
            else return expression<lc_t>{ KAIXO_PARTIAL_CONSTRUCT(partial_list_comprehension) };
#undef KAIXO_PARTIAL_CONSTRUCT
        };

        template<is_varexpr A, is_dependent B>
        constexpr auto operator|(A&& a, B&& b) {
            return construct_lc(std::forward<A>(a), std::forward<B>(b));
        }

        template<is_var ...As>
        constexpr auto operator|(info<As...>, is_dependent auto&& r) {
            return construct_lc(expression<As...>{ std::tuple{ As{}... } }, std::move(r));
        }
    }

    namespace containers {
        //template<is_range ...As>
        //struct zipped_range {
        //    using define = concat_t<define<As>...>;
        //    using depend = concat_t<depend<As>...>;

        //    std::tuple<As...> ranges;
        //};


        //template<is_range_kind ...As> 
        //struct partial_zipped_range {
        //    using is_partial_range = int;

        //    using define = concat_t<define<As>...>;
        //    using depend = concat_t<depend<As>...>;

        //    std::tuple<As...> ranges;

        //    constexpr auto eval(is_named_tuple auto& tuple) const {
        //        using remaining = depend::template remove<typename decay_t<decltype(tuple)>::vars>;
        //        return sequence<sizeof...(As)>([&]<std::size_t ...Is>() {
        //            if constexpr (remaining::size == 0) {
        //                return zipped_range{
        //                    std::tuple{ eval_in_expression(std::get<Is>(ranges), tuple)... };
        //                };
        //            } else {
        //                return partial_zipped_range{
        //                    std::tuple{ eval_in_expression(std::get<Is>(ranges), tuple)... };
        //                };
        //            }
        //        });
        //    }
        //};


        template<class ...As> struct range_t;

        template<class A> requires (!is_varexpr<A>) range_t(A, A)->range_t<A>;
        template<is_varexpr A, class B> requires (!is_varexpr<B>) range_t(A, B)->range_t<A, B>;
        template<class A, is_varexpr B> requires (!is_varexpr<A>) range_t(A, B)->range_t<A, B>;
        template<is_varexpr A, is_varexpr B> range_t(A, B) -> range_t<A, B>;

        template<trivial Ty>
            requires (!is_varexpr<Ty>)
        struct range_t<Ty> {
            Ty a{};
            Ty b{};
            constexpr range_t(Ty a, Ty b) : a(a), b(b) {}

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
            constexpr iterator end() const { return { b + 1 }; }
        };

        template<is_varexpr A, trivial Ty>
            requires (!is_varexpr<Ty>)
        struct range_t<A, Ty> {
            using is_partial_range = int;
            using depend = info<A>;

            Ty b{};
            constexpr range_t(const A&, Ty b) : b(b) {}

            constexpr auto eval(is_named_tuple auto& tuple) const {
                if constexpr (tuple.contains<A>)
                    return range_t<Ty>{ tuple.get<A>(), b };
                else return *this;
            }
        };

        template<trivial Ty, is_varexpr B>
            requires (!is_varexpr<Ty>)
        struct range_t<Ty, B> {
            using is_partial_range = int;
            using depend = info<B>;

            Ty a{};
            constexpr range_t(Ty a, const B&) : a(a) {}

            constexpr auto eval(is_named_tuple auto& tuple) const {
                if constexpr (tuple.contains<B>)
                    return range_t<Ty>{ a, tuple.get<B>() };
                else return *this;
            }
        };

        template<is_varexpr A, is_varexpr B>
        struct range_t<A, B> {
            using is_partial_range = int;
            using depend = info<A, B>;

            constexpr range_t(const A&, const B&) {}

            constexpr auto eval(is_named_tuple auto& tuple) const {
                if constexpr (tuple.contains<A> && tuple.contains<B>)
                    return range_t{ tuple.get<A>(), tuple.get<B>() };
                return *this;
            }
        };

        template<class A, class B>
        constexpr auto range(A&& a, B&& b) {
            using range_type = decltype(range_t{ std::forward<A>(a), std::forward<B>(b) });
            if constexpr (is_partial_range<range_type>) 
                return expression<range_type>{ range_type{ std::forward<A>(a), std::forward<B>(b) } };
            else return range_type{ std::forward<A>(a), std::forward<B>(b) };
        }
    }
}