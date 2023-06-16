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
    template<class Ty> concept is_range = ranges::range<Ty>;
    template<class Ty> concept is_partial = depend<Ty>::size != 0;
    template<class Ty> concept is_partial_range = is_partial<Ty> && explicit_range<Ty>;
    template<class Ty> concept is_var = requires() { { decay_t<Ty>::name }; };
    template<class Ty> concept is_operator = requires() { typename Ty::is_operator; };
    template<class Ty> concept is_varexpr = is_partial<Ty> || is_var<Ty>;
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

        value_type value;
    };

    // Is type a named value.
    template<class Ty> concept is_named_value = specialization<Ty, named_value>;

    /**
     * Links an expression to a variable.
     * @tparam A expression
     * @tparam Var linked variable
     */
    template<is_varexpr A, is_var Var>
    struct partial_named_value {
        using define = info<Var>;
        using depend = depend<A>;

        A expr;

        constexpr decltype(auto) evaluate(auto& tuple) const {
            return expr.evaluate(tuple);
        }

        constexpr decltype(auto) execute(auto&, auto& tuple) const;
    };

    // Is type a named value.
    template<class Ty> concept is_partial_named_value = specialization<Ty, partial_named_value>;

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
            if constexpr (is_varexpr<decay_t<Ty>>)
                return partial_named_value<decay_t<Ty>, var>{ std::forward<Ty>(value) };
            else return named_value<decay_t<Ty>, var>{ std::forward<Ty>(value) };
        }

        constexpr decltype(auto) evaluate(auto& tuple) const {
            constexpr bool contains = define<decltype(tuple)>::template occurs<var>;
            if constexpr (contains) return tuple.get<var>();
            else return var{};
        }
    };

    /**
     * Tuple of named values.
     * @tparam ...Args named values
     */
    template<is_named_value ...Args>
    struct named_tuple {
        using tuple_type = std::tuple<typename Args::value_type...>;
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
            static_assert(contains<decay_t<Var>>, "Variable is not part of tuple");
            constexpr std::size_t index = define::template index<Var>;
            return std::get<index>(std::forward<Self>(self).value);
        }

        template<class Self, is_named_value ...Tys>
        constexpr decltype(auto) assign(this Self&& self, named_tuple<Tys...>&& val) {
            if constexpr ((contains<typename kaixo::define<Tys>::type>&& ...)) {
                ((std::forward<Self>(self).get<typename kaixo::define<Tys>::type>()
                    = std::move(val).get<typename kaixo::define<Tys>::type>()), ...);
                return std::forward<Self>(self);
            }
            else {
                return named_tuple<Args..., Tys...>{
                    std::tuple_cat(std::forward<Self>(self).value, std::move(val).value)
                };
            }
        }
    };

    template<is_varexpr A, is_var Var>
    constexpr decltype(auto) partial_named_value<A, Var>::execute(auto&, auto& tuple) const {
        return tuple.assign(named_tuple{ Var{} = evaluate(tuple) });
    }

    // Is type a named value.
    template<class Ty> concept is_named_tuple = specialization<Ty, named_tuple>;

    template<class A, class Tuple>
    concept has_eval_for = requires(A & a, Tuple & tuple) { { a.evaluate(tuple) }; };

    /**
     * Evaluate a variable in an expression.
     * @param var value
     * @param tuple named tuple
     */
    template<class A>
    constexpr decltype(auto) evaluate(A&& val, is_named_tuple auto& tuple) {
        if constexpr (has_eval_for<decay_t<A>, decay_t<decltype(tuple)>>)
            return val.evaluate(tuple);
        else return std::forward<A>(val);
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
    template<is_varexpr A, is_operator Op>
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

        std::tuple<As...> parts;

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            auto res = [&]<std::size_t ...Is>(std::index_sequence<Is...>) {
                return std::tuple{ kaixo::evaluate(std::get<Is>(self.parts), tuple)... };
            }(std::index_sequence_for<As...>{});
            if constexpr (as_info<decltype(res)>::template count_filter < []<is_varexpr>{} > != 0)
                return move_tparams_t<decltype(res), tuple_operation>{ std::move(res) };
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
                return op(a);                                 \
            }                                                 \
        };                                                                  \
                                                                            \
        template<is_varexpr A> requires (!is_partial<A>)                    \
        constexpr unary_operation<decay_t<A>, name> operator op(A&& a) {    \
            return unary_operation<decay_t<A>, name>{ std::forward<A>(a) }; \
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
}

namespace kaixo {

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                           List Comprehension

     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

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
    struct defined_values<R, T> : std::type_identity<prepend_t<as_info<T>, typename R::value_type>> {};
    
    template<is_partial_range R, is_named_tuple T>
    struct defined_values<R, T> : defined_values<full_type_t<R, T>, T> {};

    template<class R, is_named_tuple T>
    using defined_values_t = defined_values<R, T>::type;

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
    struct iterator_data<R, T> : std::type_identity<std::ranges::iterator_t<const R>> {};

    template<class R, is_named_tuple T>
    using iterator_data_t = iterator_data<full_type_t<R, T>, T>::type;

    /**
     * Execute overloads.
     */
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
     * Range linked to variable.
     * @tparam Range range
     * @tparam Var variable
     */
    template<is_range Range, is_var Var>
    struct named_range {
        using define = info<Var>;

        using range_type = Range;
        using value_type = named_tuple<named_value<std::ranges::range_value_t<Range>, Var>>;

        Range rng;

        constexpr named_range(Range&& range, const Var&) : rng(std::move(range)) {}
        constexpr named_range(const Range& range, const Var&) : rng(range) {}

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = value_type;

            std::ranges::iterator_t<const Range> it{};

            constexpr iterator& operator++() { ++it; return *this; }
            constexpr iterator operator++(int) { iterator b = *this; operator++(); return b; }

            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr bool operator==(const iterator& other) const { return it == other.it; }

            constexpr value_type operator*() { return value_type{ *it }; }
        };

        iterator begin() const { return iterator{ std::ranges::begin(rng) }; }
        iterator end() const { return iterator{ std::ranges::end(rng) }; }
    };

    /**
     * Partial range linked to variable, still depends on variables.
     * @tparam Range range
     * @tparam Var variable
     */
    template<is_partial Range, is_var Var>
    struct partial_named_range : Range {
        using is_range = int;

        using define = info<Var>;
        using depend = depend<Range>;

        constexpr partial_named_range(Range&& range, const Var&) : Range(std::move(range)) {}
        constexpr partial_named_range(const Range& range, const Var&) : Range(range) {}

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            using remaining = depend::template remove<kaixo::define<decltype(tuple)>>;
            if constexpr (remaining::size == 0)
                return named_range{ std::forward<Self>(self).Range::evaluate(tuple), Var{} };
            else return partial_named_range{ std::forward<Self>(self).Range::evaluate(tuple), Var{} };
        }
    };

    /**
     * List comprehension object.
     * @tparam R result expression
     * @tparam ...Parts comprehension parts
     */
    template<class R, class ...Parts>
    struct list_comprehension {
        using value_type = decay_t<decltype(evaluate(std::declval<R&>(), 
            std::declval<named_tuple_type_t<Parts...>&>()))>;

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = value_type;

            constexpr iterator() : at_end(true) {}
            constexpr iterator(const list_comprehension& self) : self(&self) { prepare(); }

            constexpr iterator& operator++() { increment<sizeof...(Parts) - 1>(); return *this; }
            constexpr iterator operator++(int) { iterator b = *this; operator++(); return b; }

            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr bool operator==(const iterator& other) const {
                return end() == true && other.end() == true // If end, iterators don't matter.
                    || other.end() == end() && other.iterators == iterators;
            }

            constexpr value_type operator*() {
                if (end()) throw; // Can't access past end
                return evaluate(self->result, values.value());
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

                    if constexpr (I == sizeof...(Parts) - 1) return code;
                    else return choose_code(code, evaluate_i<I + 1>());
                }
                else return return_code::none;
            }

            template<std::int64_t I, class Tuple>
            constexpr return_code initialize(Tuple&& cur_values) {
                if constexpr (I == sizeof...(Parts)) {
                    values = cur_values;
                    return return_code::none;
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

                        return initialize<I + 1>(std::forward<Tuple>(cur_values).assign(*iter));
                    } else {
                        return_code code = return_code::none;
                        decltype(auto) res = execute(part, code, cur_values);
                        return choose_code(code, initialize<I + 1>(res));
                    }
                }
            }

            constexpr inline void prepare() {
                return_code _code = initialize<0>(named_tuple<>{}); // Set iterators to begin
                if (_code == return_code::skip) operator++();
                if (_code == return_code::stop) set_end();
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
        using is_range = int;

        using depend = concat_t<depend<R>, depend<Parts>...>::unique
            ::template remove<typename concat_t<define<Parts>...>::unique>;

        R result;
        std::tuple<Parts...> parts;

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            // Find remaining dependencies.
            using remaining = depend::template remove<define<decltype(tuple)>>;
            return sequence<sizeof...(Parts)>([&]<std::size_t ...Is>() {
                if constexpr (remaining::size == 0) { // No more dependencies
                    return list_comprehension{
                        kaixo::evaluate(std::forward<Self>(self).result, tuple),
                        std::make_tuple(kaixo::evaluate(std::get<Is>(std::forward<Self>(self).parts), tuple)...)
                    };
                } else {
                    return partial_list_comprehension{
                        kaixo::evaluate(std::forward<Self>(self).result, tuple),
                        std::make_tuple(kaixo::evaluate(std::get<Is>(std::forward<Self>(self).parts), tuple)...)
                    };
                }
            });
        }
    };

    template<class Ty> concept is_lc = specialization<Ty, list_comprehension>;
    template<class Ty> concept is_partial_lc = specialization<Ty, partial_list_comprehension>;

    namespace operators {
        constexpr auto operator-(is_range auto& r) { return views::all(r); }
        constexpr auto operator-(is_range auto&& r) { return views::all(std::move(r)); }
        constexpr auto operator-(is_partial auto& r) { return r; }
        constexpr auto operator-(is_partial auto&& r) { return std::move(r); }
        
        constexpr auto operator<(is_var auto v, is_range auto&& r) { 
            return named_range{ std::move(r), v }; 
        }

        constexpr auto operator<(is_var auto v, is_partial auto&& r) {
            return partial_named_range{ std::move(r), v };
        }
        
        template<class Ty>
        concept is_valid_part = true; // is_executable<Ty> || is_range_kind<Ty> || is_partial<Ty>;

        template<is_varexpr A, is_valid_part B>
        constexpr decltype(auto) construct_lc(A&& a, B&& b) {
#define KAIXO_PARTIAL_CONSTRUCT(type) type<decay_t<A>, decay_t<B>>{  \
                std::forward<A>(a), std::tuple{ std::forward<B>(b) } \
            }
            // Determine whether the list comprehension is complete.
            using lc_t = decltype(KAIXO_PARTIAL_CONSTRUCT(partial_list_comprehension));
            if constexpr (depend<lc_t>::size == 0) return KAIXO_PARTIAL_CONSTRUCT(list_comprehension);
            else return KAIXO_PARTIAL_CONSTRUCT(partial_list_comprehension);
#undef KAIXO_PARTIAL_CONSTRUCT
        };

        template<class Ty, is_valid_part Part>
            requires (is_lc<Ty> || is_partial_lc<Ty>)
        constexpr auto operator,(Ty&& lc, Part&& part) {
#define KAIXO_PARTIAL_CONSTRUCT(type) type{ std::forward<Ty>(lc).result, std::tuple_cat(    \
                std::forward<Ty>(lc).parts, std::tuple(std::forward<Part>(part)) \
            ) }
            // Determine whether the list comprehension is complete.
            using lc_t = decltype(KAIXO_PARTIAL_CONSTRUCT(partial_list_comprehension));
            if constexpr (depend<lc_t>::size == 0) return KAIXO_PARTIAL_CONSTRUCT(list_comprehension);
            else return KAIXO_PARTIAL_CONSTRUCT(partial_list_comprehension);
#undef KAIXO_PARTIAL_CONSTRUCT
        };
        
        template<is_varexpr A, is_valid_part B>
        constexpr auto operator|(A&& a, B&& b) {
            return construct_lc(std::forward<A>(a), std::forward<B>(b));
        }

        template<is_var ...As, is_valid_part B>
        constexpr auto operator|(info<As...>, B&& b) {
            return construct_lc(tuple_operation<As...>{ std::tuple{ As{}... } }, std::forward<B>(b));
        }
    }

    template<class A>
    struct break_expression {
        using depend = depend<A>;

        A expr;

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

    constexpr struct inf_t {} inf{};

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

        //    constexpr auto evaluate(is_named_tuple auto& tuple) const {
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


        template<class ...As> struct range;

        template<class A> requires (!is_varexpr<A>) range(A, A)->range<A>;
        template<class A> requires (!is_varexpr<A>) range(A, inf_t)->range<A, inf_t>;
        template<is_varexpr A> range(A, inf_t)->range<A, inf_t>;
        template<is_varexpr A, class B> requires (!is_varexpr<B>) range(A, B)->range<A, B>;
        template<class A, is_varexpr B> requires (!is_varexpr<A>) range(A, B)->range<A, B>;
        template<is_varexpr A, is_varexpr B> range(A, B) -> range<A, B>;

        template<trivial Ty>
            requires (!is_varexpr<Ty>)
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
            constexpr iterator end() const { return { b + 1 }; }
        };
        
        template<trivial Ty>
            requires (!is_varexpr<Ty>)
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

        template<is_varexpr A, trivial Ty>
            requires (!is_varexpr<Ty>)
        struct range<A, Ty> {
            using is_range = int;
            using depend = info<A>;

            Ty b{};
            constexpr range(const A&, Ty b) : b(b) {}

            constexpr auto evaluate(is_named_tuple auto& tuple) const {
                if constexpr (tuple.contains<A>)
                    return range<Ty>{ tuple.get<A>(), b };
                else return *this;
            }
        };
        
        template<is_varexpr A>
        struct range<A, inf_t> {
            using is_range = int;
            using depend = info<A>;

            constexpr range(const A&, inf_t) {}

            constexpr auto evaluate(is_named_tuple auto& tuple) const {
                if constexpr (tuple.contains<A>)
                    return range<decay_t<decltype(tuple.get<A>())>, inf_t>{ tuple.get<A>(), inf };
                else return *this;
            }
        };

        template<trivial Ty, is_varexpr B>
            requires (!is_varexpr<Ty>)
        struct range<Ty, B> {
            using is_range = int;
            using depend = info<B>;

            Ty a{};
            constexpr range(Ty a, const B&) : a(a) {}

            constexpr auto evaluate(is_named_tuple auto& tuple) const {
                if constexpr (tuple.contains<B>)
                    return range<Ty>{ a, tuple.get<B>() };
                else return *this;
            }
        };

        template<is_varexpr A, is_varexpr B>
        struct range<A, B> {
            using is_range = int;
            using depend = info<A, B>;

            constexpr range(const A&, const B&) {}

            constexpr auto evaluate(is_named_tuple auto& tuple) const {
                if constexpr (tuple.contains<A> && tuple.contains<B>)
                    return range{ tuple.get<A>(), tuple.get<B>() };
                return *this;
            }
        };
    }
}