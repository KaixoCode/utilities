#pragma once
#include "type_utils.hpp"
#include <ranges>

namespace kaixo {
    /**
     * Quick cartesian product range implementation, C++23 gets it in the standard.
     */
    template<std::ranges::input_range ...As> requires (std::ranges::view<As> && ...)
        struct cartesian_product_view : std::ranges::view_interface<cartesian_product_view<As...>> {
        using _range_types = info<As...>;
        using value_type = info<As...>::template transform<std::ranges::range_reference_t>::template as<std::tuple>;
        using difference_type = _range_types::template transform<std::ranges::range_difference_t>::template sort<type_sorters::size>::template element<0>::type;

        std::tuple<As...> _ranges;

        constexpr cartesian_product_view(As... ranges)
            : _ranges{ std::move(ranges)... } {};

        template<bool Const> struct iterator {
            using _bases = std::conditional_t<Const, info<const As...>, info<As...>>;
            using _parent_t = std::conditional_t<Const, const cartesian_product_view, cartesian_product_view>;
            using _base_iterators = _bases::template transform<std::ranges::iterator_t>;

            using iterator_concept = std::input_iterator_tag;
            using iterator_category = std::input_iterator_tag;
            using value_type = cartesian_product_view::value_type;
            using difference_type = cartesian_product_view::difference_type;

            _base_iterators::template as<std::tuple> _iterators;
            _parent_t* _parent;

            template<class ...Its>
            constexpr iterator(_parent_t* parent = nullptr, Its&&... its)
                : _iterators(std::move(its)...), _parent(parent) {}

            constexpr iterator& operator++() {
                _incr_impl<0>();
                return *this;
            }

            constexpr iterator operator++(int) {
                iterator _it = *this;
                _incr_impl<0>();
                return _it;
            }

            template<std::size_t I>
            constexpr void _incr_impl() {
                if constexpr (I == _range_types::size) _parent = nullptr;
                else {
                    auto& _it = std::get<I>(_iterators);
                    auto& _range = std::get<I>(_parent->_ranges);

                    if (++_it == std::ranges::end(_range)) {
                        _it = std::ranges::begin(_range);
                        _incr_impl<I + 1>();
                    }
                }
            }

            constexpr value_type operator*() const {
                return sequence<_range_types::size>([&]<std::size_t ...Is>{
                    return value_type{ *std::get<Is>(_iterators)... };
                });
            }

            constexpr bool operator==(const iterator& other) const {
                return _parent == nullptr && other._parent == nullptr;
            }
        };

        constexpr iterator<false> begin() {
            return sequence<_range_types::size>([&]<std::size_t ...Is> {
                if (((std::ranges::begin(std::get<Is>(_ranges))
                    == std::ranges::end(std::get<Is>(_ranges))) || ...)) return iterator<false>{};
                else return iterator<false>{ this, std::ranges::begin(std::get<Is>(_ranges))... };
            });
        }

        constexpr iterator<true> cbegin() const {
            return sequence<_range_types::size>([&]<std::size_t ...Is> {
                if (((std::ranges::begin(std::get<Is>(_ranges))
                    == std::ranges::end(std::get<Is>(_ranges))) || ...)) return iterator<true>{};
                else return iterator<true>{ this, std::ranges::begin(std::get<Is>(_ranges))... };
            });
        }

        constexpr iterator<false> end() { return {}; }
        constexpr iterator<true> cend() const { return {}; }

        constexpr value_type operator[](std::size_t pos) requires (
            (std::ranges::random_access_range<As> && ...) && (std::ranges::sized_range<As> && ...)) {
            return sequence<_range_types::size>([&]<std::size_t ...Ns>() {
                const std::array sizes{ std::ranges::size(std::get<Ns>(_ranges))... };
                std::size_t _t_pos = 0;
                return value_type{ (_t_pos = pos % sizes[Ns], pos /= sizes[Ns], std::get<Ns>(_ranges)[_t_pos])... };
            });
        }

        constexpr bool empty()
            requires (std::ranges::sized_range<As> && ...) {
            return size() == 0;
        }

        constexpr std::size_t size()
            requires (std::ranges::sized_range<As> && ...) {
            return sequence<_range_types::size>([&]<std::size_t ...Is> {
                return (std::ranges::size(std::get<Is>(_ranges)) * ...);
            });
        }
    };

    template<class ...As>
    cartesian_product_view(As&&...)->cartesian_product_view<std::views::all_t<As>...>;

    namespace views {
        struct cartesian_product_fn {
            template <std::ranges::viewable_range ...As>
            [[nodiscard]] constexpr auto operator()(As&& ...ranges) const noexcept(noexcept(
                cartesian_product_view(std::forward<As>(ranges)...))) {
                return cartesian_product_view(std::forward<As>(ranges)...);
            }
        };

        inline constexpr cartesian_product_fn cartesian_product;
    }

    template<std::ranges::range R, class B>
    constexpr auto operator|(R&& a, B&& b) {
        return std::forward<B>(b)(std::forward<R>(a));
    }

    /**
     * Lazily evaluated expression.
     */

    template<class ...Tys>        struct expression;
    template<class Ty>            struct value_wrapper { Ty value; };
    template<class Ty>           concept is_var = requires(Ty ty) { { Ty::name }; };
    template<class Ty>           concept is_value_wrapper = specialization<Ty, value_wrapper>;
    template<class Ty>           concept is_expression = specialization<Ty, expression>;
    template<class A>            concept disable_expr = std::ranges::range<A> || requires(A a) { typename A::disable_expression; };
    template<class A>            concept valid_op_arg = (is_var<A> || is_expression<A>) && !disable_expr<A>;
    template<class ...As>        concept valid_op_args = (valid_op_arg<decay_t<As>> || ...) && (!disable_expr<decay_t<As>> && ...);
    template<class A>             struct to_parts { using type = expression<value_wrapper<A>>; };
    template<is_var A>            struct to_parts<A> { using type = expression<A>; };
    template<class ...As>         struct to_parts<expression<As...>> { using type = expression<As...>; };
    template<class T>              using to_parts_t = typename to_parts<T>::type;
    template<auto Op, class ...As> using combine_parts_t = concat_t<to_parts_t<decay_t<As>>..., expression<value_t<Op>>>;

    template<string_literal Name> struct var {
        constexpr static auto name = Name;

        template<class Ty> struct named_value : value_wrapper<Ty>, var {};
        template<class T> constexpr auto operator=(T&& arg) const {
            return named_value<T>{ std::forward<T>(arg) };
        }

        template<class B> constexpr auto operator[](B&& b) const {
            constexpr auto op_lambda = []<class Q, class R> (Q && q, R && r)
                -> decltype(auto) {
                return std::forward<Q>(q)[std::forward<R>(r)];
            };
            using type = combine_parts_t<op_lambda, var, B>;
            return type{ *this, std::forward<B>(b) };
        }
    };

#define KAIXO_EOP(OP) template<class A, class B> requires valid_op_args<A, B>          \
constexpr auto operator OP(A&& a, B&& b) {                                             \
    constexpr auto op_lambda = []<class Q, class R>(Q&& q, R&& r)                      \
    -> decltype(auto) { return std::forward<Q>(q) OP std::forward<R>(r); };            \
    return combine_parts_t<op_lambda, A, B>{ std::forward<A>(a), std::forward<B>(b) }; \
}
#define KAIXO_UOP(OP) template<valid_op_args A>                                                         \
constexpr auto operator OP(A&& a) {                                                                     \
    constexpr auto op_lambda = []<class Q> (Q&& q) -> decltype(auto) { return OP std::forward<Q>(q); }; \
    return combine_parts_t<op_lambda, A>{ std::forward<A>(a) };                                         \
}
    KAIXO_EOP(+); KAIXO_EOP(<= ); KAIXO_EOP(|| ); KAIXO_EOP(|= );  KAIXO_EOP(/ ); KAIXO_EOP(^);
    KAIXO_EOP(-); KAIXO_EOP(<< ); KAIXO_EOP(+= ); KAIXO_EOP(&= );  KAIXO_EOP(< ); KAIXO_EOP(->*);
    KAIXO_EOP(*); KAIXO_EOP(>> ); KAIXO_EOP(-= ); KAIXO_EOP(^= );  KAIXO_EOP(> ); KAIXO_EOP(!= );
    KAIXO_EOP(%); KAIXO_EOP(>= ); KAIXO_EOP(*= ); KAIXO_EOP(%= );  KAIXO_EOP(| ); KAIXO_EOP(>>= );
    KAIXO_EOP(&); KAIXO_EOP(== ); KAIXO_EOP(/= ); KAIXO_EOP(<<= ); KAIXO_EOP(&&); KAIXO_EOP(<=> );
    KAIXO_UOP(-); KAIXO_UOP(+); KAIXO_UOP(--); KAIXO_UOP(++); KAIXO_UOP(~); KAIXO_UOP(!); KAIXO_UOP(*);
#undef KAIXO_EOP
#undef KAIXO_UOP

    /**
     * Create tuple of values
     */
    template<class A, class B> requires valid_op_args<A, B>
    constexpr auto operator,(A&& a, B&& b) {
        constexpr auto op_lambda = []<class Q, class R>(Q && q, R && r) -> decltype(auto) {
            constexpr bool _q_tuple = specialization<decay_t<Q>, std::tuple>;
            constexpr bool _r_tuple = specialization<decay_t<R>, std::tuple>;
            if constexpr (_q_tuple && _r_tuple) return std::tuple_cat(std::forward<Q>(q), std::forward<R>(r));
            else if constexpr (_q_tuple) return std::tuple_cat(std::forward<Q>(q), std::tuple{ std::forward<R>(r) });
            else if constexpr (_r_tuple) return std::tuple_cat(std::tuple{ std::forward<Q>(q) }, std::forward<R>(r));
            else return std::tuple_cat(std::tuple{ std::forward<Q>(q) }, std::tuple{ std::forward<R>(r) });
        };
        return combine_parts_t<op_lambda, A, B>{ std::forward<A>(a), std::forward<B>(b) };
    }

    template<class T> constexpr auto as_tuple(T&& arg) {
        if constexpr (is_expression<decay_t<T>>) return arg._data;
        else if constexpr (is_var<decay_t<T>>) return std::tuple{};
        else return std::tuple{ value_wrapper{ std::forward<T>(arg) } };
    }

    /**
     * Expression object, stores all the expression parts
     * in postfix format, and 'eval' then evaluates the expression
     * with the given values for the variables used in the expression.
     */
    template<class ...Tys> struct expression {
        using parts = info<Tys...>;
        using data_types = parts::template filter<is_specialization<value_wrapper>>;
        data_types::template as<std::tuple> _data;

        template<class ...As> constexpr expression(As&& ...as)
            : _data{ std::tuple_cat(as_tuple(std::forward<As>(as))...) } {}

        template<std::size_t I, std::size_t J, class ...Values>
        constexpr decltype(auto) get(std::tuple<Values...>& vals) const {
            using type = parts::template element<I>::type;
            if constexpr (is_var<type>) {
                constexpr auto find_name = []<class Ty>{ return Ty::name == type::name; };
                constexpr auto indices = indices_filter_v<find_name, Values...>;
                if constexpr (indices.size() == 0) return type{};
                else return std::get<indices[0]>(vals).value;
            }
            else return std::get<J>(_data).value;
        }

        /**
         * Evaluate the expression.
         * @param ...tys named_values
         */
        template<class ...Tys>
        constexpr decltype(auto) operator()(Tys&&... tys) const {
            std::tuple _vals{ tys... };
            return eval<0>(_vals);
        }

        template<std::size_t I = 0, std::size_t J = 0, class ...Values, class ...Tys>
        constexpr decltype(auto) eval(std::tuple<Values...>& vals, Tys&&...tys) const {
            if constexpr (I == parts::size) return (std::forward<Tys>(tys), ...);
            else {
                using type = parts::template element<I>::type;
                // If is operator (no var and no value)
                if constexpr (!is_var<type> && !is_value_wrapper<type>) {
                    // Get the stack of values in a pack for easy manipulation
                    template_pack<Tys...> _tys{ tys... };
                    // Check if operator is unary (by testing invocability on a single argument)
                    constexpr bool is_unary = invocable<decltype(type::value), decltype(tuples::get<sizeof...(Tys) - 1>(_tys))>;
                    // Extract the proper amount of arguments from the stack of values
                    return tuples::call([&]<class ...Args>(Args&&...args) {
                        // And evaluate the operator, and recurse to next element in the expression
                        return eval<I + 1, J>(vals, std::forward<Args>(args)..., tuples::call(type::value)(tuples::last<is_unary ? 1 : 2>(_tys)));
                    })(tuples::drop_last<is_unary ? 1 : 2>(_tys));
                }
                // If not operator, get I'th value and append to stack (function arguments).
                else return eval<I + 1, is_var<type> ? J : J + 1>(vals, std::forward<Tys>(tys)..., get<I, J>(vals));
            }
        }
    };

    /**
     * Range that's linked to a variable
     * @tparam A variable
     * @tparam B range
     */
    template<is_var A, std::ranges::range B>
    struct linked_range : B {
        using disable_expression = void; // Disables expression operators
        using var = A;
    };
    template<class A> concept is_linked_range = specialization<A, linked_range>;

    /**
     * Convert container to range(owning / non - owningm happens automatically)
     * @param a container or range
     * @return range
     */
    template<std::ranges::range A>
    constexpr std::views::all_t<A> operator-(A&& a) {
        return std::forward<A>(a);
    }

    /**
     * Link range to variable.
     * @param A variable
     * @param range range
     */
    template<is_var A, std::ranges::range B>
    constexpr linked_range<A, B> operator<(const A&, B&& range) {
        return linked_range<A, B>{ std::forward<B>(range) };
    }

    /**
     * Used during construction of a list comprehension range.
     * @tparam E final transform
     * @tparam Range range
     * @tparam Vars pack of used vars in construct
     */
    template<class E, class Range, class Vars>
    struct _comprehension_construct {
        using disable_expression = void;
        using vars = Vars;
        E transform;
        Range range;
        [[no_unique_address]] Vars defined_vars;
    };
    template<class A> concept is_comprehension = specialization<A, _comprehension_construct>;

    /**
     * Get the I'th element from a nested tuple of kind
     * std::tuple<std::tuple<... std::tuple<type, type>, type> ...>, type>
     * requires you know the amount of elements in the nested tuple.
     * @tparam I index of element
     * @tparam Size amount of elements in nested tuple
     * @param tuple the nested tuple
     * @return I'th element in nested tuple
     */
    template<std::size_t I, std::size_t Size, class A>
    constexpr decltype(auto) get_recursive(A&& tuple) {
        if constexpr (specialization<decay_t<A>, std::tuple>) {
            if constexpr (I == Size - 1)
                return std::get<1>(std::forward<A>(tuple));
            else return get_recursive<I, Size - 1>(std::get<0>(std::forward<A>(tuple)));
        }
        else return std::forward<A>(tuple);
    }

    /**
     * Construct a comprehension construct with an expression and the
     * first linked range.
     * @param expr final transform
     * @param range first range
     * @return comprehension construct
     */
    template<is_expression A, is_linked_range B>
    constexpr _comprehension_construct<A, B, info<decay_t<typename B::var>>> operator|(A&& expr, B&& range) {
        return { std::forward<A>(expr), std::forward<B>(range) };
    }

    /**
     * Construct a comprehension construct with a variable
     * and the first linked range.
     * @param a variable
     * @param range first range
     * @return comprehension construct
     */
    template<is_var A, is_linked_range B>
    constexpr _comprehension_construct<expression<A>, B, info<decay_t<typename B::var>>> operator|(const A&, B&& range) {
        return { expression<A>{}, std::forward<B>(range) };
    }

    /**
     * Add a range to a comprehension construct.
     * @param lc the comprehension construct
     * @param range a linked range
     * @return comprehension construct with the cartesian
     *         product of the existing range and the new one.
     */
    template<is_comprehension A, is_linked_range B>
    constexpr auto operator,(A&& lc, B&& range) {
        return _comprehension_construct{
            std::move(lc.transform), // Just move the final transform
            // Get the cartesian product of the construct's range and the new range
            views::cartesian_product(std::move(lc.range), std::move(range)),
            // Also append the range's linked variable.
            typename A::vars::template append<decay_t<typename B::var>>{}
        };
    }

    /**
     * Add a filter to a comprehension construct.
     * @param lc the comprehension construct
     * @param expr the filter
     * @return comprehension construct with the filter applied
     */
    template<is_comprehension L, is_expression B>
    constexpr auto operator,(L&& lc, B&& filter) {
        return _comprehension_construct{
            std::move(lc.transform), // Just move the final transform
            // Apply a filter to the construct's range.
            std::views::filter(std::move(lc.range),
            [filter = filter]<class Ty>(Ty && val) -> bool {
                constexpr std::size_t size = L::vars::size;
                // Loop over the vars in the construct
                return sequence<size>([&]<std::size_t ...Is>{
                    return filter(typename L::vars::template element<Is>::type{}
                        = get_recursive<Is, size>(val)...);
                });
            }), lc.defined_vars // Copy over the vars
        };
    }

    constexpr struct {
        template<is_comprehension L> constexpr auto operator[](L&& lc) const {
            // Apply the final transform
            return std::views::transform(lc.range, [transform = lc.transform]<class T>(T && val) {
                constexpr std::size_t size = L::vars::size;
                // Loop over all the defined vars in the comprehension construct
                return sequence<size>([&]<std::size_t ...Is>{
                    return transform(typename L::vars::template element<Is>::type{}
                    = get_recursive<Is, size>(val)...);
                });
            });
        }
    }
    /**
     * Final operator that converts the comprehension construct to a range.
     */
    lc;
}