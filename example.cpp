
// ------------------------------------------------

#include <iostream>
#include <fstream>
#include <string>
#include <source_location>
#include <vector>
#include <map>
#include <ranges>
#include <algorithm>
#include <utility>
#include <functional>
#include <cassert>
#include <set>
#include <expected>
#include <thread>
#include <any>
#include <mutex>
#include <type_traits>
#include <concepts>
#include <cstddef>
#include <string_view>
#include <unordered_set>
#include <regex>
#include <complex>
#include <print>

// ------------------------------------------------

namespace kaixo {

    // ------------------------------------------------
    //                     Utils
    // ------------------------------------------------

    constexpr std::size_t npos = static_cast<std::size_t>(-1);

    // ------------------------------------------------

    namespace detail {

        // ------------------------------------------------
        
        struct dud {};

        // ------------------------------------------------

        template<class Ty>
        constexpr std::tuple<Ty> store_as_tuple(Ty&& t) { return { std::forward<Ty>(t) }; }
        template<class ...Tys>
        constexpr std::tuple<Tys...> store_as_tuple(std::tuple<Tys...>&& t) { return std::move(t); }

        // ------------------------------------------------
        
        template<class Ty>
        constexpr auto flatten_tuple(Ty&& t) {
            if constexpr (requires { typename std::tuple_size<std::decay_t<Ty>>::type; }) {
                return[&]<std::size_t ...Is>(std::index_sequence<Is...>) {
                    return std::tuple_cat(flatten_tuple(static_cast<std::tuple_element_t<Is, std::decay_t<Ty>>&&>(std::get<Is>(t)))...);
                }(std::make_index_sequence<std::tuple_size_v<std::decay_t<Ty>>>{});
            } else return std::tuple<Ty>{ std::forward<Ty>(t) };
        }

        // ------------------------------------------------

    }

    // ------------------------------------------------

    template<std::size_t I, class Tuple>
    constexpr auto recursive_get(Tuple&& tuple)
        -> std::tuple_element_t<I, decltype(detail::flatten_tuple(tuple))> 
    {
        return std::get<I>(detail::flatten_tuple(tuple));
    }

    template<class Tuple>
    constexpr std::size_t recursive_tuple_size_v = std::tuple_size_v<decltype(detail::flatten_tuple(std::declval<Tuple>()))>;
    
    template<std::size_t I, class Tuple>
    using recursive_tuple_element_t = std::tuple_element_t<I, decltype(detail::flatten_tuple(std::declval<Tuple>()))>;

    // ------------------------------------------------
    //                      Var
    // ------------------------------------------------

    template<class... Vars>
    struct var {

        // ------------------------------------------------

        using defines = var<>;
        using depends = var;

        // ------------------------------------------------

        constexpr static std::size_t size = sizeof...(Vars);

        template<class Find>
        constexpr static std::size_t index = [](std::size_t i = 0) {
            return ((i++, std::same_as<Find, Vars>) || ...) ? i - 1 : npos;
        }();

        // ------------------------------------------------

        template<class Tuple>
        constexpr static decltype(auto) evaluate(Tuple&& v) {
            using defines = std::decay_t<Tuple>::defines;
            if constexpr (size == 1) {
                if constexpr (std::same_as<Vars..., detail::dud>) return detail::dud{};
                else if constexpr (defines::template index<Vars...> == npos) return var{};
                else return v.template get<Vars...>();
            } else if constexpr (((defines::template index<Vars> != npos) && ...)) {
                return std::tuple<decltype(var<Vars>::evaluate(std::forward<Tuple>(v)))...>{
                    var<Vars>::evaluate(std::forward<Tuple>(v))...
                };
            } else {
                return (var<Vars>::evaluate(std::forward<Tuple>(v)), ...);
            }
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------

    template<class ...As, class ...Bs>
    constexpr var<As..., Bs...> operator,(var<As...>, var<Bs...>) { return {}; }

    // ------------------------------------------------

    template<class In, class Out = var<>> struct unique;
    template<class ...Bs> // Base case
    struct unique<var<>, var<Bs...>> : std::type_identity<var<Bs...>> {};
    template<class A, class ...As, class... Bs>
    struct unique<var<A, As...>, var<Bs...>> 
        : std::conditional_t<
            (std::same_as<A, Bs> || ...),     // If 'Bs...' already contains 'A'
            unique<var<As...>, var<Bs...>>,   // Do not add 'A'
            unique<var<As...>, var<Bs..., A>> // Otherwise do add 'A'
        > {};
    template<class ...Vars> using unique_t = typename unique<Vars...>::type;

    // ------------------------------------------------

    template<class...> struct concat;
    template<class ...As, class ...Bs, class ...Cs>
    struct concat<var<As...>, var<Bs...>, Cs...> : concat<var<As..., Bs...>, Cs...> {};
    template<class ...As>
    struct concat<var<As...>> : std::type_identity<var<As...>> {};
    template<class ...Vars> using concat_t = typename concat<Vars...>::type;
    
    // ------------------------------------------------

    template<class From, class Remove, class Result = var<>> struct remove_all;
    template<class ...Bs, class ...Cs>
    struct remove_all<var<>, var<Bs...>, var<Cs...>> : std::type_identity<var<Cs...>> {};
    template<class A, class ...As, class ...Bs, class ...Cs>
    struct remove_all<var<A, As...>, var<Bs...>, var<Cs...>> : 
        std::conditional_t<
            (std::same_as<A, Bs> || ...),                     // If 'Bs...' contains 'A'
            remove_all<var<As...>, var<Bs...>, var<Cs...>>,   // Do not add 'A'
            remove_all<var<As...>, var<Bs...>, var<Cs..., A>> // Otherwise do add 'A'
        > {};
    template<class From, class Remove> using remove_all_t = typename remove_all<From, Remove>::type;

    // ------------------------------------------------
    //                 Defines/Depends
    // ------------------------------------------------
    //  Keeps track of a type's dependencies and 
    //  definitions. This also determines whether
    //  something is unevaluated; if it has 
    //  dependencies.
    // ------------------------------------------------

    template<class> struct depends : std::type_identity<var<>> {};
    template<class> struct defines : std::type_identity<var<>> {};

    template<class Ty> using depends_t = typename depends<Ty>::type;
    template<class Ty> using defines_t = typename defines<Ty>::type;

    template<class Ty> requires requires { typename std::decay_t<Ty>::depends; }
    struct depends<Ty> : std::type_identity<typename std::decay_t<Ty>::depends> {};

    template<class Ty> requires requires { typename std::decay_t<Ty>::defines; }
    struct defines<Ty> : std::type_identity<typename std::decay_t<Ty>::defines> {};

    // ------------------------------------------------

    template<class Vars, class For>
    concept has_all_defines_for = unique_t<concat_t<depends_t<For>, Vars>>::size == Vars::size;

    // ------------------------------------------------
    //                    Evaluate
    // ------------------------------------------------

    template<class Ty>
    concept unevaluated = depends_t<std::decay_t<Ty>>::size != 0;

    template<class Tuple, class Ty> requires (!unevaluated<Ty>)
    constexpr Ty evaluate(Ty&& o, Tuple&&) { return std::forward<Ty>(o); }

    template<class Tuple, unevaluated Ty> requires requires (Ty&& o, Tuple&& v) { { o.evaluate(v) }; }
    constexpr auto evaluate(Ty&& o, Tuple&& v) 
        -> decltype(std::declval<Ty&&>().evaluate(std::declval<Tuple&&>()))
    {
        return std::forward<Ty>(o).evaluate(v);
    }

    template<class Ty, class Tuple>
    using evaluate_result_t = decltype(kaixo::evaluate(std::declval<Ty>(), std::declval<Tuple>()));

    // ------------------------------------------------
    //                  Named Tuple
    // ------------------------------------------------
    //  Linkes values inside a tuple to variables.
    //  This is used to evaluate expressions by reading
    //  the value corresponding to the variable and
    //  using it inside of the expression.
    // ------------------------------------------------

    template<class Vars, class Tuple>
    struct named_tuple {

        // ------------------------------------------------

        using depends = var<>;
        using defines = Vars;

        // ------------------------------------------------

        Tuple tuple{};

        // ------------------------------------------------

        template<class Find, class Self>
        constexpr recursive_tuple_element_t<Vars::template index<Find>, Tuple&&> get(this Self&& self) {
            return recursive_get<Vars::template index<Find>>(std::forward<Self>(self).tuple);
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------
    //                  Expressions
    // ------------------------------------------------
    //  Lazy evaluated expressions containing variables
    //  that can be substituted with values using
    //  a named_tuple instance and the 'evaluate'
    //  function.
    // ------------------------------------------------

    template<class Ty>
    concept unevaluated_range = unevaluated<Ty> && requires() { typename std::decay_t<Ty>::is_range; };

    template<class Ty>
    concept evaluated_range = !unevaluated<Ty> && std::ranges::range<Ty>;
    
    template<class Ty>
    concept any_range = unevaluated_range<Ty> || evaluated_range<Ty>;
    
    template<class ...As>
    concept valid_expression_arguments =
           (unevaluated<As> || ...) // Expressions require at least 1 argument to be unevaluated
        && (!any_range<As> && ...); // Cannot be a range, as this messes with the operator overloads.

    // ------------------------------------------------

    // Tuple operation is an expression resulting in a tuple.
    // This can be constructed using the comma operator.
    template<class ...Args>
        requires valid_expression_arguments<Args...>
    struct tuple_operation {

        // ------------------------------------------------

        using defines = var<>;
        using depends = unique_t<concat_t<depends_t<Args>...>>;

        // ------------------------------------------------

        std::tuple<Args...> args{};

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr auto evaluate(this Self&& self, Tuple&& v) {
            return std::apply([&]<class ...Tys>(Tys&& ...tys) {
                if constexpr ((unevaluated<evaluate_result_t<Tys&&, Tuple>> || ...)) {
                    return tuple_operation<evaluate_result_t<Tys&&, Tuple>...>{
                        { kaixo::evaluate(std::forward<Tys>(tys), v)... }
                    };
                } else {
                    return std::tuple<evaluate_result_t<Tys&&, Tuple>...>{ 
                        kaixo::evaluate(std::forward<Tys>(tys), v)...
                    };
                }
            }, std::forward<Self>(self).args);
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------

    template<class A, class B> requires valid_expression_arguments<A, B>
    constexpr tuple_operation<std::decay_t<A>, std::decay_t<B>> operator,(A&& a, B&& b) {
        return { { std::forward<A>(a), std::forward<B>(b) } };
    }

    template<class ...Args, class B> requires valid_expression_arguments<Args..., B>
    constexpr tuple_operation<Args..., std::decay_t<B>> operator,(tuple_operation<Args...>&& a, B&& b) {
        return { { std::tuple_cat(std::move(a).args, std::forward_as_tuple(std::forward<B>(b))) } };
    }

    // ------------------------------------------------

    // Binary operation overloads all binary operators for expressions.
    template<class A, class B, class Op> 
        requires valid_expression_arguments<A, B>
    struct binary_operation {

        // ------------------------------------------------

        using defines = var<>;
        using depends = unique_t<concat_t<depends_t<A>, depends_t<B>>>;

        // ------------------------------------------------

        [[no_unique_address]] A a{};
        [[no_unique_address]] B b{};
        [[no_unique_address]] Op operation{};

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr auto evaluate(this Self&& self, Tuple&& v) {
            return std::forward<Self>(self).operation(
                kaixo::evaluate(std::forward<Self>(self).a, v),
                kaixo::evaluate(std::forward<Self>(self).b, v));
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------

#define KAIXO_BINARY_OP(op, name)                                                                             \
    struct name##_operator {                                                                                  \
        template<class A, class B>                                                                            \
        constexpr decltype(auto) operator()(A&& a, B&& b) const {                                             \
            return std::forward<A>(a) op std::forward<B>(b);                                                  \
        }                                                                                                     \
    };                                                                                                        \
                                                                                                              \
    template<class A, class B> requires valid_expression_arguments<A, B>                                      \
    constexpr binary_operation<std::decay_t<A>, std::decay_t<B>, name##_operator> operator op(A&& a, B&& b) { \
        return { std::forward<A>(a), std::forward<B>(b) };                                                    \
    }

    KAIXO_BINARY_OP(+, add);      KAIXO_BINARY_OP(< , less_than);          KAIXO_BINARY_OP(| , bit_or);      KAIXO_BINARY_OP(*=, multiply_assign);
    KAIXO_BINARY_OP(-, subtract); KAIXO_BINARY_OP(> , greater_than);       KAIXO_BINARY_OP(^, bit_xor);      KAIXO_BINARY_OP(/=, divide_assign);
    KAIXO_BINARY_OP(*, multiply); KAIXO_BINARY_OP(<= , less_or_equals);    KAIXO_BINARY_OP(&&, logic_and);   KAIXO_BINARY_OP(%=, modulo_assign);
    KAIXO_BINARY_OP(/ , divide);  KAIXO_BINARY_OP(>= , greater_or_equals); KAIXO_BINARY_OP(|| , logic_or);   KAIXO_BINARY_OP(&=, bit_and_assign);
    KAIXO_BINARY_OP(%, modulo);   KAIXO_BINARY_OP(+=, add_assign);         KAIXO_BINARY_OP(== , equals);     KAIXO_BINARY_OP(|=, bit_or_assign);
    KAIXO_BINARY_OP(&, bit_and);  KAIXO_BINARY_OP(-=, subtract_assign);    KAIXO_BINARY_OP(!= , not_equals); KAIXO_BINARY_OP(^=, bit_xor_assign);

    // ------------------------------------------------

    // Unary operation overloads all unary operators for expressions.
    template<valid_expression_arguments A, class Op>
    struct unary_operation {

        // ------------------------------------------------

        using defines = var<>;
        using depends = depends_t<A>;

        // ------------------------------------------------

        [[no_unique_address]] A a{};
        [[no_unique_address]] Op operation{};

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr auto evaluate(this Self&& self, Tuple&& v) {
            return std::forward<Self>(self).operation(
                kaixo::evaluate(std::forward<Self>(self).a, v));
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------

#define KAIXO_UNARY_OP(op, name)                                                     \
    struct name##_operator {                                                         \
        template<class A>                                                            \
        constexpr decltype(auto) operator()(A&& a) const {                           \
            return op std::forward<A>(a);                                            \
        }                                                                            \
    };                                                                               \
                                                                                     \
    template<valid_expression_arguments A>                                           \
    constexpr unary_operation<std::decay_t<A>, name##_operator> operator op(A&& a) { \
        return { std::forward<A>(a) };                                               \
    }

    KAIXO_UNARY_OP(++, increment);
    KAIXO_UNARY_OP(--, decrement);
    KAIXO_UNARY_OP(-, negate);
    KAIXO_UNARY_OP(!, logic_not);
    KAIXO_UNARY_OP(&, pointer);
    KAIXO_UNARY_OP(*, dereference);

    // ------------------------------------------------
    //                 Named Range
    // ------------------------------------------------
    //  Constructs a named_tuple using the value_type
    //  of the range, and the variables. This is then
    //  used to evaluate the Expression, which will
    //  be the result type of this named range.
    // ------------------------------------------------

    template<class, class, class = detail::dud>
    struct named_range;

    // ------------------------------------------------

    template<class Vars, class Range, class Expression>
    struct named_range_storage {

        // ------------------------------------------------

        using defines = Vars;
        using depends = remove_all_t<depends_t<Range>, Vars>;

        // ------------------------------------------------

        using is_range = int;

        // ------------------------------------------------

        Range range;
        [[no_unique_address]] Expression expression;

        // ------------------------------------------------

        template<class Self, class Arg>
        constexpr decltype(auto) transform(this Self&& self, Arg&& arg) {
            // First case, no Expression defined, just forward the result
            if constexpr (std::same_as<Expression, detail::dud>) return static_cast<Arg>(arg);
            // Second case, evaluate the expression using the Vars and range result Arg
            else return kaixo::evaluate(std::forward<Self>(self).expression, named_tuple<Vars, Arg&&>{ std::forward<Arg>(arg) });
        }

        // ------------------------------------------------

    };
    
    // ------------------------------------------------

    template<class Vars, evaluated_range Range, class Expression> // Evaluated version
        requires (Vars::size == recursive_tuple_size_v<std::ranges::range_value_t<Range>>)
    struct named_range<Vars, Range, Expression> : named_range_storage<Vars, Range, Expression> {

        // ------------------------------------------------
        
        using base_iterator = std::ranges::iterator_t<Range>;
        using base_sentinel = std::ranges::sentinel_t<Range>;

        // ------------------------------------------------
        
        struct iterator {

            // ------------------------------------------------

            using reference = decltype(std::declval<named_range_storage<Vars, Range, Expression>&>().transform(std::declval<std::iter_reference_t<base_iterator>>()));
            using value_type = std::decay_t<reference>;
            using difference_type = std::iter_difference_t<base_iterator>;
            using iterator_category = std::conditional_t<
                std::random_access_iterator<base_iterator>, 
                std::random_access_iterator_tag, 
                std::conditional_t<
                    std::bidirectional_iterator<base_iterator>, 
                    std::bidirectional_iterator_tag, 
                    std::conditional_t<
                        std::forward_iterator<base_iterator>,
                        std::forward_iterator_tag,
                        std::input_iterator_tag>>>;

            // ------------------------------------------------

            base_iterator base;
            named_range<Vars, Range, Expression>* self = nullptr;

            // ------------------------------------------------

            constexpr iterator& operator++() {
                ++base;
                return *this;
            }

            constexpr iterator operator++(int) {
                iterator copy = *this;
                ++base;
                return copy;
            }
            
            constexpr iterator& operator--() requires std::bidirectional_iterator<base_iterator> {
                --base;
                return *this;
            }

            constexpr iterator operator--(int) requires std::bidirectional_iterator<base_iterator> {
                iterator copy = *this;
                --base;
                return copy;
            }

            // ------------------------------------------------

            template<class Self>
            constexpr reference operator*(this Self&& me) {
                return std::forward<Self>(me).self->transform(*std::forward<Self>(me).base);
            }

            // ------------------------------------------------

            constexpr static friend iterator operator+(const iterator& s, difference_type i) 
                requires std::ranges::random_access_range<Range> { return iterator{ s.base + i, s.self }; }
            
            constexpr static friend iterator operator+(difference_type i, const iterator& s) 
                requires std::ranges::random_access_range<Range> { return iterator{ i + s.base, s.self }; }
            
            constexpr static friend iterator operator-(const iterator& s, difference_type i) 
                requires std::ranges::random_access_range<Range> { return iterator{ s.base - i, s.self }; }
            
            constexpr static friend iterator operator-(difference_type i, const iterator& s) 
                requires std::ranges::random_access_range<Range> { return iterator{ i - s.base, s.self }; }

            // ------------------------------------------------

            constexpr iterator& operator+=(difference_type i) requires std::ranges::random_access_range<Range> {
                base += i;
                return *this;
            }

            constexpr iterator& operator-=(difference_type i) requires std::ranges::random_access_range<Range> {
                base -= i;
                return *this;
            }

            // ------------------------------------------------

            constexpr friend difference_type operator-(const iterator& i, const iterator& s) requires std::ranges::random_access_range<Range> {
                return i.base - s.base;
            }

            // ------------------------------------------------

            constexpr bool operator<(const iterator& o) const requires std::ranges::random_access_range<Range> { return base < o.base; }
            constexpr bool operator<=(const iterator& o) const requires std::ranges::random_access_range<Range> { return base <= o.base; }
            constexpr bool operator>(const iterator& o) const requires std::ranges::random_access_range<Range> { return base > o.base; }
            constexpr bool operator>=(const iterator& o) const requires std::ranges::random_access_range<Range> { return base >= o.base; }

            // ------------------------------------------------

            constexpr reference operator[](difference_type i) const requires std::ranges::random_access_range<Range> {
                return self->transform(base[i]);
            }

            // ------------------------------------------------

            constexpr bool operator==(const auto& o) const { return base == o; }

            // ------------------------------------------------

        };
        
        // ------------------------------------------------

        constexpr iterator begin() { return { std::ranges::begin(this->range), this }; }
        constexpr base_sentinel end() { return std::ranges::end(this->range); }

        // ------------------------------------------------

        constexpr decltype(auto) operator[](std::size_t i) requires std::ranges::random_access_range<Range> {
            return this->transform(this->range[i]);
        }

        // ------------------------------------------------

    };
    
    template<class Vars, unevaluated_range Range, class Expression> // Unevaluated version
    struct named_range<Vars, Range, Expression> : named_range_storage<Vars, Range, Expression> {

        // ------------------------------------------------

        template<class Self, class Tuple, class evaluated_t = decltype(kaixo::evaluate(std::declval<Self&&>().range, std::declval<Tuple&&>()))>
        constexpr auto evaluate(this Self&& self, Tuple&& tuple) {
            auto evaluated = kaixo::evaluate(std::forward<Self>(self).range, tuple);
            // First case, result is still not fully evaluated
            if constexpr (unevaluated_range<evaluated_t>) return named_range<Vars, evaluated_t, Expression> { {
                .range = std::move(evaluated),
                .expression = std::forward<Self>(self).expression,
            } };
            // Second case, result is fully evaluated, wrap in an 'all_t', which will store 
            // it as either an owning_view or a ref_view.
            else return named_range<Vars, std::views::all_t<evaluated_t&&>, Expression> { {
                .range = std::views::all(std::move(evaluated)),
                .expression = std::forward<Self>(self).expression,
            } };
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------
    
    // This '-' operator is half of the custom '<-' operator
    // this instance is for evaluated ranges, and wraps it in
    // either an owning_view or ref_view using all_t.
    template<evaluated_range Range>
    constexpr std::views::all_t<Range&&> operator-(Range&& range) {
        return std::views::all(std::forward<Range>(range));
    }

    // The other instance of '-' operator, for unevaluated
    // ranges. Since we can't do much here, just forward.
    // It's wrapped into an owning_view or ref_view later
    // inside the named_view once it has been evaluated.
    template<unevaluated_range Range>
    constexpr Range&& operator-(Range&& range) { return std::forward<Range>(range); }
    
    // Links a range to variables to create a named range
    template<class ...Vars, any_range Range>
    constexpr named_range<var<Vars...>, std::decay_t<Range>> operator<(var<Vars...>, Range&& range) {
        return { { 
            .range = std::forward<Range>(range),
        } };
    }

    // ------------------------------------------------
    
    // Link an expression to a named range
    template<unevaluated Expression, class Vars, class Range>
    constexpr named_range<Vars, Range, std::decay_t<Expression>> operator|(Expression&& e, named_range<Vars, Range>&& r) {
        return { { 
            .range = std::move(r.range),
            .expression = std::forward<Expression>(e),
        } };
    }

    // ------------------------------------------------
    
    // Handles main case for combining 2 ranges, adds them to a cartesian_product_view.
    template<class ...V1s, evaluated_range Range1, class Expression, class ...V2s, evaluated_range Range2>
    constexpr auto operator,(named_range<var<V1s...>, Range1, Expression>&& r1, named_range<var<V2s...>, Range2>&& r2)
        -> named_range<var<V1s..., V2s...>, std::ranges::cartesian_product_view<std::views::all_t<Range1&&>, std::views::all_t<Range2&&>>, Expression> 
    {
        using combined_t = std::ranges::cartesian_product_view<std::views::all_t<Range1&&>, std::views::all_t<Range2&&>>;
        return { {
            .range = combined_t{ std::views::all(std::move(r1.range)), std::views::all(std::move(r2.range)) },
            .expression = std::move(r1.expression),
        } };
    }

    // ------------------------------------------------

    // Combine an evaluated named range with an unevaluated named range, 
    // this forwards any previous definitions so it can turn this 
    // unevaluated range into an evaluated one.
    // It does this by evaluating the unevaluated range on the fly inside 
    // a 'transform_view' using the current iteration values, and then 
    // joining the resulting ranges, which creates a cartesian product.
    template<class Vars, unevaluated_range Range>
        requires has_all_defines_for<Vars, Range>
    struct range_evaluator {
        Range range;

        template<class Self, class Tuple>
        constexpr auto operator()(this Self&& self, Tuple&& tuple) {
            // This operator evaluates this unevaluated range on the fly
            // for each result of the parent range. This is done as follows:
            return std::views::transform(
                // First step is to evaluate the unevaluated range, we know Vars is
                // enough to fully evaluate the range, so this results in an evaluated range
                // Wrap this inside an owning_view or ref_view using 'all', as transform
                // might need this.
                std::views::all(kaixo::evaluate(std::forward<Self>(self).range, named_tuple<Vars, Tuple&&>{ std::forward<Tuple>(tuple) })), 
                // Then combine the results of the now evaluated range with the original
                // results passed to this evaluator. This creates a cartesian product.
                [tuple = detail::store_as_tuple(std::forward<Tuple>(tuple))]<class R>(R && r) {
                    return std::make_tuple(tuple, std::forward<R>(r));
                }
            );
        }
    };

    // Handles default case for evaluation of consecutive ranges
    template<class ...V1s, evaluated_range Range1, class Expression, class ...V2s, unevaluated_range Range2>
        requires has_all_defines_for<var<V1s...>, Range2>
    constexpr auto operator,(named_range<var<V1s...>, Range1, Expression>&& r1, named_range<var<V2s...>, Range2>&& r2)
        -> named_range<var<V1s..., V2s...>, std::ranges::join_view<std::ranges::transform_view<std::views::all_t<Range1&&>, range_evaluator<var<V1s...>, Range2>>>, Expression>
    {
        using evaluated_t = std::ranges::transform_view<std::views::all_t<Range1>, range_evaluator<var<V1s...>, Range2>>;
        using joined_t = std::ranges::join_view<evaluated_t>;
        return { {
            .range = joined_t{ evaluated_t{ std::views::all(std::move(r1.range)), { std::move(r2.range) } } },
            .expression = std::move(r1.expression),
        } };
    }
    
    // ------------------------------------------------
    
    // The moment an unevaluated part is encountered that cannot be 
    // evaluated on the fly, we need to cache all the parts for later
    // evaluation. That is what this cache does.
    template<class A, class B>
        requires (unevaluated<A> || unevaluated<B>)
    struct unevaluated_cache {

        // ------------------------------------------------
        
        using is_range = int;

        // ------------------------------------------------

        using defines = var<>;
        using depends = unique_t<concat_t<depends_t<A>, depends_t<B>>>;

        // ------------------------------------------------

        [[no_unique_address]] A a;
        [[no_unique_address]] B b;

        // ------------------------------------------------

        constexpr static auto&& copy_if_reference(auto&& value) { return std::move(value); }
        constexpr static auto copy_if_reference(auto& value) { return value; } // TODO: potentially construct a new named_range with ref_view?

        template<class Self, class Tuple>
        constexpr auto evaluate(this Self&& self, Tuple&& tuple) {
            return operator,(
                copy_if_reference(kaixo::evaluate(std::forward<Self>(self).a, tuple)),
                copy_if_reference(kaixo::evaluate(std::forward<Self>(self).b, tuple)));
        }

        // ------------------------------------------------

    };

    // Handles 2 cases for later evaluation:
    //  - Continue building upon an unevaluated range, need to cache all ranges
    //  - Encounters unevaluated range which can not be evaluated using V1s...
    template<class ...V1s, unevaluated_range Range1, class Expression, class ...V2s, any_range Range2>
        requires (unevaluated_range<Range1> && any_range<Range2> // Case 1
               || evaluated_range<Range1> && unevaluated_range<Range2> && !has_all_defines_for<var<V1s...>, Range2>) // Case 2
    constexpr auto operator,(named_range<var<V1s...>, Range1, Expression>&& r1, named_range<var<V2s...>, Range2>&& r2)
        -> named_range<var<V1s..., V2s...>, unevaluated_cache<named_range<var<V1s...>, Range1>, named_range<var<V2s...>, Range2>>, Expression>
    {
        using product_t = unevaluated_cache<named_range<var<V1s...>, Range1>, named_range<var<V2s...>, Range2>>;
        return { {
            .range = product_t{ { std::move(r1.range) }, std::move(r2) },
            .expression = std::move(r1.expression),
        } };
    }
    
    // ------------------------------------------------

    // Add range filter, this simply adds a filter_view with this
    // range_filter as callback, which will evaluate the stored Condition
    // to filter the results of the parent range.
    template<class Vars, class Condition>
    struct range_filter {
        Condition condition;

        template<class Self, class Tuple>
        constexpr bool operator()(this Self&& self, Tuple&& tuple) {
            return static_cast<bool>(kaixo::evaluate(std::forward<Self>(self).condition, named_tuple<Vars, Tuple&&>{ std::forward<Tuple>(tuple) }));
        }
    };

    // Handles default case for a range filter
    template<class Vars, evaluated_range Range, class Expression, valid_expression_arguments Condition>
        requires has_all_defines_for<Vars, Condition>
    constexpr auto operator,(named_range<Vars, Range, Expression>&& r, Condition&& c)
        -> named_range<Vars, std::ranges::filter_view<Range, range_filter<Vars, Condition>>, Expression> 
    {
        using filtered_t = std::ranges::filter_view<Range, range_filter<Vars, Condition>>;
        return { {
            .range = filtered_t{ std::move(r.range), { std::forward<Condition>(c) } },
            .expression = std::move(r.expression),
        } };
    }
    
    // Handles 2 cases for later evaluation:
    //  - Continues building upon an unevaluated range, need to cache all parts
    //  - Encounters Condition which can not be evaluated using Vars
    template<class Vars, class Range, class Expression, valid_expression_arguments Condition>
        requires (unevaluated_range<Range> || evaluated_range<Range> && !has_all_defines_for<Vars, Condition>)
    constexpr auto operator,(named_range<Vars, Range, Expression>&& r, Condition&& c)
        -> named_range<Vars, unevaluated_cache<named_range<Vars, Range>, Condition>, Expression>
    {
        using product_t = unevaluated_cache<named_range<Vars, Range>, Condition>;
        return { {
            .range = product_t{ { std::move(r.range) }, std::forward<Condition>(c) },
            .expression = std::move(r.expression),
        } };
    }
    
    // ------------------------------------------------
    //                  Zipped Range
    // ------------------------------------------------

    template<class ...Ranges>
    struct zip_range;

    template<evaluated_range ...Ranges> // Evaluated version
    struct zip_range<Ranges...> : std::ranges::zip_view<std::views::all_t<Ranges>...> {
        // Just uses the zip_view from the ranges library.
        using std::ranges::zip_view<std::views::all_t<Ranges>...>::zip_view;
    };
    
    template<class ...Ranges> // Unevaluated version
        requires (unevaluated_range<Ranges> || ...)
    struct zip_range<Ranges...> : std::tuple<Ranges...> {

        // ------------------------------------------------
        
        using defines = var<>;
        using depends = unique_t<concat_t<depends_t<Ranges>...>>;

        // ------------------------------------------------

        using is_range = int;

        // ------------------------------------------------

        using std::tuple<Ranges...>::tuple;

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr auto evaluate(this Self&& self, Tuple&& tuple) {
            return std::apply([&]<class ...Args>(Args&& ...args) {
                // Evaluate all ranges, and apply the comma operator again
                // This will construct a new zip_range, automatically picking
                // the correct one (unevaluated or evaluated).
                return (kaixo::evaluate(std::forward<Args>(args), tuple), ...);
            }, static_cast<const std::tuple<Ranges...>&>(self));
        }

        // ------------------------------------------------

    };

    // Construct the zip range
    template<any_range A, any_range B>
    constexpr zip_range<A, B> operator,(A&& a, B&& b) {
        return zip_range<A, B>{ std::forward<A>(a), std::forward<B>(b) };
    }

    // Explicitly delete the case for named ranges, as this messes with existing comma operators.
    template<class ...As, class ...Bs>
    constexpr auto operator,(named_range<As...>&&, named_range<Bs...>&&) = delete;
    
    // ------------------------------------------------
    //                    Break
    // ------------------------------------------------
    //  Adds a condition, that when met, will stop
    //  generating output. It does this by using the
    //  take_while_view.
    // ------------------------------------------------
    
    template<class Vars, unevaluated Break>
    struct break_point {
        Break expression{};

        template<class Self, class Tuple>
        constexpr bool operator()(this Self&& self, Tuple&& tuple) {
            return !static_cast<bool>(kaixo::evaluate(std::forward<Self>(self).expression, named_tuple<Vars, Tuple&&>{ std::forward<Tuple>(tuple) }));
        }
    };

    // ------------------------------------------------

    constexpr struct break_t {
        template<unevaluated Break>
        constexpr break_point<var<>, Break> operator=(Break&& e) const {
            return { std::forward<Break>(e) };
        }
    } brk;

    // ------------------------------------------------

    // Handles default case for break condition
    template<class Vars, evaluated_range Range, class Expression, unevaluated Break>
        requires has_all_defines_for<Vars, Break>
    constexpr auto operator,(named_range<Vars, Range, Expression>&& r, break_point<var<>, Break>&& b) 
        -> named_range<Vars, std::ranges::take_while_view<Range, break_point<Vars, Break>>, Expression> 
    {
        using break_t = std::ranges::take_while_view<Range, break_point<Vars, Break>>;
        return { {
            .range = break_t{ std::move(r.range), break_point<Vars, Break>{ std::move(b.expression) } },
            .expression = std::move(r.expression),
        } };
    }

    // Handles 2 cases for later evaluation:
    //  - Continues building upon an unevaluated range, need to cache all parts
    //  - Encounters Break condition which can not be evaluated using Vars
    template<class Vars, class Range, class Expression, unevaluated Break>
        requires (unevaluated_range<Range> || evaluated_range<Range> && !has_all_defines_for<Vars, Break>)
    constexpr auto operator,(named_range<Vars, Range, Expression>&& r, break_point<var<>, Break>&& b) 
        -> named_range<Vars, unevaluated_cache<named_range<Vars, Range>, break_point<Vars, Break>>, Expression>
    {
        using product_t = unevaluated_cache<named_range<Vars, Range>, break_point<Vars, Break>>;
        return { {
            .range = product_t{ { std::move(r.range) }, break_point<Vars, Break>{ std::move(b.expression) } },
            .expression = std::move(r.expression),
        } };
    }
    
    // ------------------------------------------------
    //                 Range Inserter
    // ------------------------------------------------
    //  Insert into a range dynamically while iterating
    // ------------------------------------------------
    
    template<class Vars, evaluated_range Range, unevaluated Expression>
        requires (!std::is_const_v<Range>)
    struct range_inserter {
        Range& range;
        Expression expression;

        template<class Self, class Tuple>
        constexpr Tuple&& operator()(this Self&& self, Tuple&& tuple) {
            std::inserter(std::forward<Self>(self).range, std::ranges::end(std::forward<Self>(self).range)) 
                = kaixo::evaluate(std::forward<Self>(self).expression, named_tuple<Vars, Tuple&&>{ std::forward<Tuple>(tuple) });
            return std::forward<Tuple>(tuple);
        }
    };

    // Create the inserter like this: 'range << expression'
    template<evaluated_range Range, unevaluated Expression>
        requires (!std::is_const_v<Range>)
    constexpr range_inserter<var<>, Range, Expression> operator<<(Range& range, Expression&& expression) {
        return { range, std::forward<Expression>(expression) };
    }

    // Handles default case for range inserter
    template<class Vars, evaluated_range Range1, class Expression1, evaluated_range Range2, unevaluated Expression2>
        requires has_all_defines_for<Vars, Expression2>
    constexpr auto operator,(named_range<Vars, Range1, Expression1>&& r, range_inserter<var<>, Range2, Expression2>&& i) 
        -> named_range<Vars, std::ranges::transform_view<Range1, range_inserter<Vars, Range2, Expression2>>, Expression1>
    {
        using inserter_t = range_inserter<Vars, Range2, Expression2>;
        using transform_t = std::ranges::transform_view<Range1, inserter_t>;
        return { {
            .range = transform_t{ std::move(r.range), inserter_t{ i.range, std::move(i.expression) } },
            .expression = std::move(r.expression),
        } };
    }
    
    // Handles 2 cases for later evaluation:
    //  - Continues building upon an unevaluated range, need to cache all parts
    //  - Encounters expression which can not be evaluated using Vars
    template<class Vars, class Range1, class Expression1, evaluated_range Range2, unevaluated Expression2>
        requires (unevaluated_range<Range1> || evaluated_range<Range1> && !has_all_defines_for<Vars, Expression2>)
    constexpr auto operator,(named_range<Vars, Range1, Expression1>&& r, range_inserter<var<>, Range2, Expression2>&& i) 
        -> named_range<Vars, unevaluated_cache<named_range<Vars, Range1>, range_inserter<Vars, Range2, Expression2>>, Expression1>
    {
        using inserter_t = range_inserter<Vars, Range2, Expression2>;
        using product_t = unevaluated_cache<named_range<Vars, Range1>, inserter_t>;
        return { {
            .range = product_t{ { std::move(r.range) }, inserter_t{ i.range, std::move(i.expression) } },
            .expression = std::move(r.expression),
        } };
    }
    
    // ------------------------------------------------
    //                     Range
    // ------------------------------------------------
    //  The range object can be used to create
    //  unevaluated ranges, where the begin, end
    //  and increment can be dependent on outside
    //  variables.
    // ------------------------------------------------

    template<class Begin, class End, class Increment = detail::dud>
    struct range;

    // ------------------------------------------------

    template<class Begin, class End, class Increment = detail::dud>
    struct range_storage {

        // ------------------------------------------------

        Begin beginValue;
        End endValue;
        Increment increment;

        // ------------------------------------------------

        constexpr bool is_end(const Begin& value) const { return static_cast<bool>(value == endValue); }
        constexpr void do_increment(Begin& value) const {
            if constexpr (std::same_as<Increment, detail::dud>) ++value;
            else if constexpr (std::invocable<Increment, Begin>) increment(value);
            else value += increment;
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------

    template<class, class, class = void>
    struct range_implementation;

    template<class Begin, class End, class Increment> // Unevaluated version
        requires (unevaluated<Begin> || unevaluated<End> || unevaluated<Increment>)
    struct range_implementation<Begin, End, Increment> : range_storage<Begin, End, Increment> {

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr auto evaluate(this Self&& self, Tuple&& tuple) {
            return range{
                kaixo::evaluate(std::forward<Self>(self).beginValue, tuple),
                kaixo::evaluate(std::forward<Self>(self).endValue, tuple),
                kaixo::evaluate(std::forward<Self>(self).increment, tuple),
            };
        }

        // ------------------------------------------------

    };

    template<class Begin, class End, class Increment> // Evaluated version
        requires (!unevaluated<Begin> && !unevaluated<End> && !unevaluated<Increment>)
    struct range_implementation<Begin, End, Increment> : range_storage<Begin, End, Increment> {

        // ------------------------------------------------

        struct sentinel {};
        struct iterator {

            // ------------------------------------------------

            using value_type = Begin;
            using reference = value_type;
            using difference_type = std::ptrdiff_t;
            using iterator_category = std::forward_iterator_tag;

            // ------------------------------------------------

            const range_storage<Begin, End, Increment>* self;
            Begin value;

            // ------------------------------------------------

            constexpr iterator& operator++() {
                self->do_increment(value);
                return *this;
            }

            constexpr iterator operator++(int) {
                iterator copy = *this;
                self->do_increment(value);
                return copy;
            }

            constexpr Begin operator*() const { return value; }

            // ------------------------------------------------

            constexpr bool operator==(const iterator& o) const { return value == o.value; }
            constexpr bool operator==(sentinel) const { return self->is_end(value); }

            // ------------------------------------------------

        };

        // ------------------------------------------------

        constexpr iterator begin() const { return { this, this->beginValue }; }
        constexpr sentinel end() const { return {}; }

        // ------------------------------------------------

    };

    // ------------------------------------------------

    template<class Begin, class End, class Increment>
    struct range : range_implementation<Begin, End, Increment> {

        // ------------------------------------------------

        using defines = var<>;
        using depends = unique_t<concat_t<depends_t<Begin>, depends_t<End>, depends_t<Increment>>>;

        // ------------------------------------------------

        using is_range = int;

        // ------------------------------------------------

        template<class ...Args>
            requires (sizeof...(Args) == 2 || sizeof...(Args) == 3)
        constexpr range(Args&& ...args)
            : range_implementation<Begin, End, Increment>{ std::forward<Args>(args)... }
        {}

        // ------------------------------------------------

    };

    template<class ...Args>
    range(Args&&...) -> range<std::decay_t<Args>...>;

    // ------------------------------------------------

    constexpr struct inf_t {} inf;

    constexpr static bool operator==(const auto&, inf_t) { return false; }
    constexpr static bool operator==(inf_t, const auto&) { return false; }

    // ------------------------------------------------
    //                    Empty
    // ------------------------------------------------
    //  Empty expression checks whether an unevaluated
    //  range is empty when evaluated on the fly.
    // ------------------------------------------------

    template<unevaluated_range Range>
    struct empty_operation {

        // ------------------------------------------------

        using defines = var<>;
        using depends = depends_t<Range>;

        // ------------------------------------------------

        Range range;

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr bool evaluate(this Self&& self, Tuple&& tuple) {
            auto evaluated = kaixo::evaluate(std::forward<Self>(self).range, tuple);
            return std::ranges::empty(evaluated);
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------

    template<unevaluated_range Range>
    constexpr auto is_empty(Range&& range) {
        return empty_operation<Range>{
            .range = std::forward<Range>(range) 
        };
    }

    // ------------------------------------------------

}

// ------------------------------------------------

struct A {};
struct B {};
struct C {};
struct D {};
struct E {};

int main() {
    using namespace kaixo;

    constexpr var<A> a;
    constexpr var<B> b;
    constexpr var<C> c;
    constexpr var<D> d;
    constexpr var<E> e;
    constexpr var<detail::dud> _;

    std::vector<int> primes{};
    for (auto a : a <- range(2, inf), is_empty((b <- primes, b < a, a % b == 0)), primes << a) {
        std::println("{}", a);
    }

    named_tuple<var<C>, std::tuple<int>> values56{ { 4 } };
    auto oesin = ((a, b, c) | a <- range(0, 10), b <- range(0, 10));

    //std::println("==== 1");
    //for (auto [a, b, c] : (a, b, c) | a <- range(0, 10), (b, c) <- ((c, d) | d <- range(0, 10), c <- range(a, d))) {
    //    std::println("({}, {}, {})", a, b, c);
    //}
    
    std::println("==== 2");
    for (auto [a, b, c] : (a, b, c) | a <- range(0, 10), (b, c) <- ((c, d) | d <- range(a, a + 10), c <- range(a, d))) {
        std::println("({}, {}, {})", a, b, c);
    }

    std::vector<std::tuple<int, int, int>> r3{ { 1, 2, 3 }, { 2, 3, 4 } };

    std::println("==== 3");
    for (auto a : a | (a, _, _) <- r3) {
        std::println("({})", a);
    }

    constexpr named_tuple<var<A, B, C>, std::tuple<std::tuple<int, float>, double>> values{ { { 1, 2 }, 3 } };
    constexpr named_tuple<var<D, E>, std::tuple<int, float>> values2{ { 4, 5 } };
    
    std::vector<int> r1{ 1, 2, 3, 4 };
    std::map<int, int> r2{ { 1, 2 }, { 3, 4 } };

    std::println("==== 4");
    for (auto a : a | a <- range(0, inf), brk = a == 10) {
        std::println("({})", a);
    }

    std::println("==== 5");
    for (auto [a, b, c] : ((a, b, c) | a <- range(1, 10), (b, c) <- (r1, range(1, a)))) {
        std::println("({}, {}, {})", a, b, c);
    }

    std::println("==== 6");
    for (auto [a, b, c] : (a, b, c) | (a, b) <- r2, c <- r1) {
        std::println("({}, {}, {})", a, b, c);
    }

    auto named = ((a, b, c) | a <- range(1, 100), b <- range(1, a), c <- range(1, b), a * a - b * b == c * c);

    std::println("==== 7");
    for (auto tpl : named) {

        auto [a, b, c] = detail::flatten_tuple(tpl);

        std::println("({}, {}, {})", a, b, c);
    }

    return 0;
}

// ------------------------------------------------
