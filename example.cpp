
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

    // ------------------------------------------------
    //                     Var
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
            if constexpr (size == 1) {
                if constexpr (std::decay_t<Tuple>::defines::template index<Vars...> == npos) return var{};
                else return (v.template get<Vars>(), ...);
            } else {
                return std::tuple<decltype(var<Vars>::evaluate(std::forward<Tuple>(v)))...>{
                    var<Vars>::evaluate(std::forward<Tuple>(v))...
                };
            }
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------

    template<class ...As, class ...Bs>
    constexpr var<As..., Bs...> operator,(var<As...>, var<Bs...>) { return {}; }

    // ------------------------------------------------

    template<class In, class Out = var<>> struct unique;
    template<class ...Bs>
    struct unique<var<>, var<Bs...>> : std::type_identity<var<Bs...>> {};
    template<class A, class ...As, class... Bs> requires (!(std::same_as<A, Bs> || ...))
    struct unique<var<A, As...>, var<Bs...>> : unique<var<As...>, var<Bs..., A>> {};
    template<class A, class ...As, class... Bs> requires (std::same_as<A, Bs> || ...)
    struct unique<var<A, As...>, var<Bs...>> : unique<var<As...>, var<Bs...>> {};
    template<class ...Vars> using unique_t = typename unique<Vars...>::type;

    // ------------------------------------------------

    template<class...> struct concat;
    template<class ...As, class ...Bs, class ...Cs>
    struct concat<var<As...>, var<Bs...>, Cs...> : concat<var<As..., Bs...>, Cs...> {};
    template<class ...As>
    struct concat<var<As...>> : std::type_identity<var<As...>> {};
    template<class ...Vars> using concat_t = typename concat<Vars...>::type;

    // ------------------------------------------------
    //                 Defines/Depends
    // ------------------------------------------------

    template<class> struct depends : std::type_identity<var<>> {};
    template<class> struct defines : std::type_identity<var<>> {};

    template<class Ty> using depends_t = typename depends<Ty>::type;
    template<class Ty> using defines_t = typename defines<Ty>::type;

    template<class ...Tys> struct defines<std::tuple<Tys...>> : unique<concat_t<defines_t<std::decay_t<Tys>>...>> {};
    template<class ...Tys> struct depends<std::tuple<Tys...>> : unique<concat_t<depends_t<std::decay_t<Tys>>...>> {};

    template<class Ty> requires requires { typename std::decay_t<Ty>::depends; }
    struct depends<Ty> : std::type_identity<typename std::decay_t<Ty>::depends> {};

    template<class Ty> requires requires { typename std::decay_t<Ty>::defines; }
    struct defines<Ty> : std::type_identity<typename std::decay_t<Ty>::defines> {};

    // ------------------------------------------------
    //                    Evaluate
    // ------------------------------------------------

    template<class Ty>
    concept unevaluated = depends_t<std::decay_t<Ty>>::size != 0;

    template<class Tuple, class Ty> requires (!unevaluated<Ty>)
    constexpr Ty evaluate(Ty&& o, Tuple&&) { return std::forward<Ty>(o); }

    template<class Tuple, unevaluated Ty> requires requires (Ty&& o, Tuple&& v) { { o.evaluate(v) }; }
    constexpr decltype(auto) evaluate(Ty&& o, Tuple&& v) {
        return std::forward<Ty>(o).evaluate(std::forward<Tuple>(v));
    }

    template<class Ty, class Tuple>
    using evaluate_result_t = decltype(kaixo::evaluate(std::declval<Ty>(), std::declval<Tuple>()));

    // ------------------------------------------------
    //                  Named Tuple
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
        constexpr decltype(auto) get(this Self&& self) {
            return recursive_get<Vars::template index<Find>>(std::forward<Self>(self).tuple);
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------
    //                  Expressions
    // ------------------------------------------------

    template<class Ty>
    concept unevaluated_range = unevaluated<Ty> && requires() { typename std::decay_t<Ty>::is_range; };

    template<class ...As>
    concept valid_expression_arguments = (unevaluated<As> || ...) && (!unevaluated_range<As> && ...) && (!std::ranges::range<As> && ...);

    // ------------------------------------------------

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
                if constexpr ((unevaluated<evaluate_result_t<Tys, Tuple>> || ...)) {
                    return tuple_operation<evaluate_result_t<Tys, Tuple>...>{
                        { kaixo::evaluate(std::forward<Tys>(tys), std::forward<Tuple>(v))... }
                    };
                } else {
                    return std::tuple<evaluate_result_t<Tys, Tuple>...>{ kaixo::evaluate(std::forward<Tys>(tys), std::forward<Tuple>(v))... };
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

    template<class A, class B, class Op> requires valid_expression_arguments<A, B>
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
        constexpr decltype(auto) evaluate(this Self&& self, Tuple&& v) {
            return std::forward<Self>(self).operation(
                kaixo::evaluate(std::forward<Self>(self).a, std::forward<Tuple>(v)),
                kaixo::evaluate(std::forward<Self>(self).b, std::forward<Tuple>(v)));
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
        constexpr decltype(auto) evaluate(this Self&& self, Tuple&& v) {
            return std::forward<Self>(self).operation(
                kaixo::evaluate(std::forward<Self>(self).a, std::forward<Tuple>(v)));
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
    //                     Range
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

    template<class Begin, class End, class Increment>
        requires (unevaluated<Begin> || unevaluated<End> || unevaluated<Increment>)
    struct range_implementation<Begin, End, Increment> : range_storage<Begin, End, Increment> {

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr auto evaluate(this Self&& self, Tuple&& tuple) {
            return range{
                kaixo::evaluate(std::forward<Self>(self).beginValue, std::forward<Tuple>(tuple)),
                kaixo::evaluate(std::forward<Self>(self).endValue, std::forward<Tuple>(tuple)),
                kaixo::evaluate(std::forward<Self>(self).increment, std::forward<Tuple>(tuple)),
            };
        }

        // ------------------------------------------------

    };

    template<class Begin, class End, class Increment>
        requires (!unevaluated<Begin> && !unevaluated<End> && !unevaluated<Increment>)
    struct range_implementation<Begin, End, Increment> : range_storage<Begin, End, Increment> {

        // ------------------------------------------------

        struct sentinel {};
        struct iterator {

            // ------------------------------------------------

            using value_type = Begin;
            using difference_type = std::ptrdiff_t;

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

    constexpr bool operator==(const auto&, inf_t) { return false; }
    constexpr bool operator==(inf_t, const auto&) { return false; }

    // ------------------------------------------------
    //                  Named Range
    // ------------------------------------------------

    template<class, class, class = detail::dud>
    struct named_range;

    // ------------------------------------------------

    template<class Vars, class Range, class Expression>
    struct named_range_storage {

        // ------------------------------------------------

        using defines = Vars;
        using depends = depends_t<Range>;

        // ------------------------------------------------

        using is_range = int;

        // ------------------------------------------------

        Range range;
        [[no_unique_address]] Expression expression;

        // ------------------------------------------------

        template<class Self, class Arg>
        constexpr decltype(auto) transform(this Self&& self, Arg&& arg) { 
            if constexpr (std::same_as<Expression, detail::dud>) return static_cast<Arg>(arg);
            else return kaixo::evaluate(std::forward<Self>(self).expression, named_tuple<Vars, Arg>{ std::forward<Arg>(arg) });
        }

        // ------------------------------------------------

    };
    
    // ------------------------------------------------

    template<class Vars, std::ranges::range Range, class Expression>
    struct named_range<Vars, Range, Expression> : named_range_storage<Vars, Range, Expression> {

        // ------------------------------------------------
                
        using base_iterator = std::ranges::iterator_t<Range>;
        using base_sentinel = std::ranges::sentinel_t<Range>;

        // ------------------------------------------------
        
        struct iterator : base_iterator {
            named_range<Vars, Range, Expression>* self = nullptr;
        
            constexpr decltype(auto) operator*() {
                return self->transform(base_iterator::operator*());
            }
        
            constexpr decltype(auto) operator[](std::size_t i) requires std::ranges::random_access_range<Range> {
                return self->transform(base_iterator::operator[](i));
            }
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
    
    template<class Vars, unevaluated_range Range, class Expression>
    struct named_range<Vars, Range, Expression> : named_range_storage<Vars, Range, Expression> {};

    // ------------------------------------------------
    
    // Construct named range
    template<std::ranges::range Range>
    constexpr std::views::all_t<Range> operator-(Range&& range) {
        return std::views::all(std::forward<Range>(range));
    }
    
    template<unevaluated_range Range>
    constexpr Range&& operator-(Range&& range) { return std::forward<Range>(range); }
    
    template<class ...Vars, class Range>
        requires (std::ranges::range<Range> || unevaluated_range<Range>)
    constexpr named_range<var<Vars...>, std::decay_t<Range>> operator<(var<Vars...>, Range&& range) {
        return { { 
            .range = std::forward<Range>(range),
        } };
    }

    // ------------------------------------------------
    
    template<unevaluated Expression, class Vars, class Range>
    constexpr named_range<Vars, Range, std::decay_t<Expression>> operator|(Expression&& e, named_range<Vars, Range>&& r) {
        return { { 
            .range = std::move(r.range),
            .expression = std::forward<Expression>(e),
        } };
    }

    // ------------------------------------------------
    
    // Combine complete named ranges
    template<class ...V1s, std::ranges::range Range1, class Expression, class ...V2s, std::ranges::range Range2>
    constexpr auto operator,(named_range<var<V1s...>, Range1, Expression>&& r1, named_range<var<V2s...>, Range2>&& r2)
        -> named_range<var<V1s..., V2s...>, std::ranges::cartesian_product_view<Range1, Range2>, Expression> 
    {
        using combined_t = std::ranges::cartesian_product_view<Range1, Range2>;
        return { {
            .range = combined_t{ std::forward<Range1>(r1.range), std::forward<Range2>(r2.range) },
            .expression = std::move(r1.expression),
        } };
    }

    // ------------------------------------------------

    // Combine complete with unevaluated range
    template<class Vars, unevaluated_range Range>
    struct range_evaluator {
        Range range;

        template<class Self, class Tuple>
        constexpr auto operator()(this Self&& self, Tuple&& tuple) {
            auto evaluated = kaixo::evaluate(std::forward<Self>(self).range, named_tuple<Vars, Tuple&&>{ std::forward<Tuple>(tuple) });
            auto combiner = [tuple = detail::store_as_tuple(std::forward<Tuple>(tuple))]<class R>(R && r) { 
                return std::make_tuple(tuple, std::forward<R>(r));
            };

            return std::views::transform(std::move(evaluated), std::move(combiner));
        }
    };

    template<class ...V1s, std::ranges::range Range1, class Expression, class ...V2s, unevaluated_range Range2>
    constexpr auto operator,(named_range<var<V1s...>, Range1, Expression>&& r1, named_range<var<V2s...>, Range2>&& r2)
        -> named_range<var<V1s..., V2s...>, std::ranges::join_view<std::ranges::transform_view<Range1, range_evaluator<var<V1s...>, Range2>>>, Expression>
    {
        using evaluated_t = std::ranges::transform_view<Range1, range_evaluator<var<V1s...>, Range2>>;
        using joined_t = std::ranges::join_view<evaluated_t>;
        return { {
            .range = joined_t{ evaluated_t{ std::move(r1.range), { std::move(r2.range) } } },
            .expression = std::move(r1.expression),
        } };
    }

    // ------------------------------------------------

    // Add range filter
    template<class Vars, class Condition>
    struct range_filter {
        Condition condition;

        template<class Tuple>
        constexpr bool operator()(Tuple&& tuple) const {
            return kaixo::evaluate(condition, named_tuple<Vars, Tuple&&>{ std::forward<Tuple>(tuple) });
        }
    };

    template<class Vars, std::ranges::range Range, class Expression, valid_expression_arguments Condition>
    constexpr auto operator,(named_range<Vars, Range, Expression>&& r1, Condition&& c)
        -> named_range<Vars, std::ranges::filter_view<Range, range_filter<Vars, Condition>>, Expression> 
    {
        using filtered_t = std::ranges::filter_view<Range, range_filter<Vars, Condition>>;
        return { {
            .range = filtered_t{ std::move(r1.range), { std::forward<Condition>(c) } },
            .expression = std::move(r1.expression),
        } };
    }

    // ------------------------------------------------
    //                     Zip
    // ------------------------------------------------

    template<class ...Ranges>
    struct zip_range;

    template<std::ranges::range ...Ranges>
    struct zip_range<Ranges...> : std::ranges::zip_view<std::views::all_t<Ranges>...> {
        using std::ranges::zip_view<std::views::all_t<Ranges>...>::zip_view;
    };
    
    template<class ...Ranges>   
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
                return (kaixo::evaluate(std::forward<Args>(args), tuple), ...);
            }, static_cast<const std::tuple<Ranges...>&>(self));
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------
    
    template<class A, class B>
        requires ((unevaluated_range<A> || std::ranges::range<A>) && (unevaluated_range<B> || std::ranges::range<B>))
    constexpr zip_range<A, B> operator,(A&& a, B&& b) {
        return zip_range<A, B>{ std::forward<A>(a), std::forward<B>(b) };
    }

    // ------------------------------------------------
    //                   Break
    // ------------------------------------------------
    
    template<class Vars, unevaluated Break>
    struct break_point {

        // ------------------------------------------------

        Break expression{};

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr bool operator()(this Self&& self, Tuple&& tuple) {
            return !static_cast<bool>(kaixo::evaluate(std::forward<Self>(self).expression, named_tuple<Vars, Tuple&&>{ std::forward<Tuple>(tuple) }));
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------

    constexpr struct break_t {

        // ------------------------------------------------

        template<unevaluated Break>
        constexpr break_point<var<>, Break> operator=(Break&& e) const {
            return { std::forward<Break>(e) };
        }

        // ------------------------------------------------

    } brk;

    // ------------------------------------------------

    template<class Vars, std::ranges::range Range, class Expression, unevaluated Break>
    constexpr auto operator,(named_range<Vars, Range, Expression>&& r, break_point<var<>, Break>&& b) 
        -> named_range<Vars, std::ranges::take_while_view<Range, break_point<Vars, Break>>, Expression> {
        using break_t = std::ranges::take_while_view<Range, break_point<Vars, Break>>;
        return { {
            .range = break_t{ std::move(r.range), break_point<Vars, Break>{ std::move(b.expression) } },
            .expression = std::move(r.expression),
        } };
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

    constexpr named_tuple<var<A, B, C>, std::tuple<std::tuple<int, float>, double>> values{ { { 1, 2 }, 3 } };
    constexpr named_tuple<var<D, E>, std::tuple<int, float>> values2{ { 4, 5 } };
    
    std::vector<int> r1{ 1, 2, 3, 4 };
    std::map<int, int> r2{ { 1, 2 }, { 3, 4 } };

    for (auto a : a | a <- range(0, inf), brk = a == 10) {
        std::println("({})", a);
    }
    
    for (auto [a, b, c] : ((a, b, c) | a <- range(1, 10), (b, c) <- (r1, range(1, a)))) {
        std::println("({}, {}, {})", a, b, c);
    }
    
    for (auto [a, b, c] : (a, b, c) | (a, b) <- r2, c <- r1) {
        std::println("({}, {}, {})", a, b, c);
    }

    auto named = ((a, b, c) | a <- range(1, 100), b <- range(1, a), c <- range(1, b), a * a - b * b == c * c);
    
    for (auto tpl : named) {

        auto [a, b, c] = detail::flatten_tuple(tpl);

        std::println("({}, {}, {})", a, b, c);
    }

    return 0;
}

// ------------------------------------------------


/*
named_range<
    var<A, B, C>,
    std::ranges::filter_view<
        std::ranges::join_view<
            std::ranges::transform_view<
                std::ranges::join_view<
                    std::ranges::transform_view<
                        std::ranges::owning_view<range<int, inf_t>>,
                        range_evaluator<var<A>, range<int, var<A>>>
                    >
                >,
                range_evaluator<var<A, B>, range<int, var<B>>>
            >
        >,
        range_filter<
            var<A, B, C>,
            binary_operation<
                binary_operation<
                    binary_operation<var<A>, var<A>, multiply_operator>,
                    binary_operation<var<B>, var<B>, multiply_operator>,
                    subtract_operator
                >,
                binary_operation<var<C>, var<C>, multiply_operator>,
                equals_operator
            >
        >
    >,
    tuple_operation<var<A>, var<B>, var<C>>
>

named_range<
    var<A, B, C>,
    std::ranges::filter_view<
        std::ranges::join_view<
            std::ranges::transform_view<
                std::ranges::cartesian_product_view<
                    std::ranges::owning_view<range<int, inf_t>>,
                    std::ranges::ref_view<std::vector<int>>
                >,
                range_evaluator<var<A, B>, range<int, var<B>>>
            >
        >,
        range_filter<
            var<A, B, C>,
            binary_operation<
                binary_operation<
                    binary_operation<var<A>, var<A>, multiply_operator>,
                    binary_operation<var<B>, var<B>, multiply_operator>,
                    subtract_operator
                >,
                binary_operation<var<C>, var<C>, multiply_operator>,
                equals_operator
            >
        >
    >,
    tuple_operation<var<A>, var<B>, var<C>>
>


*/