
// ------------------------------------------------

#pragma once

// ------------------------------------------------

#include <algorithm>
#include <cstddef>
#include <functional>
#include <ranges>
#include <tuple>
#include <type_traits>
#include <utility>

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
                    return std::tuple_cat(flatten_tuple(static_cast<std::tuple_element_t<Is, std::decay_t<Ty>>>(std::get<Is>(t)))...);
                }(std::make_index_sequence<std::tuple_size_v<std::decay_t<Ty>>>{});
            } else return std::tuple<Ty>{ std::forward<Ty>(t) };
        }

        // ------------------------------------------------

    }

    // ------------------------------------------------
    
    template<class Ty>
    concept not_dud = !std::same_as<std::decay_t<Ty>, detail::dud>;

    // ------------------------------------------------

    template<class Tuple>
    constexpr std::size_t recursive_tuple_size_v = std::tuple_size_v<decltype(detail::flatten_tuple(std::declval<Tuple>()))>;

    template<class Tuple>
    using flatten_tuple_t = decltype(detail::flatten_tuple(std::declval<Tuple>()));
    
    template<std::size_t I, class Tuple>
    using recursive_tuple_element_t = std::tuple_element_t<I, flatten_tuple_t<Tuple>>;

    template<std::size_t I, class Tuple>
    constexpr recursive_tuple_element_t<I, Tuple&&> recursive_get(Tuple&& tuple) {
        return std::get<I>(detail::flatten_tuple(tuple));
    }

    // ------------------------------------------------
    //                      Var
    // ------------------------------------------------

    template<class Ty>
    concept is_var = requires { typename std::decay_t<Ty>::is_var; };

    template<class... Vars>
    struct var {

        // ------------------------------------------------

        using defines = var<>;
        using depends = var;

        // ------------------------------------------------
        
        using is_var = int;

        // ------------------------------------------------

        constexpr static std::size_t size = sizeof...(Vars);

        template<class Find>
        constexpr static std::size_t index = [](std::size_t i = 0) {
            return ((i++, std::same_as<Find, Vars>) || ...) ? i - 1 : npos;
        }();

        // ------------------------------------------------

        template<class Tuple>
        constexpr static decltype(auto) evaluate(Tuple&& v) {
            using tplDefines = std::decay_t<Tuple>::defines;
            // Only a single var, returns single value
            if constexpr (size == 1) {
                using Var = std::type_identity_t<Vars...>;
                // First case, variable is 'dud', used for ignoring a value with '_'.
                if constexpr (std::same_as<Var, detail::dud>) return detail::dud{};
                // Second case, variable is not defined by input tuple, return var (remains unevaluated)
                else if constexpr (tplDefines::template index<Var> == npos) return var{};
                // Third case, get the value from the tuple
                else return v.template get<Var>();
            } 
            // Otherwise multiple vars; ff none of the vars stay unevaluated, return resulting tuple.
            else if constexpr (((tplDefines::template index<Vars> != npos) && ...)) {
                return std::tuple<decltype(var<Vars>::evaluate(std::forward<Tuple>(v)))...>{
                    var<Vars>::evaluate(std::forward<Tuple>(v))...
                };
            } 
            // Otherwise, use the comma operator to construct a tuple_operation for later evaluation
            else return (var<Vars>::evaluate(std::forward<Tuple>(v)), ...);
        }

        // ------------------------------------------------
        
        template<class Class, class Type> 
        constexpr auto operator[](Type(Class::* ptr)) const;
        
        template<class Index> 
        constexpr auto operator[](Index&& index) const;

        template<class Class, class Type, class ...Args, class ...Tys>
        constexpr auto operator()(Type(Class::* ptr)(Args...), Tys&&... tys) const;

        // ------------------------------------------------
      
        template<class ...Args>
            requires (sizeof...(Vars) == recursive_tuple_size_v<std::tuple<Args...>>)
        constexpr auto operator=(std::tuple<Args...>&& args) const;
        
        template<class Arg>
            requires (!kaixo::is_var<Arg> && sizeof...(Vars) == recursive_tuple_size_v<std::tuple<Arg>>)
        constexpr auto operator=(Arg&& arg) const;

        // ------------------------------------------------
        
    };

    // ------------------------------------------------

    template<class ...As, class ...Bs>
    constexpr var<As..., Bs...> operator,(var<As...>, var<Bs...>) { return {}; }
    template<class ...As>
    constexpr var<As..., detail::dud> operator,(var<As...>, detail::dud) { return {}; }
    template<class ...As>
    constexpr var<detail::dud, As...> operator,(detail::dud, var<As...>) { return {}; }

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
    template<> struct concat<> : std::type_identity<var<>> {};
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

    template<class ...Args> struct depends<std::tuple<Args...>> : std::type_identity<unique_t<concat_t<depends_t<std::decay_t<Args>>...>>> {};
    template<class ...Args> struct defines<std::tuple<Args...>> : std::type_identity<unique_t<concat_t<depends_t<std::decay_t<Args>>...>>> {};

    template<class Ty> requires requires { typename std::decay_t<Ty>::depends; }
    struct depends<Ty> : std::type_identity<typename std::decay_t<Ty>::depends> {};

    template<class Ty> requires requires { typename std::decay_t<Ty>::defines; }
    struct defines<Ty> : std::type_identity<typename std::decay_t<Ty>::defines> {};

    // ------------------------------------------------

    template<class Vars, class For> // Checks if Vars is enough to fully evaluate For
    concept has_all_defines_for = unique_t<concat_t<depends_t<For>, Vars>>::size == Vars::size;

    // ------------------------------------------------
    //                    Evaluate
    // ------------------------------------------------

    template<class, class, class = detail::dud>
    struct named_range;

    // ------------------------------------------------

    template<class Ty>
    concept unevaluated = depends_t<std::decay_t<Ty>>::size != 0 || requires { typename std::decay_t<Ty>::explicitly_unevaluated; };
    
    template<class Ty, class Tuple> 
        requires (!unevaluated<Ty>)
    constexpr auto evaluate(Ty&& o, Tuple&&) { return std::forward<Ty>(o); }
    
    template<unevaluated Ty, class Tuple> 
        requires requires (Ty&& o, Tuple&& v) { { std::forward<Ty>(o).evaluate(v) }; }
    constexpr auto evaluate(Ty&& o, Tuple&& v)
        -> decltype(std::forward<Ty>(o).evaluate(v))
    {
        return std::forward<Ty>(o).evaluate(v);
    }
    
    // Owning view does not define a copy constructor, so here we 
    // manually copy the contained range into a new owning_view.
    // This is only really used in conjunction with the expression_wrapper
    // in the case that the stored expression is itself an owning_view.
    template<std::copy_constructible Ty, class Tuple>
    constexpr std::ranges::owning_view<Ty> evaluate(const std::ranges::owning_view<Ty>& o, Tuple&&) {
        return std::views::all(static_cast<Ty>(o.base()));
    }
    
    template<std::copy_constructible Ty, class Tuple>
    constexpr std::ranges::owning_view<Ty> evaluate(std::ranges::owning_view<Ty>& o, Tuple&&) {
        return std::views::all(static_cast<Ty>(o.base()));
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

        using depends = depends_t<std::decay_t<Tuple>>;
        using defines = Vars;

        // ------------------------------------------------

        Tuple tuple;

        // ------------------------------------------------

        template<class Find, class Self>
        constexpr auto get(this Self&& self) 
            -> recursive_tuple_element_t<Vars::template index<Find>, decltype(std::declval<Self&&>().tuple)>
        {
            return recursive_get<Vars::template index<Find>>(std::forward<Self>(self).tuple);
        }

        // ------------------------------------------------
        
        template<class Self, class Tuple>
        constexpr auto evaluate(this Self&& self, Tuple&& tuple) {
            return std::apply([&]<class ...Tys>(Tys&& ...tys) {
                using tuple_t = decltype(std::make_tuple(kaixo::evaluate(std::declval<Tys&&>(), std::declval<Tuple&>())...));
                return named_tuple<Vars, tuple_t>{ .tuple = { kaixo::evaluate(std::forward<Tys>(tys), tuple)... } };
            }, std::forward<Self>(self).tuple);
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------
    
    template< class ...V1s
            , class ...As
            , class ...V2s
            , class ...Bs>
    constexpr auto operator,( named_tuple<var<V1s...>, std::tuple<As...>>&& a
                            , named_tuple<var<V2s...>, std::tuple<Bs...>>&& b)
        -> named_tuple<var<V1s..., V2s...>, std::tuple<As..., Bs...>>
    {
        return { .tuple = std::tuple_cat(std::move(a).tuple, std::move(b).tuple) };
    }

    // ------------------------------------------------

    template<class ...Vars>
    template<class ...Args>
        requires (sizeof...(Vars) == recursive_tuple_size_v<std::tuple<Args...>>)
    constexpr auto var<Vars...>::operator=(std::tuple<Args...>&& args) const {
        return named_tuple<var<Vars...>, std::tuple<std::decay_t<Args>...>>{
            .tuple = std::move(args),
        };
    }
    
    template<class ...Vars>
    template<class Arg>
        requires (!is_var<Arg> && sizeof...(Vars) == recursive_tuple_size_v<std::tuple<Arg>>)
    constexpr auto var<Vars...>::operator=(Arg&& arg) const {
        return named_tuple<var<Vars...>, std::tuple<std::decay_t<Arg>>> {
            .tuple = { std::forward<Arg>(arg) },
        };
    }

    // ------------------------------------------------
    //                  Expressions
    // ------------------------------------------------
    //  Lazy evaluated expressions containing variables
    //  that can be substituted with values using
    //  a named_tuple instance and the 'evaluate'
    //  function.
    // ------------------------------------------------

    template<class Ty>
    concept explicit_unevaluated_range = unevaluated<Ty> && requires { typename std::decay_t<Ty>::is_range; };
    
    template<class Ty>
    concept unevaluated_range = is_var<Ty> || explicit_unevaluated_range<Ty>;

    template<class Ty>
    concept evaluated_range = !unevaluated<Ty> && std::ranges::range<Ty>;
    
    template<class Ty>
    concept any_range = unevaluated_range<Ty> || evaluated_range<Ty>;
    
    template<class Ty>
    concept explicit_range = explicit_unevaluated_range<Ty> || evaluated_range<Ty>;
    
    template<class ...As>
    concept valid_overload_arguments =
           (unevaluated<As> || ...) // Overloads require at least 1 argument to be unevaluated
        && (not_dud<As> && ...);    // Dud is not allowed in expressions

    template<class ...As>
    concept valid_expression_arguments =
           (unevaluated<As> || ...)      // Expressions require at least 1 argument to be unevaluated
        && (not_dud<As> && ...)          // Dud is not allowed in expressions
        && (!explicit_range<As> && ...); // Cannot be a range, as this messes with the operator overloads.
    
    template<class ...As>
    concept valid_tuple_arguments =
           (unevaluated<As> || ...)  // Tuple operation requires at least 1 argument to be unevaluated
        && (not_dud<As> && ...);     // Dud is not allowed in expressions
    
    // ------------------------------------------------
    
    // Tuple operation is an expression resulting in a tuple.
    // This can be constructed using the comma operator.
    template<class ...Args>
        requires (sizeof...(Args) == 0 || valid_tuple_arguments<Args...>)
    struct tuple_operation {

        // ------------------------------------------------

        using defines = var<>;
        using depends = unique_t<concat_t<depends_t<Args>...>>;

        // ------------------------------------------------
        
        using explicitly_unevaluated = int; // Necessary for when empty tuple

        // ------------------------------------------------

        std::tuple<Args...> args;

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr auto evaluate(this Self&& self, Tuple&& v) {
            if constexpr (sizeof...(Args) == 0) return std::tuple{};
            else return std::apply([&]<class ...Tys>(Tys&& ...tys) {
                // First case, at least one of the elements cannot be fully evaluated
                // return another tuple_operation for later evaluation.
                if constexpr ((unevaluated<evaluate_result_t<Tys&&, Tuple>> || ...)) {
                    return tuple_operation<evaluate_result_t<Tys&&, Tuple>...>{
                        { kaixo::evaluate(std::forward<Tys>(tys), v)... }
                    };
                }
                // Second case, fully evaluated, return results as tuple.
                else return std::tuple<evaluate_result_t<Tys&&, Tuple>...>{ 
                    kaixo::evaluate(std::forward<Tys>(tys), v)...
                };
            }, std::forward<Self>(self).args);
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------

    // Handles default case for tuple operation
    template<class A, class B> 
        requires valid_expression_arguments<A, B>
    constexpr tuple_operation<std::decay_t<A>, std::decay_t<B>> operator,(A&& a, B&& b) {
        return { { std::forward<A>(a), std::forward<B>(b) } };
    }

    // Adds to an existing tuple operation
    template<class ...Args, class B> 
        requires valid_tuple_arguments<Args..., B>
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

        [[no_unique_address]] A a;
        [[no_unique_address]] B b;
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
        constexpr auto operator()(A&& a, B&& b) const                                                         \
            -> decltype(std::declval<A&&>() op std::declval<B&&>())                                           \
        {                                                                                                     \
            return std::forward<A>(a) op std::forward<B>(b);                                                  \
        }                                                                                                     \
    };                                                                                                        \
                                                                                                              \
    template<class A, class B> requires valid_expression_arguments<A, B>                                      \
    constexpr binary_operation<std::decay_t<A>, std::decay_t<B>, name##_operator> operator op(A&& a, B&& b) { \
        return { std::forward<A>(a), std::forward<B>(b) };                                                    \
    }

    KAIXO_BINARY_OP(+, add);         
    KAIXO_BINARY_OP(-, subtract);    
    KAIXO_BINARY_OP(*, multiply);    
    KAIXO_BINARY_OP(/ , divide);     
    KAIXO_BINARY_OP(%, modulo);  

    KAIXO_BINARY_OP(&, bit_and); 
    KAIXO_BINARY_OP(| , bit_or);
    KAIXO_BINARY_OP(^, bit_xor);
    KAIXO_BINARY_OP(&&, logic_and);
    KAIXO_BINARY_OP(|| , logic_or);

    KAIXO_BINARY_OP(< , less_than);
    KAIXO_BINARY_OP(> , greater_than);
    KAIXO_BINARY_OP(<= , less_or_equals);
    KAIXO_BINARY_OP(>= , greater_or_equals);
    KAIXO_BINARY_OP(== , equals);
    KAIXO_BINARY_OP(!= , not_equals);

    KAIXO_BINARY_OP(*=, multiply_assign);
    KAIXO_BINARY_OP(/=, divide_assign);
    KAIXO_BINARY_OP(%=, modulo_assign);
    KAIXO_BINARY_OP(&=, bit_and_assign);
    KAIXO_BINARY_OP(+=, add_assign);         
    KAIXO_BINARY_OP(-=, subtract_assign);    
    KAIXO_BINARY_OP(|=, bit_or_assign);
    KAIXO_BINARY_OP(^=, bit_xor_assign);

    // ------------------------------------------------

    // Unary operation overloads all unary operators for expressions.
    template<valid_expression_arguments A, class Op>
    struct unary_operation {

        // ------------------------------------------------

        using defines = var<>;
        using depends = depends_t<A>;

        // ------------------------------------------------

        [[no_unique_address]] A a;
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
    KAIXO_UNARY_OP(!, logic_not);
    //KAIXO_UNARY_OP(&, pointer);    // <<< Crashes MSVC for some reason...
    KAIXO_UNARY_OP(*, dereference);
    
    // ------------------------------------------------
    //                Index Operation
    // ------------------------------------------------
    //  Allows the index operator to be used inside
    //  expressions.
    // ------------------------------------------------
    
    template<unevaluated Expression, class Class, class Type>
    struct member_operation;

    template<unevaluated Expression, class Fptr, class TupleOperation>
    struct member_function_operation;

    template<class Expression, class Index>
        requires (unevaluated<Expression> || unevaluated<Index>)
    struct index_operation {

        // ------------------------------------------------

        [[no_unique_address]] Expression expression;
        [[no_unique_address]] Index index;

        // ------------------------------------------------

        using defines = var<>;
        using depends = unique_t<concat_t<depends_t<Expression>, depends_t<Index>>>;

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr decltype(auto) evaluate(this Self&& self, Tuple&& tuple) {
            using evaluated1_t = decltype(kaixo::evaluate(std::declval<Self&&>().expression, std::declval<Tuple&>()));
            using evaluated2_t = decltype(kaixo::evaluate(std::declval<Self&&>().index, std::declval<Tuple&>()));
            // First case, either the expression or the index is still unevaluated, return new index operation
            if constexpr (unevaluated<evaluated1_t> || unevaluated<evaluated2_t>) {
                return index_operation<evaluated1_t, evaluated2_t>{
                    .expression = kaixo::evaluate(std::forward<Self>(self).expression, tuple),
                    .index = kaixo::evaluate(std::forward<Self>(self).index, tuple),
                };
            }
            // Second case, both expression and index are fully evaluated
            else return kaixo::evaluate(std::forward<Self>(self).expression, tuple)[kaixo::evaluate(std::forward<Self>(self).index, tuple)];
        }

        // ------------------------------------------------

        template<class Self, class Class, class Type>
        constexpr member_operation<index_operation, Class, Type> operator[](this Self&& self, Type(Class::* ptr)) {
            return {
                .expression = std::forward<Self>(self), 
                .ptr = ptr,
            };
        }
        
        template<class Self, class Index>
        constexpr index_operation<index_operation, std::decay_t<Index>> operator[](this Self&& self, Index&& index) {
            return {
                .expression = std::forward<Self>(self), 
                .index = std::forward<Index>(index),
            };
        }

        template<class Self, class Class, class Type, class ...Args, class ...Tys>
        constexpr auto operator()(this Self&& self, Type(Class::* ptr)(Args...), Tys&&... tys) 
            -> member_function_operation<index_operation, Type(Class::*)(Args...), tuple_operation<std::decay_t<Tys>...>>
        {
            return {
                .expression = std::forward<Self>(self), 
                .ptr = ptr,
                .arguments = { .args = { std::forward<Tys>(tys)... } },
            };
        }

        // ------------------------------------------------

    };

    template<class ...Vars>
    template<class Index>
    constexpr auto var<Vars...>::operator[](Index&& index) const {
        return index_operation<var<Vars...>, std::decay_t<Index>>{
            .index = std::forward<Index>(index) 
        };
    }
    
    // ------------------------------------------------
    //                Member Operation
    // ------------------------------------------------
    //  Access a class member inside an expression
    // ------------------------------------------------
    
    template<unevaluated Expression, class Class, class Type>
    struct member_operation {

        // ------------------------------------------------

        [[no_unique_address]] Expression expression;
        Type(Class::* ptr);

        // ------------------------------------------------

        using defines = var<>;
        using depends = depends_t<Expression>;

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr decltype(auto) evaluate(this Self&& self, Tuple&& tuple) {
            using evaluated_t = decltype(kaixo::evaluate(std::declval<Self&&>().expression, std::declval<Tuple&>()));
            // First case, result still not fully evaluated, so return another member operation
            if constexpr (unevaluated<evaluated_t>) {
                return member_operation<evaluated_t, Class, Type>{
                    .expression = kaixo::evaluate(std::forward<Self>(self).expression, tuple),
                    .ptr = std::forward<Self>(self).ptr,
                };
            } 
            // Second case, fully evaluated, so access the member
            else return kaixo::evaluate(std::forward<Self>(self).expression, tuple).*(std::forward<Self>(self).ptr);
        }

        // ------------------------------------------------
        
        template<class Self, class Class, class Type>
        constexpr member_operation<member_operation, Class, Type> operator[](this Self&& self, Type(Class::* ptr)) {
            return {
                .expression = std::forward<Self>(self), 
                .ptr = ptr,
            };
        }
        
        template<class Self, class Index>
        constexpr index_operation<member_operation, std::decay_t<Index>> operator[](this Self&& self, Index&& index) {
            return {
                .expression = std::forward<Self>(self), 
                .index = std::forward<Index>(index),
            };
        }
                
        template<class Self, class Class, class Type, class ...Args, class ...Tys>
        constexpr auto operator()(this Self&& self, Type(Class::* ptr)(Args...), Tys&&... tys) 
            -> member_function_operation<member_operation, Type(Class::*)(Args...), tuple_operation<std::decay_t<Tys>...>>
        {
            return {
                .expression = std::forward<Self>(self), 
                .ptr = ptr,
                .arguments = { .args = { std::forward<Tys>(tys)... } },
            };
        }


        // ------------------------------------------------

    };

    // ------------------------------------------------

    template<class ...Vars>
    template<class Class, class Type>
    constexpr auto var<Vars...>::operator[](Type(Class::* ptr)) const {
        return member_operation<var<Vars...>, Class, Type>{ .ptr = ptr };
    }
    
    // ------------------------------------------------
    //                Member Operation
    // ------------------------------------------------
    //  Access a class member inside an expression
    // ------------------------------------------------
    
    template<unevaluated Expression, class Fptr, class TupleOperation>
    struct member_function_operation {

        // ------------------------------------------------

        [[no_unique_address]] Expression expression;
        Fptr ptr;
        TupleOperation arguments;

        // ------------------------------------------------

        using defines = var<>;
        using depends = unique_t<concat_t<depends_t<Expression>, depends_t<TupleOperation>>>;

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr decltype(auto) evaluate(this Self&& self, Tuple&& tuple) {
            using evaluated1_t = decltype(kaixo::evaluate(std::declval<Self&&>().expression, std::declval<Tuple&>()));
            using evaluated2_t = decltype(kaixo::evaluate(std::declval<Self&&>().arguments, std::declval<Tuple&>()));
            // First case, result still not fully evaluated, so return another member operation
            if constexpr (unevaluated<evaluated1_t> || unevaluated<evaluated2_t>) {
                return member_function_operation<evaluated1_t, Fptr, evaluated2_t>{
                    .expression = kaixo::evaluate(std::forward<Self>(self).expression, tuple),
                    .ptr = std::forward<Self>(self).ptr,
                    .arguments = kaixo::evaluate(std::forward<Self>(self).arguments, tuple),
                };
            } 
            // Second case, fully evaluated, so access the member
            else return std::apply([&]<class ...Args>(Args&& ...args) {
                return (kaixo::evaluate(std::forward<Self>(self).expression, tuple)
                    .*(std::forward<Self>(self).ptr))(std::forward<Args>(args)...);
            }, kaixo::evaluate(std::forward<Self>(self).arguments, tuple));
        }

        // ------------------------------------------------
        
        template<class Self, class Class, class Type>
        constexpr member_operation<member_function_operation, Class, Type> operator[](this Self&& self, Type(Class::* ptr)) {
            return {
                .expression = std::forward<Self>(self), 
                .ptr = ptr,
            };
        }
        
        template<class Self, class Index>
        constexpr index_operation<member_function_operation, std::decay_t<Index>> operator[](this Self&& self, Index&& index) {
            return {
                .expression = std::forward<Self>(self), 
                .index = std::forward<Index>(index),
            };
        }

        template<class Self, class Class, class Type, class ...Args, class ...Tys>
        constexpr auto operator()(this Self&& self, Type(Class::* ptr)(Args...), Tys&&... tys) 
            -> member_function_operation<member_function_operation, Type(Class::*)(Args...), tuple_operation<std::decay_t<Tys>...>>
        {
            return {
                .expression = std::forward<Self>(self), 
                .ptr = ptr,
                .arguments = { { std::forward<Tys>(tys)... } },
            };
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------

    template<class ...Vars>
    template<class Class, class Type, class ...Args, class ...Tys>
    constexpr auto var<Vars...>::operator()(Type(Class::* ptr)(Args...), Tys&&... tys) const {
        using tuple_t = tuple_operation<std::decay_t<Tys>...>;
        return member_function_operation<var<Vars...>, Type (Class::*)(Args...), tuple_t>{
            .ptr = ptr,
            .arguments = tuple_t{ { std::forward<Tys>(tys)... } },
        };
    }

    // ------------------------------------------------
    //                 Named Range
    // ------------------------------------------------
    //  Constructs a named_tuple using the value_type
    //  of the range, and the variables. This is then
    //  used to evaluate the Expression, which will
    //  be the result type of this named range.
    // ------------------------------------------------

    template<class Vars, class Range, class Expression>
    struct named_range_storage {

        // ------------------------------------------------

        using defines = Vars;
        using depends = remove_all_t<unique_t<concat_t<depends_t<Range>, depends_t<Expression>>>, Vars>;

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
        requires (Vars::size == recursive_tuple_size_v<std::ranges::range_value_t<Range>> && has_all_defines_for<Vars, Expression>)
    struct named_range<Vars, Range, Expression> : named_range_storage<Vars, Range, Expression> {
        
        // ------------------------------------------------
        
        // Some ranges (like std::views::filter) cannot const-iterator, because they have side-effects
        // For that reason, we need to separate the base_iterator types for const and non-const like
        // this, because evaluating std::ranges::begin for a const-version that cannot const iterate
        // will cause a compile-time error.
        constexpr static bool can_const_iterate = std::ranges::range<const Range>;

        template<bool Const>
        struct base_iterator_types;
        
        template<bool Const>
            requires (!Const || !can_const_iterate)
        struct base_iterator_types<Const> {
            using base_iterator = decltype(std::ranges::begin(std::declval<named_range_storage<Vars, Range, Expression>&>().range));
            using base_sentinel = decltype(std::ranges::end(std::declval<named_range_storage<Vars, Range, Expression>&>().range));
        };
        
        template<bool Const>
            requires (Const && can_const_iterate)
        struct base_iterator_types<Const>{
            using base_iterator = decltype(std::ranges::begin(std::declval<const named_range_storage<Vars, Range, Expression>&>().range));
            using base_sentinel = decltype(std::ranges::end(std::declval<const named_range_storage<Vars, Range, Expression>&>().range));
        };

        using base_iterator = base_iterator_types<false>::base_iterator;
        using base_sentinel = base_iterator_types<false>::base_sentinel;
        using base_const_iterator = base_iterator_types<true>::base_iterator;
        using base_const_sentinel = base_iterator_types<true>::base_sentinel;

        // ------------------------------------------------
        
        // Iterator implementation just wraps around the Range's iterator
        // and only modifies the output when an Expression is defined.
        template<bool Const>
        struct iterator_impl {

            // ------------------------------------------------

            using self_type = std::conditional_t<Const, 
                const named_range_storage<Vars, Range, Expression>, 
                      named_range_storage<Vars, Range, Expression>>;

            using base_iterator_type = base_iterator_types<Const>::base_iterator;
            using reference = decltype(std::declval<self_type&>().transform(std::declval<std::iter_reference_t<base_iterator_type>>()));
            using value_type = std::decay_t<reference>;
            using difference_type = std::iter_difference_t<base_iterator_type>;
            using iterator_category = std::conditional_t<
                std::random_access_iterator<base_iterator_type>,
                std::random_access_iterator_tag, 
                std::conditional_t<
                    std::bidirectional_iterator<base_iterator_type>,
                    std::bidirectional_iterator_tag, 
                    std::conditional_t<
                        std::forward_iterator<base_iterator_type>,
                        std::forward_iterator_tag,
                        std::input_iterator_tag>>>;

            // ------------------------------------------------

            base_iterator_type base{};
            self_type* self = nullptr;

            // ------------------------------------------------
            
            constexpr operator iterator_impl<true>() const { return { base, self }; }

            // ------------------------------------------------

            constexpr iterator_impl& operator++() {
                ++base;
                return *this;
            }

            constexpr iterator_impl operator++(int) {
                iterator_impl copy = *this;
                ++base;
                return copy;
            }
            
            constexpr iterator_impl& operator--() requires std::ranges::bidirectional_range<Range> {
                --base;
                return *this;
            }

            constexpr iterator_impl operator--(int) requires std::ranges::bidirectional_range<Range> {
                iterator_impl copy = *this;
                --base;
                return copy;
            }

            // ------------------------------------------------

            template<class Self>
            constexpr reference operator*(this Self&& me) {
                return std::forward<Self>(me).self->transform(*std::forward<Self>(me).base);
            }

            // ------------------------------------------------

            constexpr static friend iterator_impl operator+(const iterator_impl& s, difference_type i)
                requires std::ranges::random_access_range<Range> { return iterator_impl{ s.base + i, s.self }; }
            
            constexpr static friend iterator_impl operator+(difference_type i, const iterator_impl& s)
                requires std::ranges::random_access_range<Range> { return iterator_impl{ i + s.base, s.self }; }
            
            constexpr static friend iterator_impl operator-(const iterator_impl& s, difference_type i)
                requires std::ranges::random_access_range<Range> { return iterator_impl{ s.base - i, s.self }; }
            
            constexpr static friend iterator_impl operator-(difference_type i, const iterator_impl& s)
                requires std::ranges::random_access_range<Range> { return iterator_impl{ i - s.base, s.self }; }

            // ------------------------------------------------

            constexpr iterator_impl& operator+=(difference_type i) requires std::ranges::random_access_range<Range> {
                base += i;
                return *this;
            }

            constexpr iterator_impl& operator-=(difference_type i) requires std::ranges::random_access_range<Range> {
                base -= i;
                return *this;
            }

            // ------------------------------------------------

            constexpr friend difference_type operator-(const iterator_impl& i, const iterator_impl& s) requires std::ranges::random_access_range<Range> {
                return i.base - s.base;
            }

            // ------------------------------------------------

            constexpr bool operator< (const iterator_impl& o) const requires std::ranges::random_access_range<Range> { return base <  o.base; }
            constexpr bool operator<=(const iterator_impl& o) const requires std::ranges::random_access_range<Range> { return base <= o.base; }
            constexpr bool operator> (const iterator_impl& o) const requires std::ranges::random_access_range<Range> { return base >  o.base; }
            constexpr bool operator>=(const iterator_impl& o) const requires std::ranges::random_access_range<Range> { return base >= o.base; }

            // ------------------------------------------------

            constexpr reference operator[](difference_type i) const requires std::ranges::random_access_range<Range> {
                return self->transform(base[i]);
            }

            // ------------------------------------------------

            constexpr bool operator==(const auto& o) const { return base == o; }

            // ------------------------------------------------

        };
        
        // ------------------------------------------------
        
        using iterator = iterator_impl<false>;
        using const_iterator = iterator_impl<true>;

        // ------------------------------------------------

        constexpr iterator begin() { return { std::ranges::begin(this->range), this }; }
        constexpr base_sentinel end() { return std::ranges::end(this->range); }

        constexpr const_iterator begin() const requires can_const_iterate { return { std::ranges::begin(this->range), this };  }
        constexpr base_const_sentinel end() const requires can_const_iterate { return std::ranges::end(this->range); }

        // ------------------------------------------------
        
        constexpr std::size_t size() const requires std::ranges::sized_range<Range> {
            return std::ranges::size(this->range);
        }

        // ------------------------------------------------
        
        template<class Ty, class Self>
            requires requires(Self&& self) { { std::ranges::to<Ty>(std::forward<Self>(self)) } -> std::same_as<Ty>; }
        constexpr operator Ty(this Self&& self) {
            return std::ranges::to<Ty>(std::forward<Self>(self));
        }
        
        template<template<class...> class Ty, class Self>
            requires requires(Self&& self) { { std::ranges::to<Ty>(std::forward<Self>(self)) }; }
        constexpr auto as(this Self&& self) {
            return std::ranges::to<Ty>(std::forward<Self>(self));
        }
        
        template<class Ty, class Self>
            requires requires(Self&& self) { { std::ranges::to<Ty>(std::forward<Self>(self)) } -> std::same_as<Ty>; }
        constexpr auto as(this Self&& self) {
            return std::ranges::to<Ty>(std::forward<Self>(self));
        }

        // ------------------------------------------------

        template<class Self>
            requires std::ranges::random_access_range<Range>
        constexpr decltype(auto) operator[](this Self&& self, std::size_t i) {
            return std::forward<Self>(self).transform(std::forward<Self>(self).range[i]);
        }
        
        // ------------------------------------------------
        
        template<class Self, class Index>
        constexpr index_operation<named_range, std::decay_t<Index>> operator[](this Self&& self, Index&& index) {
            return {
                .expression = std::forward<Self>(self), 
                .index = std::forward<Index>(index),
            };
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------

    template<class Vars, class Range, class Expression> // Unevaluated version
        requires (unevaluated<Range> || !has_all_defines_for<Vars, Expression>)
    struct named_range<Vars, Range, Expression> : named_range_storage<Vars, Range, Expression> {

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr auto evaluate(this Self&& self, Tuple&& tuple) {
            using evaluated_t = decltype(kaixo::evaluate(std::declval<Self&&>().range, std::declval<Tuple&>()));
            using expression_t = decltype(kaixo::evaluate(std::declval<Self&&>().expression, std::declval<Tuple&>()));
            // First case, result is still not fully evaluated
            if constexpr (unevaluated<evaluated_t>) return named_range<Vars, evaluated_t, expression_t> { {
                .range = kaixo::evaluate(std::forward<Self>(self).range, tuple),
                .expression = kaixo::evaluate(std::forward<Self>(self).expression, tuple),
            } };
            // Second case, result is fully evaluated, wrap in an 'all_t', which will store 
            // it as either an owning_view or a ref_view.
            else return named_range<Vars, std::views::all_t<evaluated_t>, expression_t> { {
                .range = std::views::all(kaixo::evaluate(std::forward<Self>(self).range, tuple)),
                .expression = kaixo::evaluate(std::forward<Self>(self).expression, tuple),
            } };
        }

        // ------------------------------------------------
        
        template<class Self, class Index>
        constexpr index_operation<named_range, std::decay_t<Index>> operator[](this Self&& self, Index&& index) {
            return {
                .expression = std::forward<Self>(self), 
                .index = std::forward<Index>(index),
            };
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
    template<unevaluated Range>
    constexpr Range&& operator-(Range&& range) { return std::forward<Range>(range); }
    
    // Links a range to variables to create a named range
    template<class ...Vars, class Range>
        requires (evaluated_range<Range> || unevaluated<Range>)
    constexpr named_range<var<Vars...>, std::decay_t<Range>> operator<(var<Vars...>, Range&& range) {
        return { { 
            .range = std::forward<Range>(range),
        } };
    }

    // ------------------------------------------------
    
    // Link an expression to a named range
    template<class Expression, class Vars, class Range>
    constexpr auto operator|(Expression&& e, named_range<Vars, Range>&& r) 
        -> named_range<Vars, Range, std::decay_t<Expression>> 
    {
        return { { 
            .range = std::move(r).range,
            .expression = std::forward<Expression>(e),
        } };
    }

    // ------------------------------------------------
    
    // Handles main case for combining 2 ranges, adds them to a cartesian_product_view.
    template< class           ...V1s
            , evaluated_range    Range1
            , class              Expression
            , class           ...V2s
            , evaluated_range    Range2>

    constexpr auto operator,( named_range<var<V1s...>, Range1, Expression>&& r1
                            , named_range<var<V2s...>, Range2>&&             r2)
        -> named_range <var<V1s..., V2s...>
                      , std::ranges::cartesian_product_view<Range1, Range2>
                      , Expression>
    {
        using range_t = std::ranges::cartesian_product_view<Range1, Range2>;
        return { {
            .range = range_t{ std::move(r1).range, std::move(r2).range },
            .expression = std::move(r1).expression,
        } };
    }
    
    // ------------------------------------------------
    //             Range Expression Wrapper
    // ------------------------------------------------
    //  Wraps any range inside a fake unevaluated
    //  expression this is the only way to have a fully
    //  evaluated range inside of an expression.
    // ------------------------------------------------
    
    template<any_range Range>
    struct range_expression_wrapper {

        // ------------------------------------------------

        using defines = defines_t<Range>;
        using depends = depends_t<Range>;

        // ------------------------------------------------

        using explicitly_unevaluated = int;

        // ------------------------------------------------

        Range range;

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr auto evaluate(this Self&& self, Tuple&& tuple) 
            -> decltype(kaixo::evaluate(std::forward<Self>(self).range, tuple))
        {
            return kaixo::evaluate(std::forward<Self>(self).range, tuple);
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------

    // Wraps a range (both evaluated, and unevaluated) in an explicitly
    // unevaluated wrapper, to be used in other expressions. This is
    // to prevent the otherwise ambiguous comma operator overloads for
    // the tuple_operation and zip_view.
    template<explicit_unevaluated_range Range>
    constexpr range_expression_wrapper<std::decay_t<Range>> operator+(Range&& range) {
        return { std::forward<Range>(range) };
    }

    template<evaluated_range Range>
    constexpr range_expression_wrapper<std::views::all_t<Range&&>> operator+(Range&& range) {
        return { std::views::all(std::forward<Range>(range)) };
    }
    
    // ------------------------------------------------
    //                 Range Evaluator
    // ------------------------------------------------
    //  Handles case where an unevaluated range can
    //  be evaluated using the previous definitions
    //  in a named_range.
    // ------------------------------------------------

    // This evaluator evaluates the unevaluated range on the fly inside 
    // a 'transform_view' using the current iteration values, and then 
    // joining the resulting ranges, which creates a cartesian product.
    template<class Vars, unevaluated Range>
        requires has_all_defines_for<Vars, Range>
    struct range_evaluator {
        Range range;

        template<class Self, class Tuple>
        constexpr auto operator()(this Self&& self, Tuple&& tuple) {
            using tuple_t = decltype(detail::store_as_tuple(std::declval<Tuple&&>()));
            // This operator evaluates this unevaluated range on the fly
            // for each result of the parent range. This is done as follows:
            
            // First step is to evaluate the unevaluated range, we know Vars is
            // enough to fully evaluate the range, so this results in an evaluated range.
            auto evaluated = std::views::all(kaixo::evaluate(std::forward<Self>(self).range, named_tuple<Vars, Tuple&&>{ std::forward<Tuple>(tuple) }));
            return std::views::transform(std::move(evaluated),
                // Then combine the results of the now evaluated range with the original
                // results passed to this evaluator. This creates a cartesian product.
                [tuple = detail::store_as_tuple(std::forward<Tuple>(tuple))]<class R>(R && r) {
                    return std::tuple<tuple_t, R>(tuple, std::forward<R>(r));
                }
            );
        }
    };

    // ------------------------------------------------
    
    // Handles default case for evaluation of consecutive ranges
    template< class           ...V1s
            , evaluated_range    Range1
            , class              Expression
            , class           ...V2s
            , unevaluated        Range2>

        requires has_all_defines_for<var<V1s...>, Range2>

    constexpr auto operator,( named_range<var<V1s...>, Range1, Expression>&& r1
                            , named_range<var<V2s...>, Range2>&&             r2)
        -> named_range< var<V1s..., V2s...>
                      , std::ranges::join_view<std::ranges::transform_view<Range1, range_evaluator<var<V1s...>, Range2>>>
                      , Expression>
    {
        using evaluated_t = std::ranges::transform_view<Range1, range_evaluator<var<V1s...>, Range2>>;
        using joined_t = std::ranges::join_view<evaluated_t>;
        return { {
            .range = joined_t{ evaluated_t{ std::move(r1).range, { std::move(r2).range } } },
            .expression = std::move(r1).expression,
        } };
    }

    // ------------------------------------------------
    //                Unevaluated Cache
    // ------------------------------------------------
    //  Build a cache of unevaluated parts that will
    //  be combined later once they're evaluated.
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

        template<class Self, class Tuple>
        constexpr auto evaluate(this Self&& self, Tuple&& tuple) {
            return operator,(
                kaixo::evaluate(std::forward<Self>(self).a, tuple),
                kaixo::evaluate(std::forward<Self>(self).b, tuple));
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------

    // Handles 2 cases for later evaluation:
    //  - Continue building upon an unevaluated range, need to cache all ranges
    //  - Encounters unevaluated range which can not be evaluated using V1s...
    template< class   ...V1s
            , class      Range1
            , class      Expression
            , class   ...V2s
            , class      Range2>

        requires (unevaluated<Range1>     // Case 1: building upon unevaluated Range1
               && explicit_range<Range2>  //         with Range2
             ||   evaluated_range<Range1> // Case 2: encountered unevaluated Range2
               && unevaluated<Range2>     //         which cannot be evaluated with V1s...
               && !has_all_defines_for<var<V1s...>, Range2>)

    constexpr auto operator,( named_range<var<V1s...>, Range1, Expression>&& r1
                            , named_range<var<V2s...>, Range2>&&             r2)
        -> named_range< var<V1s..., V2s...>
                      , unevaluated_cache< named_range<var<V1s...>, Range1>
                                         , named_range<var<V2s...>, Range2>>
                      , Expression>
    {
        using product_t = unevaluated_cache< named_range<var<V1s...>, Range1>
                                           , named_range<var<V2s...>, Range2>>;
        return { {
            .range = product_t{ { std::move(r1.range) }, std::move(r2) },
            .expression = std::move(r1.expression),
        } };
    }

    // ------------------------------------------------
    //             Variable Assignment
    // ------------------------------------------------
    //  Assign single values to a variable for use
    //  in later expressions.
    // ------------------------------------------------
               
    template<class Vars, class ...Args>
    struct variable_assignment_operation {
        std::tuple<Args...> args;

        template<class Self, class Tuple>
        constexpr auto operator()(this Self&& self, Tuple&& tuple) {
            // First evaluate all the arguments in the variable assignment
            named_tuple<Vars, Tuple&> named{ tuple };
            auto evaluated = std::apply([&]<class ...Tys>(Tys&& ...tys) {
                using tuple_t = std::tuple<decltype(kaixo::evaluate(std::declval<Tys&&>(), std::declval<named_tuple<Vars, Tuple&>&>()))...>;
                return tuple_t{ kaixo::evaluate(std::forward<Tys>(tys), named)... };
            }, std::forward<Self>(self).args);
            // Then combine the fully evaluated arguments with the original tuple
            return std::make_tuple(std::forward<Tuple>(tuple), std::move(evaluated));
        }
    };

    // ------------------------------------------------
    
    // Handles default case for a variable assignment
    template< class                      ...V1s
            , evaluated_range               Range
            , class                         Expression
            , class                      ...V2s
            , class                      ...Args>

        requires has_all_defines_for<var<V1s...>, std::tuple<Args...>>

    constexpr auto operator,( named_range<var<V1s...>, Range, Expression>&&   r
                            , named_tuple<var<V2s...>, std::tuple<Args...>>&& c)
        -> named_range< var<V1s..., V2s...>
                      , std::ranges::transform_view< std::views::all_t<Range>
                                                   , variable_assignment_operation<var<V1s...>, Args...>>
                      , Expression> 
    {
        using range_t = std::ranges::transform_view< std::views::all_t<Range>
                                                   , variable_assignment_operation<var<V1s...>, Args...>>;
        return { {
            .range = range_t{ std::views::all(std::move(r).range), { std::move(c).tuple } },
            .expression = std::move(r).expression,
        } };
    }
    
    // Handles 2 cases for later evaluation:
    //  - Continues building upon an unevaluated range, need to cache all parts
    //  - Encounters Condition which can not be evaluated using Vars
    template< class                   ...V1s
            , class                      Range
            , class                      Expression
            , class                   ...V2s
            , class                   ...Args>

        requires (unevaluated<Range>     // Case 1: continue building upon unevaluated
             ||   evaluated_range<Range> // Case 2: Can't fully evaluate Condition
               && !has_all_defines_for<var<V1s...>, std::tuple<Args...>>)

    constexpr auto operator,( named_range<var<V1s...>, Range, Expression>&&   r
                            , named_tuple<var<V2s...>, std::tuple<Args...>>&& c)
        -> named_range< var<V1s..., V2s...>
                      , unevaluated_cache< named_range<var<V1s...>, Range>
                                         , named_tuple<var<V2s...>, std::tuple<Args...>>>
                      , Expression>
    {
        using range_t = unevaluated_cache< named_range<var<V1s...>, Range>
                                         , named_tuple<var<V2s...>, std::tuple<Args...>>>;
        return { {
            .range = range_t{ { std::move(r).range }, std::move(c) },
            .expression = std::move(r).expression,
        } };
    }

    // ------------------------------------------------
    //                 Range Filter
    // ------------------------------------------------
    //  Build upon a named_range with a filter_view.
    // ------------------------------------------------

    // Add range filter, this simply adds a filter_view with this
    // range_filter as callback, which will evaluate the stored Condition
    // to filter the results of the parent range.
    template<class Vars, class Condition>
    struct range_filter {
        Condition condition;

        template<class Self, class Tuple>
        constexpr bool operator()(this Self&& self, Tuple&& tuple) {
            return static_cast<bool>(kaixo::evaluate( std::forward<Self>(self).condition
                                                    , named_tuple<Vars, Tuple&&>{ std::forward<Tuple>(tuple) }));
        }
    };

    // ------------------------------------------------
    
    template<class Ty>
    concept valid_range_condition = valid_expression_arguments<Ty> && defines_t<Ty>::size == 0;

    // Handles default case for a range filter
    template< class                      Vars
            , evaluated_range            Range
            , class                      Expression
            , valid_range_condition      Condition>

        requires has_all_defines_for<Vars, Condition>

    constexpr auto operator,( named_range<Vars, Range, Expression>&& r
                            , Condition&&                            c)
        -> named_range< Vars
                      , std::ranges::filter_view< std::views::all_t<Range>
                                                , range_filter<Vars, Condition>>
                      , Expression> 
    {
        using filtered_t = std::ranges::filter_view< std::views::all_t<Range>
                                                   , range_filter<Vars, Condition>>;
        return { {
            .range = filtered_t{ std::views::all(std::move(r).range), { std::forward<Condition>(c) } },
            .expression = std::move(r).expression,
        } };
    }
    
    // Handles 2 cases for later evaluation:
    //  - Continues building upon an unevaluated range, need to cache all parts
    //  - Encounters Condition which can not be evaluated using Vars
    template< class                      Vars
            , class                      Range
            , class                      Expression
            , valid_range_condition      Condition>

        requires (unevaluated<Range>     // Case 1: continue building upon unevaluated
             ||   evaluated_range<Range> // Case 2: Can't fully evaluate Condition
               && !has_all_defines_for<Vars, Condition>)

    constexpr auto operator,( named_range<Vars, Range, Expression>&& r
                            , Condition&&                            c)
        -> named_range< Vars
                      , unevaluated_cache< named_range<Vars, Range>
                                         , Condition>
                      , Expression>
    {
        using product_t = unevaluated_cache< named_range<Vars, Range>
                                           , Condition>;
        return { {
            .range = product_t{ { std::move(r).range }, std::forward<Condition>(c) },
            .expression = std::move(r).expression,
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
        requires (!evaluated_range<Ranges> || ...)
    struct zip_range<Ranges...> {

        // ------------------------------------------------
        
        using defines = var<>;
        using depends = unique_t<concat_t<depends_t<std::decay_t<Ranges>>...>>;

        // ------------------------------------------------

        using is_range = int;

        // ------------------------------------------------

        std::tuple<Ranges...> ranges;

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr auto evaluate(this Self&& self, Tuple&& tuple) {
            return std::apply([&]<class ...Args>(Args&& ...args) {
                // Evaluate all ranges, and apply the comma operator again
                // This will construct a new zip_range, automatically picking
                // the correct one (unevaluated or evaluated).
                return (kaixo::evaluate(std::forward<Args>(args), tuple), ...);
            }, std::forward<Self>(self).ranges);
        }

        // ------------------------------------------------

    };

    // Construct the zip range
    template<any_range A, any_range B>
        requires (explicit_range<A> || explicit_range<B>)
    constexpr auto operator,(A&& a, B&& b) {
        using a_t = std::conditional_t<evaluated_range<A>, A, std::decay_t<A>>;
        using b_t = std::conditional_t<evaluated_range<B>, B, std::decay_t<B>>;
        if constexpr (evaluated_range<A> && evaluated_range<B>) return zip_range<A, B>{
            std::forward<A>(a),
            std::forward<B>(b),
        };
        else return zip_range<a_t, b_t>{ { 
            std::forward<A>(a), 
            std::forward<B>(b) 
        } };
    }

    // ------------------------------------------------
    //                    Break
    // ------------------------------------------------
    //  Adds a condition, that when met, will stop
    //  generating output. It does this by using the
    //  take_while_view.
    // ------------------------------------------------
    
    template<class Vars, unevaluated Break>
    struct break_point {
        Break expression;

        template<class Self, class Tuple>
        constexpr bool operator()(this Self&& self, Tuple&& tuple) {
            return !static_cast<bool>(kaixo::evaluate(
                std::forward<Self>(self).expression, 
                named_tuple<Vars, Tuple&&>{ std::forward<Tuple>(tuple) }
            ));
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
    template< class           Vars
            , evaluated_range Range
            , class           Expression
            , unevaluated     Break>

        requires has_all_defines_for<Vars, Break>

    constexpr auto operator,( named_range<Vars, Range, Expression>&& r
                            , break_point<var<>, Break>&&            b) 
        -> named_range< Vars
                      , std::ranges::take_while_view<Range, break_point<Vars, Break>>
                      , Expression> 
    {
        using break_t = std::ranges::take_while_view<Range, break_point<Vars, Break>>;
        return { {
            .range = break_t{ std::move(r).range, break_point<Vars, Break>{ std::move(b).expression } },
            .expression = std::move(r).expression,
        } };
    }

    // Handles 2 cases for later evaluation:
    //  - Continues building upon an unevaluated range, need to cache all parts
    //  - Encounters Break condition which can not be evaluated using Vars
    template< class       Vars
            , class       Range
            , class       Expression
            , unevaluated Break>

        requires (unevaluated<Range>      // Case 1: continue building upon unevaluated
             ||   evaluated_range<Range>  // Case 2: can't fully evaluate Break
               && !has_all_defines_for<Vars, Break>)

    constexpr auto operator,( named_range<Vars, Range, Expression>&& r
                            , break_point<var<>, Break>&&            b) 
        -> named_range< Vars
                      , unevaluated_cache< named_range<Vars, Range>
                                         , break_point<Vars, Break>>
                      , Expression>
    {
        using product_t = unevaluated_cache< named_range<Vars, Range>
                                           , break_point<Vars, Break>>;
        return { {
            .range = product_t{ { std::move(r).range }, break_point<Vars, Break>{ std::move(b).expression } },
            .expression = std::move(r).expression,
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
        constexpr Tuple operator()(this Self&& self, Tuple&& tuple) {
            std::inserter(std::forward<Self>(self).range, std::ranges::end(std::forward<Self>(self).range)) 
                = kaixo::evaluate(std::forward<Self>(self).expression, named_tuple<Vars, Tuple&&>{ std::forward<Tuple>(tuple) });
            return std::forward<Tuple>(tuple);
        }
    };

    // ------------------------------------------------
    
    // Create the inserter like this: 'range << expression'
    template<evaluated_range Range, unevaluated Expression>
        requires (!std::is_const_v<Range>)
    constexpr range_inserter<var<>, Range, Expression> operator<<(Range& range, Expression&& expression) {
        return { range, std::forward<Expression>(expression) };
    }

    // ------------------------------------------------

    // Handles default case for range inserter
    template< class           Vars
            , evaluated_range Range1
            , class           Expression1
            , evaluated_range Range2
            , unevaluated     Expression2>

        requires has_all_defines_for<Vars, Expression2>

    constexpr auto operator,( named_range<Vars, Range1, Expression1>&&     r
                            , range_inserter<var<>, Range2, Expression2>&& i) 
        -> named_range< Vars
                      , std::ranges::transform_view<Range1, range_inserter<Vars, Range2, Expression2>>
                      , Expression1>
    {
        using inserter_t = range_inserter<Vars, Range2, Expression2>;
        using transform_t = std::ranges::transform_view<Range1, inserter_t>;
        return { {
            .range = transform_t{ std::move(r).range, inserter_t{ i.range, std::move(i).expression } },
            .expression = std::move(r).expression,
        } };
    }
    
    // Handles 2 cases for later evaluation:
    //  - Continues building upon an unevaluated range, need to cache all parts
    //  - Encounters expression which can not be evaluated using Vars
    template< class           Vars
            , class           Range1
            , class           Expression1
            , evaluated_range Range2
            , unevaluated     Expression2>

        requires (unevaluated<Range1>     // Case 1: continue building upon unevaluated
             ||   evaluated_range<Range1> // Case 2: can't fully evaluate Expression2
               && !has_all_defines_for<Vars, Expression2>)

    constexpr auto operator,( named_range<Vars, Range1, Expression1>&&     r
                            , range_inserter<var<>, Range2, Expression2>&& i) 
        -> named_range< Vars
                      , unevaluated_cache< named_range<Vars, Range1>
                                         , range_inserter<Vars, Range2, Expression2>>
                      , Expression1>
    {
        using inserter_t = range_inserter<Vars, Range2, Expression2>;
        using product_t = unevaluated_cache<named_range<Vars, Range1>, inserter_t>;
        return { {
            .range = product_t{ { std::move(r).range }, inserter_t{ std::move(i).range, std::move(i).expression } },
            .expression = std::move(r).expression,
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

    template<class A, class B>
    concept can_add_assign = requires (A& a, const B& b) {
        { a += b } -> std::convertible_to<A&>; 
    };
    
    template<class A, class B>
    concept can_subtract_assign = requires (A& a, const B& b) {
        { a -= b } -> std::convertible_to<A&>; 
    };

    // ------------------------------------------------

    template<class Begin, class End, class Increment = detail::dud>
    struct range_storage {

        // ------------------------------------------------
        
        using value_type = typename std::conditional_t< std::same_as<Increment, detail::dud>
                                                      , std::type_identity<Begin>
                                                      , std::common_type<Begin, Increment>>::type;

        // ------------------------------------------------

        Begin beginValue;
        End endValue;
        Increment increment;

        // ------------------------------------------------
        
        constexpr std::size_t size() const {
            if constexpr (std::integral<value_type>) {
                value_type min = std::min(static_cast<value_type>(beginValue), static_cast<value_type>(endValue));
                value_type max = std::max(static_cast<value_type>(beginValue), static_cast<value_type>(endValue));
                if constexpr (not_dud<Increment>) {
                    return static_cast<std::size_t>((max - min + static_cast<value_type>(increment) - 1) / static_cast<value_type>(increment));
                } else {
                    return static_cast<std::size_t>(max - min);
                }
            } else if constexpr (std::floating_point<value_type>) {
                value_type min = std::min(static_cast<value_type>(beginValue), static_cast<value_type>(endValue));
                value_type max = std::max(static_cast<value_type>(beginValue), static_cast<value_type>(endValue));
                if constexpr (not_dud<Increment>) {
                    return static_cast<std::size_t>(std::ceil((max - min) / static_cast<value_type>(increment)));
                } else {
                    return static_cast<std::size_t>(std::ceil(max - min));
                }
            } else {
                std::size_t size = 0;
                value_type value = beginValue;
                while (!is_end(value)) {
                    this->do_increment(value);
                    ++size;
                }
                return size;
            }
        }
        
        // ------------------------------------------------

        constexpr bool is_end(const value_type& value) const {
            if constexpr (std::floating_point<value_type> || not_dud<Increment>) {
                if (beginValue < endValue) return static_cast<bool>(value >= endValue);
                else return static_cast<bool>(value <= endValue);
            } else return static_cast<bool>(value == endValue);
        }

        constexpr void do_increment(value_type& value) const {
            if constexpr (std::same_as<Increment, detail::dud>) ++value;
            else if constexpr (can_add_assign<value_type, Increment>) value += increment;
            else value = value + increment;
        }

        constexpr void do_decrement(value_type& value) const {
            if constexpr (std::same_as<Increment, detail::dud>) --value;
            else if constexpr (can_subtract_assign<value_type, Increment>) value -= increment;
            else value = value - increment;
        }
        
        constexpr void do_increment(value_type& value, std::ptrdiff_t n) const {
            if (n < 0) while (n++ != 0) do_decrement(value);
            else while (n-- != 0) do_increment(value);
        }

        constexpr void do_decrement(value_type& value, std::ptrdiff_t n) const {
            if (n < 0) while (n++ != 0) do_increment(value);
            else while (n-- != 0) do_decrement(value);
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

            using value_type = typename range_storage<Begin, End, Increment>::value_type;
            using reference = value_type;
            using difference_type = std::ptrdiff_t;
            using iterator_category = std::random_access_iterator_tag;

            // ------------------------------------------------

            value_type value;
            const range_storage<Begin, End, Increment>* self;

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

            constexpr iterator& operator--() {
                self->do_decrement(value);
                return *this;
            }

            constexpr iterator operator--(int) {
                iterator copy = *this;
                self->do_decrement(value);
                return copy;
            }

            // ------------------------------------------------

            constexpr value_type operator*() const { return value; }

            // ------------------------------------------------

            constexpr static friend iterator operator+(const iterator& s, difference_type i) { return iterator{ static_cast<Begin>(s.value + i), s.self }; }
            constexpr static friend iterator operator+(difference_type i, const iterator& s) { return iterator{ static_cast<Begin>(i + s.value), s.self }; }
            constexpr static friend iterator operator-(const iterator& s, difference_type i) { return iterator{ static_cast<Begin>(s.value - i), s.self }; }
            constexpr static friend iterator operator-(difference_type i, const iterator& s) { return iterator{ static_cast<Begin>(i - s.value), s.self }; }

            // ------------------------------------------------

            constexpr iterator& operator+=(difference_type i) {
                self->do_increment(value, i);
                return *this;
            }

            constexpr iterator& operator-=(difference_type i) {
                self->do_decrement(value, i);
                return *this;
            }

            // ------------------------------------------------

            constexpr friend difference_type operator-(const iterator& i, const iterator& s) {
                return i.value - s.value;
            }

            // ------------------------------------------------

            constexpr bool operator< (const iterator& o) const { return value <  o.value; }
            constexpr bool operator<=(const iterator& o) const { return value <= o.value; }
            constexpr bool operator> (const iterator& o) const { return value >  o.value; }
            constexpr bool operator>=(const iterator& o) const { return value >= o.value; }

            // ------------------------------------------------

            constexpr reference operator[](difference_type i) const {
                auto copy = value;
                self->do_increment(copy, i);
                return copy;
            }

            // ------------------------------------------------

            constexpr bool operator==(const iterator& o) const { return value == o.value; }
            constexpr bool operator==(sentinel) const { return self->is_end(value); }

            // ------------------------------------------------

        };

        // ------------------------------------------------

        constexpr iterator begin() const { return { this->beginValue, this }; }
        constexpr sentinel end() const { return {}; }

        // ------------------------------------------------

        constexpr Begin operator[](std::size_t i) const {
            auto copy = this->beginValue;
            this->do_increment(copy, i);
            return copy;
        }

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
    constexpr static bool operator> (const auto&, inf_t) { return false; }
    constexpr static bool operator>=(const auto&, inf_t) { return false; }
    constexpr static bool operator< (const auto&, inf_t) { return true;  }
    constexpr static bool operator<=(const auto&, inf_t) { return true;  }
    constexpr static bool operator==(inf_t, const auto&) { return false; }
    constexpr static bool operator> (inf_t, const auto&) { return true;  }
    constexpr static bool operator>=(inf_t, const auto&) { return true;  }
    constexpr static bool operator< (inf_t, const auto&) { return false; }
    constexpr static bool operator<=(inf_t, const auto&) { return false; }

    // ------------------------------------------------
    //                    Empty
    // ------------------------------------------------
    //  Empty expression checks whether an unevaluated
    //  range is empty when evaluated on the fly.
    // ------------------------------------------------

    template<unevaluated Range>
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

    template<unevaluated Range>
    constexpr empty_operation<Range> is_empty(Range&& range) {
        return empty_operation<Range>{
            .range = std::forward<Range>(range) 
        };
    }

    // ------------------------------------------------
    //                Fold Operation
    // ------------------------------------------------
    //  Operation on ranges that folds them into
    //  single values, like std::ranges::fold_left etc.
    // ------------------------------------------------

    constexpr detail::dud _{};

    // ------------------------------------------------

    template<class A, class B, class Op>
    struct fold_operation {

        // ------------------------------------------------

        using defines = var<>;
        using depends = unique_t<concat_t<depends_t<A>, depends_t<B>>>;

        // ------------------------------------------------

        [[no_unique_address]] A a;
        [[no_unique_address]] B b;
        [[no_unique_address]] Op operation{};

        // ------------------------------------------------

        template<class Self, class Tuple>
        constexpr auto evaluate(this Self&& self, Tuple&& tuple) {
            using evaluated1_t = decltype(kaixo::evaluate(std::declval<Self&&>().a, std::declval<Tuple&>()));
            using evaluated2_t = decltype(kaixo::evaluate(std::declval<Self&&>().b, std::declval<Tuple&>()));

            if constexpr (unevaluated<evaluated1_t> || unevaluated<evaluated2_t>) {
                return fold_operation<evaluated1_t, evaluated2_t, Op>{
                    kaixo::evaluate(std::forward<Self>(self).a, tuple),
                    kaixo::evaluate(std::forward<Self>(self).b, tuple),
                    std::forward<Self>(self).operation,
                };
            } else {
                // There are 2 special cases: logic_and and logic_or operator
                // when no initial value is defined, it will default to 'true'
                // for the logic_and operator, and 'false' for logic_or.
                if constexpr (std::ranges::range<evaluated1_t>) {
                    if constexpr (std::same_as<B, detail::dud>)
                        if constexpr (std::same_as<Op, logic_and_operator>)
                            return std::ranges::fold_right(
                                kaixo::evaluate(std::forward<Self>(self).a, tuple),
                                true, // Default to 'true' for logic_and
                                std::forward<Self>(self).operation);
                        else if constexpr (std::same_as<Op, logic_or_operator>)
                            return std::ranges::fold_right(
                                kaixo::evaluate(std::forward<Self>(self).a, tuple),
                                false, // Default to 'false' for logic_or
                                std::forward<Self>(self).operation);
                        else return std::ranges::fold_right_last(
                            kaixo::evaluate(std::forward<Self>(self).a, tuple),
                            std::forward<Self>(self).operation);
                    else return std::ranges::fold_right(
                        kaixo::evaluate(std::forward<Self>(self).a, tuple),
                        kaixo::evaluate(std::forward<Self>(self).b, tuple),
                        std::forward<Self>(self).operation);
                } else if constexpr (std::ranges::range<evaluated2_t>) {
                    if constexpr (std::same_as<A, detail::dud>)
                        if constexpr (std::same_as<Op, logic_and_operator>)
                            return std::ranges::fold_left(
                                kaixo::evaluate(std::forward<Self>(self).a, tuple),
                                true, // Default to 'true' for logic_and
                                std::forward<Self>(self).operation);
                        else if constexpr (std::same_as<Op, logic_or_operator>)
                            return std::ranges::fold_left(
                                kaixo::evaluate(std::forward<Self>(self).a, tuple),
                                false, // Default to 'false' for logic_or
                                std::forward<Self>(self).operation);
                        else return std::ranges::fold_left_first(
                            kaixo::evaluate(std::forward<Self>(self).b, tuple),
                            std::forward<Self>(self).operation);
                    else return std::ranges::fold_left(
                        kaixo::evaluate(std::forward<Self>(self).b, tuple),
                        kaixo::evaluate(std::forward<Self>(self).a, tuple),
                        std::forward<Self>(self).operation);
                } else {
                    static_assert(std::conditional_t<false, A, std::false_type>::value, "Fold expression expected a range!");
                }
            }
        }

        // ------------------------------------------------

    };

    // ------------------------------------------------

#define KAIXO_FOLD_OP(op, name)                                                                 \
    template<class A>                                                                           \
    constexpr auto operator op(A&& a, detail::dud)                                              \
        -> fold_operation<std::decay_t<A>, detail::dud, name##_operator>                        \
    {                                                                                           \
        return { std::forward<A>(a), {} };                                                      \
    }                                                                                           \
                                                                                                \
    template<class A>                                                                           \
    constexpr auto operator op(detail::dud, A&& a)                                              \
        -> fold_operation<detail::dud, std::decay_t<A>, name##_operator>                        \
    {                                                                                           \
        return { {}, std::forward<A>(a) };                                                      \
    }                                                                                           \
                                                                                                \
    template<class A, class B>                                                                  \
        requires (unevaluated<A> || unevaluated<B>)                                             \
    constexpr auto operator op(fold_operation<A, detail::dud, name##_operator>&& a, B&& b)      \
        -> fold_operation<std::decay_t<A>, std::decay_t<B>, name##_operator>                    \
    {                                                                                           \
        return { std::move(a).a, std::forward<B>(b) };                                          \
    }                                                                                           \
                                                                                                \
    template<class A, class B>                                                                  \
        requires (unevaluated<A> || unevaluated<B>)                                             \
    constexpr auto operator op(A&& a, fold_operation<detail::dud, B, name##_operator>&& b)      \
        -> fold_operation<std::decay_t<A>, std::decay_t<B>, name##_operator>                    \
    {                                                                                           \
        return { std::forward<A>(a), std::move(b).b };                                          \
    }

    KAIXO_FOLD_OP(+, add);      KAIXO_FOLD_OP(< , less_than);          KAIXO_FOLD_OP(| , bit_or);      KAIXO_FOLD_OP(*=, multiply_assign);
    KAIXO_FOLD_OP(-, subtract); KAIXO_FOLD_OP(> , greater_than);       KAIXO_FOLD_OP(^, bit_xor);      KAIXO_FOLD_OP(/=, divide_assign);
    KAIXO_FOLD_OP(*, multiply); KAIXO_FOLD_OP(<= , less_or_equals);    KAIXO_FOLD_OP(&&, logic_and);   KAIXO_FOLD_OP(%=, modulo_assign);
    KAIXO_FOLD_OP(/ , divide);  KAIXO_FOLD_OP(>= , greater_or_equals); KAIXO_FOLD_OP(|| , logic_or);   KAIXO_FOLD_OP(&=, bit_and_assign);
    KAIXO_FOLD_OP(%, modulo);   KAIXO_FOLD_OP(+=, add_assign);         KAIXO_FOLD_OP(== , equals);     KAIXO_FOLD_OP(|=, bit_or_assign);
    KAIXO_FOLD_OP(&, bit_and);  KAIXO_FOLD_OP(-=, subtract_assign);    KAIXO_FOLD_OP(!= , not_equals); KAIXO_FOLD_OP(^=, bit_xor_assign);
    
    // ------------------------------------------------
    //          Basic variable definitions
    // ------------------------------------------------

    namespace variables {
        constexpr auto a = var<struct variable_a>{};
        constexpr auto b = var<struct variable_b>{};
        constexpr auto c = var<struct variable_c>{};
        constexpr auto d = var<struct variable_d>{};
        constexpr auto e = var<struct variable_e>{};
        constexpr auto f = var<struct variable_f>{};
        constexpr auto g = var<struct variable_g>{};
        constexpr auto h = var<struct variable_h>{};
        constexpr auto i = var<struct variable_i>{};
        constexpr auto j = var<struct variable_j>{};
        constexpr auto k = var<struct variable_k>{};
        constexpr auto l = var<struct variable_l>{};
        constexpr auto m = var<struct variable_m>{};
        constexpr auto n = var<struct variable_n>{};
        constexpr auto o = var<struct variable_o>{};
        constexpr auto p = var<struct variable_p>{};
        constexpr auto q = var<struct variable_q>{};
        constexpr auto r = var<struct variable_r>{};
        constexpr auto s = var<struct variable_s>{};
        constexpr auto t = var<struct variable_t>{};
        constexpr auto u = var<struct variable_u>{};
        constexpr auto v = var<struct variable_v>{};
        constexpr auto w = var<struct variable_w>{};
        constexpr auto x = var<struct variable_x>{};
        constexpr auto y = var<struct variable_y>{};
        constexpr auto z = var<struct variable_z>{};

        constexpr auto key   = var<struct Key>{};
        constexpr auto value = var<struct Value>{};
    }

    // ------------------------------------------------
    
}

// ------------------------------------------------

namespace std {

    // ------------------------------------------------
    //            std function overloads
    // ------------------------------------------------
    //  Overloads for a bunch of standard functions.
    // ------------------------------------------------

#define KAIXO_FUNCTION_OVERLOAD(name)                                                                               \
    template<class Tys, class Args>                                                                                 \
    struct kaixo_##name##_overload;                                                                                 \
                                                                                                                    \
    template<class ...Tys, class... Args>                                                                           \
        requires kaixo::valid_overload_arguments<Args...>                                                           \
    struct kaixo_##name##_overload<std::tuple<Tys...>, std::tuple<Args...>> {                                       \
        using defines = kaixo::var<>;                                                                               \
        using depends = kaixo::unique_t<kaixo::concat_t<kaixo::depends_t<Args>...>>;                                \
                                                                                                                    \
        std::tuple<Args...> args;                                                                                   \
                                                                                                                    \
        template<class Self, class Tuple>                                                                           \
        constexpr decltype(auto) evaluate(this Self&& self, Tuple&& tuple) {                                        \
            return std::apply([&]<class ...Ts>(Ts&& ...ts) {                                                        \
                if constexpr (sizeof...(Tys) == 0) {                                                                \
                    return name(kaixo::evaluate(std::forward<Ts>(ts), tuple)...);                                   \
                } else {                                                                                            \
                    return name<Tys...>(kaixo::evaluate(std::forward<Ts>(ts), tuple)...);                           \
                }                                                                                                   \
            }, std::forward<Self>(self).args);                                                                      \
        }                                                                                                           \
    };                                                                                                              \
                                                                                                                    \
    template<class ...Tys, class... Args>                                                                           \
        requires kaixo::valid_overload_arguments<Args...>                                                           \
    constexpr kaixo_##name##_overload<std::tuple<Tys...>, std::tuple<std::decay_t<Args>...>> name(Args&& ...args) { \
        return { .args = { std::forward<Args>(args)... } };                                                         \
    }

    // ------------------------------------------------

    // <functional>
    KAIXO_FUNCTION_OVERLOAD(bind_front);
    KAIXO_FUNCTION_OVERLOAD(bind);
    KAIXO_FUNCTION_OVERLOAD(ref);
    KAIXO_FUNCTION_OVERLOAD(cref);
    KAIXO_FUNCTION_OVERLOAD(invoke);

    // ------------------------------------------------

    // <any>
    KAIXO_FUNCTION_OVERLOAD(any_cast);
    KAIXO_FUNCTION_OVERLOAD(make_any);

    // ------------------------------------------------

    // <algorithm>
    KAIXO_FUNCTION_OVERLOAD(adjacent_find);
    KAIXO_FUNCTION_OVERLOAD(binary_search);
    KAIXO_FUNCTION_OVERLOAD(bsearch);
    KAIXO_FUNCTION_OVERLOAD(clamp);
    KAIXO_FUNCTION_OVERLOAD(copy_backward);
    KAIXO_FUNCTION_OVERLOAD(copy_n);
    KAIXO_FUNCTION_OVERLOAD(count);
    KAIXO_FUNCTION_OVERLOAD(count_if);
    KAIXO_FUNCTION_OVERLOAD(equal);
    KAIXO_FUNCTION_OVERLOAD(equal_range);
    KAIXO_FUNCTION_OVERLOAD(fill);
    KAIXO_FUNCTION_OVERLOAD(fill_n);
    KAIXO_FUNCTION_OVERLOAD(find);
    KAIXO_FUNCTION_OVERLOAD(find_end);
    KAIXO_FUNCTION_OVERLOAD(find_first_of);
    KAIXO_FUNCTION_OVERLOAD(find_if);
    KAIXO_FUNCTION_OVERLOAD(find_if_not);
    KAIXO_FUNCTION_OVERLOAD(for_each);
    KAIXO_FUNCTION_OVERLOAD(for_each_n);
    KAIXO_FUNCTION_OVERLOAD(generate);
    KAIXO_FUNCTION_OVERLOAD(generate_n);
    KAIXO_FUNCTION_OVERLOAD(includes);
    KAIXO_FUNCTION_OVERLOAD(inplace_merge);
    KAIXO_FUNCTION_OVERLOAD(iter_swap);
    KAIXO_FUNCTION_OVERLOAD(lexicographical_compare);
    KAIXO_FUNCTION_OVERLOAD(lower_bound);
    KAIXO_FUNCTION_OVERLOAD(make_heap);
    KAIXO_FUNCTION_OVERLOAD(max);
    KAIXO_FUNCTION_OVERLOAD(max_element);
    KAIXO_FUNCTION_OVERLOAD(merge);
    KAIXO_FUNCTION_OVERLOAD(min);
    KAIXO_FUNCTION_OVERLOAD(min_element);
    KAIXO_FUNCTION_OVERLOAD(minmax);
    KAIXO_FUNCTION_OVERLOAD(minmax_element);
    KAIXO_FUNCTION_OVERLOAD(mismatch);
    KAIXO_FUNCTION_OVERLOAD(move);
    KAIXO_FUNCTION_OVERLOAD(move_backward);
    KAIXO_FUNCTION_OVERLOAD(next_permutation);
    KAIXO_FUNCTION_OVERLOAD(nth_element);
    KAIXO_FUNCTION_OVERLOAD(partial_sort);
    KAIXO_FUNCTION_OVERLOAD(partial_sort_copy);
    KAIXO_FUNCTION_OVERLOAD(partition);
    KAIXO_FUNCTION_OVERLOAD(partition_copy);
    KAIXO_FUNCTION_OVERLOAD(partition_point);
    KAIXO_FUNCTION_OVERLOAD(pop_heap);
    KAIXO_FUNCTION_OVERLOAD(prev_permutation);
    KAIXO_FUNCTION_OVERLOAD(push_heap);
    KAIXO_FUNCTION_OVERLOAD(qsort);
    KAIXO_FUNCTION_OVERLOAD(remove);
    KAIXO_FUNCTION_OVERLOAD(remove_copy);
    KAIXO_FUNCTION_OVERLOAD(replace);
    KAIXO_FUNCTION_OVERLOAD(replace_copy);
    KAIXO_FUNCTION_OVERLOAD(replace_copy_if);
    KAIXO_FUNCTION_OVERLOAD(reverse);
    KAIXO_FUNCTION_OVERLOAD(reverse_copy);
    KAIXO_FUNCTION_OVERLOAD(rotate);
    KAIXO_FUNCTION_OVERLOAD(rotate_copy);
    KAIXO_FUNCTION_OVERLOAD(sample);
    KAIXO_FUNCTION_OVERLOAD(search);
    KAIXO_FUNCTION_OVERLOAD(search_n);
    KAIXO_FUNCTION_OVERLOAD(shift_left);
    KAIXO_FUNCTION_OVERLOAD(shift_right);
    KAIXO_FUNCTION_OVERLOAD(set_difference);
    KAIXO_FUNCTION_OVERLOAD(set_intersection);
    KAIXO_FUNCTION_OVERLOAD(set_symmetric_difference);
    KAIXO_FUNCTION_OVERLOAD(set_union);
    KAIXO_FUNCTION_OVERLOAD(sort);
    KAIXO_FUNCTION_OVERLOAD(sort_heap);
    KAIXO_FUNCTION_OVERLOAD(stable_partition);
    KAIXO_FUNCTION_OVERLOAD(stable_sort);
    KAIXO_FUNCTION_OVERLOAD(swap);
    KAIXO_FUNCTION_OVERLOAD(swap_ranges);
    KAIXO_FUNCTION_OVERLOAD(transform);
    KAIXO_FUNCTION_OVERLOAD(unique);
    KAIXO_FUNCTION_OVERLOAD(unique_copy);
    KAIXO_FUNCTION_OVERLOAD(upper_bound);

    // ------------------------------------------------

    // <iterator>
    KAIXO_FUNCTION_OVERLOAD(advance);
    KAIXO_FUNCTION_OVERLOAD(back_inserter);
    KAIXO_FUNCTION_OVERLOAD(begin);
    KAIXO_FUNCTION_OVERLOAD(data);
    KAIXO_FUNCTION_OVERLOAD(distance);
    KAIXO_FUNCTION_OVERLOAD(empty);
    KAIXO_FUNCTION_OVERLOAD(end);
    KAIXO_FUNCTION_OVERLOAD(front_inserter);
    KAIXO_FUNCTION_OVERLOAD(inserter);
    KAIXO_FUNCTION_OVERLOAD(make_move_iterator);
    KAIXO_FUNCTION_OVERLOAD(make_reverse_iterator);
    KAIXO_FUNCTION_OVERLOAD(next);
    KAIXO_FUNCTION_OVERLOAD(prev);
    KAIXO_FUNCTION_OVERLOAD(rbegin);
    KAIXO_FUNCTION_OVERLOAD(rend);
    KAIXO_FUNCTION_OVERLOAD(size);

    // ------------------------------------------------

    // <memory>, <memory_resource>
    KAIXO_FUNCTION_OVERLOAD(addressof);
    KAIXO_FUNCTION_OVERLOAD(align);
    KAIXO_FUNCTION_OVERLOAD(assume_aligned);
    KAIXO_FUNCTION_OVERLOAD(calloc);
    KAIXO_FUNCTION_OVERLOAD(free);
    KAIXO_FUNCTION_OVERLOAD(malloc);
    KAIXO_FUNCTION_OVERLOAD(make_unique);
    KAIXO_FUNCTION_OVERLOAD(realloc);
    KAIXO_FUNCTION_OVERLOAD(destroy);
    KAIXO_FUNCTION_OVERLOAD(destroy_at);
    KAIXO_FUNCTION_OVERLOAD(destroy_n);
    KAIXO_FUNCTION_OVERLOAD(make_obj_using_allocator);
    KAIXO_FUNCTION_OVERLOAD(to_address);
    KAIXO_FUNCTION_OVERLOAD(uninitialized_construct_using_allocator);
    KAIXO_FUNCTION_OVERLOAD(uninitialized_copy);
    KAIXO_FUNCTION_OVERLOAD(uninitialized_copy_n);
    KAIXO_FUNCTION_OVERLOAD(uninitialized_default_construct);
    KAIXO_FUNCTION_OVERLOAD(uninitialized_default_construct_n);
    KAIXO_FUNCTION_OVERLOAD(uninitialized_fill);
    KAIXO_FUNCTION_OVERLOAD(uninitialized_fill_n);
    KAIXO_FUNCTION_OVERLOAD(uninitialized_move);
    KAIXO_FUNCTION_OVERLOAD(uninitialized_move_n);
    KAIXO_FUNCTION_OVERLOAD(uninitialized_value_construct);
    KAIXO_FUNCTION_OVERLOAD(uninitialized_value_construct_n);

    // ------------------------------------------------

    // <numeric>
    KAIXO_FUNCTION_OVERLOAD(accumulate);
    KAIXO_FUNCTION_OVERLOAD(adjacent_difference);
    KAIXO_FUNCTION_OVERLOAD(inclusive_scan);
    KAIXO_FUNCTION_OVERLOAD(inner_product);
    KAIXO_FUNCTION_OVERLOAD(iota);
    KAIXO_FUNCTION_OVERLOAD(reduce);
    KAIXO_FUNCTION_OVERLOAD(partial_sum);
    KAIXO_FUNCTION_OVERLOAD(transform_exclusive_scan);
    KAIXO_FUNCTION_OVERLOAD(transform_inclusive_scan);
    KAIXO_FUNCTION_OVERLOAD(transform_reduce);
    KAIXO_FUNCTION_OVERLOAD(bit_cast);
    KAIXO_FUNCTION_OVERLOAD(gcd);
    KAIXO_FUNCTION_OVERLOAD(lcm);
    KAIXO_FUNCTION_OVERLOAD(lerp);
    KAIXO_FUNCTION_OVERLOAD(abs);
    KAIXO_FUNCTION_OVERLOAD(acos);
    KAIXO_FUNCTION_OVERLOAD(acosh);
    KAIXO_FUNCTION_OVERLOAD(asin);
    KAIXO_FUNCTION_OVERLOAD(asinh);
    KAIXO_FUNCTION_OVERLOAD(atan);
    KAIXO_FUNCTION_OVERLOAD(atan2);
    KAIXO_FUNCTION_OVERLOAD(atanh);
    KAIXO_FUNCTION_OVERLOAD(cbrt);
    KAIXO_FUNCTION_OVERLOAD(ceil);
    KAIXO_FUNCTION_OVERLOAD(copysign);
    KAIXO_FUNCTION_OVERLOAD(cos);
    KAIXO_FUNCTION_OVERLOAD(cosh);
    KAIXO_FUNCTION_OVERLOAD(div);
    KAIXO_FUNCTION_OVERLOAD(erf);
    KAIXO_FUNCTION_OVERLOAD(erfc);
    KAIXO_FUNCTION_OVERLOAD(exp);
    KAIXO_FUNCTION_OVERLOAD(exp2);
    KAIXO_FUNCTION_OVERLOAD(expm1);
    KAIXO_FUNCTION_OVERLOAD(fabs);
    KAIXO_FUNCTION_OVERLOAD(fdim);
    KAIXO_FUNCTION_OVERLOAD(floor);
    KAIXO_FUNCTION_OVERLOAD(fma);
    KAIXO_FUNCTION_OVERLOAD(fmax);
    KAIXO_FUNCTION_OVERLOAD(fmin);
    KAIXO_FUNCTION_OVERLOAD(fmod);
    KAIXO_FUNCTION_OVERLOAD(fpclassify);
    KAIXO_FUNCTION_OVERLOAD(frexp);
    KAIXO_FUNCTION_OVERLOAD(hypot);
    KAIXO_FUNCTION_OVERLOAD(ilogb);
    KAIXO_FUNCTION_OVERLOAD(isfinite);
    KAIXO_FUNCTION_OVERLOAD(isgreater);
    KAIXO_FUNCTION_OVERLOAD(isgreaterequal);
    KAIXO_FUNCTION_OVERLOAD(isinf);
    KAIXO_FUNCTION_OVERLOAD(isless);
    KAIXO_FUNCTION_OVERLOAD(islessequal);
    KAIXO_FUNCTION_OVERLOAD(islessgreater);
    KAIXO_FUNCTION_OVERLOAD(isnan);
    KAIXO_FUNCTION_OVERLOAD(isnormal);
    KAIXO_FUNCTION_OVERLOAD(isunordered);
    KAIXO_FUNCTION_OVERLOAD(ldexp);
    KAIXO_FUNCTION_OVERLOAD(lgamma);
    KAIXO_FUNCTION_OVERLOAD(log);
    KAIXO_FUNCTION_OVERLOAD(log10);
    KAIXO_FUNCTION_OVERLOAD(log1p);
    KAIXO_FUNCTION_OVERLOAD(log2);
    KAIXO_FUNCTION_OVERLOAD(logb);
    KAIXO_FUNCTION_OVERLOAD(modf);
    KAIXO_FUNCTION_OVERLOAD(nan);
    KAIXO_FUNCTION_OVERLOAD(nearbyint);
    KAIXO_FUNCTION_OVERLOAD(nextafter);
    KAIXO_FUNCTION_OVERLOAD(pow);
    KAIXO_FUNCTION_OVERLOAD(remainder);
    KAIXO_FUNCTION_OVERLOAD(remquo);
    KAIXO_FUNCTION_OVERLOAD(rint);
    KAIXO_FUNCTION_OVERLOAD(round);
    KAIXO_FUNCTION_OVERLOAD(scalbn);
    KAIXO_FUNCTION_OVERLOAD(signbit);
    KAIXO_FUNCTION_OVERLOAD(sin);
    KAIXO_FUNCTION_OVERLOAD(sinh);
    KAIXO_FUNCTION_OVERLOAD(sqrt);
    KAIXO_FUNCTION_OVERLOAD(tan);
    KAIXO_FUNCTION_OVERLOAD(tanh);
    KAIXO_FUNCTION_OVERLOAD(tgamma);
    KAIXO_FUNCTION_OVERLOAD(trunc);
    KAIXO_FUNCTION_OVERLOAD(midpoint);
    KAIXO_FUNCTION_OVERLOAD(assoc_laguerre);
    KAIXO_FUNCTION_OVERLOAD(assoc_legendre);
    KAIXO_FUNCTION_OVERLOAD(beta);
    KAIXO_FUNCTION_OVERLOAD(comp_ellint_1);
    KAIXO_FUNCTION_OVERLOAD(comp_ellint_2);
    KAIXO_FUNCTION_OVERLOAD(comp_ellint_3);
    KAIXO_FUNCTION_OVERLOAD(cyl_bessel_i);
    KAIXO_FUNCTION_OVERLOAD(cyl_bessel_j);
    KAIXO_FUNCTION_OVERLOAD(cyl_bessel_k);
    KAIXO_FUNCTION_OVERLOAD(cyl_neumann);
    KAIXO_FUNCTION_OVERLOAD(ellint_1);
    KAIXO_FUNCTION_OVERLOAD(ellint_2);
    KAIXO_FUNCTION_OVERLOAD(ellint_3);
    KAIXO_FUNCTION_OVERLOAD(expint);
    KAIXO_FUNCTION_OVERLOAD(hermite);
    KAIXO_FUNCTION_OVERLOAD(laguerre);
    KAIXO_FUNCTION_OVERLOAD(legendre);
    KAIXO_FUNCTION_OVERLOAD(riemann_zeta);
    KAIXO_FUNCTION_OVERLOAD(sph_bessel);
    KAIXO_FUNCTION_OVERLOAD(sph_legendre);
    KAIXO_FUNCTION_OVERLOAD(sph_neumann);

    // ------------------------------------------------

    // <string>, <cstring>, <cwctype>, <cuchar>
    KAIXO_FUNCTION_OVERLOAD(atof);
    KAIXO_FUNCTION_OVERLOAD(atoi);
    KAIXO_FUNCTION_OVERLOAD(isalnum);
    KAIXO_FUNCTION_OVERLOAD(isalpha);
    KAIXO_FUNCTION_OVERLOAD(isblank);
    KAIXO_FUNCTION_OVERLOAD(iscntrl);
    KAIXO_FUNCTION_OVERLOAD(isdigit);
    KAIXO_FUNCTION_OVERLOAD(isgraph);
    KAIXO_FUNCTION_OVERLOAD(islower);
    KAIXO_FUNCTION_OVERLOAD(isprint);
    KAIXO_FUNCTION_OVERLOAD(ispunct);
    KAIXO_FUNCTION_OVERLOAD(isspace);
    KAIXO_FUNCTION_OVERLOAD(isupper);
    KAIXO_FUNCTION_OVERLOAD(isxdigit);
    KAIXO_FUNCTION_OVERLOAD(memchr);
    KAIXO_FUNCTION_OVERLOAD(memcmp);
    KAIXO_FUNCTION_OVERLOAD(memcpy);
    KAIXO_FUNCTION_OVERLOAD(memmove);
    KAIXO_FUNCTION_OVERLOAD(memset);
    KAIXO_FUNCTION_OVERLOAD(strcat);
    KAIXO_FUNCTION_OVERLOAD(strchr);
    KAIXO_FUNCTION_OVERLOAD(strcmp);
    KAIXO_FUNCTION_OVERLOAD(strcoll);
    KAIXO_FUNCTION_OVERLOAD(strcpy);
    KAIXO_FUNCTION_OVERLOAD(strcspn);
    KAIXO_FUNCTION_OVERLOAD(strerror);
    KAIXO_FUNCTION_OVERLOAD(strlen);
    KAIXO_FUNCTION_OVERLOAD(strncat);
    KAIXO_FUNCTION_OVERLOAD(strncmp);
    KAIXO_FUNCTION_OVERLOAD(strncpy);
    KAIXO_FUNCTION_OVERLOAD(strpbrk);
    KAIXO_FUNCTION_OVERLOAD(strrchr);
    KAIXO_FUNCTION_OVERLOAD(strspn);
    KAIXO_FUNCTION_OVERLOAD(strstr);
    KAIXO_FUNCTION_OVERLOAD(strtof);
    KAIXO_FUNCTION_OVERLOAD(strtok);
    KAIXO_FUNCTION_OVERLOAD(strtol);
    KAIXO_FUNCTION_OVERLOAD(strtoul);
    KAIXO_FUNCTION_OVERLOAD(strxfrm);
    KAIXO_FUNCTION_OVERLOAD(tolower);
    KAIXO_FUNCTION_OVERLOAD(toupper);
    KAIXO_FUNCTION_OVERLOAD(to_string);
    KAIXO_FUNCTION_OVERLOAD(copy);
    KAIXO_FUNCTION_OVERLOAD(btowc);
    KAIXO_FUNCTION_OVERLOAD(c16rtomb);
    KAIXO_FUNCTION_OVERLOAD(c32rtomb);
    KAIXO_FUNCTION_OVERLOAD(mblen);
    KAIXO_FUNCTION_OVERLOAD(mbrlen);
    KAIXO_FUNCTION_OVERLOAD(mbrtoc16);
    KAIXO_FUNCTION_OVERLOAD(mbrtoc32);
    KAIXO_FUNCTION_OVERLOAD(mbrtowc);
    KAIXO_FUNCTION_OVERLOAD(mbsinit);
    KAIXO_FUNCTION_OVERLOAD(mbsrtowcs);
    KAIXO_FUNCTION_OVERLOAD(mbstowcs);
    KAIXO_FUNCTION_OVERLOAD(mbtowc);
    KAIXO_FUNCTION_OVERLOAD(wcrtomb);
    KAIXO_FUNCTION_OVERLOAD(wcsrtombs);
    KAIXO_FUNCTION_OVERLOAD(wcstombs);
    KAIXO_FUNCTION_OVERLOAD(wctob);
    KAIXO_FUNCTION_OVERLOAD(wctomb);
    KAIXO_FUNCTION_OVERLOAD(iswalnum);
    KAIXO_FUNCTION_OVERLOAD(iswalpha);
    KAIXO_FUNCTION_OVERLOAD(iswblank);
    KAIXO_FUNCTION_OVERLOAD(iswcntrl);
    KAIXO_FUNCTION_OVERLOAD(iswctype);
    KAIXO_FUNCTION_OVERLOAD(iswdigit);
    KAIXO_FUNCTION_OVERLOAD(iswgraph);
    KAIXO_FUNCTION_OVERLOAD(iswlower);
    KAIXO_FUNCTION_OVERLOAD(iswprint);
    KAIXO_FUNCTION_OVERLOAD(iswpunct);
    KAIXO_FUNCTION_OVERLOAD(iswspace);
    KAIXO_FUNCTION_OVERLOAD(iswupper);
    KAIXO_FUNCTION_OVERLOAD(iswxdigit);
    KAIXO_FUNCTION_OVERLOAD(towctrans);
    KAIXO_FUNCTION_OVERLOAD(towlower);
    KAIXO_FUNCTION_OVERLOAD(towupper);
    KAIXO_FUNCTION_OVERLOAD(wcscat);
    KAIXO_FUNCTION_OVERLOAD(wcschr);
    KAIXO_FUNCTION_OVERLOAD(wcscmp);
    KAIXO_FUNCTION_OVERLOAD(wcscoll);
    KAIXO_FUNCTION_OVERLOAD(wcscpy);
    KAIXO_FUNCTION_OVERLOAD(wcscspn);
    KAIXO_FUNCTION_OVERLOAD(wcslen);
    KAIXO_FUNCTION_OVERLOAD(wcsncat);
    KAIXO_FUNCTION_OVERLOAD(wcsncmp);
    KAIXO_FUNCTION_OVERLOAD(wcsncpy);
    KAIXO_FUNCTION_OVERLOAD(wcspbrk);
    KAIXO_FUNCTION_OVERLOAD(wcsrchr);
    KAIXO_FUNCTION_OVERLOAD(wcsspn);
    KAIXO_FUNCTION_OVERLOAD(wcsstr);
    KAIXO_FUNCTION_OVERLOAD(wcstof);
    KAIXO_FUNCTION_OVERLOAD(wcstok);
    KAIXO_FUNCTION_OVERLOAD(wcstol);
    KAIXO_FUNCTION_OVERLOAD(wcstoul);
    KAIXO_FUNCTION_OVERLOAD(wcsxfrm);
    KAIXO_FUNCTION_OVERLOAD(wctrans);
    KAIXO_FUNCTION_OVERLOAD(wctype);
    KAIXO_FUNCTION_OVERLOAD(wmemchr);
    KAIXO_FUNCTION_OVERLOAD(wmemcmp);
    KAIXO_FUNCTION_OVERLOAD(wmemcpy);
    KAIXO_FUNCTION_OVERLOAD(wmemmove);
    KAIXO_FUNCTION_OVERLOAD(wmemset);

    // ------------------------------------------------

}

// ------------------------------------------------