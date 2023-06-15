#include <map>
#include <typeindex>
#include <vector>
#include <memory>
#include <type_traits>
#include <string_view>
#include <string>
#include <ranges>
#include <iostream>
#include "type_utils.hpp"

namespace aaaa {

    struct event {
        virtual ~event() = default;
    };

    struct event_listener {
        struct handler { virtual void handle(event& event) = 0; };
        template<class> struct typed;
        template<class C, class A> struct typed<void (C::*) (A) const> : C, handler {
            using event_type = std::decay_t<A>;
            template<class Q> typed(Q&& q) : C(std::forward<Q>(q)) {}
            void handle(event& e) override { (*this)(dynamic_cast<event_type&>(e)); }
        };

        template<class Lambda> void operator+=(Lambda&& lambda) {
            using handler = typed<decltype(&Lambda::operator())>;
            auto _handler = std::make_unique<handler>(std::forward<Lambda>(lambda));
            handlers[typeid(handler::event_type)].push_back(std::move(_handler));
        }

        void handle(std::derived_from<event> auto& e) {
            for (auto& handler : handlers[typeid(e)]) handler->handle(e);
        }

        std::map<std::type_index, std::vector<std::unique_ptr<handler>>> handlers;
    };

    struct mouse_pressed : event {
        int x{};
        int y{};
    };

    struct key_pressed : event {
        int key{};
    };


    template<class Ty>
    struct type_wrapper_t {
        using type = Ty;

        template<class ...Args>
        constexpr Ty operator()(Args&& ...args) const {
            return Ty{ std::forward<Args>(args)... };
        }
    };

    template<class> struct is_type_wrapper_impl : std::false_type {};
    template<class Ty> struct is_type_wrapper_impl<type_wrapper_t<Ty>> : std::true_type {};
    template<class Ty> concept is_type_wrapper = is_type_wrapper_impl<Ty>::value;

    template<class Ty> constexpr type_wrapper_t<Ty> is{};
    template<class Ty> constexpr type_wrapper_t<Ty> has{};

    template<class A, class B>
    constexpr auto operator and(type_wrapper_t<A>, type_wrapper_t<B>) {
        struct impl : A, B {};
        return type_wrapper_t<impl>{};
    }

    template<class> struct mptr_class;
    template<class R, class C, class ...Args>
    struct mptr_class<R(C::*)(Args...)> : std::type_identity<C> {};
    template<class R, class C, class ...Args>
    struct mptr_class<R(C::*)(Args...) const> : std::type_identity<const C> {};

    template<class, class> struct mptr_sign;
    template<class T, class R, class C, class ...Args>
    struct mptr_sign<T, R(C::*)(Args...)> : std::type_identity<R(*)(T*, Args...)> {};
    template<class T, class R, class C, class ...Args>
    struct mptr_sign<T, R(C::*)(Args...) const> : std::type_identity<R(*)(T*, Args...)> {};

    template<std::size_t N> constexpr std::size_t _unit() { return N; }
    template<std::size_t N = 10>
    constexpr std::size_t vtable_index(auto fptr) {
        using type = std::decay_t<typename mptr_class<decltype(fptr)>::type>;
        auto ptr = reinterpret_cast<std::size_t(type::*)(void)>(fptr);
        return[&]<std::size_t ...Is>(std::index_sequence<Is...>) {
            constexpr std::size_t(*indexer[])(void) { _unit<Is>... };
            const void* vtable = &indexer;
            return (reinterpret_cast<type*>(&vtable)->*ptr)();
        }(std::make_index_sequence<N>{});
    }

    template<class FptrT, class Lambda>
    struct virtual_impl {
        using type = mptr_class<FptrT>::type;
        using lambda_type = std::decay_t<Lambda>;
        using fptr_type = FptrT;
        FptrT fptr;
        Lambda lambda;
    };

    template<class Ty, class Lambda>
    constexpr auto operator >>(Ty fun, Lambda&& l) {
        return virtual_impl{ fun, std::forward<Lambda>(l) };
    }

    template<class Type, std::size_t N = 10, class ...Args>
    constexpr auto construct(auto construct, Args&&... parts) {
        struct impl_t : Type {
            void* _vtable[N]{};
            std::tuple<typename std::decay_t<Args>::lambda_type...> lambdas;
        };

        void* allocated = new char[sizeof(impl_t)];
        impl_t* inst = reinterpret_cast<impl_t*>(allocated);
        reinterpret_cast<void**>(allocated)[0] = &inst->_vtable;
        construct(*inst);

        auto copy_impl = [&](auto* ptr, auto& impl, auto call) {
            using arg = std::decay_t<decltype(impl)>;
            using signature = mptr_sign<impl_t, typename arg::fptr_type>::type;;
            new (ptr) arg::lambda_type(impl.lambda);
            inst->_vtable[vtable_index<N>(impl.fptr)] = (signature)(call);
        };

        [&] <std::size_t ...Is>(std::index_sequence<Is...>) {
            (copy_impl(&std::get<Is>(inst->lambdas), parts,
                []<class ...Tys>(impl_t * self, Tys... args) ->decltype(auto) {
                using type = std::tuple_element_t<Is, std::tuple<Args...>>::type;
                return std::get<Is>(self->lambdas)(*(type*)self, args...);
            }), ...);
        }(std::index_sequence_for<Args...>{});

        return std::unique_ptr<impl_t>(inst);
    }

    struct Clickable {
        virtual void click() = 0;
        virtual void test(int) = 0;
        virtual int test2() const = 0;

        int a = 1;
    };

    struct Test {
        virtual void test() = 0;
    };

    template<std::integral Ty, Ty ...Is>
    struct sequence {
        constexpr static std::size_t size = sizeof...(Is);

    };

    template<std::size_t ...Is>
    using index_sequence = sequence<std::size_t, Is...>;

    template<class Ty, Ty ...Is>
    struct make_sequence;

    template<class Ty, Ty N> requires (N > 1)
        struct make_sequence<Ty, N> : make_sequence<Ty, N - 2, N - 1> {};

    template<class Ty, Ty N> requires (N == 1)
        struct make_sequence<Ty, N> : sequence<Ty, 0> {};

    template<class Ty, Ty N> requires (N == 0)
        struct make_sequence<Ty, N> : sequence<Ty> {};

    template<class Ty, Ty N, Ty ...Is> requires (N != 0)
        struct make_sequence<Ty, N, Is...> : make_sequence<Ty, N - 1, N, Is...> {};

    template<class Ty, Ty N, Ty ...Is> requires (N == 0)
        struct make_sequence<Ty, N, Is...> : sequence<Ty, N, Is...> {};

    template<auto Fun, std::size_t N, class Ty, Ty ...Is>
    struct make_complex_sequence;

    template<auto Fun, std::size_t N, class Ty, Ty ...Is> requires (N != 1)
        struct make_complex_sequence<Fun, N, Ty, Is...> : make_complex_sequence<Fun, N - 1, Ty, Fun(N - 1), Is...> {};

    template<auto Fun, std::size_t N, class Ty, Ty ...Is> requires (N == 1)
        struct make_complex_sequence<Fun, N, Ty, Is...> : sequence<Ty, Fun(N - 1), Is...> {};

    template<auto Fun, std::size_t N, class Ty> requires (N == 0)
        struct make_complex_sequence<Fun, N, Ty> : sequence<Ty> {};

    template<auto Fun, std::size_t N>
    using make_complex_index_sequence = make_complex_sequence<Fun, N, std::size_t>;

    template<std::size_t N>
    using make_index_sequence = make_sequence<std::size_t, N>;

    constexpr double constexpr_pow(double v, std::size_t p) {
        if (p == 0) return 1;
        else if (p % 2 == 1) return constexpr_pow(v, p / 2ull) * constexpr_pow(v, p / 2ull) * v;
        else return constexpr_pow(v, p / 2ull) * constexpr_pow(v, p / 2ull);
    }

    constexpr std::size_t fib(std::size_t n) {
        return 0.5 + (constexpr_pow(1.6180339, n) - constexpr_pow(-0.6180339, n)) / 2.236067977;
    }

    template<std::size_t ...Is>
    constexpr std::size_t tests(index_sequence<Is...>) {
        return (Is + ...);
    }


    struct typeless_vector {
        struct value_data {
            std::size_t size = 0;
            std::size_t offset = 0;
            const std::type_info* type;
        };

        struct value_type {
            typeless_vector* self;
            value_data* data;

            template<class Ty>
            constexpr bool is() const {
                return (*data->type) == typeid(Ty);
            }

            template<class Ty>
            constexpr operator Ty& () const {
                return *reinterpret_cast<Ty*>(&self->m_Data[data->offset]);
            }
        };

        template<class Ty, class ...Args>
        Ty& emplace_back(Args&&...args) {
            m_Info.emplace_back(value_data{
                .size = sizeof(Ty),
                .offset = offset_back(),
                .type = &typeid(Ty)
                });
            auto it = m_Data.insert(m_Data.end(), sizeof(Ty), {});
            return *new (&*it) Ty{ std::forward<Args>(args)... };
        }

        value_type operator[](std::size_t i) {
            return value_type{ this, &m_Info[i] };
        }


    private:
        std::vector<std::byte> m_Data{};
        std::vector<value_data> m_Info{};

        constexpr std::size_t offset(std::size_t i) {
            return m_Info[i].offset;
        }

        constexpr std::size_t offset_back() {
            if (m_Info.size() == 0) return 0;
            else return m_Info.back().offset + m_Info.back().size;
        }
    };

}

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
    template<class Ty> concept is_named_range = has::range_v<Ty> && has::var_v<Ty>;
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
            return std::forward<Self>(self).value.eval(tuple);
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
            if constexpr (as_info<decltype(res)>::template count_filter<[]<is_varexpr>{}> != 0)
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
        };                                                                            \
                                                                                      \
        template<class A, class B> requires are_valid_expression<A, B>      \
        constexpr expression<decay_t<A>, decay_t<B>, name> operator op(A&& a, B&& b) {                  \
            return expression<decay_t<A>, decay_t<B>, name>{ std::forward<A>(a), std::forward<B>(b) };  \
        }

#define KAIXO_UNARY_OPERATOR(name, op)                \
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

    template<class Ty>
    concept is_executable = requires(Ty ty, named_tuple<> t) { { execute(ty, t) }; };

    /**
     * Range linked to variable. 
     * @tparam Range range
     * @tparam Var variable
     */
    template<is_range Range, is_var Var>
    struct named_range : Range {
        using define = info<Var>;
        using depend = depend<Range>;
        using range = Range;
        using var = Var;

        constexpr named_range(Range&& range, const Var&) : Range(std::move(range)) {}
        constexpr named_range(const Range& range, const Var&) : Range(range) {}
    };
    
    /**
     * Partial range linked to variable, still depends on variables.
     * @tparam Range range
     * @tparam Var variable
     */
    template<is_range_kind Range, is_var Var>
    struct partial_named_range : Range {
        using is_partial_range = int;

        using define = info<Var>;
        using depend = depend<Range>;
        using range = Range;
        using var = Var;

        constexpr partial_named_range(Range&& range, const Var&) : Range(std::move(range)) {}
        constexpr partial_named_range(const Range& range, const Var&) : Range(range) {}
        
        template<class Self>
        constexpr decltype(auto) eval(this Self&& self, is_named_tuple auto& tuple) {
            using remainding = depend::template remove<typename decay_t<decltype(tuple)>::vars>;
            if constexpr (remainding::size == 0) {
                return named_range{ std::forward<Self>(self).Range::eval(tuple), Var{} };
            } else {
                using lc_t = partial_named_range<decltype(std::declval<Range&>().eval(tuple)), Var>;
                return expression<lc_t>{ lc_t{ std::forward<Self>(self).Range::eval(tuple), Var{} } };
            }
        }
    };

    /**
     * Initial part of constructing a list comprehension object.
     * @tparam R result expression
     * @tparam Part first part of list comprehension
     */
    template<is_varexpr R, is_dependent Part>
    struct lc_construct {
        using define = define<Part>;
        using depend = concat_t<depend<R>, depend<Part>>::unique::template remove<define>;

        R result;
        Part part;
    };

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

    template<is_named_range R, is_named_tuple T> 
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

    template<is_named_range R, is_named_tuple T>
    struct defined_values<R, T> : std::type_identity<info<named_value<range_value_t<R, T>, typename R::var>>> {};

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
        none  = 0, // Continue like normal
        stop  = 1, // Stop iteration now, and set to end
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
     * List comprehension object.
     * @tparam R result expression
     * @tparam ...Parts comprehension parts
     */
    template<class R, class ...Parts>
    struct list_comprehension {
        using define = concat_t<define<Parts>...>::unique;
        using depend = concat_t<depend<R>, depend<Parts>...>::unique::template remove<define>;

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

        private:
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
                if constexpr (is_named_range<type>) {
                    do {
                        if constexpr (is_partial_range<type>) {
                            if (++iter == std::ranges::end(intr.value())) {
                                if constexpr (I != 0) increment<I - 1>();
                                intr = eval_in_expression(part, values.value());
                                iter = std::ranges::begin(intr.value());

                                if constexpr (I == 0) { end = true; return; }
                            }
                        } else {
                            if (++iter == std::ranges::end(part)) {
                                if constexpr (I != 0) increment<I - 1>(); 
                                iter = std::ranges::begin(part);

                                if constexpr (I == 0) { end = true; return; }
                            }
                        }

                        values.value().get<typename type::var>() = *iter;

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
                } else return return_code::none;
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
                    if constexpr (is_partial_range<type>) {
                        intr = part.eval(cur_values);
                        iter = std::ranges::begin(intr.value());

                        return initialize<I + 1>(std::forward<Args>(args)..., typename type::var{} = *iter);
                    } else if constexpr (is_range<type>) {
                        iter = std::ranges::begin(part);

                        return initialize<I + 1>(std::forward<Args>(args)..., typename type::var{} = *iter);
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

        R result;
        std::tuple<Parts...> parts;

        template<class Self>
        constexpr decltype(auto) eval(this Self&& self, is_named_tuple auto& tuple) {
            // Find remaining dependencies.
            using remainding = depend::template remove<typename decay_t<decltype(tuple)>::vars>;
            return sequence<sizeof...(Parts)>([&]<std::size_t ...Is>() {
                if constexpr (remainding::size == 0) { // No more dependencies
                    return list_comprehension{
                        eval_in_expression(std::forward<Self>(self).result, tuple),
                        std::make_tuple(eval_in_expression(std::get<Is>(std::forward<Self>(self).parts), tuple)...)
                    };
                } else {
                    using lc_t = partial_list_comprehension<
                        decay_t<decltype(eval_in_expression(std::declval<R&>(), tuple))>,
                        decay_t<decltype(eval_in_expression(std::declval<Parts&>(), tuple))>...>;
                    return expression<lc_t>{ lc_t{
                        eval_in_expression(std::forward<Self>(self).result, tuple),
                        std::make_tuple(eval_in_expression(std::get<Is>(std::forward<Self>(self).parts), tuple)...)
                    } };
                }
            });
        }
    };

    template<class Ty> concept is_lc_construct = specialization<Ty, lc_construct>;
    template<class Ty> concept is_lc = specialization<Ty, list_comprehension>;

    constexpr struct lc_t {
        template<is_lc_construct Ty, is_dependent ...Parts>
        constexpr decltype(auto) operator()(Ty&& construct, Parts&& ...parts) const {
#define KAIXO_PARTIAL_CONSTRUCT(type) type{ std::forward<Ty>(construct).result, std::tuple{ \
                std::forward<Ty>(construct).part, std::forward<Parts>(parts)...             \
            } }
            // Determine whether the list comprehension is complete.
            using lc_t = decltype(KAIXO_PARTIAL_CONSTRUCT(partial_list_comprehension));
            if constexpr (lc_t::depend::size == 0) return KAIXO_PARTIAL_CONSTRUCT(list_comprehension);
            else return expression<lc_t>{ KAIXO_PARTIAL_CONSTRUCT(partial_list_comprehension) };
#undef KAIXO_PARTIAL_CONSTRUCT
        };
    } lc;

    namespace operators {
        constexpr auto operator-(is_range auto& r) { return views::all(r); }
        constexpr auto operator-(is_range auto&& r) { return views::all(std::move(r)); }
        constexpr auto operator-(is_partial_range auto& r) { return r; }
        constexpr auto operator-(is_partial_range auto&& r) { return std::move(r); }
        constexpr auto operator<(is_var auto v, is_range auto&& r) { return named_range{ std::move(r), v }; }

        constexpr auto operator<(is_var auto v, is_partial_range auto&& r) { 
            return expression<decltype(partial_named_range{ std::move(r), v })>{ partial_named_range{ std::move(r), v } };
        }

        template<is_varexpr A, is_dependent B>
        constexpr auto operator|(A&& a, B&& b) { 
            return lc_construct<decay_t<A>, decay_t<B>>{ std::forward<A>(a), std::forward<B>(b) }; 
        }

        template<is_var ...As>
        constexpr auto operator|(info<As...>, is_dependent auto&& r) {
            return lc_construct{ expression<As...>{ std::tuple{ As{}... } }, std::move(r) };
        }
    }

    namespace containers {
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
            if constexpr (is_partial_range<range_type>) {
                return expression<range_type>{ range_type{ std::forward<A>(a), std::forward<B>(b) } };
            } else {
                return range_type{ std::forward<A>(a), std::forward<B>(b) };
            }
        }
    }
}

using namespace kaixo;
using namespace kaixo::operators;
using namespace kaixo::containers;

int main() {

    constexpr var_t<"a"> a;
    constexpr var_t<"b"> b;
    constexpr var_t<"c"> c;
    constexpr var_t<"x"> x;

    constexpr named_tuple vls{ b = 74, c = 10 };
    constexpr named_tuple vls2{ a = 4 };
    constexpr named_tuple vls3{ x = 4 };

    auto fgrg = (a, b + 1).eval(vls);
    auto ffffe = lc((a, b) | a <- range(1, 10), b <- range(1, a), a == b);
    auto aoinef = sizeof(ffffe);
    


    auto test = lc((a, b, c) | c <- range(1, 11), b <- range(1, c), a <- range(1, b), a*a + b*b == c*c);

    auto aionefa = lc(a | a <- range(1, 10));
    //
    auto aoin = lc(a | a <- range(1, b), b % 2 == 0).eval(vls);

    for (auto x : aoin) {}

    auto aeaef = lc(lc(lc(a | a <- range(1, b), c % 2 == 0) | b <- range(1, c)) | c <- range(1, 10));

    constexpr auto eona = a + 10;
    constexpr auto eion = execute(eona, vls2);

    for (auto x : aeaef) {
        for (auto u : x) {
            for (auto r : u) {
                std::cout << r << ",";
            }
        }
        std::cout << "\n";
    }

    //for (auto [a, b, c] : test) {
    //    std::cout << "[" << a << ", " << b << ", " << c << "]\n";
    //}

    // increment
    // value

    return 0;
}

 