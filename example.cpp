#include <map>
#include <typeindex>
#include <vector>
#include <memory>
#include <type_traits>
#include <string_view>
#include <string>
#include <ranges>
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
    namespace ranges = std::ranges;
    namespace views = std::views;

    template<class Ty> concept is_range = ranges::range<Ty>;

    namespace has {
        template<class Ty> concept depend_v = requires (Ty) { typename Ty::depend; };
        template<class Ty> concept define_v = requires (Ty) { typename Ty::define; };

        template<class Ty> struct depend_impl : std::bool_constant<depend_v<Ty>> {};
        template<class Ty> struct define_impl : std::bool_constant<define_v<Ty>> {};

        constexpr auto depend = type_trait<depend_impl>{};
        constexpr auto define = type_trait<define_impl>{};
    }

    namespace grab {
        template<class Ty> struct depend_impl { using type = info<>; };
        template<has::depend_v Ty> struct depend_impl<Ty> { using type = typename Ty::depend; };
        template<class Ty> struct define_impl { using type = info<>; };
        template<has::define_v Ty> struct define_impl<Ty> { using type = typename Ty::define; };

        template<class Ty> using depend = depend_impl<Ty>::type;
        template<class Ty> using define = define_impl<Ty>::type;
    }

    template<class Ty>
    concept is_dependent = requires() { typename decay_t<Ty>::define; typename decay_t<Ty>::depend; };

    template<class Ty> using depend = grab::depend<decay_t<Ty>>;
    template<class Ty> using define = grab::define<decay_t<Ty>>;

    template<class ...> struct expression;
    template<class Ty> concept is_var = is_dependent<Ty> && requires() { { decay_t<Ty>::name }; };
    template<class Ty> concept is_expression = specialization<Ty, expression>;
    template<class Ty> concept is_varexpr = is_expression<Ty> || is_var<Ty>;
    template<class Ty> concept is_operator = requires() { typename Ty::is_operator; };
    template<class Ty> concept is_partial_range = requires() { typename Ty::is_partial_range; };
    template<class Ty> concept is_range_kind = is_range<Ty> || is_partial_range<Ty>;
    template<class Ty> concept is_named_range = is_range_kind<Ty> && is_var<Ty>;

    template<class Ty> struct is_expression_ : std::bool_constant<is_expression<Ty>> {};
    constexpr auto is_expression_v = type_trait<is_expression_>{};
    
    template<class ...As> concept are_valid_expression_operators
        = (is_varexpr<decay_t<As>> || ...) && (!is_range_kind<decay_t<As>> && ...) && (!is_operator<decay_t<As>> && ...);

    template<class Ty, is_var Var>
    struct named_value {
        using value_type = Ty;
        using var = Var;

        value_type value;
    };

    template<string_literal Name>
    struct var_t {
        using define = info<var_t>;
        using depend = info<var_t>;

        constexpr static string_literal name = Name;

        template<class Ty>
        constexpr named_value<decay_t<Ty>, var_t> operator=(Ty&& value) const {
            return { std::forward<Ty>(value) };
        }
    };

    template<class Ty> concept is_named_value = specialization<Ty, named_value>;

    template<is_named_value ...Args>
    struct named_tuple : std::tuple<typename Args::value_type...> {
        using parent = std::tuple<typename Args::value_type...>;
        using vars = info<typename Args::var...>;

        constexpr named_tuple(Args&&...args) : parent(args.value...) {}

        template<is_var Var>
        constexpr decltype(auto) get() const {
            static_assert(vars::template occurs<decay_t<Var>>, "Variable is not part of tuple");
            constexpr std::size_t index = vars::template index<Var>;
            return std::get<index>(*this);
        }
    };

    template<is_range_kind Range, is_var Var>
    struct named_range : Range, Var {
        using define = define<Var>;
        using depend = depend<Range>;
    };

    template<is_varexpr R, class Part>
    struct lc_construct {
        using define = define<Part>;
        using depend = concat_t<depend<R>, depend<Part>>::unique::template remove<define>;

        R result;
        Part part;
    };

    template<is_varexpr R, class ...Parts>
    struct list_comprehension {
        using define = concat_t<define<Parts>...>::unique;
        using depend = concat_t<depend<R>, depend<Parts>...>::unique::template remove<define>;

        R result;
        std::tuple<Parts...> parts;

        constexpr decltype(auto) eval(auto& tuple) {
        }
    };

    template<class Ty> concept is_lc_construct = specialization<Ty, lc_construct>;
    template<class Ty> concept is_lc = specialization<Ty, list_comprehension>;

    constexpr auto operator-(is_range auto& r) { return views::all(r); }
    constexpr auto operator-(is_range auto&& r) { return views::all(std::move(r)); }
    constexpr auto operator-(is_partial_range auto& r) { return r; }
    constexpr auto operator-(is_partial_range auto&& r) { return std::move(r); }
    constexpr auto operator<(is_var auto v, is_range_kind auto&& r) { return named_range{ std::move(r), v }; }
    constexpr auto operator|(is_varexpr auto&& v, is_named_range auto&& r) { return lc_construct{ v, std::move(r) }; }

    template<is_var A, class Ty>
    constexpr decltype(auto) eval_in_expression(const A& value, Ty& tuple) {
        if constexpr (decay_t<Ty>::vars::template occurs<decay_t<A>>) return tuple.get<A>();
        else return value;
    }

    template<is_expression A, class Ty>
    constexpr decltype(auto) eval_in_expression(A& value, Ty& tuple) {
        return value.eval(tuple);
    }

    template<class A, class Ty>
    constexpr A& eval_in_expression(A& value, Ty& tuple) {
        return value;
    }

    template<class A, class B, is_operator Op> requires are_valid_expression_operators<A, B>
    struct expression<A, B, Op> {
        using define = concat_t<define<A>, define<B>>::unique;
        using depend = concat_t<depend<A>, depend<B>>::unique;

        [[no_unique_address]] A a{};
        [[no_unique_address]] B b{};

        constexpr decltype(auto) eval(auto& tuple) const {
            return Op::eval(eval_in_expression(a, tuple), eval_in_expression(b, tuple));
        }

        template<is_named_value ...Args>
        constexpr decltype(auto) operator()(Args&&...args) const {
            auto tuple = named_tuple{ std::forward<Args>(args)... };
            return Op::eval(eval_in_expression(a, tuple), eval_in_expression(b, tuple));
        }
    };

    template<is_varexpr A, is_operator Op>
    struct expression<A, Op> {
        using define = define<A>::unique;
        using depend = depend<A>::unique;

        [[no_unique_address]] A a{};

        constexpr decltype(auto) eval(auto& tuple) const {
            return Op::eval(eval_in_expression(a, tuple));
        }
    };

    template<is_partial_range A>
    struct expression<A> {
        using define = define<A>;
        using depend = depend<A>;

        A a{};

        constexpr decltype(auto) eval(auto& tuple) const {
            return a.eval(tuple);
        }
    };

    template<class ...As> requires are_valid_expression_operators<As...>
    struct expression<As...> {
        using define = concat_t<define<As>...>::unique;
        using depend = concat_t<depend<As>...>::unique;

        std::tuple<As...> parts;

        constexpr decltype(auto) eval(auto& tuple) const {
            auto res = [&]<std::size_t ...Is>(std::index_sequence<Is...>) {
                return std::tuple{ eval_in_expression(std::get<Is>(parts), tuple)... };
            }(std::index_sequence_for<As...>{});
            if constexpr (as_info<decltype(res)>::template count_filter<is_expression_v> == 0) {
                return res;
            } else {
                return move_tparams_t<decltype(res), expression>{ std::move(res) };
            }
        }
    };

    template<is_var ...As>
    constexpr auto operator|(info<As...>, is_named_range auto&& r) {
        return lc_construct{ expression<As...>{ std::tuple{ As{}... } }, std::move(r) };
    }

    constexpr struct lc_t {
        template<is_lc_construct Ty, is_dependent ...Parts>
        constexpr decltype(auto) operator()(Ty&& construct, Parts&& ...parts) const {
            using lc_t = decltype(list_comprehension{ std::forward<Ty>(construct).result, std::tuple{
                std::forward<Ty>(construct).part, std::forward<Parts>(parts)...
            } });
            if constexpr (lc_t::depend::size == 0) {
                return list_comprehension{ std::forward<Ty>(construct).result, std::tuple{
                    std::forward<Ty>(construct).part, std::forward<Parts>(parts)...
                } };
            } else {
                list_comprehension l{ std::forward<Ty>(construct).result, std::tuple{
                    std::forward<Ty>(construct).part, std::forward<Parts>(parts)...
                } };

                return expression<decltype(l)>{ std::move(l) };
            }
        };
    } lc;

    namespace operators {
        template<is_var A, is_var B> constexpr auto operator,(const A&, const B&) { return info<A, B>{}; }
        template<is_var A, is_var ...Bs> constexpr auto operator,(info<Bs...>, const A&) { return info<Bs..., A>{}; }
        template<is_var A, is_var ...Bs> constexpr auto operator,(const A&, info<Bs...>) { return info<A, Bs...>{}; }

        template<class A, class B> requires are_valid_expression_operators<A, B>
        constexpr auto operator,(A&& a, B&& b) {
            return expression<decay_t<A>, decay_t<B>>{ std::tuple{ std::forward<A>(a), std::forward<B>(b) } };
        }

        template<class A, class ...Bs> requires are_valid_expression_operators<Bs...>
        constexpr auto operator,(A&& a, expression<Bs...>&& b) {
            return expression<decay_t<A>, Bs...>{ std::tuple_cat(std::forward_as_tuple(std::forward<A>(a)), b.parts) };
        }

        template<class A, class ...Bs> requires are_valid_expression_operators<Bs...>
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
        template<class A, class B> requires are_valid_expression_operators<A, B>      \
        constexpr expression<decay_t<A>, decay_t<B>, name> operator op(A&& a, B&& b) {                  \
            return expression<decay_t<A>, decay_t<B>, name>{ std::forward<A>(a), std::forward<B>(b) };  \
        }

    #define KAIXO_UNARY_OPERATOR(name, op)                \
        struct name {                                     \
            using is_operator = int;                      \
            template<class A>                             \
            constexpr static decltype(auto) eval(A&& a) { \
                return op a;                              \
            }                                             \
        };                                                             \
                                                                       \
        template<is_varexpr A>                                         \
        constexpr expression<decay_t<A>, name> operator op(A&& a) {    \
            return expression<decay_t<A>, name>{ std::forward<A>(a) }; \
        }
        
        KAIXO_UNARY_OPERATOR(dereference, *);
        KAIXO_UNARY_OPERATOR(pointer, &);
        KAIXO_UNARY_OPERATOR(negate, -);
        KAIXO_UNARY_OPERATOR(plus, +);
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

    namespace containers {
        template<class ...As> struct range_t;

        template<trivial Ty> requires (!is_varexpr<Ty>)
            struct range_t<Ty> {
            Ty a{};
            Ty b{};
            constexpr range_t(Ty a, Ty b) : a(a), b(b) {}

            struct iterator {
                using difference_type = std::ptrdiff_t;
                using value_type = Ty;
                using iterator_category = std::forward_iterator_tag;

                Ty value;

                constexpr iterator& operator++() { ++value; return *this; }
                constexpr iterator operator++(int) { iterator b = *this; ++value; return b; }
                constexpr Ty operator*() { return value; }
                constexpr bool operator==(const iterator& b) const { return value == b.value; }
            };

            constexpr iterator begin() { return { a }; }
            constexpr iterator end() { return { b }; }
        };

        template<is_varexpr A, trivial Ty> requires (!is_varexpr<Ty>)
            struct range_t<A, Ty> {
            using is_partial_range = int;
            using depend = info<A>;

            Ty b{};
            constexpr range_t(const A&, Ty b) : b(b) {}

            constexpr auto eval(auto& tuple) { return range_t{ tuple.get<A>(), b }; }
        };

        template<trivial Ty, is_varexpr B> requires (!is_varexpr<Ty>)
            struct range_t<Ty, B> {
            using is_partial_range = int;
            using depend = info<B>;

            Ty a{};
            constexpr range_t(Ty a, const B&) : a(a) {}

            constexpr auto eval(auto& tuple) { return range_t{ a, tuple.get<B>() }; }
        };

        template<is_varexpr A, is_varexpr B>
        struct range_t<A, B> {
            using is_partial_range = int;
            using depend = info<A, B>;

            constexpr range_t(const A&, const B&) {}

            constexpr auto eval(auto& tuple) { return range_t{ tuple.get<A>(), tuple.get<B>() }; }
        };

        template<class A> requires (!is_varexpr<A>) range_t(A, A)->range_t<A>;
        template<is_varexpr A, class B>  requires (!is_varexpr<B>) range_t(A, B)->range_t<A, B>;
        template<class A, is_varexpr B>  requires (!is_varexpr<A>) range_t(A, B)->range_t<A, B>;
        template<is_varexpr A, is_varexpr B> range_t(A, B) -> range_t<A, B>;

        template<class A, class B>
        constexpr auto range(A&& a, B&& b) {
            using range_type = decltype(range_t{ std::forward<A>(a), std::forward<B>(b) });
            if constexpr (is_partial_range<range_type>) {
                return range_t{ std::forward<A>(a), std::forward<B>(b) };
            }
            else {
                return range_t{ std::forward<A>(a), std::forward<B>(b) };
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

    //constexpr named_tuple vls{ b = 1, c = 10 };
    //constexpr named_tuple vls2{ a = 4 };

    constexpr auto expr = (a, b);

    //constexpr auto ana = expr.eval(vls);
    //constexpr auto aoine = ana.eval(vls2);



    //constexpr auto r6 = lc((x, a + b) | a <- std::array{ 1 }, x <- std::array{ 1, 2, 3, 4 });

    //auto r7 = r6.eval(vls);

    //decltype(r6)::depend::size;

    //auto oaine = lc(a | a <- range(b, c), b <- range(1, c), c <- range(1, 10));

    return 0;
}

 