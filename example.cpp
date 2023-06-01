#include <string_view>
#include <string>
#include <cstddef>
#include <optional>
#include <map>
#include <unordered_map>
#include <vector>
#include <array>
#include "type_utils.hpp"

#include <ranges>

namespace kaixo {
    namespace ranges = std::ranges;
    namespace views = std::views;
    using ranges::range;

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
    concept is_dependent = requires() { typename Ty::define; typename Ty::depend; };

    template<class Ty> using depend = grab::depend<Ty>;
    template<class Ty> using define = grab::define<Ty>;

    template<class Ty>
    concept is_var = is_dependent<Ty> && requires() { { Ty::name }; };

    template<class A, class B, class Op> struct expression;
    template<class Ty> concept is_expression = specialization<Ty, expression>;

    template<class Ty> concept is_named_range = range<Ty> && is_var<Ty>;

    template<class Ty> concept is_valid_expression_part = is_expression<Ty> || is_var<Ty>;
    template<class A, class B> concept are_valid_expression_operators
        = (is_valid_expression_part<decay_t<A>> || is_valid_expression_part<decay_t<B>>)
        && !range<decay_t<A>> && !range<decay_t<B>>;

    template<string_literal Name>
    struct var_t {
        using define = info<var_t>;
        using depend = info<var_t>;

        constexpr static string_literal name = Name;
    };

    template<class Ty, is_var Var>
    struct named_value : Var {
        using value_type = Ty;
        using var = Var;
    };

    template<class Ty> concept is_named_value = specialization<Ty, named_value>;

    template<is_named_value ...Args>
    struct named_tuple : std::tuple<typename Args::value_type...> {
        template<is_var Var>
        constexpr decltype(auto) get() const {
            using vars = info<typename Args::var...>;
            static_assert(vars::template occurs<decay_t<Var>>, "Variable is not part of tuple");
            constexpr std::size_t index = vars::template index<Var>;
            return std::get<index>(*this);
        }
    };

    template<range Range, is_var Var>
    struct named_range : Range, Var {
        using define = define<Var>;
        using depend = depend<Range>;
    };

    template<is_expression R, class ...Parts>
    struct lc_construct {
        using define = concat_t<define<Parts>...>::unique;
        using depend = concat_t<depend<R>, depend<Parts>...>::unique::template remove<define>;

        R result;
        std::tuple<Parts...> parts;
    };

    template<class Ty> concept is_lc_construct = is_dependent<Ty>;

    constexpr auto operator-(range auto& r) { return views::all(r); }
    constexpr auto operator-(range auto&& r) { return views::all(std::move(r)); }
    constexpr auto operator<(is_var auto v, range auto&& r) { return named_range{ std::move(r), v }; }
    constexpr auto operator|(is_expression auto&& v, is_named_range auto&& r) { return lc_construct{ v, std::tuple(std::move(r)) }; }
    constexpr auto operator,(is_lc_construct auto&& v, is_dependent auto&& r) { return lc_construct{ v.result, std::tuple_cat(v.parts, std::tuple(std::move(r))) }; }

    constexpr struct lc_t { constexpr void operator[](auto) {}; } lc;

    template<is_var A, class Ty>
    constexpr decltype(auto) eval_in_expression(A& value, Ty& tuple) {
        return tuple.get<A>();
    }
    
    template<is_expression A, class Ty>
    constexpr decltype(auto) eval_in_expression(A& value, Ty& tuple) {
        return value.eval(tuple);
    }

    template<class A, class Ty>
    constexpr A& eval_in_expression(A& value, Ty& tuple) {
        return value;
    }

    template<class A, class B, class Op>
    struct expression {
        using define = concat_t<define<A>, define<B>>::unique;
        using depend = concat_t<depend<A>, depend<B>>::unique::template remove<define>;

        [[no_unique_address]] A a{};
        [[no_unique_address]] B b{};

        template<class Ty>
        constexpr decltype(auto) eval(Ty& tuple) const {
            return Op::eval(eval_in_expression(a, tuple), eval_in_expression(b, tuple));
        }
    };

    namespace operators {
#define KAIXO_BINARY_OPERATOR(name, op)                          \
        struct name {                                            \
            template<class A, class B>                           \
            constexpr static decltype(auto) eval(A&& a, B&& b) { \
                return a op b;                                   \
            }                                                    \
        };                                                                            \
                                                                                      \
        template<class A, class B> requires are_valid_expression_operators<A, B>      \
        constexpr expression<A, B, name> operator op(A&& a, B&& b) {                  \
            return expression<A, B, name>{ std::forward<A>(a), std::forward<B>(b) };  \
        }

        KAIXO_BINARY_OPERATOR(add, +);
        KAIXO_BINARY_OPERATOR(subtract, -);
        KAIXO_BINARY_OPERATOR(multiply, *);
        KAIXO_BINARY_OPERATOR(divide, /);
        KAIXO_BINARY_OPERATOR(modulo, %);
        KAIXO_BINARY_OPERATOR(less_than, <);
        KAIXO_BINARY_OPERATOR(less_or_equal, <=);
        KAIXO_BINARY_OPERATOR(greater_than, >);
        KAIXO_BINARY_OPERATOR(greater_or_equal, >=);
        KAIXO_BINARY_OPERATOR(equal, ==);
        KAIXO_BINARY_OPERATOR(not_equal, !=);
        KAIXO_BINARY_OPERATOR(left_shift, <<);
        KAIXO_BINARY_OPERATOR(right_shift, >>);
        KAIXO_BINARY_OPERATOR(boolean_and, &&);
        KAIXO_BINARY_OPERATOR(boolean_or, ||);
        KAIXO_BINARY_OPERATOR(bitwise_and, &);
        KAIXO_BINARY_OPERATOR(bitwise_or, |);
        KAIXO_BINARY_OPERATOR(bitwise_xor, ^);
        KAIXO_BINARY_OPERATOR(spaceship, <=>);
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

#include <functional>


#include <typeindex>
#include <map>
#include <vector>
#include <memory>
#include <iostream>
#include <ranges>
#include <algorithm>

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

template<class> struct is_type_wrapper_impl : std::false_type{};
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
    return [&]<std::size_t ...Is>(std::index_sequence<Is...>) {
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

    [&]<std::size_t ...Is>(std::index_sequence<Is...>) {
        (copy_impl(&std::get<Is>(inst->lambdas), parts, 
            []<class ...Tys>(impl_t* self, Tys... args) ->decltype(auto) {
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

int main() {

    int value = 1;

    {
        auto i = construct<Clickable, 3>(
            [&](Clickable& click) {
                click.a = 10;
            },

            &Clickable::test >> [&](Clickable& self, int v) {
                value += v;
                std::cout << "Test " << value << "\n";
            },

            &Clickable::click >> [&](Clickable& self) {
                std::cout << "Click " << value << "\n";
            },

            &Clickable::test2 >> [&](const Clickable& self) {
                return self.a + value;
            }
        );

        i->click();
        i->test(3);
        i->click();

        i->a = 4;
        auto res = i->test2();
    }
    auto p1 = vtable_index(&Clickable::click);
    auto p2 = vtable_index(&Clickable::test);
    auto p3 = vtable_index(&Clickable::test2);
    
    return 0;

    //struct vtable {
    //
    //};
    //
    //
    //auto instance = Component(
    //    Clickable::click >> []() {}
    //
    //);
    //
    //instance.click = []() { std::cout << "click"; };
    //instance.x = 1;
    //
    //instance.click();

    return 0;
}

 