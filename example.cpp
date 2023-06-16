#include <map>
#include <typeindex>
#include <vector>
#include <memory>
#include <type_traits>
#include <string_view>
#include <string>
#include <ranges>
#include <iostream>

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

#include "list_comprehension.hpp"


using namespace kaixo;
using namespace kaixo::operators;
using namespace kaixo::containers;

int main() {

    constexpr var<"a"> a{};
    constexpr var<"b"> b{};
    constexpr var<"c"> c{};
    constexpr var<"x"> x{};

    //constexpr named_tuple vls{ c = 10 };
    //constexpr named_tuple vls2{ a = 4 };
    //constexpr named_tuple vls3{ x = 4 };

    std::map<int, int> faefa{ { 1, 3 }, { 2, 5 }, { 3, 10 } };

    //auto ffffe = ((a, b) | a <- range(1, 10), a % 2 == 0, b <- range(1, a), a == b);
    
    auto ffffe = ((a) | a <- range(1, 10));
    //auto ffffe = ((a, b) | a <- range(1, 10), b = a * 2);

    auto aoine = ((a, b, c) | c <- range(1, 10), b <- range(1, c), a <- range(1, b), a * a + b * b == c * c);
    
    for (auto [a, b, c] : aoine) {
        std::cout << a << "," << b << "," << c << '\n';
    }
    //decltype(ffffe)::iterator::named_tuple_type::vars::element<1>::type;

    //for (auto [a, b] : ffffe) {
    //    std::cout << a << "," << b << '\n';
    //}

    //for (auto [a, b] : ffffe) {
    //    std::cout << a << "," << b << '\n';
    //}

    //constexpr auto aoine = is_dependent_range<decltype(a < -range(1, 10))>;

    return 0;
}

 