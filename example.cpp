#include <map>
#include <typeindex>
#include <vector>
#include <memory>
#include <type_traits>
#include <string_view>
#include <string>
#include <ranges>
#include <iostream>
#include <tuple>

//namespace aaaa {
//
//    struct event {
//        virtual ~event() = default;
//    };
//
//    struct event_listener {
//        struct handler { virtual void handle(event& event) = 0; };
//        template<class> struct typed;
//        template<class C, class A> struct typed<void (C::*) (A) const> : C, handler {
//            using event_type = std::decay_t<A>;
//            template<class Q> typed(Q&& q) : C(std::forward<Q>(q)) {}
//            void handle(event& e) override { (*this)(dynamic_cast<event_type&>(e)); }
//        };
//
//        template<class Lambda> void operator+=(Lambda&& lambda) {
//            using handler = typed<decltype(&Lambda::operator())>;
//            auto _handler = std::make_unique<handler>(std::forward<Lambda>(lambda));
//            handlers[typeid(handler::event_type)].push_back(std::move(_handler));
//        }
//
//        void handle(std::derived_from<event> auto& e) {
//            for (auto& handler : handlers[typeid(e)]) handler->handle(e);
//        }
//
//        std::map<std::type_index, std::vector<std::unique_ptr<handler>>> handlers;
//    };
//
//    struct mouse_pressed : event {
//        int x{};
//        int y{};
//    };
//
//    struct key_pressed : event {
//        int key{};
//    };
//
//
//    template<class Ty>
//    struct type_wrapper_t {
//        using type = Ty;
//
//        template<class ...Args>
//        constexpr Ty operator()(Args&& ...args) const {
//            return Ty{ std::forward<Args>(args)... };
//        }
//    };
//
//    template<class> struct is_type_wrapper_impl : std::false_type {};
//    template<class Ty> struct is_type_wrapper_impl<type_wrapper_t<Ty>> : std::true_type {};
//    template<class Ty> concept is_type_wrapper = is_type_wrapper_impl<Ty>::value;
//
//    template<class Ty> constexpr type_wrapper_t<Ty> is{};
//    template<class Ty> constexpr type_wrapper_t<Ty> has{};
//
//    template<class A, class B>
//    constexpr auto operator and(type_wrapper_t<A>, type_wrapper_t<B>) {
//        struct impl : A, B {};
//        return type_wrapper_t<impl>{};
//    }
//
//    template<class> struct mptr_class;
//    template<class R, class C, class ...Args>
//    struct mptr_class<R(C::*)(Args...)> : std::type_identity<C> {};
//    template<class R, class C, class ...Args>
//    struct mptr_class<R(C::*)(Args...) const> : std::type_identity<const C> {};
//
//    template<class, class> struct mptr_sign;
//    template<class T, class R, class C, class ...Args>
//    struct mptr_sign<T, R(C::*)(Args...)> : std::type_identity<R(*)(T*, Args...)> {};
//    template<class T, class R, class C, class ...Args>
//    struct mptr_sign<T, R(C::*)(Args...) const> : std::type_identity<R(*)(T*, Args...)> {};
//
//    template<std::size_t N> constexpr std::size_t _unit() { return N; }
//    template<std::size_t N = 10>
//    constexpr std::size_t vtable_index(auto fptr) {
//        using type = std::decay_t<typename mptr_class<decltype(fptr)>::type>;
//        auto ptr = reinterpret_cast<std::size_t(type::*)(void)>(fptr);
//        return[&]<std::size_t ...Is>(std::index_sequence<Is...>) {
//            constexpr std::size_t(*indexer[])(void) { _unit<Is>... };
//            const void* vtable = &indexer;
//            return (reinterpret_cast<type*>(&vtable)->*ptr)();
//        }(std::make_index_sequence<N>{});
//    }
//
//    template<class FptrT, class Lambda>
//    struct virtual_impl {
//        using type = mptr_class<FptrT>::type;
//        using lambda_type = std::decay_t<Lambda>;
//        using fptr_type = FptrT;
//        FptrT fptr;
//        Lambda lambda;
//    };
//
//    template<class Ty, class Lambda>
//    constexpr auto operator >>(Ty fun, Lambda&& l) {
//        return virtual_impl{ fun, std::forward<Lambda>(l) };
//    }
//
//    template<class Type, std::size_t N = 10, class ...Args>
//    constexpr auto construct(auto construct, Args&&... parts) {
//        struct impl_t : Type {
//            void* _vtable[N]{};
//            std::tuple<typename std::decay_t<Args>::lambda_type...> lambdas;
//        };
//
//        void* allocated = new char[sizeof(impl_t)];
//        impl_t* inst = reinterpret_cast<impl_t*>(allocated);
//        reinterpret_cast<void**>(allocated)[0] = &inst->_vtable;
//        construct(*inst);
//
//        auto copy_impl = [&](auto* ptr, auto& impl, auto call) {
//            using arg = std::decay_t<decltype(impl)>;
//            using signature = mptr_sign<impl_t, typename arg::fptr_type>::type;;
//            new (ptr) arg::lambda_type(impl.lambda);
//            inst->_vtable[vtable_index<N>(impl.fptr)] = (signature)(call);
//        };
//
//        [&] <std::size_t ...Is>(std::index_sequence<Is...>) {
//            (copy_impl(&std::get<Is>(inst->lambdas), parts,
//                []<class ...Tys>(impl_t * self, Tys... args) ->decltype(auto) {
//                using type = std::tuple_element_t<Is, std::tuple<Args...>>::type;
//                return std::get<Is>(self->lambdas)(*(type*)self, args...);
//            }), ...);
//        }(std::index_sequence_for<Args...>{});
//
//        return std::unique_ptr<impl_t>(inst);
//    }
//
//    struct Clickable {
//        virtual void click() = 0;
//        virtual void test(int) = 0;
//        virtual int test2() const = 0;
//
//        int a = 1;
//    };
//
//    struct Test {
//        virtual void test() = 0;
//    };
//
//    template<std::integral Ty, Ty ...Is>
//    struct sequence {
//        constexpr static std::size_t size = sizeof...(Is);
//
//    };
//
//    template<std::size_t ...Is>
//    using index_sequence = sequence<std::size_t, Is...>;
//
//    template<class Ty, Ty ...Is>
//    struct make_sequence;
//
//    template<class Ty, Ty N> requires (N > 1)
//        struct make_sequence<Ty, N> : make_sequence<Ty, N - 2, N - 1> {};
//
//    template<class Ty, Ty N> requires (N == 1)
//        struct make_sequence<Ty, N> : sequence<Ty, 0> {};
//
//    template<class Ty, Ty N> requires (N == 0)
//        struct make_sequence<Ty, N> : sequence<Ty> {};
//
//    template<class Ty, Ty N, Ty ...Is> requires (N != 0)
//        struct make_sequence<Ty, N, Is...> : make_sequence<Ty, N - 1, N, Is...> {};
//
//    template<class Ty, Ty N, Ty ...Is> requires (N == 0)
//        struct make_sequence<Ty, N, Is...> : sequence<Ty, N, Is...> {};
//
//    template<auto Fun, std::size_t N, class Ty, Ty ...Is>
//    struct make_complex_sequence;
//
//    template<auto Fun, std::size_t N, class Ty, Ty ...Is> requires (N != 1)
//        struct make_complex_sequence<Fun, N, Ty, Is...> : make_complex_sequence<Fun, N - 1, Ty, Fun(N - 1), Is...> {};
//
//    template<auto Fun, std::size_t N, class Ty, Ty ...Is> requires (N == 1)
//        struct make_complex_sequence<Fun, N, Ty, Is...> : sequence<Ty, Fun(N - 1), Is...> {};
//
//    template<auto Fun, std::size_t N, class Ty> requires (N == 0)
//        struct make_complex_sequence<Fun, N, Ty> : sequence<Ty> {};
//
//    template<auto Fun, std::size_t N>
//    using make_complex_index_sequence = make_complex_sequence<Fun, N, std::size_t>;
//
//    template<std::size_t N>
//    using make_index_sequence = make_sequence<std::size_t, N>;
//
//    constexpr double constexpr_pow(double v, std::size_t p) {
//        if (p == 0) return 1;
//        else if (p % 2 == 1) return constexpr_pow(v, p / 2ull) * constexpr_pow(v, p / 2ull) * v;
//        else return constexpr_pow(v, p / 2ull) * constexpr_pow(v, p / 2ull);
//    }
//
//    constexpr std::size_t fib(std::size_t n) {
//        return 0.5 + (constexpr_pow(1.6180339, n) - constexpr_pow(-0.6180339, n)) / 2.236067977;
//    }
//
//    template<std::size_t ...Is>
//    constexpr std::size_t tests(index_sequence<Is...>) {
//        return (Is + ...);
//    }
//
//
//    struct typeless_vector {
//        struct value_data {
//            std::size_t size = 0;
//            std::size_t offset = 0;
//            const std::type_info* type;
//        };
//
//        struct value_type {
//            typeless_vector* self;
//            value_data* data;
//
//            template<class Ty>
//            constexpr bool is() const {
//                return (*data->type) == typeid(Ty);
//            }
//
//            template<class Ty>
//            constexpr operator Ty& () const {
//                return *reinterpret_cast<Ty*>(&self->m_Data[data->offset]);
//            }
//        };
//
//        template<class Ty, class ...Args>
//        Ty& emplace_back(Args&&...args) {
//            m_Info.emplace_back(value_data{
//                .size = sizeof(Ty),
//                .offset = offset_back(),
//                .type = &typeid(Ty)
//                });
//            auto it = m_Data.insert(m_Data.end(), sizeof(Ty), {});
//            return *new (&*it) Ty{ std::forward<Args>(args)... };
//        }
//
//        value_type operator[](std::size_t i) {
//            return value_type{ this, &m_Info[i] };
//        }
//
//
//    private:
//        std::vector<std::byte> m_Data{};
//        std::vector<value_data> m_Info{};
//
//        constexpr std::size_t offset(std::size_t i) {
//            return m_Info[i].offset;
//        }
//
//        constexpr std::size_t offset_back() {
//            if (m_Info.size() == 0) return 0;
//            else return m_Info.back().offset + m_Info.back().size;
//        }
//    };
//
//}

#include "kaixo/list_comprehension.hpp"
#include "kaixo/overloads.hpp"
#include "kaixo/range.hpp"
#include "kaixo/zipped_range.hpp"
#include "kaixo/break.hpp"

using namespace kaixo;
using namespace kaixo::operators;
using namespace kaixo::overloads;

struct Test {
    int x;
    int y;

    Test() { std::cout << "Construct\n"; }
    Test(const Test&) { std::cout << "Copy\n"; }
    Test(Test&&) { std::cout << "Move\n"; }

    Test& operator=(const Test&) { std::cout << "Copy Assign\n"; return *this; }
    Test& operator=(Test&&) { std::cout << "Move Assign\n"; return *this; }

    const Test& operator+(const Test& other) const { return other; }
};

constexpr void tests() {
    using namespace kaixo;
    using namespace kaixo::operators;
    using namespace kaixo::overloads;
    using namespace kaixo::default_variables;

    constexpr std::array values1{ 1, 2, 3, 4 };
    std::array values2{ 1, 2, 3, 4 };

    constexpr auto nested1 = std::array{
        std::array{ std::tuple{ 1, 2.3 }, std::tuple{ 4, 4.6 } },
        std::array{ std::tuple{ 4, 5.2 }, std::tuple{ 8, 1.9 } },
        std::array{ std::tuple{ 2, 6.1 }, std::tuple{ 9, 7.7 } },
    };

    constexpr auto nested2 = std::array{
        std::tuple<const int, int>{ 1, 2 },
        std::tuple<const int, int>{ 3, 5 },
    };

    auto nested3 = std::array{
        std::tuple<const int, int>{ 1, 2 },
        std::tuple<const int, int>{ 3, 5 },
    };

    // Reference
    constexpr auto lc1 = (a | a <- values1);
    using lc1_t = decay_t<decltype(lc1)>;
    static_assert(lc1[2] == 3);
    static_assert(same_as<lc1_t::reference, const int&>);

    // Zipped range
    constexpr auto lc2 = ((a, b) | (a, b) <- (values1, range(0, inf)));
    using lc2_t = decay_t<decltype(lc2)>;
    static_assert(lc2[2] == std::tuple{ 3, 2 });
    static_assert(lc2[3] == std::tuple{ 4, 3 });
    static_assert(same_as<lc2_t::reference, std::tuple<const int&, int>>);

    // Nested container
    constexpr auto lc3 = ((a, b) | c <- nested1, (a, b) <- c);
    using lc3_t = decay_t<decltype(lc3)>;
    static_assert(lc3[2] == std::tuple{ 4, 5.2 });
    static_assert(lc3[5] == std::tuple{ 9, 7.7 });
    static_assert(same_as<lc3_t::reference, std::tuple<const int&, const double&>>);

    // References in deconstructed const
    constexpr auto lc4 = ((a, b) | (a, b) <- nested2);
    using lc4_t = decay_t<decltype(lc4)>;
    static_assert(same_as<lc4_t::reference, std::tuple<const int&, const int&>>);

    // References in zipped range const
    constexpr auto lc5 = ((a, b) | ((a, b), c) <- (nested2, range(0, inf)));
    using lc5_t = decay_t<decltype(lc5)>;
    static_assert(same_as<lc5_t::reference, std::tuple<const int&, const int&>>);
    static_assert(lc5[0] == std::tuple{ 1, 2 });

    // References in zipped range
    auto lc6 = ((a, b) | ((a, b), c) <- (nested3, range(0, inf)));
    using lc6_t = decay_t<decltype(lc6)>;
    static_assert(same_as<lc6_t::reference, std::tuple<const int&, int&>>);

    // Reference non-const
    auto lc7 = (a | a <- values2);
    using lc7_t = decay_t<decltype(lc7)>;
    static_assert(same_as<lc7_t::reference, int&>);

    // Constraints
    constexpr auto lc8 = (a | a <- values1, a % 2 == 0);

    // Break expression
    constexpr auto lc9 = (a | a <- range(0, inf), brk = a == 10);

    // Variable assignments
    constexpr auto lc10 = (a | b <- nested1, c = b, (a, d) <- c);
    using lc10_t = decay_t<decltype(lc10)>;
    static_assert(same_as<lc10_t::reference, const int&>);
}

#include <set>

struct Testaefa {
    int a;
    std::tuple<int> b;
};



struct aegina {
    int a;
    std::tuple<int> c;
    int f;
};


using a01 = copy_const_t<int, const int&>;
using a02 = copy_volatile_t<int, volatile int&>;
using a03 = copy_cv_t<int, const int&>;
using a04 = copy_ref_t<int, int&&>;
using a05 = copy_cvref_t<int, const int&>;
using a06 = add_const_t<int>;
using a07 = add_volatile_t<int>;
using a08 = add_cv_t<int>;
using a09 = add_ref_t<int, int&>;
using a10 = add_cvref_t<int, const int&>;
using a11 = add_lvalue_reference_t<int>;
using a12 = add_rvalue_reference_t<int>;
using a13 = add_pointer_t<int>;
using a14 = remove_const_t<int>;
using a15 = remove_volatile_t<int>;
using a16 = remove_cv_t<int>;
using a17 = remove_reference_t<int>;
using a18 = remove_cvref_t<int>;
using a19 = remove_pointer_t<int>;
using a20 = decay_t<int>;


int main() {
    using namespace kaixo::default_variables;
    using kaixo::overloads::empty;

    struct_size<aegina>::value;

    //auto sroing = (x | x <- range(2, inf), y = sqrt(x), empty((a | a <- range(0, x))));

    //auto primes = (x | x <- range(2, inf), empty((a | a <- range(2, sqrt(x)), x % a == 0)));
    //
    //std::cout << primes[999999];
    //
    //return 0;
    //
    //std::vector<int> values;
    //values.reserve(100000);
    //auto lc = (x | x <- range(2, inf),       // Infinite range
    //               y = sqrt(x),              // Precalculate sqrt(x)
    //               empty(                    // Check not divisible by previous primes
    //                     (a | a <- values,   // Go through all previous primes
    //                          x % a == 0,    // Check divisibility
    //                          brk = a > y)), // Break at sqrt of number
    //               values << x);             // Add resulting value to vector
    //
    //for (auto v : lc) { if (values.size() == 100000) break; }
    //
    //std::cout << values.back();

    //auto val2524s = std::array{
    //    std::array{ std::tuple{ 1, 2.3 }, std::tuple{ 4, 4.6 } },
    //    std::array{ std::tuple{ 4, 5.2 }, std::tuple{ 8, 1.9 } },
    //    std::array{ std::tuple{ 2, 6.1 }, std::tuple{ 9, 7.7 } },
    //};
    //
    //auto ggg = (b + c | a <- val2524s, (b, c) <- a);
    //
    //for (auto val : ggg) {
    //    std::cout << val << '\n';
    //}
    //
    //std::vector<int> values{};
    //
    //auto efoaie = (a | a <- range(0, 10), b = std::ref(values), back_inserter(b));
    //
    //auto lcaa = ((a, b, c) | a <- range(1, 20), b <- range(a, 20), c <- range(b, 20), a * a + b * b == c * c);
    //
    //for (auto [a_val, b_val, c_val] : lcaa) {
    //    std::cout << "(" << a_val << ", " << b_val << ", " << c_val << ")" << std::endl;
    //}
    //
    //
    //auto iogngr = ((a, b) | c <- range(1, 5), (a, b) <- (range(0, c), range(c, 2 * c)));
    //
    //for (auto [a, b] : iogngr) {
    //    std::cout << a << "," << b << '\n';
    //}
    //
    //auto aonef = ((a | a <- range(b, c - 2)) | (b, c) <- (range(1, 10), range(5, 15)));
    //
    //for (auto p : aonef) {
    //    for (auto a : p) {
    //        std::cout << a;
    //    }
    //    std::cout << "\n";
    //}
    //
    //const std::vector<Test> aaaff{ 1ull };
    //
    //auto fsefes = (a + a) | a <- aaaff;
    //
    //for (auto& eafa : fsefes) {
    //    std::cout << "-\n";
    //}
    //
    //auto onef = ((c, d) | a <- (range(1, 10), range(1, 10)));
    //
    //named_tuple afea{ b = 10 };
    //
    //std::vector<std::string> strings{ 
    //    "a", "hello", "apples", "test", "world", "carrot"
    //};
    //
    //auto filtered = (a | (a, b) <- (strings, range(1, 6)), size(a) == b);
    //
    //for (auto& str : filtered) {
    //    str += "a";
    //    std::cout << str << '\n';
    //}
    //
    //auto vals = ((c | a <- range(0, b)) | b <- range(0, 9), b % 2 == 0, c = 10);
    //
    //auto oaine = -(a + 1);
    //
    //for (auto r : vals) {
    //    for (auto a : r) {
    //        std::cout << a;
    //    }
    //    std::cout << '\n';
    //}
    //
    //auto triangles = ((a, b, c) | c <- range(1, inf), 
    //                              b <- range(1, c), 
    //                              a <- range(1, b),  
    //                              pow(a, 2) + pow(b, 2) == pow(c, 2));
    //
    //for (auto [a, b, c] : triangles) {
    //    std::cout << "[" << a << "," << b << "," << c << "]\n";
    //}
    //
    //std::vector<Test> tests{ 1ull };
    //std::map<int, int> mapvals{ { 1, 1 }, { 2, 2 }, { 3, 4 } };
    //
    //auto lc = (a, b, c) | (a, (b, c)) <- (tests, mapvals);
    //
    //for (auto [a, b, c] : lc) {
    //    std::cout << " === iter === ";
    //}
    //
    //auto aoing = (a, b, c) | (c, (a, b)) <- (range(1, 10), mapvals);
    //
    //for (auto [a, b, c] : aoing) {
    //    std::cout << a << "," << b << "," << c << '\n';
    //}
    //
    //auto eofin = (a, b, c) | (a, b, c) <- (range(1, 10), range(1, inf), range(1, 20));
    //
    //for (auto [a, b, c] : eofin) {
    //    std::cout << a << "," << b << "," << c << '\n';
    //}
    //
    //named_tuple ff{ b = 1 };
    //
    //auto oe = (a | a <- range(1, 10), b == 1);
    //auto fin = oe.evaluate(ff);
    //
    //auto rgonig = ((a | a <- range(0, b)) | b <- range(1, 10));
    //
    //for (auto r : rgonig) {
    //    for (auto a : r) {
    //        std::cout << a << ',';
    //    }
    //    std::cout << '\n';
    //}
    //
    //
    //auto ognrga = ((a, b) | a <- range(0, 10), b <- (c | c <- range(0, a)));
    //
    //for (auto [a, b] : ognrga) {
    //    std::cout << a << "," << b << '\n';
    //}
    //
    //auto roign = (a | a <- range(0, inf), b = a * 2, brk = b > a);
    //
    //for (auto a : roign) {
    //    std::cout << a << '\n';
    //}
    //
    //auto ffffe = ((a, b) | a <- range(0, 10), b <- range(0, a));
    //
    //
    //for (auto [a, b] : ffffe) {
    //    std::cout << a << "," << b << '\n';
    //}

    return 0;
}

 