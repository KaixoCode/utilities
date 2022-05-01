#include <string_view>
#include <utility>
#include <algorithm>
#include <charconv>
#include <stdexcept>

#include "utils.hpp"

template <class T, class Tuple, size_t... Is>
constexpr T construct_from_tuple(Tuple& tuple, std::index_sequence<Is...>) {
    return T{ std::get<Is>(tuple)... };
}

template <class T, class Tuple>
constexpr T construct_from_tuple(Tuple& tuple) {
    return construct_from_tuple<T>(tuple,
        std::make_index_sequence<std::tuple_size_v<std::decay_t<Tuple>>>{}
    );
}

// String literal wrapper for template parameter
template<std::size_t N>
struct tag {
    constexpr static auto size = N - 1;
    char value[N - 1];
    constexpr tag(std::string_view view) : value() { std::copy_n(view.data(), N - 1, value); }
    constexpr tag(const char(&val)[N]) : value() { std::copy_n(val, N - 1, value); }
    constexpr operator std::string_view() const { return { value, N - 1 }; }
    constexpr std::string_view str() const { return { value, N - 1 }; }
    constexpr bool operator==(const tag& other) const { return str() == other.str(); }
    template<std::size_t M> requires (M != N)
        constexpr bool operator==(const tag<M>&) const { return false; }
};



template<bool Parsed, tag Remainder, class Ty = std::string_view>
struct result_type {

    using value_type = Ty;

    constexpr static auto parsed = Parsed;
    constexpr static auto remainder = Remainder;
    Ty value{ };
};

struct dud {};
template<class ...Parsers>
struct and_parser_t {
    constexpr static auto size = sizeof...(Parsers);

    template<class Grammar, tag Str>
    using value_type = typename decltype(parse<Grammar, Str>())::template value_type;

    template<std::size_t I>
    using parser = std::tuple_element_t<I, std::tuple<Parsers...>>;

    template<class Grammar, tag Str>
    consteval static auto parse() {
        return parse_i<0, Grammar, Str>();
    }

    template<std::size_t I, class Grammar, tag Str>
    consteval static auto parse_i() {
        if constexpr (I == size) return result_type<true, Str, std::tuple<>>{ {} };
        else {
            constexpr auto res = parser<I>::template parse<Grammar, Str>();
            if constexpr (res.parsed) {
                constexpr auto recu = parse_i<I + 1, Grammar, res.remainder>();
                if constexpr (!recu.parsed) return result_type<false, Str>{};
                else {
                    constexpr auto tuple = std::tuple_cat(std::tuple{ res.value }, recu.value);
                    return result_type<true, recu.remainder, decltype(tuple)>{ std::move(tuple) };
                }
            }
            else return result_type<false, Str>{};
        }
    }
};

template<class ...Parsers>
struct or_parser_t {
    constexpr static auto size = sizeof...(Parsers);

    template<class Grammar, tag Str>
    using value_type = typename decltype(parse<Grammar, Str>())::template value_type;

    template<std::size_t I>
    using parser = std::tuple_element_t<I, std::tuple<Parsers...>>;

    template<class Grammar, tag Str>
    consteval static auto parse() {
        return parse_i<0, Grammar, Str>();
    }

    template<std::size_t I, class Grammar, tag Str>
    consteval static auto parse_i() {
        if constexpr (I == size) return result_type<false, Str>{ };
        else {
            constexpr auto res = parser<I>::template parse<Grammar, Str>();
            if constexpr (res.parsed) return res;
            else return parse_i<I + 1, Grammar, Str>();
        }
    }
};

template<tag Name, class Parser>
struct named_parser {
    constexpr static auto name = Name;
    using parser = Parser;

    template<class Grammar, tag Str>
    using value_type = Parser::template value_type<Grammar, Str>;

    template<class Parser, tag Str>
    consteval static auto parse() {
        return parser::template parse<Parser, Str>();
    }
};

template<tag Name, auto Lambda = 0>
struct token {
    constexpr static auto name = Name;

    template<class Grammar, tag Str>
    using value_type = typename decltype(parse<Grammar, Str>())::value_type;

    template<class Grammar, tag Str>
    consteval static auto parse() {
        if constexpr (std::same_as<int, decltype(Lambda)>) {
            if constexpr (Str.str().starts_with(Name.str())) {
                constexpr auto size = Str.size - Name.size;
                if constexpr (size == 0)
                    return result_type<true, "\0", value_type<Grammar, Str>>{ Name.str() };
                else {
                    constexpr auto new_str = Str.str().substr(Name.size);
                    return result_type < true, tag<size + 1>{ new_str }, value_type<Grammar, Str >> { Name.str() };
                }
            }
            else return result_type<false, Str>{};
        }
        else return Lambda.template operator()<Grammar, Str>();
    }
};

template<tag Name>
struct non_terminal {
    constexpr static auto name = Name;

    template<class Grammar, tag Str>
    using value_type = std::decay_t<decltype(Grammar::template get<Name>)>::template value_type<Grammar, Str>;

    template<class Parser>
    consteval auto operator<<=(Parser) const { return named_parser<Name, Parser>{}; }

    template<class Grammar, tag Str>
    consteval static auto parse() {
        return Grammar::template get<Name>.template parse<Grammar, Str>();
    }
};

template<class Ty, class Parser>
struct typed_parser {
    using type = Ty;
    using parser = Parser;

    template<class Grammar, tag Str>
    using value_type = typename decltype(parse<Grammar, Str>())::template value_type<Grammar, Str>;

    template<class Grammar, tag Str>
    consteval static auto parse() {
        constexpr auto res = Parser::template parse<Grammar, Str>();
        if constexpr (res.parsed)
            return result_type<true, res.remainder, type>{ construct(res.value) };
        else return result_type<false, Str>{};
    }

    template<class Ty>
    consteval static auto construct(const Ty& value) {
        if constexpr (kaixo::specialization<Ty, std::tuple>)
            return construct_from_tuple<type>(value);
        else return type{ value };
    }
};

template<auto ...Parsers>
struct parser {
    template<tag Name>
    static constexpr auto get_impl(auto& P, auto&... Ps) {
        if constexpr (std::decay_t<decltype(P)>::name == Name) return P;
        else return get_impl<Name>(Ps...);
    }

    template<tag Name>
    static constexpr auto get = get_impl<Name>(Parsers...);

    template<tag Name, tag Str>
    consteval static auto parse() {
        return get<Name>.template parse<parser, Str>();
    }
};

template<class A, class B> requires (!kaixo::specialization<A, and_parser_t> && !kaixo::specialization<B, and_parser_t>)
consteval auto operator*(A, B) { return and_parser_t<A, B>{}; }
template<class A, class ...Bs> requires (!kaixo::specialization<A, and_parser_t>)
consteval auto operator*(and_parser_t<Bs...>, A) { return and_parser_t<Bs..., A>{}; }
template<class A, class ...Bs> requires (!kaixo::specialization<A, and_parser_t>)
consteval auto operator*(A, and_parser_t<Bs...>) { return and_parser_t<A, Bs...>{}; }

template<class A, class B> requires (!kaixo::specialization<A, or_parser_t> && !kaixo::specialization<B, or_parser_t>)
consteval auto operator|(A, B) { return or_parser_t<A, B>{}; }
template<class A, class ...Bs> requires (!kaixo::specialization<A, or_parser_t>)
consteval auto operator|(or_parser_t<Bs...>, A) { return or_parser_t<Bs..., A>{}; }
template<class A, class ...Bs> requires (!kaixo::specialization<A, or_parser_t>)
consteval auto operator|(A, or_parser_t<Bs...>) { return or_parser_t<A, Bs...>{}; }

template<class Ty>
struct type_t {
    using type = Ty;
};

template<class Ty>
constexpr auto t = type_t<Ty>{};

template<class A, class B>
consteval auto operator^(type_t<A>, B) {
    return typed_parser<A, B>{};
}


#include <vector>
#include <iostream>

#include <numbers>

namespace kaixo {
    enum class AngleType { Degrees, Radians };

    namespace detail {
        using namespace std::numbers;
        template<AngleType A, AngleType B, class Out, class In>
        constexpr decltype(auto) convert(const In& v) { 
            if constexpr (A == B) {
                if constexpr (std::same_as<std::decay_t<In>, std::decay_t<Out>>) return v;
                else return static_cast<Out>(v);
            } 
            else if (A == AngleType::Radians) return static_cast<Out>(180. * v / pi_v<double>);
            else if (A == AngleType::Degrees) return static_cast<Out>(pi_v<double> * v / 180.);
        }
    }

    template<class Ty, AngleType Vy = AngleType::Radians> 
    class AngleBase {
    public:
        constexpr AngleBase() {}
        template<std::convertible_to<Ty> T>
        constexpr AngleBase(const T& v) : m_Value(static_cast<Ty>(v)) {}
        template<std::convertible_to<Ty> T, AngleType V>
        constexpr AngleBase(AngleBase<T, V> v) : m_Value(detail::convert<V, Vy, Ty>(v.value())) {}

        constexpr decltype(auto) degrees() { return detail::convert<Vy, AngleType::Degrees, Ty>(value()); }
        constexpr decltype(auto) radians() { return detail::convert<Vy, AngleType::Radians, Ty>(value()); }
        constexpr decltype(auto) degrees() const { return detail::convert<Vy, AngleType::Degrees, Ty>(value()); }
        constexpr decltype(auto) radians() const { return detail::convert<Vy, AngleType::Radians, Ty>(value()); }

        constexpr Ty& value() { return m_Value; }
        constexpr Ty const& value() const { return m_Value; }

        constexpr explicit operator AngleBase<Ty, AngleType::Degrees>() const { return degrees(); }
        constexpr explicit operator AngleBase<Ty, AngleType::Radians>() const { return radians(); }

        constexpr explicit operator Ty& () { return value(); }
        constexpr explicit operator Ty const& () const { return value(); }

    protected:
        Ty m_Value{};
    };

    template<class Ty> using Degrees = AngleBase<Ty, AngleType::Degrees>;
    template<class Ty> using Radians = AngleBase<Ty, AngleType::Radians>;
}


template<class Ty>
concept range_type = requires (Ty a, Ty b) {
    { a == b } -> std::convertible_to<bool>;
    { ++a } -> std::convertible_to<Ty>;
    { a + b } -> std::convertible_to<Ty>;
    { a = a + b } -> std::convertible_to<Ty>;
};

template<range_type Ty>
struct range {
    using value_type = Ty;
    using reference = Ty&;
    using const_reference = const Ty&;
    using difference_type = std::ptrdiff_t;
    using size_type = std::size_t;

    class iterator {
    public:
        using value_type = Ty;
        using reference = Ty&;
        using const_reference = const Ty&;
        using difference_type = std::ptrdiff_t;
        using size_type = std::size_t;

        constexpr iterator(Ty value, const range* r) : m_Value(value), m_Range(r) {}

        constexpr iterator& operator++() { m_Value = m_Value + m_Range->difference; return *this; }
        constexpr Ty const& operator*() const { return m_Value; }
        constexpr bool operator==(const iterator& other) const { return other.m_Value == m_Value; }

    private:
        Ty m_Value;
        const range* m_Range;
    };

    using const_iterator = iterator;

    constexpr iterator begin() const { return { a, this }; }
    constexpr iterator end() const { return { b, this }; }

    Ty a{};
    Ty b{};
    Ty difference{ [] { Ty v{}; return ++v; }() };
};

template<class Ty> range(Ty, Ty)->range<Ty>;
template<class Ty> range(Ty, Ty, Ty)->range<Ty>;

#include <map>

struct State {
    void* data{ nullptr };

    template<class Ty> void assign() {
        if constexpr (sizeof(Ty) <= sizeof data)
            new (&data) Ty{};
        else data = new Ty;
    }

    template<class Ty> Ty const& get() const {
        if constexpr (sizeof(Ty) <= sizeof data)
            return *static_cast<Ty*>((void*)&data);
        else return *static_cast<Ty*>(data);
    }

    template<class Ty> Ty& get() {
        if constexpr (sizeof(Ty) <= sizeof data)
            return *static_cast<Ty*>((void*)&data);
        else return *static_cast<Ty*>(data);
    }

    template<class Ty> void clean() {
        delete static_cast<Ty*>(data); 
    }
};

template<class Ty>
struct StateId {
    std::string_view id;
    constexpr const char* get() const { return id.data(); }
    constexpr bool operator==(std::string_view view) const {
        return id.data() == view.data();
    }
    constexpr friend bool operator==(std::string_view view, const StateId& id) {
        return id.id.data() == view.data();
    }
};

struct StateListener {
    virtual void update(std::string_view id, State& state) = 0;
};

class Object {
public:
    template<class Ty>
    Ty get(const StateId<Ty>& id) const {
        if (!m_States.contains(id.get()))
            return {};
        auto const& _state = m_States.at(id.get());
        return _state.get<Ty>();
    }

    template<class Ty, std::convertible_to<Ty> T>
    Ty set(const StateId<Ty>& id, T&& val) {
        if (!m_States.contains(id.get()))
            m_States[id.get()].assign<Ty>();
        auto& _state = m_States[id.get()];
        _state.get<Ty>() = std::forward<T>(val);
        for (auto& i : m_Listeners)
            i->update(id.id, _state);
        return _state.get<Ty>();
    }

    template<std::derived_from<StateListener> Ty>
    void link(Ty& l) {
        m_Listeners.push_back(dynamic_cast<StateListener*>(&l));
    }

private:
    std::map<const char*, State> m_States{};
    std::vector<StateListener*> m_Listeners;
};

constexpr StateId<bool> Hovering{ "Hovering"};
constexpr StateId<bool> Focused{ "Focused" };
constexpr StateId<int> Pressed{ "Pressed" };

struct MyObject : public Object, public StateListener {

    MyObject() {
        link(*this);
    }

    void update(std::string_view id, State& state) override {

        if (Hovering == id) {
            std::cout << state.get<bool>() << "\n";
        }

        return;
    }

};

template<class Ty> struct delegate;

template<class R, class ...A>
struct delegate<R(A...)> {
    
    template<class Obj>
    constexpr delegate(Obj& o, R(Obj::*fun)(A...)) 
        : object_ptr(static_cast<void*>(&o)), 
            fun_ptr(mem_method<Obj>) {
        new (&storage) mem_storage<Obj>{ fun };
    }

    template<std::invocable<A...> Lambda> 
        requires (sizeof(Lambda) <= sizeof(void*) 
        && !std::convertible_to<Lambda, R(*)(A...)>)
    constexpr delegate(Lambda lambda)
        : fun_ptr(small_lambda_method<Lambda>) {
        new (&object_ptr) Lambda{ std::move(lambda) };
    }
    
    template<std::invocable<A...> Lambda> 
        requires (sizeof(Lambda) > sizeof(void*) 
        && !std::convertible_to<Lambda, R(*)(A...)>)
    constexpr delegate(Lambda lambda)
        : fun_ptr(big_lambda_method<Lambda>), 
            cleanup(cleanup_method<Lambda>) {
        object_ptr = new Lambda{ std::move(lambda) };
    }

    template<std::invocable<A...> Lambda> 
        requires (std::convertible_to<Lambda, R(*)(A...)>)
    constexpr delegate(Lambda lambda)
        : fun_ptr(fun_ptr_method) {
        new (&storage) (R(*)(A...)) { (R(*)(A...)) lambda };
    }

    template<class Obj>
    constexpr delegate& operator=(std::pair<Obj&, R(Obj::*)(A...)> a) {
        clean();
        object_ptr = static_cast<void*>(&a.first);
        new (&storage) mem_storage<Obj>{ a.second };
        fun_ptr = mem_method<Obj>;
        return *this;
    }

    template<std::invocable<A...> Lambda>
        requires (sizeof(Lambda) <= sizeof(void*)
        && !std::convertible_to<Lambda, R(*)(A...)>)
    constexpr delegate& operator=(Lambda lambda) {
        clean();
        fun_ptr = small_lambda_method<Lambda>;
        new (&object_ptr) Lambda{ std::move(lambda) };
        return *this;
    }

    template<std::invocable<A...> Lambda>
        requires (sizeof(Lambda) > sizeof(void*)
        && !std::convertible_to<Lambda, R(*)(A...)>)
    constexpr delegate& operator=(Lambda lambda) {
        clean();
        fun_ptr = big_lambda_method<Lambda>;
        object_ptr = new Lambda{ std::move(lambda) };
        cleanup = cleanup_method<Lambda>;
        return *this;
    }

    template<std::invocable<A...> Lambda>
        requires (std::convertible_to<Lambda, R(*)(A...)>)
    constexpr delegate& operator=(Lambda lambda) {
        clean();
        fun_ptr = fun_ptr_method;
        new (&storage) (R(*)(A...)){ (R(*)(A...)) lambda };
        return *this;
    }

    template<std::convertible_to<A> ...Args>
    constexpr R operator()(Args...args) {
        return (*fun_ptr)(object_ptr, storage, args...);
    }

    constexpr ~delegate() { clean(); }

private:
    template<class Obj>
    struct mem_storage {
        R(Obj::* fun)(A...);
    };
    struct dummy {};
    constexpr static auto mem_fun_size = sizeof(mem_storage<dummy>);

    void* object_ptr = nullptr;
    R(*fun_ptr)(void*, uint8_t(&)[mem_fun_size], A...);
    uint8_t storage[mem_fun_size]{};
    void(*cleanup)(void*) = nullptr;

    constexpr void clean() {
        if (cleanup) {
            cleanup(object_ptr);
            cleanup = nullptr;
        }
    }

    template<class Obj> 
    constexpr static void cleanup_method(void* obj) {
        delete static_cast<Obj*>(obj);
    }

    template<class Obj>
    constexpr static R mem_method(void* o, uint8_t(&fun)[mem_fun_size], A...args) {
        return (static_cast<Obj*>(o)->*reinterpret_cast<mem_storage<Obj>*>(&fun)->fun)(args...);
    }

    template<class Lambda>
    constexpr static R small_lambda_method(void* o, uint8_t(&fun)[mem_fun_size], A...args) {
        return (*reinterpret_cast<Lambda*>(&o))(args...);
    }
    
    template<class Lambda>
    constexpr static R big_lambda_method(void* o, uint8_t(&fun)[mem_fun_size], A...args) {
        return (*static_cast<Lambda*>(o))(args...);
    }

    constexpr static R fun_ptr_method(void* o, uint8_t(&fun)[mem_fun_size], A...args) {
        return (*reinterpret_cast<R(**)(A...)>(&fun))(args...);
    }
};

struct Type {
    int thing(int a) {
        std::cout << a << '\n';
        return a;
    }
};

struct Functor {
    int operator()(int a) { return a + 1; }
};
struct Functor2 {
    int m1 = 10;
    int m2 = 20;
    int m3 = 30;
    int operator()(int a) { return a + m1 + m2 + m3; }
};

#include <functional>


class no_template {
    using L = decltype([]<class Ty>(Ty arg) {});
public:

    template<class Ty> 
    constexpr no_template(Ty)
        : saved_type(save_type<Ty>) {}

    template<class Ty> 
    constexpr void operator=(Ty) {
        saved_type = save_type<Ty>;
    }

    void get(auto l) {
        lambda = *reinterpret_cast<L*>(&l);
        saved_type(this);
    }

private:

    void(*saved_type)(no_template*);
    L lambda{};

    template<class Ty>
    static void save_type(no_template* me) {
        me->lambda(Ty{});
    }
};


template<class Lambda>
struct pa_delegate {
    Lambda lambda;

    template<class Ty>
    void link(Ty arg1) {
        saved_type = save_type<Ty>;
    }

    void call() { saved_type(this); }

    void(*saved_type)(pa_delegate*);

    template<class Ty>
    static void save_type(pa_delegate* me) {
        me->lambda(Ty{});
    }
};


namespace kaixo {
    template<size_t N>
    struct incrementer {
        friend constexpr auto magic_incr(incrementer<N>);
    };

    template<size_t N, size_t V>
    struct incrementer_def {
        friend constexpr auto magic_incr(incrementer<N>) { return V; };
    };

    template<size_t N, class, auto = []{}>
    concept checker_c = requires() {
        magic_incr(incrementer<N>{});
    };

    template<size_t N, class T>
    struct checker : std::bool_constant<checker_c<N, T, []{}> && (sizeof(incrementer_def<N, N + 1>), true)> {};

    template<size_t, class>
    struct incr;

    template<size_t V, class T> requires (!checker<V, T>::value)
        struct incr<V, T> {
        constexpr static size_t get() { return V; }
    };

    template<size_t V, class T> requires (checker<V, T>::value)
        struct incr<V, T> : incr<V + 1, T> {
        using incr<V + 1, T>::get;
    };
}

#include "type_linker.hpp"

template<std::size_t I = 10>
struct my_any {
    std::size_t stored_type = static_cast<std::size_t>(-1);

    template<class Ty>
    constexpr static std::size_t link() {
        constexpr auto index = kaixo::incr<0, Ty>::get();
        constexpr bool link = kaixo::link_types<kaixo::type_group<Ty>, 
            kaixo::type_group<kaixo::number<index>>>;
        return index;
    }

    template<class Ty>
    void operator=(Ty) { 
        constexpr auto index = link<Ty>();
        stored_type = index;
    }

    template<std::size_t I, class Fun, auto = []{} >
    constexpr static auto run_one(std::size_t i, Fun fun) {
        constexpr bool v = kaixo::checker_c<I, void, []{}>;
        if constexpr (v) {
            if (i == I) fun(std::tuple_element_t<0,
                typename kaixo::template linked_types<kaixo::number<I>>::types>{});
        }
    };

    constexpr void visit(auto fun) {
        []<std::size_t ...Is>
            (std::size_t i, auto fun, std::index_sequence<Is...>) {
            ((run_one<Is, decltype(fun), []{}>(i, fun)), ...);
        }(stored_type, fun, std::make_index_sequence<I>{});
    }
};

struct my_struct {

};

int main() {
    my_any any;
    any = 1;
    
    any.visit([](auto v) {
        std::cout << typeid(v).name() << '\n';
    });

    any = 1.;

    any.visit([](auto v) {
        std::cout << typeid(v).name() << '\n';
    });

    any = my_struct{};

    any.visit([](auto v) {
        std::cout << typeid(v).name() << '\n';
    });


    Type thing;
    delegate<int(int)> d{ thing, &Type::thing };
    int res = d(1.f);

    constexpr auto aeff = sizeof(delegate<int(int)>);
    constexpr auto girn = sizeof(std::function<int(int)>);

    d = [](int a) { return a + 1; };
    res = d(1);
    
    int a1 = 10;
    int a2 = 20;
    d = [a1, a2](int a) { return a + a1 + a2; };
    res = d(1);

    int b1 = 10;
    int b2 = 20;
    int b3 = 30;
    d = [b1, b2, b3](int a) { return a + b1 + b2 + b3; };
    res = d(1);

    d = Functor{};
    res = d(1);
    
    d = Functor2{};
    res = d(1);

    MyObject obj;
    obj.set(Hovering, true);
    bool v = obj.get(Hovering);
    obj.set(Hovering, false);

    for (auto& v : range(0, 10, 3)) {
        std::cout << v;
    }

    return 0;
}