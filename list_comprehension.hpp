#include <vector>
#include <deque>
#include <forward_list>
#include <set>
#include <map>
#include <list>
#include <unordered_set>
#include <unordered_map>
#include <stack>
#include <queue>
#include <span>
#include <functional>
#include <iterator>
#include <tuple>
#include <map>
#include <optional>
#include "utils.hpp"

namespace kaixo {

    // Utils
    template<class Type>
    using container_for = std::conditional_t<std::is_same_v<Type, char>, std::string, std::vector<Type>>;
    template<class Type, class ...Tys>
    concept has_emplace_back = requires(Type t, Tys...tys) { t.emplace_back(tys...); };
    template<class Type, class ...Tys>
    concept has_try_emplace = requires(Type t, Tys...tys) { t.try_emplace(tys...); };
    template<class Type>
    concept has_begin_end = requires(Type a) { a.begin(), a.end(), a.size(); };
    template<class> struct return_type;
    template<class R, class...T> struct return_type<R(T...)> { using type = R; };

    /*
     * Expression storage, basically type erased lambda storage 
     * through inheritance. Works as shared ptr with ref count.
     */
    template<class Ty>
    struct expr_storage_base {
        bool cache_value = false;
        std::optional<std::remove_reference_t<Ty>> evaluated;
        size_t refs = 1;
        virtual Ty get() = 0;
    };

    template<class R, class Ty>
    struct lambda_expr_storage : expr_storage_base<R> {
        Ty lambda;
        lambda_expr_storage(Ty t) : lambda(t) {}
        R get() { 
            if (!this->cache_value)
                return lambda();

            else {
                if (!this->evaluated)
                    this->evaluated = lambda();
                return this->evaluated.value();
            }
        }
    };

    template<class Ty>
    struct expr_storage {
        template<class T>
        expr_storage(T t) : storage(new lambda_expr_storage<Ty, decltype(t)>{ t }) {}
        expr_storage() : storage(nullptr) {}
        expr_storage(expr_storage&& other) : storage(other.storage) { other.storage = nullptr; }
        expr_storage(const expr_storage& other) : storage(other.storage) { storage->refs++; }
        ~expr_storage() { clean(); }
        template<class T>
        void operator=(T t) { clean(); storage = new lambda_expr_storage<Ty, decltype(t)>{ t }; }
        void clean() { if (storage && --storage->refs == 0) delete storage; }
        expr_storage_base<Ty>* storage;
    };

    /**
     * Variable storage, stores either a value or a reference (as pointer).
     * Also works as a shared pointer, makes sure the contained value stays
     * alive as long as any list comprehension object uses this variable.
     */
    template<class Ty>
    struct var_storage_base {
        enum _state { v, r } state = v;
        
        var_storage_base(Ty&& val) : value(std::move(val)), state(v) {}
        var_storage_base(Ty& val) : ref(&val), state(r) {}
        var_storage_base(const Ty& val) : value(val), state(v) {}
        ~var_storage_base() { clean(); }

        template<class T> requires (!std::is_reference_v<T>)
        void operator=(T& val) { clean(); ref = &val; state = r; }
        template<class T> requires (!std::is_reference_v<T>)
        void operator=(T&& val) { clean(); new (&value) Ty{ val }; state = v; }
        template<class T> requires (!std::is_reference_v<T>)
        void operator=(const T& val) { clean(); new (&value) Ty{ val }; state = v; }

        size_t refs = 1;
        
        inline Ty& get() { return state == v ? value : *ref; };

        inline void clean() { 
            if constexpr (!std::is_trivially_destructible_v<Ty>)
                if (state == v) value.~Ty();
        }

        union {
            Ty value;
            Ty* ref;
        };
    };

    template<class Ty>
    struct var_storage {
        using value_type = std::decay_t<Ty>;
        var_storage() : storage(new var_storage_base{ Ty{} }) {}
        var_storage(value_type&& t) : storage(new var_storage_base<Ty>{ std::move(t) }) {}
        var_storage(value_type& t) : storage(new var_storage_base<Ty>{ t }) {}
        var_storage(const value_type& t) : storage(new var_storage_base<Ty>{ t }) {}
        var_storage(var_storage&& other) : storage(other.storage) { other.storage = nullptr; }
        var_storage(const var_storage& other) : storage(other.storage) { storage->refs++; }
        ~var_storage() { clean(); }

        template<class Arg>
        var_storage& operator=(Arg&& arg) { *storage = std::forward<Arg>(arg); return *this; };


        void clean() { if (storage && --storage->refs == 0) delete storage; }

        var_storage_base<Ty>* storage;
    };

    template<class Ty, class Storage = expr_storage<Ty>>
    struct expr;

    /**
     * Basis for an expr and var, has certain storage that will generate
     * the result, either variable or from expression stored in a lambda.
     */
    template<class Ty>
    struct var;
    struct is_an_expr {};
    template<class Ty, class Storage = expr_storage<Ty>>
    struct expr_base : is_an_expr {
        using type = Ty;

        expr_base() = default;
        expr_base(expr_base&&) = default;
        expr_base(expr_base&) = default;
        expr_base(const expr_base&) = default;

        expr_base(expr<Ty, Storage>& other) : storage(other.storage) {}
        expr_base(expr<Ty, Storage>&& other) : storage(std::move(other.storage)) {}
        expr_base(const expr<Ty, Storage>& other) : storage(other.storage) {}

        expr_base(var<Ty> v) requires std::same_as<Storage, expr_storage<Ty>> : storage([v]() { return v(); }) {}

        template<class ...Args> 
        expr_base(Args&&...args)
            : storage(std::forward<Args>(args)...) {}

        Ty operator()() const { return storage.storage->get(); }

        template<class T>
        expr_base& operator=(T&& t) { storage.operator=(std::forward<T>(t)); return *this; }

        template<class Ty> expr<Ty> to() {
            return { [e = expr<type>{ *this }] () { return static_cast<Ty>(e()); } };
        }

        Storage storage;
    };

    template<class Ty, class Storage>
    struct expr : expr_base<Ty, Storage> {
        using type = Ty;
        using expr_base<Ty, Storage>::expr_base;
        using expr_base<Ty, Storage>::operator=;
    };

    template<class Ty>
    struct var : expr<Ty&, var_storage<Ty>> {
        using type = Ty&;
        using expr<Ty&, var_storage<Ty>>::expr;
        using expr<Ty&, var_storage<Ty>>::operator=;

        var(var& other) : expr<Ty&, var_storage<Ty>>(other.storage) {}
        var(var&& other) : expr<Ty&, var_storage<Ty>>(std::move(other.storage)) {}
        var(const var& other) : expr<Ty&, var_storage<Ty>>(other.storage) {}

        template<class T>
        var& operator=(T&& t) { this->storage.operator=(std::forward<T>(t)); return *this; }
    };

    template<class Type>
    expr(Type)->expr<typename return_type<lambda_signature_t<Type>>::type>;

    template<class Type>
    var(Type)->var<Type>;

    // Forward declarations
    template<class Container, class ...Types>
    struct container_syntax;

    /**
     * Expression wrapper is used to simplify templated functions by generalizing how
     * to get the value, for an expression that requires the operator(). 
     */
    template<class Type>
    struct expr_wrapper;
    template<class Type> requires std::derived_from<std::decay_t<Type>, is_an_expr>
    struct expr_wrapper<Type> {
        std::decay_t<Type> value;
        typename std::decay_t<Type>::type get() {
            return value(); 
        }
    };

    template<class Type> requires (!std::derived_from<std::decay_t<Type>, is_an_expr>)
    struct expr_wrapper<Type> {
        Type value;
        Type get() {
            return value;
        }
    };

    /**
     * Specializations of the expression class for standard classes 
     * and containers that contain all their member functions.
     */
#define lc_mem_fun(y, x) \
    template<class ...Args> auto x(Args&& ...exprs) const { return kaixo::expr{ [s = *this, ...args = expr_wrapper<Args>{ \
        std::forward<Args>(exprs) }]() mutable { return s().y::x(args.get()...); } }; } \

    template<class String, class Storage = expr_storage<String>>
    struct string_expression : expr_base<String, Storage> {
        using type = std::decay_t<String>;
        using expr_base<String, Storage>::expr_base;
        using expr_base<String, Storage>::operator=;

        lc_mem_fun(type, at);
        lc_mem_fun(type, operator[]);
        lc_mem_fun(type, front);
        lc_mem_fun(type, back);
        lc_mem_fun(type, data);
        lc_mem_fun(type, c_str);
        lc_mem_fun(type, begin);
        lc_mem_fun(type, cbegin);
        lc_mem_fun(type, end);
        lc_mem_fun(type, cend);
        lc_mem_fun(type, rbegin);
        lc_mem_fun(type, crbegin);
        lc_mem_fun(type, rend);
        lc_mem_fun(type, crend);
        lc_mem_fun(type, empty);
        lc_mem_fun(type, size);
        lc_mem_fun(type, length);
        lc_mem_fun(type, max_size);
        lc_mem_fun(type, reserve);
        lc_mem_fun(type, capacity);
        lc_mem_fun(type, shrink_to_fit);
        lc_mem_fun(type, clear);
        lc_mem_fun(type, insert);
        lc_mem_fun(type, erase);
        lc_mem_fun(type, push_back);
        lc_mem_fun(type, pop_back);
        lc_mem_fun(type, append);
        lc_mem_fun(type, operator+=);
        lc_mem_fun(type, compare);
        lc_mem_fun(type, starts_with);
        lc_mem_fun(type, ends_with);
        lc_mem_fun(type, replace);
        lc_mem_fun(type, substr);
        lc_mem_fun(type, copy);
        lc_mem_fun(type, resize);
        lc_mem_fun(type, swap);
        lc_mem_fun(type, find);
        lc_mem_fun(type, rfind);
        lc_mem_fun(type, find_first_of);
        lc_mem_fun(type, find_first_not_of);
        lc_mem_fun(type, find_last_of);
        lc_mem_fun(type, find_last_not_of);
    };

    template<class Tuple, class Storage = expr_storage<Tuple>>
    struct tuple_expression : expr_base<Tuple, Storage> {
        using type = std::decay_t<Tuple>;
        using expr_base<Tuple, Storage>::expr_base;
        using expr_base<Tuple, Storage>::operator=;

        lc_mem_fun(type, swap);

        template<size_t N>
        expr<std::decay_t<decltype(std::get<N>(std::declval<type>()))>>get() {
            return { [c = *this] () { return std::get<N>(c()); } };
        }
    };

    template<class Vector, class Storage = expr_storage<Vector>>
    struct vector_expression : expr_base<Vector, Storage> {
        using type = std::decay_t<Vector>;
        using expr_base<Vector, Storage>::expr_base;
        using expr_base<Vector, Storage>::operator=;

        lc_mem_fun(type, operator=);
        lc_mem_fun(type, assign);
        lc_mem_fun(type, get_allocator);
        lc_mem_fun(type, at);
        lc_mem_fun(type, operator[]);
        lc_mem_fun(type, front);
        lc_mem_fun(type, back);
        lc_mem_fun(type, data);
        lc_mem_fun(type, begin);
        lc_mem_fun(type, cbegin);
        lc_mem_fun(type, end);
        lc_mem_fun(type, cend);
        lc_mem_fun(type, rbegin);
        lc_mem_fun(type, crbegin);
        lc_mem_fun(type, rend);
        lc_mem_fun(type, crend);
        lc_mem_fun(type, empty);
        lc_mem_fun(type, size);
        lc_mem_fun(type, max_size);
        lc_mem_fun(type, reserve);
        lc_mem_fun(type, capacity);
        lc_mem_fun(type, shrink_to_fit);
        lc_mem_fun(type, clear);
        lc_mem_fun(type, insert);
        lc_mem_fun(type, emplace);
        lc_mem_fun(type, erase);
        lc_mem_fun(type, push_back);
        lc_mem_fun(type, emplace_back);
        lc_mem_fun(type, pop_back);
        lc_mem_fun(type, resize);
        lc_mem_fun(type, swap);
    };

    template<class Array, class Storage = expr_storage<Array>>
    struct array_expression : expr_base<Array, Storage> {
        using type = std::decay_t<Array>;
        using expr_base<Array, Storage>::expr_base;
        using expr_base<Array, Storage>::operator=;

        lc_mem_fun(type, operator=);
        lc_mem_fun(type, at);
        lc_mem_fun(type, operator[]);
        lc_mem_fun(type, front);
        lc_mem_fun(type, back);
        lc_mem_fun(type, data);
        lc_mem_fun(type, begin);
        lc_mem_fun(type, cbegin);
        lc_mem_fun(type, end);
        lc_mem_fun(type, cend);
        lc_mem_fun(type, rbegin);
        lc_mem_fun(type, crbegin);
        lc_mem_fun(type, rend);
        lc_mem_fun(type, crend);
        lc_mem_fun(type, empty);
        lc_mem_fun(type, size);
        lc_mem_fun(type, max_size);
        lc_mem_fun(type, fill);
        lc_mem_fun(type, swap);
    };

    template<class Map, class Storage = expr_storage<Map>>
    struct map_expression : expr_base<Map, Storage> {
        using type = std::decay_t<Map>;
        using expr_base<Map, Storage>::expr_base;
        using expr_base<Map, Storage>::operator=;

        lc_mem_fun(type, operator=);
        lc_mem_fun(type, get_allocator);
        lc_mem_fun(type, at);
        lc_mem_fun(type, operator[]);
        lc_mem_fun(type, begin);
        lc_mem_fun(type, cbegin);
        lc_mem_fun(type, end);
        lc_mem_fun(type, cend);
        lc_mem_fun(type, rbegin);
        lc_mem_fun(type, crbegin);
        lc_mem_fun(type, rend);
        lc_mem_fun(type, crend);
        lc_mem_fun(type, empty);
        lc_mem_fun(type, size);
        lc_mem_fun(type, max_size);
        lc_mem_fun(type, clear);
        lc_mem_fun(type, insert);
        lc_mem_fun(type, insert_or_assign);
        lc_mem_fun(type, emplace);
        lc_mem_fun(type, emplace_hint);
        lc_mem_fun(type, try_emplace);
        lc_mem_fun(type, erase);
        lc_mem_fun(type, swap);
        lc_mem_fun(type, extract);
        lc_mem_fun(type, merge);
        lc_mem_fun(type, count);
        lc_mem_fun(type, find);
        lc_mem_fun(type, contains);
        lc_mem_fun(type, equal_range);
        lc_mem_fun(type, lower_bound);
        lc_mem_fun(type, upper_bound);
        lc_mem_fun(type, key_comp);
        lc_mem_fun(type, value_comp);
    };

#define COMMA ,
#define make_expr(cls, e, ...) \
    template<__VA_ARGS__ class Storage> struct expr<cls, Storage> : e<cls, Storage> { using type = cls; using e<cls, Storage>::e; }; \
    template<__VA_ARGS__ class Storage> struct expr<cls&, Storage> : e<cls&, Storage> { using type = cls; using e<cls&, Storage>::e; }; \

    make_expr(std::string, string_expression);
    make_expr(std::tuple<Args...>, tuple_expression, class ...Args,);
    make_expr(std::vector<Args...>, vector_expression, class ...Args,);
    make_expr(std::array<Arg COMMA N>, array_expression, class Arg, size_t N,);
    make_expr(std::map<Args...>, map_expression, class ...Args,);
#undef COMMA

#define var_op(x)\
    template<class A, class B> auto operator x(var<A> a, var<B> b) { return expr{ [a, b]() { return a() x b(); } }; } \
    template<class A, class B> auto operator x(expr<A> a, var<B> b) { return expr{ [a, b]() { return a() x b(); } }; } \
    template<class A, class B> auto operator x(var<A> a, expr<B> b) { return expr{ [a, b]() { return a() x b(); } }; } \
    template<class A, class B> auto operator x(expr<A> a, expr<B> b) { return expr{ [a, b]() { return a() x b(); } }; } \
    template<class A, class B> auto operator x(expr<A> a, const B& b) { return expr{ [a, b]() { return a() x b; } }; } \
    template<class A, class B> auto operator x(var<A> a, const B& b) { return expr{ [a, b]() { return a() x b; } }; } \
    template<class A, class B> auto operator x(const A& a, expr<B> b) { return expr{ [a, b]() { return a x b(); } }; } \
    template<class A, class B> auto operator x(const A& a, var<B> b) { return expr{ [a, b]() { return a x b(); } }; }

    var_op(+);
    var_op(-);
    var_op(/ );
    var_op(*);
    var_op(%);
    var_op(== );
    var_op(!= );
    var_op(<= );
    var_op(>= );
    var_op(> );
    var_op(< );
    var_op(<=> );
    var_op(&&);
    var_op(&);
    var_op(|| );
    var_op(| );
    var_op(<< );
    var_op(>> );

#define u_var_op(x)\
    template<class A> auto operator x(var<A> a) { return expr{ [a]() { return x a(); } }; } \
    template<class A> auto operator x(expr<A> a) { return expr{ [a]() { return x a(); } }; } \

    u_var_op(-);
    u_var_op(+);
    u_var_op(~);
    u_var_op(!);
    u_var_op(*);
    u_var_op(&);

    /**
     * Simple tuple of vars.
     */
    template<class ...Args>
    struct tuple_of_vars {
        using type = std::tuple<Args...>;
        std::tuple<var<Args>...> vars;

        void operator=(type&& a) { vars = a; }
    };

    template<class A, class B>
    tuple_of_vars<A, B> operator,(var<A> a, var<B> b) { return { { a, b } }; }

    template<class A, class ...Args>
    tuple_of_vars<Args..., A> operator,(tuple_of_vars<Args...> a, var<A> b) { return { std::tuple_cat(a.vars, std::tuple{ b }) }; }

    /**
     * Wrapper for a container, used when creating a cartesian product, works
     * with any class that defines a begin and end method.
     */
    template<class Type, has_begin_end Container>
    struct container {
        using type = Type;
        Container container;

        auto begin() { return container.begin(); }
        auto end() { return container.end(); }
        auto size() const { return container.size(); }
    };

    /**
     * A struct that links a var to a container, this is stored in the final
     * link comprehension object.
     */
    template<class Type, has_begin_end Container>
    struct linked_container {
        using type = typename Type::type;
        constexpr static bool has_var = false;
        Type variable;
        Container container;
        using iterator = decltype(container.begin());

        iterator begin() { return container.begin(); }
        iterator end() { return container.end(); }
        size_t size() const { return container.size(); }
    };

    /**
     * Specialization for storing a variable that results in a container.
     * So we can store the variable as a reference.
     */
    template<class Type, has_begin_end VarType>
    struct linked_container<Type, container<typename VarType::value_type, var<VarType>>> {
        using type = typename Type::type;
        constexpr static bool has_var = true;
        Type variable;
        container<typename VarType::value_type, var<VarType>> container;
        using iterator = decltype(container.begin()());

        iterator begin() { return container.begin()(); }
        iterator end() { return container.end()(); }
        size_t size() const { return container.size()(); }
    };

    /**
     * the '-' and '<' operators are used to create a '<-' operator, the unary '-'
     * creates a container from some class that defines a begin() and end(), and the '<'
     * links that container to a variable.
     */
    template<template<class...> class Container, class ...Type>
    auto operator-(Container<Type...>& r) -> container<typename Container<Type...>::value_type, Container<Type...>&> { return { r }; }

    template<template<class...> class Container, class ...Type>
    auto operator-(Container<Type...>&& r) -> container<typename Container<Type...>::value_type, Container<Type...>> { return { std::move(r) }; }

    template<has_begin_end Type>
    auto operator-(var<Type> r) -> container<typename Type::value_type, var<Type>> { return { r }; }

    template<class Type, class CType, class Container>
    auto operator<(var<Type> v, container<CType, Container>&& r) -> linked_container<var<Type>, container<CType, Container>> { return { v, std::move(r) }; }

    template<class CType, class Container, class ...Args>
    auto operator<(tuple_of_vars<Args...>&& v, container<CType, Container>&& r) -> linked_container<tuple_of_vars<Args...>, container<CType, Container>> { return { v, std::move(r) }; }

    /**
     * This is the part before the '|', and defined what type of container the
     * result will be stored in. Also contains a generate method to easily generate an entry
     * for the resulting container given the values currently in the variables.
     */
    template<class Container, class ...Types>
    struct container_syntax {
        using container = Container;
        std::tuple<expr<Types>...> expressions;

        template<class T>
        inline void generate(T& container) { return m_Gen(container, std::make_index_sequence<sizeof...(Types)>{}); }

        template<class T, size_t ...Is>
        inline void m_Gen(T& container, std::index_sequence<Is...>) {
            if constexpr (std::same_as<std::string, T>)
                container.push_back(std::get<Is>(expressions)()...);
            else if constexpr (has_emplace_back<T, typename expr<Types>::type...>)
                container.emplace_back(std::get<Is>(expressions)()...);
            else if constexpr (has_try_emplace<T, typename expr<Types>::type...>)
                container.try_emplace(std::get<Is>(expressions)()...);
            else
                container.emplace(std::get<Is>(expressions)()...);
        }
    };

    /**
     * Operators and functions for constructing a container syntax.
     * Eiter a comma operator to use standard vector, or a function to choose
     * the container.
     */

    template<class A, class B>
    container_syntax<std::vector<std::tuple<A, B>>, A, B> operator,(expr<A> a, var<B> b) {
        return { { a, b } };
    }

    template<class A, class B>
    container_syntax<std::vector<std::tuple<A, B>>, A, B> operator,(var<A> a, expr<B> b) {
        return { { a, b } };
    }

    template<class A, class B>
    container_syntax<std::vector<std::tuple<A, B>>, A, B> operator,(expr<A> a, expr<B> b) {
        return { { a, b } };
    }

    template<class A, class ...Rest>
    container_syntax<std::vector<std::tuple<Rest..., A>>, Rest..., A> operator,(tuple_of_vars<Rest...>&& a, expr<A> b) {
        return { std::tuple_cat(a.vars, std::tuple{ b }) };
    }

    template<class A, class ...Rest>
    container_syntax<std::vector<std::tuple<Rest..., A>>, Rest..., A> operator,(container_syntax<std::vector<std::tuple<Rest...>>, Rest...>&& a, var<A> b) {
        return { std::tuple_cat(a.expressions, std::tuple{ b }) };
    }

    template<class A, class ...Rest>
    container_syntax<std::vector<std::tuple<Rest..., A>>, Rest..., A> operator,(container_syntax<std::vector<std::tuple<Rest...>>, Rest...>&& a, expr<A> b) {
        return { std::tuple_cat(a.expressions, std::tuple{ b }) };
    }

    template<class ...Types>
    container_syntax<std::vector<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> vector(Types&& ...tys) {
        return { { expr{ [tys]() { return tys(); } }... } };
    }

    template<class ...Types>
    container_syntax<std::list<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> list(Types&& ...tys) {
        return { { expr{ [tys]() { return tys(); } }... } };
    }

    template<class ...Types>
    container_syntax<std::forward_list<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> forward_list(Types&& ...tys) {
        return { { expr{ [tys]() { return tys(); } }... } };
    }

    template<class ...Types>
    container_syntax<std::deque<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> deque(Types&& ...tys) {
        return { { expr{ [tys]() { return tys(); } }... } };
    }
    
    template<class ...Types>
    container_syntax<std::stack<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> stack(Types&& ...tys) {
        return { { expr{ [tys]() { return tys(); } }... } };
    }
    
    template<class ...Types>
    container_syntax<std::queue<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> queue(Types&& ...tys) {
        return { { expr{ [tys]() { return tys(); } }... } };
    }
    
    template<class ...Types>
    container_syntax<std::priority_queue<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> priority_queue(Types&& ...tys) {
        return { { expr{ [tys]() { return tys(); } }... } };
    }

    template<class ...Types>
    container_syntax<std::set<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> set(Types&& ...tys) {
        return { { expr{ [tys]() { return tys(); } }... } };
    }

    template<class ...Types>
    container_syntax<std::multiset<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> multiset(Types&& ...tys) {
        return { { expr{ [tys]() { return tys(); } }... } };
    }

    template<class ...Types>
    container_syntax<std::unordered_set<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> unordered_set(Types&& ...tys) {
        return { { expr{ [tys]() { return tys(); } }... } };
    }
    
    template<class ...Types>
    container_syntax<std::unordered_multiset<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> unordered_multiset(Types&& ...tys) {
        return { { expr{ [tys]() { return tys(); } }... } };
    }

    template<class Ty, class ...Types>
    container_syntax<std::map<typename std::decay_t<Ty>::type, std::conditional_t<sizeof...(Types) == 1, 
        std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>, std::tuple<typename std::decay_t<Types>::type...>>>, 
        typename std::decay_t<Ty>::type, typename std::decay_t<Types>::type...> map(Ty&& ty, Types&& ...tys) {
        return { { expr{ [ty]() { return ty(); } }, expr{ [tys]() { return tys(); } }... } };
    }

    template<class Ty, class ...Types>
    container_syntax<std::multimap<typename std::decay_t<Ty>::type, std::conditional_t<sizeof...(Types) == 1,
        std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>, std::tuple<typename std::decay_t<Types>::type...>>>,
        typename std::decay_t<Ty>::type, typename std::decay_t<Types>::type...> multimap(Ty&& ty, Types&& ...tys) {
        return { { expr{ [ty]() { return ty(); } }, expr{ [tys]() { return tys(); } }... } };
    }

    template<class Ty, class ...Types>
    container_syntax<std::unordered_map<typename std::decay_t<Ty>::type, std::conditional_t<sizeof...(Types) == 1,
        std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>, std::tuple<typename std::decay_t<Types>::type...>>>,
        typename std::decay_t<Ty>::type, typename std::decay_t<Types>::type...> unordered_map(Ty&& ty, Types&& ...tys) {
        return { { expr{ [ty]() { return ty(); } }, expr{ [tys]() { return tys(); } }... } };
    }
    
    template<class Ty, class ...Types>
    container_syntax<std::unordered_multimap<typename std::decay_t<Ty>::type, std::conditional_t<sizeof...(Types) == 1,
        std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>, std::tuple<typename std::decay_t<Types>::type...>>>,
        typename std::decay_t<Ty>::type, typename std::decay_t<Types>::type...> unordered_multimap(Ty&& ty, Types&& ...tys) {
        return { { expr{ [ty]() { return ty(); } }, expr{ [tys]() { return tys(); } }... } };
    }

    /**
     * Everything combined into a single object, contains the container syntax, all
     * the linked containers, and also some constraints, which are expressions that evaluate to 'bool'
     */
    template<class ContainerSyntax, class ...LinkedContainers>
    struct list_comprehension {
        using container = typename ContainerSyntax::container;
        constexpr static auto size = sizeof...(LinkedContainers);
        constexpr static auto sequence = std::make_index_sequence<size>{};
        constexpr static bool has_var = (LinkedContainers::has_var || ...);
        
        ContainerSyntax syntax;
        std::tuple<LinkedContainers...> containers;
        std::vector<expr<bool>> constraints;

        // Get result as an expression for later evaluation.
        expr<container> get_var() && {
            return { [cpy = std::move(*this)]() mutable { return cpy.m_Get(); } };
        }

        expr<container> get_var()& {
            return { [this] () { return this->m_Get(); } };
        }

        container operator*() { return m_Get(); }
        container get() { return this->m_Get(); }

    private:
        inline container m_Get() {
            container result;

            std::tuple<LinkedContainers::iterator...> its;
            set_begin(its, sequence); // Initialize all iterators to begin

            int index = size - 1;
            bool done = false;
            while (!done) { // This will loop through all the values in the cartesian product of the linked containers.
                set_values(its, sequence); // Set all vars to the values that the iterators point to.

                bool _match = true;
                for (auto& c : constraints) // Check all constraints.
                    _match &= c();

                if (_match) // If all matched, generate an entry, and add to result.
                    syntax.generate(result);

                if (!check_end(its, index, sequence))
                    increment(its, index, sequence); // Increment the iterator
                while (check_end(its, index, sequence)) { // And check if it's now at the end.
                    set_begin(its, index, sequence); // Reset the iterator
                    index--;                         // And go to the next index to increment that one.
                    if (index == -1) {               // If we're at the end, we're done.
                        done = true;
                        break;
                    }
                    if (!check_end(its, index, sequence)) {
                        increment(its, index, sequence); // Otherwise increment the iterator and loop to check if also at the end.
                        
                        if constexpr (has_var) { // When containers contain variables, we must keep iterator up to date.
                            if (!check_end(its, index, sequence)) {
                                set_values(its, sequence);
                                set_begin(its, index + 1, sequence);
                            }
                        }
                    }
                }

                index = size - 1; // Reset index back to 0 for the next iteration.
            }

            return result;
        }

        /**
         * Some helper functions because dealing with tuples
         * is a living nightmare when also working with changing indices...
         */
        template<class T, std::size_t ...Is>
        void set_begin(T& tuple, std::index_sequence<Is...>) {
            ((void(std::get<Is>(tuple) = std::get<Is>(containers).begin()),
              void(std::get<Is>(containers).variable = *std::get<Is>(containers).begin())), ...);
        }

        template<class T, std::size_t ...Is>
        void set_begin(T& tuple, std::size_t i, std::index_sequence<Is...>) {
            ((Is == i ? (std::get<Is>(tuple) = std::get<Is>(containers).begin(), true) : true), ...);
        }

        template<class T, std::size_t ...Is>
        bool check_end(T& tuple, size_t i, std::index_sequence<Is...>) {
            bool is_end = false;
            ((Is == i ? (is_end = std::get<Is>(tuple) == std::get<Is>(containers).end(), true) : true), ...);
            return is_end;
        }

        template<class T, std::size_t ...Is>
        void set_values(T& tuple, std::index_sequence<Is...>) {
            ((std::get<Is>(containers).variable = *std::get<Is>(tuple)), ...);
        }

        template<class T, std::size_t ...Is>
        void increment(T& tuple, std::size_t index, std::index_sequence<Is...>) {
            ((Is == index ? (++std::get<Is>(tuple), true) : true), ...);
        }

        template<class T, std::size_t ...Is>
        void set_as_end(T& tuple, std::size_t index, std::index_sequence<Is...>) {
            ((Is == index ? (std::get<Is>(tuple) = std::get<Is>(containers).end(), true) : true), ...);
        }
    };

    /**
     * Operators for initializing a list comprehension object with a container syntax and the first linked container.
     */
    template<class ContainerSyntax, class Container, class CType>
    list_comprehension<ContainerSyntax, linked_container<CType, Container>>
        operator|(const ContainerSyntax& v, linked_container<CType, Container>&& c) {
        return { v, linked_container<CType, Container>{ std::move(c) }, {} };
    }

    template<class Type, class Container, class CType>
    list_comprehension<container_syntax<container_for<std::decay_t<Type>>, Type>, linked_container<CType, Container>>
        operator|(var<Type> v, linked_container<CType, Container>&& c) {
        return { container_syntax<container_for<std::decay_t<Type>>, Type>{ expr<Type>{ [v]() { return v(); } } }, std::move(c), {} };
    }

    template<class Type, class Container, class CType>
    list_comprehension<container_syntax<container_for<std::decay_t<Type>>, Type>, linked_container<CType, Container>>
        operator|(expr<Type> v, linked_container<CType, Container>&& c) {
        return { container_syntax<container_for<std::decay_t<Type>>, Type>{ v }, std::move(c), {} };
    }

    template<class Container, class CType, class...Args>
    list_comprehension<container_syntax<std::vector<std::tuple<Args...>>, Args...>, linked_container<CType, Container>>
        operator|(tuple_of_vars<Args...>&& v, linked_container<CType, Container>&& c) {
        return { container_syntax<std::vector<std::tuple<Args...>>, Args...>{ v.vars }, std::move(c), {} };
    }

    /**
     * Operators for expanding the initial list comprehension with
     * more linked containers or constraints.
     */

    template<class ContainerSyntax, class Container, class CType, class ...LinkedContainers>
    list_comprehension<ContainerSyntax, LinkedContainers..., linked_container<CType, Container>>
        operator,(list_comprehension<ContainerSyntax, LinkedContainers...>&& v, linked_container<CType, Container>&& c) {
        return { std::move(v.syntax), std::tuple_cat(std::move(v.containers), std::tuple{ c }), std::move(v.constraints) };
    }

    template<class ContainerSyntax, class ...LinkedContainers>
    list_comprehension<ContainerSyntax, LinkedContainers...>
        operator,(list_comprehension<ContainerSyntax, LinkedContainers...>&& v, expr<bool> c) {
        v.constraints.push_back(c);
        return v;
    }

    template<std::convertible_to<bool> Type, class ContainerSyntax, class ...LinkedContainers>
    list_comprehension<ContainerSyntax, LinkedContainers...>
        operator,(list_comprehension<ContainerSyntax, LinkedContainers...>&& v, expr<Type> c) {
        v.constraints.push_back({ [c = std::move(c)] () { return static_cast<bool>(c()); } });
        return v;
    }

    /**
     * Used for parallel iteration of containers. Stores multiple containers
     * and defines an iterator that returns a tuple of references to the values.
     */
    template<has_begin_end... Containers>
    struct tuple_of_containers {
        using value_type = std::tuple<typename std::decay_t<Containers>::value_type&...>;
        std::tuple<Containers...> containers;

        struct iterator {
            using iterator_category = std::forward_iterator_tag;
            using value_type = std::tuple<typename std::decay_t<Containers>::value_type&...>;
            using difference_type = std::ptrdiff_t;
            using pointer = value_type*;
            using reference = value_type&;

            constexpr static auto seq = std::make_index_sequence<sizeof...(Containers)>{};

            iterator& operator++() { return increment_r(seq); }
            iterator operator++(int) { return increment(seq); }
            value_type operator*() { return get(seq); }
            bool operator==(const iterator& o) const { return equal(o, seq); }
            std::tuple<typename std::decay_t<Containers>::iterator...> iterators;

        private:
            template<size_t ...Is>
            value_type get(std::index_sequence<Is...>) { 
                return { *std::get<Is>(iterators)... };
            }

            template<size_t ...Is>
            iterator& increment_r(std::index_sequence<Is...>) {
                ((++std::get<Is>(iterators)), ...);
                return *this;
            }

            template<size_t ...Is>
            iterator increment(std::index_sequence<Is...>) {
                ((std::get<Is>(iterators)++), ...);
                return *this;
            }

            template<size_t ...Is>
            bool equal(const iterator& other, std::index_sequence<Is...>) const {
                return ((std::get<Is>(iterators) == std::get<Is>(other.iterators)) || ...);
            }
        };

        iterator begin() { return seq_begin(std::make_index_sequence<sizeof...(Containers)>{}); };
        iterator end() { return seq_end(std::make_index_sequence<sizeof...(Containers)>{}); };
        size_t size() const { return seq_size(std::make_index_sequence<sizeof...(Containers)>{}); }

    private:
        template<size_t ...Is>
        size_t seq_size(std::index_sequence<Is...>) const {
            size_t _size = 0;
            ((std::get<Is>(containers).size() > _size ? (_size = std::get<Is>(containers).size(), false) : false), ...);
            return _size;
        }

        template<size_t ...Is>
        iterator seq_begin(std::index_sequence<Is...>) { return { { std::get<Is>(containers).begin()... } }; }

        template<size_t ...Is>
        iterator seq_end(std::index_sequence<Is...>) { return { { std::get<Is>(containers).end()... } }; }
    };

    /**
     * Operators to construct tuple of containers.
     */
    template<has_begin_end A, has_begin_end B>
    tuple_of_containers<A, B> operator,(A&& a, B&& b) {
        return { { { std::forward<A>(a) }, { std::forward<B>(b) } } };
    }

    template<has_begin_end A, has_begin_end ...Rest>
    tuple_of_containers<Rest..., A> operator,(tuple_of_containers<Rest...>&& a, A&& b) {
        return { std::tuple_cat(a.containers, std::tuple{ std::forward<A>(b) }) };
    }

    /**
     * Some sugar on top that allows syntax like
     * 'lc[a | a <- range(0, 10)]' for 'true' list comprehension.
     */
    struct lce {
        template<class ContainerSyntax, class ...LinkedContainers>
        auto operator[](list_comprehension<ContainerSyntax, LinkedContainers...>&& l) { return std::move(l).get(); };
    } lc;

    struct lcev {
        template<class ContainerSyntax, class ...LinkedContainers>
        auto operator[](list_comprehension<ContainerSyntax, LinkedContainers...>&& l) { return std::move(l).get_var(); };
    } lcv;

    /**
     * Simple range class with a start and end, plus an iterator because
     * list comprehension uses iterators to create the cartesian product of all sets.
     */
    template<class Type>
    struct range {
        using value_type = Type;

        struct iterator {
            using iterator_category = std::bidirectional_iterator_tag;
            using value_type = Type;
            using difference_type = std::ptrdiff_t;
            using pointer = Type*;
            using reference = Type&;
            const range* r;
            Type cur;
            iterator& operator++() { r->a() <= r->b() ? ++cur : --cur; return *this; }
            iterator& operator--() { r->a() <= r->b() ? --cur : ++cur; return *this; }
            iterator operator++(int) { return { r, r->a() <= r->b() ? cur + 1 : cur - 1 }; }
            iterator operator--(int) { return { r, r->a() <= r->b() ? cur - 1 : cur + 1 }; }
            Type& operator*() { return cur; }
            Type* operator&() { return &cur; }
            bool operator==(const iterator& o) const { return o.cur == cur; }
        };

        using const_iterator = iterator;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

        range(Type&& x, Type&& y) { init(a, std::move(x)); init(b, std::move(y)); }
        range(Type& x, Type&& y) { init(a, x); init(b, std::move(y)); }
        range(Type&& x, Type& y) { init(a, std::move(x)); init(b, y); }
        range(Type& x, Type& y) { init(a, x); init(b, y); }
        range(Type&& x, expr<Type> y) { init(a, std::move(x)); init(b, y); }
        range(Type& x, expr<Type> y) { init(a, x); init(b, y); }
        range(Type&& x, var<Type> y) { init(a, std::move(x)); init(b, y); }
        range(Type& x, var<Type> y) { init(a, x); init(b, y); }
        range(var<Type> x, Type&& y) { init(a, x); init(b, std::move(y)); }
        range(expr<Type> x, Type&& y) { init(a, x); init(b, std::move(y)); }
        range(var<Type> x, Type& y) { init(a, x); init(b, y); }
        range(expr<Type> x, Type& y) { init(a, x); init(b, y); }
        range(var<Type> x, var<Type> y) { init(a, x); init(b, y); }
        range(expr<Type> x, var<Type> y) { init(a, x); init(b, y); }
        range(var<Type> x, expr<Type> y) { init(a, x); init(b, y); }
        range(expr<Type> x, expr<Type> y) { init(a, x); init(b, y); }

        iterator begin() { return { this, a() }; }
        iterator end() { return { this, b() + 1 }; }
        const_iterator begin() const { return { this, a() }; }
        const_iterator end() const { return { this, b() + 1 }; }
        const_iterator cbegin() const { return begin(); }
        const_iterator cend() const { return end(); }
        reverse_iterator rbegin() { return end(); }
        reverse_iterator rend() { return begin(); }
        const_reverse_iterator rbegin() const { return end(); }
        const_reverse_iterator rend() const { return begin(); }
        const_reverse_iterator crbegin() const { return end(); }
        const_reverse_iterator crend() const { return begin(); }

        size_t size() const { return b() - a(); }
    private:
        expr<Type> a, b;

        void init(expr<Type>& a, var<Type> t) { a = [t]() { return t(); }; }
        void init(expr<Type>& a, expr<Type> t) { a = [t]() { return t(); }; }
        void init(expr<Type>& a, Type&& t) { a = [t = std::move(t)]() { return t; }; }
        void init(expr<Type>& a, const Type& t) { a = [&t]() { return t; }; }
    };

    template<class Type, class T2>
    range(Type, T2)->range<Type>;
}

/**
 * Most std function definitions in 'expression' form.
 * to disable a portion of this, just define KAIXO_LC_header as something other than 1.
 */

#define lc_std_fun(y, x) \
    template<class ...Args> auto x(Args&& ...exprs) { return kaixo::expr{ \
        [...args = expr_wrapper<Args>{ std::forward<Args>(exprs) }]() mutable { return y x(args.get()...); } }; } \

#ifndef KAIXO_LC_ALGORITHMS
#define KAIXO_LC_ALGORITHMS 1
#endif
#if KAIXO_LC_ALGORITHMS == 1
#include <algorithm>
namespace kaixo {
    lc_std_fun(std::, adjacent_find);
    lc_std_fun(std::, binary_search);
    lc_std_fun(std::, bsearch);
    lc_std_fun(std::, clamp);
    lc_std_fun(std::, copy_backward);
    lc_std_fun(std::, copy_n);
    lc_std_fun(std::, count);
    lc_std_fun(std::, count_if);
    lc_std_fun(std::, equal);
    lc_std_fun(std::, equal_range);
    lc_std_fun(std::, fill);
    lc_std_fun(std::, fill_n);
    lc_std_fun(std::, find);
    lc_std_fun(std::, find_end);
    lc_std_fun(std::, find_first_of);
    lc_std_fun(std::, find_if);
    lc_std_fun(std::, find_if_not);
    lc_std_fun(std::, for_each);
    lc_std_fun(std::, for_each_n);
    lc_std_fun(std::, generate);
    lc_std_fun(std::, generate_n);
    lc_std_fun(std::, includes);
    lc_std_fun(std::, inplace_merge);
    lc_std_fun(std::, iter_swap);
    lc_std_fun(std::, lexicographical_compare);
    lc_std_fun(std::, lower_bound);
    lc_std_fun(std::, make_heap);
    lc_std_fun(std::, max);
    lc_std_fun(std::, max_element);
    lc_std_fun(std::, merge);
    lc_std_fun(std::, min);
    lc_std_fun(std::, min_element);
    lc_std_fun(std::, minmax);
    lc_std_fun(std::, minmax_element);
    lc_std_fun(std::, mismatch);
    lc_std_fun(std::, move);
    lc_std_fun(std::, move_backward);
    lc_std_fun(std::, next_permutation);
    lc_std_fun(std::, nth_element);
    lc_std_fun(std::, partial_sort);
    lc_std_fun(std::, partial_sort_copy);
    lc_std_fun(std::, partition);
    lc_std_fun(std::, partition_copy);
    lc_std_fun(std::, partition_point);
    lc_std_fun(std::, pop_heap);
    lc_std_fun(std::, prev_permutation);
    lc_std_fun(std::, push_heap);
    lc_std_fun(std::, qsort);
    lc_std_fun(std::, remove);
    lc_std_fun(std::, remove_copy);
    lc_std_fun(std::, replace);
    lc_std_fun(std::, replace_copy);
    lc_std_fun(std::, replace_copy_if);
    lc_std_fun(std::, reverse);
    lc_std_fun(std::, reverse_copy);
    lc_std_fun(std::, rotate);
    lc_std_fun(std::, rotate_copy);
    lc_std_fun(std::, sample);
    lc_std_fun(std::, search);
    lc_std_fun(std::, search_n);
    lc_std_fun(std::, shift_left);
    lc_std_fun(std::, shift_right);
    lc_std_fun(std::, set_difference);
    lc_std_fun(std::, set_intersection);
    lc_std_fun(std::, set_symmetric_difference);
    lc_std_fun(std::, set_union);
    lc_std_fun(std::, sort);
    lc_std_fun(std::, sort_heap);
    lc_std_fun(std::, stable_partition);
    lc_std_fun(std::, stable_sort);
    lc_std_fun(std::, swap);
    lc_std_fun(std::, swap_ranges);
    lc_std_fun(std::, transform);
    lc_std_fun(std::, unique);
    lc_std_fun(std::, unique_copy);
    lc_std_fun(std::, upper_bound);
}
#endif

#ifndef KAIXO_LC_ITERATOR
#define KAIXO_LC_ITERATOR 1
#endif
#if KAIXO_LC_ITERATOR == 1
#include <iterator>
namespace kaixo {
    lc_std_fun(std::, advance);
    lc_std_fun(std::, back_inserter);
    lc_std_fun(std::, begin);
    lc_std_fun(std::, data);
    lc_std_fun(std::, distance);
    lc_std_fun(std::, empty);
    lc_std_fun(std::, end);
    lc_std_fun(std::, front_inserter);
    lc_std_fun(std::, inserter);
    lc_std_fun(std::, make_move_iterator);
    lc_std_fun(std::, make_reverse_iterator);
    lc_std_fun(std::, next);
    lc_std_fun(std::, prev);
    lc_std_fun(std::, rbegin);
    lc_std_fun(std::, rend);
    lc_std_fun(std::, size);
}
#endif

#ifndef KAIXO_LC_MEMORY
#define KAIXO_LC_MEMORY 1
#endif
#if KAIXO_LC_MEMORY == 1
#include <memory>
#include <memory_resource>
namespace kaixo {
    lc_std_fun(std::, addressof);
    lc_std_fun(std::, align);
    lc_std_fun(std::, assume_aligned);
    lc_std_fun(std::, calloc);
    lc_std_fun(std::, free);
    lc_std_fun(std::, malloc);
    lc_std_fun(std::, realloc);
    lc_std_fun(std::, destroy);
    lc_std_fun(std::, destroy_at);
    lc_std_fun(std::, destroy_n);
    lc_std_fun(std::pmr::, get_default_resource);
    lc_std_fun(std::, make_obj_using_allocator);
    lc_std_fun(std::pmr::, new_delete_resource);
    lc_std_fun(std::pmr::, null_memory_resource);
    lc_std_fun(std::pmr::, pool_options);
    lc_std_fun(std::pmr::, set_default_resource);
    lc_std_fun(std::, to_address);
    lc_std_fun(std::, uninitialized_construct_using_allocator);
    lc_std_fun(std::, uninitialized_copy);
    lc_std_fun(std::, uninitialized_copy_n);
    lc_std_fun(std::, uninitialized_default_construct);
    lc_std_fun(std::, uninitialized_default_construct_n);
    lc_std_fun(std::, uninitialized_fill);
    lc_std_fun(std::, uninitialized_fill_n);
    lc_std_fun(std::, uninitialized_move);
    lc_std_fun(std::, uninitialized_move_n);
    lc_std_fun(std::, uninitialized_value_construct);
    lc_std_fun(std::, uninitialized_value_construct_n);
}
#endif

#ifndef KAIXO_LC_NUMERIC
#define KAIXO_LC_NUMERIC 1
#endif
#if KAIXO_LC_NUMERIC == 1
#include <numeric>
namespace kaixo {
    lc_std_fun(std::, accumulate);
    lc_std_fun(std::, adjacent_difference);
    lc_std_fun(std::, inclusive_scan);
    lc_std_fun(std::, inner_product);
    lc_std_fun(std::, iota);
    lc_std_fun(std::, reduce);
    lc_std_fun(std::, partial_sum);
    lc_std_fun(std::, transform_exclusive_scan);
    lc_std_fun(std::, transform_inclusive_scan);
    lc_std_fun(std::, transform_reduce);
    
    lc_std_fun(std::, bit_cast);
    lc_std_fun(std::, gcd);
    lc_std_fun(std::, lcm);
    lc_std_fun(std::, lerp);
    lc_std_fun(std::, abs);
    lc_std_fun(std::, acos);
    lc_std_fun(std::, acosh);
    lc_std_fun(std::, asin);
    lc_std_fun(std::, asinh);
    lc_std_fun(std::, atan);
    lc_std_fun(std::, atan2);
    lc_std_fun(std::, atanh);
    lc_std_fun(std::, cbrt);
    lc_std_fun(std::, ceil);
    lc_std_fun(std::, copysign);
    lc_std_fun(std::, cos);
    lc_std_fun(std::, cosh);
    lc_std_fun(std::, div);
    lc_std_fun(std::, erf);
    lc_std_fun(std::, erfc);
    lc_std_fun(std::, exp);
    lc_std_fun(std::, exp2);
    lc_std_fun(std::, expm1);
    lc_std_fun(std::, fabs);
    lc_std_fun(std::, fdim);
    lc_std_fun(std::, floor);
    lc_std_fun(std::, fma);
    lc_std_fun(std::, fmax);
    lc_std_fun(std::, fmin);
    lc_std_fun(std::, fmod);
    lc_std_fun(std::, fpclassify);
    lc_std_fun(std::, frexp);
    lc_std_fun(std::, hypot);
    lc_std_fun(std::, ilogb);
    lc_std_fun(std::, isfinite);
    lc_std_fun(std::, isgreater);
    lc_std_fun(std::, isgreaterequal);
    lc_std_fun(std::, isinf);
    lc_std_fun(std::, isless);
    lc_std_fun(std::, islessequal);
    lc_std_fun(std::, islessgreater);
    lc_std_fun(std::, isnan);
    lc_std_fun(std::, isnormal);
    lc_std_fun(std::, isunordered);
    lc_std_fun(std::, ldexp);
    lc_std_fun(std::, lgamma);
    lc_std_fun(std::, log);
    lc_std_fun(std::, log10);
    lc_std_fun(std::, log1p);
    lc_std_fun(std::, log2);
    lc_std_fun(std::, logb);
    lc_std_fun(std::, modf);
    lc_std_fun(std::, nan);
    lc_std_fun(std::, nearbyint);
    lc_std_fun(std::, nextafter);
    lc_std_fun(std::, pow);
    lc_std_fun(std::, remainder);
    lc_std_fun(std::, remquo);
    lc_std_fun(std::, rint);
    lc_std_fun(std::, round);
    lc_std_fun(std::, scalbn);
    lc_std_fun(std::, signbit);
    lc_std_fun(std::, sin);
    lc_std_fun(std::, sinh);
    lc_std_fun(std::, sqrt);
    lc_std_fun(std::, tan);
    lc_std_fun(std::, tanh);
    lc_std_fun(std::, tgamma);
    lc_std_fun(std::, trunc);
    lc_std_fun(std::, midpoint);
    lc_std_fun(std::, assoc_laguerre);
    lc_std_fun(std::, assoc_legendre);
    lc_std_fun(std::, beta);
    lc_std_fun(std::, comp_ellint_1);
    lc_std_fun(std::, comp_ellint_2);
    lc_std_fun(std::, comp_ellint_3);
    lc_std_fun(std::, cyl_bessel_i);
    lc_std_fun(std::, cyl_bessel_j);
    lc_std_fun(std::, cyl_bessel_k);
    lc_std_fun(std::, cyl_neumann);
    lc_std_fun(std::, ellint_1);
    lc_std_fun(std::, ellint_2);
    lc_std_fun(std::, ellint_3);
    lc_std_fun(std::, expint);
    lc_std_fun(std::, hermite);
    lc_std_fun(std::, laguerre);
    lc_std_fun(std::, legendre);
    lc_std_fun(std::, riemann_zeta);
    lc_std_fun(std::, sph_bessel);
    lc_std_fun(std::, sph_legendre);
    lc_std_fun(std::, sph_neumann);
}
#endif

#ifndef KAIXO_LC_STRING
#define KAIXO_LC_STRING 1
#endif
#if KAIXO_LC_STRING == 1
#include <string>
#include <cstring>
#include <cwctype>
#include <cuchar>
namespace kaixo {
    lc_std_fun(std::, atof);
    lc_std_fun(std::, atoi);
    lc_std_fun(std::, isalnum);
    lc_std_fun(std::, isalpha);
    lc_std_fun(std::, isblank);
    lc_std_fun(std::, iscntrl);
    lc_std_fun(std::, isdigit);
    lc_std_fun(std::, isgraph);
    lc_std_fun(std::, islower);
    lc_std_fun(std::, isprint);
    lc_std_fun(std::, ispunct);
    lc_std_fun(std::, isspace);
    lc_std_fun(std::, isupper);
    lc_std_fun(std::, isxdigit);
    lc_std_fun(std::, memchr);
    lc_std_fun(std::, memcmp);
    lc_std_fun(std::, memcpy);
    lc_std_fun(std::, memmove);
    lc_std_fun(std::, memset);
    lc_std_fun(std::, strcat);
    lc_std_fun(std::, strchr);
    lc_std_fun(std::, strcmp);
    lc_std_fun(std::, strcoll);
    lc_std_fun(std::, strcpy);
    lc_std_fun(std::, strcspn);
    lc_std_fun(std::, strerror);
    lc_std_fun(std::, strlen);
    lc_std_fun(std::, strncat);
    lc_std_fun(std::, strncmp);
    lc_std_fun(std::, strncpy);
    lc_std_fun(std::, strpbrk);
    lc_std_fun(std::, strrchr);
    lc_std_fun(std::, strspn);
    lc_std_fun(std::, strstr);
    lc_std_fun(std::, strtof);
    lc_std_fun(std::, strtok);
    lc_std_fun(std::, strtol);
    lc_std_fun(std::, strtoul);
    lc_std_fun(std::, strxfrm);
    lc_std_fun(std::, tolower);
    lc_std_fun(std::, toupper);
    lc_std_fun(std::, copy);
    lc_std_fun(std::, btowc);
    lc_std_fun(std::, c16rtomb);
    lc_std_fun(std::, c32rtomb);
    lc_std_fun(std::, mblen);
    lc_std_fun(std::, mbrlen);
    lc_std_fun(std::, mbrtoc16);
    lc_std_fun(std::, mbrtoc32);
    lc_std_fun(std::, mbrtowc);
    lc_std_fun(std::, mbsinit);
    lc_std_fun(std::, mbsrtowcs);
    lc_std_fun(std::, mbstowcs);
    lc_std_fun(std::, mbtowc);
    lc_std_fun(std::, wcrtomb);
    lc_std_fun(std::, wcsrtombs);
    lc_std_fun(std::, wcstombs);
    lc_std_fun(std::, wctob);
    lc_std_fun(std::, wctomb);
    lc_std_fun(std::, iswalnum);
    lc_std_fun(std::, iswalpha);
    lc_std_fun(std::, iswblank);
    lc_std_fun(std::, iswcntrl);
    lc_std_fun(std::, iswctype);
    lc_std_fun(std::, iswdigit);
    lc_std_fun(std::, iswgraph);
    lc_std_fun(std::, iswlower);
    lc_std_fun(std::, iswprint);
    lc_std_fun(std::, iswpunct);
    lc_std_fun(std::, iswspace);
    lc_std_fun(std::, iswupper);
    lc_std_fun(std::, iswxdigit);
    lc_std_fun(std::, towctrans);
    lc_std_fun(std::, towlower);
    lc_std_fun(std::, towupper);
    lc_std_fun(std::, wcscat);
    lc_std_fun(std::, wcschr);
    lc_std_fun(std::, wcscmp);
    lc_std_fun(std::, wcscoll);
    lc_std_fun(std::, wcscpy);
    lc_std_fun(std::, wcscspn);
    lc_std_fun(std::, wcslen);
    lc_std_fun(std::, wcsncat);
    lc_std_fun(std::, wcsncmp);
    lc_std_fun(std::, wcsncpy);
    lc_std_fun(std::, wcspbrk);
    lc_std_fun(std::, wcsrchr);
    lc_std_fun(std::, wcsspn);
    lc_std_fun(std::, wcsstr);
    lc_std_fun(std::, wcstof);
    lc_std_fun(std::, wcstok);
    lc_std_fun(std::, wcstol);
    lc_std_fun(std::, wcstoul);
    lc_std_fun(std::, wcsxfrm);
    lc_std_fun(std::, wctrans);
    lc_std_fun(std::, wctype);
    lc_std_fun(std::, wmemchr);
    lc_std_fun(std::, wmemcmp);
    lc_std_fun(std::, wmemcpy);
    lc_std_fun(std::, wmemmove);
    lc_std_fun(std::, wmemset);
}
#endif

#undef lc_std_fun
#undef lc_mem_fun