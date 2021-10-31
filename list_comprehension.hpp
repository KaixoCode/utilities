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
#include "utils.hpp"

namespace kaixo {
    template<class Type>
    using container_for = std::conditional_t<std::is_same_v<Type, char>, std::string, std::vector<Type>>;

    template<class Container, class ...Types>
    struct container_syntax;

    /**
     * An expression is basically just nested lambdas that all execute some operator on
     * either another expression, a var, or a value.
     */
    template<class Type>
    struct expression;

    template<class Type>
    struct var;
    template<class Type>
    struct expression_base {
        using type = Type;
        expression_base() = default;
        expression_base(std::function<type()> f) : c(f) {};
        expression_base(const std::decay_t<Type>& f) : c([&]() { return f; }) {};
        expression_base(std::decay_t<Type>&& f) : c([f = std::move(f)]() { return f; }) {};
        expression_base(std::decay_t<Type>& f) : c([&]() { return f; }) {};

        std::function<type()> c;
        explicit operator type() const { return c(); }
        type operator()() const { return c(); }

        template<class Ty>
        expression<Ty> to()&& { return { [c = this->c] () { return static_cast<Ty>(c()); } }; }

        template<class Ty>
        expression<Ty> to()& { return { [this] () { return static_cast<Ty>(this->c()); } }; }
    };

    template<class Type>
    struct expression : expression_base<Type> {
        using type = Type;
        using expression_base<Type>::expression_base;
    };

    template<class>
    struct return_type;

    template<class R, class...T>
    struct return_type<R(T...)> {
        using type = R;
    };

    template<class Type>
    expression(Type)->expression<typename return_type<lambda_signature_t<Type>>::type>;

    template<class Type>
    struct expression_wrapper {
        Type value;
        auto get() { return value; }
    };

    template<class Type>
    struct expression_wrapper<var<Type>> {
        var<Type> value;
        auto get() { return value(); }
    };

    template<class Type>
    struct expression_wrapper<var<Type>&&> {
        var<Type> value;
        auto get() { return value(); }
    };

    template<class Type>
    struct expression_wrapper<var<Type>&> {
        var<Type>& value;
        auto get() { return value(); }
    };

    template<class Type>
    struct expression_wrapper<expression<Type>&&> {
        expression<Type> value;
        auto get() { return value(); }
    };

    template<class Type>
    struct expression_wrapper<expression<Type>> {
        expression<Type> value;
        auto get() { return value(); }
    };

    template<class Type>
    struct expression_wrapper<expression<Type>&> {
        expression<Type>& value;
        auto get() { return value(); }
    };

    /**
     * A var is an expression with a simple assignment, a var's lifetime must be longer than the
     * list comprehension object that is generated, because it is stored as a reference.
     */
    template<class Type>
    struct var : expression<Type> {
        using type = Type;

        var() { this->c = []() { return Type{}; }; }
        using expression<Type>::expression;

        var& operator=(const Type& t) { this->c = [&]() { return t; }; return *this; }
        var& operator=(Type& t) { this->c = [&]() { return t; }; return *this; }
        var& operator=(Type&& t) { this->c = [t = std::move(t)]() { return std::move(t); }; return *this; }
    };

#define lc_mem_fun(y, x) \
    template<class ...Args> \
    auto x(Args&& ...exprs) const& { \
        return kaixo::expression{ [this, ...args = expression_wrapper<Args>{ std::forward<Args>(exprs) }]() mutable { return this->c().y::x(args.get()...); } }; } \
    template<class ...Args> \
    auto x(Args&& ...exprs) const&& { \
        return kaixo::expression{ [c = this->c, ...args = expression_wrapper<Args>{ std::forward<Args>(exprs) }]() mutable { return c().y::x(args.get()...); } }; } \

    template<>
    struct expression<std::string> : expression_base<std::string> {
        using type = std::string;
        using expression_base<type>::expression_base;

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

    template<class ...Args>
    struct expression<std::tuple<Args...>> : expression_base<std::tuple<Args...>> {
        using type = std::tuple<Args...>;
        using expression_base<type>::expression_base;

        lc_mem_fun(type, swap);

        template<size_t N>
        expression<std::decay_t<decltype(std::get<N>(std::declval<std::tuple<Args...>>()))>>get()&& {
            return { [c = this->c] () { return std::get<N>(c()); } };
        }

        template<size_t N>
        expression<std::decay_t<decltype(std::get<N>(std::declval<std::tuple<Args...>>()))>>get()& {
            return { [this]() { return std::get<N>(this->c()); } };
        }
    };

    template<class ...Args>
    struct expression<std::vector<Args...>> : expression_base<std::vector<Args...>> {
        using type = std::vector<Args...>;
        using expression_base<type>::expression_base;

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

    template<class ...Args>
    struct expression<std::deque<Args...>> : expression_base<std::deque<Args...>> {
        using type = std::deque<Args...>;
        using expression_base<type>::expression_base;

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

    template<class Arg, size_t N>
    struct expression<std::array<Arg, N>> : expression_base<std::array<Arg, N>> {
        using type = std::array<Arg, N>;
        using expression_base<type>::expression_base;

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


    /**
     * Macro to define operator overloads for the expression/var/value combinations.
     */
#define var_op(x)\
    template<class A, class B> auto operator x(var<A>& a, var<B>& b) { return expression<decltype(a() x b())>{ [&a, &b]() { return a() x b(); } }; } \
    template<class A, class B> auto operator x(const expression<A>& a, var<B>& b) { return expression<decltype(a() x b())>{ [a, &b]() { return a() x b(); } }; } \
    template<class A, class B> auto operator x(var<A>& a, const expression<B>& b) { return expression<decltype(a() x b())>{ [&a, b]() { return a() x b(); } }; } \
    template<class A, class B> auto operator x(const expression<A>& a, const expression<B>& b) { return expression<decltype(a() x b())>{ [a, b]() { return a() x b(); } }; } \
    template<class A, class B> auto operator x(const expression<A>& a, const B& b) { return expression<decltype(a() x b)>{ [a, b]() { return a() x b; } }; } \
    template<class A, class B> auto operator x(const A& a, const expression<B>& b) { return expression<decltype(a x b())>{ [a, b]() { return a x b(); } }; } \
    template<class A, class B> auto operator x(var<A>& a, const B& b) { return expression<decltype(a() x b)>{ [&a, b]() { return a() x b; } }; } \
    template<class A, class B> auto operator x(const A& a, var<B>& b) { return expression<decltype(a x b())>{ [a, &b]() { return a x b(); } }; }

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
    template<class A> auto operator x(var<A>& a) { return expression<decltype(x a())>{ [&a]() { return x a(); } }; } \
    template<class A> auto operator x(const expression<A>& a) { return expression<decltype(x a())>{ [a]() { return x a(); } }; } \

    u_var_op(-);
    u_var_op(+);
    u_var_op(~);
    u_var_op(!);
    u_var_op(*);
    u_var_op(&);

    /**
     * Simple range class with a start and end, plus an iterator because
     * list comprehension uses iterators to create the cartesian product of all sets.
     */
    template<class Type>
    struct range {
        using value_type = Type;
        range(Type&& x, Type&& y) { init(a, std::move(x)); init(b, std::move(y)); }
        range(Type& x, Type&& y) { init(a, x); init(b, std::move(y)); }
        range(Type&& x, Type& y) { init(a, std::move(x)); init(b, y); }
        range(Type& x, Type& y) { init(a, x); init(b, y); }
        range(Type&& x, const expression<Type>& y) { init(a, std::move(x)); init(b, y); }
        range(Type& x, const expression<Type>& y) { init(a, x); init(b, y); }
        range(Type&& x, var<Type>& y) { init(a, std::move(x)); init(b, y); }
        range(Type& x, var<Type>& y) { init(a, x); init(b, y); }
        range(var<Type>& x, Type&& y) { init(a, x); init(b, std::move(y)); }
        range(const expression<Type>& x, Type&& y) { init(a, x); init(b, std::move(y)); }
        range(var<Type>& x, Type& y) { init(a, x); init(b, y); }
        range(const expression<Type>& x, Type& y) { init(a, x); init(b, y); }
        range(var<Type>& x, var<Type>& y) { init(a, x); init(b, y); }
        range(const expression<Type>& x, var<Type>& y) { init(a, x); init(b, y); }
        range(var<Type>& x, const expression<Type>& y) { init(a, x); init(b, y); }
        range(const expression<Type>& x, const expression<Type>& y) { init(a, x); init(b, y); }

        expression<Type> a, b;
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
            iterator operator++(int) { return { r->a() <= r->b() ? cur + 1 : cur - 1 }; }
            iterator operator--(int) { return { r->a() <= r->b() ? cur - 1 : cur + 1 }; }
            Type& operator*() { return cur; }
            Type* operator&() { return &cur; }
            bool operator==(const iterator& o) const { return o.cur == cur; }
        };

        iterator begin() { return { this, a() }; }
        iterator end() { return { this, b() }; }
        iterator begin() const { return { this, a() }; }
        iterator end() const { return { this, b() }; }
        size_t size() const { return b() - a(); }
    private:
        void init(expression<Type>& a, var<Type>& t) { a.c = [&t]() { return t(); }; }
        void init(expression<Type>& a, const expression<Type>& t) { a.c = t.c; }
        void init(expression<Type>& a, Type&& t) { a.c = [t = std::move(t)]() { return t; }; }
        void init(expression<Type>& a, const Type& t) { a.c = [&t]() { return t; }; }
    };

    template<class Type>
    range(Type, Type)->range<Type>;

    template<class Type>
    concept has_begin_end = requires(Type a) {
        a.begin(), a.end(), a.size();
    };

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
        using type = Type;
        std::reference_wrapper<var<Type>> variable;
        Container container;
        using iterator = decltype(container.begin());

        iterator begin() { return container.begin(); }
        iterator end() { return container.end(); }
        size_t size() const { return container.size(); }
    };

    /**
     * the '-' and '<' operators are used to create a '<-' operator, the unary '-'
     * creates a container from some class that defines a begin() and end(), and the '<'
     * links that container to a variable.
     */
    template<template<class...> class Container, class ...Type>
    container<typename Container<Type...>::value_type, Container<Type...>&> operator-(Container<Type...>& r) { return { r }; }
    template<template<class...> class Container, class ...Type>
    container<typename Container<Type...>::value_type, Container<Type...>> operator-(Container<Type...>&& r) { return { std::move(r) }; }
    template<class Type, class CType, class Container>
    linked_container<Type, container<CType, Container>> operator<(var<Type>& v, const container<CType, Container>& r) { return { v, r }; }

    template<class Type, class ...Tys>
    concept has_emplace_back = requires(Type t, Tys...tys) { t.emplace_back(tys...); };
    
    template<class Type, class ...Tys>
    concept has_try_emplace = requires(Type t, Tys...tys) { t.try_emplace(tys...); };

    /**
     * This is the part before the '|', and defined what type of container the
     * result will be stored in. Also contains a generate method to easily generate an entry
     * for the resulting container given the values currently in the variables.
     */
    template<class Container, class ...Types>
    struct container_syntax {
        using container = Container;
        std::tuple<expression<Types>...> expressions;

        template<class T>
        inline void generate(T& container) { return m_Gen(container, std::make_index_sequence<sizeof...(Types)>{}); }

        template<class T, size_t ...Is>
        inline void m_Gen(T& container, std::index_sequence<Is...>) {
            if constexpr (std::same_as<std::string, T>)
                container.push_back(std::get<Is>(expressions).c()...);
            else if constexpr (has_emplace_back<T, typename expression<Types>::type...>)
                container.emplace_back(std::get<Is>(expressions).c()...);
            else if constexpr (has_try_emplace<T, typename expression<Types>::type...>)
                container.try_emplace(std::get<Is>(expressions).c()...);
            else
                container.emplace(std::get<Is>(expressions).c()...);
        }
    };

    /**
     * Everything combined into a single object, contains the container syntax, all
     * the linked containers, and also some constraints, which are expressions that evaluate to 'bool'
     */
    template<class ContainerSyntax, class ...LinkedContainers>
    struct list_comprehension {
        using container = ContainerSyntax::container;
        constexpr static auto size = sizeof...(LinkedContainers);
        constexpr static auto sequence = std::make_index_sequence<size>{};
        ContainerSyntax syntax;
        std::tuple<LinkedContainers...> containers;
        std::vector<expression<bool>> constraints;

        container operator*() { return get(); }

        container get() {
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
                    if (!check_end(its, index, sequence))
                        increment(its, index, sequence); // Otherwise increment the iterator and loop to check if also at the end.
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
            ((std::get<Is>(tuple) = std::get<Is>(containers).begin(), std::get<Is>(containers).variable.get() = *std::get<Is>(containers).begin()), ...);
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
            ((std::get<Is>(containers).variable.get() = *std::get<Is>(tuple)), ...);
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
     * Operators for constructing a container syntax.
     */
    template<class A, class B>
    container_syntax<std::vector<std::tuple<A, B>>, A, B> operator,(var<A>& a, var<B>& b) {
        return { { { [&]() { return a(); } }, { [&]() { return b(); } } } };
    }

    template<class A, class B>
    container_syntax<std::vector<std::tuple<A, B>>, A, B> operator,(const expression<A>& a, var<B>& b) {
        return { { a, { [&]() { return b(); } } } };
    }

    template<class A, class B>
    container_syntax<std::vector<std::tuple<A, B>>, A, B> operator,(var<A>& a, const expression<B>& b) {
        return { { { [&]() { return a(); } }, b } };
    }

    template<class A, class B>
    container_syntax<std::vector<std::tuple<A, B>>, A, B> operator,(const expression<A>& a, const expression<B>& b) {
        return { { a, b } };
    }

    template<class ...Types>
    container_syntax<std::vector<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> vector(Types&& ...tys) {
        return { { (std::is_lvalue_reference_v<Types> ? expression<typename std::decay_t<Types>::type>{ [&]() { return tys(); } } : tys)... } };
    }

    template<class ...Types>
    container_syntax<std::list<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> list(Types&& ...tys) {
        return { { (std::is_lvalue_reference_v<Types> ? expression<typename std::decay_t<Types>::type>{ [&]() { return tys(); } } : tys)... } };
    }

    template<class ...Types>
    container_syntax<std::forward_list<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> forward_list(Types&& ...tys) {
        return { { (std::is_lvalue_reference_v<Types> ? expression<typename std::decay_t<Types>::type>{ [&]() { return tys(); } } : tys)... } };
    }

    template<class ...Types>
    container_syntax<std::deque<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> deque(Types&& ...tys) {
        return { { (std::is_lvalue_reference_v<Types> ? expression<typename std::decay_t<Types>::type>{ [&]() { return tys(); } } : tys)... } };
    }
    
    template<class ...Types>
    container_syntax<std::stack<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> stack(Types&& ...tys) {
        return { { (std::is_lvalue_reference_v<Types> ? expression<typename std::decay_t<Types>::type>{ [&]() { return tys(); } } : tys)... } };
    }
    
    template<class ...Types>
    container_syntax<std::queue<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> queue(Types&& ...tys) {
        return { { (std::is_lvalue_reference_v<Types> ? expression<typename std::decay_t<Types>::type>{ [&]() { return tys(); } } : tys)... } };
    }
    
    template<class ...Types>
    container_syntax<std::priority_queue<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> priority_queue(Types&& ...tys) {
        return { { (std::is_lvalue_reference_v<Types> ? expression<typename std::decay_t<Types>::type>{ [&]() { return tys(); } } : tys)... } };
    }

    template<class ...Types>
    container_syntax<std::set<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> set(Types&& ...tys) {
        return { { (std::is_lvalue_reference_v<Types> ? expression<typename std::decay_t<Types>::type>{ [&]() { return tys(); } } : tys)... } };
    }

    template<class ...Types>
    container_syntax<std::multiset<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> multiset(Types&& ...tys) {
        return { { (std::is_lvalue_reference_v<Types> ? expression<typename std::decay_t<Types>::type>{ [&]() { return tys(); } } : tys)... } };
    }

    template<class ...Types>
    container_syntax<std::unordered_set<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> unordered_set(Types&& ...tys) {
        return { { (std::is_lvalue_reference_v<Types> ? expression<typename std::decay_t<Types>::type>{ [&]() { return tys(); } } : tys)... } };
    }
    
    template<class ...Types>
    container_syntax<std::unordered_multiset<std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>,
        std::tuple<typename std::decay_t<Types>::type...>>>, typename std::decay_t<Types>::type...> unordered_multiset(Types&& ...tys) {
        return { { (std::is_lvalue_reference_v<Types> ? expression<typename std::decay_t<Types>::type>{ [&]() { return tys(); } } : tys)... } };
    }

    template<class Ty, class ...Types>
    container_syntax<std::map<typename std::decay_t<Ty>::type, std::conditional_t<sizeof...(Types) == 1, 
        std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>, std::tuple<typename std::decay_t<Types>::type...>>>, 
        typename std::decay_t<Ty>::type, typename std::decay_t<Types>::type...> map(Ty&& ty, Types&& ...tys) {
        return { { (std::is_lvalue_reference_v<Ty> ? expression<typename std::decay_t<Ty>::type>{ [&]() { return ty(); } } : ty),
                (std::is_lvalue_reference_v<Types> ? expression<typename std::decay_t<Types>::type>{ [&]() { return tys(); } } : tys)... } };
    }

    template<class Ty, class ...Types>
    container_syntax<std::multimap<typename std::decay_t<Ty>::type, std::conditional_t<sizeof...(Types) == 1,
        std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>, std::tuple<typename std::decay_t<Types>::type...>>>,
        typename std::decay_t<Ty>::type, typename std::decay_t<Types>::type...> multimap(Ty&& ty, Types&& ...tys) {
        return { { (std::is_lvalue_reference_v<Ty> ? expression<typename std::decay_t<Ty>::type>{ [&]() { return ty(); } } : ty),
                (std::is_lvalue_reference_v<Types> ? expression<typename std::decay_t<Types>::type>{ [&]() { return tys(); } } : tys)... } };
    }

    template<class Ty, class ...Types>
    container_syntax<std::unordered_map<typename std::decay_t<Ty>::type, std::conditional_t<sizeof...(Types) == 1,
        std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>, std::tuple<typename std::decay_t<Types>::type...>>>,
        typename std::decay_t<Ty>::type, typename std::decay_t<Types>::type...> unordered_map(Ty&& ty, Types&& ...tys) {
        return { { (std::is_lvalue_reference_v<Ty> ? expression<typename std::decay_t<Ty>::type>{ [&]() { return ty(); } } : ty),
                (std::is_lvalue_reference_v<Types> ? expression<typename std::decay_t<Types>::type>{ [&]() { return tys(); } } : tys)... } };
    }
    
    template<class Ty, class ...Types>
    container_syntax<std::unordered_multimap<typename std::decay_t<Ty>::type, std::conditional_t<sizeof...(Types) == 1,
        std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::type...>>, std::tuple<typename std::decay_t<Types>::type...>>>,
        typename std::decay_t<Ty>::type, typename std::decay_t<Types>::type...> unordered_multimap(Ty&& ty, Types&& ...tys) {
        return { { (std::is_lvalue_reference_v<Ty> ? expression<typename std::decay_t<Ty>::type>{ [&]() { return ty(); } } : ty),
                (std::is_lvalue_reference_v<Types> ? expression<typename std::decay_t<Types>::type>{ [&]() { return tys(); } } : tys)... } };
    }

    template<class A, class ...Rest>
    container_syntax<std::vector<std::tuple<Rest..., A>>, Rest..., A> operator,(container_syntax<std::vector<std::tuple<Rest...>>, Rest...>&& a, var<A>& b) {
        return { std::tuple_cat(a.expressions, std::tuple{ expression<A>{ [&]() { return b(); } } }) };
    }

    template<class A, class ...Rest>
    container_syntax<std::vector<std::tuple<Rest..., A>>, Rest..., A> operator,(container_syntax<std::vector<std::tuple<Rest...>>, Rest...>&& a, const expression<A>& b) {
        return { std::tuple_cat(a.expressions, std::tuple{ b }) };
    }

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
        operator|(var<Type>& v, linked_container<CType, Container>&& c) {
        return { container_syntax<container_for<std::decay_t<Type>>, Type>{ expression<Type>{ [&]() { return v(); } } }, std::move(c), {} };
    }

    template<class Type, class Container, class CType>
    list_comprehension<container_syntax<container_for<std::decay_t<Type>>, Type>, linked_container<CType, Container>>
        operator|(const expression<Type>& v, linked_container<CType, Container>&& c) {
        return { container_syntax<container_for<std::decay_t<Type>>, Type>{ v }, std::move(c), {} };
    }

    /**
     * Operators for expanding the initial list comprehension with linked containers or constraints.
     */
    template<class ContainerSyntax, class Container, class CType, class ...LinkedContainers>
    list_comprehension<ContainerSyntax, LinkedContainers..., linked_container<CType, Container>>
        operator,(list_comprehension<ContainerSyntax, LinkedContainers...>&& v, linked_container<CType, Container>&& c) {
        return { v.syntax, std::tuple_cat(v.containers, std::tuple{ c }), v.constraints };
    }

    template<class ContainerSyntax, class ...LinkedContainers>
    list_comprehension<ContainerSyntax, LinkedContainers...>
        operator,(list_comprehension<ContainerSyntax, LinkedContainers...>&& v, expression<bool>&& c) {
        v.constraints.push_back(c);
        return v;
    }

    template<std::convertible_to<bool> Type, class ContainerSyntax, class ...LinkedContainers>
    list_comprehension<ContainerSyntax, LinkedContainers...>
        operator,(list_comprehension<ContainerSyntax, LinkedContainers...>&& v, expression<Type>&& c) {
        v.constraints.push_back({ [c = std::move(c)] () { return static_cast<bool>(c()); } });
        return v;
    }

    /**
     * Used for parallel iteration of containers.
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

        size_t size() const { return seq_size(std::make_index_sequence<sizeof...(Containers)>{}); }
        iterator begin() { return seq_begin(std::make_index_sequence<sizeof...(Containers)>{}); };
        iterator end() { return seq_end(std::make_index_sequence<sizeof...(Containers)>{}); };

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
        auto operator[](list_comprehension<ContainerSyntax, LinkedContainers...>&& l) { return l.get(); };
    } lc;

#define lc_std_fun(y, x) \
    template<class ...Args> \
    auto x(Args&& ...exprs) { \
        return kaixo::expression{ [...args = expression_wrapper<Args>{ std::forward<Args>(exprs) }]() mutable { return y x(args.get()...); } }; \
    }
}

#define LC_ALGORITHMS
#ifdef LC_ALGORITHMS
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

#define LC_ITERATOR
#ifdef LC_ITERATOR
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

#define LC_MEMORY
#ifdef LC_MEMORY
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

#define LC_NUMERIC
#ifdef LC_NUMERIC
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

#define LC_STRING
#ifdef LC_STRING
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