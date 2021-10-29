#include <vector>
#include <functional>
#include <iterator>
#include <tuple>

namespace kaixo {

    /**
     * An expression is basically just nested lambdas that all execute some operator on 
     * either another expression, a var, or a value.
     */
    template<class Type>
    struct var;
    template<class Type>
    struct expression {
        using type = Type;
        std::function<type()> c;
        explicit operator type() const { return c(); }
        type operator()() const { return c(); }
    };

    /**
     * A var is an expression with a simple assignment, a var's lifetime must be longer than the 
     * list comprehension object that is generated, because it is stored as a reference.
     */
    template<class Type>
    struct var_base : expression<Type> {
        using type = Type;
        void operator=(Type& t) { this->c = [&]() { return t; }; }
        void operator=(Type&& t) { this->c = [=]() { return t; }; }
    };

    template<class Type>
    struct var : var_base<Type> {
        using type = Type;
        using var_base<Type>::operator=;
        using var_base<Type>::operator();
    };

    template<>
    struct var<std::string> : var_base<std::string> {
        using var_base<std::string>::operator=;
#define m(x) \
        template<class ...Args> \
        expression<bool> x(Args&&...args) { return { [this, ...args = std::forward<Args>(args)] () { return (*this)().x(args...); } }; }
        m(at)            m(operator[])  m(front)     m(back)
        m(data)          m(c_str)       m(begin)     m(cbegin)
        m(end)           m(cend)        m(rbegin)    m(crbegin)
        m(rend)          m(crend)       m(empty)     m(size)
        m(length)        m(max_size)    m(reserve)   m(capacity)
        m(shrink_to_fit) m(clear)       m(insert)    m(erase)
        m(push_back)     m(pop_back)    m(append)    m(operator+=)
        m(compare)       m(starts_with) m(ends_with) m(contains)
        m(replace)       m(substr)      m(copy)      m(resize)
        m(swap)          m(find)        m(rfind)     m(find_first_of)
        m(find_first_not_of)
        m(find_last_of)
        m(find_last_not_of)
#undef m;
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
    var_op(/);
    var_op(*);
    var_op(%);
    var_op(==);
    var_op(!=);
    var_op(<=);
    var_op(>=);
    var_op(>);
    var_op(<);
    var_op(<=>);
    var_op(&&);
    var_op(||);
    var_op(<<);
    var_op(>>);

    /**
     * Simple range class with a start and end, plus an iterator because
     * list comprehension uses iterators to create the cartesian product of all sets.
     */
    template<class Type>
    struct range {
        Type a, b;
        struct iterator {
            using iterator_category = std::bidirectional_iterator_tag;
            using value_type = Type;
            using difference_type = std::ptrdiff_t;
            using pointer = Type*;
            using reference = Type&;

            Type cur;
            iterator& operator++() { ++cur; return *this; }
            iterator& operator--() { --cur; return *this; }
            iterator operator++(int) { return { cur + 1 }; }
            iterator operator--(int) { return { cur - 1 }; }
            Type& operator*() { return cur; }
            Type* operator&() { return &cur; }
            bool operator==(const iterator& o) const { return o.cur == cur; }
        };

        iterator begin() { return { a }; }
        iterator end() { return { b }; }
    };

    template<class Type>
    range(Type, Type)->range<Type>;

    /**
     * Wrapper for a container, used when creating a cartesian product, works
     * with any class that defines a begin and end method.
     */
    template<class Type, class Container>
    struct container {
        using type = Type;
        Container container;

        auto begin() { return container.begin(); }
        auto end() { return container.end(); }
    };

    /**
     * A struct that links a var to a container, this is stored in the final
     * link comprehension object.
     */
    template<class Type, class Container>
    struct linked_container {
        using type = Type;
        std::reference_wrapper<var<Type>> variable;
        container<Type, Container> container;
        using iterator = decltype(container.begin());
    };

    /**
     * the '-' and '<' operators are used to create a '<-' operator, the unary '-'
     * creates a container from some class that defines a begin() and end(), and the '<'
     * links that container to a variable.
     */
    template<template<class...> class Container, class Type>
    container<Type, Container<Type>> operator-(const Container<Type>& r) { return { r }; }
    template<class Type, class Container>
    linked_container<Type, Container> operator<(var<Type>& v, container<Type, Container>&& r) { return { v, std::move(r) }; }

    /**
     * This is the part before the '|', and defined what type of container the 
     * result will be stored in. Also contains a generate method to easily generate an entry
     * for the resulting container given the values currently in the variables.
     */
    template<class Container, class ...Types>
    struct container_syntax {
        using container = Container;
        using generated_type = std::conditional_t<sizeof...(Types) == 1, std::tuple_element_t<0, std::tuple<Types...>>, std::tuple<Types...>>;
        std::tuple<expression<Types>...> expressions;

        generated_type generate() { return m_Gen(std::make_index_sequence<sizeof...(Types)>{}); }

        template<size_t ...Is>
        generated_type m_Gen(std::index_sequence<Is...>) {
            return { std::get<Is>(expressions)()... };
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

            int index = 0;
            bool done = false;
            while (!done) { // This will loop through all the values in the cartesian product of the linked containers.
                set_values(its, sequence); // Set all vars to the values that the iterators point to.

                bool _match = true;
                for (auto& c : constraints) // Check all constraints.
                    _match &= c();

                if (_match) // If all matched, generate an entry, and add to result.
                    result.push_back(syntax.generate());

                increment(its, index, sequence); // Increment the iterator
                while (check_end(its, index, sequence)) { // And check if it's now at the end.
                    set_begin(its, index, sequence); // Reset the iterator
                    index++;                         // And go to the next index to increment that one.
                    if (index == size) {             // If we're at the end, we're done.
                        done = true;
                        break;
                    }
                    increment(its, index, sequence); // Otherwise increment the iterator and loop to check if also at the end.
                }

                index = 0; // Reset index back to 0 for the next iteration.
            }

            return result;
        }

        /**
         * Some helper functions because dealing with tuples
         * is a living nightmare when also working with changing indices...
         */
        template<class T, std::size_t ...Is>
        void set_begin(T& tuple, std::index_sequence<Is...>) {
            ((std::get<Is>(tuple) = std::get<Is>(containers).container.begin()), ...);
        }

        template<class T, std::size_t ...Is>
        void set_begin(T& tuple, std::size_t i, std::index_sequence<Is...>) {
            ((Is == i ? (std::get<Is>(tuple) = std::get<Is>(containers).container.begin(), true) : true), ...);
        }

        template<class T, std::size_t ...Is>
        bool check_end(T& tuple, size_t i, std::index_sequence<Is...>) {
            bool is_end = false;
            ((Is == i ? (is_end = std::get<Is>(tuple) == std::get<Is>(containers).container.end(), true) : true), ...);
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
            ((Is == index ? (std::get<Is>(tuple) = std::get<Is>(containers).container.end(), true) : true), ...);
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
    list_comprehension<container_syntax<std::vector<Type>, Type>, linked_container<CType, Container>>
        operator|(var<Type>& v, linked_container<CType, Container>&& c) {
        return { container_syntax<std::vector<Type>, Type>{ expression<Type>{ [&]() { return v(); } } }, std::move(c), {} };
    }

    template<class Type, class Container, class CType>
    list_comprehension<container_syntax<std::vector<Type>, Type>, linked_container<CType, Container>>
        operator|(const expression<Type>& v, linked_container<CType, Container>&& c) {
        return { container_syntax<std::vector<Type>, Type>{ v }, std::move(c), {} };
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
        v.constraints.push_back({ [c = std::move(c)]() { return static_cast<bool>(c()); } });
        return v;
    }

    struct lce {
        template<class ContainerSyntax, class ...LinkedContainers>
        auto operator[](list_comprehension<ContainerSyntax, LinkedContainers...>&& l) { return l.get(); };
    } lc;
}