#include <iostream>
#include <string_view>
#include <utility>
#include <algorithm>
#include <concepts>
#include <cstddef>

namespace kaixo {
    // is specialization of templated class
    template<typename Test, template<typename...> class Ref> struct is_specialization : std::false_type {};
    template<template<typename...> class Ref, typename... Args> struct is_specialization<Ref<Args...>, Ref> : std::true_type {};
    template<typename Test, template<typename...> class Ref> concept specialization = is_specialization<std::decay_t<Test>, Ref>::value;

    // Index of type in tuple
    template<typename T, typename C, std::size_t I> struct tuple_index_r;
    template<typename H, typename ...R, typename C, std::size_t I>
    struct tuple_index_r<std::tuple<H, R...>, C, I>
        : public std::conditional<std::is_same<C, H>::value,
        std::integral_constant<std::size_t, I>,
        tuple_index_r<std::tuple<R...>, C, I + 1>>::type{};
    template<typename C, std::size_t I> struct tuple_index_r<std::tuple<>, C, I> {};
    template<typename T, typename C> struct tuple_index_of : public std::integral_constant<std::size_t, tuple_index_r<T, C, 0>::value> {};

    // Type as tuple, takes existing tuple into account
    template<class Ty> struct as_tuple { using type = std::tuple<Ty>; };
    template<class A, class B> struct as_tuple<std::pair<A, B>> { using type = std::tuple<std::remove_const_t<A>, B>; };
    template<class ...Tys> struct as_tuple<std::tuple<Tys...>> { using type = std::tuple<Tys...>; };
    template<class Ty> using as_tuple_t = typename as_tuple<Ty>::type;

    // String literal wrapper for template parameter
    template<std::size_t N>
    struct tag {
        char value[N - 1];
        constexpr tag(const char(&val)[N]) : value() { std::copy_n(val, N - 1, value); }
        constexpr operator std::string_view() const { return { value, N - 1 }; }
        constexpr std::string_view str() const { return { value, N - 1 }; }
    };

    // Variable, type with string literal as template parameter
    template<tag Name> struct var_t { constexpr static inline auto name = Name; consteval var_t() {} };
    template<tag Name> constexpr auto var = var_t<Name>{};

    // Collection of tags
    template<tag ...Names> struct tags {
        using names = std::tuple<var_t<Names>...>;
        template<tag Name> constexpr static auto index_of = tuple_index_of<names, var_t<Name>>::value;
    };

    // Tuple of var_t's to tags class
    template<class> struct tuple_to_tags;
    template<class ...As> struct tuple_to_tags<std::tuple<As...>> { using type = tags<As::name...>; };;
    template<class A> using tuple_to_tags_t = typename tuple_to_tags<A>::type;
    template<class A, class B> using combine_names_t = tuple_to_tags_t<decltype(std::tuple_cat(std::declval<A>(), std::declval<B>()))>;

    // Tuple with names linked, uses index of name to find correct element of tuple
    template<class Names, class Tuple>
    struct tuple_with_names : public Tuple {
        using type = Tuple;

        constexpr tuple_with_names& operator=(Tuple&& val) { Tuple::operator=(std::move(val)); return *this; }
        template<tag Name> constexpr decltype(auto) get() const { return std::get<Names::index_of<Name>>(*this); }
    };

    // Expression is simple wrapper for a lambda
    template<class Lambda>
    struct expression : public Lambda {};

    // Var type has a name
    template<class Ty>
    concept is_var_type = requires() { Ty::name; };

    // Container needs to have these things for list comprehension
    template<class Ty>
    concept container_type = requires(const std::decay_t<Ty> ty) {
        typename std::decay_t<Ty>::value_type;
        typename std::decay_t<Ty>::size_type;
        typename std::decay_t<Ty>::const_iterator;
        { ty.begin() } -> std::same_as<typename std::decay_t<Ty>::const_iterator>;
        { ty.end() } -> std::same_as<typename std::decay_t<Ty>::const_iterator>;
    };

    // Type has names type alias
    template<class Ty>
    concept has_names = requires () {
        typename std::decay_t<Ty>::names;
    };

    // Wrapped container is used when creating list comprehension object, needs names
    template<class Ty>
    concept is_named_container = container_type<Ty> && has_names<Ty>;

    // Only valid argument for operations if not variable or container
    template<class Ty>
    concept valid_op_type = (!is_var_type<Ty> && !container_type<Ty> && !specialization<Ty, expression>);

    namespace lc_operators {
#define create_op(op)                                                                                                                      \
        template<is_var_type A, is_var_type B>                                                                                             \
        constexpr auto operator op(const A&, const B&) {                                                                                   \
            return expression{ [](auto& vals) { return vals.get<A::name>() op vals.get<B::name>(); } };                                    \
        }                                                                                                                                  \
                                                                                                                                           \
        template<is_var_type B>                                                                                                            \
        constexpr auto operator op(specialization<expression> auto&& expr1, const B&) {                                                    \
            return expression{ [expr1 = std::move(expr1)] (auto& vals) { return expr1(vals) op vals.get<B::name>(); } };                   \
        }                                                                                                                                  \
                                                                                                                                           \
        template<is_var_type A>                                                                                                            \
        constexpr auto operator op(const A&, specialization<expression> auto&& expr2) {                                                    \
            return expression{ [expr2 = std::move(expr2)] (auto& vals) { return vals.get<A::name>() op expr2(vals); } };                   \
        }                                                                                                                                  \
                                                                                                                                           \
        constexpr auto operator op(specialization<expression> auto&& expr1, specialization<expression> auto&& expr2) {                     \
            return expression{ [expr1 = std::move(expr1), expr2 = std::move(expr2)] (auto& vals) { return expr1(vals) op expr2(vals); } }; \
        }                                                                                                                                  \
                                                                                                                                           \
        template<is_var_type A, valid_op_type B>                                                                                           \
        constexpr auto operator op(const A&, B& b) {                                                                                       \
            return expression{ [&](auto& vals) { return vals.get<A::name>() op b; } };                                                     \
        }                                                                                                                                  \
                                                                                                                                           \
        template<valid_op_type B>                                                                                                          \
        constexpr auto operator op(specialization<expression> auto&& expr1, B& b) {                                                        \
            return expression{ [&, expr1 = std::move(expr1)] (auto& vals) { return b op expr1(vals); } };                                  \
        }                                                                                                                                  \
                                                                                                                                           \
        template<is_var_type A, valid_op_type B>                                                                                           \
        constexpr auto operator op(B& b, const A&) {                                                                                       \
            return expression{ [&](auto& vals) { return vals.get<A::name>() op b; } };                                                     \
        }                                                                                                                                  \
                                                                                                                                           \
        template<valid_op_type B>                                                                                                          \
        constexpr auto operator op(B& b, specialization<expression> auto&& expr1) {                                                        \
            return expression{ [&, expr1 = std::move(expr1)] (auto& vals) { return b op expr1(vals); } };                                  \
        }                                                                                                                                  \
                                                                                                                                           \
        template<is_var_type A, valid_op_type B>                                                                                           \
        constexpr auto operator op(const A&, B&& b) {                                                                                      \
            return expression{ [b = std::move(b)](auto& vals) { return vals.get<A::name>() op b; } };                                      \
        }                                                                                                                                  \
                                                                                                                                           \
        template<valid_op_type B>                                                                                                          \
        constexpr auto operator op(specialization<expression> auto&& expr1, B&& b) {                                                       \
            return expression{ [b = std::move(b), expr1 = std::move(expr1)] (auto& vals) { return b op expr1(vals); } };                   \
        }                                                                                                                                  \
                                                                                                                                           \
        template<is_var_type A, valid_op_type B>                                                                                           \
        constexpr auto operator op(B&& b, const A&) {                                                                                      \
            return expression{ [b = std::move(b)](auto& vals) { return vals.get<A::name>() op b; } };                                      \
        }                                                                                                                                  \
                                                                                                                                           \
        template<valid_op_type B>                                                                                                          \
        constexpr auto operator op(B&& b, specialization<expression> auto&& expr1) {                                                       \
            return expression{ [b = std::move(b), expr1 = std::move(expr1)] (auto& vals) { return b op expr1(vals); } };                   \
        }                                                                                                                                  \

        create_op(+) create_op(-) create_op(*) create_op(/ ) create_op(== ) create_op(!= ) create_op(<= )
            create_op(>= ) create_op(> ) create_op(< ) create_op(%) create_op(<=> ) create_op(<< ) create_op(>> )
            create_op(&) create_op(| ) create_op(&&) create_op(|| ) 


#define create_uop(op)                                                                                                       \
        template<is_var_type A>                                                                                              \
        constexpr auto operator op(const A&) {                                                                               \
            return expression{ [](auto& vals) { return  op vals.get<A::name>(); } };                                         \
        }                                                                                                                    \
                                                                                                                             \
        constexpr auto operator op(specialization<expression> auto&& expr1) {                                                \
            return expression{ [expr1 = std::move(expr1)](auto& vals) { return op expr1(vals); } };                          \
        }

            create_uop(-) create_uop(~) create_uop(!) create_uop(*) create_uop(&)

#undef create_op
#undef create_uop
    }

    // Determines what type to store, if reference, stores reference, 
    // otherwise by value, always const because list comprehension never changes input values
    template<class Ty>
    using stored_type_t = std::conditional_t<std::is_lvalue_reference_v<Ty>, const std::decay_t<Ty>&, const std::decay_t<Ty>>;

    // Zip 2 containers into a single container of tuples, tuple_cat's if containers have tuples themselves
    template<class A, class B>
    class zip_t {
        using container_type_a = std::decay_t<A>;
        using container_type_b = std::decay_t<B>;
        using value_type_a = typename container_type_a::value_type;
        using value_type_b = typename container_type_b::value_type;
        using iterator_a = typename container_type_a::const_iterator;
        using iterator_b = typename container_type_b::const_iterator;
    public:
        using value_type = decltype(std::tuple_cat(std::declval<as_tuple_t<value_type_a>>(), std::declval<as_tuple_t<value_type_b>>()));
        using size_type = typename container_type_a::size_type;

        explicit constexpr zip_t(A a, B b) : a(std::forward<A>(a)), b(std::forward<B>(b)) {}

        class iterator {
        public:
            using value_type = zip_t::value_type;
            using size_type = zip_t::size_type;

            constexpr iterator(const iterator& other) : it_a(other.it_a), it_b(other.it_b) {}
            constexpr iterator(iterator_a it_a, iterator_b it_b) : it_a(it_a), it_b(it_b) {}
            constexpr iterator& operator=(const iterator& other) { it_a = other.it_a, it_b = other.it_b; return *this; }
            constexpr iterator& operator++() { ++it_a, ++it_b; return *this; }
            constexpr iterator& operator--() { --it_a, --it_b; return *this; }
            constexpr bool operator==(const iterator& other) const { return other.it_a == it_a || other.it_b == it_b; }
            constexpr value_type operator*() const { return std::tuple_cat(std::tuple{ *it_a }, std::tuple{ *it_b }); }

        private:
            iterator_a it_a;
            iterator_b it_b;
        };

        using const_iterator = iterator;

        constexpr size_type size() const { return std::min(a.size(), b.size()); }
        constexpr iterator begin() const { return iterator{ a.begin(), b.begin() }; }
        constexpr iterator end() const { return iterator{ a.end(), b.end() }; }
        constexpr value_type operator[](size_type index) const { return std::tuple_cat(std::tuple{ a[index] }, std::tuple{ b[index] }); }

    private:
        stored_type_t<A> a;
        stored_type_t<B> b;
    };

    template<class A, class B> zip_t(A&&, B&&)->zip_t<A&&, B&&>;
    template<class A, class B> zip_t(A&, B&&)->zip_t<A&, B&&>;
    template<class A, class B> zip_t(A&&, B&)->zip_t<A&&, B&>;
    template<class A, class B> zip_t(A&, B&)->zip_t<A&, B&>;

    // Map a function over a container
    template<class Fun, class Ty>
    class map_t {
        using container_type = std::decay_t<Ty>;
        using container_value_type = typename container_type::value_type;
        using function_type = std::decay_t<Fun>;
        using container_iterator = typename container_type::const_iterator;
    public:
        using value_type = decltype(std::declval<Fun>()(*std::declval<container_iterator>()));
        using size_type = typename container_type::size_type;

        explicit constexpr map_t(Fun transform, Ty container)
            : transform(std::forward<Fun>(transform)), container(std::forward<Ty>(container)) {}

        class iterator {
        public:
            using value_type = map_t::value_type;
            using size_type = map_t::size_type;

            constexpr iterator(const iterator& other) : it(other.it), transform(other.transform) {}
            constexpr iterator(const function_type& transform, container_iterator it) : it(it), transform(transform) {}
            constexpr iterator& operator=(const iterator& other) { it = other.it; return *this; }
            constexpr iterator& operator++() { ++it; return *this; }
            constexpr iterator& operator--() { --it; return *this; }
            constexpr bool operator==(const iterator& other) const { return other.it == it; }
            constexpr value_type operator*() const { return transform(*it); }

        private:
            container_iterator it;
            const function_type& transform;
        };

        using const_iterator = iterator;

        constexpr size_type size() const { return container.size(); }
        constexpr iterator begin() const { return iterator{ transform, container.begin() }; }
        constexpr iterator end() const { return iterator{ transform, container.end() }; }
        constexpr value_type operator[](size_type index) const { return transform(container[index]); }

    private:
        function_type transform;
        stored_type_t<Ty> container;
    };

    template<class A, class B> map_t(A&, B&)->map_t<A&, B&>;
    template<class A, class B> map_t(A&, B&&)->map_t<A&, B&&>;
    template<class A, class B> map_t(A&&, B&)->map_t<A&&, B&>;
    template<class A, class B> map_t(A&&, B&&)->map_t<A&&, B&&>;

    // Cartesian product of 2 containers, result is 1 container of tuples,
    // tuple_cat's if original containers contain tuples
    template<class A, class B>
    class cart_t {
        using container_type_a = std::decay_t<A>;
        using container_type_b = std::decay_t<B>;
        using value_type_a = typename container_type_a::value_type;
        using value_type_b = typename container_type_b::value_type;
        using iterator_a = typename container_type_a::const_iterator;
        using iterator_b = typename container_type_b::const_iterator;
    public:
        using value_type = decltype(std::tuple_cat(std::declval<as_tuple_t<value_type_a>>(), std::declval<as_tuple_t<value_type_b>>()));
        using size_type = typename container_type_a::size_type;

        explicit constexpr cart_t(A a, B b) : a(std::forward<A>(a)), b(std::forward<B>(b)) {}

        class iterator {
        public:
            using value_type = cart_t::value_type;
            using size_type = cart_t::size_type;

            constexpr iterator(const iterator& other) : cart(other.cart), it_a(other.it_a), it_b(other.it_b) {}
            constexpr iterator(const cart_t& cart, iterator_a it_a, iterator_b it_b) : it_a(it_a), it_b(it_b), cart(cart) {}
            constexpr iterator& operator=(const iterator& other) { it_a = other.it_a, it_b = other.it_b; return *this; }

            constexpr iterator& operator++() {
                if (++it_b == cart.b.end()) {
                    it_b = cart.b.begin();
                    ++it_a;
                }
                return *this;
            }

            constexpr iterator& operator--() {
                if (--it_b == cart.b.begin()) {
                    it_b = cart.b.end();
                    --it_a;
                }
                return *this;
            }
            constexpr bool operator==(const iterator& other) const { return other.it_a == it_a && other.it_b == it_b; }
            constexpr value_type operator*() const { return std::tuple_cat(std::tuple{ *it_a }, std::tuple{ *it_b }); }

        private:
            iterator_a it_a;
            iterator_b it_b;
            const cart_t& cart;
        };

        using const_iterator = iterator;

        constexpr size_type size() const { return a.size() * b.size(); }
        constexpr iterator begin() const { return iterator{ *this, a.begin(), b.begin() }; }
        constexpr iterator end() const { return iterator{ *this, a.end(), b.begin() }; }
        constexpr value_type operator[](size_type index) const {
            return std::tuple_cat(std::tuple{ a[index / b.size()] }, std::tuple{ b[index % b.size()] });
        }

    private:
        stored_type_t<A> a;
        stored_type_t<B> b;
    };

    template<class A, class B> cart_t(A&&, B&&)->cart_t<A&&, B&&>;
    template<class A, class B> cart_t(A&, B&&)->cart_t<A&, B&&>;
    template<class A, class B> cart_t(A&&, B&)->cart_t<A&&, B&>;
    template<class A, class B> cart_t(A&, B&)->cart_t<A&, B&>;

    // Named container, holds a container, but has names for all indices 
    // in the tuple that the container holds.
    template<class Container, has_names Names>
    class named_container {
    public:
        using container_type = std::decay_t<Container>;
        using value_type = typename container_type::value_type;
        using size_type = typename container_type::size_type;
        using iterator = typename container_type::const_iterator;
        using const_iterator = typename container_type::const_iterator;

        using names = Names;

        constexpr named_container(Container container)
            : container(std::forward<Container>(container)) {}

        constexpr iterator begin() const { return container.begin(); }
        constexpr iterator end() const { return container.end(); }
        constexpr size_type size() const { return container.size(); }
        constexpr decltype(auto) operator[](size_type index) const { return container[index]; }

    private:
        stored_type_t<Container> container;
    };

    // List comprehension base, only has data, is used when still constructing final object
    // Expression is what'll be outputted, Container determines the values going into the Expression
    // Constraints is a tuple of Expressions that filter the final output.
    template<specialization<expression> Expression, is_named_container Container, class Constraints>
    class list_comprehension_base {
    public:
        constexpr list_comprehension_base(Expression expr, Container cont, Constraints css)
            : expr(expr), container(cont), constraints(css) {}

        stored_type_t<Expression> expr;
        stored_type_t<Container> container;
        Constraints constraints;
    };

    // Final list comprehension object
    template<specialization<expression> Expression, is_named_container Container, class Constraints>
    class list_comprehension {
        using container_type = std::decay_t<Container>;
        using container_iterator = typename container_type::const_iterator;
        using container_value_type = typename container_type::value_type;
        using named_tuple_type = tuple_with_names<typename Container::names, as_tuple_t<container_value_type>>;
        constexpr static auto seq = std::make_index_sequence<std::tuple_size_v<Constraints>>{};

    public:
        using value_type = decltype(std::declval<Expression>()(std::declval<named_tuple_type&>()));
        using size_type = typename container_type::size_type;

        constexpr list_comprehension(list_comprehension_base<Expression, Container, Constraints>&& base)
            : data(std::move(base)) {}

        class iterator {
        public:
            using value_type = list_comprehension::value_type;
            using size_type = list_comprehension::size_type;

            constexpr iterator(const iterator& other) : me(other.me), it(other.it) { prepare(); }
            constexpr iterator(const list_comprehension& me, container_iterator it) : me(me), it(it) { prepare(); }
            constexpr iterator& operator=(const iterator& other) { it = other.it; return *this; }

            constexpr iterator& operator++() {
                if constexpr (std::tuple_size_v<Constraints> == 0)
                    ++it; // no constraints = just decrement
                else { // decrement until constraints pass or at end
                    do ++it;
                    while (!(it == me.data.container.end()) && !me.check_constraints(me.seq, value = *it));
                }
                return *this;
            }

            constexpr iterator& operator--() {
                if constexpr (std::tuple_size_v<Constraints> == 0)
                    --it; // no constraints = just decrement
                else { // decrement until constraints pass or at end
                    do --it;
                    while (!(it == me.data.container.begin()) && !me.check_constraints(me.seq, value = *it));
                }
                return *this;
            }

            constexpr bool operator==(const iterator& other) const { return other.it == it; }
            constexpr value_type operator*() {
                if constexpr (std::tuple_size_v<Constraints> == 0) value = *it;
                return me.data.expr(value);
            }

            named_tuple_type value;
            container_iterator it;
            const list_comprehension& me;
        private:
            // Prepare on create, so we start at a valid index
            constexpr void prepare() {
                if (!(it == me.data.container.end())) {
                    value = *it;
                    if (!me.check_constraints(me.seq, value)) operator++();
                }
            }
        };

        using const_iterator = iterator;

        constexpr iterator begin() const { return iterator{ *this, data.container.begin() }; }
        constexpr iterator end() const { return iterator{ *this, data.container.end() }; }

        constexpr decltype(auto) operator[](size_type index) const {
            if constexpr (std::tuple_size_v<Constraints> == 0) {
                named_tuple_type _tpl{ data.container[index] };
                return data.expr(_tpl);
            }
            else // If constraints, we don't know where what index is, so use  
            {    // the increment operator to count to nth element
                auto _b = begin();
                while (index--) ++_b;
                return *_b;
            }
        }

    private:
        list_comprehension_base<Expression, Container, Constraints> data;
        friend class iterator;

        template<std::size_t ...Is>
        constexpr bool check_constraints(std::index_sequence<Is...>, auto& val) const {
            return (std::get<Is>(data.constraints)(val) && ...);
        }
    };

    template<is_var_type ...Vars>
    struct tuple_of_vars {
        using vars = std::tuple<Vars...>;
    };

    // All operators for constructing a list comprehension object.
    namespace lc_operators {

        // Expr | Container
        template<specialization<expression> Expression, is_named_container Container>
        constexpr auto operator|(Expression&& expr, Container&& cont) {
            return list_comprehension_base<Expression, Container, std::tuple<>>{ std::forward<Expression>(expr), std::forward<Container>(cont), {} };
        }

        // Var | Container
        template<is_var_type Var, is_named_container Container>
        constexpr auto operator|(Var&, Container&& cont) {
            auto expr = expression{ [](auto& val) { return val.get<Var::name>(); } };
            return list_comprehension_base<decltype(expr), Container, std::tuple<>>{ std::move(expr), std::forward<Container>(cont), {} };
        }

        // Var | Container
        template<is_named_container Container, is_var_type ...Vars>
        constexpr auto operator|(tuple_of_vars<Vars...>&&, Container&& cont) {
            auto expr = expression{ [](auto& val) { return std::tuple{ val.get<Vars::name>()... }; } };
            return list_comprehension_base<decltype(expr), Container, std::tuple<>>{ std::move(expr), std::forward<Container>(cont), {} };
        }

        // - Container, necessary sugar for 'x <- container' syntax, just forwards container
        template<container_type Container>
        constexpr Container&& operator-(Container&& c) { return std::forward<Container>(c); }

        // Convert container into a named container
        template<is_var_type Var, container_type Container>
        constexpr auto operator<(Var&, Container&& c) {
            // If container is zip_t, it means it's type is a tuple, which will be concatenated
            // so we wrap it inside another tuple, using map, to prevent that.
            if constexpr (specialization<Container, zip_t>) {
                const auto mapped = map_t{ [] <typename Ty>(Ty && val) {
                    return std::tuple<Ty>{ std::forward<Ty>(val) };
                }, std::forward<Container>(c) };
                return named_container<decltype(mapped), tags<Var::name>>{ mapped };
            }
            else // Otherwise it's a normal container that can simply be named
                return named_container<Container&&, tags<Var::name>>{ std::forward<Container>(c) };
        }

        // Convert container into a named container
        template<container_type Container, is_var_type ...Vars>
        constexpr auto operator<(tuple_of_vars<Vars...>&&, Container&& c) {
            static_assert(std::tuple_size_v<std::decay_t<Container>::value_type> == sizeof...(Vars));
            return named_container<Container&&, tags<Vars::name...>>{ std::forward<Container>(c) };
        }

        // 'list comprehension base, named container'; Creates new named container of cartesian product with combined names
        template<specialization<list_comprehension_base> A, is_named_container B>
        constexpr auto operator,(A&& a, B&& b) {
            using combined = combine_names_t<std::decay_t<decltype(a.container)>::names::names, std::decay_t<B>::names::names>;
            const cart_t _cont{ std::move(a.container), std::forward<B>(b) };
            const named_container<decltype(_cont), combined> _c{ _cont };
            return list_comprehension_base<decltype(a.expr), decltype(_c), decltype(a.constraints)>{ std::move(a.expr), std::move(_c), std::move(a.constraints) };
        }

        // 'list comprehension base, expression'; Adds a constraint.
        template<specialization<list_comprehension_base> A, specialization<expression> B>
        constexpr auto operator,(A&& a, B&& b) {
            const std::tuple _css = std::tuple_cat(a.constraints, std::tuple{ b });
            return list_comprehension_base<decltype(a.expr), decltype(a.container), decltype(_css)>{ std::move(a.expr), std::move(a.container), std::move(_css) };
        }

        // 'container, container'; For parallel iteration, creates a new zipped container.
        template<container_type A, container_type B> requires (!is_named_container<A> && !is_named_container<B>)
            constexpr zip_t<A&&, B&&> operator,(A&& a, B&& b) {
            return zip_t<A&&, B&&>{ std::forward<A>(a), std::forward<B>(b) };
        }

        // 'var, var'; Converts 2 vars to tuple of vars
        template<is_var_type A, is_var_type B> constexpr auto operator,(A&, B&) { return tuple_of_vars<A, B>{}; }
        template<is_var_type A, is_var_type ...Bs> constexpr auto operator,(tuple_of_vars<Bs...>&&, A&) { return tuple_of_vars<Bs..., A>{}; }
        template<is_var_type A, is_var_type ...Bs> constexpr auto operator,(A&, tuple_of_vars<Bs...>&&) { return tuple_of_vars<A, Bs...>{}; }

        // 'expression, var'; Same as above, but with 1 expression
        template<is_var_type B>
        constexpr auto operator,(specialization<expression> auto&& expr1, B&) {
            return expression{ [expr1 = std::move(expr1)] (auto& vals) {
                return std::tuple_cat(std::tuple{ expr1(vals) }, std::tuple<std::decay_t<decltype(vals.get<B::name>())>>{ vals.get<B::name>() }); } };
        }

        // 'var, expression'; Same as above
        template<is_var_type A>
        constexpr auto operator,(A&, specialization<expression> auto&& expr2) {
            return expression{ [expr2 = std::move(expr2)] (auto& vals) {
                return std::tuple_cat(std::tuple< std::decay_t<decltype(vals.get<A::name>())>>{ vals.get<A::name>() }, std::tuple{expr2(vals)}); } };
        }
        
        // 'expression, vars'; Same as above
        template<is_var_type ...Bs>
        constexpr auto operator,(specialization<expression> auto&& expr1, tuple_of_vars<Bs...>&&) {
            return expression{ [expr1 = std::move(expr1)] (auto& vals) {
                return std::tuple_cat(std::tuple{ expr1(vals) }, std::tuple<std::decay_t<decltype(vals.get<Bs::name>())>>{ vals.get<Bs::name>() }...); } };
        }

        // 'vars, expression'; Same as above
        template<is_var_type ...As>
        constexpr auto operator,(tuple_of_vars<As...>&, specialization<expression> auto&& expr2) {
            return expression{ [expr2 = std::move(expr2)] (auto& vals) {
                return std::tuple_cat(std::tuple<std::decay_t<decltype(vals.get<As::name>())>>{ vals.get<As::name>() }..., std::tuple{expr2(vals)}); } };
        }

        // 'expression, expression'; Same as above, but both arguments are expressions
        template<is_var_type A>
        constexpr auto operator,(specialization<expression> auto&& expr1, specialization<expression> auto&& expr2) {
            return expression{ [expr1 = std::move(expr1), expr2 = std::move(expr2)] (auto& vals) {
                return std::tuple_cat(std::tuple{ expr1(vals) }, std::tuple{ expr2(vals) }); } };
        }
    }

    // Construct the final list comprehension using the operator[] in 
    // this global constexpr object.
    struct lc_op {
        template<class A, class B, class C>
        constexpr auto operator[](list_comprehension_base<A, B, C>&& val) const { 
            return list_comprehension<A, B, C>{ std::move(val) }; 
        }
    };
    constexpr lc_op lc;

    // Infinite type, used by range
    struct inf_t {};
    constexpr inf_t inf;

    // Simple range class
    template<class Ty>
    class range {
    public:
        using value_type = Ty;
        using size_type = std::size_t;

        constexpr range(const Ty& a) : m_Start(), m_End(a) {}
        constexpr range(const Ty& a, const Ty& b) : m_Start(a), m_End(b) {}
        constexpr range(const Ty& a, inf_t) : m_Start(a), m_End(std::numeric_limits<Ty>::max()) {}

        class iterator {
        public:
            using value_type = range::value_type;
            using size_type = range::size_type;

            constexpr iterator(const iterator& val) : val(val.val) {}
            constexpr iterator(Ty val) : val(val) {}
            constexpr iterator& operator=(const iterator& other) { val = other.val; return *this; }
            constexpr iterator& operator++() { ++val; return *this; }
            constexpr iterator& operator--() { --val; return *this; }
            constexpr bool operator==(const iterator& other) const { return other.val == val; }
            constexpr Ty operator*() const { return val; }

        private:
            Ty val;
        };

        using const_iterator = iterator;

        constexpr iterator begin() const { return iterator{ m_Start }; }
        constexpr iterator end() const { return iterator{ m_End }; }
        constexpr size_type size() const { return m_End - m_Start; }
        constexpr Ty operator[](size_type index) const { return m_Start + static_cast<Ty>(index); }

    private:
        Ty m_Start;
        Ty m_End;
    };
}
