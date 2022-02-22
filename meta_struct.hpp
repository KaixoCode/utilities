#include <concepts>
#include <cstddef>
#include <string_view>
#include <utility>

namespace kaixo {
    namespace detail {
        template<class Ty> struct type {};

        template<class Ty>
        concept is_lambda = !std::same_as<decltype(&Ty::operator()), void>;

        template<std::size_t N>
        struct string_struct { // Simple constexpr string literal wrapper
            char value[N - 1];
            constexpr string_struct(const char(&val)[N])
                : value() {
                std::copy_n(val, N - 1, value);
            }

            constexpr operator std::string_view() const { return { value, N - 1 }; }
            constexpr std::string_view str() const { return { value, N - 1 }; }
        };

        template<class Ty> // Default initializer
        struct default_init { constexpr auto operator()() const { return Ty{}; } };

        struct no_match {}; // No match in constructor params
        template<class ...Fs>
        struct params : Fs... { // Constructor params
            constexpr explicit(false) operator no_match() const { return no_match{ }; }
        }; // ^^^ Operator for if there's no match, implicit conversion happens after inheritence

        template<class Ty, class Self, class F> // Call initializer with no self
        constexpr auto call_init(Self&, F& f) requires(requires{
            { f() } -> std::convertible_to<Ty>; }) {
            return f();
        }

        template<class Ty, class Self, class F> // Call initializer with self
        constexpr auto call_init(Self& self, F& f) requires(requires{
            { f(self) } -> std::convertible_to<Ty>; }) {
            return f(self);
        }

        template<std::size_t i, class Tuple, std::size_t... is>
        constexpr auto element_as_tuple(Tuple&& tuple, std::index_sequence<is...>)
        {
            using T = std::remove_reference_t<Tuple>;
            if constexpr (!(std::is_same_v<std::tuple_element_t<i, T>,
                std::tuple_element_t<is, T>> || ...))
                return std::tuple<std::tuple_element_t<i, T>>(
                    std::get<i>(std::forward<Tuple>(tuple)));
            else
                return std::make_tuple();
        }

        template<class Tuple, std::size_t... is>
        constexpr auto make_tuple_unique(Tuple&& tuple, std::index_sequence<is...>)
        {
            return std::tuple_cat(element_as_tuple<is>(std::forward<Tuple>(tuple),
                std::make_index_sequence<is>())...);
        }

        template<class... Tuples>
        constexpr auto make_tuple_unique(Tuples&&... tuples)
        {
            auto all = std::tuple_cat(std::forward<Tuples>(tuples)...);
            return make_tuple_unique(std::move(all),
                std::make_index_sequence<std::tuple_size_v<decltype(all)>>{});
        }
    }

    template<class ...Fields>
    struct meta_struct;

    template<detail::string_struct Name, class Type, auto Init = detail::default_init<Type>()>
    struct field {
        using type = Type;
        constexpr static auto name = Name;
        Type value;

        constexpr field(const Type& v) : value(v) {}

        template<class Self, class Ty>
        constexpr field(Self&, const field<Name, Ty>& a)
            : value((Type)a.value) {}

        template<class Self>
        constexpr field(Self& self, detail::no_match)
            : value(detail::call_init<Type>(self, Init)) {}
    };

    namespace detail {
        template<detail::string_struct Name>
        struct arg_type { // Constructor argument without value
            template<class T> // Equal opertator returns arg_val
            constexpr auto operator=(T t) const { return field<Name, std::decay_t<T>>{ std::move(t) }; }
        };

        // Type erasure through inheritance, virtual run method.
        template<class Ret, class ...Args> // vvv Ref counted base class for function storage
        struct virtual_function_base { std::size_t ref = 1; virtual Ret run(Args&&...args) const = 0; };
        template<class Fun, class Ret, class ...Args>
        struct virtual_function_typed_base : virtual_function_base<Ret, Args...> {
            Fun fun;
            virtual_function_typed_base(detail::type<Ret(Args...)>, Fun fun) : fun(fun) {}
            Ret run(Args&&...args) const override { return fun(std::forward<Args>(args)...); };
        };
    }

    template<detail::string_struct Name> // Constructor arg with equal operator
    constexpr auto arg = detail::arg_type<Name>{};

    template<detail::string_struct Name, auto Fun>
    struct function {
        template<class Self>
        constexpr function(Self&, detail::no_match) {}
        constexpr static inline decltype(auto) run(auto&&...args) { return Fun(std::forward<decltype(args)>(args)...); }
    };

    template<detail::string_struct Name, class Fun>
    struct virtual_function;

    namespace detail {

        // Const and non-const 'get' implementations
        template<string_struct Name, auto Fun>
        constexpr decltype(auto) get_impl(const auto& self, const function<Name, Fun>& m) {
            return [&](auto&& ...args) { return (function<Name, Fun>::run(self, std::forward<decltype(args)>(args)...)); };
        }        
        template<string_struct Name, auto Fun>
        constexpr decltype(auto) get_impl(auto& self, const function<Name, Fun>& m) {
            return [&](auto&& ...args) { return (function<Name, Fun>::run(self, std::forward<decltype(args)>(args)...)); };
        }       
        template<string_struct Name, class Fun>
        constexpr decltype(auto) get_impl(auto&, const virtual_function<Name, Fun>& m) {
            return [&](auto&& ...args) { return m.fun->run(std::forward<decltype(args)>(args)...); };
        }
        template<string_struct Name, class Ret, class ...Args>
        constexpr decltype(auto) get_impl(auto&, virtual_function<Name, Ret(Args...)>& m) {
            return [&](auto&& ...args) { return m.fun->run(static_cast<Args>(std::forward<decltype(args)>(args))...); };
        }
        template<string_struct Name, class Type, auto Default>
        constexpr Type& get_impl(field<Name, Type, Default>& m, auto&) { return (m.value); }
        template<string_struct Name, class Type, auto Default>
        constexpr Type const& get_impl(const field<Name, Type, Default>& m, auto&) { return (m.value); }
        template<string_struct Name, class MetaStruct> // Uses type deduction to get correct value
        constexpr decltype(auto) get(MetaStruct& s) { return get_impl<Name>(s, s); }

        template<string_struct Name, auto Fun>
        constexpr decltype(auto) run_impl(const function<Name, Fun>& m, auto&&...args) {
            return (function<Name, Fun>::run(std::forward<decltype(args)>(args)...));
        }
        template<string_struct Name, class Fun>
        constexpr decltype(auto) run_impl(const virtual_function<Name, Fun>& m, auto&, auto&&...args) {
            return m.fun->run(std::forward<decltype(args)>(args)...);
        }
        template<string_struct Name, class Ret, class ...Args>
        constexpr decltype(auto) run_impl(virtual_function<Name, Ret(Args...)>& m, auto&, auto&&...args) {
            return m.fun->run(static_cast<Args>(std::forward<decltype(args)>(args))...);
        }
        template<string_struct Name, class MetaStruct, class ...Args>
        constexpr decltype(auto) run(MetaStruct& s, Args&&...args) {
            return run_impl<Name>(s, s, std::forward<Args>(args)...);
        }
    }

    template<detail::string_struct Name, class Ret, class ...Args>
    struct virtual_function<Name, Ret(Args...)> {
        constexpr ~virtual_function() { if (fun && --fun->ref == 0) delete fun; }
        constexpr virtual_function(const virtual_function& other) : fun(other.fun) { if (fun) ++fun->ref; }
        constexpr virtual_function(virtual_function&& other) : fun(other.fun) { other.fun = nullptr; }
        detail::virtual_function_base<Ret, Args...>* fun = nullptr;

        template<class Self, class ...Tys>
        virtual_function(Self&, meta_struct<Tys...> m)
            : virtual_function(m, m, 0) {}

        template<class Self>
        virtual_function(Self&, detail::no_match) {}

    private:
        template<auto Fun, class ...Tys>
        virtual_function(meta_struct<Tys...> m, function<Name, Fun>, int)
            : fun(new detail::virtual_function_typed_base{ detail::type<Ret(Args...)>{},
                [m](Args...args) { return Fun(m, std::forward<Args>(args)...); } })
        {}
    };

    template<class ...Fields>
    struct meta_struct : Fields... {

        template<class ...Tys>
        constexpr meta_struct(Tys... args)
            : meta_struct(detail::params{ args... }) {}

        template<class Self, class ...Args>
        constexpr meta_struct(Self&, detail::params<Args...> val)
            : meta_struct(val) {}

        template<class Self>
        constexpr meta_struct(Self&, meta_struct val)
            : meta_struct(val) {}

        template<detail::string_struct Name>
        constexpr auto const& get() const { return detail::get<Name>(*this); }
        template<detail::string_struct Name>
        constexpr auto& get() { return detail::get<Name>(*this); }

        template<detail::string_struct Name>
        constexpr decltype(auto) run(auto&&...args) const { return detail::run<Name>(*this, std::forward<decltype(args)>(args)...); };
        template<detail::string_struct Name>
        constexpr decltype(auto) run(auto&&...args) { return detail::run<Name>(*this, std::forward<decltype(args)>(args)...); };

        template<detail::string_struct Name>
        constexpr decltype(auto) operator[](detail::arg_type<Name>) { return detail::get<Name>(*this); }
        template<detail::string_struct Name>
        constexpr decltype(auto) operator[](detail::arg_type<Name>) const { return detail::get<Name>(*this); }
        
        template<detail::string_struct Name>
        constexpr decltype(auto) operator()(detail::arg_type<Name>, auto&& ...args) { return detail::run<Name>(*this, std::forward<decltype(args)>(args)...); }
        template<detail::string_struct Name>
        constexpr decltype(auto) operator()(detail::arg_type<Name>, auto&& ...args) const { return detail::run<Name>(*this, std::forward<decltype(args)>(args)...); }

    private:
        template<class ...Fs>
        constexpr meta_struct(detail::params<Fs...> arg) : Fields(*this, std::move(arg))... {}

        template<class ...Fs>
        friend class meta_struct;
    };

    template<class> struct msty;
    template<class ...Tys> struct msty<meta_struct<Tys...>> { using type = std::tuple<Tys...>; };
    template<detail::string_struct Name, class Type, auto Init> struct msty<field<Name, Type, Init>> { using type = std::tuple<field<Name, Type, Init>>; };
    template<detail::string_struct Name, auto Fun> struct msty<function<Name, Fun>> { using type = std::tuple<function<Name, Fun>>; };
    template<class ...Tys> using mstyt = decltype(detail::make_tuple_unique(std::declval<typename msty<Tys>::type>()...));
    template<class Ty> struct mstyttms { using type = meta_struct<>; };;
    template<class ...Tys> struct mstyttms<std::tuple<Tys...>> { using type = meta_struct<Tys...>; };
    template<class ...Tys> using mstyttmst = typename mstyttms<mstyt<Tys...>>::type;

    template<class ...Tys>
    constexpr auto construct(Tys...args) {
        return mstyttmst<std::decay_t<Tys>...>{ args... };
    }

    template<class Ty> struct tomst { using type = Ty; };
    template<detail::string_struct Name, class Ty, auto Init> struct tomst<field<Name, Ty, Init>> { using type = field<Name, Ty, Init>; };
    template<class Ty> using tomstt = typename tomst<Ty>::type;

    template<class ...Tys>
    meta_struct(Tys...)->meta_struct<tomstt<Tys>...>;
}