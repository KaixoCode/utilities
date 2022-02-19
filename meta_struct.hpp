#include <concepts>
#include <cstddef>
#include <string_view>
#include <utility>

namespace kaixo {
    namespace detail {
        template<std::size_t N>
        struct string_struct { // Simple constexpr string literal wrapper
            char value[N - 1];
            constexpr string_struct(const char(&val)[N])
                : value() { std::copy_n(val, N - 1, value); }

            constexpr operator std::string_view() const { return { value, N - 1 }; }
            constexpr std::string_view str() const { return { value, N - 1 }; }
        };

        template<string_struct Name, class Type>
        struct arg_val { Type value; }; // Argument with value

        template<class Ty> // Default initializer
        struct default_init { constexpr auto operator()() const { return Ty{}; } };

        struct no_match {}; // No match in constructor params
        template<class ...Fs>
        struct params : public Fs... { // Constructor params
            constexpr explicit(false) operator no_match() const { return no_match{}; }
        }; // ^^^ Operator for if there's no match, implicit conversion happens after inheritence

        template<class Ty, class Self, class F> // Call initializer with no self
        constexpr auto call_init(Self&, F& f) requires(requires{
            { f() } -> std::convertible_to<Ty>; }) { return f(); }

        template<class Ty, class Self, class F> // Call initializer with self
        constexpr auto call_init(Self& self, F& f) requires(requires{
            { f(self) } -> std::convertible_to<Ty>; }) { return f(self); }
    }

    template<detail::string_struct Name, class Type, auto Init = detail::default_init<Type>()>
    struct field { // meta struct field
        using type = typename Type;

        template<class Ty, auto Init> constexpr field(field<Name, Ty, Init>& v) : value{ v.value } {}
        template<class Ty, auto Init> constexpr field(field<Name, Ty, Init>&& v) : value{ std::move(v.value) } {}
        template<class Ty, auto Init> constexpr field(const field<Name, Ty, Init>& v) : value{ v.value } {}

        template<class Ty>
        constexpr field(auto&, detail::arg_val<Name, Ty> v) : value{ static_cast<Type>(std::move(v.value)) } {}
        constexpr field(auto& self, detail::no_match v) : value{ detail::call_init<Type>(self, Init) } {}

        Type value;
    };

    template<detail::string_struct Name, auto Fun>
    struct function { // meta struct member function
        constexpr function(auto&, detail::no_match) {}
        constexpr static inline decltype(auto) run(auto&&...args) { return Fun(std::forward<decltype(args)>(args)...); }
    };

    namespace detail {
        template<string_struct Name>
        struct arg_type { // Constructor argument without value
            template<class T> // Equal opertator returns arg_val
            constexpr auto operator=(T t) const { return arg_val<Name, T>{ std::move(t) }; }
        };

        // Const and non-const 'get' implementations
        template<string_struct Name, class Type, auto Default>
        constexpr decltype(auto) get_impl(field<Name, Type, Default>& m) { return (m.value); }
        template<string_struct Name, class Type, auto Default>
        constexpr decltype(auto) get_impl(const field<Name, Type, Default>& m) { return (m.value); }
        template<string_struct Name, class MetaStruct> // Uses type deduction to get correct value
        constexpr decltype(auto) get(MetaStruct&& s) { return get_impl<Name>(std::forward<MetaStruct>(s)); }

        // Member function doesn't need separate const/non-const because the instance is in the args pack (auto const)
        template<string_struct Name, auto Fun>
        constexpr decltype(auto) run_impl(const function<Name, Fun>& m, auto&&...args) {
            return (function<Name, Fun>::run(std::forward<decltype(args)>(args)...));
        }
        template<string_struct Name, class MetaStruct>
        constexpr decltype(auto) run(MetaStruct&& s, auto&&...args) {
            return run_impl<Name>(std::forward<MetaStruct>(s), // Insert struct twice, once for type deduct
                std::forward<MetaStruct>(s), std::forward<decltype(args)>(args)...); // 2nd one for argument in call
        }
    }

    template<detail::string_struct Name> // Constructor arg with equal operator
    inline constexpr auto arg = detail::arg_type<Name>{};

    inline constexpr auto required = [] {}; // Required is empty initializer (won't compile if you don't initialize)

    template<class ...Fields>
    struct meta_struct : Fields... {
        // Conversion constructors
        template<class ...Fs> constexpr meta_struct(meta_struct<Fs...>& arg) : Fields(arg)... {}
        template<class ...Fs> constexpr meta_struct(const meta_struct<Fs...>& arg) : Fields(arg)... {}
        template<class ...Fs> constexpr meta_struct(meta_struct<Fs...>&& arg) : Fields(std::move(arg))... {}

        template<class ...Args>
        constexpr meta_struct(Args... args) : meta_struct(detail::params{ std::move(args)... }) {}
        constexpr meta_struct() : meta_struct(detail::params{}) {}

        template<detail::string_struct Name>
        constexpr decltype(auto) get() const { return detail::get<Name>(*this); }
        template<detail::string_struct Name>
        constexpr decltype(auto) get() { return detail::get<Name>(*this); }

        template<detail::string_struct Name>
        constexpr decltype(auto) run(auto&&...args) const { return detail::run<Name>(*this, std::forward<decltype(args)>(args)...); };
        template<detail::string_struct Name>
        constexpr decltype(auto) run(auto&&...args) { return detail::run<Name>(*this, std::forward<decltype(args)>(args)...); };

    private:
        template<class ...Fs> // use type deduction in the field class to construct with proper argument
        constexpr meta_struct(detail::params<Fs...> args) : Fields(*this, std::move(args))... {}
    };
}
