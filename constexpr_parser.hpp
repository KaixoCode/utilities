#pragma once
#include "utils.hpp"

namespace kaixo {

    template <class T, class Tuple, size_t... Is>
    constexpr T construct_from_tuple(Tuple&& tuple, std::index_sequence<Is...>) {
        return T{ std::forward<Tuple>(tuple).get<Is>()... };
    }

    template <class T, class Tuple>
    constexpr T construct_from_tuple(Tuple&& tuple) {
        return construct_from_tuple<T>(std::forward<Tuple>(tuple),
            std::make_index_sequence<std::decay_t<Tuple>::size>{}
        );
    }

    struct dud {};
    template<std::size_t N>
    struct tag {
        char value[N - 1];
        constexpr tag(const char(&val)[N]) : value() { std::copy_n(val, N - 1, value); }
        constexpr operator std::string_view() const { return { value, N - 1 }; }
        constexpr std::string_view str() const { return { value, N - 1 }; }
        constexpr bool operator==(const tag& other) const { return str() == other.str(); }
        template<std::size_t M> requires (M != N)
            constexpr bool operator==(const tag<M>&) const { return false; }
    };

    // Struct for storing multiple types
    template<class ...Tys> struct and_struct;
    template<> struct and_struct<> {};
    template<class Ty, class ...Tys>
    struct and_struct<Ty, Tys...> : and_struct<Tys...> {
        constexpr static auto size = sizeof...(Tys) + 1;

        constexpr and_struct() : val() {}

        template<class T> requires std::same_as<std::decay_t<Ty>, std::decay_t<T>>
        constexpr and_struct(T&& val, and_struct<Tys...>&& vals) : val(std::forward<T>(val)), and_struct<Tys...>(std::move(vals)) {}

        template<class T, class ...Ts>
        constexpr and_struct(T&& val, Ts&&... vals) : val(std::forward<T>(val)), and_struct<Tys...>(std::forward<Ts>(vals)...) {}

        template<std::size_t N> constexpr auto& get() const {
            if constexpr (N == 0) return val;
            else return and_struct<Tys...>::template get<N - 1>();
        }

        template<std::size_t N> constexpr auto& get() {
            if constexpr (N == 0) return val;
            else return and_struct<Tys...>::template get<N - 1>();
        }

        Ty val;
    };
    template<class ...Tys> and_struct(Tys&&...)->and_struct<std::decay_t<Tys>...>;
    template<class Ty, class ...Tys> and_struct(Ty&&, and_struct<Tys...>&&)->and_struct<std::decay_t<Ty>, std::decay_t<Tys>...>;

    template<class Ty, class ...Tys, std::size_t ...Is>
    constexpr auto and_struct_cat_i(and_struct<Tys...>&& vals, Ty&& val, std::index_sequence<Is...>) {
        return and_struct<std::decay_t<Tys>..., std::decay_t<Ty>>{ std::move(vals.get<Is>())..., std::move(val) };
    }

    template<class Ty, class ...Tys>
    constexpr auto and_struct_cat(and_struct<Tys...>&& vals, Ty&& val) {
        return and_struct_cat_i(std::move(vals), std::forward<Ty>(val), std::make_index_sequence<sizeof...(Tys)>{});
    }

    // Struct for storing one of multiple types (union)
    template<class ...Tys> struct or_struct;
    template<> struct or_struct<> {
        constexpr std::size_t index() const { return 1; }
    };
    template<class Ty, class ...Tys>
    struct or_struct<Ty, Tys...> {
        bool set = false;

        constexpr or_struct() : val(), set(true) {}

        template<class T> requires (std::same_as<std::decay_t<Ty>, std::decay_t<T>>)
            constexpr or_struct(T&& val) : val(std::forward<T>(val)), set(true) {}

        template<class T> requires (!std::same_as<std::decay_t<Ty>, std::decay_t<T>>)
            constexpr or_struct(T&& val) : other(std::forward<T>(val)), set(false) {}

        template<class T>
        constexpr operator T() const {
            if constexpr (std::constructible_from<std::decay_t<T>, Ty>) return T{ val };
            else return other.operator T();
        }

        constexpr or_struct(or_struct&& other) {
            set = other.set;
            if (other.set) val = std::move(other.val);
            else this->other = std::move(other.other);
        }

        constexpr or_struct& operator=(or_struct&& other) {
            set = other.set;
            if (other.set) val = std::move(other.val);
            else this->other = std::move(other.other);
            return *this;
        }

        constexpr or_struct(const or_struct& other) {
            set = other.set;
            if (other.set) val = other.val;
            else this->other = other.other;
        }

        constexpr or_struct& operator=(const or_struct& other) {
            set = other.set;
            if (other.set) val = other.val;
            else this->other = other.other;
            return *this;
        }

        constexpr std::size_t index() const {
            return set ? 0 : 1 + other.index();
        }

        template<std::size_t N> constexpr auto& get() const {
            if constexpr (N == 0) return val;
            else return other.get<N - 1>();
        }

        union {
            Ty val;
            or_struct<Tys...> other;
        };
    };

    // Result of a parse operation
    template<class Ty>
    struct result {
        bool success = true;
        std::string_view remainder;
        Ty value;
    };

    // Base of a single parser
    template<tag Name, class Parser>
    struct parser_base {
        static constexpr auto name = Name;
        Parser parser;

        template<class Parser, class ...Tys>
        constexpr auto parse(std::string_view str, Tys&&... args) const {
            return parser.template parse<Parser>(str, std::forward<Tys>(args)...);
        }
    };

    // Simple satisfy parser
    template<auto Lambda>
    struct satisfy_t {
        template<class Parser, class ...Tys>
        constexpr auto parse(std::string_view str, Tys&&... args) const {
            return Lambda(str, std::forward<Tys>(args)...);
        }
    };

    template<auto Lambda> constexpr auto satisfy = satisfy_t<Lambda>{};

    // And parser, to parse multiple things in a row
    template<class Ty = dud, class ...Tys>
    struct and_parser_t {
        and_struct<Tys...> parsers;

        template<class Parser>
        constexpr auto parse(std::string_view str) const {
            if constexpr (std::same_as<Ty, dud>)
                return parse_i<Parser, 0>(str);
            else {
                auto res = parse_i<Parser, 0>(str);
                auto value = res.success ? construct_from_tuple<Ty>(std::move(res.value)) : Ty{};
                return result{ res.success, res.remainder, std::move(value) };
            }
        }

        template<class Parser, std::size_t N>
        constexpr auto parse_i(std::string_view str) const {
            auto a = parsers.get<N>().template parse<Parser>(str);
            if constexpr (N != sizeof...(Tys) - 1) {
                using b_t = decltype(parse_i<Parser, N + 1>(a.remainder));
                using res_t = result<decltype(and_struct{ std::move(a.value), std::move(std::declval<b_t>().value) }) > ;
                if (!a.success) return res_t{ false, str, {} };
                auto b = parse_i<Parser, N + 1>(a.remainder);
                return res_t{ b.success, b.success ? b.remainder : str, and_struct{ std::move(a.value), std::move(b.value) } };
            }
            else return result{ a.success, a.success ? a.remainder : str, and_struct{ std::move(a.value) } };
        }
    };

    template<class A, class B> requires (!specialization<A, and_parser_t> && !specialization<B, and_parser_t>)
        constexpr auto operator*(A&& a, B&& b) {
        return and_parser_t{ and_struct{ std::forward<A>(a), std::forward<B>(b) } };
    }

    template<class A, class ...Bs>
    constexpr auto operator*(and_parser_t<dud, Bs...>&& b, A&& a) {
        return and_parser_t{ and_struct_cat(std::move(b.parsers), std::forward<A>(a)) };
    }


    // And parser, to parse multiple things in a row
    template<class Ty = dud, class ...Tys>
    struct or_parser_t {
        and_struct<Tys...> parsers;

        template<class Parser>
        constexpr auto parse(std::string_view str) const {
            if constexpr (std::same_as<Ty, dud>)
                return parse_i<Parser, 0>(str);
            else {
                auto res = parse_i<Parser, 0>(str);
                auto value = res.success ? std::move(res.value).operator Ty() : Ty{};
                return result{ res.success, res.remainder, std::move(value) };
            }
        }

        template<class Parser, std::size_t N>
        constexpr auto parse_i(std::string_view str) const {
            using res_type = or_struct<decltype(std::declval<Tys>().parse<Parser>(str).value)...>;
            auto a = parsers.get<N>().parse<Parser>(str);
            if (a.success) return result{ true, a.remainder, res_type{ std::move(a.value) } };
            if constexpr (N != sizeof...(Tys) - 1) {
                return parse_i<Parser, N + 1>(str);
            }
            else return result{ false, str, res_type{ } };
        }
    };

    template<class A, class B> requires (!specialization<A, or_parser_t> && !specialization<B, or_parser_t>)
        constexpr auto operator|(A&& a, B&& b) {
        return or_parser_t{ and_struct{ std::forward<A>(a), std::forward<B>(b) } };
    }

    template<class A, class ...Bs>
    constexpr auto operator|(or_parser_t<Bs...>&& b, A&& a) {
        return or_parser_t{ and_struct_cat(std::move(b.parsers), std::forward<A>(a)) };
    }

    template<class Ty>
    struct ti {
        using type = Ty;
        template<class ...Tys>
        constexpr auto operator<<(and_parser_t<dud, Tys...>&& a) const {
            return and_parser_t<Ty, Tys...>{ std::move(a.parsers) };
        }

        template<class ...Tys>
        constexpr auto operator<<(or_parser_t<dud, Tys...>&& a) const {
            return or_parser_t<Ty, Tys...>{ std::move(a.parsers) };
        }

        template<class Ty>
        constexpr auto operator<<(Ty&& a) const {
            return and_parser_t<Ty>{ and_struct{ std::forward<Ty>(a) } };
        }
    };
    template<class Ty>
    constexpr auto t = ti<Ty>{};

    template<tag Name, class ...Args> struct named_parser {
        static constexpr auto name = Name;
        and_struct<Args...> values;

        template<class ...Tys> constexpr auto operator()(Tys...args) const {
            return named_parser<Name, std::decay_t<Tys>...>{
                and_struct<std::decay_t<Tys>...>{ std::forward<Tys>(args)... } };
        }

        template<class Ty> constexpr auto operator=(Ty&& ty) const {
            return parser_base<Name, std::decay_t<Ty>>{ std::forward<Ty>(ty) };
        }

        template<class Parser> constexpr auto parse(std::string_view str) const {
            return parse_i<Parser>(str, std::make_index_sequence<sizeof...(Args)>{});
        }

        template<class Parser, std::size_t ...Is>
        constexpr auto parse_i(std::string_view str, std::index_sequence<Is...>) const {
            return Parser::template get<Name>.parse<Parser>(str, values.get<Is>()...);
        }
    };

    template<tag Name> constexpr auto p = named_parser<Name>{};

    // Collection of parser bases
    template<auto... Parsers>
    struct parser {
        template<tag Name>
        static constexpr auto& get_impl(auto& P, auto&... Ps) {
            if constexpr (std::decay_t<decltype(P)>::name == Name) return P;
            else return get_impl<Name>(Ps...);
        }

        template<tag Name>
        static constexpr auto& get = get_impl<Name>(Parsers...);

        template<tag Name, class ...Tys>
        static constexpr auto parse(std::string_view str, Tys&& ...args) {
            return get<Name>.template parse<parser>(str, std::forward<Tys>(args)...);
        }
    };
}