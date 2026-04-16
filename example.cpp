
// ------------------------------------------------

#include <iostream>
#include <fstream>
#include <string>
#include <source_location>
#include <vector>
#include <map>
#include <ranges>
#include <algorithm>
#include <utility>
#include <functional>
#include <cassert>
#include <set>
#include <expected>
#include <thread>
#include <any>
#include <mutex>
#include <type_traits>
#include <concepts>
#include <cstddef>
#include <string_view>
#include <unordered_set>
#include <regex>
#include <span>
#include <complex>
#include <print>
#include <coroutine>
#include <typeindex>
#include <filesystem>
#include <variant>

// ------------------------------------------------

#include "json.hpp"

// ------------------------------------------------

namespace kaixo {

    // ------------------------------------------------

    struct parser {

        // ------------------------------------------------

        struct empty_t {};

        // ------------------------------------------------

        struct error_message {

            // ------------------------------------------------

            std::string_view message;

            // ------------------------------------------------

            template<std::size_t N> // Consteval enforces error message to be string literal
            consteval error_message(const char(&msg)[N])
                : message(msg)
            {}

            // ------------------------------------------------

        };

        struct error {
            std::size_t line = 0;
            std::size_t character = 0;
            error_message message;

            std::string what() const {
                return std::format("line {}, character {}: {}", line, character, message.message);
            }
        };

        struct error_result {

            // ------------------------------------------------

            std::string_view parsed_until;
            error_message message;

            // ------------------------------------------------

            operator error() const {
                std::size_t newlines = 1;
                std::size_t charsInLine = 1;
                for (auto& c : parsed_until) {
                    ++charsInLine;
                    if (c == '\n') ++newlines, charsInLine = 1;
                }

                return {
                    .line = newlines,
                    .character = charsInLine,
                    .message = message,
                };
            }

            // ------------------------------------------------

        };

        // ------------------------------------------------

        template<class Ty = void> struct parse_result;
        template<class Ty = void>
        struct result {
            std::vector<error> _errors;
            std::optional<Ty> _value;

            result(error_message msg)
                : _errors{ error{.message = msg } }
            {}

            result(Ty&& result)
                : _value(std::move(result))
            {}

            template<class T> 
                requires (!std::same_as<Ty, void>&& std::constructible_from<Ty, T>)
            result(T&& result)
                : _value(std::move(result))
            {}

            template<class T>
                requires (!std::same_as<Ty, void>&& std::constructible_from<Ty, T>)
            result(parse_result<T>&& result)
                : _errors(result._errors.begin(), result._errors.end())
                , _value(std::move(result._value))
            {}

            result(parse_result<void>&& result)
                : _errors(result._errors.begin(), result._errors.end())
            {}

            const std::vector<error>& errors() const { return _errors; }
            explicit operator bool() const { return _value.has_value(); }
            bool has_value() const { return _value.has_value(); }
            Ty& value() { return _value.value(); }

            Ty* operator->() { return &_value.value(); }
        };

        // ------------------------------------------------

        template<class A, class B>
        struct convert;

        template<class A, class B>
        struct combine;

        // ------------------------------------------------

        enum class parse_result_state {
            success,     // Parsed successfully 
            recoverable, // Failed to parse, but continue
            fatal,       // Fatal parse error, non-recoverable
        };

        template<class Ty>
        struct parse_result {
            using result_type = Ty;

            std::vector<error_result> _errors;
            std::optional<Ty> _value;
            parse_result_state _state;

            parse_result(Ty&& result)
                : _value(std::move(result))
                , _state(parse_result_state::success)
            {}

            template<class T> 
                requires (!std::same_as<T, void> && std::constructible_from<Ty, T>)
            parse_result(parse_result<T>&& result)
                : _errors(std::move(result._errors))
                , _value(result.has_value() ? std::optional{ Ty{ std::move(result._value.value()) } } : std::nullopt)
                , _state(std::move(result._state))
            {}

            template<class T>
                requires (!std::same_as<T, void> && !std::constructible_from<Ty, T>)
            parse_result(parse_result<T>&& result)
                : _errors(std::move(result._errors))
                , _value(result.has_value() ? std::optional<Ty>{ convert<T, Ty>::operator()(std::move(result._value.value())) } : std::nullopt)
                , _state(std::move(result._state))
            {}

            parse_result(parse_result<void>&& result)
                : _errors(std::move(result._errors))
                , _state(std::move(result._state))
            {}

            bool has_value() const { return _value.has_value(); }
            Ty& value() { return _value.value(); }

            template<class T, class Self>
            Self&& merge_errors(this Self&& self, parse_result<T>&& other) {
                self._errors.append_range(std::move(other._errors));
                return std::forward<Self>(self);
            }

            template<class T, class Self>
            Self&& merge_errors(this Self&& self, const parse_result<T>& other) {
                self._errors.append_range(other._errors);
                return std::forward<Self>(self);
            }

            bool fatal() const { return _state == parse_result_state::fatal; }
            bool recoverable() const { return _state == parse_result_state::recoverable; }
            bool success() const { return _state == parse_result_state::success; }

            // When true, it should return, when false it should continue with the next
            // potential path. If this returns true, it either succeeded, or was fatal
            // and it must stop parsing.
            explicit operator bool() const {
                return _state == parse_result_state::success
                    || _state == parse_result_state::fatal;
            }
        };

        template<>
        struct parse_result<void> {
            using result_type = empty_t;

            std::vector<error_result> _errors;
            parse_result_state _state;

            template<class T, class Self>
            Self&& merge_errors(this Self&& self, parse_result<T>&& other) {
                self._errors.append_range(std::move(other._errors));
                return std::forward<Self>(self);
            }

            template<class T, class Self>
            Self&& merge_errors(this Self&& self, const parse_result<T>& other) {
                self._errors.append_range(other._errors);
                return std::forward<Self>(self);
            }

            bool has_value() const { return false; }
            empty_t value() const { return {}; }

            bool fatal() const { return _state == parse_result_state::fatal; }
            bool recoverable() const { return _state == parse_result_state::recoverable; }
            bool success() const { return _state == parse_result_state::success; }

            // When true, it should return, when false it should continue with the next
            // potential path. If this returns true, it either succeeded, or was fatal
            // and it must stop parsing.
            explicit operator bool() const {
                return _state == parse_result_state::success
                    || _state == parse_result_state::fatal;
            }
        };

        // ------------------------------------------------

        struct context;
        struct backup_struct {

            // ------------------------------------------------

            context* self;
            std::string_view backup;

            // ------------------------------------------------

            void do_revert() { self->value = backup; }

            // ------------------------------------------------

            parse_result<> revert_as_success() {
                do_revert();
                return {
                    ._state = parse_result_state::success,
                };
            }

            parse_result<> warning(error_message message) {
                std::string_view parsed = self->original.substr(0, self->original.size() - self->value.size());
                do_revert();
                return {
                    ._errors = { error_result{ parsed, message } },
                    ._state = parse_result_state::success,
                };
            }

            parse_result<> revert(error_message message) {
                std::string_view parsed = self->original.substr(0, self->original.size() - self->value.size());
                do_revert();
                return {
                    ._errors = { error_result{ parsed, message } },
                    ._state = parse_result_state::recoverable,
                };
            }

            parse_result<> fail(error_message message) {
                std::string_view parsed = self->original.substr(0, self->original.size() - self->value.size());
                do_revert();
                return {
                    ._errors = { error_result{ parsed, message } },
                    ._state = parse_result_state::fatal,
                };
            }

            parse_result<> revert() {
                do_revert();
                return {
                    ._state = parse_result_state::recoverable,
                };
            }

            parse_result<> fail() {
                do_revert();
                return {
                    ._state = parse_result_state::fatal,
                };
            }

            // ------------------------------------------------

        };

        // ------------------------------------------------

        struct context {
            std::string_view original;
            std::string_view value = original;

            bool consume(std::string_view word) {
                if (!value.starts_with(word)) return false;
                value = value.substr(word.size());
                return true;
            }

            backup_struct backup() { return backup_struct{ this, value }; }
            parse_result<> success() { return backup().revert_as_success(); }
            parse_result<> revert(error_message message) { return backup().revert(message); }
            parse_result<> revert() { return backup().revert(); }
            parse_result<> fail(error_message message) { return backup().fail(message); }
            parse_result<> fail() { return backup().fail(); }
            parse_result<> warning(error_message message) { return backup().warning(message); }
        };

        // ------------------------------------------------

        template<class R = void>
        struct symbol {

            // ------------------------------------------------

            using fun = std::function<parse_result<R>(context&)>;

            // ------------------------------------------------

            symbol(const symbol&) = default;
            symbol(symbol&&) = default;

            symbol(symbol& self)
                : def([&self](context& ctx) { return self.def(ctx); })
            {}

		    template<std::invocable<context&> T> 
                requires std::same_as<parse_result<R>, std::invoke_result_t<T, context&>>
            symbol(T&& def)
                : def(std::move(def))
            {}

            template<class T>
                requires (std::constructible_from<std::string, T> && !std::same_as<R, void>)
		    symbol(T&& def) 
                : def([def = std::forward<T>(def)](context& ctx) -> parse_result<std::string> {
                    if (ctx.consume(def)) return std::string(def);
                    return ctx.revert();
                })
            {}

            template<class T>
                requires (std::constructible_from<std::string, T> && std::same_as<R, void>)
		    symbol(T&& def) 
                : def([def = std::forward<T>(def)](context& ctx) -> parse_result<void> {
                    if (ctx.consume(def)) return {};
                    return ctx.revert();
                })
            {}

            fun def;

            parse_result<R> operator()(context& ctx) const { return def(ctx); }

            parse_result<R> parse(std::string_view str) const {
                context ctx{ .original = str };
                return def(ctx);
            }

            template<class Self, class To>
                requires (!std::same_as<To, void> && !std::same_as<To, R>)
            operator symbol<To>(this Self&& self) {
                return [self = std::forward<Self>(self)](context& ctx) -> parse_result<To> { return self(ctx); };
            }

        };

        // ------------------------------------------------
    
        symbol<> ignore(std::string_view a) {
            return [a = std::move(a)](context& ctx) -> parse_result<> {
                auto _ = ctx.backup();
                if (ctx.consume(a)) return ctx.success();
                return _.revert();
            };
        }
    
        template<class Ty>
        symbol<> ignore(symbol<Ty> a) {
            return [a = std::move(a)](context& ctx) -> parse_result<> {
                auto _ = ctx.backup();

                auto res = a(ctx);
                if (res) return ctx.success().merge_errors(res);

                return _.revert();
            };
        }

        // ------------------------------------------------

        symbol<std::optional<std::string>> optional(std::string_view a) {
            return [a = std::move(a)](context& ctx) -> parse_result<std::optional<std::string>> {
                auto _ = ctx.backup();
                if (ctx.consume(a)) return { std::string(a) };
                _.revert();
                return { std::nullopt };
            };
        }

        template<class Ty>
        symbol<std::optional<Ty>> optional(symbol<Ty> a) {
            return [a = std::move(a)](context& ctx) -> parse_result<std::optional<Ty>> {
                auto _ = ctx.backup();

                auto res = a(ctx);
                if (res) return res;

                _.revert();
                return { std::nullopt };
            };
        }

        // ------------------------------------------------

        //parser_fun invert(std::string_view a) {
        //    return [a = std::move(a)](context& ctx) -> parse_result<node> {
        //        auto _ = ctx.backup();
        //        if (ctx.consume(a)) return node{ .result = std::string(a) };
        //        _.revert();
        //        return node{};
        //    };
        //}

        // ------------------------------------------------

        symbol<std::vector<std::string>> many(std::string_view a) {
            return [a = std::move(a)](context& ctx) -> parse_result<std::vector<std::string>> {
                parse_result<std::vector<std::string>> result{ std::vector<std::string>{} };

                while (true) {
                    auto _ = ctx.backup();
                    if (ctx.consume(a)) {
					    result.value().push_back(std::string(a));
                        continue;
                    }
                    _.revert();
                    break;
                }

                return result;
            };
        }

        symbol<std::vector<std::string>> some(std::string_view a) {
            return [a = std::move(a)](context& ctx) -> parse_result<std::vector<std::string>> {
                parse_result<std::vector<std::string>> result{ std::vector<std::string>{} };
                while (true) {
                    auto _ = ctx.backup();
                    if (ctx.consume(a)) {
                        result.value().push_back(std::string(a));
                        continue;
                    }

                    if (result.value().empty()) {
                        return _.revert().merge_errors(result);
                    }
                
                    _.revert();
                    break;
                }

                return result;
            };
        }

        template<class Ty>
        symbol<std::vector<Ty>> many(symbol<Ty> a) {
            return [a = std::move(a)](context& ctx) -> parse_result<std::vector<Ty>> {
                parse_result<std::vector<Ty>> result{ std::vector<Ty>{} };

                while (true) {
                    auto _ = ctx.backup();
                    auto res = a(ctx);
                    if (res) {
                        if (res.fatal()) return _.fail();

                        result.value().push_back(res.value());
					    result.merge_errors(res);
                        continue;
                    }
                    _.revert();
                    break;
                }

                return result;
            };
        }

        template<class Ty>
        symbol<std::vector<Ty>> some(symbol<Ty> a) {
            return [a = std::move(a)](context& ctx) -> parse_result<std::vector<Ty>> {
                parse_result<std::vector<Ty>> result{ std::vector<Ty>{} };
                while (true) {
                    auto _ = ctx.backup();

                    auto res = a(ctx);
                    if (res) {
                        if (res.fatal()) return _.fail();

                        result.value().push_back(res.value());
                        result.merge_errors(res);
                        continue;
                    }

                    if (result.value().empty()) {
                        return _.revert().merge_errors(res);
                    }

                    _.revert();
                    break;
                }

                return result;
            };
        }

        // ------------------------------------------------


        symbol<char> one_of(std::string_view a) {
            return [a = std::move(a)](context& ctx) -> parse_result<char> {
                auto _ = ctx.backup();

                if (ctx.value.empty()) return _.revert();

                char front = ctx.value.front();
                if (a.find(front) == std::string_view::npos) return _.revert();

                return front;
            };
        }

        symbol<char> one_not_of(std::string_view a) {
            return [a = std::move(a)](context& ctx) -> parse_result<char> {
                auto _ = ctx.backup();

			    if (ctx.value.empty()) return _.revert();

                char front = ctx.value.front();
                if (a.find(front) != std::string_view::npos) return _.revert();

                return front;
            };
        }

        // ------------------------------------------------

        template<class A, class B, class R = typename combine<A, B>::result>
        friend symbol<R> operator+(symbol<A> a, symbol<B> b) {
            return [a = std::move(a), b = std::move(b)](context& ctx) -> parse_result<R> {
                auto _ = ctx.backup();

                auto res_a = a(ctx);
                if (!res_a) return _.revert().merge_errors(res_a);

                auto res_b = b(ctx);
                if (!res_b) return _.revert().merge_errors(res_a).merge_errors(res_b);

                return combine<A, B>::operator()(res_a.value(), res_b.value());
            };
        }
    
        template<class A, class R = typename combine<A, std::string>::result>
        friend symbol<R> operator+(symbol<A> a, std::string_view b) {
		    return a + symbol<std::string>(b);
        }

        template<class B, class R = typename combine<std::string, B>::result>
        friend symbol<R> operator+(std::string_view a, symbol<B> b) {
            return symbol<std::string>(a) + b;
        }

        // ------------------------------------------------

        template<class A, class B>
        struct create_variant : std::type_identity<std::variant<A, B>> {};

        template<class T>
        struct create_variant<T, T> : std::type_identity<T> {};

        template<class ...As, class B>
            requires (std::same_as<As, B> || ...)
        struct create_variant<std::variant<As...>, B> : std::type_identity<std::variant<As...>> {};

        template<class ...As, class B>
            requires ((!std::same_as<As, B>) && ...)
        struct create_variant<std::variant<As...>, B> : std::type_identity<std::variant<As..., B>> {};

        template<class A, class ...Bs>
            requires (std::same_as<A, Bs> || ...)
        struct create_variant<A, std::variant<Bs...>> : std::type_identity<std::variant<Bs...>> {};

        template<class A, class ...Bs>
            requires ((!std::same_as<A, Bs>) && ...)
        struct create_variant<A, std::variant<Bs...>> : std::type_identity<std::variant<A, Bs...>> {};

        template<class A, class B, class R = typename create_variant<A, B>::type>
        friend symbol<R> operator|(symbol<A> a, symbol<B> b) {
            return [a = std::move(a), b = std::move(b)](context& ctx) -> parse_result<R> {
                auto _ = ctx.backup();

                auto res_a = a(ctx);
                if (res_a) return res_a;

                auto res_b = b(ctx);
                if (res_b) return res_b;

                return _.revert();
            };
        }

        template<class A, class R = typename create_variant<A, std::string>::type>
        friend symbol<R> operator|(symbol<A> a, std::string_view b) {
            return a | symbol<std::string>(b);
        }

        template<class B, class R = typename create_variant<std::string, B>::type>
        friend symbol<R> operator|(std::string_view a, symbol<B> b) {
            return symbol<std::string>(a) | b;
        }

        // ------------------------------------------------

        template<>
        struct convert<std::vector<char>, std::string> {
            template<class Ta>
            static std::string operator()(Ta&& chars) {
                return std::string(std::begin(chars), std::end(chars));
            }
        };

        // ------------------------------------------------
        
        template<class A, class B>
            requires (!std::same_as<A, void> && !std::same_as<B, void>)
        struct combine<A, B> {
		    using result = std::tuple<A, B>;
            static result operator()(A a, B b) { return std::make_tuple(a, b); }
        };

        template<class A, class ...Bs>
            requires (!std::same_as<A, void>)
        struct combine<A, std::tuple<Bs...>> {
		    using result = std::tuple<A, Bs...>;
            static result operator()(A a, std::tuple<Bs...> b) { return std::tuple_cat(std::make_tuple(a), b); }
        };

        template<class ...As, class B>
            requires (!std::same_as<B, void>)
        struct combine<std::tuple<As...>, B> {
		    using result = std::tuple<As..., B>;
            static result operator()(std::tuple<As...> a, B b) { return std::tuple_cat(a, std::make_tuple(b)); }
        };

        template<class ...As, class ...Bs>
        struct combine<std::tuple<As...>, std::tuple<Bs...>> {
		    using result = std::tuple<As..., Bs...>;
            static result operator()(std::tuple<As...> a, std::tuple<Bs...> b) { return std::tuple_cat(a, b); }
        };

        template<>
        struct combine<void, void> {
            using result = empty_t;

            static empty_t operator()(empty_t, empty_t) { return {}; }
        };

        template<class B>
        struct combine<void, B> {
            using result = B;

            template<class Tb>
            static result operator()(empty_t, Tb&& b) { 
                return static_cast<B>(b); 
            }
        };

        template<class A>
        struct combine<A, void> {
            using result = A;

            template<class Ta>
            static result operator()(Ta&& a, empty_t) { 
                return static_cast<A>(a); 
            }
        };

        template<>
        struct combine<std::string, std::string> {
            using result = std::string;

            static result operator()(std::string_view a, std::string_view b) { 
                return std::string(a) + std::string(b); 
            }
        };

        template<class A, class B>
        struct combine<std::optional<A>, std::optional<B>> {
            using result = std::optional<typename combine<A, B>::result>;

            template<class Ta, class Tb>
            static result operator()(Ta&& a, Tb&& b) {
                if (a && b) return combine<A, B>::operator()(*std::forward<Ta>(a), *std::forward<Tb>(b));
                return std::nullopt;
            }
        };

        template<class A, class B>
            requires (!std::same_as<A, void>)
        struct combine<A, std::optional<B>> {
            using result = std::optional<typename combine<A, B>::result>;

            template<class Ta, class Tb>
            static result operator()(Ta&& a, Tb&& b) {
                if (b) return combine<A, B>::operator()(std::forward<Ta>(a), *std::forward<Tb>(b));
                return std::nullopt;
            }
        };

        template<class A, class B>
            requires (!std::same_as<B, void>)
        struct combine<std::optional<A>, B> {
            using result = std::optional<typename combine<A, B>::result>;

            template<class Ta, class Tb>
            static result operator()(Ta&& a, Tb&& b) {
                if (a) return combine<A, B>::operator()(*std::forward<Ta>(a), std::forward<Tb>(b));
                return std::nullopt;
            }
        };

        template<class T, class B>
            requires (std::constructible_from<T, B> && !std::same_as<B, void>)
        struct combine<std::vector<T>, B> {
            using result = std::vector<T>;

            template<class Va, class Tb>
            static result operator()(Va&& a, Tb&& b) {
                std::vector<T> result = std::forward<Va>(a);
                result.push_back(static_cast<T>(std::forward<Tb>(b)));
                return result;
            }
        };

        template<class A, class T>
			requires (std::constructible_from<T, A> && !std::same_as<A, void>)
        struct combine<A, std::vector<T>> {
            using result = std::vector<T>;

            template<class Ta, class Vb>
            static result operator()(Ta&& a, Vb&& b) {
                std::vector<T> result = std::forward<Vb>(b);
                result.insert(result.begin(), static_cast<A>(std::forward<Ta>(a)));
                return result;
            }
        };

        template<class T>
        struct combine<std::vector<T>, std::vector<T>> {
            using result = std::vector<T>;

            template<class Va, class Vb>
            static result operator()(Va&& a, Vb&& b) {
                std::vector<T> result = std::forward<Va>(a);
                result.insert_range(std::begin(result), std::forward<Vb>(b));
                return result;
            }
        };

        // ------------------------------------------------

    };

    // ------------------------------------------------

    //parser::parser_fun operator ""_symbol(const char* str, std::size_t n) {
    //    return [str = std::string_view(str, n)](parser::context& ctx) -> parser::parse_result<parser::node> {
    //        if (ctx.consume(str)) return parser::node{ .result = std::string(str) };
    //        return ctx.revert();
    //    };
    //}
    //
    //parser::parser_fun operator ""_regex(const char* str, std::size_t n) {
    //    return [regex = std::regex(str)](parser::context& ctx) -> parser::parse_result<parser::node> {
    //        auto it = std::regex_iterator<std::string_view::iterator>(ctx.value.begin(), ctx.value.end(), regex);
    //        if (it == std::regex_iterator<std::string_view::iterator>{}) {
    //            return ctx.revert();
    //        }
    //
    //        parser::node result{ .result = it->str() };
    //        ctx.value = ctx.value.substr(result.result.size());
    //        return result;
    //    };
    //}

    // ------------------------------------------------

    //struct json_parser : parser {
    //
    //    symbol start = object;
    //
    //    symbol object = "{" + optional(member + many("," + member)) + "}";
    //    symbol array = "[" + optional(value + many("," + value)) + "]";
    //    symbol string = "\"" + many(one_not_of("\"")) + "\"";
    //    symbol number = "[0-9]*"_regex;
    //
    //    symbol value = string
    //                 | object
    //                 | array
    //                 | "true"
    //                 | "false"
    //                 | "null"
    //                 ;
    //
    //    symbol member = string + ":" + value;
    //
    //};
}

using namespace kaixo;

template<>
struct parser::convert<std::string, bool> {
    template<class Ty>
    static bool operator()(Ty&& str) {
        if (str == "true") return true;
        return false;
    }
};

template<>
struct parser::convert<std::variant<std::string, bool, basic_json>, basic_json> {
    static basic_json operator()(std::variant<std::string, bool, basic_json> val) {
        return std::visit([](auto& v) { return basic_json(v); }, val);
    }
};

template<>
struct parser::convert<std::optional<std::vector<std::tuple<std::string, basic_json>>>, basic_json> {
    static basic_json operator()(std::optional<std::vector<std::tuple<std::string, basic_json>>> val) {
        if (!val) return basic_json::object_t{};

        basic_json result = basic_json::object_t{};
		for (auto& [key, value] : *val) {
            result[std::move(key)] = std::move(value);
        }

        return result;
    }
};

template<>
struct parser::convert<std::optional<std::vector<basic_json>>, basic_json> {
    static basic_json operator()(std::optional<std::vector<basic_json>> val) {
        if (!val) return basic_json::array_t{};

        basic_json result = basic_json::array_t{};
		for (auto& v : *val) {
            result.push_back(std::move(v));
        }

        return result;
    }
};

struct json_parser : kaixo::parser {
    
    symbol<> lbracket = "[";
    symbol<> rbracket = "]";
    symbol<> lbrace = "{";
    symbol<> rbrace = "}";
    symbol<> comma = ",";
    symbol<> quote = "\"";
    symbol<> colon = ":";
    
    symbol<bool> true_string = "true";
    symbol<bool> false_string = "false";
	symbol<bool> boolean = true_string | false_string;

    symbol<std::string> string = quote + many(one_not_of("\"")) + quote;

    symbol<basic_json> value = string | boolean | object | array;
    
    symbol<std::tuple<std::string, basic_json>> member = string + colon + value;
    symbol<basic_json> object = lbrace + optional(many(member + comma) + member) + rbrace;
    symbol<basic_json> array = lbracket + optional(many(value + comma) + value) + rbracket;
};

int main() {
    json_parser parser{};

	auto res = parser.boolean.parse("true");


    return 0;
}