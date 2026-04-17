
// ------------------------------------------------

#include <string>
#include <vector>
#include <algorithm>
#include <utility>
#include <functional>
#include <type_traits>
#include <concepts>
#include <string_view>
#include <regex>
#include <variant>

// ------------------------------------------------

#include "json.hpp"
#include <charconv>
#include <cmath>
#include <cstddef>
#include <cassert>
#include <cstdint>
#include <format>
#include <initializer_list>
#include <iterator>
#include <optional>
#include <tuple>

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

        template<class Ty>
        struct vector_of : std::type_identity<std::vector<Ty>> {};

        template<>
        struct vector_of<char> : std::type_identity<std::string> {};

        // ------------------------------------------------

        enum class parse_result_state {
            success,     // Parsed successfully 
            recoverable, // Failed to parse, but continue
            fatal,       // Fatal parse error, non-recoverable
        };

        template<class Ty>
        struct parse_result {

            // ------------------------------------------------

            using result_type = Ty;

            // ------------------------------------------------

            std::vector<error_result> _errors;
            std::optional<Ty> _value;
            parse_result_state _state;

            // ------------------------------------------------

            parse_result(Ty&& result)
                : _value(std::move(result))
                , _state(parse_result_state::success)
            {}

            // ------------------------------------------------

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

            // ------------------------------------------------

            bool has_value() const { return _value.has_value(); }
            Ty& value() { return _value.value(); }

            // ------------------------------------------------

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

            // ------------------------------------------------

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

            // ------------------------------------------------

        };

        template<>
        struct parse_result<void> {

            // ------------------------------------------------

            using result_type = empty_t;

            // ------------------------------------------------

            std::vector<error_result> _errors;
            parse_result_state _state;

            // ------------------------------------------------

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

            // ------------------------------------------------

            bool has_value() const { return false; }
            empty_t value() const { return {}; }

            // ------------------------------------------------

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

            // ------------------------------------------------

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

            // ------------------------------------------------

            std::string_view original;
            std::string_view value = original;

            // ------------------------------------------------

            bool consume(std::string_view word) {
                if (!value.starts_with(word)) return false;
                value = value.substr(word.size());
                return true;
            }

            // ------------------------------------------------

            backup_struct backup() { return backup_struct{ this, value }; }
            parse_result<> success() { return backup().revert_as_success(); }
            parse_result<> revert(error_message message) { return backup().revert(message); }
            parse_result<> revert() { return backup().revert(); }
            parse_result<> fail(error_message message) { return backup().fail(message); }
            parse_result<> fail() { return backup().fail(); }
            parse_result<> warning(error_message message) { return backup().warning(message); }

            // ------------------------------------------------

        };

        // ------------------------------------------------
        
        template<class R = void>
        struct token;

        // ------------------------------------------------

        template<class R = void>
        struct symbol {

            // ------------------------------------------------

            symbol(const symbol&) = default;
            symbol(symbol&&) = default;

            // ------------------------------------------------
            
		    template<std::invocable<context&> T> 
                requires std::same_as<parse_result<R>, std::invoke_result_t<T, context&>>
            symbol(T&& fun)
                : _def(std::move(fun))
            {}

            // ------------------------------------------------

            template<class T>
            symbol(symbol<T>& self)
                : _def([&self](context& ctx) { return self(ctx); })
            {}

            template<class T>
            symbol(symbol<T>&& self)
                : _def([self = std::move(self)](context& ctx) { return self(ctx); })
            {}

            template<class T>
            symbol(token<T>& self)
                : _def([&self](context& ctx) { return self(ctx); })
            {}

            template<class T>
            symbol(token<T>&& self)
                : _def([self = std::move(self)](context& ctx) { return self(ctx); })
            {}

            // ------------------------------------------------
            
            template<class T>
                requires std::constructible_from<std::string, T>
		    symbol(T&& str) 
                : _def([str = std::forward<T>(str)](context& ctx) -> parse_result<std::string> {
                    if (ctx.consume(str)) return std::string(str);
                    return ctx.revert();
                })
            {}

            // ------------------------------------------------

            parse_result<R> operator()(context& ctx) const { return _def(ctx); }

            parse_result<R> parse(std::string_view str) const {
                context ctx{ .original = str };
                return _def(ctx);
            }

            // ------------------------------------------------

        private:
            std::function<parse_result<R>(context&)> _def;

            // ------------------------------------------------

        };

        // ------------------------------------------------
    
        template<class R>
        struct token : symbol<R> {

            // ------------------------------------------------

            template<class T>
            static symbol<R> ignore(symbol<T> val, std::string_view ignore = " \t\n\r") {
                return [val = std::move(val), ignore](context& ctx) -> parse_result<R> {
                    auto _ = ctx.backup();
                    auto preignore = ctx.value.find_first_not_of(ignore);
                    if (preignore != std::string_view::npos) {
                        ctx.value = ctx.value.substr(preignore);
                    }

                    auto res = val(ctx);
                    if (res) {
                        auto postignore = ctx.value.find_first_not_of(ignore);
                        if (postignore != std::string_view::npos) {
                            ctx.value = ctx.value.substr(postignore);
                        }

                        if constexpr (std::same_as<T, R>) {
                            return res;
                        } else if constexpr (std::same_as<R, void>) {
                            return ctx.success().merge_errors(res);
                        } else {
                            return parse_result<R>(convert<T, R>::operator()(std::move(res.value()))).merge_errors(res);
                        }
                    }

                    return _.revert();
                };
            }

            // ------------------------------------------------

            template<class T>
            token(symbol<T> val, std::string_view to_ignore = " \t\n\r")
                : symbol<R>(ignore(std::move(val), to_ignore))
            { }
        
            template<class T>
                requires std::constructible_from<std::string, T>
            token(T&& a, std::string_view to_ignore = " \t\n\r")
                : symbol<R>(ignore(symbol<std::string>(std::forward<T>(a)), to_ignore))
            { }

            // ------------------------------------------------

		};
    
        // ------------------------------------------------
    
        template<class Ty>
        symbol<> ignore(symbol<Ty> a) {
            return [a = std::move(a)](context& ctx) -> parse_result<> {
                auto _ = ctx.backup();

                auto res = a(ctx);
                if (res) return ctx.success().merge_errors(res);

                return _.revert();
            };
        }

        symbol<> ignore(std::string_view a) {
            return ignore(symbol<std::string>(a));
        }

        // ------------------------------------------------

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

        template<class Ty>
        symbol<std::optional<Ty>> optional(symbol<std::optional<Ty>> a) {
            return [a = std::move(a)](context& ctx) -> parse_result<std::optional<Ty>> {
                auto _ = ctx.backup();

                auto res = a(ctx);
                if (res) return res;

                _.revert();
                return { std::nullopt };
            };
        }

        template<class Ty>
        symbol<std::vector<Ty>> optional(symbol<std::vector<Ty>> a) { return std::move(a); }
        
        symbol<std::optional<std::string>> optional(std::string_view a) {
			return optional(symbol<std::string>(a));
        }

        template<class Ty>
        symbol<bool> has_value(symbol<std::optional<Ty>> a) {
            return [a = std::move(a)](context& ctx) -> parse_result<bool> {
                auto res = a(ctx);
				if (res.has_value() && res.value().has_value()) return true;
                return false;
            };
        }

        template<class Ty>
        symbol<bool> flag(Ty&& a) {
            return has_value(optional(std::forward<Ty>(a)));
        }

        // ------------------------------------------------

        template<class Ty, class V = typename vector_of<Ty>::type>
        symbol<V> many(symbol<Ty> a) {
            return [a = std::move(a)](context& ctx) -> parse_result<V> {
                parse_result<V> result{ V{} };

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

        template<class Ty, class V = typename vector_of<Ty>::type>
        symbol<V> some(symbol<Ty> a) {
            return [a = std::move(a)](context& ctx) -> parse_result<V> {
                parse_result<V> result{ V{} };
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
        
        symbol<std::vector<std::string>> many(std::string_view a) {
            return many(symbol<std::string>(a));
        }

        symbol<std::vector<std::string>> some(std::string_view a) {
            return some(symbol<std::string>(a));
        }

        // ------------------------------------------------

        symbol<char> one_of(std::string_view a) {
            return [a = std::move(a)](context& ctx) -> parse_result<char> {
                auto _ = ctx.backup();

                if (ctx.value.empty()) return _.revert();

                char front = ctx.value.front();
                if (a.find(front) == std::string_view::npos) return _.revert();
				ctx.value = ctx.value.substr(1);

                return front;
            };
        }

        symbol<char> one_not_of(std::string_view a) {
            return [a = std::move(a)](context& ctx) -> parse_result<char> {
                auto _ = ctx.backup();

			    if (ctx.value.empty()) return _.revert();

                char front = ctx.value.front();
                if (a.find(front) != std::string_view::npos) return _.revert();
                ctx.value = ctx.value.substr(1);

                return front;
            };
        }

        // ------------------------------------------------

        symbol<std::string> regex(std::string_view a) {
            return [regex = std::regex(std::string(a))](parser::context& ctx) -> parser::parse_result<std::string> {
                auto it = std::regex_iterator<std::string_view::iterator>(ctx.value.begin(), ctx.value.end(), regex);
                if (it == std::regex_iterator<std::string_view::iterator>{}) {
                    return ctx.revert();
                }

                std::string result = it->str();
                ctx.value = ctx.value.substr(result.size());
                return result;
            };
        }

        // ------------------------------------------------

        template<class To, class Ty>
        symbol<To> as(symbol<Ty> a) {
            return [a = std::move(a)](context& ctx) -> parse_result<To> {
                auto res = a(ctx);
                if (!res) return ctx.revert().merge_errors(res);
				if (res.fatal()) return ctx.fail().merge_errors(res);
                return parse_result<To>(convert<Ty, To>::operator()(std::move(res.value()))).merge_errors(res);
            };
        }
        
        template<class To>
        symbol<To> as(std::string_view a) {
			return as<To>(symbol<std::string>(a));
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

                return parse_result<R>(combine<A, B>::operator()(res_a.value(), res_b.value()))
                    .merge_errors(res_a).merge_errors(res_b);
            };
        }
    
        template<class A, class R = typename combine<A, std::string>::result>
        friend symbol<R> operator+(symbol<A> a, std::string_view b) {
		    return std::move(a) + symbol<std::string>(b);
        }

        template<class B, class R = typename combine<std::string, B>::result>
        friend symbol<R> operator+(std::string_view a, symbol<B> b) {
            return symbol<std::string>(a) + std::move(b);
        }

        // ------------------------------------------------

        template<class A, class B>
        struct create_variant : std::type_identity<std::variant<A, B>> {};

        template<class T>
        struct create_variant<T, T> : std::type_identity<T> {};

        template<class T>
        struct create_variant<void, T> : std::type_identity<T> {};

        template<class T>
        struct create_variant<T, void> : std::type_identity<T> {};

        template<class ...As>
        struct create_variant<std::variant<As...>, void> : std::type_identity<std::variant<As...>> {};

        template<class ...Bs>
        struct create_variant<void, std::variant<Bs...>> : std::type_identity<std::variant<Bs...>> {};

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

        // ------------------------------------------------

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
            return std::move(a) | symbol<std::string>(b);
        }

        template<class B, class R = typename create_variant<std::string, B>::type>
        friend symbol<R> operator|(std::string_view a, symbol<B> b) {
            return symbol<std::string>(a) | std::move(b);
        }

        // ------------------------------------------------

        template<class Ty>
            requires std::is_arithmetic_v<Ty>
        struct convert<std::string, Ty> {
            static Ty operator()(std::string_view sv) {
                Ty value = Ty{};
                std::from_chars(sv.data(), sv.data() + sv.size(), value);
                return value;
            }
        };

        template<class Ty>
        struct convert<Ty, std::nullptr_t> {
            static std::nullptr_t operator()(Ty) { return {}; }
        };

        template<class A, class B>
            requires std::convertible_to<A, B>
        struct convert<A, B> {
            static B operator()(A value) { return static_cast<B>(value); }
        };

        template<>
        struct convert<std::vector<char>, std::string> {
            static std::string operator()(std::vector<char> chars) {
                return std::string(std::begin(chars), std::end(chars));
            }
        };

        template<class ...As, class ...Bs>
        struct convert<std::variant<As...>, std::variant<Bs...>> {
            static std::variant<Bs...> operator()(std::variant<As...> v) {
                return std::visit([](auto&& v) { 
					static_assert(std::constructible_from<std::variant<Bs...>, decltype(v)>, "Cannot convert variant type");
                    return std::variant<Bs...>(std::move(v)); 
                }, std::move(v));
            }
        };

        template<class ...Vs, class T>
            requires (std::constructible_from<T, Vs> || ...)
        struct convert<std::variant<Vs...>, T> {
            static T operator()(std::variant<Vs...> val) {
                return std::visit([](auto&& v) { return static_cast<T>(v); }, std::move(val));
            }
        };

        template<class ...As, class ...Bs>
        struct convert<std::tuple<As...>, std::tuple<Bs...>> {
            static std::tuple<Bs...> operator()(std::tuple<As...> val) {
                return std::apply([](auto&&... args) { 
                    return std::tuple<Bs...>{ convert<As, Bs>::operator()(std::move(args))... };
				}, std::move(val));
            }
        };

        // ------------------------------------------------
        
        template<class A, class B>
            requires (!std::same_as<A, void> && !std::same_as<B, void>)
        struct combine<A, B> {
		    using result = std::tuple<A, B>;

            static result operator()(A a, B b) { 
                return std::make_tuple(std::move(a), std::move(b)); 
            }
        };

        template<class A, class ...Bs>
            requires (!std::same_as<A, void>)
        struct combine<A, std::tuple<Bs...>> {
		    using result = std::tuple<A, Bs...>;

            static result operator()(A a, std::tuple<Bs...> b) {
                return std::tuple_cat(std::make_tuple(std::move(a)), std::move(b)); 
            }
        };

        template<class ...As, class B>
            requires (!std::same_as<B, void>)
        struct combine<std::tuple<As...>, B> {
		    using result = std::tuple<As..., B>;

            static result operator()(std::tuple<As...> a, B b) {
                return std::tuple_cat(std::move(a), std::make_tuple(std::move(b)));
            }
        };

        template<class ...As, class ...Bs>
        struct combine<std::tuple<As...>, std::tuple<Bs...>> {
		    using result = std::tuple<As..., Bs...>;

            static result operator()(std::tuple<As...> a, std::tuple<Bs...> b) {
                return std::tuple_cat(std::move(a), std::move(b)); 
            }
        };

        template<>
        struct combine<void, void> {
            using result = empty_t;

            static empty_t operator()(empty_t, empty_t) { return {}; }
        };

        template<class B>
        struct combine<void, B> {
            using result = B;

            static result operator()(empty_t, B b) { return b; }
        };

        template<class A>
        struct combine<A, void> {
            using result = A;

            static result operator()(A a, empty_t) { return a; }
        };

        template<>
        struct combine<std::string, std::string> {
            using result = std::string;

            static result operator()(std::string a, std::string b) { 
                return std::move(a) + std::move(b);
            }
        };

        template<>
        struct combine<std::string, char> {
            using result = std::string;

            static result operator()(std::string a, char b) { 
                return std::move(a) + b; 
            }
        };

        template<>
        struct combine<char, std::string> {
            using result = std::string;

            static result operator()(char a, std::string b) {
                return a + std::move(b); 
            }
        };

        template<>
        struct combine<char, char> {
            using result = std::string;

            static result operator()(char a, char b) { 
                return std::string(1, a) + b; 
            }
        };

        template<>
        struct combine<std::string, std::optional<std::string>> {
            using result = std::string;

            static result operator()(std::string a, std::optional<std::string> b) {
                return b ? std::move(a) + *std::move(b) : std::move(a);
            }
        };

        template<>
        struct combine<std::optional<std::string>, std::string> {
            using result = std::string;

            static result operator()(std::optional<std::string> a, std::string b) {
                return a ? *std::move(a) + std::move(b) : std::move(b);
            }
        };

        template<class T, class B>
            requires (std::constructible_from<T, B> && !std::same_as<B, void>)
        struct combine<std::vector<T>, B> {
            using result = std::vector<T>;

            static result operator()(std::vector<T> a, B b) {
                a.push_back(static_cast<T>(std::move(b)));
                return a;
            }
        };

        template<class A, class T>
			requires (std::constructible_from<T, A> && !std::same_as<A, void>)
        struct combine<A, std::vector<T>> {
            using result = std::vector<T>;

            static result operator()(A a, std::vector<T> b) {
                b.insert(b.begin(), static_cast<T>(std::move(a)));
                return b;
            }
        };

        template<class T>
        struct combine<std::vector<T>, std::vector<T>> {
            using result = std::vector<T>;

            static result operator()(std::vector<T> a, std::vector<T> b) {
                a.insert_range(std::begin(a), std::move(b));
                return a;
            }
        };

        // ------------------------------------------------

    };

    // ------------------------------------------------

}

// ------------------------------------------------

namespace kaixo {
    template<>
    struct parser::convert<std::string, bool> {
        template<class Ty>
        static bool operator()(Ty&& str) {
            if (str == "true") return true;
            return false;
        }
    };

    template<>
    struct parser::convert<std::vector<std::tuple<std::string, basic_json>>, basic_json> {
        static basic_json operator()(std::vector<std::tuple<std::string, basic_json>> val) {
            basic_json result = basic_json::object_t{};
		    for (auto& [key, value] : val) {
                result[std::move(key)] = std::move(value);
            }

            return result;
        }
    };

    template<>
    struct parser::convert<std::tuple<bool, std::size_t, std::optional<std::size_t>, std::optional<std::tuple<bool, std::size_t>>>, basic_json> {
        static basic_json operator()(std::tuple<bool, std::size_t, std::optional<std::size_t>, std::optional<std::tuple<bool, std::size_t>>> val) {
            auto& [neg, whole, frac, exp] = val;

            if (frac.has_value() || exp.has_value()) {
                double result = static_cast<double>(whole);
            
                if (frac) {
                    std::size_t f = *frac;
                    std::size_t digits = 0;
                    std::size_t tmp = f;
                    if (tmp == 0) {
                        digits = 1;
                    } else {
                        while (tmp > 0) {
                            tmp /= 10;
                            ++digits;
                        }
                    }

                    result += f / std::pow(10.0, digits);
                }

                if (exp) {
                    auto& [e_sign, e_val] = *exp;
                    std::int64_t signed_exp = e_sign ? -e_val : e_val;
                    result *= std::pow(10.0, signed_exp);
                }

                if (neg) result = -result;
                return result;
            }

            if (neg) {
                return -static_cast<int64_t>(whole);
            } else {
                return whole;
            }
        }
    };
}

using namespace kaixo;

struct json_parser : kaixo::parser {
    token<> colon = ":";
    token<> lbracket = "{";
	token<> rbracket = "}";
	token<> lbrace = "[";
	token<> rbrace = "]";
	token<> comma = ",";

	symbol<char> digit = one_of("0123456789");
	symbol<std::size_t> digits = some(digit);

    symbol<basic_json> number = flag("-")                           // Signedness
        + as<std::size_t>("0" | one_of("123456789") + many(digit)) // Whole part
        + optional(ignore(".") + digits)                           // Fractional part
        + optional(ignore(one_of("eE")) + (flag("-") | ignore("+")) + digits); // Exponent

    symbol<bool> boolean = as<bool>("true") | as<bool>("false");

    symbol<std::nullptr_t> null = "null";

    symbol<std::string> string = ignore("\"") + many(one_not_of("\"")) + ignore("\"");

    token<basic_json> object = lbracket
                             + optional(many(string + colon + value + comma) + (string + colon + value)) 
                             + rbracket;

    token<basic_json> array  = lbrace
                             + optional(many(value + comma) + value) 
                             + rbrace;

    token<basic_json> value = null
                            | string
                            | boolean 
                            | object 
                            | array 
                            | number;
};

int main() {
    json_parser parser{};

 	auto res = parser.object.parse(R"({ "test" : 1.123E2, "woof": true, "carrot": null, "value": "string thing" })");
    if (res) {
        basic_json& value = res.value();
    
        double test = value["test"].as<double>();
		assert(test == 112.3);
    }

    return 0;
}

