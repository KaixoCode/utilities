
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

// ------------------------------------------------

struct parser {
    
    // ------------------------------------------------

    struct error_message {

        // ------------------------------------------------

        std::string_view message;

        // ------------------------------------------------

        template<std::size_t N> // Consteval enforces error message to be string literal
        consteval error_message(const char(&msg)[N])
            : message(msg)
        {
        }

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
        {
        }

        result(Ty&& result)
            : _value(std::move(result))
        {
        }

        template<class T> requires (!std::same_as<Ty, void>&& std::constructible_from<Ty, T>)
            result(T&& result)
            : _value(std::move(result))
        {
        }

        template<class T> requires (!std::same_as<Ty, void>&& std::constructible_from<Ty, T>)
            result(parse_result<T>&& result)
            : _errors(result._errors.begin(), result._errors.end())
            , _value(std::move(result._value))
        {
        }

        result(parse_result<void>&& result)
            : _errors(result._errors.begin(), result._errors.end())
        {
        }

        const std::vector<error>& errors() const { return _errors; }
        explicit operator bool() const { return _value.has_value(); }
        bool has_value() const { return _value.has_value(); }
        Ty& value() { return _value.value(); }

        Ty* operator->() { return &_value.value(); }
    };

    // ------------------------------------------------

    enum class parse_result_state {
        success,     // Parsed successfully 
        recoverable, // Failed to parse, but continue
        fatal,       // Fatal parse error, non-recoverable
    };

    template<class Ty>
    struct parse_result {
        std::vector<error_result> _errors;
        std::optional<Ty> _value;
        parse_result_state _state;

        parse_result(Ty&& result)
            : _value(std::move(result))
            , _state(parse_result_state::success)
        {}

        template<class T> requires (!std::same_as<T, void>&& std::constructible_from<Ty, T>)
        parse_result(parse_result<T>&& result)
            : _errors(std::move(result._errors))
            , _value(result.has_value() ? std::optional{ Ty{ std::move(result._value.value()) } } : std::nullopt)
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
        void value() const { return; }

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

    struct node {
        std::vector<node> nodes;
        std::string result;
    };

    // ------------------------------------------------

    using parser_fun = std::function<parse_result<node>(context&)>;

    // ------------------------------------------------

    struct symbol {

        symbol(symbol& self) 
            : def([&self](context& ctx) { return self.def(ctx); }) 
        {}

        symbol(parser_fun&& def)
            : def(def)
        {}

        symbol(std::string_view def)
            : def([def = def](context& ctx) -> parse_result<node> { 
                if (ctx.consume(def)) return node{ .result = std::string(def) };
                return ctx.revert();
            })
        {}

        parser_fun def;

        operator parser_fun (this symbol& self) { 
            return [&self](context& ctx) { return self.def(ctx); };
        }

        parse_result<node> parse(std::string_view str) const {
            context ctx{ .original = str };
            return def(ctx);
        }
    };

    // ------------------------------------------------

    parser_fun optional(std::string_view a) {
        return [a = std::move(a)](context& ctx) -> parse_result<node> {
            auto _ = ctx.backup();
            if (ctx.consume(a)) return node{ .result = std::string(a) };
            _.revert();
            return node{};
        };
    }

    parser_fun optional(parser_fun&& a) {
        return [a = std::move(a)](context& ctx) -> parse_result<node> {
            auto _ = ctx.backup();

            auto res = a(ctx);
            if (res) return res;

            _.revert();
            return node{};
        };
    }

    // ------------------------------------------------
    
    parser_fun invert(std::string_view a) {
        return [a = std::move(a)](context& ctx) -> parse_result<node> {
            auto _ = ctx.backup();
            if (ctx.consume(a)) return node{ .result = std::string(a) };
            _.revert();
            return node{};
        };
    }

    // ------------------------------------------------

    parser_fun many(std::string_view a) {
        return [a = std::move(a)](context& ctx) -> parse_result<node> {
            auto _ = ctx.backup();
            if (ctx.consume(a)) return node{ .result = std::string(a) };
            _.revert();
            return node{};
        };
    }

    parser_fun some(std::string_view a) {
        return [a = std::move(a)](context& ctx) -> parse_result<node> {
            auto _ = ctx.backup();
            if (ctx.consume(a)) return node{ .result = std::string(a) };
            _.revert();
            return node{};
        };
    }

    parser_fun many(parser_fun&& a) {
        return [a = std::move(a)](context& ctx) -> parse_result<node> {
            auto _ = ctx.backup();

            auto res = a(ctx);
            if (res) return res;

            _.revert();
            return node{};
        };
    }

    parser_fun some(parser_fun&& a) {
        return [a = std::move(a)](context& ctx) -> parse_result<node> {
            auto _ = ctx.backup();

            auto res = a(ctx);
            if (res) return res;

            _.revert();
            return node{};
        };
    }

    // ------------------------------------------------
    
    parser_fun one_of(std::string_view a) {
        return [a = std::move(a)](context& ctx) -> parse_result<node> {
            auto _ = ctx.backup();
            if (ctx.consume(a)) return node{ .result = std::string(a) };
            _.revert();
            return node{};
        };
    }
    
    parser_fun one_not_of(std::string_view a) {
        return [a = std::move(a)](context& ctx) -> parse_result<node> {
            auto _ = ctx.backup();
            if (ctx.consume(a)) return node{ .result = std::string(a) };
            _.revert();
            return node{};
        };
    }

    // ------------------------------------------------

    friend parser_fun operator+(parser_fun&& a, std::string_view b) {
        return [a = std::move(a), b = std::move(b)](context& ctx) -> parse_result<node> {
            auto _ = ctx.backup();
                
            auto res = a(ctx);
            if (!res) return _.revert().merge_errors(res);
            if (!ctx.consume(b)) return _.revert().merge_errors(res);

            return node{ .nodes {
                std::move(res.value()),
                node{ .result = std::string(b) },
            } };
        };
    }

    friend parser_fun operator+(std::string_view a, parser_fun&& b) {
        return [a = std::move(a), b = std::move(b)](context& ctx) -> parse_result<node> {
            auto _ = ctx.backup();

            if (!ctx.consume(a)) return _.revert();
            auto res = b(ctx);
            if (!res) return _.revert().merge_errors(res);

            return node{ .nodes {
                node{.result = std::string(a) },
                std::move(res.value()),
            } };
        };
    }

    friend parser_fun operator+(parser_fun&& a, parser_fun&& b) {
        return [a = std::move(a), b = std::move(b)](context& ctx) -> parse_result<node> {
            auto _ = ctx.backup();

            auto res_a = a(ctx);
            if (!res_a) return _.revert().merge_errors(res_a);

            auto res_b = b(ctx);
            if (!res_b) return _.revert().merge_errors(res_a).merge_errors(res_b);

            return node{ .nodes {
                std::move(res_a.value()),
                std::move(res_b.value()),
            } };
        };
    }

    friend parser_fun operator|(parser_fun&& a, parser_fun&& b) {
        return [a = std::move(a), b = std::move(b)](context& ctx) -> parse_result<node> {
            auto _ = ctx.backup();

            auto res_a = a(ctx);
            if (res_a) return res_a;

            auto res_b = b(ctx);
            if (res_b) return res_b;

            return _.revert();
        };
    }

    friend parser_fun operator|(std::string_view a, parser_fun&& b) {
        return [a = std::move(a), b = std::move(b)](context& ctx) -> parse_result<node> {
            auto _ = ctx.backup();

            if (ctx.consume(a)) return node{ .result = std::string(a) };

            auto res_b = b(ctx);
            if (res_b) return res_b;

            return _.revert();
        };
    }

    friend parser_fun operator|(parser_fun&& a, std::string_view b) {
        return [a = std::move(a), b = std::move(b)](context& ctx) -> parse_result<node> {
            auto _ = ctx.backup();

            auto res_a = a(ctx);
            if (res_a) return res_a;

            if (ctx.consume(b)) return node{ .result = std::string(b) };

            return _.revert();
        };
    }
};

parser::parser_fun operator ""_symbol(const char* str, std::size_t n) {
    return [str = std::string_view(str, n)](parser::context& ctx) -> parser::parse_result<parser::node> {
        if (ctx.consume(str)) return parser::node{ .result = std::string(str) };
        return ctx.revert();
    };
}

parser::parser_fun operator ""_regex(const char* str, std::size_t n) {
    return [regex = std::regex(str)](parser::context& ctx) -> parser::parse_result<parser::node> {
        auto it = std::regex_iterator<std::string_view::iterator>(ctx.value.begin(), ctx.value.end(), regex);
        if (it == std::regex_iterator<std::string_view::iterator>{}) {
            return ctx.revert();
        }

        parser::node result{ .result = it->str() };
        ctx.value = ctx.value.substr(result.result.size());
        return result;
    };
}

struct my_parser : parser {

    symbol start = expr
                 ;

    symbol expr = product + "+" + expr
                | product + "-" + expr
                | product
                ;

    symbol product = factor + "*" + product
                   | factor + "/" + product
                   | factor
                   ;

    symbol factor = "(" + expr + ")"
                  | NUMBER
                  ;

    symbol NUMBER = "0|[0-9][1-9]*"_regex;
};

struct json_parser : parser {

    symbol start = object;

    symbol object = "{" + optional(member + many(("\n"_symbol | ",") + member) + optional(",")) + "}";
    symbol array  = "[" + optional(value  + many(("\n"_symbol | ",") + value ) + optional(",")) + "]";

    symbol member = (json_string | some(one_not_of(",:[]{} \t\n\r\f\v"))) + ":" + value;

    symbol value = string
                 | number
                 | object
                 | array
                 | "true"
                 | "false"
                 | "null"
                 ;

    symbol string = json_string
                  | quoteless_string
                  | multiline_string
                  ;

    symbol json_string = ("\""_symbol | "'") + many(one_not_of("'\"\\")) + ("\""_symbol | "'");

    symbol quoteless_string = one_not_of(",:[]{}") + many(one_not_of("\n"));

    symbol multiline_string = "'''" + many(invert("'''")) + "'''";

    symbol number;

};

// ------------------------------------------------

#include <memory>

int main() {

    my_parser p;

    auto result = p.start.parse("(1+2)*9");

    return 0;
}