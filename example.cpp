
// ------------------------------------------------

#include <iostream>
#include <string>
#include <source_location>
#include <vector>
#include <map>
#include <ranges>
#include <algorithm>
#include <utility>
#include <functional>
#include <cassert>
#include <expected>
#include <any>

// ------------------------------------------------

#include "pack_utils/pack_utils.hpp"

// ------------------------------------------------

struct basic_parser {

    // ------------------------------------------------

    constexpr static std::string_view whitespace = " \t\n\r\f\v";

    // ------------------------------------------------

    std::string_view original;
    std::string_view input;
    std::string_view ignoring; // characters currently ignoring

    // ------------------------------------------------

    struct undo {

        // ------------------------------------------------

        undo(basic_parser* self, std::string_view backup, bool success = false)
            : m_Self(self), m_Backup(backup), m_Committed(success)
        {}

        ~undo() noexcept { if (!m_Committed) m_Self->input = m_Backup; }

        // ------------------------------------------------

        void success() { m_Committed = true; }
        void fail() { m_Committed = false; }

        // ------------------------------------------------

    private:
        basic_parser* m_Self;
        std::string_view m_Backup;
        bool m_Committed;

        // ------------------------------------------------

    };

    // ------------------------------------------------
    
    template<class Ty = void>
    using result = std::expected<Ty, std::string>;
    using error = std::unexpected<std::string>;

    // ------------------------------------------------
    
    error die_impl(std::string_view message) const {
        std::size_t parsed = original.size() - input.size();
        std::size_t newlines = 1;
        std::size_t charsInLine = 1;
        for (auto& c : original.substr(0, parsed)) {
            ++charsInLine;
            if (c == '\n') {
                ++newlines;
                charsInLine = 1;
            }
        }

        return std::unexpected(std::format("line {}, character {}: {}", newlines, charsInLine, message));
    }

    // ------------------------------------------------
    
    undo probably_success() { return { this, input, true }; }
    undo probably_fail() { return { this, input, false }; }

    // ------------------------------------------------
    
    bool eof() const { return input.empty(); }

    // ------------------------------------------------

    bool is_next(char c) const {
        return input.starts_with(c);
    }

    bool is_next(std::string_view word) const {
        return input.starts_with(word);
    }
    
    // ------------------------------------------------
    
    void do_ignore() {
        ignore_impl(ignoring);
    }

    // ------------------------------------------------

    bool consume_impl(char c, bool ignore = true) {
        if (ignore) do_ignore();
        if (eof() || !is_next(c)) return false;
        input = input.substr(1);
        return true;
    }

    bool consume_impl(std::string_view word, bool ignore = true) {
        if (ignore) do_ignore();
        if (eof() || !is_next(word)) return false;
        input = input.substr(word.size());
        return true;
    }

    // ------------------------------------------------

    bool consume_one_of_impl(std::string_view oneOf, bool ignore = true) {
        if (ignore) do_ignore();
        if (eof()) return false;
        for (char c : oneOf) {
            if (consume_impl(c, false)) return true;
        }
        return false;
    }

    // ------------------------------------------------

    std::string_view consume_while_impl(std::string_view oneOf, bool ignore = true) {
        if (ignore) do_ignore();
        auto backup = input;
        for (std::size_t _end = 0; !eof(); ++_end) {
            if (!consume_one_of_impl(oneOf, false)) return backup.substr(0, _end);
        }

        return backup;
    }
    
    std::string_view consume_while_not_impl(std::string_view oneOf, bool ignore = true) {
        if (ignore) do_ignore();
        auto backup = input;
        for (std::size_t _end = 0; !eof(); ++_end) {
            if (consume_one_of_impl(oneOf, false)) return backup.substr(0, _end);
        }

        return backup;
    }

    void ignore_impl(std::string_view oneOf) {
        bool consumed = true;
        while (consumed) {
            if (eof()) return;
            consumed = false;
            for (char c : oneOf) {
                if (is_next(c)) {
                    input = input.substr(1);
                    consumed = true;
                    break;
                }
            }
        }
    }

    // ------------------------------------------------

    void parse(std::string_view str) {
        input = str;
        original = str;
        do_parse();
    }

    virtual void do_parse() = 0;

    // ------------------------------------------------
    
    template<class Result = int, class ...Tys>
    struct block {
        Result output{};
        std::string_view ignore;
        std::tuple<Tys...> to_try{};

        block(Tys&&... tys) 
            : to_try(std::forward<Tys>(tys)...)
        {}
        
        block(Result res, std::tuple<Tys...>&& tys) 
            : output(res), to_try(std::move(tys))
        {}

        template<class ResultType>
        block<ResultType&, Tys...> with_result(ResultType& out) {
            return { out, std::move(to_try) };
        }
        
        operator result<std::decay_t<Result>>() {
            result<int> result = 1;
            auto _ = std::get<0>(to_try).state;
            execute(result);
            if (result) return output;
            else {
                _.fail();
                return std::unexpected(result.error());
            }
        }

        template<std::size_t I = 0>
        void execute(auto& result) {
            if constexpr (I == sizeof...(Tys)) {

            } else {
                auto& entry = std::get<I>(to_try);

                if constexpr (std::same_as<decltype(entry.to_try()), void>) {
                    entry.to_try();
                    execute<I + 1>(result);
                } else {
                    if (entry.until_fail) {
                        std::size_t successful_parsed = 0;
                        decltype(entry.to_try()) try_result;
                        while (true) {
                            try_result = entry.to_try();
                            if (try_result) {
                                entry.execute_thens(try_result.value());
                                successful_parsed++;
                            } else {
                                break;
                            }
                        }

                        if (successful_parsed < entry.at_least) {
                            if constexpr (std::same_as<decltype(entry.otherwise), int>) {
                                result = std::unexpected(try_result.error());
                            } else {
                                result = entry.otherwise();
                            }
                            return;
                        } else {
                            execute<I + 1>(result);
                        }
                    } else {
                        auto try_result = entry.to_try();
                        if (try_result) {
                            entry.execute_thens(try_result.value());
                            execute<I + 1>(result);
                        } else {
                            if constexpr (std::same_as<decltype(entry.otherwise), int>) {
                                result = std::unexpected(try_result.error());
                            } else {
                                result = entry.otherwise();
                            }
                            return;
                        }
                    }
                }
            }
        }
    };

    // ------------------------------------------------
    
    template<class Try, class Error = int, class ...And >
    struct try_block {
        Try to_try;
        Error otherwise{};
        std::tuple<And...> thens;
        bool until_fail = false;
        std::size_t at_least = 1;
        undo state;

        void execute_thens(auto& result) {
            [&]<std::size_t ...Is>(std::index_sequence<Is...>) {
                (std::get<Is>(thens)(result), ...);
            }(std::index_sequence_for<And...>{});
        }

        template<class Ty>
        auto operator or(Ty otherwise) const {
            return try_block<Try, Ty, And...>{
                .to_try = std::move(to_try),
                .otherwise = otherwise,
                .thens = std::move(thens),
                .until_fail = until_fail,
                .at_least = at_least,
                .state = state
            };
        }

        template<class Ty>
        auto operator and(Ty then) const {
            return try_block<Try, Error, And..., Ty>{
                .to_try = std::move(to_try),
                .otherwise = std::move(otherwise),
                .thens = std::tuple_cat(std::move(thens), std::forward_as_tuple(then)),
                .until_fail = until_fail,
                .at_least = at_least,
                .state = state 
            };
        }
    };

    // ------------------------------------------------

    auto consume(char v) { return try_block{ .to_try = [&, v]() -> result<char> {
        auto _ = probably_fail();
        if (consume_impl(v)) { _.success(); return result<char>(v); }
        else return die_impl(std::format("Failed to parse character '{}'", v));
    }, .state = probably_success() }; }

    auto consume(std::string_view v) { return try_block{ .to_try = [&, v]() -> result<std::string> {
        auto _ = probably_fail();
        if (consume_impl(v)) { _.success(); return result<std::string>(v); }
        else return die_impl(std::format("Failed to parse string '{}'", v));
    }, .state = probably_success() }; }
    
    auto maybe_consume(char v) { return try_block{ .to_try = [&, v] { consume_impl(v); }, .state = probably_success() }; }
    auto maybe_consume(std::string_view v) { return try_block{ .to_try = [&, v] { consume_impl(v); }, .state = probably_success() }; }
    
    auto consume_while(std::string_view v, std::size_t minLength = 1) { return try_block{ .to_try = [&, v, minLength]() -> result<std::string> {
        auto _ = probably_fail();
        auto consumed = consume_while_impl(v); 
        if (consumed.size() < minLength) {
            if (minLength == 1) {
                return die_impl(std::format("Failed to parse at least 1 character of \"{}\", parsed \"{}\"", v, consumed));
            } else {
                return die_impl(std::format("Failed to parse at least {} characters of \"{}\", parsed \"{}\"", minLength, v, consumed));
            }
        } else {
            _.success(); 
            return result<std::string>(consumed);
        }
    }, .state = probably_success() }; }
    auto consume_while_not(std::string_view v, std::size_t minLength = 1) { return try_block{ .to_try = [&, v, minLength]() -> result<std::string> {
        auto _ = probably_fail();
        auto consumed = consume_while_not_impl(v);
        if (consumed.size() < minLength) {
            if (minLength == 1) {
                return die_impl(std::format("Failed to parse at least 1 character of \"{}\", parsed \"{}\"", v, consumed));
            } else {
                return die_impl(std::format("Failed to parse at least {} characters of \"{}\", parsed \"{}\"", minLength, v, consumed));
            }
        } else {
            _.success();
            return result<std::string>(consumed);
        }
    }, .state = probably_success() }; }
    
    auto maybe_consume_while(std::string_view v) { return try_block{ .to_try = [&, v] { consume_while_impl(v); }, .state = probably_success() }; }
    auto maybe_consume_while_not(std::string_view v) { return try_block{ .to_try = [&, v] { consume_while_not_impl(v); }, .state = probably_success() }; }
    
    auto ignore(std::string_view oneOf) { return try_block{ .to_try = [this, oneOf] { this->ignore_impl(oneOf); }, .state = probably_success() }; }
    auto always_ignore(std::string_view oneOf) { return try_block{ .to_try = [this, oneOf] { this->ignoring = oneOf; }, .state = probably_success() }; }

    // ------------------------------------------------
    
    template<class Ty>
    auto assign(Ty& val) { return [&](auto& result) { val = static_cast<Ty>(result); }; }
    
    template<class Ty>
    auto append(Ty& val) { return [&](auto& result) { val.push_back(result); }; }

    // ------------------------------------------------
    
    auto die(std::string_view message) {
        return [&, message] { return die_impl(message); };
    }

    // ------------------------------------------------
    
    template<class Ty>
    auto many(Ty value) {
        return try_block{ 
            .to_try = [&, value] { return value(); },
            .until_fail = true,
            .at_least = 0,
            .state = probably_success()
        };
    }
    
    template<class Ty>
    auto some(Ty value) {
        return try_block{ 
            .to_try = [&, value] { return value(); },
            .until_fail = true,
            .at_least = 1,
            .state = probably_success()
        };
    }

    // ------------------------------------------------

};

// ------------------------------------------------

struct my_parser : basic_parser {

    // ------------------------------------------------
    
    struct class_type {
        std::string name;

        struct member {
            std::string type;
            std::string name;
        };

        std::vector<member> members;
    };

    // ------------------------------------------------
    
    constexpr static auto valid_identifier = "abcdefghijklmnopqrstuvwxyz";

    // ------------------------------------------------ 
    
    result<class_type::member> parseMember() {
        class_type::member member{};

        return block {
            consume_while(valid_identifier) and assign(member.type) or die("Expected a type-name"),
            consume_while(valid_identifier) and assign(member.name) or die("Expected an identifier"),
            consume(';') or die("Expected ';' after a member"),
        }.with_result(member);
    }

    // ------------------------------------------------ 

    result<class_type> parseObject() {
        class_type result{};

        return block {
            always_ignore(whitespace),
            consume("class"),
            consume_while(valid_identifier) and assign(result.name) or die("Expected a class-name"),
            consume('{') or die("Expected '{' to begin class"),
            many([&] { return parseMember(); }) and append(result.members),
            consume('}') or die("Expected '}' to end class"),
            consume(';') or die("Expected ';' after class"),
        }.with_result(result);
    }

    // ------------------------------------------------

    void do_parse() override {
        if (auto value = parseObject()) {
            std::cout << value.has_value();
        } else {
            std::cout << value.error() << "\n";
        }
    }

    // ------------------------------------------------

};

// ------------------------------------------------

#include <filesystem>
#include <string>
#include <fstream>
#include <sstream>
#include <map>
#include <ranges>

std::map<int, std::string> get_words_from_file(std::filesystem::path message_file) {
    std::map<int, std::string> words{};

    std::ifstream file{ message_file };

    int number;
    std::string word;

    while (file >> number >> word)
        words[number] = word;

    return words;
}

std::string decode(std::filesystem::path message_file) {
    auto words = get_words_from_file(message_file);

    std::stringstream sentence{};
    std::size_t row_length = 1;
    std::size_t index_in_row = 0;

    for (auto& [number, word] : words) {
        if (index_in_row == row_length - 1) {
            sentence << word << ' ';

            row_length++;
            index_in_row = 0;
        } else {
            index_in_row++;
        }
    }

    std::string result = sentence.str();
    return result.substr(0, result.length() - 1);
}



int main() {

    std::cout << decode("C:/Users/Jeroen/Desktop/a.txt");

}