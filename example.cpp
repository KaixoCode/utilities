
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
    
    error die(std::string_view message) const {
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

    bool consume(char c) {
        if (eof() || !is_next(c)) return false;
        input = input.substr(1);
        return true;
    }

    bool consume(std::string_view word) {
        if (eof() || !is_next(word)) return false;
        input = input.substr(word.size());
        return true;
    }

    // ------------------------------------------------

    bool consume_one_of(std::string_view oneOf) {
        if (eof()) return false;
        for (char c : oneOf) {
            if (consume(c)) return true;
        }
        return false;
    }

    // ------------------------------------------------

    std::string_view consume_while(std::string_view oneOf) {
        auto backup = input;
        for (std::size_t _end = 0; !eof(); ++_end) {
            if (!consume_one_of(oneOf)) return backup.substr(0, _end);
        }

        return backup;
    }
    
    std::string_view consume_while_not(std::string_view oneOf) {
        auto backup = input;
        for (std::size_t _end = 0; !eof(); ++_end) {
            if (consume_one_of(oneOf)) return backup.substr(0, _end);
        }

        return backup;
    }

    void ignore(std::string_view oneOf) {
        consume_while(oneOf);
    }

    // ------------------------------------------------

    void parse(std::string_view str) {
        input = str;
        original = str;
        do_parse();
    }

    virtual void do_parse() = 0;

    // ------------------------------------------------

};

// ------------------------------------------------

struct json_parser : basic_parser {

    // ------------------------------------------------

    result<int> parseObject() {
        auto _ = probably_fail();

        ignore(whitespace);
        if (!consume('{')) return die("Expected '{' to begin Object");

        ignore(whitespace);
        if (!consume('}')) return die("Expected '}' to close Object");

        _.success();
        return 1;
    }

    // ------------------------------------------------

    void do_parse() override {
        if (auto value = parseObject()) {
        } else {
            std::cout << value.error() << "\n";
        }
    }

    // ------------------------------------------------

};

// ------------------------------------------------

int main() {

    std::string content = R"~~({
  a}

)~~";

    json_parser parser;
    parser.parse(content);


}