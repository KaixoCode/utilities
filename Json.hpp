#pragma once
#include <string_view>
#include <vector>
#include <charconv>
#include <unordered_map>
#include <variant>
#include <optional>
#include <string>
#include <array>

namespace kaixo {
    constexpr bool oneOf(char c, std::string_view cs) { return cs.find(c) != std::string_view::npos; }
    constexpr std::string_view trim(std::string_view view, const char* t = " \t\n\r\f\v") {
        if (auto i = view.find_first_not_of(t); i != std::string_view::npos) view = view.substr(i);
        if (auto i = view.find_last_not_of(t); i != std::string_view::npos) view = view.substr(0, i + 1);
        return view;
    }

    class json {
        struct object_hash : std::hash<std::string_view> { using is_transparent = std::true_type; };
    public:
        enum { Floating, Integral, Unsigned, String, Boolean, Array, Object, Null };
        using floating = double;
        using integral = std::int64_t;
        using unsigned_integral = std::uint64_t;
        using string = std::string;
        using boolean = bool;
        using array = std::vector<json>;
        using object = std::unordered_map<std::string, json, object_hash, std::equal_to<void>>;
        using null = std::nullptr_t;
    private:
        using value = std::variant<floating, integral, unsigned_integral, string, boolean, array, object, null>;
        template<class Ty> struct type_alias { using type = Ty; };
        template<> struct type_alias<float> { using type = floating; };
        template<> struct type_alias<bool> { using type = boolean; };
        template<> struct type_alias<std::string_view> { using type = string; };
        template<std::size_t N> struct type_alias<char[N]> { using type = string; };
        template<std::signed_integral Ty> struct type_alias<Ty> { using type = integral; };
        template<std::unsigned_integral Ty> struct type_alias<Ty> { using type = unsigned_integral; };
        value _value;
    public:
        template<class Ty = null>
        json(const Ty& ty = {}) : _value(static_cast<typename type_alias<Ty>::type>(ty)) {}

        template<class Ty> Ty& as() { return std::get<Ty>(_value); }
        template<class Ty> const Ty& as() const { return std::get<Ty>(_value); }
        auto type() const { return static_cast<decltype(Floating)>(_value.index()); }
        bool is(decltype(Floating) t) const { return t == type(); }

        bool contains(std::string_view index) const {
            if (is(Null) || !is(Object)) return false;
            auto _it = as<object>().find(index);
            return _it != as<object>().end();
        }

        json& operator[](std::string_view index) {
            if (is(Null)) _value = object{};
            else if (!is(Object)) throw std::exception("Not an object.");
            auto _it = as<object>().find(index);
            if (_it == as<object>().end()) return as<object>()[std::string{ index }];
            else return _it->second;
        }

        template<class Ty> json& emplace(const Ty& val) {
            if (is(Null)) _value = array{};
            else if (!is(Array)) throw std::exception("Not an array.");
            return std::get<array>(_value).emplace_back(val);
        }

        std::size_t size() const {
            return is(Array) ? as<array>().size() : is(Object) ? as<object>().size() : 0ull;
        }

        static std::optional<json> parse(std::string_view val) {
            if ((val = trim(val)).empty()) return {};
            std::optional<json> _result = {};
            if ((_result = parseJsonObject(val)) && trim(val).empty()) return _result;
            if ((_result = parseJsonArray(val)) && trim(val).empty()) return _result;
            return {};
        }

    private:
        static std::string removeDoubleEscapes(std::string_view str) {
            std::string _str{ str };
            for (auto _i = _str.begin(); _i != _str.end();)
                if (*_i == '\\') _i = _str.erase(_i); else ++_i;
            return _str;
        }

        static bool consume(std::string_view& val, char c, bool empty = false) {
            if ((val = trim(val)).empty() || !val.starts_with(c)) return false;
            return !(val = trim(val.substr(1))).empty() || empty;
        }

        static bool consume(std::string_view& val, std::string_view word) {
            return val.starts_with(word) ? val = val.substr(word.size()), true : false;
        }

        static std::optional<json> parseJsonBool(std::string_view& val) {
            return consume(val, "true") ? true 
                : consume(val, "false") ? false : std::optional<json>{};
        }

        static std::optional<json> parseJsonNull(std::string_view& val) {
            return consume(val, "null") ? nullptr : std::optional<json>{};
        }

        static std::optional<json> parseJsonNumber(std::string_view& val) {
            std::string_view _json = val;
            std::size_t _size = 0ull;
            bool _floating = false,  _signed = false;
            auto _isDigit = [&] { return oneOf(_json.front(), "0123456789"); };
            auto _consume = [&] { return ++_size, !(_json = _json.substr(1)).empty(); };
            auto _consumeDigits = [&] {
                if (!_isDigit()) return false;
                while (_isDigit()) if (!_consume()) return false;
                return true;
            };

            if (_signed = _json.starts_with('-'))
                if (!_consume()) return {};
 
            if (_json.starts_with('0')) {      // when leading 0
                if (!_consume()) return {}; // 
                if (_isDigit()) return {};     // can't be followed by digit
            } else if (!_consumeDigits()) return {};
 
            if (_floating = _json.starts_with('.')) {
                if (!_consume()) return {};
                if (!_consumeDigits()) return {};
            }
 
            if (oneOf(_json.front(), "eE")) {
                if (!_consume()) return {};
                if (oneOf(_json.front(), "-+") && !_consume()) return {};
                if (!_consumeDigits()) return {};
            }

            _json = val.substr(0, _size);
            auto _parse = [&]<class Ty>(Ty val) {
                std::from_chars(_json.data(), _json.data() + _json.size(), val);
                return json{ val };
            };
            val = val.substr(_size);
            return _floating ? _parse(0.0) : _signed ? _parse(0ll) : _parse(0ull);
        }

        static std::optional<json> parseJsonString(std::string_view& val) {
            std::string_view _json = val, _result = _json;
            if (!consume(_json, '"')) return {};                 // parse '"'
            if (consume(_json, '"')) return val = _json, "";     // empty string if parse '"'
            for (std::size_t _offset = 1ull;;) {                 //
                std::size_t _index = _json.find_first_of('"');   // find next '"'
                if (_index == std::string_view::npos) return {}; // if not exist, invalid string
                if (_result[_offset + _index - 1] == '\\') {     // if escaped
                    _offset += _index + 1;                       //   add offset
                    _json = _result.substr(_offset);             //   remove suffix from search
                } else {                                         // else not escaped
                    val = _result.substr(_offset + _index + 1);  //   remove from remainder
                    return removeDoubleEscapes(_result.substr(1, _offset + _index - 1));
                }
            }
        }

        static std::optional<json> parseJsonArray(std::string_view& val) {
            std::string_view _json = val;
            if (!consume(_json, '[')) return {};                      // parse '['
            std::optional<json> _result = array{}, _value = {};       // 
            while (_value = parseJsonValue(_json)) {                  // try parse value
                _result.value().emplace(_value.value());              // add value to result
                if (!consume(_json, ',')) break;                      // if no comma, break
            }                                                         // 
            if (!consume(_json, ']', true)) return {};                // parse ']'
            return val = _json, _result; // on success, save to val, return result
        }

        static std::optional<json> parseJsonObject(std::string_view& val) {
            std::string_view _json = val;
            if (!consume(_json, '{')) return {};                       // parse '{'
            std::optional<json> _result = object{}, _value = {};       //
            while (_value = parseJsonString(_json)) {                  // parse key
                std::string _key = _value.value().as<string>();        // 
                if (!consume(_json, ':')) return {};                   // parse ':'
                if (!(_value = parseJsonValue(_json))) return {};      // parse value
                _result.value()[_key] = _value.value();                // add to object
                if (!consume(_json, ',')) break;                       // if no comma, break
            }                                                          //
            if (!consume(_json, '}', true)) return {};                 // parse '}'
            return val = _json, _result; // on success, save to val, return result
        }

        static std::optional<json> parseJsonValue(std::string_view& val) {
            std::optional<json> _result = {};
            return (_result = parseJsonString(val)) || (_result = parseJsonArray(val))
                || (_result = parseJsonObject(val)) || (_result = parseJsonBool(val))
                || (_result = parseJsonNumber(val)) || (_result = parseJsonNull(val))
                ? _result : std::optional<json>{};
        }
    };
}