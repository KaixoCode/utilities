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

    class basic_json {
        struct object_hash : std::hash<std::string_view> { using is_transparent = std::true_type; };
    public:
        enum Type { Floating, Integral, Unsigned, String, Boolean, Array, Object, Null };
        using floating = double;
        using integral = std::int64_t;
        using unsigned_integral = std::uint64_t;
        using string = std::string;
        using boolean = bool;
        using array = std::vector<basic_json>;
        using object = std::unordered_map<std::string, basic_json, object_hash, std::equal_to<void>>;
        using null = std::nullptr_t;
    private:
        using value = std::variant<floating, integral, unsigned_integral, string, boolean, array, object, null>;
        template<class Ty> struct type_alias { using type = Ty; };
        template<> struct type_alias<double> { using type = floating; };
        template<> struct type_alias<float> { using type = floating; };
        template<> struct type_alias<bool> { using type = boolean; };
        template<> struct type_alias<std::string> { using type = string; };
        template<> struct type_alias<std::string_view> { using type = string; };
        template<std::size_t N> struct type_alias<char[N]> { using type = string; };
        template<std::signed_integral Ty> struct type_alias<Ty> { using type = integral; };
        template<std::unsigned_integral Ty> struct type_alias<Ty> { using type = unsigned_integral; };
        value _value;
    public:
        template<class Ty = null>
        basic_json(const Ty& ty = {}) : _value(static_cast<typename type_alias<Ty>::type>(ty)) {}

        template<class Ty> Ty& as() { return std::get<Ty>(_value); }
        template<class Ty> const Ty& as() const { return std::get<Ty>(_value); }
        Type type() const { return static_cast<Type>(_value.index()); }
        bool is(Type t) const { return t == type(); }

        bool contains(std::string_view index) const {
            if (is(Null) || !is(Object)) return false;
            auto _it = as<object>().find(index);
            return _it != as<object>().end();
        }

        basic_json& operator[](std::string_view index) {
            if (is(Null)) _value = object{};
            else if (!is(Object)) throw std::exception("Not an object.");
            auto _it = as<object>().find(index);
            if (_it == as<object>().end()) return as<object>()[std::string{ index }];
            else return _it->second;
        }

        template<class Ty> basic_json& emplace(const Ty& val) {
            if (is(Null)) _value = array{};
            else if (!is(Array)) throw std::exception("Not an array.");
            return std::get<array>(_value).emplace_back(val);
        }

        std::size_t size() const {
            return is(Array) ? as<array>().size() : is(Object) ? as<object>().size() : 0ull;
        }

        static std::optional<basic_json> parse(std::string_view val) {
            if ((val = trim(val)).empty()) return {};
            std::optional<basic_json> _result = {};
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

        static bool checkFront(std::string_view& val, char c, bool empty = false) {
            if ((val = trim(val)).empty() || !val.starts_with(c)) return false;
            return !(val = trim(val.substr(1))).empty() || empty;
        }

        static bool parseWord(std::string_view& val, std::string_view word) {
            return val.starts_with(word) ? val = val.substr(word.size()), true : false;
        }

        static std::optional<basic_json> parseJsonBool(std::string_view& val) {
            if (parseWord(val, "true")) return true;
            if (parseWord(val, "false")) return false;
            return {};
        }

        static std::optional<basic_json> parseJsonNull(std::string_view& val) {
            if (parseWord(val, "null")) return nullptr;
            return {};
        }

        static std::optional<basic_json> parseJsonNumber(std::string_view& val) {
            auto _json = val;
            auto _size = 0ull;
            bool _floating = false,  _signed = false;
            auto _isDigit = [&] { return oneOf(_json.front(), "0123456789"); };
            auto _consumeOne = [&] { return ++_size, !(_json = _json.substr(1)).empty(); };
            auto _consumeDigits = [&] {
                if (!_isDigit()) return false;
                while (_isDigit()) if (!_consumeOne()) return false;
                return true;
            };
            if (_signed = _json.starts_with('-')) if (!_consumeOne()) return {};
            if (_json.starts_with('0')) {
                if (!_consumeOne()) return {};
                if (_isDigit()) return {};
            } else if (!_consumeDigits()) return {};
            if (_floating = _json.starts_with('.')) {
                if (!_consumeOne()) return {};
                if (!_consumeDigits()) return {};
            }
            if (oneOf(_json.front(), "eE")) {
                if (!_consumeOne()) return {};
                if (oneOf(_json.front(), "-+") && !_consumeOne()) return {};
                if (!_consumeDigits()) return {};
            }
            _json = val.substr(0, _size);
            auto _parse = [&]<class Ty>(Ty val) {
                std::from_chars(_json.data(), _json.data() + _json.size(), val);
                return val;
            };
            val = val.substr(_size);
            return _floating ? _parse(0.0) : _signed ? _parse(0ll) : _parse(0ull);
        }

        static std::optional<basic_json> parseJsonString(std::string_view& val) {
            auto _json = val, _result = _json;
            if (!checkFront(_json, '"')) return {};
            if (checkFront(_json, '"')) return val = _json, "";
            for (auto _offset = 0ull;;) {
                auto _index = _json.find_first_of('"');
                if (_index == std::string_view::npos) return {};
                if (_result[_offset + _index - 1] == '\\') // Escaped "
                    _json = _result.substr(_offset += _index + 1);
                else return val = _result.substr(_offset + _index + 1), 
                    removeDoubleEscapes(_result.substr(0, _offset + _index));
            }
        }

        static std::optional<basic_json> parseJsonArray(std::string_view& val) {
            auto _json = val;
            if (!checkFront(_json, '[')) return {};
            std::optional<basic_json> _result = array{}, _value = {};
            while (_value = parseJsonValue(_json)) {
                _result.value().emplace(_value.value());
                if (!checkFront(_json, ',')) break;
            }
            if (!checkFront(_json, ']', true)) return {};
            return val = _json, _result;
        }

        static std::optional<basic_json> parseJsonObject(std::string_view& val) {
            auto _json = val;
            if (!checkFront(_json, '{')) return {};
            std::optional<basic_json> _result = object{}, _value = {};
            while (_value = parseJsonString(_json)) {
                auto _key = _value.value().as<string>();
                if (!checkFront(_json, ':') || !(_value = parseJsonValue(_json))) return {};
                _result.value()[_key] = _value.value();
                if (!checkFront(_json, ',')) break;
            }
            if (!checkFront(_json, '}', true)) return {};
            return val = _json, _result;
        }

        static std::optional<basic_json> parseJsonValue(std::string_view& val) {
            std::optional<basic_json> _result = {};
            return (_result = parseJsonString(val)) || (_result = parseJsonArray(val))
                || (_result = parseJsonObject(val)) || (_result = parseJsonBool(val))
                || (_result = parseJsonNumber(val)) || (_result = parseJsonNull(val))
                ? _result : std::optional<basic_json>{};
        }
    };

    using json = basic_json;
}