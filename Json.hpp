#pragma once
#include <string_view>
#include <vector>
#include <charconv>
#include <map>
#include <variant>
#include <optional>
#include <string>
#include <array>
#include <stack>

namespace kaixo {
    /**
     * Check if c is one of the characters in cs.
     * @param c character to check
     * @param cs string view of characters to check against
     * @return true if c is in cs
     */
    constexpr bool oneOf(char c, std::string_view cs) { return cs.find(c) != std::string_view::npos; }

    /**
     * Trim the ends of a string view, trims all characters in t
     * @param view view to trimg
     * @param t literal that contains the characters to trim
     * @return trimmed view
     */
    constexpr std::string_view trim(std::string_view view, const char* t = " \t\n\r\f\v") {
        if (auto i = view.find_first_not_of(t); i != std::string_view::npos) view = view.substr(i);
        if (auto i = view.find_last_not_of(t); i != std::string_view::npos) view = view.substr(0, i + 1);
        return view;
    }

    class json {
        struct object_hash : std::hash<std::string_view> { using is_transparent = std::true_type; };
    public:
        enum value_type { Floating, Integral, Unsigned, String, Boolean, Array, Object, Null, None };
        using floating = double;
        using integral = std::int64_t;
        using unsigned_integral = std::uint64_t;
        using string = std::string;
        using boolean = bool;
        using array = std::vector<json>;
        using object = std::map<std::string, json, std::less<void>>;
        using null = std::nullptr_t;
    private:
        using value = std::variant<floating, integral, unsigned_integral, string, boolean, array, object, null>;
        template<class Ty> struct type_alias { using type = Ty; };
        template<> struct type_alias<float> { using type = floating; };
        template<> struct type_alias<double> { using type = floating; };
        template<> struct type_alias<bool> { using type = boolean; };
        template<> struct type_alias<std::string_view> { using type = string; };
        template<std::size_t N> struct type_alias<char[N]> { using type = string; };
        template<std::signed_integral Ty> struct type_alias<Ty> { using type = integral; };
        template<std::unsigned_integral Ty> struct type_alias<Ty> { using type = unsigned_integral; };
        value _value;
    public:
        template<class Ty = null> requires (!std::same_as<std::decay_t<Ty>, json>)
        json(const Ty& ty = {}) : _value(static_cast<typename type_alias<Ty>::type>(ty)) {}
        
        template<class Ty> Ty& as() { return std::get<Ty>(_value); }
        template<class Ty> const Ty& as() const { return std::get<Ty>(_value); }
        auto type() const { return static_cast<value_type>(_value.index()); }
        bool is(value_type t) const { return t == type(); }

        /**
         * Check if object contains key.
         * @param index json key
         * @return true if found, false if not object
         */
        bool contains(std::string_view index, value_type type = None) const {
            if (!is(Object)) return false;
            auto _it = as<object>().find(index);
            if (type == None) return _it != as<object>().end();
            else return _it != as<object>().end() && _it->second.is(type);
        }

        /**
         * Accessor for object. Becomes object if it's null. Throws if not object.
         * @param index name of json key
         * @return reference to json value at that key
         */
        json& operator[](std::string_view index) {
            if (is(Null)) _value = object{};
            else if (!is(Object)) throw std::exception("Not an object.");
            auto _it = as<object>().find(index);
            if (_it == as<object>().end()) return as<object>()[std::string{ index }];
            else return _it->second;
        }

        /**
         * Accessor for array. Becomes array if it's null. Throws if not array.
         * If index > size, it resizes the array.
         * @param index index in array
         * @return reference to json value at that index
         */
        json& operator[](std::size_t index) {
            if (is(Null)) _value = array{};
            else if (!is(Array)) throw std::exception("Not an array.");
            if (as<array>().size() <= index) as<array>().resize(index + 1);
            return as<array>()[index];
        }
        
        /**
         * Accessor for object. Becomes object if it's null. Throws if not object.
         * @param index name of json key
         * @return reference to json value at that key
         */
        const json& operator[](std::string_view index) const {
            if (!is(Object)) throw std::exception("Not an object.");
            auto _it = as<object>().find(index);
            if (_it == as<object>().end()) throw std::exception("Invalid key.");
            else return _it->second;
        }

        /**
         * Accessor for array. Becomes array if it's null. Throws if not array.
         * If index > size, it resizes the array.
         * @param index index in array
         * @return reference to json value at that index
         */
        const json& operator[](std::size_t index) const {
            if (!is(Array)) throw std::exception("Not an array.");
            if (as<array>().size() <= index) throw std::exception("Out of bounds");
            return as<array>()[index];
        }

        /**
         * Emplace value to array, becomes array if null, throws if not array.
         * @param val value to emplace
         * @return reference to emplaced json value
         */
        template<class Ty> json& emplace(const Ty& val) {
            if (is(Null)) _value = array{};
            else if (!is(Array)) throw std::exception("Not an array.");
            return std::get<array>(_value).emplace_back(val);
        }

        /**
         * @return size of either object or array, or 0 of it's neither
         */
        std::size_t size() const {
            return is(Array) ? as<array>().size() : is(Object) ? as<object>().size() : 0ull;
        }

        /**
         * Parse json from a string.
         * @param val json string
         * @return optional, value if correct json
         */
        static std::optional<json> parse(std::string_view val) {
            if ((val = trim(val)).empty()) return {};
            std::optional<json> _result = {};
            if ((_result = parseJsonObject(val)) && trim(val).empty()) return _result;
            if ((_result = parseJsonArray(val)) && trim(val).empty()) return _result;
            return {};
        }

        /**
         * Convert this json to a json string.
         */
        std::string to_string() {
            switch (type()) {
            case Floating: return std::to_string(as<floating>());
            case Integral: return std::to_string(as<integral>());
            case Unsigned: return std::to_string(as<unsigned_integral>());
            case String: return '"' + as<string>() + '"';
            case Boolean: return as<boolean>() ? "true" : "false";
            case Null: return "null";
            case Array: {
                std::string result = "[";
                bool first = true;
                for (auto& val : as<array>()) {
                    if (!first) result += ",";
                    result += val.to_string();
                    first = false;
                }
                return result + "]";
            }
            case Object: {
                std::string result = "{";
                bool first = true;
                for (auto& [key, val] : as<object>()) {
                    if (!first) result += ",";
                    result += '"' + key + '"' + ":" + val.to_string();
                    first = false;
                }
                return result + "}";
            }
            default: return "";
            }
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
            return val.starts_with(word) ? (val = val.substr(word.size()), true) : false;
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
            bool _floating = false, _signed = false;
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
            }
            else if (!_consumeDigits()) return {};

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
                }
                else {                                         // else not escaped
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