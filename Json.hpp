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
#include <list>

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
        enum value_type { Floating, Integral, Unsigned, String, Boolean, Array, Object, Null };
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
        json(const std::optional<json>& opt) : _value(opt ? (value)opt.value() : null{}) {}

        template<class Ty> Ty& as() { return std::get<Ty>(_value); }
        template<class Ty> const Ty& as() const { return std::get<Ty>(_value); }
        auto type() const { return static_cast<value_type>(_value.index()); }
        bool is(value_type t) const { return t == type(); }
        template<class Ty> Ty cast() const {
            switch (type()) {
                if constexpr (std::convertible_to<floating, Ty>)
            case Floating: return static_cast<Ty>(std::get<floating>(_value));
                if constexpr (std::convertible_to<integral, Ty>)
            case Integral: return static_cast<Ty>(std::get<integral>(_value));
                if constexpr (std::convertible_to<unsigned_integral, Ty>)
            case Unsigned: return static_cast<Ty>(std::get<unsigned_integral>(_value));
                if constexpr (std::convertible_to<string, Ty>)
            case String: return static_cast<Ty>(std::get<string>(_value));
                if constexpr (std::convertible_to<boolean, Ty>)
            case Boolean: return static_cast<Ty>(std::get<boolean>(_value));
                if constexpr (std::convertible_to<array, Ty>)
            case Array: return static_cast<Ty>(std::get<array>(_value));
                if constexpr (std::convertible_to<object, Ty>)
            case Object: return static_cast<Ty>(std::get<object>(_value));
                if constexpr (std::convertible_to<null, Ty>)
            case Null: return static_cast<Ty>(std::get<null>(_value));
            default: return {};
            }
        }

        template<class Ty> operator Ty() const { return cast<Ty>(); }

        /**
         * Check if object contains key.
         * @param index json key
         * @return true if found, false if not object
         */
        bool contains(std::string_view index) const {
            if (!is(Object)) return false;
            auto _it = as<object>().find(index);
            return _it != as<object>().end();
        }

        template<std::size_t N>
        json& operator[](const char(&index)[N]) { return operator[](std::string_view{index}); }

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

    private:
        static std::string removeDoubleEscapes(std::string_view str) {
            std::string _str{ str };
            for (auto _i = _str.begin(); _i != _str.end();)
                if (*_i == '\\') ++(_i = _str.erase(_i)); else ++_i;
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
                if (_result[_offset + _index - 1] == '\\') {     // if we find a '\'
                    std::size_t _check = _offset + _index - 1;   //
                    std::size_t _count = 0;                      //
                    while (_result[_check] == '\\') {            // count how many there are
                        ++_count;                                //
                        if (_check-- == 0) break;                //
                    }                                            // if even amount, the '\'
                    if (_count % 2 == 0) {                       // itself is escaped, so this is the end
                        val = _result.substr(_offset + _index + 1);  //   remove from remainder
                        return removeDoubleEscapes(_result.substr(1, _offset + _index - 1));
                    } else {
                        _offset += _index + 1;                       //   add offset
                        _json = _result.substr(_offset);             //   remove suffix from search
                    }
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

    /**
     * Converts json to C++ code.
     */
    struct json2hpp {
        using enum kaixo::json::value_type;

        constexpr static auto tab = "  ";

        // 2D mapping to find common type given 2 json types
        constexpr static int common[8][8]{
            /*               floating  integral  unsigned  string   boolean  array  object      null */
            /* floating */ { Floating, Floating, Floating,     -1, Floating,    -1,     -1, Floating },
            /* integral */ { Floating, Integral, Integral,     -1, Integral,    -1,     -1, Integral },
            /* unsigned */ { Floating, Integral, Unsigned,     -1, Unsigned,    -1,     -1, Unsigned },
            /*   string */ {       -1,       -1,       -1, String,       -1,    -1,     -1,   String },
            /*  boolean */ { Floating, Integral, Unsigned,     -1,  Boolean,    -1,     -1,  Boolean },
            /*    array */ {       -1,       -1,       -1,     -1,       -1,    -1,     -1,       -1 },
            /*   object */ {       -1,       -1,       -1,     -1,       -1,    -1, Object,   Object },
            /*     null */ { Floating, Integral, Unsigned, String,  Boolean,    -1, Object,     Null }
        }; // Array is always ignored as common type because of size difference ^^

        // Mapping from json type to C++ type name
        constexpr static const char* type_name[8]{
            "double", "std::int64_t", "std::uint64_t", "std::string_view", "bool", "", "", "std::nullptr_t"
        };

        // List of reserved words for C++
        constexpr static const char* reserved_words[]{
            "alignas", "alignof", "and", "and_eq", "asm", "atomic_cancel", "atomic_commit",
            "atomic_noexcept", "auto", "bitand", "bitor", "bool", "break", "case", "catch",
            "char", "char8_t", "char16_t", "char32_t", "class", "compl", "concept", "const",
            "consteval", "constexpr", "constinit", "const_cast", "continue", "co_await",
            "co_return", "co_yield", "decltype", "default", "delete", "do", "double",
            "dynamic_cast", "else", "enum", "explicit", "export", "extern", "false", "float",
            "for", "friend", "goto", "if", "inline", "int", "long", "mutable", "namespace",
            "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq", "private",
            "protected", "public", "reflexpr", "register", "reinterpret_cast", "requires",
            "return", "short", "signed", "sizeof", "static", "static_assert", "static_cast",
            "struct", "switch", "synchronized", "template", "this", "thread_local", "throw",
            "true", "try", "typedef", "typeid", "typename", "union", "unsigned", "using",
            "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq",
        };

        // List of valid identifier characters in C++
        constexpr static auto valid_identifier_characters =
            "_0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

        // List of characters that should be substituted with another character
        constexpr static std::pair<char, char> substitute_character[]{
            { '-', '_' }, { ' ', '_' },
        };

        /**
         * Converts a json string to a valid C++ identifier.
         * @param name json string
         * @return valid C++ identifier
         */
        static std::string filterName(std::string name) {
            if (name.size() == 0) return "_";
            // Make sure string doesn't start with digit
            if (oneOf(name[0], "0123456789")) name = "_" + name;
            // If string is reserved word, add "_" at end
            for (auto& reserved : reserved_words)
                if (name == reserved) return name + "_";
            // Substitute any characters, and remove invalid ones
            for (auto i = name.begin(); i != name.end();) {
                for (auto& s : substitute_character)
                    if (*i == s.first) *i = s.second;
                if (!oneOf(*i, valid_identifier_characters))
                    name.erase(i);
                else ++i;
            }
            return name;
        }

        struct object {
            struct member {
                std::string type; // Type of member
                std::string name; // Name of member
            };

            std::string type;             // Type of object
            std::string name;             // Name of object
            bool needs_generating = true; // When false, it uses a standard container
            std::list<member> members;    // List of members in this object

            /**
             * Convert this object to C++ code.
             * @return C++ code
             */
            std::string to_string() {
                if (!needs_generating) return "";
                std::string str = "struct " + filterName(type) + " {\n";
                for (auto& member : members)
                    str += tab + member.type + " " + filterName(member.name) + ";\n";
                return str + "};\n\n";
            }
        };

        std::list<object> objects;
        std::size_t counter = 0;

        /**
         * Generates a unique id.
         * @return unique id as string
         */
        std::string id() { return std::to_string(counter++); }

        /**
         * Converts a name to a unique type name.
         * @param name name
         * @return unique type name
         */
        std::string name2type(const std::string& name) {
            return filterName(name) + "_" + id() + "_t";
        }

        /**
         * Generate C++ object from json.
         * @param name name to use for the outer most object
         * @param data json to convert to a C++ object
         * @return C++ code
         */
        static std::string generate(const std::string& name, json data) {
            json2hpp converter;
            if (data.is(Object)) {
                auto& obj = converter.generate_object(name, data.as<json::object>());
                std::string result = "";
                for (auto& elem : converter.objects) result = elem.to_string() + result;
                return result + "constexpr " + obj.type + " " + filterName(name) 
                    + " = " + generate_constructor(data) + ";\n";
            } else if (data.is(Array)) {
                auto& obj = converter.generate_array(name, data.as<json::array>());
                std::string result = "";
                for (auto& elem : converter.objects) result = elem.to_string() + result;
                return result + "constexpr " + obj.type + " " + filterName(name) 
                    + " = " + generate_constructor(data) + ";\n";
            }
            return "";
        }

        /**
         * Add indent to a string.
         * @param str string
         * @param indent amount of tabs (4 spaces)
         * @param before when true, adds before string, otherwise after
         * @return string with the added indenting
         */
        static std::string add_indent(const std::string& str, int indent, bool before = true) {
            std::string res = str;
            if (before) for (int i = 0; i < indent; ++i) res = tab + res;
            else for (int i = 0; i < indent; ++i) res += tab;
            return res;
        }

        /**
         * Recursively generates the constructor for the generated C++ object.
         * @param data json
         * @param indent indent, used for formatting
         * @return string containing C++ code that constructs the generated C++ object
         */
        static std::string generate_constructor(json& data, int indent = 0) {
            switch (data.type()) {
            case Object: {
                if (data.size() == 0) return "{}";
                std::string construct = "{\n";
                for (auto& [key, value] : data.as<json::object>())
                    construct += add_indent("." + filterName(key) + " = " + generate_constructor(value, indent + 1) + ", \n", indent + 1);
                return construct + add_indent("}", indent);
            }
            case Array: {
                if (data.size() == 0) return "{}";
                json all = json::object{};
                bool extra = combine_json_objects(all, data.as<json::array>());
                std::string construct = extra ? "{{\n" : "{\n";
                for (auto& value : data.as<json::array>())
                    construct += add_indent(generate_constructor(value, indent + 1) + ", \n", indent + 1);
                return construct + add_indent(extra ? "}}" : "}", indent);
            }
            case Boolean: return data.as<json::boolean>() ? "true" : "false";
            case Floating: return std::to_string(data.as<json::floating>());
            case Unsigned: return std::to_string(data.as<json::unsigned_integral>());
            case Integral: return std::to_string(data.as<json::integral>());
            case String: return "R\"##(" + data.as<json::string>() + ")##\"";
            case Null: return "{}";
            }
        }

        /**
         * Add a json value to an object as a member with a given name.
         * @param object object to add to
         * @param name name of the member in the object
         * @param value json value to add as member
         */
        void add_to_object(object& object, const std::string& name, json& value) {
            switch (value.type()) {
            case Object: {
                auto& obj = generate_object(name, value.as<json::object>());
                object.members.emplace_back(obj.type, obj.name);
                break;
            }
            case Array: {
                auto& obj = generate_array(name, value.as<json::array>());
                object.members.emplace_back(obj.type, obj.name);
                break;
            }
            default: object.members.emplace_back(type_name[value.type()], name);
            }
        }

        /**
         * Recursively generates objects for a json object
         * @param name name of the object
         * @param obj json object
         * @return reference to the generated object
         */
        object& generate_object(const std::string& name, json::object& obj) {
            if (obj.size() == 0) return objects.emplace_back("std::tuple<>", name, false);
            auto& object = objects.emplace_back(name2type(name), name);
            for (auto& [elem_name, elem] : obj)
                add_to_object(object, elem_name, elem);
            return object;
        }

        /**
         * Find the common type among all the element in a json array
         * @param arr json array
         * @return -3 if empty, -1 if no common type, type index if common type
         */
        static int find_common_type(json::array& arr) {
            int common_type = -3;
            for (auto& elem : arr)
                if (common_type == -3) common_type = elem.type();
                else if ((common_type = common[common_type][elem.type()]) == -1) return -1;
            return common_type;
        }

        static bool combine_to_common_type(json& a, const json& b) {
            int common_type = common[b.type()][a.type()];
            switch (common_type) {
            case Floating: a = json::floating{}; return true;
            case Integral: a = json::integral{}; return true;
            case Unsigned: a = json::unsigned_integral{}; return true;
            case String: a = json::string{}; return true;
            case Boolean: a = json::boolean{}; return true;
            default: return false;
            }
        }

        /**
         * Combine the json fields of both json objects.
         * @param a first json object
         * @param b second json object
         * @return false if failed to combine (type mismatch)
         */
        static bool combine_json_objects(json& a, const json& b) {
            if (b.type() == Null) return true;
            if (b.type() != Object) return combine_to_common_type(a, b);
            auto& _obj = b.as<json::object>();
            for (auto& [key, value] : _obj) {
                // No matching type, means not able to combine
                if (a[key].type() != Null && a[key].type() != value.type())
                    return combine_to_common_type(a[key], value);
                // Both an object, simply combine objects
                if (a[key].type() == Object) {
                    if (!combine_json_objects(a[key], value)) return false;
                }
                // Both an array, combine into first index, and fill up to size
                else if ((a[key].type() == Array
                    || a[key].type() == Null)
                    && value.type() == Array) {
                    for (auto& m : value.as<json::array>())
                        if (!combine_json_objects(a[key][0], m)) return false;
                    for (std::size_t i = a[key].size(); i < value.size(); ++i)
                        a[key].emplace(a[key][0]); // Fill up to size with copies
                }
                else a[key] = value;
            }
            return true;
        }

        /**
         * Combine all the json fields in arr into all
         * @param all combine into
         * @param arr fields to combine into all
         * @return true if possible
         */
        static bool combine_json_objects(json& all, json::array& arr) {
            for (auto& elem : arr) if (!combine_json_objects(all, elem)) return false;
            return true;
        }

        /**
         * Recursively generates objects for a json array
         * @param name name of the array
         * @param arr json array
         * @return reference to generated object
         */
        object& generate_array(const std::string& name, json::array& arr) {
            using namespace std::string_literals;
            int common_type = find_common_type(arr);
            // If common type is object, we try to combine all objects
            if (common_type == Object) {
                if (json all = json::object{}; combine_json_objects(all, arr)) {
                    auto& obj = generate_object(name, all.as<kaixo::json::object>());
                    return objects.emplace_back("std::array<"s + obj.type
                        + ", " + std::to_string(arr.size()) + ">", name, false);
                }
            }

            // Empty array
            if (common_type == -3)
                return objects.emplace_back("std::array<int, 0>", name, false);
            // No common type in array, or the Object one didn't work
            else if (common_type == Object || common_type == -1) {
                auto& object = objects.emplace_back(name2type(name), name);
                for (std::size_t index = 0; auto & elem : arr)
                    add_to_object(object, "_" + std::to_string(index++), elem);
                return object;
            }
            // Common type, so just use std::array
            else return objects.emplace_back("std::array<"s + type_name[common_type]
                + ", " + std::to_string(arr.size()) + ">", name, false);
        }
    };

    template<class Ty> constexpr std::size_t json_size = 0;
    template<class Ty, std::size_t I> constexpr auto json_member{};

    struct json2cpp {
        template<class C, class T> static T _memptr_t_impl(T C::*);
        template<class C, class T, class Arg> static Arg _memptr_t_impl(T(C::*)(Arg));
        template<auto M> using _memptr_t = decltype(_memptr_t_impl(M));

        template<class Ty>
        static Ty&& array_from_json(json& val, Ty&& result = {}) {
            using decayed = std::decay_t<Ty>;
            using type = decayed::value_type;
            using type_decayed = std::decay_t<type>;

            auto with = [&]<class T>(T & e) {
                if constexpr (!std::convertible_to<T, type>) throw std::bad_cast{};
                else result.insert(result.end(), static_cast<type>(e));
            };

            for (auto& e : val.as<json::array>()) {
                switch (e.type()) {
                case json::Unsigned: with(e.as<json::unsigned_integral>()); break;
                case json::Integral: with(e.as<json::integral>()); break;
                case json::Floating: with(e.as<json::floating>()); break;
                case json::Boolean:  with(e.as<json::boolean>()); break;
                case json::Null:     with(e.as<json::null>()); break;
                case json::String:   with(e.as<json::string>()); break;
                case json::Object: result.insert(result.end(), object_from_json<type>(e)); break;
                case json::Array: {
                    if constexpr (requires (type_decayed container) {
                        typename type_decayed::value_type;
                        { container.insert(container.end(), {}) };
                    }) {
                        result.insert(result.end(), array_from_json<type_decayed>(e));
                    }
                    break;
                }
                }
            }
            return std::forward<Ty>(result);
        }

        template<class Ty>
        static Ty&& object_from_json(json& val, Ty&& result = {}) {
            using decayed = std::decay_t<Ty>;
            constexpr std::size_t size = json_size<decayed>;
            auto set = [&]<std::size_t I>() {
                constexpr std::string_view name = json_member<decayed, I>.second;
                auto access = json_member<decayed, I>.first;
                using type = _memptr_t<json_member<decayed, I>.first>;
                using type_decayed = std::decay_t<type>;

                if (!val.contains(name)) return;

                auto& v = val[name];
                auto with = [&]<class T>(T & v) {
                    if constexpr (!std::convertible_to<T, type>) throw std::bad_cast{};
                    else if constexpr (std::is_member_object_pointer_v<decltype(access)>)
                        result.*access = static_cast<type>(v);
                    else if constexpr (std::is_member_function_pointer_v<decltype(access)>)
                        (result.*access)(static_cast<type>(v));
                };
                switch (v.type()) {
                case json::Unsigned: with(v.as<json::unsigned_integral>()); break;
                case json::Integral: with(v.as<json::integral>()); break;
                case json::Floating: with(v.as<json::floating>()); break;
                case json::Boolean:  with(v.as<json::boolean>()); break;
                case json::Null:     with(v.as<json::null>()); break;
                case json::String:   with(v.as<json::string>()); break;
                case json::Object:
                    if constexpr (std::is_member_object_pointer_v<decltype(access)>)
                        object_from_json(v, result.*access);
                    else if constexpr (std::is_member_function_pointer_v<decltype(access)>)
                        (result.*access)(object_from_json<type_decayed>(v));
                    break;
                case json::Array: {
                    if constexpr (requires (type container) {
                        typename type::value_type;
                        { container.insert(container.end(), {}) };
                    }) {
                        if constexpr (std::is_member_object_pointer_v<decltype(access)>)
                            array_from_json(v, result.*access);
                        else if constexpr (std::is_member_function_pointer_v<decltype(access)>)
                            (result.*access)(array_from_json<type_decayed>(v));
                    }
                    else if constexpr (std::is_member_function_pointer_v<decltype(access)>) {
                        for (auto& e : v.as<json::array>()) {
                            switch (e.type()) {
                            case json::Unsigned: with(e.as<json::unsigned_integral>()); break;
                            case json::Integral: with(e.as<json::integral>()); break;
                            case json::Floating: with(e.as<json::floating>()); break;
                            case json::Boolean:  with(e.as<json::boolean>()); break;
                            case json::Null:     with(e.as<json::null>()); break;
                            case json::String:   with(e.as<json::string>()); break;
                            case json::Object: (result.*access)(object_from_json<type_decayed>(e)); break;
                            case json::Array: throw std::bad_cast{};
                            }
                        }
                    }
                    break;
                }
                }
            };
            [&] <std::size_t ...Is>(std::index_sequence<Is...>) {
                (set.operator() < Is > (), ...);
            }(std::make_index_sequence<size>{});
            return std::forward<Ty>(result);
        }
    };
}