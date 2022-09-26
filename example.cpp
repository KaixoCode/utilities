#include <iostream>
#include <string_view>
#include <vector>
#include <charconv>
#include <map>
#include <variant>
#include <optional>
#include <string>
#include <array>

constexpr std::vector<std::string_view> split(const std::string_view str, const char delim = ',') {
    std::vector<std::string_view> result;
    int indexCommaToLeftOfColumn = 0;
    int indexCommaToRightOfColumn = -1;
    for (int i = 0; i < static_cast<int>(str.size()); i++) {
        if (str[i] == delim) {
            indexCommaToLeftOfColumn = indexCommaToRightOfColumn;
            indexCommaToRightOfColumn = i;
            int index = indexCommaToLeftOfColumn + 1;
            int length = indexCommaToRightOfColumn - index;
            std::string_view column(str.data() + index, length);
            result.push_back(column);
        }
    }
    const std::string_view finalColumn(str.data() + indexCommaToRightOfColumn + 1, str.size() - indexCommaToRightOfColumn - 1);
    result.push_back(finalColumn);
    return result;
}

constexpr std::string_view trim(std::string_view view, const char* t = " \t\n\r\f\v") {
    auto _first = view.find_first_not_of(t);
    if (_first != std::string_view::npos) view = view.substr(_first);
    auto _last = view.find_last_not_of(t);
    if (_last != std::string_view::npos) view = view.substr(0, _last + 1);
    return view;
}

constexpr bool oneOf(char str, std::string_view cs) {
    for (auto& c : cs)
        if (c == str) return true;
    return false;
}

template<class Ty, class ...Tys>
concept OneOf = (std::same_as<Ty, Tys> || ...);

class Json {
public:
    using Floating = double;
    using Integral = int64_t;
    using Unsigned = uint64_t;
    using String = std::string;
    using Boolean = bool;
    using Array = std::vector<Json>;
    using Object = std::map<String, Json, std::less<void>>;
    using Null = std::nullptr_t;

    enum class Type { Floating, Integral, Unsigned, String, Boolean, Array, Object, Null };
    friend constexpr bool operator==(Type t, auto b) { return t == static_cast<Type>(b); }
private:
    using JsonValue = std::variant<Floating, Integral, Unsigned, String, Boolean, Array, Object, Null>;
    JsonValue m_Value;

    template<class Ty> struct Alias { using Type = Ty; };
    template<> struct Alias<std::string_view> { using Type = String; };
    template<> struct Alias<bool> { using Type = Boolean; };
    template<std::size_t N> struct Alias<char[N]> { using Type = String; };
    template<std::signed_integral Ty> struct Alias<Ty> { using Type = Integral; };
    template<std::unsigned_integral Ty> struct Alias<Ty> { using Type = Unsigned; };

public:
    template<class Ty = Null>
    Json(const Ty& ty = {}) : m_Value(static_cast<Alias<Ty>::Type>(ty)) {}

    template<class Ty> Ty get() const { return static_cast<Ty>(std::get<Alias<Ty>::Type>(m_Value)); }
    template<class Ty> Ty& ref() { return std::get<Ty>(m_Value); }
    template<class Ty> const Ty& ref() const { return std::get<Ty>(m_Value); }
    Type type() const { return static_cast<Type>(m_Value.index()); }

    Json& operator[](std::string_view index) {
        if (m_Value.index() == Type::Null) m_Value = Object{};
        else if (m_Value.index() != Type::Object) throw std::exception("Not an object.");
        auto _it = ref<Object>().find(index);
        if (_it == ref<Object>().end()) return ref<Object>()[std::string{ index }];
        else return _it->second;
    }

    void push_back(Json json) {
        if (m_Value.index() == Type::Null) m_Value = Array{};
        else if (m_Value.index() != Type::Array) throw std::exception("Not an array.");
        std::get<Array>(m_Value).push_back(json);
    }
};

std::optional<Json> parseJsonObject(std::string_view& json);
std::optional<Json> parseJsonValue(std::string_view& json);
std::optional<Json> parseJsonArray(std::string_view& json);
std::optional<Json> parseJsonString(std::string_view& json);
std::optional<Json> parseJsonBool(std::string_view& json);
std::optional<Json> parseJsonNull(std::string_view& json);
std::optional<Json> parseJsonNumber(std::string_view& json);

std::optional<Json> parseJsonString(std::string_view& json) {
    auto _json = trim(json);

    // Find first "
    if (_json.size() == 0) return {};
    if (_json[0] != '"') return {};
    _json = _json.substr(1); // remove "

    // Find closing "
    auto _offset = 0ull;
    auto _result = _json;
    while (true) {
        auto _index = _json.find_first_of('"');
        // If no more " in string, can't parse pair
        if (_index == std::string_view::npos) return {};
        // If at index 0, means we have empty string
        if (_index == 0) {
            _result = "";
            _json = _json.substr(1);
            break;
        }
        // If \ appears before it, it's not valid end
        if (_json[_index - 1] == '\\') {
            _json = _json.substr(_index + 1); // Remove from view
            _offset += _index + 1; // And add offset
        } else {
            // Otherwise it's the end of the key
            _result = _result.substr(0, _offset + _index);
            _json = _json.substr(_index + 1);
            break;
        }
    }

    json = _json;
    std::string _s{ _result };
    for (auto _i = _s.begin(); _i != _s.end();) {
        if (*_i == '\\') _s.erase(_i);
        else ++_i;
    }

    return _s;
}

std::optional<Json> parseJsonArray(std::string_view& json) {
    std::optional<Json> _result = Json::Array{};
    auto _json = trim(json);
    // Start object with {
    if (_json.size() == 0) return {};
    if (_json[0] != '[') return {};
    _json = trim(_json.substr(1));

    // Parse key value pairs
    std::optional<Json> _value = {};
    while (_value = parseJsonValue(_json)) {
        _result.value().push_back(_value.value());
        // If next character is comma, end of pairs
        _json = trim(_json);
        if (_json.size() == 0) return {};
        if (_json[0] != ',') break;
        _json = _json.substr(1);
    }

    // End object with "}"
    _json = trim(_json);
    if (_json[0] != ']') return {};
    _json = _json.substr(1);

    json = _json;
    return _result;
}

std::optional<Json> parseJsonBool(std::string_view& json) {
    auto _json = trim(json);
    if (_json.starts_with("true")) {
        json = _json.substr(4);
        return true;
    }
    if (_json.starts_with("false")) {
        json = _json.substr(5);
        return false;
    }
    return {};
}

std::optional<Json> parseJsonNull(std::string_view& json) {
    auto _json = trim(json);
    if (_json.starts_with("null")) {
        json = _json.substr(4);
        return nullptr;
    }
    return {};
}

std::optional<Json> parseJsonNumber(std::string_view& json) {
    auto _json = trim(json);
    auto _number = _json;
    auto _size = 0ull;
    bool _floating = false;
    bool _signed = false;
    if (_json.size() == 0) return {};
    if (_json[0] == '-') _json = _json.substr(1), ++_size, _signed = true;
    if (_json.size() == 0) return {};

    // If number has leading 0
    if (_json[0] == '0') {
        _json = _json.substr(1), ++_size;
        if (_json.size() == 0) return {};
        // It can't have another digit following
        if (oneOf(_json[0], "0123456789")) return {};
    }
    // Otherwise keep parsing digits
    else if (oneOf(_json[0], "0123456789")) {
        while (oneOf(_json[0], "0123456789")) {
            _json = _json.substr(1), ++_size;
            if (_json.size() == 0) return {};
        }
    }
    // Otherwise not valid digit
    else return {};

    // Floating point number
    if (_json[0] == '.') {
        _floating = true;
        _json = _json.substr(1), ++_size;
        if (_json.size() == 0) return {};

        // Parse digits after the dot
        if (oneOf(_json[0], "0123456789")) {
            while (oneOf(_json[0], "0123456789")) {
                _json = _json.substr(1), ++_size;
                if (_json.size() == 0) return {};
            }
        }
        // must have at least 1 digit after dot
        else return {};
    }

    // Exponent
    if (_json[0] == 'e' || _json[0] == 'E') {
        _json = _json.substr(1), ++_size;
        if (_json.size() == 0) return {};

        // Sign of exponent
        if (_json[0] == '-' || _json[0] == '+') _json = _json.substr(1), ++_size;
        if (_json.size() == 0) return {};

        // Parse digits in exponent
        if (oneOf(_json[0], "0123456789")) {
            while (oneOf(_json[0], "0123456789")) {
                _json = _json.substr(1), ++_size;
                if (_json.size() == 0) return {};
            }
        }
        // Must have at least 1 digit after exponent
        else return {};
    }

    _number = _number.substr(0, _size);
    json = _json;

    if (_floating) {
        double _asNum;
        std::from_chars(_number.data(), _number.data() + _number.size(), _asNum);
        return _asNum;
    } else if (_signed) {
        std::int64_t _asNum;
        std::from_chars(_number.data(), _number.data() + _number.size(), _asNum);
        return _asNum;
    } else {
        std::uint64_t _asNum;
        std::from_chars(_number.data(), _number.data() + _number.size(), _asNum);
        return _asNum;
    }
}

std::optional<Json> parseJsonValue(std::string_view& json) {
    std::optional<Json> _result = {};
    auto _json = trim(json);

    if ((_result = parseJsonString(_json))
        || (_result = parseJsonObject(_json))
        || (_result = parseJsonArray(_json))
        || (_result = parseJsonBool(_json))
        || (_result = parseJsonNull(_json))
        || (_result = parseJsonNumber(_json))) {
        json = _json;
        return _result;
    }
    return {};
}

std::optional<Json> parseJsonObject(std::string_view& json) {
    std::optional<Json> _result = Json::Object{};
    auto _json = trim(json);
    // Start object with {
    if (_json.size() == 0) return {};
    if (_json[0] != '{') return {};
    _json = trim(_json.substr(1));

    // Parse key value pairs
    while (true) {
        std::optional<Json> _this = {};
        auto _backup = trim(_json);

        // Start with key of the pair
        if (!(_this = parseJsonString(_backup))) break;
        auto _key = _this.value().get<Json::String>();

        // Make sure there's a :
        _backup = trim(_backup);
        if (_backup.size() == 0) break;
        if (_backup[0] != ':') break;
        _backup = _backup.substr(1);

        // Then parse value
        if (!(_this = parseJsonValue(_backup))) break;

        _result.value()[_key] = _this.value();

        // If next character is comma, end of pairs
        _json = trim(_backup);
        if (_json.size() == 0) return {};
        if (_json[0] != ',') break;
        _json = _json.substr(1);
    }

    // End object with "}"
    _json = trim(_json);
    if (_json[0] != '}') return {};
    _json = _json.substr(1);

    json = _json;
    return _result;
}

std::optional<Json> parseJson(std::string_view json) {
    return parseJsonObject(json);
}

std::string escapeString(std::string str) {
    for (auto _i = str.begin(); _i != str.end(); ++_i)
        if (oneOf(*_i, "\"\\\/\b\f\n\r\t")) str.insert(_i, '\\'), ++_i;
    return str;
}

std::string jsonToCode(const std::string& name, const Json& json, int indent = 0, bool nested = false, bool ce = true) {
    std::string result = "";
    if (ce) result += "constexpr ";
    result += "struct " + name + "_t {\n";

    for (auto& [key, val] : json.ref<Json::Object>()) {
        using enum Json::Type;
        for (int i = 0; i < indent + 1; ++i) result += "    ";
        switch (val.type()) {
        case Floating: result += "double " + std::string{ key } + " = " + std::to_string(val.ref<Json::Floating>()) + ";\n"; break;
        case Integral: result += "std::int64_t " + std::string{ key } + " = " + std::to_string(val.ref<Json::Integral>()) + ";\n"; break;
        case Unsigned: result += "std::uint64_t " + std::string{ key } + " = " + std::to_string(val.ref<Json::Unsigned>()) + ";\n"; break;
        case String:   result += "std::string_view " + std::string{ key } + " = \"" + escapeString(val.ref<Json::String>()) + "\";\n"; break;
        case Boolean:  result += "bool " + std::string{ key } + " = " + (val.ref<Json::Boolean>() ? "true" : "false") + ";\n"; break;
        case Null:     result += "std::nullptr_t " + std::string{ key } + " = nullptr;\n"; break;
        case Array: {
            auto& _arr = val.ref<Json::Array>();
            if (_arr.size() == 0) {
                result += "std::array<int, 0> " + std::string{ key } + "{};\n";
                break;
            }
            Json::Type _type = _arr[0].type();
            bool _sameTypes = _type != Array && _type != Object;
            for (auto& elem : _arr) {
                if (!_sameTypes) break;
                bool _number = _type == Unsigned || _type == Integral || _type == Floating;

                if (_number && elem.type() == Integral) _type = _type == Floating ? Floating : Integral;
                else if (_number && elem.type() == Unsigned);
                else if (_number && elem.type() == Floating) _type = Floating;
                else if (_type != elem.type()) _sameTypes = false;
            }

            if (_sameTypes) {
                switch (_type) {
                case Floating: result += "std::array<float, " + std::to_string(_arr.size()) + "> " + std::string{ key } + "{ "; break;
                case Integral: result += "std::array<std::int64_t, " + std::to_string(_arr.size()) + "> " + std::string{ key } + "{ "; break;
                case Unsigned: result += "std::array<std::uint64_t, " + std::to_string(_arr.size()) + "> " + std::string{ key } + "{ "; break;
                case String:   result += "std::array<std::string_view, " + std::to_string(_arr.size()) + "> " + std::string{ key } + "{ "; break;
                case Boolean:  result += "std::array<bool, " + std::to_string(_arr.size()) + "> " + std::string{ key } + "{ "; break;
                case Null:     result += "std::array<std::nullptr_t, " + std::to_string(_arr.size()) + "> " + std::string{ key } + "{ "; break;
                }
                for (auto& elem : _arr) {
                    switch (elem.type()) {
                    case Floating: result += std::to_string(elem.ref<Json::Floating>()) + ", "; break;
                    case Integral: result += std::to_string(elem.ref<Json::Integral>()) + ", "; break;
                    case Unsigned: result += std::to_string(elem.ref<Json::Unsigned>()) + ", "; break;
                    case String:   result += "\"" + escapeString(elem.ref<Json::String>()) + "\", "; break;
                    case Boolean:  result += (elem.ref<Json::Boolean>() ? "true, " : "false, "); break;
                    case Null:     result += "nullptr, "; break;
                    }
                }
                result += "};\n";
            } else {
                result += "\n";
            }

            break;
        }
        case Object: {
            result += jsonToCode(key, val, indent + 1, false, false);
            break;
        }
        }
    }

    for (int i = 0; i < indent; ++i) result += "    ";
    if (!nested) result += "} " + name + "{};\n";
    else result += "};";
    return result;
}

#include <fstream>
#include <filesystem>

#include "bin/data.json"

int main(int argc, char* argv[]) {
    if (argc != 4) return -1;

    std::string name = argv[1];
    std::filesystem::path input = argv[2];
    std::filesystem::path output = argv[3];
    
    input = std::filesystem::absolute(input);
    output = std::filesystem::absolute(output);

    std::ifstream _in{ input };
    std::ofstream _out{ output };

    std::string _json{ std::istreambuf_iterator<char>{ _in }, std::istreambuf_iterator<char>{} };

    auto _res = parseJson(_json);
    if (_res) {
        std::string _str = jsonToCode(name, _res.value());
        _out << _str;
    }

    return 0;
}
