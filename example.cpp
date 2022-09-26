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

    template<class> struct ToType {};
    template<> struct ToType<Floating> { constexpr static Type type = Type::Floating; };
    template<> struct ToType<Integral> { constexpr static Type type = Type::Integral; };
    template<> struct ToType<Unsigned> { constexpr static Type type = Type::Unsigned; };
    template<> struct ToType<String> { constexpr static Type type = Type::String; };
    template<> struct ToType<Boolean> { constexpr static Type type = Type::Boolean; };
    template<> struct ToType<Array> { constexpr static Type type = Type::Array; };
    template<> struct ToType<Object> { constexpr static Type type = Type::Object; };
    template<> struct ToType<Null> { constexpr static Type type = Type::Null; };

    template<class Ty> struct Alias { using Type = Ty; };
    template<> struct Alias<double> { using Type = Floating; };
    template<> struct Alias<float> { using Type = Floating; };
    template<> struct Alias<bool> { using Type = Boolean; };
    template<> struct Alias<std::string> { using Type = String; };
    template<> struct Alias<std::string_view> { using Type = String; };
    template<std::size_t N> struct Alias<char[N]> { using Type = String; };
    template<std::signed_integral Ty> struct Alias<Ty> { using Type = Integral; };
    template<std::unsigned_integral Ty> struct Alias<Ty> { using Type = Unsigned; };

public:
    template<class Ty = Null>
    Json(const Ty& ty = {}) : m_Value(static_cast<Alias<Ty>::Type>(ty)) {}

    template<class Ty> Ty& as() { return std::get<Ty>(m_Value); }
    template<class Ty> const Ty& as() const { return std::get<Ty>(m_Value); }
    template<class Ty> bool is() const { return ToType<Ty>::type == type(); }
    bool is(Type t) const { return t == type(); }
    Type type() const { return static_cast<Type>(m_Value.index()); }

    bool has(std::string_view index) const {
        if (m_Value.index() == Type::Null) return false;
        else if (m_Value.index() != Type::Object) throw std::exception("Not an object.");
        auto _it = as<Object>().find(index);
        return _it != as<Object>().end();
    }

    Json& operator[](std::string_view index) {
        if (m_Value.index() == Type::Null) m_Value = Object{};
        else if (m_Value.index() != Type::Object) throw std::exception("Not an object.");
        auto _it = as<Object>().find(index);
        if (_it == as<Object>().end()) return as<Object>()[std::string{ index }];
        else return _it->second;
    }

    void push_back(Json json) {
        if (m_Value.index() == Type::Null) m_Value = Array{};
        else if (m_Value.index() != Type::Array) throw std::exception("Not an array.");
        std::get<Array>(m_Value).push_back(json);
    }

private:
    static std::string removeDoubleEscapes(std::string_view str) {
        std::string _str{ str };
        for (auto _i = _str.begin(); _i != _str.end();)
            if (*_i == '\\') _i = _str.erase(_i); else ++_i;
        return _str;
    }

    static bool checkFront(std::string_view& json, char c, bool empty = false) {
        if ((json = trim(json)).empty()) return false;
        if (json.front() != c) return false;
        return !(json = trim(json.substr(1))).empty() || empty;
    }

    static bool parseWord(std::string_view& json, std::string_view word) {
        return json.starts_with(word) ? json = json.substr(word.size()), true : false;
    }

    static std::optional<Json> parseJsonBool(std::string_view& json) {
        if (parseWord(json, "true")) return true;
        if (parseWord(json, "false")) return false;
        return {};
    }

    static std::optional<Json> parseJsonNull(std::string_view& json) {
        if (parseWord(json, "null")) return nullptr;
        return {};
    }

    static std::optional<Json> parseJsonNumber(std::string_view& json) {
        auto _json = json;
        auto _size = 0ull;
        bool _floating = false;
        bool _signed = false;

        auto _isDigit = [&] { return oneOf(_json.front(), "0123456789"); };
        auto _consumeOne = [&] { return ++_size, !(_json = _json.substr(1)).empty(); };
        auto _consumeDigits = [&] {
            if (!_isDigit()) return false;
            while (_isDigit()) if (!_consumeOne()) return false;
            return true;
        };
        
        if (_json.front() == '-') {
            _signed = true; 
            if (!_consumeOne()) return {}; 
        } 

        if (_json.front() == '0') {
            if (!_consumeOne()) return {};
            if (_isDigit()) return {};
        } else if (!_consumeDigits()) return {};

        if (_json.front() == '.') {
            _floating = true;
            if (!_consumeOne()) return {};
            if (!_consumeDigits()) return {};
        }

        if (oneOf(_json.front(), "eE")) {
            if (!_consumeOne()) return {};
            if (oneOf(_json.front(), "-+") && !_consumeOne()) return {};
            if (!_consumeDigits()) return {};
        }

        _json = json.substr(0, _size);
        auto _parse = [&]<class Ty>(Ty val) {
            std::from_chars(_json.data(), _json.data() + _json.size(), val);
            return val;
        };
        json = json.substr(_size);
        return _floating ? _parse(0.0) : _signed ? _parse(0ll) : _parse(0ull);
    }

    static std::optional<Json> parseJsonString(std::string_view& json) {
        auto _json = json;
        if (!checkFront(_json, '"')) return {};
        if (checkFront(_json, '"')) return json = _json, "";
        auto _offset = 0ull;
        auto _result = _json;
        while (true) {
            auto _index = _json.find_first_of('"');
            if (_index == std::string_view::npos) return {};
            if (_result[_offset + _index - 1] == '\\') // Escaped "
                _json = _result.substr(_offset += _index + 1);
            else {
                json = _result.substr(_offset + _index + 1);
                return removeDoubleEscapes(_result.substr(0, _offset + _index));
            }
        }
    }

    static std::optional<Json> parseJsonArray(std::string_view& json) {
        auto _json = json;
        if (!checkFront(_json, '[')) return {};
        std::optional<Json> _result = Json::Array{};
        std::optional<Json> _value = {};
        while (_value = parseJsonValue(_json)) {
            _result.value().push_back(_value.value());
            if (!checkFront(_json, ',')) break;
        }
        if (!checkFront(_json, ']', true)) return {};
        json = _json;
        return _result;
    }

    static std::optional<Json> parseJsonObject(std::string_view& json) {
        auto _json = json;
        if (!checkFront(_json, '{')) return {};
        std::optional<Json> _result = Json::Object{};
        std::optional<Json> _value = {};
        while (_value = parseJsonString(_json)) {
            auto _key = _value.value().as<Json::String>();
            if (!checkFront(_json, ':')) break;
            if (!(_value = parseJsonValue(_json))) break;
            _result.value()[_key] = _value.value();
            if (!checkFront(_json, ',')) break;
        }
        if (!checkFront(_json, '}', true)) return {};
        json = _json;
        return _result;
    }

    static std::optional<Json> parseJsonValue(std::string_view& json) {
        std::optional<Json> _result = {};
        if ((_result = parseJsonString(json)) || (_result = parseJsonObject(json))
            || (_result = parseJsonBool(json)) || (_result = parseJsonArray(json))
            || (_result = parseJsonNull(json)) || (_result = parseJsonNumber(json))) {
            return _result;
        }
        return {};
    }

public:
    static std::optional<Json> parse(std::string_view json) {
        if ((json = trim(json)).empty()) return {};
        std::optional<Json> _result = {};
        if ((_result = parseJsonObject(json)) && trim(json).empty()) return _result;
        if ((_result = parseJsonArray(json)) && trim(json).empty()) return _result;
        return {};
    }
};

std::string escapeString(std::string str) {
    for (auto _i = str.begin(); _i != str.end(); ++_i)
        if (oneOf(*_i, "\"\\\/\b\f\n\r\t")) str.insert(_i, '\\'), ++_i;
    return str;
}

std::string jsonToCode(const std::string& name, const Json& json, int indent = 0, bool nested = false, bool ce = true) {
    std::string result = "";
    if (ce) result += "constexpr ";
    result += "struct " + name + "_t {\n";

    for (auto& [key, val] : json.as<Json::Object>()) {
        using enum Json::Type;
        for (int i = 0; i < indent + 1; ++i) result += "    ";
        switch (val.type()) {
        case Floating: result += "double " + std::string{ key } + " = " + std::to_string(val.as<Json::Floating>()) + ";\n"; break;
        case Integral: result += "std::int64_t " + std::string{ key } + " = " + std::to_string(val.as<Json::Integral>()) + ";\n"; break;
        case Unsigned: result += "std::uint64_t " + std::string{ key } + " = " + std::to_string(val.as<Json::Unsigned>()) + ";\n"; break;
        case String:   result += "std::string_view " + std::string{ key } + " = \"" + escapeString(val.as<Json::String>()) + "\";\n"; break;
        case Boolean:  result += "bool " + std::string{ key } + " = " + (val.as<Json::Boolean>() ? "true" : "false") + ";\n"; break;
        case Null:     result += "std::nullptr_t " + std::string{ key } + " = nullptr;\n"; break;
        case Array: {
            auto& _arr = val.as<Json::Array>();
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
                    case Floating: result += std::to_string(elem.as<Json::Floating>()) + ", "; break;
                    case Integral: result += std::to_string(elem.as<Json::Integral>()) + ", "; break;
                    case Unsigned: result += std::to_string(elem.as<Json::Unsigned>()) + ", "; break;
                    case String:   result += "\"" + escapeString(elem.as<Json::String>()) + "\", "; break;
                    case Boolean:  result += (elem.as<Json::Boolean>() ? "true, " : "false, "); break;
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

#include "data.json"

constexpr auto json = R"(
{
  "str1": "",
  "str2": "\"",
  "age": 23,
  "name": "John",
  "data": {
    "number": -13.42e19,
    "array": [ 1, 2, 3, 4, 5 ],
    "boolean": true
  },
  "aawa": {  },
  "test": "value",
  "carrot" : 1.42e4
}

)";

int main() {
    auto data = Json::parse(json);
    if (data) {
        Json json = data.value();
        if (json.has("name") && json["name"].is<Json::String>()) {
            auto& str1 = json["name"].as<Json::String>();
            std::cout << str1 << "\n";
        }
    }
    return 0;
}

//int main(int argc, char* argv[]) {
//    if (argc != 4) return -1;
//
//    std::string name = argv[1];
//    std::filesystem::path input = argv[2];
//    std::filesystem::path output = argv[3];
//    
//    input = std::filesystem::absolute(input);
//    output = std::filesystem::absolute(output);
//
//    std::ifstream _in{ input };
//    std::ofstream _out{ output };
//
//    std::string _json{ std::istreambuf_iterator<char>{ _in }, std::istreambuf_iterator<char>{} };
//
//    auto _res = parseJson(_json);
//    if (_res) {
//        std::string _str = jsonToCode(name, _res.value());
//        _out << _str;
//    }
//
//    return 0;
//}
