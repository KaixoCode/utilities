#include <iostream>
#include <string_view>
#include <vector>
#include <charconv>
#include <map>
#include <variant>
#include <optional>
#include <string>
#include <array>


#include "Json.hpp"

constexpr auto val = R"(
{
  "afaoin" : [ "woof", 1.0, -10 ],
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
  "carrot" : 1.42e4,
  "woof" : [ "hello", "world" ],
  "efaee" : [ -1, 30.0, 130 ]
}

)";

constexpr auto test = R"(
{"hello"}
)";

struct JsonToCode {
    constexpr static int common[8][8]{
        { kaixo::json::Floating, kaixo::json::Floating, kaixo::json::Floating,                  -1, kaixo::json::Floating, -1, -1, kaixo::json::Floating },
        { kaixo::json::Floating, kaixo::json::Integral, kaixo::json::Integral,                  -1, kaixo::json::Integral, -1, -1, kaixo::json::Integral },
        { kaixo::json::Floating, kaixo::json::Integral, kaixo::json::Unsigned,                  -1, kaixo::json::Unsigned, -1, -1, kaixo::json::Unsigned },
        {                    -1,                    -1,                    -1, kaixo::json::String,                    -1, -1, -1,   kaixo::json::String },
        { kaixo::json::Floating, kaixo::json::Integral, kaixo::json::Unsigned,                  -1,  kaixo::json::Boolean, -1, -1,  kaixo::json::Boolean },
        {                    -1,                    -1,                    -1,                  -1,                    -1, -2, -1,                    -1 },
        {                    -1,                    -1,                    -1,                  -1,                    -1, -1, -2,                    -1 },
        { kaixo::json::Floating, kaixo::json::Integral, kaixo::json::Unsigned, kaixo::json::String,  kaixo::json::Boolean, -1, -1,     kaixo::json::Null }
    };
    constexpr static std::string_view names[8]{
        "double", "std::int64_t", "std::uint64_t", "std::string_view", "bool", "", "", "std::nullptr_t"
    };

    kaixo::json json;
    std::string definitions = "";
    std::string output = "";
    int indent = 0;
    int index = 0;
    
    void generate(const std::string& n) {
        std::string name = filterName(n);
        output += "constexpr " + name + "_t " + name + " = ";
        generate(json, name, name);
        definitions += output + ";";
    }

    std::string filterName(std::string name) {
        if (name.size() == 0) return "_" + std::to_string(index++);
        if (kaixo::oneOf(name[0], "0123456789")) name = "_" + name;
        for (auto i = name.begin(); i != name.end();) {
            if (*i == '-') *i = '_';
            if (!kaixo::oneOf(*i, "_0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")) 
                name.erase(i);
            else ++i;
        }
        return name;
    }

    std::string generate_single(const kaixo::json& json, const std::string& n) {
        std::string name = filterName(n);
        switch (json.type()) {
        case kaixo::json::Null:
            output += "nullptr";
            return "std::nullptr_t " + name + ";";
        case kaixo::json::Boolean:
            output += json.as<bool>() ? "true" : "false";
            return "bool " + name + ";";
        case kaixo::json::Floating:
            output += std::to_string(json.as<kaixo::json::floating>());
            return "double " + name + ";";
        case kaixo::json::Integral:
            output += std::to_string(json.as<kaixo::json::integral>());
            return "std::int64_t " + name + ";";
        case kaixo::json::Unsigned:
            output += std::to_string(json.as<kaixo::json::unsigned_integral>());
            return "std::uint64_t " + name + ";";
        case kaixo::json::String:
            output += "R\"(" + json.as<kaixo::json::string>() + ")\"";
            return "std::string_view " + name + ";";
        case kaixo::json::Object: {
            std::string _name = name + "_" + std::to_string(index++);
            generate(json, name, _name);
            return _name + "_t " + name + ";";
        }
        case kaixo::json::Array: {
            std::string _name = name + "_" + std::to_string(index++);
            generate(json, name, _name);
            return _name + "_t " + name + ";";
        }
        }
    }

    void generate(const kaixo::json& json, const std::string& name, const std::string& type) {
        using namespace std::string_literals;
        std::string definition = "struct " + type + "_t {\n";
        output += type + "_t {\n";
        indent++;
        if (json.is(kaixo::json::Object)) {
            auto& obj = json.as<kaixo::json::object>();

            for (auto& [key, value] : obj) {
                definition += "    ";
                for (int i = 0; i < indent; ++i) output += "    ";
                definition += generate_single(value, key);
                definition += "\n";
                output += ",\n";
            }
        } else if (json.is(kaixo::json::Array)) {
            auto& arr = json.as<kaixo::json::array>();

            std::string accessor = "";
            std::size_t index = 0;
            int common_type = -3;

            for (auto& value : arr) {
                if (common_type < 0 && common_type != -3);
                else if (common_type != -3) common_type = common[common_type][value.type()];
                else common_type = value.type();

                auto istr = std::to_string(index);
                for (int i = 0; i < indent; ++i) output += "    ";
                definition += "    " + generate_single(value, name + "_" + istr) + "\n";
                accessor += "        case " + istr + ": return static_cast<type>(" + name + "_" + istr + ");\n";
                output += ",\n";
                index++;
            }

            if (common_type >= 0 && common_type != kaixo::json::Array && common_type != kaixo::json::Object) {
                definition += "\n"
                    "    constexpr auto operator[](std::size_t index) const {\n"
                    "        using type = " + std::string{ names[common_type] } + ";\n"
                    "        switch(index) {\n" + accessor +
                    "        }\n"
                    "    }\n"
                    "    \n"
                    "    struct iterator {\n"
                    "        std::size_t index = 0;\n"
                    "        const " + type + "_t* me = nullptr;\n"
                    "        \n"
                    "        constexpr auto operator*() { return (*me)[index]; }\n"
                    "        constexpr iterator& operator++() { ++index; return *this; }\n"
                    "        constexpr bool operator==(const iterator& o) const { return index == o.index; }\n"
                    "    };\n"
                    "    \n"
                    "    constexpr std::size_t size() const { return " + std::to_string(arr.size()) + "; }\n"
                    "    constexpr iterator begin() const { return iterator{ 0, this }; }\n"
                    "    constexpr iterator end() const { return iterator{ size(), this }; }\n";
            }
        }

        indent--;
        definition += "};\n\n";
        definitions += definition;
        for (int i = 0; i < indent; ++i) output += "    ";
        output += "}";
    }
};

constexpr auto roinrs = R"(

)";

int main() {
    using namespace kaixo;

    auto aeoa = json::parse(roinrs);
    if (aeoa.has_value()) {
        JsonToCode aefa{ aeoa.value() };
        aefa.generate("data");

        std::cout << aefa.definitions;

        return 0;
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
