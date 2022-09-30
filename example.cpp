#include <iostream>
#include <string_view>
#include <vector>
#include <charconv>
#include <map>
#include <variant>
#include <optional>
#include <string>
#include <array>
#include <list>


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
        { kaixo::json::Floating, kaixo::json::Floating, kaixo::json::Floating,                  -1, kaixo::json::Floating, -1,                  -1, kaixo::json::Floating },
        { kaixo::json::Floating, kaixo::json::Integral, kaixo::json::Integral,                  -1, kaixo::json::Integral, -1,                  -1, kaixo::json::Integral },
        { kaixo::json::Floating, kaixo::json::Integral, kaixo::json::Unsigned,                  -1, kaixo::json::Unsigned, -1,                  -1, kaixo::json::Unsigned },
        {                    -1,                    -1,                    -1, kaixo::json::String,                    -1, -1,                  -1,   kaixo::json::String },
        { kaixo::json::Floating, kaixo::json::Integral, kaixo::json::Unsigned,                  -1,  kaixo::json::Boolean, -1,                  -1,  kaixo::json::Boolean },
        {                    -1,                    -1,                    -1,                  -1,                    -1, -1,                  -1,                    -1 },
        {                    -1,                    -1,                    -1,                  -1,                    -1, -1, kaixo::json::Object,   kaixo::json::Object },
        { kaixo::json::Floating, kaixo::json::Integral, kaixo::json::Unsigned, kaixo::json::String,  kaixo::json::Boolean, -1, kaixo::json::Object,     kaixo::json::Null }
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

    std::string generate_single(const kaixo::json& json, const std::string& n, bool def = true, bool out = true) {
        std::string name = filterName(n);
        switch (json.type()) {
        case kaixo::json::Null:
            if (out) output += "nullptr";
            return "std::nullptr_t " + name + "{};";
        case kaixo::json::Boolean:
            if (out) output += json.as<bool>() ? "true" : "false";
            return "bool " + name + "{};";
        case kaixo::json::Floating:
            if (out) output += std::to_string(json.as<kaixo::json::floating>());
            return "double " + name + "{};";
        case kaixo::json::Integral:
            if (out) output += std::to_string(json.as<kaixo::json::integral>());
            return "std::int64_t " + name + "{};";
        case kaixo::json::Unsigned:
            if (out) output += std::to_string(json.as<kaixo::json::unsigned_integral>());
            return "std::uint64_t " + name + "{};";
        case kaixo::json::String:
            if (out) output += "R\"(" + json.as<kaixo::json::string>() + ")\"";
            return "std::string_view " + name + "{};";
        case kaixo::json::Object: {
            std::string _name = name + "_" + std::to_string(index++);
            generate(json, name, _name, def, out);
            return _name + "_t " + name + "{};";
        }
        case kaixo::json::Array: {
            std::string _name = name + "_" + std::to_string(index++);
            generate(json, name, _name, def, out);
            return _name + "_t " + name + "{};";
        }
        }
    }

    void generate(const kaixo::json& json, const std::string& n, const std::string& type, bool def = true, bool out = true) {
        using namespace std::string_literals;
        std::string name = filterName(n);
        std::string definition = "struct " + type + "_t {\n";
        if (out) output += "{\n";
        indent++;
        if (json.is(kaixo::json::Object)) {
            auto& obj = json.as<kaixo::json::object>();

            for (auto& [key, value] : obj) {
                definition += "    ";
                if (out) for (int i = 0; i < indent; ++i) output += "    ";
                if (out) output += "." + filterName(key) + " = ";
                definition += generate_single(value, key, def, out);
                definition += "\n";
                if (out) output += ",\n";
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
            }

            // If array of objects
            bool _works = true;
            if (common_type == kaixo::json::Object) {
                // Collect all different keys used
                constexpr auto combine_objects = [](auto& self, kaixo::json& a, const kaixo::json& b) -> bool {
                    if (b.type() == kaixo::json::Null) return true;
                    if (b.type() != kaixo::json::Object) {
                        if (b.type() == a.type() || a.type() == kaixo::json::Null) return a = b, true;
                        else return false;
                    }
                    auto& _obj = b.as<kaixo::json::object>();
                    for (auto& [key, value] : _obj) {
                        // If clashing names and types, it won't work
                        if (a[key].type() == kaixo::json::Null || a[key].type() == value.type()) {
                            if (a[key].type() == kaixo::json::Object) {
                                if (!self(self, a[key], value)) return false;
                            } else if (a[key].type() == kaixo::json::Array) {
                                for (auto& m : value.as<kaixo::json::array>()) {
                                    if (!self(self, a[key][0], m)) return false;
                                }
                                for (std::size_t i = a[key].size(); i < value.size(); ++i)
                                    a[key].emplace(a[key][0]);
                            } else if (value.type() == kaixo::json::Array) {
                                a[key] = kaixo::json::array{};
                                for (auto& m : value.as<kaixo::json::array>()) {
                                    if (!self(self, a[key][0], m)) return false;
                                }
                                for (std::size_t i = a[key].size(); i < value.size(); ++i)
                                    a[key].emplace(a[key][0]);
                            } else a[key] = value;
                        } else {
                            return false;
                            break;
                        }
                    }
                    return true;
                };
                kaixo::json _all = kaixo::json::object{};
                for (auto& value : arr) {
                    if (!combine_objects(combine_objects, _all, value)) {
                        _works = false;
                        break;
                    }
                }

                if (_works) { // If no clashing names/types
                    std::string common_object = "struct " + type + "_mems_t {\n";
                    for (auto& [key, value] : _all.as<kaixo::json::object>()) {
                        common_object += "    ";
                        common_object += generate_single(value, key, def, false);
                        common_object += "\n";
                    }
                    common_object += "};\n\n";
                    if (def) definitions += common_object;

                    for (auto& value : arr) {
                        auto istr = std::to_string(index);
                        if (out) for (int i = 0; i < indent; ++i) output += "    ";
                        generate(value, name + "_" + istr, type + "_mems", false, out);
                        definition += "    " + type + "_mems_t " + name + "_" + istr + ";" + "\n";
                        accessor += "        case " + istr + ": return static_cast<type>(" + name + "_" + istr + ");\n";
                        if (out) output += ",\n";
                        index++;
                    }

                    definition += "\n"
                        "    constexpr auto operator[](std::size_t index) const {\n"
                        "        using type = " + type + "_mems_t" + ";\n"
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
            
            if (common_type != kaixo::json::Object || !_works) {
                for (auto& value : arr) {
                    auto istr = std::to_string(index);
                    if (out) for (int i = 0; i < indent; ++i) output += "    ";
                    definition += "    " + generate_single(value, name + "_" + istr, def, out) + "\n";
                    accessor += "        case " + istr + ": return static_cast<type>(" + name + "_" + istr + ");\n";
                    if (out) output += ",\n";
                    index++;
                }
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
        if (def) definitions += definition;
        if (out) for (int i = 0; i < indent; ++i) output += "    ";
        if (out) output += "}";
    }
}; 





constexpr auto roinrs = R"json(
{
    "integer" : 1,
    "uinteger" : -1,
    "floating" : -0.31314e+10,
    "boolean" : false,
    "object" : {},
    "array1" : [ 
        { "name" : "test" },
        { "name" : "test", "id" : 10 }
    ],
    "array2" : [],
    "array3" : [1, 2, 3, 4],
    "array4" : ["a", 1, true]
}
)json";

int main() {
    using namespace kaixo;
    auto aeoa = json::parse(roinrs);
    if (aeoa.has_value()) {
        auto res = json2hpp::generate("data", aeoa.value());
        std::cout << res;

        return 0;
    }

    return 0;
}

//#include <filesystem>
//#include <fstream>
//
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
//    auto _res = kaixo::json::parse(_json);
//    if (_res) {
//        JsonToCode _toCode{ _res.value() };
//        _toCode.generate(name);
//        _out << _toCode.definitions;
//    }
//
//    return 0;
//}
