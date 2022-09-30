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
