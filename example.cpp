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

constexpr auto test = R"(
{"hello"}
)";

int main() {
    using namespace kaixo;


    auto data = json::parse(val);
    if (data) {
        json json = data.value();
        if (json.contains("name") && json["name"].is(json::String)) {
            auto& str1 = json["name"].as<json::string>();
            std::cout << str1 << "\n";
        }
    }
    json _val = json::object{};

    json::A_t;

    _val["1"] = json::array{ 1, 2, 3, 4 };

    _val["1"].as<json::array>()[0].as<json::integral>();

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
