
// ------------------------------------------------

#include <iostream>
#include <fstream>
#include <string>
#include <source_location>
#include <vector>
#include <map>
#include <ranges>
#include <algorithm>
#include <utility>
#include <functional>
#include <cassert>
#include <set>
#include <expected>
#include <thread>
#include <any>
#include <mutex>
#include <type_traits>
#include <concepts>
#include <cstddef>
#include <string_view>
#include <unordered_set>
#include <regex>
#include <span>
#include <complex>
#include <print>
#include <coroutine>
#include <typeindex>
#include <filesystem>
#include <fstream>


constexpr std::string_view path = R"~(C:\Users\Jeroen\AppData\Roaming\.minecraft\resourcepacks\Kaixopolis\assets\minecraft\items)~";

inline std::string file_to_string(std::filesystem::path path) {
    std::ifstream file{ path };
    std::stringstream _stream{};
    _stream << file.rdbuf();
    return _stream.str();
}

constexpr void replace_str(std::string& str, std::string_view from, std::string_view to) {
    if (from.empty()) return;
    size_t start_pos = 0;
    while ((start_pos = str.find(from, start_pos)) != std::string::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length(); // In case 'to' contains 'from', like replacing 'x' with 'yx'
    }
}

constexpr std::string_view item_model = R"~({
  "parent": "minecraft:item/generated",
  "textures": {
    "layer0": "uno:item/$1"
  }
}
)~";

constexpr std::string_view item_model = R"~({
  "type": "minecraft:chest",
  "pools": [
    {
      "bonus_rolls": 0.0,
      "rolls": 1.0,
      "entries": [
        {
          "type": "minecraft:item",
          "name": "minecraft:jigsaw",
          "weight": 1,
          "functions": [
            {
              "function": "minecraft:set_components",
              "components": {
                "custom_model_name": "$1",
                "item_name": "$2"
              }
            }
          ]
        },
      ]
    }
  ]
}
)~";

constexpr std::string_view item_case = R"~({ "when": "$1", "model": { "type": "model", "model": "uno:item/$1" } },
)~";

int main() {
    std::vector<std::string> colors{ "r", "g", "b", "y" };
    std::vector<std::string> cards{ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "y", "d", "r", "s" };
    std::vector<std::string> extra{ "qu", "qw" };

    std::filesystem::path outpath = "./items";

    std::string cases = "";

    for (auto& color : colors) {
        for (auto& card : cards) {
            std::string filename = "uno_" + color + card;

            std::string content{ item_model };
            replace_str(content, "$1", filename);

            std::string itemcase{ item_case };
            replace_str(itemcase, "$1", filename);
            cases += itemcase;

            std::ofstream out{ outpath / (filename + ".json") };
            out << content;
        }
    }

    for (auto& name : extra) {
        std::string filename = "uno_" + name;

        std::string content{ item_model };
        replace_str(content, "$1", filename);

        std::string itemcase{ item_case };
        replace_str(itemcase, "$1", filename);
        cases += itemcase;

        std::ofstream out{ outpath / (filename + ".json") };
        out << content;
    }

    std::ofstream out{ outpath / "cases.json" };
    out << cases;

    return 0;
    std::filesystem::path files{ path };

    //std::filesystem::path outpath = "./items";

    for (auto& file : std::filesystem::directory_iterator(files)) {

        if (!file.exists()) continue;
        if (!file.is_regular_file()) continue;
        if (file.path().extension() != ".json") continue;

        auto filename = file.path().filename().string();

        if (!filename.contains("axe.json") &&
            !filename.contains("pickaxe.json") &&
            !filename.contains("shovel.json") &&
            !filename.contains("sword.json") &&
            !filename.contains("hoe.json")) continue; // not a tool

        auto content = file_to_string(file.path());

        replace_str(content, R"~("when": "minecraft:quartz")~", R"~("when": { "trim": "minecraft:quartz" })~");
        replace_str(content, R"~("when": "minecraft:iron")~", R"~("when": { "trim": "minecraft:iron" })~");
        replace_str(content, R"~("when": "minecraft:netherite")~", R"~("when": { "trim": "minecraft:netherite" })~");
        replace_str(content, R"~("when": "minecraft:redstone")~", R"~("when": { "trim": "minecraft:redstone" })~");
        replace_str(content, R"~("when": "minecraft:copper")~", R"~("when": { "trim": "minecraft:copper" })~");
        replace_str(content, R"~("when": "minecraft:gold")~", R"~("when": { "trim": "minecraft:gold" })~");
        replace_str(content, R"~("when": "minecraft:emerald")~", R"~("when": { "trim": "minecraft:emerald" })~");
        replace_str(content, R"~("when": "minecraft:diamond")~", R"~("when": { "trim": "minecraft:diamond" })~");
        replace_str(content, R"~("when": "minecraft:lapis")~", R"~("when": { "trim": "minecraft:lapis" })~");
        replace_str(content, R"~("when": "minecraft:amethyst")~", R"~("when": { "trim": "minecraft:amethyst" })~");
        replace_str(content, R"~("when": "minecraft:resin")~", R"~("when": { "trim": "minecraft:resin" })~");

        replace_str(content, R"~("when": "kaixopolis:olivine")~", R"~("when": { "trim": "kaixopolis:olivine" })~");
        replace_str(content, R"~("when": "kaixopolis:fluorite")~", R"~("when": { "trim": "kaixopolis:fluorite" })~");
        replace_str(content, R"~("when": "kaixopolis:amber")~", R"~("when": { "trim": "kaixopolis:amber" })~");
        replace_str(content, R"~("when": "kaixopolis:thulite")~", R"~("when": { "trim": "kaixopolis:thulite" })~");
        replace_str(content, R"~("when": "kaixopolis:pyrite")~", R"~("when": { "trim": "kaixopolis:pyrite" })~");
        replace_str(content, R"~("when": "kaixopolis:azurite")~", R"~("when": { "trim": "kaixopolis:azurite" })~");
        replace_str(content, R"~("when": "kaixopolis:red_calcite")~", R"~("when": { "trim": "kaixopolis:red_calcite" })~");
        replace_str(content, R"~("when": "kaixopolis:chromium")~", R"~("when": { "trim": "kaixopolis:chromium" })~");
        replace_str(content, R"~("when": "kaixopolis:rhodium")~", R"~("when": { "trim": "kaixopolis:rhodium" })~");

        replace_str(content, R"~("property": "minecraft:trim_material")~", R"~("property": "minecraft:component", "component": "minecraft:custom_data")~");

        std::ofstream out{ outpath / filename };
        out << content;
    }

    return 0;
}