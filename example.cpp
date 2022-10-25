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


#include "json.hpp"

using namespace kaixo;



struct window {
    double width;
    double height;

    void setTitle(const std::string& title) { this->title = title; }
    void setName(const std::string& name) { this->name = name; }
private:
    std::string title;
    std::string name;
};

template<> constexpr auto json_size<window> = 4;
template<> constexpr auto json_member<window, 0> = std::pair{ &window::setTitle, "title" };
template<> constexpr auto json_member<window, 1> = std::pair{ &window::setName, "name" };
template<> constexpr auto json_member<window, 2> = std::pair{ &window::width, "width" };
template<> constexpr auto json_member<window, 3> = std::pair{ &window::height, "height" };

struct menu_item {
    std::string value;
    std::string onclick;
};

template<> constexpr auto json_size<menu_item> = 2;
template<> constexpr auto json_member<menu_item, 0> = std::pair{ &menu_item::value, "value" };
template<> constexpr auto json_member<menu_item, 1> = std::pair{ &menu_item::onclick, "onclick" };

struct menu {
    void setId(const std::string& id) { this->id = id; }
    void setValue(const std::string& value) { this->value = value; }
    void addMenuItem(const menu_item& item) { items.push_back(item); }
private:
    std::string id;
    std::string value;
    std::vector<menu_item> items;
};

template<> constexpr auto json_size<menu> = 3;
template<> constexpr auto json_member<menu, 0> = std::pair{ &menu::setId, "id" };
template<> constexpr auto json_member<menu, 1> = std::pair{ &menu::setValue, "value" };
template<> constexpr auto json_member<menu, 2> = std::pair{ &menu::addMenuItem, "menuitem" };

struct widget {
    bool debug;
    window window;
    menu menu;
};

template<> constexpr auto json_size<widget> = 3;
template<> constexpr auto json_member<widget, 0> = std::pair{ &widget::debug, "debug" };
template<> constexpr auto json_member<widget, 1> = std::pair{ &widget::window, "window" };
template<> constexpr auto json_member<widget, 2> = std::pair{ &widget::menu, "menu" };

constexpr auto vaione = R"##(
{
    "audio": "Synchronous Audio Router",
    "samplerate": 48000,
    "buffersize": 32,
    "midiin": "Arturia KeyLab Essential 49",
    "midiout": "loopMIDI Port",
    "buttons": [ ],
    "channels" : {
        "outputs" : [
            { "endpoints" : [ "OBS 1", "OBS 2" ], "name" : "OBS", "midimapping" : [] },
            { "endpoints" : [ "FLStudio 1", "FLStudio 2" ], "name" : "FLStudio", "midimapping" : [] },
            { "endpoints" : [ "Ableton 1", "Ableton 2" ], "name" : "Ableton", "midimapping" : [] },
            { "endpoints" : [ "Discord 1", "Discord 2" ], "name" : "Discord", "midimapping" : [] },
            { "endpoints" : [ "Output 1", "Output 2" ], "name" : "Output", "midimapping" : [ { "cc" : 73, "param" : "gain" } ] }
        ],
        "inputs" : [
            { "endpoints" : [ "Music 1", "Music 2" ], "name" : "Music", "midimapping" : [ { "cc" : 75, "param" : "gain" } ] },
            { "endpoints" : [ "Input 1" ], "name" : "Modular", "midimapping" : [ { "cc" : 79, "param" : "gain" } ] },
            { "endpoints" : [ "FLStudio 1", "FLStudio 2" ], "name" : "FLStudio", "midimapping" : [ { "cc" : 72, "param" : "gain" } ] },
            { "endpoints" : [ "Discord 1", "Discord 2" ], "name" : "Discord", "midimapping" : [ { "cc" : 80, "param" : "gain" } ] },
            { "endpoints" : [ "System 1", "System 2" ], "name" : "System", "midimapping" : [ { "cc" : 81, "param" : "gain" } ] },
            { "endpoints" : [ "Ableton 1", "Ableton 2" ], "name" : "Ableton", "midimapping" : [ { "cc" : 82, "param" : "gain" } ] },
            { "endpoints" : [ "Chrome 1", "Chrome 2" ], "name" : "Chrome", "midimapping" : [ { "cc" : 83, "param" : "gain" } ] },
            { "endpoints" : [ "Input 2" ], "name" : "Mic", "midimapping" : [ { "cc" : 85, "param" : "gain" } ] }
        ]
    },
    "theme": {  }
}
)##";

struct ButtonLink { std::uint64_t cc; std::string run; };
template<> constexpr auto json_size<ButtonLink> = 2;
template<> constexpr auto json_member<ButtonLink, 0> = std::pair{ &ButtonLink::cc, "cc" };
template<> constexpr auto json_member<ButtonLink, 1> = std::pair{ &ButtonLink::run, "run" };

struct MidiMapping { std::uint64_t cc; std::string param; };
template<> constexpr auto json_size<MidiMapping> = 2;
template<> constexpr auto json_member<MidiMapping, 0> = std::pair{ &MidiMapping::cc, "cc" };
template<> constexpr auto json_member<MidiMapping, 1> = std::pair{ &MidiMapping::param, "param" };

struct Channel {
    std::vector<std::string> endpoints;
    std::string name;
    std::vector<MidiMapping> midimapping;
};

template<> constexpr auto json_size<Channel> = 3;
template<> constexpr auto json_member<Channel, 0> = std::pair{ &Channel::endpoints, "endpoints" };
template<> constexpr auto json_member<Channel, 1> = std::pair{ &Channel::name, "name" };
template<> constexpr auto json_member<Channel, 2> = std::pair{ &Channel::midimapping, "midimapping" };

struct Channels {
    void addOutput(const Channel& channel) {
        std::cout << "output: " << channel.name << "\n";
    }

    void addInput(const Channel& channel) {
        std::cout << "input: " << channel.name << "\n";
    }
};

template<> constexpr auto json_size<Channels> = 2;
template<> constexpr auto json_member<Channels, 0> = std::pair{ &Channels::addOutput, "outputs" };
template<> constexpr auto json_member<Channels, 1> = std::pair{ &Channels::addInput, "inputs" };

struct Settings {
    std::string audio;
    std::uint64_t samplerate;
    std::uint64_t buffersize;
    std::string midiin;
    std::string midiout;
    std::vector<ButtonLink> buttons;
    Channels channels;
};

template<> constexpr auto json_size<Settings> = 7;
template<> constexpr auto json_member<Settings, 0> = std::pair{ &Settings::audio, "audio" };
template<> constexpr auto json_member<Settings, 1> = std::pair{ &Settings::samplerate, "samplerate" };
template<> constexpr auto json_member<Settings, 2> = std::pair{ &Settings::buffersize, "buffersize" };
template<> constexpr auto json_member<Settings, 3> = std::pair{ &Settings::midiin, "midiin" };
template<> constexpr auto json_member<Settings, 4> = std::pair{ &Settings::midiout, "midiout" };
template<> constexpr auto json_member<Settings, 5> = std::pair{ &Settings::buttons, "buttons" };
template<> constexpr auto json_member<Settings, 6> = std::pair{ &Settings::channels, "channels" };

int main() {
    auto _json = json::parse(vaione);

    Settings res = json2cpp::object_from_json<Settings>(_json.value());

    return 0;
}

 