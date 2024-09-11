
// ------------------------------------------------

#include <iostream>
#include <string>
#include <source_location>
#include <vector>
#include <map>
#include <ranges>
#include <algorithm>
#include <utility>
#include <functional>
#include <cassert>
#include <expected>
#include <thread>
#include <any>
#include <mutex>
#include <type_traits>
#include <concepts>
#include <cstddef>
#include <utility>
#include <algorithm>

// ------------------------------------------------

template<class Ty>
struct State {};

// ------------------------------------------------

struct States {
private:

    // ------------------------------------------------

    struct Value {
        constexpr virtual ~Value() = default;
    };

    // ------------------------------------------------

    template<class Ty>
    struct TypedValue : Value {

        // ------------------------------------------------

        constexpr TypedValue() = default;
        constexpr ~TypedValue() override = default;

        // ------------------------------------------------

        template<class ...Args>
        constexpr TypedValue(Args&& ...args)
            : value(std::forward<Args>(args)...)
        {}

        // ------------------------------------------------

        Ty value{};

        // ------------------------------------------------

    };

    // ------------------------------------------------

public:

    // ------------------------------------------------

    template<class Ty>
        requires std::is_default_constructible_v<Ty>
    constexpr Ty& operator[](const State<Ty>& state) {
        if (!m_States.contains(&state)) {
            m_States.emplace(&state, std::make_unique<TypedValue<Ty>>());
        }
        return dynamic_cast<TypedValue<Ty>&>(*m_States[&state]).value;
    }
    
    template<class Ty>
        requires std::is_default_constructible_v<Ty>
    constexpr Ty& at(const State<Ty>& state) {
        if (!m_States.contains(&state)) throw std::out_of_range("Key did not exist");
        return dynamic_cast<TypedValue<Ty>&>(*m_States[&state]).value;
    }
    
    template<class Ty, class ...Args>
        requires std::is_constructible_v<Ty, Args&&...>
    constexpr Ty& emplace(const State<Ty>& state, Args&& ...args) {
        m_States.emplace(&state, std::make_unique<TypedValue<Ty>>(std::forward<Args>(args)...));
        return dynamic_cast<TypedValue<Ty>&>(*m_States[&state]).value;
    }
    
    // ------------------------------------------------

private:
    std::unordered_map<const void*, std::unique_ptr<Value>> m_States{};

    // ------------------------------------------------

};

constexpr State<int> hovering{};
constexpr State<int> pressed{};

// ------------------------------------------------

int main() {

    States states{};
    states.emplace(hovering, 1);
        
    auto& aoin = states[hovering];

    states.emplace(pressed, 10);

    auto& efgae = states.at(pressed);

    return 1;
}

// ------------------------------------------------
