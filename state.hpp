
template<class Ty, class Unique>
struct State {
    constexpr static const std::type_info& _id = typeid(std::type_identity<Unique>);
    using type = Ty;
};

template<class Ty> struct IsStateImpl : std::false_type {};
template<class A, class B> struct IsStateImpl<State<A, B>> : std::true_type {};
template<class Ty> concept IsState = IsStateImpl<Ty>::value;

struct StateValueBase {
    virtual ~StateValueBase() = default;
};

template<class Ty>
struct StateValue : StateValueBase {
    template<class ...Args>
    constexpr StateValue(Args&&...args)
        : value(std::forward<Args>(args)...) {}
    Ty value{};
};

struct States {
    template<class Ty>
    struct Sentinel {
        std::unique_ptr<StateValueBase>& value = nullptr;

        constexpr Sentinel(std::unique_ptr<StateValueBase>& v) : value(v) {}
        constexpr Sentinel(const Sentinel&) = delete;
        constexpr Sentinel(Sentinel&&) = delete;
        constexpr ~Sentinel() = default;

        constexpr Ty& get() const {
            if (value == nullptr) throw std::exception("Object was not initialized yet.");
            return reinterpret_cast<StateValue<Ty>*>(value.get())->value;
        }

        constexpr operator Ty& () const { return get(); }

        template<class ...Args>
        constexpr Ty& assign(Args&& ...args) const {
            value = std::make_unique<StateValue<Ty>>(std::forward<Args>(args)...);
            return get();
        }

        template<class Arg>
        constexpr Ty& operator=(Arg&& arg) const {
            return assign(std::forward<Arg>(arg));
        }

        constexpr Ty& operator=(Ty&& val) const { return assign(std::move(val)); }
        constexpr Ty& operator=(const Ty& val) const { return assign(val); }
    };

    template<IsState State>
    [[no_discard]] constexpr decltype(auto) get(State) {
        using value_type = State::type;
        using state_type = StateValue<value_type>;
        auto& value = m_States[State::_id];
        if constexpr (std::is_default_constructible_v<value_type>) {
            if (value == nullptr) emplace(State{});
            auto& v = reinterpret_cast<state_type*>(value.get())->value;
            return v;
        }
        else {
            return Sentinel<value_type>{ value };
        }
    }

    template<IsState State>
    [[no_discard]] constexpr decltype(auto) operator[](State) {
        return get(State{});
    }

    template<IsState State, class ...Args>
    constexpr auto& emplace(State, Args&& ...args) {
        using value_type = State::type;
        using state_type = StateValue<value_type>;
        auto& value = m_States[State::_id];
        value = std::make_unique<state_type>(std::forward<Args>(args)...);
        return reinterpret_cast<state_type*>(value.get())->value;
    }

    template<IsState State>
    constexpr bool contains(State) const {
        return m_States.contains(typeid(State));
    }

private:
    std::unordered_map<std::type_index, std::unique_ptr<StateValueBase>> m_States{};
};

struct Position {
    float x{};
    float y{};
    float width{};
    float height{};
};

constexpr State<int, struct Hovering> hovering{};
constexpr State<int, struct Pressed> pressed{};
constexpr State<Position, Position> position{};

struct Object {
    States states;

    Position& pos() { return states[position]; }
};