#include <concepts>

#include <bit>
#include <iostream>

#include "utils.hpp"








#include <variant>

template<class ...Args, class ...Functors>
constexpr void visit(std::variant<Args...>& v, Functors&& ...functors) {
    const kaixo::overloaded _overloaded{ std::forward<Functors>(functors)... };
    using types = kaixo::pack<Args...>;
    kaixo::generate_template_switch<sizeof...(Args)>(
        [&]<std::size_t I> { 
            using type = typename types::template element<I>;
            _overloaded(std::get<type>(v));
        }
    )(v.index());
}



void myfun(int, double, float, char, long) {}

template<kaixo::type_concepts::aggregate Ty> // require aggregate type
    requires (kaixo::info<Ty>::members::size > 1 // With at least 2 members
           && kaixo::info<Ty>::members::are_arithmetic) // And all members are arithmetic
constexpr auto sum_struct(const Ty& val) {
    using type_info = kaixo::info<Ty>; // Sum all members
    return kaixo::sequence<type_info::members::size>([&]<std::size_t ...Is>{
        return ((val.*type_info::template member<Is>) + ...);
    });
}; 


int func(double&, int, long&&);

#include <vector>

struct serialized_data {
    std::size_t read_index = 0;
    std::vector<std::byte> bytes;

    constexpr void seek(std::size_t v) { read_index = v; }
    constexpr void seek_start() { seek(0); }

    template<kaixo::type_concepts::trivial Ty>
    constexpr void write(const Ty& val) {
        constexpr std::size_t size = sizeof(Ty);
        const auto start = reinterpret_cast<const std::byte*>(&val);
        bytes.insert(bytes.end(), start, start + size);
    }

    template<class Ty> requires requires(Ty ty) {
        { std::begin(ty) };
        { std::end(ty) };
        { std::size(ty) };
    } 
    constexpr void write(const Ty& val) {
        using type = std::decay_t<decltype(*std::begin(val))>;
        bytes.reserve(bytes.size() + std::size(val) * sizeof(type));
        write<std::size_t>(std::size(val));
        for (auto& v : val) write(v);
    }
    
    template<kaixo::type_concepts::aggregate Ty>
    constexpr void write(const Ty& val) {
        using info = kaixo::info<Ty>;
        constexpr std::size_t size = info::members::size;
        kaixo::sequence<size>([&]<std::size_t ...Is>{
            (write(val.*info::template member_ptr<Is>), ...);
        });
    }

    template<kaixo::type_concepts::trivial Ty>
    constexpr Ty read() {
        constexpr std::size_t size = sizeof(Ty);
        const auto start = bytes.data() + read_index;
        read_index += size;
        return *reinterpret_cast<const Ty*>(start);
    }

    template<class Ty> requires requires(Ty ty) {
        { std::begin(ty) };
        { std::end(ty) };
        { std::size(ty) };
    }
    constexpr Ty read() {
        using type = std::decay_t<decltype(*std::begin(std::declval<Ty>()))>;
        std::size_t size = read<std::size_t>();
        Ty val{};
        for (std::size_t i = 0; i < size; ++i)
            val.insert(std::end(val), read<type>());
        return val;
    }

    template<kaixo::type_concepts::aggregate Ty>
    constexpr Ty read() {
        using members = kaixo::info<Ty>::members;
        return members::for_each([&]<class ...Tys>{
            return Ty{ read<Tys>()... };
        });
    }

    template<class Ty>
    constexpr auto& operator<<(const Ty& val) {
        write(val);
        return *this;
    }

    template<class Ty>
    constexpr auto& operator>>(Ty& val) {
        val = read<Ty>();
        return *this;
    }
};

#include <map>
#include <typeindex>
#include <any>

struct runtime {

    struct member_info {
        std::string_view name;
        std::size_t offset;
        const std::type_info* type;
    };
    
    struct function_info {
        std::string_view name;
        void* value;
        const std::type_info* result;
        std::vector<const std::type_info*> arguments;
    };

    struct class_info {
        std::map<std::string_view, member_info> members;
        std::map<std::string_view, function_info> functions;
    };

    std::map<std::type_index, class_info> registered_classes;

    template<class Ty>
    inline auto add(std::string_view name, Ty val) {

        using type_info = kaixo::info<Ty>;
        
        if constexpr (type_info::is_member_object_pointer) {
            using object = type_info::object::type;
            using type = type_info::value_type::type;
            auto offset = ((std::size_t) & reinterpret_cast<char const volatile&>((((object*)0)->*val)));
            std::type_index _type = typeid(object);
            registered_classes[_type].members.emplace(name, member_info{
                .name = name,
                .offset = offset,
                .type = &typeid(type)
            });
        }
        else if constexpr (type_info::is_member_function_pointer) {
            using object = type_info::object::type;
            using signature = type_info::signature;
            std::type_index _type = typeid(object);
            registered_classes[_type].functions.emplace(name, function_info{
                .name = name,
                .value = std::bit_cast<void*>(val),
                .result = &typeid(typename signature::result::type),
                .arguments = kaixo::sequence<signature::arguments::size>([&]<std::size_t ...Is> {
                    std::vector<const std::type_info*> _arguments{};
                    _arguments.reserve(signature::arguments::size);
                    ((_arguments.push_back(&typeid(typename signature::arguments::template element<Is>))), ...);
                    return _arguments;
                })
            });
        }
        
        return val;
    }
} runtime;

template<auto Ty>
constexpr std::string_view get_type_or_fun_name(std::string_view name) {
    using info = kaixo::info_v<Ty>;
    if constexpr (info::is_member_function_pointer)
        return info::function_name;
    else if constexpr (info::is_member_object_pointer)
        return name.substr(name.find_last_of(":") + 1);
    else return "";
}

#define KAIXO_GLUE(a, b) KAIXO_GLUE_I(a, b)
#define KAIXO_GLUE_I(a, b) a##b
//#define register(x) auto KAIXO_GLUE(_registered_type_, __COUNTER__) = runtime.add(get_type_or_fun_name<&x>(#x), &x);

template<std::size_t I>
constexpr auto _register_type = kaixo::dud{};

constexpr std::size_t _type_register_start = __COUNTER__;

template<auto V, kaixo::string_literal Name>
struct type_storage {
    constexpr static auto value = V;
    constexpr static std::string_view name = Name.view();
};

struct type_info_storage_t {
    std::map<std::type_index, std::map<std::string_view, std::size_t>> storage;
} type_info_storage;

#define register_impl(x, c) template<>                         \
constexpr auto _register_type<c - _type_register_start - 1> =  \
type_storage<&x, #x>{};                                        \
std::size_t KAIXO_GLUE(_kaixo_init, c) = [](){                 \
    constexpr auto name = get_type_or_fun_name<&x>(#x);        \
    using object = kaixo::info<decltype(&x)>::object::type;    \
    type_info_storage.storage[typeid(object)][name] =          \
        c - _type_register_start - 1;                          \
    return c - _type_register_start - 1;                       \
}();
//#define register(x) register_impl(x, __COUNTER__)


template<class ...Args>
struct dynamic_call_result {
    std::tuple<Args...> args;
    void* object = nullptr;
    void* value = nullptr;
    const std::type_info* type = nullptr;

    template<class Ty>
    operator Ty () {
        if (typeid(Ty(void*, Args...)) == *type) {
            Ty(*fun)(void*, Args...) = static_cast<Ty(*)(void*, Args...)>(value);
            return std::apply(fun, std::tuple_cat(std::tuple{ object }, args));
        }
        else throw std::bad_cast();
    }
};

struct dynamic_access_result {
    void* object = nullptr;
    void* value = nullptr;
    const std::type_info* type = nullptr;

    template<class Ty>
    operator Ty&() {
        if (typeid(Ty) == *type)
            return *static_cast<Ty*>(value);
        else throw std::bad_cast();
    }

    template<class ...Args>
    dynamic_call_result<Args...> operator()(Args&& ...args) {
        if (typeid(void(void*, Args...)) == *type) {
            void(*fun)(void*, Args...) = static_cast<void(*)(void*, Args...)>(value);
            std::apply(fun, std::tuple{ object, args... });
            return {};
        } else return dynamic_call_result<Args...>{
            .args = std::tuple{ std::forward<Args>(args)... }, 
            .object = object,
            .value = value, 
            .type = type 
        };
    }
};

template<auto V>
dynamic_access_result handle_access(void* ptr) {
    using info = kaixo::info_v<V.value>;
    if constexpr (info::is_member_object_pointer) {
        using object = info::object::type;
        object* value = static_cast<object*>(ptr);
        return dynamic_access_result{
            .object = ptr,
            .value = &(value->*(V.value)),
            .type = &typeid(value->*(V.value))
        };
    }
    else if constexpr (info::is_member_function_pointer) {
        using object = info::object::type;
        using fptr = info::arguments::
            template prepend<void*>:: // Add void* as first argument
            template to_function<typename info::result::type>;
        constexpr auto _fun = []<class ...Args>(void* self, Args...args) 
            -> typename info::result::type {
            return ((*static_cast<object*>(self)).*(V.value))(args...);
        };
        fptr* _ptr = _fun;
        return dynamic_access_result{
            .object = ptr,
            .value = _ptr,
            .type = &typeid(fptr)
        };
    }
    return {};
}

template<class Ty = int>
auto access(void* ptr, const std::type_info& type, std::string_view name) {
    std::size_t index = type_info_storage.storage[type][name];

    return kaixo::generate_template_switch<32ull>([&]<std::size_t I> {
        using _type = std::decay_t<decltype(_register_type<I>)>;
        if constexpr (!std::is_same_v<_type, kaixo::dud>) {
            return handle_access<_register_type<I>>(ptr);
        }
        else return dynamic_access_result{};
    })(index);
}

struct dynamic {
    void* data;
    const std::type_info* type;
    void(*deleter)(void*);

    template<class Ty>
    dynamic(Ty* data)
        : deleter(&kaixo::RTTI_Ftable<Ty>::deleter),
        data(static_cast<void*>(data)), type(&typeid(Ty)) {}

    dynamic(const dynamic&) = delete;
    dynamic(dynamic&& other) : data(other.data), type(other.type), deleter(other.deleter) {
        other.data = nullptr, other.type = nullptr, other.deleter = nullptr;
    };

    template<class Ty>
    dynamic& operator=(Ty* ptr) {
        if (data) deleter(data);
        deleter = kaixo::RTTI_Ftable<Ty>::deleter;
        data = static_cast<void*>(ptr);
        type = &typeid(Ty);
        return *this;
    }

    dynamic& operator=(dynamic&& other) {
        data = other.data, type = other.type, deleter = other.deleter;
        other.data = nullptr, other.type = nullptr, other.deleter = nullptr;
        return *this;
    }

    template<class Ty>
    auto operator[](Ty v) {
        return access<>(data, *type, v);
    }

    ~dynamic() { if (deleter) deleter(data); }
};



struct MyClass {
    int value = 10;

    void add(int val) { value += val; }
};

register(MyClass, value);
register(MyClass, add);


struct N {
    std::string name;
    std::vector<int> values;
    std::size_t length;
};

register(N, name);
register(N, values);
register(N, length);




struct Struct {
    int f = 3;
    double a = 1;
    char c = 6;
    double e = 2;
    int b = 4;
    long d = 5;
};

class Base {

};

class Derived : public Base {

};

template<class ...Args>
struct forwarding_tuple : std::tuple<Args&&...> {
    using types = kaixo::pack<Args&&...>;
    using std::tuple<Args&&...>::tuple;

    template<std::size_t I>
    constexpr typename types::template element<I> get() {
        using result = typename types::template element<I>;
        if constexpr (kaixo::info<result>::is_lvalue_reference)
            return std::get<I>(*this);
        else if constexpr (kaixo::info<result>::is_rvalue_reference)
            return std::move(std::get<I>(*this));
    }
};

template<class ...Args>
forwarding_tuple(Args&&...)->forwarding_tuple<Args...>;

//template<std::size_t I, class ...Args>
//constexpr decltype(auto) get(Args&&...args) {
//    forwarding_tuple _args{ std::forward<Args>(args)... };
//
//    return _args.get<I>();
//}

template<class ...Args>
struct template_pack : std::tuple<std::add_lvalue_reference_t<Args>...>, kaixo::pack<Args...> {
    using _parent = std::tuple<std::add_lvalue_reference_t<Args>...>;
    using _parent::_parent;

    template<std::size_t I>
    constexpr decltype(auto) get() const {
        using result = template_pack::element_info<I>;
        if constexpr (result::is_lvalue_reference)
            return std::get<I>(*this);
        else return std::move(std::get<I>(*this));
    }
};

template<class Ty>
struct forwarder {
    constexpr forwarder(Ty val) {}
};

template<class Ty>
forwarder(Ty)->forwarder<Ty>;

template<std::size_t I, class ...Args>
constexpr decltype(auto) g(Args&&...args) {
    const template_pack<Args...> _args{ args... };

    return _args.get<I>();
}


using namespace kaixo;
using namespace kaixo::fold;
using namespace kaixo::type_concepts;
using namespace kaixo::type_traits;

#include "linq.hpp"


int main() {

    {
        std::vector<int> n1{ 1, 2, 3, 4 };
        std::vector<int> n2{ 1, 2, 3, 4 };

        auto res1 = from x in n1, y in n2 where x < y select (x * x, y + 2);

        for (auto [x, y] : res1) {
            std::cout << "(" << x << ", " << y << ")\n";
        }


    }

    int ggggre = 1;
    auto& resfaea = g<1>(1, ggggre, 3, 4, 5);

    constexpr auto res = array_to_pack<info<Struct>::members::indices_filter<is_integral>>::fold(Args + ___);
    
    constexpr auto aeaoine = function_name<next_multiple<int>>;
    constexpr auto aione = 10.;
    constexpr auto aoine = value_name<aione>;

    constexpr auto res2 = sequence<5, 6>(Args || ___);

    //constexpr auto res3 = sequence<5, 10>([]<auto ...Args>{ return (Args + ...); });

    info<N>::member_info<0>::name;


    N a{ "hello world", { 1, 2, 5, 1, 24, 2 }, 1381ull };
    
    serialized_data data;
    data << a;
    
    N m;
    data >> m;

    bool same = a.name   == m.name &&
                a.values == m.values &&
                a.length == m.length;


    info<Struct>::members;

    using namespace kaixo;
    using func_sig = info_v<&func>;

    auto n = func_sig::function_name;
    auto r = func_sig::result::type_name;

    auto a1 = func_sig::arguments::element_info<0>::type_name;
    auto a2 = func_sig::arguments::element_info<1>::type_name;
    auto a3 = func_sig::arguments::element_info<2>::type_name;

    info<Struct>::members::size;

    static_assert(info<Struct>::offset<0> == offsetof(Struct, f));
    static_assert(info<Struct>::offset<1> == offsetof(Struct, a));
    static_assert(info<Struct>::offset<2> == offsetof(Struct, c));
    static_assert(info<Struct>::offset<3> == offsetof(Struct, e));
    static_assert(info<Struct>::offset<4> == offsetof(Struct, b));
    static_assert(info<Struct>::offset<5> == offsetof(Struct, d));



    Struct val;
    auto r1 = val.*info<Struct>::member_ptr<0>;
    auto r2 = val.*info<Struct>::member_ptr<1>;
    auto r3 = val.*info<Struct>::member_ptr<2>;
    auto r4 = val.*info<Struct>::member_ptr<3>;
    auto r5 = val.*info<Struct>::member_ptr<4>;
    auto r6 = val.*info<Struct>::member_ptr<5>;

    enum Fruit {
        Apple, 
        Banana, 
        Pear, 
        Orange,
        Size
    };


    constexpr Fruit my_fruit = Fruit::Apple;

    constexpr auto aine = info<Struct>::type_name;
    constexpr auto name = kaixo::enum_to_string(my_fruit);

    std::tuple<int, double, float, long> tuple;
    


    constexpr auto aion = kaixo::invocable_no_conversions<decltype([](double) {}), double&>;

    

    kaixo::tuple_for(tuple, 
        [](int v) { std::cout << "int\n"; },
        [](double v) { std::cout << "double\n"; },
        [](std::integral auto v) { std::cout << "integral\n"; },
        [](std::floating_point auto v) { std::cout << "floating\n"; }
    );

    return 0;
}