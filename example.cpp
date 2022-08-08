#include <iostream>
#include <variant>
#include <map>
#include <vector>
#include <string>

#include "type_utils.hpp"
using namespace kaixo;

template<class Ty, class ...Tys>
concept OneOf = (std::same_as<Ty, Tys> || ...);

template<class Ty> concept attribute = requires(Ty) { typename Ty::attribute_tag; };
template<class Ty> struct is_attribute_impl { constexpr static bool value = attribute<Ty>; };

constexpr auto is_attribute = type_trait<is_attribute_impl>{};

template<class Ty, class ...Attributes>
struct member_attributes {
    using attributes = info<Attributes...>;
    using type = Ty;
};

template<class ...Tys>
struct members_with_attributes : info<Tys...> {
    template<std::size_t I>
    using member = info<Tys...>::template element<I>::type;
};

template<class Ty>
struct attributes_from_struct {
    using _members = struct_members_t<Ty>;
    constexpr static auto _indices = _members::template indices_filter<not is_attribute>;
    
    template<std::size_t ...Is> struct helper1 {
        using type = info<typename _members::template element<Is>::type...>;
    };
    template<class> struct helper2;
    template<std::size_t ...Is> struct helper2<std::index_sequence<Is...>> {
        template<std::size_t I>
        using _one = typename array_to_pack_t<
            generate_indices_v<I == 0 ? 0 : _indices.at(I - 1) + 1, _indices.at(I)>, helper1>::type
            ::template prepend<typename _members::template element<_indices.at(I)>::type>
            ::template as<member_attributes>;
        using type = members_with_attributes<_one<Is>...>;
    };

    using type = helper2<std::make_index_sequence<_indices.size()>>::type;
};

template<class Ty>
using attributes_from_struct_t = typename attributes_from_struct<Ty>::type;

template<class Ty, class Tag> concept attribute_tag = requires(Ty) {
    std::same_as<typename Ty::attribute_tag, Tag>;
};

template<class Tag> struct is_attribute_tag {
    template<class Ty> struct type : std::bool_constant<attribute_tag<Ty, Tag>>{};
};

#define ATTRIBUTE(x) no_unique_address]] x KAIXO_MERGE(_attribute, __COUNTER__){}; [[

template<class Ty> struct member_offsets {
    constexpr static auto value = [] {
        using members = struct_members_t<decay_t<Ty>>;
        std::array<std::size_t, members::size> _offsets{};
        indexed_for<members::size>([&]<std::size_t I>{
            if constexpr (I == 0) _offsets[0] = 0;
            else if constexpr (I != 0) {
                using prev = typename members::template element<I - 1>::type;
                using curr = typename members::template element<I>::type;

                std::size_t off = _offsets[I - 1] + sizeof_v<prev>;
                std::size_t align = alignof_v<curr>;

                _offsets[I] = next_multiple(off, align);
            }
        });
        return _offsets;
    }();
};

template<aggregate Ty> constexpr auto get_member_ptrs() {
    using members = struct_members_t<decay_t<Ty>>;
    using member_pointers = members::template to_member_pointer<decay_t<Ty>>;
    constexpr auto _indices = members::template indices_filter<not is_attribute>;
    constexpr auto offsets = member_offsets<decay_t<Ty>>::value;
    return iterate<_indices>([&]<std::size_t...Is>{
        return std::tuple{
            std::bit_cast<typename member_pointers::template element<Is>::type>
                (static_cast<std::uint32_t>(offsets[Is]))...
        };
    });
}

struct json_property_tag {};
template<string_literal Name> struct json_property {
    using attribute_tag = json_property_tag;
    constexpr static std::string_view name = Name.view();
};

constexpr type_trait<is_attribute_tag<json_property_tag>::type> is_json_property{};
#define JsonPropertyName(NAME) ATTRIBUTE(json_property<NAME>)

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

private:
    using JsonValue = std::variant<Floating, Integral, Unsigned, String, Boolean, Array, Object, Null>;
    struct Type { enum { Floating, Integral, Unsigned, String, Boolean, Array, Object, Null }; };
    JsonValue m_Value;

    template<class Ty> struct Alias { using Type = Ty; };
    template<std::signed_integral Ty> struct Alias<Ty> { using Type = Integral; };
    template<std::unsigned_integral Ty> struct Alias<Ty> { using Type = Unsigned; };

public:
    template<class Ty = Null>
    Json(const Ty& ty = {}) : m_Value(static_cast<Alias<Ty>::Type>(ty)) {}

    template<aggregate Ty>
    Json(const Ty& value) : m_Value(Object{}) {
        static const auto member_pointers = get_member_ptrs<decay_t<Ty>>();
        using members = attributes_from_struct_t<decay_t<Ty>>;

        indexed_for<members::size>([&]<std::size_t I>{
            using member = members::template member<I>;
            using type = member::type;
            using attributes = member::attributes;
            using find_json_property = attributes::template filter<is_json_property>;
            if constexpr (find_json_property::size != 0) {
                constexpr std::string_view name = find_json_property::template element<0>::type::name;
                (*this)[name] = (value.*std::get<I>(member_pointers));
            }
        });
    }

    template<class Ty> Ty get() const { return static_cast<Ty>(std::get<typename Alias<Ty>::Type>(m_Value)); }
    template<class Ty> Ty& ref() { return std::get<Ty>(m_Value); }
    template<class Ty> const Ty& ref() const { return std::get<Ty>(m_Value); }
    template<aggregate Ty> Ty get() { 
        static const auto member_pointers = get_member_ptrs<decay_t<Ty>>();
        using members = attributes_from_struct_t<decay_t<Ty>>;

        Ty _result{};
        indexed_for<members::size>([&]<std::size_t I>{
            using member = members::template member<I>;
            using type = member::type;
            using attributes = member::attributes;
            using find_json_property = attributes::template filter<is_json_property>;
            if constexpr (find_json_property::size != 0) {
                constexpr std::string_view name = find_json_property::template element<0>::type::name;
                (_result.*std::get<I>(member_pointers)) = (*this)[name].get<type>();
            }
        });
        return _result;
    }

    Json& operator[](std::string_view index)
    {
        if (m_Value.index() == Type::Null) m_Value = Object{};
        else if (m_Value.index() != Type::Object) throw std::exception("Not an object.");
        auto _it = ref<Object>().find(index);
        if (_it == ref<Object>().end()) return ref<Object>()[std::string{ index }];
        else return _it->second;
    }
};


struct Person {
    [[JsonPropertyName("age")]]
    [[JsonPropertyName("age")]]
    int age;

    [[JsonPropertyName("name")]]
    std::string name;

    int carrot = 0;
    std::vector<int> data{};

    [[JsonPropertyName("height")]]
    double height;
};

struct Struct1 {
    [[JsonPropertyName("value")]]
    int value;

    [[JsonPropertyName("carrot")]]
    double carrot;
};

struct Struct2 {
    [[JsonPropertyName("nested")]]
    Struct1 nested;

    [[JsonPropertyName("value")]]
    std::string value;
};


struct meta_tag {};
template<string_literal Name> struct MetaName {
    using attribute_tag = meta_tag;
    constexpr static auto name = Name.view();
};
constexpr type_trait<is_attribute_tag<meta_tag>::type> is_meta{};
#define Meta(NAME) ATTRIBUTE(MetaName<NAME>)


template<string_literal Name, class Ty>
decltype(auto) get(Ty&& value) {
    static const auto member_pointers = get_member_ptrs<decay_t<Ty>>();
    using members = attributes_from_struct_t<decay_t<Ty>>;

    auto _lambda = [&]<std::size_t I, class Lambda>(Lambda& lambda) -> decltype(auto) {
        if constexpr (I == members::size) return;
        else {
            using member = members::template member<I>;
            using type = member::type;
            using attributes = member::attributes;
            using find_meta = attributes::template filter<is_meta>;
            if constexpr (find_meta::size != 0)
                if constexpr (find_meta::template element<0>::type::name == Name)
                    return (std::forward<Ty>(value).*std::get<I>(member_pointers));
                else return lambda.operator()<I + 1> (lambda);
            else return lambda.operator()<I + 1>(lambda);
        }
    };
    return _lambda.operator()<0>(_lambda);
}

#define Property(NAME) ]] \
decltype(auto) get_##NAME() { return get<#NAME>(*this); } \
template<class Ty> void set_##NAME(Ty&& val) { get<#NAME>(*this) = std::forward<Ty>(val); } \
[[Meta(#NAME)

struct MyStruct {
    [[Property(carrot)]]
    float value;
};

int main() {
    MyStruct _val{
        .value = 1
    };

    auto& carrot = _val.get_carrot();
    _val.set_carrot(3);

    return 0;
}