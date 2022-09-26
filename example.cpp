#include <iostream>
#include <string_view>
#include <array>
#include <vector>
#include <fstream>
#include <string>
#include <ranges>
#include <algorithm>
#include <charconv>

#include <functional>


template<class Ty> constexpr bool _is_char_t = false;
template<> constexpr bool _is_char_t<char> = true;
template<> constexpr bool _is_char_t<signed char> = true;
template<> constexpr bool _is_char_t<unsigned char> = true;
template<> constexpr bool _is_char_t<wchar_t> = true;
template<> constexpr bool _is_char_t<char8_t> = true;
template<> constexpr bool _is_char_t<char16_t> = true;
template<> constexpr bool _is_char_t<char32_t> = true;

template<class C, class V>
struct split_view : std::ranges::view_interface<split_view<C, V>> {
    using range = C;
    using _c_value_type = std::ranges::range_value_t<range>;
    using _c_iterator = std::ranges::iterator_t<range>;

    constexpr static bool _is_contiguous = std::contiguous_iterator<_c_iterator>;
    constexpr static bool _is_string = _is_char_t<_c_value_type>;
    constexpr static bool _can_string_view = _is_contiguous && _is_string;
    constexpr static bool _is_range = std::ranges::range<V>;

    using value_type = std::conditional_t<_can_string_view, std::string_view, std::ranges::subrange<_c_iterator>>;

    constexpr split_view(C range, V value)
        : _range(std::move(range)), _value(std::move(value)) {};

    struct iterator {
        const split_view* _self;
        _c_iterator _it;
        _c_iterator _last;

        constexpr iterator(const split_view* self = nullptr)
            : _self(self) {
            if (_self) {
                _last = std::ranges::begin(self->_range);
                const auto _end = std::ranges::end(_self->_range);
                if (_last == _end) _self = nullptr;
                else {
                    if constexpr (_is_range) {
                        const auto _vstart = std::ranges::begin(_self->_value);
                        const auto _vend = std::ranges::end(_self->_value);
                        while (_last != _end && std::search(_last, _end, _vstart, _vend) == _last) ++_last;
                        _it = _last;
                        do ++_last;
                        while (_last != _end && std::search(_last, _end, _vstart, _vend) != _last);
                    }
                    else {
                        while (_last != _end && *_last == _self->_value) ++_last;
                        _it = _last;
                        do ++_last;
                        while (_last != _end && *_last != _self->_value);
                    }
                }
            }
        }

        constexpr iterator& operator++() {
            seek_valid();
            return *this;
        }

        constexpr value_type operator*() {
            return { _it, _last };
        }

        constexpr bool operator==(const iterator& o) const {
            return o._self == nullptr && _self == nullptr;
        }

        constexpr void seek_valid() {
            const auto _end = std::ranges::end(_self->_range);
            if (_last == _end) _self = nullptr;
            else {
                if constexpr (_is_range) {
                    const auto _vstart = std::ranges::begin(_self->_value);
                    const auto _vend = std::ranges::end(_self->_value);
                    _last += std::ranges::size(_self->_value);
                    _it = _last;
                    do ++_last;
                    while (_last != _end && std::search(_last, _end, _vstart, _vend) != _last);
                } else {
                    ++_last;
                    _it = _last;
                    do ++_last;
                    while (_last != _end && *_last != _self->_value);
                }
            }
        }
    };

    constexpr iterator begin() const { return iterator{ this }; }
    constexpr iterator end() const { return iterator{ }; }

    C _range;
    V _value;
};

template<class C, std::ranges::range V>
split_view(C&&, V&&)->split_view<std::views::all_t<C>, std::views::all_t<V>>;

template<class C, class V>
split_view(C&&, V&&)->split_view<std::views::all_t<C>, V>;




#include <set>
#include <deque>



template<class Impl> struct infix {
    constexpr static Impl impl{};
    consteval infix(Impl) {}

    template<class A> struct temp { A&& a; };

    template<class A>
    friend constexpr temp<A> operator<(A&& a, const infix&) {
        return { std::forward<A>(a) };
    }

    template<class A, class B >
    friend constexpr auto operator>(temp<A>&& a, B&& b) {
        return impl(std::forward<A>(a.a), std::forward<B>(b));
    }
};

constexpr infix contains = []<class A, class B>(A&& self, B&& elem) -> bool {
    return std::ranges::find(self, elem) != std::ranges::end(self);
};

// Map
constexpr infix to = []<class A, class B>(A&& a, B&& b) {
    return std::pair{ std::forward<A>(a), std::forward<B>(b) };
};

#include <variant>
#include <map>
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
    template<class Ty> struct type : std::bool_constant<attribute_tag<Ty, Tag>> {};
};

#define AS_STRING(x) #x
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

struct JsonProperties {
    std::array<std::pair<std::string_view, std::string_view>, 2> props;
    constexpr std::string_view operator[](std::string_view name) {
        for (auto& p : props)
            if (p.first == name) return p.second;
        return "";
    }
};

constexpr JsonProperties parseJson(std::string_view json) {
    return { .props{ std::pair{ "age", "23" }, std::pair{ "name", "John" } } };
}

template<class Ty>
constexpr Ty parseAs(std::string_view val) {
    if constexpr (std::is_integral_v<Ty> || std::is_floating_point_v<Ty>) {
       // Ty value{};
        //auto res = std::from_chars(val.data(), val.data() + val.size(), value);
        return 23;
    } else if constexpr (std::is_same_v<Ty, std::string_view>) {
        return val;
    }
}

template<kaixo::string_literal Json> 
struct constexpr_json {
    template<class Ty>
    consteval static Ty as() {
        auto json = parseJson(Json.view());
        using members = struct_members_t<Ty>;
        
        auto try_one = [&]<std::size_t I>() {
            using type = members::template element<I>::type;
            if constexpr (attribute<type>) return type{};
            else {
                using attr = members::template element<I - 1>::type;
                return parseAs<type>(json[attr::name]);
            } 
        };

        return sequence<members::size>([&]<std::size_t ...I> {
            return Ty{ try_one.operator()<I>()... };
        });
    }
};


struct Settings {
    [[JsonPropertyName("age")]]
    int age;
    [[JsonPropertyName("name")]]
    std::string_view name;
};

constexpr auto settings = constexpr_json<""
#include "bin/data.json"
>::as<Settings>();

int main() {

    constexpr auto age = settings.age;
    constexpr auto name = settings.name;

    return 0;
}
