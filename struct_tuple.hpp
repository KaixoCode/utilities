#pragma once
#include "utils.hpp"
#include <tuple>
#include <cmath>

namespace kaixo {
    template<class Type>
    struct wrapper {
        using type = Type;
    };

    template<class Type, size_t Index>
    struct member {
        // Defines a friend function with 'auto' return type
        friend auto magic_friend(member<Type, Index>);
    };

    template<class Type, class MemberType, size_t Index>
    struct define {
        // Creates the implementation of the friend function, and sets the 'auto' return type
        // to 'wrapper<MemberType>', we've now created a function with the Type and Index as template
        // arguments, and the return type is the type of the member associated with that Type at that Index.
        friend auto magic_friend(member<Type, Index>) { return wrapper<MemberType>{}; };
    };

    // Sneaky class that can be implicitly converted to any type.
    template<class Type, size_t Index>
    struct all {
        // Whenever a conversion operator is instatiated, we also instantiate a 'define'
        // which will create the implementation of the friend function, giving the function
        // the return type of this implicit conversion.
        template<class MemberType, int = sizeof(define<Type, MemberType, Index>)>
        operator MemberType();
    };

    // Concept that tests if Type is instantiable with sizeof...(Ns) arguments.
    template<class Type, size_t... Ns>
    concept instantiable = requires() {
        new Type{ all<Type, Ns>{}... };
    };

    // These functions test if type Type has sizeof...(Ns) fields, using a concept
    // that tries to instatiate the type using that many arguments.
    template<class Type, size_t... Ns> requires instantiable<Type, Ns...>
    constexpr size_t has_n_fields(std::index_sequence<Ns...>) { return sizeof...(Ns); }
    template<class, size_t...>
    constexpr size_t has_n_fields(...) { return 0; }

    // Find the field count of the Type using recursive inheritance with a base case of 0.
    template<class Type, size_t N, size_t Count = has_n_fields<Type>(std::make_index_sequence<N>{}) >
    struct find_field_count : std::conditional_t<Count == 0, find_field_count<Type, N - 1>, std::integral_constant<size_t, Count>> {};
    template<class Type, size_t N>
    struct find_field_count<Type, 0, N> : std::integral_constant<size_t, 0> {};

    // Get the type of the member at Index for Type using the magic friend function.
    template<class Type, size_t Index>
    using member_type = typename decltype(magic_friend(member<Type, Index>{}))::type;

    // Get the field types using an index sequence.
    template<typename Type, size_t... Ns>
    constexpr auto get_field_types(std::index_sequence<Ns...>) -> std::tuple<member_type<Type, Ns>...> {};

    // Struct info, like amount of fields, and their types.
    template<typename Type> requires std::is_aggregate_v<Type>
    struct struct_info {
        constexpr static size_t fields = find_field_count<Type, sizeof(Type)>::value;
        using field_types = decltype(get_field_types<Type>(std::make_index_sequence<fields>{}));
    };

    // Calculates the byte offset of the sizeof...(Ns)th member in Type. Only tested in MSVC, and
    // probably still wrong in many ways...
    template<class Type, size_t ...Ns>
    constexpr inline size_t get_offset(std::index_sequence<Ns...>) {
        size_t _offset = 0;
        ((_offset = alignof(member_type<Type, Ns>) * std::floor(1 +
            (_offset + alignof(member_type<Type, Ns>) - 1) / (float)alignof(member_type<Type, Ns>))), ...);
        return _offset - alignof(member_type<Type, sizeof...(Ns) - 1>);
    }

    // Get the Nth member of the struct Type
    template<size_t N, typename Type> requires std::is_aggregate_v<Type>
    inline auto& get(Type& s) {
        return *reinterpret_cast<member_type<Type, N>*>((reinterpret_cast<char8_t*>(
            std::addressof(s)) + get_offset<Type>(std::make_index_sequence<N + 1>{})));
    }

    // Convert Type to tuple using index sequence and the get<N>(Type) method.
    template<class Type, size_t ...Ns>
    inline auto as_tuple_seq(Type& s, std::index_sequence<Ns...>) {
        return struct_info<Type>::field_types(get<Ns>(s)...);
    }

    // Convert Type to a tuple.
    template<typename Type>
    inline auto as_tuple(Type& s) {
        return as_tuple_seq(s, std::make_index_sequence<struct_info<Type>::fields>{});
    }
}