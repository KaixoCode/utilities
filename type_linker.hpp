#pragma once
#include "utils.hpp"

namespace kaixo {
    template<class ...Types>
    struct type_group {
        constexpr static inline size_t count = sizeof...(Types);
    };

    template<class> struct tag;
    template<class ...Input>
    struct tag<type_group<Input...>> {
        friend auto magic_linker(tag<type_group<Input...>>);
    };

    template<class, class> struct magic_define;
    template<class ...Input, class ...Output>
    struct magic_define<type_group<Input...>, type_group<Output...>> {
        friend auto magic_linker(tag<type_group<Input...>>) { return type_group<Output...>{}; };
    };

    template<class> struct generate_definitions;
    template<class ...Input>
    struct generate_definitions<type_group<Input...>> {
        template<class Type> requires (sizeof(magic_define<type_group<Input...>, Type>), true)
        operator Type();
    };

    template<class Outputs, class Inputs>
    concept link_types = requires() {
        generate_definitions<Inputs>{}.operator Outputs();
    };

    template<class ...Input>
    using linked_types = decltype(magic_linker(tag<type_group<Input...>>{}));
}