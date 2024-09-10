#pragma once

namespace kaixo {
    template<class ...Tys>
    struct info {};

    template<class> struct tag;
    template<class ...Input>
    struct tag<info<Input...>> {
        friend auto magic_linker(tag<info<Input...>>);
    };

    template<class, class> struct magic_define;
    template<class ...Input, class ...Output>
    struct magic_define<info<Input...>, info<Output...>> {
        friend auto magic_linker(tag<info<Input...>>) { return info<Output...>{}; };
    };

    template<class> struct generate_definitions;
    template<class ...Input>
    struct generate_definitions<info<Input...>> {
        template<class Type> requires (sizeof(magic_define<info<Input...>, Type>), true)
        operator Type();
    };

    template<class Outputs, class Inputs>
    concept link_types = requires() {
        generate_definitions<Inputs>{}.operator Outputs();
    };

    template<class ...Input>
    using linked_types = decltype(magic_linker(tag<info<Input...>>{}));
}