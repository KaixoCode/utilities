#include <any>
#include "utils.hpp"

namespace kaixo {
    template<size_t V>
    struct number {
        constexpr static inline size_t value = V;
    };

    // Type linker, uses magic auto return friend function trick
    template<class ...Types>
    struct type_group {
        constexpr static inline size_t count = sizeof...(Types);
        using types = std::tuple<Types...>;
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

    // Compiletime counter
    template<size_t N, class C>
    struct incrementer {
        friend constexpr auto magic_incr(incrementer<N, C>);
    };

    template<size_t N, class C, size_t V>
    struct incrementer_def {
        friend constexpr auto magic_incr(incrementer<N, C>) { return V; };
    };

    template<size_t N, class C, class>
    concept checker_c = requires() {
        magic_incr(incrementer<N, C>{});
    };

    // This checker first checks if the magic_incr friend function has been defined for N
    // in any case, because everything in the boolean expression needs to be valid code
    // the incrementer_def is evaluated, creating the definition for the magic_incr friend
    // function anyway. So the next time this checker is evaluated for N, it will be valid.
    // we need a unique class T to reevaluate N each time.
    template<size_t N, class C, class T>
    struct checker : std::bool_constant<checker_c<N, C, T> && (sizeof(incrementer_def<N, C, N + 1>), true)> {};

    template<size_t, class, auto>
    struct incr;

    template<size_t V, class C, auto L> requires (!checker<V, C, decltype(L)>::value)
        struct incr<V, C, L> {
        constexpr static size_t get() { return V; }
    };

    template<size_t V, class C, auto L> requires (checker<V, C, decltype(L)>::value)
        struct incr<V, C, L> : incr<V + 1, C, L> {
        using incr<V + 1, C, L>::get;
    };

    // Typed any
    template<auto Me = [] {} > // Unique lambda type to distinguish multiple instances
    struct Any : std::any {
    private:
        using MeType = decltype(Me);
        struct thing_dud {};
    public:
        constexpr Any() {}

        // Indirection is necessary due to how the compiler instantiates templates
        template<class Ty, class Cnt, class ...Tys>
        using my_type = linked_types<Ty, Cnt>;

        template<class Ty, // Type we're setting it to 
                 auto Cur = [] {}, // Unique lambda to distinguish from previous sets
                 class...Tys> // Funky unused type group
            requires (link_types<type_group<Ty>,  // Link to the type
                      type_group<
                          thing_dud, // Thing dud contains all our own instance info
                          number<incr<0, MeType, Cur>::get()>>>) // Compiletime counter
        constexpr auto set(Ty val) {
            return this->emplace<Ty>(val); // Set any value
        }

        template<class Ty = thing_dud, // Our own type info
                 auto Cur = [] {}, // Unique lambda to distinguish from previous gets
                 class ...Tys > // Funky unused type group for more indirection
        constexpr decltype(auto) get() {
            using type = head_t< // Get first type from type group
                my_type<Ty, // Our type info
                        // Compiletime counter - 1, should equal the one in previous set call
                        number<incr<0, MeType, Cur>::get() - 1>, 
                        Tys...>::types>; 
            return std::any_cast<type&>(*this); // Any cast to the retrieved type.
        }
    };
}