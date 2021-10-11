#pragma once
namespace kaixo {
    template<size_t N>
    struct incrementer {
        friend constexpr auto magic_incr(incrementer<N>);
    };

    template<size_t N, size_t V>
    struct incrementer_def {
        friend constexpr auto magic_incr(incrementer<N>) { return V; };
    };

    template<size_t N, class>
    concept checker_c = requires() {
        magic_incr(incrementer<N>{});
    };

    // This checker first checks if the magic_incr friend function has been defined for N
    // in any case, because everything in the boolean expression needs to be valid code
    // the incrementer_def is evaluated, creating the definition for the magic_incr friend
    // function anyway. So the next time this checker is evaluated for N, it will be valid.
    // we need a unique class T to reevaluate N each time.
    template<size_t N, class T>
    struct checker : std::bool_constant<checker_c<N, T> && (sizeof(incrementer_def<N, N + 1>), true)> {};

    template<size_t, class>
    struct incr;

    template<size_t V, class L> requires (!checker<V, L>::value)
    struct incr<V, L> {
        constexpr static inline size_t get() { return V; }
    };

    template<size_t V, class L> requires (checker<V, L>::value)
    struct incr<V, L> : incr<V + 1, L> {
        using incr<V + 1, L>::get;
    };

    // Use a decltype lambda to get a unique type for each usage.
#define incr incr<0, decltype([](){})>
}