#pragma once
namespace kaixo {
    template<size_t N, size_t C>
    struct incrementer {
        friend constexpr auto magic_incr(incrementer<N, C>);
    };

    template<size_t N, size_t C, size_t V>
    struct incrementer_def {
        friend constexpr auto magic_incr(incrementer<N, C>) { return V; };
    };

    template<size_t N, size_t C, class>
    concept checker_c = requires() {
        magic_incr(incrementer<N, C>{});
    };

    // This checker first checks if the magic_incr friend function has been defined for N
    // in any case, because everything in the boolean expression needs to be valid code
    // the incrementer_def is evaluated, creating the definition for the magic_incr friend
    // function anyway. So the next time this checker is evaluated for N, it will be valid.
    // we need a unique class T to reevaluate N each time.
    template<size_t N, size_t C, class T>
    struct checker : std::bool_constant<checker_c<N, C, T> && (sizeof(incrementer_def<N, C, N + 1>), true)> {};

    template<size_t, size_t, auto>
    struct incr;

    template<size_t V, size_t C, auto L> requires (!checker<V, C, decltype(L)>::value)
    struct incr<V, C, L> {
        constexpr static size_t get() { return V; }
    };

    template<size_t V, size_t C, auto L> requires (checker<V, C, decltype(L)>::value)
    struct incr<V, C, L> : incr<V + 1, C, L> {
        using incr<V + 1, C, L>::get;
    };

    // Use a decltype lambda to get a unique type for each usage.
#define counter(x) incr<0, x, []{}>
}