#include <memory>
#include <concepts>
#include <cstddef>
#include <array>
#include <utility>
#include <type_traits>

// This template magic hides the lambda type behind an incrementing index
// and uses a function lookup table to find the correct lambda at runtime, 
// dynamic cast to it, and then call it with the arguments.
// The type of the lambda itself can then be erased from the class it is stored inside of.
namespace kaixo {
    namespace detail {
        // Type linker, to link the lambda type to its given index. Makes
        // use of the auto return type friend function trick for stateful metaprogramming.
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

        // Constexpr incrementer, the thing that gives each lambda class
        // its unique index. Also uses the auto return type friend function trick.
        template<size_t N, class C> struct incrementer {
            friend constexpr auto magic_incr(incrementer<N, C>);
        };

        template<size_t N, class C, size_t V> struct incrementer_def {
            friend constexpr auto magic_incr(incrementer<N, C>) { return V; };
        };

        template<size_t N, class C, class> concept checker_c = requires() {
            magic_incr(incrementer<N, C>{});
        };

        template<size_t N, class C, class T>
        struct checker : std::bool_constant<
            checker_c<N, C, T> && // Checks value at index N
            // vvv Makes sure to define N for the next call.
            (sizeof(incrementer_def<N, C, N + 1>), true)> {}; 

        // vvv Same as 'checker' but doesn't define next index (so readonly)
        template<size_t N, class C, class T>
        struct end_checker : std::bool_constant<checker_c<N, C, T>> {};

        // Increment implementation, last template class needs to be 
        // unique each time to increment.
        template<size_t, class, class> struct incr;
        template<size_t V, class C, class L> requires (!checker<V, C, L>::value)
            struct incr<V, C, L> {
            constexpr static size_t get() { return V; }
        };

        template<size_t V, class C, class L> requires (checker<V, C, L>::value)
            struct incr<V, C, L> : incr<V + 1, C, L> {
            using incr<V + 1, C, L>::get;
        };

        // Get current value of the counter, last template value needs
        // to be unique each time to re-evaluate each time.
        template<size_t, class, auto> struct end;
        template<size_t V, class C, auto L> requires (!end_checker<V, C, decltype(L)>::value)
            struct end<V, C, L> {
            constexpr static size_t get() { return V; }
        };

        template<size_t V, class C, auto L> requires (end_checker<V, C, decltype(L)>::value)
            struct end<V, C, L> : end<V + 1, C, L> {
            using end<V + 1, C, L>::get;
        };
    }

    // Base class for the lambda storage, used in the dynamic cast
    struct callable_base {
        std::size_t index; // Store index of assigned lambda
        constexpr callable_base(std::size_t index) : index(index) {}
        constexpr virtual ~callable_base() {}
        template<class ...Args>
        constexpr auto call(Args&&...args);
    };

    // Actual storage, only supports lambdas of 64 bytes or less, 
    // completely erases the type of the lambda
    template<std::size_t I>
    struct lambda : callable_base {

        // This line links the lambda to the given index I
        template<class Lambda>
        constexpr static bool define = detail::link_types<
            detail::type_group<Lambda>, detail::type_group<lambda<I>>>;

        // Make sure lambda is within size constraints, and 'call' define 
        // in the requires statement, we need this level of indirection!!
        template<class Lambda> requires (sizeof(Lambda) <= 64 && define<Lambda>)
        constexpr lambda(Lambda l) : callable_base{ I } {
            // Simply copy construct lambda inside bytes of 'data'
            Lambda* const _ptr = reinterpret_cast<Lambda* const>(data);
            std::construct_at(_ptr, l);
        }

        uint8_t data[64]{};
    };

    // Stores a fully type erased lambda, uses a lookup table to 
    // find the correct lambda type during runtime, cast, and call 
    // the lambda with the arguments given in the operator().
    struct callable {
        template<class Lambda>
        constexpr callable(Lambda l)
            : _Ptr(new lambda{ l }) {}

        template<class ...Args, auto = []{} /* Need this unique template 
                                            value each call to re-evaluate*/>
        constexpr void operator()(Args&&...args) {
            // Forward call to implementation
            _Ptr->call<Args...>(std::forward<Args>(args)...);
        }
    private:
        std::unique_ptr<callable_base> _Ptr;
    };

    // Single lambda caller, call lambda linked to index I.
    template<std::size_t I>
    constexpr auto caller_impl = []<class ...Args>(callable_base * me, Args&&...args) {
        // Make sure we have the correct lambda by dynamic casting.
        if (auto _ptr = dynamic_cast<lambda<I>*>(me)) {
            // Get the linked lambda type using the index I
            using type = std::tuple_element_t<0, detail::linked_types<lambda<I>>::types>;
            // Make sure we can invoke the lambda with the arguments.
            if constexpr (std::invocable<type, Args&&...>) {
                // Finally, cast and call lambda with arguments.
                type* _lambda = reinterpret_cast<type*>(_ptr->data);
                (*_lambda)(std::forward<Args>(args)...);
            }
        }
    };

    // dynamic call implementation
    template<class ...Args>
    constexpr auto callable_base::call(Args&&...args) {
        // We need to wrap this in a lambda to make the index sequence
        auto caller = [&]<std::size_t ...Is>(std::index_sequence<Is...>) {
            // Create caller lookup table so we can instantly get
            // correct lambda type at runtime.
            using fun_type = void(*)(callable_base*, Args&&...);
            constexpr static std::array<fun_type, sizeof...(Is)> _callers{
                static_cast<fun_type>(caller_impl<Is>)...
            };

            _callers[index](this, std::forward<Args>(args)...);
        };
        // Find out how many lambdas have been defined by reading the constexpr counter.
        constexpr std::size_t end = detail::end<0, callable_base, []{}>::get();
        caller(std::make_index_sequence<end>{}); // Create call lookup table using index sequence.
    }

    // Deduction guide to help assign incrementing index to each unique lambda
    template<class Lambda>
    lambda(Lambda)->lambda<(detail::incr<0, callable_base, Lambda>::get())>;
}