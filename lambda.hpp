#pragma once
#include "utils.hpp"
#include "function.hpp"

// Based on
// * http://alexpolt.github.io/type-loophole.html
//   https://github.com/alexpolt/luple/blob/master/type-loophole.h
//   by Alexandr Poltavsky, http://alexpolt.github.io
// * https://www.youtube.com/watch?v=UlNUNxLtBI0
//   Better C++14 reflections - Antony Polukhin - Meeting C++ 2018

namespace refl {

    // tag<T, N> generates friend declarations and helps with overload resolution.
    // There are two types: one with the auto return type, which is the way we read types later.
    // The second one is used in the detection of instantiations without which we'd get multiple
    // definitions.
    template <typename T, int N>
    struct tag {
        friend auto loophole(tag<T, N>);
        constexpr friend int cloophole(tag<T, N>);
    };

    // The definitions of friend functions.
    template <typename T, typename U, int N, bool B,
        typename = typename std::enable_if_t<
        !std::is_same_v<
        std::remove_cv_t<std::remove_reference_t<T>>,
        std::remove_cv_t<std::remove_reference_t<U>>>>>
        struct fn_def {
        friend auto loophole(tag<T, N>) { return U{}; }
        constexpr friend int cloophole(tag<T, N>) { return 0; }
    };

    // This specialization is to avoid multiple definition errors.
    template <typename T, typename U, int N> struct fn_def<T, U, N, true> {};

    // This has a templated conversion operator which in turn triggers instantiations.
    // Important point, using sizeof seems to be more reliable. Also default template
    // arguments are "cached" (I think). To fix that I provide a U template parameter to
    // the ins functions which do the detection using constexpr friend functions and SFINAE.
    template <typename T, int N>
    struct c_op {
        template <typename U, int M>
        static auto ins(...) -> int;
        template <typename U, int M, int = cloophole(tag<T, M>{}) >
        static auto ins(int) -> char;

        template <typename U, int = sizeof(fn_def<T, U, N, sizeof(ins<U, N>(0)) == sizeof(char)>)>
        operator U();
    };

    // Here we detect the data type field number. The byproduct is instantiations.
    // Uses list initialization. Won't work for types with user-provided constructors.
    // In C++17 there is std::is_aggregate which can be added later.
    template <typename T, int... Ns>
    constexpr int fields_number(...) { return sizeof...(Ns) - 1; }

    template <typename T, int... Ns>
    constexpr auto fields_number(int) -> decltype(T{ c_op<T, Ns>{}... }, 0) {
        return fields_number<T, Ns..., sizeof...(Ns)>(0);
    }

    // Here is a version of fields_number to handle user-provided ctor.
    // NOTE: It finds the first ctor having the shortest unambigious set
    //       of parameters.
    template <typename T, int... Ns>
    constexpr auto fields_number_ctor(int) -> decltype(T(c_op<T, Ns>{}...), 0) {
        return sizeof...(Ns);
    }

    template <typename T, int... Ns>
    constexpr int fields_number_ctor(...) {
        return fields_number_ctor<T, Ns..., sizeof...(Ns)>(0);
    }

    // This is a helper to turn a ctor into a tuple type.
    // Usage is: refl::as_tuple<data_t>
    template <typename T, typename U> struct loophole_tuple;

    template <typename T, int... Ns>
    struct loophole_tuple<T, std::integer_sequence<int, Ns...>> {
        using type = std::tuple<decltype(loophole(tag<T, Ns>{}))... > ;
    };

    template <typename T>
    using as_tuple =
        typename loophole_tuple<T, std::make_integer_sequence<int, fields_number_ctor<T>(0)>>::type;

}  // namespace refl

namespace kaixo {
    template<class ...Tys>
    struct captures {};
    template<class>
    struct captures_size;
    template<class ...Tys>
    struct captures_size<captures<Tys...>> : public std::integral_constant<size_t, sizeof...(Tys)> {};
    template<std::size_t, class>
    struct nth_capture;
    template<std::size_t N, class ...Tys>
    struct nth_capture<N, captures<Tys...>> {
        using type = typename std::tuple_element<N, std::tuple<Tys...>>::type;
    };

    template<class, class = captures<>>
    class lambda;
    template<class ...Tys, class Return, class ...Args>
    class lambda<Return(Args...), captures<Tys...>> : public function<Return(Args...)> {
        using parent = function<Return(Args...)>;
        using seq = std::make_index_sequence<sizeof...(Tys)>;
    public:
        using captures = captures<Tys...>;

        struct value {
            template<one_of<Tys...> T>
            inline operator T& () { return get<T>(); }

            template<one_of<Tys...> T>
            inline operator T const& () const { return get<T>(); }

            template<one_of<Tys...> T>
            inline T& get() {
                check_type<T>(seq{});

                if constexpr (std::is_reference_v<T>)
                    return *reinterpret_cast<std::remove_reference_t<T>*>(data);

                else
                    return *reinterpret_cast<T*>(data);
            }

            template<one_of<Tys...> T>
            inline T const& get() const {
                check_type<T>(seq{});
                if constexpr (std::is_reference_v<T>)
                    return *reinterpret_cast<std::remove_reference_t<T>*>(data);

                else
                    return *reinterpret_cast<T*>(data);
            }

        private:
            value(std::size_t i) : index(i) {}

            template<class T, std::size_t ...Is>
            inline void check_type(std::index_sequence<Is...>) {
                ((index == Is && !std::is_same_v<nth_capture<Is, captures>::type, T>
                    ? throw std::bad_cast{} : false), ...);
            }

            void* data = nullptr;
            std::size_t index = 0;
            friend class lambda;
        };

        template<std::invocable<Args...> T>
        lambda(T&& t)
            : alignment(alignof(T)), parent(std::forward<T>(t)), lambda_data(reinterpret_cast<std::byte*>(std::addressof(
                dynamic_cast<typed_function_storage<T, parent::result_type(Args...)>*>(this->storage)->function)))
        {}

        lambda(lambda&& other)
            : alignment(other.alignment), lambda_data(other.lambda_data), parent((parent)other)
        {}

        lambda(const lambda& other)
            : alignment(other.alignment), lambda_data(other.lambda_data), parent((parent)other)
        {}

        template<std::size_t I> 
        inline auto& get() requires (I < captures_size<captures>::value) {
            using Type = nth_capture<I, captures>::type;
            std::size_t _offset = alignment * I;
            if constexpr (std::is_reference_v<Type>)
                return *reinterpret_cast<std::remove_reference_t<Type>*>(reinterpret_cast<std::ptrdiff_t*>(lambda_data + _offset)[0]);

            else
                return *reinterpret_cast<Type*>(lambda_data + _offset);
        }

        template<std::size_t I>
        inline auto const& get() const requires (I < captures_size<captures>::value) {
            using Type = nth_capture<I, captures>::type;
            std::size_t _offset = alignment * I;
            if constexpr (std::is_reference_v<Type>)
                return *reinterpret_cast<std::remove_reference_t<Type>*>(reinterpret_cast<std::ptrdiff_t*>(lambda_data + _offset)[0]);

            else
                return *reinterpret_cast<Type*>(lambda_data + _offset);
        }

        inline value operator[](std::size_t index) { return get(index, seq{}); }
        inline const value operator[](std::size_t index) const { return get(index, seq{}); }
        inline auto& get(std::size_t index) { return get(index, seq{}); }
        inline auto const& get(std::size_t index) const { return get(index, seq{}); }

    private:
        size_t alignment;
        std::byte* lambda_data;

        template<std::size_t... Is>
        inline value get(std::size_t index, std::index_sequence<Is...>) {
            if (index >= sizeof...(Tys))
                throw std::out_of_range{ "Index if out of range for this tuple." };

            value _value = index;
            return ((index == Is ? (_value.data = (void*)&(this->get<Is>()), _value) : _value), ...);
        }
    };

    template<class>
    struct tuple_to_captures;
    template<class ...Tys>
    struct tuple_to_captures<std::tuple<Tys...>> {
        using type = captures<Tys...>;
    };
    template<class T>
    using tuple_to_captures_t = typename tuple_to_captures<T>::type;

    template<class T>
    decltype(auto) lambda_captures() {
        struct lambda_type : public T {
            using T::T;
        };
        return tuple_to_captures_t<refl::as_tuple<lambda_type>>{};
    }

    template<class T>
    lambda(T)->lambda<lambda_signature_t<T>, decltype(lambda_captures<T>())>;
}