#pragma once
#include <utility>
#include <type_traits>

#include "pack_utils.hpp"

namespace kaixo {
    namespace detail {
        struct dud {};
    }

#define KAIXO_MEMBER_CALL_C(MAC, V, REF, NOEXCEPT) \
MAC(     , V, REF, NOEXCEPT)                       \
MAC(const, V, REF, NOEXCEPT)

#define KAIXO_MEMBER_CALL_V(MAC, REF, NOEXCEPT)   \
KAIXO_MEMBER_CALL_C(MAC,         , REF, NOEXCEPT) \
KAIXO_MEMBER_CALL_C(MAC, volatile, REF, NOEXCEPT)

#define KAIXO_MEMBER_CALL_NOEXCEPT(MAC, NOEXCEPT) \
KAIXO_MEMBER_CALL_V(MAC,   , NOEXCEPT)            \
KAIXO_MEMBER_CALL_V(MAC,  &, NOEXCEPT)            \
KAIXO_MEMBER_CALL_V(MAC, &&, NOEXCEPT)

#define KAIXO_MEMBER_CALL(MAC)            \
KAIXO_MEMBER_CALL_NOEXCEPT(MAC,         ) \
KAIXO_MEMBER_CALL_NOEXCEPT(MAC, noexcept) 

    template<class Ty> concept is_functor = requires(decltype(&Ty::operator()) a) { a; };

    template<class> struct function_info_impl {};
    template<is_functor Ty> struct function_info_impl<Ty>
        : function_info_impl<decltype(Ty::operator())> {};

#define KAIXO_MEMBER_FUNCTION_INFO_MOD(CONST, VOLATILE, REF, NOEXCEPT)                  \
template<class Ty, class R, class ...Args>                                              \
struct function_info_impl<R(Ty::*)(Args...) CONST VOLATILE REF NOEXCEPT> {              \
    using pointer = R(*)(Args...) NOEXCEPT;                                             \
    using minimal_pointer = R(*)(Args...);                                              \
    using signature = R(Args...) CONST VOLATILE REF NOEXCEPT;                           \
    using minimal_signature = R(Args...);                                               \
    using object = CONST VOLATILE Ty REF;                                               \
    using minimal_object = Ty;                                                          \
    using return_type = R;                                                              \
    using argument_types = kaixo::pack<Args...>;                                        \
    constexpr static bool is_const = std::same_as<const int, CONST int>;                \
    constexpr static bool is_volatile = std::same_as<volatile int, VOLATILE int>;       \
    constexpr static bool is_lvalue = std::same_as<int&, int REF>;                      \
    constexpr static bool is_rvalue = std::same_as<int&&, int REF>;                     \
    constexpr static bool is_reference = is_lvalue || is_rvalue;                        \
    constexpr static bool is_noexcept = std::same_as<void() noexcept, void() NOEXCEPT>; \
};

KAIXO_MEMBER_CALL(KAIXO_MEMBER_FUNCTION_INFO_MOD)
#undef KAIXO_MEMBER_FUNCTION_INFO_MOD

#define KAIXO_FUNCTION_INFO_MOD(PTR, CONST, VOLATILE, REF, NOEXCEPT)                    \
template<class R, class ...Args>                                                        \
struct function_info_impl<R PTR (Args...) CONST VOLATILE REF NOEXCEPT> {                \
    using pointer = R(*)(Args...) NOEXCEPT;                                             \
    using minimal_pointer = R(*)(Args...);                                              \
    using signature = R(Args...) CONST VOLATILE REF NOEXCEPT;                           \
    using minimal_signature = R(Args...);                                               \
    using object = CONST VOLATILE detail::dud REF;                                      \
    using minimal_object = detail::dud;                                                 \
    using return_type = R;                                                              \
    using argument_types = kaixo::pack<Args...>;                                        \
    constexpr static bool is_const = std::same_as<const int, CONST int>;                \
    constexpr static bool is_volatile = std::same_as<volatile int, VOLATILE int>;       \
    constexpr static bool is_lvalue = std::same_as<int&, int REF>;                      \
    constexpr static bool is_rvalue = std::same_as<int&&, int REF>;                     \
    constexpr static bool is_reference = is_lvalue || is_rvalue;                        \
    constexpr static bool is_noexcept = std::same_as<void() noexcept, void() NOEXCEPT>; \
};

#define KAIXO_FUNCTION_INFO_PTR(CONST, VOLATILE, REF, NOEXCEPT) \
KAIXO_FUNCTION_INFO_MOD(, CONST, VOLATILE, REF, NOEXCEPT)

KAIXO_MEMBER_CALL(KAIXO_FUNCTION_INFO_PTR)
KAIXO_FUNCTION_INFO_MOD((*), , , , )
KAIXO_FUNCTION_INFO_MOD((*), , , , noexcept)
#undef KAIXO_FUNCTION_INFO_MOD

        template<class Ty> using function_info = function_info_impl<std::remove_cv_t<std::remove_reference_t<Ty>>>;
    template<auto Ty> using function_info_v = function_info_impl<std::remove_cv_t<std::remove_reference_t<decltype(Ty)>>>;
}