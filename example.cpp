

#include <cmath>

//namespace kaixo {
//
//    template<class Ty>
//    consteval bool contains_lmbd() {
//        return std::string_view{ __FUNCSIG__ }.contains("lambda");
//    }
//
//    template<class Ty> concept is_lambda = contains_lmbd<Ty>();
//
//    namespace grab {
//        template<class Ty> using name = value_t<Ty::name>;
//    }
//
//    template<string_literal Name>
//    struct meta_member {
//        constexpr static auto name = Name;
//
//        template<class Ty>
//        struct typed {
//            constexpr static auto name = Name;
//            using type = Ty;
//            type value{};
//        };
//
//        template<class Ty>
//        consteval auto operator=(Ty&& ty) const 
//            -> typed<decay_t<Ty>> { 
//            return { std::forward<Ty>(ty) };
//        }
//    };
//
//    template<class Ty>
//    concept is_typed_meta_member = requires (Ty ty) {
//        typename Ty::type;
//        { Ty::name };
//        { ty.value };
//    };
//    
//    template<class Ty>
//    concept is_meta_struct = requires (Ty ty) {
//        typename Ty::parent;
//        typename Ty::members;
//        typename Ty::names;
//        typename Ty::types;
//    };
//
//    template<auto ...Members>
//    using meta_struct_tuple = typename info<decltype(Members)...>::template transform<grab::type>::template as<std::tuple>;
//
//    template<class Ty>
//    struct ref {
//        using type = Ty;
//        Ty* value;
//    };
//
//    template<class Ty> struct remove_ref { using type = Ty; };
//    template<class Ty> struct remove_ref<ref<Ty>> { using type = Ty; };
//    template<class Ty> using remove_ref_t = typename remove_ref<Ty>::type;
//
//    template<auto ...Members>
//    struct meta_struct : meta_struct_tuple<Members...> {
//        using parent = meta_struct_tuple<Members...>;
//        using values = info_v<Members...>;
//        using members = info<decltype(Members)...>;
//        using names = members::template transform<grab::name>;
//        using types = members::template transform<grab::type>::template transform<remove_ref_t>;
//        using _full_types = members::template transform<grab::type>;
//
//        constexpr meta_struct() : parent{ Members.value... } {}
//
//        template<is_typed_meta_member ...Tys>
//        constexpr meta_struct(Tys&&... tys)
//            : parent{ Members.value... } {
//            (_set_member<decay_t<Tys>>(std::forward<Tys>(tys).value), ...);
//        }
//
//        template<class M> requires 
//            (is_meta_struct<decay_t<M>> && !same_as<decay_t<M>, meta_struct>)
//        constexpr meta_struct(M&& other)
//            : parent{ Members.value... } {
//            decay_t<M>::values::for_each([&]<class ...Args>{
//                (_set_member<decltype(Args::value)>(std::forward<M>(other)._get(Args::value)), ...);
//            });
//        }
//
//        template<class Ty, class Arg>
//        constexpr void _set_member(Arg&& val) {
//            constexpr std::size_t index = names::template index<value_t<Ty::name>>;
//            if constexpr (index != npos) {
//                using full = _full_types::template type<index>;
//                using to = types::template type<index>;
//                if constexpr (specialization<full, ref> && same_as<to&, Arg&&>) 
//                    std::get<index>(*this).value = &std::forward<Arg>(val);
//                else if constexpr (!specialization<full, ref> && convertible_to<Arg&&, to>)
//                    this->_get_i<index>() = std::forward<Arg>(val);
//            }
//        }
//
//        template<class Self, class Ty>
//        constexpr decltype(auto) operator[](this Self& self, const Ty&) {
//            constexpr std::size_t index = names::template index<value_t<Ty::name>>;
//            if constexpr (is_lambda<types::template type<index>>) {
//                return[&]<class ...Tys> (Tys&&...args) -> decltype(auto) {
//                    return self._get_i<index>()(self, std::forward<Tys>(args)...);
//                };
//            } else return self._get_i<index>();
//        }
//
//        template<class Self, class Ty>
//        constexpr decltype(auto) _get(this Self&& self, const Ty&) {
//            constexpr std::size_t index = names::template index<value_t<Ty::name>>;
//            return std::forward<Self>(self)._get_i<index>();
//        }
//
//        template<std::size_t I, class Self>
//        constexpr decltype(auto) _get_i(this Self&& self) {
//            using type = _full_types::template type<I>;
//            if constexpr (specialization<type, ref>) return *std::get<I>(std::forward<Self>(self)).value;
//            else return std::get<I>(std::forward<Self>(self));
//        }
//    };
//
//    template<class Ty>
//    constexpr auto meta_deduction_helper = typename meta_member<Ty::name>::template typed<typename Ty::type>{};
//
//    template<is_typed_meta_member ...Tys>
//    meta_struct(Tys...)->meta_struct<meta_deduction_helper<Tys>...>;
//
//    template<class ...Members>
//    struct meta_trait : meta_struct<Members::member...> {
//        template<class M>
//        constexpr meta_trait(M&& other)
//            : meta_struct<Members::member...>{ std::forward<M>(other) } {
//        }
//    };
//
//    template<class Ty, auto Member>
//    struct has {
//        constexpr static auto member = typename decltype(Member)::template typed<ref<Ty>>{ nullptr };
//        using type = Ty;
//    };
//}
//
//
//constexpr meta_member<"x"> x{};
//constexpr meta_member<"y"> y{};
//constexpr meta_member<"magnitude"> magnitude{};
//constexpr meta_member<"get"> get{};



#include <iostream>
#include "type_utils.hpp"

using namespace kaixo;

template<class A, class B>
using bigger_than = value_t<(sizeof_v<A> > sizeof_v<B>)>;

int main() {




    return 0;
}