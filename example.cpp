#include <iostream>
#include <vector>
#include "type_utils.hpp"
namespace kaixo {
    template<class ...Tys>        struct expression;
    template<class Ty>            struct value_wrapper { Ty value; };
    template<class Ty>           concept is_var = requires(Ty ty) { { Ty::name }; };
    template<class Ty>           concept is_value_wrapper = specialization<Ty, value_wrapper>;
    template<class Ty>           concept is_expression = specialization<Ty, expression>;
    template<class ...As>        concept valid_op_args = ((is_var<decay_t<As>> || is_expression<decay_t<As>>) || ...);
    template<class A>             struct to_parts { using type = expression<value_wrapper<A>>; };
    template<is_var A>            struct to_parts<A> { using type = expression<A>; };
    template<class ...As>         struct to_parts<expression<As...>> { using type = expression<As...>; };
    template<class T>              using to_parts_t = typename to_parts<T>::type;
    template<auto Op, class ...As> using combine_parts_t = concat_t<to_parts_t<decay_t<As>>..., expression<value_t<Op>>>;

    template<string_literal Name> struct var {
        constexpr static auto name = Name;

        template<class Ty> struct named_value : value_wrapper<Ty>, var{};
        template<class T> constexpr auto operator=(T&& arg) const {
            return named_value<T>{ std::forward<T>(arg) };
        }

        template<class B> constexpr auto operator[](B&& b) const {
            constexpr auto op_lambda = []<class Q, class R> (Q && q, R && r)
            -> decltype(auto) { return std::forward<Q>(q)[std::forward<R>(r)]; };
            using type = combine_parts_t<op_lambda, var, B>;
            return type{ *this, std::forward<B>(b) };
        }
    };

#define KAIXO_EOP(OP) template<class A, class B> requires valid_op_args<A, B>          \
constexpr auto operator OP(A&& a, B&& b) {                                             \
    constexpr auto op_lambda = []<class Q, class R>(Q&& q, R&& r)                      \
    -> decltype(auto) { return std::forward<Q>(q) OP std::forward<R>(r); };            \
    return combine_parts_t<op_lambda, A, B>{ std::forward<A>(a), std::forward<B>(b) }; \
}
#define KAIXO_UOP(OP) template<valid_op_args A>                                                         \
constexpr auto operator OP(A&& a) {                                                                     \
    constexpr auto op_lambda = []<class Q> (Q&& q) -> decltype(auto) { return OP std::forward<Q>(q); }; \
    return combine_parts_t<op_lambda, A>{ std::forward<A>(a) };                                         \
}
    KAIXO_EOP(+); KAIXO_EOP(<= ); KAIXO_EOP(|| ); KAIXO_EOP(|= );  KAIXO_EOP(/ ); KAIXO_EOP(^);
    KAIXO_EOP(-); KAIXO_EOP(<< ); KAIXO_EOP(+= ); KAIXO_EOP(&= );  KAIXO_EOP(< ); KAIXO_EOP(->*);
    KAIXO_EOP(*); KAIXO_EOP(>> ); KAIXO_EOP(-= ); KAIXO_EOP(^= );  KAIXO_EOP(> ); KAIXO_EOP(!= );
    KAIXO_EOP(%); KAIXO_EOP(>= ); KAIXO_EOP(*= ); KAIXO_EOP(%= );  KAIXO_EOP(| ); KAIXO_EOP(>>= );
    KAIXO_EOP(&); KAIXO_EOP(== ); KAIXO_EOP(/= ); KAIXO_EOP(<<= ); KAIXO_EOP(&&); KAIXO_EOP(<=> );
    KAIXO_UOP(-); KAIXO_UOP(+); KAIXO_UOP(--); KAIXO_UOP(++); KAIXO_UOP(~); KAIXO_UOP(!); KAIXO_UOP(*);
#undef KAIXO_EOP
#undef KAIXO_UOP

    template<class T> constexpr auto as_tuple(T&& arg) {
        if constexpr (is_expression<decay_t<T>>) return arg._data;
        else if constexpr (is_var<decay_t<T>>) return std::tuple{};
        else return std::tuple{ value_wrapper{ std::forward<T>(arg) } };
    }

    template<class ...Tys> struct expression {
        using parts = info<Tys...>;
        using data_types = parts::template filter<is_specialization<value_wrapper>>;
        data_types::template as<std::tuple> _data;

        template<class ...As> constexpr expression(As&& ...as) 
            : _data{ std::tuple_cat(as_tuple(std::forward<As>(as))...) } {}

        template<std::size_t I, class ...Values>
        constexpr decltype(auto) get(std::tuple<Values...>& vals) const {
            using type = parts::template element<I>::type;
            if constexpr (is_var<type>) {
                constexpr auto find_name = []<class Ty>{ return Ty::name == type::name; };
                constexpr auto indices = indices_filter_v<find_name, Values...>;
                if constexpr (indices.size() == 0) return type{};
                else return std::get<indices[0]>(vals).value;
            } else return std::get<data_types::template index<type>>(_data).value;
        }

        template<class ...Tys> 
        constexpr decltype(auto) operator()(Tys&&... tys) const {
            std::tuple args{ tys... };
            return eval<0>(args);
        }

        template<std::size_t I = 0, class ...Values, class ...Tys>
        constexpr decltype(auto) eval(std::tuple<Values...>& vals, Tys&&...tys) const {
            if constexpr (I == parts::size) return (std::forward<Tys>(tys), ...);
            else { using type = parts::template element<I>::type;
                if constexpr (!is_var<type> && !is_value_wrapper<type>) {
                    template_pack<Tys...> _tys{ tys... };
                    constexpr bool is_unary = invocable<decltype(type::value), decltype(_tys.get<sizeof...(Tys) - 1>())>;
                    return _tys.drop_last<is_unary ? 1 : 2>().call([&]<class ...Args>(Args&&...args) {
                        return eval<I + 1>(vals, std::forward<Args>(args)..., _tys.last<is_unary ? 1 : 2>().call(type::value));
                    });
                } else return eval<I + 1>(vals, std::forward<Tys>(tys)..., get<I>(vals));
            }
        }
    };
}


using namespace kaixo;

struct Thing {
    int member = 10;
};

int main() {

    constexpr var<"a"> a;
    constexpr var<"b"> b;
    constexpr var<"c"> c;
    constexpr var<"x"> x;

    constexpr auto aon = 2 + -a + 1 + b * c + a * 1;
    constexpr auto oenai = aon(a = 1, b = 3, c = 4);
    constexpr auto eoain = sizeof(aon);

    //constexpr auto quadratic = a * x * x + b * x + c;
    //constexpr auto f = quadratic(a = 2, b = -3, c = 5);
    //
    //constexpr auto y = f(x = 3);


    /**
     *
     *
     * a + b -> ab+
     * c + (ab+) -> cab++
     * (ab+) + c -> ab+c+
     *
     *
     *
     *
     */

    return 0;
}