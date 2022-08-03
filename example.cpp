#include <iostream>
#include <vector>
#include "type_utils.hpp"
//namespace kaixo {
//    template<class ...Tys>        struct expression;
//    template<class Ty>            struct value_wrapper { Ty value; };
//    template<class Ty>           concept is_var = requires(Ty ty) { { Ty::name }; };
//    template<class Ty>           concept is_value_wrapper = specialization<Ty, value_wrapper>;
//    template<class Ty>           concept is_expression = specialization<Ty, expression>;
//    template<class A>            concept disable_expr = requires(A a) { typename A::disable_expression; };
//    template<class A>            concept valid_op_arg = (is_var<A> || is_expression<A>) && !disable_expr<A>;
//    template<class ...As>        concept valid_op_args = (valid_op_arg<decay_t<As>> || ...) && (!disable_expr<decay_t<As>> && ...);
//    template<class A>             struct to_parts { using type = expression<value_wrapper<A>>; };
//    template<is_var A>            struct to_parts<A> { using type = expression<A>; };
//    template<class ...As>         struct to_parts<expression<As...>> { using type = expression<As...>; };
//    template<class T>              using to_parts_t = typename to_parts<T>::type;
//    template<auto Op, class ...As> using combine_parts_t = concat_t<to_parts_t<decay_t<As>>..., expression<value_t<Op>>>;
//
//    template<string_literal Name> struct var {
//        constexpr static auto name = Name;
//
//        template<class Ty> struct named_value : value_wrapper<Ty>, var{};
//        template<class T> constexpr auto operator=(T&& arg) const {
//            return named_value<T>{ std::forward<T>(arg) };
//        }
//
//        template<class B> constexpr auto operator[](B&& b) const {
//            constexpr auto op_lambda = []<class Q, class R> (Q && q, R && r)
//            -> decltype(auto) { return std::forward<Q>(q)[std::forward<R>(r)]; };
//            using type = combine_parts_t<op_lambda, var, B>;
//            return type{ *this, std::forward<B>(b) };
//        }
//    };
//
//#define KAIXO_EOP(OP) template<class A, class B> requires valid_op_args<A, B>          \
//constexpr auto operator OP(A&& a, B&& b) {                                             \
//    constexpr auto op_lambda = []<class Q, class R>(Q&& q, R&& r)                      \
//    -> decltype(auto) { return std::forward<Q>(q) OP std::forward<R>(r); };            \
//    return combine_parts_t<op_lambda, A, B>{ std::forward<A>(a), std::forward<B>(b) }; \
//}
//#define KAIXO_UOP(OP) template<valid_op_args A>                                                         \
//constexpr auto operator OP(A&& a) {                                                                     \
//    constexpr auto op_lambda = []<class Q> (Q&& q) -> decltype(auto) { return OP std::forward<Q>(q); }; \
//    return combine_parts_t<op_lambda, A>{ std::forward<A>(a) };                                         \
//}
//    KAIXO_EOP(+); KAIXO_EOP(<= ); KAIXO_EOP(|| ); KAIXO_EOP(|= );  KAIXO_EOP(/ ); KAIXO_EOP(^);
//    KAIXO_EOP(-); KAIXO_EOP(<< ); KAIXO_EOP(+= ); KAIXO_EOP(&= );  KAIXO_EOP(< ); KAIXO_EOP(->*);
//    KAIXO_EOP(*); KAIXO_EOP(>> ); KAIXO_EOP(-= ); KAIXO_EOP(^= );  KAIXO_EOP(> ); KAIXO_EOP(!= );
//    KAIXO_EOP(%); KAIXO_EOP(>= ); KAIXO_EOP(*= ); KAIXO_EOP(%= );  KAIXO_EOP(| ); KAIXO_EOP(>>= );
//    KAIXO_EOP(&); KAIXO_EOP(== ); KAIXO_EOP(/= ); KAIXO_EOP(<<= ); KAIXO_EOP(&&); KAIXO_EOP(<=> );
//    KAIXO_UOP(-); KAIXO_UOP(+); KAIXO_UOP(--); KAIXO_UOP(++); KAIXO_UOP(~); KAIXO_UOP(!); KAIXO_UOP(*);
//#undef KAIXO_EOP
//#undef KAIXO_UOP
//    template<class A, class B> requires valid_op_args<A, B>
//    constexpr auto operator,(A&& a, B&& b) {
//        constexpr auto op_lambda = []<class Q, class R>(Q&& q, R&& r) -> decltype(auto) {
//            constexpr bool _q_tuple = specialization<decay_t<Q>, std::tuple>;
//            constexpr bool _r_tuple = specialization<decay_t<R>, std::tuple>;
//            if constexpr (_q_tuple && _r_tuple) return std::tuple_cat(std::forward<Q>(q), std::forward<R>(r));
//            else if constexpr (_q_tuple) return std::tuple_cat(std::forward<Q>(q), std::tuple{ std::forward<R>(r) });
//            else if constexpr (_r_tuple) return std::tuple_cat(std::tuple{ std::forward<Q>(q) }, std::forward<R>(r));
//            else return std::tuple_cat(std::tuple{ std::forward<Q>(q) }, std::tuple{ std::forward<R>(r) });
//        };
//        return combine_parts_t<op_lambda, A, B>{ std::forward<A>(a), std::forward<B>(b) };
//    }
//
//    template<class T> constexpr auto as_tuple(T&& arg) {
//        if constexpr (is_expression<decay_t<T>>) return arg._data;
//        else if constexpr (is_var<decay_t<T>>) return std::tuple{};
//        else return std::tuple{ value_wrapper{ std::forward<T>(arg) } };
//    }
//
//    template<class ...Tys> struct expression {
//        using parts = info<Tys...>;
//        using data_types = parts::template filter<is_specialization<value_wrapper>>;
//        data_types::template as<std::tuple> _data;
//
//        template<class ...As> constexpr expression(As&& ...as) 
//            : _data{ std::tuple_cat(as_tuple(std::forward<As>(as))...) } {}
//
//        template<std::size_t I, class ...Values>
//        constexpr decltype(auto) get(tuple<Values...>& vals) const {
//            using type = parts::template element<I>::type;
//            if constexpr (is_var<type>) {
//                constexpr auto find_name = []<class Ty>{ return Ty::name == type::name; };
//                constexpr auto indices = indices_filter_v<find_name, Values...>;
//                if constexpr (indices.size() == 0) return type{};
//                else return std::get<indices[0]>(vals).value;
//            } else return std::get<data_types::template index<type>>(_data).value;
//        }
//
//        template<class ...Tys> 
//        constexpr decltype(auto) operator()(Tys&&... tys) const {
//            tuple _vals{ tys... };
//            return eval<0>(_vals);
//        }
//
//        template<std::size_t I = 0, class ...Values, class ...Tys>
//        constexpr decltype(auto) eval(tuple<Values...>& vals, Tys&&...tys) const {
//            if constexpr (I == parts::size) return (std::forward<Tys>(tys), ...);
//            else { using type = parts::template element<I>::type;
//                if constexpr (!is_var<type> && !is_value_wrapper<type>) {
//                    template_pack<Tys...> _tys{ tys... };
//                    constexpr bool is_unary = invocable<decltype(type::value), decltype(_tys.get<sizeof...(Tys) - 1>())>;
//                    return _tys.drop_last<is_unary ? 1 : 2>().call([&]<class ...Args>(Args&&...args) {
//                        return eval<I + 1>(vals, std::forward<Args>(args)..., _tys.last<is_unary ? 1 : 2>().call(type::value));
//                    });
//                } else return eval<I + 1>(vals, std::forward<Tys>(tys)..., get<I>(vals));
//            }
//        }
//    };
//
//    namespace aina {
//        template<class Ty> concept is_lc_container = requires(Ty ty) {
//            typename decay_t<Ty>::value_type;
//            typename decay_t<Ty>::iterator;
//            { ty.begin() }; { ty.end() };
//        };
//
//        template<is_var V, class Ty> struct named_container : Ty, V {
//            using disable_expression = void;
//            using type = V;
//        };
//
//        template<class Ty> concept is_named_container = specialization<Ty, named_container>;
//        template<is_lc_container Ty> struct container {
//            using value_type = decay_t<Ty>::value_type;
//            using iterator = decay_t<Ty>::iterator;
//            Ty& value;
//            constexpr auto begin() const { return std::begin(value); }
//            constexpr auto end() const { return std::end(value); }
//        };
//
//        template<class ...As> struct to_named_value {
//            template<class ...Bs>
//            using assign = info<typename As::template named_value<Bs>...>;
//        };
//
//        template<class E, class ...Ps> struct list_comprehension {
//            using disable_expression = void;
//            using parts = info<Ps...>;
//            using containers = parts::template filter < []<is_named_container>{} > ;
//            using names = containers::template transform<grab::type>;
//            using constraints = parts::template filter < []<is_expression>{} > ;
//            using iterators = containers::template transform<grab::iterator>;
//            using value_types = containers::template transform<grab::value_type>;
//            using named_values = value_types::template as<typename names::template as<to_named_value>::assign>;
//            using iterator_tuple = iterators::template as<std::tuple>;
//            using value_tuple = named_values::template as<std::tuple>;
//            using value_type = decltype(std::declval<E&>().eval(std::declval<value_tuple&>()));
//
//            [[no_unique_address]] E _expression;
//            std::tuple<Ps...> _parts;
//
//            template<std::size_t I>
//            constexpr static std::size_t container = containers::template index<typename parts::template element<I>::type>;
//            template<std::size_t I>
//            constexpr static std::size_t constraint = parts::template index<typename constraints::template element<I>::type>;
//
//            struct iterator {
//                enum ReturnCode { None = 0x0, Again = 0x1, Break = 0x2, };
//                list_comprehension* _self;
//                bool _at_end;
//                iterator_tuple _iterators;
//                value_tuple _values;
//
//                constexpr iterator(list_comprehension* self, bool at_end)
//                    : _self(self), _at_end(at_end),
//                    _iterators(sequence<containers::size>([&]<std::size_t ...Is>{
//                    return iterator_tuple{ std::get<container<Is>>(self->_parts).begin()... };
//                })), _values(sequence<containers::size>([&]<std::size_t ...Is> {
//                    return value_tuple{ { *std::get<Is>(_iterators) }... };
//                })) {
//                    if (!is_valid()) increment<parts::size - 1>();
//                }
//
//                constexpr iterator& operator++() {
//                    increment<parts::size - 1>();
//                    return *this;
//                }
//
//                constexpr bool operator==(const iterator& other) const {
//                    return _at_end == true && other._at_end == true
//                        || other._at_end == _at_end && other._iterators == _iterators;
//                }
//
//                constexpr decltype(auto) operator*() {
//                    if (std::is_constant_evaluated() && _at_end) throw;
//                    return _self->_expression.eval(_values);
//                }
//
//                template<std::size_t I> constexpr void increment() {
//                    using type = parts::template element<I>::type;
//                    if constexpr (is_named_container<type>) {
//                        auto& _part = std::get<I>(_self->_parts);
//                        auto& _iterator = std::get<container<I>>(_iterators);
//                        do {
//                            if (++_iterator == _part.end()) {
//                                if constexpr (I != 0) increment<I - 1>();
//                                _iterator = _part.begin();
//                                if constexpr (I == 0) { _at_end = true; return; }
//                            }
//                            std::get<I>(_values).value = *_iterator;
//                            if constexpr (I != parts::size - 1) {
//                                int _code = execute<I + 1>();
//                                if (_at_end || _code & ReturnCode::Break) _at_end = true;
//                                else if (_code & ReturnCode::Again) continue;
//                            }
//                            return;
//                        } while (true);
//                    }
//                    else if constexpr (I == 0) _at_end = true;
//                    else increment<I - 1>();
//                }
//
//                template<std::size_t I> constexpr int execute() {
//                    using type = parts::template element<I>::type;
//                    if constexpr (is_expression<type>) {
//                        auto& _part = std::get<I>(_self->_parts);
//                        int _code = _part.eval(_values)
//                            ? ReturnCode::None
//                            : ReturnCode::Again;
//                        if constexpr (I != parts::size - 1)
//                            return _code | execute<I + 1>();
//                        else return _code;
//                    }
//                    else return ReturnCode::None;
//                }
//
//                constexpr bool is_valid() {
//                    return sequence<constraints::size>([&]<std::size_t ...Is>{
//                        return (std::get<constraint<Is>>(_self->_parts).eval(_values) && ...);
//                    });
//                }
//            };
//
//            constexpr iterator begin() { return iterator{ this, false }; }
//            constexpr iterator end() { return iterator{ this, true }; }
//        };
//
//        template<class Ty> concept is_list_comprehension = specialization<Ty, list_comprehension>;
//        template<is_lc_container Ty> constexpr auto operator-(Ty& c) { return container{ c }; }
//        template<class V, class Ty> constexpr auto operator<(V, container<Ty>&& c) {
//            return named_container<decay_t<V>, container<Ty>>{ c };
//        }
//
//        template<class A, is_named_container B> requires
//            (is_var<decay_t<A>> || is_expression<decay_t<A>>)
//            constexpr auto operator|(A&& a, B&& b) {
//            using expression_type = to_parts_t<decay_t<A>>;
//            return list_comprehension<expression_type, B>{ { std::forward<A>(a) }, { std::forward<B>(b) } };
//        }
//
//        template<is_list_comprehension A, class B> requires
//            (is_named_container<decay_t<B>> || is_expression<decay_t<B>>)
//            constexpr auto operator,(A&& a, B&& b) {
//            return append_t<B, A>{ std::move(a._expression),
//                std::tuple_cat(std::move(a._parts), std::forward_as_tuple(std::forward<B>(b))) };
//        }
//    }
//}
//
//using namespace kaixo;
//
//template<class ...Tys>
//struct tuple_wrapper : value_wrapper<tuple<Tys...>&>, info<Tys...> {
//    using disable_expression = void;
//    using value_type = tuple<Tys...>;
//};
//
//template<is_var V, class Ty> struct named_tuple : Ty, V {
//    using var_type = V;
//};
//template<class Ty> concept is_named_tuple = specialization<Ty, named_tuple>;
//template<class Ty> struct is_named_tuple_t {
//    constexpr static bool value = is_named_tuple<Ty>;
//};
//template<class Ty> struct is_not_named_tuple_t {
//    constexpr static bool value = !is_named_tuple<Ty>;
//};
//
//template<class ...Tys>
//constexpr tuple_wrapper<Tys...> operator-(tuple<Tys...>& tpl) { return { tpl }; }
//template<is_var V, class ...Tys> 
//constexpr named_tuple<V, tuple_wrapper<Tys...>> operator<(V, tuple_wrapper<Tys...>&& w) { return { std::move(w) }; }
//
//template<class...> struct type_map;
//template<class...As, class...Bs>
//struct type_map<info<info<As, Bs>...>> {
//    using pairs = info<info<As, Bs>...>;
//    using keys = info<As...>;
//    using values = info<Bs...>;
//
//    template<class Ty> using at = typename values::template element<keys::template index<Ty>>::type;
//};
//template<class Ty> concept is_type_map = specialization<Ty, type_map>;
//
//template<is_var A, is_type_trait B> struct type_trait_constraint {
//    template<is_type_map T> struct type {
//        constexpr static bool value = B::template value<typename T::template at<A>>;
//    };
//};
//
//template<class A, class B> 
//    requires (is_var<decay_t<A>> && is_type_trait<decay_t<B>>)
//constexpr type_trait<typename type_trait_constraint<decay_t<A>, decay_t<B>>::type> operator==(A&, B&) { return {}; }
//
//namespace kaixo::grab {
//    template<class Ty> using var_type = typename Ty::var_type;
//}
//
//template<class E, class ...Tys>
//struct tuple_comprehension {
//    using parts = info<Tys...>;
//    using named_tuples = parts::template filter<type_trait<is_named_tuple_t>{}>;
//    using constraints = parts::template filter<type_trait<is_not_named_tuple_t>{}>;
//    using tuples = named_tuples::template transform<grab::value_type>;
//    using variables = named_tuples::template transform<grab::var_type>;
//    constexpr static auto sizes = tuples::template transform<grab::size>::for_each([]<class ...Vs> { return std::array{ Vs::value... }; });
//    constexpr static auto cartesian_size = iterate<sizes>([]<std::size_t ...Is>{ return (Is + ... + 0); });
//
//    [[no_unique_address]] E _expression;
//    tuple<Tys...> _data;
// 
//    template<std::size_t I> constexpr static std::size_t _tuple = parts::template index<typename named_tuples::template element<I>::type>;
//    
//    constexpr static std::array<std::size_t, tuples::size> indices_at_index(std::size_t pos) {
//        return sequence<tuples::size>([&]<std::size_t ...Is>()
//            -> std::array<std::size_t, tuples::size> {
//            std::size_t _t_pos = 0;
//            return { (_t_pos = pos % sizes[Is], pos /= sizes[Is], _t_pos)... };
//        });
//    }
//    
//    constexpr auto eval() {
//        return sequence<cartesian_size>([&]<std::size_t ...Is>{
//            return tuple{ eval_at<Is>()... }.remove<dud>();
//        });
//    }
//    
//    template<class T>
//    constexpr static bool matches() {
//        return sequence<constraints::size>([]<std::size_t ...Is>{
//            return ((constraints::template element<Is>::type::template value<T>) && ...);
//        });
//    }
//    
//    template<std::size_t I> constexpr auto eval_at() {
//        constexpr auto _indices = indices_at_index(I);
//        return sequence<tuples::size>([&]<std::size_t ...Is>{
//            using tuple_type = decltype(tuple{ _data.get<_tuple<Is>>().value.get<_indices[Is]>()... });
//            using types = as_info<tuple_type>;
//            using map = type_map<zip_t<variables, types>>;
//            constexpr bool _matches = matches<map>();
//            if constexpr (_matches) {
//                return _expression(typename variables::template element<Is>::type::template named_value<
//                    decltype(_data.get<_tuple<Is>>().value.get<_indices[Is]>())>{
//                    _data.get<_tuple<Is>>().value.get<_indices[Is]>()
//                }...);
//            } else return dud{};
//        });
//    }
//};
//
//template<class Ty> concept is_tuple_comprehension = specialization<Ty, tuple_comprehension>;
//
//template<class A, is_named_tuple B> requires
//    (is_var<decay_t<A>> || is_expression<decay_t<A>>)
//constexpr auto operator|(A&& a, B&& b) {
//    using expression_type = to_parts_t<decay_t<A>>;
//    return tuple_comprehension<expression_type, B>{ { std::forward<A>(a) }, { std::forward<B>(b) } };
//}
//
//template<is_tuple_comprehension A, class B> 
//    requires (is_named_tuple<decay_t<B>> || is_type_trait<decay_t<B>>)
//constexpr auto operator,(A&& a, B&& b) {
//    return append_t<decay_t<B>, decay_t<A>>{
//        std::move(a._expression), a._data.append(std::forward<B>(b))
//    };
//}
//
//constexpr struct tuple_comprehension_evaluator {
//    template<is_tuple_comprehension A>
//    constexpr auto operator[](A&& a) const {
//        return a.eval();
//    }
//} tc;


#include <iostream>
#include <typeindex>
#include <typeinfo>
#include <any>
#include <map>

using namespace kaixo;

struct runtime {
    constexpr static std::size_t HANDLER_COUNT = 10;
    static inline std::size_t i = 0;
    static inline std::map<std::type_index, void(*)(std::size_t, std::any&, void*)> handlers{};

    template<class Ty> struct register_type {};

    template<std::size_t Id>
    static void call(std::any& value, void* data = nullptr) {
        handlers[value.type()](Id, value, data);
    }
};


template<std::size_t I>
constexpr auto type_handler = []<class Ty>(Ty&, void*) {};

template<class Ty>
struct type_call_handler {
    static void call(std::size_t id, Ty& value, void* userData) {
        generate_template_switch<runtime::HANDLER_COUNT>([&]<std::size_t I>{
            type_handler<I>(value, userData);
        })(id);
    }
};

template<class Ty> struct assign_id {
    static inline auto _ = [] { 
        runtime::handlers[typeid(Ty)] = [](std::size_t id, std::any& data, void* userData) {
            type_call_handler<Ty>::call(id, std::any_cast<Ty&>(data), userData);
        };
        return 0;
    }();
};

template<std::size_t C, std::size_t I>
struct counter_impl : std::integral_constant<std::size_t, counter_impl<C, I - 1>::value> {};

template<std::size_t C>
struct counter_impl<C, 0> : std::integral_constant<std::size_t, 0> {};

#define counter(c) template<> struct counter_impl<c, __COUNTER__> : std::integral_constant<std::size_t, counter_impl<c, __COUNTER__ - 2>::value + 1> {}
#define read_counter(c) (counter_impl<c, __COUNTER__>::value)

#define register(x) template<> struct runtime::register_type<x> : assign_id<x> {};

#define create_handler(x) constexpr std::size_t x = read_counter(0); counter(0); template<> constexpr auto type_handler<x>

struct MyType {
    double value;
};

register(MyType);

create_handler(type_name_printer_id) = []<class Ty>(Ty & value, void* data) {
    std::cout << typeid(value).name() << "\n";
};

void print_name(std::any value) {
    runtime::call<type_name_printer_id>(value);
}


create_handler(get_type_size_t) = []<class Ty>(Ty & value, void* data) {
    (*static_cast<std::size_t*>(data)) = sizeof(Ty);
};

std::size_t sizeof_t(std::any value) {
    std::size_t size;
    runtime::call<get_type_size_t>(value, &size);
    return size;
}


template<class ...Tys> struct tuple {};
template<> struct tuple<> {};             

#define KAIXO_TUPLE_DEF_I(I, ...)                                       \
template<class ...Tys> requires (info<Tys...>::size == I)               \
struct tuple<Tys...> {                                                  \
    using types = info<Tys...>;                                         \
    template<std::size_t N>                                             \
    using _type = typename types::template element<N>::type;            \
    __VA_ARGS__;                                                        \
};

#define KAIXO_TUPLE_DEF_100(...) KAIXO_TUPLE_DEF_I(100, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_99(...) KAIXO_TUPLE_DEF_100(__VA_ARGS__ _type<99> _100;) KAIXO_TUPLE_DEF_I(99, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_98(...) KAIXO_TUPLE_DEF_99(__VA_ARGS__ _type<98> _99;) KAIXO_TUPLE_DEF_I(98, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_97(...) KAIXO_TUPLE_DEF_98(__VA_ARGS__ _type<97> _98;) KAIXO_TUPLE_DEF_I(97, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_96(...) KAIXO_TUPLE_DEF_97(__VA_ARGS__ _type<96> _97;) KAIXO_TUPLE_DEF_I(96, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_95(...) KAIXO_TUPLE_DEF_96(__VA_ARGS__ _type<95> _96;) KAIXO_TUPLE_DEF_I(95, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_94(...) KAIXO_TUPLE_DEF_95(__VA_ARGS__ _type<94> _95;) KAIXO_TUPLE_DEF_I(94, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_93(...) KAIXO_TUPLE_DEF_94(__VA_ARGS__ _type<93> _94;) KAIXO_TUPLE_DEF_I(93, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_92(...) KAIXO_TUPLE_DEF_93(__VA_ARGS__ _type<92> _93;) KAIXO_TUPLE_DEF_I(92, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_91(...) KAIXO_TUPLE_DEF_92(__VA_ARGS__ _type<91> _92;) KAIXO_TUPLE_DEF_I(91, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_90(...) KAIXO_TUPLE_DEF_91(__VA_ARGS__ _type<90> _91;) KAIXO_TUPLE_DEF_I(90, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_89(...) KAIXO_TUPLE_DEF_90(__VA_ARGS__ _type<89> _90;) KAIXO_TUPLE_DEF_I(89, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_88(...) KAIXO_TUPLE_DEF_89(__VA_ARGS__ _type<88> _89;) KAIXO_TUPLE_DEF_I(88, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_87(...) KAIXO_TUPLE_DEF_88(__VA_ARGS__ _type<87> _88;) KAIXO_TUPLE_DEF_I(87, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_86(...) KAIXO_TUPLE_DEF_87(__VA_ARGS__ _type<86> _87;) KAIXO_TUPLE_DEF_I(86, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_85(...) KAIXO_TUPLE_DEF_86(__VA_ARGS__ _type<85> _86;) KAIXO_TUPLE_DEF_I(85, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_84(...) KAIXO_TUPLE_DEF_85(__VA_ARGS__ _type<84> _85;) KAIXO_TUPLE_DEF_I(84, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_83(...) KAIXO_TUPLE_DEF_84(__VA_ARGS__ _type<83> _84;) KAIXO_TUPLE_DEF_I(83, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_82(...) KAIXO_TUPLE_DEF_83(__VA_ARGS__ _type<82> _83;) KAIXO_TUPLE_DEF_I(82, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_81(...) KAIXO_TUPLE_DEF_82(__VA_ARGS__ _type<81> _82;) KAIXO_TUPLE_DEF_I(81, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_80(...) KAIXO_TUPLE_DEF_81(__VA_ARGS__ _type<80> _81;) KAIXO_TUPLE_DEF_I(80, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_79(...) KAIXO_TUPLE_DEF_80(__VA_ARGS__ _type<79> _80;) KAIXO_TUPLE_DEF_I(79, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_78(...) KAIXO_TUPLE_DEF_79(__VA_ARGS__ _type<78> _79;) KAIXO_TUPLE_DEF_I(78, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_77(...) KAIXO_TUPLE_DEF_78(__VA_ARGS__ _type<77> _78;) KAIXO_TUPLE_DEF_I(77, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_76(...) KAIXO_TUPLE_DEF_77(__VA_ARGS__ _type<76> _77;) KAIXO_TUPLE_DEF_I(76, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_75(...) KAIXO_TUPLE_DEF_76(__VA_ARGS__ _type<75> _76;) KAIXO_TUPLE_DEF_I(75, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_74(...) KAIXO_TUPLE_DEF_75(__VA_ARGS__ _type<74> _75;) KAIXO_TUPLE_DEF_I(74, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_73(...) KAIXO_TUPLE_DEF_74(__VA_ARGS__ _type<73> _74;) KAIXO_TUPLE_DEF_I(73, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_72(...) KAIXO_TUPLE_DEF_73(__VA_ARGS__ _type<72> _73;) KAIXO_TUPLE_DEF_I(72, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_71(...) KAIXO_TUPLE_DEF_72(__VA_ARGS__ _type<71> _72;) KAIXO_TUPLE_DEF_I(71, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_70(...) KAIXO_TUPLE_DEF_71(__VA_ARGS__ _type<70> _71;) KAIXO_TUPLE_DEF_I(70, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_69(...) KAIXO_TUPLE_DEF_70(__VA_ARGS__ _type<69> _70;) KAIXO_TUPLE_DEF_I(69, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_68(...) KAIXO_TUPLE_DEF_69(__VA_ARGS__ _type<68> _69;) KAIXO_TUPLE_DEF_I(68, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_67(...) KAIXO_TUPLE_DEF_68(__VA_ARGS__ _type<67> _68;) KAIXO_TUPLE_DEF_I(67, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_66(...) KAIXO_TUPLE_DEF_67(__VA_ARGS__ _type<66> _67;) KAIXO_TUPLE_DEF_I(66, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_65(...) KAIXO_TUPLE_DEF_66(__VA_ARGS__ _type<65> _66;) KAIXO_TUPLE_DEF_I(65, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_64(...) KAIXO_TUPLE_DEF_65(__VA_ARGS__ _type<64> _65;) KAIXO_TUPLE_DEF_I(64, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_63(...) KAIXO_TUPLE_DEF_64(__VA_ARGS__ _type<63> _64;) KAIXO_TUPLE_DEF_I(63, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_62(...) KAIXO_TUPLE_DEF_63(__VA_ARGS__ _type<62> _63;) KAIXO_TUPLE_DEF_I(62, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_61(...) KAIXO_TUPLE_DEF_62(__VA_ARGS__ _type<61> _62;) KAIXO_TUPLE_DEF_I(61, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_60(...) KAIXO_TUPLE_DEF_61(__VA_ARGS__ _type<60> _61;) KAIXO_TUPLE_DEF_I(60, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_59(...) KAIXO_TUPLE_DEF_60(__VA_ARGS__ _type<59> _60;) KAIXO_TUPLE_DEF_I(59, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_58(...) KAIXO_TUPLE_DEF_59(__VA_ARGS__ _type<58> _59;) KAIXO_TUPLE_DEF_I(58, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_57(...) KAIXO_TUPLE_DEF_58(__VA_ARGS__ _type<57> _58;) KAIXO_TUPLE_DEF_I(57, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_56(...) KAIXO_TUPLE_DEF_57(__VA_ARGS__ _type<56> _57;) KAIXO_TUPLE_DEF_I(56, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_55(...) KAIXO_TUPLE_DEF_56(__VA_ARGS__ _type<55> _56;) KAIXO_TUPLE_DEF_I(55, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_54(...) KAIXO_TUPLE_DEF_55(__VA_ARGS__ _type<54> _55;) KAIXO_TUPLE_DEF_I(54, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_53(...) KAIXO_TUPLE_DEF_54(__VA_ARGS__ _type<53> _54;) KAIXO_TUPLE_DEF_I(53, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_52(...) KAIXO_TUPLE_DEF_53(__VA_ARGS__ _type<52> _53;) KAIXO_TUPLE_DEF_I(52, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_51(...) KAIXO_TUPLE_DEF_52(__VA_ARGS__ _type<51> _52;) KAIXO_TUPLE_DEF_I(51, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_50(...) KAIXO_TUPLE_DEF_51(__VA_ARGS__ _type<50> _51;) KAIXO_TUPLE_DEF_I(50, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_49(...) KAIXO_TUPLE_DEF_50(__VA_ARGS__ _type<49> _50;) KAIXO_TUPLE_DEF_I(49, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_48(...) KAIXO_TUPLE_DEF_49(__VA_ARGS__ _type<48> _49;) KAIXO_TUPLE_DEF_I(48, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_47(...) KAIXO_TUPLE_DEF_48(__VA_ARGS__ _type<47> _48;) KAIXO_TUPLE_DEF_I(47, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_46(...) KAIXO_TUPLE_DEF_47(__VA_ARGS__ _type<46> _47;) KAIXO_TUPLE_DEF_I(46, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_45(...) KAIXO_TUPLE_DEF_46(__VA_ARGS__ _type<45> _46;) KAIXO_TUPLE_DEF_I(45, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_44(...) KAIXO_TUPLE_DEF_45(__VA_ARGS__ _type<44> _45;) KAIXO_TUPLE_DEF_I(44, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_43(...) KAIXO_TUPLE_DEF_44(__VA_ARGS__ _type<43> _44;) KAIXO_TUPLE_DEF_I(43, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_42(...) KAIXO_TUPLE_DEF_43(__VA_ARGS__ _type<42> _43;) KAIXO_TUPLE_DEF_I(42, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_41(...) KAIXO_TUPLE_DEF_42(__VA_ARGS__ _type<41> _42;) KAIXO_TUPLE_DEF_I(41, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_40(...) KAIXO_TUPLE_DEF_41(__VA_ARGS__ _type<40> _41;) KAIXO_TUPLE_DEF_I(40, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_39(...) KAIXO_TUPLE_DEF_40(__VA_ARGS__ _type<39> _40;) KAIXO_TUPLE_DEF_I(39, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_38(...) KAIXO_TUPLE_DEF_39(__VA_ARGS__ _type<38> _39;) KAIXO_TUPLE_DEF_I(38, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_37(...) KAIXO_TUPLE_DEF_38(__VA_ARGS__ _type<37> _38;) KAIXO_TUPLE_DEF_I(37, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_36(...) KAIXO_TUPLE_DEF_37(__VA_ARGS__ _type<36> _37;) KAIXO_TUPLE_DEF_I(36, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_35(...) KAIXO_TUPLE_DEF_36(__VA_ARGS__ _type<35> _36;) KAIXO_TUPLE_DEF_I(35, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_34(...) KAIXO_TUPLE_DEF_35(__VA_ARGS__ _type<34> _35;) KAIXO_TUPLE_DEF_I(34, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_33(...) KAIXO_TUPLE_DEF_34(__VA_ARGS__ _type<33> _34;) KAIXO_TUPLE_DEF_I(33, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_32(...) KAIXO_TUPLE_DEF_33(__VA_ARGS__ _type<32> _33;) KAIXO_TUPLE_DEF_I(32, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_31(...) KAIXO_TUPLE_DEF_32(__VA_ARGS__ _type<31> _32;) KAIXO_TUPLE_DEF_I(31, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_30(...) KAIXO_TUPLE_DEF_31(__VA_ARGS__ _type<30> _31;) KAIXO_TUPLE_DEF_I(30, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_29(...) KAIXO_TUPLE_DEF_30(__VA_ARGS__ _type<29> _30;) KAIXO_TUPLE_DEF_I(29, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_28(...) KAIXO_TUPLE_DEF_29(__VA_ARGS__ _type<28> _29;) KAIXO_TUPLE_DEF_I(28, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_27(...) KAIXO_TUPLE_DEF_28(__VA_ARGS__ _type<27> _28;) KAIXO_TUPLE_DEF_I(27, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_26(...) KAIXO_TUPLE_DEF_27(__VA_ARGS__ _type<26> _27;) KAIXO_TUPLE_DEF_I(26, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_25(...) KAIXO_TUPLE_DEF_26(__VA_ARGS__ _type<25> _26;) KAIXO_TUPLE_DEF_I(25, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_24(...) KAIXO_TUPLE_DEF_25(__VA_ARGS__ _type<24> _25;) KAIXO_TUPLE_DEF_I(24, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_23(...) KAIXO_TUPLE_DEF_24(__VA_ARGS__ _type<23> _24;) KAIXO_TUPLE_DEF_I(23, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_22(...) KAIXO_TUPLE_DEF_23(__VA_ARGS__ _type<22> _23;) KAIXO_TUPLE_DEF_I(22, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_21(...) KAIXO_TUPLE_DEF_22(__VA_ARGS__ _type<21> _22;) KAIXO_TUPLE_DEF_I(21, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_20(...) KAIXO_TUPLE_DEF_21(__VA_ARGS__ _type<20> _21;) KAIXO_TUPLE_DEF_I(20, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_19(...) KAIXO_TUPLE_DEF_20(__VA_ARGS__ _type<19> _20;) KAIXO_TUPLE_DEF_I(19, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_18(...) KAIXO_TUPLE_DEF_19(__VA_ARGS__ _type<18> _19;) KAIXO_TUPLE_DEF_I(18, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_17(...) KAIXO_TUPLE_DEF_18(__VA_ARGS__ _type<17> _18;) KAIXO_TUPLE_DEF_I(17, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_16(...) KAIXO_TUPLE_DEF_17(__VA_ARGS__ _type<16> _17;) KAIXO_TUPLE_DEF_I(16, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_15(...) KAIXO_TUPLE_DEF_16(__VA_ARGS__ _type<15> _16;) KAIXO_TUPLE_DEF_I(15, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_14(...) KAIXO_TUPLE_DEF_15(__VA_ARGS__ _type<14> _15;) KAIXO_TUPLE_DEF_I(14, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_13(...) KAIXO_TUPLE_DEF_14(__VA_ARGS__ _type<13> _14;) KAIXO_TUPLE_DEF_I(13, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_12(...) KAIXO_TUPLE_DEF_13(__VA_ARGS__ _type<12> _13;) KAIXO_TUPLE_DEF_I(12, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_11(...) KAIXO_TUPLE_DEF_12(__VA_ARGS__ _type<11> _12;) KAIXO_TUPLE_DEF_I(11, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_10(...) KAIXO_TUPLE_DEF_11(__VA_ARGS__ _type<10> _11;) KAIXO_TUPLE_DEF_I(10, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_9(...) KAIXO_TUPLE_DEF_10(__VA_ARGS__ _type<9> _10;) KAIXO_TUPLE_DEF_I(9, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_8(...) KAIXO_TUPLE_DEF_9(__VA_ARGS__ _type<8> _9;) KAIXO_TUPLE_DEF_I(8, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_7(...) KAIXO_TUPLE_DEF_8(__VA_ARGS__ _type<7> _8;) KAIXO_TUPLE_DEF_I(7, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_6(...) KAIXO_TUPLE_DEF_7(__VA_ARGS__ _type<6> _7;) KAIXO_TUPLE_DEF_I(6, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_5(...) KAIXO_TUPLE_DEF_6(__VA_ARGS__ _type<5> _6;) KAIXO_TUPLE_DEF_I(5, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_4(...) KAIXO_TUPLE_DEF_5(__VA_ARGS__ _type<4> _5;) KAIXO_TUPLE_DEF_I(4, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_3(...) KAIXO_TUPLE_DEF_4(__VA_ARGS__ _type<3> _4;) KAIXO_TUPLE_DEF_I(3, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_2(...) KAIXO_TUPLE_DEF_3(__VA_ARGS__ _type<2> _3;) KAIXO_TUPLE_DEF_I(2, __VA_ARGS__)
#define KAIXO_TUPLE_DEF_1(...) KAIXO_TUPLE_DEF_2(__VA_ARGS__ _type<1> _2;) KAIXO_TUPLE_DEF_I(1, __VA_ARGS__)
#define KAIXO_TUPLE_DEF KAIXO_TUPLE_DEF_1(_type<0> _1;) 

KAIXO_TUPLE_DEF;

int main() {

    tuple<
        int, int, int, int, int, int, int, int, int, int,
        int, int, int, int, int, int, int, int, int, int,
        int, int, int, int, int, int, int, int, int, int,
        int, int, int, int, int, int, int, int, int, int,
        int, int, int, int, int, int, int, int, int, int,
        int, int, int, int, int, int, int, int, int, int,
        int, int, int, int, int, int, int, int, int, int,
        int, int, int, int, int, int, int, int, int, int,
        int, int, int, int, int, int, int, int, int, int,
        int, int, int, int, int, int, int, int, int, int
    > my_tuple;

    my_tuple._1;
    my_tuple._39;
    my_tuple._99;


    constexpr std::tuple a{ 1, 2, 3 };
    constexpr std::tuple b{ 1, 2, 3 };
    constexpr std::tuple c{ 1, 2, 3 };

    auto oiane = concat_v(a, b, c);

    constexpr std::tuple vals{ 1, 2., 3.f, 4u, 5ll, 6ull, 7l };
    constexpr auto res1 = vals | filter_v<is_integral> | remove_v<long, long long>;
    constexpr auto res2 = vals | filter_v<is_floating_point> | reverse_v;
    constexpr auto res3 = zip_v(res1, res2);
    constexpr auto res4 = zip_v(res2, res1);

    static_assert((res3 | get_v<0> | get_v<0>) == 1);
    static_assert((res3 | get_v<0> | get_v<1>) == 3.f);
    static_assert((res3 | get_v<1> | get_v<0>) == 4u);
    static_assert((res3 | get_v<1> | get_v<1>) == 2.);

    int q = 1;
    int r = 2;
    template_pack<int, int&> g{ q, r };

    auto aein1 = g | append_v(1);

    return 0;
}