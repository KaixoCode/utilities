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


#include <variant>
#include <map>
#include <vector>
#include <string>

template<class Ty, class ...Tys>
concept OneOf = (std::same_as<Ty, Tys> || ...);

class Json {
public:
    using Floating = double;
    using Integral = int64_t;
    using Unsigned = uint64_t;
    using String = std::string;
    using Boolean = bool;
    using Array = std::vector<Json>;
    using Object = std::map<String, Json, std::less<void>>;
    using Null = std::nullptr_t;

private:
    using JsonValue = std::variant<Floating, Integral, Unsigned, String, Boolean, Array, Object, Null>;
    struct Type { enum { Floating, Integral, Unsigned, String, Boolean, Array, Object, Null }; };
    JsonValue m_Value;

    template<class Ty> struct Alias { using Type = Ty; };
    template<std::signed_integral Ty> struct Alias<Ty> { using Type = Integral; };
    template<std::unsigned_integral Ty> struct Alias<Ty> { using Type = Unsigned; };

public:
    template<class Ty = Null>
    Json(const Ty& ty = {}) : m_Value(static_cast<Alias<Ty>::Type>(ty)) {}

    template<class Ty> Ty get() const { return static_cast<Ty>(std::get<typename Alias<Ty>::Type>(m_Value)); }
    template<class Ty> Ty& ref() { return std::get<Ty>(m_Value); }
    template<class Ty> const Ty& ref() const { return std::get<Ty>(m_Value); }

    Json& operator[](std::string_view index)
    {
        if (m_Value.index() == Type::Null) m_Value = Object{};
        else if (m_Value.index() != Type::Object) throw std::exception("Not an object.");
        auto _it = ref<Object>().find(index);
        if (_it == ref<Object>().end()) return ref<Object>()[std::string{ index }];
        else return _it->second;
    }
};

template<class Ty> concept attribute = requires(Ty) { typename Ty::is_attribute; };
template<class Ty> struct is_attribute_impl { constexpr static bool value = attribute<Ty>; };

constexpr auto is_attribute = type_trait<is_attribute_impl>{};

template<class Ty, class ...Attributes>
struct member_attributes {
    using attributes = info<Attributes...>;
    using type = Ty;
};

template<class ...Tys>
struct members_with_attributes : info<Tys...> {
    template<std::size_t I>
    using member = info<Tys...>::template element<I>::type;
};

template<class Ty>
struct attributes_from_struct {
    using _members = struct_members_t<Ty>;
    constexpr static auto _indices = _members::template indices_filter<not is_attribute>;
    
    template<std::size_t ...Is> struct helper1 {
        using type = info<typename _members::template element<Is>::type...>;
    };
    template<class> struct helper2;
    template<std::size_t ...Is> struct helper2<std::index_sequence<Is...>> {
        template<std::size_t I>
        using _one = typename array_to_pack_t<
            generate_indices_v<I == 0 ? 0 : _indices.at(I - 1) + 1, _indices.at(I)>, helper1>::type
            ::template prepend<typename _members::template element<_indices.at(I)>::type>
            ::template as<member_attributes>;
        using type = members_with_attributes<_one<Is>...>;
    };

    using type = helper2<std::make_index_sequence<_indices.size()>>::type;
};

template<class Ty>
using attributes_from_struct_t = typename attributes_from_struct<Ty>::type;

template<class Ty> struct member_offsets {
    constexpr static auto value = [] {
        using members = struct_members_t<decay_t<Ty>>;
        std::array<std::size_t, members::size> _offsets{};
        indexed_for<members::size>([&]<std::size_t I>{
            if constexpr (I == 0) _offsets[0] = 0;
            else if constexpr (I != 0) {
                using prev = typename members::template element<I - 1>::type;
                using curr = typename members::template element<I>::type;

                std::size_t off = _offsets[I - 1] + sizeof_v<prev>;
                std::size_t align = alignof_v<curr>;

                _offsets[I] = next_multiple(off, align);
            }
        });
        return _offsets;
    }();
};

template<aggregate Ty>
constexpr auto get_member_ptrs() {
    using members = struct_members_t<decay_t<Ty>>;
    using member_pointers = members::template to_member_pointer<decay_t<Ty>>;
    constexpr auto _indices = members::template indices_filter<not is_attribute>;
    constexpr auto offsets = member_offsets<decay_t<Ty>>::value;
    return iterate<_indices>([&]<std::size_t...Is>{
        return std::tuple{
            std::bit_cast<typename member_pointers::template element<Is>::type>
                (static_cast<std::uint32_t>(offsets[Is]))...
        };
    });
}

struct json_property_flag {};
template<string_literal Name> struct json_property {
    using is_attribute = json_property_flag;
    constexpr static std::string_view name = Name.view();
};

template<class Ty> concept json_property_attribute = requires(Ty) { std::same_as<typename Ty::is_attribute, json_property_flag>; };
template<class Ty> struct is_json_property_attribute_impl { constexpr static bool value = json_property_attribute<Ty>; };
constexpr type_trait<is_json_property_attribute_impl> is_json_property{};

#define JsonPropertyName(NAME) no_unique_address]] json_property<NAME> KAIXO_MERGE(_json_property, __COUNTER__){}; [[

template<aggregate Ty>
Json to_json(const Ty& value) {
    static const auto member_pointers = get_member_ptrs<decay_t<Ty>>();
    using members = attributes_from_struct_t<decay_t<Ty>>;
    
    Json _result{};
    indexed_for<members::size>([&]<std::size_t I>{
        using member = members::template member<I>;
        using type = member::type;
        using attributes = member::attributes;
        using find_json_property = attributes::template filter<is_json_property>;
        if constexpr (find_json_property::size != 0) {
            constexpr std::string_view name = find_json_property::type::name;
            _result[name] = (value.*std::get<I>(member_pointers));
        }
    });
    return _result;
}

template<aggregate Ty>
Ty from_json(Json& json) {
    static const auto member_pointers = get_member_ptrs<decay_t<Ty>>();
    using members = attributes_from_struct_t<decay_t<Ty>>;
    
    Ty _result{};
    indexed_for<members::size>([&]<std::size_t I>{
        using member = members::template member<I>;
        using type = member::type;
        using attributes = member::attributes;
        using find_json_property = attributes::template filter<is_json_property>;
        if constexpr (find_json_property::size != 0) {
            constexpr std::string_view name = find_json_property::type::name;
            (_result.*std::get<I>(member_pointers)) = json[name].get<type>();
        }
    });
    return _result;
}

struct Person {
    [[JsonPropertyName("age")]]
    int age;

    [[JsonPropertyName("name")]]
    std::string name;

    int carrot = 0;
    std::vector<int> data{};

    [[JsonPropertyName("height")]]
    double height;
};

int main() {

    Person _person{
        .age = 19,
        .name = "Jimmy",
        .height = 167,
    };

    Json _json = to_json(_person);








    Person _value = from_json<Person>(_json);




    return 0;
}