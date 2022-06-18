#include <string_view>
#include <utility>
#include <algorithm>
#include <charconv>
#include <stdexcept>

#include <string>
#include <iostream>

#include <memory>

#include "utils.hpp"


template<class Ty>
struct get_t {
    template<class Access>
    constexpr get_t(Access access) 
        : data(std::bit_cast<void*>(access)),
        access(&access_impl<Access>) {}

    constexpr get_t(Ty& val)
        : data(&val), access(&access_impl2) {}

    constexpr get_t()
        : data(nullptr), access(access_impl2) {}

    void* data;
    Ty(*access)(void*);

    template<class Access>
    constexpr static Ty access_impl(void* data) {
        return std::bit_cast<Access>(data)();
    }

    constexpr static Ty access_impl2(void* data) {
        return *static_cast<Ty*>(data);
    }
};

template<class Ty>
struct set_t {
    template<class Access>
    constexpr set_t(Access access)
        : data(std::bit_cast<void*>(access)),
        access(&access_impl<Access>) {}

    constexpr set_t(Ty& val) 
        : data(&val), access(&access_impl2) {}

    constexpr set_t() 
        : data(nullptr), access(access_impl2) {}

    void* data;
    void(*access)(void*, Ty);

    template<class Access>
    constexpr static void access_impl(void* data, Ty val) {
        (*reinterpret_cast<Access*>(data))(val);
    }

    constexpr static void access_impl2(void* data, Ty val) {
        *static_cast<Ty*>(data) = val;
    }
};

template<class Ty, class Me = void>
struct alias {
    constexpr alias& operator=(Ty val) { set.access(set.data, val); return *this; }
    constexpr operator Ty() const { return get.access(get.data); }

    get_t<Ty> get;
    set_t<Ty> set;

    friend Me;
};

template<class Ty, class Me = void>
struct readonly {
    constexpr operator Ty() const { return get.access(get.data); }

    get_t<Ty> get;
    set_t<Ty> set;

private:
    constexpr readonly& operator=(Ty val) { set.access(set.data, val); return *this; }
    friend Me;
};

template<class Ty, class Me = void>
struct writeonly {
    constexpr writeonly& operator=(Ty val) { set.access(set.data, val); return *this; }

    get_t<Ty> get;
    set_t<Ty> set;

private:
    constexpr operator Ty() const { return get.access(get.data); }
    friend Me;
};


#include <numeric>
#include <vector>
#include <array>


template<class M>
struct extension_method {
    using Method = M;
    consteval extension_method(Method) {}

    template<class ...Args>
    struct arguments {
        std::tuple<Args...> args;

        template<class Self>
        constexpr auto call(Self&& me) const {
            return std::apply(Method{}, std::tuple_cat(
                std::forward_as_tuple(std::forward<Self>(me)), std::move(args)));
        }

        constexpr arguments&& operator-(this auto&& self) { return std::move(self); }
        
        template<class Self> requires std::invocable<Method, Self, Args...>
        constexpr friend auto operator<(Self&& self, arguments&& args) {
            return std::forward<arguments>(args).call(std::forward<Self>(self));
        }
    };

    template<class ...Args>
    constexpr auto operator()(Args&&...args) const {
        return arguments<Args&&...>{ std::forward_as_tuple(std::forward<Args>(args)...) };
    }
};

constexpr extension_method contains = 
[]<class Ty> (Ty const& container, typename Ty::value_type const& value) {
    return std::find(container.begin(), container.end(), value) != container.end();
};

constexpr extension_method to_string = 
[]<class Ty>(Ty&& val) {
    return std::to_string(std::forward<Ty>(val));
};

#include <functional>

template<class Ty> struct to_arg { using type = Ty&&; };
template<class Ty> struct to_arg<Ty&> { using type = Ty&; };
template<class Ty> using to_arg_t = typename to_arg<Ty>::type;

constexpr static std::size_t _small_Storage = 16;
template<class Callable>
constexpr bool _is_Big = sizeof(Callable) >
(_small_Storage - sizeof(_del_Base<void>) - 1);

template<class R, class ...As>
struct _del_Base {
    _del_Base* (*_move_Impl)(void*, void*);
    _del_Base* (*_copy_Impl)(const void*, void*);
    void(*_delete_Impl)(void*);
    R(*_call_Impl)(void*, As...);

    _del_Base* _move(void* at) { return _move_Impl(this, at); }
    _del_Base* _copy(void* at) const { return _copy_Impl(this, at); }
    void _delete() { _delete_Impl(this); }
    R _call(As...as) { return _call_Impl(this, std::forward<As>(as)...); }
};

template<class Callable, class R, class ...As>
struct _del_Impl : _del_Base<R, As...> {
    constexpr static bool _is_Big = _is_Big<Callable>;
    Callable _fun;

    static auto _self(void* self) { return static_cast<_del_Impl*>(self); }
    static auto _self(const void* self) { return static_cast<const _del_Impl*>(self); }

    template<class Arg> static auto _construct_At(Arg&& arg, void* at) {
        if constexpr (_is_Big) return new _del_Impl{ std::forward<Arg>(arg) };
        else return new (at) _del_Impl{ std::forward<Arg>(arg) };
    };

    template<class Arg> _del_Impl(Arg&& callable) : _fun(std::forward<Arg>(_fun)) {
        this->_move_Impl = [](void* self, void* to) -> _del_Base<R, As...>*{
            return _construct_At(std::move(_self(self)->_fun), to); 
        };
        this->_copy_Impl = [](const void* self, void* to) -> _del_Base<R, As...>* {
            return _construct_At(_self(self)->_fun, to); 
        };
        this->_delete_Impl = [](void* self) {
            if constexpr (_is_Big) delete _self(self);
            else _self(self)->~_del_Impl(); 
        };
        this->_call_Impl = [](void* self, As...as) -> R {
            return _self(self)->_fun(std::forward<As>(as)...);
        };
    }
};

template<class> struct delegate;
template<class R, class ...As>
struct delegate<R(As...)> {
    using _impl = _del_Base<R, As...>;

    template<class Functor> requires (!std::same_as<delegate, std::decay_t<Functor>>)
    delegate(Functor&& fun) { _reset(std::forward<Functor>(fun)); }
    delegate(const delegate& val) { _copy(val); }
    delegate(delegate&& val) noexcept { _move(std::move(val)); }
    ~delegate() { _tidy(); }

    auto& operator=(const delegate& val) { _copy(val); return *this; }
    auto& operator=(delegate&& val) { if (&val != this) _move(val); return *this; }

    R operator()(As... as) {
        return _get_Impl()->_call(std::forward<As>(as)...);
    }

    explicit operator bool() const { return !_empty(); }

protected:
    template<class Functor>
    void _reset(Functor&& fun) {
        using _impl_Type = _del_Impl<std::decay_t<Functor>, R, As...>;
        if constexpr (_is_Big<Functor>) _set(new _impl_Type{ fun });
        else _set(new (&_data) _impl_Type{ fun });
    }

    void _set(_impl* impl) { _data[_small_Storage - 1] = impl; }
    _impl* _get_Impl() const { return _data[_small_Storage - 1]; }
    bool _empty() const { return !_get_Impl(); }
    void _tidy() { if (!_empty()) _get_Impl()->_delete(), _set(nullptr); }
    
    void _copy(const delegate& val) { 
        if (!val._empty()) _set(val._get_Impl()->_copy(&_data));
    }

    void _move(delegate&& val) {
        if (!val._empty()) {
            _set(val._get_Impl()->_move(&_data));
            val._tidy();
        }
    }

    _impl* _data[_small_Storage];
};


#include "string_literal.hpp"
#include "utils.hpp"
#include "pack_utils.hpp"

//template<class T> struct add_any_arg { using type = T(std::any&); };
//template<class R, class ...As> 
//struct add_any_arg<R(As...)> { using type = R(std::any&, As...); };
//template<class Ty> using add_any_arg_t = typename add_any_arg<Ty>::type;
//template<class T> struct get_ret_type { using type = T; };
//template<class R, class ...As> 
//struct get_ret_type<R(As...)> { using type = R; };
//template<class Ty> using get_ret_type_t = typename get_ret_type<Ty>::type;
//
//template<class Ty>
//concept has_signature = requires() {
//    typename Ty::signature;
//};
//
//template<class Ty> 
//struct type_impl {
//    using type = typename Ty::type&;
//};
//
//template<class Ty> requires has_signature<Ty>
//struct type_impl<Ty> {
//    using type = typename Ty::signature;
//};
//
//template<class Fun>
//using type_impl_t = typename type_impl<Fun>::type;
//
//template<class Ty>
//struct member {
//    member(const member&) = delete;
//    member(member&&) = default;
//    member& operator=(const member&) = delete;
//    member& operator=(member&&) = default;
//    
//    member(Ty& val) : _value(&val) {}
//
//    operator Ty& () { return *_value; }
//    operator Ty const& () const { return *_value; }
//    template<std::convertible_to<Ty> Arg>
//    member& operator=(Arg&& v) { return (*_value = std::forward<Arg>(v), *this); }
//
//private:
//    Ty* _value;
//};
//
//template<class ...Funs>
//class interface {
//    using vtable = std::tuple<add_any_arg_t<type_impl_t<Funs>>*...>;
//public:
//    interface(const interface&) = delete;
//    interface(interface&&) = default;
//    interface& operator=(const interface&) = delete;
//    interface& operator=(interface&&) = default;
//    
//    template<class Ty>
//    interface(Ty& value) : _storage(std::ref(value)), _vtable{
//        [](std::any& val, auto ...args) -> get_ret_type_t<type_impl_t<Funs>> {
//            if constexpr (has_signature<Funs>) {
//                constexpr auto v = &Funs::template call<Ty>;
//                return v(std::any_cast<std::reference_wrapper<Ty>&>(val).get(), args...);
//            } else {
//                constexpr auto v = &Funs::template get<Ty>;
//                return v(std::any_cast<std::reference_wrapper<Ty>&>(val).get(), args...);
//            }
//        }... } {}
//
//    template<class Ty>
//    interface(Ty&& value) : _storage(std::forward<Ty>(value)), _vtable{
//        [](std::any& val, auto ...args) -> get_ret_type_t<type_impl_t<Funs>> {
//            if constexpr (has_signature<Funs>) {
//                constexpr auto v = &Funs::template call<Ty>;
//                return v(std::any_cast<Ty&>(val), args...);
//            } else {
//                constexpr auto v = &Funs::template get<Ty>;
//                return v(std::any_cast<Ty&>(val), args...);
//            }
//        }... } {}
//
//    template<class Fun, class ...Args> decltype(auto) call(Args&& ...args) const {
//        using type = add_any_arg_t<type_impl_t<Fun>>*;
//        return std::get<type>(const_cast<vtable&>(_vtable))(
//            const_cast<std::any&>(_storage), std::forward<Args>(args)...);
//    }
//
//    template<class Fun> auto get() -> decltype(call<Fun>()) {
//        return call<Fun>();
//    }
//
//private:
//    std::any _storage;
//    vtable _vtable;
//};
//
//struct DrawContext { /* ... */ };
//struct Point {
//    double x;
//    double y;
//};
//
//struct draw_fun {
//    using signature = void(DrawContext&);
//    void call(this auto& self, DrawContext& c) { self.draw(c); }
//};
//
//struct update_fun {
//    using signature = void();
//    void call(this auto& self) { self.update(); }
//};
//
//struct hitbox_fun {
//    using signature = bool(const Point&);
//    bool call(this const auto& self, const Point& p) { return self.hitbox(p); }
//};
//
//struct value_mem {
//    using type = int;
//    int& get(this auto& self) { return self.value; }
//};
//
//struct GuiObject : interface<
//    draw_fun, 
//    update_fun,
//    hitbox_fun,
//    value_mem
//> {
//    using interface::interface;
//
//    void draw(DrawContext& v) { call<draw_fun>(v); }
//    void update() { call<update_fun>(); }
//    bool hitbox(const Point& p) { return call<hitbox_fun>(p); }
//    member<int> value = get<value_mem>();
//};
//
//struct CustomGuiObject {
//    int value = 1;
//    void draw(DrawContext& v) { std::cout << value << ": drawing...\n"; }
//    void update() { std::cout << value << ": updating...\n"; }
//    bool hitbox(const Point& p) const {
//        std::cout << value << ": hitbox test: [" << p.x << ", " << p.y << "]\n";
//        return false;
//    }
//};
//
//void setValue(GuiObject obj, int v) {
//    obj.value = v;
//}


//namespace odl {
//    template<class Ty>
//    const std::type_info* stored_type(const Ty& val) { return &typeid(Ty); }
//
//    const std::type_info* stored_type(const std::any& val) {
//        return &val.type();
//    }
//
//    template<class...Args>
//    const std::type_info* stored_type(const std::variant<Args...>& val) {
//        constexpr const std::type_info* _infos[sizeof...(Args)]{ &typeid(Args)... };
//        const auto _index = val.index();
//        return _index == std::variant_npos ? nullptr : _infos[_index];
//    }
//
//    template<class Ty>
//    Ty cast_to(std::any& val) { return std::any_cast<Ty>(val); }
//
//    template<class Ty, class ...Args>
//    Ty cast_to(std::variant<Args...>& val) { return std::get<Ty>(val); }
//
//    namespace match_literal {
//        constexpr struct wildcard {
//            template<class Arg>
//            constexpr bool operator==(Arg&&) const { return true; }
//        } _;
//
//        template<class Ty>
//        struct match_type {
//            template<class Arg>
//            constexpr bool operator==(Arg&& arg) const {
//                const std::type_info& _info = typeid(Ty);
//                const std::type_info* ty = stored_type(std::forward<Arg>(arg));
//                return ty && _info == *ty;
//            }
//        };
//
//        template<class Ty> constexpr match_type<Ty> t{};
//    }
//    template<class Handler, class...Args >
//    struct match_case_handler;
//
//    template<class A, class B>
//    concept equaliable = requires(A a, B b) {
//        { a == b } -> std::convertible_to<bool>;
//    };
//
//    template<class A, class B>
//    concept castable_to = requires(B b) {
//        { cast_to<A>(b) } -> std::same_as<A>;
//    };
//
//    template<class...Args>
//    struct match_case {
//        std::tuple<Args&&...> args;
//
//        constexpr match_case(Args&& ...args)
//            : args(std::forward_as_tuple(std::forward<Args>(args)...)) {}
//
//        template<class ...Tys>
//        constexpr bool operator==(match_case<Tys...>& other) {
//            if constexpr (sizeof...(Tys) != sizeof...(Args)) return false;
//            else return[&]<std::size_t ...Is>(std::index_sequence<Is...>) {
//                constexpr auto _compare = [](auto& a, auto& b) {
//                    if constexpr (equaliable<decltype(a), decltype(b)>) {
//                        return a == b;
//                    }
//                    else if constexpr (castable_to<decltype(a), decltype(b)>) {
//                        const std::type_info& _infoa = typeid(a);
//                        const std::type_info* _infob = stored_type(b);
//                        if (_infob && _infoa == *_infob) {
//                            decltype(auto) v = cast_to<decltype(a)>(b);
//                            if constexpr (equaliable<decltype(a), decltype(v)>) return a == v;
//                        }
//                    }
//                    return false;
//                };
//
//                return ((_compare(std::get<Is>(args), std::get<Is>(other.args))) && ...);
//            }(std::index_sequence_for<Args...>{});
//        }
//
//        template<class Handler>
//        constexpr match_case_handler<Handler, Args...>
//            operator =(this match_case&& a, Handler&& b) {
//            return { std::move(b), std::move(a) };
//        }
//    };
//
//    template<class ...Args>
//    match_case(Args&&...)->match_case<Args...>;
//
//    template<class ...Args>
//    struct l : match_case<Args...> {
//        using match_case<Args...>::match_case;
//        using match_case<Args...>::operator=;
//        using match_case<Args...>::operator==;
//    };
//
//    template<class ...Args>
//    l(Args&&...)->l<Args...>;
//
//    template<class Handler, class...Args>
//    struct match_case_handler {
//        constexpr static bool takes_args =
//            kaixo::has_fun_op<Handler> && !std::invocable<Handler>;
//        Handler handler;
//        match_case<Args...> args;
//
//        template<class ...Args>
//        constexpr bool can_call(match_case<Args...>& arg) {
//            if constexpr (takes_args) {
//                using fargs = kaixo::function_args_t<kaixo::minimal_signature_t<Handler>>;
//                return[&]<std::size_t ...Is>(std::index_sequence<Is...>) {
//
//                    constexpr auto _compare = [](auto a, auto b) {
//                        return b && *a == *b;
//                    };
//
//                    if (!(_compare(&typeid(std::tuple_element_t<Is, fargs>),
//                        stored_type(std::get<Is>(arg.args))) && ...)) return false;
//
//                    constexpr auto _equal = []<std::size_t I>(auto & a, auto & b) {
//                        if constexpr (castable_to<std::tuple_element_t<I, fargs>, decltype(a)>) {
//                            decltype(auto) v = cast_to<std::tuple_element_t<I, fargs>>(a);
//                            if constexpr (equaliable<decltype(v), decltype(b)>) return v == b;
//                            else return false;
//                        }
//                        if constexpr (equaliable<decltype(a), decltype(b)>) return a == b;
//                        else return false;
//                    };
//
//                    return (_equal.operator() < Is > (std::get<Is>(arg.args), std::get<Is>(args.args)) && ...);
//
//                }(std::index_sequence_for<Args...>{});
//            }
//            else { return args == arg; }
//        }
//
//        template<class ...Args>
//        constexpr auto execute(match_case<Args...>& arg) const {
//            if constexpr (std::invocable<Handler>) return handler();
//            else if constexpr (takes_args) {
//                using args = kaixo::function_args_t<kaixo::minimal_signature_t<Handler>>;
//                return[&]<std::size_t ...Is>(std::index_sequence<Is...>) {
//                    return handler(cast_to<std::tuple_element_t<Is, args>>(std::get<Is>(arg.args))...);
//                }(std::index_sequence_for<Args...>{});
//            }
//            else return handler;
//        }
//    };
//
//    constexpr struct match_brackets {
//        template<class ...Args>
//        constexpr match_case<Args...> operator()(Args&&...args) const {
//            return match_case<Args...>{ std::forward<Args>(args)... };
//        }
//    } m;
//
//    template<class ...Handlers>
//    struct match {
//        std::tuple<Handlers&&...> handlers;
//
//        constexpr match(Handlers&&...handlers)
//            : handlers(std::forward_as_tuple(std::forward<Handlers>(handlers)...)) {}
//
//        template<class ...Args>
//        constexpr auto check(match_case<Args...>&& arg) {
//            return[&]<std::size_t Is>(this auto && self) {
//                auto& handler = std::get<Is>(handlers);
//                using type = decltype(handler.execute(arg));
//                if (handler.can_call(arg))
//                    return handler.execute(arg);
//                else if constexpr (Is == sizeof...(Handlers) - 1) {
//                    if constexpr (std::same_as<type, void>) return;
//                    else return type{};
//                }
//                else return std::move(self).operator() < Is + 1 > ();
//            }.operator() < 0 > ();
//        }
//    };
//
//    template<class ...Args>
//    match(Args&&...)->match<Args...>;
//
//    template<class ...Handlers, class ...Args>
//    constexpr auto operator >>(match_case<Args...>&& a, match<Handlers...>&& b) {
//        return b.check(std::move(a));
//    }
//}

#include <optional>
#include <any>
#include <variant>

namespace kaixo {
    // Get actual stored type, overloaded on things like std::any and 
    // std::variant to get internally stored type at runtime.
    template<class Ty>
    const std::type_info& type(Ty&&) { return typeid(Ty); }
    const std::type_info& type(std::any& v) { return v.type(); }
    const std::type_info& type(const std::any& v) { return v.type(); }
    template<class ...Args>
    const std::type_info& type(std::variant<Args...>& v) {
        constexpr const std::type_info* _infos[sizeof...(Args)]{ &typeid(Args)... };
        return *_infos[v.index()];
    }
    template<class ...Args>
    const std::type_info& type(const std::variant<Args...>& v) {
        constexpr const std::type_info* _infos[sizeof...(Args)]{ &typeid(Args)... };
        return *_infos[v.index()];
    }

    // General cast, also overloaded on things like std::any and std::variant 
    // so there's a general way to cast those kinds of types to another type
    template<class Ty, class Arg> constexpr Ty general_cast(Arg&& arg) { return static_cast<Ty>(std::forward<Arg>(arg)); }
    template<class Ty> Ty general_cast(std::any& v) { return std::any_cast<Ty>(v); }
    template<class Ty> Ty general_cast(const std::any& v) { return std::any_cast<const Ty>(v); }
    template<class Ty, class ...As> Ty general_cast(std::variant<As...>& v) { return std::get<Ty>(v); }
    template<class Ty, class ...As> Ty general_cast(const std::variant<As...>& v) { return std::get<const Ty>(v); }

    template<class A, class B> // Simple test if equality operator exists
    concept comparable = requires(A a, B b) { { a == b } -> std::convertible_to<bool>; };

    // Compare 2 tuples, if not directly comparable, it tries the general cast, 
    // it tries casting the types in 'a' to the types in 'b'.
    template<class ...As, class ...Bs>
    constexpr bool compare_tuples(std::tuple<As...>& a, std::tuple<Bs...>& b) {

        // If not same size, can't be equal
        if constexpr (sizeof...(Bs) != sizeof...(As)) return false;

        // Otherwise go over all arguments recursively
        else return[&]<std::size_t I>(this auto&& self) {
            if constexpr (I == sizeof...(As)) return true; // base case
            else { // Get both types
                using A = decltype(std::get<I>(a));
                using B = decltype(std::get<I>(b));

                if constexpr (comparable<A, B>) return // Just compare and recurse
                    std::get<I>(a) == std::get<I>(b) && self.operator()<I + 1>();

                else { // Otherwise try casting
                    // First check if stored type in As match up with Bs, 
                    // to make sure we can actually cast to Bs
                    if (type(std::get<I>(a)) != typeid(B)) return false;
                    return general_cast<B>(std::get<I>(a)) == std::get<I>(b)
                        && self.operator()<I + 1>(); // And recurse
                }
            }
        }.operator()<0>();
    }

    template<class Body, class ...As> struct match_case;
    namespace match_types {
        struct wildcard { // Wildcard is a simple equality operator that always returns true
            template<class Arg> constexpr bool operator==(Arg&&) { return true; }
        } _;

        template<class Ty>
        struct range {
            Ty start;
            Ty end;

            template<class Arg>
            constexpr bool operator==(Arg& arg) {
                if constexpr (requires(Ty a, Arg && b) { 
                    { b >= a && b < a } -> std::convertible_to<bool>; })
                    return arg >= start && arg < end;
                else return false;
            }
        };

        template<class...As> struct l : std::tuple<As&&...> {
            using std::tuple<As&&...>::tuple;

            template<class Body> constexpr match_case<Body&&, As...>
            operator=(this l&& self, Body&& b) { return { std::move(self), std::forward<Body>(b) }; }

            constexpr std::tuple<As&&...>& tuple() { return *this; }
        };
        template<class ...Args> l(Args&&...)->l<Args...>;

        template<class...As> struct l<std::tuple<As...>&> {
            std::tuple<As...>& vals;

            template<class Body> constexpr match_case<Body&&, std::tuple<As...>&>
            operator=(this l&& self, Body&& b) { return { std::move(self), std::forward<Body>(b) }; }

            constexpr std::tuple<As...>& tuple() { return vals; }
        };
        template<class ...Args> l(std::tuple<Args...>&)->l<std::tuple<Args...>&>;

        template<class...As> struct l<const std::tuple<As...>&> {
            const std::tuple<As...>& vals;

            template<class Body> constexpr match_case<Body&&, const std::tuple<As...>&>
            operator=(this l&& self, Body&& b) { return { std::move(self), std::forward<Body>(b) }; }

            constexpr std::tuple<As...> const& tuple() { return vals; }
        };
        template<class ...Args> l(const std::tuple<Args...>&)->l<const std::tuple<Args...>&>;

        struct default_t {};
        template<> struct l<default_t> {

            template<class Body> constexpr match_case<Body&&, default_t>
            operator=(this l self, Body&& b) { return { self, std::forward<Body>(b) }; }

            constexpr wildcard tuple() { return {}; }
        };
        constexpr l<default_t> otherwise;
    }

    template<class ...As>
    constexpr bool compare_tuples(std::tuple<As...>& a, match_types::wildcard b) { return true; }

    // Match result for non-references, uses union
    // to optionally store value, boolean flag for empty.
    template<class Ty> struct match_result {
        constexpr match_result() : empty(nullptr), has_val(false) {}
        constexpr match_result(Ty&& v) : value(std::move(v)), has_val(true) {}

        union { void* empty; Ty value; };

        bool has_val = false;
        constexpr operator bool() { return has_val; }
        constexpr Ty get() { return std::move(value); }
    };

    // Match result for references, uses pointer
    // nullptr when empty
    template<class Ty> struct match_result<Ty&> {
        constexpr match_result() : value(nullptr) {}
        constexpr match_result(Ty& v) : value(&v) {}

        Ty* value;
        constexpr operator bool() { return value != nullptr; }
        constexpr Ty& get() { return *value; }
    };

    // Match result for references, uses pointer
    // nullptr when empty
    template<class Ty> struct match_result<Ty&&> {
        constexpr match_result() : value(nullptr) {}
        constexpr match_result(Ty&& v) : value(&v) {}

        Ty* value;
        constexpr operator bool() { return value != nullptr; }
        constexpr Ty& get() { return *value; }
    };

    // Match result for void, has special int constructor
    // to distinguish between empty and non-empty.
    template<> struct match_result<void> {
        constexpr match_result() : has_val(false) {}
        constexpr match_result(int) : has_val(true) {}

        bool has_val;
        constexpr operator bool() { return has_val; }
        constexpr void get() { }
    };

    template<class Body, class ...As>
    struct match_case {
        using body_t = std::decay_t<Body>;
        enum class body_type { Value, NoArgs, Args };
        constexpr static body_type TYPE =
              std::invocable<body_t>    ? body_type::NoArgs // If invocable with no arguments: NoArgs
            : kaixo::has_fun_op<body_t> ? body_type::Args // If it does have a function operator: Args
            : body_type::Value; // Otherwise it's just a value

        match_types::l<As...> args;
        Body body;

        template<class ...Tys>
        constexpr auto check(match_types::l<Tys...>& c) {

            // Simple value as body, just check equality of tuples, and return the body 
            if constexpr (TYPE == body_type::Value) return check_value(c);

            // Lambda that takes no arguments, check equality of tuples, return result 
            // of call to body. If return type is void we need to differentiate, so 
            // call constructor with '0' (specialized match_result for type == void)
            else if constexpr (TYPE == body_type::NoArgs) return check_noarg(c);

            // Lambda that takes arguments, bit more complicated with edge cases.
            else if constexpr (TYPE == body_type::Args) return check_arg(c);
        }

        template<class ...Tys>
        constexpr auto check_value(match_types::l<Tys...>& c) {
            using result = match_result<Body>;
            if (compare_tuples(c.tuple(), args.tuple())) return result{ std::move(body) };
            else return result{};
        }

        template<class ...Tys>
        constexpr auto check_noarg(match_types::l<Tys...>& c) {
            using res_type = decltype(body());
            using result = match_result<res_type>;
            if (compare_tuples(c.tuple(), args.tuple()))
                if constexpr (std::same_as<res_type, void>) return result{ 0 };
                else return result{ body() };
            else return result{};
        }

        template<class ...Tys>
        constexpr auto check_arg(match_types::l<Tys...>& c) {
            using fargs = kaixo::function_args_t<kaixo::minimal_signature_t<body_t>>;
            using res_type = decltype(std::apply(body, std::declval<fargs>()));
            using result = match_result<res_type>;

            // Step 1: make sure amount of arguments of lambda matches amount of inputs provided
            if constexpr (std::tuple_size_v<std::decay_t<decltype(c.tuple())>> != std::tuple_size_v<fargs>) return result{};

            // Step 2: check if function is callable with inputs
            else return[&]<std::size_t ...Is>(std::index_sequence<Is...>) {
                if constexpr (!(std::convertible_to<   // If the input type is not convertible to
                    decltype(std::get<Is>(c.tuple())), // the provided lambda arguments, we'll have
                    std::tuple_element_t<Is, fargs>>   // to check if we can cast them.
                    && ...)) {
                    // We use the overloaded 'type' function to get the actual stored type in the input
                    // and we do a runtime check using std::type_info to check if they equal.
                    if (((type(std::get<Is>(c.tuple())) != typeid(std::tuple_element_t<Is, fargs>)) || ...))
                        return result{}; // If no equal, there's nothing we can do, can't compare them...
                }

                // After checking if they're convertible or castible, collect a casted tuple and compare 
                // that new tuple to the match case. If successful, call body with casted inputs.
                std::tuple _casted{ general_cast<std::tuple_element_t<Is, fargs>>(std::get<Is>(c.tuple()))... };
                if (compare_tuples(_casted, args.tuple())) return result{ std::apply(body, _casted) };
                else return result{};
            }(std::make_index_sequence<std::tuple_size_v<fargs>>{});
        }
    };

    template<class ...Cases> struct match : std::tuple<Cases&&...> {
        using std::tuple<Cases&&...>::tuple;
    };
    template<class ...Args> match(Args&&...)->match<Args...>;

    template<class ...As, class ...Cases>
    constexpr auto operator>>(match_types::l<As...>&& arg, match<Cases...>&& m) {
        // Go through all match cases and check if it's a match
        return[&]<std::size_t I>(this auto && self) {
            auto res = std::get<I>(m).check(arg);
            if (res) return res.get();
            // If no match, throw if last (prevent infinite recursive type)
            else if constexpr (I == sizeof...(Cases) - 1)
                throw std::exception("no match!");
            else return self.operator()<I + 1>(); // Otherwise recurse.
        }.operator() < 0 > ();
    }
}


template<class ...Args>
constexpr auto test(Args&&...args) {
    using namespace kaixo;
    using namespace kaixo::match_types;
    return l{ std::forward<Args>(args)... } >> match{
        l{ 1 } = [] { return 1; },
        l{ _, 1 } = [] { return 2; },
        l{ _, _, 1 } = [] { return 3; },
        l{ _, _, _, 1 } = [] { return 4; },
        otherwise = [] { return 5; }
    };
}

int main() {
    using namespace kaixo;
    using namespace kaixo::match_types;

    constexpr auto aeoi = l{ 2, 1, 1 } >> match{
        l{ 1 } = 1,
        l{ _, 1 } = 2,
        l{ _, _, 1 } = 3,
        l{ _, _, _, 1 } = 4,
        otherwise = 5
    };

    constexpr std::tuple<int, double> as{ 1, 2 };

    constexpr auto res1 = l{ as } >> match {
        l{ 1, 2 } = [](int, int) { return 1; },
        l{ 2, 2 } = [](int, int) { return 2; },
        l{ _, _ } = [](int, int) { return 3; },
    };

    std::tuple<std::any, std::any> v{ 1.f, 1 };

    auto res2 = l{ v } >> match{
        l{ 1.f, 2 } = 1,
        l{ 1.f, _ } = 2,
        l{ 2.f, 1 } = 3,
        l{   _, 1 } = 4,
        otherwise = 5
    };

    constexpr std::tuple<int, double> t1{ 1, 2. };
    constexpr std::tuple<int, float> t2{ 1, 2.f };

    constexpr auto eae = l{ t1, t2 } >> match{
        l{ l{ 2, 2 }, l{ 1, 2 } } = [] { return 1; },
        l{ l{ 1, 2 }, l{ 1, 2 } } = [] { return 2; },
        l{ l{ 1, 2 }, l{ 1, 2 } } = [] { return 3; },
    };

    return 0;
}