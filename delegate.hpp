#pragma once
#include <type_traits>
#include <concepts>
#include <functional>
#include <memory>
#include "utils.hpp"

namespace kaixo {
    template<class> class delegate;
    template<class Ret, class ...Args>
    class delegate<Ret(Args...)> {
        template<class> struct functor;
        template<class Functor>
            requires (sizeof(std::decay_t<Functor>) <= 16)
        struct functor<Functor> {
            std::decay_t<Functor> fun;
            constexpr static Ret invoke(void* data, Args...args) {
                return static_cast<functor*>(data)->fun(std::forward<Args>(args)...);
            }
            constexpr static void clean(void* data) {}
        };

        template<class Functor>
            requires (sizeof(std::decay_t<Functor>) > 16)
        struct functor<Functor> {
            std::unique_ptr<std::decay_t<Functor>> fun;
            constexpr functor(Functor functor)
                : fun(std::make_unique<std::decay_t<Functor>>(
                    std::forward<Functor>(functor))) {}
            constexpr static Ret invoke(void* data, Args...args) {
                return (*static_cast<functor*>(data)->fun)(std::forward<Args>(args)...);
            }
            constexpr static void clean(void* data) {
                (static_cast<functor*>(data)->fun).release();
            }
        };

        template<class Obj, class Fun>
        struct mem_fun {
            Obj* obj; Fun fun;
            constexpr static Ret invoke(void* data, Args...args) {
                mem_fun* _fun = static_cast<mem_fun*>(data);
                return (_fun->obj->*_fun->fun)(std::forward<Args>(args)...);
            }
        };

        struct fun_ptr {
            Ret(*fun)(Args...);
            constexpr static Ret invoke(void* data, Args...args) {
                return static_cast<fun_ptr*>(data)->fun(std::forward<Args>(args)...);
            }
        };
    public:
        template<std::invocable<Args...> Arg>
        constexpr delegate(Arg&& arg)
            : fun(&functor<Arg>::invoke), clean(&functor<Arg>::clean) {
            new (data) functor<Arg>{ std::forward<Arg>(arg) };
        }

        template<class Obj, class Fun>
        constexpr delegate(Obj& obj, Fun fun)
            : fun(&mem_fun<Obj, Fun>::invoke) {
            new (data) mem_fun<Obj, Fun>(&obj, fun);
        }

        constexpr delegate(Ret(*fun)(Args...))
            : fun(&fun_ptr::invoke) {
            new (data) fun_ptr{ fun };
        }

        constexpr Ret operator()(Args...args) const {
            return (*fun)(const_cast<void*>(static_cast<const void*>(data)),
                std::forward<Args>(args)...);
        }

        constexpr ~delegate() { if (clean) clean(static_cast<void*>(data)); }

        uint8_t data[16];
        Ret(*fun)(void*, Args...) = nullptr;
        void(*clean)(void*) = nullptr;
    };


    template<class Lambda>
    delegate(Lambda)->delegate<minimal_signature_t<Lambda>>;
    template<class Obj, class Fun>
    delegate(Obj&, Fun)->delegate<minimal_signature_t<Fun>>;
}

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
        this->_copy_Impl = [](const void* self, void* to) -> _del_Base<R, As...>*{
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