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