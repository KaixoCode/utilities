#pragma once
#include "utils.hpp"

namespace kaixo {
    template<class Return, class ...Args>
    class function_storage {
    public:
        virtual Return call(Args&&...) = 0;
        std::size_t ref_count = 1;
    };

    template<class, class>
    class typed_function_storage;
    template<class Func, class Return, class ...Args>
    class typed_function_storage<Func, Return(Args...)> : public function_storage<Return, Args...> {
    public:
        Func function;

        typed_function_storage(Func&& f)
            : function(std::forward<Func>(f)) {}

        Return call(Args&&...args) override {
            return function(std::forward<Args>(args)...);
        }
    };

    template<class, class>
    class member_function_storage;
    template<class Object, class Return, class ...Args>
    class member_function_storage<Object, Return(Args...)> : public function_storage<Return, Args...> {
    public:
        Return(Object::* function)(Args...);
        Object& obj;

        member_function_storage(Return(Object::* function)(Args...), Object& obj)
            : function(function), obj(obj) {}

        Return call(Args&&...args) override {
            return (obj.*function)(std::forward<Args>(args)...);
        }
    };

    template<class T>
    class function;
    template<class Return, class ...Args>
    class function<Return(Args...)> {
    public:
        using result_type = Return;
        using argument_types = std::tuple<Args...>;

        constexpr function() = default;

        template<class Type>
        function(result_type(Type::* a)(Args...), Type& t)
            : storage(new member_function_storage<Type, result_type(Args...)>{ a, t }) {}

        template<std::invocable<Args...> Func>
        function(Func&& t)
            : storage(new typed_function_storage<Func, result_type(Args...)>{ std::forward<Func>(t) }) {}

        function(const function& f)
            : storage(f.storage) {
            if (storage) storage->ref_count++;
        }

        function(function&& f)
            : storage(f.storage) {
            f.storage = nullptr;
        }

        constexpr ~function() { clean(); }

        auto& operator=(const function& f) {
            clean();
            storage = f.storage;
            if (storage)
                storage->ref_count++;
            return *this;
        }

        auto& operator=(function&& f) {
            clean();
            storage = f.storage;
            f.storage = nullptr;
            return *this;
        }

        inline result_type operator()(Args ...args) const {
            return storage->call(std::forward<Args>(args)...);
        }

        inline operator bool() const { return storage; }

        function_storage<result_type, Args...>* storage = nullptr;

    private:
        constexpr void clean() {
            if (storage) {
                storage->ref_count--;
                if (storage->ref_count == 0)
                    delete storage;
            }
        }
    };
}