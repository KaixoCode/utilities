#pragma once
#include "utils.hpp"
#include "function.hpp"

namespace kaixo {
    template<class>
    class pa_function;
    template<class Return, class...Args>
    class pa_function<Return(Args...)> {
    public:
        using result_type = Return;
        using argument_types = std::tuple<Args...>;

        template<class ...Tys> 
        pa_function(Tys&&...f) requires std::constructible_from<function<result_type(Args...)>, Tys...>
            : function(std::forward<Tys>(f)...) {}

        auto& operator=(pa_function&& f) {
            function = std::move(f.function);
            return *this;
        }

        auto& operator=(const pa_function& f) {
            function = f.function;
            return *this;
        }

        template<class ...Tys> requires (are_first_n<result_type(Args...), Tys...> && sizeof...(Tys) < sizeof...(Args) && sizeof...(Tys) > 0)
        inline pa_function<typename last_n_args<result_type(Args...), sizeof...(Args) - sizeof...(Tys)>::type> operator()(Tys&&...args) const {
            auto function = this->function;
            return [func = std::move(function), ...args = std::forward<Tys>(args)] (auto...rest) -> result_type {
                return func(args..., std::forward<decltype(rest)>(rest)...); };
        }

        inline result_type operator()(Args ...args) const { return function(std::forward<Args>(args)...); }

        inline operator bool() const { return function; }

    protected:
        function<result_type(Args...)> function;
    };

    template <class Return, class ...Args>
    function(Return(Args...))->function<Return(Args...)>;

    template <class Return, class T, class ...Args>
    function(Return(T::* a)(Args...), T&)->function<Return(Args...)>;

    template <class _Fx>
    function(_Fx)->function<typename lambda_signature<_Fx>::type>;

    template <class Return, class ...Args>
    pa_function(Return(Args...))->pa_function<Return(Args...)>;

    template <class Return, class T, class ...Args>
    pa_function(Return(T::* a)(Args...), T&)->pa_function<Return(Args...)>;

    template <class _Fx>
    pa_function(_Fx)->pa_function<typename lambda_signature<_Fx>::type>;
}