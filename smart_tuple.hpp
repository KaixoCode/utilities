#pragma once
#include "utils.hpp"

namespace kaixo {
    template<class ...Tys>
    class smart_tuple : public std::tuple<Tys...> {
        using seq = std::make_index_sequence<sizeof...(Tys)>;

    public:
        template<std::size_t N>
        using nth_type = typename std::tuple_element<N, std::tuple<Tys...>>::type;
        using std::tuple<Tys...>::tuple;

        struct value {
            template<one_of<Tys...> T>
            inline operator T& () { return get<T>(); }

            template<one_of<Tys...> T>
            inline operator T const& () const { return get<T>(); }

            template<one_of<Tys...> T>
            inline T& get() {
                check_type<T>(seq{});
                return *reinterpret_cast<T*>(data);
            }

            template<one_of<Tys...> T>
            inline T const& get() const {
                check_type<T>(seq{});
                return *reinterpret_cast<T*>(data);
            }

        private:
            value(std::size_t i) : index(i) {}

            template<class T, std::size_t ...Is>
            inline void check_type(std::index_sequence<Is...>) {
                ((index == Is && !std::is_same_v<nth_type<Is>, T>
                    ? throw std::bad_cast{} : false), ...);
            }

            void* data = nullptr;
            std::size_t index = 0;
            friend class smart_tuple;
        };

        inline value operator[](std::size_t index) { return get(index, seq{}); }
        inline const value operator[](std::size_t index) const { return get(index, seq{}); }

        inline auto& get(std::size_t index) { return get(index, seq{}); }
        inline auto const& get(std::size_t index) const { return get(index, seq{}); }

        template<std::size_t I>
        inline auto& get() { return std::get<I>(*this); }

        template<std::size_t I>
        inline auto const& get() const { return std::get<I>(*this); }

    private:
        template<std::size_t... Is>
        inline value get(std::size_t index, std::index_sequence<Is...>) {
            if (index >= sizeof...(Tys))
                throw std::out_of_range{ "Index is out of range for this tuple." };

            value _value = index;
            return ((index == Is ? (_value.data = (void*)&std::get<Is>(*this), _value) : _value), ...);
        }
    };

    template<typename ...Tys>
    smart_tuple(Tys...)->smart_tuple<Tys...>;
}