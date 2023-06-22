#pragma once
#include "kaixo/type_utils.hpp"

namespace kaixo {
    template<class Ty>
    struct initializer_storage {
        Ty* _storage = nullptr;
        std::size_t _size = 0;

        constexpr initializer_storage() {}
        constexpr initializer_storage(Ty* storage, std::size_t size)
            : _storage(storage), _size(size) {}
        constexpr initializer_storage(const initializer_storage&) = delete;
        constexpr initializer_storage(initializer_storage&& other) noexcept
            : _storage(other._storage), _size(other._size) {
            other._storage = nullptr, other._size = 0;
        }

        constexpr ~initializer_storage() { delete[] _storage; }

        constexpr const Ty* begin() const { return _storage; }
        constexpr const Ty* end() const { return _storage + _size; }
    };

    template<class ...Tys>
        requires std::same_as<info<Tys...>,
                     typename info<Tys...>::unique> // Make sure only unique types
    struct multi_initializer_list {
        using types = info<Tys...>;
        std::tuple<initializer_storage<Tys>...> _lists;

        template<class Ty, class Self>
            requires info<Tys...>::template occurs<Ty>
        constexpr const initializer_storage<Ty>& get(this Self&& self) {
            return std::get<types::template index<Ty>>(self._lists);
        }

        template<class Ty>
            requires info<Tys...>::template occurs<Ty>
        constexpr std::size_t count() const { return get<Ty>().size(); }

        template<class ...Args>
        constexpr multi_initializer_list(Args&&...args)
            : multi_initializer_list{ std::forward_as_tuple(std::forward<Args>(args)...) } {}

    private:
        template<class ...Args>
        constexpr multi_initializer_list(std::tuple<Args&&...>&& args) : _lists{
            [&]() -> initializer_storage<Tys> { // Pack expansion over Tys
                using args_pack = info<Args...>;
                using value_type = Tys;

                constexpr auto indices = args_pack::decay::template indices<value_type>;

                const auto l = [&]<std::size_t I>(value_t<I>) {
                    using type = args_pack::template element<I>::type;
                    return std::forward<type>(std::get<I>(args));
                };

                if constexpr (indices.size() == 0) return {};
                else return iterate<indices>([&]<std::size_t... Is>() {
                    constexpr std::size_t size = sizeof...(Is);
                    return initializer_storage<value_type>{
                        new value_type[size]{ l(value_t<Is>{})... }, size
                    };
                });
            }()... } {}
    };
}