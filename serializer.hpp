#pragma once
#include "kaixo/type_utils.hpp"

namespace kaixo {
    struct serialized_object;

    template<class Ty>
    struct serialize;

    namespace serialize_modes {
        template<class Ty> // Overload for serialize<Ty> class
        concept can_serialize = requires (Ty val, serialized_object & data) {
            { serialize<Ty>::write(val) } -> concepts::same_as<serialized_object>;
            { serialize<Ty>::read(data) } -> concepts::same_as<Ty>;
        };

        template<class Ty> // Contiguous range of trivials
        concept can_contiguous = std::ranges::contiguous_range<Ty> 
            && concepts::trivial<std::ranges::range_value_t<Ty>>;

        template<class Ty> concept can_range = std::ranges::range<Ty>;
        template<class Ty> concept can_trivial = concepts::trivial<Ty>;
        template<class Ty> concept can_aggregate = concepts::aggregate<Ty>;
    }

    class serialized_object {
        enum class mode { None, Trivial, Aggregate, Contiguous, Range, Serializable };
        
        template<class Ty>
        constexpr static mode pick_mode =
              serialize_modes::can_serialize<decay_t<Ty>>  ? mode::Serializable
            : serialize_modes::can_trivial<decay_t<Ty>>    ? mode::Trivial
            : serialize_modes::can_aggregate<decay_t<Ty>>  ? mode::Aggregate
            : serialize_modes::can_contiguous<decay_t<Ty>> ? mode::Contiguous
            : serialize_modes::can_range<decay_t<Ty>>      ? mode::Range
            :                                                mode::None;

    public:
        constexpr serialized_object() = default;
        constexpr serialized_object(const serialized_object&) = delete;

        constexpr serialized_object(serialized_object&& o)
            : _data(o._data), _first(o._first), _last(o._last), _end(o._end) {
            o.invalidate();
        }

        constexpr ~serialized_object() { delete[] _data; }
        
        constexpr serialized_object& operator=(const serialized_object&) = delete;

        constexpr serialized_object& operator=(serialized_object&& o) {
            _data = o._data, _first = o._first, _last = o._last, _end = o._end;
            o.invalidate();
            return *this;
        }

        template<class Ty>
            requires (pick_mode<Ty> == mode::Serializable)
        constexpr serialized_object& write(Ty&& value) {
            push_range(serialize<decay_t<Ty>>::write(std::forward<Ty>(value)));
            return *this;
        }

        template<class Ty> 
            requires (pick_mode<Ty> == mode::Serializable)
        constexpr Ty read() {
            return serialize<decay_t<Ty>>::read(*this);
        }

        template<class Ty>
            requires (pick_mode<Ty> == mode::Trivial)
        constexpr serialized_object& write(Ty&& value) {
            using value_type = decay_t<Ty>;
            std::size_t _size = sizeof(value_type);
            std::uint8_t* _mem = reinterpret_cast<std::uint8_t*>(&value);
            push(_mem, _size);
            return *this;
        }

        template<class Ty>
            requires (pick_mode<Ty> == mode::Trivial)
        constexpr Ty read() {
            using value_type = decay_t<Ty>;
            std::size_t _size = sizeof(value_type);
            std::uint8_t* _result = _first;
            pop(_size);
            return *reinterpret_cast<Ty*>(_result);
        }

        template<class Ty>
            requires (pick_mode<Ty> == mode::Aggregate)
        constexpr serialized_object& write(Ty&& value) {
            tuples::call(std::forward<Ty>(value), [this]<class ...Args>(Args&&... args) {
                (write(std::forward<Args>(args)), ...);
            });
            return *this;
        }

        template<class Ty>
            requires (pick_mode<Ty> == mode::Aggregate)
        constexpr Ty read() {
            return struct_members_t<Ty>::for_each([this]<class ...Args>{
                return Ty{ this->read<Args>()... };
            });
        }

        template<class Ty>
            requires (pick_mode<Ty> == mode::Contiguous)
        constexpr serialized_object& write(Ty&& value) {
            using value_type = std::ranges::range_value_t<decay_t<Ty>>;
            std::size_t _size = std::ranges::size(value);
            std::size_t _sizeBytes = _size * sizeof(value_type);
            std::uint8_t* _mem = reinterpret_cast<std::uint8_t*>(std::ranges::data(value));
            write(_size);
            push(_mem, _sizeBytes);
            return *this;
        }

        template<class Ty>
            requires (pick_mode<Ty> == mode::Contiguous)
        constexpr Ty read() {
            using value_type = std::ranges::range_value_t<decay_t<Ty>>;
            std::size_t _size = read<std::size_t>();
            std::size_t _sizeBytes = _size * sizeof(value_type);
            value_type* _at = reinterpret_cast<value_type*>(_first);
            pop(_sizeBytes);
            if constexpr (constructible<Ty, value_type*, value_type*>) {
                return Ty{ _at, _at + _size };
            }
            else {
                Ty _result{};
                std::copy_n(_at, _size, std::begin(_result));
                return _result;
            }
        }

        template<class Ty>
            requires (pick_mode<Ty> == mode::Range)
        constexpr serialized_object& write(Ty&& value) {
            using reference = std::ranges::range_reference_t<decay_t<Ty>>;
            write<std::size_t>(std::ranges::size(value));
            for (reference val : value) write(val);
            return *this;
        }

        template<class Ty>
            requires (pick_mode<Ty> == mode::Range)
        constexpr Ty read() {
            using value_type = std::ranges::range_value_t<decay_t<Ty>>;
            using reference = std::ranges::range_reference_t<decay_t<Ty>>;
            std::size_t _size = read<std::size_t>();
            Ty _result{};
            for (std::size_t i = 0; i < _size; ++i)
                _result.push_back(read<value_type>());
            return _result;
        }

        template<class Ty>
            requires (pick_mode<Ty> != mode::None)
        constexpr serialized_object& operator<<(Ty&& value) {
            return write(std::forward<Ty>(value));
        }

        template<class Ty>
            requires (pick_mode<Ty> != mode::None)
        constexpr Ty& operator>>(Ty& value) {
            return value = read<decay_t<Ty>>();
        }

        constexpr std::size_t size() const { return _last - _first; }
        constexpr std::uint8_t* begin() { return _first; }
        constexpr std::uint8_t* end() { return _last; }
        constexpr const std::uint8_t* begin() const { return _first; }
        constexpr const std::uint8_t* end() const { return _last; }

        constexpr void clear() { delete[] _data; invalidate(); }

        constexpr std::string to_string() const {
            constexpr auto chars = "0123456789ABCDEF";
            std::string _result = "";
            _result.reserve(size() * 3);
            for (std::uint8_t byte : *this) {
                _result += chars[byte & 0x0F];
                _result += chars[(byte & 0xF0) >> 8ull];
                _result += ' ';
            }
            return _result;
        }

    private:
        std::uint8_t* _data = nullptr;  // Allocated pointer
        std::uint8_t* _first = nullptr; // First stored byte
        std::uint8_t* _last = nullptr;  // Last stored byte
        std::uint8_t* _end = nullptr;   // End of allocated memory

        constexpr std::size_t available_space() const { return _end - _last; }

        constexpr void pop(std::size_t nbytes) { _first += nbytes; }

        constexpr void push(std::uint8_t* ptr, std::size_t size) {
            if (available_space() < size) reallocate(size);
            std::copy_n(ptr, size, _last);
            _last += size;
        }

        constexpr void push_range(const serialized_object& other) {
            push(other._first, other.size());
        }

        constexpr void reallocate(std::size_t more) {
            std::size_t _newSize = size() * 2 + more;
            std::uint8_t* _newData = new std::uint8_t[_newSize];
            if (_data != nullptr) {
                std::copy_n(_first, size(), _newData);
                delete[] _data;
            }
            _last = _newData + size();
            _data = _newData;
            _first = _newData;
            _end = _newData + _newSize;
        }

        constexpr void invalidate() {
            _data = nullptr;
            _first = nullptr;
            _last = nullptr;
            _end = nullptr;
        }
    };
}