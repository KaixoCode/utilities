#pragma once
#include <iostream>
#include "kaixo/type_utils.hpp"

namespace kaixo {
    template<class Byte = std::uint8_t, class Allocator = std::allocator<Byte>>
    class serialized_object;

    template<class Ty>
    // Specialize this to support serialization on classes.
    struct serialize; 

    namespace detail {
        template<class Ty> // Overload for serialize<Ty> class
        concept can_serialize = requires (Ty val, serialized_object<> & data) {
            { serialize<Ty>::write(data, val) } -> concepts::same_as<void>;
            { serialize<Ty>::read(data) } -> concepts::same_as<Ty>;
        };

        template<class Ty> // Contiguous range of trivials
        concept can_contiguous = std::ranges::contiguous_range<Ty>
            && concepts::trivially_copyable<std::ranges::range_value_t<Ty>>;

        template<class Ty>
        concept can_structured = concepts::structured_binding<Ty>
            && binding_types_t<Ty>::template can_construct<Ty>::value;

        template<class Ty> concept can_range = std::ranges::range<Ty>;
        template<class Ty> concept can_trivial = concepts::trivially_copyable<Ty>;
    }

    template<class Byte, class Allocator>
    class serialized_object {
        static_assert(sizeof(Byte) == 1, "Requires size of Byte to be 1 byte.");

        enum class mode { 
            None,        // Not supported
            Trivial,     // Trivially copyable (std::copy_n)
            Structured,  // Has structured binding, and can be constructed with binding types
            Contiguous,  // Contiguous range of trivially copyable types
            Range,       // Any range
            Serializable // Has overloaded serialize<Ty> class
        };
        
        template<class Ty>
        constexpr static mode Choice =
              detail::can_serialize<decay_t<Ty>>  ? mode::Serializable
            : detail::can_trivial<decay_t<Ty>>    ? mode::Trivial
            : detail::can_structured<decay_t<Ty>> ? mode::Structured
            : detail::can_contiguous<decay_t<Ty>> ? mode::Contiguous
            : detail::can_range<decay_t<Ty>>      ? mode::Range
            :                                       mode::None;

    public:
        using value_type = Byte;
        using allocator_type = Allocator;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;
        using reference = Byte&;
        using const_reference = const Byte&;
        using pointer = std::allocator_traits<Allocator>::pointer;
        using const_pointer = std::allocator_traits<Allocator>::const_pointer;
        using iterator = const_pointer;
        using const_iterator = const_pointer;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

        // =======================================================

        constexpr ~serialized_object() { clear(); }
        constexpr serialized_object() = default;
        constexpr serialized_object(const serialized_object&) = delete;
        constexpr serialized_object(serialized_object&& o)
            : _data(o._data), _first(o._first), 
              _last(o._last), _end(o._end) { o.invalidate(); }
        
        constexpr serialized_object& operator=(const serialized_object&) = delete;
        constexpr serialized_object& operator=(serialized_object&& o) {
            _data = o._data;
            _first = o._first; 
            _last = o._last;
            _end = o._end;
            o.invalidate();
            return *this;
        }

        // =======================================================

        template<class Ty> requires (Choice<Ty> == mode::Serializable)
        constexpr serialized_object& write(Ty&& value) {
            serialize<decay_t<Ty>>::write(*this, std::forward<Ty>(value));
            return *this;
        }

        template<class Ty> requires (Choice<Ty> == mode::Serializable)
        constexpr Ty read() {
            return serialize<decay_t<Ty>>::read(*this);
        }

        // =======================================================

        template<class Ty> requires (Choice<Ty> == mode::Trivial)
        constexpr serialized_object& write(Ty&& value) {
            const Byte* _mem = reinterpret_cast<const Byte*>(&value);
            push_bytes(_mem, sizeof(decay_t<Ty>));
            return *this;
        }

        template<class Ty> requires (Choice<Ty> == mode::Trivial)
        constexpr Ty read() {
            return *reinterpret_cast<Ty*>(pop_bytes(sizeof(decay_t<Ty>)));
        }

        // =======================================================

        template<class Ty> requires (Choice<Ty> == mode::Structured)
        constexpr serialized_object& write(Ty&& value) {
            tuples::call(std::forward<Ty>(value), [this]<class ...Args>(Args&&... args) {
                (this->write(std::forward<Args>(args)), ...);
            });
            return *this;
        }

        template<class Ty> requires (Choice<Ty> == mode::Structured)
        constexpr Ty read() {
            return binding_types_t<Ty>::for_each([this]<class ...Args>{
                return Ty{ this->read<Args>()... };
            });
        }

        // =======================================================

        template<class Ty> requires (Choice<Ty> == mode::Contiguous)
        constexpr serialized_object& write(Ty&& value) {
            using value_type = std::ranges::range_value_t<decay_t<Ty>>;
            std::size_t _size = std::ranges::size(value);
            write(_size); // Save range size
            const Byte* _mem = reinterpret_cast<const Byte*>(std::ranges::data(value));
            push_bytes(_mem, _size * sizeof(value_type));
            return *this;
        }

        template<class Ty> requires (Choice<Ty> == mode::Contiguous)
        constexpr Ty read() {
            using value_type = std::ranges::range_value_t<decay_t<Ty>>;
            std::size_t _size = read<std::size_t>(); // Read range size
            value_type* _at = reinterpret_cast<value_type*>(pop_bytes(_size * sizeof(value_type)));
            if constexpr (concepts::constructible<Ty, value_type*, value_type*>) {
                return Ty{ _at, _at + _size };
            } else {
                Ty _result{}; 
                std::copy_n(_at, _size, std::begin(_result));
                return _result;
            }
        }

        // =======================================================

        template<class Ty> requires (Choice<Ty> == mode::Range)
        constexpr serialized_object& write(Ty&& value) {
            using reference = std::ranges::range_reference_t<decay_t<Ty>>;
            write<std::size_t>(std::ranges::size(value)); // Save range size
            for (reference val : value) write(val);
            return *this;
        }

        template<class Ty> requires (Choice<Ty> == mode::Range)
        constexpr Ty read() {
            using value_type = std::ranges::range_value_t<decay_t<Ty>>;
            using reference = std::ranges::range_reference_t<decay_t<Ty>>;
            const std::size_t _size = read<std::size_t>(); // Read range size
            Ty _result{};
            if constexpr (requires (Ty ty) { { ty.reserve(1ull) }; })
                _result.reserve(_size); // reserve if possible
            for (std::size_t i = 0; i < _size; ++i)
                _result.push_back(read<value_type>());
            return _result;
        }

        // =======================================================

        template<class Ty> requires (Choice<Ty> != mode::None)
        constexpr serialized_object& operator<<(Ty&& value) {
            return write(std::forward<Ty>(value));
        }

        template<class Ty> requires (Choice<Ty> != mode::None)
        constexpr Ty& operator>>(Ty& value) {
            return value = read<decay_t<Ty>>();
        }

        // =======================================================

        constexpr void push(const serialized_object& other) {
            push_bytes(other._first, other.size());
        }

        constexpr std::size_t empty() const { return _last == _first; }
        constexpr std::size_t size() const { return _last - _first; }
        constexpr std::size_t max_size() const { return std::numeric_limits<difference_type>::max(); }
        constexpr std::size_t capacity() const { return _end - _first; }

        constexpr void reserve(std::size_t bytes) { 
            if (bytes > capacity()) 
                reallocate(bytes - capacity()); 
        }

        constexpr const Byte* data() const { return _first; }

        constexpr const_iterator begin() const { return _first; }
        constexpr const_iterator end() const { return _last; }
        constexpr const_iterator cbegin() const { return begin(); }
        constexpr const_iterator cend() const { return end(); }
        constexpr const_reverse_iterator rbegin() const { return end(); }
        constexpr const_reverse_iterator rend() const { return begin(); }
        constexpr const_reverse_iterator crbegin() const { return end(); }
        constexpr const_reverse_iterator crend() const { return begin(); }

        constexpr void clear() {
            if (_data != nullptr)
                _alloc.deallocate(_data, _end - _data); 
            invalidate(); 
        }

        constexpr bool operator==(const serialized_object& other) const {
            return size() == other.size() && std::memcmp(_first, other._first, size()) == 0;
        }
        
        constexpr std::weak_ordering operator<=>(const serialized_object& other) const {
            auto _order = std::memcmp(_first, other._first, std::min(size(), other.size()));
            return _order < 0 ? std::weak_ordering::less :
                   _order > 0 ? std::weak_ordering::greater :
                                size() <=> other.size();
        }

    private:
        Byte* _data = nullptr;  // Allocated pointer
        Byte* _first = nullptr; // First stored byte
        Byte* _last = nullptr;  // Last stored byte
        Byte* _end = nullptr;   // End of allocated memory
        [[no_unique_address]] Allocator _alloc{};

        constexpr std::size_t available_space() const { return _end - _last; }

        // Pops from front, does no move memory or deallocate
        constexpr Byte* pop_bytes(std::size_t nbytes) {
            Byte* _backup = _first;
            _first += nbytes; 
            return _backup;
        }

        // Pushes block of memory, reallocates if necessary
        constexpr void push_bytes(const Byte* ptr, std::size_t size) {
            if (available_space() < size) reallocate(size);
            std::copy_n(ptr, size, _last);
            _last += size;
        }

        constexpr void reallocate(std::size_t more) {
            std::size_t _newSize = size() * 2 + more;
            Byte* _newData = _alloc.allocate(_newSize);
            if (_data != nullptr) {
                std::copy_n(_first, size(), _newData);
                _alloc.deallocate(_data, _end - _data);
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

        friend std::ostream& operator<<(std::ostream& os, const serialized_object& data) {
            auto state = os.rdstate();
            os << std::hex;
            for (Byte byte : data) {
                os << (byte & 0x0F)
                    << ((byte & 0xF0) >> 8ull)
                    << ' ';
            }
            os.clear(state);
            return os;
        }
    };
}