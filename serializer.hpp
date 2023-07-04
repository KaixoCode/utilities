#pragma once
#include <iostream>
#include "kaixo/type_utils.hpp"

namespace kaixo {
    template<class Allocator = std::allocator<std::uint8_t>>
    class serializer_base;

    template<class Type>
    class basic_serializer;

    template<class Ty>
    // Specialize this to support serialization on classes.
    struct serialize; 

    namespace detail {
        template<class Ty, class Type> // Overload for serialize<Ty> class
        concept can_serialize = requires (Ty val, basic_serializer<Type> & data) {
            { serialize<Ty>::write(data, val) } -> concepts::same_as<void>;
            { serialize<Ty>::read(data) } -> concepts::same_as<Ty>;
        };

        template<class Ty> // Contiguous range of trivials
        concept can_contiguous = std::ranges::contiguous_range<Ty>
            && concepts::trivially_copyable<std::ranges::range_value_t<Ty>>;

        template<class Ty>
        concept can_structured = concepts::structured_binding<Ty>
            && binding_types_t<Ty>::template can_construct<Ty>::value;

        template<class Ty> concept can_range = std::ranges::range<Ty> && default_constructible<Ty>;
        template<class Ty> concept can_trivial = concepts::trivially_copyable<Ty>;
    }

    // =======================================================

    // Serializer stores the serialized data in an internal container.
    template<class Allocator>
    class serializer_base {
    public:
        using _can_pop = int; // Can directly pop bytes
        using _can_i = int;   // Can input 
        using _can_o = int;   // Can output

        using value_type = std::uint8_t;
        using allocator_type = Allocator;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;
        using reference = std::uint8_t&;
        using const_reference = const std::uint8_t&;
        using pointer = std::allocator_traits<Allocator>::pointer;
        using const_pointer = std::allocator_traits<Allocator>::const_pointer;
        using iterator = const_pointer;
        using const_iterator = const_pointer;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

        // =======================================================

        constexpr ~serializer_base() { clear(); }
        constexpr serializer_base() = default;
        constexpr serializer_base(Allocator allocator) : _alloc(allocator) {};
        constexpr serializer_base(const serializer_base&) = delete;
        constexpr serializer_base(serializer_base&& o)
            : _data(o._data), _first(o._first),
            _last(o._last), _end(o._end) {
            o.invalidate();
        }

        constexpr serializer_base& operator=(const serializer_base&) = delete;
        constexpr serializer_base& operator=(serializer_base&& o) {
            _data = o._data;
            _first = o._first;
            _last = o._last;
            _end = o._end;
            o.invalidate();
            return *this;
        }

        // =======================================================
        
        constexpr std::uint8_t* pop_bytes(std::size_t nbytes) {
            std::uint8_t* _backup = _first;
            _first += nbytes;
            return _backup;
        }

        constexpr void push_bytes(const std::uint8_t* ptr, std::size_t size) {
            if (available_space() < size) reallocate(size);
            std::copy_n(ptr, size, _last);
            _last += size;
        }

        constexpr std::size_t empty() const { return _last == _first; }
        constexpr std::size_t size() const { return _last - _first; }
        constexpr std::size_t max_size() const { return std::numeric_limits<difference_type>::max(); }
        constexpr std::size_t capacity() const { return _end - _first; }

        constexpr void reserve(std::size_t bytes) { 
            if (bytes > capacity()) 
                reallocate(bytes - capacity()); 
        }

        constexpr void clear() {
            if (_data != nullptr)
                _alloc.deallocate(_data, _end - _data); 
            invalidate(); 
        }

    private:
        std::uint8_t* _data = nullptr;  // Allocated pointer
        std::uint8_t* _first = nullptr; // First stored byte
        std::uint8_t* _last = nullptr;  // Last stored byte
        std::uint8_t* _end = nullptr;   // End of allocated memory
        [[no_unique_address]] Allocator _alloc{};

        constexpr std::size_t available_space() const { return _end - _last; }

        constexpr void reallocate(std::size_t more) {
            std::size_t _newSize = size() * 2 + more;
            std::uint8_t* _newData = _alloc.allocate(_newSize);
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

        // =======================================================

        friend std::ostream& operator<<(std::ostream& os, const serializer_base& data) {
            auto state = os.rdstate();
            os << std::hex;
            for (std::size_t i = 0; i < data.size(); ++i) {
                std::uint8_t byte = data._first[i];
                os << (byte & 0x0F)
                   << ((byte & 0xF0) >> 8ull)
                   << ' ';
            }
            os.clear(state);
            return os;
        }
    };

    // =======================================================

    // (De)serialize directly to and from an iostream.
    class ios_serializer_base {
    public:
        using _can_i = int; // Can input 
        using _can_o = int; // Can output

        constexpr ios_serializer_base(std::iostream& stream)
            : _stream(&stream) {}

        template<class Ty> requires trivially_copyable<Ty>
        Ty get_direct() {
            char _bytes[sizeof(Ty)];
            _stream->read(_bytes, sizeof(Ty));
            return *reinterpret_cast<Ty*>(_bytes);
        }

        void push_bytes(const std::uint8_t* ptr, std::size_t size) {
            _stream->write(reinterpret_cast<const char*>(ptr), size);
        }

    private:
        std::iostream* _stream;
    };
    
    // =======================================================
    
    // Serialize directly to an ostream.
    class os_serializer_base {
    public:
        using _can_o = int; // Can output

        constexpr os_serializer_base(std::ostream& stream)
            : _stream(&stream) {}

        void push_bytes(const std::uint8_t* ptr, std::size_t size) {
            _stream->write(reinterpret_cast<const char*>(ptr), size);
        }

    private:
        std::ostream* _stream;
    };
    
    // =======================================================

    // Deserialize directly from an istream.
    class is_serializer_base {
    public:
        using _can_i = int; // Can input 

        constexpr is_serializer_base(std::istream& stream)
            : _stream(&stream) {}

        template<class Ty> requires trivially_copyable<Ty>
        Ty get_direct() {
            char _bytes[sizeof(Ty)];
            _stream->read(_bytes, sizeof(Ty));
            return *reinterpret_cast<Ty*>(_bytes);
        }

    private:
        std::istream* _stream;
    };

    // =======================================================

    template<class Type>
    class basic_serializer : public Type {
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
              detail::can_serialize<decay_t<Ty>, Type> ? mode::Serializable
            : detail::can_trivial<decay_t<Ty>>         ? mode::Trivial
            : detail::can_structured<decay_t<Ty>>      ? mode::Structured
            : detail::can_contiguous<decay_t<Ty>>      ? mode::Contiguous
            : detail::can_range<decay_t<Ty>>           ? mode::Range
            :                                            mode::None;

        // CanPop means can retrieve pointer to memory, does not work for streams.
        constexpr static bool CanPop = requires() { typename Type::_can_pop; };
        constexpr static bool CanI = requires() { typename Type::_can_i; };
        constexpr static bool CanO = requires() { typename Type::_can_o; };

    public:
        using Type::Type;

        // =======================================================

        template<class Ty, class Self> requires (CanO && Choice<Ty> == mode::Serializable)
        constexpr Self& write(this Self& self, Ty&& value) {
            serialize<decay_t<Ty>>::write(self, std::forward<Ty>(value));
            return self;
        }

        template<class Ty> requires (CanI && Choice<Ty> == mode::Serializable)
        constexpr Ty read() {
            return serialize<decay_t<Ty>>::read(*this);
        }

        // =======================================================

        template<class Ty, class Self> requires (CanO && Choice<Ty> == mode::Trivial)
        constexpr Self& write(this Self& self, Ty&& value) {
            const std::uint8_t* _mem = reinterpret_cast<const std::uint8_t*>(&value);
            self.push_bytes(_mem, sizeof(decay_t<Ty>));
            return self;
        }

        template<class Ty> requires (CanI && Choice<Ty> == mode::Trivial)
        constexpr Ty read() {
            if constexpr (CanPop) {
                return *reinterpret_cast<Ty*>(this->pop_bytes(sizeof(decay_t<Ty>)));
            } else {
                return this->get_direct<Ty>();
            }
        }

        // =======================================================

        template<class Ty, class Self> requires (CanO && Choice<Ty> == mode::Structured)
        constexpr Self& write(this Self& self, Ty&& value) {
            tuples::call(std::forward<Ty>(value), [&]<class ...Args>(Args&&... args) {
                (self.write(std::forward<Args>(args)), ...);
            });
            return self;
        }

        template<class Ty> requires (CanI && Choice<Ty> == mode::Structured)
        constexpr Ty read() {
            return binding_types_t<Ty>::for_each([this]<class ...Args>{
                return Ty{ this->read<Args>()... };
            });
        }

        // =======================================================

        template<class Ty, class Self> requires (CanO && Choice<Ty> == mode::Contiguous)
        constexpr Self& write(this Self& self, Ty&& value) {
            using value_type = std::ranges::range_value_t<decay_t<Ty>>;
            const std::size_t _size = std::ranges::size(value);
            self.write(_size); // Save range size
            const std::uint8_t* _mem = reinterpret_cast<const std::uint8_t*>(std::ranges::data(value));
            self.push_bytes(_mem, _size * sizeof(value_type));
            return self;
        }

        template<class Ty> requires (CanI && Choice<Ty> == mode::Contiguous)
        constexpr Ty read() {
            using value_type = std::ranges::range_value_t<decay_t<Ty>>;
            const std::size_t _size = read<std::size_t>(); // Read range size
            if constexpr (CanPop) {
                value_type* _at = reinterpret_cast<value_type*>(this->pop_bytes(_size * sizeof(value_type)));
                if constexpr (concepts::constructible<Ty, value_type*, value_type*>) {
                    return Ty{ _at, _at + _size };
                } else {
                    Ty _result{}; 
                    std::copy_n(_at, _size, std::begin(_result));
                    return _result;
                }
            } else {
                Ty _result{};
                if constexpr (requires (Ty ty) { { ty.reserve(1ull) }; })
                    _result.reserve(_size); // reserve if possible
                for (std::size_t i = 0; i < _size; ++i)
                    _result.insert(_result.end(), read<value_type>());
                return _result;
            }
        }

        // =======================================================

        template<class Ty, class Self> requires (CanO && Choice<Ty> == mode::Range)
        constexpr Self& write(this Self& self, Ty&& value) {
            using reference = std::ranges::range_reference_t<decay_t<Ty>>;
            self.template write<std::size_t>(std::ranges::size(value)); // Save range size
            for (reference val : value) self.write(val);
            return self;
        }

        template<class Ty> requires (CanI && Choice<Ty> == mode::Range)
        constexpr Ty read() {
            using value_type = std::ranges::range_value_t<decay_t<Ty>>;
            using reference = std::ranges::range_reference_t<decay_t<Ty>>;
            const std::size_t _size = read<std::size_t>(); // Read range size
            Ty _result{};
            if constexpr (requires (Ty ty) { { ty.reserve(1ull) }; })
                _result.reserve(_size); // reserve if possible
            for (std::size_t i = 0; i < _size; ++i)
                _result.insert(_result.end(), read<value_type>());
            return _result;
        }

        // =======================================================

        template<class Ty, class Self> requires (CanO && Choice<Ty> != mode::None)
        constexpr Self& operator<<(this Self& self, Ty&& value) {
            return self.write(std::forward<Ty>(value));
        }

        template<class Ty, class Self> requires (CanI && Choice<Ty> != mode::None)
        constexpr Self& operator>>(this Self& self, Ty& value) {
            value = self.template read<decay_t<Ty>>();
            return self;
        }
    };

    using serializer = basic_serializer<serializer_base<>>;
    using ios_serializer = basic_serializer<ios_serializer_base>;
    using os_serializer = basic_serializer<os_serializer_base>;
    using is_serializer = basic_serializer<is_serializer_base>;
}