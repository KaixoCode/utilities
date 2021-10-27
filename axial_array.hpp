#include <cstdint>
#include <concepts>
#include <memory>
#include <ranges>
#include <initializer_list>
#include <iterator>
#include <algorithm>

namespace kaixo {
    template<class Type, std::size_t Side, std::signed_integral IndexType = std::int64_t>
    struct axial_array {
        using value_type = Type;
        using size_type = std::size_t;
        using index_type = IndexType;
        using difference_type = std::ptrdiff_t;
        using pointer = Type*;
        using const_pointer = const Type*;
        using reference = Type&;
        using const_reference = const Type&;

        using iterator = Type*;
        using const_iterator = const Type*;

        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

        constexpr static size_type side = Side;
        constexpr static size_type array_size = 3 * (side * side - side) + 1;
        constexpr static size_type width = 2 * side - 1;
        constexpr static index_type begin_index = -(side - 1);
        constexpr static index_type end_index = (side - 1);

        struct key {
            index_type x;
            index_type y;

            constexpr index_type z() const { return -(x + y); }
            constexpr size_type index() const {
                return (x + side - 1 + (y < 0 ? y : 0)) // x coord
                    + (y + side - 1) * side // Square
                    + (y < 0
                        ? (0.5 * (y + side - 2) * (y + side - 1)) // First triangle till y
                        : (0.5 * (side - 2) * (side - 1)))        // Full first triangle
                    + (y > 0 ? side - 1 : 0) // Middle 
                    + (y > 1 ? 0.5 * ((side - 2) * (side - 1) - (side - y) * (side - y - 1)) : 0); // 2nd triangle till y
            }
        };

        constexpr axial_array() = default;
        constexpr axial_array(axial_array&&) = default;
        constexpr axial_array(const axial_array&) = default;
        constexpr axial_array(std::initializer_list<value_type> vals) { std::move(vals.begin(), vals.end(), m_Data); };

        template<std::same_as<value_type> ...Types>
        constexpr axial_array(std::initializer_list<Types>&& ...args) requires (sizeof...(Types) == width) {
            index_type i = 0;
            (std::ranges::for_each(args, [&](const value_type& v) { m_Data[i] = v; i++; }), ...);
        }

        constexpr reference operator[](key k) { return m_Data[k.index()]; }
        constexpr const_reference operator[](key k) const { return m_Data[k.index()]; }
        constexpr void fill(const_reference value) { std::fill_n(m_Data, array_size, value); }
        constexpr iterator begin() { return m_Data; }
        constexpr const_iterator begin() const { return m_Data; }
        constexpr iterator end() { return m_Data + array_size; }
        constexpr const_iterator end() const { return m_Data + array_size; }
        constexpr reverse_iterator rbegin() { return m_Data; }
        constexpr const_reverse_iterator rbegin() const { return m_Data; }
        constexpr reverse_iterator rend() { return m_Data + array_size; }
        constexpr const_reverse_iterator rend() const { return m_Data + array_size; }
        constexpr const_iterator cbegin() const { return m_Data; }
        constexpr const_iterator cend() const { return m_Data + array_size; }
        constexpr const_reverse_iterator crbegin() const { return m_Data; }
        constexpr const_reverse_iterator crend() const { return m_Data + array_size; }
        constexpr reference at(key k) { return m_Data[k.index()]; }
        constexpr const_reference at(key k) const { return m_Data[k.index()]; }
        constexpr reference front() { return m_Data[0]; }
        constexpr const_reference front() const { return m_Data[0]; }
        constexpr reference back() { return m_Data[array_size - 1]; }
        constexpr const_reference back() const { return m_Data[array_size - 1]; }
        constexpr pointer data() { return m_Data; }
        constexpr const_pointer data() const { return m_Data; }
        constexpr bool empty() const { return false; }
        constexpr size_type size() const { return array_size; }
        constexpr size_type max_size() const { return array_size; }
        constexpr void swap(axial_array& other) { std::swap_ranges(begin(), end(), other.begin()); }

    private:
        value_type m_Data[array_size];
    };
}