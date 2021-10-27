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

        constexpr static size_type side = Side;
        constexpr static size_type array_size = 3 * (side * side - side) + 1;
        constexpr static size_type width = 2 * side - 1;
        constexpr static index_type begin_index = -(side - 1);
        constexpr static index_type end_index = (side - 1);

        struct key {
            index_type x;
            index_type y;

            constexpr key& operator++() { 
                x++;
                if (y <= 0 && x == end_index + 1 || y > 0 && x == end_index - y + 1) {
                    y++;
                    x = begin_index + (y < 0 ? -y : 0);
                }
                return *this; 
            }

            constexpr key& operator--() {
                x--;
                if (y <= 0 && x == begin_index + y - 1 || y > 0 && x == begin_index - 1) {
                    y--;
                    x = end_index - (y > 0 ? y : 0);
                }
                return *this;
            }

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

        template<bool IsConst>
        struct axial_iterator {
            using iterator_concept = std::bidirectional_iterator_tag;
            using iterator_category = std::bidirectional_iterator_tag;
            using value_type = Type;
            using difference_type = ptrdiff_t;
            using pointer = std::conditional_t<IsConst, const Type*, Type*>;
            using reference = std::conditional_t<IsConst, const Type&, Type&>;

            constexpr axial_iterator() : arr(0), pos(0, 0) {};
            constexpr explicit axial_iterator(std::conditional_t<IsConst, 
                const axial_array*, axial_array*> arr, key pos)
                : arr(arr), pos(pos) {}

            constexpr reference operator*() const { return arr->at(pos); }
            constexpr pointer operator->() const noexcept { return &arr->at(pos); }

            constexpr axial_iterator& operator++() { ++pos; return *this; }
            constexpr axial_iterator operator++(int) {
                axial_iterator _temp{ *this };
                ++pos;
                return _temp;
            }

            constexpr axial_iterator& operator--() { --pos; return *this; }
            constexpr axial_iterator operator--(int) {
                axial_iterator _temp{ *this };
                --pos;
                return _temp;
            }

            constexpr bool operator==(const axial_iterator& other) const {
                return pos.x == other.pos.x && pos.y == other.pos.y;
            }

            constexpr std::strong_ordering operator<=>(const axial_iterator& other) const {
                return pos.index() <=> other.pos.index();
            }

        private:
            std::conditional_t<IsConst, const axial_array*, axial_array*> arr;
            key pos;
            friend class axial_array;
        };

        template<bool IsConst>
        struct index_iterator : axial_iterator<IsConst> {
            using axial_iterator<IsConst>::axial_iterator;
            struct reference_value {
                axial_iterator<IsConst>::reference value;
                key pos;
            };

            struct pointer_value {
                axial_iterator<IsConst>::pointer value;
                key pos;
            };

            using axial_iterator<IsConst>::operator++;
            using axial_iterator<IsConst>::operator--;
            using axial_iterator<IsConst>::operator<=>;

            constexpr reference_value operator*() const { return { this->arr->at(this->pos), this->pos }; }
            constexpr pointer_value operator->() const noexcept { return { &this->arr->at(this->pos), this->pos }; }
        };

        template<bool IsConst>
        struct index_loop {
            using iterator = index_iterator<IsConst>;
            using reverse_iterator = std::reverse_iterator<iterator>;

            constexpr iterator begin() { return iterator{ &data, { 0, begin_index } }; }
            constexpr iterator end() { return iterator{ &data, { begin_index, end_index + 1 } }; }
            constexpr reverse_iterator rbegin() { return end(); }
            constexpr reverse_iterator rend() { return begin(); }

            std::conditional_t<IsConst, const axial_array&, axial_array&> data;
        };

        using iterator = axial_iterator<false>;
        using const_iterator = axial_iterator<true>;

        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

        constexpr axial_array() = default;
        constexpr axial_array(axial_array&&) = default;
        constexpr axial_array(const axial_array&) = default;
        constexpr axial_array(std::initializer_list<value_type> vals) { std::move(vals.begin(), vals.end(), m_Data); };

        template<std::same_as<value_type> ...Types>
        constexpr axial_array(std::initializer_list<Types>&& ...args) requires (sizeof...(Types) == width) {
            index_type i = 0;
            (std::ranges::for_each(args, [&](const value_type& v) { m_Data[i] = v; i++; }), ...);
        }

        constexpr index_loop<false> with_index() { return index_loop<false>{ *this }; }
        constexpr index_loop<true> with_index() const { return index_loop<true>{ *this }; }
        constexpr reference operator[](key k) { return m_Data[k.index()]; }
        constexpr const_reference operator[](key k) const { return m_Data[k.index()]; }
        constexpr void fill(const_reference value) { std::fill_n(m_Data, array_size, value); }
        constexpr iterator begin() { return iterator{ this, { 0, begin_index } }; }
        constexpr const_iterator begin() const { return const_iterator{ this, { 0, begin_index } }; }
        constexpr iterator end() { return iterator{ this, { begin_index, end_index + 1 } }; }
        constexpr const_iterator end() const { return const_iterator{ this, { begin_index, end_index + 1 } }; }
        constexpr reverse_iterator rbegin() { return end(); }
        constexpr const_reverse_iterator rbegin() const { return end(); }
        constexpr reverse_iterator rend() { return begin(); }
        constexpr const_reverse_iterator rend() const { return begin(); }
        constexpr const_iterator cbegin() const { return begin(); }
        constexpr const_iterator cend() const { return end(); }
        constexpr const_reverse_iterator crbegin() const { return end(); }
        constexpr const_reverse_iterator crend() const { return begin(); }
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