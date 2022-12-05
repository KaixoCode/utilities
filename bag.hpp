#pragma once
#include <cstddef>
#include <utility>
#include <vector>
#include <memory>

namespace kaixo {
    struct rtti {
        virtual ~rtti() = default;
        virtual std::size_t size() const = 0;
        virtual const std::type_info& type() const = 0;
        virtual void* get() const = 0;

        virtual void move_assign(void* to) = 0;
        virtual void copy_assign(void* to) const = 0;
        virtual void move_construct(std::byte* at) = 0;
        virtual void copy_construct(std::byte* at) const = 0;
    };

    template<class Ty>
    struct rtti_typed : rtti {
        Ty value;

        template<class ...Args>
        rtti_typed(Args&&... args)
            : value(std::forward<Args>(args)...) {};

        std::size_t size() const override { return sizeof(rtti_typed<Ty>); }
        const std::type_info& type() const override { return typeid(Ty); }
        void* get() const override { return const_cast<void*>(static_cast<const void*>(&value)); }

        void move_assign(void* to) override { *static_cast<Ty*>(to) = std::move(value); }
        void copy_assign(void* to) const override { *static_cast<Ty*>(to) = value; }
        void move_construct(std::byte* at) override { new (at) rtti_typed<Ty>(std::move(value)); }
        void copy_construct(std::byte* at) const override { new (at) rtti_typed<Ty>(value); }
    };

    template<class Allocator = std::allocator<std::byte>>
    struct bag {
    private:

    public:
        struct value_type {
            template<class Ty> bool is() const { return typeid(Ty) == _value->type(); }
            template<class Ty> Ty* cast() { return is<Ty>() ? static_cast<Ty*>(_value->get()) : nullptr; }
            template<class Ty> const Ty* cast() const { return is<Ty>() ? static_cast<Ty*>(_value->get()) : nullptr; }
            const std::type_info& type() const { return _value->type(); }
            std::size_t size() const { return _value->size(); }

            value_type(rtti* value) : _value(value) {}
            value_type(const value_type&) = delete;
            value_type(value_type&&) = default;
            value_type& operator=(const value_type&) = delete;
            value_type& operator=(value_type&&) = default;

        private:
            rtti* _value;
            friend class bag;
        };

        using allocator_type = Allocator;
        using different_type = std::ptrdiff_t;
        using size_type = std::size_t;
        using reference = std::vector<value_type>::reference;
        using const_reference = std::vector<value_type>::const_reference;
        using pointer = std::vector<value_type>::pointer;
        using const_pointer = std::vector<value_type>::const_pointer;
        using iterator = std::vector<value_type>::iterator;
        using reverse_iterator = std::vector<value_type>::reverse_iterator;
        using const_reverse_iterator = std::vector<value_type>::const_reverse_iterator;
        using const_iterator = std::vector<value_type>::const_iterator;

        ~bag() { cleanup(); }
        bag() = default;
        bag(const bag& other) { assign(other.begin(), other.end()); }
        bag(bag&& other) noexcept :
            _storage(other._storage), _size(other._size), _end(other._end) {
            other._storage = other._size = other._end = nullptr;
        }

        template<class ...Args> bag(Args&& ...args) { assign(std::forward<Args>(args)...); }
        template<std::forward_iterator Ty> bag(Ty first, Ty last) { assign(first, last); }

        bag& operator=(const bag& other) {
            clear();
            assign(other.begin(), other.end());
            return *this;
        }

        bag& operator=(bag&& other) noexcept {
            cleanup();
            _storage = other._storage, _size = other._size, _end = other._end;
            other._storage = other._size = other._end = nullptr;
            return *this;
        }

        value_type& operator[](std::size_t i) { return _values[i]; }
        value_type& at(std::size_t i) { return _values.at(i); }
        value_type& front() { return _values.front(); }
        value_type& back() { return _values.back(); }
        std::byte* data() { return _storage; }

        const value_type& operator[](std::size_t i) const { return _values[i]; }
        const value_type& at(std::size_t i) const { return _values.at(i); }
        const value_type& front() const { return _values.front(); }
        const value_type& back() const { return _values.back(); }
        const std::byte* data() const { return _storage; }

        allocator_type get_allocator() const { return _alloc; }

        iterator begin() { return _values.begin(); }
        iterator end() { return _values.end(); }
        const_iterator begin() const { return _values.begin(); }
        const_iterator end() const { return _values.end(); }
        const_iterator cbegin() const { return _values.cbegin(); }
        const_iterator cend() const { return _values.cend(); }
        reverse_iterator rbegin() { return _values.rbegin(); }
        reverse_iterator rend() { return _values.rend(); }
        const_reverse_iterator rbegin() const { return _values.rbegin(); }
        const_reverse_iterator rend() const { return _values.rend(); }
        const_reverse_iterator crbegin() const { return _values.crbegin(); }
        const_reverse_iterator crend() const { return _values.crend(); }

        bool empty() const { return _values.empty(); }
        std::size_t size() const { return _values.size(); }
        std::size_t bytes() const { return std::distance(_storage, _size); }
        std::size_t capacity() const { return std::distance(_storage, _end); }
        std::size_t available() const { return std::distance(_size, _end); }

        void reserve(size_type new_bytes) { if (new_bytes > capacity()) _resize_to_bytes(new_bytes); }
        void shrink_to_fit() { _resize_to_bytes(bytes()); }

        template<class Ty> void push_back(const Ty& value) { emplace_back<Ty>(value); }
        template<class Ty> void push_back(Ty&& value) { emplace_back<std::decay_t<Ty>>(std::move(value)); }

        void pop_back() {
            auto& value = _values.back();
            _size -= value.size();
            value._value->~rtti();
            _values.pop_back();
        }

        void clear() {
            for (auto& value : _values) value._value->~rtti();
            _values.clear();
            _size = _storage;
        }

        void swap(bag& other) {
            std::swap(_storage, other._storage);
            std::swap(_size, other._size);
            std::swap(_end, other._end);
            std::swap(_values, other._values);
        }

        /**
         * Assign a pack of values.
         * @param ...args values to assign
         */
        template<class ...Args>
        void assign(Args&&...args) {
            clear();
            constexpr std::size_t needed = (sizeof(std::decay_t<Args>) + ...);
            reserve(needed);
            (emplace_back<std::decay_t<Args>>(std::forward<Args>(args)), ...);
        }

        /**
         * Assign a range of values.
         * @param first begin of range
         * @param last end of range
         */
        template<std::forward_iterator Ty>
            requires (!std::same_as<Ty, iterator>)
        void assign(Ty first, Ty last) {
            clear();
            reserve(std::distance(first, last) * sizeof(std::iter_value_t<Ty>));
            insert(begin(), first, last);
        }

        /**
         * Assign a range of bag values.
         * @param first begin of range
         * @param last end of range
         */
        void assign(const_iterator first, const_iterator last) {
            clear();
            reserve(std::distance(first->_value, last->_value));
            insert(begin(), first, last);
        }

        /**
         * Construct Ty at back using ...Args.
         * @tparam Ty type to emplace
         * @tparam ...Args types to construct with
         * @param ...args arguments to construct Ty with
         * @return reference to emplaced value
         */
        template<class Ty, class ...Args>
            requires (!std::same_as<std::decay_t<Args>, value_type> && ...)
        Ty& emplace_back(Args&&...args) {
            constexpr std::size_t needed = sizeof(rtti_typed<Ty>);
            if (needed > available()) reserve(capacity() * 2 + needed);
            rtti_typed<Ty>* obj = new (_size) rtti_typed<Ty>{ std::forward<Args>(args)... };
            _values.emplace_back(obj);
            _size += needed;
            return obj->value;
        }

        /**
         * Emplace a bag value to back.
         * @param val bag value
         * @return reference to emplaced bag value
         */
        value_type& emplace_back(const value_type& val) {
            const std::size_t needed = val.size();
            if (needed > available()) reserve(capacity() * 2 + needed);
            val._value->copy_construct(_size);
            rtti* obj = reinterpret_cast<rtti*>(_size);
            _size += needed;
            return _values.emplace_back(obj);
        }

        /**
         * Emplace Ty at pos using ...Args.
         * @tparam Ty type to emplace
         * @tparam ...Args types to construct Ty
         * @param pos position to construct at
         * @param ...args argument to construct Ty
         * @return reference to emplaced value
         */
        template<class Ty, class ...Args>
            requires (!std::same_as<std::decay_t<Args>, value_type> && ...)
        Ty& emplace(const_iterator pos, Args&&...args) {
            if (pos == end()) return emplace_back<Ty>(std::forward<Args>(args)...);
            iterator it = _c2i(pos);
            constexpr std::size_t needed = sizeof(rtti_typed<Ty>);
            std::byte* at = _move_from<false>(it, needed);
            rtti_typed<Ty>* obj = new (at) rtti_typed<Ty>{ std::forward<Args>(args)... };
            _values.emplace(it, obj);
            return obj->value;
        }

        /**
         * Emplace a bag value at pos.
         * @param pos position to emplace at
         * @param val bag value to emplace
         * @return iterator to emplaced bag value
         */
        iterator emplace(const_iterator pos, const value_type& val) {
            if (pos == end()) return emplace_back(val), --end();
            iterator it = _c2i(pos);
            const std::size_t needed = val.size();
            std::byte* at = _move_from<false>(it, needed);
            val._value->copy_construct(at);
            rtti* obj = reinterpret_cast<rtti*>(at);
            return _values.emplace(it, obj);
        }

        /**
         * Insert a range of elements at pos.
         * @param pos position to insert at
         * @param first begin of range
         * @param last end of range
         */
        template<std::forward_iterator Ty>
            requires (!std::same_as<Ty, iterator>)
        void insert(const_iterator pos, Ty first, Ty last) {
            using type = std::iter_value_t<Ty>;
            std::size_t index = std::distance(cbegin(), pos);
            while (first != last) emplace<type>(begin() + index, *first++), ++index;
        }

        /**
         * Insert a range of elements at pos.
         * @param pos position to insert at
         * @param first begin of range
         * @param last end of range
         */
        void insert(const_iterator pos, const_iterator first, const_iterator last) {
            std::size_t index = std::distance(cbegin(), pos);
            while (first != last) emplace(begin() + index, *first++), ++index;
        }

        /**
         * Insert a range of elements at pos.
         * @param pos position to insert at
         * @param first begin of range
         * @param last end of range
         */
        template<class Ty>
        void insert(const_iterator pos, Ty&& val) {
            emplace<std::decay_t<Ty>>(pos, std::forward<Ty>(val));
        }

        /**
         * Erase element at pos.
         * @param pos position to erase
         * @return iterator following removed value
         */
        iterator erase(const_iterator pos) {
            if (pos == end()) return end();
            iterator it = _c2i(pos);
            std::size_t bytes = it->_value->size();
            it->_value->~rtti(); // Delete at iterator
            _move_from<true>(it + 1, bytes); // Move everything after it over by its size in bytes
            return _values.erase(it); // Remove from values
        }

        /**
         * Erase range of elements.
         * @param first begin of range
         * @param last end of range
         * @return iterator following last removed value
         */
        iterator erase(const_iterator first, const_iterator last) {
            if (first == last) return _c2i(last);
            iterator fst = _c2i(first), lst = _c2i(last);
            std::size_t bytes = 0;
            for (; fst != lst; ++fst) {
                bytes += fst->size();
                fst->_value->~rtti();
            }
            _move_from<true>(lst, bytes);
            return _values.erase(first, last);
        }

    private:
        constexpr static std::size_t initial_size = 64;

        std::vector<value_type> _values{};
        [[no_unique_address]] allocator_type _alloc;
        std::byte* _storage = _alloc.allocate(initial_size);
        std::byte* _size = _storage;
        std::byte* _end = _storage + initial_size;

        /**
         * Const iterator to normal interator.
         * @param it const iterator
         * @return interator
         */
        iterator _c2i(const_iterator it) {
            return _values.begin() + std::distance(_values.cbegin(), it);
        }

        void cleanup() {
            clear();
            _alloc.deallocate(_storage, capacity());
            _storage = _size = _end = nullptr;
        }

        /**
         * Resize storage to given amount of bytes.
         * Doesn't check if all values fit in this size!
         * @param new_bytes amount of bytes to resize to
         */
        void _resize_to_bytes(std::size_t new_bytes) {
            if (capacity() == new_bytes) return;
            std::byte* fresh = _alloc.allocate(new_bytes);
            for (auto& value : _values) {
                std::byte* location = fresh + std::distance(_storage,
                    reinterpret_cast<std::byte*>(value._value));
                value._value->move_construct(location);
                value._value->~rtti();
                value._value = reinterpret_cast<rtti*>(location);
            }
            _alloc.deallocate(_storage, capacity());
            _size = fresh + bytes();
            _end = fresh + new_bytes;
            _storage = fresh;
        }

        /**
         * Move everything after pos a certain amount of bytes.
         * @tparam Backwards if true, move -bytes, otherwise bytes
         * @param pos first iterator
         * @param bytes amount of bytes to move
         */
        template<bool Backwards>
        std::byte* _move_from(iterator pos, std::size_t move_bytes) {
            if (bytes() + move_bytes > capacity()) reserve(capacity() * 2 + move_bytes);
            iterator it = Backwards ? pos : end();
            iterator it_end = Backwards ? end() : pos;
            while (it != it_end) {
                if constexpr (!Backwards) --it;
                const std::size_t obj_size = it->size();
                std::byte* to = reinterpret_cast<std::byte*>(it->_value) + (Backwards ? -move_bytes : move_bytes);
                if (obj_size <= move_bytes) { // Can move without overlapping itself
                    it->_value->move_construct(to);
                    it->_value->~rtti();
                    it->_value = reinterpret_cast<rtti*>(to);
                } else { // Otherwise, first move to end of capacity, then to destination
                    // If not enough unused memory at end of capacity, reserve more
                    const std::size_t temp_space_needed = Backwards ? obj_size : obj_size + move_bytes;
                    if (temp_space_needed > available()) reserve(capacity() * 2 + temp_space_needed);
                    rtti* temp = reinterpret_cast<rtti*>(Backwards ? _size : _size + move_bytes);
                    it->_value->move_construct(reinterpret_cast<std::byte*>(temp)); // Move away from location
                    it->_value->~rtti(); // Remove from location
                    it->_value = reinterpret_cast<rtti*>(to); // Set new location
                    temp->move_construct(to); // Move from temp location
                    temp->~rtti(); // Remove from temp location
                }
                if constexpr (Backwards) ++it;
            }
            if constexpr (Backwards) {
                _size -= move_bytes;
                return reinterpret_cast<std::byte*>(pos->_value) - move_bytes;
            } else {
                _size += move_bytes;
                return reinterpret_cast<std::byte*>(it->_value) - move_bytes;
            }
        }
    };
}