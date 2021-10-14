#pragma once
#include <utility>

template<class T>
class dynamic_array {
    T* m_Data{ nullptr };
    size_t m_Size{ 0 };
public:

    // Default constructor, initializes to nullptr and size = 0.
    dynamic_array() {}

    // Create a dynamic array of size.
    dynamic_array(size_t size)
        : m_Data(new T[size]), m_Size(size) {}

    // !WARNING! This takes in a raw pointer, and means that this dynamic array
    // will now take ownership of this raw pointer, and delete it when it is destroyed.
    dynamic_array(T* data, size_t size)
        : m_Data(data), m_Size(size) {}

    // Copy constructor, creates a copy of the other dynamic array by
    // allocating a new buffer of the same size, and then copying all elements.
    dynamic_array(const dynamic_array& other)
        : m_Data(new T[other.m_Size]), m_Size(other.m_Size) {
        std::copy_n(other.m_Data, m_Size, m_Data);
    }

    // Move constructor, invalidates the other dynamic array by setting
    // its data to nullptr and size to 0.
    dynamic_array(dynamic_array&& other)
        : m_Data(other.m_Data), m_Size(other.m_Size) {
        other.invalidate();
    }

    // Destructor makes sure to free the data.
    ~dynamic_array() {
        delete[] m_Data;
    }

    // Copy assignment, first we delete our previous data, then
    // we create a new buffer of the new size, and copy the data from other.
    dynamic_array& operator=(const dynamic_array& other) {
        delete[] m_Data;
        m_Data = new T[other.m_Size];
        m_Size = other.m_Size;
        std::copy_n(other.m_Data, m_Size, m_Data);
    }

    // Move assignments, delete our previous data, then move the
    // data from the other into here, and invalidate the other dynamic array.
    dynamic_array& operator=(dynamic_array&& other) {
        delete[] m_Data;
        m_Data = other.m_Data;
        m_Size = other.m_Size;
        other.invalidate();
    }

    // Const and non-const index operators
    inline auto& operator[](size_t index) { return m_Data[index]; }
    inline const auto& operator[](size_t index) const { return m_Data[index]; }

    // Const and non-const front/back.
    inline auto& front() { return *begin(); }
    inline auto& back() { return *(end() - 1); }
    inline const auto& front() const { return *begin(); }
    inline const auto& back() const { return *(end() - 1); }

    // Begin and end are used in range based loop syntax
    // like "for (auto& i : array)".
    inline auto begin() { return m_Data; }
    inline auto end() { return m_Data + m_Size; }
    inline auto begin() const { return m_Data; }
    inline auto end() const { return m_Data + m_Size; }

    inline auto fill(const T& value) { std::fill(begin(), end(), value); }

    inline auto size() const { return m_Size; }
    inline auto size_bytes() const { return m_Size * sizeof T; }
    inline auto empty() const { return m_Size == 0; }

private:
    // Invalidate this dynamic array by setting data and size to 0.
    inline auto invalidate() { m_Data = m_Size = nullptr; }
};

template<class T>
dynamic_array(T*, size_t)->dynamic_array<T>;