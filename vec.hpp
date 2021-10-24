#include <concepts>
#include <memory>
#include <utility>
#include "nmmintrin.h" // for SSE4.2
#include "immintrin.h" // for AVX 

template<class, size_t>
struct vec_union;

template<std::floating_point, size_t>
struct vec;

template<class _Ty>
struct vec_union<_Ty, 2> {
    union alignas(sizeof _Ty) {
        _Ty data[2];

        struct alignas(sizeof _Ty) {
            union alignas(sizeof _Ty) { _Ty x; };
            union alignas(sizeof _Ty) { _Ty y; };
        };
    };
};

template<class _Ty>
struct vec_union<_Ty, 3> {
    union alignas(sizeof _Ty) {
        _Ty data[3];

        struct alignas(sizeof _Ty) {
            union alignas(sizeof _Ty) { _Ty x; _Ty r; _Ty h; };
            union alignas(sizeof _Ty) { _Ty y; _Ty g; _Ty s; };
            union alignas(sizeof _Ty) { _Ty z; _Ty b; _Ty v; };
        };
        struct alignas(sizeof _Ty) {
            union alignas(sizeof _Ty) { vec<_Ty, 2> xy; vec<_Ty, 2> rg; vec<_Ty, 2> hs; };
            union alignas(sizeof _Ty) { _Ty z; _Ty b; _Ty v; };
        };
        struct alignas(sizeof _Ty) {
            union alignas(sizeof _Ty) { _Ty x; _Ty r; _Ty h; };
            union alignas(sizeof _Ty) { vec<_Ty, 2> yz; vec<_Ty, 2> gb; vec<_Ty, 2> sv; };
        };
    };
};

template<class _Ty>
struct vec_union<_Ty, 4> {
    union alignas(sizeof _Ty) {
        _Ty data[4];

        struct alignas(sizeof _Ty) {
            union alignas(sizeof _Ty) { _Ty x; _Ty r; _Ty h; };
            union alignas(sizeof _Ty) { _Ty y; _Ty g; _Ty s; };
            union alignas(sizeof _Ty) { _Ty z; _Ty b; _Ty v; _Ty width; };
            union alignas(sizeof _Ty) { _Ty w; _Ty a; _Ty height; };
        };
        struct alignas(sizeof _Ty) {
            union alignas(sizeof _Ty) { _Ty x; _Ty r; _Ty h; };
            union alignas(sizeof _Ty) { vec<_Ty, 2> yz; vec<_Ty, 2> gb; vec<_Ty, 2> sv; };
            union alignas(sizeof _Ty) { _Ty w; _Ty a; };
        };
        struct alignas(sizeof _Ty) {
            union alignas(sizeof _Ty) { vec<_Ty, 3> xyz; vec<_Ty, 3> rgb; vec<_Ty, 3> hsv; };
            union alignas(sizeof _Ty) { _Ty w; _Ty a; };
        };
        struct alignas(sizeof _Ty) {
            union alignas(sizeof _Ty) { vec<_Ty, 2> xy; vec<_Ty, 2> rg; vec<_Ty, 2> hs; vec<_Ty, 2> position; };
            union alignas(sizeof _Ty) { vec<_Ty, 2> zw; vec<_Ty, 2> ba; vec<_Ty, 2> va; vec<_Ty, 2> size; };
        };
        struct alignas(sizeof _Ty) {
            union alignas(sizeof _Ty) { _Ty x; _Ty r; _Ty h; };
            union alignas(sizeof _Ty) { vec<_Ty, 3> yzw; vec<_Ty, 3> bga; vec<_Ty, 3> sva; };
        };
    };
};

template<std::floating_point _Ty, std::size_t N> requires (N >= 2 && N <= 4)
struct vec<_Ty, N> : vec_union<_Ty, N> {
    using value_type = _Ty;
    using reference = value_type&;
    using const_reference = const reference;
    using pointer = value_type*;
    using const_pointer = value_type const*;
    using size_type = std::size_t;

    constexpr static size_type size = N;
    constexpr static size_type value_bytes = sizeof value_type;
    constexpr static size_type bytes = N * sizeof value_type;

    using simd_type = std::conditional_t<value_bytes == 8, std::conditional_t<size == 2, __m128d, __m256d>, __m128>;

    constexpr vec() = default;
    constexpr vec(const vec&) = default;
    constexpr vec(vec&&) = default;
    constexpr vec& operator=(const vec&) = default;
    constexpr vec& operator=(vec&&) = default;

    constexpr vec(
        const value_type& p1,
        const value_type& p2,
        const value_type& p3,
        const value_type& p4) requires (N == 4)
        : vec_union<_Ty, N>{ .x = p1, .y = p2, .z = p3, .w = p4 } {}

    constexpr vec(
        const value_type& p1,
        const vec<value_type, 2>& p2,
        const value_type& p3) requires (N == 4)
        : vec_union<_Ty, N>{ .x = p1, .y = p2.x, .z = p2.y, .w = p3 } {}

    constexpr vec(
        const vec<value_type, 2>& p1,
        const vec<value_type, 2>& p2) requires (N == 4)
        : vec_union<_Ty, N>{ .xy = p1, .zw = p2 } {}    
    
    constexpr vec(
        const vec<value_type, 3>& p1,
        const value_type& p2) requires (N == 4)
        : vec_union<_Ty, N>{ .x = p1.x, .y = p1.y, .z = p1.z, .w = p2 } {}

    constexpr vec(
        const value_type& p1,
        const vec<value_type, 3>& p2) requires (N == 4)
        : vec_union<_Ty, N>{ .x = p1, .y = p2.x, .z = p2.y, .w = p2.z } {}

    constexpr vec(const value_type& p1) requires (N == 4)
        : vec_union<_Ty, N>{ .x = p1, .y = p1, .z = p1, .w = p1 } {}

    constexpr vec(
        const value_type& p1,
        const value_type& p2,
        const value_type& p3) requires (N == 3)
        : vec_union<_Ty, N>{ .x = p1, .y = p2, .z = p3 } {}

    constexpr vec(
        const vec<value_type, 2>& p1,
        const value_type& p2) requires (N == 3)
        : vec_union<_Ty, N>{ .x = p1.x, .y = p1.y, .z = p2 } {}

    constexpr vec(
        const value_type& p1,
        const vec<value_type, 2>& p2) requires (N == 3)
        : vec_union<_Ty, N>{ .x = p1, .y = p2.x, .z = p2.y } {}

    constexpr vec(const value_type& p1) requires (N == 3)
        : vec_union<_Ty, N>{ .x = p1, .y = p1, .z = p1 } {}

    constexpr vec(
        const value_type& p1,
        const value_type& p2) requires (N == 2)
        : vec_union<_Ty, N>{ .x = p1, .y = p2 } {}

    constexpr vec(const value_type& p1) requires (N == 2)
        : vec_union<_Ty, N>{ .x = p1, .y = p1 } {}

    inline simd_type to_simd() const {
        const auto _data = this->data();
        if constexpr (value_bytes == 8 && size == 2) return    _mm_load_pd(_data);
        if constexpr (value_bytes == 8 && size == 3) return _mm256_set_pd(0, _data[2], _data[1], _data[0]);
        if constexpr (value_bytes == 8 && size == 4) return _mm256_load_pd(_data);
        if constexpr (value_bytes == 4 && size == 2) return    _mm_set_ps(0, 0, _data[1], _data[0]);
        if constexpr (value_bytes == 4 && size == 3) return    _mm_set_ps(0, _data[2], _data[1], _data[0]);
        if constexpr (value_bytes == 4 && size == 4) return    _mm_load_ps(_data);
    }

    static inline simd_type to_simd(value_type& a) {
        if constexpr (value_bytes == 8 && size == 2) return _mm_set1_pd(a);
        if constexpr (value_bytes == 4 && size == 4) return _mm_set1_ps(a);
    }

    static inline simd_type add(simd_type& a, simd_type& b) {
        if constexpr (value_bytes == 8 && size == 2) return _mm_add_pd(a, b);
        if constexpr (value_bytes == 8 && size != 2) return _mm256_add_pd(a, b);
        if constexpr (value_bytes == 4) return _mm_add_ps(a, b);
    }

    static inline simd_type subtract(simd_type& a, simd_type& b) {
        if constexpr (value_bytes == 8 && size == 2) return _mm_sub_pd(a, b);
        if constexpr (value_bytes == 8 && size != 2) return _mm256_sub_pd(a, b);
        if constexpr (value_bytes == 4) return _mm_sub_ps(a, b);
    }

    static inline simd_type multiply(simd_type& a, simd_type& b) {
        if constexpr (value_bytes == 8 && size == 2) return _mm_mul_pd(a, b);
        if constexpr (value_bytes == 8 && size != 2) return _mm256_mul_pd(a, b);
        if constexpr (value_bytes == 4) return _mm_mul_ps(a, b);
    }

    static inline simd_type divide(simd_type& a, simd_type& b) {
        if constexpr (value_bytes == 8 && size == 2) return _mm_div_pd(a, b);
        if constexpr (value_bytes == 8 && size != 2) return _mm256_div_pd(a, b);
        if constexpr (value_bytes == 4) return _mm_div_ps(a, b);
    }

    explicit inline operator pointer() { return reinterpret_cast<pointer>(this); }
    explicit inline operator const_pointer() const { return reinterpret_cast<const_pointer>(this); }
    
    inline pointer data() { return reinterpret_cast<pointer>(this); }
    inline const_pointer data() const { return reinterpret_cast<const_pointer>(this); }
    inline reference at(size_type index) { return data()[index]; }
    inline const_reference at(size_type index) const { return data()[index]; }
    inline reference operator[](size_type index) { return data()[index]; }
    inline const_reference operator[](size_type index) const { return data()[index]; }

    inline vec& operator+=(const vec& other) { fast_inplace<add>(other); return *this; }
    inline vec& operator-=(const vec& other) { fast_inplace<subtract>(other); return *this; }
    inline vec& operator*=(const vec& other) { fast_inplace<multiply>(other); return *this; }
    inline vec& operator/=(const vec& other) { fast_inplace<divide>(other); return *this; }

    inline auto operator+(const vec& other) const { return fast_new<add>(other); }
    inline auto operator-(const vec& other) const { return fast_new<subtract>(other); }
    inline auto operator*(const vec& other) const { return fast_new<multiply>(other); }
    inline auto operator/(const vec& other) const { return fast_new<divide>(other); }

    inline vec& operator+=(const_reference other) { fast_inplace<add>(other); return *this; }
    inline vec& operator-=(const_reference other) { fast_inplace<subtract>(other); return *this; }
    inline vec& operator*=(const_reference other) { fast_inplace<multiply>(other); return *this; }
    inline vec& operator/=(const_reference other) { fast_inplace<divide>(other); return *this; }

    inline auto operator+(const_reference other) const { return fast_new<add>(other); }
    inline auto operator-(const_reference other) const { return fast_new<subtract>(other); }
    inline auto operator*(const_reference other) const { return fast_new<multiply>(other); }
    inline auto operator/(const_reference other) const { return fast_new<divide>(other); }

    constexpr bool operator==(const vec& other) const { return !std::memcmp(this, &other, bytes); };

    template<auto _Fun>
    inline void fast_inplace(const vec& other) {
        simd_type _data1 = this->to_simd();
        simd_type _data2 = other.to_simd();
        simd_type _res = _Fun(_data1, _data2);
        std::memcpy(this->data(), &_res, size * value_bytes);
    }

    template<auto _Fun>
    inline auto fast_new(const vec& other) const {
        simd_type _data1 = this->to_simd();
        simd_type _data2 = other.to_simd();
        simd_type _res = _Fun(_data1, _data2);
        value_type _new[size];
        std::memcpy(_new, &_res, size * value_bytes);
        return *reinterpret_cast<vec*>(&_new);
    }

    template<auto _Fun>
    inline void fast_inplace(const_reference other) {
        simd_type _data1 = this->to_simd();
        simd_type _data2 = to_simd(other);
        simd_type _res = _Fun(_data1, _data2);
        std::memcpy(this->data(), &_res, size * value_bytes);
    }

    template<auto _Fun>
    inline auto fast_new(const_reference other) const {
        simd_type _data1 = this->to_simd();
        simd_type _data2 = to_simd(other);
        simd_type _res = _Fun(_data1, _data2);
        value_type _new[size];
        std::memcpy(_new, &_res, size * value_bytes);
        return *reinterpret_cast<vec*>(&_new);
    }

    template <size_type I> auto& get()& {
        if constexpr (I == 0) return this->x;
        else if constexpr (I == 1) return this->y;
        else if constexpr (I == 2) return this->z;
        else if constexpr (I == 3) return this->w;
    }

    template <size_type I> auto const& get() const& {
        if constexpr (I == 0) return this->x;
        else if constexpr (I == 1) return this->y;
        else if constexpr (I == 2) return this->z;
        else if constexpr (I == 3) return this->w;
    }
};

namespace std {
    template <class _Ty, std::size_t N> struct tuple_size<vec<_Ty, N>> : std::integral_constant<std::size_t, N> { };
                            
    template <class _Ty, std::size_t N> struct tuple_element<0, vec<_Ty, N>> { using type = _Ty; };
    template <class _Ty, std::size_t N> struct tuple_element<1, vec<_Ty, N>> { using type = _Ty; };
    template <class _Ty, std::size_t N> struct tuple_element<2, vec<_Ty, N>> { using type = _Ty; };
    template <class _Ty, std::size_t N> struct tuple_element<3, vec<_Ty, N>> { using type = _Ty; };
}