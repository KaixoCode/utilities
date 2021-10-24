#include <utility>
#include "nmmintrin.h" // for SSE4.2
#include "immintrin.h" // for AVX 

template<class _Ty, size_t N>
struct vec_union {
    consteval static _Ty _consteval_at(const std::size_t)
    {
        static_assert(false, "Index out of bounds");
    }
};

template<class _Ty>
struct vec_union<_Ty, 1> {
    consteval _Ty _consteval_at(const std::size_t index)
        const {
        return index == 0 ? x : vec_union<_Ty, 0>::_consteval_at(index);
    }

    union __declspec(align(sizeof _Ty)) {
        _Ty x; _Ty r; _Ty h;
    };
};

template<class _Ty>
struct vec_union<_Ty, 2> : vec_union<_Ty, 1> {
    consteval _Ty _consteval_at(const std::size_t index)
        const {
        return index == 1 ? y : vec_union<_Ty, 1>::_consteval_at(index);
    }

    union __declspec(align(sizeof _Ty)) {
        _Ty y; _Ty g; _Ty s;
    };
};

template<class _Ty>
struct vec_union<_Ty, 3> : vec_union<_Ty, 2> {
    consteval _Ty _consteval_at(const std::size_t index)
        const {
        return index == 2 ? z : vec_union<_Ty, 2>::_consteval_at(index);
    }

    union __declspec(align(sizeof _Ty)) {
        _Ty z; _Ty b; _Ty v; _Ty width;
    };
};

template<class _Ty>
struct vec_union<_Ty, 4> : vec_union<_Ty, 3> {
    consteval _Ty _consteval_at(const std::size_t index)
        const {
        return index == 3 ? w : vec_union<_Ty, 3>::_consteval_at(index);
    }

    union __declspec(align(sizeof _Ty)) {
        _Ty w; _Ty a; _Ty height;
    };
};

template<std::floating_point _Ty, size_t N> requires (N >= 2 && N <= 4)
struct vec : vec_union<_Ty, N> {
    using value_type = _Ty;
    using reference = value_type&;
    using const_reference = const reference;
    using pointer = value_type*;
    using const_pointer = const pointer;
    using size_type = size_t;

    constexpr static size_type size = N;
    constexpr static size_type value_bytes = sizeof value_type;

    using simd_type = std::conditional_t<value_bytes == 8, std::conditional_t<size == 2, __m128d, __m256d>, __m128>;

    inline simd_type to_simd() const {
        if constexpr (value_bytes == 8 && size == 2) return    _mm_set_pd(this->y, this->x);
        if constexpr (value_bytes == 8 && size == 3) return _mm256_set_pd(0, this->z, this->y, this->x);
        if constexpr (value_bytes == 8 && size == 4) return _mm256_set_pd(this->w, this->z, this->y, this->x);
        if constexpr (value_bytes == 4 && size == 2) return    _mm_set_ps(0, 0, this->y, this->x);
        if constexpr (value_bytes == 4 && size == 3) return    _mm_set_ps(0, this->z, this->y, this->x);
        if constexpr (value_bytes == 4 && size == 4) return    _mm_set_ps(this->w, this->z, this->y, this->x);
    }

    static inline simd_type add(simd_type& a, simd_type& b) {
        if constexpr (value_bytes == 8 && size == 2) return _mm_add_pd(a, b);
        if constexpr (value_bytes == 8) return _mm256_add_pd(a, b);
        if constexpr (value_bytes == 4) return _mm_add_ps(a, b);
    }

    static inline simd_type subtract(simd_type& a, simd_type& b) {
        if constexpr (value_bytes == 8 && size == 2) return _mm_sub_pd(a, b);
        if constexpr (value_bytes == 8) return _mm256_sub_pd(a, b);
        if constexpr (value_bytes == 4) return _mm_sub_ps(a, b);
    }

    static inline simd_type multiply(simd_type& a, simd_type& b) {
        if constexpr (value_bytes == 8 && size == 2) return _mm_mul_pd(a, b);
        if constexpr (value_bytes == 8) return _mm256_mul_pd(a, b);
        if constexpr (value_bytes == 4) return _mm_mul_ps(a, b);
    }

    static inline simd_type divide(simd_type& a, simd_type& b) {
        if constexpr (value_bytes == 8 && size == 2) return _mm_div_pd(a, b);
        if constexpr (value_bytes == 8) return _mm256_div_pd(a, b);
        if constexpr (value_bytes == 4) return _mm_div_ps(a, b);
    }

    inline auto data()->value_type(&)[size] { return reinterpret_cast<value_type(&)[size]>(*this); }
    inline auto data() const -> const value_type(&)[size] { return reinterpret_cast<const value_type(&)[size]>(*this); }
    inline reference at(size_type index) { return data()[index]; }
    inline const_reference at(size_type index) const requires (!std::is_constant_evaluated()) { return data()[index]; }
    inline reference operator[](size_type index) { return data()[index]; }
    inline const_reference operator[](size_type index) const requires (!std::is_constant_evaluated()) { return data()[index]; }

    consteval value_type at(size_type index) const { return vec_union<_Ty, N>::_consteval_at(index); }
    consteval value_type operator[](size_type index) const { return vec_union<_Ty, N>::_consteval_at(index); }

    vec& operator+=(const vec& other) { fast_inplace<add>(other); return *this; }
    vec& operator-=(const vec& other) { fast_inplace<subtract>(other); return *this; }
    vec& operator*=(const vec& other) { fast_inplace<multiply>(other); return *this; }
    vec& operator/=(const vec& other) { fast_inplace<divide>(other); return *this; }

    auto operator+(const vec& other) const { return fast_new<add>(other); }
    auto operator-(const vec& other) const { return fast_new<subtract>(other); }
    auto operator*(const vec& other) const { return fast_new<multiply>(other); }
    auto operator/(const vec& other) const { return fast_new<divide>(other); }

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
};
