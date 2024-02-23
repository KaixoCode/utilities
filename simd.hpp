#pragma once
#include <cstddef>
#include <concepts>
#include <tuple>
#include <algorithm>
#include <numeric>

#include <intrin.h>

#define KAIXO_VECTORCALL __vectorcall

namespace kaixo {

    template<class = float, std::size_t = 256>
    struct simd;

    template<class Ty, std::size_t Bits> struct underlying_simd;
    template<> struct underlying_simd<int, 128> : std::type_identity<__m128i> {};
    template<> struct underlying_simd<int, 256> : std::type_identity<__m256i> {};
    template<> struct underlying_simd<int, 512> : std::type_identity<__m512i> {};
    template<> struct underlying_simd<float, 128> : std::type_identity<__m128> {};
    template<> struct underlying_simd<float, 256> : std::type_identity<__m256> {};
    template<> struct underlying_simd<float, 512> : std::type_identity<__m512> {};
    template<> struct underlying_simd<double, 128> : std::type_identity<__m128d> {};
    template<> struct underlying_simd<double, 256> : std::type_identity<__m256d> {};
    template<> struct underlying_simd<double, 512> : std::type_identity<__m512d> {};
    template<class Ty, std::size_t Bits> using underlying_simd_t = typename underlying_simd<Ty, Bits>::type;

    __m128i KAIXO_VECTORCALL operator-(__m128i a, __m128i b) noexcept { return _mm_sub_epi32(a, b); }
    __m256i KAIXO_VECTORCALL operator-(__m256i a, __m256i b) noexcept { return _mm256_sub_epi32(a, b); }
    __m512i KAIXO_VECTORCALL operator-(__m512i a, __m512i b) noexcept { return _mm512_sub_epi32(a, b); }
    __m128 KAIXO_VECTORCALL operator-(__m128 a, __m128 b) noexcept { return _mm_sub_ps(a, b); }
    __m256 KAIXO_VECTORCALL operator-(__m256 a, __m256 b) noexcept { return _mm256_sub_ps(a, b); }
    __m512 KAIXO_VECTORCALL operator-(__m512 a, __m512 b) noexcept { return _mm512_sub_ps(a, b); }
    __m128d KAIXO_VECTORCALL operator-(__m128d a, __m128d b) noexcept { return _mm_sub_pd(a, b); }
    __m256d KAIXO_VECTORCALL operator-(__m256d a, __m256d b) noexcept { return _mm256_sub_pd(a, b); }
    __m512d KAIXO_VECTORCALL operator-(__m512d a, __m512d b) noexcept { return _mm512_sub_pd(a, b); }

    __m128i KAIXO_VECTORCALL operator*(__m128i a, __m128i b) noexcept { return _mm_mul_epi32(a, b); }
    __m256i KAIXO_VECTORCALL operator*(__m256i a, __m256i b) noexcept { return _mm256_mul_epi32(a, b); }
    __m512i KAIXO_VECTORCALL operator*(__m512i a, __m512i b) noexcept { return _mm512_mul_epi32(a, b); }
    __m128 KAIXO_VECTORCALL operator*(__m128 a, __m128 b) noexcept { return _mm_mul_ps(a, b); }
    __m256 KAIXO_VECTORCALL operator*(__m256 a, __m256 b) noexcept { return _mm256_mul_ps(a, b); }
    __m512 KAIXO_VECTORCALL operator*(__m512 a, __m512 b) noexcept { return _mm512_mul_ps(a, b); }
    __m128d KAIXO_VECTORCALL operator*(__m128d a, __m128d b) noexcept { return _mm_mul_pd(a, b); }
    __m256d KAIXO_VECTORCALL operator*(__m256d a, __m256d b) noexcept { return _mm256_mul_pd(a, b); }
    __m512d KAIXO_VECTORCALL operator*(__m512d a, __m512d b) noexcept { return _mm512_mul_pd(a, b); }

    __m128i KAIXO_VECTORCALL operator+(__m128i a, __m128i b) noexcept { return _mm_add_epi32(a, b); }
    __m256i KAIXO_VECTORCALL operator+(__m256i a, __m256i b) noexcept { return _mm256_add_epi32(a, b); }
    __m512i KAIXO_VECTORCALL operator+(__m512i a, __m512i b) noexcept { return _mm512_add_epi32(a, b); }
    __m128 KAIXO_VECTORCALL operator+(__m128 a, __m128 b) noexcept { return _mm_add_ps(a, b); }
    __m256 KAIXO_VECTORCALL operator+(__m256 a, __m256 b) noexcept { return _mm256_add_ps(a, b); }
    __m512 KAIXO_VECTORCALL operator+(__m512 a, __m512 b) noexcept { return _mm512_add_ps(a, b); }
    __m128d KAIXO_VECTORCALL operator+(__m128d a, __m128d b) noexcept { return _mm_add_pd(a, b); }
    __m256d KAIXO_VECTORCALL operator+(__m256d a, __m256d b) noexcept { return _mm256_add_pd(a, b); }
    __m512d KAIXO_VECTORCALL operator+(__m512d a, __m512d b) noexcept { return _mm512_add_pd(a, b); }

    __m128i KAIXO_VECTORCALL operator/(__m128i a, __m128i b) noexcept { return _mm_div_epi32(a, b); }
    __m256i KAIXO_VECTORCALL operator/(__m256i a, __m256i b) noexcept { return _mm256_div_epi32(a, b); }
    __m512i KAIXO_VECTORCALL operator/(__m512i a, __m512i b) noexcept { return _mm512_div_epi32(a, b); }
    __m128 KAIXO_VECTORCALL operator/(__m128 a, __m128 b) noexcept { return _mm_div_ps(a, b); }
    __m256 KAIXO_VECTORCALL operator/(__m256 a, __m256 b) noexcept { return _mm256_div_ps(a, b); }
    __m512 KAIXO_VECTORCALL operator/(__m512 a, __m512 b) noexcept { return _mm512_div_ps(a, b); }
    __m128d KAIXO_VECTORCALL operator/(__m128d a, __m128d b) noexcept { return _mm_div_pd(a, b); }
    __m256d KAIXO_VECTORCALL operator/(__m256d a, __m256d b) noexcept { return _mm256_div_pd(a, b); }
    __m512d KAIXO_VECTORCALL operator/(__m512d a, __m512d b) noexcept { return _mm512_div_pd(a, b); }
    
    __m128i KAIXO_VECTORCALL operator&(__m128i a, __m128i b) noexcept { return _mm_and_epi32(a, b); }
    __m256i KAIXO_VECTORCALL operator&(__m256i a, __m256i b) noexcept { return _mm256_and_epi32(a, b); }
    __m512i KAIXO_VECTORCALL operator&(__m512i a, __m512i b) noexcept { return _mm512_and_epi32(a, b); }
    __m128 KAIXO_VECTORCALL operator&(__m128 a, __m128 b) noexcept { return _mm_and_ps(a, b); }
    __m256 KAIXO_VECTORCALL operator&(__m256 a, __m256 b) noexcept { return _mm256_and_ps(a, b); }
    __m512 KAIXO_VECTORCALL operator&(__m512 a, __m512 b) noexcept { return _mm512_and_ps(a, b); }
    __m128d KAIXO_VECTORCALL operator&(__m128d a, __m128d b) noexcept { return _mm_and_pd(a, b); }
    __m256d KAIXO_VECTORCALL operator&(__m256d a, __m256d b) noexcept { return _mm256_and_pd(a, b); }
    __m512d KAIXO_VECTORCALL operator&(__m512d a, __m512d b) noexcept { return _mm512_and_pd(a, b); }

    __m128i KAIXO_VECTORCALL operator|(__m128i a, __m128i b) noexcept { return _mm_or_epi32(a, b); }
    __m256i KAIXO_VECTORCALL operator|(__m256i a, __m256i b) noexcept { return _mm256_or_epi32(a, b); }
    __m512i KAIXO_VECTORCALL operator|(__m512i a, __m512i b) noexcept { return _mm512_or_epi32(a, b); }
    __m128 KAIXO_VECTORCALL operator|(__m128 a, __m128 b) noexcept { return _mm_or_ps(a, b); }
    __m256 KAIXO_VECTORCALL operator|(__m256 a, __m256 b) noexcept { return _mm256_or_ps(a, b); }
    __m512 KAIXO_VECTORCALL operator|(__m512 a, __m512 b) noexcept { return _mm512_or_ps(a, b); }
    __m128d KAIXO_VECTORCALL operator|(__m128d a, __m128d b) noexcept { return _mm_or_pd(a, b); }
    __m256d KAIXO_VECTORCALL operator|(__m256d a, __m256d b) noexcept { return _mm256_or_pd(a, b); }
    __m512d KAIXO_VECTORCALL operator|(__m512d a, __m512d b) noexcept { return _mm512_or_pd(a, b); }
    
    __mmask8 KAIXO_VECTORCALL operator==(__m128i a, __m128i b) noexcept { return _mm_cmpeq_epi32_mask(a, b); }
    __mmask8 KAIXO_VECTORCALL operator==(__m256i a, __m256i b) noexcept { return _mm256_cmpeq_epi32_mask(a, b); }
    __mmask16 KAIXO_VECTORCALL operator==(__m512i a, __m512i b) noexcept { return _mm512_cmpeq_epi32_mask(a, b); }
    __mmask8 KAIXO_VECTORCALL operator==(__m128 a, __m128 b) noexcept { return _mm_cmp_ps_mask(a, b, _CMP_EQ_OS); }
    __mmask8 KAIXO_VECTORCALL operator==(__m256 a, __m256 b) noexcept { return _mm256_cmp_ps_mask(a, b, _CMP_EQ_OS); }
    __mmask16 KAIXO_VECTORCALL operator==(__m512 a, __m512 b) noexcept { return _mm512_cmpeq_ps_mask(a, b); }
    __mmask8 KAIXO_VECTORCALL operator==(__m128d a, __m128d b) noexcept { return _mm_cmp_pd_mask(a, b, _CMP_EQ_OS); }
    __mmask8 KAIXO_VECTORCALL operator==(__m256d a, __m256d b) noexcept { return _mm256_cmp_pd_mask(a, b, _CMP_EQ_OS); }
    __mmask16 KAIXO_VECTORCALL operator==(__m512d a, __m512d b) noexcept { return _mm512_cmpeq_pd_mask(a, b); }
    
    __mmask8 KAIXO_VECTORCALL operator!=(__m128i a, __m128i b) noexcept { return _mm_cmpneq_epi32_mask(a, b); }
    __mmask8 KAIXO_VECTORCALL operator!=(__m256i a, __m256i b) noexcept { return _mm256_cmpneq_epi32_mask(a, b); }
    __mmask16 KAIXO_VECTORCALL operator!=(__m512i a, __m512i b) noexcept { return _mm512_cmpneq_epi32_mask(a, b); }
    __mmask8 KAIXO_VECTORCALL operator!=(__m128 a, __m128 b) noexcept { return _mm_cmp_ps_mask(a, b, _CMP_NEQ_OS); }
    __mmask8 KAIXO_VECTORCALL operator!=(__m256 a, __m256 b) noexcept { return _mm256_cmp_ps_mask(a, b, _CMP_NEQ_OS); }
    __mmask16 KAIXO_VECTORCALL operator!=(__m512 a, __m512 b) noexcept { return _mm512_cmpneq_ps_mask(a, b); }
    __mmask8 KAIXO_VECTORCALL operator!=(__m128d a, __m128d b) noexcept { return _mm_cmp_pd_mask(a, b, _CMP_NEQ_OS); }
    __mmask8 KAIXO_VECTORCALL operator!=(__m256d a, __m256d b) noexcept { return _mm256_cmp_pd_mask(a, b, _CMP_NEQ_OS); }
    __mmask16 KAIXO_VECTORCALL operator!=(__m512d a, __m512d b) noexcept { return _mm512_cmpneq_pd_mask(a, b); }
    
    __mmask8 KAIXO_VECTORCALL operator>(__m128i a, __m128i b) noexcept { return _mm_cmpgt_epi32_mask(a, b); }
    __mmask8 KAIXO_VECTORCALL operator>(__m256i a, __m256i b) noexcept { return _mm256_cmpgt_epi32_mask(a, b); }
    __mmask16 KAIXO_VECTORCALL operator>(__m512i a, __m512i b) noexcept { return _mm512_cmpgt_epi32_mask(a, b); }
    __mmask8 KAIXO_VECTORCALL operator>(__m128 a, __m128 b) noexcept { return _mm_cmp_ps_mask(a, b, _CMP_GT_OS); }
    __mmask8 KAIXO_VECTORCALL operator>(__m256 a, __m256 b) noexcept { return _mm256_cmp_ps_mask(a, b, _CMP_GT_OS); }
    __mmask16 KAIXO_VECTORCALL operator>(__m512 a, __m512 b) noexcept { return _mm512_cmp_ps_mask(a, b, _CMP_GT_OS); }
    __mmask8 KAIXO_VECTORCALL operator>(__m128d a, __m128d b) noexcept { return _mm_cmp_pd_mask(a, b, _CMP_GT_OS); }
    __mmask8 KAIXO_VECTORCALL operator>(__m256d a, __m256d b) noexcept { return _mm256_cmp_pd_mask(a, b, _CMP_GT_OS); }
    __mmask16 KAIXO_VECTORCALL operator>(__m512d a, __m512d b) noexcept { return _mm512_cmp_pd_mask(a, b, _CMP_GT_OS); }
    
    __mmask8 KAIXO_VECTORCALL operator<(__m128i a, __m128i b) noexcept { return _mm_cmplt_epi32_mask(a, b); }
    __mmask8 KAIXO_VECTORCALL operator<(__m256i a, __m256i b) noexcept { return _mm256_cmplt_epi32_mask(a, b); }
    __mmask16 KAIXO_VECTORCALL operator<(__m512i a, __m512i b) noexcept { return _mm512_cmplt_epi32_mask(a, b); }
    __mmask8 KAIXO_VECTORCALL operator<(__m128 a, __m128 b) noexcept { return _mm_cmp_ps_mask(a, b, _CMP_LT_OS); }
    __mmask8 KAIXO_VECTORCALL operator<(__m256 a, __m256 b) noexcept { return _mm256_cmp_ps_mask(a, b, _CMP_LT_OS); }
    __mmask16 KAIXO_VECTORCALL operator<(__m512 a, __m512 b) noexcept { return _mm512_cmp_ps_mask(a, b, _CMP_LT_OS); }
    __mmask8 KAIXO_VECTORCALL operator<(__m128d a, __m128d b) noexcept { return _mm_cmp_pd_mask(a, b, _CMP_LT_OS); }
    __mmask8 KAIXO_VECTORCALL operator<(__m256d a, __m256d b) noexcept { return _mm256_cmp_pd_mask(a, b, _CMP_LT_OS); }
    __mmask16 KAIXO_VECTORCALL operator<(__m512d a, __m512d b) noexcept { return _mm512_cmp_pd_mask(a, b, _CMP_LT_OS); }
    
    __mmask8 KAIXO_VECTORCALL operator>=(__m128i a, __m128i b) noexcept { return _mm_cmpge_epi32_mask(a, b); }
    __mmask8 KAIXO_VECTORCALL operator>=(__m256i a, __m256i b) noexcept { return _mm256_cmpge_epi32_mask(a, b); }
    __mmask16 KAIXO_VECTORCALL operator>=(__m512i a, __m512i b) noexcept { return _mm512_cmpge_epi32_mask(a, b); }
    __mmask8 KAIXO_VECTORCALL operator>=(__m128 a, __m128 b) noexcept { return _mm_cmp_ps_mask(a, b, _CMP_GE_OS); }
    __mmask8 KAIXO_VECTORCALL operator>=(__m256 a, __m256 b) noexcept { return _mm256_cmp_ps_mask(a, b, _CMP_GE_OS); }
    __mmask16 KAIXO_VECTORCALL operator>=(__m512 a, __m512 b) noexcept { return _mm512_cmp_ps_mask(a, b, _CMP_GE_OS); }
    __mmask8 KAIXO_VECTORCALL operator>=(__m128d a, __m128d b) noexcept { return _mm_cmp_pd_mask(a, b, _CMP_GE_OS); }
    __mmask8 KAIXO_VECTORCALL operator>=(__m256d a, __m256d b) noexcept { return _mm256_cmp_pd_mask(a, b, _CMP_GE_OS); }
    __mmask16 KAIXO_VECTORCALL operator>=(__m512d a, __m512d b) noexcept { return _mm512_cmp_pd_mask(a, b, _CMP_GE_OS); }
    
    __mmask8 KAIXO_VECTORCALL operator<=(__m128i a, __m128i b) noexcept { return _mm_cmple_epi32_mask(a, b); }
    __mmask8 KAIXO_VECTORCALL operator<=(__m256i a, __m256i b) noexcept { return _mm256_cmple_epi32_mask(a, b); }
    __mmask16 KAIXO_VECTORCALL operator<=(__m512i a, __m512i b) noexcept { return _mm512_cmple_epi32_mask(a, b); }
    __mmask8 KAIXO_VECTORCALL operator<=(__m128 a, __m128 b) noexcept { return _mm_cmp_ps_mask(a, b, _CMP_LE_OS); }
    __mmask8 KAIXO_VECTORCALL operator<=(__m256 a, __m256 b) noexcept { return _mm256_cmp_ps_mask(a, b, _CMP_LE_OS); }
    __mmask16 KAIXO_VECTORCALL operator<=(__m512 a, __m512 b) noexcept { return _mm512_cmp_ps_mask(a, b, _CMP_LE_OS); }
    __mmask8 KAIXO_VECTORCALL operator<=(__m128d a, __m128d b) noexcept { return _mm_cmp_pd_mask(a, b, _CMP_LE_OS); }
    __mmask8 KAIXO_VECTORCALL operator<=(__m256d a, __m256d b) noexcept { return _mm256_cmp_pd_mask(a, b, _CMP_LE_OS); }
    __mmask16 KAIXO_VECTORCALL operator<=(__m512d a, __m512d b) noexcept { return _mm512_cmp_pd_mask(a, b, _CMP_LE_OS); }


#define _mm_setzero_epi32() _mm_setzero_si128()
#define _mm256_setzero_epi32() _mm256_setzero_si256()

#define KAIXO_BASIC_SUM(v)                                     \
    [this](auto& val) -> auto {                  \
        base vals[elements];                                   \
        get(vals);                                             \
        return std::accumulate(vals, vals + elements, base{}); \
    }(v)

#define _mm_sum_epi32(v) KAIXO_BASIC_SUM(v)
#define _mm256_sum_epi32(v) KAIXO_BASIC_SUM(v)
#define _mm512_sum_epi32(v) KAIXO_BASIC_SUM(v)

#define _mm_sum_pd(v) _mm_cvtsd_f64(v)
    
#define _mm_sum_ps(v)                         \
    [this](auto& val) -> auto { \
        auto shuf = _mm_movehdup_ps(val);     \
        auto sums = _mm_add_ps(val, shuf);    \
        shuf = _mm_movehl_ps(shuf, sums);     \
        sums = _mm_add_ss(sums, shuf);        \
        return _mm_cvtss_f32(sums);           \
    }(v)
    
#define _mm256_sum_pd(v) KAIXO_BASIC_SUM(v)

#define _mm256_sum_ps(v)                                  \
    [this](auto& val) -> auto {             \
        auto hiQuad = _mm256_extractf128_ps(val, 1);      \
        auto loQuad = _mm256_castps256_ps128(val);        \
        auto sumQuad = _mm_add_ps(loQuad, hiQuad);        \
        auto loDual = sumQuad;                            \
        auto hiDual = _mm_movehl_ps(sumQuad, sumQuad);    \
        auto sumDual = _mm_add_ps(loDual, hiDual);        \
        auto lo = sumDual;                                \
        auto hi = _mm_shuffle_ps(sumDual, sumDual, 0x1);  \
        auto sum = _mm_add_ss(lo, hi);                    \
        return _mm_cvtss_f32(sum);                        \
    }(v)
    
#define _mm512_sum_pd(v) KAIXO_BASIC_SUM(v)
    
#define _mm512_sum_ps(v)                                \
    [this](auto& val) -> auto {           \
        const auto v0 = _mm512_castps512_ps256(val);    \
        const auto v1 = _mm512_extractf32x8_ps(val, 1); \
        const auto x0 = _mm256_add_ps(v0, v1);          \
        return simd<float, 256>(x0).sum();              \
    }(v)


#define SIMD_INSTANCE_FLOATING_PART(BASE, TYPE, BITS, PRE, POST)\
        friend simd KAIXO_VECTORCALL floor(const simd& v) noexcept { return PRE##_floor_##POST(v.value); }                                                                   \
        friend simd KAIXO_VECTORCALL ceil(const simd& v) noexcept { return PRE##_ceil_##POST(v.value); }                                                                     \
        friend simd KAIXO_VECTORCALL round(const simd& v) noexcept { return PRE##_roundscale_##POST(v.value, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC); }                                                                   \
        friend simd KAIXO_VECTORCALL log(const simd& v) noexcept { return PRE##_log_##POST(v.value); }                                                                       \
        friend simd KAIXO_VECTORCALL log2(const simd& v) noexcept { return PRE##_log2_##POST(v.value); }                                                                     \
        friend simd KAIXO_VECTORCALL log10(const simd& v) noexcept { return PRE##_log10_##POST(v.value); }                                                                   \
        friend simd KAIXO_VECTORCALL sqrt(const simd& v) noexcept { return PRE##_sqrt_##POST(v.value); }                                                                     \
        friend simd KAIXO_VECTORCALL cbrt(const simd& v) noexcept { return PRE##_cbrt_##POST(v.value); }                                                                     \
        friend simd KAIXO_VECTORCALL exp(const simd& v) noexcept { return PRE##_exp_##POST(v.value); }                                                                       \
        friend simd KAIXO_VECTORCALL exp2(const simd& v) noexcept { return PRE##_exp2_##POST(v.value); }                                                                     \
        friend simd KAIXO_VECTORCALL exp10(const simd& v) noexcept { return PRE##_exp10_##POST(v.value); }                                                                   \
        friend simd KAIXO_VECTORCALL tanh(const simd& v) noexcept { return PRE##_tanh_##POST(v.value); }                                                                     \
        friend simd KAIXO_VECTORCALL abs(const simd& v) noexcept { return PRE##_andnot_##POST(PRE##_set1_##POST(-0.0), v.value); }                                           \
        friend simd KAIXO_VECTORCALL cos(const simd& v) noexcept { return PRE##_cos_##POST(v.value); }                                                                       \
        friend simd KAIXO_VECTORCALL sin(const simd& v) noexcept { return PRE##_sin_##POST(v.value); }                                                                       \
        friend std::pair<simd, simd> KAIXO_VECTORCALL sincos(const simd& v) noexcept { simd cos; simd sin = PRE##_sincos_##POST(&cos.value, v.value); return { sin, cos }; } \
        friend simd KAIXO_VECTORCALL copysign(const simd& a, const simd& b) noexcept { return (-0.f & a) | (~simd(-0.f) & b); }                 \
        friend simd KAIXO_VECTORCALL pow(const simd& a, const simd& b) noexcept { return PRE##_pow_##POST(a.value, b.value); }                        \

#define SIMD_INSTANCE_FLOAT_PART(BASE, TYPE, BITS, PRE, POST)\
        simd<int, bits> to_int() const noexcept { return PRE##_cvt##POST##_epi32(value); }                                                        \
        template<std::size_t N = 1> simd<float,  sizeof(float)  * elements> lookup(float*  data) const noexcept { return PRE##_i32gather_ps(   data, to_int().value, N * sizeof(float)); }  \
        template<std::size_t N = 1> simd<double, sizeof(double) * elements> lookup(double* data) const noexcept { return PRE##_i32gather_pd(   data, to_int().value, N * sizeof(double)); } \
        template<std::size_t N = 1> simd<int,    sizeof(int)    * elements> lookup(int*    data) const noexcept { return PRE##_i32gather_epi32(data, to_int().value, N * sizeof(int)); }    \
        SIMD_INSTANCE_FLOATING_PART(BASE, TYPE, BITS, PRE, POST)                                                                                  \

#define SIMD_INSTANCE_DOUBLE_PART(BASE, TYPE, BITS, PRE, POST)\
        simd abs() const noexcept { return PRE##_andnot_##POST(PRE##_set1_##POST(-0.0), value); }                                               \
        SIMD_INSTANCE_FLOATING_PART(BASE, TYPE, BITS, PRE, POST)                                                                                \

#define SIMD_INSTANCE_INT_PART(BASE, TYPE, BITS, PRE, POST)\
        friend simd KAIXO_VECTORCALL operator<<(const simd& a, const simd& b) noexcept { return PRE##_sllv_##POST(a.value, b.value); }          \
        friend simd KAIXO_VECTORCALL operator>>(const simd& a, const simd& b) noexcept { return PRE##_srlv_##POST(a.value, b.value); }          \
        friend simd KAIXO_VECTORCALL abs(const simd& v) noexcept { return PRE##_abs_##POST(v.value); }                                          \
        template<std::size_t N = 1> simd<float,  sizeof(float)  * elements> lookup(float*  data) const noexcept { return PRE##_i32gather_ps(   data, value, N * sizeof(float)); }  \
        template<std::size_t N = 1> simd<double, sizeof(double) * elements> lookup(double* data) const noexcept { return PRE##_i32gather_pd(   data, value, N * sizeof(double)); } \
        template<std::size_t N = 1> simd<int,    sizeof(int)    * elements> lookup(int*    data) const noexcept { return PRE##_i32gather_epi32(data, value, N * sizeof(int)); }    \

#define SIMD_INSTANCE(BASE, TYPE, BITS, PRE, POST, PART)\
    template<>                                                                                                                                  \
    struct simd<BASE, BITS> {                                                                                                                   \
        constexpr static std::size_t bits = BITS;                                                                                               \
        constexpr static size_t elements = BITS / sizeof(BASE);                                                                                 \
        using base = BASE;                                                                                                                      \
        using type = TYPE;                                                                                                                      \
                                                                                                                                                \
        static simd zero() noexcept { return PRE##_setzero_##POST(); }                                                                          \
                                                                                                                                                \
        type value;                                                                                                                             \
                                                                                                                                                \
        explicit simd(bool v) : value(v ? PRE##_set1_##POST(UINT_MAX) : PRE##_set1_##POST(0)) {}                                                \
        simd() : value{} {}                                                                                                                     \
        simd(base* v) : value(PRE##_loadu_##POST(v)) {}                                                                                         \
        simd(base v) : value(PRE##_set1_##POST(v)) {}                                                                                           \
        simd(const type& v) : value(v) {}                                                                                                       \
                                                                                                                                                \
        template<std::same_as<base> ...Args>                                                                                                    \
        simd(Args...args) : value(PRE##_set_##POST(args...)) {}                                                                                 \
                                                                                                                                                \
        void get(base* data) const noexcept { return PRE##_storeu_##POST(data, value); }                                                        \
                                                                                                                                                \
        base sum() const noexcept { return PRE##_sum_##POST(value); }                                                                           \
                                                                                                                                                \
        simd operator~() const noexcept { return PRE##_xor_##POST(value, PRE##_set1_##POST(UINT_MAX)); }                                        \
                                                                                                                                                \
        friend simd KAIXO_VECTORCALL operator +(const simd& a, const simd& b) noexcept { return a.value  + b.value; } \
        friend simd KAIXO_VECTORCALL operator -(const simd& a, const simd& b) noexcept { return a.value  - b.value; } \
        friend simd KAIXO_VECTORCALL operator *(const simd& a, const simd& b) noexcept { return a.value  * b.value; } \
        friend simd KAIXO_VECTORCALL operator /(const simd& a, const simd& b) noexcept { return a.value  / b.value; } \
        friend simd KAIXO_VECTORCALL operator &(const simd& a, const simd& b) noexcept { return a.value  & b.value; } \
        friend simd KAIXO_VECTORCALL operator |(const simd& a, const simd& b) noexcept { return a.value  | b.value; } \
        friend simd KAIXO_VECTORCALL operator <(const simd& a, const simd& b) noexcept { return a.value  < b.value; } \
        friend simd KAIXO_VECTORCALL operator >(const simd& a, const simd& b) noexcept { return a.value  > b.value; } \
        friend simd KAIXO_VECTORCALL operator<=(const simd& a, const simd& b) noexcept { return a.value <= b.value; } \
        friend simd KAIXO_VECTORCALL operator>=(const simd& a, const simd& b) noexcept { return a.value >= b.value; } \
        friend simd KAIXO_VECTORCALL operator==(const simd& a, const simd& b) noexcept { return a.value == b.value; } \
        friend simd KAIXO_VECTORCALL operator!=(const simd& a, const simd& b) noexcept { return a.value != b.value; } \
                                                                                                                                                \
        friend simd KAIXO_VECTORCALL min(const simd& a, const simd& b) noexcept { return PRE##_min_##POST(a.value, b.value); }                  \
        friend simd KAIXO_VECTORCALL max(const simd& a, const simd& b) noexcept { return PRE##_max_##POST(a.value, b.value); }                  \
        friend simd KAIXO_VECTORCALL clamp(const simd& a, const simd& b, const simd& c) noexcept { return min(max(a, b), c); }                  \
                                                                                                                                                \
        PART(BASE, TYPE, BITS, PRE, POST)                                                                                                       \
    };                                                                                                                                          \

    SIMD_INSTANCE(    int, __m128i, 128, _mm   , epi32, SIMD_INSTANCE_INT_PART)
    SIMD_INSTANCE(    int, __m256i, 256, _mm256, epi32, SIMD_INSTANCE_INT_PART)
    SIMD_INSTANCE(    int, __m512i, 512, _mm512, epi32, SIMD_INSTANCE_INT_PART)
    SIMD_INSTANCE(  float, __m128 , 128, _mm   , ps   , SIMD_INSTANCE_FLOAT_PART)
    SIMD_INSTANCE(  float, __m256 , 256, _mm256, ps   , SIMD_INSTANCE_FLOAT_PART)
    SIMD_INSTANCE(  float, __m512 , 512, _mm512, ps   , SIMD_INSTANCE_FLOAT_PART)
    SIMD_INSTANCE( double, __m128d, 128, _mm   , pd   , SIMD_INSTANCE_DOUBLE_PART)
    SIMD_INSTANCE( double, __m256d, 256, _mm256, pd   , SIMD_INSTANCE_DOUBLE_PART)
    SIMD_INSTANCE( double, __m512d, 512, _mm512, pd   , SIMD_INSTANCE_DOUBLE_PART)


        /*
                std::pair<simd<float, 256>, simd<float, 256>> split() const noexcept {
            const auto v0 = _mm512_castps512_ps256(value);
            const auto v1 = _mm512_extractf32x8_ps(value, 1);
            return { v0, v1 };
        }*/
} 