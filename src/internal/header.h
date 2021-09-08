#ifdef __cplusplus
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif
#endif
#include <stddef.h>
#include <limits.h>
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include <string.h>
#include <assert.h>
#include <ctypes_cstubs_internals.h>

#if (defined(__GNUC__) && ( __GNUC__ >= 4 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 3))) || (defined(__clang_major__) && (__clang_major__ >= 3))
#define PPXC_HAS_TYPEOF 1
#endif
#if !defined(__cplusplus) && ((defined(__GNUC__) && ( __GNUC__ >= 4 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1))) || (defined(__clang_major__) && (__clang_major__ >= 3)))
#define PPXC_HAS_BUILTIN_TYPES_COMPATIBLE_P 1
#endif
#if (defined(__GNUC__) && ( __GNUC__ >= 4 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1))) || (defined(__clang_major__) && (__clang_major__ >= 3))
#define PPXC_HAS_BUILTIN_CLASSIFY_TYPE 1
#endif
#if !defined(__cplusplus) && ((defined(__GNUC__) && ( __GNUC__ >= 4 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 2))) || (defined(__clang_major__) && (__clang_major__ >= 3)))
#define PPXC_HAS_BUILTIN_CHOOSE_EXPR 1
#endif

#if defined(__cplusplus)
#if __cplusplus >= 201103L || (defined(_WIN32) && (_MSC_VER) && _MSC_VER >= 1900)
#define PPXC_DECLTYPE(X) decltype(X)
#define PPXC_MODERN_CPP 1
#include <type_traits>
#elif defined(PPXC_HAS_TYPEOF)
#define PPXC_DECLTYPE(X) __typeof__(X)
#elif defined(_WIN32) && (_MSC_VER) && _MSC_VER >= 1600
#define PPXC_DECLTYPE(X) decltype(X)
#endif
#endif

#if defined(__cplusplus) && defined(_WIN32) && defined(_MSC_VER)
#define PPXC_OFFSETOF(t,f)                                              \
  (reinterpret_cast<uintptr_t>(&reinterpret_cast<t*>(16)->f) - static_cast<uintptr_t>(16u))
#else
#define PPXC_OFFSETOF offsetof
#endif

/* subtle differences between the different alignof-macors don't matter.
   They are only used as a hint for type compatibilty */
#ifdef PPXC_MODERN_CPP
#define PPXC_ALIGNOF(x) alignof(x)
#elif !defined(__cplusplus) && defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)
#define PPXC_ALIGNOF(x) _Alignof(x)
#elif (defined(__GNUC__) && __GNUC__ >= 4) || (defined(__clang_major__) && __clang_major__ > 3)
#define PPXC_ALIGNOF(x) __alignof__(x)
#elif defined(_WIN32) && defined(_MSC_VER)
#define PPXC_ALIGNOF(x) __alignof(x)
#endif

#if defined(__cplusplus)
#define PPXC_SCAST(typ,val) static_cast<typ>(val)
#define PPXC_RCAST(typ,val) reinterpret_cast<typ>(val)
#else
#define PPXC_SCAST(typ,val) ((typ)(val))
#define PPXC_RCAST(typ,val) ((typ)(val))
#endif

#if (defined __GNUC__ || defined __clang__) && defined __SIZEOF_INT128__
#define PPXC_INT128 1
#endif

#ifdef PPXC_HAS_BUILTIN_CLASSIFY_TYPE
#if defined(__clang__) && __clang_major__ < 4
/* workaround for https://reviews.llvm.org/D16846 */
#define PPXC_CLASSIFY_TYPE_P(x)                                         \
  ((__builtin_classify_type(x) == 15 && sizeof(x) == sizeof(char) && PPXC_ALIGNOF(x) ==  PPXC_ALIGNOF(char)) || __builtin_classify_type(x) == 3 || __builtin_classify_type(x) == 4 ? 1 : \
   (__builtin_classify_type(x) == 10 || __builtin_classify_type(x) == 14) ? __builtin_classify_type(PPXC_RCAST(void*,0)) : __builtin_classify_type(x))
#elif defined(__clang__) && !defined(__cplusplus)
#define PPXC_CLASSIFY_TYPE_P(x)                                       \
  (__builtin_classify_type(x) == 4 ? 1 : __builtin_classify_type(x))
#else
#define PPXC_CLASSIFY_TYPE_P(x) __builtin_classify_type(x)
#endif

#if defined(__cplusplus)
/* c mode for integers ... */
#define PPXC_CLASSIFY_TYPE(x)                   \
  ( PPXC_CLASSIFY_TYPE_P(x) == 2 || PPXC_CLASSIFY_TYPE_P(x) == 3 || \
    PPXC_CLASSIFY_TYPE_P(x) == 4 ? 1 : PPXC_CLASSIFY_TYPE_P(x) )
#else
#define PPXC_CLASSIFY_TYPE(x)                   \
  PPXC_CLASSIFY_TYPE_P(x)
#endif
#endif /* ifdef PPXC_HAS_BUILTIN_CLASSIFY_TYPE */


#if defined(__cplusplus)
#if defined(__clang__)
#if defined(__has_feature) && __has_feature(is_enum)
#define PPXC_CPP_IS_ENUM(E) __is_enum(E)
#endif
#if defined(__has_feature) && __has_feature(is_union)
#define PPXC_CPP_IS_UNION(E) __is_union(E)
#endif
#elif (defined(__ghs__) && (__GHS_VERSION_NUMBER >= 600)) ||  defined(__CODEGEARC__) || (defined(_MSC_VER) && (_MSC_VER >= 1500)) || (defined(__SUNPRO_CC) && (__SUNPRO_CC >= 0x5130)) || (defined(__ghs__) && (__GHS_VERSION_NUMBER >= 600)) || (defined(__GNUC__) && ((__GNUC__ > 4) || ((__GNUC__ == 4) && (__GNUC_MINOR__ >= 3) && !defined(__GCCXML__))))
#define PPXC_CPP_IS_ENUM(E) __is_enum(E)
#define PPXC_CPP_IS_UNION(E) __is_union(E)
#endif
#endif



#if defined(_WIN32) && defined(_MSC_VER) && !defined(__cplusplus) && defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)

enum ppx_cstubs_dummy_enum {
  PPX_CSTUBS_DUMMY_ENUM_A,
  PPX_CSTUBS_DUMMY_ENUM_B = INT_MAX
};

#define PPXC_MSVC_IS_ENUM(x)                    \
  _Generic((x),                                 \
           enum ppx_cstubs_dummy_enum: 1,       \
           default: 0)

#endif


/* PPXC_IS_INTEGER_TYPE / PPXC_IS_INTEGER_EXPR */
#if defined(__cplusplus)
#ifdef PPXC_MODERN_CPP
#define PPXC_IS_INTEGER_TYPE(T)                             \
  ( std::is_integral<T>::value || std::is_enum<T>::value )
#define PPXC_IS_INTEGER_EXPR(T)                  \
  PPXC_IS_INTEGER_TYPE(decltype(T))

#elif defined(PPXC_HAS_BUILTIN_CLASSIFY_TYPE)

#define PPXC_IS_INTEGER_EXPR(x)                     \
  (PPXC_CLASSIFY_TYPE(x) == PPXC_CLASSIFY_TYPE(1))

#define PPXC_IS_INTEGER_TYPE(typ)                     \
  PPXC_IS_INTEGER_EXPR(*(reinterpret_cast<typ*>(0)))

#elif defined(PPXC_CPP_IS_ENUM)
namespace ppxc_extract {

  template <typename T>
  struct is_int
  { static const bool value = false; };

#define PPXC_IS_INTEGER_TYPE_H(typ)             \
  template <>                                   \
  struct is_int<typ>                            \
  { static const bool value = true; };

  template <typename T>
  struct is_int2
  { static const bool value = false; };

#define PPXC_IS_INTEGER_TYPE_H2(typ)            \
  template <>                                   \
  struct is_int2<typ>                           \
  { static const bool value = true; };

  PPXC_IS_INTEGER_TYPE_H(bool)
  PPXC_IS_INTEGER_TYPE_H(unsigned char)
  PPXC_IS_INTEGER_TYPE_H(unsigned short)
  PPXC_IS_INTEGER_TYPE_H(unsigned int)
  PPXC_IS_INTEGER_TYPE_H(unsigned long)
  PPXC_IS_INTEGER_TYPE_H(unsigned long long)

  PPXC_IS_INTEGER_TYPE_H(signed char)
  PPXC_IS_INTEGER_TYPE_H(short)
  PPXC_IS_INTEGER_TYPE_H(int)
  PPXC_IS_INTEGER_TYPE_H(long)
  PPXC_IS_INTEGER_TYPE_H(long long)

#ifdef PPXC_INT128
  PPXC_IS_INTEGER_TYPE_H(__int128_t)
  PPXC_IS_INTEGER_TYPE_H(__uint128_t)
#endif

  PPXC_IS_INTEGER_TYPE_H(char)
  PPXC_IS_INTEGER_TYPE_H2(wchar_t)

#undef PPXC_IS_INTEGER_TYPE_H

}

#define PPXC_IS_INTEGER_TYPE(typ)                                       \
  (PPXC_CPP_IS_ENUM(typ) ||                                             \
   ppxc_extract::is_int<typ>::value ||                                  \
   ppxc_extract::is_int2<typ>::value)
#ifdef PPXC_DECLTYPE
#define PPXC_IS_INTEGER_EXPR(T) PPXC_IS_INTEGER_TYPE(PPXC_DECLTYPE(T))
#endif

#endif /* PPXC_MODERN_CPP */

#elif defined(PPXC_HAS_BUILTIN_CLASSIFY_TYPE)

#define PPXC_IS_INTEGER_EXPR(x)                     \
  (PPXC_CLASSIFY_TYPE(x) == PPXC_CLASSIFY_TYPE(1))

#define PPXC_IS_INTEGER_TYPE(typ)               \
  PPXC_IS_INTEGER_EXPR(*((typ*)0))

#elif defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)

#ifdef PPXC_MSVC_IS_ENUM
#define PPXC_IS_INTEGER_H2(x) PPXC_MSVC_IS_ENUM(x)
#else
#define PPXC_IS_INTEGER_H2(x) 0
#endif

#ifdef PPXC_INT128
#define PPXC_IS_INTEGER_H1(x)                   \
  _Generic((x),                                 \
           char: 1,                             \
           __int128_t: 1,                       \
           __uint128_t: 1,                      \
           default: PPXC_IS_INTEGER_H2(x))
#else
#define PPXC_IS_INTEGER_H1(x)                   \
  _Generic((x),                                 \
           char: 1,                             \
           default: PPXC_IS_INTEGER_H2(x))
#endif

#define PPXC_IS_INTEGER_EXPR(x)                 \
  _Generic((x),                                 \
           _Bool: 1,                            \
           unsigned char: 1,                    \
           unsigned short: 1,                   \
           unsigned int: 1,                     \
           unsigned long: 1,                    \
           unsigned long long: 1,               \
           signed char: 1,                      \
           short: 1,                            \
           int: 1,                              \
           long: 1,                             \
           long long: 1,                        \
           default: PPXC_IS_INTEGER_H1(x))

#define PPXC_IS_INTEGER_TYPE(x)                 \
  PPXC_IS_INTEGER_EXPR(*((x*)NULL))


#endif /* PPXC_IS_INTEGER_TYPE / PPXC_IS_INTEGER_EXPR */


#ifdef PPXC_IS_INTEGER_EXPR
#define PPXC_IS_INTEGER_EXPR_DEF_FALSE PPXC_IS_INTEGER_EXPR
#define PPXC_IS_INTEGER_EXPR_DEF_TRUE PPXC_IS_INTEGER_EXPR
#else
#define PPXC_NO_PROPER_INTEGER_TEST 1
#define PPXC_IS_INTEGER_EXPR_DEF_FALSE(x) 0
#define PPXC_IS_INTEGER_EXPR_DEF_TRUE(x) 1
#endif

#ifdef PPXC_IS_INTEGER_TYPE
#define PPXC_IS_INTEGER_TYPE_DEF_FALSE PPXC_IS_INTEGER_TYPE
#define PPXC_IS_INTEGER_TYPE_DEF_TRUE PPXC_IS_INTEGER_TYPE
#else
#define PPXC_IS_INTEGER_TYPE_DEF_FALSE(x) 0
#define PPXC_IS_INTEGER_TYPE_DEF_TRUE(x) 1
#endif

/* PPXC_IS_UNSIGNED_TYPE / PPXC_IS_UNSIGNED_EXPR */
#if defined(__cplusplus)

#ifdef PPXC_MODERN_CPP
namespace pxxc_extract {
  template<typename T,bool = std::is_integral<T>::value || std::is_enum<T>::value>
  struct h_is_unsigned : std::integral_constant<bool, static_cast<T>(0) < static_cast<T>(-1)> {};

  template<typename T> struct h_is_unsigned<T,false> : std::false_type {};
  template<typename T> struct is_unsigned : h_is_unsigned<T>::type {};
}

#define PPXC_IS_UNSIGNED_TYPE(x)                \
  pxxc_extract::is_unsigned<x>::value

#define PPXC_IS_UNSIGNED_EXPR(x)                      \
  pxxc_extract::is_unsigned<decltype(x)>::value

#elif defined(PPXC_CPP_IS_ENUM)

namespace pxxc_extract {

  template <typename T>
  struct is_unsigned_int
  { static const bool value = false; };

#define PPXC_UNSIGNED_HELPER_B(typ,bval)        \
  template <>                                   \
  struct is_unsigned_int<typ>                   \
  { static const bool value = bval; };

#define PPXC_UNSIGNED_HELPER(typ)               \
  PPXC_UNSIGNED_HELPER_B(typ,true)

  template <typename T>
  struct is_unsigned_int2
  { static const bool value = false; };

#define PPXC_UNSIGNED_HELPER_B2(typ,bval)       \
  template <>                                   \
  struct is_unsigned_int2<typ>                  \
  { static const bool value = bval; };

  PPXC_UNSIGNED_HELPER(bool)
  PPXC_UNSIGNED_HELPER(unsigned char)
  PPXC_UNSIGNED_HELPER(unsigned short)
  PPXC_UNSIGNED_HELPER(unsigned int)
  PPXC_UNSIGNED_HELPER(unsigned long)
  PPXC_UNSIGNED_HELPER(unsigned long long)

#ifdef PPXC_INT128
  PPXC_UNSIGNED_HELPER(__uint128_t)
#endif

  PPXC_UNSIGNED_HELPER_B(char,static_cast<char>(-1) > 0)
  PPXC_UNSIGNED_HELPER_B2(wchar_t,static_cast<wchar_t>(-1) > 0)

#undef PPXC_UNSIGNED_HELPER
#undef PPXC_UNSIGNED_HELPER_B
#undef PPXC_UNSIGNED_HELPER_B2

  template <typename T,bool isEnum>
  struct is_unsigned_enum;

  template<typename T>
  struct is_unsigned_enum<T,true> {
      static const bool value = static_cast<T>(0) < static_cast<T>(-1);
  };

  template<typename T>
  struct is_unsigned_enum<T,false> {
      static const bool value = false;
  };
}

#define PPXC_IS_UNSIGNED_TYPE(typ)                                      \
  (pxxc_extract::is_unsigned_enum<typ,PPXC_CPP_IS_ENUM(typ)>::value ||  \
   pxxc_extract::is_unsigned_int<typ>::value ||                         \
   pxxc_extract::is_unsigned_int2<typ>::value )

#ifdef PPXC_DECLTYPE
#define PPXC_IS_UNSIGNED_EXPR(x) PPXC_IS_UNSIGNED_TYPE(PPXC_DECLTYPE(x))
#endif

#endif /* PPXC_MODERN_CPP */

#elif defined(PPXC_HAS_TYPEOF) && defined(PPXC_HAS_BUILTIN_CLASSIFY_TYPE) && ( (defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)) || defined(PPXC_HAS_BUILTIN_CHOOSE_EXPR) )

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)
#define PPXC_IS_UNSIGNED_TYPE_H(x)                                      \
  _Generic((1 ? (x*)0 : (void*)(!PPXC_IS_INTEGER_TYPE(x))),             \
           x*: ((__typeof__(*(1 ? (x*)0 : (void*)(!PPXC_IS_INTEGER_TYPE(x))))) -1) , \
           default: (int)(-1))
#else
#define PPXC_IS_UNSIGNED_TYPE_H(x)                                      \
  __builtin_choose_expr((PPXC_IS_INTEGER_TYPE(x)),                      \
                        ((__typeof__(*(1 ? (x*)0 : (void*)(!PPXC_IS_INTEGER_TYPE(x)))))-1), \
                        ((int)(-1)))
#endif
#define PPXC_IS_UNSIGNED_TYPE(x)                \
  (PPXC_IS_UNSIGNED_TYPE_H(x) > 0)


#define PPXC_IS_UNSIGNED_EXPR(x) PPXC_IS_UNSIGNED_TYPE(__typeof__(x))

#elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L

#ifdef PPXC_INT128
#define PPXC_IS_UNSIGNED_EXPR_H(x)              \
  _Generic((x),                                 \
           char: CHAR_MAX == UCHAR_MAX,         \
           __uint128_t: 1,                      \
           default: 0 )
#else
#define PPXC_IS_UNSIGNED_EXPR_H(x)              \
  _Generic((x),                                 \
           char: CHAR_MAX == UCHAR_MAX,         \
           default: 0 )
#endif

#define PPXC_IS_UNSIGNED_EXPR(x)                \
  _Generic((x),                                 \
           _Bool: 1,                            \
           unsigned char: 1,                    \
           unsigned short: 1,                   \
           unsigned int: 1,                     \
           unsigned long: 1,                    \
           unsigned long long: 1,               \
           default: PPXC_IS_UNSIGNED_EXPR_H(x))

#define PPXC_IS_UNSIGNED_TYPE(x)                \
  PPXC_IS_UNSIGNED_EXPR( *((x*)NULL) )


#endif /* PPXC_IS_UNSIGNED_TYPE / PPXC_IS_UNSIGNED_EXPR */


#ifdef  PPXC_IS_UNSIGNED_EXPR
#define PPXC_IS_UNSIGNED_EXPR_DEF_FALSE PPXC_IS_UNSIGNED_EXPR
#else
#define PPXC_IS_UNSIGNED_EXPR_DEF_FALSE(x) 0
#endif

#ifdef  PPXC_IS_UNSIGNED_TYPE
#define PPXC_IS_UNSIGNED_TYPE_DEF_FALSE PPXC_IS_UNSIGNED_TYPE
#else
#define PPXC_IS_UNSIGNED_TYPE_DEF_FALSE(x) 0
#endif

/* PPXC_IS_FLOAT_TYPE / PPXC_IS_FLOAT_EXPR */
#if defined(__cplusplus)

#ifdef PPXC_MODERN_CPP
#define PPXC_IS_FLOAT_TYPE(T)                   \
  std::is_floating_point<T>::value

#define PPXC_IS_FLOAT_EXPR(T)                   \
  PPXC_IS_FLOAT_TYPE(decltype(T))

#elif defined(PPXC_HAS_BUILTIN_CLASSIFY_TYPE)

#define PPXC_IS_FLOAT_EXPR(x)                           \
  (PPXC_CLASSIFY_TYPE(x) == PPXC_CLASSIFY_TYPE(0.815))

#define PPXC_IS_FLOAT_TYPE(typ)                     \
  PPXC_IS_FLOAT_EXPR(*(reinterpret_cast<typ*>(0)))

#else

namespace ppxc_extract {

  template <typename T>
  struct is_float
  { static const bool value = false; };

#define PPXC_FLOAT_HELPER(typ)                  \
  template <>                                   \
  struct is_float<typ>                          \
  { static const bool value = true; };

  PPXC_FLOAT_HELPER(float)
  PPXC_FLOAT_HELPER(double)
  PPXC_FLOAT_HELPER(long double)
#undef PPXC_FLOAT_HELPER
}

#define PPXC_IS_FLOAT_TYPE(T)                   \
  ppxc_extract::is_float<T>::value

#ifdef PPXC_DECLTYPE
#define PPXC_IS_FLOAT_EXPR(T)                   \
  PPXC_IS_FLOAT_TYPE(PPXC_DECLTYPE(T))
#endif

#endif /* PPXC_MODERN_CPP */

#elif defined(PPXC_HAS_BUILTIN_CLASSIFY_TYPE)

#define PPXC_IS_FLOAT_EXPR(x)                         \
  (PPXC_CLASSIFY_TYPE(x) == PPXC_CLASSIFY_TYPE(1.01))

#define PPXC_IS_FLOAT_TYPE(typ)                 \
  PPXC_IS_FLOAT_EXPR(*((typ*)NULL))

#elif defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)

#define PPXC_IS_FLOAT_EXPR(var)                 \
  _Generic( (var),                              \
            float: 1,                           \
            double: 1,                          \
            long double: 1,                     \
            default: 0)

#define PPXC_IS_FLOAT_TYPE(x)                   \
  PPXC_IS_FLOAT_EXPR( *((x*)NULL) )


#endif /* PPXC_IS_FLOAT_TYPE / PPXC_IS_FLOAT_EXPR */


#ifdef PPXC_IS_FLOAT_EXPR
#define PPXC_IS_FLOAT_EXPR_DEF_FALSE PPXC_IS_FLOAT_EXPR
#else
#define PPXC_IS_FLOAT_EXPR_DEF_FALSE(x) 0
#endif



/* PPXC_IS_COMPLEX_TYPE / PPXC_IS_COMPLEX_EXPR */
#if defined(PPXC_HAS_BUILTIN_CLASSIFY_TYPE) && !defined(__cplusplus)

#define PPXC_IS_COMPLEX_EXPR(x)                                 \
  (PPXC_CLASSIFY_TYPE(x) == PPXC_CLASSIFY_TYPE(*((double _Complex *)NULL)))

#define PPXC_IS_COMPLEX_TYPE(typ)               \
  PPXC_IS_COMPLEX_EXPR(*((typ*)NULL))

#elif defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)

#ifndef _MSC_VER
#define PPXC_IS_COMPLEX_EXPR(var)               \
  _Generic( (var),                              \
            float _Complex : 1,                 \
            double _Complex : 1,                \
            long double _Complex : 1,           \
            default: 0)
#else
#define PPXC_IS_COMPLEX_EXPR(var)               \
  _Generic( (var),                              \
            _Fcomplex: 1,                       \
            _Dcomplex: 1,                       \
            _Lcomplex: 1,                       \
            default: 0)
#endif

#define PPXC_IS_COMPLEX_TYPE(x)                   \
  PPXC_IS_COMPLEX_EXPR( *((x*)NULL) )


#endif /* PPXC_IS_COMPLEX_TYPE / PPXC_IS_COMPLEX_EXPR */

#ifdef PPXC_IS_COMPLEX_EXPR
#define PPXC_IS_COMPLEX_EXPR_DEF_FALSE PPXC_IS_COMPLEX_EXPR
#else
#define PPXC_IS_COMPLEX_EXPR_DEF_FALSE(x) 0
#endif


/* PPXC_IS_POINTER_TYPE / PPXC_IS_POINTER_EXPR */
#if defined(__cplusplus)

#ifdef PPXC_MODERN_CPP
#define PPXC_IS_POINTER_TYPE(T)                 \
  std::is_pointer<T>::value
#else
namespace ppxc_extract {
  template <typename T>
  struct is_pointer
  { static const bool value = false; };

  template <typename T>
  struct is_pointer <T*>
  { static const bool value = true; };

  template <typename T>
  struct is_pointer <T* const>
  { static const bool value = true; };

  template <typename T>
  struct is_pointer <T* volatile>
  { static const bool value = true; };

  template <typename T>
  struct is_pointer <T* const volatile>
  { static const bool value = true; };

}
#define PPXC_IS_POINTER_TYPE(T)                 \
  ppxc_extract::is_pointer<T>::value
#endif

#ifdef PPXC_DECLTYPE
#define PPXC_IS_POINTER_EXPR(T)                 \
  PPXC_IS_POINTER_TYPE(PPXC_DECLTYPE(T))
#endif

#elif defined(PPXC_HAS_BUILTIN_CLASSIFY_TYPE)

#define PPXC_IS_POINTER_EXPR(x)                               \
  (PPXC_CLASSIFY_TYPE(x) == PPXC_CLASSIFY_TYPE((void*)NULL))

#define PPXC_IS_POINTER_TYPE(typ)               \
  PPXC_IS_POINTER_EXPR(*((typ*)NULL))

#endif /* PPXC_IS_POINTER_TYPE / PPXC_IS_POINTER_EXPR */

#ifdef PPXC_IS_POINTER_EXPR
#define PPXC_IS_POINTER_EXPR_DEF_FALSE PPXC_IS_POINTER_EXPR
#else
#define PPXC_IS_POINTER_EXPR_DEF_FALSE(x) 0
#endif

#ifdef PPXC_IS_POINTER_TYPE
#define PPXC_IS_POINTER_TYPE_DEF_FALSE PPXC_IS_POINTER_TYPE
#else
#define PPXC_IS_POINTER_TYPE_DEF_FALSE(x) 0
#endif

/* PPXC_IS_UNION_TYPE / PPXC_IS_UNION_EXPR */
#if defined(__cplusplus) && (defined(PPXC_MODERN_CPP) || defined(PPXC_CPP_IS_UNION))

#ifdef PPXC_MODERN_CPP
#define PPXC_IS_UNION_TYPE(T)                   \
  std::is_union<T>::value
#elif defined(PPXC_CPP_IS_UNION)
#define PPXC_IS_UNION_TYPE(T)                   \
  PPXC_CPP_IS_UNION(T)
#endif

#if defined(PPXC_IS_UNION_TYPE) && defined(PPXC_DECLTYPE)
#define PPXC_IS_UNION_EXPR(T)                   \
  PPXC_IS_UNION_TYPE(PPXC_DECLTYPE(T))
#endif

#elif defined(PPXC_HAS_BUILTIN_CLASSIFY_TYPE)

#define PPXC_IS_UNION_TYPE_CLASSIFY_TYPE_USED 1

union ppxc_example_union {
    int a;
    float b;
} ppxc_example_union;

#define PPXC_IS_UNION_EXPR(x)                                       \
  (PPXC_CLASSIFY_TYPE(x) == PPXC_CLASSIFY_TYPE(ppxc_example_union))

#define PPXC_IS_UNION_TYPE(typ)                 \
  PPXC_IS_UNION_EXPR(*(PPXC_RCAST(typ*,NULL)))

#endif /* PPXC_IS_UNION_TYPE / PPXC_IS_UNION_EXPR */

#ifdef PPXC_IS_UNION_EXPR
#define PPXC_IS_UNION_EXPR_DEF_FALSE PPXC_IS_UNION_EXPR
#else
#define PPXC_IS_UNION_EXPR_DEF_FALSE(x) 0
#endif


/* PPXC_IS_STRUCT_TYPE / PPXC_IS_STRUCT_EXPR */
#if !defined(__cplusplus) && defined(PPXC_HAS_BUILTIN_CLASSIFY_TYPE)

struct ppxc_example_struct {
    int a;
    float b;
} ppxc_example_struct;

#define PPXC_IS_STRUCT_EXPR(x)                                          \
  (PPXC_CLASSIFY_TYPE(x) == PPXC_CLASSIFY_TYPE(ppxc_example_struct))

#define PPXC_IS_STRUCT_TYPE(typ)                \
  PPXC_IS_STRUCT_EXPR(*((typ*)NULL))

#endif /* PPXC_IS_STRUCT_TYPE / PPXC_IS_STRUCT_EXPR */

#ifdef PPXC_IS_STRUCT_EXPR
#define PPXC_IS_STRUCT_EXPR_DEF_FALSE PPXC_IS_STRUCT_EXPR
#else
#define PPXC_IS_STRUCT_EXPR_DEF_FALSE(x) 0
#endif


#define PPXC__UD00(x) PPXC_SCAST(unsigned char, (x) & UINT64_C(255) )
#define PPXC__UD01(x) PPXC_SCAST(unsigned char, ( (x) >>  8u ) & UINT64_C(255) ), PPXC__UD00(x)
#define PPXC__UD02(x) PPXC_SCAST(unsigned char, ( (x) >> 16u ) & UINT64_C(255) ), PPXC__UD01(x)
#define PPXC__UD03(x) PPXC_SCAST(unsigned char, ( (x) >> 24u ) & UINT64_C(255) ), PPXC__UD02(x)
#define PPXC__UD04(x) PPXC_SCAST(unsigned char, ( (x) >> 32u ) & UINT64_C(255) ), PPXC__UD03(x)
#define PPXC__UD05(x) PPXC_SCAST(unsigned char, ( (x) >> 40u ) & UINT64_C(255) ), PPXC__UD04(x)
#define PPXC__UD06(x) PPXC_SCAST(unsigned char, ( (x) >> 48u ) & UINT64_C(255) ), PPXC__UD05(x)
#define PPXC__UD07(x) PPXC_SCAST(unsigned char, (x) >> 56u ), PPXC__UD06(x)
#define PPXC__NSTR(x) ((x) >= 0 ? '0' : '-'), PPXC__UD07(((x) >= 0 ? (PPXC_SCAST(uint64_t,x)) : (-PPXC_SCAST(uint64_t,x))))

#define PPXC__SUD00(x) PPXC_SCAST(unsigned char, PPXC_SCAST(uint32_t,x) & UINT32_C(255) )
#define PPXC__SUD01(x) PPXC_SCAST(unsigned char, ( PPXC_SCAST(uint32_t,x) >>  8u ) & UINT32_C(255) ), PPXC__SUD00(x)
#define PPXC__SUD02(x) PPXC_SCAST(unsigned char, ( PPXC_SCAST(uint32_t,x) >> 16u ) & UINT32_C(255) ), PPXC__SUD01(x)
#define PPXC__SUD03(x) PPXC_SCAST(unsigned char, PPXC_SCAST(uint32_t,x) >> 24u ), PPXC__SUD02(x)

#define PPXC_CTYPES_CHECK_UNSIGNED(TYPENAME)                            \
  ( PPXC_SCAST(TYPENAME,-1) > 0 ? (1u << CTYPES_UNSIGNED_FLAG_BIT) : 0u )

#if defined(_WIN32) && defined(_MSC_VER)
/* macro doesn't work for doubles .... */
#ifdef PPXC_IS_FLOAT_TYPE
#define PPXC_CTYPES_CHECK_FLOATING(T)                                 \
  ( PPXC_IS_FLOAT_TYPE(T) ? (1u << CTYPES_FLOATING_FLAG_BIT) : 0u )
#else
#define PPXC_CTYPES_CHECK_FLOATING(T) 0u
#endif

#define PPXC_CTYPES_CLASSIFY(TYPENAME)                                  \
  (PPXC_CTYPES_CHECK_UNSIGNED(TYPENAME) | PPXC_CTYPES_CHECK_FLOATING(TYPENAME))

#define PPXC_CTYPES_ARITHMETIC_TYPEINFO(TYPENAME)     \
  (PPXC_CTYPES_CLASSIFY(TYPENAME) | sizeof(TYPENAME))

#elif defined(__cplusplus)

#define PPXC_CTYPES_CHECK_FLOATING(TYPENAME)                            \
  ( static_cast<TYPENAME>(0.5) != 0 ? (1u << CTYPES_FLOATING_FLAG_BIT) : 0u)

#define PPXC_CTYPES_CLASSIFY(TYPENAME)                                  \
  (PPXC_CTYPES_CHECK_FLOATING(TYPENAME)| PPXC_CTYPES_CHECK_UNSIGNED(TYPENAME))

#define PPXC_CTYPES_ARITHMETIC_TYPEINFO(TYPENAME)     \
  (PPXC_CTYPES_CLASSIFY(TYPENAME) | sizeof(TYPENAME))

#else

#define PPXC_CTYPES_ARITHMETIC_TYPEINFO(x) CTYPES_ARITHMETIC_TYPEINFO(x)

#endif /* defined(_WIN32) && defined(_MSC_VER) */


/* PPXC_TYPES_COMPATIBLE */
#if !defined(__cplusplus) && defined(PPXC_HAS_BUILTIN_CLASSIFY_TYPE)
#define PPXC_TYPES_COMPATIBLE_CT(a,b)               \
  (PPXC_CLASSIFY_TYPE(a) == PPXC_CLASSIFY_TYPE(b))
#else

#if !defined(__cplusplus)
#define PPXC_CLASSIFY_TYPE_EMULATE(a)             \
  ( PPXC_IS_INTEGER_EXPR_DEF_FALSE(a) ? 1 :       \
    PPXC_IS_FLOAT_EXPR_DEF_FALSE(a) ? 2 :         \
    PPXC_IS_COMPLEX_EXPR_DEF_FALSE(a) ? 4 :       \
    PPXC_IS_POINTER_EXPR_DEF_FALSE(a) ? 8 : 256 )
#else
/* less strict. "Dirty" conversations might be necessary
   in order to deal with c++ features. I'm not really
   sure yet. */
#define PPXC_CLASSIFY_TYPE_EMULATE(a)           \
  ( PPXC_IS_INTEGER_EXPR_DEF_FALSE(a) ? 1 :     \
    PPXC_IS_FLOAT_EXPR_DEF_FALSE(a) ? 2 :       \
    PPXC_IS_UNION_EXPR_DEF_FALSE(a) ? 16 :      \
    256 )
#endif

#define PPXC_TYPES_COMPATIBLE_CT(a,b)                               \
  (PPXC_CLASSIFY_TYPE_EMULATE(a) == PPXC_CLASSIFY_TYPE_EMULATE(b))

#endif

#define PPXC_TYPES_COMPATIBLE(a,b)              \
  (sizeof(a) == sizeof(b) &&                    \
   PPXC_TYPES_COMPATIBLE_CT(a,b) &&             \
   PPXC_IS_UNSIGNED_EXPR_DEF_FALSE(a) ==        \
   PPXC_IS_UNSIGNED_EXPR_DEF_FALSE(b))



/* PPXC_INT_ALIGN_SIZE */

/* while __alignof__ and anonymous structs are supported by nearly all current
   c compilers, there still exist a few that emit a warning or fail to compile,
   if strict flags are passed. (c++ compilers also dislike anonymous structs.)*/

#define AHELPER_1(t)                            \
  struct ppx_cstubs_alignof_ ## t {             \
    char c;                                     \
    t x;                                        \
  };

AHELPER_1(int)
AHELPER_1(int8_t)
AHELPER_1(int16_t)
AHELPER_1(int32_t)
AHELPER_1(int64_t)

#undef AHELPER_1

#define PPXC_TYPE_HELPER_H_1(i,typ,stru,t1)               \
  ( sizeof(typ) == sizeof(t1) &&                          \
    PPXC_OFFSETOF(struct ppx_cstubs_alignof_ ## t1 ,x) == \
    PPXC_OFFSETOF(struct stru,x) ? (1u << i) : 0u)

#define PPXC_INT_ALIGN_SIZE(typ,stru,example)    \
  (PPXC_TYPE_HELPER_H_1(0u,typ,stru,int) |       \
   PPXC_TYPE_HELPER_H_1(1u,typ,stru,int8_t) |    \
   PPXC_TYPE_HELPER_H_1(2u,typ,stru,int16_t) |   \
   PPXC_TYPE_HELPER_H_1(3u,typ,stru,int32_t) |   \
   PPXC_TYPE_HELPER_H_1(4u,typ,stru,int64_t))


/* OPAQUE */

#define PPXC_TYPE_HELPER_OPAQUE_CT(typ,stru,var)  \
  ( PPXC_IS_INTEGER_TYPE_DEF_FALSE(typ) ?         \
    PPXC_INT_ALIGN_SIZE(typ,stru,var) :           \
    PPXC_IS_POINTER_TYPE_DEF_FALSE(typ) ?         \
    (1u << 28) : 0u )


/* PPXC_TYPE_HELPER_H1 / PPXC_TYPE_HELPER_H2 */
#if defined(__cplusplus)


namespace ppxc_extract
{
  template<typename T, typename U> struct types_compatible {static const char ok = false;};
  template<typename T> struct types_compatible<T, T> {static const char ok = true;};
  template<typename T> struct types_compatible<T const, T> {static const char ok = true;};
  template<typename T> struct types_compatible<T const volatile, T> {static const char ok = true;};
  template<typename T> struct types_compatible<T volatile, T> {static const char ok = true;};
}

#define PPXC_CXX_TYPES_COMPATIBLE(t1,t2)        \
  ppxc_extract::types_compatible<t1,t2>::ok

#define PPXC_TYPE_HELPER_H1(i,ex,ex_typ,typ)                    \
  ( PPXC_CXX_TYPES_COMPATIBLE(ex_typ, typ) ? (1u  << i) : 0u )

#ifdef PPXC_ALIGNOF
#define PPXC_TYPE_HELPER_H2(i,ex,ex_typ,ct,ot)                \
  ( PPXC_CXX_TYPES_COMPATIBLE(ex_typ, ct)                     \
    && sizeof(ct) == sizeof(ot)                               \
    && PPXC_ALIGNOF(ct) == PPXC_ALIGNOF(ot) ? (1u << i) : 0u )
#else
#define PPXC_TYPE_HELPER_H2(i,ex,ex_typ,ct,ot) 0u
#endif

#elif (defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)) || defined(PPXC_HAS_BUILTIN_TYPES_COMPATIBLE_P)

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)
#define PPXC_TYPE_HELPER_H1(i,ex,ex_typ,typ)    \
  _Generic((ex),typ:(1u  << i), default: 0u)

#define PPXC_TYPE_HELPER_H2(i,ex,ex_typ,ct,ot)                \
  (_Generic((ex),ct:1,default: 0)                             \
   && sizeof(ct) == sizeof(ot)                                \
   && PPXC_ALIGNOF(ct) == PPXC_ALIGNOF(ot) ? (1u << i) : 0u )
#else
#define PPXC_TYPE_HELPER_H1(i,ex,ex_typ,typ)                    \
  (__builtin_types_compatible_p(ex_typ, typ) ? (1u << i) : 0u)

#define PPXC_TYPE_HELPER_H2(i,ex,ex_typ,ct,ot)                  \
  ( __builtin_types_compatible_p(ex_typ, ct)                    \
    && sizeof(ct) == sizeof(ot)                                 \
    && PPXC_ALIGNOF(ct) == PPXC_ALIGNOF(ot) ? (1u << i) : 0u )
#endif

#endif /* PPXC_TYPE_HELPER_H1 / PPXC_TYPE_HELPER_H2 */

#ifdef PPXC_TYPE_HELPER_H1

#define PPXC_TYPE_HELPER_OPAQUE_TC(ex,ex_typ)              \
  (PPXC_TYPE_HELPER_H1(5u,ex,ex_typ,signed char) |         \
   PPXC_TYPE_HELPER_H1(6u,ex,ex_typ,unsigned char) |       \
   PPXC_TYPE_HELPER_H1(7u,ex,ex_typ,short) |               \
   PPXC_TYPE_HELPER_H1(8u,ex,ex_typ,int) |                 \
   PPXC_TYPE_HELPER_H1(9u,ex,ex_typ,long) |                \
   PPXC_TYPE_HELPER_H1(10u,ex,ex_typ,long long) |          \
   PPXC_TYPE_HELPER_H1(11u,ex,ex_typ,unsigned short) |     \
   PPXC_TYPE_HELPER_H1(12u,ex,ex_typ,unsigned int) |       \
   PPXC_TYPE_HELPER_H1(13u,ex,ex_typ,unsigned long) |      \
   PPXC_TYPE_HELPER_H1(14u,ex,ex_typ,unsigned long long) | \
   PPXC_TYPE_HELPER_H1(15u,ex,ex_typ,int8_t) |             \
   PPXC_TYPE_HELPER_H1(16u,ex,ex_typ,int16_t) |            \
   PPXC_TYPE_HELPER_H1(17u,ex,ex_typ,int32_t) |            \
   PPXC_TYPE_HELPER_H1(18u,ex,ex_typ,int64_t) |            \
   PPXC_TYPE_HELPER_H1(19u,ex,ex_typ,uint8_t) |            \
   PPXC_TYPE_HELPER_H1(20u,ex,ex_typ,uint16_t) |           \
   PPXC_TYPE_HELPER_H1(21u,ex,ex_typ,uint32_t) |           \
   PPXC_TYPE_HELPER_H1(22u,ex,ex_typ,uint64_t) |           \
   PPXC_TYPE_HELPER_H1(23u,ex,ex_typ,intnat) |             \
   PPXC_TYPE_HELPER_H1(24u,ex,ex_typ,char) |               \
   PPXC_TYPE_HELPER_H1(25u,ex,ex_typ,bool) |               \
   PPXC_TYPE_HELPER_H2(26u,ex,ex_typ,long long,intnat) |   \
   PPXC_TYPE_HELPER_H2(27u,ex,ex_typ,long long,int64_t))

#else
#define PPXC_TYPE_HELPER_OPAQUE_TC(ex,ex_typ) 0u
#endif /* ifdef PPXC_TYPE_HELPER_H1 */

#if defined(PPXC_MSVC_IS_ENUM) && defined(PPXC_ALIGNOF)
#define PPXC_OPAQUE_MSVC_ENUM(typ,ex)             \
  (PPXC_MSVC_IS_ENUM(ex)                          \
   && sizeof(typ) == sizeof(int)                  \
   && sizeof(typ) == 4                            \
   && sizeof(int32_t) == sizeof(int)              \
   && PPXC_ALIGNOF(typ) == PPXC_ALIGNOF(int)      \
   && PPXC_ALIGNOF(int) == PPXC_ALIGNOF(int32_t)  \
   && (enum ppx_cstubs_dummy_enum)(-1) < 0  ?     \
   ((1u << 8u) | (1u << 17u)) : 0u )
#else
#define PPXC_OPAQUE_MSVC_ENUM(typ,ex) 0u
#endif

#if !defined(PPXC_IS_INTEGER_TYPE)
#define PPXC_OPAQUE_MANUAL_INT(typ,stru,ex)                             \
  (  sizeof(ex) == sizeof(int8_t) || sizeof(ex) == sizeof(int16_t) ||   \
     sizeof(ex) == sizeof(int32_t) || sizeof(ex) == sizeof(int64_t)  ?  \
     (PPXC_INT_ALIGN_SIZE(typ,stru,ex) | (1u << 30)) : 0u )
#else
#define PPXC_OPAQUE_MANUAL_INT(typ,stru,ex) 0u
#endif

#if !defined(PPXC_IS_POINTER_TYPE)
#define PPXC_OPAQUE_MANUAL_POINTER(ex)                          \
  (sizeof(ex) == sizeof(PPXC_RCAST(void*,0)) ? (1u << 30) : 0u)
#else
#define PPXC_OPAQUE_MANUAL_POINTER(ex) 0u
#endif

#define PPXC_OPAQUE_MANUAL(typ,stru,ex)                                 \
  (PPXC_OPAQUE_MANUAL_INT(typ,stru,ex) | PPXC_OPAQUE_MANUAL_POINTER(ex))

#define PPXC_OPAQUE(typ,stru,ex)                              \
  (PPXC_TYPE_HELPER_OPAQUE_TC(ex,typ) |                       \
   PPXC_TYPE_HELPER_OPAQUE_CT(typ,stru,ex) |                  \
   PPXC_OPAQUE_MANUAL(typ,stru,ex) |                          \
   PPXC_OPAQUE_MSVC_ENUM(typ,ex) |                            \
   (PPXC_IS_UNSIGNED_TYPE_DEF_FALSE(typ) ? (1u << 29) : 0u) )



#define PPXC_INTALIAS(typ,stru,ex)                              \
  (PPXC_INT_ALIGN_SIZE(typ,stru,ex)                     |       \
   PPXC_TYPE_HELPER_OPAQUE_TC(ex,typ)                   |       \
   (PPXC_IS_INTEGER_TYPE_DEF_TRUE(typ) ? (1u  << 28) : 0u)  |   \
   ((PPXC_SCAST(typ,-1) > 0) ? (1u << 29) : 0u) )


/* Neither `sizeof(mem) == sizeof(typ)` nor
   `__builtin_types_compatible_p (__typeof__ (mem), typ)`, nor
   `(((__typeof__ (mem))-1) < 1) == ((typ)-1 < 1)` hold. */

#define PPXC_ENUM_MEMBER_CHECK(mem,typ)                     \
  ((sizeof(mem) <= 8 ? 1u : 0u)  |                          \
   (PPXC_IS_INTEGER_EXPR_DEF_TRUE(mem) ? (1u << 1) : 0u)  | \
   (sizeof(mem) <= sizeof(typ) ? (1u << 2) : 0u) )


#if defined(__clang__) && (__clang_major__ > 3 || ((__clang_major__ == 3) && (__clang_minor__ >= 3)))
#define DISABLE_LIMIT_WARNINGS_PUSH()                             \
  _Pragma("clang diagnostic push")                                \
  _Pragma("clang diagnostic ignored \"-Wtype-limits\"")           \
  _Pragma("clang diagnostic ignored \"-Wtautological-compare\"")  \
  _Pragma("clang diagnostic ignored \"-Wsign-conversion\"")       \
  _Pragma("clang diagnostic ignored \"-Wsign-compare\"")
#define DISABLE_LIMIT_WARNINGS_POP()            \
  _Pragma("clang diagnostic pop")
#elif defined(__GNUC__) && ( __GNUC__ >= 5 )
#define DISABLE_LIMIT_WARNINGS_PUSH()                   \
  _Pragma("GCC diagnostic push")                        \
  _Pragma("GCC diagnostic ignored \"-Wtype-limits\"")   \
  _Pragma("GCC diagnostic ignored \"-Wsign-compare\"")  \
  _Pragma("GCC diagnostic ignored \"-Wbool-compare\"")
#define DISABLE_LIMIT_WARNINGS_POP()            \
  _Pragma("GCC diagnostic pop")
#elif defined(__GNUC__) && ((__GNUC__ == 4) && (__GNUC_MINOR__ >= 6))
#define DISABLE_LIMIT_WARNINGS_PUSH()                   \
  _Pragma("GCC diagnostic push")                        \
  _Pragma("GCC diagnostic ignored \"-Wtype-limits\"")   \
  _Pragma("GCC diagnostic ignored \"-Wsign-compare\"")
#define DISABLE_LIMIT_WARNINGS_POP()            \
  _Pragma("GCC diagnostic pop")
#else
#define DISABLE_LIMIT_WARNINGS_PUSH()
#define DISABLE_LIMIT_WARNINGS_POP()
#endif

#if defined(__clang__) && (__clang_major__ >= 3)
#define DISABLE_STRUCT_WARNINGS_PUSH()                                  \
  _Pragma("clang diagnostic push")                                      \
  _Pragma("clang diagnostic ignored \"-Wmissing-braces\"")              \
  _Pragma("clang diagnostic ignored \"-Wmissing-field-initializers\"")
#define DISABLE_STRUCT_WARNINGS_POP()           \
  _Pragma("clang diagnostic pop")
#elif defined(__GNUC__) && (( __GNUC__ > 4 ) || (( __GNUC__ == 4 ) && (__GNUC_MINOR__ >= 6)))
#define DISABLE_STRUCT_WARNINGS_PUSH()                                \
  _Pragma("GCC diagnostic push")                                      \
  _Pragma("GCC diagnostic ignored \"-Wmissing-braces\"")              \
  _Pragma("GCC diagnostic ignored \"-Wmissing-field-initializers\"")
#define DISABLE_STRUCT_WARNINGS_POP()           \
  _Pragma("GCC diagnostic pop")
#else
#define DISABLE_STRUCT_WARNINGS_PUSH()
#define DISABLE_STRUCT_WARNINGS_POP()
#endif

#if defined(_WIN32) && defined(_MSC_VER) && _MSC_VER >= 1600
#define PPXC_STATIC_ASSERT(a,b,c)               \
  static_assert(a,b)
#elif !defined(__cplusplus)
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
#define PPXC_STATIC_ASSERT(a,b,c)               \
  _Static_assert(a,b)
#elif defined(__clang__)
#if __has_feature(c_static_assert)
#define PPXC_STATIC_ASSERT(a,b,c)               \
  _Static_assert(a,b)
#endif
#elif defined(__GNUC__) && __GNUC__ > 6
#define PPXC_STATIC_ASSERT(a,b,c)               \
  __extension__ _Static_assert(a,b)
#elif defined(__INTEL_COMPILER) && __INTEL_COMPILER >= 1300
#define PPXC_STATIC_ASSERT(a,b,c)               \
  _Static_assert(a,b)
#endif /* !defined(__cplusplus) */
#else
#if defined( __clang__ )
#if __has_feature(cxx_static_assert)
#define PPXC_STATIC_ASSERT(a,b,c)               \
  static_assert(a,b)
#endif
#elif defined(PPXC_MODERN_CPP)
#define PPXC_STATIC_ASSERT(a,b,c)               \
  static_assert(a,b)
#endif
#endif

#ifndef PPXC_STATIC_ASSERT
#define PPXC_STATIC_ASSERT4(cond,msg) typedef char static_assert_##msg[(!!(cond))*2-1]
#define PPXC_STATIC_ASSERT3(cond,line,msg) PPXC_STATIC_ASSERT4(cond, msg ## _ ## line )
#define PPXC_STATIC_ASSERT2(cond,line,msg) PPXC_STATIC_ASSERT3(cond, line, msg)
#define PPXC_STATIC_ASSERT(cond,msg1,msg2) PPXC_STATIC_ASSERT2(cond,__LINE__,msg2)
#endif


#if defined(PPXC_IS_UNION_TYPE) && ( !defined(__cplusplus) || !defined(PPXC_IS_UNION_TYPE_CLASSIFY_TYPE_USED) )
#if defined(__cplusplus) || !defined(PPXC_IS_UNION_EXPR)
#define PPXC_ASSERT_UNION(t,e,msg1,msg2)                \
  PPXC_STATIC_ASSERT(PPXC_IS_UNION_TYPE(t),msg1,msg2);
#else
#define PPXC_ASSERT_UNION(t,x,msg1,msg2)                \
  PPXC_STATIC_ASSERT(PPXC_IS_UNION_EXPR(x),msg1,msg2);
#endif
#else
#define PPXC_ASSERT_UNION(t,x,msg1,msg2)
#endif

#if defined(PPXC_IS_STRUCT_EXPR)
#define PPXC_ASSERT_STRUCT(t,x,msg1,msg2)               \
  PPXC_STATIC_ASSERT(PPXC_IS_STRUCT_EXPR(x),msg1,msg2);
#elif defined(PPXC_IS_UNION_TYPE) && ( !defined(__cplusplus) || !defined(PPXC_IS_UNION_TYPE_CLASSIFY_TYPE_USED) )
#define PPXC_ASSERT_STRUCT(t,x,msg1,msg2)                 \
  PPXC_STATIC_ASSERT(!(PPXC_IS_UNION_TYPE(t)),msg1,msg2);
#else
#define PPXC_ASSERT_STRUCT(t,x,msg1,msg2)
#endif


#define PPXC_HALF_MAX_SIGNED_TYPE(typ) (PPXC_SCAST(typ,1) << (sizeof(typ)*8-2))
#define PPXC_MAX_SIGNED_TYPE(typ) ((PPXC_HALF_MAX_SIGNED_TYPE(typ) - PPXC_SCAST(typ,1)) + PPXC_HALF_MAX_SIGNED_TYPE(typ))
#define PPXC_MIN_SIGNED_TYPE(typ) ( PPXC_SCAST(typ,-1) - PPXC_MAX_SIGNED_TYPE(typ) )

#define PPXC_MIN_TYPE(typ) ( PPXC_SCAST(typ,-1) < 1 ? PPXC_MIN_SIGNED_TYPE(typ) : PPXC_SCAST(typ,0) )
#define PPXC_MAX_TYPE(typ) ( PPXC_SCAST(typ, ~PPXC_MIN_TYPE(typ)) )


#include <stdio.h>

struct foo {
    int a;
    char b;
};

union ufoo {
    int a;
    char b;
};


typedef enum { Clubs =-1, Diamonds, Spades, Hearts = INT_MAX} card;


#ifdef PPXC_MODERN_CPP
#define UENUM 1
typedef enum uenum : unsigned int { UA = 0, UB = 2 } uenum;
#elif !defined(_MSC_VER)
#define UENUM 1
typedef enum uenum { UA = 0, UB = UINT64_MAX } uenum;
#else
#define UENUM 0
#endif


#if defined(__GNUC__) && defined(__x86_64__) && ( !defined(__cplusplus) || (defined(__cplusplus) && __cplusplus >= 201103L) )
#define I128 1
#else
#define I128 0
#endif

#if defined(__cplusplus)
#define TCOMPLEX 0
#else
#define TCOMPLEX 1
#ifdef _MSC_VER
typedef _Dcomplex dcomplex;
#else
typedef _Complex double dcomplex;
#endif
#endif


#define AS(x)                                   \
  do {                                          \
    assert(x);                                  \
    fputs(".",stdout);                          \
  } while(0)

int main(void){
  struct foo myfoo;
  union ufoo myufoo;
  const char * p = "Hallo";
  bool b = 1;
  int i = 3;
  char c = 'a';
  unsigned int ui = 1;
  double d = 3.0;
  card mcard = Clubs;
#if UENUM
  uenum muenum = UB;
#endif
#if TCOMPLEX
  dcomplex dcplx;
#endif
#if I128
  __uint128_t u128;
  __int128_t i128;
#endif

#ifdef PPXC_IS_INTEGER_TYPE
  AS(!PPXC_IS_INTEGER_TYPE(struct foo));
  AS(!PPXC_IS_INTEGER_TYPE(union ufoo));
  AS(!PPXC_IS_INTEGER_TYPE(char *));
  AS(!PPXC_IS_INTEGER_TYPE(double));
  AS(PPXC_IS_INTEGER_TYPE(long));
  AS(PPXC_IS_INTEGER_TYPE(unsigned long));
  AS(PPXC_IS_INTEGER_TYPE(bool));
  AS(PPXC_IS_INTEGER_TYPE(wchar_t));
  AS(PPXC_IS_INTEGER_TYPE(char));
  AS(PPXC_IS_INTEGER_TYPE(card));
#if UENUM
  AS(PPXC_IS_INTEGER_TYPE(uenum));
#endif
#if I128
  AS(PPXC_IS_INTEGER_TYPE(__uint128_t));
  AS(PPXC_IS_INTEGER_TYPE(__int128_t));
#endif
#if TCOMPLEX
  AS(!PPXC_IS_INTEGER_TYPE(dcomplex));
#endif
#endif

#ifdef PPXC_IS_INTEGER_EXPR
  AS(!PPXC_IS_INTEGER_EXPR(myfoo));
  AS(!PPXC_IS_INTEGER_EXPR(myufoo));
  AS(!PPXC_IS_INTEGER_EXPR(p));
  AS(!PPXC_IS_INTEGER_EXPR(d));
  AS(PPXC_IS_INTEGER_EXPR(i));
  AS(PPXC_IS_INTEGER_EXPR(ui));
  AS(PPXC_IS_INTEGER_EXPR(b));
  AS(PPXC_IS_INTEGER_EXPR(c));
  AS(PPXC_IS_INTEGER_EXPR(mcard));
#if UENUM
  AS(PPXC_IS_INTEGER_EXPR(muenum));
#endif
#if I128
  AS(PPXC_IS_INTEGER_EXPR(u128));
  AS(PPXC_IS_INTEGER_EXPR(i128));
#endif
#if TCOMPLEX
  AS(!PPXC_IS_INTEGER_EXPR(dcplx));
#endif
#endif

#ifdef PPXC_IS_UNSIGNED_TYPE
  AS(!PPXC_IS_UNSIGNED_TYPE(struct foo));
  AS(!PPXC_IS_UNSIGNED_TYPE(union ufoo));
  AS(!PPXC_IS_UNSIGNED_TYPE(char *));
  AS(!PPXC_IS_UNSIGNED_TYPE(double));
  AS(!PPXC_IS_UNSIGNED_TYPE(long));
  AS(PPXC_IS_UNSIGNED_TYPE(unsigned long));
  AS(PPXC_IS_UNSIGNED_TYPE(bool));
  AS(!PPXC_IS_UNSIGNED_TYPE(card));
#if UENUM
  AS(PPXC_IS_UNSIGNED_TYPE(uenum));
#endif
#if I128
  AS(PPXC_IS_UNSIGNED_TYPE(__uint128_t));
  AS(!PPXC_IS_UNSIGNED_TYPE(__int128_t));
#endif
#if TCOMPLEX
  AS(!PPXC_IS_UNSIGNED_TYPE(dcomplex));
#endif
#endif

#ifdef PPXC_IS_UNSIGNED_EXPR
  AS(!PPXC_IS_UNSIGNED_EXPR(myfoo));
  AS(!PPXC_IS_UNSIGNED_EXPR(myufoo));
  AS(!PPXC_IS_UNSIGNED_EXPR(p));
  AS(!PPXC_IS_UNSIGNED_EXPR(d));
  AS(!PPXC_IS_UNSIGNED_EXPR(i));
  AS(PPXC_IS_UNSIGNED_EXPR(ui));
  AS(PPXC_IS_UNSIGNED_EXPR(b));
  AS(!PPXC_IS_UNSIGNED_EXPR(Clubs));
#if UENUM
  AS(PPXC_IS_UNSIGNED_EXPR(UB));
#endif
#if I128
  AS(PPXC_IS_UNSIGNED_EXPR(u128));
  AS(!PPXC_IS_UNSIGNED_EXPR(i128));
#endif
#if TCOMPLEX
  AS(!PPXC_IS_UNSIGNED_EXPR(dcplx));
#endif
#endif


#ifdef PPXC_IS_FLOAT_TYPE
  AS(!PPXC_IS_FLOAT_TYPE(struct foo));
  AS(!PPXC_IS_FLOAT_TYPE(union ufoo));
  AS(!PPXC_IS_FLOAT_TYPE(char *));
  AS(PPXC_IS_FLOAT_TYPE(double));
  AS(!PPXC_IS_FLOAT_TYPE(long));
  AS(!PPXC_IS_FLOAT_TYPE(unsigned long));
  AS(!PPXC_IS_FLOAT_TYPE(bool));
  AS(!PPXC_IS_FLOAT_TYPE(char));
  AS(!PPXC_IS_FLOAT_TYPE(card));
#if UENUM
  AS(!PPXC_IS_FLOAT_TYPE(uenum));
#endif
#if I128
  AS(!PPXC_IS_FLOAT_TYPE(__uint128_t));
  AS(!PPXC_IS_FLOAT_TYPE(__int128_t));
#endif
#if TCOMPLEX
  AS(!PPXC_IS_FLOAT_TYPE(dcomplex));
#endif
#endif

#ifdef PPXC_IS_FLOAT_EXPR
  AS(!PPXC_IS_FLOAT_EXPR(myfoo));
  AS(!PPXC_IS_FLOAT_EXPR(myufoo));
  AS(!PPXC_IS_FLOAT_EXPR(p));
  AS(PPXC_IS_FLOAT_EXPR(d));
  AS(!PPXC_IS_FLOAT_EXPR(i));
  AS(!PPXC_IS_FLOAT_EXPR(ui));
  AS(!PPXC_IS_FLOAT_EXPR(b));
  AS(!PPXC_IS_FLOAT_EXPR(c));
  AS(!PPXC_IS_FLOAT_EXPR(Clubs));
#if UENUM
  AS(!PPXC_IS_FLOAT_EXPR(UB));
#endif
#if I128
  AS(!PPXC_IS_FLOAT_EXPR(u128));
  AS(!PPXC_IS_FLOAT_EXPR(i128));
#endif
#if TCOMPLEX
  AS(!PPXC_IS_FLOAT_EXPR(dcplx));
#endif
#endif

#ifdef PPXC_IS_COMPLEX_TYPE
  AS(!PPXC_IS_COMPLEX_TYPE(struct foo));
  AS(!PPXC_IS_COMPLEX_TYPE(union ufoo));
  AS(!PPXC_IS_COMPLEX_TYPE(char *));
  AS(!PPXC_IS_COMPLEX_TYPE(double));
  AS(!PPXC_IS_COMPLEX_TYPE(long));
  AS(!PPXC_IS_COMPLEX_TYPE(unsigned long));
  AS(!PPXC_IS_COMPLEX_TYPE(bool));
  AS(!PPXC_IS_COMPLEX_TYPE(char));
  AS(!PPXC_IS_COMPLEX_TYPE(card));
#if UENUM
  AS(!PPXC_IS_COMPLEX_TYPE(uenum));
#endif
#if I128
  AS(!PPXC_IS_COMPLEX_TYPE(__uint128_t));
  AS(!PPXC_IS_COMPLEX_TYPE(__int128_t));
#endif
#if TCOMPLEX
  AS(PPXC_IS_COMPLEX_TYPE(dcomplex));
#endif
#endif

#ifdef PPXC_IS_COMPLEX_EXPR
  AS(!PPXC_IS_COMPLEX_EXPR(myfoo));
  AS(!PPXC_IS_COMPLEX_EXPR(myufoo));
  AS(!PPXC_IS_COMPLEX_EXPR(p));
  AS(!PPXC_IS_COMPLEX_EXPR(d));
  AS(!PPXC_IS_COMPLEX_EXPR(i));
  AS(!PPXC_IS_COMPLEX_EXPR(ui));
  AS(!PPXC_IS_COMPLEX_EXPR(b));
  AS(!PPXC_IS_COMPLEX_EXPR(c));
  AS(!PPXC_IS_COMPLEX_EXPR(Clubs));
#if UENUM
  AS(!PPXC_IS_COMPLEX_EXPR(UB));
#endif
#if I128
  AS(!PPXC_IS_COMPLEX_EXPR(u128));
  AS(!PPXC_IS_COMPLEX_EXPR(i128));
#endif
#if TCOMPLEX
  AS(PPXC_IS_COMPLEX_EXPR(dcplx));
#endif
#endif


#ifdef PPXC_IS_POINTER_TYPE
  AS(!PPXC_IS_POINTER_TYPE(struct foo));
  AS(!PPXC_IS_POINTER_TYPE(union ufoo));
  AS(PPXC_IS_POINTER_TYPE(char *));
  AS(!PPXC_IS_POINTER_TYPE(double));
  AS(!PPXC_IS_POINTER_TYPE(long));
  AS(!PPXC_IS_POINTER_TYPE(unsigned long));
  AS(!PPXC_IS_POINTER_TYPE(bool));
  AS(!PPXC_IS_POINTER_TYPE(char));
  AS(!PPXC_IS_POINTER_TYPE(card));
#if UENUM
  AS(!PPXC_IS_POINTER_TYPE(uenum));
#endif
#if I128
  AS(!PPXC_IS_POINTER_TYPE(__uint128_t));
  AS(!PPXC_IS_POINTER_TYPE(__int128_t));
#endif
#if TCOMPLEX
  AS(!PPXC_IS_POINTER_TYPE(dcomplex));
#endif
#endif

#ifdef PPXC_IS_POINTER_EXPR
  AS(!PPXC_IS_POINTER_EXPR(myfoo));
  AS(!PPXC_IS_POINTER_EXPR(myufoo));
  AS(PPXC_IS_POINTER_EXPR(p));
  AS(!PPXC_IS_POINTER_EXPR(d));
  AS(!PPXC_IS_POINTER_EXPR(i));
  AS(!PPXC_IS_POINTER_EXPR(ui));
  AS(!PPXC_IS_POINTER_EXPR(b));
  AS(!PPXC_IS_POINTER_EXPR(c));
  AS(!PPXC_IS_POINTER_EXPR(Clubs));
#if UENUM
  AS(!PPXC_IS_POINTER_EXPR(UB));
#endif
#if I128
  AS(!PPXC_IS_POINTER_EXPR(u128));
  AS(!PPXC_IS_POINTER_EXPR(i128));
#endif
#if TCOMPLEX
  AS(!PPXC_IS_POINTER_EXPR(dcplx));
#endif
#endif


#ifdef PPXC_IS_UNION_TYPE
  AS(!PPXC_IS_UNION_TYPE(struct foo));
  AS(PPXC_IS_UNION_TYPE(union ufoo));
  AS(!PPXC_IS_UNION_TYPE(char *));
  AS(!PPXC_IS_UNION_TYPE(double));
  AS(!PPXC_IS_UNION_TYPE(long));
  AS(!PPXC_IS_UNION_TYPE(unsigned long));
  AS(!PPXC_IS_UNION_TYPE(bool));
  AS(!PPXC_IS_UNION_TYPE(char));
  AS(!PPXC_IS_UNION_TYPE(card));
#if UENUM
  AS(!PPXC_IS_UNION_TYPE(uenum));
#endif
#if I128
  AS(!PPXC_IS_UNION_TYPE(__uint128_t));
  AS(!PPXC_IS_UNION_TYPE(__int128_t));
#endif
#if TCOMPLEX
  AS(!PPXC_IS_UNION_TYPE(dcomplex));
#endif
#endif

#ifdef PPXC_IS_UNION_EXPR
  AS(!PPXC_IS_UNION_EXPR(myfoo));
  AS(PPXC_IS_UNION_EXPR(myufoo));
  AS(!PPXC_IS_UNION_EXPR(p));
  AS(!PPXC_IS_UNION_EXPR(d));
  AS(!PPXC_IS_UNION_EXPR(i));
  AS(!PPXC_IS_UNION_EXPR(ui));
  AS(!PPXC_IS_UNION_EXPR(b));
  AS(!PPXC_IS_UNION_EXPR(c));
  AS(!PPXC_IS_UNION_EXPR(Clubs));
#if UENUM
  AS(!PPXC_IS_UNION_EXPR(UB));
#endif
#if I128
  AS(!PPXC_IS_UNION_EXPR(u128));
  AS(!PPXC_IS_UNION_EXPR(i128));
#endif
#if TCOMPLEX
  AS(!PPXC_IS_UNION_EXPR(dcplx));
#endif
#endif


#ifdef PPXC_IS_STRUCT_TYPE
  AS(!PPXC_IS_STRUCT_TYPE(struct foo));
  AS(PPXC_IS_STRUCT_TYPE(union ufoo));
  AS(!PPXC_IS_STRUCT_TYPE(char *));
  AS(!PPXC_IS_STRUCT_TYPE(double));
  AS(!PPXC_IS_STRUCT_TYPE(long));
  AS(!PPXC_IS_STRUCT_TYPE(unsigned long));
  AS(!PPXC_IS_STRUCT_TYPE(bool));
  AS(!PPXC_IS_STRUCT_TYPE(char));
  AS(!PPXC_IS_STRUCT_TYPE(card));
  AS(!PPXC_IS_STRUCT_EXPR(Clubs));
#if UENUM
  AS(!PPXC_IS_STRUCT_EXPR(UB));
#endif
#if UENUM
  AS(!PPXC_IS_STRUCT_TYPE(uenum));
#endif
#if I128
  AS(!PPXC_IS_STRUCT_TYPE(__uint128_t));
  AS(!PPXC_IS_STRUCT_TYPE(__int128_t));
#endif
#if TCOMPLEX
  AS(!PPXC_IS_STRUCT_TYPE(dcomplex));
#endif
#endif

#ifdef PPXC_IS_STRUCT_EXPR
  AS(!PPXC_IS_STRUCT_EXPR(myfoo));
  AS(PPXC_IS_STRUCT_EXPR(myufoo));
  AS(!PPXC_IS_STRUCT_EXPR(p));
  AS(!PPXC_IS_STRUCT_EXPR(d));
  AS(!PPXC_IS_STRUCT_EXPR(i));
  AS(!PPXC_IS_STRUCT_EXPR(ui));
  AS(!PPXC_IS_STRUCT_EXPR(b));
  AS(!PPXC_IS_STRUCT_EXPR(c));
  AS(!PPXC_IS_STRUCT_EXPR(Clubs));
#if UENUM
  AS(!PPXC_IS_STRUCT_EXPR(UB));
#endif
#if I128
  AS(!PPXC_IS_STRUCT_EXPR(u128));
  AS(!PPXC_IS_STRUCT_EXPR(i128));
#endif
#if TCOMPLEX
  AS(!PPXC_IS_STRUCT_EXPR(dcplx));
#endif
#endif

/*
  int a;
  int b;
  int c;

  a = PPXC_IS_INTEGER_TYPE(struct foo);
  b = PPXC_IS_INTEGER_TYPE(__uint128_t);
  c = PPXC_IS_INTEGER_TYPE(card);
  printf("unssigned type: %d %d %d\n", a, b, c);

  a = PPXC_IS_INTEGER_EXPR(myfoo);
  b = PPXC_IS_INTEGER_EXPR(128);
  c = PPXC_IS_INTEGER_EXPR(Hearts);
  printf("integer expr: %d %d %d\n", a, b, c);

  puts("");

  a = PPXC_IS_UNSIGNED_TYPE(struct foo);
  b = PPXC_IS_UNSIGNED_TYPE(__uint128_t);
  c = PPXC_IS_UNSIGNED_TYPE(card);
  printf("unssigned type: %d %d %d\n", a, b, c);

  a = PPXC_IS_UNSIGNED_EXPR(myfoo);
  b = PPXC_IS_UNSIGNED_EXPR(128);
  c = PPXC_IS_UNSIGNED_EXPR(Hearts);
  printf("unsigned expr: %d %d %d\n", a, b, c);
  puts("");


  a = PPXC_IS_FLOAT_TYPE(struct foo);
  b = PPXC_IS_FLOAT_TYPE(double);
  c = PPXC_IS_FLOAT_TYPE(card);
  printf("float type: %d %d %d\n", a, b, c);

  a = PPXC_IS_FLOAT_EXPR(myfoo);
  b = PPXC_IS_FLOAT_EXPR(d);
  c = PPXC_IS_FLOAT_EXPR(dcplx;
  printf("float expr: %d %d %d\n", a, b, c);
  puts("");

#ifdef PPXC_IS_COMPLEX_TYPE
  a = PPXC_IS_COMPLEX_TYPE(struct foo);
  b = PPXC_IS_COMPLEX_TYPE(double);
  c = PPXC_IS_COMPLEX_TYPE(_Complex float);
  printf("complex type: %d %d %d\n", a, b, c);

  a = PPXC_IS_COMPLEX_EXPR(myfoo);
  b = PPXC_IS_COMPLEX_EXPR(d);
  c = PPXC_IS_COMPLEX_EXPR(dcplx;
  printf("complex expr: %d %d %d\n", a, b, c);
  puts("");
#endif

  a = PPXC_IS_POINTER_TYPE(struct foo *);
  b = PPXC_IS_POINTER_TYPE(void *);
  c = PPXC_IS_POINTER_TYPE(_Complex float);
  printf("pointer type: %d %d %d\n", a, b, c);

  a = PPXC_IS_POINTER_EXPR(myfoo);
  b = PPXC_IS_POINTER_EXPR((int*)0);
  c = PPXC_IS_POINTER_EXPR(dcplx;
  printf("pointer expr: %d %d %d\n", a, b, c);
  puts("");

  */
    puts("");

  return 0;
}
