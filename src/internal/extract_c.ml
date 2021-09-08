(* This file is part of ppx_cstubs (https://github.com/fdopen/ppx_cstubs)
 * Copyright (c) 2018-2019 fdopen
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Std
open Std.Result

let prologue =
  {|
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
|}

let cnt =
  let i = ref 0 in
  fun () ->
    let res = !i in
    incr i;
    res

let int_to_char_array i =
  let s = string_of_int i in
  let b = Buffer.create (String.length s * 4) in
  String.iter (fun c -> Printf.bprintf b "'%c'," c) s;
  Buffer.contents b

type id = int

type intern =
  | String
  | Integer of id
  | Integer_no_type of id
  | Unchecked_integer

type extract_info = {
  id : id;
  intern : intern;
}

type extract_int =
  [ `Unchecked_U8
  | `Unchecked_U32
  | `Any_int
  | `Int_type of string
  ]

let prepare_extract_int ~loc etyp expr =
  let com_loc = Std.Util.cloc_comment loc in
  let s_check =
    match etyp with
    | `Unchecked_U8 | `Unchecked_U32 -> ""
    | `Any_int | `Int_type _ ->
      let assign =
        match etyp with
        | `Any_int -> ""
        | `Int_type ctype ->
          let var = Std.Util.safe_cname ~prefix:"type_defined" in
          Printf.sprintf "\n%s %s = %s;" ctype var expr
        | `Unchecked_U8 | `Unchecked_U32 -> assert false
      in
      let var = Std.Util.safe_cname ~prefix:"extract_is_constexpr" in
      let var2 = Std.Util.safe_cname ~prefix:"extract_is_integer" in
      Printf.sprintf
        {|
%s%s
char %s[2] = { ((char)((%s) > 0)), '\0' }; /* %s not a constant expression? */
#if defined(PPXC_NO_PROPER_INTEGER_TEST) && !defined(__cplusplus)
int64_t %s = (int64_t)(~(%s)); /* enforce error for doubles */
#endif
|}
        com_loc assign var expr
        (Std.Util.no_c_comments expr)
        var2 expr
  in
  let expr, prepend =
    match etyp with
    | `Unchecked_U32 ->
      let var = Std.Util.safe_cname ~prefix:expr in
      let prepend =
        Printf.sprintf
          {|#if !defined(__cplusplus) || PPXC_MODERN_CPP || defined(__clang__) || !defined(__GNUC__)
DISABLE_LIMIT_WARNINGS_PUSH()
enum { %s = (%s) };
DISABLE_LIMIT_WARNINGS_POP()
#else
#define %s (%s)
#endif
|}
          var expr var expr
      in
      (var, prepend)
    | `Any_int | `Int_type _ | `Unchecked_U8 -> (expr, "")
  in
  let gen ~etyp prepend info =
    let stringify =
      match etyp with
      | `Unchecked_U8 -> "PPXC__SUD00"
      | `Unchecked_U32 -> "PPXC__SUD03"
      | `Any_int | `Int_type _ -> "PPXC__NSTR"
    in
    let id = cnt () in
    let ar = int_to_char_array id in
    let s =
      Printf.sprintf
        {|%s
%s
DISABLE_LIMIT_WARNINGS_PUSH()
unsigned char ppx_c_extract_char_array_%d[] = {
'P','P','X','C','_','C','O','N','S','T','_','N','R','_', %s '|',
%s(%s),
'|', %s '_','R','N','_','T','S','N','O','C','_','C','X','P','P', '\0' };
DISABLE_LIMIT_WARNINGS_POP()
|}
        prepend com_loc id ar stringify info ar
    in
    (id, s)
  in
  let id, res_str1 = gen ~etyp prepend expr in
  let res, src2 =
    match etyp with
    | `Unchecked_U8 | `Unchecked_U32 ->
      ({ id; intern = Unchecked_integer }, res_str1)
    | `Any_int | `Int_type _ -> (
      let s_int = Printf.sprintf "( PPXC_IS_INTEGER_EXPR_DEF_TRUE(%s) )" expr in
      let s_min =
        Printf.sprintf "( (%s) >= 0 || (%s) >= INT64_MIN )" expr expr
      in
      let s_max =
        Printf.sprintf "( (%s) <= 0 || (%s) <= UINT64_MAX )" expr expr
      in
      let cstr =
        Printf.sprintf
          "(((unsigned)(%s)) | (((unsigned)(%s)) << 1u) | (((unsigned)(%s)) << 2u))"
          s_int s_min s_max
      in
      match etyp with
      | `Unchecked_U8 | `Unchecked_U32 -> assert false
      | `Any_int ->
        let id_x, str = gen ~etyp:`Unchecked_U8 res_str1 cstr in
        ({ id; intern = Integer_no_type id_x }, str)
      | `Int_type ctype ->
        let s_user_min =
          Printf.sprintf "( (%s) >= 0 || (%s) >= (PPXC_MIN_TYPE(%s)) )" expr
            expr ctype
        in
        let s_user_max =
          Printf.sprintf "( (%s) <= 0 || (%s) <= (PPXC_MAX_TYPE(%s)) )" expr
            expr ctype
        in
        let id_x, str =
          gen ~etyp:`Unchecked_U8 res_str1
          @@ Printf.sprintf
               "( %s | (((unsigned)(%s)) << 3u) | (((unsigned)(%s)) << 4u) )"
               cstr s_user_min s_user_max
        in
        ({ id; intern = Integer id_x }, str))
  in
  (res, s_check, src2)

let prepare_extract_string ~loc expr =
  let cnt = cnt () in
  let com_loc = Std.Util.cloc_comment loc in
  let var = Std.Util.safe_cname ~prefix:"extract_is_string" in
  (* src1 is redudant, but it makes the algorithm to locate errors easier *)
  let src1 =
    Printf.sprintf {|
    %s
char * %s = (char*) "test" %s;
|} com_loc var expr
  in
  let src2 =
    Printf.sprintf
      {|
char *ppx_c_extract_char_string%d = (char*)"PPXC_CONST_NR_%d|" %s "|%d_RN_TSNOC_CXPP";
     |}
      cnt cnt expr cnt
  in
  ({ id = cnt; intern = String }, src1, src2)

type obj = (int, string) Hashtbl.t

let compile ~ebuf c_prog =
  C_compile.compile ~stderr:(`Buffer ebuf) c_prog (fun ec obj ->
      if ec <> 0 then
        Error (Printf.sprintf "`ocamlfind ocamlc -c` failed with %d" ec)
      else
        CCIO.with_in ?mode:None ~flags:[ Open_binary ] obj @@ fun ch ->
        let s = CCIO.read_all ch in
        if s = "" then Error "`ocamlfind ocamlc -c` created an empty obj file"
        else Ok s)

let rex =
  Re.Perl.re ~opts:[ `Ungreedy; `Dotall ]
    "PPXC_CONST_NR_([0-9]+)\\|(.*)\\|([0-9]+)_RN_TSNOC_CXPP\000"
  |> Re.compile

let compile ~ebuf c_prog =
  match compile ~ebuf c_prog with
  | Error _ as x -> x
  | Ok s ->
    let rec iter i s len htl =
      if i >= len then htl
      else
        match Re.exec_opt ~pos:i rex s with
        | None -> htl
        | Some g ->
          let end' =
            try
              let id1 = Re.Group.get g 1 in
              let id2 = Re.Group.get g 3 in
              if id1 = id2 then (
                let str = Re.Group.get g 2 in
                Hashtbl.add htl (int_of_string id1) str;
                Re.Group.stop g 0)
              else succ i
            with Not_found | Failure _ -> succ i
          in
          iter end' s len htl
    in
    let h = iter 0 s (String.length s) (Hashtbl.create 64) in
    Ok h

type extract_error =
  | Info_not_found
  | Overflow of string
  | Underflow of string
  | Not_an_integer

let normalise_int r str =
  match String.length str with
  | 1 -> Char.code str.[0] |> string_of_int
  | 4 ->
    let c i = Char.code str.[i] |> Unsigned.UInt32.of_int in
    let open Unsigned.UInt32.Infix in
    let e = (c 0 lsl 24) lor (c 1 lsl 16) lor (c 2 lsl 8) lor c 3 in
    Unsigned.UInt32.to_string e
  | 9 -> (
    let c i = Char.code str.[i] |> Unsigned.UInt64.of_int in
    let open Unsigned.UInt64.Infix in
    let e =
      (c 1 lsl 56)
      lor (c 2 lsl 48)
      lor (c 3 lsl 40)
      lor (c 4 lsl 32)
      lor (c 5 lsl 24)
      lor (c 6 lsl 16)
      lor (c 7 lsl 8)
      lor c 8
    in
    let e = Unsigned.UInt64.to_string e in
    match str.[0] with
    | '-' -> "-" ^ e
    | '0' -> e
    | _ -> r.return (Error Info_not_found))
  | _ -> r.return (Error Info_not_found)

let extract info htl =
  with_return @@ fun r ->
  let er er = r.return (Error er) in
  let extract_single id =
    match Hashtbl.find htl id with
    | exception Not_found -> er Info_not_found
    | s -> s
  in
  let res = extract_single info.id in
  (match info.intern with
  | String -> r.return (Ok res)
  | Unchecked_integer | Integer _ | Integer_no_type _ -> ());
  let res = normalise_int r res in
  let int' =
    match info.intern with
    | String -> assert false
    | Unchecked_integer -> r.return (Ok res)
    | Integer x | Integer_no_type x ->
      let s = extract_single x in
      if String.length s <> 1 then er Info_not_found;
      Char.code s.[0]
  in
  if int' land (1 lsl 0) = 0 then er Not_an_integer;
  if int' land (1 lsl 1) = 0 then er (Underflow res);
  if int' land (1 lsl 2) = 0 then er (Overflow res);
  (match info.intern with
  | Integer _ ->
    if int' land (1 lsl 3) = 0 then er (Underflow res);
    if int' land (1 lsl 4) = 0 then er (Overflow res)
  | Integer_no_type _ -> ()
  | String | Unchecked_integer -> assert false);
  Ok res
