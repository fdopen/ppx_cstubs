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

#define PPXC__UD00(x) ('0' + ((char)(( (x) / UINT64_C(1)                    ) % UINT64_C(10))))
#define PPXC__UD01(x) ('0' + ((char)(( (x) / UINT64_C(10)                   ) % UINT64_C(10)))), PPXC__UD00(x)
#define PPXC__UD02(x) ('0' + ((char)(( (x) / UINT64_C(100)                  ) % UINT64_C(10)))), PPXC__UD01(x)
#define PPXC__UD03(x) ('0' + ((char)(( (x) / UINT64_C(1000)                 ) % UINT64_C(10)))), PPXC__UD02(x)
#define PPXC__UD04(x) ('0' + ((char)(( (x) / UINT64_C(10000)                ) % UINT64_C(10)))), PPXC__UD03(x)
#define PPXC__UD05(x) ('0' + ((char)(( (x) / UINT64_C(100000)               ) % UINT64_C(10)))), PPXC__UD04(x)
#define PPXC__UD06(x) ('0' + ((char)(( (x) / UINT64_C(1000000)              ) % UINT64_C(10)))), PPXC__UD05(x)
#define PPXC__UD07(x) ('0' + ((char)(( (x) / UINT64_C(10000000)             ) % UINT64_C(10)))), PPXC__UD06(x)
#define PPXC__UD08(x) ('0' + ((char)(( (x) / UINT64_C(100000000)            ) % UINT64_C(10)))), PPXC__UD07(x)
#define PPXC__UD09(x) ('0' + ((char)(( (x) / UINT64_C(1000000000)           ) % UINT64_C(10)))), PPXC__UD08(x)
#define PPXC__UD10(x) ('0' + ((char)(( (x) / UINT64_C(10000000000)          ) % UINT64_C(10)))), PPXC__UD09(x)
#define PPXC__UD11(x) ('0' + ((char)(( (x) / UINT64_C(100000000000)         ) % UINT64_C(10)))), PPXC__UD10(x)
#define PPXC__UD12(x) ('0' + ((char)(( (x) / UINT64_C(1000000000000)        ) % UINT64_C(10)))), PPXC__UD11(x)
#define PPXC__UD13(x) ('0' + ((char)(( (x) / UINT64_C(10000000000000)       ) % UINT64_C(10)))), PPXC__UD12(x)
#define PPXC__UD14(x) ('0' + ((char)(( (x) / UINT64_C(100000000000000)      ) % UINT64_C(10)))), PPXC__UD13(x)
#define PPXC__UD15(x) ('0' + ((char)(( (x) / UINT64_C(1000000000000000)     ) % UINT64_C(10)))), PPXC__UD14(x)
#define PPXC__UD16(x) ('0' + ((char)(( (x) / UINT64_C(10000000000000000)    ) % UINT64_C(10)))), PPXC__UD15(x)
#define PPXC__UD17(x) ('0' + ((char)(( (x) / UINT64_C(100000000000000000)   ) % UINT64_C(10)))), PPXC__UD16(x)
#define PPXC__UD18(x) ('0' + ((char)(( (x) / UINT64_C(1000000000000000000)  ) % UINT64_C(10)))), PPXC__UD17(x)
#define PPXC__UD19(x) ('0' + ((char)(( (x) / UINT64_C(10000000000000000000) ) % UINT64_C(10)))), PPXC__UD18(x)
#define PPXC__NSTR(x) ((x) >= 0 ? '0' : '-'), PPXC__UD19(((x) >= 0 ? ((uint64_t)(x)) : \
 ( (x) == INT64_MIN ? UINT64_C(9223372036854775808) : ((uint64_t)(-((int64_t)(x)))))))

#define PPXC__SUD00(x) ('0' + ((char)(( (x) / UINT32_C(1)                    ) % UINT32_C(10))))
#define PPXC__SUD01(x) ('0' + ((char)(( (x) / UINT32_C(10)                   ) % UINT32_C(10)))), PPXC__SUD00(x)
#define PPXC__SUD02(x) ('0' + ((char)(( (x) / UINT32_C(100)                  ) % UINT32_C(10)))), PPXC__SUD01(x)
#define PPXC__SUD03(x) ('0' + ((char)(( (x) / UINT32_C(1000)                 ) % UINT32_C(10)))), PPXC__SUD02(x)
#define PPXC__SUD04(x) ('0' + ((char)(( (x) / UINT32_C(10000)                ) % UINT32_C(10)))), PPXC__SUD03(x)
#define PPXC__SUD05(x) ('0' + ((char)(( (x) / UINT32_C(100000)               ) % UINT32_C(10)))), PPXC__SUD04(x)
#define PPXC__SUD06(x) ('0' + ((char)(( (x) / UINT32_C(1000000)              ) % UINT32_C(10)))), PPXC__SUD05(x)
#define PPXC__SUD07(x) ('0' + ((char)(( (x) / UINT32_C(10000000)             ) % UINT32_C(10)))), PPXC__SUD06(x)
#define PPXC__SUD08(x) ('0' + ((char)(( (x) / UINT32_C(100000000)            ) % UINT32_C(10)))), PPXC__SUD07(x)
#define PPXC__SUD09(x) ('0' + ((char)(( (x) / UINT32_C(1000000000)           ) % UINT32_C(10)))), PPXC__SUD08(x)
#define PPXC__SNSTR(x) ((x) >= 0 ? '0' : '-'), PPXC__SUD09(((x) >= 0 ? ((uint32_t)(x)) : \
               ( (x) == INT32_MIN ? UINT32_C(2147483648) : ((uint32_t)(-((int64_t)(x)))))))

#ifdef PPXC_HAS_BUILTIN_CLASSIFY_TYPE
#if defined(__clang__) && __clang_major__ < 4
/* workaround for https://reviews.llvm.org/D16846 */
#define PPXC_CLASSIFY_TYPE_P(x)                                         \
  (__builtin_classify_type(x) == 15 && sizeof(x) == sizeof(char) &&  __alignof__(x) ==  __alignof__(char) ? 1 : \
   (__builtin_classify_type(x) == 3 ? 1 :                               \
    (__builtin_classify_type(x) == 10 ? __builtin_classify_type((void*)0) : \
     (__builtin_classify_type(x) == 14 ? __builtin_classify_type((void*)0) : \
      __builtin_classify_type(x)))))
#else
#define PPXC_CLASSIFY_TYPE_P(x) __builtin_classify_type(x)
#endif

#if defined(__cplusplus)
/* c mode for integers ... */
#define PPXC_CLASSIFY_TYPE(x)                   \
  (PPXC_CLASSIFY_TYPE_P(x) == 2 ? 1 :           \
   (PPXC_CLASSIFY_TYPE_P(x) == 3 ? 1 :          \
    (PPXC_CLASSIFY_TYPE_P(x) == 4 ? 1 :         \
     PPXC_CLASSIFY_TYPE_P(x))))
#else
#define PPXC_CLASSIFY_TYPE(x)                   \
  PPXC_CLASSIFY_TYPE_P(x)
#endif
#endif /* ifdef PPXC_HAS_BUILTIN_CLASSIFY_TYPE */

#ifdef PPXC_HAS_BUILTIN_CLASSIFY_TYPE
#define PPXC_IS_INTEGER(var)                                       \
  ((unsigned)(PPXC_CLASSIFY_TYPE(var) == PPXC_CLASSIFY_TYPE(0)))
#define PPXC_IS_INTEGER_DEF_TRUE(x) PPXC_IS_INTEGER(x)
#elif !defined(__cplusplus) && (defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L))
#define PPXC_IS_INTEGER_DEF_TRUE(var)             \
  (_Generic( (var),                               \
             ptrdiff_t: 1,                        \
             float: 0,                            \
             double: 0,                           \
             long double: 0,                      \
             float _Complex: 0,                   \
             double _Complex: 0,                  \
             long double _Complex: 0,             \
             default: (_Generic(((var) - (var)),  \
                                ptrdiff_t: 0,     \
                                default:1  ))))
#else
#define PPXC_IS_INTEGER_DEF_TRUE(x) 1
#endif

#if defined(_WIN32) && defined(_MSC_VER)

/* macro doesn't work for doubles .... */

#define PPXC_CTYPES_CHECK_UNSIGNED(TYPENAME)             \
  ((unsigned)(((TYPENAME) (-1)) > 0)         \
   << ((unsigned)CTYPES_UNSIGNED_FLAG_BIT))

#define PPXC_CTYPES_CLASSIFY(TYPENAME) (PPXC_CTYPES_CHECK_UNSIGNED(TYPENAME))

#define PPXC_CTYPES_ARITHMETIC_TYPEINFO(TYPENAME) (PPXC_CTYPES_CLASSIFY(TYPENAME)   \
                                       | sizeof(TYPENAME))

#else

#define PPXC_CTYPES_ARITHMETIC_TYPEINFO(x) CTYPES_ARITHMETIC_TYPEINFO(x)

#endif

#if !defined(__cplusplus) && defined(PPXC_HAS_TYPEOF) && defined(PPXC_HAS_BUILTIN_CLASSIFY_TYPE)

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)
#define PPXC_IS_UNSIGNED(x)                                             \
  (!(_Generic((1 ? (x*)0 : (void*)(!PPXC_IS_INTEGER(*((x*)0)))),        \
              x*: ((__typeof__(*(1 ? (x*)0 : (void*)(!PPXC_IS_INTEGER(*((x*)1)))))) -1) , \
              default: (int)(-1)) < 1))
#elif defined(PPXC_HAS_BUILTIN_CHOOSE_EXPR)
#define PPXC_IS_UNSIGNED(x)                                             \
  (!((__builtin_choose_expr((PPXC_IS_INTEGER(*((x*)0))),                \
                            ((__typeof__(*(1 ? (x*)0 : (void*)(!PPXC_IS_INTEGER(*((x*)1)))))) -1), \
                            ((int)(-1)))) < 1))
#endif
#endif
#ifdef PPXC_IS_UNSIGNED
#define PPXC_IS_UNSIGNED_VAR(x) PPXC_IS_UNSIGNED(__typeof__(x))
#else
#define PPXC_IS_UNSIGNED(x) 0
#define PPXC_IS_UNSIGNED_VAR(x) 0
#endif

#ifdef PPXC_HAS_BUILTIN_CLASSIFY_TYPE
#define PPXC_TYPES_COMPATIBLE_CT(a,b)  \
  (PPXC_CLASSIFY_TYPE(a) == PPXC_CLASSIFY_TYPE(b))
#else
#define PPXC_TYPES_COMPATIBLE_CT(a,b) 1
#endif

#define PPXC_TYPES_COMPATIBLE(a,b)                      \
  ((sizeof(a) == sizeof(b)) &&                          \
   PPXC_TYPES_COMPATIBLE_CT(a,b) &&                     \
   PPXC_IS_UNSIGNED_VAR(a) == PPXC_IS_UNSIGNED_VAR(b))

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

#define PPXC_TYPE_HELPER_H_1(i,typ,stru,t1)                             \
  (((uint64_t)(((sizeof (typ)) == (sizeof (t1))) &&                     \
               ((offsetof(struct ppx_cstubs_alignof_ ## t1 ,x)) == (offsetof(struct stru,x))))) << UINT64_C(i))

#define PPXC_INT_ALIGN_SIZE(typ,stru,example)   \
  (PPXC_TYPE_HELPER_H_1(0,typ,stru,int) |       \
   PPXC_TYPE_HELPER_H_1(1,typ,stru,int8_t) |    \
   PPXC_TYPE_HELPER_H_1(2,typ,stru,int16_t) |   \
   PPXC_TYPE_HELPER_H_1(3,typ,stru,int32_t) |   \
   PPXC_TYPE_HELPER_H_1(4,typ,stru,int64_t))

#if !defined(__cplusplus) && ((defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)) || defined(PPXC_HAS_BUILTIN_TYPES_COMPATIBLE_P))

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)
#define PPXC_TYPE_HELPER_H1(i,ex,typ)                             \
  (((uint64_t)(_Generic((ex),typ:1,default: 0))) << UINT64_C(i))

#define PPXC_TYPE_HELPER_H2(i,ex,ct,ot)                             \
  (((uint64_t)((_Generic((ex),ct:1,default: 0))                     \
               && (sizeof(ct) == sizeof(ot))                        \
               && (_Alignof(ct) == _Alignof(ot)))) << UINT64_C(i))
#else
#define PPXC_TYPE_HELPER_H1(i,ex,typ)                                    \
  (((uint64_t)(__builtin_types_compatible_p(__typeof__(ex), typ))) << UINT64_C(i))

#define PPXC_TYPE_HELPER_H2(i,ex,ct,ot)                                 \
  (((uint64_t)((__builtin_types_compatible_p(__typeof__(ex), ct))       \
               && (sizeof(ct) == sizeof(ot))                            \
               && (__alignof__(ct) == __alignof__(ot)))) << UINT64_C(i))
#endif

#define PPXC_TYPE_HELPER_OPAQUE_TC(ex)              \
  (PPXC_TYPE_HELPER_H1(20,ex,signed char) |         \
   PPXC_TYPE_HELPER_H1(21,ex,unsigned char) |       \
   PPXC_TYPE_HELPER_H1(22,ex,short) |               \
   PPXC_TYPE_HELPER_H1(23,ex,int) |                 \
   PPXC_TYPE_HELPER_H1(24,ex,long) |                \
   PPXC_TYPE_HELPER_H1(25,ex,long long) |           \
   PPXC_TYPE_HELPER_H1(26,ex,unsigned short) |      \
   PPXC_TYPE_HELPER_H1(27,ex,unsigned int) |        \
   PPXC_TYPE_HELPER_H1(28,ex,unsigned long) |       \
   PPXC_TYPE_HELPER_H1(29,ex,unsigned long long) |  \
   PPXC_TYPE_HELPER_H1(30,ex,size_t) |              \
   PPXC_TYPE_HELPER_H1(31,ex,int8_t) |              \
   PPXC_TYPE_HELPER_H1(32,ex,int16_t) |             \
   PPXC_TYPE_HELPER_H1(33,ex,int32_t) |             \
   PPXC_TYPE_HELPER_H1(34,ex,int64_t) |             \
   PPXC_TYPE_HELPER_H1(35,ex,uint8_t) |             \
   PPXC_TYPE_HELPER_H1(36,ex,uint16_t) |            \
   PPXC_TYPE_HELPER_H1(37,ex,uint32_t) |            \
   PPXC_TYPE_HELPER_H1(38,ex,uint64_t) |            \
   PPXC_TYPE_HELPER_H1(39,ex,intnat) |              \
   PPXC_TYPE_HELPER_H1(40,ex,char) |                \
   PPXC_TYPE_HELPER_H1(41,ex,bool) |                \
   PPXC_TYPE_HELPER_H2(50,ex,long,intnat) |         \
   PPXC_TYPE_HELPER_H2(51,ex,long long,intnat) |    \
   PPXC_TYPE_HELPER_H2(52,ex,long,int32_t) |        \
   PPXC_TYPE_HELPER_H2(53,ex,long,int64_t) |        \
   PPXC_TYPE_HELPER_H2(54,ex,long long,int64_t))

#else
#define PPXC_TYPE_HELPER_OPAQUE_TC(ex) UINT64_C(0)
#endif /*  !defined(__cplusplus) && ((defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)) || defined(PPXC_HAS_BUILTIN_TYPES_COMPATIBLE_P)) */

#ifdef PPXC_HAS_BUILTIN_CLASSIFY_TYPE
#define PPXC_TYPE_HELPER_OPAQUE_CT(typ,stru,var)                \
  ( PPXC_CLASSIFY_TYPE(var) == PPXC_CLASSIFY_TYPE(0) ?          \
    (PPXC_INT_ALIGN_SIZE(typ,stru,var)) :                       \
    (PPXC_CLASSIFY_TYPE(var) == PPXC_CLASSIFY_TYPE((void*)0) ?  \
     (UINT64_C(1)<< UINT64_C(60)) : UINT64_C(0)) )
#else
#define PPXC_TYPE_HELPER_OPAQUE_CT(typ,stru,var) UINT64_C(0)
#endif

#define PPXC_OPAQUE(typ,stru,ex)                                  \
  (PPXC_TYPE_HELPER_OPAQUE_TC(ex) |                               \
   PPXC_TYPE_HELPER_OPAQUE_CT(typ,stru,ex) |                      \
   (((uint64_t)(PPXC_IS_UNSIGNED_VAR(ex))) << UINT64_C(5)))

#define PPXC_INTALIAS(typ,stru,ex)                        \
  (PPXC_INT_ALIGN_SIZE(typ,stru,ex)                     | \
   PPXC_TYPE_HELPER_OPAQUE_TC(ex)                       | \
   (((uint64_t)(PPXC_IS_INTEGER_DEF_TRUE(ex)) << UINT64_C(62))) | \
   (((uint64_t)(((typ) (-1)) > 0)) << UINT64_C(63)))

/* Neither `sizeof(mem) == sizeof(typ)` nor
   `__builtin_types_compatible_p (__typeof__ (mem), typ)`, nor
   `(((__typeof__ (mem))-1) < 1) == ((typ)-1 < 1)` hold. */

#define PPXC_ENUM_MEMBER_CHECK(mem,typ)               \
  (((unsigned)(sizeof(mem) <= 8 ))  |                 \
   (((unsigned)(PPXC_IS_INTEGER_DEF_TRUE(mem))) <<  1u) |     \
   (((unsigned)( sizeof(mem) <= sizeof(typ))) << 2u))

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
#elif (defined(__GNUC__) && __GNUC__ > 6) || (defined(__INTEL_COMPILER) && __INTEL_COMPILER >= 1300)
#define PPXC_STATIC_ASSERT(a,b,c)               \
  _Static_assert(a,b)
#endif /* !defined(__cplusplus) */
#else
#if defined( __clang__ )
#if __has_feature(cxx_static_assert)
#define PPXC_STATIC_ASSERT(a,b,c)               \
  static_assert(a,b)
#endif
#elif __cplusplus >= 201103L
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

#ifdef PPXC_HAS_BUILTIN_CLASSIFY_TYPE
struct ppxc_example_struct {
    int a;
    int b;
} ppxc_example_struct;

union ppxc_example_union {
    int a;
    float b;
} ppxc_example_union;
#define PPXC_ASSERT_STRUCT(x,msg1,msg2)                                 \
  PPXC_STATIC_ASSERT(PPXC_CLASSIFY_TYPE(x) == PPXC_CLASSIFY_TYPE(ppxc_example_struct),msg1,msg2)

#define PPXC_ASSERT_UNION(x,msg1,msg2)                                  \
  PPXC_STATIC_ASSERT(PPXC_CLASSIFY_TYPE(x) == PPXC_CLASSIFY_TYPE(ppxc_example_union),msg1,msg2)
#else
#define PPXC_ASSERT_STRUCT(x,msg1,msg2)
#define PPXC_ASSERT_UNION(x,msg1,msg2)
#endif

#define PPXC_HALF_MAX_SIGNED_TYPE(typ) ((typ)1 << (sizeof(typ)*8-2))
#define PPXC_MAX_SIGNED_TYPE(typ) ((PPXC_HALF_MAX_SIGNED_TYPE(typ) - ((typ)1)) + PPXC_HALF_MAX_SIGNED_TYPE(typ))
#define PPXC_MIN_SIGNED_TYPE(typ) ( ((typ)-1) - PPXC_MAX_SIGNED_TYPE(typ))

#define PPXC_MIN_TYPE(typ) ( ((typ)-1) < 1 ? PPXC_MIN_SIGNED_TYPE(typ) : (typ)0)
#define PPXC_MAX_TYPE(typ) ( (typ) (~PPXC_MIN_TYPE(typ)))
|}

module List = CCListLabels

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
  | Unchecked_integer

type extract_info = {
  id : id;
  intern : intern;
}

let remove_file f = try Sys.remove f with Sys_error _ -> ()

let prepare_extract_int ?(bit32 = false) ?(disable_checks = false) ~ctype ~expr
    ~loc () =
  let com_loc = Std.Util.cloc_comment loc in
  let s_check =
    if disable_checks then ""
    else
      let var1 = Std.Util.safe_cname ~prefix:"type_defined" in
      let var2 = Std.Util.safe_cname ~prefix:"extract_is_constexpr" in
      Printf.sprintf
        {|
%s
%s %s = %s;
char %s[2] = { ((char)((%s) > 0)), '\0' }; /* %s not a constant expression? */
|}
        com_loc ctype var1 expr var2 expr
        (Std.Util.no_c_comments expr)
  in
  let stringify = if bit32 then "PPXC__SNSTR" else "PPXC__NSTR" in
  let gen prepend info =
    let id = cnt () in
    let ar = int_to_char_array id in
    let s =
      Printf.sprintf
        {|%s
%s
DISABLE_LIMIT_WARNINGS_PUSH()
char ppx_c_extract_char_array_%d[] = {
'P','P','X','C','_','C','O','N','S','T','_','N','R','_', %s '|',
%s(%s),
'|', %s '_','R','N','_','T','S','N','O','C','_','C','X','P','P', '\0' };
DISABLE_LIMIT_WARNINGS_POP()
|}
        prepend com_loc id ar stringify info ar
    in
    (id, s)
  in
  let id, res_str1 = gen "" expr in
  let res, src2 =
    if disable_checks = true then ({ id; intern = Unchecked_integer }, res_str1)
    else
      let s_int = Printf.sprintf "( PPXC_IS_INTEGER_DEF_TRUE(%s) )" expr in
      let int_size = if bit32 then "32" else "64" in
      let s_min =
        Printf.sprintf "( (%s) >= 0 || (%s) >= INT%s_MIN )" expr expr int_size
      in
      let s_max =
        Printf.sprintf "( (%s) <= 0 || (%s) <= UINT%s_MAX )" expr expr int_size
      in
      let s_user_min =
        Printf.sprintf "( (%s) >= 0 || (%s) >= (PPXC_MIN_TYPE(%s)) )" expr expr
          ctype
      in
      let s_user_max =
        Printf.sprintf "( (%s) <= 0 || (%s) <= (PPXC_MAX_TYPE(%s)) )" expr expr
          ctype
      in
      let id_x, str =
        gen res_str1
        @@ Printf.sprintf
             "( (((unsigned)(%s)) << 0u) | (((unsigned)(%s)) << 1u) | (((unsigned)(%s)) << 2u) | (((unsigned)(%s)) << 3u) | (((unsigned)(%s)) << 4u) )"
             s_int s_min s_max s_user_min s_user_max
      in
      ({ id; intern = Integer id_x }, str)
  in
  (res, s_check, src2)

let prepare_extract_string ~expr ~loc () =
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
  let ocaml_flags = !Options.ocaml_flags in
  let pre_suf =
    match Std.Util.unsuffixed_file_name () with "" -> "" | x -> x ^ "_"
  in
  let pre = "ppxc_extract_" ^ pre_suf in
  let cfln = Filename.temp_file pre ".c" in
  finally ~h:(fun () -> if not !Options.keep_tmp then remove_file cfln)
  @@ fun () ->
  CCIO.with_out ?mode:None ~flags:[ Open_creat; Open_trunc; Open_binary ] cfln
    (fun ch -> output_string ch c_prog);
  let obj = Filename.chop_suffix cfln ".c" ^ Ocaml_config.ext_obj () in
  let c_flags =
    (* that's a suboptimal solution. `ocamlc -c foo.c -o foo.o` doesn't work:
       "Options -c and -o are incompatible when compiling C files" But I might
       have no write access in the current directory and I'm unsure how '-I'
       flags and similar options are affected, if I change the current working
       directory ... *)
    match Ocaml_config.system () |> CCString.lowercase_ascii with
    | "win32" | "win64" -> [ "-Fo:" ^ obj ]
    | _ -> [ "-o"; obj ]
  in
  let dir =
    match !Options.ml_input_file with
    | None -> failwith "ml_input_file not set"
    | Some s -> Filename.dirname s
  in
  let c_flags = "-I" :: dir :: c_flags in
  let c_flags = !Options.c_flags @ c_flags in
  let args = List.map c_flags ~f:(fun c -> [ "-ccopt"; c ]) |> List.flatten in
  let args' =
    List.map !Options.findlib_pkgs ~f:(fun c -> [ "-package"; c ])
    |> List.flatten
  in
  let args = args' @ args in
  let args = if !Options.verbosity > 2 then args @ [ "-verbose" ] else args in
  let args = ocaml_flags @ args in
  let args =
    match Std.Various.use_threads () with
    | false -> args
    | true -> "-thread" :: args
    (* just to make ocamlfind silent. *)
  in
  let args =
    match !Options.cc with None -> args | Some s -> "-cc" :: s :: args
  in
  let args = "c" :: "-c" :: cfln :: args in
  let args =
    match !Options.toolchain with
    | None -> args
    | Some s -> "-toolchain" :: s :: args
  in
  let stdout = if !Options.verbosity > 0 then `Stdout else `Null in
  let prog = Options.ocamlfind in
  finally ~h:(fun () -> if not !Options.keep_tmp then remove_file obj)
  @@ fun () ->
  if !Options.verbosity > 1 then Run.cmd_to_string prog args |> prerr_endline;
  match Run.run prog args ~stdout ~stderr:(`Buffer ebuf) with
  | exception Unix.Unix_error (e, s, _) ->
    let cmd = Run.cmd_to_string prog args in
    Error
      (Printf.sprintf "Process creation \"%s\" failed with %s (%S)" cmd
         (Unix.error_message e) s)
  | 0 ->
    CCIO.with_in ?mode:None ~flags:[ Open_binary ] obj @@ fun ch ->
    let s = CCIO.read_all ch in
    if s = "" then Error "`ocamlfind ocamlc -c` created an empty obj file"
    else Ok s
  | ec -> Error (Printf.sprintf "`ocamlfind ocamlc -c` failed with %d" ec)

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
                Re.Group.stop g 0 )
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

let normalise_int str =
  let len = String.length str in
  if len < 1 then str
  else
    let b = Buffer.create len in
    let start =
      match str.[0] with
      | '-' as c ->
        Buffer.add_char b c;
        1
      | _ -> 0
    in
    let rec iter i =
      if i >= len then Buffer.add_char b '0'
      else
        match str.[i] with
        | '0' -> iter (succ i)
        | _ -> Buffer.add_substring b str i (len - i)
    in
    iter start;
    Buffer.contents b

let extract info htl =
  with_return @@ fun r ->
  let extract_single id =
    match Hashtbl.find htl id with
    | exception Not_found -> r.return (Error Info_not_found)
    | s -> s
  in
  let res = extract_single info.id in
  match info.intern with
  | String -> Ok res
  | Unchecked_integer -> Ok (normalise_int res)
  | Integer x ->
    let res = normalise_int res in
    let int' =
      match int_of_string @@ extract_single x with
      | exception Failure _ -> r.return (Error Info_not_found)
      | x -> x
    in
    let er er = r.return (Error er) in
    if int' land (1 lsl 0) = 0 then er Not_an_integer;
    if int' land (1 lsl 1) = 0 then er (Underflow res);
    if int' land (1 lsl 2) = 0 then er (Overflow res);
    if int' land (1 lsl 3) = 0 then er (Underflow res);
    if int' land (1 lsl 4) = 0 then er (Overflow res);
    Ok res
