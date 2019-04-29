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

type extr_info =
  | Extr_char
  | Extr_schar
  | Extr_short
  | Extr_int
  | Extr_int8_t
  | Extr_int16_t
  | Extr_camlint
  | Extr_uchar
  | Extr_bool
  | Extr_long
  | Extr_llong
  | Extr_ushort
  | Extr_sint
  | Extr_uint
  | Extr_ulong
  | Extr_ullong
  | Extr_size_t
  | Extr_int32_t
  | Extr_int64_t
  | Extr_uint8_t
  | Extr_uint16_t
  | Extr_uint32_t
  | Extr_uint64_t
  | Extr_nativeint

type info =
  { signed : bool
  ; min : string option
  ; max : string option
  ; info : extr_info }

let prepare : type a. a Ctypes_static.typ -> info option =
  let open Ctypes_primitive_types in
  let module C = Ctypes_static in
  function
  | C.Void -> None
  | C.Struct _ -> None
  | C.Union _ -> None
  | C.Array _ -> None
  | C.Bigarray _ -> None
  | C.Abstract _ -> None
  | C.Pointer _ -> None
  | C.Funptr _ -> None
  | C.OCaml _ -> None
  | C.View _ -> None (* yes intentionally, views can break cross compiling *)
  | C.Primitive p -> (
    let f ?min ?max signed info = Some {signed; min; max; info} in
    match p with
    | Char -> f ~min:"CHAR_MIN" ~max:"CHAR_MAX" true Extr_char
    | Schar -> f true Extr_schar
    | Uchar -> f false Extr_uchar
    | Bool -> f true Extr_bool
    | Short -> f true ~min:"SHRT_MIN" ~max:"SHRT_MAX" Extr_short
    | Int -> f true ~min:"INT_MIN" ~max:"INT_MAX" Extr_int
    | Long -> f true ~min:"LONG_MIN" ~max:"LONG_MAX" Extr_long
    | Llong -> f true ~min:"LLONG_MIN" ~max:"LLONG_MAX" Extr_llong
    | Ushort -> f false ~max:"USHRT_MAX" Extr_ushort
    | Sint -> f true ~min:"INT_MIN" ~max:"INT_MAX" Extr_sint
    | Uint -> f false ~max:"UINT_MAX" Extr_uint
    | Ulong -> f false ~max:"ULONG_MAX" Extr_ulong
    | Ullong -> f false ~max:"ULLONG_MAX" Extr_ullong
    | Size_t -> f false ~max:"SIZE_MAX" Extr_size_t
    | Int8_t -> f true Extr_int8_t
    | Int16_t -> f true Extr_int16_t
    | Int32_t -> f true Extr_int32_t
    | Int64_t -> f true Extr_int64_t
    | Uint8_t -> f false Extr_uint8_t
    | Uint16_t -> f false Extr_uint16_t
    | Uint32_t -> f false Extr_uint32_t
    | Uint64_t -> f false Extr_uint64_t
    | Camlint -> f true Extr_camlint
    | Nativeint -> f true Extr_nativeint
    | Float -> None
    | Double -> None
    | LDouble -> None
    | Complex32 -> None
    | Complex64 -> None
    | Complexld -> None )

let int8_max = Big_int.big_int_of_int 127

let int8_min = Big_int.big_int_of_int (-128)

let int16_max = Big_int.big_int_of_int 32767

let int16_min = Big_int.big_int_of_int (-32768)

let int32_max = Big_int.big_int_of_int32 Int32.max_int

let int32_min = Big_int.big_int_of_int32 Int32.min_int

let int64_min = Big_int.big_int_of_int64 Int64.min_int

let int64_max = Big_int.big_int_of_int64 Int64.max_int

let uint8_max = Big_int.big_int_of_int 255

let uint16_max = Big_int.big_int_of_int 65535

let uint32_max = Big_int.big_int_of_int64 4294967295L

let uint64_max = Big_int.big_int_of_string "18446744073709551615"

module X = struct
  open Mparsetree.Ast_cur.Ast_helper

  let string x = Exp.constant (Const.string x)

  let char x = Exp.constant (Const.char x)

  let int x = Exp.constant (Const.int x)

  let int32 x = Exp.constant (Const.int32 x)

  let int64 x = Exp.constant (Const.int64 x)

  let nativeint x = Exp.constant (Const.nativeint x)
end

type result =
  | Expr of Mparsetree.Ast_cur.Parsetree.expression
  | Underflow
  | Overflow

exception Eunderflow

exception Eoverflow

let gen t str =
  let camlint_max =
    if Ocaml_config.word_size () = 64 then
      Big_int.big_int_of_int64 4611686018427387903L
    else Big_int.big_int_of_int 1073741823
  in
  let camlint_min =
    if Ocaml_config.word_size () = 64 then
      Big_int.big_int_of_int64 (-4611686018427387904L)
    else Big_int.big_int_of_int (-1073741824)
  in
  let intnative_max =
    if Ocaml_config.word_size () = 64 then
      Big_int.big_int_of_int64 9223372036854775807L
    else Big_int.big_int_of_int32 2147483647l
  in
  let intnative_min =
    if Ocaml_config.word_size () = 64 then
      Big_int.big_int_of_int64 (-9223372036854775808L)
    else Big_int.big_int_of_int32 (-2147483648l)
  in
  let ( < ) a b = Big_int.compare_big_int a b < 0 in
  let ( <= ) a b = Big_int.compare_big_int a b <= 0 in
  let ( > ) a b = Big_int.compare_big_int a b > 0 in
  let ( >= ) a b = Big_int.compare_big_int a b >= 0 in
  let ( = ) a b = Big_int.compare_big_int a b = 0 in
  let xint r = X.int (Big_int.int_of_big_int r) in
  let xint64 r = X.int64 (Big_int.int64_of_big_int r) in
  let xstr r = Big_int.string_of_big_int r |> X.string in
  let check_limits r min max =
    if r < min then raise_notrace Eunderflow ;
    if r > max then raise_notrace Eoverflow
  in
  let normal_int r min max =
    check_limits r min max ;
    xint r
  in
  let rec as_sum ~add ~of_int ~of_int64 r =
    if r <= camlint_max then [%expr [%e of_int] [%e xint r]]
    else if r <= int64_max then [%expr [%e of_int64] [%e xint64 r]]
    else
      let ( - ) = Big_int.sub_big_int in
      let e = as_sum ~add ~of_int ~of_int64 (r - int64_max) in
      [%expr [%e add] [%e e] ([%e of_int64] [%e xint64 int64_max])]
  in
  let r = Big_int.big_int_of_string str in
  match t.info with
  | Extr_char -> Char.chr ((Big_int.int_of_big_int r + 256) mod 256) |> X.char
  | Extr_schar -> normal_int r int8_min int8_max
  | Extr_short -> Big_int.int_of_big_int r |> X.int
  | Extr_int -> normal_int r camlint_min camlint_max
  | Extr_int8_t -> normal_int r int8_min int8_max
  | Extr_int16_t -> normal_int r int16_min int16_max
  | Extr_camlint -> normal_int r camlint_min camlint_max
  | Extr_bool ->
    check_limits r Big_int.zero_big_int Big_int.unit_big_int ;
    if r = Big_int.zero_big_int then [%expr false] else [%expr true]
  | Extr_sint ->
    if r >= camlint_min && r <= camlint_max then
      [%expr Signed.SInt.of_int [%e xint r]]
    else if r >= int64_min && r <= int64_max then
      [%expr Signed.SInt.of_int64 [%e xint64 r]]
    else [%expr Signed.SInt.of_string [%e xstr r]]
  | Extr_long ->
    if r >= camlint_min && r <= camlint_max then
      [%expr Signed.Long.of_int [%e xint r]]
    else if r >= int64_min && r <= int64_max then
      [%expr Signed.Long.of_int64 [%e xint64 r]]
    else [%expr Signed.Long.of_string [%e xstr r]]
  | Extr_llong ->
    if r >= camlint_min && r <= camlint_max then
      [%expr Signed.LLong.of_int [%e xint r]]
    else if r >= int64_min && r <= int64_max then
      [%expr Signed.LLong.of_int64 [%e xint64 r]]
    else [%expr Signed.LLong.of_string [%e xstr r]]
  | Extr_int32_t ->
    check_limits r int32_min int32_max ;
    Big_int.int32_of_big_int r |> X.int32
  | Extr_nativeint ->
    check_limits r intnative_min intnative_max ;
    Big_int.nativeint_of_big_int r |> X.nativeint
  | Extr_int64_t -> xint64 r
  | Extr_uchar ->
    check_limits r Big_int.zero_big_int uint8_max ;
    [%expr Unsigned.UChar.of_int [%e xint r]]
  | Extr_ushort ->
    if r <= camlint_max then [%expr Unsigned.UShort.of_int [%e xint r]]
    else if r <= int64_max then [%expr Unsigned.UShort.of_int64 [%e xint64 r]]
    else [%expr Unsigned.UShort.of_string [%e xstr r]]
  | Extr_uint ->
    if r <= camlint_max then [%expr Unsigned.UInt.of_int [%e xint r]]
    else if r <= int64_max then [%expr Unsigned.UInt.of_int64 [%e xint64 r]]
    else [%expr Unsigned.UInt.of_string [%e xstr r]]
  | Extr_ulong ->
    as_sum r ~add:[%expr Unsigned.ULong.add]
      ~of_int:[%expr Unsigned.ULong.of_int]
      ~of_int64:[%expr Unsigned.ULong.of_int64]
  | Extr_ullong ->
    as_sum r ~add:[%expr Unsigned.ULLong.add]
      ~of_int:[%expr Unsigned.ULLong.of_int]
      ~of_int64:[%expr Unsigned.ULLong.of_int64]
  | Extr_size_t ->
    as_sum r ~add:[%expr Unsigned.Size_t.add]
      ~of_int:[%expr Unsigned.Size_t.of_int]
      ~of_int64:[%expr Unsigned.Size_t.of_int64]
  | Extr_uint8_t ->
    check_limits r Big_int.zero_big_int uint8_max ;
    [%expr Unsigned.UInt8.of_int [%e xint r]]
  | Extr_uint16_t ->
    check_limits r Big_int.zero_big_int uint16_max ;
    [%expr Unsigned.UInt16.of_int [%e xint r]]
  | Extr_uint32_t ->
    check_limits r Big_int.zero_big_int uint32_max ;
    if r <= camlint_max then [%expr Unsigned.UInt32.of_int [%e xint r]]
    else [%expr Unsigned.UInt32.of_int64 [%e xint64 r]]
  | Extr_uint64_t ->
    check_limits r Big_int.zero_big_int uint64_max ;
    as_sum r ~add:[%expr Unsigned.UInt64.add]
      ~of_int:[%expr Unsigned.UInt64.of_int]
      ~of_int64:[%expr Unsigned.UInt64.of_int64]

let gen t str =
  try Expr (gen t str) with Eunderflow -> Underflow | Eoverflow -> Overflow
