(* This file is part of ppx_cstubs (https://github.com/fdopen/ppx_cstubs)
 * Copyright (c) 2018 fdopen
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>. *)

open Std

let prologue = {|
#include <stddef.h>
#include <limits.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctypes_cstubs_internals.h>

#ifdef ARCH_SIXTYFOUR
#define CAMLINT_MAX INT64_C(4611686018427387903)
#define CAMLINT_MIN INT64_C(-4611686018427387904)
#else
#define CAMLINT_MAX  1073741823
#define CAMLINT_MIN (-1073741824)
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
#define PPXC__UD19(x) ('0' + ((char)(( ((uint64_t)(x)) / UINT64_C(10000000000000000000)) % UINT64_C(10)))), PPXC__UD18(((uint64_t)(x)))

#define PPXC__D00(x) ('0' + ((char)(( (x) / INT64_C(1)                    ) % INT64_C(10))))
#define PPXC__D01(x) ('0' + ((char)(( (x) / INT64_C(10)                   ) % INT64_C(10)))), PPXC__D00(x)
#define PPXC__D02(x) ('0' + ((char)(( (x) / INT64_C(100)                  ) % INT64_C(10)))), PPXC__D01(x)
#define PPXC__D03(x) ('0' + ((char)(( (x) / INT64_C(1000)                 ) % INT64_C(10)))), PPXC__D02(x)
#define PPXC__D04(x) ('0' + ((char)(( (x) / INT64_C(10000)                ) % INT64_C(10)))), PPXC__D03(x)
#define PPXC__D05(x) ('0' + ((char)(( (x) / INT64_C(100000)               ) % INT64_C(10)))), PPXC__D04(x)
#define PPXC__D06(x) ('0' + ((char)(( (x) / INT64_C(1000000)              ) % INT64_C(10)))), PPXC__D05(x)
#define PPXC__D07(x) ('0' + ((char)(( (x) / INT64_C(10000000)             ) % INT64_C(10)))), PPXC__D06(x)
#define PPXC__D08(x) ('0' + ((char)(( (x) / INT64_C(100000000)            ) % INT64_C(10)))), PPXC__D07(x)
#define PPXC__D09(x) ('0' + ((char)(( (x) / INT64_C(1000000000)           ) % INT64_C(10)))), PPXC__D08(x)
#define PPXC__D10(x) ('0' + ((char)(( (x) / INT64_C(10000000000)          ) % INT64_C(10)))), PPXC__D09(x)
#define PPXC__D11(x) ('0' + ((char)(( (x) / INT64_C(100000000000)         ) % INT64_C(10)))), PPXC__D10(x)
#define PPXC__D12(x) ('0' + ((char)(( (x) / INT64_C(1000000000000)        ) % INT64_C(10)))), PPXC__D11(x)
#define PPXC__D13(x) ('0' + ((char)(( (x) / INT64_C(10000000000000)       ) % INT64_C(10)))), PPXC__D12(x)
#define PPXC__D14(x) ('0' + ((char)(( (x) / INT64_C(100000000000000)      ) % INT64_C(10)))), PPXC__D13(x)
#define PPXC__D15(x) ('0' + ((char)(( (x) / INT64_C(1000000000000000)     ) % INT64_C(10)))), PPXC__D14(x)
#define PPXC__D16(x) ('0' + ((char)(( (x) / INT64_C(10000000000000000)    ) % INT64_C(10)))), PPXC__D15(x)
#define PPXC__D17(x) ('0' + ((char)(( (x) / INT64_C(100000000000000000)   ) % INT64_C(10)))), PPXC__D16(x)
#define PPXC__D18(x) ('0' + ((char)(( ((int64_t)(x)) / INT64_C(1000000000000000000)) % INT64_C(10)))), PPXC__D17(((int64_t)(x)))

#if !defined(__cplusplus) && ((defined(__GNUC__) && ( __GNUC__ >= 4 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1))) || defined(__clang__))
#define PPXC_IS_INTEGER(x)                                            \
  (__builtin_types_compatible_p (__typeof__ (x), _Bool) ||            \
   __builtin_types_compatible_p (__typeof__ (x), int8_t) ||           \
   __builtin_types_compatible_p (__typeof__ (x), int16_t) ||          \
   __builtin_types_compatible_p (__typeof__ (x), int32_t) ||          \
   __builtin_types_compatible_p (__typeof__ (x), int64_t) ||          \
   __builtin_types_compatible_p (__typeof__ (x), int) ||              \
   __builtin_types_compatible_p (__typeof__ (x), signed char) ||      \
   __builtin_types_compatible_p (__typeof__ (x), signed long) ||      \
   __builtin_types_compatible_p (__typeof__ (x), signed long long) || \
   __builtin_types_compatible_p (__typeof__ (x), uint8_t) ||          \
   __builtin_types_compatible_p (__typeof__ (x), uint16_t) ||         \
   __builtin_types_compatible_p (__typeof__ (x), uint32_t) ||         \
   __builtin_types_compatible_p (__typeof__ (x), uint64_t) ||         \
   __builtin_types_compatible_p (__typeof__ (x), unsigned char) ||    \
   __builtin_types_compatible_p (__typeof__ (x), unsigned int) ||     \
   __builtin_types_compatible_p (__typeof__ (x), unsigned long) ||    \
   __builtin_types_compatible_p (__typeof__ (x), unsigned long long) || \
   __builtin_types_compatible_p (__typeof__ (x), size_t))

#else
#define PPXC_IS_INTEGER(x) (1)
#endif

#if defined(_WIN32) && defined(_MSC_VER)

/* macro doesn't work for doubles .... */

#define PPXC_CTYPES_CHECK_UNSIGNED(TYPENAME)             \
  ((unsigned)(((TYPENAME) (-1)) > 0)         \
   << CTYPES_UNSIGNED_FLAG_BIT)

#define PPXC_CTYPES_CLASSIFY(TYPENAME) (PPXC_CTYPES_CHECK_UNSIGNED(TYPENAME))

#define PPXC_CTYPES_ARITHMETIC_TYPEINFO(TYPENAME) (PPXC_CTYPES_CLASSIFY(TYPENAME)   \
                                       | sizeof(TYPENAME))

#else

#define PPXC_CTYPES_ARITHMETIC_TYPEINFO(x) CTYPES_ARITHMETIC_TYPEINFO(x)

#endif


|}

module List = CCListLabels

let cnt =
  let i = ref 0 in
  fun () ->
    let res = !i in
    incr i;
    res

let int_to_char_array i =
  let s = Printf.sprintf "%x" i in
  let b = Buffer.create ((String.length s) * 4) in
  String.iter (fun c ->
    Printf.bprintf b "'%c'," c) s;
  Buffer.contents b

type id = int

type int_assert_check = {
  id_min: id;
  id_max: id;
  id_is_int: id;
  id_user_min: id option;
  id_user_max: id option;
}

type intern =
  | String
  | Integer of int_assert_check

type extract_info = {
  id: id;
  single_prog: string;
  intern: intern;
}

let remove_file f = try Sys.remove f with Sys_error _ -> ()

let prepare_extract_int ?min ?max ~buf ~c_header ~expr ~signed () =
  let len = String.length prologue + String.length c_header + 3072 in
  let buf_single = Buffer.create len in
  Buffer.add_string buf_single prologue;
  Buffer.add_string buf_single c_header;
  let gen info =
    let id = cnt () in
    let ar = int_to_char_array id in
    let s = Printf.sprintf {|
      const char ppx_c_extract_char_array_%x[] = {
      'P','P','X','C','_','C','O','N','S','T','_','N','R','_', %s '#',
      %s,
      '#', %s '_','R','N','_','T','S','N','O','C','_','C','X','P','P', '\0' };
     |} id ar info ar in
    id,s in
  let macro,def_int_min,def_int_max = match signed with
  | false -> "PPXC__UD19","0","UINT64_MAX"
  | true -> "PPXC__D18","INT64_MIN","INT64_MAX" in
  let id,ex_str = gen @@ Printf.sprintf "%s((%s))" macro expr in
  let id_is_int,ex_is_int =
    gen @@ Printf.sprintf "PPXC__D18( PPXC_IS_INTEGER(%s) )" expr in
  let id_min,ex_min =
    gen @@ Printf.sprintf "PPXC__D18( (%s) >= 0 || (%s) >= %s)"
      expr expr def_int_min in
  let id_max,ex_max =
    gen @@ Printf.sprintf "PPXC__D18( (%s) <= 0 || (%s) <= %s)"
      expr expr def_int_max in
  let f x format = match x with
  | None -> None, ""
  | Some x ->
    let a,b = gen @@ Printf.sprintf format expr expr x in
    Some a,b in
  let id_user_min,ex_user_min = f min "PPXC__D18( (%s) >= 0 || (%s) >= %s )" in
  let id_user_max,ex_user_max = f max "PPXC__D18( (%s) <= 0 || (%s) <= %s )" in
  List.iter [buf_single; buf] ~f:(fun buf ->
    Buffer.add_string buf ex_str;
    Buffer.add_string buf ex_min;
    Buffer.add_string buf ex_max;
    Buffer.add_string buf ex_is_int;
    Buffer.add_string buf ex_user_min;
    Buffer.add_string buf ex_user_max);
  { id ;
    single_prog = Buffer.contents buf_single;
    intern = Integer {
      id_min;
      id_max;
      id_is_int;
      id_user_min;
      id_user_max; }
  }

let prepare_extract_string ~buf ~c_header ~expr () =
  let buf_single = Buffer.create 4096 in
  Buffer.add_string buf_single prologue;
  Buffer.add_string buf_single c_header;
  let cnt = cnt () in
  List.iter [buf_single; buf] ~f:(fun buf ->
    Printf.bprintf buf {|
const char *ppx_c_extract_char_string%x = "PPXC_CONST_NR_%x#" %s "#%x_RN_TSNOC_CXPP";
     |} cnt cnt expr cnt);
  { id = cnt;
    single_prog = Buffer.contents buf_single;
    intern = String }

type obj = string

let compile ?ebuf c_prog =
  let ocaml_flags = !Options.ocaml_flags in
  let cfln = Filename.temp_file "ppxc_extract" ".c" in
  finally ~h:(fun () -> if not !Options.keep_tmp then remove_file cfln) @@ fun () ->
  CCIO.with_out
    ?mode:None ~flags:[Open_creat; Open_trunc; Open_binary] cfln (fun ch ->
      output_string ch c_prog);
  let obj = Filename.chop_suffix cfln ".c" ^ (Ocaml_config.ext_obj ()) in
  let c_flags =
    (* that's a suboptimal solution. `ocamlc -c foo.c -o foo.o` doesn't work:
        "Options -c and -o are incompatible when compiling C files"
       But I might have no write access in the current directory and I'm
       unsure how '-I' flags and similar options are affected, if I change the
       current working directory ... *)
    match Ocaml_config.system () |> CCString.lowercase_ascii with
    | "win32" | "win64" -> ["-Fo:" ^ obj]
    | _ -> ["-o";obj] in
  let dir = match !Options.ml_input_file with
  | None -> failwith "ml_input_file not set"
  | Some s -> Filename.dirname s in
  let c_flags = "-I"::dir::c_flags in
  let c_flags = !Options.c_flags @ c_flags in
  let args = List.map c_flags ~f:(fun c -> "-ccopt"::c::[]) |> List.flatten in
  let args = ocaml_flags @ args in
  let args = "c"::"-c"::cfln::args in
  let args = match !Options.toolchain with
  | None -> args
  | Some s -> "-toolchain"::s::args in
  let stdout = if !Options.verbosity > 0 then `Stdout else `Null in
  let stderr =
    if !Options.verbosity > 1 then `Stderr
    else match ebuf with
    | None -> `Null
    | Some ebuf -> `Buffer ebuf in
  let prog = Options.ocamlfind in
  finally ~h:(fun () -> if not !Options.keep_tmp then remove_file obj) @@ fun () ->
  if !Options.verbosity > 0 then
    Run.cmd_to_string prog args |> prerr_endline;
  match Run.run prog args ~stdout ~stderr with
  | exception (Unix.Unix_error(e,s,_)) ->
    let cmd = Run.cmd_to_string prog args in
    Error (Printf.sprintf
             "Process creation \"%s\" failed with %s (%S)"
             cmd (Unix.error_message e) s)
  | 0 ->
    CCIO.with_in
      ?mode:None ~flags:[Open_binary] obj @@ fun ch ->
    let s = CCIO.read_all ch in
    if s = "" then
      Error "`ocamlfind ocamlc -c` created an empty obj file"
    else
      Ok s
  | ec -> Error (Printf.sprintf "`ocamlfind ocamlc -c` failed with %d" ec)

type extract_error =
  | Info_not_found
  | Overflow
  | Underflow
  | User_overflow
  | User_underflow
  | Not_an_integer

let negative_convert s =
  if false = CCString.exists (fun c -> c >= '\'' && c <= '/') s then s
  else
  let s = CCString.map (fun c ->
    if c >= '\'' && c <= '/' then Char.chr (96 - Char.code c) else c) s in
  "-" ^ s

let extract info (obj:obj) =
  with_return @@ fun r ->
  let extract_single id =
    let s = Printf.sprintf "%x" id in
    let prefix = "PPXC_CONST_NR_" ^ s ^ "#" in
    let suffix = "#" ^ s ^ "_RN_TSNOC_CXPP" in
    let start = CCString.find ~sub:prefix obj in
    if start < 0 then
      r.return (Error Info_not_found);
    let start = start + String.length prefix in
    let end' = CCString.find ~start ~sub:suffix obj in
    if end' < 0 then
      r.return (Error Info_not_found);
    String.sub obj start (end' - start) in
  let res = extract_single info.id in
  match info.intern with
  | String -> Ok res
  | Integer x ->
    let res = negative_convert res in
    let verify id er = match int_of_string @@ extract_single id with
    | exception (Failure _) -> r.return (Error er)
    | n -> if n <> 1 then r.return (Error er) in
    verify x.id_min Underflow;
    verify x.id_max Overflow;
    verify x.id_is_int Not_an_integer;
    let verify' er = function
    | None -> ()
    | Some x -> verify x er in
    verify' User_underflow x.id_user_min;
    verify' User_overflow x.id_user_max;
    Ok res
