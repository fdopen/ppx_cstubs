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
#define PPXC__UD19(x) ('0' + ((char)(( (x) / UINT64_C(10000000000000000000) ) % UINT64_C(10)))), PPXC__UD18(x)
#define PPXC__NSTR(x) ((x) >= 0 ? '0' : '-'), PPXC__UD19(((x) >= 0 ? ((uint64_t)(x)) : \
 ( (x) == INT64_MIN ? UINT64_C(9223372036854775808) : ((uint64_t)(-((int64_t)(x)))))))

#if !defined(__cplusplus) && ((defined(__GNUC__) && ( __GNUC__ >= 4 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1))) || defined(__clang__))
#define PPXC_IS_INTEGER(x)                                            \
  (__builtin_types_compatible_p (__typeof__ (x), _Bool) ||            \
   __builtin_types_compatible_p (__typeof__ (x), int8_t) ||           \
   __builtin_types_compatible_p (__typeof__ (x), int16_t) ||          \
   __builtin_types_compatible_p (__typeof__ (x), int32_t) ||          \
   __builtin_types_compatible_p (__typeof__ (x), int64_t) ||          \
   __builtin_types_compatible_p (__typeof__ (x), signed char) ||      \
   __builtin_types_compatible_p (__typeof__ (x), short) ||            \
   __builtin_types_compatible_p (__typeof__ (x), int) ||              \
   __builtin_types_compatible_p (__typeof__ (x), signed long) ||      \
   __builtin_types_compatible_p (__typeof__ (x), signed long long) || \
   __builtin_types_compatible_p (__typeof__ (x), uint8_t) ||          \
   __builtin_types_compatible_p (__typeof__ (x), uint16_t) ||         \
   __builtin_types_compatible_p (__typeof__ (x), uint32_t) ||         \
   __builtin_types_compatible_p (__typeof__ (x), uint64_t) ||         \
   __builtin_types_compatible_p (__typeof__ (x), unsigned char) ||    \
   __builtin_types_compatible_p (__typeof__ (x), unsigned short) ||   \
   __builtin_types_compatible_p (__typeof__ (x), unsigned int) ||     \
   __builtin_types_compatible_p (__typeof__ (x), unsigned long) ||    \
   __builtin_types_compatible_p (__typeof__ (x), unsigned long long) || \
   __builtin_types_compatible_p (__typeof__ (x), size_t))

#define PPXC_IS_FLOAT(x)                                              \
  (__builtin_types_compatible_p (__typeof__ (x), float) ||            \
   __builtin_types_compatible_p (__typeof__ (x), double) ||           \
   __builtin_types_compatible_p (__typeof__ (x), long double))

#define PPXC_IS_COMPLEX(x)                                            \
  (__builtin_types_compatible_p (__typeof__ (x), float _Complex) ||   \
   __builtin_types_compatible_p (__typeof__ (x), double _Complex) ||  \
   __builtin_types_compatible_p (__typeof__ (x), long double _Complex))

#else
#define PPXC_IS_INTEGER(x) (1)
#define PPXC_IS_FLOAT(x) (1)
#define PPXC_IS_COMPLEX(x) (1)
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

/* __builtin_types_compatible_p is too picky with regard to const qualified
   pointers.  */
#define PPXC_TYPES_COMPATIBLE(a,b)                \
  ((sizeof(a) == sizeof(b)) &&                    \
   (PPXC_IS_INTEGER(a) == PPXC_IS_INTEGER(b)) &&  \
   (PPXC_IS_FLOAT(a) == PPXC_IS_FLOAT(b)) &&      \
   (PPXC_IS_COMPLEX(a) == PPXC_IS_COMPLEX(b)))
|}

module List = CCListLabels

let cnt =
  let i = ref 0 in
  fun () ->
    let res = !i in
    incr i;
    res

let int_to_char_array i =
  let s = Printf.sprintf "%d" i in
  let b = Buffer.create ((String.length s) * 4) in
  String.iter (fun c -> Printf.bprintf b "'%c'," c) s;
  Buffer.contents b

type id = int

type intern =
  | String
  | Integer of id
  | Unchecked_integer

type extract_info = {
  id: id;
  single_prog: string;
  intern: intern;
}

let remove_file f = try Sys.remove f with Sys_error _ -> ()

let prepare_extract_int
    ?(disable_checks=false) ?min ?max ~buf ~c_header ~expr ~signed () =
  let len = String.length prologue + String.length c_header + 3072 in
  let buf_single = Buffer.create len in
  Buffer.add_string buf_single prologue;
  Buffer.add_string buf_single c_header;
  let gen info =
    let id = cnt () in
    let ar = int_to_char_array id in
    let s = Printf.sprintf {|
      const char ppx_c_extract_char_array_%d[] = {
      'P','P','X','C','_','C','O','N','S','T','_','N','R','_', %s '|',
      PPXC__NSTR(%s),
      '|', %s '_','R','N','_','T','S','N','O','C','_','C','X','P','P', '\0' };
     |} id ar info ar in
    id,s in
  let def_int_min,def_int_max = match signed with
  | false -> "0","UINT64_MAX"
  | true -> "INT64_MIN","INT64_MAX" in
  let id,ex_str = gen expr in
  Buffer.add_string buf ex_str;
  Buffer.add_string buf_single ex_str;
  if disable_checks = true then
    {id ; single_prog = Buffer.contents buf_single; intern = Unchecked_integer}
  else
  let s_int =
    Printf.sprintf "( PPXC_IS_INTEGER(%s) )" expr in
  let s_min =
    Printf.sprintf "( (%s) >= 0 || (%s) >= %s )" expr expr def_int_min in
  let s_max =
    Printf.sprintf "( (%s) <= 0 || (%s) <= %s )" expr expr def_int_max in
  let f x format = match x with
  | None -> "1"
  | Some x -> Printf.sprintf format expr expr x in
  let s_user_min = f min "( (%s) >= 0 || (%s) >= %s )" in
  let s_user_max = f max "( (%s) <= 0 || (%s) <= %s )" in
  let id_x,str  =
    gen @@ Printf.sprintf
      "( (((unsigned)(%s)) << 0u) | (((unsigned)(%s)) << 1u) | (((unsigned)(%s)) << 2u) | (((unsigned)(%s)) << 3u) | (((unsigned)(%s)) << 4u) )"
      s_int s_min s_max s_user_min s_user_max in
  Buffer.add_string buf str;
  Buffer.add_string buf_single str;
  {id ; single_prog = Buffer.contents buf_single; intern = Integer id_x}

let prepare_extract_string ~buf ~c_header ~expr () =
  let buf_single = Buffer.create 4096 in
  Buffer.add_string buf_single prologue;
  Buffer.add_string buf_single c_header;
  let cnt = cnt () in
  List.iter [buf_single; buf] ~f:(fun buf ->
    Printf.bprintf buf {|
const char *ppx_c_extract_char_string%d = "PPXC_CONST_NR_%d|" %s "|%d_RN_TSNOC_CXPP";
     |} cnt cnt expr cnt);
  { id = cnt;
    single_prog = Buffer.contents buf_single;
    intern = String }

let threads () = (* just to make ocamlfind silent, not necessary at all *)
  match Findlib.package_directory "threads" with
  | exception (Fl_package_base.No_such_package _ ) -> None
  | dir ->
    if List.exists !Config.load_path ~f:((=) dir) then
      Some "-thread"
    else
      None

type obj = (int,string) Hashtbl.t

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
  let args = match threads () with
  | None -> args
  | Some x -> x::args in
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

let rex =
  Re.Perl.re ~opts:[`Ungreedy; `Dotall]
    "PPXC_CONST_NR_([0-9]+)\\|(.*)\\|([0-9]+)_RN_TSNOC_CXPP\000"
  |> Re.compile

let compile ?ebuf c_prog =
  match compile ?ebuf c_prog with
  | (Error _) as x -> x
  | Ok s ->
    let rec iter i s len htl =
      if i >= len then htl else
      match Re.exec_opt ~pos:i rex s with
      | None -> htl
      | Some g ->
        let end' =
          try
            let id1 = Re.Group.get g 1 in
            let id2 = Re.Group.get g 3 in
            if id1 = id2 then
              let str = Re.Group.get g 2 in
              Hashtbl.add htl (int_of_string id1) str;
              Re.Group.stop g 0
            else
              succ i
          with
          | Not_found | Failure _ -> succ i in
        iter end' s len htl in
    let h = iter 0 s (String.length s) (Hashtbl.create 64) in
    Ok h

type extract_error =
  | Info_not_found
  | Overflow of string
  | Underflow of string
  | User_overflow of string
  | User_underflow of string
  | Not_an_integer

let normalise_int str =
  let len = String.length str in
  if len < 1 then str else
  let b = Buffer.create len in
  let start = match str.[0] with
  | '-' as c -> Buffer.add_char b c; 1
  | _ -> 0 in
  let rec iter i =
    if i >= len then
      Buffer.add_char b '0'
    else match str.[i] with
    | '0' -> iter (succ i)
    | _ -> Buffer.add_substring b str i (len - i) in
  iter start;
  Buffer.contents b

let extract info htl =
  with_return @@ fun r ->
  let extract_single id =
    match Hashtbl.find htl id with
    | exception Not_found -> r.return (Error Info_not_found)
    | s -> s in
  let res = extract_single info.id in
  match info.intern with
  | String -> Ok res
  | Unchecked_integer -> Ok (normalise_int res)
  | Integer x ->
    let res = normalise_int res in
    let int' = match int_of_string @@ extract_single x with
    | exception (Failure _) -> r.return (Error Info_not_found)
    | x -> x in
    let verify i er =
      if int' land (1 lsl i) = 0 then
        r.return (Error er) in
    verify 0 Not_an_integer;
    verify 1 (Underflow res);
    verify 2 (Overflow res);
    verify 3 (User_underflow res);
    verify 4 (User_overflow res);
    Ok res
