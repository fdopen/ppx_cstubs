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

module OLocation = Location

type top_run =
  < init :
      nopervasives:bool ->
      pkgs:string list ->
      use_threads:bool ->
      cma_files:string list ->
      unit ->
      unit
  ; eval : Parsetree.structure -> unit
  ; is_merlin_ppx : bool >

open Mparsetree.Ast_cur
module List = CCListLabels
module Internals = Ppx_cstubs.Ppx_cstubs_internals

type id = Marshal_types.id

module G = Script_result

module U = struct
  include Std.Util

  let from_id_loc_param x : Marshal_types.id_loc_param = Marshal.from_string x 0

  let unmarshal_expr s : Marshal_types.expr = Marshal.from_string s 0
end

module type Empty = sig end

module C_content_make (E : Empty) = struct
  type t =
    | Header of string * Marshal_types.loc
    | Extract_header of string * Marshal_types.loc * exn
    | Extract_source of
        string * Marshal_types.loc * exn * (Extract_c.obj -> unit)
    | Function of id

  let default_header =
    {|
#include <ctypes_cstubs_internals.h>
#include <caml/callback.h>
#include <caml/fail.h>

#if !defined(__cplusplus)
#if defined(__clang__) && (__clang_major__ > 3 || ((__clang_major__ == 3) && (__clang_minor__ >= 3)))
#define DISABLE_CONST_WARNINGS_PUSH()                                   \
  _Pragma("clang diagnostic push")                                      \
  _Pragma("clang diagnostic ignored \"-Wincompatible-pointer-types-discards-qualifiers\"")
#define DISABLE_CONST_WARNINGS_POP()            \
  _Pragma("clang diagnostic pop")
#elif !defined(__clang__) && defined(__GNUC__) && ( __GNUC__ >= 5 )
#define DISABLE_CONST_WARNINGS_PUSH()                           \
  _Pragma("GCC diagnostic push")                                \
  _Pragma("GCC diagnostic ignored \"-Wdiscarded-qualifiers\"") \
  _Pragma("GCC diagnostic ignored \"-Wdiscarded-array-qualifiers\"")
#define DISABLE_CONST_WARNINGS_POP() \
    _Pragma("GCC diagnostic pop")
#endif
#endif

#ifndef DISABLE_CONST_WARNINGS_PUSH
#define DISABLE_CONST_WARNINGS_PUSH()
#endif

#ifndef DISABLE_CONST_WARNINGS_POP
#define DISABLE_CONST_WARNINGS_POP()
#endif

#ifndef CAMLdrop
#define CAMLdrop caml_local_roots = caml__frame
#endif

#ifdef __cplusplus
#define PPX_CSTUBS_ADDR_OF_FATPTR(typ,var)      \
  (typ)(CTYPES_ADDR_OF_FATPTR(var))
#else
#define PPX_CSTUBS_ADDR_OF_FATPTR(typ,var)      \
  CTYPES_ADDR_OF_FATPTR(var)
#endif

#ifndef CTYPES_PTR_OF_OCAML_BYTES
#ifdef Bytes_val
#define CTYPES_PTR_OF_OCAML_BYTES(s)   \
  (Bytes_val(Field(s, 1)) + Long_val(Field(s, 0)))
#else
#define CTYPES_PTR_OF_OCAML_BYTES(s) CTYPES_PTR_OF_OCAML_STRING(s)
#endif
#endif
|}

  let htl_id_func = Hashtbl.create 16

  let init_entries =
    let loc = !Ast_helper.default_loc in
    let loc = { loc with Location.loc_ghost = true } in
    let exn = U.error_exn "ppx_cstubs header erroneous?" in
    [
      Header (default_header, loc); Extract_header (Extract_c.prologue, loc, exn);
    ]

  let entries = ref init_entries

  let clear () =
    entries := init_entries;
    Hashtbl.clear htl_id_func

  let add_header h loc = entries := Header (h, loc) :: !entries

  let add_extract_header h loc exn =
    entries := Extract_header (h, loc, exn) :: !entries

  let add_extract_source h loc exn f =
    entries := Extract_source (h, loc, exn, f) :: !entries

  let add_function id s = Hashtbl.replace htl_id_func id s

  let add_function_place id = entries := Function id :: !entries

  let get_extract_source () =
    List.fold_left ~init:[] !entries ~f:(fun a -> function
      | Header (h, _) | Extract_source (h, _, _, _) | Extract_header (h, _, _)
        ->
        h :: a
      | Function _ -> a)
    |> String.concat "\n"

  let get_cur_headers () =
    List.fold_left ~init:[] !entries ~f:(fun a -> function
      | Header (h, _) -> h :: a
      | Extract_source _ | Extract_header _ | Function _ -> a)
    |> List.rev

  let get_source () =
    let cnt, l =
      List.fold_left ~init:(0, []) !entries ~f:(fun ((cnt, ac) as cnt_ac) ->
        function
        | Function id ->
          let s =
            try Hashtbl.find htl_id_func id
            with Not_found -> U.error "fatal: incomplete source code"
          in
          (succ cnt, s :: ac)
        | Header (s, _) -> (cnt, s :: ac)
        | Extract_header _ | Extract_source _ -> cnt_ac)
    in
    if Hashtbl.length htl_id_func <> cnt then
      U.error "fatal: source code incomplete";
    if cnt = 0 then None else Some (String.concat "\n\n" l)
end

module C_content_phase0 = C_content_make ()

module type Const_common_s = sig
  include module type of C_content_phase0
end

module Const_common (C_content : Const_common_s) = struct
  let htl_id_entries = Hashtbl.create 32

  let compiled = ref false

  let clear () =
    Hashtbl.clear htl_id_entries;
    compiled := false

  let find_failing ebuf =
    let orig_ebuf = Buffer.contents ebuf in
    let fail ~emsg l n =
      let emsg =
        if !Options.verbosity > 0 then emsg
        else
          CCString.split_on_char '\n' emsg
          |> List.take 7
          |> List.map ~f:CCString.rtrim
          |> String.concat "\n"
      in
      let elem = List.nth l n in
      let loc = match elem with _, `Header l | _, `Eheader (l, _) -> l in
      OLocation.print_loc Format.err_formatter loc;
      Format.pp_print_char Format.err_formatter ':';
      Format.pp_print_newline Format.err_formatter ();
      prerr_string emsg;
      match elem with
      | _, `Header loc -> U.error ~loc "error in header"
      | _, `Eheader (_, e) -> raise e
    in
    (* binary search for first failing code snippet.
       low - 1: always compiles
       high + 1: always fails to compile *)
    let rec iter l ~emsg low high =
      assert (low <= high);
      let mid = (low + high) / 2 in
      let l_s = List.take (succ mid) l in
      let source = String.concat "\n" l_s in
      Buffer.clear ebuf;
      match Extract_c.compile ~ebuf source with
      | Ok _ ->
        if low = high then (succ low, emsg) else iter l ~emsg (succ mid) high
      | Error _ ->
        let emsg = Buffer.contents ebuf in
        if low = mid then (low, emsg) else iter l ~emsg low (pred mid)
    in
    (* The code for extraction gives obscure error messages and its
       compilation is slow. Therefore I first try to find the error in
       other source parts that have been created for this purpose *)
    let l =
      List.fold_left !C_content.entries ~init:[] ~f:(fun ac -> function
        | C_content.Header (s, l) -> (s, `Header l) :: ac
        | C_content.Extract_header (s, l, e) -> (s, `Eheader (l, e)) :: ac
        | C_content.Extract_source _ | C_content.Function _ -> ac)
    in
    let l_s = List.map l ~f:fst in
    let len = List.length l in
    let n, emsg = iter l_s ~emsg:orig_ebuf 0 (pred len) in
    if n < len then fail ~emsg l n
    else
      let l =
        List.fold_left !C_content.entries ~init:[] ~f:(fun ac -> function
          | C_content.Header (s, l) -> (s, `Header l) :: ac
          | C_content.Extract_header (s, l, e)
          | C_content.Extract_source (s, l, e, _) ->
            (s, `Eheader (l, e)) :: ac
          | C_content.Function _ -> ac)
      in
      let l_s = List.map l ~f:fst in
      let len = List.length l in
      let n, emsg = iter l_s ~emsg:orig_ebuf 0 (len - 2) in
      fail ~emsg l n

  let extract_all () =
    if !compiled = false && Options.(!mode <> Emulate) then (
      let open C_content in
      compiled := true;
      if
        List.exists !C_content.entries ~f:(function
          | Header _ | Extract_header _ | Function _ -> false
          | Extract_source _ -> true)
      then
        let source = C_content.get_extract_source () in
        let ebuf = Buffer.create 1024 in
        match Extract_c.compile ~ebuf source with
        | Ok obj ->
          if Buffer.length ebuf > 0 then Buffer.output_buffer stderr ebuf;
          List.iter (List.rev !C_content.entries) ~f:(function
            | Header _ | Extract_header _ | Function _ -> ()
            | Extract_source (_, _, _, f) -> f obj)
        | Error _ -> find_failing ebuf)

  let extract_single_einfo id =
    extract_all ();
    match Hashtbl.find htl_id_entries id with
    | exception Not_found -> None
    | x -> Some x

  let extract_single id =
    match extract_single_einfo id with None -> None | Some (_, x) -> Some x
end

module Const_phase0 = struct
  include Const_common (C_content_phase0)

  let f_er loc x =
    match x with
    | Extract_c.Info_not_found ->
      U.error ~loc "constant extraction failed for unknown reasons"
    | Extract_c.Overflow s -> U.error ~loc "number too large:%s" s
    | Extract_c.Underflow s -> U.error ~loc "number too small:%s" s
    | Extract_c.Not_an_integer -> U.error ~loc "constant is not an integer"

  let add ~info_str id loc str x =
    assert (!compiled = false);
    let ri, s1, s2 = Extract_c.prepare_extract_int ~loc x str in
    let exn = U.error_exn "%s" info_str in
    if s1 <> "" then C_content_phase0.add_extract_header s1 loc exn;
    let h s = Hashtbl.replace htl_id_entries id (`Int_no_expr, s) in
    let f obj =
      match Extract_c.extract ri obj with Ok s -> h s | Error x -> f_er loc x
    in
    if Options.(!mode = Emulate) then h "1"
    else C_content_phase0.add_extract_source s2 loc exn f
end

module C_content = C_content_make ()

module Const = struct
  include Const_common (C_content)

  let expr_exn loc s = function
    | Extract_c_ml.Overflow -> U.error ~loc "number too large: %s" s
    | Extract_c_ml.Underflow -> U.error ~loc "number too small: %s" s
    | Extract_c_ml.Expr x -> x

  let add ~info_str id loc x str =
    let (rinfo, s1, s2), einfo =
      match x with
      | `Any t -> (
        match Extract_c_ml.prepare t with
        | None ->
          (* TODO: fixme. Special case for strings? *)
          if t != Obj.magic Ctypes.string then
            U.error "can't extract constants of type %s"
              (Ctypes.string_of_typ t);
          (Extract_c.prepare_extract_string ~loc str, `String)
        | Some einf ->
          let { Extract_c_ml.ctype; _ } = einf in
          (Extract_c.prepare_extract_int ~loc (`Int_type ctype) str, `Int einf))
      | `U8 | `U32 | `U32_to_expr | `A64 ->
        let p =
          match x with
          | `U8 -> `Unchecked_U8
          | `U32 | `U32_to_expr -> `Unchecked_U32
          | `A64 -> `Any_int
          | `Any _ -> assert false
        in
        (Extract_c.prepare_extract_int ~loc p str, `Int_no_expr)
    in
    let exn = U.error_exn "Failed to extract %s" info_str in
    if s1 <> "" then C_content.add_extract_header s1 loc exn;
    let h s =
      Hashtbl.replace htl_id_entries id (einfo, s);
      match x with
      | `U32_to_expr ->
        let i64 = Int64.of_string s in
        if i64 > Int64.of_int max_int then U.error ~loc "number too large: %s" s;
        let e = U.int_expr ~loc (Int64.to_int i64) in
        Hashtbl.replace G.htl_expr id e
      | `U8 | `U32 | `A64 | `Any _ -> ()
    in
    let f obj =
      match Extract_c.extract rinfo obj with
      | Ok s -> h s
      | Error x -> Const_phase0.f_er loc x
    in
    if Options.(!mode = Emulate) then h "1"
    else C_content.add_extract_source s2 loc exn f
end

module Trace = struct
  type typ

  type fn

  let typs_struct : (typ * int) list ref = ref []

  let typs_union : (typ * int) list ref = ref []

  let typs_view : (typ * int) list ref = ref []

  let typs_pointer : (typ * int) list ref = ref []

  let typs_funptr : (typ * int) list ref = ref []

  let typs_array : (typ * int) list ref = ref []

  let typs_bigarray : (typ * int) list ref = ref []

  let typs_ocaml : (typ * int) list ref = ref []

  let typs_prim : (typ * int) list ref = ref []

  let typs_void : (typ * int) list ref = ref []

  let typs_abstract : (typ * int) list ref = ref []

  let fns : (fn * int) list ref = ref []

  let clear () =
    fns := [];
    typs_struct := [];
    typs_union := [];
    typs_view := [];
    typs_pointer := [];
    typs_funptr := [];
    typs_array := [];
    typs_bigarray := [];
    typs_ocaml := [];
    typs_prim := [];
    typs_void := [];
    typs_abstract := []

  let register_type : type a. int -> bool -> a Ctypes.typ -> a Ctypes.typ =
    let open Ctypes_static in
    fun id no_dup t ->
      let dup : type b. b Ctypes.typ -> b Ctypes.typ = function
        | Struct x -> Struct x
        | Union x -> Union x
        | View x -> View x
        | Pointer y -> Pointer y
        | Funptr x -> Funptr x
        | Array (a, b) -> Array (a, b)
        | Bigarray x -> Bigarray x
        | OCaml x -> OCaml x
        | Primitive x -> Primitive x
        | Void -> Void
        | Abstract x -> Abstract x
      in
      let t = if no_dup then t else dup t in
      let x =
        match t with
        | Struct _ -> typs_struct
        | Union _ -> typs_union
        | View _ -> typs_view
        | Pointer _ -> typs_pointer
        | Funptr _ -> typs_funptr
        | Array _ -> typs_array
        | Bigarray _ -> typs_bigarray
        | OCaml _ -> typs_ocaml
        | Primitive _ -> typs_prim
        | Void -> typs_void
        | Abstract _ -> typs_abstract
      in
      x := (Obj.magic t, id) :: !x;
      t

  let register_fn id (t : 'a Ctypes.fn) =
    let t = Obj.magic t in
    fns := (t, id) :: !fns

  let rec trace : type a. a Ctypes.typ -> unit =
    let open Ctypes_static in
    fun cur ->
      let h l =
        let cur = Obj.magic cur in
        let r =
          List.find_map l ~f:(fun (x, id) -> if x == cur then Some id else None)
        in
        match r with None -> () | Some id -> Hashtbl.replace G.htl_used id ()
      in
      match cur with
      | Struct _ -> h !typs_struct
      | Union _ -> h !typs_union
      | View { ty; _ } ->
        h !typs_view;
        trace ty
      | Pointer ty ->
        h !typs_pointer;
        trace ty
      | Funptr fn ->
        h !typs_funptr;
        trace_fn fn
      | Array (ty, _) ->
        h !typs_array;
        trace ty
      | Bigarray _ -> h !typs_bigarray
      | OCaml _ -> h !typs_ocaml
      | Primitive _ -> h !typs_prim
      | Void -> h !typs_void
      | Abstract _ -> h !typs_abstract

  and trace_fn : type b. b Ctypes.fn -> unit =
    let open Ctypes_static in
    fun cur ->
      let t = Obj.magic cur in
      let r =
        List.find_map !fns ~f:(fun (x, id) -> if x == t then Some id else None)
      in
      (match r with None -> () | Some id -> Hashtbl.replace G.htl_used id ());
      match cur with
      | Returns a -> trace a
      | Function (a, b) ->
        trace a;
        trace_fn b
end

module type Opaque_result = sig
  type t

  val t : t Ctypes.typ
end

module Int_alias = struct
  let is_set r x = r land (1 lsl x) <> 0

  let int ?(strict = false) ctyp r : (module Ppx_cstubs.Types.Signed) * 'a * 'b
      =
    let loc = !Ast_helper.default_loc in
    let emulate = Options.(!mode = Emulate) in
    if not emulate then (
      if is_set r 28 = false then U.error "type %s is not an integer type" ctyp;
      if is_set r 29 then U.error "type %s is unsigned" ctyp);
    let g (type a) (t : a Ctypes.typ) (m : (module Signed.S with type t = a)) =
      (module struct
        include (val m)

        let t = Ctypes.typedef t ctyp
      end : Ppx_cstubs.Types.Signed)
    in
    let f : _ -> (module Ppx_cstubs.Types.Signed) * expression * string =
      function
      | `Int8 ->
        ( g Ctypes.int8_t (module Internals.Signed.Int8),
          [%expr Ctypes.int8_t],
          "Ppx_cstubs.Ppx_cstubs_internals.Signed.Int8" )
      | `Int16 ->
        ( g Ctypes.int16_t (module Internals.Signed.Int16),
          [%expr Ctypes.int16_t],
          "Ppx_cstubs.Ppx_cstubs_internals.Signed.Int16" )
      | `Int32 ->
        ( g Ctypes.int32_t (module Signed.Int32),
          [%expr Ctypes.int32_t],
          "Signed.Int32" )
      | `Int64 ->
        ( g Ctypes.int64_t (module Signed.Int64),
          [%expr Ctypes.int64_t],
          "Signed.Int64" )
      | `Intnat ->
        ( g Ctypes.nativeint (module Internals.Signed.Nativeint),
          [%expr Ctypes.nativeint],
          "Ppx_cstubs.Ppx_cstubs_internals.Signed.Nativeint" )
      | `Schar ->
        ( g Ctypes.schar (module Internals.Signed.Schar),
          [%expr Ctypes.schar],
          "Ppx_cstubs.Ppx_cstubs_internals.Signed.Schar" )
      | `Short ->
        ( g Ctypes.short (module Internals.Signed.Short),
          [%expr Ctypes.short],
          "Ppx_cstubs.Ppx_cstubs_internals.Signed.Short" )
      | `Int ->
        ( g Ctypes.int (module Internals.Signed.Int32),
          [%expr Ctypes.int],
          "Ppx_cstubs.Ppx_cstubs_internals.Signed.Int32" )
      | `Sint ->
        (g Ctypes.sint (module Signed.SInt), [%expr Ctypes.sint], "Signed.SInt")
      | `Long ->
        (g Ctypes.long (module Signed.Long), [%expr Ctypes.long], "Signed.Long")
      | `Long_long ->
        ( g Ctypes.llong (module Signed.LLong),
          [%expr Ctypes.llong],
          "Signed.LLong" )
    in
    let ws64 = Ocaml_config.word_size () = 64 in
    if emulate then f `Int
    else if is_set r 23 then f `Intnat
    else if is_set r 15 then f `Int8
    else if is_set r 16 then f `Int16
    else if ws64 && is_set r 8 && is_set r 3 then f `Int
    else if strict = false && ws64 && is_set r 0 && is_set r 3 then f `Int
    else if is_set r 17 then f `Int32
    else if is_set r 18 then f `Int64
    else if strict = false && is_set r 1 then f `Int8
    else if strict = false && is_set r 2 then f `Int16
    else if strict = false && is_set r 3 then f `Int32
    else if strict = false && is_set r 4 then f `Int64
    else if is_set r 5 then f `Schar
    else if is_set r 7 then f `Short
    else if is_set r 8 then f `Sint
    else if is_set r 9 then f `Long
    else if is_set r 10 then f `Long_long
    else if ws64 && is_set r 0 && is_set r 3 then f `Int
    else if is_set r 1 then f `Int8
    else if is_set r 2 then f `Int16
    else if is_set r 3 then f `Int32
    else if is_set r 4 then f `Int64
    else U.error "no suitable integer type found for %s" ctyp

  let uint ?strict ctyp r :
      (module Ppx_cstubs.Types.Unkown_signedness) * 'a * 'b =
    (* strict is not used yet, but it might be in the future, when better
       representations of uints are available *)
    ignore (strict : bool option);
    let loc = !Ast_helper.default_loc in
    let emulate = Options.(!mode = Emulate) in
    if not emulate then (
      if is_set r 28 = false then U.error "type %s is not an integer type" ctyp;
      if is_set r 29 = false then U.error "type %s is signed" ctyp);
    let g (type a) (t : a Ctypes.typ) (m : (module Unsigned.S with type t = a))
        =
      (module struct
        include (val m)

        let min_int = zero

        let t = Ctypes.typedef t ctyp
      end : Ppx_cstubs.Types.Unkown_signedness)
    in
    let f = function
      | `Uint8 ->
        ( g Ctypes.uint8_t (module Unsigned.UInt8),
          [%expr Ctypes.uint8_t],
          "Unsigned.UInt8" )
      | `Uint16 ->
        ( g Ctypes.uint16_t (module Unsigned.UInt16),
          [%expr Ctypes.uint16_t],
          "Unsigned.UInt16" )
      | `Uint32 ->
        ( g Ctypes.uint32_t (module Unsigned.UInt32),
          [%expr Ctypes.uint32_t],
          "Unsigned.UInt32" )
      | `Uint64 ->
        ( g Ctypes.uint64_t (module Unsigned.UInt64),
          [%expr Ctypes.uint64_t],
          "Unsigned.UInt64" )
      | `Uchar ->
        ( g Ctypes.uchar (module Unsigned.UChar),
          [%expr Ctypes.uchar],
          "Unsigned.UChar" )
      | `Ushort ->
        ( g Ctypes.ushort (module Unsigned.UShort),
          [%expr Ctypes.ushort],
          "Unsigned.UShort" )
      | `Uint ->
        ( g Ctypes.uint (module Unsigned.UInt),
          [%expr Ctypes.uint],
          "Unsigned.UInt" )
      | `Ulong ->
        ( g Ctypes.ulong (module Unsigned.ULong),
          [%expr Ctypes.ulong],
          "Unsigned.ULong" )
      | `Ullong ->
        ( g Ctypes.ullong (module Unsigned.ULLong),
          [%expr Ctypes.ullong],
          "Unsigned.ULLong" )
    in
    if emulate then f `Uint
    else if is_set r 19 then f `Uint8
    else if is_set r 20 then f `Uint16
    else if is_set r 21 then f `Uint32
    else if is_set r 22 then f `Uint64
    else if is_set r 6 then f `Uchar
    else if is_set r 11 then f `Ushort
    else if is_set r 12 then f `Uint
    else if is_set r 13 then f `Ulong
    else if is_set r 14 then f `Ullong
    else if is_set r 0 then f `Uint
    else if is_set r 1 then f `Uint8
    else if is_set r 2 then f `Uint16
    else if is_set r 3 then f `Uint32
    else if is_set r 4 then f `Uint64
    else U.error "no suitable integer type found for %s" ctyp

  let id_r id_loc ctyp =
    let id, _ = U.from_id_loc_param id_loc in
    let r =
      match Const_phase0.extract_single id with
      | None -> U.error "Can't find type info for %S\n" ctyp
      | Some r -> int_of_string r
    in
    (id, r)
end

module Opaque_try_compile = struct
  let sources = Hashtbl.create 8

  let save_cur_headers (id : int) =
    C_content.get_cur_headers () |> Hashtbl.replace sources id

  let does_compile id code =
    let src =
      try Hashtbl.find sources id
      with Not_found -> U.error "fatal: code fragment not found"
    in
    let src = List.rev (code :: src) |> String.concat "\n" in
    let stderr = if !Options.verbosity > 2 then `Stderr else `Null in
    let stdout = if !Options.verbosity > 2 then `Stdout else `Null in
    C_compile.compile ~stdout ~stderr src (fun ec _ -> ec = 0)

  let clear () = Hashtbl.clear sources
end

module Extract_phase0 = struct
  let bound_constant id_loc str =
    let id, loc = U.from_id_loc_param id_loc in
    let info_str = Printf.sprintf "Failed to extract constant %S" str in
    Const_phase0.add ~info_str id loc str `Any_int

  let header (s : string) =
    C_content_phase0.add_header s !Ast_helper.default_loc

  let int_alias id_loc ctyp =
    let id, loc = U.from_id_loc_param id_loc in
    let ntyp = U.safe_cname ~prefix:ctyp in
    let stru = U.safe_cname ~prefix:ctyp in
    let expl = U.safe_cname ~prefix:ctyp in
    let expl2 = U.safe_cname ~prefix:ctyp in
    let cloc = U.cloc_comment loc in
    let txt =
      Printf.sprintf
        {|%s
typedef %s %s; /* does the type exist at all? */
struct %s {char c; %s x;};
#if !defined(__cplusplus)
%s %s = 0; /* trigger error for non scalar types */
#if defined(PPXC_NO_PROPER_INTEGER_TEST) && !defined(__cplusplus)
%s %s = ~((%s)0); /* enforce error for doubles */
#endif
#else
%s %s = static_cast<%s>(0);
#endif
|}
        cloc ctyp ntyp stru ntyp ntyp expl ntyp expl2 ntyp ntyp expl ntyp
    in
    let exn = U.error_exn "%s not defined or not an integer?" ctyp in
    let to_extract = Printf.sprintf "PPXC_INTALIAS(%s,%s,%s)" ntyp stru expl in
    let info_str = Printf.sprintf "type info %s" ctyp in
    C_content_phase0.add_extract_header txt loc exn;
    Const_phase0.add ~info_str id loc to_extract `Unchecked_U32
end

(* Sring comparison relies on Extract_c.normalise_int *)
let typed_int_of_string ~verify t res =
  let ef t =
    U.error "constants of type %s not possible" (Ctypes.string_of_typ t)
  in
  let er () = U.error "%s out of range" res in
  let int64_of_string res =
    try Int64.of_string res
    with Failure _ -> (
      try Unsigned.UInt64.of_string res |> Unsigned.UInt64.to_int64
      with Failure _ -> er ())
  in
  let int () =
    if not verify then int64_of_string res |> Int64.to_int
    else
      let r = try int_of_string res with Failure _ -> er () in
      if string_of_int r <> res then er ();
      r
  in
  let f_i : type a. (module Signed.S with type t = a) -> a =
   fun m ->
    let module S = (val m) in
    if not verify then int64_of_string res |> S.of_int64
    else
      let r = try S.of_string res with Failure _ -> er () in
      if S.to_string r <> res then er ();
      r
  in
  let f_u : type a. (module Unsigned.S with type t = a) -> a =
   fun m ->
    let module S = (val m) in
    let r = S.of_string res in
    if verify && S.to_string r <> res then er ();
    r
  in
  let rec f : type a. a Ctypes.typ -> a =
   fun t ->
    let module CS = Ctypes_static in
    match t with
    | CS.Void -> ef t
    | CS.Struct _ -> ef t
    | CS.Union _ -> ef t
    | CS.Array _ -> ef t
    | CS.Bigarray _ -> ef t
    | CS.Abstract _ -> ef t
    | CS.Pointer _ -> ef t
    | CS.Funptr _ -> ef t
    | CS.OCaml _ -> ef t
    | CS.View { ty; read; write; _ } ->
      let a = f ty in
      let r = read (f ty) in
      if verify && write r <> a then er ();
      r
    | CS.Primitive p -> (
      let module Cp = Ctypes_primitive_types in
      match p with
      | Cp.Schar ->
        let r = int () in
        if verify && (r < -128 || r > 127) then er ();
        r
      | Cp.Char ->
        let r = int () in
        if verify && (r < -128 || r > 255) then er ();
        Char.chr ((r + 256) mod 256)
      | Cp.Uchar -> f_u (module Unsigned.UChar)
      | Cp.Bool ->
        let r = int () in
        if verify && r <> 0 && r <> 1 then er ();
        r <> 0
      | Cp.Short -> int ()
      | Cp.Int -> int ()
      | Cp.Long -> f_i (module Signed.Long)
      | Cp.Llong -> f_i (module Signed.LLong)
      | Cp.Ushort -> f_u (module Unsigned.UShort)
      | Cp.Sint -> f_i (module Signed.SInt)
      | Cp.Uint -> f_u (module Unsigned.UInt)
      | Cp.Ulong -> f_u (module Unsigned.ULong)
      | Cp.Ullong -> f_u (module Unsigned.ULLong)
      | Cp.Size_t -> f_u (module Unsigned.Size_t)
      | Cp.Int8_t -> int ()
      | Cp.Int16_t -> int ()
      | Cp.Int32_t -> f_i (module Signed.Int32)
      | Cp.Int64_t -> int64_of_string res
      | Cp.Uint8_t -> f_u (module Unsigned.UInt8)
      | Cp.Uint16_t -> f_u (module Unsigned.UInt16)
      | Cp.Uint32_t -> f_u (module Unsigned.UInt32)
      | Cp.Uint64_t -> f_u (module Unsigned.UInt64)
      | Cp.Camlint -> int ()
      | Cp.Nativeint ->
        let r = int64_of_string res |> Int64.to_nativeint in
        if verify && Nativeint.to_string r <> res then er ();
        r
      | Cp.Float -> ef t
      | Cp.Double -> ef t
      | Cp.LDouble -> ef t
      | Cp.Complex32 -> ef t
      | Cp.Complex64 -> ef t
      | Cp.Complexld -> ef t)
  in
  f t

module Extract = struct
  let constant id_loc str _ t =
    let id, loc = U.from_id_loc_param id_loc in
    let info_str = Printf.sprintf "constant %S" str in
    Const.add ~info_str id loc (`Any t) str

  let bound_constant id_loc str t_expr t =
    let id, loc = U.from_id_loc_param id_loc in
    let var = Std.Util.safe_cname ~prefix:"type_defined" in
    let cloc = U.cloc_comment loc in
    let orig_ctype =
      match Extract_c_ml.prepare t with
      | None ->
        U.error "can't extract constants of type %s" (Ctypes.string_of_typ t)
      | Some x -> x.Extract_c_ml.ctype
    in
    let ctype = U.safe_cname ~prefix:orig_ctype in
    let eheader =
      Printf.sprintf {|%s
typedef %s %s;
%s %s = %s;
|} cloc orig_ctype ctype
        ctype var str
    in
    let exn = U.error_exn "%s not defined" ctype in
    C_content.add_extract_header eheader loc exn;
    let min =
      Printf.sprintf "( (%s) >= 0 || (%s) >= (PPXC_MIN_TYPE(%s)) )" str str
        ctype
    in
    let max =
      Printf.sprintf "( (%s) <= 0 || (%s) <= (PPXC_MAX_TYPE(%s)) )" str str
        ctype
    in
    let extr =
      Printf.sprintf "((((unsigned)(%s)) << 0u) | (((unsigned)(%s)) << 1u))" min
        max
    in
    let info_str = Printf.sprintf "constant %S" str in
    Const.add ~info_str id loc `U8 extr;
    let er () = U.error "constant %S not found" str in
    match Const_phase0.extract_single id with
    | None -> er ()
    | Some res ->
      let t_expr = Marshal.from_string t_expr 0 in
      (match Extract_c_ml.gen_ext t res t_expr with
      | None -> er ()
      | Some x -> Const.expr_exn loc res x |> Hashtbl.replace G.htl_expr id);
      typed_int_of_string ~verify:false t res

  let register_fun_place id_loc_external =
    let id_external, _ = U.from_id_loc_param id_loc_external in
    C_content.add_function_place id_external

  let header (s : string) = C_content.add_header s !Ast_helper.default_loc

  let field id_loc stru field_name field_type =
    let id, loc = U.from_id_loc_param id_loc in
    let ctyp = Gen_c.string_of_typ_exn stru in
    let var1 = U.safe_cname ~prefix:ctyp in
    let var1_typed = Gen_c.string_of_typ_exn ~name:var1 stru in
    let var2 = U.safe_cname ~prefix:field_name in
    let var2_typed = Gen_c.string_of_typ_exn ~name:var2 field_type in
    let param = U.safe_cname ~prefix:ctyp in
    let param_typed = Gen_c.string_of_typ_exn ~name:param (Ctypes.ptr stru) in
    let fun_name = U.safe_cname ~prefix:ctyp in
    let com_loc = U.cloc_comment loc in
    let rec iter : type a. a Ctypes.typ -> _ =
      let open Ctypes_static in
      function
      | Struct x when x.fields = [] -> `Struct_empty
      | Struct _ -> `Struct
      | Union x when x.ufields = [] -> `Union_empty
      | Union _ -> `Union
      | View { ty; format_typ = Some _; _ } -> (
        match iter ty with
        | `Struct_empty -> `Typedef_struct_empty
        | `Union_empty -> `Typedef_union_empty
        | x -> x)
      | View { ty; _ } -> iter ty
      | Pointer _ -> assert false
      | Funptr _ -> assert false
      | Array _ -> assert false
      | Bigarray _ -> assert false
      | OCaml _ -> assert false
      | Primitive _ -> assert false
      | Void -> assert false
      | Abstract _ -> assert false
    in
    let x = iter stru in
    (match x with
    | `Typedef_struct_empty | `Typedef_union_empty ->
      let t, ass =
        if x = `Typedef_struct_empty then ("struct", "PPXC_ASSERT_STRUCT")
        else ("union", "PPXC_ASSERT_UNION")
      in
      let msg1 = Printf.sprintf "%s is not a %s" ctyp t in
      let msg2 =
        CCString.replace ~which:`All ~sub:" " ~by:"_" msg1 |> U.safe_ascii_only
      in
      let msg1 = String.concat "" [ "\""; msg1; "\"" ] in
      let ctyp_tp = U.safe_cname ~prefix:ctyp in
      let txt =
        Printf.sprintf
          {|
%s
#ifndef __cplusplus
  DISABLE_STRUCT_WARNINGS_PUSH()
  %s = {0};
  DISABLE_STRUCT_WARNINGS_POP()
#else
  %s;
#endif
  typedef %s %s;
  %s(%s,%s,%s,%s)
|}
          com_loc var1_typed var1_typed ctyp ctyp_tp ass ctyp_tp var1 msg1 msg2
      in
      let e = U.error_exn "%s" msg1 in
      C_content.add_extract_header txt loc e
    | `Struct_empty | `Union_empty ->
      let txt =
        Printf.sprintf
          {|
%s
#ifndef __cplusplus
  DISABLE_STRUCT_WARNINGS_PUSH()
  %s = {0};
  DISABLE_STRUCT_WARNINGS_POP()
#else
  %s;
#endif
|}
          com_loc var1_typed var1_typed
      in
      let e =
        let t = if x = `Struct_empty then "struct" else "union" in
        U.error_exn "%s is not a %s" ctyp t
      in
      C_content.add_extract_header txt loc e
    | `Struct | `Union ->
      let txt = Printf.sprintf "%s\n%s;\n" com_loc var1_typed in
      let e = U.error_exn "%s unavailable" ctyp in
      C_content.add_extract_header txt loc e);
    let txt =
      Printf.sprintf
        {|
DISABLE_CONST_WARNINGS_PUSH()
%s;
void %s (%s) {
  /* just to trigger errors and warnings */
#if defined(PPXC_HAS_TYPEOF) && !defined(__cplusplus)
  __typeof__(%s->%s) ppx__var;
  memcpy((void*)&ppx__var, &(%s->%s), sizeof(ppx__var));
  (void) (&ppx__var == &%s); /* field %s in %s incompatible? */
#else
  memcpy(&%s,&(%s->%s),sizeof(%s));
#endif
}
DISABLE_CONST_WARNINGS_POP()
|}
        var2_typed fun_name param_typed param field_name param field_name var2
        (U.no_c_comments field_name)
        (U.no_c_comments ctyp) var2 param field_name var2
    in
    let e = U.error_exn "field %s in %s not available?" field_name ctyp in
    C_content.add_extract_header txt loc e;
    let to_extract =
      Printf.sprintf
        "((PPXC_OFFSETOF(%s,%s))|(PPXC_TYPES_COMPATIBLE(%s.%s,%s) << 29u))" ctyp
        field_name var1 field_name var2
    in
    let info_str = Printf.sprintf "field %s in %s" field_name ctyp in
    Const.add ~info_str id loc `U32 to_extract;
    Ctypes.field stru field_name field_type

  let size_align id_loc_size id_loc_align x ctyp =
    let id_size, loc_size = U.from_id_loc_param id_loc_size in
    let id_align, loc_align = U.from_id_loc_param id_loc_align in
    let size_str = Printf.sprintf "sizeof(%s)" ctyp in
    let struct_name = U.safe_cname ~prefix:"extract_struct" in
    let cloc = U.cloc_comment loc_size in
    let txt =
      Printf.sprintf "%s\nstruct %s {char c; %s x;};\n" cloc struct_name ctyp
    in
    let exn = U.error_exn "%s not defined?" ctyp in
    C_content.add_extract_header txt loc_size exn;
    let align_str = Printf.sprintf "PPXC_OFFSETOF(struct %s,x)" struct_name in
    Const.add ~info_str:("size of " ^ ctyp) id_size loc_size x size_str;
    Const.add ~info_str:("align of " ^ ctyp) id_align loc_align x align_str;
    struct_name

  let seal id_loc_size id_loc_align stru =
    Ctypes.seal stru;
    let ctyp = Gen_c.string_of_typ_exn stru in
    ignore (size_align id_loc_size id_loc_align `U32_to_expr ctyp : string)

  let get_enum_id = function
    | Marshal_types.E_normal x
    | Marshal_types.E_bitmask x
    | Marshal_types.E_normal_bitmask (x, _) ->
      x

  let enum_fake_view (l : 'a list) (typedef : bool) (name : string)
      (typ : 'b Ctypes.typ) : 'a Ctypes.typ * 'a list Ctypes.typ =
    let () = ignore (l : 'a list) in
    (* just to type the function *)
    let typedef = if typedef then "" else "enum " in
    let format_typ k fmt = Format.fprintf fmt "%s%s%t" typedef name k in
    let write _ = assert false in
    let read _ = assert false in
    ( Ctypes.view ~read ~write ~format_typ typ,
      Ctypes.view ~read ~write ~format_typ typ )

  let enum (l : 'a list) (p : string) : 'a Ctypes.typ * 'a list Ctypes.typ =
    let open Marshal_types in
    let {
      enum_l;
      enum_name;
      enum_is_typedef;
      enum_loc;
      enum_type_id;
      enum_is_int_bitmask = _;
      enum_unexpected = _;
      enum_unexpected_bits = _;
    } =
      Marshal.from_string p 0
    in
    U.with_loc enum_loc @@ fun () ->
    let info_str = "enum_type " ^ enum_name in
    let orig_type_str =
      match enum_is_typedef with
      | false -> String.concat " " [ "enum"; enum_name ]
      | true -> enum_name
    in
    let type_str = U.safe_cname ~prefix:enum_name in
    let evar = U.safe_cname ~prefix:enum_name in
    let cloc = U.cloc_comment enum_loc in
    let txt =
      Printf.sprintf
        {|%s
typedef %s %s; /* type %s defined? */
#if defined(PPXC_NO_PROPER_INTEGER_TEST) && !defined(__cplusplus)
%s %s = ~((%s)0); /* enforce error for doubles */
#endif
|}
        cloc orig_type_str type_str
        (U.no_c_comments enum_name)
        type_str evar type_str
    in
    let exn = U.error_exn ~loc:enum_loc "type %s not defined?" enum_name in
    C_content.add_extract_header txt enum_loc exn;
    List.iter enum_l ~f:(fun c ->
        U.with_loc c.ee_loc @@ fun () ->
        let cname = c.ee_cname in
        let evar1 = U.safe_cname ~prefix:cname in
        let evar2 = U.safe_cname ~prefix:("char_" ^ cname) in
        let evar3 = U.safe_cname ~prefix:cname in
        let cloc = U.cloc_comment c.ee_loc in
        let txt =
          Printf.sprintf
            {|
%s
%s %s = %s; /* trigger errors */
char %s[2] = { ((char)((%s) > 1)), '\0' };  /* %s not a constant expression? */
#if defined(PPXC_NO_PROPER_INTEGER_TEST) && !defined(__cplusplus)
%s %s = ~(%s); /* enforce error for doubles */
#endif
|}
            cloc type_str evar1 cname evar2 cname (U.no_c_comments cname)
            type_str evar3 cname
        in
        let exn = U.error_exn "enum %s not defined?" cname in
        C_content.add_extract_header txt c.ee_loc exn;
        let info_str = "enum constant " ^ cname in
        Const.add ~info_str c.ee_int_id c.ee_loc `A64 cname;
        let info_str = "type of " ^ c.ee_cname in
        Printf.sprintf "PPXC_ENUM_MEMBER_CHECK(%s,%s)" cname type_str
        |> Const.add ~info_str c.ee_type_check c.ee_loc `U8);
    let str = "PPXC_CTYPES_ARITHMETIC_TYPEINFO(" ^ type_str ^ ")" in
    let eid = get_enum_id enum_type_id in
    Const.add ~info_str eid enum_loc `U32 str;
    (* wrong size, but not used inside extraction script *)
    enum_fake_view l enum_is_typedef enum_name Ctypes.int32_t

  let int_alias id_loc ~mod_name:_ ?strict ctyp :
      (module Ppx_cstubs.Types.Signed) =
    let _, r = Int_alias.id_r id_loc ctyp in
    let md, _, _ = Int_alias.int ?strict ctyp r in
    md

  let uint_alias id_loc ~mod_name:_ ?strict ctyp :
      (module Ppx_cstubs.Types.Unsigned) =
    let _, r = Int_alias.id_r id_loc ctyp in
    let md, _, _ = Int_alias.uint ?strict ctyp r in
    (module (val md))

  let aint_alias id_loc ~mod_name:_ ?strict ctyp :
      (module Ppx_cstubs.Types.Unkown_signedness) =
    let _, r = Int_alias.id_r id_loc ctyp in
    if Int_alias.is_set r 29 then
      let md, _, _ = Int_alias.uint ?strict ctyp r in
      md
    else
      let md, _, _ = Int_alias.int ?strict ctyp r in
      (module (val md))

  let common_abstract ~size ~align x ctyp =
    let _, loc = U.from_id_loc_param size in
    let cloc = U.cloc_comment loc in
    let ntyp = U.safe_cname ~prefix:ctyp in
    let var = U.safe_cname ~prefix:ctyp in
    let txt =
      Printf.sprintf {|%s
typedef %s %s;
%s %s;
|} cloc ctyp ntyp ntyp var
    in
    let exn = U.error_exn "%s not defined?" ctyp in
    C_content.add_extract_header txt loc exn;
    let stru' = size_align size align x ctyp in
    (stru', var, ntyp)

  let opaque ~size ~align ~typ ~mi:_ ctyp : (module Opaque_result) =
    let stru, var, ntyp = common_abstract ~size ~align `U32 ctyp in
    let id, loc = U.from_id_loc_param typ in
    let te = Printf.sprintf "PPXC_OPAQUE(%s, %s, %s)" ntyp stru var in
    let info_str = Printf.sprintf "type info %s" ctyp in
    Const.add ~info_str id loc `U32 te;
    Opaque_try_compile.save_cur_headers id;
    (module struct
      type t = [ `a ] Ctypes_static.abstract

      let t =
        Ctypes.view ~read:Internals.identity ~write:Internals.identity
          (Ctypes.abstract ~name:ctyp ~size:1 ~alignment:1)
    end)

  let abstract ~size ~align ctyp =
    ignore
      (common_abstract ~size ~align `U32_to_expr ctyp
        : string * string * string);
    Ctypes.abstract ~name:ctyp ~size:1 ~alignment:1

  let ocaml_funptr _ _ = () (* just to trigger errors early *)
end

module Build = struct
  let reg_trace_fn id t =
    Trace.register_fn id t;
    t

  let reg_trace ?(no_dup = false) id t = Trace.register_type id no_dup t

  let extract_single_str id = Const.extract_single id

  let extract_single_int op id =
    match extract_single_str id with
    | Some x -> ( try Some (op x) with Failure _ -> None)
    | None -> None

  let extract_single_int32 = extract_single_int Int32.of_string

  let extract_single_int = extract_single_int int_of_string

  let constant id_loc str t_expr t =
    let id, loc = U.from_id_loc_param id_loc in
    let fail () = U.error ~loc "couldn't extract constant %S" str in
    let t_expr = Marshal.from_string t_expr 0 in
    Trace.trace t;
    let einfo_old, res =
      match Const.extract_single_einfo id with Some x -> x | None -> fail ()
    in
    match (einfo_old, Extract_c_ml.prepare t) with
    | `String, None ->
      if t != Obj.magic Ctypes.string then fail ();
      U.str_expr res |> Hashtbl.replace G.htl_expr id
    | `Int x_old, Some x_new -> (
      if x_new.Extract_c_ml.ctype <> x_old.Extract_c_ml.ctype then fail ();
      match Extract_c_ml.gen_ext t res t_expr with
      | None -> fail ()
      | Some x -> Const.expr_exn loc res x |> Hashtbl.replace G.htl_expr id)
    | `Int_no_expr, _ | `String, Some _ | `Int _, None -> fail ()

  let is_set = Int_alias.is_set

  let bound_constant id_loc str _ t =
    let id, loc = U.from_id_loc_param id_loc in
    Trace.trace t;
    let res, ck =
      match (Const_phase0.extract_single id, Const.extract_single id) with
      | None, Some _ | Some _, None | None, None ->
        U.error ~loc "couldn't extract constant %S" str
      | Some a, Some b -> (a, int_of_string b)
    in
    if Options.(!mode <> Emulate) then (
      if not (is_set ck 0) then Const_phase0.f_er loc (Extract_c.Underflow res);
      if not (is_set ck 1) then Const_phase0.f_er loc (Extract_c.Overflow res));
    typed_int_of_string ~verify:true t res

  let foreign_value id_loc_external id_loc_stri_expr ctypes_t foreign_value name
      typ_expr =
    let id_external, _loc_external = U.from_id_loc_param id_loc_external in
    let id_stri_expr, _loc_stri_expr = U.from_id_loc_param id_loc_stri_expr in
    let typ_expr = U.unmarshal_expr typ_expr in
    Trace.trace ctypes_t;
    let fun' =
      Internals.Shadow.(Ctypes.void @-> returning (Ctypes.ptr ctypes_t))
    in
    let loc = !Ast_helper.default_loc in
    let ptrexpr = [%expr Ctypes.ptr [%e typ_expr]] in
    let stubname = U.safe_cname ~prefix:("value_" ^ foreign_value) in
    let cinfo = Gen_c.gen_value fun' ~stubname ~value:foreign_value in
    C_content.add_function id_external cinfo.Gen_c.stub_source;
    let res = Gen_ml.foreign_value fun' name ptrexpr cinfo in
    Hashtbl.replace G.htl_stri id_external res.Gen_ml.extern;
    Hashtbl.replace G.htl_stri id_stri_expr res.Gen_ml.intern

  let external' id_loc_external id_loc_stri_expr ctypes_fn ~marshal_info =
    let id_external, _loc_external = U.from_id_loc_param id_loc_external in
    let id_stri_expr, _loc_stri_expr = U.from_id_loc_param id_loc_stri_expr in
    let {
      Marshal_types.el;
      ret;
      release_runtime_lock;
      noalloc;
      is_inline;
      remove_labels;
      return_errno;
      c_name;
      prim_name;
      uniq_ref_id;
    } =
      Marshal.from_string marshal_info 0
    in
    let locs =
      let module P = Parsetree in
      List.rev
        (ret.P.pexp_loc :: List.rev_map el ~f:(fun (_, b) -> b.P.pexp_loc))
    in
    let ocaml_name = Uniq_ref.get_final_name uniq_ref_id in
    let c =
      if is_inline = false then
        let stubname = U.safe_cname ~prefix:c_name in
        Gen_c.gen_fun ctypes_fn ~locs ~stubname ~cfunc:c_name
          ~release_runtime_lock ~noalloc ~return_errno
      else
        let c_body = c_name in
        let prim_name = U.safe_ascii_only prim_name in
        let c_name = U.safe_cname ~prefix:("i" ^ prim_name) in
        let body =
          Gen_c.build_inline_fun ~locs ctypes_fn ~c_name ~c_body ~noalloc el
        in
        C_content.add_function id_stri_expr body;
        let stubname = U.safe_cname ~prefix:("f" ^ prim_name) in
        Gen_c.gen_fun ctypes_fn ~stubname ~cfunc:c_name ~release_runtime_lock
          ~locs ~noalloc ~return_errno
    in
    C_content.add_function id_external c.Gen_c.stub_source;
    let el =
      if is_inline = false then el
      else if remove_labels then List.map el ~f:(fun (_, b) -> (Nolabel, b))
      else
        List.map el ~f:(fun ((x, y) as xy) ->
            match x with
            | Labelled s ->
              let l = String.length s in
              if l > 0 && s.[l - 1] = '_' then (Nolabel, y) else xy
            | Nolabel | Optional _ -> xy)
    in
    let res = Gen_ml.external' ctypes_fn ocaml_name el ret c in
    Hashtbl.replace G.htl_stri id_external res.Gen_ml.extern;
    Hashtbl.replace G.htl_stri id_stri_expr res.Gen_ml.intern

  let enum_size_unsigned id name =
    let res =
      match extract_single_int id with
      | Some x -> x
      | None -> U.error "can't determine type of enum %s" name
    in
    (* constants from ctypes_primitives.h *)
    let float_flag_bit = 15
    and unsigned_flag_bit = 14 in
    let is_float = res land (1 lsl float_flag_bit) <> 0 in
    let is_unsigned = res land (1 lsl unsigned_flag_bit) <> 0 in
    let size = res land lnot (1 lsl float_flag_bit) in
    let size = size land lnot (1 lsl unsigned_flag_bit) in
    if is_float then U.error "Enum type detected as float type: %s" name;
    (size, is_unsigned)

  let enum (l : 'a list) (p : string) : 'a Ctypes.typ * 'a list Ctypes.typ =
    let loc = !Ast_helper.default_loc in
    let open Marshal_types in
    let {
      enum_l;
      enum_name;
      enum_is_typedef;
      enum_type_id;
      enum_unexpected;
      enum_unexpected_bits;
      enum_is_int_bitmask;
      enum_loc = _;
    } =
      Marshal.from_string p 0
    in
    let type_size, type_unsigned =
      enum_size_unsigned (Extract.get_enum_id enum_type_id) enum_name
    in
    let unboxed_ints =
      type_unsigned = false
      && Ocaml_config.word_size () = 64
      && Options.toolchain_used () = false
      && Ctypes.alignment Ctypes.int32_t = Ctypes.alignment Ctypes.int
      && Ctypes.sizeof Ctypes.int32_t = Ctypes.sizeof Ctypes.int
    in
    let typ, typ_expr, live_res =
      let def typ expr =
        let a =
          match Extract_c_ml.prepare typ with
          | None -> assert false
          | Some x -> x
        in
        (a, expr, Extract.enum_fake_view l enum_is_typedef enum_name typ)
      in
      match (type_size, type_unsigned) with
      | 1, false -> def Ctypes.int8_t [%expr Ctypes.int8_t]
      | 2, false -> def Ctypes.int16_t [%expr Ctypes.int16_t]
      | 4, false ->
        if unboxed_ints then def Ctypes.int [%expr Ctypes.int]
        else def Ctypes.int32_t [%expr Ctypes.int32_t]
      | 8, false -> def Ctypes.int64_t [%expr Ctypes.int64_t]
      | 1, true -> def Ctypes.uint8_t [%expr Ctypes.uint8_t]
      | 2, true -> def Ctypes.uint16_t [%expr Ctypes.uint16_t]
      | 4, true -> def Ctypes.uint32_t [%expr Ctypes.uint32_t]
      | 8, true -> def Ctypes.uint64_t [%expr Ctypes.uint64_t]
      | x, _ -> U.error "enum %s has unsupported size of %d" enum_name x
    in
    let additional_check =
      if unboxed_ints && type_size = 4 then Extract_c_ml.prepare Ctypes.int32_t
      else None
    in
    let list =
      ListLabels.fold_right ~init:[%expr []] enum_l ~f:(fun c ac ->
          U.with_loc c.ee_loc @@ fun () ->
          let cname = c.ee_cname in
          if Options.(!mode <> Emulate) then (
            let id =
              match extract_single_int c.ee_type_check with
              | None -> U.error "can't determine type info of %S\n" enum_name
              | Some x -> x
            in
            if id land (1 lsl 0) = 0 then
              U.error "%s is too large for type %s" cname enum_name;
            if id land (1 lsl 1) = 0 then
              U.error "%s is not of type %s" cname enum_name;
            if (not enum_is_int_bitmask) && id land (1 lsl 2) = 0 then
              U.error "%s is not of type %s" cname enum_name);
          let i_str =
            match extract_single_str c.ee_int_id with
            | Some x -> x
            | None -> U.error "can't determine enum constant %S" c.ee_cname
          in
          let f typ =
            match Extract_c_ml.gen typ i_str with
            | Extract_c_ml.Underflow | Extract_c_ml.Overflow ->
              U.error "enum constant %S is not of type %S" c.ee_cname enum_name
            | Extract_c_ml.Expr s -> s
          in
          let integer = f typ in
          (match additional_check with
          | None -> ()
          | Some t -> ignore (f t : Marshal_types.expr));
          [%expr ([%e c.ee_expr], [%e integer]) :: [%e ac]])
    in
    let name = U.str_expr enum_name in
    let typedef =
      match enum_is_typedef with true -> [%expr true] | false -> [%expr false]
    in
    (match enum_type_id with
    | Marshal_types.E_normal id | Marshal_types.E_normal_bitmask (id, _) ->
      let e =
        [%expr
          Ppx_cstubs.Ppx_cstubs_internals.build_enum [%e name] [%e typ_expr]
            ~typedef:[%e typedef] ?unexpected:[%e enum_unexpected] [%e list]]
      in
      Hashtbl.replace G.htl_expr id e
    | Marshal_types.E_bitmask _ -> ());
    (match enum_type_id with
    | Marshal_types.E_bitmask id | Marshal_types.E_normal_bitmask (_, id) ->
      let e =
        [%expr
          Ppx_cstubs.Ppx_cstubs_internals.build_enum_bitmask [%e name]
            [%e typ_expr] ~typedef:[%e typedef]
            ?unexpected:[%e enum_unexpected_bits] [%e list]]
      in
      Hashtbl.replace G.htl_expr id e
    | Marshal_types.E_normal _ -> ());
    live_res

  let foreign id_loc_external id_loc_stri_expr ~ocaml_name ~typ_expr
      ?(release_runtime_lock = false) ?(noalloc = false) ?(return_errno = false)
      c_name fn =
    let id_external, _loc_external = U.from_id_loc_param id_loc_external in
    let id_stri_expr, _loc_stri_expr = U.from_id_loc_param id_loc_stri_expr in
    let stubname = U.safe_cname ~prefix:c_name in
    Trace.trace_fn fn;
    let c =
      Gen_c.gen_fun fn ~locs:[] ~noalloc ~return_errno ~stubname ~cfunc:c_name
        ~release_runtime_lock
    in
    C_content.add_function id_external c.Gen_c.stub_source;
    let typ_expr = U.unmarshal_expr typ_expr in
    let res = Gen_ml.foreign fn ocaml_name c typ_expr in
    Hashtbl.replace G.htl_stri id_external res.Gen_ml.extern;
    Hashtbl.replace G.htl_stri id_stri_expr res.Gen_ml.intern

  let field id_loc stru field_name field_type =
    let id, _ = U.from_id_loc_param id_loc in
    let r =
      match extract_single_int id with
      | Some x -> x
      | None ->
        U.error "couldn't extract info for field %s in %s" field_name
          (Ctypes.string_of_typ stru)
    in
    let offset = r land lnot (1 lsl 29) in
    if !Options.mode <> Options.Emulate && r land (1 lsl 29) = 0 then
      U.error "field %s in %s has the wrong size or type" field_name
        (Ctypes.string_of_typ stru);
    Hashtbl.replace G.htl_expr id (U.int_expr offset);
    Trace.trace field_type;
    Internals.add_field stru field_name offset field_type

  let typedef ?name t =
    let loc = !Ast_helper.default_loc in
    let f =
      match name with
      | None -> [%expr None]
      | Some x ->
        let x = U.str_expr (x ^ "%t") in
        [%expr Some (fun k fmt -> Format.fprintf fmt [%e x] k)]
    in
    [%expr
      Ctypes_static.View
        {
          Ctypes_static.read = Ppx_cstubs.Ppx_cstubs_internals.identity;
          Ctypes_static.write = Ppx_cstubs.Ppx_cstubs_internals.identity;
          Ctypes_static.format_typ = [%e f];
          Ctypes_static.format = None;
          Ctypes_static.ty = [%e t];
        }]

  let common_int_alias =
    let open Ast_helper in
    fun id ~mod_name ~sig_typ ~alias_mod expr ctyp ->
      let loc = !Ast_helper.default_loc in
      let alias =
        match sig_typ with
        | `Signed | `Unsigned -> []
        | `Any_signed -> []
        | `Any_unsigned -> [ [%stri let min_int = zero] ]
      in
      let texpr = typedef ~name:ctyp expr in
      let alias = [%stri let t = [%e texpr]] :: alias in
      let mod' = Mod.mk (Pmod_ident (U.mk_lid alias_mod)) in
      let idl = Incl.mk mod' in
      let alias = Str.include_ idl :: alias in
      let ms = Mod.structure alias in
      let sig' =
        match sig_typ with
        | `Signed -> "Signed"
        | `Unsigned -> "Unsigned"
        | `Any_signed | `Any_unsigned -> "Unkown_signedness"
      in
      let sig' = Mty.ident (U.mk_lid_l [ "Ppx_cstubs"; "Types"; sig' ]) in
      let m = Mod.constraint_ ms sig' in
      let r = Str.module_ (Mb.mk (U.mk_oloc mod_name) m) in
      Hashtbl.replace G.htl_stri id r

  let int_alias id_loc ~mod_name ?strict ctyp =
    let id, r = Int_alias.id_r id_loc ctyp in
    let md, expr, alias_mod = Int_alias.int ?strict ctyp r in
    common_int_alias id ~sig_typ:`Signed ~mod_name expr ~alias_mod ctyp;
    md

  let uint_alias id_loc ~mod_name ?strict ctyp :
      (module Ppx_cstubs.Types.Unsigned) =
    let id, r = Int_alias.id_r id_loc ctyp in
    let md, expr, alias_mod = Int_alias.uint ?strict ctyp r in
    common_int_alias ~sig_typ:`Unsigned id ~mod_name expr ~alias_mod ctyp;
    (module (val md))

  let aint_alias id_loc ~mod_name ?strict ctyp :
      (module Ppx_cstubs.Types.Unkown_signedness) =
    let id, r = Int_alias.id_r id_loc ctyp in
    if is_set r 29 then (
      let md, expr, alias_mod = Int_alias.uint ?strict ctyp r in
      common_int_alias ~sig_typ:`Any_unsigned id ~mod_name expr ~alias_mod ctyp;
      md)
    else
      let md, expr, alias_mod = Int_alias.int ?strict ctyp r in
      common_int_alias ~sig_typ:`Any_signed id ~mod_name expr ~alias_mod ctyp;
      (module (val md))

  let g_size_align ~size ~align name =
    let id_s, _ = U.from_id_loc_param size in
    let id_a, _ = U.from_id_loc_param align in
    let f id =
      match extract_single_int id with
      | Some x -> x
      | None -> U.error "can't find info about %s" name
    in
    (f id_s, f id_a)

  let opaque_manual_is_int id ctyp =
    let fun_name = U.safe_cname ~prefix:"is_integer" in
    let src =
      Printf.sprintf
        {|
int %s (%s p) {
   int64_t z = (int64_t)(~p);
   int r = (int) z;
   return ( ( p %% 2 ) ? 0 : r );
}
|}
        fun_name ctyp
    in
    Opaque_try_compile.does_compile id src

  let opaque_manual_is_signed id ctyp =
    let type_name = U.safe_cname ~prefix:"is_signed" in
    let src =
      Printf.sprintf
        {|
typedef char %s[(!!( ((%s)(-1)) < 1  ))*2-1];
    |}
        type_name ctyp
    in
    Opaque_try_compile.does_compile id src

  let opaque_manual_is_pointer id ctyp =
    let fun_name = U.safe_cname ~prefix:"is_pointer" in
    let src =
      Printf.sprintf
        {|
ptrdiff_t %s (%s a)
{
  %s x = NULL;
  void * y = a;
  (void)x;
  (void)y;
  return (a-a);
}
|}
        fun_name ctyp ctyp
    in
    Opaque_try_compile.does_compile id src

  type opr = (module Opaque_result)

  let opaque =
    let module Const_orig = Const in
    let open Ast_helper in
    fun ~size ~align ~typ ~mi ctyp : opr ->
      let loc = !Ast_helper.default_loc in
      let emu = Options.(!mode = Emulate) in
      let { Marshal_types.o_uniq_ref_id; o_binding_name } =
        Marshal.from_string mi 0
      in
      let uniq_ref_id = o_uniq_ref_id in
      let binding_name = o_binding_name in
      let id_typ, _ = U.from_id_loc_param typ in
      let r =
        if emu then 0l
        else
          match extract_single_int32 id_typ with
          | Some x -> x
          | None -> U.error "can't find info about %s" ctyp
      in
      let g manifest expr =
        let loc_binding_name = U.mk_loc binding_name in
        let t = Type.mk ~kind:Ptype_abstract loc_binding_name ~manifest in
        let t_abstr = Type.mk ~kind:Ptype_abstract loc_binding_name in
        let stris = [ Str.type_ Recursive [ t ] ] in
        let sigs = [ Sig.type_ Recursive [ t_abstr ] ] in
        let loc_binding_name = U.mk_loc binding_name in
        let real_binding_name = Uniq_ref.get_final_name uniq_ref_id in
        let pat = U.mk_pat real_binding_name in
        let constr = [%type: [%t U.mk_typc binding_name] Ctypes.typ] in
        let val' = Val.mk (U.mk_loc real_binding_name) constr in
        let stris = [%stri let [%p pat] = [%e expr]] :: stris in
        let sigs = Sig.value val' :: sigs in
        let stris, sigs =
          if real_binding_name = binding_name then (stris, sigs)
          else
            let expr = U.mk_ident real_binding_name in
            let val' = Val.mk loc_binding_name constr in
            let nwa = Sig.attribute (U.ocaml_warning "-32") in
            let p = U.mk_pat binding_name in
            ( [%stri let [%p p] = [%e expr]] :: stris,
              Sig.value val' :: nwa :: sigs )
        in
        let stris = Mod.structure (List.rev stris) in
        let sigs = Mty.signature (List.rev sigs) in
        let r = Str.include_ (Incl.mk (Mod.constraint_ stris sigs)) in
        Hashtbl.replace G.htl_stri (fst (U.from_id_loc_param size)) r
      in
      let f (type a) (ctypes_t : a Ctypes.typ) expr manifest : opr =
        g manifest (typedef ~name:ctyp expr);
        (module struct
          type t = a

          let t = Ctypes.typedef ctypes_t ctyp
        end)
      in
      let ws64 = Ocaml_config.word_size () = 64 in
      let b30_set = Int32.shift_left 1l 30 in
      let manual_check = Int32.logand r b30_set <> 0l in
      let r = Int32.logand r (Int32.lognot b30_set) in
      let r = Int32.to_int r in
      let int_sizes ~signed =
        let usigned = not signed in
        if signed && ws64 && is_set r 0 && is_set r 3 then
          f Ctypes.int [%expr Ctypes.int] [%type: int]
        else if signed && is_set r 1 then
          f Ctypes.int8_t [%expr Ctypes.int8_t] [%type: int]
        else if signed && is_set r 2 then
          f Ctypes.int16_t [%expr Ctypes.int16_t] [%type: int]
        else if signed && is_set r 3 then
          f Ctypes.int32_t [%expr Ctypes.int32_t] [%type: int32]
        else if signed && is_set r 4 then
          f Ctypes.int64_t [%expr Ctypes.int64_t] [%type: int64]
        else if usigned && is_set r 0 then
          f Ctypes.uint [%expr Ctypes.uint] [%type: Unsigned.uint]
        else if usigned && is_set r 1 then
          f Ctypes.uint8_t [%expr Ctypes.uint8_t] [%type: Unsigned.uint8]
        else if usigned && is_set r 2 then
          f Ctypes.uint16_t [%expr Ctypes.uint16_t] [%type: Unsigned.uint16]
        else if usigned && is_set r 3 then
          f Ctypes.uint32_t [%expr Ctypes.uint32_t] [%type: Unsigned.uint32]
        else (
          assert (usigned && is_set r 4);
          f Ctypes.uint64_t [%expr Ctypes.uint64_t] [%type: Unsigned.uint64])
      in
      let pointer () : opr =
        let manifest = [%type: unit Ctypes.ptr] in
        let expr = [%expr Ctypes.ptr Ctypes.void] in
        g manifest @@ typedef expr;
        (module struct
          type t = unit Ctypes_static.ptr

          let t : t Ctypes.typ =
            Ctypes.view ~read:Internals.identity ~write:Internals.identity
              ~format_typ:(fun k fmt -> Format.fprintf fmt "%s%t" ctyp k)
              (Ctypes.ptr Ctypes.void)
        end)
      in
      let default () : opr =
        let manifest = [%type: [ `a ] Ctypes.abstract] in
        let size, alignment = g_size_align ~size ~align ctyp in
        let expr =
          [%expr
            Ctypes_static.Abstract
              {
                Ctypes_static.aname = [%e U.str_expr ctyp];
                Ctypes_static.asize = [%e U.int_expr size];
                Ctypes_static.aalignment = [%e U.int_expr alignment];
              }]
        in
        g manifest @@ typedef expr;
        (module struct
          type t = [ `a ] Ctypes.abstract

          let t : t Ctypes.typ =
            Ctypes.view ~read:Internals.identity ~write:Internals.identity
              (Ctypes.abstract ~name:ctyp ~size ~alignment)
        end)
      in
      let int_aligned =
        is_set r 0 || is_set r 1 || is_set r 2 || is_set r 3 || is_set r 4
      in
      let usigned = is_set r 29 in
      let signed = not usigned in
      if is_set r 23 then
        f Ctypes.nativeint [%expr Ctypes.nativeint] [%type: nativeint]
      else if is_set r 15 then
        f Ctypes.int8_t [%expr Ctypes.int8_t] [%type: int]
      else if is_set r 16 then
        f Ctypes.int16_t [%expr Ctypes.int16_t] [%type: int]
      else if ws64 && is_set r 8 && (is_set r 3 || is_set r 17) then
        f Ctypes.int [%expr Ctypes.int] [%type: int]
      else if is_set r 17 then
        f Ctypes.int32_t [%expr Ctypes.int32_t] [%type: int32]
      else if is_set r 18 then
        f Ctypes.int64_t [%expr Ctypes.int64_t] [%type: int64]
      else if is_set r 5 then f Ctypes.schar [%expr Ctypes.schar] [%type: int]
      else if is_set r 19 then
        f Ctypes.uint8_t [%expr Ctypes.uint8_t] [%type: Unsigned.uint8]
      else if is_set r 20 then
        f Ctypes.uint16_t [%expr Ctypes.uint16_t] [%type: Unsigned.uint16]
      else if is_set r 21 then
        f Ctypes.uint32_t [%expr Ctypes.uint32_t] [%type: Unsigned.uint32]
      else if is_set r 22 then
        f Ctypes.uint64_t [%expr Ctypes.uint64_t] [%type: Unsigned.uint64]
      else if is_set r 6 then
        f Ctypes.uchar [%expr Ctypes.uchar] [%type: Unsigned.uchar]
      else if is_set r 7 then
        f Ctypes.ushort [%expr Ctypes.short] [%type: Signed.short]
      else if is_set r 26 then
        f Ctypes.nativeint [%expr Ctypes.nativeint] [%type: nativeint]
      else if is_set r 27 then
        f Ctypes.int64_t [%expr Ctypes.int64_t] [%type: int64]
      else if is_set r 8 then
        f Ctypes.sint [%expr Ctypes.sint] [%type: Signed.sint]
      else if is_set r 9 then
        f Ctypes.long [%expr Ctypes.long] [%type: Signed.long]
      else if is_set r 10 then
        f Ctypes.llong [%expr Ctypes.llong] [%type: Signed.llong]
      else if is_set r 11 then
        f Ctypes.ushort [%expr Ctypes.ushort] [%type: Unsigned.ushort]
      else if is_set r 12 then
        f Ctypes.uint [%expr Ctypes.uint] [%type: Unsigned.uint]
      else if is_set r 13 then
        f Ctypes.ulong [%expr Ctypes.ulong] [%type: Unsigned.ulong]
      else if is_set r 14 then
        f Ctypes.ullong [%expr Ctypes.ullong] [%type: Unsigned.ullong]
      else if is_set r 24 then f Ctypes.char [%expr Ctypes.char] [%type: char]
      else if is_set r 25 then f Ctypes.bool [%expr Ctypes.bool] [%type: bool]
        (* via __builtin_classify_type *)
      else if (not manual_check) && int_aligned then int_sizes ~signed
      else if is_set r 28 then pointer ()
      else if manual_check then
        let is_integer = opaque_manual_is_int id_typ ctyp in
        if is_integer && int_aligned then
          int_sizes ~signed:(opaque_manual_is_signed id_typ ctyp)
        else if (not is_integer) && opaque_manual_is_pointer id_typ ctyp then
          pointer ()
        else default ()
      else default ()

  let abstract ~size ~align name =
    let size, alignment = g_size_align ~size ~align name in
    Ctypes.abstract ~size ~alignment ~name

  let seal size align stru =
    let size, align = g_size_align ~size ~align "struct/union" in
    Internals.seal stru ~size ~align

  let trace_custom t =
    Trace.trace t;
    t

  let ocaml_funptr p funptr =
    let callback_fn =
      match funptr with
      | Ctypes_static.Funptr f -> f
      | _ -> U.error "invalid funptr for callback"
    in
    let mof : Marshal_types.ocaml_funptr = Marshal.from_string p 0 in
    let source = Gen_c.gen_callback_fun callback_fn mof in
    C_content.add_function mof.cb_bottom source;
    Gen_ml.ocaml_funptr mof callback_fn
end

module Ctypes_make = struct
  module type Signed_type = sig
    include Signed.S

    val t : t Ctypes_static.typ
  end

  module type Unsigned_type = sig
    include Unsigned.S

    val t : t Ctypes_static.typ
  end

  module type Target_env = sig
    module Uintptr : Unsigned_type

    module Intptr : Signed_type

    module Ptrdiff : Signed_type (* FIXME: PosixTypes missing ... *)
  end

  module Info : Target_env = struct
    module Intptr : Signed_type = struct
      include Ctypes.Intptr

      let t = Ctypes.intptr_t
    end

    module Uintptr : Unsigned_type = struct
      include Ctypes.Uintptr

      let t = Ctypes.uintptr_t
    end

    module Ptrdiff : Signed_type = struct
      include Ctypes.Ptrdiff

      let t = Ctypes.ptrdiff_t
    end
  end

  module Info32 : Target_env = struct
    module Intptr : Signed_type = struct
      include Signed.Int32

      let t = Ctypes_static.(typedef int32_t "intptr_t")
    end

    module Uintptr : Unsigned_type = struct
      include Unsigned.UInt32

      let t = Ctypes_static.(typedef uint32_t "uintptr_t")
    end

    module Ptrdiff : Signed_type = struct
      include Signed.Int32

      let t = Ctypes_static.(typedef int32_t "ptrdiff_t")
    end
  end

  module Info64 : Target_env = struct
    module Intptr : Signed_type = struct
      include Signed.Int64

      let t = Ctypes_static.(typedef int64_t "intptr_t")
    end

    module Uintptr : Unsigned_type = struct
      include Unsigned.UInt64

      let t = Ctypes_static.(typedef uint64_t "uintptr_t")
    end

    module Ptrdiff : Signed_type = struct
      include Signed.Int64

      let t = Ctypes_static.(typedef int64_t "ptrdiff_t")
    end
  end

  module Ctypes (T_env : Target_env) = struct
    include Ctypes_static

    let typ_of_bigarray_kind = Ctypes.typ_of_bigarray_kind

    let string_opt = Ctypes.string_opt

    let string = Ctypes.string

    let genarray = Ctypes.genarray

    let array1 = Ctypes.array1

    let array2 = Ctypes.array2

    let array3 = Ctypes.array3

    let static_funptr x =
      Trace.trace_fn x;
      static_funptr x

    let ( @-> ) a b =
      Trace.trace a;
      Trace.trace_fn b;
      Internals.Shadow.( @-> ) a b

    let returning a =
      Trace.trace a;
      Internals.Shadow.returning a

    let typedef a b =
      Trace.trace a;
      typedef a b

    let view ?format_typ ?format ~read ~write a =
      Trace.trace a;
      view ?format_typ ?format ~read ~write a

    let array i a =
      Trace.trace a;
      array i a

    let ptr a =
      Trace.trace a;
      ptr a

    let ptr_opt a =
      Trace.trace a;
      Ctypes.ptr_opt a

    module Intptr = T_env.Intptr
    module Uintptr = T_env.Uintptr
    module Ptrdiff = T_env.Ptrdiff

    let intptr_t = Intptr.t

    let uintptr_t = Uintptr.t

    let ptrdiff_t = Ptrdiff.t

    let coerce_fn a b c =
      Trace.trace_fn a;
      Trace.trace_fn b;
      Ctypes.coerce_fn a b c

    let coerce a b c =
      Trace.trace a;
      Trace.trace b;
      Ctypes.coerce a b c

    let addr = Ctypes.addr

    let unavailable s =
      U.error "function %s is not available during code generation" s

    let ( !@ ) = Ctypes.( !@ )

    let ( +@ ) = Ctypes.( +@ )

    let ( -@ ) = Ctypes.( -@ )

    let ( <-@ ) = Ctypes.( <-@ )

    let ( @. ) = Ctypes.( @. )

    let allocate = Ctypes.allocate

    let allocate_n = Ctypes.allocate_n

    let array_of_bigarray = Ctypes.array_of_bigarray

    let bigarray_of_array = Ctypes.bigarray_of_array

    let bigarray_of_ptr = Ctypes.bigarray_of_ptr

    let bigarray_start = Ctypes.bigarray_start

    let format _ _ _ = unavailable "format"

    let format_fn = Ctypes.format_fn

    let format_typ = Ctypes.format_typ

    let fortran_bigarray_of_ptr = Ctypes.fortran_bigarray_of_ptr

    let from_voidp = Ctypes.from_voidp

    let funptr_of_raw_address = Ctypes.funptr_of_raw_address

    let getf = Ctypes.getf

    let is_null = Ctypes.is_null

    let make = Ctypes.make

    let null = Ctypes.null

    let ocaml_bytes_start = Ctypes.ocaml_bytes_start

    let ocaml_string_start = Ctypes.ocaml_string_start

    let ptr_compare = Ctypes.ptr_compare

    let ptr_diff = Ctypes.ptr_diff

    let ptr_of_raw_address = Ctypes.ptr_of_raw_address

    let raw_address_of_ptr = Ctypes.raw_address_of_ptr

    let reference_type = Ctypes.reference_type

    let setf = Ctypes.setf

    let string_from_ptr = Ctypes.string_from_ptr

    let string_of _ _ = unavailable "string_of"

    let string_of_fn = Ctypes.string_of_fn

    let string_of_typ = Ctypes.string_of_typ

    let to_voidp = Ctypes.to_voidp

    let ( |-> ) = Ctypes.( |-> )

    module CArray = Ctypes.CArray

    let offsetof _ = unavailable "offsetof"

    let alignment _ = unavailable "alignment"

    let sizeof _ = unavailable "sizeof"

    let ppxc__private_ocaml_typ typ =
      let s : [ `priv ] Ctypes_static.structure Ctypes.typ =
        Ctypes.structure typ
      in
      let (_ : _ Ctypes.field) = Ctypes.field s "i" Ctypes.camlint in
      let () = Ctypes.seal s in
      Ctypes.view ~format_typ:Evil_hack.format_typ ~read:Std.identity
        ~write:Std.identity s

    let ppxc__private_structure = structure

    let ppxc__private_union = union

    let ppxc__unavailable = unavailable
  end
end

module Run_environment = struct
  module Libffi_abi = struct
    type abi = unit

    let aix = ()

    let darwin = ()

    let eabi = ()

    let fastcall = ()

    let gcc_sysv = ()

    let linux = ()

    let linux64 = ()

    let linux_soft_float = ()

    let ms_cdecl = ()

    let n32 = ()

    let n32_soft_float = ()

    let n64 = ()

    let n64_soft_float = ()

    let o32 = ()

    let o32_soft_float = ()

    let osf = ()

    let pa32 = ()

    let stdcall = ()

    let sysv = ()

    let thiscall = ()

    let unix = ()

    let unix64 = ()

    let v8 = ()

    let v8plus = ()

    let v9 = ()

    let vfp = ()

    let default_abi = ()
  end

  module Foreign = struct
    let funptr ?abi:_ ?name:_ ?check_errno:_ ?runtime_lock:_
        ?thread_registration:_ fn =
      Trace.trace_fn fn;
      let read _ = assert false
      and write _ = assert false in
      Ctypes_static.(view ~read ~write (static_funptr fn))

    let funptr_opt ?abi:_ ?name:_ ?check_errno:_ ?runtime_lock:_
        ?thread_registration:_ fn =
      Trace.trace_fn fn;
      let read _ = assert false
      and write _ = assert false in
      Ctypes_static.(view ~read ~write (static_funptr fn))
  end

  let funptr ?abi ?name ?check_errno ?runtime_lock ?thread_registration fn =
    G.foreign_used := true;
    Foreign.funptr ?abi ?name ?check_errno ?runtime_lock ?thread_registration fn

  let funptr_opt ?abi ?name ?check_errno ?runtime_lock ?thread_registration fn =
    G.foreign_used := true;
    Foreign.funptr_opt ?abi ?name ?check_errno ?runtime_lock
      ?thread_registration fn

  module Ctypes_static = struct end

  module Ctypes_primitive_types = struct end

  module Ctypes_printers = struct end

  module Ctypes_structs = struct end

  module Ctypes_types = struct end
end

module Main = struct
  let clear () =
    C_content_phase0.clear ();
    C_content.clear ();
    Trace.clear ();
    Const.clear ();
    Const_phase0.clear ();
    Opaque_try_compile.clear ()

  let run () =
    Const.extract_all ();
    G.c_source := C_content.get_source ();
    clear ()
end

let set_loc loc =
  let loc : Marshal_types.loc = Marshal.from_string loc 0 in
  Ast_helper.default_loc := loc

(* This file is only used through the toplevel and not referenced directly.
   This way a dependency order is enforced *)
let _init _ = ()
