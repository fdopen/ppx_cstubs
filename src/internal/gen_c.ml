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

open Ctypes
open Mparsetree.Ast_cur
module List = CCListLabels
module U = Std.Util

let error = Std.Util.error

(* structs need a name and I can't print bigarrays ... *)
let rec check_printable : type a. a typ -> bool =
  let open Ctypes_static in
  function
  | Struct { tag = ""; _ } ->
    error "passing or returning unnamed structs is not supported"
  | Struct _ -> true
  | Union { utag = ""; _ } ->
    error "passing or returning unnamed unions is not supported"
  | Union _ -> true
  | View { format_typ = Some _; _ } -> true
  | View { ty; _ } -> check_printable ty
  | Pointer ty -> check_printable ty
  | Funptr fn -> check_printable_fn fn
  | Array _ -> true
  | Bigarray _ -> false
  | OCaml _ -> true
  | Primitive _ -> true
  | Void -> true
  | Abstract _ -> true

and check_printable_fn : type b. b fn -> bool =
  let open Ctypes_static in
  function
  | Returns a -> check_printable a
  | Function (a, b) -> check_printable_fn b && check_printable a

let string_of_typ_exn ?name t =
  if check_printable t then Ctypes_type_printing_fake.string_of_typ ?name t
  else Ctypes.string_of_typ ?name t

type t_typ =
  | Rvoid
  | Rfloat
  | Rother

type noalloc_typ =
  | Noalloc_always
  | Noalloc_opt
  | Noalloc_never

type ret = {
  r_typ : t_typ;
  ocaml_ret_var : string;
  c_rvar : string;
  decl_rvar : unit -> string;
  r_noalloc_typ : noalloc_typ;
  inj_alloc : unit -> string;
  inj_noalloc : unit -> string;
  inj_byte_noalloc : string -> string;
  (* caml_copy_double(call_native(..,.. *)
  inative_ret_type : string;
      (* if something else than value should be used, necessary to support
           passing unboxed values *)
}

let ret_info_prim :
    type a.
    a Ctypes_primitive_types.prim ->
    all_float:bool ->
    ocaml_ret_var:string ->
    c_rvar:string ->
    decl_rvar:(unit -> string) ->
    ret =
  let open Ctypes_primitive_types in
  fun p ~all_float ~ocaml_ret_var ~c_rvar ~decl_rvar ->
    let inj_alloc, inj_noalloc', to_value =
      let s =
        match p with
        | Char -> "Ctypes_val_char"
        | Schar -> "Val_int"
        | Uchar -> "Integers_val_uint8"
        | Bool -> "Val_bool"
        | Short -> "Val_int"
        | Int -> "Val_long"
        | Long -> "ctypes_copy_long"
        | Llong -> "ctypes_copy_llong"
        | Ushort -> "ctypes_copy_ushort"
        | Sint -> "ctypes_copy_sint"
        | Uint -> "ctypes_copy_uint"
        | Ulong -> "ctypes_copy_ulong"
        | Ullong -> "ctypes_copy_ullong"
        | Size_t -> "ctypes_copy_size_t"
        | Int8_t -> "Val_int"
        | Int16_t -> "Val_int"
        | Int32_t -> "caml_copy_int32"
        | Int64_t -> "caml_copy_int64"
        | Uint8_t -> "Integers_val_uint8"
        | Uint16_t -> "Integers_val_uint16"
        | Uint32_t -> "integers_copy_uint32"
        | Uint64_t -> "integers_copy_uint64"
        | Camlint -> "Val_long"
        | Nativeint -> "caml_copy_nativeint"
        | Float -> "caml_copy_double"
        | Double -> "caml_copy_double"
        | LDouble -> "ctypes_copy_ldouble"
        | Complex32 -> "ctypes_copy_float_complex"
        | Complex64 -> "ctypes_copy_double_complex"
        | Complexld -> "ctypes_copy_ldouble_complex"
      in
      ( (fun () -> Printf.sprintf "%s(%s)" s c_rvar),
        (fun () -> Printf.sprintf "%s" c_rvar),
        fun var -> Printf.sprintf "%s(%s)" s var )
    in
    let inj_c ~noalloc r_typ =
      {
        r_typ;
        ocaml_ret_var;
        c_rvar;
        decl_rvar;
        r_noalloc_typ = noalloc;
        inj_alloc;
        inj_noalloc = inj_alloc;
        inj_byte_noalloc = Std.identity;
        inative_ret_type = "value";
      }
    in
    let inj_noalloc_impossible () = inj_c ~noalloc:Noalloc_never Rother in
    let inj_float () =
      if Ocaml_config.version () < (4, 3, 0) && all_float = false then
        inj_c ~noalloc:Noalloc_never Rfloat
      else
        {
          r_typ = Rfloat;
          ocaml_ret_var;
          c_rvar;
          decl_rvar;
          r_noalloc_typ = Noalloc_opt;
          inj_alloc;
          inj_noalloc = inj_noalloc';
          inj_byte_noalloc = to_value;
          inative_ret_type = "double";
        }
    in
    let inj_camlint () =
      if Ocaml_config.version () < (4, 3, 0) then
        inj_c ~noalloc:Noalloc_always Rother
      else
        {
          r_typ = Rother;
          ocaml_ret_var;
          c_rvar;
          decl_rvar;
          r_noalloc_typ = Noalloc_always;
          inj_alloc;
          inj_noalloc = inj_noalloc';
          inj_byte_noalloc = to_value;
          inative_ret_type = "intnat";
        }
    in
    let inj_intalias () =
      {
        r_typ = Rother;
        ocaml_ret_var;
        c_rvar;
        decl_rvar;
        r_noalloc_typ = Noalloc_always;
        inj_alloc;
        inj_noalloc = inj_alloc;
        inj_byte_noalloc = Std.identity;
        inative_ret_type = "value";
      }
    in
    let inj_intb ctype =
      if Ocaml_config.version () < (4, 3, 0) then inj_noalloc_impossible ()
      else
        {
          r_typ = Rother;
          ocaml_ret_var;
          c_rvar;
          decl_rvar;
          r_noalloc_typ = Noalloc_opt;
          inj_alloc;
          inj_noalloc = inj_noalloc';
          inj_byte_noalloc = to_value;
          inative_ret_type = ctype;
        }
    in
    match p with
    | Schar -> inj_camlint ()
    | Int8_t -> inj_camlint ()
    | Int16_t -> inj_camlint ()
    | Camlint -> inj_camlint ()
    | Int -> inj_camlint ()
    | Short -> inj_camlint ()
    | Float -> inj_float ()
    | Double -> inj_float ()
    | Char -> inj_intalias ()
    | Uchar -> inj_intalias ()
    | Bool -> inj_intalias ()
    | Uint8_t -> inj_intalias ()
    | Uint16_t -> inj_intalias ()
    | Int32_t -> inj_intb "int32_t"
    | Int64_t -> inj_intb "int64_t"
    | Nativeint -> inj_intb "intnat"
    | Long -> inj_noalloc_impossible ()
    | Llong -> inj_noalloc_impossible ()
    | Ushort -> inj_noalloc_impossible ()
    | Sint -> inj_noalloc_impossible ()
    | Uint -> inj_noalloc_impossible ()
    | Ulong -> inj_noalloc_impossible ()
    | Ullong -> inj_noalloc_impossible ()
    | Size_t -> inj_noalloc_impossible ()
    | Uint32_t -> inj_noalloc_impossible ()
    | Uint64_t -> inj_noalloc_impossible ()
    | LDouble -> inj_noalloc_impossible ()
    | Complex32 -> inj_noalloc_impossible ()
    | Complex64 -> inj_noalloc_impossible ()
    | Complexld -> inj_noalloc_impossible ()

let rec ret_info :
    type a.
    a typ ->
    all_float:bool ->
    user_noalloc:bool ->
    ocaml_ret_var:string ->
    c_rvar:string ->
    decl_rvar:(unit -> string) ->
    ret =
  let open Ctypes_static in
  let error s = error "cstubs does not support returning %s" s in
  fun t ~all_float ~user_noalloc ~ocaml_ret_var ~c_rvar ~decl_rvar ->
    let standard ?(is_void = false) ~noalloc inj =
      {
        r_typ = (if is_void then Rvoid else Rother);
        ocaml_ret_var;
        c_rvar;
        decl_rvar;
        r_noalloc_typ = noalloc;
        inj_alloc = inj;
        inj_noalloc = inj;
        inj_byte_noalloc = Std.identity;
        inative_ret_type = "value";
      }
    in
    let pptr () =
      let inj_alloc () = Printf.sprintf "CTYPES_FROM_PTR(%s)" c_rvar in
      if Ocaml_config.version () < (4, 3, 0) then
        standard ~noalloc:Noalloc_never inj_alloc
      else
        {
          r_typ = Rother;
          ocaml_ret_var;
          c_rvar;
          decl_rvar;
          r_noalloc_typ = Noalloc_opt;
          inj_alloc;
          inj_noalloc = (fun () -> Printf.sprintf "(intnat)(%s)" c_rvar);
          inj_byte_noalloc = Printf.sprintf "CTYPES_FROM_PTR(%s)";
          inative_ret_type = "intnat";
        }
    in
    let cp () =
      let inj () =
        Printf.sprintf "ctypes_copy_bytes(&(%s),sizeof(%s))" c_rvar c_rvar
      in
      standard ~noalloc:Noalloc_never inj
    in
    match t with
    | Void ->
      standard ~noalloc:Noalloc_always ~is_void:true (fun () -> "Val_unit")
    | Primitive p ->
      ret_info_prim p ~all_float ~ocaml_ret_var ~c_rvar ~decl_rvar
    | Pointer _ -> pptr ()
    | Funptr _ -> pptr ()
    | Struct _ -> cp ()
    | Union _ -> cp ()
    | Abstract _ -> cp ()
    | View { format_typ = Some ft; _ } when ft == Evil_hack.format_typ ->
      let inj () = Printf.sprintf "%s" c_rvar in
      let noalloc = if user_noalloc then Noalloc_always else Noalloc_never in
      standard ~noalloc inj
    | View { ty; _ } ->
      ret_info ty ~all_float ~user_noalloc ~ocaml_ret_var ~c_rvar ~decl_rvar
    | Array _ -> error "arrays"
    | Bigarray _ -> error "bigarrays"
    | OCaml _ -> error "ocaml references as return values"

type param = {
  typ : t_typ;
  ocaml_param : string;
  c_var : string;
  runtime_protect : bool;
  noalloc_possible : bool;
  prj_alloc : unit -> string;
  prj_noalloc : unit -> string;
  prj_byte_noalloc : string -> string;
  (* call_native(Long_val(a[0]),...) *)
  native_param_type : string;
      (* if something else than value should be used, necessary to support
           passing unboxed values *)
}

let pinfo_prim :
    type a.
    a Ctypes_primitive_types.prim ->
    ctype:string ->
    c_var:string ->
    ocaml_param:string ->
    all_float:bool ->
    param =
  let open Ctypes_primitive_types in
  fun p ~ctype ~c_var ~ocaml_param ~all_float ->
    let f s =
      ( (fun () -> Printf.sprintf "%s %s = %s(%s);" ctype c_var s ocaml_param),
        fun g -> Printf.sprintf "%s(%s)" s g )
    in
    let prj_alloc, prj_byte_noalloc' =
      match p with
      | Char -> f "Int_val"
      | Schar -> f "Int_val"
      | Uchar -> f "Uint8_val"
      | Bool -> f "Bool_val"
      | Short -> f "Int_val"
      | Int -> f "Long_val"
      | Long -> f "ctypes_long_val"
      | Llong -> f "ctypes_llong_val"
      | Ushort -> f "ctypes_ushort_val"
      | Sint -> f "ctypes_sint_val"
      | Uint -> f "ctypes_uint_val"
      | Ulong -> f "ctypes_ulong_val"
      | Ullong -> f "ctypes_ullong_val"
      | Size_t -> f "ctypes_size_t_val"
      | Int8_t -> f "Int_val"
      | Int16_t -> f "Int_val"
      | Int32_t -> f "Int32_val"
      | Int64_t -> f "Int64_val"
      | Uint8_t -> f "Uint8_val"
      | Uint16_t -> f "Uint16_val"
      | Uint32_t -> f "Uint32_val"
      | Uint64_t -> f "Uint64_val"
      | Camlint -> f "Long_val"
      | Nativeint -> f "Nativeint_val"
      | Float -> f "Double_val"
      | Double -> f "Double_val"
      | LDouble -> f "ctypes_ldouble_val"
      | Complex32 -> f "ctypes_float_complex_val"
      | Complex64 -> f "ctypes_double_complex_val"
      | Complexld -> f "ctypes_ldouble_complex_val"
    in
    let fprj_noalloc xtype =
      if xtype = ctype then fun () ->
        Printf.sprintf "%s %s = %s;" ctype c_var ocaml_param
      else fun () ->
        Printf.sprintf "%s %s = (%s)(%s);" ctype c_var ctype ocaml_param
    in
    let standard_c typ =
      {
        typ;
        ocaml_param;
        c_var;
        runtime_protect = false;
        prj_alloc;
        noalloc_possible = true;
        prj_noalloc = prj_alloc;
        prj_byte_noalloc = Std.identity;
        native_param_type = "value";
      }
    in
    let standard () = standard_c Rother in
    let int_t t =
      if Ocaml_config.version () < (4, 3, 0) then standard ()
      else
        {
          typ = Rother;
          ocaml_param;
          c_var;
          runtime_protect = false;
          prj_alloc;
          noalloc_possible = true;
          prj_noalloc = fprj_noalloc t;
          prj_byte_noalloc = prj_byte_noalloc';
          native_param_type = t;
        }
    in
    let noalloc_int () = int_t "intnat" in
    let float () =
      if Ocaml_config.version () < (4, 3, 0) && all_float = false then
        standard_c Rfloat
      else
        {
          typ = Rfloat;
          ocaml_param;
          c_var;
          runtime_protect = false;
          prj_alloc;
          noalloc_possible = true;
          prj_noalloc = fprj_noalloc "double";
          prj_byte_noalloc = prj_byte_noalloc';
          native_param_type = "double";
        }
    in
    let p' = p in
    match p' with
    | Schar -> noalloc_int ()
    | Int8_t -> noalloc_int ()
    | Int16_t -> noalloc_int ()
    | Camlint -> noalloc_int ()
    | Int -> noalloc_int ()
    | Short -> noalloc_int ()
    | Float -> float ()
    | Double -> float ()
    | Int32_t -> int_t "int32_t"
    | Int64_t -> int_t "int64_t"
    | Nativeint -> int_t "intnat"
    | Char -> standard ()
    | Uchar -> standard ()
    | Bool -> standard ()
    | Long -> standard ()
    | Llong -> standard ()
    | Ushort -> standard ()
    | Sint -> standard ()
    | Uint -> standard ()
    | Ulong -> standard ()
    | Ullong -> standard ()
    | Size_t -> standard ()
    | Uint8_t -> standard ()
    | Uint16_t -> standard ()
    | Uint32_t -> standard ()
    | Uint64_t -> standard ()
    | LDouble -> standard ()
    | Complex32 -> standard ()
    | Complex64 -> standard ()
    | Complexld -> standard ()

let rec pinfo :
    type a b.
    a typ ->
    b typ ->
    user_noalloc:bool ->
    c_var:string ->
    ocaml_param:string ->
    all_float:bool ->
    param =
  let module C = Ctypes_static in
  let error s = error "cstubs does not support passing %s" s in
  fun p orig ~user_noalloc ~c_var ~ocaml_param ~all_float ->
    let standard ?(runtime_protect = true) ?(noalloc_possible = true)
        ?(is_void = false) f =
      {
        typ = (if is_void then Rvoid else Rother);
        ocaml_param;
        c_var;
        runtime_protect;
        prj_alloc = f;
        noalloc_possible;
        prj_noalloc = f;
        prj_byte_noalloc = Std.identity;
        native_param_type = "value";
      }
    in
    let stru () =
      standard (fun () ->
          let cast_name = string_of_typ_exn (ptr orig) in
          Printf.sprintf "%s = *(%s)(CTYPES_ADDR_OF_FATPTR(%s));"
            (string_of_typ_exn ~name:c_var orig)
            cast_name ocaml_param)
    in
    let hptr noalloc_possible =
      standard ~noalloc_possible (fun () ->
          Printf.sprintf "%s = PPX_CSTUBS_ADDR_OF_FATPTR(%s,%s);"
            (string_of_typ_exn ~name:c_var orig)
            (string_of_typ_exn orig) ocaml_param)
    in
    match p with
    | C.Void ->
      standard ~runtime_protect:false ~is_void:true (fun () ->
          Printf.sprintf "(void)%s;" ocaml_param)
    | C.Primitive x ->
      let ct = string_of_typ_exn p in
      pinfo_prim ~all_float ~ctype:ct x ~c_var ~ocaml_param
    | C.Array (_, _) -> error "arrays"
    | C.Bigarray _ -> error "bigarrays"
    | C.Pointer _ -> hptr true
    | C.Abstract _ -> stru ()
    | C.View { C.format_typ = Some ft; _ } when ft == Evil_hack.format_typ ->
      standard ~noalloc_possible:user_noalloc ~runtime_protect:false (fun () ->
          Printf.sprintf "value %s = %s;" c_var ocaml_param)
    | C.View { C.ty; _ } ->
      (* TODO: if there is a view of a view and both define format_typ, which
         one should be used? *)
      pinfo ty orig ~all_float ~user_noalloc ~c_var ~ocaml_param
    | C.Struct _ -> stru ()
    | C.Union _ -> stru ()
    | C.Funptr _ -> hptr false
    | C.OCaml x ->
      let s =
        match x with
        | C.String -> "CTYPES_PTR_OF_OCAML_STRING"
        | C.Bytes -> "CTYPES_PTR_OF_OCAML_BYTES"
        | C.FloatArray -> "CTYPES_PTR_OF_FLOAT_ARRAY"
      in
      standard (fun () ->
          Printf.sprintf "%s = %s(%s);"
            (string_of_typ_exn ~name:c_var orig)
            s ocaml_param)

let pinfo p = pinfo p p

let with_loc locs f =
  let loc, locs =
    match locs with [] -> (!Ast_helper.default_loc, []) | hd :: tl -> (hd, tl)
  in
  U.with_loc loc @@ fun () -> f locs

let extract =
  let open Ctypes_static in
  fun ~all_float ~user_noalloc ~locs a ->
    let i = ref 0 in
    let name () =
      let res = Printf.sprintf "%s%x" Myconst.private_prefix !i in
      incr i;
      res
    in
    let rec extract : type a. a Ctypes.fn -> Location.t list -> ret * param list
        =
     fun t locs ->
      with_loc locs @@ fun locs ->
      match t with
      | Returns t ->
        let ocaml_ret_var = name () in
        let c_rvar = name () in
        let decl_rvar () = string_of_typ_exn ~name:c_rvar t in
        let r =
          ret_info t ~all_float ~user_noalloc ~ocaml_ret_var ~c_rvar ~decl_rvar
        in
        (r, [])
      | Function (a, b) ->
        let ocaml_param = name () in
        let c_var = name () in
        let c = pinfo a ~all_float ~user_noalloc ~c_var ~ocaml_param in
        let r, l = extract b locs in
        (r, c :: l)
    in
    extract a locs

type info = {
  stub_source : string;
  stub_name : string;
  stub_name_byte : string option;
  noalloc : bool;
  float : bool;
  return_errno : bool;
}

let check_no_ocaml fn locs msg =
  let rec check_no_ocaml : type a. a Ctypes.fn -> Location.t list -> unit =
    let open Ctypes_static in
    fun fn locs ->
      with_loc locs @@ fun locs ->
      match fn with
      | Returns a -> check_no_ocaml_t locs a
      | Function (a, b) ->
        check_no_ocaml_t locs a;
        check_no_ocaml b locs
  and check_no_ocaml_t : type a. Location.t list -> a Ctypes.typ -> unit =
    let open Ctypes_static in
    fun locs -> function
      | OCaml _ -> error msg
      | Struct _ -> ()
      | Union _ -> ()
      | View { ty; _ } -> check_no_ocaml_t locs ty
      | Pointer ty -> check_no_ocaml_t locs ty
      | Funptr _ -> ()
      | Array _ -> ()
      | Bigarray _ -> ()
      | Primitive _ -> ()
      | Void -> ()
      | Abstract _ -> ()
  in
  check_no_ocaml fn locs

let funptr_transform =
  let open Ctypes_static in
  fun a ->
    let res = ref [] in
    let rec iter2 : type a. a Ctypes.typ -> a Ctypes.typ =
     fun a ->
      match a with
      | Struct _ -> a
      | Union _ -> a
      | Pointer a -> Pointer (iter2 a)
      | Array (a, i) -> Array (iter2 a, i)
      | Bigarray _ -> a
      | OCaml _ -> a
      | Primitive _ -> a
      | Void -> a
      | Abstract _ -> a
      | View { format_typ = Some _; _ } -> a
      | View ({ ty; _ } as x) -> View { x with ty = iter2 ty }
      | Funptr _ ->
        let name = U.safe_cname ~prefix:"typedef" in
        let t = string_of_typ_exn ~name a in
        let t = Printf.sprintf "typedef %s;\n" t in
        res := t :: !res;
        Ctypes.typedef a name
    in
    let rec iter1 : type a. a Ctypes.fn -> a Ctypes.fn = function
      | Function (a, b) -> Function (iter2 a, iter1 b)
      | Returns b -> Returns (iter2 b)
    in
    let a = iter1 a in
    (a, !res)

let gen_common fn ~locs ~stubname ~cfunc_value ~release_runtime_lock ~noalloc
    ~return_errno =
  let fn, lt = funptr_transform fn in
  let user_noalloc = noalloc in
  let ((ret, params) as rp) = extract ~all_float:false ~user_noalloc ~locs fn in
  let ret, params =
    if
      Ocaml_config.version () >= (4, 3, 0)
      || user_noalloc = false
      || return_errno
      || release_runtime_lock
      || ret.r_typ <> Rfloat
      || List.exists params ~f:(fun x -> x.typ <> Rfloat)
    then rp
    else extract ~all_float:true ~user_noalloc ~locs fn
  in
  let params_length = List.length params in
  if params_length > 1 && List.exists ~f:(fun s -> s.typ = Rvoid) params then
    error
      "you can't pass void as parameter to a function with two or more parameters";
  if release_runtime_lock then
    check_no_ocaml fn locs
      "You can't pass OCaml values to C, if you release the runtime lock";
  (* noalloc must be kept in sync with float formular above for OCaml 4.02 *)
  let noalloc =
    noalloc
    && return_errno = false
    && release_runtime_lock = false
    && List.for_all params ~f:(fun x -> x.noalloc_possible)
    &&
    match ret.r_noalloc_typ with
    | Noalloc_opt | Noalloc_always -> true
    | Noalloc_never -> false
  in
  let gen_byte_version =
    params_length > 5
    || List.exists params ~f:(fun x -> x.native_param_type <> "value")
    || (ret.inative_ret_type <> "value" && return_errno = false)
  in
  let buf = Buffer.create 2048 in
  List.iter lt ~f:(Buffer.add_string buf);
  Buffer.add_string buf "DISABLE_CONST_WARNINGS_PUSH();\n";
  let cname_main = stubname in
  let cname_byte = "b" ^ stubname in
  (* first character is always significant. It's not the case for a suffix *)
  let native_ret_type =
    if return_errno then "value" else ret.inative_ret_type
  in
  Buffer.add_string buf {|#ifdef __cplusplus
extern "C" {
#endif
|};
  (* native prototype *)
  Printf.bprintf buf "%s %s(" native_ret_type cname_main;
  List.iteri params ~f:(fun i p ->
      if i <> 0 then Buffer.add_string buf ", ";
      Buffer.add_string buf p.native_param_type);
  Buffer.add_string buf ");\n";
  (* byte prototype *)
  if gen_byte_version then
    if params_length > 5 then
      Printf.bprintf buf "value %s(value *,int);\n" cname_byte
    else (
      Printf.bprintf buf "value %s(" cname_byte;
      List.iteri params ~f:(fun i _ ->
          if i <> 0 then Buffer.add_string buf ", ";
          Buffer.add_string buf "value");
      Buffer.add_string buf ");\n" );
  Printf.bprintf buf {|#ifdef __cplusplus
}
#endif

|};
  (* main function header *)
  Printf.bprintf buf "%s %s(" native_ret_type cname_main;
  List.iteri params ~f:(fun i x ->
      if i <> 0 then Buffer.add_string buf ", ";
      Printf.bprintf buf "%s %s" x.native_param_type x.ocaml_param);
  Buffer.add_string buf ")  {\n";
  let params_with_protection =
    if
      release_runtime_lock = false
      && user_noalloc = true
      && List.for_all params ~f:(fun x -> x.noalloc_possible)
    then []
    else
      List.filter_map params ~f:(fun x ->
          if x.runtime_protect = false then None else Some x.ocaml_param)
  in
  ( match params_with_protection with
  | [] -> ()
  | [ h ] -> Printf.bprintf buf "  CAMLparam1(%s);\n" h
  | [ h1; h2 ] -> Printf.bprintf buf "  CAMLparam2(%s,%s);\n" h1 h2
  | [ h1; h2; h3 ] -> Printf.bprintf buf "  CAMLparam3(%s,%s,%s);\n" h1 h2 h3
  | [ h1; h2; h3; h4 ] ->
    Printf.bprintf buf "  CAMLparam4(%s,%s,%s,%s);\n" h1 h2 h3 h4
  | h1 :: h2 :: h3 :: h4 :: h5 :: tl ->
    Printf.bprintf buf "  CAMLparam5(%s,%s,%s,%s,%s);\n" h1 h2 h3 h4 h5;
    let rec iter = function
      | [] -> ()
      | [ h ] -> Printf.bprintf buf "  CAMLxparam1(%s);\n" h
      | [ h1; h2 ] -> Printf.bprintf buf "  CAMLxparam2(%s,%s);\n" h1 h2
      | [ h1; h2; h3 ] ->
        Printf.bprintf buf "  CAMLxparam3(%s,%s,%s);\n" h1 h2 h3
      | [ h1; h2; h3; h4 ] ->
        Printf.bprintf buf "  CAMLxparam4(%s,%s,%s,%s);\n" h1 h2 h3 h4
      | h1 :: h2 :: h3 :: h4 :: h5 :: tl ->
        Printf.bprintf buf "  CAMLxparam5(%s,%s,%s,%s,%s);\n" h1 h2 h3 h4 h5;
        iter tl
    in
    iter tl );
  (* declare variables, convert ocaml values to native ctypes *)
  List.iter params ~f:(fun x ->
      Buffer.add_string buf "  ";
      Buffer.add_string buf @@ x.prj_noalloc ();
      Buffer.add_char buf '\n');
  (* declare variables for result *)
  if ret.r_typ <> Rvoid then (
    Printf.bprintf buf "  %s %s;\n" native_ret_type ret.ocaml_ret_var;
    Printf.bprintf buf "  %s;\n" @@ ret.decl_rvar () );
  if release_runtime_lock then
    Printf.bprintf buf "  caml_release_runtime_system();\n";
  if return_errno then Printf.bprintf buf "  errno = 0;\n";
  if ret.r_typ <> Rvoid then Printf.bprintf buf "  %s = " ret.c_rvar
  else Buffer.add_string buf "  ";
  ( match cfunc_value with
  | `Fun cfunc ->
    Buffer.add_string buf cfunc;
    Buffer.add_char buf '(';
    List.iteri params ~f:(fun i x ->
        if i <> 0 then Buffer.add_string buf ", ";
        if x.typ <> Rvoid then Buffer.add_string buf x.c_var);
    Buffer.add_string buf ");\n"
  | `Value v -> Printf.bprintf buf "&%s;\n" v );
  if release_runtime_lock then
    Printf.bprintf buf "  caml_acquire_runtime_system();\n";
  if ret.r_typ <> Rvoid then (
    Printf.bprintf buf "  %s = " ret.ocaml_ret_var;
    let s = if return_errno then ret.inj_alloc () else ret.inj_noalloc () in
    Buffer.add_string buf s;
    Buffer.add_string buf ";\n" );
  let return_val =
    if ret.r_typ = Rvoid then "Val_unit" else ret.ocaml_ret_var
  in
  let return_val =
    if return_errno = false then return_val
    else Printf.sprintf "ctypes_pair_with_errno(%s)" return_val
  in
  if params_with_protection = [] then
    Printf.bprintf buf "  return %s;" return_val
  else if native_ret_type = "value" then
    Printf.bprintf buf "  CAMLreturn(%s);" return_val
  else Printf.bprintf buf "  CAMLreturnT(%s,%s);" native_ret_type return_val;
  Buffer.add_string buf "\n}\n";
  if gen_byte_version then (
    if params_length > 5 then
      Printf.bprintf buf "value %s(value *a,int n)\n{\n  return(" cname_byte
    else (
      Printf.bprintf buf "value %s(value a0" cname_byte;
      ( match params with
      | [] -> ()
      | _ :: tl ->
        List.iteri tl ~f:(fun i _ -> Printf.bprintf buf ", value a%d" (succ i))
      );
      Buffer.add_string buf ")  {\n  return(" );
    let l =
      List.mapi params ~f:(fun i x ->
          let t =
            if params_length < 6 then Printf.sprintf "a%d" i
            else Printf.sprintf "a[%d]" i
          in
          x.prj_byte_noalloc t)
    in
    let t = String.concat ", " l in
    let t = String.concat "" [ "("; cname_main; "("; t; ")"; ")" ] in
    let t = if return_errno then t else ret.inj_byte_noalloc t in
    Buffer.add_string buf t;
    Buffer.add_string buf ");\n}\n" );
  Buffer.add_string buf "DISABLE_CONST_WARNINGS_POP();\n";
  let float =
    noalloc
    && ret.r_typ = Rfloat
    && List.for_all params ~f:(fun x -> x.typ = Rfloat)
  in
  {
    stub_source = Buffer.contents buf;
    stub_name = cname_main;
    stub_name_byte = (if gen_byte_version then Some cname_byte else None);
    noalloc;
    float;
    return_errno;
  }

let gen_fun a ~locs ~stubname ~cfunc ~release_runtime_lock ~noalloc
    ~return_errno =
  gen_common a ~locs ~stubname ~cfunc_value:(`Fun cfunc) ~release_runtime_lock
    ~noalloc ~return_errno

let gen_value a ~stubname ~value =
  (* It is safe for noalloc usage, as long as the user doesn't pass a function
     call or similar to it. If he passes a macro, he deserves all errors that
     might occur :) *)
  let noalloc = U.safe_ascii_only value = value in
  gen_common a ~locs:[] ~stubname ~return_errno:false
    ~cfunc_value:(`Value value) ~release_runtime_lock:false ~noalloc

let rec is_void : type a. a typ -> bool =
  let open Ctypes_static in
  function
  | Void -> true
  | View { ty; _ } -> is_void ty
  | Primitive _ -> false
  | Array _ -> false
  | Bigarray _ -> false
  | Pointer _ -> false
  | Abstract _ -> false
  | Struct _ -> false
  | Union _ -> false
  | Funptr _ -> false
  | OCaml _ -> false

module Inline = struct
  open Inline_lexer

  let rec tokens accu lexbuf =
    let x = token lexbuf in
    match x with
    | Textend -> accu
    | Variable _ | Literal _ -> tokens (x :: accu) lexbuf

  let replace_vars htl_names s =
    let l = tokens [] (Lexing.from_string s) in
    let htl_names_used = Hashtbl.create 10 in
    let f acc elem =
      match elem with
      | Literal s -> s :: acc
      | Textend -> acc
      | Variable (name, _poss, _pose) ->
        let name' =
          match CCHashtbl.Poly.get htl_names name with
          | None ->
            (* TODO: use _pos for better error messages *)
            error "parameter %S not defined" name
          | Some x -> x
        in
        Hashtbl.replace htl_names_used name ();
        name' :: acc
    in
    let s = List.fold_left ~init:[] l ~f in
    (s, Hashtbl.length htl_names <> Hashtbl.length htl_names_used)

  let rec extract :
      type a. a Ctypes.fn -> 'b -> string * (string -> string) list =
    let open Ctypes_static in
    fun t locs ->
      with_loc locs @@ fun locs ->
      match t with
      | Returns a -> (string_of_typ_exn a, [])
      | Function (a, b) ->
        let f s = string_of_typ_exn ~name:s a in
        let r, l = extract b locs in
        (r, f :: l)
end

let build_inline_fun fn ~c_name ~c_body ~locs ~noalloc el =
  let el_len = List.length el in
  let htl_names = Hashtbl.create el_len in
  let fun_is_void = ref false in
  let l =
    List.mapi el ~f:(fun i (label, _) ->
        let r =
          match label with
          | Asttypes.Nolabel ->
            let is_void_fn : type a. a Ctypes.fn -> bool = function
              | Ctypes_static.Function (a, _) -> is_void a
              | Ctypes_static.Returns _ -> false
            in
            if el_len = 1 && is_void_fn fn then (
              fun_is_void := true;
              "$dummy$" )
            else U.error "inline code requires named parameters"
          | Asttypes.Optional _ -> assert false
          | Asttypes.Labelled s -> s
        in
        if Hashtbl.mem htl_names r then U.error "labels must be unique";
        let s =
          Printf.sprintf "%svar%d_%s" Myconst.private_prefix i
            (U.safe_ascii_only r)
        in
        Hashtbl.replace htl_names r s;
        s)
  in
  let fn, lt = funptr_transform fn in
  if noalloc = false then
    check_no_ocaml fn []
      "passing OCaml values to inline code is only supported for noalloc functions";
  if CCString.for_all (function ' ' | '\n' | '\t' -> false | _ -> true) c_body
  then
    (* avoid muddling `external foo : ...` and `external%c foo : ... ` *)
    error "function code doesn't look like inline code";
  let ret_type, param_i = Inline.extract fn locs in
  let buf = Buffer.create (256 + String.length c_body + String.length c_name) in
  List.iter lt ~f:(Buffer.add_string buf);
  Printf.bprintf buf "static %s %s(" ret_type c_name;
  List.iteri2 param_i l ~f:(fun i f n ->
      if i <> 0 then Buffer.add_string buf ", ";
      if !fun_is_void then Buffer.add_string buf "void"
      else Buffer.add_string buf @@ f n);
  Buffer.add_string buf "){\n";
  let ls, unused_vars =
    try Inline.replace_vars htl_names c_body
    with Inline_lexer.Bad_expander ->
      error "not escaped '$' in inline source code"
  in
  if unused_vars && !fun_is_void = false then
    error "not all labelled parameters have been used";
  List.iter ls ~f:(Buffer.add_string buf);
  Buffer.add_string buf "\n}\n";
  Buffer.contents buf

module Callback = struct
  let extract =
    let open Ctypes_static in
    fun a fun_name ->
      let i = ref 0 in
      let name () =
        let res = Printf.sprintf "%s%x" Myconst.private_prefix !i in
        incr i;
        res
      in
      let rec extract : type a. a Ctypes.fn -> ret list * param * string =
        function
        | Returns t ->
          let fun_typed = string_of_typ_exn ~name:fun_name t in
          let ocaml_param = name () in
          let c_var = name () in
          let c =
            pinfo t ~all_float:false ~user_noalloc:false ~c_var ~ocaml_param
          in
          ([], c, fun_typed)
        | Function (a, b) ->
          let ocaml_ret_var = name () in
          let c_rvar = name () in
          let decl_rvar () = string_of_typ_exn ~name:c_rvar a in
          let r =
            ret_info a ~all_float:false ~user_noalloc:false ~ocaml_ret_var
              ~c_rvar ~decl_rvar
          in
          let c, l, s = extract b in
          (r :: c, l, s)
      in
      extract a
end

let gen_callback_fun fn mof =
  let module M = Marshal_types in
  let bind_name = mof.M.cb_binding_name in
  let init_fun = mof.M.cb_init_fun in
  let acquire_runtime = mof.M.cb_acquire_runtime in
  let thread_registration = mof.M.cb_thread_registration in
  let callback_name = U.safe_cname ~prefix:bind_name in
  let fn, l_typedefs = funptr_transform fn in
  let params, ret, typed_fun_name = Callback.extract fn callback_name in
  let params_length = List.length params in
  let global_callback_var = U.safe_cname ~prefix:("var " ^ bind_name) in
  if params_length > 1 && List.exists ~f:(fun s -> s.r_typ = Rvoid) params then
    error
      "you can't pass void as parameter to a function with two or more parameters";
  check_no_ocaml fn []
    "OCaml values are not supported as parameters for callbacks";
  let buf = Buffer.create 2048 in
  if thread_registration then
    Buffer.add_string buf
      {|
#ifndef PPX_CSTUBS_CTYPES_THREAD_REGISTER_DECLARED
#define PPX_CSTUBS_CTYPES_THREAD_REGISTER_DECLARED 1
#ifdef __cplusplus
extern "C" {
#endif
extern int ( *ctypes_thread_register)(void);
#ifdef __cplusplus
}
#endif
#endif
|};
  Printf.bprintf buf "static value %s = Val_unit;\n" global_callback_var;
  List.iter l_typedefs ~f:(Buffer.add_string buf);
  Buffer.add_string buf "DISABLE_CONST_WARNINGS_PUSH();\n";
  (* function prototype and function start *)
  for i = 0 to 1 do
    Buffer.add_string buf "static ";
    Buffer.add_string buf typed_fun_name;
    Buffer.add_char buf '(';
    List.iteri params ~f:(fun i s ->
        if i <> 0 then Buffer.add_string buf ", ";
        if s.r_typ <> Rvoid then Buffer.add_string buf @@ s.decl_rvar ()
        else Buffer.add_string buf "void");
    Buffer.add_char buf ')';
    if i = 0 then Buffer.add_string buf ";\n\n"
    else Buffer.add_string buf " {\n"
  done;
  if thread_registration then
    Buffer.add_string buf "  ctypes_thread_register();\n";
  if acquire_runtime then
    Buffer.add_string buf "  caml_leave_blocking_section();\n";
  let alloc_params =
    List.filter params ~f:(fun s ->
        match s.r_noalloc_typ with
        | Noalloc_never | Noalloc_opt -> true
        | Noalloc_always -> false)
  in
  ( if List.length alloc_params > 6 then (
    Buffer.add_string buf "  CAMLparam0();\n";
    Printf.bprintf buf "  CAMLlocalN(%sbuf, %d);\n" Myconst.private_prefix
      params_length;
    List.iteri params ~f:(fun i s ->
        Printf.bprintf buf "  %sbuf[%d] = %s;\n" Myconst.private_prefix i
        @@ s.inj_alloc ());
    Buffer.add_string buf "  CAMLdrop;\n";
    Printf.bprintf buf "  value %s = caml_callbackN(%s, %d, %sbuf);\n"
      ret.ocaml_param global_callback_var params_length Myconst.private_prefix )
  else
    let g p s =
      Printf.bprintf buf "  %s%s = %s;\n" p s.ocaml_ret_var @@ s.inj_alloc ()
    in
    let g_val = g "value " in
    ( match alloc_params with
    | [] -> ()
    | [ hd ] -> g_val hd
    | hd :: tl ->
      Buffer.add_string buf "  CAMLparam0();\n";
      ( match tl with
      | [ s1 ] -> Printf.bprintf buf "  CAMLlocal1(%s);\n" s1.ocaml_ret_var
      | [ s1; s2 ] ->
        Printf.bprintf buf "  CAMLlocal2(%s, %s);\n" s1.ocaml_ret_var
          s2.ocaml_ret_var
      | [ s1; s2; s3 ] ->
        Printf.bprintf buf "  CAMLlocal3(%s, %s, %s);\n" s1.ocaml_ret_var
          s2.ocaml_ret_var s3.ocaml_ret_var
      | [ s1; s2; s3; s4 ] ->
        Printf.bprintf buf "  CAMLlocal4(%s, %s, %s, %s);\n" s1.ocaml_ret_var
          s2.ocaml_ret_var s3.ocaml_ret_var s4.ocaml_ret_var
      | [ s1; s2; s3; s4; s5 ] ->
        Printf.bprintf buf "  CAMLlocal5(%s, %s, %s, %s, %s);\n"
          s1.ocaml_ret_var s2.ocaml_ret_var s3.ocaml_ret_var s4.ocaml_ret_var
          s5.ocaml_ret_var
      | [] | _ :: _ -> assert false );
      List.iter tl ~f:(g "");
      g_val hd;
      Buffer.add_string buf "  CAMLdrop;\n" );
    if params_length > 3 then (
      Printf.bprintf buf "  value %sbuf[%d];\n" Myconst.private_prefix
        params_length;
      List.iteri params ~f:(fun i s ->
          match s.r_noalloc_typ with
          | Noalloc_never | Noalloc_opt ->
            Printf.bprintf buf "  %sbuf[%d] = %s;\n" Myconst.private_prefix i
              s.ocaml_ret_var
          | Noalloc_always ->
            Printf.bprintf buf "  %sbuf[%d] = %s;\n" Myconst.private_prefix i
            @@ s.inj_alloc ());
      Printf.bprintf buf "  value %s = caml_callbackN(%s, %d, %sbuf);\n"
        ret.ocaml_param global_callback_var params_length Myconst.private_prefix
      )
    else (
      List.iter params ~f:(fun s ->
          match s.r_noalloc_typ with
          | Noalloc_never | Noalloc_opt -> ()
          | Noalloc_always -> g_val s);
      Printf.bprintf buf "  value %s = " ret.ocaml_param;
      match params with
      | [ s ] ->
        Printf.bprintf buf "caml_callback(%s, %s);\n" global_callback_var
          s.ocaml_ret_var
      | [ s1; s2 ] ->
        Printf.bprintf buf "caml_callback2(%s, %s, %s);\n" global_callback_var
          s1.ocaml_ret_var s2.ocaml_ret_var
      | [ s1; s2; s3 ] ->
        Printf.bprintf buf "caml_callback3(%s, %s, %s, %s);\n"
          global_callback_var s1.ocaml_ret_var s2.ocaml_ret_var s3.ocaml_ret_var
      | [] | _ :: _ -> assert false ) );
  if ret.typ = Rvoid then Printf.bprintf buf "  (void)%s;\n" ret.ocaml_param
  else (
    Buffer.add_string buf "  ";
    Buffer.add_string buf @@ ret.prj_alloc ();
    Buffer.add_char buf '\n' );
  if acquire_runtime then
    Buffer.add_string buf "  caml_enter_blocking_section();\n";
  if ret.typ <> Rvoid then Printf.bprintf buf "  return %s;\n" ret.c_var;
  Buffer.add_string buf "}\nDISABLE_CONST_WARNINGS_POP();\n";
  Printf.bprintf buf
    {|
#ifdef __cplusplus
extern "C" {
#endif
value %s (value);
#ifdef __cplusplus
}
#endif
value %s (value p) {
  if ( %s != Val_unit ) {
    caml_failwith("API abuse ppx_cstubs: OCaml function pointer not static");
  }
  %s = p;
  caml_register_generational_global_root(&%s);
  return (CTYPES_FROM_PTR(&%s));
}

|}
    init_fun init_fun global_callback_var global_callback_var
    global_callback_var callback_name;
  Buffer.contents buf
