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

open Ctypes
module List = CCListLabels

let error = Std.Util.error

(* structs need a name and I can't print bigarrays ... *)
let rec check_printable : type a. a typ -> bool =
  let open Ctypes_static in
  function
  | Struct {tag = ""; _} ->
    error "passing or returning unnamed structs is not supported"
  | Struct _ -> true
  | Union {utag = ""; _} ->
    error "passing or returning unnamed unions is not supported"
  | Union _ -> true
  | View {format_typ = Some _; _} -> true
  | View {ty; _} -> check_printable ty
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

type ret =
  { is_rvoid : bool
  ; ocaml_ret_var : string
  ; c_rvar : string
  ; decl_rvar : unit -> string
  ; inj_noalloc_possible : bool
  ; inj_alloc : unit -> string
  ; inj_noalloc : unit -> string
  ; inj_byte_noalloc : string -> string
  ; (* caml_copy_double(call_native(..,.. *)
    ibyte_native_accessor_differ : bool
  ; inative_ret_type : string
        (* if something else than value should be used, necessary to support
           passing unboxed values *) }

let ret_info_prim : type a.
       a Ctypes_primitive_types.prim
    -> ocaml_ret_var:string
    -> c_rvar:string
    -> decl_rvar:(unit -> string)
    -> ret =
  let open Ctypes_primitive_types in
  fun p ~ocaml_ret_var ~c_rvar ~decl_rvar ->
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
      ( (fun () -> Printf.sprintf "%s = %s(%s);" ocaml_ret_var s c_rvar)
      , (fun () -> Printf.sprintf "%s = %s;" ocaml_ret_var c_rvar)
      , fun var -> Printf.sprintf "%s(%s)" s var )
    in
    let inj_noalloc_impossible () =
      { is_rvoid = false
      ; ocaml_ret_var
      ; c_rvar
      ; decl_rvar
      ; inj_noalloc_possible = false
      ; inj_alloc
      ; inj_noalloc = inj_alloc
      ; inj_byte_noalloc = Std.identity
      ; ibyte_native_accessor_differ = false
      ; inative_ret_type = "value" }
    in
    let inj_float () =
      { is_rvoid = false
      ; ocaml_ret_var
      ; c_rvar
      ; decl_rvar
      ; inj_noalloc_possible = true
      ; inj_alloc
      ; inj_noalloc = inj_noalloc'
      ; inj_byte_noalloc = to_value
      ; ibyte_native_accessor_differ = true
      ; inative_ret_type = "double" }
    in
    let inj_camlint () =
      { is_rvoid = false
      ; ocaml_ret_var
      ; c_rvar
      ; decl_rvar
      ; inj_noalloc_possible = true
      ; inj_alloc
      ; inj_noalloc = inj_noalloc'
      ; inj_byte_noalloc = to_value
      ; ibyte_native_accessor_differ = true
      ; inative_ret_type = "intnat" }
    in
    let inj_intalias () =
      { is_rvoid = false
      ; ocaml_ret_var
      ; c_rvar
      ; decl_rvar
      ; inj_noalloc_possible = true
      ; inj_alloc
      ; inj_noalloc = inj_alloc
      ; inj_byte_noalloc = Std.identity
      ; ibyte_native_accessor_differ = false
      ; inative_ret_type = "value" }
    in
    let inj_intb ctype =
      { is_rvoid = false
      ; ocaml_ret_var
      ; c_rvar
      ; decl_rvar
      ; inj_noalloc_possible = true
      ; inj_alloc
      ; inj_noalloc = inj_noalloc'
      ; inj_byte_noalloc = to_value
      ; ibyte_native_accessor_differ = true
      ; inative_ret_type = ctype }
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

let rec ret_info : type a.
       a typ
    -> user_noalloc:bool
    -> ocaml_ret_var:string
    -> c_rvar:string
    -> decl_rvar:(unit -> string)
    -> ret =
  let open Ctypes_static in
  let error s = error "cstubs does not support returning %s" s in
  fun t ~user_noalloc ~ocaml_ret_var ~c_rvar ~decl_rvar ->
    let pptr () =
      { is_rvoid = false
      ; ocaml_ret_var
      ; c_rvar
      ; decl_rvar
      ; inj_noalloc_possible = true
      ; inj_alloc =
          (fun () ->
            Printf.sprintf "%s = CTYPES_FROM_PTR(%s);" ocaml_ret_var c_rvar )
      ; inj_noalloc =
          (fun () -> Printf.sprintf "%s = (intnat)(%s);" ocaml_ret_var c_rvar)
      ; inj_byte_noalloc = Printf.sprintf "CTYPES_FROM_PTR(%s)"
      ; ibyte_native_accessor_differ = true
      ; inative_ret_type = "intnat" }
    in
    let standard ?(is_void = false) ~inj_noalloc_possible inj =
      { is_rvoid = is_void
      ; ocaml_ret_var
      ; c_rvar
      ; decl_rvar
      ; inj_noalloc_possible
      ; inj_alloc = inj
      ; inj_noalloc = inj
      ; inj_byte_noalloc = Std.identity
      ; ibyte_native_accessor_differ = false
      ; inative_ret_type = "value" }
    in
    let cp () =
      let inj () =
        Printf.sprintf "%s = ctypes_copy_bytes(&(%s),sizeof(%s));"
          ocaml_ret_var c_rvar c_rvar
      in
      standard ~inj_noalloc_possible:false inj
    in
    match t with
    | Void ->
      let inj () = Printf.sprintf "%s = Val_unit;" ocaml_ret_var in
      standard ~inj_noalloc_possible:true ~is_void:true inj
    | Primitive p -> ret_info_prim p ~ocaml_ret_var ~c_rvar ~decl_rvar
    | Pointer _ -> pptr ()
    | Funptr _ -> pptr ()
    | Struct _ -> cp ()
    | Union _ -> cp ()
    | Abstract _ -> error "values of abstract type"
    | View {format_typ = Some ft; _} when ft == Evil_hack.format_typ ->
      let inj () = Printf.sprintf "%s = %s;" ocaml_ret_var c_rvar in
      standard ~inj_noalloc_possible:user_noalloc inj
    | View {ty; _} ->
      ret_info ty ~user_noalloc ~ocaml_ret_var ~c_rvar ~decl_rvar
    | Array _ -> error "arrays"
    | Bigarray _ -> error "bigarrays"
    | OCaml _ -> error "ocaml references as return values"

type param =
  { is_void : bool
  ; ocaml_param : string
  ; c_var : string
  ; runtime_protect : bool
  ; noalloc_possible : bool
  ; prj_alloc : unit -> string
  ; prj_noalloc : unit -> string
  ; prj_byte_noalloc : string -> string
  ; (* call_native(Long_val(a[0]),...) *)
    byte_native_accessor_differ : bool
  ; native_param_type : string
        (* if something else than value should be used, necessary to support
           passing unboxed values *) }

let pinfo_prim : type a.
       a Ctypes_primitive_types.prim
    -> ctype:string
    -> c_var:string
    -> ocaml_param:string
    -> param =
  let open Ctypes_primitive_types in
  fun p ~ctype ~c_var ~ocaml_param ->
    let f s =
      ( (fun () -> Printf.sprintf "%s %s = %s(%s);" ctype c_var s ocaml_param)
      , fun g -> Printf.sprintf "%s(%s)" s g )
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
    let int_t t =
      { is_void = false
      ; ocaml_param
      ; c_var
      ; runtime_protect = false
      ; prj_alloc
      ; noalloc_possible = true
      ; prj_noalloc = fprj_noalloc t
      ; prj_byte_noalloc = prj_byte_noalloc'
      ; byte_native_accessor_differ = true
      ; native_param_type = t }
    in
    let noalloc_int () = int_t "intnat" in
    let standard () =
      { is_void = false
      ; ocaml_param
      ; c_var
      ; runtime_protect = false
      ; prj_alloc
      ; noalloc_possible = true
      ; prj_noalloc = prj_alloc
      ; prj_byte_noalloc = Std.identity
      ; byte_native_accessor_differ = false
      ; native_param_type = "value" }
    in
    let float () =
      { is_void = false
      ; ocaml_param
      ; c_var
      ; runtime_protect = false
      ; prj_alloc
      ; noalloc_possible = true
      ; prj_noalloc = fprj_noalloc "double"
      ; prj_byte_noalloc = prj_byte_noalloc'
      ; byte_native_accessor_differ = true
      ; native_param_type = "double" }
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

let rec pinfo : type a b.
       a typ
    -> b typ
    -> user_noalloc:bool
    -> c_var:string
    -> ocaml_param:string
    -> param =
  let module C = Ctypes_static in
  let error s = error "cstubs does not support passing %s" s in
  fun p orig ~user_noalloc ~c_var ~ocaml_param ->
    let standard ?(runtime_protect = true) ?(noalloc_possible = true)
        ?(is_void = false) f =
      { is_void
      ; ocaml_param
      ; c_var
      ; runtime_protect
      ; prj_alloc = f
      ; noalloc_possible
      ; prj_noalloc = f
      ; prj_byte_noalloc = Std.identity
      ; byte_native_accessor_differ = false
      ; native_param_type = "value" }
    in
    let stru () =
      standard (fun () ->
          let cast_name = string_of_typ_exn (ptr orig) in
          Printf.sprintf
            "%s = *(%s)(CTYPES_ADDR_OF_FATPTR(Field(Field(%s,0),0)));"
            (string_of_typ_exn ~name:c_var orig)
            cast_name ocaml_param )
    in
    let f ?noalloc_possible s =
      standard ?noalloc_possible (fun () ->
          Printf.sprintf "%s = %s(%s);"
            (string_of_typ_exn ~name:c_var orig)
            s ocaml_param )
    in
    let str () = f "CTYPES_PTR_OF_OCAML_STRING" in
    let hptr noalloc_possible =
      standard ~noalloc_possible (fun () ->
          Printf.sprintf "%s = CTYPES_ADDR_OF_FATPTR(Field(%s,0));"
            (string_of_typ_exn ~name:c_var orig)
            ocaml_param )
    in
    match p with
    | C.Void ->
      standard ~runtime_protect:false ~is_void:true (fun () ->
          Printf.sprintf "(void)%s;" ocaml_param )
    | C.Primitive x ->
      let ct = string_of_typ_exn p in
      pinfo_prim ~ctype:ct x ~c_var ~ocaml_param
    | C.Array (_, _) -> error "arrays"
    | C.Bigarray _ -> error "bigarrays"
    | C.Pointer _ -> hptr true
    | C.Abstract _ -> error "values of abstract type"
    | C.View {C.format_typ = Some ft; _} when ft == Evil_hack.format_typ ->
      standard ~noalloc_possible:user_noalloc ~runtime_protect:false (fun () ->
          Printf.sprintf "value %s = %s;" c_var ocaml_param )
    | C.View {C.ty; _} -> pinfo ty p ~user_noalloc ~c_var ~ocaml_param
    | C.Struct _ -> stru ()
    | C.Union _ -> stru ()
    | C.Funptr _ -> hptr false
    | C.OCaml C.String -> str ()
    | C.OCaml C.Bytes -> str ()
    | C.OCaml C.FloatArray -> f "CTYPES_PTR_OF_FLOAT_ARRAY"

let pinfo p = pinfo p p

let with_loc locs f =
  let loc, locs =
    match locs with [] -> (!Ast_helper.default_loc, []) | hd :: tl -> (hd, tl)
  in
  Std.Util.with_loc loc @@ fun () -> f locs

let extract =
  let open Ctypes_static in
  fun ~user_noalloc ~locs a ->
    let i = ref 0 in
    let name () =
      let res = Printf.sprintf "ppxc__%x" !i in
      incr i ;
      res
    in
    let rec extract : type a.
        a Ctypes.fn -> Location.t list -> ret * param list =
     fun t locs ->
      with_loc locs
      @@ fun locs ->
      match t with
      | Returns t ->
        let ocaml_ret_var = name () in
        let c_rvar = name () in
        let decl_rvar () = string_of_typ_exn ~name:c_rvar t in
        let r = ret_info t ~user_noalloc ~ocaml_ret_var ~c_rvar ~decl_rvar in
        (r, [])
      | Function (a, b) ->
        let ocaml_param = name () in
        let c_var = name () in
        let c = pinfo a ~user_noalloc ~c_var ~ocaml_param in
        let r, l = extract b locs in
        (r, c :: l)
    in
    extract a locs

type info =
  { stub_source : string
  ; stub_name : string
  ; stub_name_byte : string option
  ; noalloc : bool
  ; return_errno : bool }

let rec check_no_ocaml : type a. a Ctypes.fn -> Location.t list -> unit =
  let open Ctypes_static in
  fun fn locs ->
    with_loc locs
    @@ fun locs ->
    match fn with
    | Returns _ -> ()
    | Function (a, b) ->
      check_no_ocaml_t locs a ;
      check_no_ocaml b locs

and check_no_ocaml_t : type a. Location.t list -> a Ctypes.typ -> unit =
  let open Ctypes_static in
  fun locs -> function
    | OCaml _ ->
      error "You can't pass OCaml values to C, if you release the runtime lock"
    | Struct _ -> ()
    | Union _ -> ()
    | View {ty; _} -> check_no_ocaml_t locs ty
    | Pointer ty -> check_no_ocaml_t locs ty
    | Funptr _ -> ()
    | Array _ -> ()
    | Bigarray _ -> ()
    | Primitive _ -> ()
    | Void -> ()
    | Abstract _ -> ()

let gen_common a ~locs ~stubname ~cfunc_value ~release_runtime_lock ~noalloc
    ~return_errno =
  let user_noalloc = noalloc in
  let ret, params = extract ~user_noalloc ~locs a in
  let params_length = List.length params in
  if params_length > 1 && List.exists ~f:(fun s -> s.is_void) params then
    error
      {| you can't pass void as parameter to a function with two or more parameters |} ;
  if release_runtime_lock then check_no_ocaml a locs ;
  let native_accessors = Ocaml_config.version () >= (4, 3, 0) in
  (* [@unboxed] and [@untagged] only supported since 4.03.0. No workarounds in
     order to support older versions :D *)
  let noalloc =
    noalloc
    && return_errno = false
    && release_runtime_lock = false
    && List.for_all params ~f:(fun x -> x.noalloc_possible)
    && ret.inj_noalloc_possible
    && native_accessors
  in
  let gen_byte_version =
    params_length > 5
    || ( native_accessors
       && ( List.exists params ~f:(fun x -> x.byte_native_accessor_differ)
          || (ret.ibyte_native_accessor_differ && return_errno = false) ) )
  in
  let buf = Buffer.create 128 in
  let cname_main = stubname in
  let cname_byte = "b" ^ stubname in
  (* first character is always significant. It's not the case for a suffix *)
  let native_ret_type =
    if native_accessors && return_errno = false then ret.inative_ret_type
    else "value"
  in
  Printf.bprintf buf {|#ifdef __cplusplus
extern "C" {
#endif
|} ;
  (* native prototype *)
  Printf.bprintf buf "%s %s(" native_ret_type cname_main ;
  List.iteri params ~f:(fun i p ->
      if i <> 0 then Buffer.add_string buf ", " ;
      if native_accessors then Buffer.add_string buf p.native_param_type
      else Buffer.add_string buf "value" ) ;
  Buffer.add_string buf ");\n" ;
  (* byte prototype *)
  if gen_byte_version then
    if params_length > 5 then
      Printf.bprintf buf "value %s(value *,int);\n" cname_byte
    else (
      Printf.bprintf buf "value %s(" cname_byte ;
      List.iteri params ~f:(fun i _ ->
          if i <> 0 then Buffer.add_string buf ", " ;
          Buffer.add_string buf "value" ) ;
      Buffer.add_string buf ");\n" ) ;
  Printf.bprintf buf {|#ifdef __cplusplus
}
#endif

|} ;
  (* main function header *)
  Printf.bprintf buf "%s %s(" native_ret_type cname_main ;
  List.iteri params ~f:(fun i x ->
      if i <> 0 then Buffer.add_string buf ", " ;
      let t = if native_accessors then x.native_param_type else "value" in
      Printf.bprintf buf "%s %s" t x.ocaml_param ) ;
  Buffer.add_string buf ")  {\n" ;
  let params_with_protection =
    if release_runtime_lock = false && user_noalloc = true then []
    else
      List.filter_map params ~f:(fun x ->
          if x.runtime_protect = false then None else Some x.ocaml_param )
  in
  ( match params_with_protection with
  | [] -> ()
  | [h] -> Printf.bprintf buf "  CAMLparam1(%s);\n" h
  | [h1; h2] -> Printf.bprintf buf "  CAMLparam2(%s,%s);\n" h1 h2
  | [h1; h2; h3] -> Printf.bprintf buf "  CAMLparam3(%s,%s,%s);\n" h1 h2 h3
  | [h1; h2; h3; h4] ->
    Printf.bprintf buf "  CAMLparam4(%s,%s,%s,%s);\n" h1 h2 h3 h4
  | h1 :: h2 :: h3 :: h4 :: h5 :: tl ->
    Printf.bprintf buf "  CAMLparam5(%s,%s,%s,%s,%s);\n" h1 h2 h3 h4 h5 ;
    let rec iter = function
      | [] -> ()
      | [h] -> Printf.bprintf buf "  CAMLxparam1(%s);\n" h
      | [h1; h2] -> Printf.bprintf buf "  CAMLxparam2(%s,%s);\n" h1 h2
      | [h1; h2; h3] ->
        Printf.bprintf buf "  CAMLxparam3(%s,%s,%s);\n" h1 h2 h3
      | [h1; h2; h3; h4] ->
        Printf.bprintf buf "  CAMLxparam4(%s,%s,%s,%s);\n" h1 h2 h3 h4
      | h1 :: h2 :: h3 :: h4 :: h5 :: tl ->
        Printf.bprintf buf "  CAMLxparam5(%s,%s,%s,%s,%s);\n" h1 h2 h3 h4 h5 ;
        iter tl
    in
    iter tl ) ;
  (* declare variables, convert ocaml values to native ctypes *)
  List.iter params ~f:(fun x ->
      Buffer.add_string buf "  " ;
      let t = if native_accessors then x.prj_noalloc () else x.prj_alloc () in
      Buffer.add_string buf t ;
      Buffer.add_char buf '\n' ) ;
  (* declare variables for result *)
  if ret.is_rvoid = false then (
    Printf.bprintf buf "  %s %s;\n" native_ret_type ret.ocaml_ret_var ;
    Printf.bprintf buf "  %s;\n" @@ ret.decl_rvar () ) ;
  if release_runtime_lock then
    Printf.bprintf buf "  caml_release_runtime_system();\n" ;
  if return_errno then Printf.bprintf buf "  errno = 0;\n" ;
  if ret.is_rvoid = false then Printf.bprintf buf "  %s = " ret.c_rvar
  else Buffer.add_string buf "  " ;
  ( match cfunc_value with
  | `Fun cfunc ->
    Buffer.add_string buf cfunc ;
    Buffer.add_char buf '(' ;
    List.iteri params ~f:(fun i x ->
        if i <> 0 then Buffer.add_string buf ", " ;
        if x.is_void = false then Buffer.add_string buf x.c_var ) ;
    Buffer.add_string buf ");\n"
  | `Value v -> Printf.bprintf buf "&%s;\n" v ) ;
  if release_runtime_lock then
    Printf.bprintf buf "  caml_acquire_runtime_system();\n" ;
  if ret.is_rvoid = false then (
    Buffer.add_string buf "  " ;
    let s =
      if native_accessors && return_errno = false then ret.inj_noalloc ()
      else ret.inj_alloc ()
    in
    Buffer.add_string buf s ;
    Buffer.add_char buf '\n' ) ;
  let return_val = if ret.is_rvoid then "Val_unit" else ret.ocaml_ret_var in
  let return_val =
    if return_errno = false then return_val
    else Printf.sprintf "ctypes_pair_with_errno(%s)" return_val
  in
  if params_with_protection = [] then
    Printf.bprintf buf "  return %s;" return_val
  else if native_ret_type = "value" then
    Printf.bprintf buf "  CAMLreturn(%s);" return_val
  else Printf.bprintf buf "  CAMLreturnT(%s,%s);" native_ret_type return_val ;
  Buffer.add_string buf "\n}\n" ;
  if gen_byte_version then (
    if params_length > 5 then
      Printf.bprintf buf "value %s(value *a,int n)\n{\n  return(" cname_byte
    else (
      Printf.bprintf buf "value %s(value a0" cname_byte ;
      ( match params with
      | [] -> ()
      | _ :: tl ->
        List.iteri tl ~f:(fun i _ -> Printf.bprintf buf ", value a%d" (succ i))
      ) ;
      Buffer.add_string buf ")  {\n  return(" ) ;
    let l =
      List.mapi params ~f:(fun i x ->
          let t =
            if params_length < 6 then Printf.sprintf "a%d" i
            else Printf.sprintf "a[%d]" i
          in
          if native_accessors then x.prj_byte_noalloc t else t )
    in
    let t = String.concat ", " l in
    let t = String.concat "" ["("; cname_main; "("; t; ")"; ")"] in
    let t = if native_accessors then ret.inj_byte_noalloc t else t in
    Buffer.add_string buf t ;
    Buffer.add_string buf ");\n}\n" ) ;
  { stub_source = Buffer.contents buf
  ; stub_name = cname_main
  ; stub_name_byte = (if gen_byte_version then Some cname_byte else None)
  ; noalloc
  ; return_errno }

let gen_fun a ~locs ~stubname ~cfunc ~release_runtime_lock ~noalloc
    ~return_errno =
  gen_common a ~locs ~stubname ~cfunc_value:(`Fun cfunc) ~release_runtime_lock
    ~noalloc ~return_errno

let gen_value a ~stubname ~value =
  (* It is safe for noalloc usage, as long as the user doesn't pass a function
     call or similar to it. If he passes a macro, he deserves all errors that
     might occur :) *)
  let noalloc = Std.Util.safe_ascii_only value = value in
  gen_common a ~locs:[] ~stubname ~return_errno:false
    ~cfunc_value:(`Value value) ~release_runtime_lock:false ~noalloc

let rec is_void : type a. a typ -> bool =
  let open Ctypes_static in
  function
  | Void -> true
  | View {ty; _} -> is_void ty
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

  let replace_vars names s =
    let l = tokens [] (Lexing.from_string s) in
    let htl_names_used = Hashtbl.create 10 in
    let htl_names = CCHashtbl.Poly.of_list names in
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
        Hashtbl.replace htl_names_used name () ;
        name' :: acc
    in
    let s = List.fold_left ~init:[] l ~f in
    (s, Hashtbl.length htl_names <> Hashtbl.length htl_names_used)

  (* main checks in gen_fun, only additional restrictions are tested here.
     TODO: funptr *)
  let rec check : type a. a typ -> noalloc:bool -> unit =
    let open Ctypes_static in
    fun a ~noalloc ->
      match a with
      | Struct _ -> ()
      | Union _ -> ()
      | View {ty; _} -> check ~noalloc ty
      | Pointer ty -> check ~noalloc ty
      | Funptr _ -> error "function pointers are not supported in inline code"
      | Array _ -> error "arrays are not supported in inline code"
      | Bigarray _ -> error "bigarrays are not supported in inline code"
      | OCaml _ ->
        (* a pointer into the OCaml heap might become invalid, if the garbage
           collector is triggered. Therefore I only support it for the noalloc
           case. *)
        if noalloc = false then
          error
            {|passing OCaml values to inline code is only supported for noalloc functions|}
      | Primitive _ -> ()
      | Void -> ()
      | Abstract _ -> ()

  (* TODO and check_fn : type b. b fn -> bool = let open Ctypes_static in
     function | Returns a -> check a | Function (a, b) -> check a; check_fn b *)

  let rec extract : type a.
         a Ctypes.fn
      -> noalloc:bool
      -> 'b
      -> string * (bool * (string -> string)) list =
    let open Ctypes_static in
    fun t ~noalloc locs ->
      with_loc locs
      @@ fun locs ->
      match t with
      | Returns a ->
        check ~noalloc a ;
        (string_of_typ_exn a, [])
      | Function (a, b) ->
        check ~noalloc a ;
        let is_void = is_void a in
        let f s = string_of_typ_exn ~name:s a in
        let r, l = extract ~noalloc b locs in
        (r, (is_void, f) :: l)
end

let build_inline_fun fn ~c_name ~c_body ~locs ~noalloc l =
  let ret_type, param_i = Inline.extract ~noalloc fn locs in
  let buf =
    Buffer.create (256 + String.length c_body + String.length c_name)
  in
  Printf.bprintf buf "static %s %s(" ret_type c_name ;
  List.iteri2 param_i l ~f:(fun i (is_void, f) (_, n) ->
      if i <> 0 then Buffer.add_string buf ", " ;
      if is_void then Buffer.add_string buf "void"
      else Buffer.add_string buf @@ f n ) ;
  Buffer.add_string buf "){\n" ;
  let ls, unused_vars =
    try Inline.replace_vars l c_body
    with Inline_lexer.Bad_expander ->
      error "not escaped '$' in inline source code"
  in
  let is_void = List.exists ~f:fst param_i in
  if unused_vars && is_void = false then
    error "not all labelled parameters have been used" ;
  if
    is_void
    && CCString.for_all
         (function ' ' | '\n' | '\t' -> false | _ -> true)
         c_body
  then
    (* avoid muddling `external foo : ...` and `external%c foo : ... ` *)
    error "function code doesn't look like inline code" ;
  List.iter ls ~f:(Buffer.add_string buf) ;
  Buffer.add_string buf "\n}\n" ;
  Buffer.contents buf
