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

open Mparsetree.Ast_cur
open Ast_helper
open Ctypes
module List = CCListLabels
module CS = Ctypes_static
module U = Std.Util

let create_param_name () =
  let i = ref 0 in
  fun () ->
    let loc = !Ast_helper.default_loc in
    let name = U.unsuffixed_file_name () in
    let s =
      Printf.sprintf "%s%d_%s_%x" Myconst.private_prefix !i name
        loc.Location.loc_start.Lexing.pos_lnum
    in
    incr i;
    s

let mk_attrs = function
  | None -> []
  | Some s -> [ Attr.mk (U.mk_loc s) (PStr []) ]

let mk_ex ?attrs s = Exp.ident ~attrs:(mk_attrs attrs) (U.mk_lid s)

let mk_ex_pat s = (mk_ex s, U.mk_pat s)

let mk_typ ?attrs ?(l = []) s =
  Typ.constr ~attrs:(mk_attrs attrs) (U.mk_lid s) l

let prim_supports_attr :
    type a. a Ctypes_primitive_types.prim -> cinfo:Gen_c.info -> bool =
  let open Ctypes_primitive_types in
  fun t ~cinfo ->
    match Ctypes_primitive_types.ml_prim t with
    | ML_float -> Ocaml_config.version () >= (4, 3, 0) || cinfo.Gen_c.float
    | ML_int -> Ocaml_config.version () >= (4, 3, 0)
    | ML_int32 -> Ocaml_config.version () >= (4, 3, 0)
    | ML_int64 -> Ocaml_config.version () >= (4, 3, 0)
    | ML_nativeint -> Ocaml_config.version () >= (4, 3, 0)
    | ML_char -> false
    | ML_bool -> false
    | ML_complex -> false
    | ML_llong -> false
    | ML_long -> false
    | ML_sint -> false
    | ML_size_t -> false
    | ML_uchar -> false
    | ML_uint -> false
    | ML_uint16 -> false
    | ML_uint32 -> false
    | ML_uint64 -> false
    | ML_uint8 -> false
    | ML_ullong -> false
    | ML_ulong -> false
    | ML_ushort -> false
    | ML_ldouble -> false
    | ML_complexld -> false

let ident_of_ml_prim :
    type a. no_attr:bool -> a Ctypes_primitive_types.ml_prim -> core_type =
  let open Ctypes_primitive_types in
  fun ~no_attr t ->
    let f ~a t =
      if no_attr = false && Ocaml_config.version () >= (4, 3, 0) then
        mk_typ ~attrs:a t
      else mk_typ t
    in
    let loc = !Ast_helper.default_loc in
    match t with
    | ML_char -> [%type: char]
    | ML_bool -> [%type: bool]
    | ML_complex -> [%type: Complex.t]
    | ML_float -> f ~a:"unboxed" "float"
    | ML_int -> f ~a:"untagged" "int"
    | ML_int32 -> f ~a:"unboxed" "int32"
    | ML_int64 -> f ~a:"unboxed" "int64"
    | ML_llong -> [%type: Signed.llong]
    | ML_long -> [%type: Signed.long]
    | ML_sint -> [%type: Signed.sint]
    | ML_nativeint -> f ~a:"unboxed" "nativeint"
    | ML_size_t -> [%type: Unsigned.size_t]
    | ML_uchar -> [%type: Unsigned.uchar]
    | ML_uint -> [%type: Unsigned.uint]
    | ML_uint16 -> [%type: Unsigned.uint16]
    | ML_uint32 -> [%type: Unsigned.uint32]
    | ML_uint64 -> [%type: Unsigned.uint64]
    | ML_uint8 -> [%type: Unsigned.uint8]
    | ML_ullong -> [%type: Unsigned.ullong]
    | ML_ulong -> [%type: Unsigned.ulong]
    | ML_ushort -> [%type: Unsigned.ushort]
    | ML_ldouble -> [%type: LDouble.t]
    | ML_complexld -> [%type: ComplexL.t]

let rec typ_of_ctyp : type a. a typ -> 'b =
  let open Ctypes_static in
  fun a ->
    let loc = !Ast_helper.default_loc in
    match a with
    | Void -> (`Complete, [%type: unit])
    | Primitive p ->
      ( `Complete,
        ident_of_ml_prim ~no_attr:true (Ctypes_primitive_types.ml_prim p) )
    | Pointer x ->
      let i, l = typ_of_ctyp x in
      (i, [%type: [%t l] Ctypes.ptr])
    | Funptr _ -> (`Incomplete, [%type: _ Ctypes.static_funptr])
    | Struct _ -> (`Incomplete, [%type: _ Ctypes.structure])
    | Union _ -> (`Incomplete, [%type: _ Ctypes.union])
    | Abstract _ -> (`Incomplete, [%type: _ Ctypes.abstract])
    | View _ -> (`Incomplete, Typ.any ())
    | Array (x, _) ->
      let i, l = typ_of_ctyp x in
      (i, [%type: [%t l] Ctypes.carray])
    | Bigarray _ -> (`Incomplete, Typ.any ())
    | OCaml String -> (`Complete, [%type: string Ctypes.ocaml])
    | OCaml Bytes -> (`Complete, [%type: bytes Ctypes.ocaml])
    | OCaml FloatArray -> (`Complete, [%type: float array Ctypes.ocaml])

let constraint_of_typ t =
  let loc = !Ast_helper.default_loc in
  [%type: [%t snd (typ_of_ctyp t)] Ctypes.typ]

let ml_typ_of_arg_typ ~cinfo t =
  let loc = !Ast_helper.default_loc in
  let sptr () = (`Incomplete, [%type: _ Cstubs_internals.fatptr]) in
  let rec iter : type a. bool -> a typ -> 'b =
    let open Ctypes_static in
    fun inside_view -> function
      | Void when inside_view -> (`Incomplete, Typ.any ())
      | Pointer _ when inside_view -> (`Incomplete, Typ.any ())
      | Funptr _ when inside_view -> (`Incomplete, Typ.any ())
      | Struct _ when inside_view -> (`Incomplete, Typ.any ())
      | Union _ when inside_view -> (`Incomplete, Typ.any ())
      | Abstract _ when inside_view -> (`Incomplete, Typ.any ())
      | OCaml _ when inside_view -> (`Incomplete, Typ.any ())
      | Void -> (`Complete, [%type: unit])
      | Primitive p ->
        if inside_view && prim_supports_attr p ~cinfo = false then
          (`Incomplete, Typ.any ())
        else
          ( `Complete,
            ident_of_ml_prim ~no_attr:false (Ctypes_primitive_types.ml_prim p)
          )
      | Pointer _ -> sptr ()
      | Funptr _ -> (`Incomplete, [%type: _ Cstubs_internals.fatfunptr])
      | Struct _ -> sptr ()
      | Union _ -> sptr ()
      | Abstract _ -> sptr ()
      | View { ty = Struct { tag; _ }; format_typ = Some ft; _ }
        when ft == Evil_hack.format_typ ->
        (`Complete, Marshal.from_string tag 0)
      | View { ty; _ } -> iter true ty
      | Array _ as a ->
        U.error "Unexpected array in an argument type: %s"
          (Ctypes.string_of_typ a)
      | Bigarray _ as a ->
        U.error "Unexpected bigarray in an argument type: %s"
          (Ctypes.string_of_typ a)
      | OCaml String -> (`Complete, [%type: string Ctypes.ocaml])
      | OCaml Bytes -> (`Complete, [%type: bytes Ctypes.ocaml])
      | OCaml FloatArray -> (`Complete, [%type: float array Ctypes.ocaml])
  in
  iter false t

let rec ml_typ_of_return_typ :
    type a. a typ -> inside_view:bool -> cinfo:Gen_c.info -> core_type =
  let open Ctypes_static in
  let mk_ptr cinfo =
    let loc = !Ast_helper.default_loc in
    if cinfo.Gen_c.return_errno = false && Ocaml_config.version () >= (4, 3, 0)
    then [%type: (nativeint[@unboxed])]
    else [%type: nativeint]
  in
  fun t ~inside_view ~cinfo ->
    let loc = !Ast_helper.default_loc in
    match t with
    | Void -> if inside_view then Typ.any () else [%type: unit]
    | Primitive p ->
      if inside_view && prim_supports_attr p ~cinfo = false then Typ.any ()
      else
        ident_of_ml_prim ~no_attr:cinfo.Gen_c.return_errno
          (Ctypes_primitive_types.ml_prim p)
    | Struct _ -> [%type: Cstubs_internals.managed_buffer]
    | Union _ -> [%type: Cstubs_internals.managed_buffer]
    | Abstract _ -> [%type: Cstubs_internals.managed_buffer]
    | Pointer _ -> mk_ptr cinfo
    | Funptr _ -> mk_ptr cinfo
    | View { ty = Struct { tag; _ }; format_typ = Some ft; _ }
      when ft == Evil_hack.format_typ ->
      Marshal.from_string tag 0
    | View { ty; _ } -> ml_typ_of_return_typ ty ~cinfo ~inside_view:true
    | Array _ as a ->
      U.error "Unexpected array type in the return type: %s"
        (Ctypes.string_of_typ a)
    | Bigarray _ as a ->
      U.error "Unexpected bigarray type in the return type: %s"
        (Ctypes.string_of_typ a)
    | OCaml String ->
      U.error "cstubs does not support OCaml strings as return values"
    | OCaml Bytes ->
      U.error "cstubs does not support OCaml bytes values as return values"
    | OCaml FloatArray ->
      U.error "cstubs does not support OCaml float arrays as return values"

let ml_typ_of_return_typ t ~cinfo =
  ml_typ_of_return_typ t ~inside_view:false ~cinfo

let pat_expand_prim : type a. a Ctypes_primitive_types.prim -> pattern =
  let open Ctypes_primitive_types in
  fun a ->
    let loc = !Ast_helper.default_loc in
    match a with
    | Char -> [%pat? Ctypes_primitive_types.Char]
    | Schar -> [%pat? Ctypes_primitive_types.Schar]
    | Uchar -> [%pat? Ctypes_primitive_types.Uchar]
    | Bool -> [%pat? Ctypes_primitive_types.Bool]
    | Short -> [%pat? Ctypes_primitive_types.Short]
    | Int -> [%pat? Ctypes_primitive_types.Int]
    | Long -> [%pat? Ctypes_primitive_types.Long]
    | Llong -> [%pat? Ctypes_primitive_types.Llong]
    | Ushort -> [%pat? Ctypes_primitive_types.Ushort]
    | Sint -> [%pat? Ctypes_primitive_types.Sint]
    | Uint -> [%pat? Ctypes_primitive_types.Uint]
    | Ulong -> [%pat? Ctypes_primitive_types.Ulong]
    | Ullong -> [%pat? Ctypes_primitive_types.Ullong]
    | Size_t -> [%pat? Ctypes_primitive_types.Size_t]
    | Int8_t -> [%pat? Ctypes_primitive_types.Int8_t]
    | Int16_t -> [%pat? Ctypes_primitive_types.Int16_t]
    | Int32_t -> [%pat? Ctypes_primitive_types.Int32_t]
    | Int64_t -> [%pat? Ctypes_primitive_types.Int64_t]
    | Uint8_t -> [%pat? Ctypes_primitive_types.Uint8_t]
    | Uint16_t -> [%pat? Ctypes_primitive_types.Uint16_t]
    | Uint32_t -> [%pat? Ctypes_primitive_types.Uint32_t]
    | Uint64_t -> [%pat? Ctypes_primitive_types.Uint64_t]
    | Camlint -> [%pat? Ctypes_primitive_types.Camlint]
    | Nativeint -> [%pat? Ctypes_primitive_types.Nativeint]
    | Float -> [%pat? Ctypes_primitive_types.Float]
    | Double -> [%pat? Ctypes_primitive_types.Double]
    | LDouble -> [%pat? Ctypes_primitive_types.LDouble]
    | Complex32 -> [%pat? Ctypes_primitive_types.Complex32]
    | Complex64 -> [%pat? Ctypes_primitive_types.Complex64]
    | Complexld -> [%pat? Ctypes_primitive_types.Complexld]

let extract_pointer opt e =
  match e.pexp_desc with
  | Pexp_apply
      ( {
          pexp_desc = Pexp_ident { txt = Ldot (Lident "Ctypes", ptr); loc = _ };
          pexp_attributes = [];
          _;
        },
        [ (Nolabel, ({ pexp_desc = Pexp_ident _; _ } as a)) ] )
    when (ptr = "ptr" && opt = false) || (opt && ptr = "ptr_opt") ->
    Some a
  | _ -> None

let extract_pointer_opt = extract_pointer true

let extract_pointer = extract_pointer false

let cond_expand_prim p inside_view cinfo =
  if
    inside_view
    &&
    match cinfo with None -> false | Some cinfo -> prim_supports_attr ~cinfo p
  then
    let loc = !Ast_helper.default_loc in
    Some [%pat? Ctypes_static.Primitive [%p pat_expand_prim p]]
  else None

type expand_in =
  | In_ptr_bind of pattern * expression * (expression -> expression)
  | In_fptr_bind of pattern * expression * (expression -> expression)
  | In_trans of (expression -> expression)
  | In_ident

let pat_expand_in t ?cinfo ?type_expr param_name =
  let error t =
    let x = Ctypes.string_of_typ t in
    U.error "cstubs does not support passing %s as parameter" x
  in
  let mptr ?(f = Std.identity) w =
    let loc = !Ast_helper.default_loc in
    let n = param_name () in
    let e = U.mk_ident_l [ n ] in
    match w with
    | `Ptr -> In_ptr_bind ([%pat? Ctypes_static.CPointer [%p U.mk_pat n]], e, f)
    | `Fptr ->
      In_fptr_bind ([%pat? Ctypes_static.Static_funptr [%p U.mk_pat n]], e, f)
  in
  let structured () =
    let loc = !Ast_helper.default_loc in
    mptr ~f:(fun e -> [%expr [%e e].Ctypes_static.structured]) `Ptr
  in
  let rec iter : type a. a typ -> ?type_expr:expression -> bool -> 'b =
    let open Ctypes_static in
    fun t ?type_expr inside_view ->
      let loc = !Ast_helper.default_loc in
      let ce p = if inside_view then Some p else None in
      match t with
      | Void -> (None, In_ident)
      | Primitive p -> (cond_expand_prim p inside_view cinfo, In_ident)
      | Pointer _ -> (ce [%pat? Ctypes_static.Pointer _], mptr `Ptr)
      | Funptr _ -> (ce [%pat? Ctypes_static.Funptr _], mptr `Fptr)
      | Struct _ -> (ce [%pat? Ctypes_static.Struct _], structured ())
      | Union _ -> (ce [%pat? Ctypes_static.Union _], structured ())
      | Abstract _ -> (ce [%pat? Ctypes_static.Abstract _], structured ())
      | OCaml _ -> (None, In_ident)
      | View { format_typ = Some ft; _ } when ft == Evil_hack.format_typ ->
        (None, In_ident)
      | View { ty; _ } -> (
        match inline_view ty ?type_expr inside_view with
        | Some s -> s
        | None ->
          let f, p = mk_ex_pat @@ param_name () in
          let pat2, nexpr2 = iter ty true in
          let pat =
            match pat2 with
            | None ->
              [%pat? Ctypes_static.View { Ctypes_static.write = [%p p]; _ }]
            | Some pat2 ->
              [%pat?
                Ctypes_static.View
                  {
                    Ctypes_static.write = [%p p];
                    Ctypes_static.ty = [%p pat2];
                    _;
                  }]
          in
          let h f' e = f' [%expr [%e f] [%e e]] in
          let fexpr =
            match nexpr2 with
            | In_ident -> In_trans (fun e -> [%expr [%e f] [%e e]])
            | In_trans f' -> In_trans (h f')
            | In_ptr_bind (p, e, f') -> In_ptr_bind (p, e, h f')
            | In_fptr_bind (p, e, f') -> In_fptr_bind (p, e, h f')
          in
          (Some pat, fexpr))
      | Array _ -> error t
      | Bigarray _ -> error t
  (* various dirty tricks to reduce the lines of boilerplate code *)
  and inline_view : type a. a typ -> ?type_expr:expression -> bool -> 'c =
   fun tchild ?type_expr inside_view ->
    if inside_view then None
    else
      match (tchild, type_expr) with
      | CS.Pointer _, Some x -> (
        match extract_pointer_opt x with
        | Some orig_expr ->
          let f e =
            let loc = !Ast_helper.default_loc in
            [%expr
              match [%e e] with
              | None -> Ctypes.from_voidp [%e orig_expr] Ctypes.null
              | Some x -> x]
          in
          Some (None, mptr ~f `Ptr)
        | None -> None)
      | _ -> None
  in
  iter t ?type_expr false

let pat_expand_out ?type_expr ?cinfo t param_name =
  let error t =
    let x = Ctypes.string_of_typ t in
    U.error "cstubs does not support %s as return values" x
  in
  let rec iter :
      type a.
      ?type_expr:expression ->
      a typ ->
      bool ->
      pattern option * (expression -> expression) option =
   fun ?type_expr t inside_view ->
    let open Ctypes_static in
    let loc = !Ast_helper.default_loc in
    let structured typ =
      match type_expr with
      | None ->
        let s = param_name () in
        let e = mk_ex s in
        let p =
          match typ with
          | `Union -> [%pat? Ctypes_static.Union _]
          | `Struct -> [%pat? Ctypes_static.Struct _]
          | `Abstract -> [%pat? Ctypes_static.Abstract _]
        in
        let p = Pat.alias p (U.mk_loc s) in
        let f expr =
          [%expr Cstubs_internals.make_structured [%e e] [%e expr]]
        in
        (Some p, Some f)
      | Some e ->
        let f expr =
          [%expr Cstubs_internals.make_structured [%e e] [%e expr]]
        in
        (None, Some f)
    in
    match t with
    | Void -> (None, None)
    | Primitive p -> (cond_expand_prim p inside_view cinfo, None)
    | Pointer _ -> (
      let f e expr =
        [%expr
          Cstubs_internals.make_ptr [%e e]
            (Ppx_cstubs.Ppx_cstubs_internals.to_voidp [%e expr])]
      in
      let e =
        match type_expr with None -> None | Some e -> extract_pointer e
      in
      match e with
      | None ->
        let e, p = mk_ex_pat @@ param_name () in
        (Some [%pat? Ctypes_static.Pointer [%p p]], Some (f e))
      | Some e -> (None, Some (f e)))
    | Funptr _ ->
      let e, p = mk_ex_pat @@ param_name () in
      let f expr =
        [%expr
          Cstubs_internals.make_fun_ptr [%e e]
            (Ppx_cstubs.Ppx_cstubs_internals.to_voidp [%e expr])]
      in
      (Some [%pat? Ctypes_static.Funptr [%p p]], Some f)
    | Struct _ -> structured `Struct
    | Union _ -> structured `Union
    | View { format_typ = Some ft; _ } when ft == Evil_hack.format_typ ->
      (None, None)
    | View { ty; _ } -> (
      match inline_view ?type_expr ty inside_view with
      | Some s -> s
      | None ->
        let s = param_name () in
        let f, p = mk_ex_pat s in
        let pat2, nexpr = iter ty true in
        let f =
          match nexpr with
          | None -> fun expr -> [%expr [%e f] [%e expr]]
          | Some f' ->
            fun expr ->
              let expr = f' expr in
              [%expr [%e f] [%e expr]]
        in
        let pat =
          match pat2 with
          | None ->
            [%pat? Ctypes_static.View { Ctypes_static.read = [%p p]; _ }]
          | Some pat2 ->
            [%pat?
              Ctypes_static.View
                { Ctypes_static.read = [%p p]; Ctypes_static.ty = [%p pat2]; _ }]
        in
        (Some pat, Some f))
    | Abstract _ -> structured `Abstract
    | OCaml _ -> error t
    | Array _ -> error t
    | Bigarray _ -> error t
  and inline_view :
      type a.
      ?type_expr:expression ->
      a typ ->
      bool ->
      (pattern option * (expression -> expression) option) option =
   fun ?type_expr tchild inside_view ->
    if inside_view then None
    else
      match (tchild, type_expr) with
      | CS.Pointer _, Some x -> (
        match extract_pointer_opt x with
        | Some orig_expr ->
          let f expr =
            let loc = !Ast_helper.default_loc in
            [%expr
              match [%e expr] with
              | 0n -> None
              | ppxc__not_null ->
                Some
                  (Cstubs_internals.make_ptr [%e orig_expr]
                     (Ppx_cstubs.Ppx_cstubs_internals.to_voidp ppxc__not_null))]
          in
          Some (None, Some f)
        | None -> None)
      | _ -> None
  in
  iter ?type_expr t false

type param_info = {
  ext_ptype : core_type;
  (* what to use in external : ... *)
  fun_pat : pattern;
  (* ocaml function pattern fun a .. -> *)
  fun_expr : expression;
  (* pattern above as expr in the function body *)
  (* info extraction with pattern matching necessary ? *)
  match_pat : pattern option;
  (* if the parameter needs to be tranformed first .. *)
  param_trans : expand_in;
  label : arg_label;
  annot_needed : bool;
  (* do I need to repeat the Ctype.typ to type the generated function *)
  constr_ptype : core_type; (* x y z Ctypes.typ expanded as far as possible *)
}

type ret_info = {
  rext_ptype : core_type;
  rmatch_pat : pattern option;
  res_trans : (expression -> expression) option;
  rannot_needed : bool;
  rconstr_ptype : core_type;
}

let build_external ~ocaml_name param_infos ret_info cinfo =
  let prim =
    if Ocaml_config.version () >= (4, 3, 0) then []
    else
      let l = if cinfo.Gen_c.noalloc then [ "noalloc" ] else [] in
      if cinfo.Gen_c.float then "float" :: l else l
  in
  let prim = cinfo.Gen_c.stub_name :: prim in
  let prim =
    match cinfo.Gen_c.stub_name_byte with None -> prim | Some s -> s :: prim
  in
  let name = U.mk_loc ocaml_name in
  let t =
    ListLabels.fold_right param_infos ~init:ret_info.rext_ptype ~f:(fun a acc ->
        Typ.arrow a.label a.ext_ptype acc)
  in
  let attrs =
    if Ocaml_config.version () < (4, 3, 0) then []
    else
      match cinfo.Gen_c.noalloc with
      | false -> []
      | true -> [ Attr.mk (U.mk_loc "noalloc") (PStr []) ]
  in
  let p = Val.mk ~attrs ~prim name t in
  Str.primitive p

(* (fun ~p1:ppxc__x0 -> fun ~p2:ppxc__x1 -> fun ppxc__x2 -> res_transform
   ($ext_fun (param_trans ppxc__x0) ...) ) *)
let build_fun param_infos ret_info ~ext_fun =
  let is_simple_fun =
    List.for_all param_infos ~f:(fun x ->
        x.match_pat = None && x.param_trans = In_ident)
    && ret_info.rmatch_pat = None
    && ret_info.res_trans = None
  in
  if is_simple_fun then None
  else
    let l =
      List.map param_infos ~f:(fun a ->
          ( a.label,
            match a.param_trans with
            | In_ident -> a.fun_expr
            | In_trans f -> f a.fun_expr
            | In_ptr_bind (_, e, _) | In_fptr_bind (_, e, _) -> e ))
    in
    let fb = Exp.apply ext_fun l in
    let fb = match ret_info.res_trans with None -> fb | Some f -> f fb in
    let fb =
      List.fold_left (List.rev param_infos) ~init:fb ~f:(fun ac el ->
          match el.param_trans with
          | In_ident | In_trans _ -> ac
          | In_ptr_bind (p, _, f) | In_fptr_bind (p, _, f) ->
            let loc = !Ast_helper.default_loc in
            [%expr
              let [%p p] = [%e f el.fun_expr] in
              [%e ac]])
    in
    Some
      (ListLabels.fold_right ~init:fb param_infos ~f:(fun a ac ->
           Exp.fun_ a.label None a.fun_pat ac))

let poly_prefix pos =
  String.concat "" [ Myconst.private_prefix; "t_"; string_of_int pos ]

let ctypes_typ_constr expr i ct =
  let expr =
    match ct.ptyp_desc with
    | Ptyp_constr
        ( { txt = Ldot (Lident "Ctypes", "typ"); _ },
          [ { ptyp_desc = Ptyp_any; _ } ] ) ->
      expr
    | _ -> Exp.constraint_ expr ct
  in
  let t = Typ.var (poly_prefix i) in
  let loc = !Ast_helper.default_loc in
  let t = [%type: [%t t] Ctypes.typ] in
  Exp.constraint_ expr t

let stdlib_fun s =
  let l = [ s ] in
  let l =
    let ver = Ocaml_config.version () in
    if ver >= (4, 8, 0) then "Stdlib" :: l
    else if ver >= (4, 7, 0) then "Stdlib" :: "Pervasives" :: l
    else "Pervasives" :: l
  in
  U.mk_ident_l l

let ignore_fun () = stdlib_fun "ignore"

(* if false then ( Stdlib.Pervasives.ignore (Ctypes.ptr void : 'ppxc__t_0
   Ctypes.typ); Stdlib.Pervasives.ignore ( ... : 'ppxc__t_1 Ctypes.typ); ... ) *)
let build_ignore_expr (el, params) (retexpr, ret_info) =
  let loc = !Ast_helper.default_loc in
  let f ac i e match_pat annot_needed ct =
    match (match_pat, annot_needed) with
    | None, false | Some _, _ -> ac
    | _, true -> (
      let e = ctypes_typ_constr e i ct in
      let e = [%expr [%e ignore_fun ()] [%e e]] in
      match ac with None -> Some e | Some x -> Some (Exp.sequence x e))
  in
  let i, e =
    ListLabels.fold_left2 ~init:(0, None) el params ~f:(fun (i, ac) (_, e) a ->
        let ac = f ac i e a.match_pat a.annot_needed a.constr_ptype in
        (succ i, ac))
  in
  let e =
    f e i retexpr ret_info.rmatch_pat ret_info.rannot_needed
      ret_info.rconstr_ptype
  in
  match e with None -> None | Some s -> Some [%expr if false then [%e s]]

let match_nw expr case =
  let loc = !Ast_helper.default_loc in
  let ic = [%expr Ppx_cstubs.Ppx_cstubs_internals.invalid_code ()] in
  let any = Exp.case (Pat.any ()) ic in
  Exp.match_ ~attrs:[ U.ocaml_warning "-4" ] expr [ case; any ]

(* match (t1 : 'ppxc__t_1 Ctypes.typ), (t2 : 'ppxc__t_0 Ctypes.typ) with |
   (Ctypes_static.View ..), (Ctypes_static.Foo ..) -> $func | _ ->
   Ppx_cstubs_internals.invalid_code () *)
let build_parallel_pattern (el, param_infos) (retexpr, ret_info) func =
  let cnt = ref 0 in
  let l =
    ListLabels.map2 el param_infos ~f:(fun (_, expr) a ->
        let i = !cnt in
        incr cnt;
        match a.match_pat with
        | None -> None
        | Some s -> Some (ctypes_typ_constr expr i a.constr_ptype, s))
    |> CCList.filter_map Std.identity
  in
  let l =
    match ret_info.rmatch_pat with
    | None -> l
    | Some s -> (ctypes_typ_constr retexpr !cnt ret_info.rconstr_ptype, s) :: l
  in
  match l with
  | [] -> func
  | [ (expr, pat) ] -> match_nw expr (Exp.case pat func)
  | l ->
    let expr = Exp.tuple (List.map ~f:fst l) in
    let cl = Exp.case (Pat.tuple (List.map ~f:snd l)) func in
    match_nw expr cl

(* match $fn_expr with | Ctypes_static.Function(...,Ctypes_static.Function(...,
   Ctypes_static.Returns(...))) -> $func | _ ->
   Ppx_cstubs_internals.invalid_code () with $f_expr: (t1 @-> t2 @-> return t3)
   : ('ppxc__t_1 -> 'ppxc__t_2 -> 'ppxc__t_3) Ctypes.fn *)
let build_pattern fn_expr patterns ~func =
  let loc = !Ast_helper.default_loc in
  let rec iter = function
    | [] -> assert false
    | [ hd ] -> (
      match hd with
      | None -> None
      | Some s -> Some [%pat? Ctypes_static.Returns [%p s]])
    | hd :: tl -> (
      match iter tl with
      | None -> (
        match hd with
        | None -> None
        | Some s -> Some [%pat? Ctypes_static.Function ([%p s], _)])
      | Some s -> (
        match hd with
        | None -> Some [%pat? Ctypes_static.Function (_, [%p s])]
        | Some x -> Some [%pat? Ctypes_static.Function ([%p x], [%p s])]))
  in
  match iter patterns with
  | None -> None
  | Some pat -> Some (match_nw fn_expr (Exp.case pat func))

let collect_info fn cinfo lexpr =
  let param_name = create_param_name () in
  let rec iter :
      type a.
      a Ctypes.fn ->
      expression list ->
      param_info list ->
      param_info list * ret_info =
   fun fn lexpr accu ->
    let type_expr, loc, lexpr' =
      match lexpr with
      | [] -> (None, !Ast_helper.default_loc, [])
      | hd :: tl -> (Some hd, hd.pexp_loc, tl)
    in
    U.with_loc loc @@ fun () ->
    match fn with
    | CS.Returns t ->
      let rext_ptype = ml_typ_of_return_typ t ~cinfo in
      let rext_ptype =
        match cinfo.Gen_c.return_errno with
        | false -> rext_ptype
        | true ->
          let t = ml_typ_of_return_typ Ctypes.sint ~cinfo in
          Typ.tuple [ rext_ptype; t ]
      in
      let rmatch_pat, res_trans =
        pat_expand_out ~cinfo ?type_expr t param_name
      in
      let res_trans =
        match (cinfo.Gen_c.return_errno, res_trans) with
        | false, _ | true, None -> res_trans
        | true, Some f ->
          let res_expr, res_pat = mk_ex_pat @@ param_name () in
          let errno_expr, errno_pat = mk_ex_pat @@ param_name () in
          let f expr =
            let res_expr = f res_expr in
            [%expr
              let [%p res_pat], [%p errno_pat] = [%e expr] in
              ([%e res_expr], [%e errno_expr])]
          in
          Some f
      in
      let rannot_needed = rmatch_pat <> None && res_trans <> None in
      (* ?? *)
      let rconstr_ptype = constraint_of_typ t in
      ( List.rev accu,
        { rext_ptype; rmatch_pat; rannot_needed; res_trans; rconstr_ptype } )
    | CS.Function (a, b) ->
      let fun_expr, fun_pat = mk_ex_pat @@ param_name () in
      let match_pat, param_trans =
        pat_expand_in ~cinfo ?type_expr a param_name
      in
      let type_info, ext_ptype = ml_typ_of_arg_typ a ~cinfo in
      let i =
        {
          ext_ptype;
          fun_pat;
          fun_expr;
          annot_needed = type_info <> `Complete;
          constr_ptype = constraint_of_typ a;
          match_pat;
          param_trans;
          label = Nolabel;
        }
      in
      iter b lexpr' (i :: accu)
  in
  iter fn lexpr []

(* (w1:'ppxc__t_1 -> 'ppxc__t_2 -> 'ppxc__t_3), ('ppxc__t_1 -> 'ppxc__t_2 ->
   'ppxc__t_3) Ctypes_static.fn *)
let build_fun_constraint params ~return_errno =
  let loc = !Ast_helper.default_loc in
  let length = List.length params in
  let var n = Typ.var (poly_prefix n) in
  let arrow with_label start =
    ListLabels.fold_right
      ~init:(pred length, start)
      params
      ~f:(fun p (i, last) ->
        let label = if with_label then p.label else Nolabel in
        (pred i, Typ.arrow label (var i) last))
    |> snd
  in
  let vl = var length in
  let start =
    match return_errno with
    | false -> vl
    | true -> [%type: [%t vl] * Signed.sint]
  in
  let t = arrow true start in
  let fn = arrow false vl in
  (t, [%type: [%t fn] Ctypes.fn])

type result = {
  extern : structure_item;
  intern : structure_item;
}

let common fn ext_name cinfo common =
  let loc = !Ast_helper.default_loc in
  let param_infos, ret_info =
    match common with
    | `Extern (el, ret) ->
      let exprl = List.rev (ret :: List.rev_map el ~f:snd) in
      let param_infos, ret_info = collect_info fn cinfo exprl in
      ( List.map2 el param_infos ~f:(fun (l, _) p -> { p with label = l }),
        ret_info )
    | `Foreign_value _ | `Foreign _ -> collect_info fn cinfo []
  in
  let extern = build_external ~ocaml_name:ext_name param_infos ret_info cinfo in
  let ext_fun = mk_ex ext_name in
  let intern_expanded = build_fun param_infos ret_info ~ext_fun in
  let typ, typ_fn =
    build_fun_constraint param_infos ~return_errno:cinfo.Gen_c.return_errno
  in
  let build_stri already_constrained res =
    let ext_fun_pat = U.mk_pat ext_name in
    let ext_fun_pat_c =
      if Ocaml_config.version () <> (4, 5, 0) || not !Options.pretty then
        Pat.constraint_ ext_fun_pat (Typ.poly [] typ)
      else ext_fun_pat
    in
    let res_c = Exp.constraint_ res typ in
    let no_type_repeat () =
      let res, fun_pat =
        if
          ret_info.rmatch_pat <> None
          || List.exists param_infos ~f:(fun x -> x.match_pat <> None)
        then (res_c, ext_fun_pat_c)
        else (res, ext_fun_pat)
      in
      match common with
      | `Foreign _ | `Extern _ -> [%stri let [%p fun_pat] = [%e res]]
      | `Foreign_value _ -> [%stri let [%p ext_fun_pat] = [%e res] ()]
    in
    if already_constrained then no_type_repeat ()
    else
      let dexpr =
        match common with
        | `Foreign fn_expr ->
          if
            ret_info.rannot_needed
            || List.exists param_infos ~f:(fun s -> s.annot_needed)
          then
            let e = Exp.constraint_ fn_expr typ_fn in
            Some [%expr if false then [%e ignore_fun ()] [%e e]]
          else None
        | `Extern (el, retexpr) | `Foreign_value (el, retexpr) ->
          build_ignore_expr (el, param_infos) (retexpr, ret_info)
      in
      match dexpr with
      | None -> no_type_repeat ()
      | Some dexpr -> (
        match common with
        | `Foreign _ | `Extern _ ->
          [%stri
            let [%p ext_fun_pat_c] = [%e res_c]

            and _ = [%e dexpr]]
        | `Foreign_value _ ->
          [%stri
            let [%p ext_fun_pat] = [%e res_c] ()

            and _ = [%e dexpr]])
  in
  let intern =
    match intern_expanded with
    | None ->
      if false = List.exists param_infos ~f:(fun x -> x.annot_needed) then
        Std.Util.empty_stri ()
      else build_stri false ext_fun
    | Some func ->
      let constr_used, res =
        match common with
        | `Extern (el, retexpr) | `Foreign_value (el, retexpr) ->
          ( false,
            build_parallel_pattern (el, param_infos) (retexpr, ret_info) func )
        | `Foreign fn_expr -> (
          let fn_expr = Exp.constraint_ fn_expr typ_fn in
          let pats =
            List.map param_infos ~f:(fun a -> a.match_pat)
            @ [ ret_info.rmatch_pat ]
          in
          match build_pattern fn_expr pats ~func with
          | None -> (false, func)
          | Some pat -> (true, pat))
      in
      build_stri constr_used res
  in
  { extern; intern }

let external' fn name el retexpr cinfo =
  common fn name cinfo (`Extern (el, retexpr))

let foreign_value fn name retexpr cinfo =
  let loc = !Ast_helper.default_loc in
  let el = [ (Nolabel, [%expr void]) ] in
  common fn name cinfo (`Foreign_value (el, retexpr))

let foreign fn name cinfo fn_expr = common fn name cinfo (`Foreign fn_expr)

type cb_param_info = {
  cb_fun_pat : pattern;
  cb_fun_expr : expression;
  cb_match_pat : pattern option;
  cb_param_trans : (expression -> expression) option;
}

type cb_ret_info = {
  cb_rfun_pat : pattern;
  cb_rmatch_pat : pattern option;
  cb_res_trans : expand_in;
}

(* FIXME: Avoid code duplication with build_fun without
   adding too much obscurities ... *)
let build_cb_fun param_infos ret_info ~user_fun =
  let is_simple_fun =
    List.for_all param_infos ~f:(fun x ->
        x.cb_match_pat = None && x.cb_param_trans = None)
    && ret_info.cb_rmatch_pat = None
    && ret_info.cb_res_trans = In_ident
  in
  if is_simple_fun then None
  else
    let loc = !Ast_helper.default_loc in
    let l =
      List.map param_infos ~f:(fun a ->
          ( Nolabel,
            match a.cb_param_trans with
            | Some f -> f a.cb_fun_expr
            | None -> a.cb_fun_expr ))
    in
    let fbody = Exp.apply user_fun l in
    let fbody =
      match ret_info.cb_res_trans with
      | In_ident -> fbody
      | In_trans f -> f fbody
      | In_ptr_bind (p, e, f) ->
        [%expr
          let [%p p] = [%e f fbody] in
          [%e e]]
      | In_fptr_bind (p, e, f) ->
        [%expr
          let [%p p] = [%e f fbody] in
          [%e e]]
    in
    Some
      (ListLabels.fold_right ~init:fbody param_infos ~f:(fun a ac ->
           Exp.fun_ Nolabel None a.cb_fun_pat ac))

let collect_info_cb fn =
  let param_name = create_param_name () in
  let rec iter :
      type a.
      a Ctypes.fn -> cb_param_info list -> cb_param_info list * cb_ret_info =
   fun fn accu ->
    match fn with
    | CS.Returns t ->
      let cb_rfun_pat = U.mk_pat @@ param_name () in
      let cb_rmatch_pat, cb_res_trans = pat_expand_in t param_name in
      let r = { cb_rfun_pat; cb_rmatch_pat; cb_res_trans } in
      (List.rev accu, r)
    | CS.Function (a, b) ->
      let cb_fun_expr, cb_fun_pat = mk_ex_pat @@ param_name () in
      let cb_match_pat, cb_param_trans = pat_expand_out a param_name in
      let r = { cb_fun_pat; cb_fun_expr; cb_match_pat; cb_param_trans } in
      iter b (r :: accu)
  in
  iter fn []

let ocaml_funptr mof callback_fn =
  let loc = !Ast_helper.default_loc in
  let {
    Marshal_types.cb_binding_name;
    cb_mod_path;
    cb_bottom;
    cb_top_mod;
    cb_acquire_runtime = _;
    cb_thread_registration = _;
    cb_user_fun;
    cb_init_fun;
  } =
    mof
  in
  let mod_path = cb_mod_path in
  let param_infos, ret_info = collect_info_cb callback_fn in
  let mpath s = mod_path @ [ cb_top_mod; s ] in
  let fn_l = mpath "fn" in
  let fn_typ = U.mk_typc_l fn_l in
  let user_fun = Exp.constraint_ cb_user_fun fn_typ in
  let fun' = build_cb_fun param_infos ret_info ~user_fun in
  let fn_ref = U.mk_ident_l fn_l in
  let pat_binding_name = U.mk_pat cb_binding_name in
  let prim_name = U.safe_mlname () in
  let external' =
    let pointer = U.mk_typc_l (mpath "raw_pointer") in
    let typ =
      ListLabels.fold_right param_infos ~init:(Typ.any ()) ~f:(fun _ acc ->
          Typ.arrow Nolabel (Typ.any ()) acc)
    in
    let typ = Typ.arrow Nolabel typ pointer in
    let p = Val.mk ~prim:[ cb_init_fun ] (U.mk_loc prim_name) typ in
    Str.primitive p
  in
  let mk_pointer = U.mk_ident_l (mpath "make_pointer") in
  let f e =
    let f = U.mk_ident_l [ prim_name ] in
    [%expr [%e mk_pointer] ([%e f] [%e e])]
  in
  let expr =
    match fun' with
    | None -> f user_fun
    | Some func -> (
      let func = f func in
      let pats =
        List.map param_infos ~f:(fun a -> a.cb_match_pat)
        @ [ ret_info.cb_rmatch_pat ]
      in
      match build_pattern fn_ref pats ~func with None -> func | Some f -> f)
  in
  let stri = [%stri let [%p pat_binding_name] = [%e expr]] in
  let m = Mod.structure [ external'; stri ] in
  let e =
    [%expr [%e mk_pointer] (Ppx_cstubs.Ppx_cstubs_internals.obj_magic 0n)]
  in
  let e =
    if Ocaml_config.use_open_struct () then U.alias_impl_mod_let e
    else
      let n = !Lconst.impl_mod_name in
      Exp.letmodule (U.mk_oloc n) (Mod.ident (U.mk_lid n)) e
  in
  let mt =
    U.sig_from_mod_type
      [%stri
        module type X = sig
          include module type of struct
            let [%p pat_binding_name] = [%e e]
          end
        end]
  in
  let mt = Mty.signature mt in
  let stri = Str.include_ (Incl.mk (Mod.constraint_ m mt)) in
  Hashtbl.replace Script_result.htl_stri cb_bottom stri

type record_stris = {
  r_stri_top : structure_item list;
  r_stri_bottom : structure_item list;
  r_stri_type_mod : structure_item list;
}

let gen_record_stris ~mod_path ~type_name l =
  let loc = !Ast_helper.default_loc in
  let sig_shared_name = U.safe_mlname ~prefix:"S" () in
  let sig_shared_name_lid = U.mk_lid_l [ sig_shared_name ] in
  let stri_sig_shared =
    let types =
      List.mapi l ~f:(fun i _ ->
          let n = U.mk_loc ("x" ^ string_of_int i) in
          let t = Type.mk ~kind:Ptype_abstract n in
          Sig.type_ Recursive [ t ])
    in
    let typ = Mty.signature types in
    let s = Mtd.mk ~typ (U.mk_loc sig_shared_name) in
    Str.modtype s
  in
  let functor_param = U.safe_mlname ~capitalize:true ~prefix:"M" () in
  let make_type ?attrs ?manifest mod_path =
    let fields =
      List.mapi l ~f:(fun i (txt, loc, _) ->
          let l = mod_path @ [ functor_param; "x" ^ string_of_int i ] in
          Type.field { txt; loc } (U.mk_typc_l l))
    in
    let n = U.mk_loc type_name in
    let t = Type.mk ?attrs ?manifest ~kind:(Ptype_record fields) n in
    Str.type_ Recursive [ t ]
  in
  let unpacked_module =
    let pl1, pl2 =
      List.mapi l ~f:(fun i _ ->
          let i = string_of_int i in
          let n = U.mk_lid_l [ "x" ^ i ] in
          ((n, Typ.var ("t" ^ i)), (n, U.mk_typc_l [ "t" ^ i ])))
      |> List.split
    in
    let ppat = Typ.package sig_shared_name_lid pl1 in
    let pexpr = Typ.package sig_shared_name_lid pl2 in
    let pl_len = List.length l - 1 in
    let x, constr_pat, constr_expr =
      ListLabels.fold_right ~init:(pl_len, ppat, pexpr) l
        ~f:(fun _ (i, last_pat, last_expr) ->
          let si = string_of_int i in
          let var = Typ.var ("t" ^ si) in
          let t = [%type: [%t var] Ctypes.typ] in
          let t_pat = Typ.arrow Nolabel t last_pat in
          let var = U.mk_typc_l [ "t" ^ si ] in
          let t = [%type: [%t var] Ctypes.typ] in
          let t_expr = Typ.arrow Nolabel t last_expr in
          (pred i, t_pat, t_expr))
    in
    assert (x = -1);
    let types =
      List.mapi l ~f:(fun i (_, loc, _) ->
          U.with_loc loc @@ fun () ->
          let i = string_of_int i in
          let manifest = U.mk_typc_l [ "t" ^ i ] in
          let t = Type.mk ~manifest ~kind:Ptype_abstract (U.mk_loc ("x" ^ i)) in
          Str.type_ Recursive [ t ])
    in
    let pmod = Mod.structure types in
    let init = Exp.pack pmod in
    let init =
      ListLabels.fold_right ~init l ~f:(fun _ ac ->
          Exp.fun_ Nolabel None (Pat.any ()) ac)
    in
    let init = Exp.constraint_ init constr_expr in
    let x, expr =
      ListLabels.fold_right ~init:(pl_len, init) l ~f:(fun _ (i, last) ->
          let n = U.mk_loc ("t" ^ string_of_int i) in
          (pred i, Exp.newtype n last))
    in
    assert (x = -1);
    let sl = List.mapi l ~f:(fun i _ -> U.mk_loc ("t" ^ string_of_int i)) in
    let t = Typ.poly sl constr_pat in
    let fun_name = U.safe_mlname () in
    let p = Pat.var (U.mk_loc fun_name) in
    let p = Pat.constraint_ p t in
    let vb = Vb.mk p expr in
    let lexpr = List.map l ~f:(fun (_, _, l) -> (Nolabel, l)) in
    let lexpr = Exp.apply (U.mk_ident_l [ fun_name ]) lexpr in
    let w = Exp.let_ Nonrecursive [ vb ] lexpr in
    Mod.unpack w
  in
  if Ocaml_config.use_open_struct () = false then
    let functor_name = U.safe_mlname ~capitalize:true ~prefix:"F" () in
    let pmod =
      let p1 = Named (U.mk_oloc functor_param, Mty.ident sig_shared_name_lid) in
      Mod.functor_ p1 (Mod.structure [ make_type [] ])
    in
    let nf = Ocaml_config.version () <= (4, 3, 0) && !Options.pretty in
    let mod_name_typ = CCString.capitalize_ascii type_name ^ "__typ" in
    let m =
      let x = if nf then Mod.ident (U.mk_lid functor_name) else pmod in
      Mod.apply x unpacked_module
    in
    let stri_typ = Str.module_ (Mb.mk (U.mk_oloc mod_name_typ) m) in
    let mk l = Str.include_ (Incl.mk (Mod.ident (U.mk_lid_l l))) in
    let incl_top = mk [ mod_name_typ ] in
    let incl_bottom = mk (mod_path @ [ mod_name_typ ]) in
    let r_stri_top =
      if nf then
        let functor' = Str.module_ (Mb.mk (U.mk_oloc functor_name) pmod) in
        [ stri_sig_shared; functor'; stri_typ; incl_top ]
      else [ stri_sig_shared; stri_typ; incl_top ]
    in
    let r_stri_bottom = [ incl_bottom ] in
    { r_stri_top; r_stri_bottom; r_stri_type_mod = [] }
  else
    let make_module e = Str.module_ (Mb.mk (U.mk_oloc functor_param) e) in
    let m = make_module unpacked_module in
    let typ' = make_type [] in
    let r_stri_top = [ stri_sig_shared; m; typ' ] in
    let alias_name = !Lconst.impl_mod_name ^ "_a" in
    let os = U.alias_impl_mod_os ~alias_name () in
    let manifest = U.mk_typc_l (mod_path @ [ type_name ]) in
    let t1 = make_type ~manifest mod_path in
    let os_path = alias_name :: List.tl mod_path in
    let f attrs =
      [%str
        include (
          struct
            [%%i t1]
          end :
            sig
              include module type of struct
                [%%i os]

                [%%i make_type ?attrs ~manifest os_path]
              end
            end)]
    in
    let attrs = Some [ Attributes.manifest_replace_attrib ] in
    { r_stri_top; r_stri_bottom = f attrs; r_stri_type_mod = f None }
