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

open Migrate_parsetree
open Ast_405
open Ast_helper
open Parsetree
open Ctypes

module List = CCListLabels

let create_param_name () =
  let i = ref 0 in
  fun () ->
    let s = Printf.sprintf "ppxc__x%d\n" !i in
    incr i;
    s

module U = Std.Util

let mk_attrs = function
| None -> []
| Some s -> [U.mk_loc s, PStr []]

let mk_ex ?attrs s =
  Exp.ident ~attrs:(mk_attrs attrs) (U.mk_lid s)

let mk_ex_pat s = mk_ex s, U.mk_pat s

let mk_typ ?attrs ?(l=[]) s =
  Typ.constr ~attrs:(mk_attrs attrs) (U.mk_lid s) l

let managed_buffer =
  mk_typ "Cstubs_internals.managed_buffer"

let ident_of_ml_prim : type a. a Ctypes_primitive_types.ml_prim ->
  noalloc:bool -> Ast_405.Parsetree.core_type =
  let open Ctypes_primitive_types in
  fun t ~noalloc ->
    let f ~a t =
      if noalloc then
        mk_typ ~attrs:a t
      else
        mk_typ t in
    match t with
    | ML_char -> mk_typ "char"
    | ML_bool -> mk_typ "bool"
    | ML_complex -> mk_typ "Complex.t"
    | ML_float -> f ~a:"unboxed" "float"
    | ML_int -> f ~a:"untagged" "int"
    | ML_int32 -> f ~a:"unboxed" "int32"
    | ML_int64 -> f ~a:"unboxed" "int64"
    | ML_llong -> mk_typ "Signed.llong"
    | ML_long -> mk_typ "Signed.long"
    | ML_sint -> mk_typ "Signed.sint"
    | ML_nativeint -> f ~a:"unboxed" "nativeint"
    | ML_size_t -> mk_typ "Unsigned.size_t"
    | ML_uchar -> mk_typ "Unsigned.uchar"
    | ML_uint -> mk_typ "Unsigned.uint"
    | ML_uint16 -> mk_typ "Unsigned.uint16"
    | ML_uint32 -> mk_typ "Unsigned.uint32"
    | ML_uint64 -> mk_typ "Unsigned.uint64"
    | ML_uint8 -> mk_typ "Unsigned.uint8"
    | ML_ullong -> mk_typ "Unsigned.ullong"
    | ML_ulong -> mk_typ "Unsigned.ulong"
    | ML_ushort -> mk_typ "Unsigned.ushort"
    | ML_ldouble -> mk_typ "LDouble.t"
    | ML_complexld -> mk_typ "ComplexL.t"

let mk_struct_typ s =
  let v = Rtag(s,[],true,[]) in
  let v = Typ.variant [v] Asttypes.Closed None in
  mk_typ ~l:[Typ.any (); v] "Ctypes.structured"

let mk_ocaml_typ s =
  mk_typ ~l:[mk_typ s] "Ctypes_static.ocaml"

let rec ml_typ_of_arg_typ :
  type a. a typ ->  noalloc:bool -> Ast_405.Parsetree.core_type =
  let open Ctypes_static in
  fun t ~noalloc ->
    match t with
    | Void -> mk_typ "unit"
    | Primitive p -> ident_of_ml_prim ~noalloc (Ctypes_primitive_types.ml_prim p)
    | Pointer _   -> mk_typ ~l:[Typ.any ()] "Ctypes.ptr"
    | Funptr _    -> mk_typ ~l:[Typ.any ()] "Ctypes_static.static_funptr"
    | Struct _    -> mk_struct_typ "Struct"
    | Union _     -> mk_struct_typ "Union"
    | Abstract _  -> mk_typ ~l:[Typ.any ()] "Ctypes.ptr"
    | View { ty = Struct {tag ;_ }; format_typ = Some ft; _ }
      when ft == Evil_hack.format_typ ->
      Marshal.from_string tag 0
    | View { ty; _ } -> ml_typ_of_arg_typ ~noalloc ty
    | Array _    as a ->
      Std.Util.error
        "Unexpected array in an argument type: %s" (Ctypes.string_of_typ a)
    | Bigarray _ as a ->
      Std.Util.error
        "Unexpected bigarray in an argument type: %s" (Ctypes.string_of_typ a)
    | OCaml String -> mk_ocaml_typ "string"
    | OCaml Bytes -> mk_ocaml_typ "bytes"
    | OCaml FloatArray ->
      let float = mk_typ "float" in
      let array = mk_typ ~l:[float] "array" in
      mk_typ ~l:[array] "Ctypes_static.ocaml"


let rec ml_typ_of_return_typ :
  type a. a typ -> noalloc:bool -> Ast_405.Parsetree.core_type =
  let open Ctypes_static in
  let mk_ptr noalloc =
    let attrs = if noalloc = false then None else Some "unboxed" in
    mk_typ ?attrs "nativeint" in
  fun t ~noalloc ->
    match t with
    | Void -> mk_typ "unit"
    | Primitive p -> ident_of_ml_prim ~noalloc (Ctypes_primitive_types.ml_prim p)
    | Struct _    -> managed_buffer
    | Union _     -> managed_buffer
    | Abstract _  -> managed_buffer
    | Pointer _   -> mk_ptr noalloc
    | Funptr _    -> mk_ptr noalloc
    | View { ty = Struct {tag ;_ }; format_typ = Some ft; _ }
      when ft == Evil_hack.format_typ ->
      Marshal.from_string tag 0
    | View { ty; _ } -> ml_typ_of_return_typ ty ~noalloc
    | Array _    as a ->
      Std.Util.error
        "Unexpected array type in the return type: %s"
        (Ctypes.string_of_typ a)
    | Bigarray _ as a ->
      Std.Util.error
        "Unexpected bigarray type in the return type: %s"
        (Ctypes.string_of_typ a)
    | OCaml String ->
      unsupported "cstubs does not support OCaml strings as return values"
    | OCaml Bytes ->
      unsupported
        "cstubs does not support OCaml bytes values as return values"
    | OCaml FloatArray ->
      unsupported
        "cstubs does not support OCaml float arrays as return values"

let pat_expand_prim :
  type a. a Ctypes_primitive_types.prim -> Parsetree.pattern =
  let open Ctypes_primitive_types in function
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

let rec pat_expand_in :
  type a.
  a typ
  -> Parsetree.expression
  -> (unit -> string)
  -> bool
  -> Parsetree.pattern option * Parsetree.expression option =
  let open Ctypes_static in
  fun t expr param_name force ->
    let unsupported t =
      let x = Ctypes.string_of_typ t in
      unsupported "cstubs does not support passing %s as parameter" x in
    match t with
    | Void  ->
      (if force then Some [%pat? Ctypes_static.Void] else None),
      None
    | Primitive p ->
      let r =
        if force = false then None
        else
        let p = pat_expand_prim p in
        Some [%pat? Ctypes_static.Primitive [%p p]] in
      r,None
    | Pointer _ ->
      (if force then Some [%pat? Ctypes_static.Pointer _] else None),
      None
    | Funptr _ ->
      (if force then Some [%pat? Ctypes_static.Funptr _] else None),
      None
    | Struct _ ->
      (if force then Some [%pat? Ctypes_static.Struct _] else None),
      None
    | Union _ ->
      (if force then Some [%pat? Ctypes_static.Union _] else None),
      None
    | View { format_typ = Some ft; _ }
      when ft == Evil_hack.format_typ ->
      None,None
    | View { ty; _} ->
      let f,p = mk_ex_pat @@ param_name () in
      let nexpr = [%expr [%e f] [%e expr]] in
      let pat2,nexpr2 = pat_expand_in ty nexpr param_name true in
      let pat = match pat2 with
      | None -> [%pat? Ctypes_static.View({ Ctypes_static.write = [%p p]; _ })]
      | Some pat2 -> [%pat?
          Ctypes_static.View({ Ctypes_static.write = [%p p] ;
                               Ctypes_static.ty = [%p pat2]; _ })] in
      let expr = match nexpr2 with
      | None -> nexpr
      | Some x -> x in
      Some pat,
      Some expr
    | OCaml ty ->
      let out =
        if force = false then None
        else
        let s = match ty with
        | String -> [%pat? Ctypes_static.OCaml Ctypes_static.String]
        | Bytes -> [%pat? Ctypes_static.OCaml Ctypes_static.Bytes]
        | FloatArray -> [%pat? Ctypes_static.OCaml Ctypes_static.FloatArray] in
        Some s in
      out,None
    | Abstract _ -> unsupported t
    | Array _ -> unsupported t
    | Bigarray _ -> unsupported t

let pat_expand_in a b f =
  pat_expand_in a b f false


let extract_pointer =
  let open Longident in
  let open Asttypes in
  fun e ->
    match e.pexp_desc with
    | Pexp_apply({pexp_desc = Pexp_ident
                      {txt = Ldot (Lident "Ctypes", "ptr");
                       loc = _};
                  pexp_attributes = []; _},
                 [Nolabel,
                  ({pexp_desc = Pexp_ident _ ; _} as a)
                 ]) ->
      Some a
    | _ -> None

let rec pat_expand_out :
  type a.
  ?type_expr: Parsetree.expression
  -> a typ -> (unit -> string) -> bool
  -> Parsetree.pattern option *
     (Parsetree.expression -> Parsetree.expression) option =
  fun ?type_expr t param_name force ->
    let open Ctypes_static in
    let unsupported t =
      let x = Ctypes.string_of_typ t in
      unsupported "cstubs does not support %s as return values" x in
    let union_struct is_struct =
      match type_expr with
      | None ->
        let s = param_name () in
        let e = mk_ex s in
        let p = match is_struct with
        | false -> [%pat? Ctypes_static.Union _]
        | true -> [%pat? Ctypes_static.Struct _] in
        let p = Pat.alias p (U.mk_loc s) in
        let f expr =
          [%expr Cstubs_internals.make_structured [%e e] [%e expr]] in
        Some p, Some f
      | Some e ->
        let f expr =
          [%expr Cstubs_internals.make_structured [%e e] [%e expr]] in
        None, Some f in
    match t with
    | Void  ->
      (if force then Some [%pat? Ctypes_static.Void] else None),
      None
    | Primitive p ->
      (if force then
         let p = pat_expand_prim p in
         Some [%pat? Ctypes_static.Primitive [%p p]] else None),
      None
    | Pointer _ ->
      let f e expr =
        [%expr Cstubs_internals.make_ptr [%e e]
            (Ppx_cstubs_internals.to_voidp [%e expr])] in
      let e = match type_expr with
      | None -> None
      | Some e -> extract_pointer e in
      (match e with
      | None ->
        let e,p = mk_ex_pat @@ param_name () in
        Some [%pat? Ctypes_static.Pointer [%p p]],
        Some (f e)
      | Some e ->
        None,
        Some (f e))
    | Funptr _ ->
      let e,p = mk_ex_pat @@ param_name () in
      let f expr =
        [%expr
          Cstubs_internals.make_fun_ptr
            [%e e]
            (Ppx_cstubs_internals.to_voidp [%e expr])] in
      Some [%pat? Ctypes_static.Funptr [%p p]],
      Some f
    | Struct _ -> union_struct true
    | Union _ -> union_struct false
    | View { format_typ = Some ft; _ } when ft == Evil_hack.format_typ ->
      None, None
    | View {ty; _} ->
      let s = param_name () in
      let f,p = mk_ex_pat s in
      let pat2,nexpr = pat_expand_out ty param_name true in
      let f = match nexpr with
      | None -> fun expr -> [%expr [%e f] [%e expr]]
      | Some f' ->
        fun expr ->
          let expr = f' expr in
          [%expr [%e f] [%e expr]] in
      let pat = match pat2 with
      | None -> [%pat? Ctypes_static.View({ Ctypes_static.read = [%p p] ; _})]
      | Some pat2 -> [%pat?
          Ctypes_static.View({ Ctypes_static.read = [%p p] ;
                               Ctypes_static.ty = [%p pat2] ;_})] in
      Some pat,
      Some f
    | Abstract _ -> unsupported t
    | OCaml _ -> unsupported t
    | Array _ -> unsupported t
    | Bigarray _ -> unsupported t

let pat_expand_out ?type_expr t f =
  pat_expand_out ?type_expr t f false

type param_info =
  {
    ext_ptype: core_type; (* what to use in external : ... *)
    fun_pat: pattern; (* ocaml function pattern fun a .. -> *)
    fun_expr: expression; (* pattern above as expr in the function body *)
    (* info extraction with pattern matching necessary ? *)
    match_pat: pattern option;
    (* if the parameter needs to be tranformed first .. *)
    param_trans: expression option;
    label: Asttypes.arg_label;
    is_inline_ocaml_type: bool;
    annot_needed: bool; (* do I need to repeat the Ctype.typ to type the
                           generated function *)
  }

type ret_info =
  {
    rext_ptype: core_type;
    rmatch_pat: pattern option;
    res_trans: (expression -> expression) option;
    ris_inline_ocaml_type: bool;
    rannot_needed: bool;
  }

let build_external ~ocaml_name param_infos ret_info cinfo =
  let prim = cinfo.Gen_c.stub_name::[] in
  let prim = match cinfo.Gen_c.stub_name_byte with
  | None -> prim
  | Some s -> s::prim in
  let name = U.mk_loc ocaml_name in
  let t =
    ListLabels.fold_right param_infos ~init:ret_info.rext_ptype
      ~f:(fun a acc -> Typ.arrow a.label a.ext_ptype acc) in
  let attrs = match cinfo.Gen_c.noalloc with
  | false -> []
  | true -> [ U.mk_loc "noalloc", PStr [] ] in
  let p = Val.mk ~attrs ~prim name t in
  Str.primitive p

(* (fun ~p1:ppxc__x0 -> fun ~p2:ppxc__x1 -> fun ppxc__x2 ->
      res_transform ($ext_fun (param_trans ppxc__x0) ...) ) *)
let build_fun param_infos ret_info ~ext_fun =
  let is_simple_fun =
    List.for_all param_infos
      ~f:(fun x -> x.match_pat = None && x.param_trans = None) &&
    ret_info.rmatch_pat = None && ret_info.res_trans = None in
  if is_simple_fun then None
  else
  let l = List.map param_infos ~f:(fun a ->
    a.label,(match a.param_trans with
    | Some a -> a | None -> a.fun_expr)) in
  let fbody = Exp.apply ext_fun l in
  let fbody = match ret_info.res_trans with
  | None -> fbody
  | Some f -> f fbody in
  Some(ListLabels.fold_right ~init:fbody param_infos ~f:(fun a ac ->
      Exp.fun_ a.label None a.fun_pat ac))


let poly_prefix pos = "ppxc__t_" ^ string_of_int pos

let ctypes_typ_constr expr i =
  let t = Typ.var (poly_prefix i) in
  let t = Typ.constr (U.mk_lid "Ctypes.typ") [t] in
  Exp.constraint_ expr t

let ignore_fun () =
  let ver = Ocaml_config.version () in
  if ver >= (4,8,0) then
    [%expr Stdlib.ignore ]
  else if ver >= (4,7,0) then
    [%expr Stdlib.Pervasives.ignore ]
  else
    [%expr Pervasives.ignore ]

(* if false then (
     Stdlib.Pervasives.ignore (Ctypes.ptr void : 'ppxc__t_0 Ctypes.typ);
     Stdlib.Pervasives.ignore ( ... : 'ppxc__t_1 Ctypes.typ);
     ... ) *)
let build_ignore_expr (el,params) (retexpr,ret_info) =
  let f ac i e match_pat annot_needed =
    match match_pat, annot_needed with
    | None, false
    | Some _, _ -> ac
    | _, true ->
      let e = ctypes_typ_constr e i in
      let e = [%expr [%e ignore_fun ()] [%e e]] in
      match ac with
      | None -> Some e
      | Some x -> Some (Exp.sequence x e) in
  let i,e = ListLabels.fold_left2 ~init:(0,None) el params
      ~f:(fun (i,ac) (_,e) a ->
        let ac = f ac i e a.match_pat a.annot_needed in
        (succ i),ac) in
  let e = f e i retexpr ret_info.rmatch_pat ret_info.rannot_needed in
  match e with
  | None -> None
  | Some s -> Some ([%expr if false then [%e s];])

(* match (t1 : 'ppxc__t_1 Ctypes.typ), (t2 : 'ppxc__t_0 Ctypes.typ) with
   | (Ctypes_static.View ..), (Ctypes_static.Foo ..) -> $func
   | _ -> Ppx_cstubs_internals.invalid_code () *)
let build_parallel_pattern (el,param_infos) (retexpr,ret_info) func =
  let cnt = ref 0 in
  let l =
    ListLabels.map2 el param_infos ~f:(fun (_,expr) a ->
      let i = !cnt in
      incr cnt;
      match a.match_pat with
      | None -> None
      | Some s -> Some(ctypes_typ_constr expr i,s))
    |> CCList.filter_map Std.identity in
  let l = match ret_info.rmatch_pat with
  | None -> l
  | Some s -> (ctypes_typ_constr retexpr !cnt,s)::l in
  if l = [] then func
  else
  let dummy_end = {
    pc_lhs = Pat.any ();
    pc_guard = None;
    pc_rhs = [%expr Ppx_cstubs_internals.invalid_code ()] } in
  let attrs = [ U.mk_loc "ocaml.warning", PStr [[%stri "-4"]] ] in
  match l with
  | (expr,pat)::[] ->
    let cl = {
      pc_lhs = pat;
      pc_guard = None;
      pc_rhs = func;
    } :: dummy_end :: [] in
    Exp.match_ ~attrs expr cl
  | l ->
    let expr = Exp.tuple (List.map ~f:fst l) in
    let cl = {
      pc_lhs = Pat.tuple (List.map ~f:snd l);
      pc_guard = None;
      pc_rhs = func;
    } :: dummy_end :: [] in
    Exp.match_ ~attrs expr cl

(* match $fn_expr with
  | Ctypes_static.Function(...,Ctypes_static.Function(...,
                                         Ctypes_static.Returns(...))) ->
     $func
  | _ -> Ppx_cstubs_internals.invalid_code ()
  with
  $f_expr:
  (t1 @-> t2 @-> return t3) : ('ppxc__t_1 -> 'ppxc__t_2 -> 'ppxc__t_3) Ctypes.fn
*)
let build_pattern fn fn_expr param_infos ret_info ~func =
  let rec iter : type a. a Ctypes.fn -> 'b -> 'c =
    let open Ctypes_static in
    fun a b ->
      match a,b with
      | Returns _,[] ->
        (match ret_info.rmatch_pat with
        | None -> None
        | Some s -> Some [%pat? Ctypes_static.Returns([%p s])])
      | Returns _, _ -> failwith "parameter mismatch"
      | Function(_,b),hd::tl ->
        (match iter b tl with
        | None ->
          (match hd.match_pat with
          | None -> None
          | Some s ->
            Some [%pat? Ctypes_static.Function( [%p s], _ )])
        | Some s ->
          (match hd.match_pat with
          | None -> Some [%pat? Ctypes_static.Function(_,[%p s])]
          | Some x -> Some [%pat? Ctypes_static.Function([%p x],[%p s])]))
      | Function _,[] -> failwith "parameter mismatch" in
  match iter fn param_infos with
  | None -> None
  | Some pat ->
    let dummy_end = {
      pc_lhs = Pat.any ();
      pc_guard = None;
      pc_rhs = [%expr Ppx_cstubs_internals.invalid_code ()]}  in
    let attrs = [U.mk_loc "ocaml.warning", PStr [[%stri "-4"]]] in
    let cl = {
      pc_lhs = pat;
      pc_guard = None;
      pc_rhs = func;
    } :: dummy_end :: [] in
    Some (Exp.match_ ~attrs fn_expr cl)

let is_inline_ocaml_type : type a. a Ctypes.typ -> bool =
  let open Ctypes_static in function
  | View { format_typ = Some f; _ } when f == Evil_hack.format_typ -> true
  | _ -> false

let annot_needed : type a. a typ ->  bool =
  let open Ctypes_static in function
  | Void -> false
  | Primitive _ -> false
  | View { format_typ = Some ft ; _ } when ft == Evil_hack.format_typ -> false
  | OCaml _ -> false
  | _ -> true

let rec collect_info :
  type a. a Ctypes.fn
  -> Gen_c.info
  -> (unit -> string)
  -> Parsetree.expression option
  -> param_info list
  -> param_info list * ret_info =
  let open Ctypes_static in
  fun fn cinfo param_name eo accu ->
    match fn with
    | Returns t ->
      let ris_inline_ocaml_type = is_inline_ocaml_type t in
      let rext_ptype = ml_typ_of_return_typ t ~noalloc:cinfo.Gen_c.noalloc in
      let rext_ptype = match cinfo.Gen_c.return_errno with
      | false -> rext_ptype
      | true ->
        let t = ml_typ_of_return_typ Ctypes.sint ~noalloc:false in
        Typ.tuple [rext_ptype; t] in
      let rmatch_pat, res_trans = pat_expand_out ?type_expr:eo  t param_name in
      let res_trans = match cinfo.Gen_c.return_errno, res_trans with
      | false, _ | true, None -> res_trans
      | true, Some f ->
        let res_expr,res_pat = mk_ex_pat @@ param_name () in
        let errno_expr,errno_pat = mk_ex_pat @@ param_name () in
        let f expr =
          let res_expr = f res_expr in
          [%expr
            let ([%p res_pat],[%p errno_pat]) = [%e expr] in
            [%e res_expr],
            [%e errno_expr]] in
        Some f in
      let rannot_needed = rmatch_pat <> None in (* ?? *)
      List.rev accu, {rext_ptype ; rmatch_pat ; rannot_needed ;
                      res_trans; ris_inline_ocaml_type}
    | Function(a,b) ->
      let fun_expr,fun_pat = mk_ex_pat @@ param_name () in
      let match_pat,param_trans = pat_expand_in a fun_expr param_name in
      let ext_ptype = ml_typ_of_arg_typ a ~noalloc:cinfo.Gen_c.noalloc in
      let label = Asttypes.Nolabel in
      let is_inline_ocaml_type = is_inline_ocaml_type a in
      let annot_needed = annot_needed a in
      let i = {
        ext_ptype; fun_pat; fun_expr; annot_needed;
        match_pat; param_trans; label; is_inline_ocaml_type} in
      collect_info b cinfo param_name eo (i::accu)

let collect_info a cinfo eo =
  collect_info a cinfo (create_param_name ()) eo []

(* (w1:'ppxc__t_1 -> 'ppxc__t_2 -> 'ppxc__t_3),
   ('ppxc__t_1 -> 'ppxc__t_2 -> 'ppxc__t_3) Ctypes_static.fn *)
let build_fun_constraint params ~return_errno =
  let length = List.length params in
  let var n = Typ.var (poly_prefix n) in
  let arrow with_label start =
    ListLabels.fold_right ~init:(pred length,start) params ~f:(fun p (i,last) ->
      let label = if with_label then p.label else Asttypes.Nolabel in
      pred i, Typ.arrow label (var i) last)
    |> snd in
  let t =
    let start =
      if return_errno = false then (var length)
      else
      let errno = Typ.constr (U.mk_lid "Signed.sint") [] in
      Typ.tuple [var length; errno] in
    arrow true start in
  let fn = arrow false (var length) in
  let lid = U.mk_lid "Ctypes_static.fn" in
  let fn = Typ.constr lid [fn] in
  t,fn

type result = {
  extern: Parsetree.structure_item;
  intern: Parsetree.structure_item;
}

let common fn ext_name cinfo common =
  let param_infos,ret_info = match common with
  | `Extern(el,ret) ->
    let param_infos,ret_info = collect_info fn cinfo (Some ret) in
    List.map2 el param_infos ~f:(fun (l,_) p -> { p with label = l }),
    ret_info
  | `Foreign_value _ | `Foreign _ -> collect_info fn cinfo None in
  let extern = build_external ~ocaml_name:ext_name param_infos ret_info cinfo in
  let ext_fun = mk_ex ext_name in
  let intern_expanded = build_fun param_infos ret_info ~ext_fun in
  let typ,typ_fn = build_fun_constraint param_infos
      ~return_errno:cinfo.Gen_c.return_errno in
  let build_stri already_constrained res =
    let res = Exp.constraint_ res typ in
    let ext_fun_pat = U.mk_pat ext_name in
    let no_type_repeat () =
      match common with
      | `Foreign _ | `Extern _ -> [%stri let [%p ext_fun_pat] = [%e res]]
      | `Foreign_value _ -> [%stri let [%p ext_fun_pat] = [%e res] ()] in
    if already_constrained then
      no_type_repeat ()
    else
    let dexpr = match common with
    | `Foreign fn_expr ->
      let e = Exp.constraint_ fn_expr typ_fn in
      Some [%expr if false then [%e ignore_fun ()] [%e e]]
    | `Extern(el,retexpr) | `Foreign_value(el,retexpr) ->
      match build_ignore_expr (el,param_infos) (retexpr,ret_info) with
      | None -> None
      | Some e -> Some e in
    match dexpr with
    | None -> no_type_repeat ()
    | Some dexpr ->
      match common with
      | `Foreign _ | `Extern _ ->
        [%stri let [%p ext_fun_pat] = [%e res]
          and _ = [%e dexpr]]
      | `Foreign_value _ ->
        [%stri let [%p ext_fun_pat] = [%e res] ()
          and _ = [%e dexpr]] in
  let intern = match intern_expanded with
  | None ->
    if false = List.exists param_infos ~f:(fun x -> x.annot_needed) then
      let vb = Vb.mk ~attrs:[Attributes.remove_attrib] [%pat? ()] [%expr ()] in
      Str.value Asttypes.Nonrecursive [vb]
    else
      build_stri false ext_fun
  | Some func ->
    let constr_used,res = match common with
    | `Extern(el,retexpr) | `Foreign_value(el,retexpr) ->
      false,
      build_parallel_pattern (el,param_infos) (retexpr,ret_info) func
    | `Foreign fn_expr ->
      let fn_expr = Exp.constraint_ fn_expr typ_fn in
      match build_pattern fn fn_expr param_infos ret_info ~func with
      | None -> false, func
      | Some pat -> true, pat in
    build_stri constr_used res in
  {extern; intern}

let external' fn name el retexpr cinfo =
  common fn name cinfo (`Extern(el,retexpr))

let foreign_value fn name retexpr cinfo =
  let el = [Asttypes.Nolabel,[%expr void]] in
  common fn name cinfo (`Foreign_value(el,retexpr))

let foreign fn name cinfo fn_expr =
  common fn name cinfo (`Foreign fn_expr)
