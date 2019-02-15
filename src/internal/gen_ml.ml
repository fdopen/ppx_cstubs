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

open Mparsetree.Ast_cur
open Ast_helper
open Parsetree
open Ctypes
module List = CCListLabels
module CS = Ctypes_static

let create_param_name () =
  let i = ref 0 in
  fun () ->
    let s = Printf.sprintf "ppxc__x%d\n" !i in
    incr i ;
    s

module U = Std.Util

let mk_attrs = function None -> [] | Some s -> [(U.mk_loc s, PStr [])]

let mk_ex ?attrs s = Exp.ident ~attrs:(mk_attrs attrs) (U.mk_lid s)

let mk_ex_pat s = (mk_ex s, U.mk_pat s)

let mk_typ ?attrs ?(l = []) s =
  Typ.constr ~attrs:(mk_attrs attrs) (U.mk_lid s) l

let managed_buffer () = mk_typ "Cstubs_internals.managed_buffer"

let prim_supports_attr : type a.
    a Ctypes_primitive_types.prim -> cinfo:Gen_c.info -> bool =
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

let ident_of_ml_prim : type a.
    no_attr:bool -> a Ctypes_primitive_types.ml_prim -> Parsetree.core_type =
  let open Ctypes_primitive_types in
  fun ~no_attr t ->
    let f ~a t =
      if no_attr = false && Ocaml_config.version () >= (4, 3, 0) then
        mk_typ ~attrs:a t
      else mk_typ t
    in
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
  let v = Rtag (U.mk_loc s, [], true, []) in
  let v = Typ.variant [v] Asttypes.Closed None in
  mk_typ ~l:[Typ.any (); v] "Ctypes.structured"

let mk_ocaml_typ s = mk_typ ~l:[mk_typ s] "Ctypes_static.ocaml"

let rec typ_of_ctyp : type a. mod_path:string list -> a typ -> 'b =
  let open Ctypes_static in
  fun ~mod_path t ->
    match Created_types.get_core_type t mod_path with
    | `Complete s -> (`Complete, s)
    | `Incomplete s -> (`Incomplete, s)
    | `Unknown -> (
      match t with
      | Void -> (`Complete, mk_typ "unit")
      | Primitive p ->
        ( `Complete
        , ident_of_ml_prim ~no_attr:true (Ctypes_primitive_types.ml_prim p) )
      | Pointer x ->
        let i, l = typ_of_ctyp ~mod_path x in
        (i, mk_typ ~l:[l] "Ctypes_static.ptr")
      | Funptr x ->
        let i, l = typ_of_fn ~mod_path x in
        (i, mk_typ ~l:[l] "Ctypes_static.static_funptr")
      | Struct _ -> (`Incomplete, mk_struct_typ "Struct")
      | Union _ -> (`Incomplete, mk_struct_typ "Union")
      | Abstract _ -> (`Incomplete, mk_typ ~l:[Typ.any ()] "Ctypes_static.ptr")
      | View _ -> (`Incomplete, Typ.any ())
      | Array (x, _) ->
        let i, l = typ_of_ctyp ~mod_path x in
        (i, mk_typ ~l:[l] "Ctypes_static.carray")
      | Bigarray _ -> (`Incomplete, Typ.any ())
      | OCaml String -> (`Complete, mk_ocaml_typ "string")
      | OCaml Bytes -> (`Complete, mk_ocaml_typ "bytes")
      | OCaml FloatArray ->
        ( `Complete
        , mk_typ ~l:[mk_typ ~l:[mk_typ "float"] "array"] "Ctypes_static.ocaml"
        ) )

and typ_of_fn : type a. mod_path:string list -> a fn -> 'b =
 fun ~mod_path -> function
  | CS.Returns a -> typ_of_ctyp ~mod_path a
  | CS.Function (a, b) ->
    let i1, a = typ_of_ctyp ~mod_path a in
    let i2, b = typ_of_fn ~mod_path b in
    ( (if i1 = `Complete && i2 = `Complete then `Complete else `Incomplete)
    , Typ.arrow Asttypes.Nolabel a b )

let constraint_of_typ ~mod_path t =
  let _, s = typ_of_ctyp ~mod_path t in
  U.mk_typc ~l:[s] "Ctypes.typ"

let create_struct ~mod_path ~type_name ~field_names ~locs str =
  let rec iter ~mod_path names locs fields i params ac =
    match (names, locs, fields) with
    | [], [], [] ->
      let params = List.rev params in
      let fields = List.rev ac in
      let n = U.mk_loc type_name in
      (Type.mk ~params ~kind:(Ptype_record fields) n, List.map ~f:fst params)
    | [], _, _ | _, [], _ | _, _, [] -> assert false
    | name :: tl1, loc :: tl2, CS.BoxedField {CS.ftype; _} :: tl3 ->
      U.with_loc loc
      @@ fun () ->
      let t, i, params =
        match typ_of_ctyp ~mod_path ftype with
        | `Incomplete, _ ->
          let s =
            if i > 26 then "z" ^ string_of_int i
            else String.init 1 (fun _ -> Char.chr (97 + i))
          in
          let t = Typ.mk ~loc (Ptyp_var s) in
          let a = (t, Asttypes.Invariant) in
          (t, succ i, a :: params)
        | `Complete, t -> (t, i, params)
      in
      let field = Type.field (U.mk_loc name) t in
      iter ~mod_path tl1 tl2 tl3 i params (field :: ac)
  in
  let rec unbox : type a. a typ -> 'b = function
    | CS.View {CS.ty; _} -> unbox ty
    | CS.Struct {CS.fields; _} ->
      (* twice. Once with referefence from within the top module, once from the
         bottom *)
      let res1, sl = iter ~mod_path field_names locs fields 0 [] [] in
      let res2, _ = iter ~mod_path:[] field_names locs fields 0 [] [] in
      (res1, res2, sl)
    | _ -> assert false
  in
  unbox str

let ml_typ_of_arg_typ ~mod_path ~cinfo t =
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
      | Void -> (`Complete, mk_typ "unit")
      | Primitive p ->
        if inside_view && prim_supports_attr p ~cinfo = false then
          (`Incomplete, Typ.any ())
        else
          ( `Complete
          , ident_of_ml_prim ~no_attr:false (Ctypes_primitive_types.ml_prim p)
          )
      | Pointer x -> (
        match typ_of_ctyp ~mod_path x with
        | `Incomplete, _ -> (`Incomplete, mk_typ ~l:[Typ.any ()] "Ctypes.ptr")
        | `Complete, x -> (`Complete, mk_typ ~l:[x] "Ctypes_static.ptr") )
      | Funptr fn -> (
        match typ_of_fn ~mod_path fn with
        | `Incomplete, _ ->
          (`Incomplete, mk_typ ~l:[Typ.any ()] "Ctypes_static.static_funptr")
        | `Complete, s ->
          (`Complete, mk_typ ~l:[s] "Ctypes_static.static_funptr") )
      | Struct _ as s -> typ_of_ctyp ~mod_path s
      | Union _ as u -> typ_of_ctyp ~mod_path u
      | Abstract _ -> (`Incomplete, mk_typ ~l:[Typ.any ()] "Ctypes.ptr")
      | View {ty = Struct {tag; _}; format_typ = Some ft; _}
        when ft == Evil_hack.format_typ ->
        (`Complete, Marshal.from_string tag 0)
      | View {ty; _} -> iter true ty
      | Array _ as a ->
        U.error "Unexpected array in an argument type: %s"
          (Ctypes.string_of_typ a)
      | Bigarray _ as a ->
        U.error "Unexpected bigarray in an argument type: %s"
          (Ctypes.string_of_typ a)
      | OCaml String -> (`Complete, mk_ocaml_typ "string")
      | OCaml Bytes -> (`Complete, mk_ocaml_typ "bytes")
      | OCaml FloatArray ->
        let float = mk_typ "float" in
        let array = mk_typ ~l:[float] "array" in
        (`Complete, mk_typ ~l:[array] "Ctypes_static.ocaml")
  in
  iter false t

let rec ml_typ_of_return_typ : type a.
    a typ -> inside_view:bool -> cinfo:Gen_c.info -> Parsetree.core_type =
  let open Ctypes_static in
  let mk_ptr cinfo =
    let attrs =
      if
        cinfo.Gen_c.return_errno = false && Ocaml_config.version () >= (4, 3, 0)
      then Some "unboxed"
      else None
    in
    mk_typ ?attrs "nativeint"
  in
  fun t ~inside_view ~cinfo ->
    match t with
    | Void -> if inside_view then Typ.any () else mk_typ "unit"
    | Primitive p ->
      if inside_view && prim_supports_attr p ~cinfo = false then Typ.any ()
      else
        ident_of_ml_prim ~no_attr:cinfo.Gen_c.return_errno
          (Ctypes_primitive_types.ml_prim p)
    | Struct _ -> managed_buffer ()
    | Union _ -> managed_buffer ()
    | Abstract _ -> managed_buffer ()
    | Pointer _ -> mk_ptr cinfo
    | Funptr _ -> mk_ptr cinfo
    | View {ty = Struct {tag; _}; format_typ = Some ft; _}
      when ft == Evil_hack.format_typ ->
      Marshal.from_string tag 0
    | View {ty; _} -> ml_typ_of_return_typ ty ~cinfo ~inside_view:true
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

let pat_expand_prim : type a.
    a Ctypes_primitive_types.prim -> Parsetree.pattern =
  let open Ctypes_primitive_types in
  function
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

let extract_pointer opt =
  let open Longident in
  let open Asttypes in
  fun e ->
    match e.pexp_desc with
    | Pexp_apply
        ( { pexp_desc = Pexp_ident {txt = Ldot (Lident "Ctypes", ptr); loc = _}
          ; pexp_attributes = []
          ; _ }
        , [(Nolabel, ({pexp_desc = Pexp_ident _; _} as a))] )
      when (ptr = "ptr" && opt = false) || (opt && ptr = "ptr_opt") ->
      Some a
    | _ -> None

let extract_pointer_opt = extract_pointer true

let extract_pointer = extract_pointer false

let pat_expand_in t ~fparam ~cinfo ?type_expr param_name =
  let error t =
    let x = Ctypes.string_of_typ t in
    U.error "cstubs does not support passing %s as parameter" x
  in
  let rec iter : type a.
         a typ
      -> fparam:Parsetree.expression
      -> ?type_expr:Parsetree.expression
      -> bool
      -> Parsetree.pattern option * Parsetree.expression option =
    let open Ctypes_static in
    fun t ~fparam ?type_expr inside_view ->
      match t with
      | Void -> (None, None)
      | Primitive p ->
        let r =
          if inside_view && prim_supports_attr ~cinfo p then
            let p = pat_expand_prim p in
            Some [%pat? Ctypes_static.Primitive [%p p]]
          else None
        in
        (r, None)
      | Pointer _ -> (None, None)
      | Funptr _ -> (None, None)
      | Struct _ -> (None, None)
      | Union _ -> (None, None)
      | OCaml _ -> (None, None)
      | View {format_typ = Some ft; _} when ft == Evil_hack.format_typ ->
        (None, None)
      | View {ty; _} -> (
        match inline_view t ty ?type_expr ~fparam inside_view with
        | Some s -> s
        | None ->
          let f, p = mk_ex_pat @@ param_name () in
          let nfparam = [%expr [%e f] [%e fparam]] in
          let pat2, nexpr2 = iter ty ~fparam:nfparam true in
          let pat =
            match pat2 with
            | None ->
              [%pat? Ctypes_static.View {Ctypes_static.write = [%p p]; _}]
            | Some pat2 ->
              [%pat?
                Ctypes_static.View
                  { Ctypes_static.write = [%p p]
                  ; Ctypes_static.ty = [%p pat2]
                  ; _ }]
          in
          let expr = match nexpr2 with None -> nfparam | Some x -> x in
          (Some pat, Some expr) )
      | Abstract _ -> error t
      | Array _ -> error t
      | Bigarray _ -> error t
  (* various dirty tricks to reduce the lines of boilerplate code *)
  and inline_view : type a b.
         a typ
      -> b typ
      -> fparam:Parsetree.expression
      -> ?type_expr:Parsetree.expression
      -> bool
      -> (Parsetree.pattern option * Parsetree.expression option) option =
   fun tparent tchild ~fparam ?type_expr inside_view ->
    if Created_types.is_typedef_struct tparent then
      let pat_trans = iter tchild ~fparam ?type_expr inside_view in
      match pat_trans with None, None -> Some pat_trans | _ -> None
    else if tparent == Obj.magic Ctypes.string then
      let e = [%expr Ppx_cstubs_internals.string_write [%e fparam]] in
      Some (None, Some e)
    else if tparent == Obj.magic Ctypes.string_opt then
      let e = [%expr Ppx_cstubs_internals.string_opt_write [%e fparam]] in
      Some (None, Some e)
    else
      match (inside_view, tchild, type_expr) with
      | false, CS.Pointer _, Some x -> (
        match extract_pointer_opt x with
        | Some orig_expr ->
          let res =
            [%expr
              match [%e fparam] with
              | None -> Ctypes.from_voidp [%e orig_expr] Ctypes.null
              | Some x -> x]
          in
          Some (None, Some res)
        | None -> None )
      | _ -> None
  in
  iter t ?type_expr ~fparam false

let pat_expand_out ?type_expr ~cinfo t param_name =
  let error t =
    let x = Ctypes.string_of_typ t in
    U.error "cstubs does not support %s as return values" x
  in
  let rec iter : type a.
         ?type_expr:Parsetree.expression
      -> a typ
      -> bool
      -> Parsetree.pattern option
         * (Parsetree.expression -> Parsetree.expression) option =
   fun ?type_expr t inside_view ->
    let open Ctypes_static in
    let union_struct is_struct =
      match type_expr with
      | None ->
        let s = param_name () in
        let e = mk_ex s in
        let p =
          match is_struct with
          | false -> [%pat? Ctypes_static.Union _]
          | true -> [%pat? Ctypes_static.Struct _]
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
    | Primitive p ->
      ( ( if inside_view && prim_supports_attr ~cinfo p then
          let p = pat_expand_prim p in
          Some [%pat? Ctypes_static.Primitive [%p p]]
        else None )
      , None )
    | Pointer _ -> (
      let f e expr =
        [%expr
          Cstubs_internals.make_ptr [%e e]
            (Ppx_cstubs_internals.to_voidp [%e expr])]
      in
      let e =
        match type_expr with None -> None | Some e -> extract_pointer e
      in
      match e with
      | None ->
        let e, p = mk_ex_pat @@ param_name () in
        (Some [%pat? Ctypes_static.Pointer [%p p]], Some (f e))
      | Some e -> (None, Some (f e)) )
    | Funptr _ ->
      let e, p = mk_ex_pat @@ param_name () in
      let f expr =
        [%expr
          Cstubs_internals.make_fun_ptr [%e e]
            (Ppx_cstubs_internals.to_voidp [%e expr])]
      in
      (Some [%pat? Ctypes_static.Funptr [%p p]], Some f)
    | Struct _ -> union_struct true
    | Union _ -> union_struct false
    | View {format_typ = Some ft; _} when ft == Evil_hack.format_typ ->
      (None, None)
    | View {ty; _} -> (
      match inline_view ?type_expr t ty inside_view with
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
          | None -> [%pat? Ctypes_static.View {Ctypes_static.read = [%p p]; _}]
          | Some pat2 ->
            [%pat?
              Ctypes_static.View
                {Ctypes_static.read = [%p p]; Ctypes_static.ty = [%p pat2]; _}]
        in
        (Some pat, Some f) )
    | Abstract _ -> error t
    | OCaml _ -> error t
    | Array _ -> error t
    | Bigarray _ -> error t
  and inline_view : type a b.
         ?type_expr:Parsetree.expression
      -> a typ
      -> b typ
      -> bool
      -> ( Parsetree.pattern option
         * (Parsetree.expression -> Parsetree.expression) option )
         option =
   fun ?type_expr tparent tchild inside_view ->
    if Created_types.is_typedef_struct tparent then
      let pat_trans = iter ?type_expr tchild inside_view in
      match pat_trans with None, None -> Some pat_trans | _ -> None
    else if tparent == Obj.magic Ctypes.string then
      let f expr = [%expr Ppx_cstubs_internals.string_read [%e expr]] in
      Some (None, Some f)
    else if tparent == Obj.magic Ctypes.string_opt then
      let f expr = [%expr Ppx_cstubs_internals.string_opt_read [%e expr]] in
      Some (None, Some f)
    else
      match (inside_view, tchild, type_expr) with
      | false, CS.Pointer _, Some x -> (
        match extract_pointer_opt x with
        | Some orig_expr ->
          let f expr =
            [%expr
              match [%e expr] with
              | 0n -> None
              | ppxc__not_null ->
                Some
                  (Cstubs_internals.make_ptr [%e orig_expr]
                     (Ppx_cstubs_internals.to_voidp ppxc__not_null))]
          in
          Some (None, Some f)
        | None -> None )
      | _ -> None
  in
  iter ?type_expr t false

type param_info =
  { ext_ptype : core_type
  ; (* what to use in external : ... *)
    fun_pat : pattern
  ; (* ocaml function pattern fun a .. -> *)
    fun_expr : expression
  ; (* pattern above as expr in the function body *)
    (* info extraction with pattern matching necessary ? *)
    match_pat : pattern option
  ; (* if the parameter needs to be tranformed first .. *)
    param_trans : expression option
  ; label : Asttypes.arg_label
  ; is_inline_ocaml_type : bool
  ; annot_needed : bool
  ; (* do I need to repeat the Ctype.typ to type the generated function *)
    constr_ptype : core_type (* x y z Ctypes.typ expanded as far as possible *)
  }

type ret_info =
  { rext_ptype : core_type
  ; rmatch_pat : pattern option
  ; res_trans : (expression -> expression) option
  ; ris_inline_ocaml_type : bool
  ; rannot_needed : bool
  ; rconstr_ptype : core_type }

let build_external ~ocaml_name param_infos ret_info cinfo =
  let prim =
    if Ocaml_config.version () >= (4, 3, 0) then []
    else
      let l = if cinfo.Gen_c.noalloc then ["noalloc"] else [] in
      if cinfo.Gen_c.float then "float" :: l else l
  in
  let prim = cinfo.Gen_c.stub_name :: prim in
  let prim =
    match cinfo.Gen_c.stub_name_byte with None -> prim | Some s -> s :: prim
  in
  let name = U.mk_loc ocaml_name in
  let t =
    ListLabels.fold_right param_infos ~init:ret_info.rext_ptype
      ~f:(fun a acc -> Typ.arrow a.label a.ext_ptype acc )
  in
  let attrs =
    if Ocaml_config.version () < (4, 3, 0) then []
    else
      match cinfo.Gen_c.noalloc with
      | false -> []
      | true -> [(U.mk_loc "noalloc", PStr [])]
  in
  let p = Val.mk ~attrs ~prim name t in
  Str.primitive p

(* (fun ~p1:ppxc__x0 -> fun ~p2:ppxc__x1 -> fun ppxc__x2 -> res_transform
   ($ext_fun (param_trans ppxc__x0) ...) ) *)
let build_fun param_infos ret_info ~ext_fun =
  let is_simple_fun =
    List.for_all param_infos ~f:(fun x ->
        x.match_pat = None && x.param_trans = None )
    && ret_info.rmatch_pat = None
    && ret_info.res_trans = None
  in
  if is_simple_fun then None
  else
    let l =
      List.map param_infos ~f:(fun a ->
          (a.label, match a.param_trans with Some a -> a | None -> a.fun_expr)
      )
    in
    let fbody = Exp.apply ext_fun l in
    let fbody =
      match ret_info.res_trans with None -> fbody | Some f -> f fbody
    in
    Some
      (ListLabels.fold_right ~init:fbody param_infos ~f:(fun a ac ->
           Exp.fun_ a.label None a.fun_pat ac ))

let poly_prefix pos = "ppxc__t_" ^ string_of_int pos

let ctypes_typ_constr expr i ct =
  let expr =
    match ct.ptyp_desc with
    | Ptyp_constr
        ({txt = Ldot (Lident "Ctypes", "typ"); _}, [{ptyp_desc = Ptyp_any; _}])
      ->
      expr
    | _ -> Exp.constraint_ expr ct
  in
  let t = Typ.var (poly_prefix i) in
  let t = U.mk_typc ~l:[t] "Ctypes.typ" in
  Exp.constraint_ expr t

let stdlib_fun s =
  let prefix =
    let ver = Ocaml_config.version () in
    if ver >= (4, 8, 0) then "Stdlib."
    else if ver >= (4, 7, 0) then "Stdlib.Pervasives."
    else "Pervasives."
  in
  U.mk_ident (prefix ^ s)

let ignore_fun () = stdlib_fun "ignore"

(* if false then ( Stdlib.Pervasives.ignore (Ctypes.ptr void : 'ppxc__t_0
   Ctypes.typ); Stdlib.Pervasives.ignore ( ... : 'ppxc__t_1 Ctypes.typ); ... ) *)
let build_ignore_expr (el, params) (retexpr, ret_info) =
  let f ac i e match_pat annot_needed ct =
    match (match_pat, annot_needed) with
    | None, false | Some _, _ -> ac
    | _, true -> (
      let e = ctypes_typ_constr e i ct in
      let e = [%expr [%e ignore_fun ()] [%e e]] in
      match ac with None -> Some e | Some x -> Some (Exp.sequence x e) )
  in
  let i, e =
    ListLabels.fold_left2 ~init:(0, None) el params ~f:(fun (i, ac) (_, e) a ->
        let ac = f ac i e a.match_pat a.annot_needed a.constr_ptype in
        (succ i, ac) )
  in
  let e =
    f e i retexpr ret_info.rmatch_pat ret_info.rannot_needed
      ret_info.rconstr_ptype
  in
  match e with None -> None | Some s -> Some [%expr if false then [%e s]]

(* match (t1 : 'ppxc__t_1 Ctypes.typ), (t2 : 'ppxc__t_0 Ctypes.typ) with |
   (Ctypes_static.View ..), (Ctypes_static.Foo ..) -> $func | _ ->
   Ppx_cstubs_internals.invalid_code () *)
let build_parallel_pattern (el, param_infos) (retexpr, ret_info) func =
  let cnt = ref 0 in
  let l =
    ListLabels.map2 el param_infos ~f:(fun (_, expr) a ->
        let i = !cnt in
        incr cnt ;
        match a.match_pat with
        | None -> None
        | Some s -> Some (ctypes_typ_constr expr i a.constr_ptype, s) )
    |> CCList.filter_map Std.identity
  in
  let l =
    match ret_info.rmatch_pat with
    | None -> l
    | Some s -> (ctypes_typ_constr retexpr !cnt ret_info.rconstr_ptype, s) :: l
  in
  if l = [] then func
  else
    let dummy_end =
      { pc_lhs = Pat.any ()
      ; pc_guard = None
      ; pc_rhs = [%expr Ppx_cstubs_internals.invalid_code ()] }
    in
    let attrs = [(U.mk_loc "ocaml.warning", PStr [[%stri "-4"]])] in
    match l with
    | [(expr, pat)] ->
      let cl = [{pc_lhs = pat; pc_guard = None; pc_rhs = func}; dummy_end] in
      Exp.match_ ~attrs expr cl
    | l ->
      let expr = Exp.tuple (List.map ~f:fst l) in
      let cl =
        [ { pc_lhs = Pat.tuple (List.map ~f:snd l)
          ; pc_guard = None
          ; pc_rhs = func }
        ; dummy_end ]
      in
      Exp.match_ ~attrs expr cl

(* match $fn_expr with | Ctypes_static.Function(...,Ctypes_static.Function(...,
   Ctypes_static.Returns(...))) -> $func | _ ->
   Ppx_cstubs_internals.invalid_code () with $f_expr: (t1 @-> t2 @-> return t3)
   : ('ppxc__t_1 -> 'ppxc__t_2 -> 'ppxc__t_3) Ctypes.fn *)
let build_pattern fn fn_expr param_infos ret_info ~func =
  let rec iter : type a. a Ctypes.fn -> 'b -> 'c =
    let open Ctypes_static in
    fun a b ->
      match (a, b) with
      | Returns _, [] -> (
        match ret_info.rmatch_pat with
        | None -> None
        | Some s -> Some [%pat? Ctypes_static.Returns [%p s]] )
      | Returns _, _ -> failwith "parameter mismatch"
      | Function (_, b), hd :: tl -> (
        match iter b tl with
        | None -> (
          match hd.match_pat with
          | None -> None
          | Some s -> Some [%pat? Ctypes_static.Function ([%p s], _)] )
        | Some s -> (
          match hd.match_pat with
          | None -> Some [%pat? Ctypes_static.Function (_, [%p s])]
          | Some x -> Some [%pat? Ctypes_static.Function ([%p x], [%p s])] ) )
      | Function _, [] -> failwith "parameter mismatch"
  in
  match iter fn param_infos with
  | None -> None
  | Some pat ->
    let dummy_end =
      { pc_lhs = Pat.any ()
      ; pc_guard = None
      ; pc_rhs = [%expr Ppx_cstubs_internals.invalid_code ()] }
    in
    let attrs = [(U.mk_loc "ocaml.warning", PStr [[%stri "-4"]])] in
    let cl = [{pc_lhs = pat; pc_guard = None; pc_rhs = func}; dummy_end] in
    Some (Exp.match_ ~attrs fn_expr cl)

let is_inline_ocaml_type : type a. a Ctypes.typ -> bool =
  let open Ctypes_static in
  function
  | View {format_typ = Some f; _} when f == Evil_hack.format_typ -> true
  | _ -> false

let collect_info fn ~mod_path cinfo lexpr =
  let param_name = create_param_name () in
  let rec iter : type a.
         a Ctypes.fn
      -> Parsetree.expression list
      -> param_info list
      -> param_info list * ret_info =
   fun fn lexpr accu ->
    let type_expr, loc, lexpr' =
      match lexpr with
      | [] -> (None, !Ast_helper.default_loc, [])
      | hd :: tl -> (Some hd, hd.pexp_loc, tl)
    in
    U.with_loc loc
    @@ fun () ->
    match fn with
    | CS.Returns t ->
      let ris_inline_ocaml_type = is_inline_ocaml_type t in
      let rext_ptype = ml_typ_of_return_typ t ~cinfo in
      let rext_ptype =
        match cinfo.Gen_c.return_errno with
        | false -> rext_ptype
        | true ->
          let t = ml_typ_of_return_typ Ctypes.sint ~cinfo in
          Typ.tuple [rext_ptype; t]
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
      let rconstr_ptype = constraint_of_typ ~mod_path t in
      ( List.rev accu
      , { rext_ptype
        ; rmatch_pat
        ; rannot_needed
        ; res_trans
        ; ris_inline_ocaml_type
        ; rconstr_ptype } )
    | CS.Function (a, b) ->
      let fun_expr, fun_pat = mk_ex_pat @@ param_name () in
      let match_pat, param_trans =
        pat_expand_in ~cinfo ?type_expr a ~fparam:fun_expr param_name
      in
      let type_info, ext_ptype = ml_typ_of_arg_typ a ~cinfo ~mod_path in
      let label = Asttypes.Nolabel in
      let is_inline_ocaml_type = is_inline_ocaml_type a in
      let annot_needed = type_info <> `Complete && param_trans = None in
      let constr_ptype = constraint_of_typ ~mod_path a in
      let i =
        { ext_ptype
        ; fun_pat
        ; fun_expr
        ; annot_needed
        ; constr_ptype
        ; match_pat
        ; param_trans
        ; label
        ; is_inline_ocaml_type }
      in
      iter b lexpr' (i :: accu)
  in
  iter fn lexpr []

(* (w1:'ppxc__t_1 -> 'ppxc__t_2 -> 'ppxc__t_3), ('ppxc__t_1 -> 'ppxc__t_2 ->
   'ppxc__t_3) Ctypes_static.fn *)
let build_fun_constraint params ~return_errno =
  let length = List.length params in
  let var n = Typ.var (poly_prefix n) in
  let arrow with_label start =
    ListLabels.fold_right
      ~init:(pred length, start)
      params
      ~f:(fun p (i, last) ->
        let label = if with_label then p.label else Asttypes.Nolabel in
        (pred i, Typ.arrow label (var i) last) )
    |> snd
  in
  let vl = var length in
  let start =
    match return_errno with
    | false -> vl
    | true -> Typ.tuple [vl; mk_typ "Signed.sint"]
  in
  let t = arrow true start in
  let fn = arrow false vl in
  let fn = U.mk_typc ~l:[fn] "Ctypes_static.fn" in
  (t, fn)

type result =
  { extern : Parsetree.structure_item
  ; intern : Parsetree.structure_item }

let common ~mod_path fn ext_name cinfo common =
  let param_infos, ret_info =
    match common with
    | `Extern (el, ret) ->
      let exprl = List.rev (ret :: List.rev_map el ~f:snd) in
      let param_infos, ret_info = collect_info ~mod_path fn cinfo exprl in
      ( List.map2 el param_infos ~f:(fun (l, _) p -> {p with label = l})
      , ret_info )
    | `Foreign_value _ | `Foreign _ -> collect_info ~mod_path fn cinfo []
  in
  let extern =
    build_external ~ocaml_name:ext_name param_infos ret_info cinfo
  in
  let ext_fun = mk_ex ext_name in
  let intern_expanded = build_fun param_infos ret_info ~ext_fun in
  let typ, typ_fn =
    build_fun_constraint param_infos ~return_errno:cinfo.Gen_c.return_errno
  in
  let build_stri already_constrained res =
    let ext_fun_pat = U.mk_pat ext_name in
    let ext_fun_pat_c =
      (* Migrate parsetree doesn't seem to capture it *)
      if Ocaml_config.version () >= (4, 6, 0) then
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
        | `Extern (el, retexpr) | `Foreign_value (el, retexpr) -> (
          match build_ignore_expr (el, param_infos) (retexpr, ret_info) with
          | None -> None
          | Some e -> Some e )
      in
      match dexpr with
      | None -> no_type_repeat ()
      | Some dexpr -> (
        match common with
        | `Foreign _ | `Extern _ ->
          [%stri let [%p ext_fun_pat_c] = [%e res_c]

                 and _ = [%e dexpr]]
        | `Foreign_value _ ->
          [%stri let [%p ext_fun_pat] = [%e res_c] ()

                 and _ = [%e dexpr]] )
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
          ( false
          , build_parallel_pattern (el, param_infos) (retexpr, ret_info) func
          )
        | `Foreign fn_expr -> (
          let fn_expr = Exp.constraint_ fn_expr typ_fn in
          match build_pattern fn fn_expr param_infos ret_info ~func with
          | None -> (false, func)
          | Some pat -> (true, pat) )
      in
      build_stri constr_used res
  in
  {extern; intern}

let external' fn ~mod_path name el retexpr cinfo =
  common fn ~mod_path name cinfo (`Extern (el, retexpr))

let foreign_value fn ~mod_path name retexpr cinfo =
  let el = [(Asttypes.Nolabel, [%expr void])] in
  common fn ~mod_path name cinfo (`Foreign_value (el, retexpr))

let foreign fn ~mod_path name cinfo fn_expr =
  common fn ~mod_path name cinfo (`Foreign fn_expr)
