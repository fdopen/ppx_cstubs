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
open Std

let error = Util.error

module List = CCListLabels
module U = Std.Util

module Extract_types = struct
  type enum_entry = {
    cdecl : constructor_declaration;
    enum_cname : string;
  }

  type enum_type =
    | Enum_normal
    | Enum_bitmask
    | Enum_both

  type enum = {
    ename : string;
    el : enum_entry list;
    ename_c : string;
    enum_type : enum_type;
    etypedef : bool;
    edecl : type_declaration;
    eunexpected : Marshal_types.expr option;
    eunexpected_bits : Marshal_types.expr option;
  }

  type field = {
    field_name : string;
    field_expr : expression;
    field_cname : string;
    field_loc : Ast_helper.loc;
  }

  type struct_type =
    | Union
    | Struct_normal
    | Struct_record
    | Struct_both

  type structure = {
    sname : string;
    sl : field list;
    sname_c : string;
    stypedef : bool;
    stype : struct_type;
    sloc : Ast_helper.loc;
  }

  type ctyp =
    | Enum of enum
    | Struct of structure
end

module Extract : sig
  include module type of struct
    include Extract_types
  end

  val variable_from_pattern_constr : pattern -> string option * bool

  val variable_from_pattern : pattern -> string option

  val fun_def :
    lookup:string ->
    bool ->
    core_type ->
    expression * (arg_label * expression) list * bool

  val constant_string : expression -> string option

  val type_decl : rec_flag -> type_declaration list -> ctyp list
end = struct
  let rec variable_from_pattern_constr a =
    match a.ppat_desc with
    | Ppat_var { txt = "" | "_"; _ } -> (None, false)
    | Ppat_var s -> (Some s.txt, false)
    | Ppat_constraint (s, _) -> (fst (variable_from_pattern_constr s), true)
    | _ -> (None, false)

  let variable_from_pattern a = variable_from_pattern_constr a |> fst

  let unsupported_typ loc = error ~loc "type description not supported"

  let check_no_attribs = function
    | [] -> ()
    | a :: _ ->
      error ~loc:a.attr_loc "unsupported attribute: %S" a.attr_name.txt

  let check_no_attribs_t t = check_no_attribs t.ptyp_attributes

  let remove_attrib str l = List.filter l ~f:(fun x -> x.attr_name.txt <> str)

  let get_remove name attr =
    let at =
      List.exists attr ~f:(fun x ->
          if x.attr_name.txt <> name then false
          else
            match x.attr_payload with
            | PStr [] -> true
            | _ -> error ~loc:x.attr_loc "surprising content in %s" name)
    in
    (at, if not at then attr else remove_attrib name attr)

  let rec is_simple = function
    | Lident _ -> true
    | Ldot (t, _) -> is_simple t
    | Lapply _ -> false

  let rec type_to_ctype ~lookup typ =
    match typ.ptyp_desc with
    | Ptyp_constr (({ txt; _ } as a), []) when is_simple txt ->
      check_no_attribs_t typ;
      let c =
        match (lookup, txt) with
        | Some l, Lident l' when l = l' -> true
        | _ -> false
      in
      (Ast_helper.Exp.ident ~loc:a.loc a, c)
    | Ptyp_constr ({ txt = Lident "ptr"; loc }, [ a ]) ->
      check_no_attribs_t typ;
      let p, c = type_to_ctype ~lookup a in
      (([%expr Ctypes.ptr [%e p]] [@metaloc loc]), c)
    | Ptyp_constr ({ txt = Lident "ptr_opt"; loc }, [ a ]) ->
      check_no_attribs_t typ;
      let p, c = type_to_ctype ~lookup a in
      (([%expr Ctypes.ptr_opt [%e p]] [@metaloc loc]), c)
    | Ptyp_constr ({ txt = Lident "static_funptr"; loc }, [ a ]) ->
      check_no_attribs_t typ;
      let t, c = type_to_ctype_fn ~lookup a in
      (([%expr Ctypes.static_funptr [%e t]] [@metaloc loc]), c)
    | Ptyp_constr
        ({ txt = Lident (("funptr" | "funptr_opt") as ns); loc }, [ a ]) ->
      U.with_loc loc @@ fun () ->
      let t, c = type_to_ctype_fn ~lookup a in
      let check_errno, at = get_remove "check_errno" typ.ptyp_attributes in
      let runtime_lock, at = get_remove "release_runtime_lock" at in
      let thread_registration, at = get_remove "thread_registration" at in
      check_no_attribs at;
      let h = function true -> [%expr true] | false -> [%expr false] in
      let f =
        match ns with
        | "funptr_opt" -> [%expr Foreign.funptr_opt]
        | _ -> [%expr Foreign.funptr]
      in
      ( [%expr
          [%e f] ~check_errno:[%e h check_errno]
            ~runtime_lock:[%e h runtime_lock]
            ~thread_registration:[%e h thread_registration] [%e t]],
        c )
    | _ -> unsupported_typ typ.ptyp_loc

  and type_to_ctype_fn ~lookup typ =
    let loc = typ.ptyp_loc in
    check_no_attribs_t typ;
    match typ.ptyp_desc with
    | Ptyp_arrow
        ( Nolabel,
          ({ ptyp_desc = Ptyp_constr _; _ } as t1),
          ({ ptyp_desc = Ptyp_constr _; _ } as t2) ) ->
      let t1, c1 = type_to_ctype ~lookup t1
      and t2, c2 = type_to_ctype ~lookup t2 in
      (([%expr [%e t1] @-> returning [%e t2]] [@metaloc loc]), c1 || c2)
    | Ptyp_arrow
        ( Nolabel,
          ({ ptyp_desc = Ptyp_constr _; _ } as t1),
          ({ ptyp_desc = Ptyp_arrow _; _ } as t2) ) ->
      let t1, c1 = type_to_ctype ~lookup t1
      and t2, c2 = type_to_ctype_fn ~lookup t2 in
      (([%expr [%e t1] @-> [%e t2]] [@metaloc loc]), c1 || c2)
    | _ -> error ~loc "unsupported Ctypes.fn definition"

  let rec fun_def ~lookup is_inline accu found typ =
    let loc = typ.ptyp_loc in
    let type_conf is_inline t =
      let is_ocaml_typ, attribs = get_remove "ocaml_type" t.ptyp_attributes in
      if is_ocaml_typ = false || is_inline = false then type_to_ctype ~lookup t
      else
        let () = check_no_attribs attribs in
        let t = { t with ptyp_attributes = attribs } in
        let e = U.marshal_to_str_expr t in
        (([%expr Ctypes.ppxc__private_ocaml_typ [%e e]] [@metaloc loc]), false)
    in
    match typ.ptyp_desc with
    | Ptyp_constr _ ->
      if accu = [] then error ~loc "function expected";
      let r, f = type_conf is_inline typ in
      (r, List.rev accu, f || found)
    | Ptyp_arrow (l, t1, t2) ->
      check_no_attribs_t typ;
      let t1', found' = type_conf is_inline t1 in
      (match l with
      | Nolabel | Labelled _ -> ()
      | Optional _ ->
        error ~loc:t1.ptyp_loc "optional parameters are not supported");
      fun_def is_inline ~lookup ((l, t1') :: accu) (found' || found) t2
    | _ -> unsupported_typ typ.ptyp_loc

  let fun_def ~lookup i t = fun_def ~lookup:(Some lookup) i [] false t

  let constant_string e =
    match e.pexp_desc with
    | Pexp_constant (Pconst_string (s, _, _)) -> Some s
    | _ -> None

  let get_cname ~def l =
    let res =
      List.find_map l ~f:(fun x ->
          if x.attr_name.txt <> "cname" then None
          else
            match x.attr_payload with
            | PStr
                [
                  {
                    pstr_desc =
                      Pstr_eval
                        ( {
                            pexp_desc = Pexp_constant (Pconst_string (x, _, _));
                            _;
                          },
                          [] );
                    _;
                  };
                ] ->
              Some x
            | _ -> error ~loc:x.attr_loc "unsupported expression in cname")
    in
    match res with None -> (def, l) | Some x -> (x, remove_attrib "cname" l)

  let get_unexpected unexpected l =
    let res =
      List.find_map l ~f:(fun x ->
          if x.attr_name.txt <> unexpected then None
          else
            match x.attr_payload with
            | PStr
                [
                  {
                    pstr_desc =
                      Pstr_eval (({ pexp_desc = Pexp_fun _; _ } as e), []);
                    _;
                  };
                ] ->
              Some e
            | _ ->
              error ~loc:x.attr_loc "unsupported expression in %s" unexpected)
    in
    match res with
    | None -> (None, l)
    | Some _ as x -> (x, remove_attrib unexpected l)

  let get_unexpected_bits = get_unexpected "unexpected_bits"

  let get_unexpected = get_unexpected "unexpected"

  include Extract_types

  let extract_enum_entry = function
    | {
        pcd_name;
        pcd_args = Pcstr_tuple [];
        pcd_res = None;
        pcd_loc = _;
        pcd_attributes;
      } as whole ->
      let name = pcd_name.txt in
      let cname, attribs = get_cname ~def:name pcd_attributes in
      check_no_attribs attribs;
      let cdecl = { whole with pcd_attributes = [] } in
      { cdecl; enum_cname = cname }
    | { pcd_loc; _ } -> error ~loc:pcd_loc "Unsupported constructor type"

  let extract_field lookup
      { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } =
    if pld_mutable <> Immutable then
      error ~loc:pld_loc "only immutable is possible";
    let field_name = pld_name.txt in
    let field_expr, field_recursive = type_to_ctype ~lookup pld_type in
    if field_recursive then
      error ~loc:pld_loc "recursive record views (%s) are not possible"
        field_name;
    let field_cname, attribs = get_cname ~def:field_name pld_attributes in
    check_no_attribs attribs;
    { field_name; field_expr; field_cname; field_loc = pld_loc }

  let type_decl rec' = function
    | {
        ptype_name;
        ptype_params = [];
        ptype_cstrs = [];
        ptype_private = Public;
        ptype_manifest = _;
        ptype_attributes;
        ptype_kind = Ptype_variant (_ :: _ as l);
        ptype_loc;
      } as whole ->
      let ename = ptype_name.txt in
      let ename_c, attribs = get_cname ~def:ename ptype_attributes in
      let etypedef, attribs = get_remove "typedef" attribs in
      let ebitmask, attribs = get_remove "as_bitmask" attribs in
      let eunexpected, attribs = get_unexpected attribs in
      let eunexpected_bits, attribs = get_unexpected_bits attribs in
      let enum_type =
        match ebitmask with false -> Enum_normal | true -> Enum_bitmask
      in
      let with_bitmask, attribs = get_remove "with_bitmask" attribs in
      let enum_type =
        match with_bitmask with
        | false -> enum_type
        | true ->
          if enum_type <> Enum_normal then
            error ~loc:ptype_loc "either as_bitmask or with_bitmask - not both";
          Enum_both
      in
      check_no_attribs attribs;
      let el = List.map l ~f:extract_enum_entry in
      let edecl =
        {
          whole with
          ptype_kind = Ptype_variant (List.map el ~f:(fun x -> x.cdecl));
          ptype_attributes = [];
        }
      in
      Enum
        {
          ename;
          el;
          ename_c;
          etypedef;
          edecl;
          enum_type;
          eunexpected;
          eunexpected_bits;
        }
    | {
        ptype_name;
        ptype_params = [];
        ptype_cstrs = [];
        ptype_private = Public;
        ptype_manifest = None;
        ptype_attributes;
        ptype_kind = Ptype_record (_ :: _ as l);
        ptype_loc;
      } ->
      let sname = ptype_name.txt in
      let sname_c, attribs = get_cname ~def:sname ptype_attributes in
      let stypedef, attribs = get_remove "typedef" attribs in
      let union, attribs = get_remove "union" attribs in
      let as_record, attribs = get_remove "as_record" attribs in
      let with_record, attribs = get_remove "with_record" attribs in
      check_no_attribs attribs;
      let stype =
        match union with
        | true ->
          if as_record then
            error "@@@@union and @@@@as_record are mutually exclusive";
          if with_record then
            error "@@@@union and @@@@with_record are mutually exclusive";
          Union
        | false -> (
          match as_record with
          | true ->
            if with_record then
              error "@@@@as_record and @@@@with_record are mutually exclusive";
            Struct_record
          | false -> (
            match with_record with
            | true -> Struct_both
            | false -> Struct_normal))
      in
      let lookup =
        if rec' <> Recursive then None
        else
          match stype with
          | Union | Struct_normal -> None
          | Struct_record | Struct_both -> Some sname
      in
      let sl = List.map ~f:(extract_field lookup) l in
      Struct { sname; sl; sname_c; stypedef; stype; sloc = ptype_loc }
    | { ptype_loc; _ } -> error ~loc:ptype_loc "unsupported type definition"

  let type_decl rec' l = List.map ~f:(type_decl rec') l
end

module C_content = struct
  let write_file () =
    if Options.(!mode = Regular) then
      match (!Options.c_output_file, !Script_result.c_source) with
      | Some fln, Some source -> (
        Options.c_output_file := None;
        match fln with
        | "-" -> output_string stdout source
        | _ ->
          CCIO.with_out ~flags:[ Open_creat; Open_trunc; Open_binary ] fln
            (fun ch -> output_string ch source))
      | Some _, None ->
        prerr_endline
          "no c file necessary, remove it from the command-line invocation and update your build instructions";
        exit 2
      | None, None -> ()
      | None, Some _ ->
        prerr_endline "output file for c code missing";
        exit 2
end

module Id : sig
  type t = {
    id : int;
    script_param : expression;
    expr : expression;
    stri : structure_item;
  }

  val get : ?loc:Ast_helper.loc -> unit -> t

  val get_usage_id : unit -> expression * attributes

  val get_tdl_entries_id : unit -> int * structure_item
end = struct
  open Ast_helper

  type t = {
    id : int;
    script_param : expression;
    expr : expression;
    stri : structure_item;
  }

  let cnt = ref 0

  let with_id f =
    let id = !cnt in
    incr cnt;
    f id

  let get ?(loc = !Ast_helper.default_loc) () =
    with_id @@ fun id ->
    let (t : Marshal_types.id_loc_param) = (id, loc) in
    let loc_id_param = Marshal.to_string t [] in
    let script_param = U.str_expr ~loc loc_id_param in
    let attrs = [ Attributes.replace_expr_attrib ] in
    let expr = U.int_expr ~loc ~attrs id in
    let stri = Str.eval ~attrs expr in
    { id; script_param; expr; stri }

  let get_usage_id attr_string () =
    with_id @@ fun id ->
    let script_param = U.int_expr id in
    let loc = !Ast_helper.default_loc in
    let st = [%stri [%e script_param]] in
    let attrs =
      let x = U.mk_loc attr_string in
      [ Attr.mk x (PStr [ st ]) ]
    in
    (script_param, attrs)

  let get_usage_id = get_usage_id Attributes.replace_attr_string

  let get_tdl_entries_id () =
    with_id @@ fun id ->
    let expr = U.int_expr id in
    (id, Str.eval ~attrs:[ Attributes.tdl_attrib ] expr)
end

let htl_tdl_entries = Hashtbl.create 32

module OSTypes = Ptree.OSTypes

module H : sig
  val constant : (arg_label * expression) list -> expression

  val bound_constant : string -> (arg_label * expression) list -> expression

  val foreign_value : (arg_label * expression) list -> expression

  val header : (arg_label * expression) list -> expression

  val seal : (arg_label * expression) list -> expression

  val external' :
    is_inline:bool ->
    remove_labels:bool ->
    return_errno:bool ->
    release_runtime_lock:bool ->
    noalloc:bool ->
    attrs:Ast_helper.attrs ->
    value_description ->
    structure_item

  val foreign : ?prefix:string -> (arg_label * expression) list -> expression

  val fn : string -> expression -> expression

  val type_decl :
    enforce_union:bool ->
    c_bitmask:bool ->
    rec_flag ->
    type_declaration list ->
    structure_item

  val field : ?prefix:string -> (arg_label * expression) list -> expression

  val int_alias :
    mod_name:string ->
    [< `Aint | `Int | `Uint ] ->
    (arg_label * expression) list ->
    structure_item

  val opaque :
    string ->
    Ast_helper.attrs ->
    (arg_label * expression) list ->
    structure_item

  val abstract :
    string ->
    Ast_helper.attrs ->
    (arg_label * expression) list ->
    structure_item

  val ocaml_funptr :
    acquire_runtime:bool ->
    thread_registration:bool ->
    string ->
    expression ->
    core_type ->
    structure_item

  val type_cb :
    Ast_helper.str_opt ->
    string ->
    (arg_label * expression) list ->
    structure_item

  val typ : string -> expression -> expression

  val union : string -> (arg_label * expression) list -> structure_item

  val structure : string -> (arg_label * expression) list -> structure_item

  val pexp_const : string -> expression -> expression
end = struct
  open Ast_helper
  module Topscript = Ptree.Topscript
  module Impl_mod = Ptree.Impl_mod
  module Type_mod = Ptree.Type_mod

  let error_msg s = function
    | [] -> error "arguments missing for %s" s
    | _ -> error "too many or too few arguments for %s" s

  let constant = function
    | [ (Nolabel, str_expr); (Nolabel, type_expr) ] ->
      let loc = !Ast_helper.default_loc in
      let id = Id.get () in
      let tx = U.marshal_to_str_expr type_expr in
      let f f =
        [%stri
          let () =
            let (ppxc__s : string) = [%e str_expr]
            and (ppxc__t : _ Ctypes.typ) = [%e type_expr] in
            [%e f] [%e id.Id.script_param] ppxc__s [%e tx] ppxc__t]
      in
      Topscript.add_extract @@ f [%expr Ppxc__script.Extract.constant];
      Topscript.add_build_external @@ f [%expr Ppxc__script.Build.constant];
      let prefix =
        match Extract.constant_string str_expr with
        | None -> None
        | Some s -> Some (if String.length s < 20 then s else String.sub s 0 20)
      in
      let name = U.safe_mlname ?prefix () in
      Impl_mod.add_named ~retype:true ~name_check:false name id.Id.expr
    | l -> error_msg "constant" l

  let bound_constant name = function
    | [ (Nolabel, str_expr); (Nolabel, type_expr) ] ->
      (match Extract.constant_string str_expr with
      | Some _ -> ()
      | None ->
        U.error ~loc:str_expr.pexp_loc
          "'let%%c ... = constant' requires a string literal");
      let id = Id.get () in
      let loc = !Ast_helper.default_loc in
      Topscript.add_extract_phase0
        [%stri
          let () =
            let (ppxc__s : string) = [%e str_expr] in
            Ppxc__script.Extract_phase0.bound_constant [%e id.Id.script_param]
              ppxc__s];
      let tx = U.marshal_to_str_expr type_expr in
      let f f =
        [%stri
          let [%p U.mk_pat name] =
            let (ppxc__s : string) = [%e str_expr]
            and (ppxc__t : _ Ctypes.typ) = [%e type_expr] in
            [%e f] [%e id.Id.script_param] ppxc__s [%e tx] ppxc__t]
      in
      Topscript.add_extract @@ f [%expr Ppxc__script.Extract.bound_constant];
      Topscript.add_build_external
      @@ f [%expr Ppxc__script.Build.bound_constant];
      Impl_mod.add_named ~retype:true ~name_check:true name id.Id.expr
    | l -> error_msg "constant" l

  let marshal_expr (e : Marshal_types.expr) = U.marshal_to_str_expr e

  let register_fun id =
    let loc = !Ast_helper.default_loc in
    Topscript.add_extract
      [%stri
        let () = Ppxc__script.Extract.register_fun_place [%e id.Id.script_param]]

  let foreign_value = function
    | [ (Nolabel, str_expr); (Nolabel, typ_expr) ] ->
      let prefix = Extract.constant_string str_expr in
      let name = U.safe_mlname ?prefix () in
      let name_expr = U.str_expr name in
      let id_stri_external = Id.get () in
      let id_stri_expr = Id.get () in
      register_fun id_stri_external;
      let texpr = marshal_expr typ_expr in
      let loc = !Ast_helper.default_loc in
      Topscript.add_build_external
        [%stri
          let () =
            let (ppxc__s : string) = [%e str_expr]
            and (ppxc__t : _ Ctypes.typ) = [%e typ_expr] in
            Ppxc__script.Build.foreign_value
              [%e id_stri_external.Id.script_param]
              [%e id_stri_expr.Id.script_param] ppxc__t ppxc__s [%e name_expr]
              [%e texpr]];
      Impl_mod.add_external_anon id_stri_external.Id.stri id_stri_expr.Id.stri
        name
    | l -> error_msg "foreign_value" l

  let header = function
    | [ (Nolabel, x) ] ->
      (match Extract.constant_string x with
      | Some _ -> ()
      | None -> error "'header' requires a string literal");
      let loc = !Ast_helper.default_loc in
      Topscript.add_extract_phase0
        [%stri
          let () =
            let ppxc__1 : string = [%e x] in
            Ppxc__script.Extract_phase0.header ppxc__1];
      Topscript.add_extract
        [%stri
          let () =
            let ppxc__1 : string = [%e x] in
            Ppxc__script.Extract.header ppxc__1];
      [%expr ()]
    | l -> error_msg "header" l

  let field ?name ?prefix ~structure ~str_expr type_expr =
    let loc = !Ast_helper.default_loc in
    let id = Id.get () in
    let n = match name with None -> U.safe_mlname ?prefix () | Some s -> s in
    let p = U.mk_pat n in
    let f func =
      [%stri
        let [%p p] =
          let (ppxc__st
                : (_, [< `Struct | `Union ]) Ctypes.structured Ctypes.typ) =
            [%e structure]
          and (ppxc__s : string) = [%e str_expr]
          and (ppxc__t : _ Ctypes.typ) = [%e type_expr] in
          [%e func] [%e id.Id.script_param] ppxc__st ppxc__s ppxc__t]
    in
    Topscript.add_extract @@ f [%expr Ppxc__script.Extract.field];
    Topscript.add_build_external @@ f [%expr Ppxc__script.Build.field];
    let nexpr =
      [%expr
        Ppx_cstubs.Ppx_cstubs_internals.add_field [%e structure] [%e str_expr]
          [%e id.Id.expr] [%e type_expr]]
    in
    let nc = name <> None in
    let res = Impl_mod.add_named ~retype:true ~name_check:nc n nexpr in
    OSTypes.types_maybe_used ();
    (n, res)

  let seal = function
    | [ (Nolabel, seal_struct) ] ->
      let id_size = Id.get () in
      let id_align = Id.get () in
      let loc = !Ast_helper.default_loc in
      let f f =
        [%stri
          let () =
            let (ppxc__s
                  : (_, [< `Struct | `Union ]) Ctypes.structured Ctypes.typ) =
              [%e seal_struct]
            in
            [%e f] [%e id_size.Id.script_param] [%e id_align.Id.script_param]
              ppxc__s]
      in
      Topscript.add_extract @@ f [%expr Ppxc__script.Extract.seal];
      Topscript.add_build_external @@ f [%expr Ppxc__script.Build.seal];
      let nexpr =
        [%expr
          Ppx_cstubs.Ppx_cstubs_internals.seal [%e seal_struct]
            ~size:[%e id_size.Id.expr] ~align:[%e id_align.Id.expr]]
      in
      OSTypes.types_maybe_used ();
      Impl_mod.add_unit nexpr
    | l -> error_msg "seal" l

  let is_ocaml_operator = function
    | "mod" | "or" | "land" | "lor" | "lxor" | "lsl" | "lsr" | "asr" -> true
    | "" -> false
    | c -> (
      match c.[0] with
      | '!' | '#' | '$' | '%' | '&' | '*' | '+' | '-' | '/' | '<' | '=' | '>'
      | '?' | '@' | '^' | '|' | '~' ->
        true
      | _ -> false)

  let rec build_ctypes_fn params ret =
    match params with
    | [] ->
      let loc = ret.pexp_loc in
      [%expr returning [%e ret]]
    | hd :: tl ->
      let loc = hd.pexp_loc in
      let e = build_ctypes_fn tl ret in
      [%expr [%e hd] @-> [%e e]]

  let add_stri_to_all script =
    Topscript.add_extract script;
    Topscript.add_build_external script;
    Impl_mod.add_entry script

  let external' ~is_inline ~remove_labels ~return_errno ~release_runtime_lock
      ~noalloc ~attrs v =
    let name = v.pval_name.txt in
    let c_name =
      match v.pval_prim with
      | [ "" ] | [ "_" ] -> error ~loc:v.pval_loc "function name missing"
      | [ a ] -> a
      | _ -> error ~loc:v.pval_loc "too many c functions referenced"
    in
    if is_inline = false then (
      let c = c_name.[0] in
      if U.safe_ascii_only c_name <> c_name || (c >= '0' && c <= '9') then
        error "invalid identifier for c function:%S" c_name;
      if remove_labels then
        error ~loc:v.pval_loc "remove_labels only supported for inline code");
    let remove_labels =
      remove_labels || (is_inline && is_ocaml_operator name)
    in
    let ret, el, rec' = Extract.fun_def ~lookup:name is_inline v.pval_type in
    let fun_expr = build_ctypes_fn (List.map ~f:snd el) ret in
    let id_stri_expr = Id.get () in
    let id_stri_ext = Id.get () in
    let name_impl = if rec' then U.safe_mlname ~prefix:name () else name in
    let uniq_ref_id, fres =
      Impl_mod.add_external ~name_check:(not rec') id_stri_ext.Id.stri
        id_stri_expr.Id.stri ~name:name_impl
    in
    (if rec' then
     let mp = Impl_mod.get_mod_path () in
     let n = U.mk_ident_l [ name_impl ] in
     let r = Uniq_ref.make mp ~retype:false name n in
     (* slightly redundant. But it's an usual to name the function like
        a Ctypes.typ value that is used to construct the function. I won't
        waste time for a nicer  solution... *)
     let loc = !Ast_helper.default_loc in
     Impl_mod.add_entry
       [%stri
         include struct
           [@@@ocaml.warning "-32"]

           [%%i r.Uniq_ref.topmod_vb]

           [%%i r.Uniq_ref.topmod_ref]
         end]);
    let vb = Vb.mk ~attrs (U.mk_pat name) fres in
    let fres = Str.value Nonrecursive [ vb ] in
    let marshal_info =
      let open Marshal_types in
      let s =
        {
          el;
          ret;
          release_runtime_lock;
          noalloc;
          return_errno;
          is_inline;
          remove_labels;
          c_name;
          prim_name = name_impl;
          uniq_ref_id;
        }
      in
      U.marshal_to_str_expr s
    in
    if is_inline then (* order matters *)
      register_fun id_stri_expr;
    register_fun id_stri_ext;
    let loc = !Ast_helper.default_loc in
    Topscript.add_build_external
      [%stri
        let () =
          let (ppxc__fn : _ Ctypes.fn) = [%e fun_expr] in
          Ppxc__script.Build.external' [%e id_stri_ext.Id.script_param]
            [%e id_stri_expr.Id.script_param] ppxc__fn
            ~marshal_info:[%e marshal_info]];
    (* create a dummy function that can be referenced in Ctypes.view, etc.
       Constraint necessary to avoid warning 21 (unsound type, never returns) *)
    let script = [%expr ([%e ret] : 'b Ctypes.typ)] in
    let script =
      [%expr
        [%e Gen_ml.stdlib_fun "ignore"] [%e script];
        Ctypes.ppxc__unavailable [%e U.str_expr name]]
    in
    let script =
      ListLabels.fold_right ~init:script el ~f:(fun (l, _) ac ->
          let nac = Exp.fun_ l None (Pat.any ()) ac in
          if ac == script then
            Exp.constraint_ nac @@ Typ.arrow l (Typ.var "a") (Typ.var "b")
          else nac)
    in
    let script = U.named_stri name script in
    Topscript.add_extract script;
    Topscript.add_build_external script;
    fres

  let check_reserved_type s =
    if Hashtbl.mem Keywords.htl_types s then
      error "type name %S is reserverd, choose another name" s

  let foreign ?prefix l =
    let find_first_unlabelled l =
      List.find_mapi l ~f:(fun i el ->
          match el with Nolabel, _ -> Some (i, el) | _ -> None)
    in
    let typ_expr =
      match List.rev l |> find_first_unlabelled with
      | None -> error "function signature in foreign not found"
      | Some (_, l) -> snd l
    in
    let typ_expr = marshal_expr typ_expr in
    let ocaml_name =
      let t =
        List.find_map l ~f:(function
          | Nolabel, e -> Extract.constant_string e
          | _ -> None)
      in
      match t with
      | None -> U.safe_mlname ?prefix ()
      | Some s -> U.safe_mlname ~prefix:s ()
    in
    let id_stri_external = Id.get () in
    let id_stri_expr = Id.get () in
    register_fun id_stri_external;
    let loc = !Ast_helper.default_loc in
    let f_expr =
      [%expr
        Ppxc__script.Build.foreign [%e id_stri_external.Id.script_param]
          [%e id_stri_expr.Id.script_param]
          ~ocaml_name:[%e U.str_expr ocaml_name] ~typ_expr:[%e typ_expr]]
    in
    let call = Exp.apply f_expr l in
    Topscript.add_build_external [%stri let () = [%e call]];
    Impl_mod.add_external_anon id_stri_external.Id.stri id_stri_expr.Id.stri
      ocaml_name

  let fn binding_name expr =
    let loc = !Ast_helper.default_loc in
    let t = [%type: _ Ctypes.fn] in
    let pat = U.mk_pat_pconstr t binding_name in
    Topscript.add_extract [%stri let [%p pat] = [%e expr]];
    let id_expr, attrs = Id.get_usage_id () in
    Topscript.add_build_external
      [%stri
        let [%p pat] = Ppxc__script.Build.reg_trace_fn [%e id_expr] [%e expr]];
    Impl_mod.add_named ~constr:t ~retype:true ~attrs binding_name expr

  let add_ctyp ~constr ?extract_expr ?build_expr ?(name_check = true) ~retype
      bname expr =
    let loc = !Ast_helper.default_loc in
    let pat = U.mk_pat_pconstr constr bname in
    let extract_expr = match extract_expr with None -> expr | Some s -> s in
    Topscript.add_extract [%stri let [%p pat] = [%e extract_expr]];
    let id_expr, attrs = Id.get_usage_id () in
    let script =
      let e = match build_expr with None -> expr | Some e -> e in
      [%stri let [%p pat] = Ppxc__script.Build.reg_trace [%e id_expr] [%e e]]
    in
    Topscript.add_build_external script;
    Impl_mod.add_named ~constr ~retype ~name_check ~attrs bname expr

  let add_type_h ?tdl typ' name =
    let t = Str.type_ Recursive [ typ' ] in
    add_stri_to_all t;
    match tdl with
    | None -> ()
    | Some id -> (
      let t' top =
        let name = typ'.ptype_name.txt in
        let constr = Impl_mod.create_type_ref name in
        let typ' = { typ' with ptype_manifest = Some constr } in
        let typ' =
          if top then typ'
          else
            let p = [ Attributes.manifest_replace_attrib ] in
            { typ' with ptype_attributes = p }
        in
        Str.type_ Recursive [ typ' ]
      in
      Type_mod.add_entry (t' true);
      Hashtbl.add htl_tdl_entries id (t' false);
      match OSTypes.add_abstract name with
      | None -> ()
      | Some x -> Hashtbl.add htl_tdl_entries id x)

  let union_struct ~tdl_entry_id ?alias_name ~type_name ~stype xtype orig_name =
    let loc = !Ast_helper.default_loc in
    let alias_name = match alias_name with None -> orig_name | Some s -> s in
    let t = Type.mk ~kind:Ptype_abstract (U.mk_loc type_name) in
    let tdl =
      if stype = Extract.Struct_record then None else Some tdl_entry_id
    in
    add_type_h ?tdl t type_name;
    let is_union =
      match stype with
      | Extract.Union -> true
      | Extract.Struct_normal | Extract.Struct_record | Extract.Struct_both ->
        false
    in
    let constr =
      let x = U.mk_typc type_name in
      if is_union then [%type: [%t x] Ctypes.union Ctypes.typ]
      else [%type: [%t x] Ctypes.structure Ctypes.typ]
    in
    let expr, expr_top =
      let sexpr =
        match xtype with
        | `Typedef _ -> U.str_expr ""
        | `Named s -> U.str_expr s
        | `From_expr e -> e
      in
      if is_union then
        ( [%expr Ctypes.union [%e sexpr]],
          [%expr
            let __s : string = [%e sexpr] in
            Ctypes.ppxc__private_union __s] )
      else
        ( [%expr Ctypes.structure [%e sexpr]],
          [%expr
            let __s : string = [%e sexpr] in
            Ctypes.ppxc__private_structure __s] )
    in
    let add expr expr_top =
      match stype with
      | Extract.Struct_record ->
        let e = U.named_stri ~constr alias_name expr_top in
        Topscript.add_extract e;
        Topscript.add_build_external e;
        U.named_stri ~constr alias_name expr |> Impl_mod.add_entry
      | Extract.Struct_normal | Extract.Struct_both | Extract.Union ->
        add_ctyp ~constr ~build_expr:expr_top ~extract_expr:expr_top
          ~retype:false alias_name expr
        |> U.named_stri ~constr orig_name
        |> Hashtbl.add htl_tdl_entries tdl_entry_id
    in
    match xtype with
    | `Named _ | `From_expr _ -> add expr expr_top
    | `Typedef sname_c ->
      let f e = [%expr Ctypes.typedef [%e e] [%e U.str_expr sname_c]] in
      let expr = f expr in
      let expr_top = f expr_top in
      add expr expr_top

  let type_decl ~enforce_union ~c_bitmask type_rec_flag tl =
    let loc = !Ast_helper.default_loc in
    if tl = [] then error "empty type definition";
    let open Extract_types in
    let record_name s = s ^ "_record" in
    let bitmask_name s = s ^ "_bitmask" in
    let tdl_entry_id, tdl_entry = Id.get_tdl_entries_id () in
    let private_pref =
      if type_rec_flag = Recursive then Std.identity
      else
        let s =
          let open Location in
          let open Lexing in
          let loc = !Ast_helper.default_loc in
          Printf.sprintf "__ppxc__private_%x_%x_" loc.loc_start.pos_lnum
            loc.loc_start.pos_cnum
        in
        fun x -> s ^ x
    in
    let unhide x =
      let add name =
        let e = U.mk_ident @@ private_pref name in
        let stri = U.no_warn_unused name e in
        add_stri_to_all stri
      in
      match x with
      | Enum { ename; enum_type; _ } ->
        add ename;
        if enum_type = Enum_both then add @@ bitmask_name ename
      | Struct x ->
        add x.sname;
        if x.stype = Struct_both then add @@ record_name x.sname
    in
    let fields { sname; sl; stype; _ } =
      let orig_name = sname in
      let alias_name = private_pref orig_name in
      let struct_expr = U.mk_ident alias_name in
      let fnames =
        List.map sl
          ~f:(fun { field_name; field_expr; field_cname; field_loc } ->
            U.with_loc field_loc @@ fun () ->
            let str_expr = U.str_expr field_cname in
            let name =
              match stype with
              | Struct_record -> None
              | Union | Struct_normal | Struct_both -> Some field_name
            in
            let n, e =
              field ?name ~prefix:field_name ~structure:struct_expr ~str_expr
                field_expr
            in
            (match stype with
            | Struct_record -> ()
            | Union | Struct_normal | Struct_both ->
              let t = U.mk_typc orig_name in
              let t =
                if stype = Union then [%type: [%t t] Ctypes.union]
                else [%type: [%t t] Ctypes.structure]
              in
              let t = [%type: (_, [%t t]) Ctypes.field] in
              let x = U.named_stri ~constr:t field_name e in
              Hashtbl.add htl_tdl_entries tdl_entry_id x);
            n)
      in
      ignore (seal [ (Nolabel, struct_expr) ] : expression);
      match stype with
      | Union | Struct_normal -> ()
      | Struct_record | Struct_both ->
        (* Generate the view for the type *)
        let alias_name, orig_name =
          if stype <> Struct_both then (alias_name, orig_name)
          else (record_name alias_name, record_name orig_name)
        in
        let s =
          List.map sl ~f:(fun s -> (s.field_name, s.field_loc, s.field_expr))
        in
        let mod_path = Impl_mod.get_mod_path () in
        let r = Gen_ml.gen_record_stris ~mod_path ~type_name:orig_name s in
        List.iter ~f:add_stri_to_all r.Gen_ml.r_stri_top;
        List.iter
          ~f:(Hashtbl.add htl_tdl_entries tdl_entry_id)
          r.Gen_ml.r_stri_bottom;
        List.iter ~f:Type_mod.add_entry r.Gen_ml.r_stri_type_mod;
        (match OSTypes.add_struct_view orig_name with
        | None -> ()
        | Some b -> Hashtbl.add htl_tdl_entries tdl_entry_id b);
        let view =
          let init = [%expr ppxc__res] in
          let param = U.mk_ident_l [ Myconst.private_prefix ^ "param" ] in
          let expr =
            List.fold_left2 ~init sl fnames ~f:(fun ac el fname ->
                let fi = U.mk_lid el.field_name in
                let v = Exp.field param fi in
                let f = U.mk_ident fname in
                [%expr
                  let () = Ctypes.setf ppxc__res [%e f] [%e v] in
                  [%e ac]])
          in
          let write =
            [%expr
              fun ppxc__param ->
                let ppxc__res = Ctypes.make [%e struct_expr] in
                [%e expr]]
          in
          let init =
            Exp.record
              (List.map sl ~f:(fun el ->
                   let l = U.mk_lid el.field_name in
                   let e = Exp.ident l in
                   (l, e)))
              None
          in
          let expr =
            List.fold_left2 ~init sl fnames ~f:(fun ac el fname ->
                let p = U.mk_pat el.field_name in
                let f = U.mk_ident fname in
                [%expr
                  let [%p p] = Ctypes.getf ppxc__param [%e f] in
                  [%e ac]])
          in
          let read = [%expr fun ppxc__param -> [%e expr]] in
          [%expr Ctypes.view ~read:[%e read] ~write:[%e write] [%e struct_expr]]
        in
        let t = [%type: [%t U.mk_typc orig_name] Ctypes.typ] in
        add_ctyp ~constr:t ~retype:false alias_name view
        |> U.named_stri ~constr:t orig_name
        |> Hashtbl.add htl_tdl_entries tdl_entry_id
    in
    let single_typ = function
      | Enum
          {
            ename;
            el;
            ename_c;
            etypedef;
            edecl;
            enum_type;
            eunexpected;
            eunexpected_bits;
          } ->
        U.with_loc edecl.ptype_loc @@ fun () ->
        let orig_name_bitmask = bitmask_name ename in
        let alias_name_bitmask = private_pref orig_name_bitmask in
        let orig_name = ename in
        let alias_name = private_pref orig_name in
        add_type_h ~tdl:tdl_entry_id edecl ename;
        let exp_l, enum_l =
          let init = ([%expr []], []) in
          ListLabels.fold_right el ~init ~f:(fun cur (l1, l2) ->
              let loc = cur.cdecl.pcd_loc in
              U.with_loc loc @@ fun () ->
              let c =
                {
                  Marshal_types.ee_int_id = (Id.get ~loc ()).Id.id;
                  ee_type_check = (Id.get ~loc ()).Id.id;
                  ee_loc = loc;
                  ee_cname = cur.enum_cname;
                  ee_expr = Exp.construct (U.mk_lid cur.cdecl.pcd_name.txt) None;
                }
              in
              ([%expr [%e c.ee_expr] :: [%e l1]], c :: l2))
        in
        let id, id_bitmask, enum_type_id =
          match enum_type with
          | Enum_normal ->
            let id = Id.get () in
            (Some id, None, Marshal_types.E_normal id.Id.id)
          | Enum_bitmask ->
            let id = Id.get () in
            (None, Some id, Marshal_types.E_bitmask id.Id.id)
          | Enum_both ->
            let id1 = Id.get () in
            let id2 = Id.get () in
            ( Some id1,
              Some id2,
              Marshal_types.E_normal_bitmask (id1.Id.id, id2.Id.id) )
        in
        let sparam =
          let wrap e =
            match e with None -> [%expr None] | Some e -> [%expr Some [%e e]]
          in
          let enum_unexpected = wrap eunexpected in
          let enum_unexpected_bits = wrap eunexpected_bits in
          let open Marshal_types in
          U.marshal_to_str_expr
            {
              enum_l;
              enum_is_typedef = etypedef;
              enum_type_id;
              enum_unexpected;
              enum_unexpected_bits;
              enum_is_int_bitmask = c_bitmask;
              enum_loc = edecl.ptype_name.loc;
              enum_name = ename_c;
            }
        in
        let pat =
          let p1 =
            match enum_type with
            | Enum_normal | Enum_both -> U.mk_pat alias_name
            | Enum_bitmask -> Pat.any ()
          in
          let p2 =
            match enum_type with
            | Enum_normal -> Pat.any ()
            | Enum_bitmask -> U.mk_pat alias_name
            | Enum_both -> U.mk_pat alias_name_bitmask
          in
          [%pat? [%p p1], [%p p2]]
        in
        Topscript.add_extract
          [%stri
            let ([%p pat] : 'a Ctypes.typ * 'a list Ctypes.typ) =
              Ppxc__script.Extract.enum [%e exp_l] [%e sparam]];
        let id_expr, attrs = Id.get_usage_id () in
        Topscript.add_build_external
          [%stri
            let [%p pat] =
              let (ppxc__1, ppxc__2) : 'a Ctypes.typ * 'a list Ctypes.typ =
                Ppxc__script.Build.enum [%e exp_l] [%e sparam]
              in
              ( Ppxc__script.Build.reg_trace [%e id_expr] ppxc__1,
                Ppxc__script.Build.reg_trace [%e id_expr] ppxc__2 )];
        let f ~is_list ?(bitmask_name = false) name = function
          | None -> ()
          | Some id ->
            let constr = U.mk_typc orig_name in
            let constr =
              match is_list with
              | false ->
                constr (* FIXME: how to access list and avoid shadowing? *)
              | true -> [%type: [%t constr] list]
            in
            let constr = [%type: [%t constr] Ctypes.typ] in
            let s2 =
              Impl_mod.add_named ~constr ~retype:false ~attrs name id.Id.expr
            in
            let name =
              match bitmask_name with
              | false -> orig_name
              | true -> orig_name_bitmask
            in
            let s = U.named_stri ~constr name s2 in
            Hashtbl.add htl_tdl_entries tdl_entry_id s
        in
        (match enum_type with
        | Enum_normal -> f ~is_list:false alias_name id
        | Enum_bitmask -> f ~is_list:true alias_name id_bitmask
        | Enum_both ->
          f ~is_list:false alias_name id;
          f ~is_list:true ~bitmask_name:true alias_name_bitmask id_bitmask);
        None
      | Struct ({ sname; sl = _; sname_c; stypedef; stype; sloc } as swhole) ->
        U.with_loc sloc @@ fun () ->
        let orig_name = sname in
        let alias_name = private_pref orig_name in
        let type_name =
          match stype with
          | Union | Struct_normal | Struct_both -> orig_name
          | Struct_record -> Myconst.private_prefix ^ orig_name
        in
        let xtype = if stypedef then `Typedef sname_c else `Named sname_c in
        union_struct ~tdl_entry_id ~alias_name ~type_name ~stype xtype orig_name;
        Some swhole
    in
    let tl = Extract.type_decl type_rec_flag tl in
    if
      enforce_union
      && List.for_all tl ~f:(function Enum _ -> true | Struct _ -> false)
    then error "enum entry marked as union";
    if
      c_bitmask
      && List.for_all tl ~f:(function Enum _ -> false | Struct _ -> true)
    then error "struct entry marked as bitmask";
    let names =
      List.fold_left ~init:[] tl ~f:(fun ac -> function
        | Struct { sname = s; sl; _ } ->
          check_reserved_type s;
          List.fold_left ~init:(s :: ac) sl ~f:(fun ac el ->
              el.field_name :: ac)
        | Enum { ename = s; _ } ->
          check_reserved_type s;
          s :: ac)
    in
    let names' = CCList.uniq ~eq:CCString.equal names in
    if List.length names <> List.length names' then
      error "names of enumarations, structures and fields must be unique";
    let tl =
      List.map tl ~f:(function
        | Struct s when enforce_union ->
          let stype =
            match s.stype with
            | Struct_normal | Union -> Union
            | Struct_both ->
              error "@@@@with_record and type%%c_union are mutually exclusive"
            | Struct_record ->
              error "@@@@as_record and type%%c_union are mutually exclusive"
          in
          Struct { s with stype }
        | Enum e ->
          let res =
            match e.enum_type with
            | _ when c_bitmask = false -> e
            | Enum_both ->
              error
                "@@@@with_bitmask and type%%c_bitmask f = ... are incompatible"
            | Enum_normal ->
              if e.etypedef then
                error "@@@@typedef and type%%c_bitmask f = ... are incompatible";
              { e with enum_type = Enum_bitmask; etypedef = true }
            | Enum_bitmask ->
              error "@@@@as_bitmask and type%%c_bitmask f = ... are redundant"
          in
          (match (res.enum_type, res.eunexpected) with
          | Enum_bitmask, Some _ ->
            error "@@@@unexpected not supported for bitmasks"
          | (Enum_both | Enum_normal | Enum_bitmask), (Some _ | None) -> ());
          (match (res.enum_type, res.eunexpected_bits) with
          | Enum_normal, Some _ ->
            error "@@@@unexpected_bits not supported for enums"
          | (Enum_both | Enum_normal | Enum_bitmask), (Some _ | None) -> ());
          Enum res
        | Struct _ as x -> x)
    in
    let cnt_structs, cnt_records =
      List.fold_left tl ~init:(0, 0) ~f:(fun ((cns, cnr) as ac) -> function
        | Enum _ -> ac
        | Struct s ->
          let cns = succ cns in
          let cnr =
            match s.stype with
            | Struct_normal | Union -> cnr
            | Struct_both | Struct_record -> succ cnr
          in
          (cns, cnr))
    in
    if cnt_structs > 1 && cnt_records >= 1 && type_rec_flag = Recursive then
      error "mutually recursive records are not supported";
    let refs = List.filter_map ~f:single_typ tl in
    List.iter ~f:fields refs;
    if type_rec_flag <> Recursive then List.iter ~f:unhide tl;
    tdl_entry

  let pexp_const name expr =
    let script = U.named_stri name expr in
    Topscript.add_extract script;
    Topscript.add_build_external script;
    Impl_mod.add_named ~retype:true name expr

  let field ?prefix = function
    | [ (Nolabel, structure); (Nolabel, str_expr); (Nolabel, type_expr) ] ->
      snd (field ?prefix ~structure ~str_expr type_expr)
    | l -> error_msg "field" l

  let mk_str_type ?attrs ?manifest s =
    let t = Type.mk ?attrs ~kind:Ptype_abstract ?manifest (U.mk_loc s) in
    Str.type_ Recursive [ t ]

  let int_alias ~mod_name alias_typ l =
    let loc = !Ast_helper.default_loc in
    let id = Id.get () in
    let str_expr =
      (* optional parameter ?strict *)
      let er ?loc () = U.error ?loc "int aliases require a string literal" in
      let x =
        List.find_map l ~f:(function
          | Nolabel, str_expr -> (
            match Extract.constant_string str_expr with
            | Some _ -> Some str_expr
            | None -> er ~loc:str_expr.pexp_loc ())
          | (Labelled _ | Optional _), _ -> None)
      in
      match x with Some s -> s | None -> er ()
    in
    Topscript.add_extract_phase0
      [%stri
        let () =
          Ppxc__script.Extract_phase0.int_alias [%e id.Id.script_param]
            [%e str_expr]];
    let func_name, sig_name =
      match alias_typ with
      | `Int -> ("int_alias", "Signed")
      | `Uint -> ("uint_alias", "Unsigned")
      | `Aint -> ("aint_alias", "Unkown_signedness")
    in
    let mod_name_expr = U.str_expr mod_name in
    let lmod_name = U.mk_oloc mod_name in
    let f e_b =
      let e = U.mk_ident_l (e_b @ [ func_name ]) in
      let e =
        Exp.apply e
          [
            (Nolabel, id.Id.script_param); (Labelled "mod_name", mod_name_expr);
          ]
      in
      let e = Mod.unpack (Exp.apply e l) in
      let e = Mb.mk lmod_name e in
      Str.module_ e
    in
    Topscript.add_extract @@ f [ "Ppxc__script"; "Extract" ];
    Topscript.add_build_external @@ f [ "Ppxc__script"; "Build" ];
    Impl_mod.add_entry id.Id.stri;
    let lid = Impl_mod.create_ref_lid mod_name in
    let t = Typ.constr (U.mk_loc (Ldot (lid.txt, "t"))) [] in
    let tstri = [%stri type t = [%t t]] in
    let x = Str.module_ (Mb.mk lmod_name (Mod.structure [ tstri ])) in
    Type_mod.add_entry x;
    let mod_ident = Mod.ident lid in
    let attrs = [ Attributes.manifest_replace_attrib ] in
    let t = Type.mk ~attrs ~kind:Ptype_abstract ~manifest:t (U.mk_loc "t") in
    let t = [ Pwith_type (U.mk_lid "t", t) ] in
    let s = Mty.ident (U.mk_lid_l [ "Ppx_cstubs"; "Types"; sig_name ]) in
    let t = Mod.constraint_ mod_ident (Mty.with_ s t) in
    let stri = Str.module_ (Mb.mk lmod_name t) in
    let tdl_entry_id, tdl_entry = Id.get_tdl_entries_id () in
    Hashtbl.add htl_tdl_entries tdl_entry_id stri;
    (match OSTypes.add_abstract ~sub_module:mod_name "t" with
    | None -> ()
    | Some b -> Hashtbl.add htl_tdl_entries tdl_entry_id b);
    tdl_entry

  let opaque binding_name vb_attrs l =
    let loc = !Ast_helper.default_loc in
    check_reserved_type binding_name;
    let id_size = Id.get () in
    let id_align = Id.get () in
    let id_type = Id.get () in
    let usage_id_expr, attrs = Id.get_usage_id () in
    let uniq_ref, main_ref =
      Impl_mod.add_opaq attrs id_size.Id.stri binding_name
    in
    let marshal_info =
      let open Marshal_types in
      let s = { o_binding_name = binding_name; o_uniq_ref_id = uniq_ref } in
      U.marshal_to_str_expr s
    in
    let mod_unique = U.safe_mlname ~capitalize:true ~prefix:binding_name () in
    let f f =
      let e =
        [%expr
          [%e f] ~size:[%e id_size.Id.script_param]
            ~align:[%e id_align.Id.script_param]
            ~typ:[%e id_type.Id.script_param] ~mi:[%e marshal_info]]
      in
      let e = Mod.unpack (Exp.apply e l) in
      let e = Mb.mk (U.mk_oloc mod_unique) e in
      Str.module_ e
    in
    Topscript.add_extract @@ f [%expr Ppxc__script.Extract.opaque];
    Topscript.add_build_external @@ f [%expr Ppxc__script.Build.opaque];
    let manifest = U.mk_typc_l [ mod_unique; "t" ] in
    let stri = mk_str_type ~manifest binding_name in
    Topscript.add_extract stri;
    Topscript.add_build_external stri;
    let expr = U.mk_ident_l [ mod_unique; "t" ] in
    let constr = [%type: [%t U.mk_typc binding_name] Ctypes.typ] in
    let pat = U.mk_pat_pconstr constr binding_name in
    Topscript.add_extract [%stri let [%p pat] = [%e expr]];
    Topscript.add_build_external
      [%stri
        let [%p pat] = Ppxc__script.Build.reg_trace [%e usage_id_expr] [%e expr]];
    let manifest = Impl_mod.create_type_ref binding_name in
    Type_mod.add_entry (mk_str_type ~manifest binding_name);
    let attrs = [ Attributes.manifest_replace_attrib ] in
    let stri = mk_str_type ~attrs ~manifest binding_name in
    let tdl_entry_id, tdl_entry = Id.get_tdl_entries_id () in
    Hashtbl.add htl_tdl_entries tdl_entry_id stri;
    (match OSTypes.add_abstract binding_name with
    | None -> ()
    | Some b -> Hashtbl.add htl_tdl_entries tdl_entry_id b);
    Str.value Nonrecursive [ Vb.mk ~attrs:vb_attrs pat main_ref ]
    |> Hashtbl.add htl_tdl_entries tdl_entry_id;
    tdl_entry

  let abstract binding_name vb_attrs l =
    let loc = !Ast_helper.default_loc in
    let str_expr =
      match l with [ (Nolabel, x) ] -> x | _ -> error_msg "abstract" l
    in
    check_reserved_type binding_name;
    let id_size = Id.get () in
    let id_align = Id.get () in
    let tdl_entry_id, tdl_entry = Id.get_tdl_entries_id () in
    let usage_id_expr, attrs = Id.get_usage_id () in
    add_stri_to_all (mk_str_type binding_name);
    let expr =
      [%expr
        let s : string = [%e str_expr] in
        Ppxc__script.Extract.abstract ~size:[%e id_size.Id.script_param]
          ~align:[%e id_align.Id.script_param] s]
    in
    let t = U.mk_typc_l [ binding_name ] in
    let constr = [%type: [%t t] Ctypes.abstract Ctypes.typ] in
    let pat = U.mk_pat_pconstr constr binding_name in
    Topscript.add_extract [%stri let [%p pat] = [%e expr]];
    let expr =
      [%expr
        Ppxc__script.Build.abstract ~size:[%e id_size.Id.script_param]
          ~align:[%e id_align.Id.script_param] [%e str_expr]
        |> Ppxc__script.Build.reg_trace [%e usage_id_expr]]
    in
    Topscript.add_build_external [%stri let [%p pat] = [%e expr]];
    let expr =
      [%expr
        Ctypes_static.Abstract
          {
            Ctypes_static.aname = [%e str_expr];
            Ctypes_static.asize = [%e id_size.Id.expr];
            Ctypes_static.aalignment = [%e id_align.Id.expr];
          }]
    in
    let manifest = Impl_mod.create_type_ref binding_name in
    Type_mod.add_entry (mk_str_type ~manifest binding_name);
    let at2 = [ Attributes.manifest_replace_attrib ] in
    let stri = mk_str_type ~attrs:at2 ~manifest binding_name in
    Hashtbl.add htl_tdl_entries tdl_entry_id stri;
    (match OSTypes.add_abstract binding_name with
    | None -> ()
    | Some b -> Hashtbl.add htl_tdl_entries tdl_entry_id b);
    let main_ref =
      Impl_mod.add_named ~constr ~retype:false ~attrs binding_name expr
    in
    Str.value Nonrecursive [ Vb.mk ~attrs:vb_attrs pat main_ref ]
    |> Hashtbl.add htl_tdl_entries tdl_entry_id;
    tdl_entry

  let ocaml_funptr ~acquire_runtime ~thread_registration cb_bname cb_user_fun
      typ =
    let loc = !Ast_helper.default_loc in
    let id_bottom = Id.get () in
    register_fun id_bottom;
    let cb_top_mod = U.safe_mlname ~capitalize:true ~prefix:cb_bname () in
    let rec' =
      {
        Marshal_types.cb_mod_path = Impl_mod.get_mod_path ();
        cb_binding_name = cb_bname;
        cb_bottom = id_bottom.Id.id;
        cb_top_mod;
        cb_user_fun;
        cb_acquire_runtime = acquire_runtime;
        cb_thread_registration = thread_registration;
        cb_init_fun = U.safe_cname ~prefix:cb_bname;
      }
    in
    let rec' = U.marshal_to_str_expr rec' in
    let cb_typ =
      match typ.ptyp_desc with
      | Ptyp_constr (x, []) -> x
      | _ -> U.error "invalid type specification for callback"
    in
    let e = Exp.ident cb_typ in
    let f f =
      [%stri
        let () =
          let (__ppxc__safe : _ Ctypes.static_funptr Ctypes.typ) = [%e e] in
          [%e f] [%e rec'] __ppxc__safe]
    in
    Topscript.add_extract @@ f [%expr Ppxc__script.Extract.ocaml_funptr];
    Topscript.add_build_external @@ f [%expr Ppxc__script.Build.ocaml_funptr];
    let m =
      let er () =
        U.error ~loc:cb_typ.loc
          "please reference the original module for the callback type"
      in
      match Longident.flatten_exn cb_typ.txt |> List.rev with
      | [] | [ _ ] -> er ()
      | x :: tl ->
        if x <> "t" then er ();
        List.rev tl
    in
    let ident = Mod.ident (U.mk_lid_l m) in
    let mb = Mb.mk (U.mk_oloc cb_top_mod) ident in
    Impl_mod.add_entry (Str.module_ mb);
    id_bottom.Id.stri

  let type_cb lmod_name_opt mod_name l =
    let loc = !Ast_helper.default_loc in
    let () =
      let a =
        Exp.apply [%expr Ppx_cstubs.Ppx_cstubs_internals.Callback.make] l
      in
      let m =
        U.mk_lid_l [ "Ppx_cstubs"; "Ppx_cstubs_internals"; "Callback"; "Make" ]
      in
      let s = Mod.apply (Mod.ident m) (Mod.unpack a) in
      add_stri_to_all (Str.module_ (Mb.mk lmod_name_opt s))
    in
    let id_expr, attrs = Id.get_usage_id () in
    Topscript.add_build_external
      [%stri
        let (_ : _ Ctypes.static_funptr Ctypes.typ) =
          Ppxc__script.Build.reg_trace ~no_dup:true [%e id_expr]
            [%e U.mk_ident_l [ mod_name; "t" ]]];
    let mp = Impl_mod.get_mod_path () @ [ mod_name ] in
    let ident = Mod.ident (U.mk_lid_l mp) in
    let ident =
      if Ocaml_config.use_open_struct () = false then ident
      else
        let fname s = mp @ [ s ] in
        let t1 = U.mk_typc_l (fname "raw_pointer") in
        let t1 = [%stri type raw_pointer = [%t t1]] in
        let t2 = U.mk_typc_l ~l:[ Typ.var "a" ] (fname "t") in
        let t2 = [%stri type 'a t = [%t t2]] in
        let tm = Mb.mk lmod_name_opt (Mod.structure [ t1; t2 ]) in
        Type_mod.add_entry (Str.module_ tm);
        let o = U.alias_impl_mod_os () in
        let s = Str.include_ (Incl.mk ident) in
        let m = Mty.typeof_ (Mod.structure [ o; s ]) in
        let f s invar =
          let l, params =
            if invar = false then ([], None)
            else
              let a' = Typ.var "a" in
              ([ a' ], Some [ (a', (NoVariance, NoInjectivity)) ])
          in
          let manifest = U.mk_typc_l ~l (fname s) in
          let attrs = [ Attributes.manifest_replace_attrib ] in
          let tdl = Type.mk ~attrs ?params ~manifest (U.mk_loc s) in
          Pwith_type (U.mk_lid s, tdl)
        in
        let l = [ f "t" true; f "raw_pointer" false ] in
        Mod.constraint_ ident (Mty.with_ m l)
    in
    let mb = Mb.mk ~attrs lmod_name_opt ident in
    let stri = Str.module_ mb in
    let tdl_entry_id, tdl_entry = Id.get_tdl_entries_id () in
    Hashtbl.add htl_tdl_entries tdl_entry_id stri;
    OSTypes.add_types_cb mod_name
    |> CCOpt.iter (Hashtbl.add htl_tdl_entries tdl_entry_id);
    tdl_entry

  let typ name e =
    let loc = !Ast_helper.default_loc in
    let n = U.safe_mlname ~prefix:name () in
    let t = [%type: _ Ctypes.typ] in
    let pat = U.mk_pat_pconstr t n in
    Topscript.add_build_external [%stri let [%p pat] = [%e e]];
    let ne = U.mk_ident_l [ n ] in
    let build_expr = [%expr Ppxc__script.Build.trace_custom [%e ne]] in
    add_ctyp ~constr:t ~retype:true ~build_expr name e

  let union_struct s stype ocaml_name = function
    | [ (Nolabel, e) ] ->
      let tdl_entry_id, tdl_entry = Id.get_tdl_entries_id () in
      union_struct ~tdl_entry_id ~type_name:ocaml_name ~stype (`From_expr e)
        ocaml_name;
      tdl_entry
    | l -> error_msg s l

  let union s l = union_struct "union" Extract.Union s l

  let structure s l = union_struct "structure" Extract.Struct_normal s l
end

let own_extensions = [ "c"; "c_union"; "c_bitmask"; "cb" ]

module Scan () : sig
  val structure : structure -> structure
end = struct
  module Open_decl : sig
    val new' :
      open_declaration -> (unit -> open_declaration) -> open_declaration

    val check : unit -> unit

    val check_s : string -> unit
  end = struct
    let depth = ref 0

    let new' s f =
      let rec g s =
        match s.pmod_desc with
        | Pmod_ident _ | Pmod_structure _ -> true
        | Pmod_functor _ | Pmod_apply _ | Pmod_unpack _ | Pmod_extension _ ->
          false
        | Pmod_constraint (s, _) -> g s
      in
      if g s.popen_expr then f ()
      else (
        incr depth;
        Std.finally ~h:(fun () -> decr depth) f)

    let check () =
      if !depth <> 0 then
        U.error
          "Defining types inside non trivial 'open expr' blocks is not possible"

    let check_s s =
      if !depth <> 0 then U.error "%s is not possible in this context" s
  end

  module Include_str : sig
    (* no type definition inside restricted include blocks.
       Prevent something like this:

       type t
       include (struct type t end : sig (* no type t *) end)
    *)

    val cond_disable : bool -> (unit -> 'a) -> 'a

    val check : unit -> unit

    val new' : module_expr -> (unit -> 'a) -> 'a
  end = struct
    let inside_restricted_struct = ref false

    let states = Stack.create ()

    let set_tmp new_state f =
      Stack.push !inside_restricted_struct states;
      inside_restricted_struct := new_state;
      Std.finally
        ~h:(fun () ->
          let s = Stack.pop states in
          inside_restricted_struct := s)
        f

    let cond_disable s f = if s = false then f () else set_tmp false f

    let check () =
      if !inside_restricted_struct then
        error
          "Defininig types inside constrained 'include struct' blocks is not supported"

    let new' p f =
      let is_restricted =
        match p.pmod_desc with
        | Pmod_ident _ | Pmod_structure _ -> false
        (* just forbid types in all unusual positions for the moment *)
        | Pmod_functor _ | Pmod_apply _ | Pmod_unpack _ | Pmod_extension _
        | Pmod_constraint _ ->
          true
      in
      if is_restricted = false then f () else set_tmp true f
  end

  module Static_level : sig
    (*
    ocaml_funptr must be declared "static", eg. not inside functors, etc. *)

    val disable : (unit -> 'a) -> 'a

    val inside_closure : unit -> bool
  end = struct
    let depth = ref 0

    let disable f =
      incr depth;
      Std.finally ~h:(fun () -> decr depth) f

    let inside_closure () = !depth <> 0
  end

  module Save_position : sig
    (* forbid certain constructs inside, eg. type of `struct ... end`*)

    val disable : (unit -> 'a) -> 'a

    val check : string -> unit
  end = struct
    let depth = ref 0

    let disable f =
      incr depth;
      Std.finally ~h:(fun () -> decr depth) f

    let check s =
      if !depth > 0 then U.error "%s is not possible in this context" s
  end

  let mark_empty a =
    if a.pvb_attributes <> [] || a.pvb_pat.ppat_attributes <> [] then a
    else
      match a.pvb_pat.ppat_desc with
      | Ppat_construct ({ txt = Lident "()"; _ }, None) -> (
        match a.pvb_expr with
        | {
         pexp_desc = Pexp_construct ({ txt = Lident "()"; _ }, None);
         pexp_attributes = [];
         _;
        } ->
          { a with pvb_attributes = [ Attributes.remove_attrib ] }
        | _ -> a)
      | _ -> a

  let rec unbox_box_constr e f =
    match e.pexp_desc with
    | Pexp_constraint (e2, c) ->
      let res = unbox_box_constr e2 f in
      { e with pexp_desc = Pexp_constraint (res, c) }
    | _ -> U.with_loc e.pexp_loc @@ fun () -> f e

  let check_save_type_pos () =
    Include_str.check ();
    Open_decl.check ()

  let check_save_pos s =
    Save_position.check s;
    Open_decl.check_s s

  let name_needed () =
    error "new types need a name, parallel pattern matching is not supported"

  let stri_and_open = function
    | [] -> U.empty_stri ()
    | [ hd ] -> hd
    | l ->
      let tdl_entry_id, tdl_entry = Id.get_tdl_entries_id () in
      List.iter l ~f:(Hashtbl.add htl_tdl_entries tdl_entry_id);
      tdl_entry

  let external' ~is_inline loc strpri =
    Ast_helper.default_loc := loc;
    let release_runtime_lock = ref false in
    let noalloc = ref false in
    let return_errno = ref false in
    let remove_labels = ref false in
    let attrs = ref [] in
    List.iter strpri.pval_attributes ~f:(fun x ->
        let reuse_attrib =
          match x.attr_name.txt with
          (* TODO: what else? *)
          | "release_runtime_lock" ->
            release_runtime_lock := true;
            false
          | "noalloc" ->
            noalloc := true;
            false
          | "return_errno" ->
            return_errno := true;
            false
          | "remove_labels" when is_inline ->
            remove_labels := true;
            false
          | "ocaml.warnerror" | "ocaml.deprecated" | "ocaml.warning"
          | "warnerror" | "deprecated" | "warning" ->
            attrs := x :: !attrs;
            true
          | y -> error ~loc:x.attr_loc "unsupported attribute %s" y
        in
        if reuse_attrib = false && x.attr_payload <> PStr [] then
          error ~loc:x.attr_loc "unknown content in attribute %s"
            x.attr_name.txt);
    let attrs = List.rev !attrs in
    let release_runtime_lock = !release_runtime_lock in
    let noalloc = !noalloc in
    let return_errno = !return_errno in
    let remove_labels = !remove_labels in
    H.external' ~is_inline ~remove_labels ~return_errno ~attrs
      ~release_runtime_lock ~noalloc strpri

  let obj =
    object (self)
      inherit Ast_traverse.map as super

      method! structure_item stri =
        let pstr_loc = stri.pstr_loc in
        Ast_helper.default_loc := pstr_loc;
        match stri.pstr_desc with
        | Pstr_extension
            ( ( { txt = "c"; _ },
                PStr [ { pstr_desc = Pstr_primitive strpri; pstr_loc; _ } ] ),
              _ )
          when strpri.pval_prim <> [] ->
          check_save_pos "external";
          OSTypes.types_maybe_used ();
          external' ~is_inline:true pstr_loc strpri
        | Pstr_primitive strpri when strpri.pval_prim <> [] ->
          check_save_pos "external";
          OSTypes.types_maybe_used ();
          external' ~is_inline:false pstr_loc strpri
        | Pstr_extension
            ( ( { txt = ("c" | "c_union" | "c_bitmask") as txt; _ },
                PStr [ { pstr_desc = Pstr_type (rf, tl); pstr_loc } ] ),
              _ ) ->
          check_save_type_pos ();
          check_save_pos txt;
          Ast_helper.default_loc := pstr_loc;
          let enforce_union = txt = "c_union" in
          let c_bitmask = txt = "c_bitmask" in
          H.type_decl ~enforce_union ~c_bitmask rf tl
        | Pstr_extension
            ( ( { txt = "cb"; _ },
                PStr [ { pstr_desc = Pstr_value (Nonrecursive, [ p ]); _ } ] ),
              _ ) ->
          OSTypes.types_maybe_used ();
          let pat = p.pvb_pat in
          let exp = p.pvb_expr in
          let thread_registration = ref false in
          let acquire_runtime = ref false in
          List.iter p.pvb_attributes ~f:(fun x ->
              (match x.attr_name.txt with
              | "acquire_runtime_lock" -> acquire_runtime := true
              | "thread_registration" -> thread_registration := true
              | y -> error ~loc:x.attr_loc "unsupported attribute %s" y);
              if x.attr_payload <> PStr [] then
                error ~loc:x.attr_loc "unknown content in attribute %s"
                  x.attr_name.txt);
          let typ_pat =
            match pat.ppat_desc with
            | Ppat_constraint (_, t) -> Some t
            | _ -> None
          in
          let exp, typ_expr =
            match exp.pexp_desc with
            | Pexp_constraint (e, t) -> (e, Some t)
            | Pexp_fun _ -> (
              let rec iter e =
                match e.pexp_desc with
                | Pexp_constraint (e, t) -> Some (e, t)
                | Pexp_fun (a, b, c, e') -> (
                  match iter e' with
                  | Some (e'', t) ->
                    let pexp_desc = Pexp_fun (a, b, c, e'') in
                    Some ({ e with pexp_desc }, t)
                  | None -> None)
                | _ -> None
              in
              match iter exp with
              | None -> (exp, None)
              | Some (e, t) -> (e, Some t))
            | _ -> (exp, None)
          in
          let typ =
            match (typ_pat, typ_expr) with
            | None, None -> U.error "callbacks require Ctypes.typ specification"
            | Some t, None | None, Some t -> t
            | Some t1, Some t2 -> (
              if t1 = t2 then t1
              else
                match t1.ptyp_desc with
                | Ptyp_poly ([], t1') when t1' = t2 ->
                  (* simple equality works surprisingly, probably a very fragile
                     test *)
                  t2
                | _ -> U.error "conflicting type definitions for callback")
          in
          let name =
            match Extract.variable_from_pattern_constr pat with
            | None, _ -> name_needed ()
            | Some x, _ -> x
          in
          if Static_level.inside_closure () then
            U.error "static ocaml callbacks must be declared at the top level";
          let thread_registration = !thread_registration in
          let acquire_runtime = !acquire_runtime in
          H.ocaml_funptr ~acquire_runtime ~thread_registration name exp typ
        | Pstr_extension
            ( ( { txt = "c"; _ },
                PStr
                  [
                    {
                      pstr_desc =
                        Pstr_value
                          ( Nonrecursive,
                            [
                              {
                                pvb_pat = pat;
                                pvb_expr = exp;
                                pvb_attributes;
                                _;
                              };
                            ] );
                      _;
                    };
                  ] ),
              _ ) ->
          (*| [%stri let%c [%p? pat] = [%e? exp]]*)
          Std.with_return @@ fun ret ->
          let t =
            let orig_exp = exp in
            unbox_box_constr exp @@ fun exp ->
            let s, l, is_constant =
              match exp.pexp_desc with
              | Pexp_apply
                  ( {
                      pexp_desc = Pexp_ident { txt = Lident s; loc = _ };
                      pexp_attributes = [];
                      _;
                    },
                    l ) ->
                (s, l, false)
              | Pexp_constant _ -> ("", [], true)
              | _ -> ("", [], false)
            in
            (* TODO: really allow everything as long as a Ctypes.typ is returned? *)
            let name, constr =
              match Extract.variable_from_pattern_constr pat with
              | None, x when s = "header" -> ("", x)
              | None, _ -> name_needed ()
              | Some x, y -> (x, y)
            in
            let nc () =
              U.error ~loc:orig_exp.pexp_loc "constraint not supported here"
            in
            check_save_pos s;
            if s <> "header" then OSTypes.types_maybe_used ();
            match s with
            | "header" ->
              if orig_exp <> exp then nc ();
              H.header l
            | "opaque" | "abstract" ->
              check_save_type_pos ();
              if orig_exp <> exp || constr then nc ();
              let at = pvb_attributes in
              if s = "opaque" then ret.return (H.opaque name at l)
              else ret.return (H.abstract name at l)
            | "union" ->
              check_save_type_pos ();
              ret.return (H.union name l)
            | "structure" ->
              check_save_type_pos ();
              ret.return (H.structure name l)
            | "@->" -> H.fn name exp
            | "constant" -> H.bound_constant name l
            | _ when is_constant -> H.pexp_const name exp
            | _ -> H.typ name exp
          in
          let vb = Ast_helper.Vb.mk ~attrs:pvb_attributes pat t |> mark_empty in
          Ast_helper.Str.value Nonrecursive [ vb ]
        | Pstr_module
            {
              pmb_name = lname;
              pmb_expr =
                {
                  pmod_desc =
                    Pmod_extension
                      ( { txt = ("cb" | "c") as txt; _ },
                        PStr [ { pstr_desc = Pstr_eval (exp, _); _ } ] );
                  _;
                };
              _;
            } -> (
          check_save_type_pos ();
          let mod_name =
            match lname.txt with
            | None -> ""
            | Some txt ->
              if Hashtbl.mem Keywords.htl_modules txt then
                U.error "module name %S is reserved" txt;
              txt
          in
          if txt = "cb" then (
            if Static_level.inside_closure () then
              U.error "static ocaml callbacks must be declared at the top level";
            check_save_pos "ocaml callback";
            if mod_name = "" then
              U.error "module for ocaml callback must have a name";
            H.type_cb lname mod_name [ (Nolabel, exp) ])
          else
            let s, l, loc =
              match exp.pexp_desc with
              | Pexp_apply
                  ( {
                      pexp_desc = Pexp_ident { txt = Lident s; loc = lo };
                      pexp_attributes = [];
                      _;
                    },
                    l ) ->
                (s, l, lo)
              | _ ->
                U.error ~loc:exp.pexp_loc
                  "don't know what do do with this expression."
            in
            check_save_pos s;
            let f x =
              if mod_name = "" then
                U.error "module for int_alias must have a name";
              H.int_alias ~mod_name x l
            in
            match s with
            | "int" -> f `Int
            | "uint" -> f `Uint
            | "aint" -> f `Aint
            | _ -> U.error ~loc "unsupported function %S" s)
        | Pstr_extension (({ txt; loc }, _), _)
          when List.mem ~eq:CCString.equal txt own_extensions ->
          error ~loc "extension '%s' is not supported here" txt
        | Pstr_value
            ( Nonrecursive,
              [
                ({
                   pvb_expr =
                     {
                       pexp_desc =
                         Pexp_extension
                           ( { txt = "c"; _ },
                             PStr [ { pstr_desc = Pstr_eval (e, []); _ } ] );
                       _;
                     };
                   _;
                 } as a);
              ] ) ->
          let t =
            unbox_box_constr e @@ fun e ->
            let prefix = Extract.variable_from_pattern a.pvb_pat in
            let s, l =
              match e.pexp_desc with
              | Pexp_apply
                  ( {
                      pexp_desc = Pexp_ident { txt = Lident s; loc = _ };
                      pexp_attributes = [];
                      _;
                    },
                    l ) ->
                (s, l)
              | Pexp_apply _ -> ("", [])
              | _ ->
                error "only function application is allowed in this context"
            in
            let res =
              match s with
              | "header" -> H.header l
              | "field" -> H.field ?prefix l
              | "constant" -> H.constant l
              | "seal" -> H.seal l
              | "foreign_value" -> H.foreign_value l
              | "foreign" -> H.foreign ?prefix l
              | _ -> error "invalid function call in [%%c ... ]"
            in
            if s <> "header" then OSTypes.types_maybe_used ();
            check_save_pos s;
            res
          in
          let na = mark_empty { a with pvb_expr = t } in
          { stri with pstr_desc = Pstr_value (Nonrecursive, [ na ]) }
        | Pstr_module x ->
          Include_str.cond_disable (x.pmb_name.txt = None) @@ fun () ->
          let f () = super#structure_item stri in
          Ptree.Modules.pstr_module x f |> stri_and_open
        | Pstr_recmodule l ->
          let f x =
            Include_str.cond_disable (x.pmb_name.txt = None) @@ fun () ->
            self#module_binding x
          in
          let l' = Ptree.Modules.pstr_recmodule l f in
          { pstr_desc = Pstr_recmodule l'; pstr_loc }
        | Pstr_include i ->
          Include_str.new' i.pincl_mod @@ fun () ->
          let f () = super#structure_item stri in
          Ptree.Modules.pstr_include i f |> stri_and_open
        | Pstr_open odl ->
          Ptree.Modules.pstr_open odl (fun () -> super#structure_item stri)
          |> stri_and_open
        | _ -> super#structure_item stri

      method! expression pexp =
        Ast_helper.default_loc := pexp.pexp_loc;
        match pexp.pexp_desc with
        | Pexp_extension
            ({ txt = "c"; _ }, PStr [ { pstr_desc = Pstr_eval (e, []); _ } ]) ->
          OSTypes.types_maybe_used ();
          unbox_box_constr e @@ fun e ->
          let s, l =
            match e.pexp_desc with
            | Pexp_apply
                ( {
                    pexp_desc = Pexp_ident { txt = Lident s; loc = _ };
                    pexp_attributes = [];
                    _;
                  },
                  l ) ->
              (s, l)
            | Pexp_apply _ -> ("", [])
            | _ -> error "only function application is allowed in this context"
          in
          let r =
            match s with
            | "constant" -> H.constant l
            | "foreign_value" -> H.foreign_value l
            | "foreign" -> H.foreign l
            | _ -> error "invalid function call in [%%c ... ]"
          in
          check_save_pos s;
          r
        | Pexp_extension ({ txt; loc }, _)
          when List.mem ~eq:CCString.equal txt own_extensions ->
          error ~loc "extension '%s' is not allowed here" txt
        | Pexp_letmodule (name, mexpr, e) ->
          Include_str.cond_disable true @@ fun () ->
          Static_level.disable @@ fun () ->
          let fmexpr () = self#module_expr mexpr
          and fexpr () = self#expression e in
          let m, e = Ptree.Modules.pexp_letmodule name ~mexpr ~fmexpr ~fexpr in
          { pexp with pexp_desc = Pexp_letmodule (name, m, e) }
        | Pexp_pack m ->
          Include_str.cond_disable true @@ fun () ->
          Static_level.disable @@ fun () ->
          Ptree.Modules.pexp_pack m @@ fun () -> super#expression pexp
        | Pexp_open (odl, e) ->
          let fodl () = self#open_declaration odl in
          let fexpr () = self#expression e in
          let odl, e = Ptree.Modules.pexp_open odl ~fodl ~fexpr in
          { pexp with pexp_desc = Pexp_open (odl, e) }
        | _ -> super#expression pexp

      method! module_expr m =
        Ast_helper.default_loc := m.pmod_loc;
        match m.pmod_desc with
        | Pmod_ident _ | Pmod_structure _ | Pmod_constraint _ ->
          super#module_expr m
        | Pmod_apply _ | Pmod_unpack _ | Pmod_functor _ ->
          Static_level.disable @@ fun () -> super#module_expr m
        | Pmod_extension ({ txt; loc }, _)
          when List.mem ~eq:CCString.equal txt own_extensions ->
          error ~loc "extension '%s' is not allowed here" txt
        | Pmod_extension _ ->
          Static_level.disable @@ fun () ->
          Save_position.disable @@ fun () -> super#module_expr m

      method! module_type m =
        Ast_helper.default_loc := m.pmty_loc;
        match m.pmty_desc with
        | Pmty_typeof _ ->
          Static_level.disable @@ fun () ->
          Save_position.disable @@ fun () -> super#module_type m
        | Pmty_ident _ | Pmty_signature _ | Pmty_functor _ | Pmty_with _
        | Pmty_extension _ | Pmty_alias _ ->
          super#module_type m

      method! payload p =
        Static_level.disable @@ fun () ->
        Save_position.disable @@ fun () -> super#payload p

      method! open_declaration p =
        Ast_helper.default_loc := p.popen_loc;
        Open_decl.new' p @@ fun () -> super#open_declaration p
    end

  let structure str = obj#structure str
end

module Replace () : sig
  val structure : structure -> structure
end = struct
  let remove_attrib attr l = List.filter l ~f:(fun x -> x.attr_name.txt <> attr)

  let get_usage_id_from_attribs_exn attr =
    let pl =
      let s =
        List.find attr ~f:(fun x ->
            x.attr_name.txt = Attributes.replace_attr_string)
      in
      s.attr_payload
    in
    let attr =
      List.filter attr ~f:(fun x ->
          x.attr_name.txt <> Attributes.replace_attr_string)
    in
    let id =
      match pl with
      | PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  ( { pexp_desc = Pexp_constant (Pconst_integer (x, None)); _ },
                    [] );
              _;
            };
          ] ->
        int_of_string x
      | _ -> error "attribute %S is reserved" Attributes.replace_attr_string
    in
    (attr, id)

  let mark_if_used ?pexp_outer map stri pvb pexp =
    let pexp_attributes, id =
      get_usage_id_from_attribs_exn pexp.pexp_attributes
    in
    let pexp' = { pexp with pexp_attributes } in
    let pexp' =
      match pexp_outer with
      | None -> pexp'
      | Some (pexp_outer, ct) ->
        { pexp_outer with pexp_desc = Pexp_constraint (pexp', ct) }
    in
    let used = Hashtbl.mem Script_result.htl_used id in
    let nattrib =
      if (not used) || Ocaml_config.version () < (4, 6, 0) then
        pvb.pvb_attributes
      else U.ocaml_warning "-32" :: pvb.pvb_attributes
    in
    let pvb' = { pvb with pvb_expr = pexp'; pvb_attributes = nattrib } in
    let stri' = { stri with pstr_desc = Pstr_value (Nonrecursive, [ pvb' ]) } in
    map (if used then U.no_warn_unused_pre406 stri' else stri')

  let add_tdl_entries str =
    List.fold_left ~init:[] str ~f:(fun ac -> function
      | {
          pstr_desc =
            Pstr_eval
              ( { pexp_desc = Pexp_constant (Pconst_integer (s, None)); _ },
                (_ :: _ as l) );
          pstr_loc;
          _;
        }
        when List.exists l ~f:(fun x -> x.attr_name.txt = Attributes.tdl_string)
        -> (
        match Hashtbl.find_all htl_tdl_entries @@ int_of_string s with
        | exception Failure _ ->
          error ~loc:pstr_loc "fatal: type info not found"
        | x -> x @ ac)
      | x -> x :: ac)
    |> List.rev

  let obj =
    object (self)
      inherit Ast_traverse.map as super

      method! structure str =
        let remove_empty accu el =
          match el.pstr_desc with
          | Pstr_value (Nonrecursive, [ { pvb_attributes = [ x ]; _ } ])
            when x.attr_name.txt = Attributes.remove_string ->
            accu
          | _ -> el :: accu
        in
        let rec remove_end_open = function
          | [] -> []
          | hd :: tl as l -> (
            match hd.pstr_desc with
            | Pstr_open { popen_attributes = _ :: _ as l; _ }
              when List.exists l ~f:(fun x ->
                       let t = x.attr_name.txt in
                       t = Attributes.open_struct_body_string
                       || t = Attributes.open_struct_type_mod_string
                       || t = Attributes.open_struct_openmod_string) ->
              remove_end_open tl
            | _ -> l)
        in
        add_tdl_entries str
        |> super#structure
        |> List.fold_left ~f:remove_empty ~init:[]
        |> remove_end_open
        |> List.rev_map ~f:(function
             | {
                 pstr_desc =
                   Pstr_open ({ popen_attributes = _ :: _ as l; _ } as d);
                 _;
               } as w ->
               let l =
                 remove_attrib Attributes.open_struct_body_string l
                 |> remove_attrib Attributes.open_struct_type_mod_string
                 |> remove_attrib Attributes.open_struct_openmod_string
               in
               { w with pstr_desc = Pstr_open { d with popen_attributes = l } }
             | s -> s)

      method! structure_item stri =
        let pstr_loc = stri.pstr_loc in
        Ast_helper.default_loc := pstr_loc;
        let stri = Uniq_ref.replace_stri stri in
        match stri.pstr_desc with
        | Pstr_eval
            ( { pexp_desc = Pexp_constant (Pconst_integer (s, None)); _ },
              (_ :: _ as l) )
          when List.exists l ~f:(fun x ->
                   x.attr_name.txt = Attributes.replace_expr_string) -> (
          try Hashtbl.find Script_result.htl_stri (int_of_string s)
          with Not_found -> error "fatal error: external not found")
        | Pstr_value
            ( Nonrecursive,
              [
                ({ pvb_expr = { pexp_attributes = _ :: _ as l; _ } as pexp; _ }
                as pvb);
              ] )
          when List.exists l ~f:(fun x ->
                   x.attr_name.txt = Attributes.replace_attr_string) ->
          mark_if_used super#structure_item stri pvb pexp
        | Pstr_value
            ( Nonrecursive,
              [
                ({
                   pvb_expr =
                     {
                       pexp_desc =
                         Pexp_constraint
                           (({ pexp_attributes = _ :: _ as l; _ } as pexp), ct);
                       _;
                     } as pexp_outer;
                   _;
                 } as pvb);
              ] )
          when List.exists l ~f:(fun x ->
                   x.attr_name.txt = Attributes.replace_attr_string) ->
          mark_if_used ~pexp_outer:(pexp_outer, ct) super#structure_item stri
            pvb pexp
        | Pstr_module ({ pmb_attributes = _ :: _ as l; _ } as w2)
          when List.exists l ~f:(fun x ->
                   x.attr_name.txt = Attributes.replace_attr_string) ->
          let pmb_attributes, id = get_usage_id_from_attribs_exn l in
          let pstr_desc = Pstr_module { w2 with pmb_attributes } in
          let w1 = { stri with pstr_desc } in
          let w = self#structure_item w1 in
          (* TODO: modules are now always used because of the __alias redirection.
             Is this avoidable somehow? *)
          if
            false = Hashtbl.mem Script_result.htl_used id
            || Ocaml_config.use_open_struct ()
          then w
          else U.no_warn_unused_module ~loc:pstr_loc w
        | Pstr_open { popen_attributes = _ :: _ as l; _ }
          when List.exists l ~f:(fun x ->
                   x.attr_name.txt = Attributes.open_struct_type_mod_string)
               && OSTypes.delete_os_inside_type_mod () ->
          U.empty_stri ()
        | _ -> super#structure_item stri

      method! expression pexp =
        Ast_helper.default_loc := pexp.pexp_loc;
        let pexp = Uniq_ref.replace_expr pexp in
        let la = pexp.pexp_attributes in
        if la = [] then super#expression pexp
        else
          match pexp.pexp_desc with
          | Pexp_constant (Pconst_integer (s, None))
            when List.exists la ~f:(fun x ->
                     x.attr_name.txt = Attributes.replace_expr_string) -> (
            try Hashtbl.find Script_result.htl_expr (int_of_string s)
            with Not_found -> error "fatal: constant not found")
          | Pexp_ifthenelse (_, _, Some e3)
            when List.exists la ~f:(fun x ->
                     x.attr_name.txt = Attributes.open_struct_ifthenelse_string)
            ->
            let a = remove_attrib Attributes.open_struct_ifthenelse_string la in
            let e =
              if OSTypes.remove_alias_types () then
                { e3 with pexp_attributes = e3.pexp_attributes @ a }
              else { pexp with pexp_attributes = a }
            in
            self#expression e
          | _ -> super#expression pexp

      method! type_declaration t =
        Ast_helper.default_loc := t.ptype_loc;
        match t with
        | {
            ptype_manifest =
              Some ({ ptyp_desc = Ptyp_constr ({ txt; loc }, cl); _ } as w2);
            ptype_attributes = _ :: _ as l;
            _;
          } as w
          when List.exists l ~f:(fun x ->
                   x.attr_name.txt = Attributes.manifest_replace_string) ->
          let a = remove_attrib Attributes.manifest_replace_string l in
          let txt =
            if Ptree.type_mod_is_used () = false then txt
            else
              match Longident.flatten_exn txt with
              | [] -> assert false
              | _ :: tl -> (
                match U.lid_unflatten (!Lconst.type_mod_name :: tl) with
                | None -> assert false
                | Some l -> l)
          in
          let w2 = { w2 with ptyp_desc = Ptyp_constr ({ txt; loc }, cl) } in
          { w with ptype_manifest = Some w2; ptype_attributes = a }
        | t -> super#type_declaration t
    end

  let structure str = obj#structure str
end

module Merlin () : sig
  val structure : structure -> structure
end = struct
  let error loc =
    error ~loc
      "please run ppx_cstubs preprocessor manually first, ppx_cstubs.merlin is only intended for editor integration"

  let obj =
    object
      inherit Ast_traverse.map as super

      method! structure_item s =
        match s.pstr_desc with
        | Pstr_extension (({ txt; loc }, _), _)
          when List.mem ~eq:CCString.equal txt own_extensions ->
          error loc
        | _ -> super#structure_item s

      method! expression p =
        match p.pexp_desc with
        | Pexp_extension ({ txt; loc }, _)
          when List.mem ~eq:CCString.equal txt own_extensions ->
          error loc
        | _ -> super#expression p

      method! module_expr m =
        match m.pmod_desc with
        | Pmod_extension ({ txt; loc }, _)
          when List.mem ~eq:CCString.equal txt own_extensions ->
          error loc
        | _ -> super#module_expr m
    end

  let structure str = obj#structure str
end

let mapper top = function
  | [] -> []
  | fst :: _ as str ->
    let clear () =
      Uniq_ref.clear ();
      Script_result.clear ();
      Hashtbl.clear htl_tdl_entries;
      Ptree.clear ()
    in
    let orig_log = fst.pstr_loc in
    clear ();
    Ast_helper.default_loc := orig_log;
    Lconst.init (U.unsuffixed_file_name ());
    U.convert_ctypes_exeptions @@ fun () ->
    let module S = Scan () in
    let st = S.structure str in
    Ast_helper.default_loc := orig_log;
    Ptree.Topscript.run top;
    let ppx_main = Ptree.all_top_modules () in
    let st = ppx_main @ st in
    let module R = Replace () in
    let st = R.structure st in
    C_content.write_file ();
    clear ();
    st

let init top =
  let () = Ppxc__script_real._init None in
  (match !Options.mode with
  | Options.Regular -> ()
  | Options.Emulate ->
    Ppxlib.Driver.add_arg "-pkg"
      (Arg.String
         (fun s ->
           Options.findlib_pkgs :=
             Std.Various.split_findlib_pkgs s @ !Options.findlib_pkgs))
      ~doc:"<opt>     import types from findlib package <opt>");
  let f =
    lazy
      (if
       (match !Options.mode with
       | Options.Regular -> false
       | Options.Emulate -> true)
       && CCString.find ~sub:"merlin" (Ocaml_common.Ast_mapper.tool_name ()) < 0
      then
       let module M = Merlin () in
       M.structure
      else mapper top)
  in
  let impl str = (Lazy.force f) str in
  Ppxlib.Driver.register_transformation ~impl "ppx_cstubs"
