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
open Ast_mapper
open Parsetree
open Asttypes
open Std

let ocaml_version = Versions.ocaml_405

let error = Util.error

module List = CCListLabels

module U = struct
  include Std.Util
  let named_stri n expr =
    let pat = mk_pat n in
    [%stri let [%p pat] = [%e expr]]
end


module Extract = struct

  let rec variable_from_pattern a =
    match a.ppat_desc with
    | Ppat_var({txt=(""|"_");_}) -> None
    | Ppat_var(s) -> Some s.txt
    | Ppat_constraint(s,_) -> variable_from_pattern s
    | _ -> None

  let unsupported_typ loc = error ~loc "type description not supported"

  let check_no_attribs = function
  | [] -> ()
  | (a,_)::_ -> error ~loc:a.loc "unsupported attribute: %S" a.txt

  let check_no_attribs_t t = check_no_attribs t.ptyp_attributes

  let remove_attrib str l = List.filter l ~f:(fun (x,_) -> x.txt <> str )

  let get_remove name attr =
    let at = List.exists attr ~f:(fun (x,t) ->
        if x.txt <> name then false
        else match t with
        | PStr [] -> true
        | _ -> error ~loc:x.loc "surprising content in %s" name) in
    at,
    if not at then attr else remove_attrib name attr

  let rec is_simple =
    let open Longident in function
    | Lident _-> true
    | Ldot(t,_) -> is_simple t
    | Lapply _ -> false

  let rec type_to_ctype typ =
    let open Longident in
    match typ.ptyp_desc with
    | Ptyp_constr (a,[]) when is_simple a.txt ->
      check_no_attribs_t typ;
      Ast_helper.Exp.ident ~loc:a.loc a
    | Ptyp_constr ({txt = Lident ("ptr");loc},[a]) ->
      check_no_attribs_t typ;
      let p = type_to_ctype a in
      [%expr Ctypes.ptr [%e p]][@metaloc loc]
    | Ptyp_constr ({txt = Lident ("ptr_opt");loc},[a]) ->
      check_no_attribs_t typ;
      let p = type_to_ctype a in
      [%expr Ctypes.ptr_opt [%e p]][@metaloc loc]
    | Ptyp_constr ({txt = Lident "static_funptr"; loc},[a]) ->
      check_no_attribs_t typ;
      let t = type_to_ctype_fn a in
      [%expr Ctypes.static_funptr [%e t]][@metaloc loc]
    | Ptyp_constr ({txt = Lident (("funptr"|"funptr_opt") as ns);loc},[a]) ->
      U.with_loc loc @@ fun () ->
      let t = type_to_ctype_fn a in
      let check_errno,at = get_remove "check_errno" typ.ptyp_attributes in
      let runtime_lock,at = get_remove "release_runtime_lock" at in
      let thread_registration,at = get_remove "thread_registration" at in
      check_no_attribs at;
      let h = function
      | true -> [%expr true]
      | false -> [%expr false] in
      let f = match ns with
      | "funptr_opt" -> [%expr Foreign.funptr_opt]
      | _ -> [%expr Foreign.funptr] in
      [%expr
        [%e f]
          ~check_errno:[%e h check_errno]
          ~runtime_lock:[%e h runtime_lock]
          ~thread_registration:[%e h thread_registration]
          [%e t]]
    | _ -> unsupported_typ typ.ptyp_loc
  and type_to_ctype_fn typ =
    let loc = typ.ptyp_loc in
    check_no_attribs_t typ;
    match typ.ptyp_desc with
    | Ptyp_arrow (Nolabel,
                  ({ ptyp_desc = Ptyp_constr _ ; _ } as t1),
                  ({ ptyp_desc = Ptyp_constr _ ; _ } as t2)) ->
      let t1 = type_to_ctype t1
      and t2 = type_to_ctype t2 in
      [%expr Ctypes.(@->) [%e t1] (Ctypes.returning [%e t2])][@metaloc loc]
    | Ptyp_arrow (Nolabel,
                  ({ ptyp_desc = Ptyp_constr _ ; _ } as t1),
                  ({ ptyp_desc = Ptyp_arrow _ ; _ } as t2)) ->
      let t1 = type_to_ctype t1
      and t2 = type_to_ctype_fn t2 in
      [%expr Ctypes.(@->) [%e t1] [%e t2]][@metaloc loc]
    | _ -> error ~loc "unsupported Ctypes.fn definition"

  let rec fun_def is_inline accu typ =
    let type_conf is_inline t =
      let is_ocaml_typ,attribs = get_remove "ocaml_type" t.ptyp_attributes in
      if is_ocaml_typ = false || is_inline = false then
        type_to_ctype t
      else
      let () = check_no_attribs attribs in
      let t = {t with ptyp_attributes = attribs} in
      let e = U.str_expr (Marshal.to_string t []) in
      [%expr Ctypes.ppxc__private_ocaml_typ [%e e]] in
    match typ.ptyp_desc with
    | Ptyp_constr _ ->
      if accu = [] then
        error ~loc:typ.ptyp_loc "function expected";
      type_conf is_inline typ, List.rev accu
    | Ptyp_arrow(l,t1,t2) ->
      check_no_attribs_t typ;
      let t1' = type_conf is_inline t1 in
      (match l with
      | Nolabel | Labelled _ -> ()
      | Optional _ ->
        error ~loc:typ.ptyp_loc "optional parameters are not supported");
      fun_def is_inline ((l,t1')::accu) t2
    | _ -> unsupported_typ typ.ptyp_loc

  let constant_string e =
    match e.pexp_desc with
    | Pexp_constant (Pconst_string(s,_)) -> Some s
    | _ -> None

  type enum_entry = {
    cdecl: constructor_declaration;
    enum_cname: string;
  }

  type enum = {
    ename: string;
    el: enum_entry list;
    ename_c: string;
    etypedef: bool;
    edecl: type_declaration }

  type field = {
    field_name: string;
    field_expr: Parsetree.expression;
    field_cname: string;
    field_loc: Ast_helper.loc;
  }

  type structure = {
    sname: string;
    sl: field list;
    sname_c: string;
    stypedef: bool;
    is_struct: bool;
    socaml_record: bool;
    sloc: Ast_helper.loc }

  type ctyp =
    | Enum of enum
    | Struct of structure

  let get_cname ~def l =
    let res = List.find_map l ~f:(fun (x,t) ->
      if x.txt <> "cname" then None
      else match t with
      | PStr [{pstr_desc = Pstr_eval({
        pexp_desc =
          Pexp_constant (Pconst_string (x, _)); _ },
        []); _ }] -> Some x
      | _ -> error ~loc:x.loc "unsupported expression in cname") in
    match res with
    | None -> def, l
    | Some x -> x, remove_attrib "cname" l

  let extract_enum_entry = function
  | ({pcd_name; pcd_args = Pcstr_tuple []; pcd_res = None; pcd_loc = _;
      pcd_attributes } as whole) ->
    let name = pcd_name.txt in
    let cname, attribs = get_cname ~def:name pcd_attributes in
    check_no_attribs attribs;
    let cdecl = {whole with pcd_attributes = []} in
    {cdecl; enum_cname = cname}
  | {pcd_loc;_} -> error ~loc:pcd_loc "Unsupported constructor type"

  let extract_field {pld_name; pld_mutable; pld_type; pld_loc; pld_attributes} =
    if pld_mutable <> Asttypes.Immutable then
      error ~loc:pld_loc "only immutable is possible";
    let field_name = pld_name.txt in
    let field_expr = type_to_ctype pld_type in
    let field_cname,attribs = get_cname ~def:field_name pld_attributes in
    check_no_attribs attribs;
    {field_name; field_expr; field_cname; field_loc = pld_loc}

  let type_decl = function
  | ({ptype_name; ptype_params = []; ptype_cstrs = [];
      ptype_private = Public; ptype_manifest = _; ptype_attributes;
      ptype_kind = Ptype_variant ((_::_) as l); ptype_loc = _ } as whole) ->
    let ename = ptype_name.txt in
    let ename_c,attribs = get_cname ~def:ename ptype_attributes in
    let etypedef,attribs = get_remove "typedef" attribs in
    check_no_attribs attribs;
    let el = List.map l ~f:extract_enum_entry in
    let edecl = {
      whole with
      ptype_kind = Ptype_variant (List.map el ~f:(fun x -> x.cdecl));
      ptype_attributes = [] } in
    Enum {ename; el; ename_c; etypedef; edecl}
  | {ptype_name; ptype_params = []; ptype_cstrs = [];
     ptype_private = Public; ptype_manifest = None; ptype_attributes;
     ptype_kind = Ptype_record ((_::_) as l); ptype_loc} ->
    let sname = ptype_name.txt in
    let sname_c,attribs = get_cname ~def:sname ptype_attributes in
    let stypedef,attribs = get_remove "typedef" attribs in
    let union,attribs = get_remove "union" attribs in
    let socaml_record,attribs = get_remove "ocaml_record" attribs in
    check_no_attribs attribs;
    let is_struct = not union in
    let sl = List.map ~f:extract_field l in
    Struct {sname; sl; sname_c; stypedef; is_struct; sloc = ptype_loc;
            socaml_record}
  | {ptype_loc ; _} -> error ~loc:ptype_loc "unsupported type definition"

  let type_decl l = List.map ~f:type_decl l
end

module Ppx_mod_top_common(M:sig end) = struct
  type items = Parsetree.structure_item list
  type t =
    | Root of items
    | Module of t * string * items
    | Let_module of t * string * items

  let state = ref (Root [])
  let get_entries () =
    match !state with
    | Root x
    | Module(_,_,x)
    | Let_module(_,_,x) -> List.rev x

  let add_entry x =
    match !state with
    | Root el -> state := Root(x::el)
    | Module(t,s,l) -> state := Module(t,s,x::l)
    | Let_module(t,s,l) -> state := Let_module(t,s,x::l)

  let open_module s =
    state := Module(!state, s, [])

  let open_let_module s =
    let s_out = U.safe_mlname ~prefix:s () |> CCString.capitalize_ascii in
    state := Module(!state,s_out,[]);
    state := Let_module(!state,s,[])

  let add_module internal s l =
    if l = [] then ()
    else
    let open Ast_helper in
    if Hashtbl.mem Keywords.htl_modules s then
      error "module name %S is reserved" s;
    if internal = false && CCString.prefix ~pre:"Ppxc__" s then
      error "module prefix Ppxc__ is reserved (%S)" s;
    let ms = Mod.structure (List.rev l) in
    let mb = Mb.mk (U.mk_loc s) ms in
    let x = Str.module_ mb in
    add_entry x

  let close_let_module_module loc =
    U.with_loc loc @@ fun () ->
    match !state with
    | Let_module(t,s,l) ->
      state := t;
      add_module false s l
    | Module _  | Root _ ->
      error "ppx_cstubs failed to parse the AST"

  let close_module loc =
    U.with_loc loc @@ fun () ->
    match !state with
    | Module(t,s,l) ->
      state := t;
      add_module false s l
    | Let_module _
    | Root _ -> error "ppx_cstubs failed to parse the AST"

  let close_let_module_expr loc =
    U.with_loc loc @@ fun () ->
    match !state with
    | Module(t,s,l) ->
      state := t;
      add_module true s l
    | Let_module _
    | Root _ -> error "ppx_cstubs failed to parse the AST"

end

module Ppx_mod = struct

  let mod_name = ref None
  let main_name () =
    match !mod_name with
    | Some s -> s
    | None ->
      let module Lo = Location in
      let module Le = Lexing in
      let loc = !Ast_helper.default_loc in
      let fname = Filename.basename loc.Lo.loc_start.Le.pos_fname in
      let fname = match CCString.Split.left ~by:"." fname with
      | None -> ""
      | Some(s,_) -> s in
      let name =
        Printf.sprintf "Ppxc__%s_%x_%x"
          fname
          loc.Lo.loc_start.Le.pos_lnum
          loc.Lo.loc_start.Le.pos_cnum
        |> Std.Util.safe_ascii_only_ml in
      mod_name := Some name;
      name

  module Structure = Ppx_mod_top_common(struct end)
  open Structure

  let create_ref s =
    let open Longident in
    let rec iter = function
    | Root _ -> Lident (main_name ())
    | Module(t,s,_) | Let_module(t,s,_) -> Ldot(iter t,s) in
    U.mk_loc @@ Ldot(iter !state,s)

  let create_type_ref ?(params=[]) s =
    let r = create_ref s in
    Ast_helper.Typ.constr r params

  let create_ref ?attrs s =
    let r = create_ref s in
    Ast_helper.Exp.ident ?attrs r

  let modules () =
    let entries = get_entries () in
    if entries = [] then [] else
    let l =
      if !Options.disable_shadow = true then []
      else [%stri open! Ppx_cstubs_shadow [@@ ocaml.warning "-33"]] :: [] in
    let l =
      let pervasives =
        let ov = Ocaml_config.version () in
        if ov >= (4,8,0) then
          [%stri open! Stdlib [@@ ocaml.warning "-33"]]
        else if ov >= (4,7,0) then
          [%stri open! Stdlib.Pervasives [@@ ocaml.warning "-33"]]
        else
          [%stri open! Pervasives [@@ ocaml.warning "-33"]] in
      let open_foreign = match !Script_result.foreign_used with
      | false -> []
      | true -> [[%stri open! Foreign [@@ ocaml.warning "-33"]]] in
      let entries = get_entries () in
      let entries =
        pervasives::
        [%stri open! Ctypes [@@ ocaml.warning "-33"]]::
        (open_foreign@entries) in
      let module A = Ast_helper in
      let ms = A.Mod.structure entries in
      let mb = A.Mb.mk (U.mk_loc (main_name ())) ms in
      let generated_module = A.Str.module_ mb in
      generated_module::l
    in
    match !Options.mode with
    | Options.Regular -> l
    | Options.Emulate ->
      [%stri let () =
               failwith "ppx_cstubs.merlin is not intended to generate real code"]::l

  let add_common ?attrs n expr =
    add_entry @@ U.named_stri n expr;
    create_ref ?attrs n

  let add_external stri_external stri_expr name =
    add_entry stri_external;
    add_entry stri_expr;
    create_ref name

  let add_named ?attrs x expr =
    if x = "" || x = "_" then error "variable name empty"
    else
    let f = x.[0] in
    if not ((f >= 'a' && f <= 'z') || f = '_') then
      error "prefix or infix symbols are not allowed here";
    if Hashtbl.mem Keywords.htl x then
      error "%S is predefined. You should'nt shadow it here" x;
    if CCString.prefix ~pre:"ppxc__" x then
      error "the ppxc__ prefix is reserved for generated code";
    add_common ?attrs x expr

  let add_unit expr =
    add_entry [%stri let () = [%e expr]];
    [%expr ()]
end


module C_content = struct

  let write_file () : unit =
    if Options.(!mode = Emulate) then ()
    else match !Options.c_output_file,!Script_result.c_source with
    | Some fln, Some source ->
      Options.c_output_file := None;
      (match fln with
      | "-" -> output_string stdout source
      | _ ->
        CCIO.with_out
          ?mode:None ~flags:[Open_creat; Open_trunc; Open_binary] fln @@
        fun ch -> output_string ch source)
    | Some _, None ->
      prerr_endline
        "no c file necessary, set -o-c to 'none' and update your build instructions";
      exit 2
    | None, None -> ()
    | None, Some _ -> failwith "internal error: c source not generated"
end

module Id = struct
  open Ast_helper
  type t = {
    id: int;
    script_param: Parsetree.expression;
    expr: Parsetree.expression;
    stri: Parsetree.structure_item;
  }

  let get =
    let cnt = ref 0 in
    fun ?(loc = !Ast_helper.default_loc) () ->
      let id = !cnt in
      incr cnt;
      let (t : Marshal_types.id_loc_param) = id,loc in
      let loc_id_param = Marshal.to_string t [] in
      let script_param = U.str_expr ~loc loc_id_param in
      let attrs = [Attributes.replace_expr_attrib] in
      let expr = U.int_expr ~loc ~attrs id in
      let stri = Str.eval ~attrs expr in
      {id; script_param; expr; stri}

  let get_usage_id =
    let cnt = ref 0 in
    fun () ->
      let id = !cnt in
      incr cnt;
      let script_param = U.int_expr id in
      let st = [%stri [%e script_param]] in
      let attrs =
        let x = U.mk_loc Attributes.replace_attr_string in
        [ x, PStr [st] ] in
      script_param,attrs

  let get_tdl_entries_id =
    let cnt = ref 0 in
    fun () ->
      let id = !cnt in
      incr cnt;
      let expr = U.int_expr id in
      id, Str.eval ~attrs:[Attributes.tdl_attrib] expr
end

module Top = struct

  module Structure_extract = Ppx_mod_top_common(struct end)
  module Structure_build_external = Ppx_mod_top_common(struct end)

  let loc () =
    let open Ast_helper in
    let loc = U.str_expr (Marshal.to_string !default_loc []) in
    [%stri let () = Ppxc__script.set_loc [%e loc]]

  let add_build_external s =
    Structure_build_external.add_entry @@ loc ();
    Structure_build_external.add_entry s

  let add_extract s =
    Structure_extract.add_entry @@ loc ();
    Structure_extract.add_entry s

  let run () =
    let c_const_entries = Structure_extract.get_entries () in
    let build_entries = Structure_build_external.get_entries () in
    let ctypes =
      let ws = Ocaml_config.word_size () in
      if ws = Sys.word_size then [%stri
        module Ctypes =
          Ppxc__script.Ctypes_make.Ctypes(Ppxc__script.Ctypes_make.Info)]
      else if ws = 32 then [%stri
        module Ctypes =
          Ppxc__script.Ctypes_make.Ctypes(Ppxc__script.Ctypes_make.Info32)]
      else [%stri
        module Ctypes =
          Ppxc__script.Ctypes_make.Ctypes(Ppxc__script.Ctypes_make.Info64)] in
    let expr = [%expr
      let module M = struct
        open! Pervasives [@@ ocaml.warning "-33"]
        open! Ppxc__script.Run_environment [@@ ocaml.warning "-33"]
        [%%s [ctypes]]
        open! Ctypes [@@ ocaml.warning "-33"]
        module Extract_c_consts = struct
          [%%s c_const_entries]
        end
        module Build_externals = struct
          [%%s build_entries]
        end
      end in
      ((Ppxc__script.Main.run ()) : unit) ] in
    Toplevel.init ();
    ignore ( Toplevel.eval_expression expr : Obj.t )
end

module Scripts_structure = struct
  (* Script structure and Top module structure might differ
     in the future. Not yet sure what to do about
     functors and similar constructs *)
  let open_module s =
    Top.Structure_build_external.open_module s;
    Top.Structure_extract.open_module s;
    Ppx_mod.Structure.open_module s

  let open_let_module s =
    Top.Structure_build_external.open_let_module s;
    Top.Structure_extract.open_let_module s;
    Ppx_mod.Structure.open_let_module s

  let close_let_module_module loc =
    Top.Structure_build_external.close_let_module_module loc;
    Top.Structure_extract.close_let_module_module loc;
    Ppx_mod.Structure.close_let_module_module loc

  let close_let_module_expr loc =
    Top.Structure_build_external.close_let_module_expr loc;
    Top.Structure_extract.close_let_module_expr loc;
    Ppx_mod.Structure.close_let_module_expr loc

  let close_module loc =
    Top.Structure_build_external.close_module loc;
    Top.Structure_extract.close_module loc;
    Ppx_mod.Structure.close_module loc
end

let htl_tdl_entries = Hashtbl.create 32

module H = struct
  open Ast_helper

  let error_msg s = function
  | [] -> error "arguments missing for %s" s
  | _ -> error "too many or too few arguments for %s" s

  let constant_common name = function
  | (Nolabel,str_expr)::(Nolabel,type_expr)::[] ->
    let id = Id.get () in
    let pat = match name with
    | None -> [%pat? ()]
    | Some x -> U.mk_pat x in
    let script = [%stri
      let [%p pat] =
        let (ppxc__s : string) = [%e str_expr] in
        let (ppxc__t : _ Ctypes.typ) = [%e type_expr] in
        Ppxc__script.Extract.constant
          [%e id.Id.script_param] ppxc__s ppxc__t] in
    Top.add_extract script;

    (match name with
    | None -> id.Id.expr
    | Some name ->
      let script = [%stri
        let [%p pat] =
          Ppxc__script.Build.constant
            [%e id.Id.script_param]
            [%e str_expr]
            [%e type_expr]] in
      Top.add_build_external script;

      Ppx_mod.add_named name id.Id.expr)
  | l -> error_msg "constant" l

  let constant_bind name l = constant_common (Some name) l
  let constant l = constant_common None l

  let marshal_expr (e:Marshal_types.expr) =
    U.str_expr (Marshal.to_string e [])

  let register_fun id =
    let script = [%stri
      let () = Ppxc__script.Extract.register_fun_place
          [%e id.Id.script_param]] in
    Top.add_extract script

  let foreign_value = function
  | (Nolabel,str_expr)::(Nolabel,typ_expr)::[] ->
    let prefix = Extract.constant_string str_expr in
    let name = U.safe_mlname ?prefix () in
    let name_expr = U.str_expr name in
    let id_stri_external = Id.get () in
    let id_stri_expr = Id.get () in

    register_fun id_stri_external;

    let texpr = marshal_expr typ_expr in
    let script = [%stri
      let () =
        let (ppxc__s : string) = [%e str_expr] in
        let (ppxc__t : _ Ctypes.typ) = [%e typ_expr] in
        Ppxc__script.Build.foreign_value
          [%e id_stri_external.Id.script_param]
          [%e id_stri_expr.Id.script_param]
          ppxc__t
          ppxc__s
          [%e name_expr]
          [%e texpr]
    ] in
    Top.add_build_external script;

    Ppx_mod.add_external
        id_stri_external.Id.stri id_stri_expr.Id.stri name
  | l -> error_msg "foreign_value" l

  let header = function
  | (Nolabel,x)::[] ->
    (match Extract.constant_string x with
    | Some _ ->
      let script = [%stri
        let () = Ppxc__script.Extract.header [%e x]] in
      Top.add_extract script;
      [%expr ()]
    | None -> error "'header' requires a string constant")
  | l -> error_msg "header" l

  let field_intern ?prefix = function
  | [ (Nolabel,structure); (Nolabel,str_expr); (Nolabel,type_expr) ] ->
    let id = Id.get () in
    let n = U.safe_mlname ?prefix () in
    let p = U.mk_pat n in
    let script = [%stri
      let [%p p] =
        let (ppxc__st : _ Ctypes.typ) = [%e structure] in
        let (ppxc__s : string) = [%e str_expr] in
        let (ppxc__t : _ Ctypes.typ) = [%e type_expr] in
        let (ppxc__res  : _ Ctypes.field ) =
          Ctypes.ppxc__private_field ppxc__st ppxc__s ppxc__t in
        Ppxc__script.Extract.field [%e id.Id.script_param] ppxc__st ppxc__s;
        ppxc__res ] in
    Top.add_extract script;

    let script = [%stri
      let [%p p] : _ Ctypes.field =
        let (ppxc__typ : _ Ctypes.typ) = [%e type_expr] in
        Ctypes.ppxc__private_field
          [%e structure] [%e str_expr] ppxc__typ] in
    Top.add_build_external script;

    let nexpr = [%expr
      Ppx_cstubs_internals.add_field
        [%e structure]
        [%e str_expr]
        [%e id.Id.expr]
        [%e type_expr]] in
    let res = Ppx_mod.add_common n nexpr in
    n,res
  | l -> error_msg "field" l

  let field ?prefix x = field_intern ?prefix x |> snd

  let seal = function
  | [(Nolabel,seal_struct)] ->
    let id_size = Id.get () in
    let id_align = Id.get () in

    let script = [%stri
      let () =
        let (ppxc__s : _ Ctypes.typ) = [%e seal_struct] in
        let () = Ctypes.ppxc__private_seal ppxc__s in
        Ppxc__script.Extract.seal
          [%e id_size.Id.script_param]
          [%e id_align.Id.script_param]
          ppxc__s] in
    Top.add_extract script;

    let script = [%stri
      let () = Ctypes.ppxc__private_seal [%e seal_struct]] in
    Top.add_build_external script;

    let nexpr = [%expr
      Ppx_cstubs_internals.seal
        [%e seal_struct]
        ~size:[%e id_size.Id.expr]
        ~align:[%e id_align.Id.expr]] in
    Ppx_mod.add_unit nexpr

  | l -> error_msg "seal" l

  let is_ocaml_operator = function
  | "mod" | "or" | "land" | "lor" | "lxor" | "lsl" | "lsr" | "asr" -> true
  | "" -> false
  | c ->
    match c.[0] with
    | '!' | '#' | '$' | '%' | '&' | '*' | '+' | '-' | '/' | '<' | '='
    | '>' | '?' | '@' | '^' | '|' | '~' -> true
    | _ -> false

  let rec build_ctypes_fn params ret =
    match params with
    | [] -> [%expr Ctypes.returning [%e ret]][@metaloc ret.pexp_loc]
    | hd::tl ->
    let e = build_ctypes_fn tl ret in
    [%expr Ctypes.(@->) [%e hd] [%e e]][@metaloc hd.pexp_loc]

  let external' ~is_inline ~remove_labels v ~return_errno ~release_runtime_lock
      ~noalloc =
    let name = v.pval_name.txt in
    let c_name = match v.pval_prim with
    | [""] | ["_"] -> error ~loc:v.pval_loc "function name missing"
    | [a] -> a
    | _ -> error ~loc:v.pval_loc "two many c functions referenced" in
    if is_inline = false &&
       U.safe_ascii_only c_name <> c_name then
      U.error "invalid identifier for c function:%S" c_name;
    let remove_labels =
      remove_labels || (is_inline && is_ocaml_operator name) in
    if is_inline = false && remove_labels then
      error ~loc:v.pval_loc "remove_labels only supported for inline code";
    let ret,el = Extract.fun_def is_inline [] v.pval_type in
    let fun_expr = build_ctypes_fn (List.map ~f:snd el) ret in
    let ocaml_name = U.safe_mlname ~prefix:name () in
    let marshal_info =
      let open Marshal_types in
      let s = {el; ret; release_runtime_lock; noalloc;
               return_errno; is_inline; remove_labels;
               c_name; ocaml_name; prim_name = name} in
      U.str_expr (Marshal.to_string s []) in

    let id_stri_expr = Id.get () in
    let id_stri_external = Id.get () in

    if is_inline then (* order matters *)
      register_fun id_stri_expr;
    register_fun id_stri_external;

    let script = [%stri
      let () =
        let (ppxc__fn : _ Ctypes.fn) = [%e fun_expr] in
        Ppxc__script.Build.external'
          [%e id_stri_external.Id.script_param]
          [%e id_stri_expr.Id.script_param]
          ppxc__fn
          ~marshal_info:[%e marshal_info]
    ] in
    Top.add_build_external script;

    let expr =
      Ppx_mod.add_external
        id_stri_external.Id.stri
        id_stri_expr.Id.stri
        ocaml_name in
    U.named_stri name expr

  module E = struct
    let get_typedef_expr l =
      let t = List.find_map l ~f:(function
      | Optional "typedef",_
      | (Labelled "typedef",_) as x -> Some(x)
      | _ -> None) in
      match t with
      | Some s -> s
      | None ->
        let n_unlablled = List.fold_left ~init:0 l ~f:(fun ac -> function
        | Nolabel,_ -> succ ac
        | ((Optional _)|(Labelled _)),_ -> ac) in
        if n_unlablled > 1 then
          error "too many parameters for enum";
        Labelled "typedef",[%expr false]

    let find_first_unlabelled l =
      List.find_mapi l ~f:(fun i el -> match el with
      | Nolabel,_ -> Some (i,el)
      | _ -> None)

    let filter_pos_and_rev pos l =
      List.fold_left ~init:(0,[]) l ~f:(fun (i,ac) el ->
        if i = pos then (succ i,ac) else (succ i, el::ac))
      |> snd
  end

  let enum binding_name l =
    let open Asttypes in
    let open E in

    (* remove enum name from param list *)
    let pos,enum_name_expr = match find_first_unlabelled l with
    | None -> error "enum definitions need a constant string"
    | Some s -> s in
    let l = filter_pos_and_rev pos l |> List.rev in

    let l_typedef_expr = get_typedef_expr l in
    let id = Id.get () in
    let pat = U.mk_pat binding_name in

    let fun' = Exp.apply [%expr Ppxc__script.Extract.enum]
        [(Nolabel,id.Id.script_param);
         l_typedef_expr;
         enum_name_expr] in
    let stri = [%stri let [%p pat] : _ Ctypes.typ = [%e fun']] in
    Top.add_extract stri;

    let id_expr,attrs = Id.get_usage_id () in
    let fun' = [%expr Ppxc__script.Build.enum [%e id.Id.script_param]] in
    let fun' = Exp.apply fun' (enum_name_expr::l) in
    let stri = [%stri
      let [%p pat] : _ Ctypes.typ =
        let ppxc__t : _ Ctypes.typ = [%e fun'] in
        Ppxc__script.Build.reg_trace [%e id_expr] ppxc__t] in
    Top.add_build_external stri;

    let l = enum_name_expr::(Asttypes.Nolabel,id.Id.expr)::l in
    let expr = Exp.apply [%expr Cstubs_internals.build_enum_type] l in
    Ppx_mod.add_named ~attrs binding_name expr

  let foreign ?prefix l =
    let typ_expr = match List.rev l |> E.find_first_unlabelled with
    | None -> error "function signature in foreign not found"
    | Some(_,l) -> snd l in
    let typ_expr = marshal_expr typ_expr in
    let ocaml_name =
      let t = List.find_map l ~f:(function
      | Asttypes.Nolabel,e -> Extract.constant_string e
      | _ -> None) in
      match t with
      | None -> U.safe_mlname ?prefix ()
      | Some s -> U.safe_mlname ~prefix:s () in
    let id_stri_external = Id.get () in
    let id_stri_expr = Id.get () in

    register_fun id_stri_external;

    let f_expr = [%expr
      Ppxc__script.Build.foreign
        [%e id_stri_external.Id.script_param]
        [%e id_stri_expr.Id.script_param]
        ~ocaml_name:[%e U.str_expr ocaml_name]
        ~typ_expr:[%e typ_expr]] in
    let call = Exp.apply f_expr l in
    let script = [%stri let () = [%e call]] in
    Top.add_build_external script;
    Ppx_mod.add_external
      id_stri_external.Id.stri
      id_stri_expr.Id.stri
      ocaml_name

  let fn binding_name expr =
    let pat = U.mk_pat binding_name in

    let script = [%stri
      let [%p pat] =
        let (ppxc__fn : _ Ctypes.fn) = [%e expr] in
        ppxc__fn] in
    Top.add_extract script;

    let id_expr,attrs = Id.get_usage_id () in
    let script = [%stri
      let [%p pat] =
        Ppxc__script.Build.reg_trace_fn [%e id_expr] [%e expr]] in
    Top.add_build_external script;

    Ppx_mod.add_named ~attrs binding_name expr

  let typ_intern binding_name expr =
    let pat = U.mk_pat binding_name in

    let script = [%stri
      let [%p pat] =
        let (ppxc__t : _ Ctypes.typ) = [%e expr] in
        ppxc__t] in
    Top.add_extract script;

    let id_expr,attrs = Id.get_usage_id () in
    let script = [%stri
      let [%p pat] = Ppxc__script.Build.reg_trace [%e id_expr] [%e expr]] in
    Top.add_build_external script;
    attrs

  let typ binding_name expr =
    let attrs = typ_intern binding_name expr in
    Ppx_mod.add_named ~attrs binding_name expr

  let type_decl ~enforce_union type_rec_flag = function
  | [] -> error "empty type definition"
  | tl ->
    let open Extract in
    let tdl_entry_id,tdl_entry = Id.get_tdl_entries_id () in
    let private_pref =
      if type_rec_flag = Asttypes.Recursive then
        Std.identity
      else
      let s =
        let open Location in
        let open Lexing in
        let loc = !Ast_helper.default_loc in
        Printf.sprintf
          "__ppxc__private_%x_%x_"
          loc.loc_start.pos_lnum
          loc.loc_start.pos_cnum in
      fun x -> s ^ x in
    let unhide x =
      let name = match x with
      | Enum x -> x.ename
      | Struct x -> x.sname in
      let pat = U.mk_pat name in
      let e = Exp.ident (U.mk_lid (private_pref name)) in
      let stri = [%stri let [%p pat] = [%e e] [@@ ocaml.warning "-32"]] in
      Top.add_extract stri;
      Top.add_build_external stri;
      Ppx_mod.Structure.add_entry stri in

    let add_type ?(tdl=true) ?params typ' =
      let t = Str.type_ Asttypes.Recursive [typ'] in
      Top.add_extract t;
      Top.add_build_external t;
      Ppx_mod.Structure.add_entry t;
      if tdl then
        let t' =
          let name = typ'.ptype_name.txt in
          let constr = Ppx_mod.create_type_ref ?params name in
          let typ' = {typ' with ptype_manifest = Some constr} in
          Str.type_  Asttypes.Recursive [typ'] in
        Hashtbl.add htl_tdl_entries tdl_entry_id t' in

    let fields = function
    | Enum _ -> ()
    | Struct {sname; sl ; socaml_record ; _ } ->
      let sname = private_pref sname in
      let struct_expr = Exp.ident (U.mk_lid sname) in
      let fnames = List.map sl ~f:(function
        {field_name; field_expr; field_cname; field_loc} ->
        U.with_loc field_loc @@ fun () ->
        let str_expr = U.str_expr field_cname in
        let n,e = field_intern ~prefix:field_name [
          Nolabel, struct_expr;
          Nolabel, str_expr;
          Nolabel, field_expr ] in
        if socaml_record = false then (
          let x = U.named_stri field_name e in
          Hashtbl.add htl_tdl_entries tdl_entry_id x);
        n ) in
      ignore ( seal [Nolabel,struct_expr] : Parsetree.expression );
      if socaml_record = false then ()
      else
      (* Generate the view for the type *)

      let () = (* Generate the type *)
        (* TODO: the type is parameterized. Not sure how to avoid
           it in the general case. But at least for primitives, it could
           be avoided ... *)
        let params = List.mapi sl ~f:(fun i f ->
          let loc = f.field_loc in
          let s =
            if i > 26 then "z" ^ string_of_int i
            else String.init 1 (fun _ -> Char.chr (97 + i)) in
          let t = Typ.mk ~loc (Ptyp_var s) in
          let a = t,Asttypes.Invariant in
          let n = Location.mkloc f.field_name loc in
          let b = Type.field n t in
          a,b) in
        let params,fields = List.split params in
        let n = U.mk_loc sname in
        let typ' = Type.mk ~params ~kind:(Ptype_record fields) n in
        add_type ~params:(List.map ~f:fst params) typ' in

      (* create the view *)
      let make_view private' =
        let view,setf,getf,make = match private' with
        | false ->
          [%expr Ctypes_static.view],
          [%expr Ctypes.setf],[%expr Ctypes.getf],[%expr Ctypes.make]
        | true ->
          [%expr Ctypes.view],
          [%expr Ctypes.ppxc__private_setf],
          [%expr Ctypes.ppxc__private_getf],
          [%expr Ctypes.ppxc__private_make] in
        let init = [%expr ppxc__res] in
        let param = Exp.ident (U.mk_lid "ppxc__param") in
        let expr = List.fold_left2 ~init sl fnames ~f:(fun ac el fname ->
          let fi = U.mk_lid el.field_name in
          let v = Exp.field param fi in
          let f = Exp.ident (U.mk_lid fname) in
          [%expr let () = [%e setf] ppxc__res [%e f] [%e v] in [%e ac]]) in
        let write = [%expr
          fun ppxc__param ->
            let ppxc__res = [%e make] [%e struct_expr] in [%e expr]] in
        let init = Exp.record (List.map sl ~f:(fun el ->
          let l = U.mk_lid el.field_name in
          let e = Exp.ident l in
          l,e)) None in
        let expr = List.fold_left2 ~init sl fnames ~f:(fun ac el fname ->
          let p = U.mk_pat el.field_name in
          let f = Exp.ident (U.mk_lid fname) in
          [%expr let [%p p] = [%e getf] ppxc__param [%e f] in [%e ac]]) in
        let read = [%expr fun ppxc__param -> [%e expr]] in
        [%expr [%e view] ~read:[%e read] ~write:[%e write] [%e struct_expr]] in

      let view = make_view true in
      let attrs = typ_intern sname view in
      let view = make_view false in
      let expr = Ppx_mod.add_named ~attrs sname view in
      let expr =
        let x = Typ.constr (U.mk_lid sname) [Typ.any ()] in
        let x = Typ.constr (U.mk_lid "Ctypes.typ") [x] in
        Exp.constraint_ expr x in
      U.named_stri sname expr |> Hashtbl.add htl_tdl_entries tdl_entry_id
    in

    let single_typ = function
    | Enum {ename; el; ename_c; etypedef; edecl} ->
      U.with_loc edecl.ptype_loc @@ fun () ->
      let real_name = ename in
      let ename = private_pref ename in

      add_type edecl;

      let res,exp_l,enum_l =
        let init = ([%expr []],[%expr []],[]) in
        ListLabels.fold_right el ~init ~f:(fun cur (l1,l2,l3) ->
          let loc = cur.cdecl.pcd_loc in
          U.with_loc loc @@ fun () ->
          let id = Id.get ~loc () in
          let variant_name = Exp.construct (U.mk_lid cur.cdecl.pcd_name.txt) None in
          let cstr = cur.enum_cname in
          let tup = Exp.tuple [variant_name; id.Id.expr] in
          let tup = Exp.tuple [tup; l1] in
          let tup2 = Exp.tuple [variant_name; l2] in
          Exp.construct (U.mk_lid "::") (Some tup),
          Exp.construct (U.mk_lid "::") (Some tup2),
          (id.Id.id,loc,cstr)::l3) in
      let id = Id.get () in
      let sparam =
        let open Marshal_types in
        let s = Marshal.to_string
            {enum_l; enum_name = ename_c; enum_is_typedef = etypedef;
             enum_id = id.Id.id; enum_loc = edecl.ptype_name.loc} [] in
        U.str_expr s in

      let pat = U.mk_pat ename in
      let script = [%stri
        let [%p pat] : 'a Ctypes.typ =
          Ppxc__script.Extract.enum2 [%e exp_l] [%e sparam]] in
      Top.add_extract script;

      let id_expr,attrs = Id.get_usage_id () in
      let script = [%stri
        let [%p pat] : 'a Ctypes.typ =
          Ppxc__script.Build.enum2 [%e exp_l] [%e sparam]
          |> Ppxc__script.Build.reg_trace [%e id_expr]] in
      Top.add_build_external script;

      let e_typdf = if etypedef then [%expr true] else [%expr false] in
      let expr = [%expr Cstubs_internals.build_enum_type ~typedef:[%e e_typdf]
          [%e U.str_expr ename_c] [%e id.Id.expr] [%e res]] in
      let s2 = Ppx_mod.add_named ~attrs ename expr in
      let s2 =
        let x = Typ.constr (U.mk_lid real_name) [] in
        let x = Typ.constr (U.mk_lid "Ctypes.typ") [x] in
        Exp.constraint_ s2 x in
      U.named_stri real_name s2
      |> Hashtbl.add htl_tdl_entries tdl_entry_id

    | Struct {sname; sl = _ ; sname_c; stypedef; is_struct; sloc;
              socaml_record} ->
      U.with_loc sloc @@ fun () ->
      let real_name = sname in
      let sname = private_pref sname in
      let is_struct = if enforce_union then false else is_struct in
      if socaml_record && is_struct = false then
        error "unions can't be viewed as OCaml records";

      let type_name =
        if socaml_record = false then real_name
        else "ppxc__" ^ real_name in

      let typ' = Type.mk ~kind:Ptype_abstract (U.mk_loc type_name) in
      add_type ~tdl:(not socaml_record) typ';

      let expr =
        let name = if stypedef then "" else sname_c in
        let sexpr = U.str_expr name in
        match is_struct with
        | true -> [%expr Ctypes.structure [%e sexpr]]
        | false -> [%expr Ctypes.union [%e sexpr]] in
      let constr expr =
        let s = if is_struct then "Ctypes.structure" else "Ctypes.union" in
        let x = Typ.constr (U.mk_lid type_name) [] in
        let x = Typ.constr (U.mk_lid s) [x] in
        let x = Typ.constr (U.mk_lid "Ctypes.typ") [x] in
        Exp.constraint_ expr x in
      let expr = constr expr in

      let add x =
        let x = U.named_stri real_name (constr x) in
        if socaml_record = false then
          Hashtbl.add htl_tdl_entries tdl_entry_id x in
      let x = typ sname expr in
      if stypedef = false then add x
      else
      let sexpr = U.str_expr sname_c in
      let expr = Exp.ident (U.mk_lid sname) in
      let expr = [%expr Ctypes.typedef [%e expr] [%e sexpr]] in
      typ sname expr |> add in
    let tl = Extract.type_decl tl in
    if enforce_union &&
       List.for_all tl ~f:(function Enum _ -> true | Struct _ -> false) then
      error "enum entry marked as union";
    List.iter ~f:single_typ tl;
    List.iter ~f:fields tl;
    if type_rec_flag <> Asttypes.Recursive then
      List.iter ~f:unhide tl;
    tdl_entry
end

let convert_ctypes_exeptions f =
  try
    f ()
  with
  | Ctypes_static.ModifyingSealedType s -> error "%s is already sealed" s
  | Ctypes_static.Unsupported s -> error "ctypes error: %s" s
  | Ctypes_static.IncompleteType -> error "Incomplete Type"

let mark_if_used ?pexp_outer mapper stri pvb pexp =
  let _,pl = List.find pexp.pexp_attributes ~f:(fun (x,_) ->
    x.txt == Attributes.replace_attr_string) in
  let attrib = List.filter pexp.pexp_attributes ~f:(fun (x,_) ->
    x.txt != Attributes.replace_attr_string)  in
  let pexp' = { pexp with pexp_attributes = attrib } in
  let pexp' = match pexp_outer with
  | None -> pexp'
  | Some(pexp_outer,ct) ->
    {pexp_outer with pexp_desc = Pexp_constraint(pexp',ct)} in
  let id = match pl with
  | PStr [{pstr_desc =
             Pstr_eval({
               pexp_desc =
                 Pexp_constant (Pconst_integer (x, None)); _},
               []);
           _ }] -> int_of_string x
  | _ -> error "attribute %S is reserved" Attributes.replace_attr_string in
  let nattrib =
    if not (Hashtbl.mem Script_result.htl_used id) then pvb.pvb_attributes
    else
    let warn_id = U.str_expr "-32" in
    let st = [%stri [%e warn_id]] in
    let x = Location.mkloc "ocaml.warning" pvb.pvb_loc in
    (x,PStr [st])::pvb.pvb_attributes in
  let pvb' = {pvb with
              pvb_expr = pexp';
              pvb_attributes = nattrib} in
  let stri' = {stri with
               pstr_desc = Pstr_value(Nonrecursive,
                                      [pvb'])} in
  default_mapper.structure_item mapper stri'

let remove_empty str =
  List.filter str ~f:(function
  | {pstr_desc = Pstr_value(Nonrecursive,[
    {pvb_attributes=[({txt;_},_)];_}]); _ }
    when txt == Attributes.remove_string -> false
  | _ -> true)

let mark_empty a =
  if a.pvb_attributes <> [] ||
     a.pvb_pat.ppat_attributes <> []
  then a
  else match a.pvb_pat.ppat_desc with
  | Ppat_construct ({txt = Longident.Lident "()";_}, None) ->
    (match a.pvb_expr with
    | {pexp_desc = Pexp_construct ({txt = Longident.Lident "()";_}, None);
       pexp_attributes = [] ; _  } ->
      {a with pvb_attributes = [Attributes.remove_attrib]}
    | _ -> a)
  | _ -> a

let add_tdl_entries str =
  List.map str ~f:(fun x ->
    match x with
    | {pstr_desc = Pstr_eval({pexp_desc =
                                Pexp_constant(Pconst_integer(s,None));_},l);_}
      when List.exists l ~f:(fun (x,_) -> x.txt == Attributes.tdl_string) ->
      (match Hashtbl.find_all htl_tdl_entries @@ int_of_string s with
      | exception Not_found ->
        error ~loc:x.pstr_loc "fatal: type info not found"
      | x -> List.rev x)
    | x -> [x])
  |> List.flatten

let rec unbox_box_constr e f =
  match e.pexp_desc with
  | Pexp_constraint(e2,c) ->
    let res = unbox_box_constr e2 f in
    {e with pexp_desc = Pexp_constraint(res,c)}
  | _ -> U.with_loc e.pexp_loc @@ fun () -> f e


let external' ~is_inline loc strpri =
  Ast_helper.default_loc := loc;
  let release_runtime_lock = ref false in
  let noalloc = ref false in
  let return_errno = ref false in
  let remove_labels = ref false in
  List.iter strpri.pval_attributes ~f:(fun (s,y) ->
    (match s.txt with (* TODO: what else? *)
    | "release_runtime_lock" -> release_runtime_lock := true;
    | "noalloc" -> noalloc := true;
    | "return_errno" -> return_errno := true;
    | "remove_labels" when is_inline -> remove_labels := true;
    | x -> error ~loc:s.loc "unsupported attribute %s" x);
    if y <> PStr [] then
      error ~loc:s.loc "unknown content in attribute %s" s.txt);
  let release_runtime_lock = !release_runtime_lock in
  let noalloc = !noalloc in
  let return_errno = !return_errno in
  let remove_labels = !remove_labels in
  H.external'
    ~is_inline ~remove_labels ~return_errno ~release_runtime_lock ~noalloc strpri

let mapper _config _cookies =
  let module P = struct
    type t = (* currently the AST is traversed twice.*)
      | Initial_scan (* collect all information *)
      | Replace (* replace expressions with information extracted from C *)
  end in
  let phase = ref P.Initial_scan in
  let is_outer_structure = ref true in

  let name_needed () =
    error
      "new types need a name. Parallel pattern matching is not supported" in

  let structure mapper str =
    if !is_outer_structure = false || str = [] then
      let str =
        if !phase <> P.Replace then str
        else add_tdl_entries str in
      default_mapper.structure mapper str
      |> remove_empty
    else
    let orig_log = (List.hd str).pstr_loc in
    Script_result.clear ();
    Hashtbl.clear htl_tdl_entries;
    Ppx_mod.mod_name := None;
    convert_ctypes_exeptions @@ fun () ->
    is_outer_structure := false;
    let st = default_mapper.structure mapper str in
    Ast_helper.default_loc := orig_log;
    Top.run ();
    let ppx_main = Ppx_mod.modules () in
    let st = ppx_main @ (add_tdl_entries st) in
    phase := P.Replace;
    let st = default_mapper.structure mapper st |> remove_empty in
    C_content.write_file ();
    Script_result.clear ();
    Hashtbl.clear htl_tdl_entries;
    st in

  let structure_item mapper stri =
    Ast_helper.default_loc := stri.pstr_loc;
    match stri with
    | {pstr_desc = Pstr_eval({pexp_desc =
                                Pexp_constant(Pconst_integer(s,None));_},l);_}
      when !phase = P.Replace &&
           List.exists l ~f:(fun (x,_) ->
             x.txt == Attributes.replace_expr_string) ->
      (try Hashtbl.find Script_result.htl_stri (int_of_string s) with
      | Not_found -> error "fatal error: external not found")
    | {pstr_desc = Pstr_extension (({txt="c";_},
                                    (PStr [{pstr_desc = Pstr_primitive strpri
                                           ;pstr_loc; _}])), _);_}
      when !phase = P.Initial_scan && strpri.pval_prim <> [] ->
      external' ~is_inline:true pstr_loc strpri
    | {pstr_desc = Pstr_primitive strpri; pstr_loc}
      when !phase = P.Initial_scan && strpri.pval_prim <> [] ->
      external' ~is_inline:false pstr_loc strpri
    | {pstr_desc = Pstr_extension (({txt = (("c"|"c_union") as txt);_},
                                    (PStr [{pstr_desc = Pstr_type(rf,tl) ;
                                            pstr_loc}])), _);_}
      when !phase = P.Initial_scan ->
      Ast_helper.default_loc := pstr_loc;
      let enforce_union = txt = "c_union" in
      H.type_decl ~enforce_union rf tl

    | [%stri let%c [%p? pat] = [%e? exp]] when !phase = P.Initial_scan ->
      let t = unbox_box_constr exp @@ fun exp ->
        let s,l = match exp.pexp_desc with
        | Pexp_apply({pexp_desc = Pexp_ident {txt = Longident.Lident s;
                                              loc = _};
                      pexp_attributes = []; _},l) -> s,l
        | _ -> "",[] in
        (* TODO: really allow everything as long as a Ctypes.typ is returned? *)
        (*| _ -> error "only function application is allowed in this context"*)
        let name = match Extract.variable_from_pattern pat with
        | None when s = "header" -> ""
        | None -> name_needed () | Some x -> x in
        match s with
        | "header" -> H.header l
        | "constant" -> H.constant_bind name l
        | "enum" -> H.enum name l
        | "@->" -> H.fn name exp
        | _ -> H.typ name exp in
      let vb = Ast_helper.Vb.mk pat t |> mark_empty in
      Ast_helper.Str.value Nonrecursive [vb]
    | {pstr_desc = Pstr_extension (({txt="c";loc},_),_);_} ->
      error ~loc "extension 'c' is not supported here"
    | {pstr_desc = Pstr_value(Nonrecursive,[a]); _}
      when !phase = P.Initial_scan ->
      (match a.pvb_expr.pexp_desc with
      | Pexp_extension(({txt="c";_}), PStr[{pstr_desc =
                                              Pstr_eval(e,[]);_ }]) ->
        let t = unbox_box_constr e @@ fun e ->
          let prefix = Extract.variable_from_pattern a.pvb_pat in
          let s,l = match e.pexp_desc with
          | Pexp_apply({pexp_desc = Pexp_ident {txt = Longident.Lident s;
                                                loc = _};
                        pexp_attributes = []; _},l) -> s,l
          | Pexp_apply _ -> "",[]
          | _ -> error "only function application is allowed in this context" in
          match s with
          | "header" -> H.header l
          | "field" -> H.field ?prefix l
          | "constant" -> H.constant l
          | "seal" -> H.seal l
          | "foreign_value" -> H.foreign_value l
          | "foreign" -> H.foreign ?prefix l
          | _ -> error "invalid function call in [%%c ... ]" in
        let na = mark_empty { a with pvb_expr = t } in
        {stri with pstr_desc = Pstr_value(Nonrecursive,[na])}
      | _ ->  default_mapper.structure_item mapper stri)
    | {pstr_desc = Pstr_module x; pstr_loc; _} when !phase = P.Initial_scan ->
      Scripts_structure.open_module x.pmb_name.txt;
      let stri = default_mapper.structure_item mapper stri in
      Scripts_structure.close_module pstr_loc;
      stri
    | {pstr_desc = Pstr_recmodule l; pstr_loc}
      when !phase = P.Initial_scan ->
      let l' = List.map l ~f:(fun x ->
        Scripts_structure.open_module x.pmb_name.txt;
        let r = default_mapper.module_binding mapper x in
        Scripts_structure.close_module pstr_loc;
        r ) in
      {pstr_desc = Pstr_recmodule l'; pstr_loc}

    | {pstr_desc = Pstr_value(Nonrecursive,
                              [({pvb_expr = ({ pexp_desc = Pexp_ident _; _
                                             } as pexp); _ } as pvb)]
                             ); _}
      when !phase = P.Replace &&
           List.exists pexp.pexp_attributes
             ~f:(fun (x,_) -> x.txt == Attributes.replace_attr_string) ->
      mark_if_used  mapper stri pvb pexp
    | {pstr_desc = Pstr_value(Nonrecursive,
                              [(
                                {pvb_expr = ({ pexp_desc =
                                                 Pexp_constraint(
                                                   ({ pexp_desc =
                                                        Pexp_ident _; _} as pexp),
                                                   ct) ; _
                                             } as pexp_outer); _ } as pvb)]
                             ); _}
      when !phase = P.Replace &&
           List.exists pexp.pexp_attributes
             ~f:(fun (x,_) -> x.txt == Attributes.replace_attr_string) ->
      mark_if_used ~pexp_outer:(pexp_outer,ct) mapper stri pvb pexp
    | _ -> default_mapper.structure_item mapper stri in

  let expr mapper pexp =
    Ast_helper.default_loc := pexp.Parsetree.pexp_loc;
    match pexp.Parsetree.pexp_desc with
    | Pexp_extension(({txt="c";_}), PStr[{pstr_desc = Pstr_eval(e,[]);_ }])
      when !phase = P.Initial_scan ->
      unbox_box_constr e @@ fun e ->
      let s,l = match e.pexp_desc with
      | Pexp_apply({pexp_desc = Pexp_ident {txt = Longident.Lident s;
                                            loc = _};
                    pexp_attributes = []; _},l) -> s,l
      | Pexp_apply _ -> "",[]
      | _ -> error "only function application is allowed in this context" in
      (match s with
      | "constant" -> H.constant l
      | "foreign_value" -> H.foreign_value l
      | "foreign" -> H.foreign l
      | _ -> error "invalid function call in [%%c ... ]")
    | Pexp_extension({txt="c";loc},_) ->
      error ~loc "extension 'c' is not allowed here"
    | Pexp_constant(Pconst_integer(s,None))
      when !phase = P.Replace &&
           List.exists pexp.pexp_attributes
             ~f:(fun (x,_) -> x.txt == Attributes.replace_expr_string) ->
      (try Hashtbl.find Script_result.htl_expr (int_of_string s) with
      | Not_found -> error "fatal: constant not found")
    | Pexp_letmodule(name,mexpr,expr) when !phase = P.Initial_scan ->
      Scripts_structure.open_let_module name.txt;
      let mexpr' =
        let r = default_mapper.module_expr mapper mexpr in
        Scripts_structure.close_let_module_module mexpr.pmod_loc ;
        r in
      let expr' = default_mapper.expr mapper expr in
      Scripts_structure.close_let_module_expr mexpr.pmod_loc;
      {pexp with pexp_desc = Pexp_letmodule(name,mexpr',expr')}
    | _ -> default_mapper.expr mapper pexp in
  {default_mapper with structure_item; structure ; expr}

let init () =
  let () = Ppxc__script._init () in
  Driver.register ~name:"ppx_cstubs" ocaml_version mapper
