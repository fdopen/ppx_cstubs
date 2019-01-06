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
open Ast_mapper
open Parsetree
open Asttypes
open Std

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
    | Ppat_var {txt = "" | "_"; _} -> None
    | Ppat_var s -> Some s.txt
    | Ppat_constraint (s, _) -> variable_from_pattern s
    | _ -> None

  let unsupported_typ loc = error ~loc "type description not supported"

  let check_no_attribs = function
    | [] -> ()
    | (a, _) :: _ -> error ~loc:a.loc "unsupported attribute: %S" a.txt

  let check_no_attribs_t t = check_no_attribs t.ptyp_attributes

  let remove_attrib str l = List.filter l ~f:(fun (x, _) -> x.txt <> str)

  let get_remove name attr =
    let at =
      List.exists attr ~f:(fun (x, t) ->
          if x.txt <> name then false
          else
            match t with
            | PStr [] -> true
            | _ -> error ~loc:x.loc "surprising content in %s" name )
    in
    (at, if not at then attr else remove_attrib name attr)

  let rec is_simple =
    let open Longident in
    function
    | Lident _ -> true | Ldot (t, _) -> is_simple t | Lapply _ -> false

  let rec type_to_ctype ~lookup typ =
    let open Longident in
    match typ.ptyp_desc with
    | Ptyp_constr (({txt; _} as a), []) when is_simple txt ->
      check_no_attribs_t typ ;
      let c =
        match (lookup, txt) with
        | Some l, Lident l' when l = l' -> true
        | _ -> false
      in
      (Ast_helper.Exp.ident ~loc:a.loc a, c)
    | Ptyp_constr ({txt = Lident "ptr"; loc}, [a]) ->
      check_no_attribs_t typ ;
      let p, c = type_to_ctype ~lookup a in
      (([%expr Ctypes.ptr [%e p]] [@metaloc loc]), c)
    | Ptyp_constr ({txt = Lident "ptr_opt"; loc}, [a]) ->
      check_no_attribs_t typ ;
      let p, c = type_to_ctype ~lookup a in
      (([%expr Ctypes.ptr_opt [%e p]] [@metaloc loc]), c)
    | Ptyp_constr ({txt = Lident "static_funptr"; loc}, [a]) ->
      check_no_attribs_t typ ;
      let t, c = type_to_ctype_fn ~lookup a in
      (([%expr Ctypes.static_funptr [%e t]] [@metaloc loc]), c)
    | Ptyp_constr ({txt = Lident (("funptr" | "funptr_opt") as ns); loc}, [a])
    ->
      U.with_loc loc
      @@ fun () ->
      let t, c = type_to_ctype_fn ~lookup a in
      let check_errno, at = get_remove "check_errno" typ.ptyp_attributes in
      let runtime_lock, at = get_remove "release_runtime_lock" at in
      let thread_registration, at = get_remove "thread_registration" at in
      check_no_attribs at ;
      let h = function true -> [%expr true] | false -> [%expr false] in
      let f =
        match ns with
        | "funptr_opt" -> [%expr Foreign.funptr_opt]
        | _ -> [%expr Foreign.funptr]
      in
      ( [%expr
          [%e f] ~check_errno:[%e h check_errno]
            ~runtime_lock:[%e h runtime_lock]
            ~thread_registration:[%e h thread_registration] [%e t]]
      , c )
    | _ -> unsupported_typ typ.ptyp_loc

  and type_to_ctype_fn ~lookup typ =
    let loc = typ.ptyp_loc in
    check_no_attribs_t typ ;
    match typ.ptyp_desc with
    | Ptyp_arrow
        ( Nolabel
        , ({ptyp_desc = Ptyp_constr _; _} as t1)
        , ({ptyp_desc = Ptyp_constr _; _} as t2) ) ->
      let t1, c1 = type_to_ctype ~lookup t1
      and t2, c2 = type_to_ctype ~lookup t2 in
      ( ( [%expr Ctypes.( @-> ) [%e t1] (Ctypes.returning [%e t2])] [@metaloc
                                                                      loc] )
      , c1 || c2 )
    | Ptyp_arrow
        ( Nolabel
        , ({ptyp_desc = Ptyp_constr _; _} as t1)
        , ({ptyp_desc = Ptyp_arrow _; _} as t2) ) ->
      let t1, c1 = type_to_ctype ~lookup t1
      and t2, c2 = type_to_ctype_fn ~lookup t2 in
      (([%expr Ctypes.( @-> ) [%e t1] [%e t2]] [@metaloc loc]), c1 || c2)
    | _ -> error ~loc "unsupported Ctypes.fn definition"

  let rec fun_def is_inline accu typ =
    let loc = typ.ptyp_loc in
    let type_conf is_inline t =
      let is_ocaml_typ, attribs = get_remove "ocaml_type" t.ptyp_attributes in
      if is_ocaml_typ = false || is_inline = false then
        fst @@ type_to_ctype ~lookup:None t
      else
        let () = check_no_attribs attribs in
        let t = {t with ptyp_attributes = attribs} in
        let e = U.marshal_to_str_expr t in
        ([%expr Ctypes.ppxc__private_ocaml_typ [%e e]] [@metaloc loc])
    in
    match typ.ptyp_desc with
    | Ptyp_constr _ ->
      if accu = [] then error ~loc "function expected" ;
      (type_conf is_inline typ, List.rev accu)
    | Ptyp_arrow (l, t1, t2) ->
      check_no_attribs_t typ ;
      let t1' = type_conf is_inline t1 in
      ( match l with
      | Nolabel | Labelled _ -> ()
      | Optional _ ->
        error ~loc:t1.ptyp_loc "optional parameters are not supported" ) ;
      fun_def is_inline ((l, t1') :: accu) t2
    | _ -> unsupported_typ typ.ptyp_loc

  let constant_string e =
    match e.pexp_desc with
    | Pexp_constant (Pconst_string (s, _)) -> Some s
    | _ -> None

  type enum_entry =
    { cdecl : constructor_declaration
    ; enum_cname : string }

  type enum_type =
    | Enum_normal
    | Enum_bitmask
    | Enum_both

  type enum =
    { ename : string
    ; el : enum_entry list
    ; ename_c : string
    ; enum_type : enum_type
    ; etypedef : bool
    ; edecl : type_declaration
    ; eunexpected : Marshal_types.expr option }

  type field =
    { field_name : string
    ; field_expr : Parsetree.expression
    ; field_cname : string
    ; field_loc : Ast_helper.loc }

  type struct_type =
    | Union
    | Struct_normal
    | Struct_record
    | Struct_both

  type structure =
    { sname : string
    ; sl : field list
    ; sname_c : string
    ; stypedef : bool
    ; stype : struct_type
    ; sloc : Ast_helper.loc }

  type ctyp =
    | Enum of enum
    | Struct of structure

  let get_cname ~def l =
    let res =
      List.find_map l ~f:(fun (x, t) ->
          if x.txt <> "cname" then None
          else
            match t with
            | PStr
                [ { pstr_desc =
                      Pstr_eval
                        ( {pexp_desc = Pexp_constant (Pconst_string (x, _)); _}
                        , [] ); _ } ] ->
              Some x
            | _ -> error ~loc:x.loc "unsupported expression in cname" )
    in
    match res with None -> (def, l) | Some x -> (x, remove_attrib "cname" l)

  let get_unexpected l =
    let res =
      List.find_map l ~f:(fun (x, t) ->
          if x.txt <> "unexpected" then None
          else
            match t with
            | PStr
                [ { pstr_desc =
                      Pstr_eval (({pexp_desc = Pexp_fun _; _} as e), []); _ }
                ] ->
              Some e
            | _ -> error ~loc:x.loc "unsupported expression in unexpected" )
    in
    match res with
    | None -> (None, l)
    | Some _ as x -> (x, remove_attrib "unexpected" l)

  let extract_enum_entry = function
    | { pcd_name
      ; pcd_args = Pcstr_tuple []
      ; pcd_res = None
      ; pcd_loc = _
      ; pcd_attributes } as whole ->
      let name = pcd_name.txt in
      let cname, attribs = get_cname ~def:name pcd_attributes in
      check_no_attribs attribs ;
      let cdecl = {whole with pcd_attributes = []} in
      {cdecl; enum_cname = cname}
    | {pcd_loc; _} -> error ~loc:pcd_loc "Unsupported constructor type"

  let extract_field lookup
      {pld_name; pld_mutable; pld_type; pld_loc; pld_attributes} =
    if pld_mutable <> Asttypes.Immutable then
      error ~loc:pld_loc "only immutable is possible" ;
    let field_name = pld_name.txt in
    let field_expr, field_recursive = type_to_ctype ~lookup pld_type in
    if field_recursive then
      error ~loc:pld_loc "recursive record views (%s) are not possible"
        field_name ;
    let field_cname, attribs = get_cname ~def:field_name pld_attributes in
    check_no_attribs attribs ;
    {field_name; field_expr; field_cname; field_loc = pld_loc}

  let type_decl rec' = function
    | { ptype_name
      ; ptype_params = []
      ; ptype_cstrs = []
      ; ptype_private = Public
      ; ptype_manifest = _
      ; ptype_attributes
      ; ptype_kind = Ptype_variant (_ :: _ as l)
      ; ptype_loc } as whole ->
      let ename = ptype_name.txt in
      let ename_c, attribs = get_cname ~def:ename ptype_attributes in
      let etypedef, attribs = get_remove "typedef" attribs in
      let ebitmask, attribs = get_remove "as_bitmask" attribs in
      let eunexpected, attribs = get_unexpected attribs in
      let enum_type =
        match ebitmask with false -> Enum_normal | true -> Enum_bitmask
      in
      let whithmask, attribs = get_remove "with_bitmask" attribs in
      let enum_type =
        match whithmask with
        | false -> enum_type
        | true ->
          if enum_type <> Enum_normal then
            error ~loc:ptype_loc "either as_bitmask or with_bitmask - not both" ;
          Enum_both
      in
      check_no_attribs attribs ;
      let el = List.map l ~f:extract_enum_entry in
      let edecl =
        { whole with
          ptype_kind = Ptype_variant (List.map el ~f:(fun x -> x.cdecl))
        ; ptype_attributes = [] }
      in
      Enum {ename; el; ename_c; etypedef; edecl; enum_type; eunexpected}
    | { ptype_name
      ; ptype_params = []
      ; ptype_cstrs = []
      ; ptype_private = Public
      ; ptype_manifest = None
      ; ptype_attributes
      ; ptype_kind = Ptype_record (_ :: _ as l)
      ; ptype_loc } ->
      let sname = ptype_name.txt in
      let sname_c, attribs = get_cname ~def:sname ptype_attributes in
      let stypedef, attribs = get_remove "typedef" attribs in
      let union, attribs = get_remove "union" attribs in
      let as_record, attribs = get_remove "as_record" attribs in
      let with_record, attribs = get_remove "with_record" attribs in
      check_no_attribs attribs ;
      let stype =
        match union with
        | true ->
          if as_record then
            error "@@@@union and @@@@as_record are mutually exclusive" ;
          if with_record then
            error "@@@@union and @@@@with_record are mutually exclusive" ;
          Union
        | false -> (
          match as_record with
          | true ->
            if with_record then
              error "@@@@as_record and @@@@with_record are mutually exclusive" ;
            Struct_record
          | false -> (
            match with_record with
            | true -> Struct_both
            | false -> Struct_normal ) )
      in
      let lookup =
        if rec' <> Asttypes.Recursive then None
        else
          match stype with
          | Union | Struct_normal -> None
          | Struct_record | Struct_both -> Some sname
      in
      let sl = List.map ~f:(extract_field lookup) l in
      Struct {sname; sl; sname_c; stypedef; stype; sloc = ptype_loc}
    | {ptype_loc; _} -> error ~loc:ptype_loc "unsupported type definition"

  let type_decl rec' l = List.map ~f:(type_decl rec') l
end

module Ppx_mod_top_common (M : sig end) = struct
  type items = Parsetree.structure_item list

  type t =
    | Root of items
    | Module of t * string * items
    | Let_module of t * string * items

  let state = ref (Root [])

  let get_entries () =
    match !state with
    | Root x | Module (_, _, x) | Let_module (_, _, x) -> List.rev x

  let add_entry x =
    match !state with
    | Root el -> state := Root (x :: el)
    | Module (t, s, l) -> state := Module (t, s, x :: l)
    | Let_module (t, s, l) -> state := Let_module (t, s, x :: l)

  let open_module s = state := Module (!state, s, [])

  let open_let_module s =
    let s_out = U.safe_mlname ~prefix:s () |> CCString.capitalize_ascii in
    state := Module (!state, s_out, []) ;
    state := Let_module (!state, s, [])

  let add_module internal s l =
    if l = [] then ()
    else
      let open Ast_helper in
      if Hashtbl.mem Keywords.htl_modules s then
        error "module name %S is reserved" s ;
      if internal = false && CCString.prefix ~pre:"Ppxc__" s then
        error "module prefix Ppxc__ is reserved (%S)" s ;
      let ms = Mod.structure (List.rev l) in
      let mb = Mb.mk (U.mk_loc s) ms in
      let x = Str.module_ mb in
      add_entry x

  let close_let_module_module loc =
    U.with_loc loc
    @@ fun () ->
    match !state with
    | Let_module (t, s, l) ->
      state := t ;
      add_module false s l
    | Module _ | Root _ -> error "ppx_cstubs failed to parse the AST"

  let close_module loc =
    U.with_loc loc
    @@ fun () ->
    match !state with
    | Module (t, s, l) ->
      state := t ;
      add_module false s l
    | Let_module _ | Root _ -> error "ppx_cstubs failed to parse the AST"

  let close_let_module_expr loc =
    U.with_loc loc
    @@ fun () ->
    match !state with
    | Module (t, s, l) ->
      state := t ;
      add_module true s l
    | Let_module _ | Root _ -> error "ppx_cstubs failed to parse the AST"
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
      let fname =
        match CCString.Split.left ~by:"." fname with
        | None -> ""
        | Some (s, _) -> s
      in
      let name =
        Printf.sprintf "Ppxc__private_%s_%x%x" fname
          loc.Lo.loc_start.Le.pos_lnum loc.Lo.loc_end.Le.pos_cnum
        |> Std.Util.safe_ascii_only_ml
      in
      mod_name := Some name ;
      name

  module Structure = Ppx_mod_top_common ()

  open Structure

  let get_mod_path () =
    let rec iter accu = function
      | Root _ -> main_name () :: accu
      | Module (t, s, _) | Let_module (t, s, _) -> iter (s :: accu) t
    in
    iter [] !state

  let create_ref s =
    let open Longident in
    let rec iter = function
      | Root _ -> Lident (main_name ())
      | Module (t, s, _) | Let_module (t, s, _) -> Ldot (iter t, s)
    in
    U.mk_loc @@ Ldot (iter !state, s)

  let create_type_ref ?(params = []) s =
    let r = create_ref s in
    Ast_helper.Typ.constr r params

  let create_ref ?attrs s =
    let r = create_ref s in
    Ast_helper.Exp.ident ?attrs r

  let modules () =
    let entries = get_entries () in
    if entries = [] then []
    else
      let l =
        let l = get_entries () in
        let l =
          match !Script_result.foreign_used with
          | false -> l
          | true -> [%stri open! Foreign [@@ocaml.warning "-33"]] :: l
        in
        let l = [%stri open! Ctypes [@@ocaml.warning "-33"]] :: l in
        let l =
          if !Options.nopervasives then l
          else
            let ov = Ocaml_config.version () in
            if ov >= (4, 8, 0) then
              [%stri open! Stdlib [@@ocaml.warning "-33"]] :: l
            else if ov >= (4, 7, 0) then
              [%stri open! Stdlib.Pervasives [@@ocaml.warning "-33"]] :: l
            else [%stri open! Pervasives [@@ocaml.warning "-33"]] :: l
        in
        let module A = Ast_helper in
        let ms = A.Mod.structure l in
        let mb = A.Mb.mk (U.mk_loc (main_name ())) ms in
        let generated_module = A.Str.module_ mb in
        [generated_module]
      in
      match !Options.mode with
      | Options.Regular -> l
      | Options.Emulate ->
        let fail = Gen_ml.stdlib_fun "failwith" in
        [%stri
          let () =
            [%e fail] "ppx_cstubs.merlin is not intended to generate real code"]
        :: l

  let check_name is_fun name =
    if name = "" || name = "_" then error "variable name empty" ;
    let f = name.[0] in
    if is_fun = false && not ((f >= 'a' && f <= 'z') || f = '_') then
      error "prefix or infix symbols are not allowed here" ;
    if Hashtbl.mem Keywords.htl name then
      error "%S is predefined. You should'nt shadow it here" name ;
    if CCString.prefix ~pre:"ppxc__" name then
      error "the ppxc__ prefix is reserved for generated code" ;
    if CCString.prefix ~pre:"_ppxc__" name then
      error "the _ppxc__ prefix is reserved for generated code" ;
    match name with
    | "=" | "==" ->
      error
        "the operator %S is used inside generated code. You can't overwrite it"
        name
    | _ -> ()

  let add_named ?(name_check = true) ?attrs name expr =
    if name_check then check_name false name ;
    let module Ur = Uniq_ref in
    let mod_path = get_mod_path () in
    let r = Ur.make mod_path name expr in
    add_entry r.Ur.topmod_vb ;
    add_entry r.Ur.topmod_ref ;
    let expr = r.Ur.main_ref in
    let expr =
      match attrs with
      | None -> expr
      | Some l ->
        let pexp_attributes = expr.pexp_attributes @ l in
        {expr with pexp_attributes}
    in
    (r.Ur.id, expr)

  let add_external stri_external stri_expr ~name =
    check_name true name ;
    let module Ur = Uniq_ref in
    let mod_path = get_mod_path () in
    let r = Ur.make mod_path name [%expr ()] in
    add_entry stri_external ;
    add_entry stri_expr ;
    add_entry r.Ur.topmod_ref ;
    (r.Ur.id, r.Ur.main_ref)

  let add_external_anon stri_external stri_expr name =
    add_entry stri_external ;
    add_entry stri_expr ;
    create_ref name

  let add_unit expr =
    add_entry [%stri let () = [%e expr]] ;
    [%expr ()]
end

module C_content = struct
  let write_file () : unit =
    if Options.(!mode = Emulate) then ()
    else
      match (!Options.c_output_file, !Script_result.c_source) with
      | Some fln, Some source -> (
        Options.c_output_file := None ;
        match fln with
        | "-" -> output_string stdout source
        | _ ->
          CCIO.with_out ?mode:None
            ~flags:[Open_creat; Open_trunc; Open_binary]
            fln
          @@ fun ch -> output_string ch source )
      | Some _, None ->
        prerr_endline
          "no c file necessary, set -o-c to 'none' and update your build \
           instructions" ;
        exit 2
      | None, None -> ()
      | None, Some _ -> failwith "internal error: c source not generated"
end

module Id = struct
  open Ast_helper

  type t =
    { id : int
    ; script_param : Parsetree.expression
    ; expr : Parsetree.expression
    ; stri : Parsetree.structure_item }

  let get =
    let cnt = ref 0 in
    fun ?(loc = !Ast_helper.default_loc) () ->
      let id = !cnt in
      incr cnt ;
      let (t : Marshal_types.id_loc_param) = (id, loc) in
      let loc_id_param = Marshal.to_string t [] in
      let script_param = U.str_expr ~loc loc_id_param in
      let attrs = [Attributes.replace_expr_attrib] in
      let expr = U.int_expr ~loc ~attrs id in
      let stri = Str.eval ~attrs expr in
      {id; script_param; expr; stri}

  let get_usage_id attr_string =
    let cnt = ref 0 in
    fun () ->
      let id = !cnt in
      incr cnt ;
      let script_param = U.int_expr id in
      let st = [%stri [%e script_param]] in
      let attrs =
        let x = U.mk_loc attr_string in
        [(x, PStr [st])]
      in
      (script_param, attrs)

  let get_struct_id = get_usage_id Attributes.replace_struct_string

  let get_usage_id = get_usage_id Attributes.replace_attr_string

  let get_tdl_entries_id =
    let cnt = ref 0 in
    fun () ->
      let id = !cnt in
      incr cnt ;
      let expr = U.int_expr id in
      (id, Str.eval ~attrs:[Attributes.tdl_attrib] expr)

  let get_typ_id =
    let cnt = ref 0 in
    fun () ->
      let id = !cnt in
      incr cnt ;
      id
end

module Top = struct
  module Structure_extract = Ppx_mod_top_common ()

  module Structure_build_external = Ppx_mod_top_common ()

  let loc () =
    let loc = U.marshal_to_str_expr !Ast_helper.default_loc in
    [%stri let () = Ppxc__script.set_loc [%e loc]]

  let add_build_external s =
    Structure_build_external.add_entry @@ loc () ;
    Structure_build_external.add_entry s

  let add_extract s =
    Structure_extract.add_entry @@ loc () ;
    Structure_extract.add_entry s

  let run () =
    let c_const_entries = Structure_extract.get_entries () in
    let build_entries = Structure_build_external.get_entries () in
    let ctypes =
      let ws = Ocaml_config.word_size () in
      if ws = Sys.word_size then
        [%stri
          module Ctypes =
            Ppxc__script.Ctypes_make.Ctypes (Ppxc__script.Ctypes_make.Info)]
      else if ws = 32 then
        [%stri
          module Ctypes =
            Ppxc__script.Ctypes_make.Ctypes (Ppxc__script.Ctypes_make.Info32)]
      else
        [%stri
          module Ctypes =
            Ppxc__script.Ctypes_make.Ctypes (Ppxc__script.Ctypes_make.Info64)]
    in
    let expr =
      [%expr
        let module M : sig end = struct
          open! Ppxc__script.Run_environment [@@ocaml.warning "-33"]

          [%%s
          [ctypes]]

          open! Ctypes [@@ocaml.warning "-33"]

          module Extract_c_consts : sig end = struct
            [%%s c_const_entries]
          end

          module Build_externals : sig end = struct
            [%%s build_entries]
          end
        end in
        (Ppxc__script.Main.run () : unit)]
    in
    Toplevel.init () ;
    ignore (Toplevel.eval_expression expr : Obj.t)
end

module Scripts_structure = struct
  (* Script structure and Top module structure might differ in the future. Not
     yet sure what to do about functors and similar constructs *)
  let open_module s =
    Top.Structure_build_external.open_module s ;
    Top.Structure_extract.open_module s ;
    Ppx_mod.Structure.open_module s

  let open_let_module s =
    Top.Structure_build_external.open_let_module s ;
    Top.Structure_extract.open_let_module s ;
    Ppx_mod.Structure.open_let_module s

  let close_let_module_module loc =
    Top.Structure_build_external.close_let_module_module loc ;
    Top.Structure_extract.close_let_module_module loc ;
    Ppx_mod.Structure.close_let_module_module loc

  let close_let_module_expr loc =
    Top.Structure_build_external.close_let_module_expr loc ;
    Top.Structure_extract.close_let_module_expr loc ;
    Ppx_mod.Structure.close_let_module_expr loc

  let close_module loc =
    Top.Structure_build_external.close_module loc ;
    Top.Structure_extract.close_module loc ;
    Ppx_mod.Structure.close_module loc
end

let htl_tdl_entries = Hashtbl.create 32

module H = struct
  open Ast_helper

  let error_msg s = function
    | [] -> error "arguments missing for %s" s
    | _ -> error "too many or too few arguments for %s" s

  let constant_common name = function
    | [(Nolabel, str_expr); (Nolabel, type_expr)] -> (
      let id = Id.get () in
      let pat = match name with None -> [%pat? ()] | Some x -> U.mk_pat x in
      let script =
        [%stri
          let [%p pat] =
            let (ppxc__s : string) = [%e str_expr] in
            let (ppxc__t : _ Ctypes.typ) = [%e type_expr] in
            Ppxc__script.Extract.constant [%e id.Id.script_param] ppxc__s
              ppxc__t]
      in
      Top.add_extract script ;
      match name with
      | None -> id.Id.expr
      | Some name ->
        let script =
          [%stri
            let [%p pat] =
              Ppxc__script.Build.constant [%e id.Id.script_param] [%e str_expr]
                [%e type_expr]]
        in
        Top.add_build_external script ;
        Ppx_mod.add_named name id.Id.expr |> snd )
    | l -> error_msg "constant" l

  let constant_bind name l = constant_common (Some name) l

  let constant l = constant_common None l

  let marshal_expr (e : Marshal_types.expr) = U.marshal_to_str_expr e

  let register_fun id =
    let script =
      [%stri
        let () =
          Ppxc__script.Extract.register_fun_place [%e id.Id.script_param]]
    in
    Top.add_extract script

  let foreign_value = function
    | [(Nolabel, str_expr); (Nolabel, typ_expr)] ->
      let prefix = Extract.constant_string str_expr in
      let name = U.safe_mlname ?prefix () in
      let name_expr = U.str_expr name in
      let id_stri_external = Id.get () in
      let id_stri_expr = Id.get () in
      register_fun id_stri_external ;
      let mp = Ppx_mod.get_mod_path () |> U.marshal_to_str_expr in
      let texpr = marshal_expr typ_expr in
      let script =
        [%stri
          let () =
            let (ppxc__s : string) = [%e str_expr] in
            let (ppxc__t : _ Ctypes.typ) = [%e typ_expr] in
            Ppxc__script.Build.foreign_value
              [%e id_stri_external.Id.script_param]
              [%e id_stri_expr.Id.script_param] [%e mp] ppxc__t ppxc__s
              [%e name_expr] [%e texpr]]
      in
      Top.add_build_external script ;
      Ppx_mod.add_external_anon id_stri_external.Id.stri id_stri_expr.Id.stri
        name
    | l -> error_msg "foreign_value" l

  let header = function
    | [(Nolabel, x)] ->
      ( match Extract.constant_string x with
      | Some _ -> ()
      | None -> (
        match x.pexp_desc with
        | Pexp_ident _ -> ()
        | _ -> error "'header' requires a string constant" ) ) ;
      let script =
        [%stri
          let () =
            let ppxc__1 : string = [%e x] in
            Ppxc__script.Extract.header ppxc__1]
      in
      Top.add_extract script ;
      [%expr ()]
    | l -> error_msg "header" l

  let field ?name ?prefix ~structure ~str_expr type_expr =
    let id = Id.get () in
    let n = match name with None -> U.safe_mlname ?prefix () | Some s -> s in
    let p = U.mk_pat n in
    let f func =
      [%stri
        let [%p p] =
          let (ppxc__st : _ Ctypes.structured Ctypes.typ) = [%e structure] in
          let (ppxc__s : string) = [%e str_expr] in
          let (ppxc__t : _ Ctypes.typ) = [%e type_expr] in
          let (ppxc__res : _ Ctypes.field) =
            Ctypes.ppxc__private_field ppxc__st ppxc__s ppxc__t
          in
          let () =
            [%e func] [%e id.Id.script_param] ppxc__st ppxc__s ppxc__t
          in
          ppxc__res]
    in
    Top.add_extract @@ f [%expr Ppxc__script.Extract.field] ;
    Top.add_build_external @@ f [%expr Ppxc__script.Build.field] ;
    let nexpr =
      [%expr
        Ppx_cstubs_internals.add_field [%e structure] [%e str_expr]
          [%e id.Id.expr] [%e type_expr]]
    in
    let res = Ppx_mod.add_named ~name_check:(name <> None) n nexpr in
    (n, snd res)

  let seal = function
    | [(Nolabel, seal_struct)] ->
      let id_size = Id.get () in
      let id_align = Id.get () in
      let script =
        [%stri
          let () =
            let (ppxc__s : _ Ctypes.typ) = [%e seal_struct] in
            let () = Ctypes.ppxc__private_seal ppxc__s in
            Ppxc__script.Extract.seal [%e id_size.Id.script_param]
              [%e id_align.Id.script_param] ppxc__s]
      in
      Top.add_extract script ;
      let script =
        [%stri let () = Ctypes.ppxc__private_seal [%e seal_struct]]
      in
      Top.add_build_external script ;
      let nexpr =
        [%expr
          Ppx_cstubs_internals.seal [%e seal_struct] ~size:[%e id_size.Id.expr]
            ~align:[%e id_align.Id.expr]]
      in
      Ppx_mod.add_unit nexpr
    | l -> error_msg "seal" l

  let is_ocaml_operator = function
    | "mod" | "or" | "land" | "lor" | "lxor" | "lsl" | "lsr" | "asr" -> true
    | "" -> false
    | c -> (
      match c.[0] with
      | '!' | '#' | '$' | '%' | '&' | '*' | '+' | '-' | '/' | '<' | '=' | '>'
       |'?' | '@' | '^' | '|' | '~' -> true
      | _ -> false )

  let rec build_ctypes_fn params ret =
    match params with
    | [] -> ( [%expr Ctypes.returning [%e ret]] [@metaloc ret.pexp_loc] )
    | hd :: tl ->
      let e = build_ctypes_fn tl ret in
      ([%expr Ctypes.( @-> ) [%e hd] [%e e]] [@metaloc hd.pexp_loc])

  let add_stri_to_all script =
    Top.add_extract script ;
    Top.add_build_external script ;
    Ppx_mod.Structure.add_entry script

  let external' ~is_inline ~remove_labels ~return_errno ~release_runtime_lock
      ~noalloc ~attrs v =
    let name = v.pval_name.txt in
    let c_name =
      match v.pval_prim with
      | [""] | ["_"] -> error ~loc:v.pval_loc "function name missing"
      | [a] -> a
      | _ -> error ~loc:v.pval_loc "too many c functions referenced"
    in
    if is_inline = false && U.safe_ascii_only c_name <> c_name then
      U.error "invalid identifier for c function:%S" c_name ;
    let remove_labels =
      remove_labels || (is_inline && is_ocaml_operator name)
    in
    if is_inline = false && remove_labels then
      error ~loc:v.pval_loc "remove_labels only supported for inline code" ;
    let ret, el = Extract.fun_def is_inline [] v.pval_type in
    let fun_expr = build_ctypes_fn (List.map ~f:snd el) ret in
    let id_stri_expr = Id.get () in
    let id_stri_ext = Id.get () in
    let uniq_ref_id, fres =
      Ppx_mod.add_external id_stri_ext.Id.stri id_stri_expr.Id.stri ~name
    in
    let vb = Vb.mk ~attrs (U.mk_pat name) fres in
    let fres = Str.value Asttypes.Nonrecursive [vb] in
    let marshal_info =
      let open Marshal_types in
      let s =
        { el
        ; ret
        ; release_runtime_lock
        ; noalloc
        ; return_errno
        ; is_inline
        ; remove_labels
        ; c_name
        ; prim_name = name
        ; uniq_ref_id
        ; mod_path = Ppx_mod.get_mod_path () }
      in
      U.marshal_to_str_expr s
    in
    if is_inline then (* order matters *)
      register_fun id_stri_expr ;
    register_fun id_stri_ext ;
    let script =
      [%stri
        let () =
          let (ppxc__fn : _ Ctypes.fn) = [%e fun_expr] in
          Ppxc__script.Build.external' [%e id_stri_ext.Id.script_param]
            [%e id_stri_expr.Id.script_param] ppxc__fn
            ~marshal_info:[%e marshal_info]]
    in
    Top.add_build_external script ;
    (* create a dummy function that can be referenced in Ctypes.view, etc.
       Constraint necessary to avoid warning 21 (unsound type, never returns) *)
    let script = U.mk_typc ~l:[Typ.var "b"] "Ctypes.typ" in
    let script = Exp.constraint_ ret script in
    let script =
      [%expr
        [%e Gen_ml.stdlib_fun "ignore"] [%e script] ;
        Ctypes.ppxc__unavailable [%e U.str_expr name]]
    in
    let script =
      ListLabels.fold_right ~init:script el ~f:(fun (l, _) ac ->
          let nac = Exp.fun_ l None (Pat.any ()) ac in
          if ac == script then
            Exp.constraint_ nac @@ Typ.arrow l (Typ.var "a") (Typ.var "b")
          else nac )
    in
    let script = [%stri let [%p U.mk_pat name] = [%e script]] in
    Top.add_extract script ;
    Top.add_build_external script ;
    fres

  module E = struct
    let get_typedef_expr l =
      let t =
        List.find_map l ~f:(function
          | (Optional "typedef", _ | Labelled "typedef", _) as x -> Some x
          | _ -> None )
      in
      match t with
      | Some s -> s
      | None ->
        let n_unlabelled =
          List.fold_left ~init:0 l ~f:(fun ac -> function
            | Nolabel, _ -> succ ac | (Optional _ | Labelled _), _ -> ac )
        in
        if n_unlabelled > 1 then error "too many parameters for enum" ;
        (Labelled "typedef", [%expr false])

    let find_first_unlabelled l =
      List.find_mapi l ~f:(fun i el ->
          match el with Nolabel, _ -> Some (i, el) | _ -> None )

    let filter_pos_and_rev pos l =
      List.fold_left ~init:(0, []) l ~f:(fun (i, ac) el ->
          if i = pos then (succ i, ac) else (succ i, el :: ac) )
      |> snd
  end

  let enum binding_name l =
    let open Asttypes in
    let open E in
    (* remove enum name from param list *)
    let pos, enum_name_expr =
      match find_first_unlabelled l with
      | None -> error "enum definitions need a constant string"
      | Some s -> s
    in
    let l = filter_pos_and_rev pos l |> List.rev in
    let l_typedef_expr = get_typedef_expr l in
    let id = Id.get () in
    let pat = U.mk_pat binding_name in
    let fun' =
      Exp.apply [%expr Ppxc__script.Extract.enum]
        [(Nolabel, id.Id.script_param); l_typedef_expr; enum_name_expr]
    in
    let stri = [%stri let ([%p pat] : _ Ctypes.typ) = [%e fun']] in
    Top.add_extract stri ;
    let id_expr, attrs = Id.get_usage_id () in
    let fun' = [%expr Ppxc__script.Build.enum [%e id.Id.script_param]] in
    let fun' = Exp.apply fun' (enum_name_expr :: l) in
    let stri =
      [%stri
        let ([%p pat] : _ Ctypes.typ) =
          let ppxc__t : _ Ctypes.typ = [%e fun'] in
          Ppxc__script.Build.reg_trace [%e id_expr] ppxc__t]
    in
    Top.add_build_external stri ;
    let l = enum_name_expr :: (Asttypes.Nolabel, id.Id.expr) :: l in
    let expr = Exp.apply [%expr Cstubs_internals.build_enum_type] l in
    snd @@ Ppx_mod.add_named ~attrs binding_name expr

  let foreign ?prefix l =
    let typ_expr =
      match List.rev l |> E.find_first_unlabelled with
      | None -> error "function signature in foreign not found"
      | Some (_, l) -> snd l
    in
    let typ_expr = marshal_expr typ_expr in
    let ocaml_name =
      let t =
        List.find_map l ~f:(function
          | Asttypes.Nolabel, e -> Extract.constant_string e
          | _ -> None )
      in
      match t with
      | None -> U.safe_mlname ?prefix ()
      | Some s -> U.safe_mlname ~prefix:s ()
    in
    let id_stri_external = Id.get () in
    let id_stri_expr = Id.get () in
    register_fun id_stri_external ;
    let mp = Ppx_mod.get_mod_path () |> U.marshal_to_str_expr in
    let f_expr =
      [%expr
        Ppxc__script.Build.foreign [%e id_stri_external.Id.script_param]
          [%e id_stri_expr.Id.script_param] [%e mp]
          ~ocaml_name:[%e U.str_expr ocaml_name] ~typ_expr:[%e typ_expr]]
    in
    let call = Exp.apply f_expr l in
    let script = [%stri let () = [%e call]] in
    Top.add_build_external script ;
    Ppx_mod.add_external_anon id_stri_external.Id.stri id_stri_expr.Id.stri
      ocaml_name

  let fn binding_name expr =
    let pat = U.mk_pat binding_name in
    let script = [%stri let ([%p pat] : _ Ctypes.fn) = [%e expr]] in
    Top.add_extract script ;
    let id_expr, attrs = Id.get_usage_id () in
    let script =
      [%stri
        let [%p pat] = Ppxc__script.Build.reg_trace_fn [%e id_expr] [%e expr]]
    in
    Top.add_build_external script ;
    snd @@ Ppx_mod.add_named ~attrs binding_name expr

  let add_to_created_types =
    let open Created_types in
    fun binding_name uid t ->
      let t =
        match t with
        | `Custom -> Custom {cust_id = uid}
        | `View_structured (`Typ_id vs_type_id) ->
          View_structured {vs_id = uid; vs_type_id; vs_state = Vs_Unknown}
        | `Struct (`Typ_id s_type_id, `Is_union s_is_union) ->
          Structured {s_id = uid; s_type_id; s_is_union}
        | `Typedef (`Typ_id s_type_id, `Is_union s_is_union) ->
          View_typedef_structured {s_id = uid; s_type_id; s_is_union}
        | `Enum (`Typ_id ve_type_id, `Is_list ve_is_list) ->
          View_enum {ve_id = uid; ve_type_id; ve_is_list}
      in
      let str_expr = U.marshal_to_str_expr t in
      let name_expr = U.mk_ident binding_name in
      let script =
        [%stri
          let () = Ppxc__script.Build.add_type_ref [%e name_expr] [%e str_expr]]
      in
      Top.add_build_external script

  let add_ctyp ?(add_typ_constraint = true) ?(name_check = true) ~ct bname expr
      =
    let pat = U.mk_pat bname in
    let script = [%stri let ([%p pat] : _ Ctypes.typ) = [%e expr]] in
    Top.add_extract script ;
    let typ_id, expr_constrained =
      match add_typ_constraint with
      | false -> (None, expr)
      | true ->
        let id = U.int_expr (Id.get_typ_id ()) in
        let attrs =
          [ ( U.mk_loc Attributes.replace_typ_string
            , Parsetree.PStr [[%stri [%e id]]] ) ]
        in
        let t = Typ.any ~attrs () in
        (Some id, Exp.constraint_ expr t)
    in
    let id_expr, attrs = Id.get_usage_id () in
    let script =
      match typ_id with
      | None ->
        [%stri
          let [%p pat] = Ppxc__script.Build.reg_trace [%e id_expr] [%e expr]]
      | Some x ->
        let sl = Ppx_mod.get_mod_path () |> U.marshal_to_str_expr in
        [%stri
          let [%p pat] =
            Ppxc__script.Build.derive_typ [%e x] [%e expr] [%e sl]
            |> Ppxc__script.Build.reg_trace [%e id_expr]]
    in
    Top.add_build_external script ;
    let ((uid, _) as r) =
      Ppx_mod.add_named ~name_check ~attrs bname expr_constrained
    in
    add_to_created_types bname uid ct ;
    r

  let type_decl ~enforce_union ~enforce_bitmask type_rec_flag = function
    | [] -> error "empty type definition"
    | tl ->
      let open Extract in
      let record_name s = s ^ "_record" in
      let bitmask_name s = s ^ "_bitmask" in
      let tdl_entry_id, tdl_entry = Id.get_tdl_entries_id () in
      let private_pref =
        if type_rec_flag = Asttypes.Recursive then Std.identity
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
        | Enum {ename; enum_type; _} ->
          add ename ;
          if enum_type = Enum_both then add @@ bitmask_name ename
        | Struct x ->
          add x.sname ;
          if x.stype = Struct_both then add @@ record_name x.sname
      in
      let add_type ?tdl_attrs ?(tdl = true) ?params typ' name =
        let t = Str.type_ Asttypes.Recursive [typ'] in
        add_stri_to_all t ;
        ( if tdl then
          let t' =
            let params =
              match params with
              | None -> None
              | Some x -> Some (List.map ~f:fst x)
            in
            let name = typ'.ptype_name.txt in
            let constr = Ppx_mod.create_type_ref ?params name in
            let typ' = {typ' with ptype_manifest = Some constr} in
            Str.type_ Asttypes.Recursive [typ']
          in
          Hashtbl.add htl_tdl_entries tdl_entry_id t' ) ;
        let mp = Ppx_mod.get_mod_path () in
        let t, r = Uniq_ref.make_type_alias ?tdl_attrs ?params mp name in
        Ppx_mod.Structure.add_entry t ;
        r
      in
      let fields {sname; sl; stype; _} =
        let orig_name = sname in
        let alias_name = private_pref orig_name in
        let struct_expr = U.mk_ident alias_name in
        let fnames =
          List.map sl ~f:(function
              | {field_name; field_expr; field_cname; field_loc} ->
              U.with_loc field_loc
              @@ fun () ->
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
              ( match stype with
              | Struct_record -> ()
              | Union | Struct_normal | Struct_both ->
                let x = U.named_stri field_name e in
                Hashtbl.add htl_tdl_entries tdl_entry_id x ) ;
              n )
        in
        ignore (seal [(Nolabel, struct_expr)] : Parsetree.expression) ;
        let create_view =
          match stype with
          | Union | Struct_normal -> false
          | Struct_record | Struct_both -> true
        in
        if create_view = false then ()
        else
          (* Generate the view for the type *)
          let alias_name, orig_name =
            if stype <> Struct_both then (alias_name, orig_name)
            else (record_name alias_name, record_name orig_name)
          in
          let struct_id, attrs = Id.get_struct_id () in
          let type_ref =
            let params =
              List.mapi sl ~f:(fun i f ->
                  let loc = f.field_loc in
                  let s =
                    if i > 26 then "z" ^ string_of_int i
                    else String.init 1 (fun _ -> Char.chr (97 + i))
                  in
                  let t = Typ.mk ~loc (Ptyp_var s) in
                  let a = (t, Asttypes.Invariant) in
                  let n = Location.mkloc f.field_name loc in
                  let b = Type.field n t in
                  (a, b) )
            in
            let params, fields = List.split params in
            let n = U.mk_loc orig_name in
            let typ' = Type.mk ~attrs ~params ~kind:(Ptype_record fields) n in
            add_type ~tdl_attrs:attrs ~params typ' orig_name
          in
          (* create the view *)
          let view =
            let init = [%expr ppxc__res] in
            let param = U.mk_ident "ppxc__param" in
            let expr =
              List.fold_left2 ~init sl fnames ~f:(fun ac el fname ->
                  let fi = U.mk_lid el.field_name in
                  let v = Exp.field param fi in
                  let f = U.mk_ident fname in
                  [%expr
                    let () = Ctypes.setf ppxc__res [%e f] [%e v] in
                    [%e ac]] )
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
                     (l, e) ))
                None
            in
            let expr =
              List.fold_left2 ~init sl fnames ~f:(fun ac el fname ->
                  let p = U.mk_pat el.field_name in
                  let f = U.mk_ident fname in
                  [%expr
                    let [%p p] = Ctypes.getf ppxc__param [%e f] in
                    [%e ac]] )
            in
            let read = [%expr fun ppxc__param -> [%e expr]] in
            [%expr
              Ctypes.view ~read:[%e read] ~write:[%e write] [%e struct_expr]]
          in
          let ct = `View_structured (`Typ_id type_ref) in
          let _, expr = add_ctyp ~ct alias_name view in
          let () =
            let open Marshal_types in
            let params =
              U.marshal_to_str_expr
                { sr_mod_path = Ppx_mod.get_mod_path ()
                ; sr_type_name = orig_name
                ; sr_field_names = List.map sl ~f:(fun s -> s.field_name)
                ; sr_locs = List.map sl ~f:(fun s -> s.field_loc) }
            in
            let script =
              [%stri
                let () =
                  Ppxc__script.Build.create_record [%e struct_id] [%e params]
                    [%e U.mk_ident alias_name]]
            in
            Top.add_build_external script
          in
          let expr =
            let x = U.mk_typc ~attrs ~l:[Typ.any ()] orig_name in
            let x = U.mk_typc ~l:[x] "Ctypes.typ" in
            Exp.constraint_ expr x
          in
          U.named_stri orig_name expr
          |> Hashtbl.add htl_tdl_entries tdl_entry_id
      in
      let single_typ = function
        | Enum {ename; el; ename_c; etypedef; edecl; enum_type; eunexpected} ->
          U.with_loc edecl.ptype_loc
          @@ fun () ->
          let orig_name_bitmask = bitmask_name ename in
          let alias_name_bitmask = private_pref orig_name_bitmask in
          let orig_name = ename in
          let alias_name = private_pref orig_name in
          let type_ref = add_type edecl ename in
          let exp_l, enum_l =
            let init = ([%expr []], []) in
            ListLabels.fold_right el ~init ~f:(fun cur (l1, l2) ->
                let loc = cur.cdecl.pcd_loc in
                U.with_loc loc
                @@ fun () ->
                let open Marshal_types in
                let c =
                  { ee_signed_id = (Id.get ~loc ()).Id.id
                  ; ee_unsigned_id = (Id.get ~loc ()).Id.id
                  ; ee_loc = loc
                  ; ee_cname = cur.enum_cname
                  ; ee_expr =
                      Exp.construct (U.mk_lid cur.cdecl.pcd_name.txt) None }
                in
                let tup = Exp.tuple [c.ee_expr; l1] in
                (Exp.construct (U.mk_lid "::") (Some tup), c :: l2) )
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
              ( Some id1
              , Some id2
              , Marshal_types.E_normal_bitmask (id1.Id.id, id2.Id.id) )
          in
          let sparam =
            let enum_unexpected =
              match eunexpected with
              | None -> [%expr None]
              | Some e -> [%expr Some [%e e]]
            in
            let open Marshal_types in
            U.marshal_to_str_expr
              { enum_l
              ; enum_is_typedef = etypedef
              ; enum_type_id
              ; enum_unexpected
              ; enum_loc = edecl.ptype_name.loc
              ; enum_name = ename_c }
          in
          let pat =
            let p1 =
              match enum_type with
              | Enum_normal | Enum_both -> U.mk_pat alias_name
              | Enum_bitmask -> [%pat? _]
            in
            let p2 =
              match enum_type with
              | Enum_normal -> [%pat? _]
              | Enum_bitmask -> U.mk_pat alias_name
              | Enum_both -> U.mk_pat alias_name_bitmask
            in
            [%pat? [%p p1], [%p p2]]
          in
          let script =
            [%stri
              let ([%p pat] : 'a Ctypes.typ * 'a list Ctypes.typ) =
                Ppxc__script.Extract.enum2 [%e exp_l] [%e sparam]]
          in
          Top.add_extract script ;
          let id_expr, attrs = Id.get_usage_id () in
          let script =
            [%stri
              let [%p pat] =
                let (ppxc__1, ppxc__2) : 'a Ctypes.typ * 'a list Ctypes.typ =
                  Ppxc__script.Build.enum2 [%e exp_l] [%e sparam]
                in
                ( Ppxc__script.Build.reg_trace [%e id_expr] ppxc__1
                , Ppxc__script.Build.reg_trace [%e id_expr] ppxc__2 )]
          in
          Top.add_build_external script ;
          let f ~is_list ?(bitmask_name = false) name = function
            | None -> ()
            | Some id ->
              let constr = U.mk_typc orig_name in
              let constr =
                match is_list with
                | false ->
                  constr (* FIXME: how to access list and avoid shadowing? *)
                | true -> U.mk_typc ~l:[constr] "list"
              in
              let constr = U.mk_typc ~l:[constr] "Ctypes.typ" in
              let ref', s2 = Ppx_mod.add_named ~attrs name id.Id.expr in
              add_to_created_types name ref'
                (`Enum (`Typ_id type_ref, `Is_list is_list)) ;
              let s2 = Exp.constraint_ s2 constr in
              let name =
                match bitmask_name with
                | false -> orig_name
                | true -> orig_name_bitmask
              in
              U.named_stri name s2 |> Hashtbl.add htl_tdl_entries tdl_entry_id
          in
          ( match enum_type with
          | Enum_normal -> f ~is_list:false alias_name id
          | Enum_bitmask -> f ~is_list:true alias_name id_bitmask
          | Enum_both ->
            f ~is_list:false alias_name id ;
            f ~is_list:true ~bitmask_name:true alias_name_bitmask id_bitmask ) ;
          None
        | Struct ({sname; sl = _; sname_c; stypedef; stype; sloc} as swhole) ->
          U.with_loc sloc
          @@ fun () ->
          let orig_name = sname in
          let alias_name = private_pref orig_name in
          let type_name =
            match stype with
            | Union | Struct_normal | Struct_both -> orig_name
            | Struct_record -> "ppxc__" ^ orig_name
          in
          let t = Type.mk ~kind:Ptype_abstract (U.mk_loc type_name) in
          let type_ref = add_type ~tdl:(stype <> Struct_record) t type_name in
          let constr expr =
            let s =
              if stype = Union then "Ctypes.union" else "Ctypes.structure"
            in
            let x = U.mk_typc type_name in
            let x = U.mk_typc ~l:[x] s in
            let x = U.mk_typc ~l:[x] "Ctypes.typ" in
            Exp.constraint_ expr x
          in
          let expr =
            let name = if stypedef then "" else sname_c in
            let sexpr = U.str_expr name in
            match stype = Union with
            | true -> [%expr Ctypes.union [%e sexpr]]
            | false -> [%expr Ctypes.structure [%e sexpr]]
          in
          let expr = constr expr in
          let ct = (`Typ_id type_ref, `Is_union (stype = Union)) in
          let add ct expr =
            if stype <> Struct_record then
              let _, expr =
                add_ctyp ~add_typ_constraint:false ~ct alias_name expr
              in
              let expr = U.named_stri orig_name (constr expr) in
              Hashtbl.add htl_tdl_entries tdl_entry_id expr
            else
              let str = [%stri let [%p U.mk_pat alias_name] = [%e expr]] in
              add_stri_to_all str
          in
          ( match stypedef with
          | false -> add (`Struct ct) expr
          | true ->
            let expr =
              [%expr Ctypes.typedef [%e expr] [%e U.str_expr sname_c]]
            in
            add (`Typedef ct) expr ) ;
          Some swhole
      in
      let tl = Extract.type_decl type_rec_flag tl in
      if
        enforce_union
        && List.for_all tl ~f:(function Enum _ -> true | Struct _ -> false)
      then error "enum entry marked as union" ;
      if
        enforce_bitmask
        && List.for_all tl ~f:(function Enum _ -> false | Struct _ -> true)
      then error "struct entry marked as bitmask" ;
      let check_type s =
        if Hashtbl.mem Keywords.htl_types s then
          error "type name %s is reserverd, choose another name" s
      in
      let names =
        List.fold_left ~init:[] tl ~f:(fun ac -> function
          | Struct {sname = s; sl; _} ->
            check_type s ;
            List.fold_left ~init:(s :: ac) sl ~f:(fun ac el ->
                el.field_name :: ac )
          | Enum {ename = s; _} ->
            check_type s ;
            s :: ac )
      in
      let names' = CCList.uniq ~eq:CCString.equal names in
      if List.length names <> List.length names' then
        error "names of enumarations, structures and fields must be unique" ;
      let tl =
        List.map tl ~f:(function
          | Struct s when enforce_union ->
            let stype =
              match s.stype with
              | Struct_normal | Union -> Union
              | Struct_both ->
                error
                  "@@@@with_record and type%%c_union are mutually exclusive"
              | Struct_record ->
                error "@@@@as_record and type%%c_union are mutually exclusive"
            in
            Struct {s with stype}
          | Enum e ->
            let res =
              match e.enum_type with
              | _ when enforce_bitmask = false -> e
              | Enum_both ->
                error
                  "@@@@with_bitmask and type%%c_bitmask f = ... are \
                   incompatible"
              | Enum_normal ->
                if e.etypedef then
                  error
                    "@@@@typedef and type%%c_bitmask f = ... are incompatible" ;
                {e with enum_type = Enum_bitmask; etypedef = true}
              | Enum_bitmask ->
                error
                  "@@@@as_bitmask and type%%c_bitmask f = ... are redundant"
            in
            ( match (res.enum_type, res.eunexpected) with
            | Enum_bitmask, Some _ ->
              error "@@@@unexpected not supported for bitmasks"
            | (Enum_both | Enum_normal | Enum_bitmask), (Some _ | None) -> ()
            ) ;
            Enum res
          | Struct _ as x -> x )
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
            (cns, cnr) )
      in
      if
        cnt_structs > 1
        && cnt_records >= 1
        && type_rec_flag = Asttypes.Recursive
      then error "mutually recursive records are not supported" ;
      let refs = List.filter_map ~f:single_typ tl in
      List.iter ~f:fields refs ;
      if type_rec_flag <> Asttypes.Recursive then List.iter ~f:unhide tl ;
      tdl_entry

  let pexp_const name expr =
    let script = [%stri let [%p U.mk_pat name] = [%e expr]] in
    Top.add_extract script ;
    Top.add_build_external script ;
    snd @@ Ppx_mod.add_named name expr

  let field ?prefix = function
    | [(Nolabel, structure); (Nolabel, str_expr); (Nolabel, type_expr)] ->
      snd (field ?prefix ~structure ~str_expr type_expr)
    | l -> error_msg "field" l

  let typ name expr = snd @@ add_ctyp ~ct:`Custom name expr
end

let convert_ctypes_exeptions f =
  try f () with
  | Ctypes_static.ModifyingSealedType s -> error "%s is already sealed" s
  | Ctypes_static.Unsupported s -> error "ctypes error: %s" s
  | Ctypes_static.IncompleteType -> error "Incomplete Type"

let mark_if_used ?pexp_outer mapper stri pvb pexp =
  let _, pl =
    List.find pexp.pexp_attributes ~f:(fun (x, _) ->
        x.txt == Attributes.replace_attr_string )
  in
  let attrib =
    List.filter pexp.pexp_attributes ~f:(fun (x, _) ->
        x.txt != Attributes.replace_attr_string )
  in
  let pexp' = {pexp with pexp_attributes = attrib} in
  let pexp' =
    match pexp_outer with
    | None -> pexp'
    | Some (pexp_outer, ct) ->
      {pexp_outer with pexp_desc = Pexp_constraint (pexp', ct)}
  in
  let id =
    match pl with
    | PStr
        [ { pstr_desc =
              Pstr_eval
                ({pexp_desc = Pexp_constant (Pconst_integer (x, None)); _}, []); _
          } ] ->
      int_of_string x
    | _ -> error "attribute %S is reserved" Attributes.replace_attr_string
  in
  let used = Hashtbl.mem Script_result.htl_used id in
  let nattrib =
    if (not used) || Ocaml_config.version () < (4, 6, 0) then
      pvb.pvb_attributes
    else (U.mk_loc "ocaml.warning", PStr [[%stri "-32"]]) :: pvb.pvb_attributes
  in
  let pvb' = {pvb with pvb_expr = pexp'; pvb_attributes = nattrib} in
  let stri' = {stri with pstr_desc = Pstr_value (Nonrecursive, [pvb'])} in
  let stri' =
    match used with true -> U.no_warn_unused_pre406 stri' | false -> stri'
  in
  default_mapper.structure_item mapper stri'

let remove_empty str =
  List.filter str ~f:(function
    | { pstr_desc =
          Pstr_value (Nonrecursive, [{pvb_attributes = [({txt; _}, _)]; _}]); _
      }
      when txt == Attributes.remove_string -> false
    | _ -> true )

let mark_empty a =
  if a.pvb_attributes <> [] || a.pvb_pat.ppat_attributes <> [] then a
  else
    match a.pvb_pat.ppat_desc with
    | Ppat_construct ({txt = Longident.Lident "()"; _}, None) -> (
      match a.pvb_expr with
      | { pexp_desc = Pexp_construct ({txt = Longident.Lident "()"; _}, None)
        ; pexp_attributes = []; _ } ->
        {a with pvb_attributes = [Attributes.remove_attrib]}
      | _ -> a )
    | _ -> a

let add_tdl_entries str =
  List.flatten
  @@ List.map str ~f:(function
       | { pstr_desc =
             Pstr_eval
               ({pexp_desc = Pexp_constant (Pconst_integer (s, None)); _}, l)
         ; pstr_loc; _ }
         when List.exists l ~f:(fun (x, _) -> x.txt == Attributes.tdl_string)
       -> (
         match Hashtbl.find_all htl_tdl_entries @@ int_of_string s with
         | exception Failure _ ->
           error ~loc:pstr_loc "fatal: type info not found"
         | x -> List.rev x )
       | x -> [x] )

let rec unbox_box_constr e f =
  match e.pexp_desc with
  | Pexp_constraint (e2, c) ->
    let res = unbox_box_constr e2 f in
    {e with pexp_desc = Pexp_constraint (res, c)}
  | _ -> U.with_loc e.pexp_loc @@ fun () -> f e

let external' ~is_inline loc strpri =
  Ast_helper.default_loc := loc ;
  let release_runtime_lock = ref false in
  let noalloc = ref false in
  let return_errno = ref false in
  let remove_labels = ref false in
  let attrs = ref [] in
  List.iter strpri.pval_attributes ~f:(fun (s, y) ->
      let reuse_attrib =
        match s.txt with
        (* TODO: what else? *)
        | "release_runtime_lock" ->
          release_runtime_lock := true ;
          false
        | "noalloc" ->
          noalloc := true ;
          false
        | "return_errno" ->
          return_errno := true ;
          false
        | "remove_labels" when is_inline ->
          remove_labels := true ;
          false
        | "ocaml.warnerror" | "ocaml.deprecated" | "ocaml.warning"
         |"warnerror" | "deprecated" | "warning" ->
          attrs := (s, y) :: !attrs ;
          true
        | x -> error ~loc:s.loc "unsupported attribute %s" x
      in
      if reuse_attrib = false && y <> PStr [] then
        error ~loc:s.loc "unknown content in attribute %s" s.txt ) ;
  let attrs = List.rev !attrs in
  let release_runtime_lock = !release_runtime_lock in
  let noalloc = !noalloc in
  let return_errno = !return_errno in
  let remove_labels = !remove_labels in
  H.external' ~is_inline ~remove_labels ~return_errno ~attrs
    ~release_runtime_lock ~noalloc strpri

let clear () =
  Uniq_ref.clear () ;
  Created_types.clear () ;
  Script_result.clear () ;
  Hashtbl.clear htl_tdl_entries ;
  Ppx_mod.mod_name := None

let mapper _config _cookies =
  let module P = struct
    type t =
      (* currently the AST is traversed twice.*)
      | Initial_scan
      (* collect all information *)
      | Replace

    (* replace expressions with information extracted from C *)
  end in
  let phase = ref P.Initial_scan in
  let is_outer_structure = ref true in
  let name_needed () =
    error "new types need a name. Parallel pattern matching is not supported"
  in
  let structure mapper str =
    if !is_outer_structure = false || str = [] then
      let str = if !phase <> P.Replace then str else add_tdl_entries str in
      default_mapper.structure mapper str |> remove_empty
    else
      let orig_log = (List.hd str).pstr_loc in
      clear () ;
      convert_ctypes_exeptions
      @@ fun () ->
      is_outer_structure := false ;
      let st = default_mapper.structure mapper str in
      Ast_helper.default_loc := orig_log ;
      Top.run () ;
      let ppx_main = Ppx_mod.modules () in
      let st = ppx_main @ add_tdl_entries st in
      phase := P.Replace ;
      let st = default_mapper.structure mapper st |> remove_empty in
      C_content.write_file () ;
      clear () ;
      st
  in
  let structure_item_scan mapper = function
    | { pstr_desc =
          Pstr_extension
            ( ( {txt = "c"; _}
              , PStr [{pstr_desc = Pstr_primitive strpri; pstr_loc; _}] )
            , _ ); _ }
      when strpri.pval_prim <> [] -> external' ~is_inline:true pstr_loc strpri
    | {pstr_desc = Pstr_primitive strpri; pstr_loc} when strpri.pval_prim <> []
    -> external' ~is_inline:false pstr_loc strpri
    | { pstr_desc =
          Pstr_extension
            ( ( {txt = ("c" | "c_union" | "c_bitmask") as txt; _}
              , PStr [{pstr_desc = Pstr_type (rf, tl); pstr_loc}] )
            , _ ); _ } ->
      Ast_helper.default_loc := pstr_loc ;
      let enforce_union = txt = "c_union" in
      let enforce_bitmask = txt = "c_bitmask" in
      H.type_decl ~enforce_union ~enforce_bitmask rf tl
    | { pstr_desc =
          Pstr_extension
            ( ( {txt = "c"; _}
              , PStr
                  [ { pstr_desc =
                        Pstr_value
                          ( Nonrecursive
                          , [{pvb_pat = pat; pvb_expr = exp; pvb_attributes; _}]
                          ); _ } ] )
            , _ ); _ } ->
      (*| [%stri let%c [%p? pat] = [%e? exp]]*)
      let t =
        unbox_box_constr exp
        @@ fun exp ->
        let s, l, is_constant =
          match exp.pexp_desc with
          | Pexp_apply
              ( { pexp_desc = Pexp_ident {txt = Longident.Lident s; loc = _}
                ; pexp_attributes = []; _ }
              , l ) ->
            (s, l, false)
          | Pexp_constant _ -> ("", [], true)
          | _ -> ("", [], false)
        in
        (* TODO: really allow everything as long as a Ctypes.typ is returned? *)
        let name =
          match Extract.variable_from_pattern pat with
          | None when s = "header" -> ""
          | None -> name_needed ()
          | Some x -> x
        in
        match s with
        | "header" -> H.header l
        | "constant" -> H.constant_bind name l
        | "enum" -> H.enum name l
        | "@->" -> H.fn name exp
        | _ when is_constant -> H.pexp_const name exp
        | _ -> H.typ name exp
      in
      let vb = Ast_helper.Vb.mk ~attrs:pvb_attributes pat t |> mark_empty in
      Ast_helper.Str.value Nonrecursive [vb]
    | {pstr_desc = Pstr_extension (({txt = "c"; loc}, _), _); _} ->
      error ~loc "extension 'c' is not supported here"
    | { pstr_desc =
          Pstr_value
            ( Nonrecursive
            , [ ( { pvb_expr =
                      { pexp_desc =
                          Pexp_extension
                            ( {txt = "c"; _}
                            , PStr [{pstr_desc = Pstr_eval (e, []); _}] ); _ }; _
                  } as a ) ] ); _ } as stri ->
      let t =
        unbox_box_constr e
        @@ fun e ->
        let prefix = Extract.variable_from_pattern a.pvb_pat in
        let s, l =
          match e.pexp_desc with
          | Pexp_apply
              ( { pexp_desc = Pexp_ident {txt = Longident.Lident s; loc = _}
                ; pexp_attributes = []; _ }
              , l ) ->
            (s, l)
          | Pexp_apply _ -> ("", [])
          | _ -> error "only function application is allowed in this context"
        in
        match s with
        | "header" -> H.header l
        | "field" -> H.field ?prefix l
        | "constant" -> H.constant l
        | "seal" -> H.seal l
        | "foreign_value" -> H.foreign_value l
        | "foreign" -> H.foreign ?prefix l
        | _ -> error "invalid function call in [%%c ... ]"
      in
      let na = mark_empty {a with pvb_expr = t} in
      {stri with pstr_desc = Pstr_value (Nonrecursive, [na])}
    | {pstr_desc = Pstr_module x; pstr_loc; _} as stri ->
      Scripts_structure.open_module x.pmb_name.txt ;
      let stri = default_mapper.structure_item mapper stri in
      Scripts_structure.close_module pstr_loc ;
      stri
    | {pstr_desc = Pstr_recmodule l; pstr_loc} ->
      let l' =
        List.map l ~f:(fun x ->
            Scripts_structure.open_module x.pmb_name.txt ;
            let r = default_mapper.module_binding mapper x in
            Scripts_structure.close_module pstr_loc ;
            r )
      in
      {pstr_desc = Pstr_recmodule l'; pstr_loc}
    | stri -> default_mapper.structure_item mapper stri
  in
  let structure_item_replace mapper = function
    | { pstr_desc =
          Pstr_eval
            ( {pexp_desc = Pexp_constant (Pconst_integer (s, None)); _}
            , (_ :: _ as l) ); _ }
      when List.exists l ~f:(fun (x, _) ->
               x.txt == Attributes.replace_expr_string ) -> (
      try Hashtbl.find Script_result.htl_stri (int_of_string s)
      with Not_found -> error "fatal error: external not found" )
    | { pstr_desc =
          Pstr_value
            ( Nonrecursive
            , [ ( { pvb_expr =
                      { pexp_desc = Pexp_ident _
                      ; pexp_attributes = _ :: _ as l; _ } as pexp; _ } as pvb
                ) ] ); _ } as stri
      when List.exists l ~f:(fun (x, _) ->
               x.txt == Attributes.replace_attr_string ) ->
      mark_if_used mapper stri pvb pexp
    | { pstr_desc =
          Pstr_value
            ( Nonrecursive
            , [ ( { pvb_expr =
                      { pexp_desc =
                          Pexp_constraint
                            ( ( { pexp_desc = Pexp_ident _
                                ; pexp_attributes = _ :: _ as l; _ } as pexp )
                            , ct ); _ } as pexp_outer; _ } as pvb ) ] ); _ } as
      stri
      when List.exists l ~f:(fun (x, _) ->
               x.txt == Attributes.replace_attr_string ) ->
      mark_if_used ~pexp_outer:(pexp_outer, ct) mapper stri pvb pexp
    | stri -> default_mapper.structure_item mapper stri
  in
  let structure_item mapper stri =
    Ast_helper.default_loc := stri.pstr_loc ;
    match !phase with
    | P.Initial_scan -> structure_item_scan mapper stri
    | P.Replace -> structure_item_replace mapper @@ Uniq_ref.replace_stri stri
  in
  let expr_scan mapper = function
    | { pexp_desc =
          Pexp_extension
            ({txt = "c"; _}, PStr [{pstr_desc = Pstr_eval (e, []); _}]); _ } -> (
      unbox_box_constr e
      @@ fun e ->
      let s, l =
        match e.pexp_desc with
        | Pexp_apply
            ( { pexp_desc = Pexp_ident {txt = Longident.Lident s; loc = _}
              ; pexp_attributes = []; _ }
            , l ) ->
          (s, l)
        | Pexp_apply _ -> ("", [])
        | _ -> error "only function application is allowed in this context"
      in
      match s with
      | "constant" -> H.constant l
      | "foreign_value" -> H.foreign_value l
      | "foreign" -> H.foreign l
      | _ -> error "invalid function call in [%%c ... ]" )
    | {pexp_desc = Pexp_extension ({txt = "c"; loc}, _); _} ->
      error ~loc "extension 'c' is not allowed here"
    | {pexp_desc = Pexp_letmodule (name, mexpr, expr); _} as pexp ->
      Scripts_structure.open_let_module name.txt ;
      let mexpr' =
        let r = default_mapper.module_expr mapper mexpr in
        Scripts_structure.close_let_module_module mexpr.pmod_loc ;
        r
      in
      let expr' = default_mapper.expr mapper expr in
      Scripts_structure.close_let_module_expr mexpr.pmod_loc ;
      {pexp with pexp_desc = Pexp_letmodule (name, mexpr', expr')}
    | pexp -> default_mapper.expr mapper pexp
  in
  let expr_replace mapper = function
    | { pexp_desc = Pexp_constant (Pconst_integer (s, None))
      ; pexp_attributes = _ :: _ as attribs; _ }
      when List.exists attribs ~f:(fun (x, _) ->
               x.txt == Attributes.replace_expr_string ) -> (
      try Hashtbl.find Script_result.htl_expr (int_of_string s)
      with Not_found -> error "fatal: constant not found" )
    | pexp -> default_mapper.expr mapper pexp
  in
  let expr mapper pexp =
    Ast_helper.default_loc := pexp.Parsetree.pexp_loc ;
    match !phase with
    | P.Initial_scan -> expr_scan mapper pexp
    | P.Replace -> expr_replace mapper (Uniq_ref.replace_expr pexp)
  in
  let from_htl htl name attribs =
    let fail () = failwith "invalid parsetree generated (typ replacement)" in
    let id =
      List.find_map attribs ~f:(fun (x, t) ->
          if x.txt != name then None
          else
            match t with
            | PStr
                [ { pstr_desc =
                      Pstr_eval
                        ( {pexp_desc = Pexp_constant (Pconst_integer (s, _)); _}
                        , _ ); _ } ] ->
              Some s
            | _ -> fail () )
    in
    match id with
    | None -> fail ()
    | Some s -> (
      try int_of_string s |> Hashtbl.find htl with
      | Not_found | Failure _ -> fail () )
  in
  let rec type_declaration mapper tdl =
    if
      !phase = P.Replace
      && List.exists tdl.ptype_attributes ~f:(fun (x, _) ->
             x.txt == Attributes.replace_struct_string )
    then
      let res_top, res_bottom, params =
        from_htl Script_result.htl_records Attributes.replace_struct_string
          tdl.ptype_attributes
      in
      let res =
        match tdl.ptype_manifest with
        | Some ({ptyp_desc = Ptyp_constr (c, _); _} as whole) ->
          let ptyp_desc = Ptyp_constr (c, params) in
          let ptype_manifest = Some {whole with ptyp_desc} in
          if tdl.ptype_kind = Ptype_abstract then
            let ptype_params =
              (*uniq alias name in top module *)
              List.map params ~f:(fun s -> (s, Asttypes.Invariant))
            in
            let ptype_attributes =
              List.filter tdl.ptype_attributes ~f:(fun (x, _) ->
                  x.txt != Attributes.replace_struct_string )
            in
            {tdl with ptype_manifest; ptype_params; ptype_attributes}
          else (* alias outside top module *)
            {res_bottom with ptype_manifest}
        | None | Some _ -> res_top
      in
      type_declaration mapper res
    else default_mapper.type_declaration mapper tdl
  in
  let rec typ mapper ptyp =
    if !phase <> P.Replace then default_mapper.typ mapper ptyp
    else
      match Uniq_ref.replace_typ ptyp with
      | {ptyp_desc = Ptyp_any; ptyp_attributes = _ :: _ as attribs; _}
        when List.exists attribs ~f:(fun (x, _) ->
                 x.txt == Attributes.replace_typ_string ) ->
        from_htl Script_result.htl_type Attributes.replace_typ_string attribs
      | { ptyp_desc = Ptyp_constr (norig, _) as orig
        ; ptyp_attributes = _ :: _ as attribs; _ } as ptyp
        when List.exists attribs ~f:(fun (x, _) ->
                 x.txt == Attributes.replace_struct_string ) ->
        let _, _, params =
          from_htl Script_result.htl_records Attributes.replace_struct_string
            attribs
        in
        let ptyp_desc =
          match params with [] -> Ptyp_constr (norig, []) | _ -> orig
        in
        typ mapper {ptyp with ptyp_attributes = []; ptyp_desc}
      | x -> default_mapper.typ mapper x
  in
  {default_mapper with structure_item; structure; expr; typ; type_declaration}

let init () =
  let () = Ppxc__script_real._init () in
  Migrate_parsetree.Driver.register ~name:"ppx_cstubs" Mparsetree.ast_version
    mapper
