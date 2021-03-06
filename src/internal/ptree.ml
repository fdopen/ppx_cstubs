(* This file is part of ppx_cstubs (https://github.com/fdopen/ppx_cstubs)
 * Copyright (c) 2018-2020 fdopen
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

open Std
module List = CCListLabels
module U = Std.Util
open Mparsetree.Ast_cur

let error = Util.error

type module_kind =
  [ `Module
  | `Let_module
  | `Let_cont
  | `Anon_module
  | `Open_struct
  ]

module Ppx_mod_top_common (M : sig
  type t

  val add_from_sub : module_kind -> string -> t list -> t list
end) =
struct
  type items = M.t list

  type t =
    | Root of items
    | Module of t * module_kind * string * items

  let state = ref (Root [])

  let get_entries () =
    match !state with
    | Root x -> List.rev x
    | Module _ ->
      error
        "internal error. Ppx_mod_top_common.get_entries not called at root level"

  let add_entry x =
    match !state with
    | Root el -> state := Root (x :: el)
    | Module (t, k, s, l) -> state := Module (t, k, s, x :: l)

  let open_module k s = state := Module (!state, k, s, [])

  let add_module k s = function
    | [] -> ()
    | l ->
      let l = M.add_from_sub k s l in
      List.iter l ~f:add_entry

  let close_module loc =
    U.with_loc loc @@ fun () ->
    match !state with
    | Module (t, k, s, l) ->
      state := t;
      add_module k s l
    | Root _ -> error "ppx_cstubs failed to parse the AST"

  let clear () = state := Root []

  let cur_is_empty () =
    match !state with Root l | Module (_, _, _, l) -> l = []
end

module Make_ocaml_types () = struct
  type typ =
    | Simple
    | Poly

  type tree =
    | Type_def of typ * string
    | Sub_module of string * tree list

  module Tree = Ppx_mod_top_common (struct
    type t = tree

    let add_from_sub _ n l = [ Sub_module (n, List.rev l) ]
  end)

  let add_poly s = Tree.add_entry (Type_def (Poly, s))

  let add_simple s = Tree.add_entry (Type_def (Simple, s))

  let rec get_parent_names accu s =
    match s with
    | Tree.Root _ -> accu
    | Tree.Module (t, _, s, _) -> get_parent_names (s :: accu) t

  let get_parent_names () = get_parent_names [] !Tree.state

  open Ast_helper

  let rec get_item_a ?os_parent ~global_parents ~sub_parents ac = function
    | Type_def (typ, name) ->
      let sub_parents = List.rev sub_parents in
      let t = U.mk_lid_l (global_parents @ sub_parents @ [ name ]) in
      let l, params =
        match typ with
        | Simple -> ([], None)
        | Poly ->
          let a' = Typ.var "a" in
          ([ a' ], Some [ (a', (NoVariance, NoInjectivity)) ])
      in
      let l' = sub_parents @ [ name ] in
      let l' = match os_parent with None -> l' | Some x -> x :: l' in
      let manifest = U.mk_typc_l ~l l' in
      let tdl = Type.mk ?params ~manifest (U.mk_loc name) in
      Pwith_type (t, tdl) :: ac
    | Sub_module (n, t) ->
      List.fold_left t ~init:ac ~f:(fun ac el ->
          get_item_a ?os_parent ~global_parents ~sub_parents:(n :: sub_parents)
            ac el)

  let get_item ?os_parent ~global_parents ~sub_parents e =
    get_item_a ?os_parent ~global_parents ~sub_parents [] e |> List.rev

  let get_last () =
    let l = match !Tree.state with Root l | Module (_, _, _, l) -> l in
    let cur = match l with [] -> assert false | hd :: _ -> hd in
    let parents = get_parent_names () in
    get_item ~global_parents:parents ~sub_parents:[] cur

  let get_cur_mod () =
    let old_state = !Tree.state in
    Std.finally ~h:(fun () -> Tree.state := old_state) @@ fun () ->
    match !Tree.state with
    | Tree.Root _ -> assert false
    | Tree.Module (_, _, _, []) -> []
    | Tree.Module (t, k, s, l) ->
      Tree.state := t;
      Tree.add_module k s l;
      get_last ()

  let iopen_declaration kind l =
    let lid = U.mk_lid !Lconst.type_modtype_name in
    let incl = Incl.mk (Mty.with_ (Mty.ident lid) l) in
    let typ = Mty.signature [ Sig.include_ incl ] in
    let x = Mtd.mk ~typ (U.mk_loc !Lconst.type_modtype_name) in
    let x = Mod.structure [ Str.modtype x ] in
    let attrs = [ U.ocaml_warning "-33-66" ] in
    let attrs =
      match kind with
      | `Type_mod -> Attributes.open_struct_type_mod_attrib :: attrs
      | `Body -> Attributes.open_struct_body_attrib :: attrs
      | `None -> attrs
    in
    Opn.mk ~override:Override ~attrs x

  let types_added () =
    match !Tree.state with Tree.Root l | Tree.Module (_, _, _, l) -> l <> []

  let open_last x = Str.open_ (iopen_declaration x (get_last ()))
end

module OSTypes_visible = struct
  include Make_ocaml_types ()

  let with_dont_save_types f =
    let cur_state = !Tree.state in
    Std.finally ~h:(fun () -> Tree.state := cur_state) f

  let cond_save_types x f = if x then f () else with_dont_save_types f

  let open_last () = open_last `Body

  let open_declaration_s l = iopen_declaration `Body l

  let open_declaration_e l = iopen_declaration `None l
end

module OSTypes_all = struct
  open Ast_helper

  include Make_ocaml_types ()

  let build_top_include name =
    let idl = Incl.mk (Mty.typeof_ (Mod.ident (U.mk_lid name))) in
    let typ = Mty.signature [ Sig.include_ idl ] in
    Str.modtype (Mtd.mk ~typ (U.mk_loc !Lconst.type_modtype_name))

  let open_last () = open_last `Type_mod

  let open_wl l = Str.open_ (iopen_declaration `Type_mod l)

  let open_struct_types_mod () =
    let os_parent = !Lconst.type_mod_name in
    (match !Tree.state with Root l -> List.rev l | Module _ -> assert false)
    |> List.fold_left ~init:[] ~f:(fun ac el ->
           get_item_a ~os_parent ~global_parents:[] ~sub_parents:[] ac el)
    |> List.rev
    |> iopen_declaration `None
    |> Str.open_
end

module Ppx_mod_top_parsetree = struct
  open Ast_helper

  type t = structure_item

  let add_from_sub k s l =
    if Hashtbl.mem Keywords.htl_modules s then
      error "module name %S is reserved" s;
    (match k with
    | `Let_cont | `Anon_module | `Open_struct -> ()
    | `Module | `Let_module ->
      let pre = Myconst.private_prefix_capitalized in
      if CCString.prefix ~pre s then
        error "module prefix %s is reserved (%S)" pre s);
    let ms = Mod.structure (List.rev l) in
    let r = Str.module_ (Mb.mk (U.mk_oloc s) ms) in
    match k with
    | `Module | `Let_module | `Let_cont | `Anon_module -> [ r ]
    | `Open_struct ->
      let m = Mod.ident (U.mk_lid s) in
      let attrs = [ U.ocaml_warning "-33-44-45-66" ] in
      let attrs = Attributes.open_struct_openmod_attrib :: attrs in
      let r2 = Str.open_ (Opn.mk ~attrs ~override:Override m) in
      [ r; r2 ]
end

module Ppx_mod_main_common (M : sig
  val name_f : unit -> string
end) =
struct
  module Structure = Ppx_mod_top_common (Ppx_mod_top_parsetree)
  open Structure

  let get_mod_path () =
    let rec iter accu = function
      | Root _ -> M.name_f () :: accu
      | Module (t, _, s, _) -> iter (s :: accu) t
    in
    iter [] !state

  let create_ref_lid s =
    let rec iter = function
      | Root _ -> Lident (M.name_f ())
      | Module (t, _, s, _) -> Ldot (iter t, s)
    in
    U.mk_loc @@ Ldot (iter !state, s)

  let create_type_ref s =
    let r = create_ref_lid s in
    Ast_helper.Typ.constr r []

  let create_ref ?attrs s =
    let r = create_ref_lid s in
    Ast_helper.Exp.ident ?attrs r
end

let check_name is_fun name =
  if name = "" || name = "_" then error "variable name empty";
  let f = name.[0] in
  if is_fun = false && not ((f >= 'a' && f <= 'z') || f = '_') then
    error "prefix or infix symbols are not allowed here";
  if Hashtbl.mem Keywords.htl name then
    error "%S is predefined. You should'nt shadow it here" name;
  let pre = Myconst.private_prefix in
  if CCString.prefix ~pre name then
    error "the %s prefix is reserved for generated code" pre;
  let pre = "_" ^ pre in
  if CCString.prefix ~pre name then
    error "the %s prefix is reserved for generated code" pre;
  match name with
  | "=" | "<>" ->
    error
      "the operator %S is used inside generated code. You can't overwrite it"
      name
  | _ -> ()

module Impl_mod = struct
  include Ppx_mod_main_common (struct
    let name_f () = !Lconst.impl_mod_name
  end)

  let add_entry = Structure.add_entry

  module Ur = Uniq_ref

  let add_named ?constr ?(name_check = true) ?attrs ~retype name expr =
    if name_check then check_name false name;
    let mod_path = get_mod_path () in
    let r = Ur.make ?constr ?main_ref_attrs:attrs ~retype mod_path name expr in
    add_entry r.Ur.topmod_vb;
    add_entry r.Ur.topmod_ref;
    r.Ur.main_ref

  let add_external ~name_check stri_external stri_expr ~name =
    if name_check then check_name true name;
    let mod_path = get_mod_path () in
    let loc = !Ast_helper.default_loc in
    let r = Ur.make mod_path ~retype:true name [%expr ()] in
    add_entry stri_external;
    add_entry stri_expr;
    add_entry r.Ur.topmod_ref;
    (r.Ur.id, r.Ur.main_ref)

  let add_external_anon stri_external stri_expr n =
    add_entry stri_external;
    add_entry stri_expr;
    U.alias_type (create_ref n)

  let add_opaq main_ref_attrs stri name =
    check_name false name;
    let mod_path = get_mod_path () in
    let loc = !Ast_helper.default_loc in
    let r = Ur.make ~main_ref_attrs ~retype:false mod_path name [%expr ()] in
    add_entry stri;
    add_entry r.Ur.topmod_ref;
    (r.Ur.id, r.Ur.main_ref)

  let add_unit expr =
    let loc = !Ast_helper.default_loc in
    add_entry [%stri let () = [%e expr]];
    [%expr ()]
end

module Type_mod = struct
  include Ppx_mod_main_common (struct
    let name_f () = !Lconst.type_mod_name
  end)

  let add_entry = Structure.add_entry
end

module Topscript = struct
  module Structure_extract_phase0 = Ppx_mod_top_common (Ppx_mod_top_parsetree)
  module Structure_extract = Ppx_mod_top_common (Ppx_mod_top_parsetree)
  module Structure_build_external = Ppx_mod_top_common (Ppx_mod_top_parsetree)

  let add_help f =
    let last_loc = ref !Ast_helper.default_loc in
    fun s ->
      let loc = !Ast_helper.default_loc in
      if !last_loc <> loc || true then (
        last_loc := loc;
        let l = U.marshal_to_str_expr loc in
        f [%stri let () = Ppxc__script.set_loc [%e l]]);
      f s

  let add_build_external = add_help Structure_build_external.add_entry

  let add_extract = add_help Structure_extract.add_entry

  let add_extract_phase0 = add_help Structure_extract_phase0.add_entry

  let run top =
    let extract_phase0_entries = Structure_extract_phase0.get_entries () in
    let extract_entries = Structure_extract.get_entries () in
    let build_entries = Structure_build_external.get_entries () in
    if extract_phase0_entries = [] && extract_entries = [] && build_entries = []
    then ()
    else
      let loc = !Ast_helper.default_loc in
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
        let h l =
          let open Ast_helper in
          Str.module_ (Mb.mk (U.mk_oloc "A") (Mod.structure l))
        in
        [%expr
          let module M : sig end = struct
            [@@@ocaml.warning "-60"]

            let () =
              let module Extract_phase0 : sig end = struct
                [%%i h extract_phase0_entries]
              end in
              ()

            open! Ppxc__script.Run_environment [@@ocaml.warning "-33-66"]

            [%%i ctypes]

            open! Ctypes [@@ocaml.warning "-33-66"]

            let () =
              let module Extract_c_consts : sig end = struct
                [%%i h extract_entries]
              end in
              ()

            module Build_externals : sig end = struct
              [%%i h build_entries]
            end
          end in
          (Ppxc__script.Main.run () : unit)]
      in
      let module C = Ppxlib_ast.Select_ast (Ppxlib_ast__.Versions.OCaml_current) in
      let expr = C.To_ocaml.copy_structure [ Ast_helper.Str.eval expr ] in
      Toplevel.init top;
      top#eval expr
end

module Mod_structure = struct
  module type h = sig
    val open_module : module_kind -> string -> unit

    val close_module : Ast_helper.loc -> unit

    val clear : unit -> unit

    val cur_is_empty : unit -> bool
  end

  let l : (module h) list =
    [
      (module (Topscript.Structure_build_external : h));
      (module (Topscript.Structure_extract : h));
      (module (Impl_mod.Structure : h));
      (module (Type_mod.Structure : h));
      (module (OSTypes_visible.Tree : h));
      (module (OSTypes_all.Tree : h));
    ]

  let for_all (f : (module h) -> unit) =
    let exn = ref None in
    List.iter l ~f:(fun s ->
        try f s with x -> if !exn = None then exn := Some x);
    match !exn with None -> () | Some x -> raise x

  let close_all loc = for_all (fun (module M) -> M.close_module loc)

  let modul_c k loc s f =
    for_all (fun (module M) -> M.open_module k s);
    Std.finally f ~h:(fun () -> close_all loc)

  let modul loc s f = modul_c `Module loc s f

  let modul_c2 k loc f =
    let s = U.safe_mlname ~capitalize:true ~prefix:"_" () in
    modul_c k loc s f

  let module_anon loc f = modul_c2 `Anon_module loc f

  let open' loc f = modul_c2 `Open_struct loc f

  let let_common kind ~cont_name loc s ~fmexpr ~fexpr =
    let already_closed = ref false in
    for_all (fun (module M) ->
        M.open_module `Let_cont cont_name;
        M.open_module kind s);
    Std.finally ~h:(fun () -> if not !already_closed then close_all loc)
    @@ fun () ->
    let h () =
      let closable = ref true in
      for_all (fun (module M) ->
          M.close_module loc;
          if M.cur_is_empty () = false then closable := false);
      if !closable then (
        already_closed := true;
        close_all loc)
    in
    let mexpr = Std.finally ~h fmexpr in
    (mexpr, fexpr ())

  let let_module loc s ~fmexpr ~fexpr =
    let s, k =
      match s.txt with
      | Some s -> (s, `Let_module)
      | None -> (U.safe_mlname ~capitalize:true (), `Anon_module)
    in
    let cont_name = U.safe_mlname ~capitalize:true ~prefix:s () in
    let_common k ~cont_name loc s ~fmexpr ~fexpr

  let let_open loc ~fmexpr ~fexpr =
    let s = U.safe_mlname ~capitalize:true ~prefix:"_" () in
    let cont_name = U.safe_mlname ~capitalize:true () in
    let_common `Open_struct ~cont_name loc s ~fmexpr ~fexpr

  let clear () = for_all (fun (module M) -> M.clear ())
end

module Order = struct
  (* TODO: I could build a nice tree, look at the types
     and expressions and give a better approxomiation, if
     the type module is needed... It's probably not
     worth it. *)

  module Private = struct
    let pos = ref 0

    let last_abstract_type = ref None

    let first_abstract_type = ref None

    let n_abstract_types = ref 0

    let last_derived_type = ref None

    let first_derived_type = ref None

    let n_derived_types = ref 0

    let first_types_hidden_pos = ref None

    let last_expr = ref None

    let with_new_pos f =
      let i = !pos in
      incr pos;
      f (Some i)
  end

  open Private

  let reg_abstract () =
    with_new_pos @@ fun id ->
    incr n_abstract_types;
    (match !first_abstract_type with
    | None -> first_abstract_type := id
    | Some _ -> ());
    last_abstract_type := id

  let reg_derived () =
    with_new_pos @@ fun id ->
    incr n_derived_types;
    (match !first_derived_type with
    | None -> first_derived_type := id
    | Some _ -> ());
    last_derived_type := id

  let reg_expr () = with_new_pos @@ fun id -> last_expr := id

  let write_type_module () =
    if
      Ocaml_config.use_open_struct () = false
      || (!last_abstract_type = None && !last_derived_type = None)
    then false
    else
      match !first_types_hidden_pos with
      | None -> false
      | Some hidden_pos -> (
        (match !last_expr with
        | None -> false
        | Some last_expr -> last_expr > hidden_pos)
        ||
        match !last_derived_type with None -> false | Some x -> x > hidden_pos)

  let remove_alias_types () = !n_derived_types = 0 && !n_abstract_types = 0

  let types_hidden () =
    match !first_types_hidden_pos with
    | Some _ -> ()
    | None -> with_new_pos @@ fun id -> first_types_hidden_pos := id

  let delete_os_inside_type_mod () =
    match (!first_abstract_type, !first_derived_type) with
    | _, None -> true
    | None, Some _ -> !n_derived_types = 1
    | Some a, Some d -> !n_derived_types = 1 && a > d

  let clear () =
    pos := 0;
    last_abstract_type := None;
    n_abstract_types := 0;
    last_derived_type := None;
    n_derived_types := 0;
    first_types_hidden_pos := None;
    first_abstract_type := None;
    first_derived_type := None;
    last_expr := None
end

module OSTypes = struct
  module O = Order

  let remove_alias_types = O.remove_alias_types

  let delete_os_inside_type_mod = O.delete_os_inside_type_mod

  let with_open_module n f =
    OSTypes_visible.Tree.open_module `Module n;
    OSTypes_all.Tree.open_module `Module n;
    Std.finally f ~h:(fun () ->
        let loc = !Ast_helper.default_loc in
        OSTypes_visible.Tree.close_module loc;
        OSTypes_all.Tree.close_module loc)

  let ret () =
    if Ocaml_config.use_open_struct () = false then None
    else (
      Type_mod.add_entry (OSTypes_all.open_last ());
      Some (OSTypes_visible.open_last ()))

  let add_s x =
    OSTypes_visible.add_simple x;
    OSTypes_all.add_simple x

  let add_abstract ?sub_module x =
    (match sub_module with
    | None -> add_s x
    | Some s -> with_open_module s (fun () -> add_s x));
    O.reg_abstract ();
    ret ()

  let add_types_cb m =
    with_open_module m (fun () ->
        OSTypes_visible.add_poly "t";
        OSTypes_all.add_poly "t";
        add_s "raw_pointer");
    O.reg_derived ();
    ret ()

  let add_struct_view x =
    add_s x;
    O.reg_derived ();
    ret ()

  let types_maybe_used = O.reg_expr
end

module Modules = struct
  let is_primitive_module_expr x =
    match x.pmod_desc with
    | Pmod_ident _ | Pmod_structure _ -> true
    | Pmod_functor _ | Pmod_apply _ | Pmod_unpack _ | Pmod_extension _
    | Pmod_constraint _ ->
      false

  let types_hidden = Order.types_hidden

  let pstr_module x mapper_f =
    let is_prim = is_primitive_module_expr x.pmb_expr in
    let save_types = x.pmb_name.txt <> None && is_prim in
    let pstr_loc = x.pmb_loc in
    OSTypes_visible.cond_save_types save_types @@ fun () ->
    let f () =
      let res = mapper_f () in
      (res, OSTypes_visible.types_added (), OSTypes_all.types_added ())
    in
    let res, types_visible_added, types_all_added =
      match x.pmb_name.txt with
      | Some n -> Mod_structure.modul pstr_loc n f
      | None -> Mod_structure.module_anon pstr_loc f
    in
    if types_all_added then OSTypes_all.open_last () |> Type_mod.add_entry;
    if types_visible_added && x.pmb_name.txt <> None && is_prim = false then
      types_hidden ();
    let uos = Ocaml_config.use_open_struct () in
    if (not uos) || (not save_types) || not types_visible_added then [ res ]
    else [ res; OSTypes_visible.open_last () ]

  let pexp_pack m mapper_f =
    OSTypes_visible.with_dont_save_types @@ fun () ->
    let r, types_all_added =
      Mod_structure.module_anon m.pmod_loc @@ fun () ->
      let r = mapper_f () in
      if OSTypes_visible.types_added () then types_hidden ();
      (r, OSTypes_all.types_added ())
    in
    if types_all_added then OSTypes_all.open_last () |> Type_mod.add_entry;
    r

  let pexp_letmodule name ~mexpr ~fmexpr ~fexpr =
    OSTypes_visible.with_dont_save_types @@ fun () ->
    let types_all_added = ref false in
    let is_prim = is_primitive_module_expr mexpr in
    let visible_types = ref [] in
    let ((mexpr, expr) as res) =
      Mod_structure.let_module mexpr.pmod_loc name
        ~fmexpr:(fun () ->
          let res = fmexpr () in
          types_all_added := OSTypes_all.types_added ();
          let l = OSTypes_visible.get_cur_mod () in
          if l <> [] then (
            visible_types := l;
            if (not is_prim) && name.txt <> None then types_hidden ());
          res)
        ~fexpr:(fun () ->
          if !types_all_added then
            OSTypes_all.open_last () |> Type_mod.add_entry;
          let r = fexpr () in
          types_all_added := OSTypes_all.types_added ();
          r)
    in
    if !types_all_added then OSTypes_all.open_last () |> Type_mod.add_entry;
    let uos = Ocaml_config.use_open_struct () in
    if (not uos) || (not is_prim) || !visible_types = [] || name.txt = None then
      res
    else
      let od = OSTypes_visible.open_declaration_e !visible_types in
      (mexpr, Ast_helper.Exp.open_ od expr)

  let pstr_recmodule l mapper_f =
    OSTypes_visible.with_dont_save_types @@ fun () ->
    let new_types = ref [] in
    let lres =
      List.map l ~f:(fun x ->
          OSTypes_visible.with_dont_save_types @@ fun () ->
          let types_all_added = ref false in
          let types_visible_added = ref false in
          let f () =
            let r = mapper_f x in
            types_all_added := OSTypes_all.types_added ();
            types_visible_added := OSTypes_visible.types_added ();
            r
          in
          let res =
            match x.pmb_name.txt with
            | Some n -> Mod_structure.modul x.pmb_loc n f
            | None -> Mod_structure.module_anon x.pmb_loc f
          in
          if !types_all_added then
            new_types := [ OSTypes_all.get_last () ] @ !new_types;
          if !types_visible_added && x.pmb_name.txt <> None then types_hidden ();
          res)
    in
    (match !new_types with
    | [] -> ()
    | l ->
      List.rev l |> List.flatten |> OSTypes_all.open_wl |> Type_mod.add_entry);
    lres

  let added_in_scope let_struct f =
    let module Ov = OSTypes_visible in
    let cur_visible () =
      match !Ov.Tree.state with
      | Ov.Tree.Root l | Ov.Tree.Module (_, _, _, l) -> l
    in
    let n_old = cur_visible () |> List.length in
    let res = f () in
    let l = cur_visible () in
    let l = List.take (List.length l - n_old) l in
    match l with
    | [] -> (res, None)
    | l -> (
      let p = Ov.get_parent_names () in
      match
        List.rev_map l ~f:(fun x ->
            Ov.get_item ~global_parents:p ~sub_parents:[] x)
        |> List.flatten
      with
      | [] -> (res, None)
      | l ->
        let n =
          match let_struct with
          | `Struct -> Ov.open_declaration_s l
          | `Let -> Ov.open_declaration_e l
        in
        (res, Some n))

  let added_in_scope_l f =
    match added_in_scope `Struct f with
    | r, None -> [ r ]
    | r, Some r2 -> [ r; Ast_helper.Str.open_ r2 ]

  let pstr_include idl f =
    let module Ov = OSTypes_visible in
    let prim = is_primitive_module_expr idl.pincl_mod in
    if (not prim) || Ocaml_config.use_open_struct () = false then
      [ Ov.with_dont_save_types f ]
    else added_in_scope_l f

  let pstr_open odl mapper_f =
    let is_prim = is_primitive_module_expr odl.popen_expr in
    OSTypes_visible.with_dont_save_types @@ fun () ->
    let r, types_visible_added, types_all_added =
      Mod_structure.open' odl.popen_loc @@ fun () ->
      let res = added_in_scope_l mapper_f in
      (res, OSTypes_visible.types_added (), OSTypes_all.types_added ())
    in
    if types_all_added then OSTypes_all.open_last () |> Type_mod.add_entry;
    if is_prim = false && types_visible_added = true then types_hidden ();
    if Ocaml_config.use_open_struct () && is_prim then r
    else match r with [] | [ _ ] -> r | hd :: _ -> [ hd ]

  let pexp_open ~fodl ~fexpr odl =
    OSTypes_visible.with_dont_save_types @@ fun () ->
    let is_prim = is_primitive_module_expr odl.popen_expr in
    let types_all_added = ref false in
    let nodl = ref None in
    let fmexpr () =
      let r, n = added_in_scope `Let fodl in
      if n <> None && is_prim = false then types_hidden ();
      nodl := n;
      types_all_added := OSTypes_all.types_added ();
      r
    in
    let fexpr () =
      if !types_all_added then OSTypes_all.open_last () |> Type_mod.add_entry;
      let r = fexpr () in
      types_all_added := OSTypes_all.types_added ();
      r
    in
    let ((odl, e) as r) = Mod_structure.let_open odl.popen_loc ~fmexpr ~fexpr in
    if !types_all_added then OSTypes_all.open_last () |> Type_mod.add_entry;
    if Ocaml_config.use_open_struct () = false || is_prim = false then r
    else
      match !nodl with
      | None -> r
      | Some nodl -> (odl, Ast_helper.Exp.open_ nodl e)
end

let type_mod_is_used () =
  Ocaml_config.use_open_struct ()
  && Order.write_type_module ()
  && Type_mod.Structure.get_entries () <> []

let all_top_modules l =
  let module A = Ast_helper in
  let make_module name l =
    let ms = A.Mod.structure l in
    let mb = A.Mb.mk (U.mk_oloc name) ms in
    A.Str.module_ mb
  in
  let uos = Ocaml_config.use_open_struct () in
  let write_type_mod = Order.write_type_module () in
  let loc = !Ast_helper.default_loc in
  let impl_mod =
    let l =
      [%stri
        open! Ppx_cstubs.Ppx_cstubs_internals.Shadow [@@ocaml.warning "-33-66"]]
      :: l
    in
    let l =
      match !Script_result.foreign_used with
      | false -> l
      | true -> [%stri open! Foreign [@@ocaml.warning "-33-66"]] :: l
    in
    let l = [%stri open! Ctypes [@@ocaml.warning "-33-66"]] :: l in
    make_module !Lconst.impl_mod_name l
  in
  let l =
    if uos = false then [ impl_mod ]
    else
      let l =
        if not write_type_mod then []
        else
          match Type_mod.Structure.get_entries () with
          | [] -> []
          | l ->
            (* TODO: Do I ever need [@@@ocaml.warning "-60"] ?? *)
            let a = make_module !Lconst.type_mod_name l in
            let b = OSTypes_all.open_struct_types_mod () in
            [ a; b ]
      in
      let type_aliases = OSTypes_all.build_top_include !Lconst.impl_mod_name in
      let r = A.Opn.mk (A.Mod.structure [ impl_mod; type_aliases ]) in
      A.Str.open_ r :: l
  in
  match !Options.mode with
  | Options.Regular -> l
  | Options.Emulate ->
    let fail = Gen_ml.stdlib_fun "failwith" in
    [%stri
      let () =
        [%e fail] "ppx_cstubs.merlin is not intended to generate real code"]
    :: l

let all_top_modules () =
  match Impl_mod.Structure.get_entries () with
  | [] -> []
  | l -> all_top_modules l

let clear () =
  Mod_structure.clear ();
  Order.clear ()
