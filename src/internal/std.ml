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

open Migrate_parsetree (* to avoid warnings generated by ppx_tools_versioned *)

type 'a return = { return : 'b. 'a -> 'b }

let with_return (type a) f =
  let module E = struct
    exception E of a
  end in
  try f { return = (fun x -> raise_notrace (E.E x)) } with E.E r -> r

(* Unlike CCFun.finally I want to ensure that h is only called
   once.*)

let finally ~h f =
  match f () with
  | exception exn ->
    h ();
    raise exn
  | r ->
    h ();
    r

external identity : 'a -> 'a = "%identity"

module Util = struct
  open Mparsetree.Ast_cur
  module Lo = Location
  module Le = Lexing

  let with_loc loc f =
    let old_loc = !Ast_helper.default_loc in
    Ast_helper.default_loc := loc;
    finally ~h:(fun () -> Ast_helper.default_loc := old_loc) f

  let error ?(loc = !Ast_helper.default_loc) fmt =
    Format.ksprintf (fun s -> raise (Lo.Error (Lo.error ~loc s))) fmt

  let error_exn ?(loc = !Ast_helper.default_loc) fmt =
    Format.ksprintf (fun s -> Lo.Error (Lo.error ~loc s)) fmt

  let str_expr ?loc s = Ast_helper.(Exp.constant ?loc (Const.string s))

  let int_expr ?loc ?attrs i =
    Ast_helper.(Exp.constant ?loc ?attrs (Const.int i))

  let mk_loc s = Lo.mkloc s !Ast_helper.default_loc

  let mk_oloc s = Lo.mkloc (Some s) !Ast_helper.default_loc

  let mk_lid_c ?(loc = !Ast_helper.default_loc) s = Lo.mkloc s loc

  let mk_lid ?loc s = mk_lid_c ?loc (Longident.parse s)

  let lid_unflatten = function
    | [] -> None
    | hd :: tl ->
      let open Longident in
      Some (List.fold_left (fun p s -> Ldot (p, s)) (Lident hd) tl)

  let mk_lid_l ?loc l =
    match lid_unflatten l with
    | None -> invalid_arg "mk_lid_l"
    | Some l -> mk_lid_c ?loc l

  let mk_pat s = Ast_helper.Pat.var (mk_loc s)

  let mk_typc_c ?attrs ?(l = []) s = Ast_helper.Typ.constr ?attrs s l

  let mk_typc ?attrs ?l s = mk_typc_c ?attrs ?l (mk_lid s)

  let mk_ident n = Ast_helper.Exp.ident (mk_lid n)

  let mk_typc_l ?attrs ?l s = mk_typc_c ?attrs ?l (mk_lid_l s)

  let mk_ident_l s = Ast_helper.Exp.ident (mk_lid_l s)

  include Uniq_ids

  let empty_stri () =
    let vb =
      Ast_helper.Vb.mk
        ~attrs:[ Attributes.remove_attrib ]
        (mk_pat "()")
        (Ast_helper.Exp.ident (mk_lid "()"))
    in
    Ast_helper.Str.value Asttypes.Nonrecursive [ vb ]

  let marshal_to_str_expr a = str_expr (Marshal.to_string a [])

  let ocaml_warning s =
    let x = mk_loc "ocaml.warning" in
    let pl = Parsetree.PStr [ [%stri [%e str_expr s]] ] in
    Ast_helper.Attr.mk x pl

  let no_warn_unused_pre406 =
    let open Ast_helper in
    fun stri ->
      if Ocaml_config.version () >= (4, 6, 0) then stri
      else
        let loc = stri.Parsetree.pstr_loc in
        let a = Str.attribute ~loc (ocaml_warning "-32") in
        let mod' = Mod.structure [ a; stri ] in
        Str.include_ (Incl.mk ~loc mod')

  let no_warn_unused_post406 =
    let open Ast_helper in
    fun name expr ->
      let pat = Pat.var (mk_loc name) in
      let attrs =
        if Ocaml_config.version () < (4, 6, 0) then []
        else [ ocaml_warning "-32" ]
      in
      let vb = Vb.mk ~attrs pat expr in
      Str.value Asttypes.Nonrecursive [ vb ]

  let no_warn_unused name expr =
    no_warn_unused_post406 name expr |> no_warn_unused_pre406

  let no_warn_unused_module ?(loc = !Ast_helper.default_loc) stri =
    ( [%stri
        include struct
          [@@@ocaml.warning "-60"]

          [%%s [ stri ]]
        end] [@metaloc loc] )

  let no_c_comments s =
    CCString.replace ~which:`All ~sub:"/*" ~by:"/ *" s
    |> CCString.replace ~which:`All ~sub:"*/" ~by:"* /"

  let cloc_comment loc =
    let b = Buffer.create 128 in
    let fmt = Format.formatter_of_buffer b in
    Lo.print_loc fmt loc;
    Format.pp_print_flush fmt ();
    let s = Buffer.contents b |> no_c_comments in
    String.concat " " [ "/*"; s; "*/" ]

  let sig_from_mod_type =
    let open Parsetree in
    fun s ->
      match s.pstr_desc with
      | Pstr_modtype { pmtd_type = Some { pmty_desc = Pmty_signature s; _ }; _ }
        ->
        s
      | _ -> assert false

  module A = Ast_helper

  let mk_pat_pconstr t n =
    if Ocaml_config.version () = (4, 5, 0) && !Options.pretty then
      A.Pat.constraint_ (mk_pat n) t
    else A.Pat.constraint_ (mk_pat n) (A.Typ.poly [] t)

  let named_stri ?constr n expr =
    let p =
      match constr with None -> mk_pat n | Some t -> mk_pat_pconstr t n
    in
    [%stri let [%p p] = [%e expr]]

  let alias_impl_mod () =
    let m = A.Mod.ident (mk_lid_l [ !Lconst.impl_mod_name ]) in
    let t = A.Mty.ident (mk_lid_l [ !Lconst.type_modtype_name ]) in
    A.Mod.constraint_ m t

  let alias_impl_mod_let e =
    let m = alias_impl_mod () in
    A.Exp.letmodule (mk_oloc !Lconst.impl_mod_name) m e

  let alias_impl_mod_os ?(alias_name = !Lconst.impl_mod_name) () =
    let x = alias_impl_mod () in
    let x = A.Mb.mk (mk_oloc alias_name) x in
    let x = A.Mod.structure [ A.Str.module_ x ] in
    A.Str.open_ (A.Opn.mk ~override:Asttypes.Override x)

  (* native compilation is too slow, when the symbols are always
     resolved through constrained alias modules, although it doesn't matter
     at runtime. *)
  let alias_type ?attrs e =
    let module A = Ast_helper in
    let module P = Parsetree in
    let res ?attrs e =
      match attrs with
      | None -> e
      | Some a -> { e with P.pexp_attributes = e.P.pexp_attributes @ a }
    in
    if Ocaml_config.use_open_struct () = false then res ?attrs e
    else
      let e_constr = alias_impl_mod_let e in
      let attrs = match attrs with None -> [] | Some l -> l in
      let attrs = Attributes.open_struct_ifthenelse_attrib :: attrs in
      res ~attrs [%expr if false then [%e e_constr] else [%e e]]

  let convert_ctypes_exeptions f =
    try f () with
    | Ctypes_static.ModifyingSealedType s -> error "%s is already sealed" s
    | Ctypes_static.Unsupported s -> error "ctypes error: %s" s
    | Ctypes_static.IncompleteType -> error "Incomplete Type"
end

module Result = struct
  type ('a, 'b) result = ('a, 'b) CCResult.t =
    | Ok of 'a
    | Error of 'b
end

module Various = struct
  let use_threads () =
    match !Options.findlib_pkgs with
    | [] -> false
    | f_pkgs ->
      let pkgs =
        match Findlib.package_deep_ancestors [ "byte" ] f_pkgs with
        | exception Fl_package_base.No_such_package _ -> f_pkgs
        | d -> d
      in
      List.exists
        (function "threads" | "threads.posix" -> true | _ -> false)
        pkgs
end
