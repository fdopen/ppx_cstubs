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

open Mparsetree.Ast_cur
open Parsetree
open Asttypes
open Ast_helper
module U = Std.Util
module List = CCListLabels

let a_orig_name = "ppxc__orig_name"

let a_inmod_ref = "ppxc__orig_inmod_reference_string"

let a_reference_string = "ppxc__orig_reference_string"

(*
  `external foo : ....` is replaced by:

     module Ppx_top_mod = struct
       ...
       external foo_uniq_name : ...
       let foo_uniq_name = .. [@@a_orig_name ... ]
       and _ = ...
       let foo = foo_uniq_name [@@a_inmod_ref ...]
     end
     ...
     let foo = Ppx_top_mod.foo_uniq_name [@@a_reference_string ...]

  If `foo` turns out to be unique in the current module, the indirection step
  is removed again in a later step.
*)

let htl_ctypes = Hashtbl.create 64

type t = {
  mod_path : string list;
  uniq_name : string;
  short_name : string;
  sref : string;
}

type make_result = {
  id : t;
  topmod_vb : structure_item;
  topmod_ref : structure_item;
  main_ref : expression;
}

let attr ~attr ~cont =
  let x = U.mk_loc attr in
  let pl = PStr [ [%stri [%e U.str_expr cont]] ] in
  [ Ast_helper.Attr.mk x pl ]

let vb ?constr ~attrs n expr =
  let p =
    match constr with None -> U.mk_pat n | Some t -> U.mk_pat_pconstr t n
  in
  Str.value Nonrecursive [ Vb.mk ~attrs p expr ]

let make ?constr ?main_ref_attrs ~retype mod_path short_name expr =
  let uniq_name = U.safe_mlname ~prefix:short_name () in
  let sref = String.concat "." (mod_path @ [ short_name ]) in
  Hashtbl.add htl_ctypes sref ();
  let id = { mod_path; uniq_name; short_name; sref } in
  let attrs = attr ~cont:sref ~attr:a_inmod_ref in
  let attrs =
    if Ocaml_config.version () < (4, 6, 0) then attrs
    else U.ocaml_warning "-32" :: attrs
  in
  let topmod_ref = vb ~attrs short_name (U.mk_ident_l [ uniq_name ]) in
  let cont = sref ^ "|" ^ sref in
  let attrs1 = attr ~cont ~attr:a_reference_string in
  let attrs2 =
    match main_ref_attrs with None -> attrs1 | Some x -> x @ attrs1
  in
  let n = mod_path @ [ uniq_name ] in
  let main_ref =
    if retype = false || Ocaml_config.use_open_struct () = false then
      Exp.ident ~attrs:attrs2 (U.mk_lid_l n)
    else
      let e = Exp.ident ~attrs:attrs1 (U.mk_lid_l n) in
      U.alias_type ?attrs:main_ref_attrs e
  in
  let attrs = attr ~cont:sref ~attr:a_orig_name in
  let topmod_vb = vb ?constr ~attrs uniq_name expr in
  { id; topmod_vb; topmod_ref; main_ref }

let get_remove_string_exn name attr =
  let res = ref None in
  let attribs =
    List.filter attr ~f:(fun x ->
        if x.attr_name.txt <> name then true
        else
          match x.attr_payload with
          | PStr
              [
                {
                  pstr_desc =
                    Pstr_eval
                      ( {
                          pexp_desc = Pexp_constant (Pconst_string (s, _, _));
                          _;
                        },
                        _ );
                  _;
                };
              ] ->
            res := Some s;
            false
          | _ -> failwith "surprising content in attribute")
  in
  match !res with
  | None -> failwith "invalid parsetree generated"
  | Some s -> (s, attribs)

let is_uniq_ctype orig =
  (* created let bindings of type Ctypes.typ are not referenced in generated
     code (except from user code, where shadowing is intended).
     It's therefore enough, if they are unique inside the current module.
  *)
  match Hashtbl.find_all htl_ctypes orig with
  | [ _ ] -> true
  | [] -> failwith "invalid parsetree generated"
  | _ -> false

let get_final_name t =
  if is_uniq_ctype t.sref then t.short_name else t.uniq_name

let replace_expr = function
  | { pexp_desc = Pexp_ident _ as orig; pexp_attributes = _ :: _ as attribs; _ }
    as expr
    when List.exists attribs ~f:(fun x -> x.attr_name.txt = a_reference_string)
    ->
    let s, pexp_attributes = get_remove_string_exn a_reference_string attribs in
    let id_ref, single_ref = CCString.Split.left_exn ~by:"|" s in
    let pexp_desc =
      match is_uniq_ctype id_ref with
      | false -> orig
      | true -> Pexp_ident (U.mk_lid single_ref)
    in
    { expr with pexp_desc; pexp_attributes }
  | expr -> expr

let replace_stri = function
  | {
      pstr_desc =
        Pstr_value
          (Nonrecursive, [ ({ pvb_attributes = _ :: _ as attribs; _ } as a) ]);
      _;
    } as stri ->
    if List.exists attribs ~f:(fun x -> x.attr_name.txt = a_orig_name) then
      let s, pvb_attributes = get_remove_string_exn a_orig_name attribs in
      let pvb_pat =
        match is_uniq_ctype s with
        | false -> a.pvb_pat
        | true -> (
          let n = CCString.Split.right_exn ~by:"." s |> snd |> U.mk_pat in
          match a.pvb_pat.ppat_desc with
          | Ppat_constraint (_, c) -> Pat.constraint_ n c
          | _ -> n)
      in
      let a = { a with pvb_attributes; pvb_pat } in
      { stri with pstr_desc = Pstr_value (Nonrecursive, [ a ]) }
    else if List.exists attribs ~f:(fun x -> x.attr_name.txt = a_inmod_ref) then
      let s, pvb_attributes = get_remove_string_exn a_inmod_ref attribs in
      if is_uniq_ctype s then U.empty_stri ()
      else
        let a = { a with pvb_attributes } in
        let stri = { stri with pstr_desc = Pstr_value (Nonrecursive, [ a ]) } in
        U.no_warn_unused_pre406 stri
    else stri
  | stri -> stri

let clear () = Hashtbl.clear htl_ctypes

type merlin_state = string list

let merlin_save () = CCHashtbl.Poly.to_list htl_ctypes |> List.split |> fst

let merlin_restore l =
  clear ();
  List.iter l ~f:(fun x -> Hashtbl.add htl_ctypes x ())
