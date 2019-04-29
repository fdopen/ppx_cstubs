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
open Parsetree
open Asttypes
open Ast_helper
module U = Std.Util
module List = CCListLabels

let a_orig_name = "ppxc__orig_name"

let a_inmod_ref = "ppxc__orig_inmod_reference_string"

let a_reference_string = "ppxc__orig_reference_string"

(* e.g. external foo : ....

   is replaced by Ppx_top_mod = struct external foo_uniq_name : ... let
   foo_uniq_name = .. [@@a_orig_name ... ] and _ = ... let foo = foo_uniq_name
   [@@a_inmod_ref ...] end

   let foo = Ppx_top_mod.foo_uniq_name [@@a_reference_string ...]

   If `foo` turns out to be unique in the current module, the indirection step
   is removed again in a later step. *)

let htl_ctypes = Hashtbl.create 64

let htl_ocaml_types = Hashtbl.create 32

type t =
  { mod_path : string list
  ; uniq_name : string
  ; short_name : string
  ; sref : string }

type make_result =
  { id : t
  ; topmod_vb : structure_item
  ; topmod_ref : structure_item
  ; main_ref : expression }

let attr ~attr ~cont = [(U.mk_loc attr, PStr [[%stri [%e U.str_expr cont]]])]

let vb ~attrs n expr = Str.value Nonrecursive [Vb.mk ~attrs (U.mk_pat n) expr]

let make mod_path short_name expr =
  let uniq_name = U.safe_mlname ~prefix:short_name () in
  let sref = String.concat "." (mod_path @ [short_name]) in
  Hashtbl.add htl_ctypes sref () ;
  let id = {mod_path; uniq_name; short_name; sref} in
  let attrs = attr ~cont:sref ~attr:a_inmod_ref in
  let attrs =
    if Ocaml_config.version () < (4, 6, 0) then attrs
    else (U.mk_loc "ocaml.warning", PStr [[%stri "-32"]]) :: attrs
  in
  let topmod_ref = vb ~attrs short_name (U.mk_ident uniq_name) in
  let cont = sref ^ "|" ^ sref in
  let attrs = attr ~cont ~attr:a_reference_string in
  let n = String.concat "." (mod_path @ [uniq_name]) in
  let main_ref = Exp.ident ~attrs (U.mk_lid n) in
  let attrs = attr ~cont:sref ~attr:a_orig_name in
  let topmod_vb = vb ~attrs uniq_name expr in
  {id; topmod_vb; topmod_ref; main_ref}

let get_remove_string_exn name attr =
  let res = ref None in
  let attribs =
    List.filter attr ~f:(fun (x, t) ->
        if x.txt != name then true
        else
          match t with
          | PStr
              [ { pstr_desc =
                    Pstr_eval
                      ({pexp_desc = Pexp_constant (Pconst_string (s, _)); _}, _)
                ; _ } ] ->
            res := Some s ;
            false
          | _ -> failwith "surprising content in attribute" )
  in
  match !res with
  | None -> failwith "invalid parsetree generated"
  | Some s -> (s, attribs)

let is_uniq htl orig =
  match Hashtbl.find_all htl orig with
  | [_] -> (
    let module (* TODO: Better representation than string *)
        M = struct
      exception F
    end in
    let pre, suf =
      match CCString.Split.right ~by:"." orig with
      | Some (a, b) -> (a ^ ".", "." ^ b)
      | None -> ("", "." ^ orig)
    in
    try
      Hashtbl.iter
        (fun sref () ->
          if
            sref <> orig
            && CCString.prefix ~pre sref
            && CCString.suffix ~suf sref
          then raise_notrace M.F )
        htl ;
      true
    with M.F -> false )
  | [] -> failwith "invalid parsetree generated"
  | _ -> false

let is_uniq_type = is_uniq htl_ocaml_types

let is_uniq = is_uniq htl_ctypes

let get_final_name t = if is_uniq t.sref then t.short_name else t.uniq_name

let replace_expr = function
  | {pexp_desc = Pexp_ident _ as orig; pexp_attributes = _ :: _ as attribs; _}
    as expr
    when List.exists attribs ~f:(fun (x, _) -> x.txt == a_reference_string) ->
    let s, pexp_attributes =
      get_remove_string_exn a_reference_string attribs
    in
    let id_ref, single_ref = CCString.Split.left_exn ~by:"|" s in
    let pexp_desc =
      match is_uniq id_ref with
      | false -> orig
      | true -> Pexp_ident (U.mk_lid single_ref)
    in
    {expr with pexp_desc; pexp_attributes}
  | expr -> expr

let replace_stri = function
  | { pstr_desc =
        Pstr_value
          (Nonrecursive, [({pvb_attributes = _ :: _ as attribs; _} as a)])
    ; _ } as stri ->
    if List.exists attribs ~f:(fun (x, _) -> x.txt == a_orig_name) then
      let s, pvb_attributes = get_remove_string_exn a_orig_name attribs in
      let pvb_pat =
        match is_uniq s with
        | false -> a.pvb_pat
        | true -> CCString.Split.right_exn ~by:"." s |> snd |> U.mk_pat
      in
      let a = {a with pvb_attributes; pvb_pat} in
      {stri with pstr_desc = Pstr_value (Nonrecursive, [a])}
    else if List.exists attribs ~f:(fun (x, _) -> x.txt == a_inmod_ref) then
      let s, pvb_attributes = get_remove_string_exn a_inmod_ref attribs in
      if is_uniq s then U.empty_stri ()
      else
        let a = {a with pvb_attributes} in
        let stri = {stri with pstr_desc = Pstr_value (Nonrecursive, [a])} in
        U.no_warn_unused_pre406 stri
    else stri
  | { pstr_desc =
        Pstr_type (rec', [({ptype_attributes = _ :: _ as attribs; _} as a)])
    ; _ } as stri
    when List.exists attribs ~f:(fun (x, _) -> x.txt == a_inmod_ref) ->
    let s, ptype_attributes = get_remove_string_exn a_inmod_ref attribs in
    if is_uniq_type s then U.empty_stri ()
    else
      let a = {a with ptype_attributes} in
      {stri with pstr_desc = Pstr_type (rec', [a])}
  | stri -> stri

let rec get_parents ~ref_p ~cur_p =
  match (ref_p, cur_p) with
  | [], _ -> []
  | _ :: _, [] -> ref_p
  | hd1 :: tl1, hd2 :: tl2 ->
    if CCString.equal hd1 hd2 then get_parents ~ref_p:tl1 ~cur_p:tl2 else ref_p

type ocaml_t = t

let make_type_alias ?tdl_attrs ?params mod_path short_name =
  let uniq_name = U.safe_mlname ~nowarn:true ~prefix:short_name () in
  let sref = String.concat "." (mod_path @ [short_name]) in
  Hashtbl.add htl_ocaml_types sref () ;
  let id = {mod_path; uniq_name; short_name; sref} in
  let attrs = attr ~cont:sref ~attr:a_inmod_ref in
  let attrs = match tdl_attrs with None -> attrs | Some x -> x @ attrs in
  let manifest =
    let l = match params with None -> [] | Some s -> List.map ~f:fst s in
    U.mk_typc ~l short_name
  in
  let lid = U.mk_loc uniq_name in
  let t = Type.mk ?params ~attrs ~kind:Ptype_abstract lid ~manifest in
  (Str.type_ Recursive [t], id)

let create_type_ref_final ?(l = []) id mod_path =
  let paths = get_parents ~ref_p:id.mod_path ~cur_p:mod_path in
  let name =
    match is_uniq_type id.sref = false && paths = [] with
    | true -> id.uniq_name
    | false -> id.short_name
  in
  let ref' = String.concat "." (paths @ [name]) in
  U.mk_typc ~l ref'

let replace_typ = function
  | { ptyp_desc = Ptyp_constr (_, lorig) as orig
    ; ptyp_attributes = _ :: _ as attribs
    ; _ } as typ
    when List.exists attribs ~f:(fun (x, _) -> x.txt == a_reference_string) ->
    let s, ptyp_attributes =
      get_remove_string_exn a_reference_string attribs
    in
    let id_ref, single_ref = CCString.Split.left_exn ~by:"|" s in
    let ptyp_desc =
      (* types are uniq inside a module *)
      if is_uniq_type id_ref || CCString.contains single_ref '.' then
        Ptyp_constr (U.mk_lid single_ref, lorig)
      else orig
    in
    {typ with ptyp_desc; ptyp_attributes}
  | typ -> typ

let clear () =
  Hashtbl.clear htl_ocaml_types ;
  Hashtbl.clear htl_ctypes
