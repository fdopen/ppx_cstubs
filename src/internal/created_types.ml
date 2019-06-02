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
module List = CCListLabels
module U = Std.Util

type structured =
  { s_type_id : Uniq_ref.ocaml_t
  ; s_is_union : bool }

type view_enum =
  { ve_type_id : Uniq_ref.ocaml_t
  ; ve_is_list : bool }

type view_abstract = {via_type_id : Uniq_ref.ocaml_t}

type t =
  | Structured of structured
  | View_structured of view_abstract
  | View_typedef_structured of structured
  | View_enum of view_enum
  | View_int_alias of view_abstract
  | Opaque of view_abstract
  | Abstract of view_abstract
  | Funptr of view_abstract

type type_abstract

let abstract : 'a Ctypes.typ -> type_abstract = Obj.magic

let v_types = CCVector.create ()

let clear () = CCVector.clear v_types

let add_type : 'a Ctypes.typ -> t -> unit =
 fun t s ->
  let t = abstract t in
  CCVector.push v_types (t, s)

let find : 'a Ctypes.typ -> t option =
 fun t ->
  let t = abstract t in
  CCVector.find_map
    (fun (t', info) -> if t' == t then Some info else None)
    v_types

let get_core_type mp = function
  | View_typedef_structured {s_type_id; s_is_union}
   |Structured {s_type_id; s_is_union} ->
    let constr = Uniq_ref.create_type_ref_final s_type_id mp in
    let s =
      match s_is_union with
      | false -> "Ctypes.structure"
      | true -> "Ctypes.union"
    in
    `Complete (U.mk_typc ~l:[constr] s)
  | View_enum {ve_is_list; ve_type_id} -> (
    let constr = Uniq_ref.create_type_ref_final ve_type_id mp in
    match ve_is_list with
    | false -> `Complete constr
    | true -> `Complete (U.mk_typc ~l:[constr] "list") )
  | View_structured {via_type_id}
   |Opaque {via_type_id}
   |View_int_alias {via_type_id} ->
    `Complete (Uniq_ref.create_type_ref_final via_type_id mp)
  | Abstract {via_type_id} ->
    let constr = Uniq_ref.create_type_ref_final via_type_id mp in
    `Complete (U.mk_typc ~l:[constr] "Ctypes.abstract")
  | Funptr {via_type_id} ->
    let f t =
      let constr = Uniq_ref.create_type_ref_final ~l:[t] via_type_id mp in
      Ast_helper.Typ.constr (U.mk_lid "Ctypes.static_funptr") [constr]
    in
    `Funptr f

let get_core_type t mod_path =
  match find t with None -> `Unknown | Some s -> get_core_type mod_path s

let is_typedef_struct t =
  match find t with
  | None -> false
  | Some t -> (
    match t with
    | View_typedef_structured _ -> true
    | View_structured _ | View_enum _ | Structured _ | View_int_alias _
     |Opaque _ | Abstract _ | Funptr _ ->
      false )

let add_custom ~old ~new' =
  match find old with None -> () | Some x -> add_type new' x
