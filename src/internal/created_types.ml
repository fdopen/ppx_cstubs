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
module List = CCListLabels
module U = Std.Util

type structured =
  { s_id : Uniq_ref.t
  ; s_type_id : Uniq_ref.ocaml_t
  ; s_is_union : bool }

type view_state =
  | Vs_Unknown
  | Vs_Complete
  | Vs_Parameterized

type view_structured =
  { vs_id : Uniq_ref.t
  ; vs_type_id : Uniq_ref.ocaml_t
  ; mutable vs_state : view_state }

type view_enum =
  { ve_id : Uniq_ref.t
  ; ve_type_id : Uniq_ref.ocaml_t
  ; ve_is_list : bool }

type view_predefined = {vp_ct : unit -> Parsetree.core_type}

type custom = {cust_id : Uniq_ref.t}

type t =
  | Structured of structured
  | View_structured of view_structured
  | View_typedef_structured of structured
  | View_enum of view_enum
  | View_predefined of view_predefined
  | Custom of custom

type type_abstract

let abstract : 'a Ctypes.typ -> type_abstract = Obj.magic

let t_predefined =
  [ ( abstract Ctypes.string
    , View_predefined {vp_ct = (fun () -> U.mk_typc "string")} )
  ; ( abstract Ctypes.string_opt
    , View_predefined
        {vp_ct = (fun () -> U.mk_typc ~l:[U.mk_typc "string"] "option")} ) ]

let v_types = CCVector.create ()

let init_predef () =
  List.iter t_predefined ~f:(fun s -> CCVector.push v_types s)

let () = init_predef ()

let clear () =
  CCVector.clear v_types ;
  init_predef ()

let add_type : 'a Ctypes.typ -> t -> unit =
 fun t s ->
  (* TODO: optimize for faster find. But the order must be preserved *)
  let t = abstract t in
  CCVector.push v_types (t, s)

let find : 'a Ctypes.typ -> t option =
 fun t ->
  let t = abstract t in
  CCVector.find_map
    (fun (t', info) -> if t' == t then Some info else None)
    v_types

let get_core_type mp = function
  | View_structured {vs_type_id; vs_state = Vs_Complete; _} ->
    `Complete (Uniq_ref.create_type_ref_final vs_type_id mp)
  | View_structured {vs_type_id; vs_state = Vs_Parameterized; _} ->
    let l = [Ast_helper.Typ.any ()] in
    `Incomplete (Uniq_ref.create_type_ref_final ~l vs_type_id mp)
  | View_structured {vs_state = Vs_Unknown; _} | Custom _ -> `Unknown
  | View_typedef_structured {s_type_id; s_is_union; _}
   |Structured {s_type_id; s_is_union; _} ->
    let constr = Uniq_ref.create_type_ref_final s_type_id mp in
    let s =
      match s_is_union with
      | false -> "Ctypes_static.structure"
      | true -> "Ctypes_static.union"
    in
    `Complete (U.mk_typc ~l:[constr] s)
  | View_predefined {vp_ct; _} -> `Complete (vp_ct ())
  | View_enum {ve_is_list; ve_type_id; _} -> (
    let constr = Uniq_ref.create_type_ref_final ve_type_id mp in
    match ve_is_list with
    | false -> `Complete constr
    | true -> `Complete (U.mk_typc ~l:[constr] "list") )

let get_core_type t mod_path =
  match find t with None -> `Unknown | Some s -> get_core_type mod_path s

let change_struct_view_state t s =
  match find t with
  | None -> assert false
  | Some t -> (
    match t with
    | View_structured t -> t.vs_state <- s
    | Custom _ | View_typedef_structured _ | View_enum _ | View_predefined _
     |Structured _ -> assert false )

let is_typedef_struct t =
  match find t with
  | None -> false
  | Some t -> (
    match t with
    | View_typedef_structured _ -> true
    | View_structured _ | Custom _ | View_enum _ | View_predefined _
     |Structured _ -> false )
