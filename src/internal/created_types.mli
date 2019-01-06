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

val clear : unit -> unit

val add_type : 'a Ctypes.typ -> t -> unit

val get_core_type :
     'a Ctypes.typ
  -> string list
  -> [> `Complete of Parsetree.core_type
     | `Incomplete of Parsetree.core_type
     | `Unknown ]

val change_struct_view_state : 'a Ctypes.typ -> view_state -> unit

val is_typedef_struct : 'a Ctypes.typ -> bool
