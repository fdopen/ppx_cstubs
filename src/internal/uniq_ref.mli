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

open Mparsetree.Ast_cur.Parsetree

type t

type make_result =
  { id : t
  ; topmod_vb : structure_item
  ; topmod_ref : structure_item
  ; main_ref : expression }

val make : string list -> string -> expression -> make_result

val replace_expr : expression -> expression

val replace_stri : structure_item -> structure_item

val get_final_name : t -> string

type ocaml_t

val make_type_alias :
     ?tdl_attrs:attribute list
  -> ?params:(core_type * Mparsetree.Ast_cur.Asttypes.variance) list
  -> string list
  -> string
  -> structure_item * ocaml_t

val create_type_ref_final :
  ?l:core_type list -> ocaml_t -> string list -> core_type

val replace_typ : core_type -> core_type

val clear : unit -> unit
