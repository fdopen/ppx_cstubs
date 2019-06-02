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

val clear : unit -> unit

val add_type : 'a Ctypes.typ -> t -> unit

val get_core_type :
     'a Ctypes.typ
  -> string list
  -> [> `Complete of Mparsetree.Ast_cur.Parsetree.core_type
     | `Funptr of
          Mparsetree.Ast_cur.Parsetree.core_type
       -> Mparsetree.Ast_cur.Parsetree.core_type
     | `Unknown ]

val is_typedef_struct : 'a Ctypes.typ -> bool

val add_custom : old:'a Ctypes_static.typ -> new':'b Ctypes_static.typ -> unit
