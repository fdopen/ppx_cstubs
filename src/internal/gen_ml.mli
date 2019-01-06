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
open Parsetree

type result =
  { extern : structure_item
  ; intern : structure_item }

val external' :
     'a Ctypes.fn
  -> mod_path:string list
  -> string
  -> (Asttypes.arg_label * expression) list
  -> expression
  -> Gen_c.info
  -> result

val foreign :
     'a Ctypes.fn
  -> mod_path:string list
  -> string
  -> Gen_c.info
  -> expression
  -> result

val foreign_value :
     'a Ctypes.fn
  -> mod_path:string list
  -> string
  -> expression
  -> Gen_c.info
  -> result

val constraint_of_typ : mod_path:string list -> 'a Ctypes.typ -> core_type

val create_struct :
     mod_path:string list
  -> type_name:string
  -> field_names:string list
  -> locs:Marshal_types.loc list
  -> 'a Ctypes.typ
  -> type_declaration * type_declaration * core_type list

(* move me somewhere else ... *)
val stdlib_fun : string -> expression
