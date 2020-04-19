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

open Mparsetree.Ast_cur.Parsetree

type t

type make_result = {
  id : t;
  topmod_vb : structure_item;
  topmod_ref : structure_item;
  main_ref : expression;
}

val make :
  ?constr:core_type ->
  ?main_ref_attrs:attribute list ->
  retype:bool ->
  string list ->
  string ->
  expression ->
  make_result

val replace_expr : expression -> expression

val replace_stri : structure_item -> structure_item

val get_final_name : t -> string

val clear : unit -> unit
