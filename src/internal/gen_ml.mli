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

type result = {
  extern : structure_item;
  intern : structure_item;
}

val external' :
  'a Ctypes.fn ->
  string ->
  (Asttypes.arg_label * expression) list ->
  expression ->
  Gen_c.info ->
  result

val foreign : 'a Ctypes.fn -> string -> Gen_c.info -> expression -> result

val foreign_value : 'a Ctypes.fn -> string -> expression -> Gen_c.info -> result

val ocaml_funptr : Marshal_types.ocaml_funptr -> 'a Ctypes.fn -> unit

(* move me somewhere else ... *)
val stdlib_fun : string -> expression

type record_stris = {
  r_stri_top : structure_item list;
  r_stri_bottom : structure_item list;
  r_stri_type_mod : structure_item list;
}

val gen_record_stris :
  mod_path:string list ->
  type_name:string ->
  (string * Mparsetree.Ast_cur.Ast_helper.loc * expression) list ->
  record_stris

val pat_expand_prim : 'a Ctypes_primitive_types.prim -> pattern

val match_nw : expression -> case -> expression
