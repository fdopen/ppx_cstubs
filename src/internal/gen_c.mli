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

type info = {
  stub_source : string;
  stub_name : string;
  stub_name_byte : string option;
  noalloc : bool;
  float : bool;
  return_errno : bool;
}

val gen_fun :
  'a Ctypes.fn ->
  locs:Location.t list ->
  stubname:string ->
  cfunc:string ->
  release_runtime_lock:bool ->
  noalloc:bool ->
  return_errno:bool ->
  info

val gen_value : 'a Ctypes.fn -> stubname:string -> value:string -> info

val string_of_typ_exn : ?name:string -> 'a Ctypes.typ -> string

val build_inline_fun :
  'a Ctypes.fn ->
  c_name:string ->
  c_body:string ->
  locs:Location.t list ->
  noalloc:bool ->
  (Mparsetree.Ast_cur.Asttypes.arg_label * Marshal_types.expr) list ->
  string

val gen_callback_fun : 'a Ctypes.fn -> Marshal_types.ocaml_funptr -> string

(* fixme: MOVE*)
val is_void : 'a Ctypes.typ -> bool
