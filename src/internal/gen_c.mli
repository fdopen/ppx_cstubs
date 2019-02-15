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

type info =
  { stub_source : string
  ; stub_name : string
  ; stub_name_byte : string option
  ; noalloc : bool
  ; float : bool
  ; return_errno : bool }

val gen_fun :
     'a Ctypes.fn
  -> locs:Location.t list
  -> stubname:string
  -> cfunc:string
  -> release_runtime_lock:bool
  -> noalloc:bool
  -> return_errno:bool
  -> info

val gen_value : 'a Ctypes.fn -> stubname:string -> value:string -> info

val string_of_typ_exn : ?name:string -> 'a Ctypes.typ -> string

val build_inline_fun :
     'a Ctypes.fn
  -> c_name:string
  -> c_body:string
  -> locs:Location.t list
  -> noalloc:bool
  -> (string * string) list
  -> string

(* fixme: MOVE*)
val is_void : 'a Ctypes.typ -> bool
