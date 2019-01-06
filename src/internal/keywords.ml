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

let htl = Hashtbl.create 8

let () =
  List.iter
    (fun k -> Hashtbl.replace htl k ())
    [ "field"
    ; "funptr"
    ; "funptr_opt"
    ; "ptr"
    ; "ptr_opt"
    ; "returning"
    ; "seal"
    ; "static_funptr" ]

(* cmitomli ctypes.cmi | awk '/val [^\(]/ {print $2}' | ...*)

let htl_modules = Hashtbl.create 16

let () =
  List.iter
    (fun k -> Hashtbl.replace htl_modules k ())
    [ "Complex"
    ; "ComplexL"
    ; "Cstubs_internals"
    ; "Ctypes"
    ; "Ctypes_static"
    ; "LDouble"
    ; "Ppx_cstubs_internals"
    ; "Signed"
    ; "Unsigned" ]

let htl_types = Hashtbl.create 16

let () =
  List.iter
    (fun k -> Hashtbl.replace htl_types k ())
    [ "bool"
    ; "char"
    ; "float"
    ; "int"
    ; "int32"
    ; "int64"
    ; "list"
    ; "nativeint"
    ; "option"
    ; "string"
    ; "unit" ]
