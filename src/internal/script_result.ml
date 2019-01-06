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

let htl_expr = Hashtbl.create 64

let htl_used = Hashtbl.create 64

let htl_type = Hashtbl.create 64

let htl_stri = Hashtbl.create 16

let htl_records = Hashtbl.create 8

let foreign_used = ref false

let c_source = ref None

let clear () =
  Hashtbl.clear htl_expr ;
  Hashtbl.clear htl_stri ;
  Hashtbl.clear htl_used ;
  Hashtbl.clear htl_type ;
  Hashtbl.clear htl_records ;
  foreign_used := false ;
  c_source := None
