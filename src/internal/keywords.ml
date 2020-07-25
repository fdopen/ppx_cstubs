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

let htl = Hashtbl.create 8

let () =
  List.iter
    (fun k -> Hashtbl.replace htl k ())
    [
      "abstract";
      "aint";
      "constant";
      "field";
      "foreign";
      "foreign_value";
      "funptr";
      "funptr_opt";
      "header";
      "int";
      "opaque";
      "ptr";
      "ptr_opt";
      "returning";
      "seal";
      "static_funptr";
      "structure";
      "uint";
      "union";
      "@->";
    ]

(* cmitomli ctypes.cmi | awk '/val [^\(]/ {print $2}' | ...*)

let htl_modules = Hashtbl.create 16

let () =
  List.iter
    (fun k -> Hashtbl.replace htl_modules k ())
    [
      "Complex";
      "ComplexL";
      "Cstubs_internals";
      "Ctypes";
      "Ctypes_static";
      "LDouble";
      "Ppx_cstubs";
      "Signed";
      "Unsigned";
    ]

let htl_types = Hashtbl.create 16

let () =
  List.iter
    (fun k -> Hashtbl.replace htl_types k ())
    [
      "bool";
      "char";
      "float";
      "int";
      "int32";
      "int64";
      "list";
      "nativeint";
      "option";
      "string";
      "unit";
    ]
