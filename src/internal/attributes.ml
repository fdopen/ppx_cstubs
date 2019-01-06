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

let h s =
  let x = Location.mkloc s Location.none in
  (x, Mparsetree.Ast_cur.Parsetree.PStr [])

let replace_expr_string = "ppxc__replace_expr"

let replace_expr_attrib = h replace_expr_string

let tdl_string = "ppxc__tdl"

let tdl_attrib = h tdl_string

let remove_string = "ppxc__remove"

let remove_attrib = h remove_string

let replace_attr_string = "ppxc__replace_attr"

let replace_typ_string = "ppxc__replace_typ"

let replace_struct_string = "ppxc__replace_struct"
