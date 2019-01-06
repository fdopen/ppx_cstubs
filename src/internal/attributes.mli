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

val replace_expr_string : string
val replace_expr_attrib : string Location.loc * Parsetree.payload

val tdl_string : string
val tdl_attrib : string Location.loc * Parsetree.payload

val remove_string : string
val remove_attrib : string Location.loc * Parsetree.payload

val replace_attr_string : string
val replace_typ_string : string
val replace_struct_string: string
