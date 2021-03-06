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

val replace_expr_string : string

val replace_expr_attrib : Parsetree.attribute

val tdl_string : string

val tdl_attrib : Parsetree.attribute

val remove_string : string

val remove_attrib : Parsetree.attribute

val replace_attr_string : string

val replace_typ_string : string

val open_struct_type_mod_string : string

val open_struct_type_mod_attrib : Parsetree.attribute

val open_struct_body_string : string

val open_struct_body_attrib : Parsetree.attribute

val open_struct_ifthenelse_string : string

val open_struct_ifthenelse_attrib : Parsetree.attribute

val open_struct_openmod_string : string

val open_struct_openmod_attrib : Parsetree.attribute

val manifest_replace_string : string

val manifest_replace_attrib : Parsetree.attribute
