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

type extr_info

type info =
  { signed : bool
  ; min : string option
  ; max : string option
  ; info : extr_info }

type result =
  | Expr of Mparsetree.Ast_cur.Parsetree.expression
  | Underflow
  | Overflow

val prepare : 'a Ctypes_static.typ -> info option

val gen : info -> string -> result
