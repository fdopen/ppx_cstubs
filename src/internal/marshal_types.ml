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

type fun_params =
  { el : (Asttypes.arg_label * Parsetree.expression) list
  ; ret : Parsetree.expression
  ; release_runtime_lock : bool
  ; noalloc : bool
  ; is_inline : bool
  ; mod_path : string list
  ; return_errno : bool
  ; remove_labels : bool
  ; c_name : string
  ; (* external xyz : .... = "c_name" *)
    prim_name : string
  ; (* external prim_name : ..... *)
    uniq_ref_id : Uniq_ref.t }

type id = int

type loc = Ast_helper.loc

type id_loc_param = id * loc

type expr = Parsetree.expression

type enum_type =
  | E_normal of id
  | E_bitmask of id
  | E_normal_bitmask of id * id

type enum_entry =
  { ee_signed_id : int
  ; ee_unsigned_id : int
  ; ee_loc : Location.t
  ; ee_expr : expr
  ; ee_cname : string }

type enum =
  { enum_l : enum_entry list
  ; enum_name : string
  ; enum_is_typedef : bool
  ; enum_type_id : enum_type
  ; enum_loc : loc
  ; enum_unexpected : expr }

type struct_record_params =
  { sr_mod_path : string list
  ; sr_type_name : string
  ; sr_field_names : string list
  ; sr_locs : loc list }
