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

type fun_params = {
  el : (Ast_405.Asttypes.arg_label * Migrate_parsetree.Ast_405.Parsetree.expression) list;
  ret : Migrate_parsetree.Ast_405.Parsetree.expression;
  release_runtime_lock: bool;
  noalloc: bool;
  is_inline: bool;
  return_errno: bool;
  remove_labels: bool;
  c_name: string; (* external xyz : .... = "c_name" *)
  prim_name:string; (* external prim_name : ..... *)
  ocaml_name: string; (* unique name in generated module *)
}

type id = int
type loc = Migrate_parsetree.Ast_405.Ast_helper.loc
type id_loc_param = id * loc
type expr = Migrate_parsetree.Ast_405.Parsetree.expression

type enum = {
  enum_l : (int * Ast_405.Location.t * string) list;
  enum_name: string;
  enum_is_typedef: bool;
  enum_id: id;
  enum_loc: loc;
}
