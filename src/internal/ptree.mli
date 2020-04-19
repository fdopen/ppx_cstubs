(* This file is part of ppx_cstubs (https://github.com/fdopen/ppx_cstubs)
 * Copyright (c) 2018-2020 fdopen
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

module OSTypes : sig
  val add_abstract :
    ?sub_module:string -> string -> Parsetree.structure_item option

  val add_types_cb : string -> Parsetree.structure_item option

  val add_struct_view : string -> Parsetree.structure_item option

  val types_maybe_used : unit -> unit

  val remove_alias_types : unit -> bool

  val delete_os_inside_type_mod : unit -> bool
end

module Modules : sig
  val pstr_module :
    Parsetree.module_binding ->
    (unit -> Parsetree.structure_item) ->
    Parsetree.structure_item list

  val pstr_recmodule :
    Parsetree.module_binding list ->
    (Parsetree.module_binding -> Parsetree.module_binding) ->
    Parsetree.module_binding list

  val pexp_letmodule :
    string option Asttypes.loc ->
    mexpr:Parsetree.module_expr ->
    fmexpr:(unit -> Parsetree.module_expr) ->
    fexpr:(unit -> Parsetree.expression) ->
    Parsetree.module_expr * Parsetree.expression

  val pexp_pack :
    Parsetree.module_expr ->
    (unit -> Parsetree.expression) ->
    Parsetree.expression

  val pstr_include :
    Parsetree.include_declaration ->
    (unit -> Parsetree.structure_item) ->
    Parsetree.structure_item list

  val pstr_open :
    Parsetree.open_declaration ->
    (unit -> Parsetree.structure_item) ->
    Parsetree.structure_item list

  val pexp_open :
    fodl:(unit -> Parsetree.open_declaration) ->
    fexpr:(unit -> Parsetree.expression) ->
    Parsetree.open_declaration ->
    Parsetree.open_declaration * Parsetree.expression
end

module Impl_mod : sig
  val add_entry : Parsetree.structure_item -> unit

  val get_mod_path : unit -> string list

  val create_ref_lid : string -> Longident.t Location.loc

  val create_type_ref : string -> Parsetree.core_type

  val add_named :
    ?constr:Parsetree.core_type ->
    ?name_check:bool ->
    ?attrs:Parsetree.attribute list ->
    retype:bool ->
    string ->
    Parsetree.expression ->
    Parsetree.expression

  val add_external :
    Parsetree.structure_item ->
    Parsetree.structure_item ->
    name:string ->
    Uniq_ref.t * Parsetree.expression

  val add_external_anon :
    Parsetree.structure_item ->
    Parsetree.structure_item ->
    string ->
    Parsetree.expression

  val add_opaq :
    Parsetree.attribute list ->
    Parsetree.structure_item ->
    string ->
    Uniq_ref.t * Parsetree.expression

  val add_unit : Parsetree.expression -> Parsetree.expression
end

module Type_mod : sig
  val add_entry : Parsetree.structure_item -> unit
end

module Topscript : sig
  val add_build_external : Parsetree.structure_item -> unit

  val add_extract : Parsetree.structure_item -> unit

  val add_extract_phase0 : Parsetree.structure_item -> unit

  val run : unit -> unit
end

val all_top_modules : unit -> Parsetree.structure_item list

val type_mod_is_used : unit -> bool

val clear : unit -> unit
