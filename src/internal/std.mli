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

type 'a return = { return : 'b. 'a -> 'b }

val with_return : ('a return -> 'a) -> 'a

val finally : h:(unit -> unit) -> (unit -> 'a) -> 'a

external identity : 'a -> 'a = "%identity"

module Util : sig
  open Mparsetree.Ast_cur

  val error : ?loc:Ast_helper.loc -> ('a, unit, string, 'b) format4 -> 'a

  val error_exn : ?loc:Ast_helper.loc -> ('a, unit, string, exn) format4 -> 'a

  val safe_ascii_only : string -> string

  val safe_ascii_only_ml : string -> string

  val safe_cname : prefix:string -> string

  val safe_mlname : ?capitalize:bool -> ?prefix:string -> unit -> string

  val with_loc : Ast_helper.loc -> (unit -> 'a) -> 'a

  val str_expr : ?loc:Ast_helper.loc -> string -> Parsetree.expression

  val int_expr :
    ?loc:Ast_helper.loc ->
    ?attrs:Ast_helper.attrs ->
    int ->
    Parsetree.expression

  val mk_loc : 'a -> 'a Location.loc

  val mk_oloc : 'a -> 'a option Location.loc

  val mk_lid : ?loc:Ast_helper.loc -> string -> Longident.t Location.loc

  val mk_lid_l : ?loc:Ast_helper.loc -> string list -> Longident.t Location.loc

  val mk_pat : string -> Parsetree.pattern

  val mk_ident : string -> Parsetree.expression

  val mk_ident_l : string list -> Parsetree.expression

  val mk_typc :
    ?attrs:Ast_helper.attrs ->
    ?l:Parsetree.core_type list ->
    string ->
    Parsetree.core_type

  val mk_typc_l :
    ?attrs:Ast_helper.attrs ->
    ?l:Parsetree.core_type list ->
    string list ->
    Parsetree.core_type

  val empty_stri : unit -> Parsetree.structure_item

  val marshal_to_str_expr : 'a -> Parsetree.expression

  val no_warn_unused_pre406 :
    Parsetree.structure_item -> Parsetree.structure_item

  val no_warn_unused :
    string -> Parsetree.expression -> Parsetree.structure_item

  val no_warn_unused_module :
    ?loc:Ast_helper.loc -> Parsetree.structure_item -> Parsetree.structure_item

  val no_c_comments : string -> string

  val cloc_comment : Ast_helper.loc -> string

  val unsuffixed_file_name : unit -> string

  (* [%sig: ...] [%sigi: ...] broken for some Ocaml versions ... *)
  val sig_from_mod_type : Parsetree.structure_item -> Parsetree.signature

  val ocaml_warning : string -> Parsetree.attribute

  val named_stri :
    ?constr:Parsetree.core_type ->
    string ->
    Parsetree.expression ->
    Parsetree.structure_item

  val alias_type :
    ?attrs:Ast_helper.attrs -> Parsetree.expression -> Parsetree.expression

  val alias_impl_mod_os : ?alias_name:string -> unit -> Parsetree.structure_item

  val alias_impl_mod_let : Parsetree.expression -> Parsetree.expression

  val lid_unflatten : string list -> Longident.t option

  val mk_pat_pconstr : Parsetree.core_type -> string -> Parsetree.pattern
end

module Result : sig
  type ('a, 'b) result = ('a, 'b) Result.result =
    | Ok of 'a
    | Error of 'b
end

module Various : sig
  val use_threads : unit -> bool
end
