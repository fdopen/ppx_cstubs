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

val ocaml_include_dirs_default : string list

val keep_tmp : bool ref

val nopervasives : bool ref

val use_open_struct : bool ref

val verbosity : int ref

val c_flags : string list ref

val ocaml_include_dirs : string list ref

val c_output_file : string option ref

val ml_input_file : string option ref

val ml_output_file : string option ref

val toolchain : string option ref

val findlib_pkgs : string list ref

val cma_files : string list ref

val pretty : bool ref

val ocamlfind : string

type mode =
  | Regular
  | Emulate

val mode : mode ref

val cc : string option ref

val use_cxx : bool ref

val toolchain_used : unit -> bool

type merlin_state

val merlin_save : unit -> merlin_state

val merlin_restore : merlin_state -> unit
